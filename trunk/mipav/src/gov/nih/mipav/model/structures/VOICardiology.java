package gov.nih.mipav.model.structures;

import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.*;

import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.text.*;

import java.util.*;


/**
 * DOCUMENT ME!
 */
public class VOICardiology extends VOIBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2406308173924990457L;

    /** inner contour. */
    public static int INNER = 1;

    /** outer contour. */
    public static int OUTER = 2;

    /** infarction contour. */
    public static int INFARCTION = 3;

    /** both inner and infarction (intersection) index. */
    public static int BOTH = -100;

    /** DOCUMENT ME! */
    public static int NEAR_A_POINT = -1;

    /** DOCUMENT ME! */
    public static double HALF_PI = Math.PI / 2.0;

    /** DOCUMENT ME! */
    public static double TH_PI = Math.PI * 1.5;

    /** DOCUMENT ME! */
    public static double BIG_NUMBER = 1.0E10;

    /** DOCUMENT ME! */
    public static double SMALL_NUMBER = 1.0E-10;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean[] activeArray;

    /** DOCUMENT ME! */
    private CardioVector3f centerPt = null;

    /** Indicates whether or not the contour is closed. */
    private boolean closed = true;

    /** DOCUMENT ME! */
    private boolean flagRetrace = false;

    /** These four variables are used in the retrace mode of the Contour. */
    private int indexRetrace = -99;

    /** DOCUMENT ME! */
    private Polygon[] infarctionGons;

    /** Saves the vertex point of bounding box. See nearBoundPoint method in this class. */
    private int nearBoundPoint = NOT_A_POINT;

    /** DOCUMENT ME! */
    private int numInfarctions = 0;

    /** Number of pixels in the array used in graphing intensity along the boundary. */
    private int numPixels;

    /** DOCUMENT ME! */
    private int numSections;

    /** DOCUMENT ME! */
    private VOIContour oldContour;

    /** DOCUMENT ME! */
    private boolean showCenter = true;

    /** DOCUMENT ME! */
    private boolean showPoints = true;

    /** DOCUMENT ME! */
    private VOIContour wholeInfarction = null;

    /**
     * Stores the x coordinates for the bounding box. The upper left corner and the lower right corner, respectively.
     */
    private float[] xBounds = new float[2];

    /** Used in contains method so that memory doesn't constantly need to be reallocated. */
    private float[] xPts = null;

    /**
     * Stores the y coordinates for the bounding box. The upper left corner and the lower right corner, respectively.
     */
    private float[] yBounds = new float[2];

    /** Used in contains method so that memory doesn't constantly need to be reallocated. */
    private float[] yPts = null;

    /**
     * Stores the z coordinates for the bounding box. The upper left corner and the lower right corner, respectively.
     */
    private float[] zBounds = new float[2];

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * default constructor.
     *
     * @param  numSections  DOCUMENT ME!
     * @param  centerPt     DOCUMENT ME!
     */
    public VOICardiology(int numSections, Vector3f centerPt) {
        this.numSections = numSections;
        this.centerPt = new CardioVector3f(centerPt.X, centerPt.Y, centerPt.Z, -1, -1, false, false);
        this.activeArray = new boolean[numSections];
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param  angle     DOCUMENT ME!
     * @param  centerPt  DOCUMENT ME!
     * @param  secondPt  DOCUMENT ME!
     * @param  dimX      DOCUMENT ME!
     * @param  dimY      DOCUMENT ME!
     */
    public static void getSecondPoint(double angle, Vector3f centerPt, Vector3f secondPt, int dimX, int dimY) {
        double slope = Math.tan(angle);

        if ((slope == Double.NaN) || (slope > VOICardiology.BIG_NUMBER)) {

            // System.err.println("slope is NaN");
            secondPt.X = centerPt.X;

            if (angle < Math.PI) {
                secondPt.Y = dimY;
            } else {
                secondPt.Y = 0;
            }
        } else if ((slope == 0) || ((slope < VOICardiology.SMALL_NUMBER) && (slope > -(VOICardiology.SMALL_NUMBER)))) {

            // System.err.println("slope is zero");
            secondPt.Y = centerPt.Y;

            if ((angle > VOICardiology.HALF_PI) && (angle < VOICardiology.TH_PI)) {
                secondPt.X = 0;
            } else {
                secondPt.X = dimX;
            }
        } else {

            // System.err.println("slope is: " + slope);
            if ((angle > VOICardiology.TH_PI) || (angle < VOICardiology.HALF_PI)) {
                secondPt.X = dimX;
            } else {
                secondPt.X = 0;
            }

            secondPt.Y = (int) ((slope * (secondPt.X - centerPt.X)) + centerPt.Y);
        }
        // System.err.println(" angle: " + angle + ", line slope: " + slope +", second pt: (" + secondPt.X + "," +
        // secondPt.Y + ")");
    }

    /**
     * DOCUMENT ME!
     *
     * @param   p1            DOCUMENT ME!
     * @param   p2            DOCUMENT ME!
     * @param   p3            DOCUMENT ME!
     * @param   p4            DOCUMENT ME!
     * @param   intersection  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static boolean intersects(Vector3f p1, Vector3f p2, Vector3f p3, Vector3f p4, Vector3f intersection) {
        double denom = 0f;
        double uAnum = 0f;
        double uBnum = 0f;
        double uA = 0f;
        double uB = 0f;
        denom = ((p4.Y - p3.Y) * (p2.X - p1.X)) - ((p4.X - p3.X) * (p2.Y - p1.Y));
        uAnum = ((p4.X - p3.X) * (p1.Y - p3.Y)) - ((p4.Y - p3.Y) * (p1.X - p3.X));
        uBnum = ((p2.X - p1.X) * (p1.Y - p3.Y)) - ((p2.Y - p1.Y) * (p1.X - p3.X));

        if (denom == 0) {

            // System.err.println("Denom is 0");
            return false;
        }

        uA = uAnum / denom;
        uB = uBnum / denom;

        if ((uA >= 0) && (uA <= 1) && (uB >= 0) && (uB <= 1)) {
            intersection.X = p1.X + (float) (uA * (p2.X - p1.X));
            intersection.Y = p1.Y + (float) (uA * (p2.Y - p1.Y));

            // System.err.println("found intersection to be: " + intersection.X + "," + intersection.Y + "\n\n");
            return true;
        } else {
            return false;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  newPt           DOCUMENT ME!
     * @param  type            DOCUMENT ME!
     * @param  sectionNum      DOCUMENT ME!
     * @param  isIntersection  DOCUMENT ME!
     * @param  isShared        DOCUMENT ME!
     */
    public void addPoint(Vector3f newPt, int type, int sectionNum, boolean isIntersection, boolean isShared) {
        this.addElement(new CardioVector3f(newPt.X, newPt.Y, newPt.Z, type, sectionNum, isIntersection, isShared));
    }

    /**
     * Calculates the area of contour using vector cross product method - fast !!
     *
     * @return  returns the area
     */
    public float area() {
        int i;
        int length;
        float result = 0;
        Vector2f oldVector = new Vector2f(); 
        Vector2f newVector = new Vector2f(); 
        
        Vector2f[] pts = null;

        try {
            pts = new Vector2f[size()];
            length = size();

            for (i = 0; i < length; i++) {
                pts[i] = new Vector2f(((Vector3f) (elementAt(i))).X, ((Vector3f) (elementAt(i))).Y);
            }
        } catch (OutOfMemoryError error) {
            System.gc();

            return 0;
        }

        if (size() >= 3) {
            oldVector.Sub( pts[1], pts[0] );

            for (i = 2; i < length; i++) {
                newVector.Sub( pts[i], pts[0] );
                result += newVector.Cross(oldVector) * 0.5;
                oldVector = newVector;
            }

            // if result is negative then points are ordered clockwise
            // if result is positive then points are ordered counter-clockwise
            if (result < 0) {
                result = -result;
            }
        }

        // System.out.println("Contour Area = " + result);
        return result;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   data        DOCUMENT ME!
     * @param   sectionNum  DOCUMENT ME!
     * @param   imgXDim     DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float[] calcAverageIntensity(float[] data, int sectionNum, int imgXDim) {

        // we will build a temporary VOIContour for the section
        // for now do not worry about the infarction area
        VOIContour entireSection = new VOIContour(true);
        int[] indicesInner = new int[3];
        int[] indicesOuter = new int[3];

        /**
         * first build a counter for the entire section
         */
        // get the indices for the INNER
        getSectionIndices(INNER, sectionNum, -1, indicesInner);

        // add the inner points
        for (int i = indicesInner[0], j = 0; i <= indicesInner[1]; i++, j++) {
            entireSection.addElement((Vector3f) elementAt(i));
        }

        if (indicesInner[1] != indicesInner[2]) {
            entireSection.addElement((Vector3f) elementAt(indicesInner[2]));
        }

        // get the indices for the outer (add backwards to complete the
        // counter-clockwise contour)
        getSectionIndices(OUTER, sectionNum, -1, indicesOuter);

        if (indicesOuter[1] != indicesOuter[2]) {
            entireSection.addElement((Vector3f) elementAt(indicesOuter[2]));
        }

        for (int i = indicesOuter[1], j = 1; i >= indicesOuter[0]; i--, j++) {
            entireSection.addElement((Vector3f) elementAt(i));
        }

        // float totalIntensity = entireSection.calcIntensity(data, imgXDim);
        // int numPix = entireSection.getLastNumPixels();
        // ui.getMessageFrame().getData().append("Section: " + sectionNum +
        // " average intensity: " + (totalIntensity/numPix) +
        // " total num pixels: " + numPix + "\n");
        VOIContour entireInfarction = createInfarctionContour();
        int[] sectionXBounds = new int[2];
        int[] sectionYBounds = new int[2];
        int[] sectionZBounds = new int[2];
        entireSection.getBounds(sectionXBounds, sectionYBounds, sectionZBounds);

        // run through the bounds of the section, and do a contains() to find
        // if both the infarction and section intersect, thus finding pixel area
        int infarctionPixels = 0;
        int numPix = 0;
        float totalInfarctionIntensity = 0;
        float totalIntensity = 0;
        int offset = 0;
        int y, x;

        for (y = sectionYBounds[0]; y < sectionYBounds[1]; y++) {
            offset = y * imgXDim;

            for (x = sectionXBounds[0]; x < sectionXBounds[1]; x++) {

                if (entireSection.contains(x, y, false)) {
                    numPix++;
                    totalIntensity += data[offset + x];

                    if (entireInfarction.contains(x, y, false)) {
                        infarctionPixels++;
                        totalInfarctionIntensity += data[offset + x];
                    }
                }
            }
        }

        float[] results = new float[2];
        results[0] = (totalIntensity / numPix);

        if (infarctionPixels > 0) {
            results[1] = (totalInfarctionIntensity / infarctionPixels);
        } else {
            results[1] = 0;
        }

        System.err.println("Section: " + sectionNum + " numPixels: " + numPix + ", numInfarctionPixels: " +
                           infarctionPixels);
        ViewUserInterface.getReference().getMessageFrame().getData().append("Section: " + sectionNum +
                                                                            "\n\taverage intensity: " + results[0] +
                                                                            "\n\ttotal num pixels: " + numPix +
                                                                            "\n\tinfarction average intensity: " +
                                                                            results[1] + "\n");

        return results;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   sectionNum   DOCUMENT ME!
     * @param   dimX         DOCUMENT ME!
     * @param   dimY         DOCUMENT ME!
     * @param   resolutions  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public double calcAverageLength(int sectionNum, int dimX, int dimY, float[] resolutions) {
        double sectionAngle = (2.0 * Math.PI) / numSections;
        double sectionAngleStart = sectionAngle * sectionNum;
        double sectionAngleEnd = sectionAngleStart + sectionAngle;
        double angleIncrement = Math.PI / 400.0;
        double i;
        Vector3f secondPt = new Vector3f();
        Vector3f intersectionPtInner = new Vector3f();
        Vector3f intersectionPtOuter = new Vector3f();
        int[] indicesInner = new int[3];
        int[] indicesOuter = new int[3];

        // get the indices for the INNER
        getSectionIndices(INNER, sectionNum, -1, indicesInner);

        // get the indices for the OUTER
        getSectionIndices(OUTER, sectionNum, -1, indicesOuter);

        // get ALL indices for infarction (could be multiple sections)
        Vector infarctionIndicesVector = new Vector();
        int[] infIndices = new int[3];
        int infCounter = 0;

        while (getSectionIndices(INFARCTION, sectionNum, infCounter++, infIndices)) {
            infarctionIndicesVector.add(infIndices);
            infIndices = new int[3];
        }

        int numInfSections = infarctionIndicesVector.size();
        System.err.println("number of infarctions put into the vector: " + numInfSections);

        Vector3f[] intersectionPtInfarctions = new Vector3f[numInfSections];

        // new the points
        for (int n = 0; n < numInfSections; n++) {
            intersectionPtInfarctions[n] = new Vector3f(centerPt.X, centerPt.Y, 0);
        }

        // will count each infarction section separately
        boolean[] foundIntersectionInfarction = new boolean[numInfSections];
        double[] differenceTotalInf = new double[numInfSections];
        boolean foundIntersectionInner = false;
        boolean foundIntersectionOuter = false;

        // boolean foundIntersectionInfarction = false;
        // find the indices of the first and last points (which are points of intersection)
        // for the particular section. if it the
        double differenceTotal = 0;
        double distanceInner = 0.0;
        double distanceOuter = 0.0;
        double distanceInfarction = 0.0;
        int numIntersectionsEntire = 0;
        int[] numIntersectionsInfarction = new int[numInfSections];

        // System.err.println("found Intersection infarction length: " + foundIntersectionInfarction.length);
        for (i = sectionAngleStart; i < sectionAngleEnd; i += angleIncrement) {
            foundIntersectionInner = false;
            foundIntersectionOuter = false;

            for (int m = 0; m < foundIntersectionInfarction.length; m++) {
                foundIntersectionInfarction[m] = false;
            }

            getSecondPoint(i, centerPt, secondPt, dimX, dimY);

            // look for INNER intersection point
            foundIntersectionInner = intersects(centerPt, secondPt, (Vector3f) elementAt(indicesInner[1]),
                                                (Vector3f) elementAt(indicesInner[2]), intersectionPtInner);

            for (int j = indicesInner[0]; !foundIntersectionInner && (j < indicesInner[1]); j++) {
                foundIntersectionInner = intersects(centerPt, secondPt, (Vector3f) elementAt(j),
                                                    (Vector3f) elementAt(j + 1), intersectionPtInner);
            }

            // look for OUTER intersection poitn
            foundIntersectionOuter = intersects(centerPt, secondPt, (Vector3f) elementAt(indicesOuter[1]),
                                                (Vector3f) elementAt(indicesOuter[2]), intersectionPtOuter);

            for (int j = indicesOuter[0]; !foundIntersectionOuter && (j < indicesOuter[1]); j++) {
                foundIntersectionOuter = intersects(centerPt, secondPt, (Vector3f) elementAt(j),
                                                    (Vector3f) elementAt(j + 1), intersectionPtOuter);
            }

            // finally look for an intersection point with the infarction
            // if there are two points, then take the point farthest from the center
            Vector3f tempPoint = new Vector3f();
            double testDistance = 0;
            boolean foundInf = false;
            int endInfIndex = 0;

            // System.err.println("num inf sections: " + numInfSections);
            for (int j = 0; j < numInfSections; j++) {
                intersectionPtInfarctions[j].X = centerPt.X;
                intersectionPtInfarctions[j].Y = centerPt.Y;
                infIndices = (int[]) infarctionIndicesVector.elementAt(j);

                if ((infIndices[2] - 1) == infIndices[0]) {
                    endInfIndex = infIndices[2];
                } else {
                    endInfIndex = infIndices[2] - 1;
                }

                // System.err.println("Indices: " + infIndices[0] + "," +
                // infIndices[1] + "," + infIndices[2]);
                for (int k = infIndices[0]; k < endInfIndex; k++) {
                    foundInf = intersects(centerPt, secondPt, (Vector3f) elementAt(k), (Vector3f) elementAt(k + 1),
                                          tempPoint);

                    // check to see which is the farthest from the center (make sure it's not already set to center
                    if (foundInf) {
                        foundIntersectionInfarction[j] = true;

                        if ((intersectionPtInfarctions[j].X != centerPt.X) &&
                                (intersectionPtInfarctions[j].Y != centerPt.Y)) {
                            distanceInfarction = MipavMath.length(intersectionPtInfarctions[j].X,
                                                                  intersectionPtInfarctions[j].Y, centerPt.X,
                                                                  centerPt.Y, resolutions);
                            testDistance = MipavMath.length(tempPoint.X, tempPoint.Y, centerPt.X, centerPt.Y,
                                                            resolutions);

                            if (testDistance > distanceInfarction) {

                                // System.err.println("difference (for replace): " + (testDistance -
                                // distanceInfarction));
                                intersectionPtInfarctions[j].X = tempPoint.X;
                                intersectionPtInfarctions[j].Y = tempPoint.Y;
                            }
                        } else {
                            intersectionPtInfarctions[j].X = tempPoint.X;
                            intersectionPtInfarctions[j].Y = tempPoint.Y;
                        }
                    }
                }
            }

            // if the intersection point was found, calculate the distance
            if (foundIntersectionInner && foundIntersectionOuter) {
                distanceInner = MipavMath.length(intersectionPtInner.X, intersectionPtInner.Y, centerPt.X, centerPt.Y,
                                                 resolutions);
                distanceOuter = MipavMath.length(intersectionPtOuter.X, intersectionPtOuter.Y, centerPt.X, centerPt.Y,
                                                 resolutions);
                differenceTotal += (distanceOuter - distanceInner);
                numIntersectionsEntire++;

                for (int m = 0; m < foundIntersectionInfarction.length; m++) {

                    if (foundIntersectionInfarction[m]) {
                        distanceInfarction = MipavMath.length(intersectionPtInfarctions[m].X,
                                                              intersectionPtInfarctions[m].Y, centerPt.X, centerPt.Y,
                                                              resolutions);
                        differenceTotalInf[m] += (distanceInfarction - distanceInner);
                        numIntersectionsInfarction[m]++;
                    }
                }
            }
        }

        // System.err.println("Number of intersections used in calculation: " + numIntersectionsEntire);
        System.err.println("Entire section average length (INNER to OUTER): " +
                           (differenceTotal / numIntersectionsEntire));
        ViewUserInterface.getReference().getMessageFrame().getData().append("Section: " + sectionNum +
                                                                            " average length (inner to outer): " +
                                                                            (differenceTotal / numIntersectionsEntire) +
                                                                            "\n");

        for (int b = 0; b < numInfSections; b++) {
            System.err.println("infarction section (clockwise)" + b + " average length: " +
                               (differenceTotalInf[b] / numIntersectionsInfarction[b]));
            ViewUserInterface.getReference().getMessageFrame().getData().append("\t Infarction section (clockwise): " +
                                                                                b + " average length: " +
                                                                                (differenceTotalInf[b] /
                                                                                     numIntersectionsInfarction[b]) +
                                                                                "\n");
        }

        return (differenceTotal / numIntersectionsEntire);
    }

    /**
     * Calculates the average or total intensity of the VOIContour region.
     *
     * @param   data     image data
     * @param   imgXDim  x-dimension of the image
     *
     * @return  returns total sum of intensity in the VOIContour
     */
    public float calcIntensity(float[] data, int imgXDim) {
        int i, j;
        numPixels = 0;

        float sum = 0;
        contains(0, 0, true); // load buffers
        getBounds(xBounds, yBounds, zBounds);

        int startX = Math.round(xBounds[0]);
        int startY = Math.round(yBounds[0]);
        int endX = Math.round(xBounds[1]);
        int endY = Math.round(yBounds[1]);

        for (j = startY; j <= endY; j++) {

            for (i = startX; i <= endX; i++) {

                if (contains(i, j, false)) {
                    numPixels++;
                    sum += data[(j * imgXDim) + i];
                }
            }
        }

        return sum;
    }

    /**
     * Calculates the average or total intensity of the VOIContour region.
     *
     * @param   data       image data
     * @param   imgXDim    x-dimension of the image
     * @param   threshold  DOCUMENT ME!
     *
     * @return  returns total sum of intensity in the VOIContour
     */
    public float calcIntensityThreshold(float[] data, int imgXDim, float threshold) {
        int i, j;
        numPixels = 0;

        float sum = 0;
        contains(0, 0, true); // load buffers
        getBounds(xBounds, yBounds, zBounds);

        int startX = Math.round(xBounds[0]);
        int startY = Math.round(yBounds[0]);
        int endX = Math.round(xBounds[1]);
        int endY = Math.round(yBounds[1]);

        for (j = startY; j <= endY; j++) {

            for (i = startX; i <= endX; i++) {

                if (contains(i, j, false)) {

                    if (data[(j * imgXDim) + i] >= threshold) {
                        numPixels++;
                        sum += data[(j * imgXDim) + i];
                    }
                }
            }
        }

        return sum;
    }

    /**
     * Calculates the average or total intensity of the VOIContour region.
     *
     * @param   data     image data
     * @param   imgXDim  x-dimension of the image
     * @param   RGorB    which channel to calc. (0 = R, 1 = G, 2 = B)
     *
     * @return  returns total sum of intensity in the VOIContour
     */
    public float calcRGBIntensity(float[] data, int imgXDim, int RGorB) {
        int i, j;
        numPixels = 0;

        float sum = 0;
        int os;
        contains(0, 0, true); // load buffers
        getBounds(xBounds, yBounds, zBounds);

        int startX = Math.round(xBounds[0]);
        int startY = Math.round(yBounds[0]);
        int endX = Math.round(xBounds[1]);
        int endY = Math.round(yBounds[1]);

        for (j = startY; j <= endY; j++) {

            for (i = startX, os = 0; i <= endX; i++, os++) {

                if (contains(i, j, false)) {
                    numPixels++;

                    if (RGorB == 0) {
                        sum += data[(j * 4 * imgXDim) + (startX * 4) + (os * 4) + 1];
                    } // skip alpha and get Red info
                    else if (RGorB == 1) {
                        sum += data[(j * 4 * imgXDim) + (startX * 4) + (os * 4) + 2];
                    } else {
                        sum += data[(j * 4 * imgXDim) + (startX * 4) + (os * 4) + 3];
                    }
                }
            }
        }

        return sum;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   data       DOCUMENT ME!
     * @param   imgXDim    DOCUMENT ME!
     * @param   RGorB      DOCUMENT ME!
     * @param   threshold  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float calcRGBIntensityThreshold(float[] data, int imgXDim, int RGorB, float threshold) {
        int i, j;
        numPixels = 0;

        float sum = 0;
        int os;
        float tempSum = 0;
        contains(0, 0, true); // load buffers
        getBounds(xBounds, yBounds, zBounds);

        int startX = Math.round(xBounds[0]);
        int startY = Math.round(yBounds[0]);
        int endX = Math.round(xBounds[1]);
        int endY = Math.round(yBounds[1]);

        for (j = startY; j <= endY; j++) {

            for (i = startX, os = 0; i <= endX; i++, os++) {

                if (contains(i, j, false)) {
                    tempSum = data[(j * 4 * imgXDim) + (startX * 4) + (os * 4) + 1] +
                              data[(j * 4 * imgXDim) + (startX * 4) + (os * 4) + 2] +
                              data[(j * 4 * imgXDim) + (startX * 4) + (os * 4) + 3];

                    if ((tempSum / 3.0f) >= threshold) {
                        numPixels++;

                        if (RGorB == 0) {
                            sum += data[(j * 4 * imgXDim) + (startX * 4) + (os * 4) + 1];
                        } // skip alpha and get Red info
                        else if (RGorB == 1) {
                            sum += data[(j * 4 * imgXDim) + (startX * 4) + (os * 4) + 2];
                        } else {
                            sum += data[(j * 4 * imgXDim) + (startX * 4) + (os * 4) + 3];
                        }
                    }
                }
            }
        }

        return sum;
    }

    /**
     * Determines if the supplied point can be found within the points that define the contour.
     *
     * @param   _x           x-coordinate of the point in question
     * @param   _y           y-coordinate of the point in question
     * @param   forceReload  if true export points from VOI structure
     *
     * @return  true if point is within the contour
     */
    public boolean contains(int _x, int _y, boolean forceReload) {
        int i;
        int nPts = size();
        int j = nPts - 1;
        boolean isInside = false;
        float x = _x + 0.49f; // Matt add doc !!!
        float y = _y + 0.49f;

        // reloads points in this array for speed purposes
        // System.err.println("contains :!!!!!!!!!!!?");
        if (forceReload == true) {

            try {
                xPts = new float[size()];
                yPts = new float[size()];
                // System.err.println("contains : xPts.len = " + xPts.length + " yPts.len = " + yPts.length);
            } catch (OutOfMemoryError error) {
                System.out.println("VOIContour.contains: memory allocation error");
                System.gc();

                return false;
            }

            for (i = 0; i < nPts; i++) {
                xPts[i] = ((Vector3f) (elementAt(i))).X;
                yPts[i] = ((Vector3f) (elementAt(i))).Y;
            }
        }

        // System.out.println("contains : npts = " + nPts);
        for (i = 0; i < nPts; i++) {

            if (((yPts[j] <= y) && (y < yPts[i]) && (area2(xPts[i], yPts[i], xPts[j], yPts[j], x, y) >= 0)) ||
                    ((yPts[i] <= y) && (y < yPts[j]) && (area2(xPts[j], yPts[j], xPts[i], yPts[i], x, y) >= 0))) {
                isInside = !isInside;
            }

            j = i;
        }

        // if not inside maybe it is a striaght polyline
        if ((isInside == false) && !closed) {
            isInside = nearLine(_x, _y, 10);
        }

        return isInside;
    }

    /**
     * Forms the convexHull based on the set of points that defines this contour. The contour can be either be clockwise
     * or counter-clockwise.
     */
    public void convexHull() {
        int i, j, k;
        int length, start;
        float vAx, vAy, vBx, vBy, crossProd;
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

        for (k = start; k >= 3; k -= 2) {
            flag = true;

            while ((flag == true) && (size() > k)) {
                flag = false;

                for (i = 0; i < (length - (k - 1)); i++) {

                    // Form two vectors
                    vAx = ((Vector3f) (elementAt(i + (k / 2)))).X - ((Vector3f) (elementAt(i))).X;
                    vAy = ((Vector3f) (elementAt(i + (k / 2)))).Y - ((Vector3f) (elementAt(i))).Y;
                    vBx = ((Vector3f) (elementAt(i + (k - 1)))).X - ((Vector3f) (elementAt(i))).X;
                    vBy = ((Vector3f) (elementAt(i + (k - 1)))).Y - ((Vector3f) (elementAt(i))).Y;

                    // calc cross product
                    crossProd = (vAx * vBy) - (vAy * vBx);

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
                tmpPt = (Vector3f) (elementAt(size() - 1));
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

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public VOIContour createInfarctionContour() {

        // Create a VOI that covers the entire infarction area
        // and traces along the inner contour (to close off the VOI)
        // first find all infarction points (first index of infarction, should
        // be shared w\ inner contour)
        VOIContour entireInfarction = new VOIContour(true);
        int index = -1;
        int firstSection = -1;
        CardioVector3f tempPt = null;
        boolean foundFirst = false;

        for (int i = 0; i < size(); i++) {
            tempPt = (CardioVector3f) elementAt(i);

            if (tempPt.getType() == INFARCTION) {

                if (!foundFirst) {
                    firstSection = tempPt.getSection();
                }

                foundFirst = true;
                entireInfarction.addElement((Vector3f) elementAt(i));
            }
        }

        // next close off the contour by tracing the inner contour
        int innerStartIndex = getIndex(INNER, firstSection, false, true);

        if (innerStartIndex == -1) {
            innerStartIndex = getIndex(INNER, firstSection, true, true);
        }

        // tempPt is the last element...
        int innerEndIndex = getIndex(INNER, tempPt.getSection(), false, true);

        if (innerEndIndex == -1) {
            innerEndIndex = getIndex(INNER, tempPt.getSection(), true, true);
        }

        // System.err.println("inner start index: " + innerStartIndex);
        // System.err.println("inner end index: " + innerEndIndex);
        // check to see if we crossed the section 0 line
        if (innerEndIndex < innerStartIndex) {

            // find the last inner index...
            for (int i = 0; i < size(); i++) {
                tempPt = (CardioVector3f) elementAt(i);

                if (tempPt.getType() == OUTER) {
                    index = i - 1;

                    break;
                }
            }

            for (int i = innerEndIndex; i >= 0; i--) {
                entireInfarction.addElement((Vector3f) elementAt(i));
            }

            for (int i = index; i >= innerStartIndex; i--) {
                entireInfarction.addElement((Vector3f) elementAt(i));
            }
        } else {

            for (int i = innerEndIndex; i >= innerStartIndex; i--) {
                entireInfarction.addElement((Vector3f) elementAt(i));
            }
        }

        return entireInfarction;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  direction  DOCUMENT ME!
     */
    public void cycleActivePt(int direction) { }

    /**
     * Draws itself with the interior blended with the image pasted in as a buffer. The color 0f the VOI and the opacity
     * of the VOI are parameters of this method.
     *
     * @param  zoomX        scale for the x coordinate
     * @param  zoomY        scale for the y coordinate
     * @param  resolutionX  x resolution (aspect ratio)
     * @param  resolutionY  y resolution (aspect ratio)
     * @param  g            graphics contexts to paint in
     * @param  xDim         x dimension maximum
     * @param  yDim         y dimension maximum
     * @param  pixBuffer    pixel buffer of image
     * @param  opacity      opaqueness of the VOI
     * @param  color        color of VOI
     */
    public void drawBlendSelf(float zoomX, float zoomY, float resolutionX, float resolutionY, Graphics g, int xDim,
                              int yDim, int[] pixBuffer, float opacity, Color color) {
        int pix, oldPix;
        float alphaPrime = 1 - opacity;
        int red, green, blue;
        int index;
        int x, y;
        int zoomXDim = Math.round(xDim * zoomX * resolutionX);

        if (g == null) {
            MipavUtil.displayError("VOIContour.drawBlendSelf: graphics = null");

            return;
        }

        // reset the contains functions - loads lastest points or VOI
        contains(0, 0, true);
        getBounds(xBounds, yBounds, zBounds);

        for (y = (int) (yBounds[0] * zoomY * resolutionY); y < (int) (yBounds[1] * zoomY * resolutionY); y++) {

            for (x = (int) (xBounds[0] * zoomX * resolutionX); x < (int) (xBounds[1] * zoomX * resolutionX); x++) {

                if (contains((int) (x / (zoomX * resolutionX)), (int) (y / (zoomY * resolutionY)), false)) {
                    index = (y * zoomXDim) + x;
                    oldPix = pixBuffer[index];
                    red = (oldPix & 0x00FF0000) >> 16;
                    green = (oldPix & 0x0000FF00) >> 8;
                    blue = (oldPix & 0x000000FF);
                    pix = (255 << 24) | (((int) ((red * alphaPrime) + (color.getRed() * opacity))) << 16) |
                              (((int) ((green * alphaPrime) + (color.getGreen() * opacity))) << 8) |
                              ((int) ((blue * alphaPrime) + (color.getBlue() * opacity)));
                    pixBuffer[index] = pix;
                }
            }
        }
    }

    /**
     * Draws geometric center of contour (2D).
     *
     * @param  scaleX       scale for the x coordinate
     * @param  scaleY       scale for the y coordinate
     * @param  resolutionX  X resolution (aspect ratio)
     * @param  resolutionY  Y resolution (aspect ratio)
     * @param  g            graphics to paint in
     */
    public void drawGeometricCenter(float scaleX, float scaleY, float resolutionX, float resolutionY, Graphics g) {
        int xS, yS;

        if (g == null) {
            MipavUtil.displayError("VOIContour.drawGeometricCenter: grapics = null");

            return;
        }

        getGeometricCenter();
        xS = Math.round(gcPt.X * scaleX * resolutionX);
        yS = Math.round(gcPt.Y * scaleY * resolutionY);
        g.drawLine(xS, yS - 3, xS, yS + 3);
        g.drawLine(xS - 3, yS, xS + 3, yS);

        if (label != null) {
            g.drawString(label, xS - 10, yS - 5);
        }
    }

    /**
     * Draws the length of open contour (Polyline).
     *
     * @param  g               graphics to draw in
     * @param  zoomX           magnification for the x coordinate
     * @param  zoomY           magnification for the y coordinate
     * @param  unitsOfMeasure  units of measure to be displayed on line.
     * @param  res             DOCUMENT ME!
     */
    public void drawLength(Graphics g, float zoomX, float zoomY, int[] unitsOfMeasure, float[] res) {
        double length;
        int i;
        String tmpString;

        if (g == null) {
            MipavUtil.displayError("VOILine drawTickMarks: grapics = null");

            return;
        }

        length = getLengthPtToPt(res);

        Vector3f pt = getGeometricCenter();

        // g.setColor(Color.yellow);
        tmpString = String.valueOf(length);
        i = tmpString.indexOf('.');

        if (tmpString.length() >= (i + 3)) {
            tmpString = tmpString.substring(0, i + 3);
        }

        switch (unitsOfMeasure[0]) {

            case FileInfoBase.INCHES:
                tmpString = tmpString + " in";
                break;

            case FileInfoBase.ANGSTROMS:
                tmpString = tmpString + " A";
                break;

            case FileInfoBase.NANOMETERS:
                tmpString = tmpString + " nm";
                break;

            case FileInfoBase.MICROMETERS:
                tmpString = tmpString + " um";
                break;

            case FileInfoBase.MILLIMETERS:
                tmpString = tmpString + " mm";
                break;

            case FileInfoBase.CENTIMETERS:
                tmpString = tmpString + " cm";
                break;

            case FileInfoBase.METERS:
                tmpString = tmpString + " m";
                break;

            case FileInfoBase.KILOMETERS:
                tmpString = tmpString + " km";
                break;

            case FileInfoBase.MILES:
                tmpString = tmpString + " miles";
                break;

            default:
                tmpString = "Unknown";
        }

        g.setColor(Color.black);
        g.drawString(tmpString, (int) (pt.X * zoomX), (int) ((pt.Y * zoomY) - 1));
        g.drawString(tmpString, (int) (pt.X * zoomX), (int) ((pt.Y * zoomY) + 1));
        g.drawString(tmpString, (int) ((pt.X * zoomX) + 1), (int) (pt.Y * zoomY));
        g.drawString(tmpString, (int) ((pt.X * zoomX) - 1), (int) (pt.Y * zoomY));
        g.setColor(Color.white);
        g.drawString(tmpString, (int) (pt.X * zoomX), (int) (pt.Y * zoomY));
    }

    /**
     * Contour draws itself.
     *
     * @param  zoomX           magnification for the x coordinate
     * @param  zoomY           magnification for the y coordinate
     * @param  resolutionX     X resolution (aspect ratio)
     * @param  resolutionY     Y resolution (aspect ratio)
     * @param  originX         Start location of X origin
     * @param  originY         Start location of Y origin
     * @param  resols          array of pixel resolutions
     * @param  unitsOfMeasure  e.g. mm for millimeters etc.
     * @param  orientation     the orientation of the image slice where the VOI is to be drawn
     * @param  g               graphics to paint in
     * @param  boundingBox     boolean that indicates if boundingBox is on or off
     * @param  thickness       DOCUMENT ME!
     */
    public void drawSelf(float zoomX, float zoomY, float resolutionX, float resolutionY, float originX, float originY,
                         float[] resols, int[] unitsOfMeasure, int orientation, Graphics g, boolean boundingBox,
                         int thickness) {
        Polygon gon = null;
        Polygon innerGon = null;
        Polygon outerGon = null;
        Polygon infarctionGon = null;
        int j;

        if (g == null) {
            MipavUtil.displayError("VOIContour.drawSelf: grapics = null");

            return;
        }

        // gon = scalePolygon( zoomX, zoomY, resolutionX, resolutionY );
        innerGon = scalePolygon(zoomX, zoomY, resolutionX, resolutionY, INNER, -1);
        outerGon = scalePolygon(zoomX, zoomY, resolutionX, resolutionY, OUTER, -1);

        for (int i = 0; i < numInfarctions; i++) {
            infarctionGons[i] = scalePolygon(zoomX, zoomY, resolutionX, resolutionY, INFARCTION, i);
        }

        // infarctionGon = scalePolygon(zoomX, zoomY, resolutionX, resolutionY,
        // INFARCTION, -1);
        // draw the section lines
        g.setColor(Color.gray);

        CardioVector3f firstPt = null;
        CardioVector3f secondPt = null;
        String outerLabel = new String();

        for (int i = 0; i < numSections; i++) {

            // connect to the inner VOI
            for (j = 0; j < size(); j++) {
                firstPt = (CardioVector3f) elementAt(j);

                if ((firstPt.getType() == INNER) && (firstPt.getSection() == i) && firstPt.isIntersection()) {
                    break;
                }
            }

            for (j = 0; j < size(); j++) {
                secondPt = (CardioVector3f) elementAt(j);

                if ((secondPt.getType() == OUTER) && (secondPt.getSection() == i) && secondPt.isIntersection()) {

                    // here we can draw the section numbers next to the outer
                    // intersection points
                    outerLabel = Integer.toString(i);
                    g.setColor(Color.cyan);
                    g.drawString(outerLabel, (int) (secondPt.X * zoomX * resolutionX),
                                 (int) (secondPt.Y * zoomY * resolutionY));

                    break;
                }
            }

            if (activeArray[secondPt.getSection()]) {
                g.setColor(Color.yellow);
            } else if (((i > 0) && activeArray[secondPt.getSection() - 1]) ||
                           ((i == 0) && activeArray[numSections - 1])) {
                g.setColor(Color.white);
            } else {
                g.setColor(Color.gray);
            }

            g.drawLine((int) Math.round(firstPt.X * zoomX * resolutionX),
                       (int) Math.round(firstPt.Y * zoomY * resolutionY),
                       (int) Math.round(secondPt.X * zoomX * resolutionX),
                       (int) Math.round(secondPt.Y * zoomY * resolutionY));

            if (showCenter) {
                g.setColor(Color.lightGray);
                g.drawLine((int) Math.round(centerPt.X * zoomX * resolutionX),
                           (int) Math.round(centerPt.Y * zoomY * resolutionY),
                           (int) Math.round(firstPt.X * zoomX * resolutionX),
                           (int) Math.round(firstPt.Y * zoomY * resolutionY));
            }
        }

        g.setColor(Color.CYAN);
        g.drawPolygon(innerGon);
        g.setColor(Color.RED);
        g.drawPolygon(outerGon);
        g.setColor(Color.GREEN);

        for (int i = 0; i < numInfarctions; i++) {
            g.drawPolyline(infarctionGons[i].xpoints, infarctionGons[i].ypoints, infarctionGons[i].npoints);
        }

        if (showPoints) {

            // highlight the points on the inner contour
            for (j = 0; j < innerGon.npoints; j++) {
                g.setColor(Color.white);
                g.fillRect((int) (innerGon.xpoints[j] - 1.5 + 0.5f), (int) (innerGon.ypoints[j] - 1.5 + 0.5f), 3, 3);
                g.setColor(Color.black);
                g.drawRect((int) (innerGon.xpoints[j] - 1.5 + 0.5f), (int) (innerGon.ypoints[j] - 1.5 + 0.5f), 3, 3);
            }

            // now outer
            for (j = 0; j < outerGon.npoints; j++) {
                g.setColor(Color.white);
                g.fillRect((int) (outerGon.xpoints[j] - 1.5 + 0.5f), (int) (outerGon.ypoints[j] - 1.5 + 0.5f), 3, 3);
                g.setColor(Color.black);
                g.drawRect((int) (outerGon.xpoints[j] - 1.5 + 0.5f), (int) (outerGon.ypoints[j] - 1.5 + 0.5f), 3, 3);
            }

            // last infarctions. don't draw the first and last points
            // of the infarction because these are mirrored in the inner contour
            // as intersection points
            for (int i = 0; i < numInfarctions; i++) {

                for (j = 1; j < (infarctionGons[i].npoints - 1); j++) {
                    g.setColor(Color.white);
                    g.fillRect((int) (infarctionGons[i].xpoints[j] - 1.5 + 0.5f),
                               (int) (infarctionGons[i].ypoints[j] - 1.5 + 0.5f), 3, 3);
                    g.setColor(Color.black);
                    g.drawRect((int) (infarctionGons[i].xpoints[j] - 1.5 + 0.5f),
                               (int) (infarctionGons[i].ypoints[j] - 1.5 + 0.5f), 3, 3);
                }
            }
        }
    }

    /**
     * Contour draws itself.
     *
     * @param  zoomX           magnification for the x coordinate
     * @param  zoomY           magnification for the y coordinate
     * @param  resolutionX     X resolution (aspect ratio)
     * @param  resolutionY     Y resolution (aspect ratio)
     * @param  originX         Start location of X origin
     * @param  originY         Start location of Y origin
     * @param  resols          array of pixel resolutions
     * @param  unitsOfMeasure  e.g. mm for millimeters etc.
     * @param  orientation     the orientation of the image slice where the VOI is to be drawn
     * @param  g               graphics to paint in
     * @param  boundingBox     boolean that indicates if boundingBox is on or off
     * @param  fileInfo        DOCUMENT ME!
     * @param  dim             DOCUMENT ME!
     * @param  thickness       DOCUMENT ME!
     */
    public void drawSelf(float zoomX, float zoomY, float resolutionX, float resolutionY, float originX, float originY,
                         float[] resols, int[] unitsOfMeasure, int orientation, Graphics g, boolean boundingBox,
                         FileInfoBase fileInfo, int dim, int thickness) { }

    /**
     * Draws the vertices of the contour.
     *
     * @param  zoomX        magnification for the x coordinate
     * @param  zoomY        magnification for the y coordinate
     * @param  resolutionX  X resolution (aspect ratio)
     * @param  resolutionY  Y resolution (aspect ratio)
     * @param  g            graphics to paint in
     * @param  boundingBox  boolean that indicates if boundingBox is on or off
     */
    public void drawVertices(float zoomX, float zoomY, float resolutionX, float resolutionY, Graphics g,
                             boolean boundingBox) {
        Polygon gon = null;
        int j;

        if (g == null) {
            MipavUtil.displayError("VOIContour.drawSelf: grapics = null");

            return;
        }

        gon = scalePolygon(zoomX, zoomY, resolutionX, resolutionY);

        // if active draw little boxes at points
        if (active == true) {

            // drawCenterOfMass(scaleX, scaleY, g);
            for (j = 0; j < size(); j++) {

                // g.setColor( Color.white );
                // g.fillRect( (int) ( gon.xpoints[j] - 1.5 + 0.5f ), (int) ( gon.ypoints[j] - 1.5 + 0.5f ), 3, 3 );
                if (nearPoint == j) { // Highlight Active point

                    // g.setColor( Color.white ); g.fillRect( (int) ( gon.xpoints[j] - 1.5 + 0.5f ), (int) (
                    // gon.ypoints[j] - 1.5 + 0.5f ), 3, 3 ); g.setColor( Color.black ); g.fillRect( (int) (
                    // gon.xpoints[j] - 1.5 + 0.5f ), (int) ( gon.ypoints[j] - 1.5 + 0.5f ), 4, 4 ); g.setColor(
                    // Color.white ); g.drawRect( (int) ( gon.xpoints[j] - 1.5 + 0.5f ), (int) ( gon.ypoints[j] - 1.5 +
                    // 0.5f ), 3, 3 );
                } else {
                    g.setColor(Color.white);
                    g.fillRect((int) (gon.xpoints[j] - 1.5 + 0.5f), (int) (gon.ypoints[j] - 1.5 + 0.5f), 3, 3);
                    g.setColor(Color.black);
                    g.drawRect((int) (gon.xpoints[j] - 1.5 + 0.5f), (int) (gon.ypoints[j] - 1.5 + 0.5f), 3, 3);
                }
            }

            g.setColor(Color.yellow);
            g.drawRect((int) (gon.xpoints[0] - 1.5 + 0.5f), (int) (gon.ypoints[0] - 1.5 + 0.5f), 3, 3);
        }

        if (boundingBox == true) {
            int x0, x1, y0, y1;
            x0 = (int) ((xBounds[0] * zoomX * resolutionX) + 0.5f);
            x1 = (int) ((xBounds[1] * zoomX * resolutionX) + 0.5f);
            y0 = (int) ((yBounds[0] * zoomY * resolutionY) + 0.5f);
            y1 = (int) ((yBounds[1] * zoomY * resolutionY) + 0.5f);
            g.setColor(Color.yellow.darker());
            g.drawRect(x0, y0, x1 - x0, y1 - y0);

            // draw corners of bounding box to make handles for resizing VOI
            g.fillRect(x0 - 2, y0 - 2, 5, 5);
            g.fillRect(x1 - 2, y0 - 2, 5, 5);
            g.fillRect(x0 - 2, y1 - 2, 5, 5);
            g.fillRect(x1 - 2, y1 - 2, 5, 5);

            // draw mid points of bounding box to make handles for resizing VOI
            g.fillRect(Math.round(x0 + ((x1 - x0) / 2) - 2), y0 - 2, 5, 5);
            g.fillRect(x1 - 2, Math.round(y0 + ((y1 - y0) / 2) - 2), 5, 5);
            g.fillRect(Math.round(x0 + ((x1 - x0) / 2) - 2), y1 - 2, 5, 5);
            g.fillRect(x0 - 2, Math.round(y0 + ((y1 - y0) / 2) - 2), 5, 5);
            g.setColor(Color.yellow.brighter());

            switch (nearBoundPoint) {

                case 1:
                    g.fillRect(x0 - 2, y0 - 2, 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(x0 - 2, y0 - 2, 4, 4);
                    break;

                case 2:
                    g.fillRect(x1 - 2, y0 - 2, 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(x1 - 2, y0 - 2, 4, 4);
                    break;

                case 3:
                    g.fillRect(x1 - 2, y1 - 2, 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(x1 - 2, y1 - 2, 4, 4);
                    break;

                case 4:
                    g.fillRect(x0 - 2, y1 - 2, 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(x0 - 2, y1 - 2, 4, 4);
                    break;

                case 5:
                    g.fillRect(Math.round(x0 + ((x1 - x0) / 2) - 2), y0 - 2, 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(Math.round(x0 + ((x1 - x0) / 2) - 2), y0 - 2, 4, 4);
                    break;

                case 6:
                    g.fillRect(x1 - 2, Math.round(y0 + ((y1 - y0) / 2) - 2), 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(x1 - 2, Math.round(y0 + ((y1 - y0) / 2) - 2), 4, 4);
                    break;

                case 7:
                    g.fillRect(Math.round(x0 + ((x1 - x0) / 2) - 2), y1 - 2, 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(Math.round(x0 + ((x1 - x0) / 2) - 2), y1 - 2, 4, 4);
                    break;

                case 8:
                    g.fillRect(x0 - 2, Math.round(y0 + ((y1 - y0) / 2) - 2), 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(x0 - 2, Math.round(y0 + ((y1 - y0) / 2) - 2), 4, 4);
                    break;
            }
        }
    }

    /**
     * Exports contour that has been transformed (Affine).
     *
     * @param   tMatrix  transformation matrix
     *
     * @return  returns the polygon
     */
    public VOIContour exportContour(TransMatrix tMatrix) {
        int i;
        VOIContour transformedContour = null;
        Vector3f pt = null;

        try {

            if (closed == true) {
                transformedContour = new VOIContour(true);
            } else {
                transformedContour = new VOIContour(false);
            }

            pt = new Vector3f();
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }

        for (i = 0; i < size(); i++) {
            tMatrix.transformAsPoint3Df((Vector3f) (elementAt(i)), pt);
            transformedContour.addElement(pt);
        }

        return transformedContour;
    }

    /**
     * Exports a transformed contour.
     *
     * @param   thetaX  rotation in x in degrees
     * @param   thetaY  rotation in y in degrees
     * @param   thetaZ  rotation in z in degrees
     * @param   tX      translation in x
     * @param   tY      translation in y
     * @param   tZ      translation in z
     * @param   scaleX  zoom in x
     * @param   scaleY  zoom in y
     * @param   scaleZ  zoom in z
     *
     * @return  returns Contour
     */
    public VOIContour exportContour(float thetaX, float thetaY, float thetaZ, float tX, float tY, float tZ,
                                    float scaleX, float scaleY, float scaleZ) {
        int i;
        VOIContour transformedContour = null;
        TransMatrix tMatrix = null;
        Vector3f pt = null;

        try {

            if (closed == true) {
                transformedContour = new VOIContour(true);
            } else {
                transformedContour = new VOIContour(false);
            }

            tMatrix = new TransMatrix(4);
            pt = new Vector3f();
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }

        getGeometricCenter();

        // construct transMatrix object
        tMatrix.setTranslate((gcPt.X + tX), (gcPt.Y + tY), (gcPt.Z + tZ));
        tMatrix.setRotate(thetaX, thetaY, thetaZ, TransMatrix.DEGREES);
        tMatrix.setZoom(scaleX, scaleY, scaleZ);
        tMatrix.setTranslate(-gcPt.X, -gcPt.Y, -gcPt.Z);

        for (i = 0; i < size(); i++) {
            tMatrix.transformAsPoint3Df((Vector3f) (elementAt(i)), pt);
            transformedContour.addElement(pt);
        }

        return transformedContour;
    }

    /**
     * Exports the points of the contour as a polygon (z data can not be encoded in the polygon).
     *
     * @return  returns polygon
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
            x = (int) (((Vector3f) (elementAt(i))).X + 0.5);
            y = (int) (((Vector3f) (elementAt(i))).Y + 0.5);
            gon.addPoint(x, y);
        }

        return gon;
    }

    /**
     * Exports polygon of the contour.
     *
     * @param   tMatrix  transformation matrix
     *
     * @return  returns the polygon
     */
    public Polygon exportPolygon(TransMatrix tMatrix) {
        int i;
        Vector3f pt = null;
        Polygon transformedGon = null;

        try {
            pt = new Vector3f();
            transformedGon = new Polygon();
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }

        for (i = 0; i < size(); i++) {
            tMatrix.transformAsPoint3Df((Vector3f) (elementAt(i)), pt);
            transformedGon.addPoint((int) (pt.X + 0.5), (int) (pt.Y + 0.5));
        }

        return transformedGon;
    }

    /**
     * Returns a scaled polygon of the contour.
     *
     * @param   scaleX       scale of polygon's x coordinates
     * @param   scaleY       scale of polygon's y coordinates
     * @param   resolutionX  pixel resolution int the x direction (aspect ratio)
     * @param   resolutionY  pixel resolution int the y direction (aspect ratio)
     *
     * @return  return the polygon
     */
    public Polygon exportPolygon(float scaleX, float scaleY, float resolutionX, float resolutionY) {
        return (scalePolygon(scaleX, scaleY, resolutionX, resolutionY));
    }

    /**
     * Exports the polygon of the contour with some transformation.
     *
     * @param   thetaX  rotation in x in degrees
     * @param   thetaY  rotation in y in degrees
     * @param   thetaZ  rotation in z in degrees
     * @param   tX      translation in x
     * @param   tY      translation in y
     * @param   tZ      translation in z
     * @param   scaleX  zoom in x
     * @param   scaleY  zoom in y
     * @param   scaleZ  zoom in z
     *
     * @return  returns polygon
     */
    public Polygon exportPolygon(float thetaX, float thetaY, float thetaZ, float tX, float tY, float tZ, float scaleX,
                                 float scaleY, float scaleZ) {
        int i;
        Vector3f pt = null;
        Polygon transformedGon = null;
        TransMatrix tMatrix = null;

        // change so that I don't allocate every time ?
        try {
            pt = new Vector3f();
            transformedGon = new Polygon();
            tMatrix = new TransMatrix(4);
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }

        getGeometricCenter();

        // construct transMatrix object
        // check into order of translate
        tMatrix.setTranslate((gcPt.X + tX), (gcPt.Y + tY), (gcPt.Z + tZ));
        tMatrix.setRotate(thetaX, thetaY, thetaZ, TransMatrix.DEGREES);
        tMatrix.setZoom(scaleX, scaleY, scaleZ);
        tMatrix.setTranslate(-gcPt.X, -gcPt.Y, -gcPt.Z);

        for (i = 0; i < size(); i++) {
            tMatrix.transformAsPoint3Df((Vector3f) (elementAt(i)), pt);
            transformedGon.addPoint((int) (pt.X + 0.5), (int) (pt.Y + 0.5));
        }

        return transformedGon;
    }

    /**
     * Finds the position/intensity along the boundary of this contour VOI.
     *
     * @param   position     array that is filled with distances along the VOI (i.e. mm)
     * @param   intensity    the corresponding intensities along the line
     * @param   imageBuffer  image array in which VOIs and lines are found
     * @param   resolutions  image resolutions
     * @param   xDim         x-Dimension of image
     * @param   yDim         y-Dimension of image
     *
     * @return  the number of points in the position and intensity array that hava valid data.
     */
    public int findPositionAndIntensity(float[] position, float[] intensity, float[] imageBuffer, float[] resolutions,
                                        int xDim, int yDim) {
        int i, j, end, pt;
        int index, indexX = 0, indexY = 0;
        double myY, myX, yInc, xInc;
        double x0, x1, y0, y1;
        double totDistance = 0;
        double ptDistance = 0;
        double subPtDistance = 0;
        end = size() - 1;
        pt = 0;

        for (j = 0; j < end; j++) {
            x0 = ((Vector3f) (elementAt(j))).X;
            y0 = ((Vector3f) (elementAt(j))).Y;
            x1 = ((Vector3f) (elementAt(j + 1))).X;
            y1 = ((Vector3f) (elementAt(j + 1))).Y;
            ptDistance = MipavMath.length(x0, y0, x1, y1, resolutions);
            myX = x0;
            myY = y0;
            xInc = ((x1 - x0) * resolutions[0]) / (ptDistance * 2);
            yInc = ((y1 - y0) * resolutions[1]) / (ptDistance * 2);

            for (i = 0, subPtDistance = 0; subPtDistance < ptDistance; i++) {

                if ((indexX != Math.round(myX)) || (indexY != Math.round(myY))) {
                    indexY = (int) Math.round(myY);
                    indexX = (int) Math.round(myX);
                    index = (indexY * xDim) + indexX;
                    subPtDistance = MipavMath.length(x0, y0, myX, myY, resolutions);
                    position[pt] = (float) (totDistance + subPtDistance);
                    intensity[pt] = imageBuffer[index];
                    pt++;
                }

                myX = myX + xInc;
                myY = myY + yInc;
            }

            pt--;
            totDistance += ptDistance;
        }

        if (closed == true) {
            x0 = ((Vector3f) (elementAt(size() - 1))).X;
            y0 = ((Vector3f) (elementAt(size() - 1))).Y;
            x1 = ((Vector3f) (elementAt(0))).X;
            y1 = ((Vector3f) (elementAt(0))).Y;
            ptDistance = MipavMath.length(x0, y0, x1, y1, resolutions);
            myX = x0;
            myY = y0;
            xInc = ((x1 - x0) * resolutions[0]) / (ptDistance * 2);
            yInc = ((y1 - y0) * resolutions[1]) / (ptDistance * 2);

            for (subPtDistance = 0; subPtDistance < ptDistance;) {

                if ((indexX != Math.round(myX)) || (indexY != Math.round(myY))) {
                    indexY = (int) Math.round(myY);
                    indexX = (int) Math.round(myX);
                    index = (indexY * xDim) + indexX;
                    subPtDistance = MipavMath.length(x0, y0, myX, myY, resolutions);
                    position[pt] = (float) (totDistance + subPtDistance);
                    intensity[pt] = imageBuffer[index];
                    pt++;
                }

                myX = myX + xInc;
                myY = myY + yInc;
            }

            indexY = (int) y1;
            indexX = (int) x1;
            index = (indexY * xDim) + indexX;
            subPtDistance = MipavMath.length(x0, y0, x1, y1, resolutions);
            pt--;
            position[pt] = (float) (totDistance + subPtDistance);
            intensity[pt] = imageBuffer[index];
        }

        return pt + 1;
    }

    /**
     * Finds the position/intensity along the boundary of this contour VOI of an RGB image.
     *
     * @param   position     array that is filled with distances along the VOI (i.e. mm)
     * @param   intensity    the corresponding intensities along the line
     * @param   RGorB        channel to calculate data
     * @param   imageBuffer  image array in which VOIs and lines are found
     * @param   resolutions  image resolutions
     * @param   xDim         x-Dimension of image
     * @param   yDim         y-Dimension of image
     *
     * @return  the number of points in the position and intensity array that hava valid data.
     */
    public int findPositionAndIntensityRGB(float[] position, float[] intensity, int RGorB, float[] imageBuffer,
                                           float[] resolutions, int xDim, int yDim) {
        int i, j, end, pt;
        int index, indexX = 0, indexY = 0;
        double myY, myX, yInc, xInc;
        double x0, x1, y0, y1;
        double totDistance = 0;
        double ptDistance = 0;
        double subPtDistance = 0;
        float val;
        end = size() - 1;
        pt = 0;

        for (j = 0; j < end; j++) {
            x0 = ((Vector3f) (elementAt(j))).X;
            y0 = ((Vector3f) (elementAt(j))).Y;
            x1 = ((Vector3f) (elementAt(j + 1))).X;
            y1 = ((Vector3f) (elementAt(j + 1))).Y;
            ptDistance = MipavMath.length(x0, y0, x1, y1, resolutions);
            myX = x0;
            myY = y0;
            xInc = ((x1 - x0) * resolutions[0]) / (ptDistance * 2);
            yInc = ((y1 - y0) * resolutions[1]) / (ptDistance * 2);

            for (i = 0, subPtDistance = 0; subPtDistance < ptDistance; i++) {

                if ((indexX != Math.round(myX)) || (indexY != Math.round(myY))) {
                    indexY = (int) Math.round(myY);
                    indexX = (int) Math.round(myX);
                    index = (indexY * xDim) + indexX;
                    ptDistance = MipavMath.length(x0, y0, myX, myY, resolutions);
                    position[pt] = (float) (totDistance + subPtDistance);

                    if (RGorB == 0) {
                        val = imageBuffer[(4 * index) + 1];
                    } // skip alpha and get Red info
                    else if (RGorB == 1) {
                        val = imageBuffer[(4 * index) + 2];
                    } else {
                        val = imageBuffer[(4 * index) + 3];
                    }

                    intensity[pt] = val;
                    pt++;
                }

                myX = myX + xInc;
                myY = myY + yInc;
            }

            pt--;
            totDistance += ptDistance;
        }

        if (closed == true) {
            x0 = ((Vector3f) (elementAt(size() - 1))).X;
            y0 = ((Vector3f) (elementAt(size() - 1))).Y;
            x1 = ((Vector3f) (elementAt(0))).X;
            y1 = ((Vector3f) (elementAt(0))).Y;
            ptDistance = MipavMath.length(x0, y0, x1, y1, resolutions);
            myX = x0;
            myY = y0;
            xInc = ((x1 - x0) * resolutions[0]) / (ptDistance * 2);
            yInc = ((y1 - y0) * resolutions[1]) / (ptDistance * 2);

            for (subPtDistance = 0; subPtDistance < ptDistance;) {

                if ((indexX != Math.round(myX)) || (indexY != Math.round(myY))) {
                    indexY = (int) Math.round(myY);
                    indexX = (int) Math.round(myX);
                    index = (indexY * xDim) + indexX;
                    ptDistance = MipavMath.length(x0, y0, myX, myY, resolutions);
                    position[pt] = (float) (totDistance + subPtDistance);

                    if (RGorB == 0) {
                        val = imageBuffer[(4 * index) + 1];
                    } // skip alpha and get Red info
                    else if (RGorB == 1) {
                        val = imageBuffer[(4 * index) + 2];
                    } else {
                        val = imageBuffer[(4 * index) + 3];
                    }

                    intensity[pt] = val;
                    pt++;
                }

                myX = myX + xInc;
                myY = myY + yInc;
            }

            indexY = (int) y1;
            indexX = (int) x1;
            index = (indexY * xDim) + indexX;
            ptDistance = MipavMath.length(x0, y0, x1, y1, resolutions);
            pt--;
            position[pt] = (float) (totDistance + subPtDistance);

            if (RGorB == 0) {
                val = imageBuffer[(4 * index) + 1];
            } // skip alpha and get Red info
            else if (RGorB == 1) {
                val = imageBuffer[(4 * index) + 2];
            } else {
                val = imageBuffer[(4 * index) + 3];
            }

            intensity[pt] = val;
        }

        return pt + 1;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean[] getActive() {
        return activeArray;
    }

    /**
     * Does nothing.
     *
     * @return  DOCUMENT ME!
     */
    public Vector3f getActivePt() {
        return null;
    }

    /**
     * Calculates the bounds of the contour.
     *
     * @param  x  two element array where x[0] = min extent of the Contour and x[1] = max extent of the Contour in the x
     *            dimension
     * @param  y  two element array where y[0] = min extent of the Contour and y[1] = max extent of the Contour in the y
     *            dimension
     * @param  z  two element array where z[0] = min extent of the Contour and z[1] = max extent of the Contour in the z
     *            dimension
     */
    public void getBounds(int[] x, int[] y, int[] z) {
        int i;
        int xx, yy, zz;
        x[0] = 10000000;
        x[1] = -10000000;
        y[0] = 10000000;
        y[1] = -10000000;
        z[0] = 10000000;
        z[1] = -10000000;

        for (i = 0; i < size(); i++) {
            xx = Math.round(((Vector3f) (elementAt(i))).X);
            yy = Math.round(((Vector3f) (elementAt(i))).Y);
            zz = Math.round(((Vector3f) (elementAt(i))).Z);

            if (xx < x[0]) {
                x[0] = xx;
            }

            if (xx > x[1]) {
                x[1] = xx;
            }

            if (yy < y[0]) {
                y[0] = yy;
            }

            if (yy > y[1]) {
                y[1] = yy;
            }

            if (zz < z[0]) {
                z[0] = zz;
            }

            if (zz > z[1]) {
                z[1] = zz;
            }
        }
    }

    /**
     * Calculates the bounds of the contour.
     *
     * @param  x  two element array where x[0] = min extent of the Contour and x[1] = max extent of the Contour in the x
     *            dimension
     * @param  y  two element array where y[0] = min extent of the Contour and y[1] = max extent of the Contour in the y
     *            dimension
     * @param  z  two element array where z[0] = min extent of the Contour and z[1] = max extent of the Contour in the z
     *            dimension
     */
    public void getBounds(float[] x, float[] y, float[] z) {
        int i;
        float xx, yy, zz;
        x[0] = 100000;
        x[1] = -1;
        y[0] = 100000;
        y[1] = -1;
        z[0] = 100000;
        z[1] = -1;

        for (i = 0; i < size(); i++) {
            xx = ((Vector3f) (elementAt(i))).X;
            yy = ((Vector3f) (elementAt(i))).Y;
            zz = ((Vector3f) (elementAt(i))).Z;

            if (xx < x[0]) {
                x[0] = xx;
            }

            if (xx > x[1]) {
                x[1] = xx;
            }

            if (yy < y[0]) {
                y[0] = yy;
            }

            if (yy > y[1]) {
                y[1] = yy;
            }

            if (zz < z[0]) {
                z[0] = zz;
            }

            if (zz > z[1]) {
                z[1] = zz;
            }
        }
    }

    /**
     * Gets the geometric center of the contour.
     *
     * @return  returns the geometric center
     */
    public Vector3f getGeometricCenter() {
        int nPts = 0;
        int x, y;
        contains(0, 0, true);
        getBounds(xBounds, yBounds, zBounds);

        int xbs = (int) xBounds[0];
        int xbe = (int) xBounds[1];
        int ybs = (int) yBounds[0];
        int ybe = (int) yBounds[1];
        double sumX = 0.0;
        double sumY = 0.0;

        for (y = ybs; y <= ybe; y++) {

            for (x = xbs; x <= xbe; x++) {

                if (contains(x, y, false)) {
                    nPts++;
                    sumX += x;
                    sumY += y;
                }
            }
        }

        gcPt.X = Math.round(sumX / nPts);
        gcPt.Y = Math.round(sumY / nPts);
        gcPt.Z = ((Vector3f) (elementAt(0))).Z;

        return gcPt;
    }

    /**
     * Gets the geometric center of the contour.
     *
     * @return  returns the geometric center
     */
    public Vector3f getFGeometricCenterOld() {
        int j;
        float sumX = (float) 0.0;
        float sumY = (float) 0.0;
        float sumZ = (float) 0.0;
        int nPts = 0;

        for (j = 0; j < size(); j++) {
            sumX += ((Vector3f) (elementAt(j))).X;
            sumY += ((Vector3f) (elementAt(j))).Y;
            sumZ += ((Vector3f) (elementAt(j))).Z;
            nPts++;
        }

        try {
            gcPt = new Vector3f(Math.round(sumX / nPts), Math.round(sumY / nPts), Math.round(sumZ / nPts));
        } catch (OutOfMemoryError error) {
            gcPt = null;
            System.gc();
        }

        return gcPt;
    }

    /**
     * Accessor that returns the number of points used in the most recent intensity calculation of this contour.
     *
     * @return  DOCUMENT ME!
     */
    public int getLastNumPixels() {
        return numPixels;
    }

    /**
     * Calculates the distance as a summation of distances from point to point.
     *
     * @param   resolutions  resolutions of the pixels ([0] = x resolution; [1] = y resolution)
     *
     * @return  the distance
     */
    public double getLengthPtToPt(float[] resolutions) {
        int i, end;
        double sum = 0;
        float x0, x1, y0, y1;
        end = size() - 1;

        for (i = 0; i < end; i++) {
            x0 = ((Vector3f) (elementAt(i))).X;
            y0 = ((Vector3f) (elementAt(i))).Y;

            // z0 = ((Vector3f)(elementAt(i))).Z;
            x1 = ((Vector3f) (elementAt(i + 1))).X;
            y1 = ((Vector3f) (elementAt(i + 1))).Y;

            // z1 = ((Vector3f)(elementAt(i+1))).Z;
            sum += getLength(x0, y0, x1, y1, resolutions);
        }

        if (closed == true) {
            x0 = ((Vector3f) (elementAt(0))).X;
            y0 = ((Vector3f) (elementAt(0))).Y;

            // z0 = ((Vector3f)(elementAt(0))).Z;
            x1 = ((Vector3f) (elementAt(size() - 1))).X;
            y1 = ((Vector3f) (elementAt(size() - 1))).Y;

            // z1 = ((Vector3f)(elementAt(size()-1))).Z;
            sum += getLength(x0, y0, x1, y1, resolutions);
        }

        return sum;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getNumSections() {
        return this.numSections;
    }

    /**
     * Return the VOI crop region's origin of rectangle.
     *
     * @param   fileInfo  DOCUMENT ME!
     * @param   dim       DOCUMENT ME!
     * @param   originX   DOCUMENT ME!
     * @param   originY   DOCUMENT ME!
     * @param   resols    DOCUMENT ME!
     *
     * @return  origin rectangle origin
     */
    public float[] getOrigin(FileInfoBase fileInfo, int dim, float originX, float originY, float[] resols) {

        // FileInfoBase fileInfo;
        // int nDims = imageActive.getNDims();
        int nDims = dim;
        DecimalFormat nf = new DecimalFormat("#####0.0##");
        int direction;
        float temp, left, right, interior, superior, posterior, anterior;
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

    /**
     * Gets position/intensity along the boundary of this contour VOI.
     *
     * @param   position     array that is filled with all x,y coordinates
     * @param   intensity    the corresponding intensities along the line
     * @param   imageBuffer  image array in which VOIs and lines are found
     * @param   xDim         x-Dimension of image
     *
     * @return  the number of points in the position and intensity array that hava valid data.
     */
    public int getPositionAndIntensity(Vector3f[] position, float[] intensity, float[] imageBuffer, int xDim) {
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
            x0 = ((Vector3f) (elementAt(j))).X;
            y0 = ((Vector3f) (elementAt(j))).Y;
            x1 = ((Vector3f) (elementAt(j + 1))).X;
            y1 = ((Vector3f) (elementAt(j + 1))).Y;
            distance = MipavMath.distance(x0, y0, x1, y1);
            myX = x0;
            myY = y0;
            xInc = (x1 - x0) / (distance * 2);
            yInc = (y1 - y0) / (distance * 2);
            len = (int) (2 * distance);

            for (i = 0; i < len; i++) {

                if ((indexX != Math.round(myX)) || (indexY != Math.round(myY))) {
                    indexY = (int) Math.round(myY);
                    indexX = (int) Math.round(myX);
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
            x0 = ((Vector3f) (elementAt(size() - 1))).X;
            y0 = ((Vector3f) (elementAt(size() - 1))).Y;
            x1 = ((Vector3f) (elementAt(0))).X;
            y1 = ((Vector3f) (elementAt(0))).Y;
            distance = MipavMath.distance(x0, y0, x1, y1);
            myX = x0;
            myY = y0;
            xInc = (x1 - x0) / (2 * distance);
            yInc = (y1 - y0) / (2 * distance);
            len = (int) Math.round(2 * distance);

            for (i = 0; i < len; i++) {

                if ((indexX != Math.round(myX)) || (indexY != Math.round(myY))) {
                    indexY = (int) Math.round(myY);
                    indexX = (int) Math.round(myX);
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
     * @param   position     array that is filled with all the indices in the form of indexY*xDim + indexX
     * @param   intensity    the corresponding intensities along the line
     * @param   imageBuffer  image array in which the VOis and lines are found
     * @param   xDim         x-Dimension of image
     *
     * @return  the number of points in the position and intensity array that hava valid data.
     */
    public int getPositionAndIntensityIndex(int[] position, float[] intensity, float[] imageBuffer, int xDim) {
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
            x0 = ((Vector3f) (elementAt(j))).X;
            y0 = ((Vector3f) (elementAt(j))).Y;
            x1 = ((Vector3f) (elementAt(j + 1))).X;
            y1 = ((Vector3f) (elementAt(j + 1))).Y;
            distance = MipavMath.distance(x0, y0, x1, y1);
            myX = x0;
            myY = y0;
            xInc = (x1 - x0) / (2 * distance);
            yInc = (y1 - y0) / (2 * distance);
            len = (int) (2 * distance);

            for (i = 0; i < len; i++) {

                if ((indexX != Math.round(myX)) || (indexY != Math.round(myY))) {
                    indexY = (int) Math.round(myY);
                    indexX = (int) Math.round(myX);
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
            x0 = ((Vector3f) (elementAt(size() - 1))).X;
            y0 = ((Vector3f) (elementAt(size() - 1))).Y;
            x1 = ((Vector3f) (elementAt(0))).X;
            y1 = ((Vector3f) (elementAt(0))).Y;
            distance = MipavMath.distance(x0, y0, x1, y1);
            myX = x0;
            myY = y0;
            xInc = (x1 - x0) / (2 * distance);
            yInc = (y1 - y0) / (2 * distance);
            len = (int) Math.round(distance);

            for (i = 0; i < len; i++) {

                if ((indexX != Math.round(myX)) || (indexY != Math.round(myY))) {
                    indexY = (int) Math.round(myY);
                    indexX = (int) Math.round(myX);
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
     * DOCUMENT ME!
     *
     * @param   x  DOCUMENT ME!
     * @param   y  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getSection(int x, int y) {

        for (int i = 0; i < numSections; i++) {

            if (contains(x, y, i)) {
                return i;
            }
        }

        return -1;
    }

    /**
     * Import points into the contour.
     *
     * @param  x  array of x points
     * @param  y  array of y points
     * @param  z  array of z points
     * @param  n  number of points in the array
     */
    public void importArrays(int[] x, int[] y, int[] z, int n) {
        int i;

        try {
            this.removeAllElements();

            for (i = 0; i < n; i++) {
                this.addElement(new Vector3f(x[i], y[i], z[i]));
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }
    }

    /**
     * Import points into the contour.
     *
     * @param  x  array of x points
     * @param  y  array of y points
     * @param  z  array of z points
     * @param  n  number of points in the array
     */
    public void importArrays(float[] x, float[] y, float[] z, int n) {
        int i;

        try {
            this.removeAllElements();

            for (i = 0; i < n; i++) {
                this.addElement(new Vector3f(x[i], y[i], z[i]));
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }
    }

    /**
     * Import points into the contour.
     *
     * @param  pt  array of three dimensional points
     */
    public void importPoints(Vector3f[] pt) {
        int i;
        this.removeAllElements();

        for (i = 0; i < pt.length; i++) {
            this.addElement(pt[i]);
        }
    }

    /**
     * Saves the polygon in the contour.
     *
     * @param  gon    polygon to be saved
     * @param  slice  index to save the polygon at
     */
    public void importPolygon(Polygon gon, int slice) {
        int i;

        try {
            this.removeAllElements();

            for (i = 0; i < gon.npoints; i++) {
                this.addElement(new Vector3f(gon.xpoints[i], gon.ypoints[i], slice));
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  x  DOCUMENT ME!
     * @param  y  DOCUMENT ME!
     * @param  z  DOCUMENT ME!
     */
    public void insertCardioPoint(int x, int y, int z) {

        if ((nearPoint >= 0) && (nearPoint < size())) {
            int newPtType = ((CardioVector3f) elementAt(nearPoint)).getType();

            // if the nearPoint Type is BOTH, change the new type to INNER
            // because we never return a near point type of a BOTH that
            // was an infarction point
            if (newPtType == BOTH) {
                newPtType = INNER;
            }

            int section = ((CardioVector3f) elementAt(nearPoint)).getSection();
            insertElementAt(new CardioVector3f(x, y, z, newPtType, section, false, false), nearPoint + 1);
            nearPoint++;
            nearPoint = NOT_A_POINT;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   index  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean isActive(int index) {
        return activeArray[index];
    }

    /**
     * Flag used to indicate type of contour: true = closed contour (i.e. contour end points are connected) false = open
     * contour
     *
     * @return  flag indicating open or closed.
     */
    public boolean isClosed() {
        return closed;
    }

    /**
     * Determines if the points are ordered in a counterClockwise manner.
     *
     * @return  true if points are counter clockwise and false if clockwise.
     */
    public boolean isCounterClockwise() {
        int length = size();
        int k = 0;
        int i;
        int prev, next;
        float vAx, vAy, vBx, vBy, crossProd;

        for (i = 1; i < length; i++) {

            if ((((Vector3f) (elementAt(i))).X <= ((Vector3f) (elementAt(k))).X) &&
                    ((((Vector3f) (elementAt(i))).X < ((Vector3f) (elementAt(k))).X) ||
                         (((Vector3f) (elementAt(i))).Y < ((Vector3f) (elementAt(k))).Y))) {
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
        vAx = ((Vector3f) (elementAt(k))).X - ((Vector3f) (elementAt(prev))).X;
        vAy = ((Vector3f) (elementAt(k))).Y - ((Vector3f) (elementAt(prev))).Y;
        vBx = ((Vector3f) (elementAt(next))).X - ((Vector3f) (elementAt(prev))).X;
        vBy = ((Vector3f) (elementAt(next))).Y - ((Vector3f) (elementAt(prev))).Y;

        // calc cross product 2*area if CCW or -2*area if CW
        crossProd = (vAx * vBy) - (vAy * vBx);

        if (crossProd > 0) {
            return false;
        } else {
            return true;
        }
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
     * Checks to see if Contour is counter-clockwise. If not it makes the contour counter-clockwise.
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
     * Unchanged.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseMoved(MouseEvent mouseEvent) { }

    /**
     * Does nothing.
     *
     * @param  dir   int
     * @param  xdim  int
     * @param  ydim  int
     */
    public void moveActivePt(int dir, int xdim, int ydim) { }

    /**
     * Moves a point on the contour.
     *
     * @param  x  x coordinate
     * @param  y  y coordinate
     */
    public void movePt(int x, int y) {

        // see if the point is an intersection point (OUTER/INNER)
        CardioVector3f currentPt = (CardioVector3f) elementAt(nearPoint);
        int section = currentPt.getSection();
        boolean isIntersection = currentPt.isIntersection();
        boolean isShared = currentPt.isShared();

        if (isIntersection) {

            // get the angle
            double angle = ((2.0 * Math.PI) / (double) numSections) * (double) section;
            double slope = Math.tan(angle);

            if ((slope == Double.NaN) || (slope > BIG_NUMBER)) {
                replaceElement(centerPt.X, y, ((Vector3f) (elementAt(nearPoint))).Z);

                return;
            } else if ((slope == 0) || ((slope < SMALL_NUMBER) && (slope > -(SMALL_NUMBER)))) {
                replaceElement(x, centerPt.Y, ((Vector3f) (elementAt(nearPoint))).Z);

                return;
            }

            int newY = (int) ((slope * (x - centerPt.X)) + centerPt.Y);
            replaceElement(x, (int) newY, ((Vector3f) (elementAt(nearPoint))).Z);
        } else if (isShared) {

            // find the shared infarction point with the same x, y values
            CardioVector3f infarctionPt = null;
            int infarctionIndex = -1;

            for (int i = 0; i < size(); i++) {
                infarctionPt = (CardioVector3f) elementAt(i);

                if ((infarctionPt.getType() == INFARCTION) && (infarctionPt.getSection() == section) &&
                        infarctionPt.isShared() &&
                        ((int) infarctionPt.X == (int) ((CardioVector3f) elementAt(nearPoint)).X) &&
                        ((int) infarctionPt.Y == (int) ((CardioVector3f) elementAt(nearPoint)).Y)) {
                    infarctionIndex = i;

                    break;
                }
            }

            if (infarctionIndex == -1) {
                System.err.println("bad error, couldn't find matching point");
            } else {
                ((Vector3f) (elementAt(infarctionIndex))).X = x;
                ((Vector3f) (elementAt(infarctionIndex))).Y = y;
            }

            replaceElement(x, y, ((Vector3f) (elementAt(nearPoint))).Z);
        } else {

            // POINT is not an intersection point
            replaceElement(x, y, ((Vector3f) (elementAt(nearPoint))).Z);
        }
    }

    /**
     * Tests if a point is near a vertex of the bounding box.
     *
     * @param   x            x coordinate of point with zoom factor applied
     * @param   y            y coordinate of point with zoom factor applied
     * @param   zoom         magnification of image
     * @param   resolutionX  X resolution (aspect ratio)
     * @param   resolutionY  Y resolution (aspect ratio)
     *
     * @return  returns boolean result of test
     *
     *          <p>1----5----2 | | 8 6 | | 4----7----3</p>
     */
    public int nearBoundPoint(int x, int y, float zoom, float resolutionX, float resolutionY) {
        int x0, x1, y0, y1;
        float dist, minDistance = 100000;
        x0 = Math.round(xBounds[0] * zoom * resolutionX);
        x1 = Math.round(xBounds[1] * zoom * resolutionX);
        y0 = Math.round(yBounds[0] * zoom * resolutionY);
        y1 = Math.round(yBounds[1] * zoom * resolutionY);
        nearBoundPoint = NOT_A_POINT;
        dist = (float) MipavMath.distance(x, x0, y, y0);

        if ((dist < 3) && (dist < minDistance)) {
            minDistance = dist;
            nearBoundPoint = 1;
        }

        dist = (float) MipavMath.distance(x, x1, y, y0);

        if ((dist < 3) && (dist < minDistance)) {
            minDistance = dist;
            nearBoundPoint = 2;
        }

        dist = (float) MipavMath.distance(x, x1, y, y1);

        if ((dist < 3) && (dist < minDistance)) {
            minDistance = dist;
            nearBoundPoint = 3;
        }

        dist = (float) MipavMath.distance(x, x0, y, y1);

        if ((dist < 3) && (dist < minDistance)) {
            minDistance = dist;
            nearBoundPoint = 4;
        }

        dist = (float) MipavMath.distance(x, x0 + ((x1 - x0) / 2), y, y0);

        if ((dist < 3) && (dist < minDistance)) {
            minDistance = dist;
            nearBoundPoint = 5;
        }

        dist = (float) MipavMath.distance(x, x1, y, y0 + ((y1 - y0) / 2));

        if ((dist < 3) && (dist < minDistance)) {
            minDistance = dist;
            nearBoundPoint = 6;
        }

        dist = (float) MipavMath.distance(x, x0 + ((x1 - x0) / 2), y, y1);

        if ((dist < 3) && (dist < minDistance)) {
            minDistance = dist;
            nearBoundPoint = 7;
        }

        dist = (float) MipavMath.distance(x, x0, y, y0 + ((y1 - y0) / 2));

        if ((dist < 3) && (dist < minDistance)) {
            minDistance = dist;
            nearBoundPoint = 8;
        }

        return nearBoundPoint;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   x  DOCUMENT ME!
     * @param   y  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean nearCardioLine(int x, int y) {
        return (nearCardioLine(x, y, INNER) || nearCardioLine(x, y, OUTER) || nearCardioLine(x, y, INFARCTION));
    }

    /**
     * DOCUMENT ME!
     *
     * @param   x            DOCUMENT ME!
     * @param   y            DOCUMENT ME!
     * @param   zoom         DOCUMENT ME!
     * @param   resolutionX  DOCUMENT ME!
     * @param   resolutionY  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int nearCardioPoint(int x, int y, float zoom, float resolutionX, float resolutionY) {
        int i;
        int xC, yC;
        float dist;
        nearPoint = NOT_A_POINT;

        CardioVector3f pt = null;

        for (i = 0; i < size(); i++) {
            pt = (CardioVector3f) elementAt(i);
            xC = Math.round(pt.X * zoom * resolutionX);
            yC = Math.round(pt.Y * zoom * resolutionY);
            dist = (float) MipavMath.distance(x, xC, y, yC);

            // don't return a near point for an infarction with index BOTH
            // because this point is already mirrored in the inner contour
            if ((dist < 3) && !((pt.getType() == INFARCTION) && pt.isShared())) {
                nearPoint = i;

                break;
            }
        }

        if (nearPoint == NOT_A_POINT) {
            return NOT_A_POINT;
        } else {

            // if the point is an intersection point, return the section number
            if (((CardioVector3f) elementAt(nearPoint)).isIntersection() ||
                    ((CardioVector3f) elementAt(nearPoint)).isShared()) {
                return ((CardioVector3f) elementAt(nearPoint)).getSection();
            }
            // otherwise return -1... means near a point, but not near an intersection point
            else {
                return NEAR_A_POINT;
            }
        }
    }

    /**
     * Resets the index and flag used in the retraceContour mode. It should be called after retracing a Contour.
     */
    public void resetIndex() {
        indexRetrace = NOT_A_POINT;
        flagRetrace = false;
    }

    /**
     * Redraws contour. Must move cursor in clockwise motion to work best
     *
     * @param  zoomX        zoom for the x coordinate
     * @param  zoomY        zoom for the y coordinate
     * @param  resolutionX  resolution of the pixel in the X direction (aspect ratio)
     * @param  resolutionY  resolution of the pixel in the Y direction (aspect ratio)
     * @param  resols       array of pixel resolutions
     * @param  x1           x coordinate of new point to add
     * @param  y1           y coordinateof new point to add
     * @param  g            graphics to paint in
     * @param  thickness    DOCUMENT ME!
     */
    public void retraceContour(float zoomX, float zoomY, float resolutionX, float resolutionY, float[] resols, int x1,
                               int y1, Graphics g, int thickness) {
        Vector3f ptRetrace = null;
        double minDistance, dist;
        float x2, y2, z;
        int i, j, idx;
        int newIndex, end;
        int[] units = new int[3];

        try {

            if (g == null) {
                MipavUtil.displayError("VOIContour.retraceContour: grapics = null");

                return;
            }

            if (indexRetrace == -99) {
                makeCounterClockwise();

                if (closed == true) {
                    oldContour = new VOIContour(true);
                } else {
                    oldContour = new VOIContour(false);
                }

                // oldContour = new VOIContour();
                for (i = 0; i < size(); i++) { // Make copy and save into oldContour
                    oldContour.addElement(this.elementAt(i));
                }
            }

            // Return if trying to add the same point.
            z = ((Vector3f) (elementAt(0))).Z;

            if (contains(new Vector3f(x1, y1, z))) {
                return;
            }

            g.setColor(black);
            g.setXORMode(red);
            active = false;
            units[0] = 0;
            units[1] = 0;
            units[2] = 0;
            drawSelf(zoomX, zoomY, resolutionX, resolutionY, 0, 0, resols, units, 0, g, false, thickness);

            // Find nearest point in old contour
            minDistance = 9999999;
            end = oldContour.size();

            for (i = 0; i < end; i++) {
                x2 = ((Vector3f) (oldContour.elementAt(i))).X;
                y2 = ((Vector3f) (oldContour.elementAt(i))).Y;
                dist = MipavMath.distance(x1, x2, y1, y2);

                if (dist < minDistance) {
                    ptRetrace = (Vector3f) oldContour.elementAt(i);
                    minDistance = dist;
                }
            }

            // remove point(s) in contour
            newIndex = indexOf(ptRetrace);

            if ((newIndex >= 0) && (newIndex < size())) {

                if (indexRetrace >= newIndex) { // should not happen when indexRetrace == -99

                    for (j = 0; j < (indexRetrace - newIndex); j++) {
                        removeElementAt(newIndex);
                        flagRetrace = true;
                    }
                } else if ((flagRetrace == true) && (((newIndex - indexRetrace) / (float) size()) > 0.75f)) {

                    for (j = 0; j < indexRetrace; j++) {
                        removeElementAt(0);
                    }

                    int size = size();

                    for (j = 0; j < (size - newIndex); j++) {
                        ;
                        removeElementAt(size() - 1);
                    }
                }
            }

            // Find index where new point is to be added
            minDistance = 999999999;
            idx = 0;

            for (i = 0; i < size(); i++) {
                x2 = ((Vector3f) (elementAt(i))).X;
                y2 = ((Vector3f) (elementAt(i))).Y;
                dist = MipavMath.distance(x1, x2, y1, y2);

                if (dist < minDistance) {
                    idx = i;
                    minDistance = dist;
                }
            }

            if (indexRetrace != NOT_A_POINT) {
                insertElementAt(new Vector3f(x1, y1, z), idx);
                indexRetrace = idx;
            } else {
                indexRetrace = idx;
            }

            units[0] = 0;
            units[1] = 0;
            units[2] = 0;
            drawSelf(zoomX, zoomY, resolutionX, resolutionY, 0, 0, resols, units, 0, g, false, thickness);
            active = true;
        } catch (OutOfMemoryError error) {
            System.gc();

            return;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  index     DOCUMENT ME!
     * @param  isActive  DOCUMENT ME!
     */
    public void setActive(int index, boolean isActive) {

        if (index < numSections) {
            activeArray[index] = isActive;
        }
    }

    /**
     * Sets the bounds of entire VOI not just Contour.
     *
     * @param  x  x bounds of entire VOI
     * @param  y  y bounds of entire VOI
     * @param  z  z bounds of entire VOI
     */
    public void setBounds(float[] x, float[] y, float[] z) {
        xBounds = x;
        yBounds = y;
        zBounds = z;
    }

    /**
     * Flag used to indicate type of contour: true = closed contour (i.e. contour end points are connected) false = open
     * contour
     *
     * @param  flag  indicates if the contour is open or closed.
     */
    public void setClosed(boolean flag) {
        closed = flag;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  infCon  DOCUMENT ME!
     */
    public void setInfarctionContour(VOIContour infCon) { }

    /**
     * DOCUMENT ME!
     *
     * @param  num  DOCUMENT ME!
     */
    public void setNumInfarctions(int num) {
        this.numInfarctions = num;
        this.infarctionGons = new Polygon[num];
    }

    /**
     * DOCUMENT ME!
     */
    public void togglePoints() {
        this.showPoints = !showPoints;
    }

    /**
     * Transforms self.
     *
     * @param  tMatrix  transformation matrix
     * @param  doRound  if true round point coordinates to integers
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
            tMatrix.transformAsPoint3Df((Vector3f) (elementAt(i)), point);

            if (doRound) {
                setElementAt(new Vector3f(Math.round(point.X), Math.round(point.Y), Math.round(point.Z)), i);
            } else {
                setElementAt(new Vector3f(point.X, point.Y, point.Z), i);
            }
        }
    }

    /**
     * Tranforms self using a transformation matrix (Affine).
     *
     * @param  thetaX  rotation in x
     * @param  thetaY  rotation in y
     * @param  thetaZ  rotation in z
     * @param  tX      translation in x
     * @param  tY      translation in y
     * @param  tZ      translation in z
     * @param  scaleX  zoom in x
     * @param  scaleY  zoom in y
     * @param  scaleZ  zoom in z
     */
    public void transformContour(float thetaX, float thetaY, float thetaZ, float tX, float tY, float tZ, float scaleX,
                                 float scaleY, float scaleZ) {
        int i;
        Vector3f point = null;
        TransMatrix tMatrix = null;

        try {
            point = new Vector3f();
            tMatrix = new TransMatrix(4);
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }

        getGeometricCenter();

        // construct transMatrix object
        // tMatrix.translate((gcPt.X+tX)*scaleX, (gcPt.Y+tY)*scaleY, (gcPt.Z+tZ)*scaleZ);
        tMatrix.setTranslate((gcPt.X + tX), (gcPt.Y + tY), (gcPt.Z + tZ));
        tMatrix.setRotate(thetaX, thetaY, thetaZ, TransMatrix.DEGREES);
        tMatrix.setZoom(scaleX, scaleY, scaleZ);
        tMatrix.setTranslate(-gcPt.X, -gcPt.Y, -gcPt.Z);

        for (i = 0; i < size(); i++) {
            tMatrix.transformAsPoint3Df((Vector3f) (elementAt(i)), point);
            setElementAt(new Vector3f(point.X, point.Y, point.Z), i);
        }
    }

    /**
     * Translates a VOI by some amount in x and y.
     *
     * @param  xT  translate this amount in the x direction
     * @param  yT  translate this amount in the y direction
     */
    public void translate(float xT, float yT) {
        int i;

        for (i = 0; i < size(); i++) {
            ((Vector3f) (elementAt(i))).X += xT;
            ((Vector3f) (elementAt(i))).Y += yT;
        }
    }

    /**
     * Removes collinear points (or near collinear) in the contour. If the perpendicular distance from the middle point
     * to the line defined by the 1st and 3rd point is small than the middle point is removed.
     *
     * @param  constraint  factor that controls the number of points removed. A larger constraint removes more points
     *                     0.50 typical - removes most "almost/and collinear" points 0.10 - removes only "collinear"
     *                     points
     * @param  tFlag       if true, trim adjacient points
     */
    public void trimPoints(double constraint, boolean tFlag) {
        int i;
        float ax, ay, bx, by, cx, cy;
        boolean flag = true;
        int end;

        if (size() <= 5) {
            return;
        }

        if (tFlag == true) {
            end = size();

            for (i = 0; i < (end - 1); i++) {
                ax = ((Vector3f) (elementAt(i))).X;
                ay = ((Vector3f) (elementAt(i))).Y;
                bx = ((Vector3f) (elementAt(i + 1))).X;
                by = ((Vector3f) (elementAt(i + 1))).Y;

                if (MipavMath.distance(ax, bx, ay, by) <= 1.5) {
                    removeElementAt(i + 1); // remove adjacient points
                    end = size();
                    i = i - 1;
                }

                if (size() <= 5) {
                    return;
                }
            }
        }

        while (flag == true) {
            flag = false;
            end = size();

            if (size() <= 5) {
                return;
            }

            for (i = 0; i < (end - 2); i++) {
                ax = ((Vector3f) (elementAt(i))).X;
                ay = ((Vector3f) (elementAt(i))).Y;
                bx = ((Vector3f) (elementAt(i + 1))).X;
                by = ((Vector3f) (elementAt(i + 1))).Y;
                cx = ((Vector3f) (elementAt(i + 2))).X;
                cy = ((Vector3f) (elementAt(i + 2))).Y;

                if (testDistance(Math.round(bx), Math.round(ax), Math.round(cx), Math.round(by), Math.round(ay),
                                     Math.round(cy), constraint)) {
                    removeElementAt(i + 1);
                    end = size();
                    i = i - 1;
                    flag = true;
                }
            }
        }
    }

    /**
     * Calculates twice the area (cross product of two vectors) of a triangle given three points. This is a private
     * function only called by the function "contains".
     *
     * @param   ptA   first point of the triangle
     * @param   ptB   second point of the triangle
     * @param   ptCx  x-coordinate of the third point of the triangle
     * @param   ptCy  y-coordinate of the third point of the triangle
     *
     * @return  twice the area of the triangle if CCW or -2*area if CW
     */
    private float area2(Vector3f ptA, Vector3f ptB, float ptCx, float ptCy) {
        return (((ptA.X - ptCx) * (ptB.Y - ptCy)) - ((ptA.Y - ptCy) * (ptB.X - ptCx)));
    }

    /**
     * Calculates twice the area (cross product of two vectors) of a triangle given three points. This is a private
     * function only called by the function "contains".
     *
     * @param   ptAx  x-coordinate of the first point of the triangle
     * @param   ptAy  y-coordinate of the first point of the triangle
     * @param   ptBx  x-coordinate of the second point of the triangle
     * @param   ptBy  y-coordinate of the second point of the triangle
     * @param   ptCx  x-coordinate of the third point of the triangle
     * @param   ptCy  y-coordinate of the third point of the triangle
     *
     * @return  twice the area of the triangle if CCw or -2*area if CW
     */
    private float area2(float ptAx, float ptAy, float ptBx, float ptBy, float ptCx, float ptCy) {
        return (((ptAx - ptCx) * (ptBy - ptCy)) - ((ptAy - ptCy) * (ptBx - ptCx)));
    }

    /**
     * DOCUMENT ME!
     *
     * @param   x        DOCUMENT ME!
     * @param   y        DOCUMENT ME!
     * @param   section  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean contains(int x, int y, int section) {
        boolean isInside = false;
        int numPoints = 0;
        int i, j, k;
        int outerIndex1 = -1;
        int outerIndex2 = -1;

        // find the first intersection point on the OUTER
        boolean foundFirst = false;
        CardioVector3f currentPt = null;

        if (section < (numSections - 1)) {

            for (k = 0; k < size(); k++) {
                currentPt = (CardioVector3f) elementAt(k);

                if (!foundFirst && (currentPt.getType() == OUTER) && (currentPt.getSection() == section) &&
                        currentPt.isIntersection()) {
                    foundFirst = true;
                    outerIndex1 = k;
                } else if (foundFirst && (currentPt.getType() == OUTER) && (currentPt.getSection() == (section + 1)) &&
                               currentPt.isIntersection()) {
                    outerIndex2 = k;

                    break;
                }
            }

            numPoints = outerIndex2 - outerIndex1 + 2;
            j = numPoints - 1;
            xPts = new float[numPoints];
            yPts = new float[numPoints];
            xPts[numPoints - 1] = centerPt.X;
            yPts[numPoints - 1] = centerPt.Y;

            for (k = 0; k < (numPoints - 1); k++) {
                xPts[k] = ((CardioVector3f) elementAt(outerIndex1 + k)).X;
                yPts[k] = ((CardioVector3f) elementAt(outerIndex1 + k)).Y;
            }

            for (i = 0; i < numPoints; i++) {

                if (((yPts[j] <= y) && (y < yPts[i]) && (area2(xPts[i], yPts[i], xPts[j], yPts[j], x, y) >= 0)) ||
                        ((yPts[i] <= y) && (y < yPts[j]) && (area2(xPts[j], yPts[j], xPts[i], yPts[i], x, y) >= 0))) {
                    isInside = !isInside;
                }

                j = i;
            }
        } else if (section == (numSections - 1)) {

            // find the section-0 intersection, the last point in the OUTER contour,
            // and the section-(numSections-1) intersection
            int sectionZeroPt = -1;
            int lastSectionPt = -1;
            int lastContourPt = -1;
            boolean foundLastSection = false;

            for (k = 0; k < size(); k++) {
                currentPt = (CardioVector3f) elementAt(k);

                if (!foundFirst && (currentPt.getType() == OUTER) && (currentPt.getSection() == 0) &&
                        currentPt.isIntersection()) {
                    foundFirst = true;
                    sectionZeroPt = k;
                } else if (foundFirst && !foundLastSection && (currentPt.getType() == OUTER) &&
                               (currentPt.getSection() == section) && currentPt.isIntersection()) {
                    foundLastSection = true;
                    lastSectionPt = k;
                } else if (foundLastSection && (currentPt.getType() != OUTER)) {
                    break;
                }
            }

            lastContourPt = k - 1;
            numPoints = lastContourPt - lastSectionPt + 3;
            j = numPoints - 1;
            xPts = new float[numPoints];
            yPts = new float[numPoints];
            xPts[numPoints - 1] = centerPt.X;
            yPts[numPoints - 1] = centerPt.Y;
            xPts[numPoints - 2] = ((CardioVector3f) elementAt(sectionZeroPt)).X;
            yPts[numPoints - 2] = ((CardioVector3f) elementAt(sectionZeroPt)).Y;

            for (k = 0; k < (numPoints - 2); k++) {
                xPts[k] = ((CardioVector3f) elementAt(lastSectionPt + k)).X;
                yPts[k] = ((CardioVector3f) elementAt(lastSectionPt + k)).Y;
            }

            for (i = 0; i < numPoints; i++) {

                if (((yPts[j] <= y) && (y < yPts[i]) && (area2(xPts[i], yPts[i], xPts[j], yPts[j], x, y) >= 0)) ||
                        ((yPts[i] <= y) && (y < yPts[j]) && (area2(xPts[j], yPts[j], xPts[i], yPts[i], x, y) >= 0))) {
                    isInside = !isInside;
                }

                j = i;
            }
        }

        return isInside;
    }

    /**
     * Bresenham's line drawing algorithm. If this function fails all points in the points vector are removed.
     *
     * @param  pts  storage of points of line
     * @param  x1   first x coordinate
     * @param  y1   first y coordinate
     * @param  x2   second x coordinate
     * @param  y2   second y coordinate
     */
    private void drawLine(Vector pts, int x1, int y1, int x2, int y2) {
        int d, xx, yy, ax, ay, sx, sy, dx, dy;

        try {
            dx = x2 - x1;
            ax = Math.abs(dx) << 1;

            if (dx < 0) {
                sx = -1;
            } else {
                sx = 1;
            }

            dy = y2 - y1;
            ay = Math.abs(dy) << 1;

            if (dy < 0) {
                sy = -1;
            } else {
                sy = 1;
            }

            xx = x1;
            yy = y1;

            if (ax > ay) {
                d = ay - (ax >> 1);

                for (;;) {

                    /* write to curve structure; */
                    pts.addElement(new Vector2f(xx, yy));

                    if (xx == x2) {
                        return;
                    }

                    if (d >= 0) {
                        yy += sy;
                        d -= ax;
                    }

                    xx += sx;
                    d += ay;
                }
            } else {
                d = ax - (ay >> 1);

                for (;;) {

                    /* write to curve structure; */
                    pts.addElement(new Vector2f(xx, yy));

                    if (yy == y2) {
                        return;
                    }

                    if (d >= 0) {
                        xx += sx;
                        d -= ay;
                    }

                    yy += sy;
                    d += ax;
                }
            }
        } catch (OutOfMemoryError error) {
            pts.removeAllElements();
            System.gc();
            throw error;
        }
    }

    /**
     * Gets the Index for the point with the given parameters.
     *
     * @param   type            int the type of point
     * @param   section         int the section number of the point
     * @param   isIntersection  boolean is it is an intersection
     * @param   isShared        boolean if the point is shared (inner and infarction)
     *
     * @return  int
     */
    private int getIndex(int type, int section, boolean isIntersection, boolean isShared) {
        int index = -1;
        CardioVector3f tempPt = null;

        for (int i = 0; i < size(); i++) {
            tempPt = (CardioVector3f) elementAt(i);

            if ((tempPt.getType() == type) && (tempPt.getSection() == section) &&
                    (tempPt.isIntersection() == isIntersection) && (tempPt.isShared() == isShared)) {
                return i;
            }
        }

        return index;
    }

    /**
     * Returns the real length of the line between (x[0], y[0]) and (x[1], y[1]) using pixel resolution information.
     *
     * @param   x0   array of the x coordinates
     * @param   y0   array of the y coordinates
     * @param   x1   DOCUMENT ME!
     * @param   y1   DOCUMENT ME!
     * @param   res  resolutions in each dimension
     *
     * @return  returns the length
     */
    private double getLength(float x0, float y0, float x1, float y1, float[] res) {
        double length;

        // length is the sqrt of the sum of the squares of
        // (x1-x0) times the x resolution
        // and (y1-y0) times the y resolution
        length = Math.sqrt(((x1 - x0) * (x1 - x0) * (res[0]) * (res[0])) +
                           ((y1 - y0) * (y1 - y0) * (res[1]) * (res[1])));

        return length;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   type             DOCUMENT ME!
     * @param   section          DOCUMENT ME!
     * @param   infarctionIndex  DOCUMENT ME!
     * @param   indices          DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean getSectionIndices(int type, int section, int infarctionIndex, int[] indices) {
        CardioVector3f currentPt = null;
        boolean foundFirst = false;
        int endSection = section + 1;

        if (section == (numSections - 1)) {
            endSection = 0;
        }

        if (type != INFARCTION) {

            for (int j = 0; j < size(); j++) {
                currentPt = (CardioVector3f) elementAt(j);

                if (currentPt.getType() == type) {

                    if ((currentPt.getSection() == section) && currentPt.isIntersection()) {
                        indices[0] = j;
                        indices[1] = j;
                        foundFirst = true;
                    } else if ((currentPt.getSection() == endSection) && currentPt.isIntersection()) {
                        indices[2] = j;
                    } else if (foundFirst && (endSection == 0) && (currentPt.getSection() == section)) {
                        indices[1]++;
                    }
                }
            }

            if (endSection != 0) {
                indices[1] = indices[2] - 1;
            }
        }
        // for infarction, must grab the indices that make the edge
        // of the contour for the index given. if no contour is found,
        // will return -1,-1,-1 for indices
        else {

            // System.err.println("looking for infarction index: " + infarctionIndex);
            int infarctionCounter = 0;
            int j = 0;
            foundFirst = false;

            boolean foundLast = false;

            for (int i = 0; i < size(); i++) {
                currentPt = (CardioVector3f) elementAt(i);

                if (currentPt.getType() == INFARCTION) {

                    if ((currentPt.getSection() == section) && (currentPt.isShared() || currentPt.isIntersection())) {

                        if ((infarctionIndex * 2) == infarctionCounter) {

                            // System.err.println("found START shared or intersection point of infarction: " + i);
                            indices[0] = i;
                            foundFirst = true;
                        } else if (((infarctionIndex * 2) + 1) == infarctionCounter) {

                            // System.err.println("found END shared or intersection point of infarction: " + i);
                            indices[2] = i;
                            foundLast = true;
                        }

                        infarctionCounter++;
                    } else if ((currentPt.getSection() == endSection) && currentPt.isIntersection()) {

                        if ((infarctionIndex * 2) == infarctionCounter) {

                            // System.err.println("found START intersection point of infarction: " + i);
                            indices[0] = i;
                            foundFirst = true;
                        } else if (((infarctionIndex * 2) + 1) == infarctionCounter) {

                            // System.err.println("found END intersection point of infarction: " + i);
                            indices[2] = i;
                            foundLast = true;
                        }

                        infarctionCounter++;
                    }
                }
            }

            if (!foundFirst || !foundLast) {
                return false;
            }

            System.err.println("found infarction: " + infarctionIndex);
            indices[1] = indices[2] - 1;
        }

        // System.err.println("Indices: " + indices[0] + "," + indices[1] + "," + indices[2]);
        return true;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   type      DOCUMENT ME!
     * @param   section   DOCUMENT ME!
     * @param   sharedPt  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int getSharedIndex(int type, int section, Vector3f sharedPt) {
        int index = -1;
        CardioVector3f tempPt = null;

        for (int i = 0; i < size(); i++) {
            tempPt = (CardioVector3f) elementAt(i);

            if (tempPt.isShared() && (tempPt.getType() == type) && (tempPt.X == sharedPt.X) &&
                    (tempPt.Y == sharedPt.Y)) {
                return i;
            }
        }

        return index;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   x     DOCUMENT ME!
     * @param   y     DOCUMENT ME!
     * @param   type  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean nearCardioLine(int x, int y, int type) {
        CardioVector3f firstPt = null;
        CardioVector3f secondPt = null;
        int x1, y1, x2, y2;
        int i;
        nearPoint = NOT_A_POINT;

        for (i = 0; i < size(); i++) {

            if (((CardioVector3f) elementAt(i)).getType() == type) {
                break;
            }
        }

        int index = i;
        firstPt = (CardioVector3f) elementAt(index);

        while (firstPt.getType() == type) {
            x1 = (int) firstPt.X;
            y1 = (int) firstPt.Y;

            if (((index + 1) < size()) && (((CardioVector3f) elementAt(index + 1)).getType() == type)) {
                secondPt = (CardioVector3f) elementAt(index + 1);
            } else {

                // the INFARCTION contour is not closed, so no more testing
                if (type == INFARCTION) {
                    return false;
                }

                secondPt = (CardioVector3f) elementAt(i);
            }

            x2 = (int) secondPt.X;
            y2 = (int) secondPt.Y;

            if (testDistance(x, x1, x2, y, y1, y2, 5)) {
                nearPoint = index;

                return true;
            }

            index++;

            if (index == size()) {
                break;
            }

            firstPt = (CardioVector3f) elementAt(index);
        }

        return false;
    }

    /**
     * Takes a scale and returns a scaled polygon.
     *
     * @param   zoomX        scale for the x coordinate
     * @param   zoomY        scale for the y coordinate
     * @param   resolutionX  resolution of the pixel in the X direction (aspect ratio)
     * @param   resolutionY  resolution of the pixel in the Y direction (aspect ratio)
     *
     * @return  returns scaled polygon
     */
    private Polygon scalePolygon(float zoomX, float zoomY, float resolutionX, float resolutionY) {
        int i;
        int x;
        int y;
        Polygon scaledGon = null;

        try {
            scaledGon = new Polygon();
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }

        for (i = 0; i < size(); i++) {
            x = (int) ((((CardioVector3f) (elementAt(i))).X * zoomX * resolutionX) + 0.5);
            y = (int) ((((CardioVector3f) (elementAt(i))).Y * zoomY * resolutionY) + 0.5);
            scaledGon.addPoint(x, y);
        }

        return scaledGon;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   zoomX          DOCUMENT ME!
     * @param   zoomY          DOCUMENT ME!
     * @param   resolutionX    DOCUMENT ME!
     * @param   resolutionY    DOCUMENT ME!
     * @param   type           DOCUMENT ME!
     * @param   infarctionNum  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private Polygon scalePolygon(float zoomX, float zoomY, float resolutionX, float resolutionY, int type,
                                 int infarctionNum) {
        int i;
        int x;
        int y;
        Polygon scaledGon = null;

        try {
            scaledGon = new Polygon();
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }

        CardioVector3f currentPt = null;
        int infarctionCounter = 0;

        for (i = 0; i < size(); i++) {
            currentPt = (CardioVector3f) elementAt(i);

            // for inner and outer contours, add normally
            if ((type != INFARCTION) && (type == currentPt.getType())) {
                x = (int) ((currentPt.X * zoomX * resolutionX) + 0.5);
                y = (int) ((currentPt.Y * zoomY * resolutionY) + 0.5);
                scaledGon.addPoint(x, y);

                break;
            }
            // for infarction contours, look for the correct infarction
            else if ((type == INFARCTION) && (currentPt.getType() == INFARCTION) && currentPt.isShared()) {

                if (infarctionCounter == (infarctionNum * 2)) {
                    x = (int) ((currentPt.X * zoomX * resolutionX) + 0.5);
                    y = (int) ((currentPt.Y * zoomY * resolutionY) + 0.5);
                    scaledGon.addPoint(x, y);

                    break;
                }

                infarctionCounter++;
            }
        }

        i++;

        if (i < size()) {
            currentPt = (CardioVector3f) elementAt(i);

            while (currentPt.getType() == type) {
                x = (int) ((currentPt.X * zoomX * resolutionX) + 0.5);
                y = (int) ((currentPt.Y * zoomY * resolutionY) + 0.5);
                scaledGon.addPoint(x, y);
                i++;

                if (i == size()) {
                    break;
                }

                currentPt = (CardioVector3f) elementAt(i);

                // if we are drawing the infarction counter, stop
                // when we get to the next intersection point
                if ((infarctionNum >= 0) && (type == INFARCTION) && currentPt.isShared()) {
                    x = (int) ((currentPt.X * zoomX * resolutionX) + 0.5);
                    y = (int) ((currentPt.Y * zoomY * resolutionY) + 0.5);
                    scaledGon.addPoint(x, y);

                    break;
                }
            }
        }

        return scaledGon;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public class CardioVector3f extends Vector3f {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -8047244892701374896L;

        /** DOCUMENT ME! */
        private boolean intersection = false;

        /** DOCUMENT ME! */
        private boolean isShared = false; // means the point is mirrored in both the inner and infarction

        /** DOCUMENT ME! */
        private int sectionNum;

        /** DOCUMENT ME! */
        private int type;

        /**
         * Creates a new CardioVector3f object.
         *
         * @param  x               DOCUMENT ME!
         * @param  y               DOCUMENT ME!
         * @param  z               DOCUMENT ME!
         * @param  type            DOCUMENT ME!
         * @param  sectionNum      DOCUMENT ME!
         * @param  isIntersection  DOCUMENT ME!
         * @param  isShared        DOCUMENT ME!
         */
        public CardioVector3f(float x, float y, float z, int type, int sectionNum, boolean isIntersection,
                              boolean isShared) {
            super(x, y, z);
            this.type = type;
            this.sectionNum = sectionNum;
            this.intersection = isIntersection;
            this.isShared = isShared;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public int getSection() {
            return this.sectionNum;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public int getType() {
            return this.type;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public boolean isIntersection() {
            return this.intersection;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public boolean isShared() {
            return this.isShared;
        }
    }
}
