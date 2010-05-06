package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.Polygon;
import java.io.*;
import java.util.LinkedList;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * 
 * The algorithm works as follows: I have implemented the Generalized Hough transform scheme originally sketched out by
 * Dana H. Ballard. At every point on a curve the gradient angle omega, perpindicular to the tangent and going into the
 * object, is found. Then, the distance r and angle beta of the line segment from the center of mass of the object to
 * the point on the curve is found. An R-table of gradient angle bins, linked to lists of all possible r, beta values is
 * created. In matching the object searching must occur over different scale factors and rotations of the object.
 * 
 * <pre>
 *  for (i = 0; i &lt; numPoints; i++) {
 *      Get gradient angle omega for point
 *      for (j = 0; j &lt;  thetaBins; j++) {
 *        rotation angle = theta[j]
 *        Get list of all r, beta values at gradient angle bin for omega - rotation angle
 *        for (k = 0; k &lt; size of r, beta list; k++) {
 *            Get r, beta values
 *            for (m = 0; m &lt; scaleBins; m++) {
 *                xCenter = x + r*scaleFactor*cos[beta - rotation angle];
 *                yCenter = y + r*scaleFactor*sin[beta - rotation angle];
 * </pre>
 * 
 * so a bin in 4D space of xCenter, yCenter, rotation angle, scaleFactor is incremented.
 * 
 * The method seems to work well if only either scale factor or rotation angle is used, but often produces wrong answers
 * if both scale factor and rotation angle are used. One reason is that in 3D space I can keep the bin width small
 * enough for a thorough search, but in 4D memory restrictions force me to increase bin width.
 * 
 * References: 1.) Computer Vision by Dana H. Ballard and Christopher M. Brown, Prentice-Hall, Inc., 1982, Section 4.3.4
 * Generalizing the Hough Transform, pp. 128-131.
 */
public class AlgorithmCreateRtable extends AlgorithmBase {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------
    // Number of bins covering gradient direction, normal to the tangent angle, going from 0 radians to 2*PI radians.
    // The gradient direction is defined as going into the object.
    private int binNumber = 90;

    // Name of file in which R-table is stored
    private String fileName;

    // Number of points to take from each side of a point on a curve in determining a tangent
    // If only 1 point is used on each side, simply use avarage of slopes to each of the
    // neigboring points.
    private int sidePointsForTangent;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmCreateRtable - default constructor.
     */
    public AlgorithmCreateRtable() {}

    /**
     * AlgorithmCreateRtable.
     * 
     * @param srcImg Binary source image that has contour VOI for R-table generation
     * @param binNumber Number of bins for gradient direction, normal to the tangent angle, going from 0 to 2*PI
     *            radians. The gradient direction is defined as going into the object.
     * @param sidePointsForTangent Number of points to take from each side of a point on a curve in determining the
     *            tangent
     * @param fileName Name of file to store R-table in
     */
    public AlgorithmCreateRtable(ModelImage srcImg, int binNumber, int sidePointsForTangent, String fileName) {
        super(null, srcImg);
        this.binNumber = binNumber;
        this.sidePointsForTangent = sidePointsForTangent;
        this.fileName = fileName;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * finalize -
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Starts the program. Uses curvature based on the linefit method, which is a very poor measurement curvature =
     * (difference in tangent angles/distance between tangent points) According to Marcel Worring and Arnold W. M.
     * Smeulders in "Digital Curvature Estimation", CVGIP: Image Understanding, Vol. 58, No. 3, November, 1993, pp.
     * 366-382: The Linefit method, "based on fitting two straight lines, has poor performance. Bias is -27% to +24%,
     * where deviation is 5.8%-68%. The errors are a direct consequence of the method of differential filtering and of
     * the arclength estimator used. Orientation estimation is reliable, however. Curvature estimates are improved by
     * using a Gaussian differential kernel and a better arclength estimator."
     * 
     * "Another source of inaccuracy is the fact that the estimated derivative is divided by the length between discrete
     * pixels. The distance between pixels is restricted to either 1 or sqrt(2), a poor estimate of predigitized
     * arclength."
     * 
     * Reference: "Hough transform using slope and curvature as local properties to detect arbitrary 2D shapes" by Siang
     * De Ma and Xing Chen, 9th International Conference on Pattern Recognition, November, 1988, pp. 511-513.
     */
    public void runCurvatureAlgorithm() {
        int x, y;
        int offset;

        int xDim;

        int yDim;

        int sourceSlice;

        int numPoints;

        int i, j, k, n;
        int index;

        byte[] srcBuffer;
        byte[] maskBuffer;
        boolean test = false;
        int indexArray[];
        int neighbors;
        boolean foundArray[];
        int indexPtr = 0;
        float tangentX;
        float tangentY;
        float normalX;
        float normalY;
        float xPoints[];
        float yPoints[];
        int endPtr;
        int startWrapPoints;
        int endWrapPoints;
        int neighbor1[];
        int neighbor2[];
        AlgorithmMorphology2D algoMorph2D;
        int pruningPix;
        boolean entireImage;
        double var;
        float xpc;
        float ypc;
        float lastxpc = 0.0f;
        float lastypc = 0.0f;
        float xpc0 = 0.0f;
        float ypc0 = 0.0f;
        double xSqSum;
        double ySqSum;
        double xySum;
        double x1t;
        double x2t;
        double y1t;
        double y2t;
        double d1;
        double d2;
        double slope;
        ModelImage maskImage = null;
        ViewJComponentEditImage componentImage;
        boolean useXOR = false;
        boolean onlyActive = true;
        int startPos = 0;
        ViewVOIVector VOIs;
        int nVOI;
        VOI selectedVOI = null;
        Vector3f center;
        double centerX;
        double centerY;
        double omega = 0.0;
        double lastOmega = 0.0;
        double omega0 = 0;
        double deltaOmega;
        double curveXDist;
        double curveYDist;
        double distance;
        float curvature;
        RandomAccessFile rOut;
        float omegaArray[];
        float curvatureArray[];
        VOI closedVOI;
        int xVal[];
        int yVal[];
        int zVal[];
        Polygon gon = null;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Create R-table ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sourceSlice = xDim * yDim;

        srcBuffer = new byte[sourceSlice];

        VOIs = srcImage.getVOIs();

        nVOI = VOIs.size();

        if (nVOI == 0) {
            MipavUtil.displayError("Must create a contour VOI");

            setCompleted(false);
            return;
        }

        int contourVOI = 0;
        int activeContourVOI = 0;

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                contourVOI++;

                if (VOIs.VOIAt(i).isActive() == true) {
                    selectedVOI = VOIs.VOIAt(i);
                    activeContourVOI++;
                }
            }
        }

        if (contourVOI == 0) {
            MipavUtil.displayError("Must create a contour VOI");
            setCompleted(false);
            return;
        }

        if ( (contourVOI > 1) && (activeContourVOI != 1)) {
            MipavUtil.displayError("VOI must be selected");
            setCompleted(false);
            return;
        }

        if (contourVOI == 1) {
            for (i = 0; i < nVOI; i++) {
                if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                    selectedVOI = VOIs.VOIAt(i);
                }
            }
        }

        center = selectedVOI.getGeometricCenter();
        centerX = center.X;
        centerY = center.Y;

        componentImage = srcImage.getParentFrame().getComponentImage();

        try {

            if (componentImage.getVOIHandler().getActiveVOICount() == 0) {
                componentImage.getVOIHandler().selectAllVOIs(true);
            }

            maskImage = srcImage.generateBinaryImage(useXOR, onlyActive);

            // if (maskImage != null) {
            // maskImage.setImageName(srcImage.getImageName() + "_bmask");
            // new ViewJFrameImage(maskImage);
            // }
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: unable to open new frame");

            if (maskImage != null) {
                maskImage.disposeLocal();
            }

            maskImage = null;

            return;
        }

        try {
            maskImage.exportData(0, sourceSlice, srcBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on maskImage.exportData");

            setCompleted(false);

            return;
        }

        // Keep a copy of all pixels in the selectedVOI;
        maskBuffer = new byte[sourceSlice];
        for (i = 0; i < sourceSlice; i++) {
            maskBuffer[i] = srcBuffer[i];
        }

        // Find all set pixels that have 4 set nearest neighbors
        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if (srcBuffer[index] != 0) {
                    neighbors = 0;
                    if (y > 0) {
                        if (srcBuffer[index - xDim] != 0) {
                            neighbors++;
                        }
                    } // if (y > 0)
                    if (x > 0) {
                        if (srcBuffer[index - 1] != 0) {
                            neighbors++;
                        }
                    } // if (x > 0)
                    if (x < xDim - 1) {
                        if (srcBuffer[index + 1] != 0) {
                            neighbors++;
                        }
                    } // if (x < xDim - 1)
                    if (y < yDim - 1) {
                        if (srcBuffer[index + xDim] != 0) {
                            neighbors++;
                        }
                    } // if (y < yDim - 1)
                    if (neighbors == 4) {
                        srcBuffer[index] = 2;
                    }
                } // if (srcBuffer[index] != 0)
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)

        // Clear all set pixels that have 4 set nearest neighbors
        for (i = 0; i < sourceSlice; i++) {
            if (srcBuffer[i] == 2) {
                srcBuffer[i] = 0;
            }
        }

        try {
            maskImage.importData(0, srcBuffer, true);
        } catch (IOException e) {
            MipavUtil.displayError("IOException on maskImage.importData");
        }

        // Binary image is already skeletonized, but dangling branches may be present
        // Prune off branches with 5 or less pixels
        pruningPix = 5;
        entireImage = true;
        algoMorph2D = new AlgorithmMorphology2D(maskImage, 0, 0.0f, AlgorithmMorphology2D.SKELETONIZE, 0, 0,
                pruningPix, 0, entireImage);
        algoMorph2D.run();
        algoMorph2D.finalize();

        try {
            maskImage.exportData(0, sourceSlice, srcBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on maskImage.exportData");

            setCompleted(false);

            return;
        }

        // Find the 2 neighbors of every point
        numPoints = 0;
        neighbor1 = new int[sourceSlice];
        neighbor2 = new int[sourceSlice];
        for (i = 0; i < sourceSlice; i++) {
            neighbor1[i] = -1;
            neighbor2[i] = -1;
        }
        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if (srcBuffer[index] != 0) {
                    neighbors = 0;
                    if (y > 0) {
                        if (x > 0) {
                            if (srcBuffer[index - xDim - 1] != 0) {
                                neighbors++;
                                neighbor1[index] = index - xDim - 1;
                            }
                        }
                        if (srcBuffer[index - xDim] != 0) {
                            neighbors++;
                            if (neighbor1[index] == -1) {
                                neighbor1[index] = index - xDim;
                            } else {
                                neighbor2[index] = index - xDim;
                            }
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index - xDim + 1] != 0) {
                                neighbors++;
                                if (neighbor1[index] == -1) {
                                    neighbor1[index] = index - xDim + 1;
                                } else {
                                    neighbor2[index] = index - xDim + 1;
                                }
                            }
                        }
                    } // if (y > 0)
                    if (x > 0) {
                        if (srcBuffer[index - 1] != 0) {
                            neighbors++;
                            if (neighbor1[index] == -1) {
                                neighbor1[index] = index - 1;
                            } else {
                                neighbor2[index] = index - 1;
                            }
                        }
                    } // if (x > 0)
                    if (x < xDim - 1) {
                        if (srcBuffer[index + 1] != 0) {
                            neighbors++;
                            if (neighbor1[index] == -1) {
                                neighbor1[index] = index + 1;
                            } else {
                                neighbor2[index] = index + 1;
                            }
                        }
                    } // if (x < xDim - 1)
                    if (y < yDim - 1) {
                        if (x > 0) {
                            if (srcBuffer[index + xDim - 1] != 0) {
                                neighbors++;
                                if (neighbor1[index] == -1) {
                                    neighbor1[index] = index + xDim - 1;
                                } else {
                                    neighbor2[index] = index + xDim - 1;
                                }
                            }
                        }
                        if (srcBuffer[index + xDim] != 0) {
                            neighbors++;
                            if (neighbor1[index] == -1) {
                                neighbor1[index] = index + xDim;
                            } else {
                                neighbor2[index] = index + xDim;
                            }
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index + xDim + 1] != 0) {
                                neighbors++;
                                if (neighbor1[index] == -1) {
                                    neighbor1[index] = index + xDim + 1;
                                } else {
                                    neighbor2[index] = index + xDim + 1;
                                }
                            }
                        }
                    } // if (y < yDim - 1)
                    // Delete isolated points - should not happen
                    if (neighbors == 0) {
                        srcBuffer[index] = 0;
                        neighbor1[index] = -1;
                        neighbor2[index] = -1;
                    } else {
                        numPoints++;
                    }
                } // if (srcBuffer[index] != 0)
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)
        ViewUserInterface.getReference().setDataText("Number of points on curve = " + numPoints + "\n");

        // Find a starting position
        loop1: for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if (srcBuffer[index] != 0) {
                    startPos = index;
                    break loop1;
                }
            }
        }

        // Find a starting position and length of the closed curve
        xPoints = new float[2 * sidePointsForTangent + 1];
        yPoints = new float[2 * sidePointsForTangent + 1];
        foundArray = new boolean[sourceSlice];
        indexArray = new int[numPoints];

        index = startPos;
        indexPtr = 0;
        foundArray[index] = true;
        indexArray[indexPtr++] = index;
        while ( ( !foundArray[neighbor1[index]]) || ( !foundArray[neighbor2[index]])) {
            if ( !foundArray[neighbor1[index]]) {
                index = neighbor1[index];
            } else {
                index = neighbor2[index];
            }
            foundArray[index] = true;
            indexArray[indexPtr++] = index;
        } // while ((!foundArray[neighbor1[index]]) || (!foundArray[neighbor2[index]]))
        xVal = new int[numPoints];
        yVal = new int[numPoints];
        zVal = new int[numPoints];
        for (i = 0; i < numPoints; i++) {
            xVal[i] = indexArray[i] % xDim;
            yVal[i] = indexArray[i] / xDim;
        }
        closedVOI = new VOI((short) 0, "VOIclosed", 1, VOI.CONTOUR, -1.0f);
        closedVOI.importCurve(xVal, yVal, zVal);
        if ( ! ((VOIContour) closedVOI.getCurves().elementAt(0)).isCounterClockwise()) {
            ((VOIContour) closedVOI.getCurves().elementAt(0)).makeCounterClockwise();
            gon = ((VOIContour) closedVOI.getCurves().elementAt(0)).exportPolygon();
            try {
                closedVOI.finalize();
            } catch (Throwable e) {
                ViewUserInterface.getReference().setDataText("Exception on closedVOI.finalize()\n");
            }
            closedVOI = null;
            indexPtr = 0;
            for (i = 0; i < numPoints; i++) {
                index = gon.xpoints[i] + xDim * gon.ypoints[i];
                indexArray[indexPtr++] = index;
            }
            gon = null;
        } // if (!((VOIContour)closedVOI.getCurves()[0].elementAt(0)).isCounterClockwise())
        endPtr = indexPtr - 1;
        omegaArray = new float[numPoints];
        curvatureArray = new float[numPoints];
        for (n = 0; n <= numPoints - 1; n++) {
            // Put the tangent point at index sidePointsForTangent in the
            // center of the xPoints and yPoints array with sidePointsForTangent points
            // to each side.
            startWrapPoints = Math.max(0, sidePointsForTangent - n);
            endWrapPoints = Math.max(0, sidePointsForTangent - (numPoints - 1 - n));
            for (k = 0; k < startWrapPoints; k++) {
                xPoints[k] = indexArray[endPtr - (startWrapPoints - k)] % xDim;
                yPoints[k] = indexArray[endPtr - (startWrapPoints - k)] / xDim;
            }
            for (k = startWrapPoints, j = n - sidePointsForTangent + startWrapPoints; k < 2 * sidePointsForTangent + 1
                    - endWrapPoints; j++, k++) {
                xPoints[k] = indexArray[j] % xDim;
                yPoints[k] = indexArray[j] / xDim;
            }
            for (j = 0, k = 2 * sidePointsForTangent + 1 - endWrapPoints; k < 2 * sidePointsForTangent + 1; j++, k++) {
                xPoints[k] = indexArray[j] % xDim;
                yPoints[k] = indexArray[j] / xDim;
            }
            // For the closed curve find the slope and y axis intercept of the tangent line to the curve at a point
            // With a user specified sidePointsForTangent on each side of a point find the tangent line that
            // minimizes the sum of the squared distances from these side points to the tangent line
            if (sidePointsForTangent == 1) {
                xpc = xPoints[1];
                ypc = yPoints[1];
                tangentX = (xPoints[2] - xPoints[0]) / 2.0f;
                tangentY = (yPoints[2] - yPoints[0]) / 2.0f;
                if (tangentX == 0) {
                    normalX = 1;
                    normalY = 0;
                } else if (tangentY == 0) {
                    normalX = 0;
                    normalY = 1;
                } else {
                    normalX = -tangentY / tangentX;
                    normalY = 1;
                }
            } // if (sidePointsForTangent == 1)
            else {
                // Center all points for tangent point touching curve at (0, 0)
                // That is, use an x axis and a y axis going thru the tangent point
                xpc = xPoints[sidePointsForTangent];
                ypc = yPoints[sidePointsForTangent];
                for (k = 0; k < xPoints.length; k++) {
                    xPoints[k] = xPoints[k] - xpc;
                    yPoints[k] = yPoints[k] - ypc;
                }
                xSqSum = 0.0;
                ySqSum = 0.0;
                xySum = 0.0;
                for (k = 0; k < xPoints.length; k++) {
                    xSqSum += xPoints[k] * xPoints[k];
                    ySqSum += yPoints[k] * yPoints[k];
                    xySum += xPoints[k] * yPoints[k];
                }
                if (xySum != 0.0) {
                    var = Math.sqrt(ySqSum * ySqSum - 2.0 * xSqSum * ySqSum + xSqSum * xSqSum + 4.0 * xySum * xySum);
                    x1t = 0.5 * ( ( -ySqSum + xSqSum + var) / xySum);
                    x2t = 0.5 * ( ( -ySqSum + xSqSum - var) / xySum);
                    y1t = 1.0;
                    y2t = 1.0;
                } else {
                    // If all points are symmetric to either this new x axis or this new y axis, then
                    // their product sum is 0 and the tangentX, tangentY must be 1,0 or 0,1
                    x1t = 1.0;
                    x2t = 0.0;
                    y1t = 0.0;
                    y2t = 1.0;
                }
                // x1t, y1t and x2t, y2t are perpindicular. To find the solution, calculate the sum of
                // distances from the curve points to the line for the 2 cases
                // The shortest distance is the correct solution
                // Distance from AX + BY + C = 0 to P1 is
                // abs((A*x1 + B*y1 + C))/sqrt(A**2 + B**2)
                // Here A = slope, B = -1, and C = 0.
                d1 = 0.0;
                for (k = 0; k < xPoints.length; k++) {
                    if (x1t == 0.0) {
                        // Infinite slope thru (0,0)
                        d1 += Math.abs(xPoints[k]);
                    } else if (y1t == 0.0) {
                        // Zero slope thru (0, 0)
                        d1 += Math.abs(yPoints[k]);
                    } else {
                        slope = y1t / x1t;
                        d1 += Math.abs( (slope * xPoints[k] - yPoints[k]) / Math.sqrt(slope * slope + 1));
                    }
                }
                d2 = 0.0;
                for (k = 0; k < xPoints.length; k++) {
                    if (x2t == 0.0) {
                        // Infinite slope thru (0,0)
                        d2 += Math.abs(xPoints[k]);
                    } else if (y2t == 0.0) {
                        // Zero slope thru (0, 0)
                        d2 += Math.abs(yPoints[k]);
                    } else {
                        slope = y2t / x2t;
                        d2 += Math.abs( (slope * xPoints[k] - yPoints[k]) / Math.sqrt(slope * slope + 1));
                    }
                }
                if (d1 < d2) {
                    tangentX = (float) x1t;
                    tangentY = (float) y1t;
                    normalX = (float) x2t;
                    normalY = (float) y2t;
                } else {
                    tangentX = (float) x2t;
                    tangentY = (float) y2t;
                    normalX = (float) x1t;
                    normalY = (float) y1t;
                }
            }

            omega = Math.atan2(normalY, normalX);
            // Change omega range from -PI to PI to 0 to 2*PI
            if (omega < 0.0) {
                omega = omega + 2.0 * Math.PI;
            }
            x = (int) Math.round(xpc + 2.0 * Math.cos(omega));
            y = (int) Math.round(ypc + 2.0 * Math.sin(omega));
            index = x + xDim * y;
            if (maskBuffer[index] == 0) {
                // Not in VOI - take normal going the opposite way
                if (omega < Math.PI) {
                    omega = omega + Math.PI;
                } else {
                    omega = omega - Math.PI;
                }
            }
            if (n >= 1) {
                curveXDist = xpc - lastxpc;
                curveYDist = ypc - lastypc;
                distance = Math.sqrt(curveXDist * curveXDist + curveYDist * curveYDist);
                deltaOmega = omega - lastOmega;
                curvature = (float) (deltaOmega / distance);
                omegaArray[n] = (float) omega;
                curvatureArray[n] = curvature;
            }
            if (n == numPoints - 1) {
                curveXDist = xpc0 - xpc;
                curveYDist = ypc0 - ypc;
                distance = Math.sqrt(curveXDist * curveXDist + curveYDist * curveYDist);
                deltaOmega = omega0 - omega;
                curvature = (float) (deltaOmega / distance);
                omegaArray[0] = (float) omega;
                curvatureArray[0] = curvature;
            }
            lastxpc = xpc;
            lastypc = ypc;
            lastOmega = omega;
            if (n == 0) {
                xpc0 = xpc;
                ypc0 = ypc;
                omega0 = omega;
            }
            // centerX = xpc + floatArray[0] * Math.cos(floatArray[1]);
            // centerY = ypc + floatArray[0] * Math.sin(floatArray[1]);
            // System.out.println("centerX = " + centerX + " centerY = " + centerY);
        } // for (n = 0; n <= numPoints - 1; n++)

        // Make storage string
        fileName = ViewUserInterface.getReference().getDefaultDirectory() + File.separator + fileName;
        try {
            rOut = new RandomAccessFile(new File(fileName), "rw");
        } catch (FileNotFoundException e) {
            MipavUtil.displayError("File not found exception on " + fileName);
            setCompleted(false);
            return;
        }

        // Necessary so that if this is an overwritten file there isn't any
        // junk at the end
        try {
            rOut.setLength(0);
        } catch (IOException e) {
            MipavUtil.displayError("IOException on rOut.setLength(0) for " + fileName);
            setCompleted(false);
            return;
        }

        try {
            rOut.writeFloat((float) centerX);
            rOut.writeFloat((float) centerY);
            rOut.writeInt(sidePointsForTangent);
            rOut.writeInt(xDim);
            rOut.writeInt(numPoints);
            for (i = 0; i < numPoints; i++) {
                rOut.writeInt(indexArray[i]);
                rOut.writeFloat(omegaArray[i]);
                rOut.writeFloat(curvatureArray[i]);
            }
        } catch (IOException e) {
            MipavUtil.displayError("IOException on write to " + fileName);
            setCompleted(false);
            return;
        }
        try {
            rOut.close();
        } catch (IOException e) {
            MipavUtil.displayError("IOException on " + fileName + " close");
            setCompleted(false);
            return;
        }

        ViewUserInterface.getReference().setDataText("Have written R-table to " + fileName + "\n");

        if (test) {

            new ViewJFrameImage(maskImage);
        }

        setCompleted(true);
        return;
    }

    public void runAlgorithm() {
        int x, y;
        int offset;

        int xDim;

        int yDim;

        int sourceSlice;

        int numPoints;

        int i, j, k, n;
        int index;

        byte[] srcBuffer;
        byte[] maskBuffer;
        boolean test = false;
        int indexArray[];
        int neighbors;
        boolean foundArray[];
        int indexPtr = 0;
        float tangentX;
        float tangentY;
        float normalX;
        float normalY;
        float xPoints[];
        float yPoints[];
        int endPtr;
        int startWrapPoints;
        int endWrapPoints;
        int neighbor1[];
        int neighbor2[];
        AlgorithmMorphology2D algoMorph2D;
        int pruningPix;
        boolean entireImage;
        double var;
        float xpc;
        float ypc;
        double xSqSum;
        double ySqSum;
        double xySum;
        double x1t;
        double x2t;
        double y1t;
        double y2t;
        double d1;
        double d2;
        double slope;
        ModelImage maskImage = null;
        ViewJComponentEditImage componentImage;
        boolean useXOR = false;
        boolean onlyActive = true;
        int startPos = 0;
        ViewVOIVector VOIs;
        int nVOI;
        VOI selectedVOI = null;
        Vector3f center;
        double centerX;
        double centerY;
        double distX;
        double distY;
        double omega;
        LinkedList omegaRBetaList[];
        int omegaIndex;
        double binWidth;
        float floatArray[];
        RandomAccessFile rOut;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Create R-table ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sourceSlice = xDim * yDim;

        srcBuffer = new byte[sourceSlice];

        VOIs = srcImage.getVOIs();

        nVOI = VOIs.size();

        if (nVOI == 0) {
            MipavUtil.displayError("Must create a contour VOI");

            setCompleted(false);
            return;
        }

        int contourVOI = 0;
        int activeContourVOI = 0;

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                contourVOI++;

                if (VOIs.VOIAt(i).isActive() == true) {
                    selectedVOI = VOIs.VOIAt(i);
                    activeContourVOI++;
                }
            }
        }

        if (contourVOI == 0) {
            MipavUtil.displayError("Must create a contour VOI");
            setCompleted(false);
            return;
        }

        if ( (contourVOI > 1) && (activeContourVOI != 1)) {
            MipavUtil.displayError("VOI must be selected");
            setCompleted(false);
            return;
        }

        if (contourVOI == 1) {
            for (i = 0; i < nVOI; i++) {
                if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                    selectedVOI = VOIs.VOIAt(i);
                }
            }
        }

        center = selectedVOI.getGeometricCenter();
        centerX = center.X;
        centerY = center.Y;

        componentImage = srcImage.getParentFrame().getComponentImage();

        try {

            if (componentImage.getVOIHandler().getActiveVOICount() == 0) {
                componentImage.getVOIHandler().selectAllVOIs(true);
            }

            maskImage = srcImage.generateBinaryImage(useXOR, onlyActive);

            // if (maskImage != null) {
            // maskImage.setImageName(srcImage.getImageName() + "_bmask");
            // new ViewJFrameImage(maskImage, null, new Dimension(610, 200), false);
            // }
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: unable to open new frame");

            if (maskImage != null) {
                maskImage.disposeLocal();
            }

            maskImage = null;

            return;
        }

        try {
            maskImage.exportData(0, sourceSlice, srcBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on maskImage.exportData");

            setCompleted(false);

            return;
        }

        // Keep a copy of all pixels in the selectedVOI;
        maskBuffer = new byte[sourceSlice];
        for (i = 0; i < sourceSlice; i++) {
            maskBuffer[i] = srcBuffer[i];
        }

        // Find all set pixels that have 4 set nearest neighbors
        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if (srcBuffer[index] != 0) {
                    neighbors = 0;
                    if (y > 0) {
                        if (srcBuffer[index - xDim] != 0) {
                            neighbors++;
                        }
                    } // if (y > 0)
                    if (x > 0) {
                        if (srcBuffer[index - 1] != 0) {
                            neighbors++;
                        }
                    } // if (x > 0)
                    if (x < xDim - 1) {
                        if (srcBuffer[index + 1] != 0) {
                            neighbors++;
                        }
                    } // if (x < xDim - 1)
                    if (y < yDim - 1) {
                        if (srcBuffer[index + xDim] != 0) {
                            neighbors++;
                        }
                    } // if (y < yDim - 1)
                    if (neighbors == 4) {
                        srcBuffer[index] = 2;
                    }
                } // if (srcBuffer[index] != 0)
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)

        // Clear all set pixels that have 4 set nearest neighbors
        for (i = 0; i < sourceSlice; i++) {
            if (srcBuffer[i] == 2) {
                srcBuffer[i] = 0;
            }
        }

        try {
            maskImage.importData(0, srcBuffer, true);
        } catch (IOException e) {
            MipavUtil.displayError("IOException on maskImage.importData");
        }

        // Binary image is already skeletonized, but dangling branches may be present
        // Prune off branches with 5 or less pixels
        pruningPix = 5;
        entireImage = true;
        algoMorph2D = new AlgorithmMorphology2D(maskImage, 0, 0.0f, AlgorithmMorphology2D.SKELETONIZE, 0, 0,
                pruningPix, 0, entireImage);
        algoMorph2D.run();
        algoMorph2D.finalize();

        try {
            maskImage.exportData(0, sourceSlice, srcBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on maskImage.exportData");

            setCompleted(false);

            return;
        }

        // Find the 2 neighbors of every point
        numPoints = 0;
        neighbor1 = new int[sourceSlice];
        neighbor2 = new int[sourceSlice];
        for (i = 0; i < sourceSlice; i++) {
            neighbor1[i] = -1;
            neighbor2[i] = -1;
        }
        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if (srcBuffer[index] != 0) {
                    neighbors = 0;
                    if (y > 0) {
                        if (x > 0) {
                            if (srcBuffer[index - xDim - 1] != 0) {
                                neighbors++;
                                neighbor1[index] = index - xDim - 1;
                            }
                        }
                        if (srcBuffer[index - xDim] != 0) {
                            neighbors++;
                            if (neighbor1[index] == -1) {
                                neighbor1[index] = index - xDim;
                            } else {
                                neighbor2[index] = index - xDim;
                            }
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index - xDim + 1] != 0) {
                                neighbors++;
                                if (neighbor1[index] == -1) {
                                    neighbor1[index] = index - xDim + 1;
                                } else {
                                    neighbor2[index] = index - xDim + 1;
                                }
                            }
                        }
                    } // if (y > 0)
                    if (x > 0) {
                        if (srcBuffer[index - 1] != 0) {
                            neighbors++;
                            if (neighbor1[index] == -1) {
                                neighbor1[index] = index - 1;
                            } else {
                                neighbor2[index] = index - 1;
                            }
                        }
                    } // if (x > 0)
                    if (x < xDim - 1) {
                        if (srcBuffer[index + 1] != 0) {
                            neighbors++;
                            if (neighbor1[index] == -1) {
                                neighbor1[index] = index + 1;
                            } else {
                                neighbor2[index] = index + 1;
                            }
                        }
                    } // if (x < xDim - 1)
                    if (y < yDim - 1) {
                        if (x > 0) {
                            if (srcBuffer[index + xDim - 1] != 0) {
                                neighbors++;
                                if (neighbor1[index] == -1) {
                                    neighbor1[index] = index + xDim - 1;
                                } else {
                                    neighbor2[index] = index + xDim - 1;
                                }
                            }
                        }
                        if (srcBuffer[index + xDim] != 0) {
                            neighbors++;
                            if (neighbor1[index] == -1) {
                                neighbor1[index] = index + xDim;
                            } else {
                                neighbor2[index] = index + xDim;
                            }
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index + xDim + 1] != 0) {
                                neighbors++;
                                if (neighbor1[index] == -1) {
                                    neighbor1[index] = index + xDim + 1;
                                } else {
                                    neighbor2[index] = index + xDim + 1;
                                }
                            }
                        }
                    } // if (y < yDim - 1)
                    // Delete isolated points - should not happen
                    if (neighbors == 0) {
                        srcBuffer[index] = 0;
                        neighbor1[index] = -1;
                        neighbor2[index] = -1;
                    } else {
                        numPoints++;
                    }
                } // if (srcBuffer[index] != 0)
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)
        ViewUserInterface.getReference().setDataText("Number of points on curve = " + numPoints + "\n");

        // Find a starting position
        loop1: for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if (srcBuffer[index] != 0) {
                    startPos = index;
                    break loop1;
                }
            }
        }

        // Find a starting position and length of the closed curve
        xPoints = new float[2 * sidePointsForTangent + 1];
        yPoints = new float[2 * sidePointsForTangent + 1];
        foundArray = new boolean[sourceSlice];
        indexArray = new int[numPoints];
        omegaRBetaList = new LinkedList[binNumber];
        for (i = 0; i < binNumber; i++) {
            omegaRBetaList[i] = new LinkedList();
        }
        binWidth = (2.0 * Math.PI) / binNumber;

        index = startPos;
        indexPtr = 0;
        foundArray[index] = true;
        indexArray[indexPtr++] = index;
        while ( ( !foundArray[neighbor1[index]]) || ( !foundArray[neighbor2[index]])) {
            if ( !foundArray[neighbor1[index]]) {
                index = neighbor1[index];
            } else {
                index = neighbor2[index];
            }
            foundArray[index] = true;
            indexArray[indexPtr++] = index;
        } // while ((!foundArray[neighbor1[index]]) || (!foundArray[neighbor2[index]]))
        endPtr = indexPtr - 1;

        for (n = 0; n <= numPoints - 1; n++) {
            // Put the tangent point at index sidePointsForTangent in the
            // center of the xPoints and yPoints array with sidePointsForTangent points
            // to each side.
            startWrapPoints = Math.max(0, sidePointsForTangent - n);
            endWrapPoints = Math.max(0, sidePointsForTangent - (numPoints - 1 - n));
            for (k = 0; k < startWrapPoints; k++) {
                xPoints[k] = indexArray[endPtr - (startWrapPoints - k)] % xDim;
                yPoints[k] = indexArray[endPtr - (startWrapPoints - k)] / xDim;
            }
            for (k = startWrapPoints, j = n - sidePointsForTangent + startWrapPoints; k < 2 * sidePointsForTangent + 1
                    - endWrapPoints; j++, k++) {
                xPoints[k] = indexArray[j] % xDim;
                yPoints[k] = indexArray[j] / xDim;
            }
            for (j = 0, k = 2 * sidePointsForTangent + 1 - endWrapPoints; k < 2 * sidePointsForTangent + 1; j++, k++) {
                xPoints[k] = indexArray[j] % xDim;
                yPoints[k] = indexArray[j] / xDim;
            }
            // For the closed curve find the slope and y axis intercept of the tangent line to the curve at a point
            // With a user specified sidePointsForTangent on each side of a point find the tangent line that
            // minimizes the sum of the squared distances from these side points to the tangent line
            if (sidePointsForTangent == 1) {
                xpc = xPoints[1];
                ypc = yPoints[1];
                tangentX = (xPoints[2] - xPoints[0]) / 2.0f;
                tangentY = (yPoints[2] - yPoints[0]) / 2.0f;
                if (tangentX == 0) {
                    normalX = 1;
                    normalY = 0;
                } else if (tangentY == 0) {
                    normalX = 0;
                    normalY = 1;
                } else {
                    normalX = -tangentY / tangentX;
                    normalY = 1;
                }
            } // if (sidePointsForTangent == 1)
            else {
                // Center all points for tangent point touching curve at (0, 0)
                // That is, use an x axis and a y axis going thru the tangent point
                xpc = xPoints[sidePointsForTangent];
                ypc = yPoints[sidePointsForTangent];
                for (k = 0; k < xPoints.length; k++) {
                    xPoints[k] = xPoints[k] - xpc;
                    yPoints[k] = yPoints[k] - ypc;
                }
                xSqSum = 0.0;
                ySqSum = 0.0;
                xySum = 0.0;
                for (k = 0; k < xPoints.length; k++) {
                    xSqSum += xPoints[k] * xPoints[k];
                    ySqSum += yPoints[k] * yPoints[k];
                    xySum += xPoints[k] * yPoints[k];
                }
                if (xySum != 0.0) {
                    var = Math.sqrt(ySqSum * ySqSum - 2.0 * xSqSum * ySqSum + xSqSum * xSqSum + 4.0 * xySum * xySum);
                    x1t = 0.5 * ( ( -ySqSum + xSqSum + var) / xySum);
                    x2t = 0.5 * ( ( -ySqSum + xSqSum - var) / xySum);
                    y1t = 1.0;
                    y2t = 1.0;
                } else {
                    // If all points are symmetric to either this new x axis or this new y axis, then
                    // their product sum is 0 and the tangentX, tangentY must be 1,0 or 0,1
                    x1t = 1.0;
                    x2t = 0.0;
                    y1t = 0.0;
                    y2t = 1.0;
                }
                // x1t, y1t and x2t, y2t are perpindicular. To find the solution, calculate the sum of
                // distances from the curve points to the line for the 2 cases
                // The shortest distance is the correct solution
                // Distance from AX + BY + C = 0 to P1 is
                // abs((A*x1 + B*y1 + C))/sqrt(A**2 + B**2)
                // Here A = slope, B = -1, and C = 0.
                d1 = 0.0;
                for (k = 0; k < xPoints.length; k++) {
                    if (x1t == 0.0) {
                        // Infinite slope thru (0,0)
                        d1 += Math.abs(xPoints[k]);
                    } else if (y1t == 0.0) {
                        // Zero slope thru (0, 0)
                        d1 += Math.abs(yPoints[k]);
                    } else {
                        slope = y1t / x1t;
                        d1 += Math.abs( (slope * xPoints[k] - yPoints[k]) / Math.sqrt(slope * slope + 1));
                    }
                }
                d2 = 0.0;
                for (k = 0; k < xPoints.length; k++) {
                    if (x2t == 0.0) {
                        // Infinite slope thru (0,0)
                        d2 += Math.abs(xPoints[k]);
                    } else if (y2t == 0.0) {
                        // Zero slope thru (0, 0)
                        d2 += Math.abs(yPoints[k]);
                    } else {
                        slope = y2t / x2t;
                        d2 += Math.abs( (slope * xPoints[k] - yPoints[k]) / Math.sqrt(slope * slope + 1));
                    }
                }
                if (d1 < d2) {
                    tangentX = (float) x1t;
                    tangentY = (float) y1t;
                    normalX = (float) x2t;
                    normalY = (float) y2t;
                } else {
                    tangentX = (float) x2t;
                    tangentY = (float) y2t;
                    normalX = (float) x1t;
                    normalY = (float) y1t;
                }
            }
            omega = Math.atan2(normalY, normalX);
            // Change omega range from -PI to PI to 0 to 2*PI
            if (omega < 0.0) {
                omega = omega + 2.0 * Math.PI;
            }
            x = (int) Math.round(xpc + 2.0 * Math.cos(omega));
            y = (int) Math.round(ypc + 2.0 * Math.sin(omega));
            index = x + xDim * y;
            if (maskBuffer[index] == 0) {
                // Not in VOI - take normal going the opposite way
                if (omega < Math.PI) {
                    omega = omega + Math.PI;
                } else {
                    omega = omega - Math.PI;
                }
            }
            omegaIndex = (int) (omega / binWidth);
            distX = centerX - xpc;
            distY = centerY - ypc;
            floatArray = new float[2];
            floatArray[0] = (float) Math.sqrt(distX * distX + distY * distY);
            floatArray[1] = (float) Math.atan2(distY, distX);
            omegaRBetaList[omegaIndex].add(floatArray);
            // centerX = xpc + floatArray[0] * Math.cos(floatArray[1]);
            // centerY = ypc + floatArray[0] * Math.sin(floatArray[1]);
            // System.out.println("centerX = " + centerX + " centerY = " + centerY);
        } // for (n = 0; n <= numPoints - 1; n++)

        // Make storage string
        fileName = ViewUserInterface.getReference().getDefaultDirectory() + File.separator + fileName;
        try {
            rOut = new RandomAccessFile(new File(fileName), "rw");
        } catch (FileNotFoundException e) {
            MipavUtil.displayError("File not found exception on " + fileName);
            setCompleted(false);
            return;
        }

        // Necessary so that if this is an overwritten file there isn't any
        // junk at the end
        try {
            rOut.setLength(0);
        } catch (IOException e) {
            MipavUtil.displayError("IOException on rOut.setLength(0) for " + fileName);
            setCompleted(false);
            return;
        }

        try {
            rOut.writeInt(binNumber);
            rOut.writeInt(sidePointsForTangent);
            for (i = 0; i < binNumber; i++) {
                rOut.writeInt(omegaRBetaList[i].size());
                for (j = 0; j < omegaRBetaList[i].size(); j++) {
                    floatArray = (float[]) omegaRBetaList[i].get(j);
                    rOut.writeFloat(floatArray[0]); // r
                    rOut.writeFloat(floatArray[1]); // beta
                }
            }
        } catch (IOException e) {
            MipavUtil.displayError("IOException on write to " + fileName);
            setCompleted(false);
            return;
        }
        try {
            rOut.close();
        } catch (IOException e) {
            MipavUtil.displayError("IOException on " + fileName + " close");
            setCompleted(false);
            return;
        }

        ViewUserInterface.getReference().setDataText("Have written R-table to " + fileName + "\n");

        if (test) {

            new ViewJFrameImage(maskImage);
        }

        setCompleted(true);
        return;
    }
}
