package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector2f;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.util.*;


/**
 * Smoothing of VOI using 1 iteration of bSplines. All selected curves in all slices of the voi will be smoothed. The
 * number of interpolated points is user selectable. The user chooses whether or not to trim out nearly collinear
 * points. The user chooses whether or not to remove the original selected curve. If the original curve is not removed,
 * the new curve will have a different color. If the original curve is removed, the new curve will have the same color.
 *
 * @see  AlgorithmBSpline
 */

public class AlgorithmBSmooth extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The voi selected by the user. */
    private VOI activeVOI;

    /** Number of interpolation points. */
    private int nPts;

    /** The resultant polygon and the evolution has completed. */
    private VOI resultVOI;

    /** The initial polygon to initialize the evolution process. */
    private Polygon srcGon;

    /** Source image. */
    private ModelImage srcImage;

    /** Trim out nearly collinear points. */
    private boolean trim;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmBSmooth object.
     *
     * @param  srcImg     2D or 3D source image
     * @param  activeVOI  the selected voi
     * @param  nPts       number of interpolation points
     * @param  trim       trim out nearly collinear points
     */
    public AlgorithmBSmooth(ModelImage srcImg, VOI activeVOI, int nPts, boolean trim) {

        srcImage = srcImg;
        this.activeVOI = activeVOI;
        this.nPts = nPts;
        this.trim = trim;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        super.finalize();
    }

    /**
     * Accessor that returns a voi that is a smoothed version of the original.
     *
     * @return  resultVOI
     */
    public VOI getResultVOI() {
        return resultVOI;
    }


    /**
     * Starts the smooth algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        } else {

            if (srcImage.getNDims() == 2) {
                calc2D();
            } else if (srcImage.getNDims() > 2) {
                calc3D();
            }
        }

    }

    /**
     * Prepares the data and runs the algorithm for a 2D image.
     */
    private void calc2D() {
        float[] xPoints = new float[1];
        float[] yPoints = new float[1];
        Polygon resultGon = new Polygon();
        int elementNum;
        int nContours;
        Vector[] contours;


        fireProgressStateChanged(srcImage.getImageName(), "Bspline smooth: Evolving boundary ...");

        fireProgressStateChanged(25);

        resultVOI = new VOI((short) srcImage.getVOIs().size(), "Bsmooth-VOI", 1, VOI.CONTOUR, -1.0f);
        contours = activeVOI.getCurves();
        nContours = contours[0].size();

        for (elementNum = 0; elementNum < nContours; elementNum++) {
            fireProgressStateChanged((int) (25 + (75 * (((float) elementNum) / nContours))));

            if (((VOIContour) (contours[0].elementAt(elementNum))).isActive()) {
                srcGon = ((VOIContour) (contours[0].elementAt(elementNum))).exportPolygon(1, 1, 1, 1);
                //System.err.println("Element number is: " + elementNum);
                if (srcGon.npoints > 5) {
                    xPoints = new float[srcGon.npoints + 5];
                    yPoints = new float[srcGon.npoints + 5];
                    fireProgressStateChanged(25 + (((75 * elementNum) + 5) / nContours));
                    setPoints(xPoints, yPoints, srcGon);
                    fireProgressStateChanged(25 + (((75 * elementNum) + 25) / nContours));
                    runSmooth(xPoints, yPoints, resultGon);
                    fireProgressStateChanged(25 + (((75 * elementNum) + 50) / nContours));
                    
                    resultVOI.importPolygon(resultGon, 0);
                    resultGon = new Polygon();

                    if (threadStopped) {
                        finalize();

                        return;
                    }

                    if (trim) {
                        ((VOIContour) (resultVOI.getCurves()[0].lastElement())).trimPoints(Preferences.getTrim(),
                                                                                           Preferences.getTrimAdjacient());
                    }
                } else {
                	// nPoints is less than 5.  doesn't mean we want to scrap the contour though!
                	resultVOI.importPolygon(srcGon, 0);
                	
                }
            } // if ( ((VOIContour)(contours[0].elementAt(elementNum))).isActive() )
        } // for(elementNum = 0; elementNum < nContours; elementNum++)

        fireProgressStateChanged(100);
        
        setCompleted(true);
    }

    /**
     * Prepares the data and runs the algorithm for a 3D image.
     */
    private void calc3D() {

        int slice;
        Polygon resultGon = new Polygon();
        float[] xPoints, yPoints;
        int elementNum;
        int nContours;
        Vector[] contours;

        resultVOI = new VOI((short) srcImage.getVOIs().size(), "BsmoothVOI.voi", srcImage.getExtents()[2], VOI.CONTOUR,
                            -1.0f);

        fireProgressStateChanged(srcImage.getImageName(), "Bspline smooth: Evolving boundary ...");

        fireProgressStateChanged(0);
        contours = activeVOI.getCurves();

        for (slice = 0; slice < srcImage.getExtents()[2]; slice++) {
            nContours = contours[slice].size();
            fireProgressStateChanged((int) (100 * ((float) slice) / (srcImage.getExtents()[2] - 1)));

            for (elementNum = 0; elementNum < nContours; elementNum++) {

                if (((VOIContour) (contours[slice].elementAt(elementNum))).isActive()) {
                    srcGon = ((VOIContour) (contours[slice].elementAt(elementNum))).exportPolygon(1, 1, 1, 1);

                    if (srcGon.npoints > 5) {
                        xPoints = new float[srcGon.npoints + 5];
                        yPoints = new float[srcGon.npoints + 5];
                        setPoints(xPoints, yPoints, srcGon);
                        runSmooth(xPoints, yPoints, resultGon);
                        resultVOI.importPolygon(resultGon, slice);
                        resultGon = new Polygon();

                        if (threadStopped) {
                            finalize();

                            return;
                        }

                        if (trim) {
                            ((VOIContour) (resultVOI.getCurves()[slice].lastElement())).trimPoints(Preferences.getTrim(),
                                                                                                   Preferences.getTrimAdjacient());
                        }
                    } else {
                    	// nPoints is less than 5.  doesn't mean we want to scrap the contour though!
                    	resultVOI.importPolygon(srcGon, slice);
                    	
                    }
                }
            }
        } // for (slice = 0; slice < srcImage.getExtents()[2]; slice++)

        fireProgressStateChanged(100);
        setCompleted(true);
        

        return;


    }


    /**
     * Actual function that smooths the voi with bsplines.
     *
     * @param  xPoints    x coordinates that describe the contour
     * @param  yPoints    y coordinates that describe the contour
     * @param  resultGon  resultant polygon
     */
    private void runSmooth(float[] xPoints, float[] yPoints, Polygon resultGon) {
        int i, j;
        float pct;
        float index;
        Vector2f interpPt = new Vector2f();

        float[] newXPts, newYPts;

        AlgorithmArcLength arcLength = new AlgorithmArcLength(xPoints, yPoints);
        AlgorithmBSpline bSpline = new AlgorithmBSpline();

        newXPts = new float[nPts];
        newYPts = new float[nPts];

        for (i = 0; i < nPts; i++) {
            pct = i / (float) (nPts);

            /** Note that pct = 0 returns an index = 2 while pct = 1.0
             * returns an index 2 below the maximum array index */
            index = arcLength.invlen(pct);
            interpPt = bSpline.bSplineJetXY(0, index, xPoints, yPoints);
            newXPts[i] = interpPt.X;
            newYPts[i] = interpPt.Y;
        }

        for (j = 0; j < nPts; j++) {
            resultGon.addPoint(Math.round(newXPts[j]), Math.round(newYPts[j]));
        }
    }

    /**
     * Takes the polygon and forms two special arrays for use in the Bspline.
     *
     * @param  xPoints  storage location of array of x coord. points
     * @param  yPoints  storage location array of y coord. points
     * @param  gon      initial polygon
     */
    private void setPoints(float[] xPoints, float[] yPoints, Polygon gon) {

        /** Note that 0 is used twice - once in the 0 to 1 segment and once
         * in the n-1 to zero segment. */
        int i;

        xPoints[0] = gon.xpoints[gon.npoints - 2];
        yPoints[0] = gon.ypoints[gon.npoints - 2];

        xPoints[1] = gon.xpoints[gon.npoints - 1];
        yPoints[1] = gon.ypoints[gon.npoints - 1];

        for (i = 0; i < gon.npoints; i++) {
            xPoints[i + 2] = gon.xpoints[i];
            yPoints[i + 2] = gon.ypoints[i];
        }

        xPoints[gon.npoints + 2] = gon.xpoints[0];
        yPoints[gon.npoints + 2] = gon.ypoints[0];

        xPoints[gon.npoints + 3] = gon.xpoints[1];
        yPoints[gon.npoints + 3] = gon.ypoints[1];

        xPoints[gon.npoints + 4] = gon.xpoints[2];
        yPoints[gon.npoints + 4] = gon.ypoints[2];
    }

}
