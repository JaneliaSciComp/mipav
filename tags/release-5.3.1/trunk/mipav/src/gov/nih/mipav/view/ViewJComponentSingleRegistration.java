package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;

//import static gov.nih.mipav.view.MipavUtil.*;

import java.util.*;




/**
 * Extended version of ViewJComponentEditImage, used ONLY within the ViewJFrameRegistrationTool This class is tailored
 * to support Reference and Adjusted markers for use with Least Squares and Thin Plate Splines registration operations.
 */
public class ViewJComponentSingleRegistration extends ViewJComponentEditImage {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6751136899996457162L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** location of the center pt VOI (for registration rotation). */
    private int centerPtLocation = -1;

    /** if the image is in a registration window, is the image the reference (and not the adjusted). */
    private boolean isReference = true;

    /** Temporary buffer used when extracting points from a VOI. Save reallocating memory often. */
    private float[] ptCoord;

    /** Buffers used to save the X coordinates for the points that make up a VOI. */
    private int[] xCoords;

    /** Buffers used to save the Y coordinates for the points that make up a VOI. */
    private int[] yCoords;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor: ImageA and ImageB are expected to be of the same dimensionality !!
     *
     * @param  _frame             frame where image(s) will be displayed
     * @param  _imageA            Model of the image that will be displayed
     * @param  _LUTa              LUT used to display imageA
     * @param  imgBufferA         storage buffer used to display image A
     * @param  pixelBuffer        storage buffer used to build a displayable image
     * @param  zoom               initial magnification of image
     * @param  extents            initial display dimensions of the image
     * @param  logMagDisplay      display log magnitude of image
     * @param  _orientation       orientation of the image
     * @param  isReference        DOCUMENT ME!
     */
    public ViewJComponentSingleRegistration(ViewJFrameBase _frame, ModelImage _imageA, ModelLUT _LUTa,
            float[] imgBufferA, int[] pixelBuffer, float zoom, int[] extents,
            boolean logMagDisplay, int _orientation, boolean isReference) {
        super(_frame, _imageA, _LUTa, imgBufferA, null, null, null, pixelBuffer, zoom, extents, logMagDisplay, _orientation );


        this.isReference = isReference;

        xCoords = new int[100];
        yCoords = new int[100];
        ptCoord = new float[3];

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Gets the center point of rotation.
     *
     * @return  int
     */
    public int getCenterPtLocation() {
        return centerPtLocation;
    }

    /**
     * Finds the number of points in the active VOI contour.
     *
     * @return  the number of points in the selected VOI
     */
    public int getNumPoints() {
        ViewVOIVector VOIs = imageActive.getVOIs();

        if (VOIs.size() == 0) {
            return 0;
        } 
        else if (VOIs.size() == 1) {
            if (centerPtLocation != -1) {
                return 0;
            }
            return VOIs.elementAt(0).getCurves().size();
        } 
        else {
            if (centerPtLocation == 0) {
                return VOIs.elementAt(1).getCurves().size();
            }
            return VOIs.elementAt(0).getCurves().size();
        }
    }


    /**
     * All VOIPoints are stored in one VOI contained in this class. This function returns that VOI.
     * @return VOI for the VOIPoints in this class.
     */
    public VOI getPointVOI() {
        ViewVOIVector VOIs = imageActive.getVOIs();
        int nVOI = VOIs.size();
        if ( nVOI == 0 )
        {
            float presetHue = isReference? 0 : 1f/3f;
            VOI newVOI = new VOI( (short)0, imageActive.getImageName()+"VOI", VOI.POINT, presetHue );
            imageActive.registerVOI(newVOI);
            return newVOI;
        }
        int index = 0;

        if ((centerPtLocation == 0) && (nVOI == 1)) {
            float presetHue = isReference? 0 : 1f/3f;
            VOI newVOI = new VOI( (short)0, imageActive.getImageName()+"VOI", VOI.POINT, presetHue );
            imageActive.registerVOI(newVOI);
            return newVOI;
        } else if ((centerPtLocation == 0) && (nVOI == 2)) {
            index = 1;
        }

        return VOIs.elementAt(index);
    }

    /**
     * All VOIPoints are stored in one VOI contained in this class. This function sets that VOI
     * used when the user copies VOIPoints from the reference to the adjustable image or vice versa.
     * @param newVOI new set of VOIPoints.
     */
    public void setPointVOI(VOI newVOI) {
        float presetHue = isReference? 0 : 1f/3f;
        newVOI.setColor(presetHue);
        int nVOI;
        ViewVOIVector VOIs = imageActive.getVOIs();

        nVOI = VOIs.size();
        if ( nVOI == 0 )
        {
            VOIs.add(newVOI);
            return;
        }
        int index = 0;
        if ((centerPtLocation == 0) && (nVOI == 1)) {
            imageActive.registerVOI(newVOI);
        } else if ((centerPtLocation == 0) && (nVOI == 2)) {
            index = 1;
        }
        VOIs.set(index, newVOI);
    }


    /**
     * Returns an array containing the list of X coordinates for the VOIPoints.
     *
     * @return  int[] list of x coordinates
     */
    public int[] getXCoords() {

        int i;
        int nVOI;
        ViewVOIVector VOIs = imageActive.getVOIs();

        nVOI = VOIs.size();

        int index = 0;

        if ((centerPtLocation == 0) && (nVOI == 1)) {
            return new int[0];
        } else if ((centerPtLocation == 0) && (nVOI == 2)) {
            index = 1;
        }

        Vector<VOIBase> ptVector = VOIs.elementAt(index).getCurves();
        VOIPoint pt = null;

        // System.err.println("Point vector size: " + ptVector.size());
        try {

            for (i = 0; i < ptVector.size(); i++) {
                pt = (VOIPoint) ptVector.elementAt(i);
                pt.getCoordinates(ptCoord);
                xCoords[i] = Math.round(ptCoord[0]);
            }
        } catch (IndexOutOfBoundsException ex) {
            MipavUtil.displayWarning("Ignoring x pt coordinates after 100th point");

            return xCoords;
        }

        return (xCoords);

    }

    /**
     * Returns an array containing the list of Y coordinates for the VOIPoints.
     *
     * @return  int[] list of y coordinates
     */
    public int[] getYCoords() {

        int i;
        int nVOI;
        ViewVOIVector VOIs = imageActive.getVOIs();

        nVOI = VOIs.size();

        int index = 0;

        if ((centerPtLocation == 0) && (nVOI == 1)) {
            return new int[0];
        } else if ((centerPtLocation == 0) && (nVOI == 2)) {
            index = 1;
        }

        Vector<VOIBase> ptVector = VOIs.elementAt(index).getCurves();
        VOIPoint pt = null;

        try {

            for (i = 0; i < ptVector.size(); i++) {
                pt = (VOIPoint) ptVector.elementAt(i);
                pt.getCoordinates(ptCoord);

                yCoords[i] = Math.round(ptCoord[1]);
            }
        } catch (IndexOutOfBoundsException ex) {
            MipavUtil.displayWarning("Ignoring y pt coordinates after 100th point");

            return yCoords;
        }

        return (yCoords);

    }

    /**
     * Sets the TransMatrix for the set of VOIPoints contained in this class.
     * @param kMat new TransMatrix
     */
    public void setRotate( TransMatrix kMat )
    {
        int i;
        int nVOI;
        ViewVOIVector VOIs = imageActive.getVOIs();

        nVOI = VOIs.size();

        int index = 0;

        if ((centerPtLocation == 0) && (nVOI == 1)) {
            return;
        } else if ((centerPtLocation == 0) && (nVOI == 2)) {
            index = 1;
        }

        Vector<VOIBase> ptVector = VOIs.elementAt(index).getCurves();
        for (i = 0; i < ptVector.size(); i++) {
            ((VOIPoint)ptVector.elementAt(i)).setMatrix(kMat);
        }
    }

    /**
     * Returns true if this is the reference image.
     * @return true if this is the reference image.
     */
    public boolean isReference() {
        return isReference;
    }

    // ************************************************************************
    // ************************** Mouse Motion Events *************************
    // ************************************************************************



    /**
     * Resets all of the VOIPoint's by moving them from pointSet A to point set B.
     *
     * @param  pointSetA  current VOIPoints locations
     * @param  pointSetB  locations to where the VOIPoints should be moved
     */
    public void resetAdjustableVOIs(double[][] pointSetA, double[][] pointSetB) {
        int i;
        int deltaX = 0;
        int deltaY = 0;
        int index = 0;

        if (centerPtLocation == 0) {
            index = 1;
        }

        ViewVOIVector VOIs = imageActive.getVOIs();

        for (i = 0; i < pointSetA[0].length; i++) {
            deltaX = (int) (pointSetA[0][i] - pointSetB[0][i]);
            deltaY = (int) (pointSetA[1][i] - pointSetB[1][i]);
            ((VOIPoint) (VOIs.elementAt(index).getCurves()).elementAt(i)).moveVOIPoint(deltaX, deltaY, 0,
                    imageActive.getExtents()[0],
                    imageActive.getExtents()[1],
                    1);
        }
    }

    /* ********************************************************************** */
    /* ****************************** Accessors ***************************** */
    /* ********************************************************************** */

    /**
     * Sets the center point of rotation to this location.
     *
     * @param  centerPtLoc  int
     */
    public void setCenterPtLocation(int centerPtLoc) {
        centerPtLocation = centerPtLoc;
    }
}
