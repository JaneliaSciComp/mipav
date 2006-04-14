package gov.nih.mipav.view;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

/**
 * Extended version of ViewJComponentEditImage, used ONLY within the ViewJFrameRegistrationTool
 * This class is tailored to support Reference and Adjusted markers for use with Least Squares
 * and Thin Plate Splines registration operations
 */
public class ViewJComponentSingleRegistration extends ViewJComponentEditImage
    implements MouseListener {

    /** if the image is in a registration window, is the image the reference (and not the adjusted) */
    private boolean isReference = true;

    /** location of the center pt VOI (for registration rotation) */
    private int centerPtLocation = -1;

    /** Buffers used to save the X coordinates fo the points that make up a VOI */
    private int[] xCoords;

    /** Buffers used to save the Y coordinates fo the points that make up a VOI */
    private int[] yCoords;

    /** Temporary buffer used when extracting points from a VOI. Save reallocating memory often. */
    private float[] ptCoord;

    /**
     *   Constructor: ImageA and ImageB are expected
     *   to be of the same dimensionality !!
     *
     *   @param _frame        frame where image(s) will be displayed
     *   @param _imageA      Model of the image that will be displayed
     *   @param _LUTa        LUT used to display imageA
     *   @param imgBufferA   storage buffer used to display image A
     *   @param pixelBuffer  storage buffer used to build a displayable image
     *   @param zoom         initial magnification of image
     *   @param extents      initial display dimensions of the image
     *   @param logMagDisplay   display log magnitude of image
     *   @param _orientation  orientation of the image
     *   @param _axialOrientation true if the image is known to be in axial orientation and
     *                            is displayed in ViewJFrameTriImage
     *   @param _orient a 3 integer array containing the orientation of each axis
     */
    public ViewJComponentSingleRegistration( ViewJFrameBase _frame,
            ModelImage _imageA, ModelLUT _LUTa,
            float[] imgBufferA,
            int[] pixelBuffer,
            float zoom, int[] extents,
            boolean logMagDisplay, int _orientation,
            boolean _axialOrientation, int[] _orient,
            boolean isReference) {
        super(_frame, _imageA,_LUTa, imgBufferA, null,  null,
             null, pixelBuffer, zoom, extents,
             logMagDisplay, _orientation, _axialOrientation, _orient );


       this.isReference = isReference;

       xCoords = new int[100];
       yCoords = new int[100];
       ptCoord = new float[2];
    }

    /* ********************************************************************** */
    /* ****************************** Accessors ***************************** */
    /* ********************************************************************** */

    /**
     * Sets the center point of rotation  to this location
     * @param centerPtLoc int
     */
    public void setCenterPtLocation( int centerPtLoc ) {
        centerPtLocation = centerPtLoc;
    }

    /**
     * Gets the center point of rotation
     * @return int
     */
    public int getCenterPtLocation() {
        return centerPtLocation;
    }

    /**
     * Finds the number of points in the active VOI contour.
     * @return  the number of points in the selected VOI
     */
    public int getNumPoints() {
        ViewVOIVector VOIs = imageActive.getVOIs();

        if ( VOIs.size() == 0 ) {
            return 0;
        } else if ( VOIs.size() == 1 ) {
            if ( centerPtLocation != -1 ) {
                return 0;
            } else {
                return ( (VOI) VOIs.elementAt( 0 ) ).getCurves()[0].size();
            }
        } else {
            if ( centerPtLocation == 0 ) {
                return ( (VOI) VOIs.elementAt( 1 ) ).getCurves()[0].size();
            } else {
                return ( (VOI) VOIs.elementAt( 0 ) ).getCurves()[0].size();
            }
        }
    }

    /**
     *   Returns an array containing the list of X coordinates for the VOIPoints
     *   @return int[] list of x coordinates
     */
    public int[] getXCoords() {

        int i;
        int nVOI;
        ViewVOIVector VOIs = imageActive.getVOIs();

        nVOI = VOIs.size();
        int index = 0;

        if ( centerPtLocation == 0 && nVOI == 1 ) {
            return new int[0];
        } else if ( centerPtLocation == 0 && nVOI == 2 ) {
            index = 1;
        }

        Vector ptVector = ( (VOI) VOIs.elementAt( index ) ).getCurves()[0];
        VOIPoint pt = null;

        //System.err.println("Point vector size: " + ptVector.size());
        try {
            for ( i = 0; i < ptVector.size(); i++ ) {
                pt = (VOIPoint) ptVector.elementAt( i );
                pt.getCoordinates( ptCoord );
                xCoords[i] = Math.round( ptCoord[0] );
            }
        } catch ( IndexOutOfBoundsException ex ) {
            MipavUtil.displayWarning( "Ignoring x pt coordinates after 100th point" );
            return xCoords;
        }
        return ( xCoords );

    }

    /**
     *   Returns an array containing the list of Y coordinates for the VOIPoints
     *   @return int[] list of y coordinates
     */
    public int[] getYCoords() {

        int i;
        int nVOI;
        ViewVOIVector VOIs = imageActive.getVOIs();

        nVOI = VOIs.size();

        int index = 0;

        if ( centerPtLocation == 0 && nVOI == 1 ) {
            return new int[0];
        } else if ( centerPtLocation == 0 && nVOI == 2 ) {
            index = 1;
        }

        Vector ptVector = ( (VOI) VOIs.elementAt( index ) ).getCurves()[0];
        VOIPoint pt = null;

        try {
            for ( i = 0; i < ptVector.size(); i++ ) {
                pt = (VOIPoint) ptVector.elementAt( i );
                pt.getCoordinates( ptCoord );

                yCoords[i] = Math.round( ptCoord[1] );
            }
        } catch ( IndexOutOfBoundsException ex ) {
            MipavUtil.displayWarning( "Ignoring y pt coordinates after 100th point" );
            return yCoords;
        }
        return ( yCoords );

    }

    /**
     * Resets all of the VOIPoint's by moving them from pointSet A to point set B
     * @param pointSetA current VOIPoints locations
     * @param pointSetB locations to where the VOIPoints should be moved
     */
    public void resetAdjustableVOIs( double[][] pointSetA, double[][] pointSetB ) {
        int i;
        int deltaX = 0;
        int deltaY = 0;
        int index = 0;

        if ( centerPtLocation == 0 ) {
            index = 1;
        }

        ViewVOIVector VOIs = imageActive.getVOIs();

        for ( i = 0; i < pointSetA[0].length; i++ ) {
            deltaX = (int) ( pointSetA[0][i] - pointSetB[0][i] );
            deltaY = (int) ( pointSetA[1][i] - pointSetB[1][i] );
            ( (VOIPoint) ( ( (VOI) VOIs.elementAt( index ) ).getCurves()[0] ).elementAt( i ) ).moveVOIPoint( deltaX,
                    deltaY, 0, imageActive.getExtents()[0], imageActive.getExtents()[1], 1 );
        }
    }

    /**
     *  Compares the current VOI ID and its type to
     *  the given voi type.  If the types are different,
     *  then set the voi ID to -1, and return true (to
     *  indicate changing the type).  If the types are
     *  the same, then it's not necessary to change types
     *  so return false.
     *   @param      voiType    this should be one of the
     *                          static ints from <code>VOI</code> (for
     *                          instance <code>VOI.LINE</code>).
     *   @return     whether or not a *new* voi must be
     *               created.
     */
    public boolean isNewVoiNeeded( int voiType ) {
      if (voiID == -1) {
        return true;
      }
      if (centerPtLocation != -1) {
        if (imageActive.getVOIs().size() == 1) {
          return true;
        }
      }
      // System.err.println("returning false");
      return false;
    }

    /** Deletes the selected contour of an VOI */
    public void deleteSelectedContours() {
        int i, s, nVOI;

        ViewVOIVector VOIs = imageActive.getVOIs();

        nVOI = VOIs.size();
        if ( nVOI == 0 ) {
            return;
        }

        for ( i = 0; i < nVOI; i++ ) {
            if ( VOIs.VOIAt( i ).isActive() == true && ( i != centerPtLocation ) ) {

                break;
            } // Set i
        }
        if ( i == nVOI ) {
            MipavUtil.displayError( "VOI must be selected." );
            return; // No VOI to delete
        }

        System.err.println( "VOI is selectled, index: " + i );

        if ( imageActive.getNDims() == 2 ) {
            deleteContour( VOIs.VOIAt( i ), 0 );
        } else if ( imageActive.getNDims() >= 3 ) {
            for ( s = 0; s < imageActive.getExtents()[2]; s++ ) {
                deleteContour( VOIs.VOIAt( i ), s );
            }
        }

        // System.err.println("We are always going through this for deletion");

        if ( VOIs.VOIAt( i ).isEmpty() == true ) {
            imageActive.unregisterVOI( VOIs.VOIAt( i ) );

            int id = ( getActiveImage().getVOIs().size() > 0 )
                    ? ( ( (VOI) ( getActiveImage().getVOIs().lastElement() ) ).getID() + 1 )
                    : 0;
            int lastUID = ( getActiveImage().getVOIs().size() > 0 )
                    ? ( ( (VOI) ( getActiveImage().getVOIs().lastElement() ) ).getUID() + 1 )
                    : -1;

            this.updateVOIColor( id, lastUID );
            voiID = -1;
        }

        imageActive.notifyImageDisplayListeners( null, true );
    }

    /**
     *  Deletes all VOIs
     */
    public void deleteVOIs() {
        int i;
        int nVOI;

        ViewVOIVector VOIs = imageActive.getVOIs();

        nVOI = VOIs.size();
        if ( nVOI == 0 ) {
            return;
        }
        for ( i = ( nVOI - 1 ); i >= 0; i-- ) {
            VOIs.removeElementAt( i );
        }

        voiID = -1;
        centerPtLocation = -1;
    }

    // ************************************************************************
    // ***************************** Mouse Events *****************************
    // ************************************************************************

    /**
     *  A mouse event.  This function sets up and draws
     *  the VOI according to the mode.
     *  @param mouseEvent   event that triggered function
     */
    public void mouseReleased( MouseEvent mouseEvent ) {
        int i, j, k;
        int xR, yR;
        int nVOI;
        int xS, yS;
        ViewVOIVector VOIs = imageActive.getVOIs();
        Graphics g = getGraphics();

        if ( modifyFlag == false ) {
            return;
        }

        xS = (int)( mouseEvent.getX() / ( getZoomX() * resolutionX ) ); // zoomed x.  Used as cursor
        yS = (int)( mouseEvent.getY() / ( getZoomY() * resolutionY ) ); // zoomed y.  Used as cursor
        xR = mouseEvent.getX();
        yR = mouseEvent.getY();

        if ( xS < 0 || xS >= imageActive.getExtents()[0] || yS < 0 || yS >= imageActive.getExtents()[1] ) {
            return;
        }

        if ( mode == POINT_VOI ) {
            if ( ( mouseEvent.getModifiers() & mouseEvent.BUTTON1_MASK ) != 0 ) {
                if ( isNewVoiNeeded( VOI.POINT ) ) { // create new VOI
                    try {
                        float[] x = new float[1];
                        float[] y = new float[1];
                        float[] z = new float[1];

                        voiID = imageActive.getVOIs().size();
                        int colorID = 0;

                        if ( imageActive.getVOIs().size() > 0 ) {
                            colorID = ( (VOI) ( imageActive.getVOIs().lastElement() ) ).getID() + 1;
                        }

                        if ( imageActive.getNDims() > 2 ) {

                            newPtVOI = new VOI( (short) colorID, "point3D.voi", imageActive.getExtents()[2], VOI.POINT,
                                    -1.0f );
                        } else {
                            newPtVOI = new VOI( (short) colorID, "point2d.voi", 1, VOI.POINT, -1.0f );
                        }
                        x[0] = xS;
                        y[0] = yS;
                        z[0] = slice;
                        newPtVOI.importCurve( x, y, z, slice );
                        newPtVOI.setUID( newPtVOI.hashCode() );


                        if (isReference) {
                          newPtVOI.setColor(0.0f);
                        }
                        else {
                          newPtVOI.setColor(1.0f / 3.0f);
                        }

                    } catch ( OutOfMemoryError error ) {
                        System.gc();
                        MipavUtil.displayError( "Out of memory: ComponentEditImage.mouseReleased" );
                        setMode( DEFAULT );
                        return;
                    }
                    lastPointVOI = voiID;
                    imageActive.registerVOI( newPtVOI );
                    newPtVOI.setActive( true );

                    updateVOIColor( newPtVOI.getColor(), newPtVOI.getUID() );
                    ( (VOIPoint) ( VOIs.VOIAt( voiID ).getCurves()[slice].elementAt( 0 ) ) ).setActive( true );

                    imageActive.notifyImageDisplayListeners();

                    graphPointVOI( newPtVOI, ( (VOIPoint) ( VOIs.VOIAt( voiID ).getCurves()[slice].elementAt( 0 ) ) ), 0 );

                } // end of if (voiID == -1)
                else { // voiID != -1 add point to existing VOI

                    // System.err.println("Adding to existing voi");

                    int index;

                    nVOI = VOIs.size();

                    float[] x = new float[1];
                    float[] y = new float[1];
                    float[] z = new float[1];

                    x[0] = xS;
                    y[0] = yS;
                    z[0] = slice;

                    for ( i = 0; i < nVOI; i++ ) {
                        if ( VOIs.VOIAt( i ).getID() == voiID ) {
                            if ( VOIs.VOIAt( i ).getCurveType() == VOI.POINT && centerPtLocation != i ) {
                                VOIs.VOIAt( i ).importCurve( x, y, z, slice );
                                break;
                            } else {
                                MipavUtil.displayError( "Can't add Point VOI to other VOI structure." );
                                return;
                            }
                        }
                    }

                    int end;

                    if ( imageActive.getNDims() >= 3 ) {
                        end = imageActive.getExtents()[2];
                    } else {
                        end = 1;
                    }
                    for ( j = 0; j < end; j++ ) {
                        index = VOIs.VOIAt( i ).getCurves()[j].size();
                        for ( k = 0; k < index; k++ ) {
                            ( (VOIPoint) ( VOIs.VOIAt( i ).getCurves()[j].elementAt( k ) ) ).setActive( false );
                        }
                    }

                    index = VOIs.VOIAt( i ).getCurves()[slice].size();
                    ( (VOIPoint) ( VOIs.VOIAt( i ).getCurves()[slice].elementAt( index - 1 ) ) ).setActive( true );

                    imageActive.notifyImageDisplayListeners();

                    if ( !( VOIs.VOIAt( i ).getContourGraph() != null && imageActive.isColorImage() == true ) ) {
                        graphPointVOI( VOIs.VOIAt( i ),
                                ( (VOIPoint) ( VOIs.VOIAt( i ).getCurves()[slice].elementAt( index - 1 ) ) ), index - 1 );
                    }
                    return;
                } // end of else for if voiID != -1 add point to existing VOI
            } // end of if ((mouseEvent.getModifiers() & mouseEvent.BUTTON1_MASK) != 0)
        } // end of else if (mode == POINT_VOI)
         else if ( mode == DELETE_POINT ) { // impossible for LINE
            nVOI = VOIs.size();
            for ( i = 0; i < nVOI; i++ ) {
                if ( VOIs.VOIAt( i ).isActive() ) {
                    break;
                }
            }
            if ( i == nVOI ) {
                return;
            }

            int index = VOIs.VOIAt( i ).getActiveContourIndex( slice );

            ( (VOIContour) ( VOIs.VOIAt( i ).getCurves()[slice].elementAt( index ) ) ).removeElement();

            imageActive.notifyImageDisplayListeners();
            setMode( MOVE_POINT );
        }  else if ( mode == PAINT_VOI ) {
            imageActive.notifyImageDisplayListeners( null, true );
        } else if ( mode == MOVE ) {
            nVOI = VOIs.size();
            for ( i = 0; i < nVOI; i++ ) {
                VOIs.VOIAt( i ).setAllActive( false ); // deactivate all other VOIs
            }
            for ( i = 0; i < nVOI; i++ ) {
                VOIBase selectedCurve = null;

                for ( j = 0; j < VOIs.VOIAt( i ).getCurves()[slice].size(); j++ ) {
                    // get the curve referenced by the VOI.  We'll check it.
                    selectedCurve = ( (VOIBase) VOIs.VOIAt( i ).getCurves()[slice].elementAt( j ) );

                    if ( selectedCurve instanceof VOIPoint
                            && ( (VOIPoint) selectedCurve ).nearPoint( xR, yR, getZoomX(), resolutionX, resolutionY ) ) {
                        // points are not true curves, but we want to check if we
                        // released mouse over it. we'll at least set the point active.
                        if ( mouseEvent.isShiftDown() ) {
                            allActive = true;
                            // System.err.println("Got a shift down");
                            // if true set all points in VOI active - move all points
                            VOIs.VOIAt( i ).setAllActive( true );
                            updateVOIColor( VOIs.VOIAt( i ).getColor(), VOIs.VOIAt( i ).getUID() );
                            voiID = VOIs.VOIAt( i ).getID();
                            // and we are done with this VOI.
                            // skip the rest of the curves
                            j = VOIs.VOIAt( i ).getCurves()[slice].size();
                        } else {
                            allActive = false;
                            VOIs.VOIAt( i ).setActive( true );
                            updateVOIColor( VOIs.VOIAt( i ).getColor(), VOIs.VOIAt( i ).getUID() );
                            ( (VOIPoint) ( selectedCurve ) ).setActive( true );
                            voiID = VOIs.VOIAt( i ).getID();
                        }
                    }  else { // selected curve was not selected, so set false.
                        selectedCurve.setActive( false );
                    }
                } // end of curves in this VOI
            } // end checking all VOIs in the active image
            imageActive.notifyImageDisplayListeners();
        }

        g.dispose();

        // reset mousePressIsShiftDown for next mouse click
        mousePressIsShiftDown = false;

    } // end mouseReleased()

    // ************************************************************************
    // ************************** Mouse Motion Events *************************
    // ************************************************************************

    /**
     *  A mouse event.  Drags a VOI real time by calling a series
     *  of translations and draws.  Once the mouse is released, the
     *  positions are reset permenantly.  Also rubberbands points if
     *  the cursor indicates it by calling rubberbandVOI.
     *  @param mouseEvent   event that triggered function
     */
    public void mouseDragged( MouseEvent mouseEvent ) {
        Graphics g = getGraphics();
        int i, j, m;
        int nVOI;
        ViewVOIVector VOIs = imageActive.getVOIs();
        FileInfoBase fileInfo;
        int xS, yS;
        int distX, distY;
        int xDim, yDim;
        int zDim = 1;

        float[] lineX = new float[2];
        float[] lineY = new float[2];
        float[] lineZ = new float[2];
        float[] position;
        float[] intensity;
        String str;
        int sliceNum;
        int windowChange, levelChange;

        if ( pixBuffer == null || imageBufferActive == null || modifyFlag == false ) {
            return;
        }

        xS = (int)( mouseEvent.getX() / ( getZoomX() * resolutionX )  ); // zoomed x.  Used as cursor
        yS = (int)( mouseEvent.getY() / ( getZoomY() * resolutionY )  ); // zoomed y.  Used as cursor

        xDim = imageActive.getExtents()[0];
        yDim = imageActive.getExtents()[1];
        if ( imageActive.getNDims() >= 3 ) {
            zDim = imageActive.getExtents()[2];
        }
        if ( xS < 0 || xS >= xDim || yS < 0 || yS >= yDim ) {
            return;
        }

        try {
            if ( ( !imageActive.isColorImage() ) && ( mode == DEFAULT ) ) {
                if ( ( mouseEvent.getModifiers() & MouseEvent.BUTTON3_MASK ) != 0 ) {
                    // Dragging the mouse with the right mouse button pressed
                    // increases the window when going from left to right.
                    // Dragging the mouse with the right mouse button pressed
                    // increases the level when going from down to up.
                    setCursor( winLevelCursor );
                    if ( !winLevelSet ) {
                        winLevelSet = true;
                        if ( imageActive.getType() == ModelStorageBase.UBYTE ) {
                            minImageWin = 0;
                            maxImageWin = 255;
                        } else if ( imageActive.getType() == ModelStorageBase.BYTE ) {
                            minImageWin = -128;
                            maxImageWin = 127;
                        } else {
                            minImageWin = (float) imageActive.getMin();
                            maxImageWin = (float) imageActive.getMax();
                        }

                        minWin = Float.MAX_VALUE;
                        maxWin = -Float.MAX_VALUE;
                        for ( i = 0; i < imageBufferActive.length; i++ ) {
                            if ( imageBufferActive[i] > maxWin ) {
                                maxWin = imageBufferActive[i];
                            }
                            if ( imageBufferActive[i] < minWin ) {
                                minWin = imageBufferActive[i];
                            }
                        }

                        // THIS IS (one of) THE CULPRIT(s).  ADJUST THE FOLLOWING TO
                        // SET THE TRANSFER FUNCTION CORRECTLY (to current state)!!
                        // Set LUT min max values;
                        xWin[0] = minImageWin;
                        yWin[0] = 255;
                        zWin[0] = 0;
                        xWin[1] = minWin;
                        yWin[1] = 255;
                        zWin[1] = 0;
                        xWin[2] = maxWin;
                        yWin[2] = 0;
                        zWin[2] = 0;
                        xWin[3] = maxImageWin;
                        yWin[3] = 0;
                        zWin[3] = 0;
                        if ( imageA == imageActive ) {
                            LUTa.getTransferFunction().importArrays( xWin, yWin, 4 );
                            imageActive.notifyImageDisplayListeners( LUTa, false );
                        } else {
                            LUTb.getTransferFunction().importArrays( xWin, yWin, 4 );
                            imageActive.notifyImageDisplayListeners( LUTb, false );
                        }
                        level = ( xWin[1] + xWin[2] ) / 2.0f;
                        window = xWin[2] - xWin[1];
                        oldXS = xS;
                        oldYS = yS;
                    } // if (!winLevelSet)
                    else if ( winLevelSet && ( ( xS != oldXS ) || ( yS != oldYS ) ) ) {
                        windowChange = xS - oldXS;
                        window = window + windowChange * 4 * ( maxImageWin - minImageWin ) / ( xDim - 1 );
                        if ( window > 2 * ( maxImageWin - minImageWin ) ) {
                            window = 2 * ( maxImageWin - minImageWin );
                        } else if ( window < 1 ) {
                            window = 1;
                        }
                        levelChange = oldYS - yS;
                        level = level + levelChange * 2 * ( maxImageWin - minImageWin ) / ( yDim - 1 );
                        if ( level > maxImageWin ) {
                            level = maxImageWin;
                        } else if ( level < minImageWin ) {
                            level = minImageWin;
                        }

                        xWin[2] = level + window / 2;
                        if ( xWin[2] > maxImageWin ) {
                            yWin[2] = 255.0f * ( xWin[2] - maxImageWin ) / window;
                            xWin[2] = maxImageWin;
                        } else {
                            yWin[2] = 0.0f;
                        }

                        xWin[1] = level - window / 2;
                        if ( xWin[1] < minImageWin ) {
                            yWin[1] = 255.0f - 255.0f * ( minImageWin - xWin[1] ) / window;
                            xWin[1] = minImageWin;
                        } else {
                            yWin[1] = 255.0f;
                        }

                        // update the transfer function so the on-screen image
                        // (modelImage/viewJFrameImage) updates for the user
                        if ( imageA == imageActive ) {
                            LUTa.getTransferFunction().importArrays( xWin, yWin, 4 );
                            imageActive.notifyImageDisplayListeners( LUTa, false );
                            if ( imageA.getHistoLUTFrame() != null ) {
                                imageA.getHistoLUTFrame().update();
                            }
                        } else {
                            LUTb.getTransferFunction().importArrays( xWin, yWin, 4 );
                            imageActive.notifyImageDisplayListeners( LUTb, false );
                            if ( imageB.getHistoLUTFrame() != null ) {
                                imageB.getHistoLUTFrame().update();
                            }
                        }
                        oldXS = xS;
                        oldYS = yS;
                    } // else if (winLevelSet && ((xS != oldXS) || (yS != oldYS)))
                } // if ((mouseEvent.getModifiers() & MouseEvent.BUTTON3_MASK) != 0)
            } // if ((!imageActive.isColorImage()) && (mode == DEFAULT))
            if ( imageActive.getFileInfo( 0 ).getOrigin()[0] != 0 || imageActive.getFileInfo( 0 ).getOrigin()[1] != 0
                    || imageActive.getFileInfo( 0 ).getOrigin()[2] != 0 ) {

                fileInfo = imageActive.getFileInfo()[slice];
                String[] values = setScannerPosition( fileInfo, xS, yS, slice );

                if ( values != null ) {
                    str = "  X: " + String.valueOf( xS ) + " Y: " + String.valueOf( yS ) + "  Intensity:  "
                            + String.valueOf( imageBufferActive[yS * imageActive.getExtents()[0] + xS] ) + " Position: "
                            + values[0] + " " + values[1] + " " + values[2];
                    frame.setMessageText( str );
                    if ( ( mouseEvent.getModifiers() & MouseEvent.BUTTON2_MASK ) != 0 ) {
                        frame.getUserInterface().setDataText( "\n" + str );
                    }
                } else {
                    str = "  X: " + String.valueOf( xS ) + " Y: " + String.valueOf( yS ) + "  Intensity:  "
                            + String.valueOf( imageBufferActive[yS * imageActive.getExtents()[0] + xS] );
                    frame.setMessageText( str );
                    if ( ( mouseEvent.getModifiers() & MouseEvent.BUTTON2_MASK ) != 0 ) {
                        frame.getUserInterface().setDataText( "\n" + str );
                    }
                }
            } else {
                if ( imageActive.isColorImage() == true ) {
                    str = "  X: " + String.valueOf( xS ) + " Y: " + String.valueOf( yS ) + "  R:  "
                            + String.valueOf( imageBufferActive[4 * ( yS * imageActive.getExtents()[0] + xS ) + 1] )
                            + "  G:  "
                            + String.valueOf( imageBufferActive[4 * ( yS * imageActive.getExtents()[0] + xS ) + 2] )
                            + "  B:  "
                            + String.valueOf( imageBufferActive[4 * ( yS * imageActive.getExtents()[0] + xS ) + 3] );
                    frame.setMessageText( str );
                    if ( ( mouseEvent.getModifiers() & MouseEvent.BUTTON2_MASK ) != 0 ) {
                        frame.getUserInterface().setDataText( "\n" + str );
                    }
                } else {
                    str = "  X: " + String.valueOf( xS ) + " Y: " + String.valueOf( yS ) + "  Intensity:  "
                            + String.valueOf( imageBufferActive[yS * imageActive.getExtents()[0] + xS] );
                    frame.setMessageText( str );
                    if ( ( mouseEvent.getModifiers() & MouseEvent.BUTTON2_MASK ) != 0 ) {
                        frame.getUserInterface().setDataText( "\n" + str );
                    }
                }
            }
        } catch ( ArrayIndexOutOfBoundsException error ) {
            str = "  X: " + String.valueOf( xS ) + " Y: " + String.valueOf( yS );
            frame.setMessageText( str );
            if ( ( mouseEvent.getModifiers() & MouseEvent.BUTTON2_MASK ) != 0 ) {
                frame.getUserInterface().setDataText( "\n" + str );
            }
        }

        distX = xS - anchorPt.x; // distance from original to cursor
        distY = yS - anchorPt.y;
        int end = 1;

        nVOI = VOIs.size();

        if ( mode == MOVE ) {
            // System.out.println(" ViewJComponentEditImage.mouseDragged: nVOIs = " + nVOI);
            for ( i = 0; i < nVOI; i++ ) {
                int curveType = VOIs.VOIAt( i ).getCurveType();

                if ( VOIs.VOIAt( i ).isActive() && VOIs.VOIAt( i ).isVisible() ) {
                    if ( ( VOIs.VOIAt( i ).getCurveType() == VOI.CONTOUR
                            || VOIs.VOIAt( i ).getCurveType() == VOI.POLYLINE
                            || VOIs.VOIAt( i ).getCurveType() == VOI.LINE
                            || VOIs.VOIAt( i ).getCurveType() == VOI.PROTRACTOR )
                                    && mouseEvent.isControlDown() == false
                                    && ( mouseEvent.getModifiers() != MouseEvent.BUTTON3_MASK ) ) {

                        if ( imageActive.getNDims() < 3 ) {
                            end = 1;
                        } else {
                            end = imageActive.getExtents()[2];
                        }

                        if ( allActive ) {
                            // System.err.println("Doing shiftDown");
                            VOIs.VOIAt( i ).moveVOI( -1, xDim, yDim, zDim, distX, distY, 0 );
                            imageActive.notifyImageDisplayListeners();
                        } else {
                            // System.err.println("Not Doing shiftDown");
                            for ( int sl = 0; sl < end; sl++ ) {
                                for ( j = 0; j < VOIs.VOIAt( i ).getCurves()[sl].size(); j++ ) {
                                    boolean contains = false;
                                    boolean isActive = false;

                                    if ( curveType == VOI.CONTOUR || curveType == VOI.POLYLINE ) {
                                        // System.out.println(" j = " + j + " size = " + VOIs.VOIAt(i).getCurves()[sl].size() + " sl = " + sl + " Slice " + slice );
                                        contains = ( (VOIContour) ( VOIs.VOIAt( i ).getCurves()[sl].elementAt( j ) ) ).contains(
                                                xS, yS, true );

                                        isActive = ( (VOIContour) ( VOIs.VOIAt( i ).getCurves()[sl].elementAt( j ) ) ).isActive();
                                        // System.err.println("Contains is: " + contains + " active is: " + isActive +
                                        // " for slice " + sl);
                                    } else if ( curveType == VOI.LINE ) {
                                        contains = ( (VOILine) ( VOIs.VOIAt( i ).getCurves()[sl].elementAt( j ) ) ).contains(
                                                xS, yS, true );
                                        isActive = ( (VOILine) ( VOIs.VOIAt( i ).getCurves()[sl].elementAt( j ) ) ).isActive();
                                    } else if ( curveType == VOI.PROTRACTOR ) {
                                        contains = ( (VOIProtractor) ( VOIs.VOIAt( i ).getCurves()[sl].elementAt( j ) ) ).contains(
                                                xS, yS, true );
                                        isActive = ( (VOIProtractor) ( VOIs.VOIAt( i ).getCurves()[sl].elementAt( j ) ) ).isActive();
                                    }

                                    if ( contains && isActive ) {
                                        VOIs.VOIAt( i ).moveVOI( sl, xDim, yDim, zDim, distX, distY, 0 );
                                        imageActive.notifyImageDisplayListeners();
                                    }
                                }
                            }
                        }

                        if ( VOIs.VOIAt( i ).getContourGraph() != null && VOIs.VOIAt( i ).getContourGraph().isVisible()
                                && ( curveType == VOI.CONTOUR || curveType == VOI.POLYLINE ) ) {
                            VOI v;
                            float intensitySum;
                            int length = imageActive.getSliceSize();
                            int s;
                            int numPixels;
                            boolean foundCurve;

                            position = VOIs.VOIAt( i ).getPosition();
                            intensity = VOIs.VOIAt( i ).getIntensity();
                            float[][] rgbPositions = VOIs.VOIAt( i ).getRGBPositions();
                            float[][] rgbIntensities = VOIs.VOIAt( i ).getRGBIntensities();

                            if ( imageActive.getNDims() == 3 ) {
                                if ( imageActive.isColorImage() == true ) {
                                    try {
                                        v = VOIs.VOIAt( i );
                                        for ( s = 0, foundCurve = false; s < imageActive.getExtents()[2]; s++ ) {
                                            try {
                                                for ( int c = 0; c < 3; c++ ) {
                                                    numPixels = 0;
                                                    for ( j = 0, intensitySum = 0; j
                                                            < VOIs.VOIAt( i ).getCurves()[s].size(); j++ ) {
                                                        if ( ( (VOIContour) VOIs.VOIAt( i ).getCurves()[s].elementAt( j ) ).isActive()
                                                                || foundCurve ) {
                                                            if ( !foundCurve ) {
                                                                imageActive.exportData( s * length * 4, length * 4,
                                                                        graphImgBuff );
                                                            } // locks and releases lock
                                                            intensitySum += ( (VOIContour) ( VOIs.VOIAt( i ).getCurves()[
                                                                    s].elementAt( j ) ) ).calcRGBIntensity(
                                                                    graphImgBuff, imageActive.getExtents()[0], c );
                                                            numPixels += ( (VOIContour) ( VOIs.VOIAt( i ).getCurves()[
                                                                    s].elementAt( j ) ) ).getLastNumPixels();
                                                            foundCurve = true;
                                                        }
                                                    }
                                                    if ( foundCurve ) {
                                                        rgbPositions[c][s] = s;
                                                        if ( v.getTotalIntensity() || numPixels == 0 ) {
                                                            rgbIntensities[c][s] = intensitySum;
                                                        } else {
                                                            rgbIntensities[c][s] = intensitySum / numPixels;
                                                        }
                                                    }
                                                }
                                            } catch ( IOException error ) {
                                                MipavUtil.displayError( "Image(s) locked" );
                                                return;
                                            }
                                            foundCurve = false;
                                        }
                                        VOIs.VOIAt( i ).getContourGraph().update( rgbPositions, rgbIntensities, 0 );
                                        VOIs.VOIAt( i ).getContourGraph().setUnitsInLabel(
                                                FileInfoBase.getUnitsOfMeasureAbbrevStr(
                                                        imageActive.getFileInfo( 0 ).getUnitsOfMeasure( 0 ) ) );

                                    } catch ( OutOfMemoryError error ) {
                                        System.gc();
                                        MipavUtil.displayError( "Out of memory: ComponentEditImage.graphVOI" );
                                        return;
                                    }
                                } else {
                                    try {
                                        v = VOIs.VOIAt( i );
                                        for ( s = 0, foundCurve = false; s < imageActive.getExtents()[2]; s++ ) {
                                            try {
                                                numPixels = 0;
                                                for ( j = 0, intensitySum = 0; j < VOIs.VOIAt( i ).getCurves()[s].size(); j++ ) {
                                                    boolean isActive = ( (VOIContour) ( VOIs.VOIAt( i ).getCurves()[s].elementAt( j ) ) ).isActive();

                                                    if ( isActive || foundCurve ) {
                                                        if ( !foundCurve ) {
                                                            imageActive.exportData( s * length, length, graphImgBuff );
                                                        } // locks and releases lock
                                                        intensitySum += ( (VOIContour) ( VOIs.VOIAt( i ).getCurves()[s].elementAt( j ) ) ).calcIntensity(
                                                                graphImgBuff, imageActive.getExtents()[0] );
                                                        numPixels += ( (VOIContour) ( VOIs.VOIAt( i ).getCurves()[s].elementAt( j ) ) ).getLastNumPixels();
                                                        foundCurve = true;
                                                    }
                                                }
                                                if ( foundCurve ) {
                                                    position[s] = s;
                                                    if ( v.getTotalIntensity() || numPixels == 0 ) {
                                                        intensity[s] = intensitySum;
                                                    } else {
                                                        intensity[s] = intensitySum / numPixels;
                                                    }
                                                    foundCurve = false;
                                                }
                                            } catch ( IOException error ) {
                                                MipavUtil.displayError( "Image(s) locked" );
                                                return;
                                            }
                                        }
                                        VOIs.VOIAt( i ).getContourGraph().update( position, intensity, 0 );
                                        VOIs.VOIAt( i ).getContourGraph().setUnitsInLabel(
                                                FileInfoBase.getUnitsOfMeasureAbbrevStr(
                                                        imageActive.getFileInfo( 0 ).getUnitsOfMeasure( 0 ) ) );

                                    } catch ( OutOfMemoryError error ) {
                                        System.gc();
                                        MipavUtil.displayError( "Out of memory: ComponentEditImage.graphVOI" );
                                        return;
                                    }
                                }
                            } else if ( imageActive.getNDims() == 4 ) {
                                try {
                                    v = VOIs.VOIAt( i );
                                    for ( int t = 0; t < imageActive.getExtents()[3]; t++ ) {
                                        try {
                                            numPixels = 0;
                                            for ( s = 0, intensitySum = 0; s < imageActive.getExtents()[2]; s++ ) {
                                                imageActive.exportData( ( t * xDim * yDim * zDim ) + ( s * xDim * yDim ),
                                                        length, graphImgBuff ); // locks and releases lock
                                                for ( j = 0; j < VOIs.VOIAt( i ).getCurves()[s].size(); j++ ) {
                                                    intensitySum += ( (VOIContour) ( VOIs.VOIAt( i ).getCurves()[s].elementAt( j ) ) ).calcIntensity(
                                                            graphImgBuff, imageActive.getExtents()[0] );
                                                    numPixels += ( (VOIContour) ( VOIs.VOIAt( i ).getCurves()[s].elementAt( j ) ) ).getLastNumPixels();

                                                }
                                            }
                                            position[t] = t;
                                            if ( v.getTotalIntensity() || numPixels == 0 ) {
                                                intensity[t] = intensitySum;
                                            } else {
                                                intensity[t] = intensitySum / numPixels;
                                            }
                                        } catch ( IOException error ) {
                                            MipavUtil.displayError( "Image(s) locked" );
                                            return;
                                        }
                                    }
                                    VOIs.VOIAt( i ).getContourGraph().update( position, intensity, 0 );
                                    VOIs.VOIAt( i ).getContourGraph().setUnitsInLabel(
                                            FileInfoBase.getUnitsOfMeasureAbbrevStr(
                                                    imageActive.getFileInfo( 0 ).getUnitsOfMeasure( 0 ) ) );
                                } catch ( OutOfMemoryError error ) {
                                    System.gc();
                                    MipavUtil.displayError( "Out of memory: ComponentEditImage.graphVOI" );
                                    return;
                                }
                            }
                        }
                    } else if ( VOIs.VOIAt( i ).getCurveType() == VOI.POINT ) {
                        setCursor( crosshairCursor );
                        if ( allActive ) {
                            VOIs.VOIAt( i ).moveVOI( -1, xDim, yDim, zDim, distX, distY, 0 );
                            // System.err.println("Doing point shift down");
                        } else {
                            // System.err.println("Not doing point shift down");
                            VOIs.VOIAt( i ).moveVOI( slice, xDim, yDim, zDim, distX, distY, 0 );
                        }
                        imageActive.notifyImageDisplayListeners();

                        if ( VOIs.VOIAt( i ).getCurveType() == VOI.POINT ) {
                            Point3Df pt;

                            for ( j = 0; j < VOIs.VOIAt( i ).getCurves()[slice].size(); j++ ) {
                                if ( ( (VOIPoint) ( VOIs.VOIAt( i ).getCurves()[slice].elementAt( j ) ) ).isActive() ) {

                                    if ( imageActive.getNDims() == 3 ) {

                                        if ( imageActive.isColorImage() == true ) {

                                            for ( int s = 0; s < imageActive.getExtents()[2]; s++ ) {
                                                pt = ( (VOIPoint) ( VOIs.VOIAt( i ).getCurves()[slice].elementAt( j ) ) ).exportPoint();
                                                for ( int c = 0; c < 3; c++ ) {
                                                    if ( ptRGBPositions != null && ptRGBIntensities != null ) {
                                                        ptRGBPositions[c][s] = s;
                                                        ptRGBIntensities[c][s] = imageActive.getFloat(
                                                                (int) ( 4
                                                                        * ( s * imageActive.getExtents()[0]
                                                                        * imageActive.getExtents()[1]
                                                                                + pt.y * imageActive.getExtents()[0]
                                                                                + pt.x )
                                                                                        + c
                                                                                        + 1 ) );
                                                    }
                                                }
                                            }
                                            if ( VOIs.VOIAt( i ).getContourGraph() != null ) {
                                                VOIs.VOIAt( i ).getContourGraph().update( ptRGBPositions,
                                                        ptRGBIntensities, j );
                                                VOIs.VOIAt( i ).getContourGraph().setUnitsInLabel(
                                                        FileInfoBase.getUnitsOfMeasureAbbrevStr(
                                                                imageActive.getFileInfo( 0 ).getUnitsOfMeasure( 0 ) ) );
                                            }
                                        } else {

                                            for ( int s = 0; s < imageActive.getExtents()[2]; s++ ) {

                                                pt = ( (VOIPoint) ( VOIs.VOIAt( i ).getCurves()[slice].elementAt( j ) ) ).exportPoint();
                                                if ( ptPosition != null && ptIntensity != null ) {
                                                    ptPosition[s] = s;
                                                    ptIntensity[s] = imageActive.getFloat(
                                                            (int) ( s * imageActive.getExtents()[0]
                                                            * imageActive.getExtents()[1]
                                                                    + pt.y * imageActive.getExtents()[0] + pt.x ) );
                                                }
                                            }
                                            if ( VOIs.VOIAt( i ).getContourGraph() != null ) {
                                                VOIs.VOIAt( i ).getContourGraph().update( ptPosition, ptIntensity, j );
                                                VOIs.VOIAt( i ).getContourGraph().setUnitsInLabel(
                                                        FileInfoBase.getUnitsOfMeasureAbbrevStr(
                                                                imageActive.getFileInfo( 0 ).getUnitsOfMeasure( 0 ) ) );
                                            }
                                        }
                                    } else if ( imageActive.getNDims() == 4 ) {
                                        for ( int t = 0; t < imageActive.getExtents()[3]; t++ ) {
                                            pt = ( (VOIPoint) ( VOIs.VOIAt( i ).getCurves()[slice].elementAt( j ) ) ).exportPoint();
                                            if ( ptPosition != null && ptIntensity != null ) {
                                                ptPosition[t] = t;
                                                ptIntensity[t] = imageActive.getFloat(
                                                        (int) ( t * xDim * yDim * zDim + pt.z * xDim * yDim
                                                        + pt.y * xDim + pt.x ) );
                                            }
                                        }
                                        if ( VOIs.VOIAt( i ).getContourGraph() != null ) {
                                            VOIs.VOIAt( i ).getContourGraph().update( ptPosition, ptIntensity, j );
                                            VOIs.VOIAt( i ).getContourGraph().setUnitsInLabel(
                                                    FileInfoBase.getUnitsOfMeasureAbbrevStr(
                                                            imageActive.getFileInfo( 0 ).getUnitsOfMeasure( 0 ) ) );
                                        }
                                    }
                                }
                            }
                        }
                        break;
                    }
                }
            }

            anchorPt.x = xS;
            anchorPt.y = yS;
            if ( i == nVOI ) {
                g.dispose();
                return;
            }
            g.dispose();

            return;
        } else if ( mode == MOVE_POINT ) { // rubberband a point
            if ( mouseEvent.isShiftDown() == false ) {
                for ( i = 0; i < nVOI; i++ ) {
                    if ( VOIs.VOIAt( i ).isActive() && VOIs.VOIAt( i ).getCurveType() != VOI.POINT ) {
                        // Hides the cursor during dragging so it doesn't get in the way.
                        setCursor( blankCursor );

                        VOIs.VOIAt( i ).rubberbandVOI( xS, yS, slice, xDim, yDim, false );
                        if ( VOIs.VOIAt( i ).getCurveType() == VOI.LINE ) {
                            int length;
                            float[][] rgbPositions;
                            float[][] rgbIntensities;

                            float[][] rgbPos = null;
                            float[][] rgbInten = null;

                            VOIs.VOIAt( i ).exportArrays( lineX, lineY, lineZ, slice );

                            if ( imageActive.isColorImage() == true ) {

                                length = (int) ( Math.sqrt(
                                        ( ( lineX[1] - lineX[0] ) * ( lineX[1] - lineX[0] ) )
                                                + ( ( lineY[1] - lineY[0] ) * ( lineY[1] - lineY[0] ) ) ) );
                                rgbPositions = new float[3][length * 2 + 1];
                                rgbIntensities = new float[3][length * 2 + 1];
                                for ( int c = 0; c < 3; c++ ) {
                                    int pt = ( (VOILine) ( VOIs.VOIAt( i ).getCurves()[slice].elementAt( 0 ) ) ).findPositionAndIntensityRGB(
                                            rgbPositions[c], rgbIntensities[c], c, getActiveImageBuffer(),
                                            imageActive.getFileInfo()[slice].getResolutions(),
                                            getActiveImage().getExtents()[0], getActiveImage().getExtents()[1] );

                                    if ( c == 0 ) {
                                        rgbPos = new float[3][pt];
                                        rgbInten = new float[3][pt];
                                    }
                                    for ( m = 0; m < pt; m++ ) {
                                        rgbPos[c][m] = rgbPositions[c][m];
                                        rgbInten[c][m] = rgbIntensities[c][m];
                                    }
                                }
                                if ( VOIs.VOIAt( i ).getContourGraph() != null ) {
                                    VOIs.VOIAt( i ).getContourGraph().saveNewFunction( rgbPos, rgbInten, 0 );
                                    VOIs.VOIAt( i ).getContourGraph().setUnitsInLabel(
                                            FileInfoBase.getUnitsOfMeasureAbbrevStr(
                                                    imageActive.getFileInfo( 0 ).getUnitsOfMeasure( 0 ) ) );
                                }
                            } else {
                                length = (int) ( Math.sqrt(
                                        ( ( lineX[1] - lineX[0] ) * ( lineX[1] - lineX[0] ) )
                                                + ( ( lineY[1] - lineY[0] ) * ( lineY[1] - lineY[0] ) ) ) );
                                position = new float[length * 2 + 1];
                                intensity = new float[length * 2 + 1];
                                int pt = VOIs.VOIAt( i ).findPositionAndIntensity( slice, 0, position, intensity,
                                        imageBufferActive, imageActive.getFileInfo()[slice].getResolutions(),
                                        imageActive.getExtents()[0], imageActive.getExtents()[1] );
                                float[] pos = new float[pt];
                                float[] inten = new float[pt];

                                for ( m = 0; m < pt; m++ ) {
                                    pos[m] = position[m];
                                    inten[m] = intensity[m];
                                }
                                if ( VOIs.VOIAt( i ).getContourGraph() != null ) {
                                    VOIs.VOIAt( i ).getContourGraph().replaceFunction( pos, inten );
                                    VOIs.VOIAt( i ).getContourGraph().setUnitsInLabel(
                                            FileInfoBase.getUnitsOfMeasureAbbrevStr(
                                                    imageActive.getFileInfo( 0 ).getUnitsOfMeasure( 0 ) ) );
                                }
                            }
                        } // if (VOIs.VOIAt(i).getCurveType() == VOI.LINE)
                        break;
                    } // if( VOIs.VOIAt(i).isActive() && VOIs.VOIAt(i).getCurveType() != VOI.POINT)
                } // for (i = 0; i < nVOI; i++)
            } // end of if (mouseEvent.isShiftDown() == false)
            else { // shift key is depressed
                if ( imageActive.getNDims() >= 3 ) {
                    sliceNum = imageActive.getExtents()[2];
                } else {
                    sliceNum = 1;
                }
                for ( i = 0; i < nVOI; i++ ) {
                    if ( VOIs.VOIAt( i ).isActive() && VOIs.VOIAt( i ).getCurveType() != VOI.POINT ) {
                        for ( j = 0; j < sliceNum; j++ ) {
                            VOIs.VOIAt( i ).rubberbandVOI( xS, yS, j, xDim, yDim, false );
                        }
                        break;
                    }
                }
            } // end of else for shift key is depressed
            imageActive.notifyImageDisplayListeners();

            for ( i = 0; i < nVOI; i++ ) {
                if ( VOIs.VOIAt( i ).isActive() && VOIs.VOIAt( i ).isVisible() ) {
                    if ( ( VOIs.VOIAt( i ).getCurveType() == VOI.CONTOUR
                            || VOIs.VOIAt( i ).getCurveType() == VOI.POLYLINE )
                                    && mouseEvent.isControlDown() == false
                                    && ( mouseEvent.getModifiers() != MouseEvent.BUTTON3_MASK ) ) {

                        if ( VOIs.VOIAt( i ).getContourGraph() != null && VOIs.VOIAt( i ).getContourGraph().isVisible() ) {
                            VOI v;
                            float intensitySum;
                            int length = imageActive.getSliceSize();
                            int s;
                            int numPixels;
                            boolean foundCurve;

                            position = VOIs.VOIAt( i ).getPosition();
                            intensity = VOIs.VOIAt( i ).getIntensity();
                            float[][] rgbPositions = VOIs.VOIAt( i ).getRGBPositions();
                            float[][] rgbIntensities = VOIs.VOIAt( i ).getRGBIntensities();

                            if ( imageActive.getNDims() == 3 ) {
                                if ( imageActive.isColorImage() == true ) {
                                    try {
                                        v = VOIs.VOIAt( i );
                                        for ( s = 0, foundCurve = false; s < imageActive.getExtents()[2]; s++ ) {
                                            try {
                                                for ( int c = 0; c < 3; c++ ) {
                                                    numPixels = 0;
                                                    for ( j = 0, intensitySum = 0; j
                                                            < VOIs.VOIAt( i ).getCurves()[s].size(); j++ ) {
                                                        if ( ( (VOIContour) VOIs.VOIAt( i ).getCurves()[s].elementAt( j ) ).isActive()
                                                                || foundCurve ) {
                                                            if ( !foundCurve ) {
                                                                imageActive.exportData( s * length * 4, length * 4,
                                                                        graphImgBuff );
                                                            } // locks and releases lock
                                                            intensitySum += ( (VOIContour) ( VOIs.VOIAt( i ).getCurves()[
                                                                    s].elementAt( j ) ) ).calcRGBIntensity(
                                                                    graphImgBuff, imageActive.getExtents()[0], c );
                                                            numPixels += ( (VOIContour) ( VOIs.VOIAt( i ).getCurves()[
                                                                    s].elementAt( j ) ) ).getLastNumPixels();
                                                            foundCurve = true;
                                                        }
                                                    }
                                                    if ( foundCurve ) {
                                                        rgbPositions[c][s] = s;
                                                        if ( v.getTotalIntensity() || numPixels == 0 ) {
                                                            rgbIntensities[c][s] = intensitySum;
                                                        } else {
                                                            rgbIntensities[c][s] = intensitySum / numPixels;
                                                        }
                                                    }
                                                }
                                            } catch ( IOException error ) {
                                                MipavUtil.displayError( "Image(s) locked" );
                                                return;
                                            }
                                            foundCurve = false;
                                        }
                                        VOIs.VOIAt( i ).getContourGraph().update( rgbPositions, rgbIntensities, 0 );
                                        VOIs.VOIAt( i ).getContourGraph().setUnitsInLabel(
                                                FileInfoBase.getUnitsOfMeasureAbbrevStr(
                                                        imageActive.getFileInfo( 0 ).getUnitsOfMeasure( 0 ) ) );

                                    } catch ( OutOfMemoryError error ) {
                                        System.gc();
                                        MipavUtil.displayError( "Out of memory: ComponentEditImage.graphVOI" );
                                        return;
                                    }
                                } else {
                                    try {
                                        v = VOIs.VOIAt( i );
                                        for ( s = 0, foundCurve = false; s < imageActive.getExtents()[2]; s++ ) {
                                            try {
                                                numPixels = 0;
                                                for ( j = 0, intensitySum = 0; j < VOIs.VOIAt( i ).getCurves()[s].size(); j++ ) {
                                                    if ( ( (VOIContour) VOIs.VOIAt( i ).getCurves()[s].elementAt( j ) ).isActive()
                                                            || foundCurve ) {
                                                        if ( !foundCurve ) {
                                                            imageActive.exportData( s * length, length, graphImgBuff );
                                                        } // locks and releases lock
                                                        intensitySum += ( (VOIContour) ( VOIs.VOIAt( i ).getCurves()[s].elementAt( j ) ) ).calcIntensity(
                                                                graphImgBuff, imageActive.getExtents()[0] );
                                                        numPixels += ( (VOIContour) ( VOIs.VOIAt( i ).getCurves()[s].elementAt( j ) ) ).getLastNumPixels();
                                                        foundCurve = true;
                                                    }
                                                }
                                                if ( foundCurve ) {
                                                    position[s] = s;
                                                    if ( v.getTotalIntensity() || numPixels == 0 ) {
                                                        intensity[s] = intensitySum;
                                                    } else {
                                                        intensity[s] = intensitySum / numPixels;
                                                    }
                                                    foundCurve = false;
                                                }
                                            } catch ( IOException error ) {
                                                MipavUtil.displayError( "Image(s) locked" );
                                                return;
                                            }
                                        }
                                        VOIs.VOIAt( i ).getContourGraph().update( position, intensity, 0 );
                                        VOIs.VOIAt( i ).getContourGraph().setUnitsInLabel(
                                                FileInfoBase.getUnitsOfMeasureAbbrevStr(
                                                        imageActive.getFileInfo( 0 ).getUnitsOfMeasure( 0 ) ) );

                                    } catch ( OutOfMemoryError error ) {
                                        System.gc();
                                        MipavUtil.displayError( "Out of memory: ComponentEditImage.graphVOI" );
                                        return;
                                    }
                                }
                            } else if ( imageActive.getNDims() == 4 ) {
                                try {
                                    v = VOIs.VOIAt( i );
                                    for ( int t = 0; t < imageActive.getExtents()[3]; t++ ) {
                                        try {
                                            numPixels = 0;
                                            for ( s = 0, intensitySum = 0; s < imageActive.getExtents()[2]; s++ ) {
                                                imageActive.exportData( ( t * xDim * yDim * zDim ) + ( s * xDim * yDim ),
                                                        length, graphImgBuff ); // locks and releases lock
                                                for ( j = 0; j < VOIs.VOIAt( i ).getCurves()[s].size(); j++ ) {
                                                    intensitySum += ( (VOIContour) ( VOIs.VOIAt( i ).getCurves()[s].elementAt( j ) ) ).calcIntensity(
                                                            graphImgBuff, imageActive.getExtents()[0] );
                                                    numPixels += ( (VOIContour) ( VOIs.VOIAt( i ).getCurves()[s].elementAt( j ) ) ).getLastNumPixels();

                                                }
                                            }
                                            position[t] = t;
                                            if ( v.getTotalIntensity() || numPixels == 0 ) {
                                                intensity[t] = intensitySum;
                                            } else {
                                                intensity[t] = intensitySum / numPixels;
                                            }
                                        } catch ( IOException error ) {
                                            MipavUtil.displayError( "Image(s) locked" );
                                            return;
                                        }
                                    }
                                    VOIs.VOIAt( i ).getContourGraph().update( position, intensity, 0 );
                                    VOIs.VOIAt( i ).getContourGraph().setUnitsInLabel(
                                            FileInfoBase.getUnitsOfMeasureAbbrevStr(
                                                    imageActive.getFileInfo( 0 ).getUnitsOfMeasure( 0 ) ) );
                                } catch ( OutOfMemoryError error ) {
                                    System.gc();
                                    MipavUtil.displayError( "Out of memory: ComponentEditImage.graphVOI" );
                                    return;
                                }
                            }
                        }
                    }
                }
            }

            return;
        }
        g.dispose();
    }
}
