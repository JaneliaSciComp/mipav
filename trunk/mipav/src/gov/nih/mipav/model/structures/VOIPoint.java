package gov.nih.mipav.model.structures;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.view.Preferences;

import java.awt.*;



/**
 * Class for a point VOI, a simple extension of CurveBase.
 *
 * @version    1.0 Feb. 10, 1999
 * @author     Matthew J. McAuliffe
 * @see        VOI
 *
 * $Logfile: /mipav/src/gov/nih/mipav/model/structures/VOIPoint.java $
 * $Revision: 64 $
 * $Date: 2/24/06 3:34p $
 */
public class VOIPoint extends VOIBase {

    /** Label of the point (e.g. 1, or 2 or 3... ) */
    private String str = null;

    /** True if this point is part of a Polyline_Slice structure*/
    private boolean isPolySlice = false;

    /** True if this point is the 1st point in a slice/frame of Polyline_Slice*/
    private boolean firstSlicePoint = false;


    /** Distance is calculated and passed in by VOI for polyline_slices */
    private String totalDistanceString = null;

    /** Distance between segments shown only for active point in polyline_slices*/
    private String distanceString = null;

    /** True only for the active point of a polyline_slice structure (display's coordinates)*/
    private boolean isActivePoint = false;



    /**
     *  Default constructor
     */
    public VOIPoint() {}

    /**
     * Constructor that passes in the name (for optional name display instead of label)
     * @param voiName String the VOI parent's name
     */
    public VOIPoint(String voiName) {
        this.name = voiName;
        isPolySlice = false;
    }

    public VOIPoint(String voiName, boolean doPoly) {
        this.name = voiName;
        this.isPolySlice = doPoly;
    }


    /**
     *  Import points into VOI - Included to extend abstract method
     *  @param pt     point
     */
    public void importPoints( Vector3f[] pt ) {
        this.addElement( pt[0] );
    }

    /**
     *  Import point into VOI
     *  @param pt     point
     */
    public void importPoints( Vector3f pt ) {
        this.addElement( pt );
    }

    /**
     *  Imports a new point at the given image slice. Replaces the old point.
     *  @param point  point to be saved
     *  @param slice   index to save the point at
     */
    public void importPoint( Vector3f point, int slice ) {

        try {
            this.removeAllElements();
            this.addElement( new Vector3f( point.X, point.Y, slice ) );
            System.err.println("x: " + point.X + ", y: " + point.Y + ", z: " + slice);
        } catch ( OutOfMemoryError error ) {
            System.gc();
            throw error;
        }
    }

    /**
     *  Imports point into VOI
     *  @param x     x point
     *  @param y     y point
     *  @param z     z point
     */
    public void importArrays( int[] x, int[] y, int[] z, int i ) {
        this.addElement( new Vector3f( x[0], y[0], z[0] ) );
    }

    /**
     *  Imports point into VOI
     *  @param x     x point
     *  @param y     y point
     *  @param z     z point
     */
    public void importArrays( float[] x, float[] y, float[] z, int i ) {
        this.addElement( new Vector3f( x[0], y[0], z[0] ) );
    }

    /**
     *  Import point into  point VOI
     *  @param pt  3D point
     */
    public void importPoint( Vector3f pt ) {
        this.addElement( pt );
    }

    /**
     *  Method to access point VOI coordinate
     *  @return       3d point
     */
    public Vector3f exportPoint() {
        return (Vector3f) ( elementAt( 0 ) );
    }

    /**
     * Gets the active points location (Vector3f)
     * @return Vector3f the location
     */
    public Vector3f getActivePt() {
        return exportPoint();
    }


    /**
     *   Required to denote what points are contained by
     *   the region it encloses or is near to.
     *   @param _x   x-coordinate of the point in question
     *   @param _y   y-coordinate of the point in question
     *   @param forceReload if true export points from VOI structure
     *   @return    false for all points.
     */
    public boolean contains( int _x, int _y, boolean forceReload ) {
        return false;
    }

    /**
     *   Calculates the bounds of the point (i.e. returns the point)
     *   @param  x     two element array where x[0] = min extent of the Contour and
     *                   x[1] = max extent of the Contour in the x dimension
     *   @param  y     two element array where y[0] = min extent of the Contour and
     *                   y[1] = max extent of the Contour in the y dimension
     *   @param  z     two element array where z[0] = min extent of the Contour and
     *                   z[1] = max extent of the Contour in the z dimension
     */
    public void getBounds( int[] x, int[] y, int[] z ) {
        Vector3f pt = exportPoint();
        x[0] = x[1] = Math.round( pt.X );
        y[0] = y[1] = Math.round( pt.Y );
        z[0] = z[1] = Math.round( pt.Z );
    }

    /**
     *   Calculates the bounds of the point (i.e. returns the point)
     *   @param  x     two element array where x[0] = min extent of the Contour and
     *                   x[1] = max extent of the Contour in the x dimension
     *   @param  y     two element array where y[0] = min extent of the Contour and
     *                   y[1] = max extent of the Contour in the y dimension
     *   @param  z     two element array where z[0] = min extent of the Contour and
     *                   z[1] = max extent of the Contour in the z dimension
     */
    public void getBounds( float[] x, float[] y, float[] z ) {
        Vector3f pt = exportPoint();
        x[0] = x[1] = pt.X;
        y[0] = y[1] = pt.Y;
        z[0] = z[1] = pt.Z;
    }

    /**
     *  Abstract method: curve draws itself
     *  @param zoomX       zoom for the x coordinate
     *  @param zoomY       zoom for the y coordinate
     *  @param resolutionX X resolution (aspect ratio)
     *  @param resolutionY Y resolution (aspect ratio)
     *  @param originX     Start location of X origin
     *  @param originY     Start location of Y origin
     *  @param resols      array of pixel resolutions
     *  @param unitsOfMeasure e.g. mm for millimeters etc.
     *  @param orientation the orientation of the image slice where the VOI is to be drawn
     *  @param g           graphics to paint in
     *  @param boundingBox boolean that indicates if boundingBox is on or off
     *  @param fileInfo
     *  @param dim
     */
    public void drawSelf( float zoomX, float zoomY, float resolutionX, float resolutionY,
            float originX, float originY,
            float[] resols, int[] unitsOfMeasure, int orientation,
            Graphics g, boolean boundingBox, FileInfoBase fileInfo, int dim, int thickness ) {}

    /**
     * Return the VOI crop region's origin of rectangle.
     * @param fileInfo
     * @param dim
     * @param originX
     * @param originY
     * @param resols
     * @return  origin    rectangle origin
     */
    public float[] getOrigin( FileInfoBase fileInfo, int dim, float originX, float originY, float[] resols ) {
        return null;
    }

    /**
     *  Draws point VOI into the given graphics point
     *  @param zoomX         zoom for the x coordinate
     *  @param zoomY         zoom for the y coordinate
     *  @param resolutionX   X resolution (aspect ratio)
     *  @param resolutionY   Y resolution (aspect ratio)
     *  @param resols      array of pixel resolutions
     *  @param unitsOfMeasure e.g. mm for millimeters etc.
     *  @param g             graphics to paint in
     *  @param orientation the orientation of the image slice where the VOI is to be drawn
     *  @param solid         boolean that indicates solidity - NOT used VOIPoint
     */
    public void drawSelf( float zoomX, float zoomY, float resolutionX, float resolutionY,
            float originX, float originY,
            float[] resols, int[] unitsOfMeasure, int orientation, Graphics g, boolean solid, int thickness ) {

        int xS, yS;
        int x, y;
        int xPos, yPos;

        if ( g == null ) {
            MipavUtil.displayError( "VOIPoint.drawSelf: grapics = null" );
            return;
        }

       doName = (Preferences.is(Preferences.PREF_SHOW_VOI_NAME) && name != null);

        if ( orientation == NA || orientation == XY ) {
            // 1 -> imageDim instead of 0 -> imageDim - 1
            x = Math.round( ( (Vector3f) ( elementAt( 0 ) ) ).X );
            y = Math.round( ( (Vector3f) ( elementAt( 0 ) ) ).Y );
            xS = Math.round( ( (Vector3f) ( elementAt( 0 ) ) ).X * zoomX * resolutionX );
            yS = Math.round( ( (Vector3f) ( elementAt( 0 ) ) ).Y * zoomY * resolutionY );
        } else if ( orientation == ZY ) {
            // 1 -> imageDim instead of 0 -> imageDim - 1
            x = Math.round( ( (Vector3f) ( elementAt( 0 ) ) ).Z );
            y = Math.round( ( (Vector3f) ( elementAt( 0 ) ) ).Y );
            xS = Math.round( ( (Vector3f) ( elementAt( 0 ) ) ).Z * zoomX * resolutionX );
            yS = Math.round( ( (Vector3f) ( elementAt( 0 ) ) ).Y * zoomY * resolutionY );
        } else if ( orientation == XZ ) {
            // 1 -> imageDim instead of 0 -> imageDim - 1
            x = Math.round( ( (Vector3f) ( elementAt( 0 ) ) ).X );
            y = Math.round( ( (Vector3f) ( elementAt( 0 ) ) ).Z );
            xS = Math.round( ( (Vector3f) ( elementAt( 0 ) ) ).X * zoomX * resolutionX );
            yS = Math.round( ( (Vector3f) ( elementAt( 0 ) ) ).Z * zoomY * resolutionY );
        } else {
            // 1 -> imageDim instead of 0 -> imageDim - 1
            x = Math.round( ( (Vector3f) ( elementAt( 0 ) ) ).X );
            y = Math.round( ( (Vector3f) ( elementAt( 0 ) ) ).Y );
            xS = Math.round( ( (Vector3f) ( elementAt( 0 ) ) ).X * zoomX * resolutionX );
            yS = Math.round( ( (Vector3f) ( elementAt( 0 ) ) ).Y * zoomY * resolutionY );
        }







        if (!isPolySlice) {
            int type;


            //Create the string that will be drawn for label and name
            if (doName) {
                str = new String(name + ": (" + x + "," + y + ")");
            } else if (label != null) {
                str = new String(label + ": (" + x + "," + y + ")");
            } else {
                str = new String("(" + x + "," + y + ")");
            }

            type = 0;
            String typeStr = Preferences.getProperty(Preferences.PREF_VOI_POINT_DRAW_TYPE);
            if (typeStr != null) {
                try {
                    type = Integer.parseInt(typeStr);
                    if (type < 0 || type > 3) {
                        type = 0;
                    }
                } catch (Exception ex) {}
            }

            switch (type) {
            case 0:
                g.drawLine(xS, yS - 4, xS, yS + 4);
                g.drawLine(xS - 4, yS, xS + 4, yS);
                break;
            case 1:
                g.drawLine(xS, yS - 4, xS, yS - 1);
                g.drawLine(xS, yS + 1, xS, yS + 4);
                g.drawLine(xS - 4, yS, xS - 1, yS);
                g.drawLine(xS + 1, yS, xS + 4, yS);
                break;
            case 2:
                g.drawLine(xS - 4, yS - 4, xS + 4, yS + 4);
                g.drawLine(xS - 4, yS + 4, xS + 4, yS - 4);
                break;
            case 3:
                g.drawLine(xS - 4, yS - 4, xS - 1, yS - 1);
                g.drawLine(xS + 1, yS + 1, xS + 4, yS + 4);
                g.drawLine(xS - 4, yS + 4, xS - 1, yS + 1);
                g.drawLine(xS + 1, yS - 1, xS + 4, yS - 4);
                break;
            }

            if (active == false) {
                if (doName) {
                    if (xS < 20) {
                        g.drawString(name, xS + 10, yS);
                    } else {
                        g.drawString(name, xS - 15, yS - 5);
                    }
                } else {
                    if (xS < 20) {
                        g.drawString(label, xS + 10, yS);
                    } else {
                        g.drawString(label, xS - 15, yS - 5);
                    }
                }
            } else {

                if (type != 1 && type != 3) {
                    g.setColor(Color.black);
                    g.fillRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
                    g.setColor(Color.white);
                    g.drawRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
                }

                g.setFont(MipavUtil.font12);
                g.setColor(Color.yellow);

                xPos = xS;
                yPos = yS;
                if (xS < 70) {
                    xPos += 10;
                } else {
                    xPos -= 60;
                }

                if (yS < 30) {
                    yPos += 20;
                } else {
                    yPos -= 10;
                }

                g.drawString(str, xPos, yPos);

            }

        } else { //is poly slice

            //create the display string
            if (isActivePoint) {
                str = new String(label + ": (" + x + "," + y + ")");
            } else {
                str = new String(label);
            }

            if (active) {
                if (isActivePoint) {
                    g.setColor(Color.GREEN);
                    g.fillRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
                    g.setColor(Color.white);
                    g.drawRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
                } else {
                    g.setColor(Color.black);
                    g.fillRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
                    g.setColor(Color.white);
                    g.drawRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
                }
            } else {
                g.drawRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
                g.setColor(Color.white);
                g.fillRect((int) (xS), (int) (yS), 1, 1);
            }

            if (active) {
                g.setFont(MipavUtil.font12);
                g.setColor(Color.yellow);

                xPos = xS;
                yPos = yS;

                boolean displaySegmentDistance = true;
                if (distanceString != null) {
                	displaySegmentDistance = !(distanceString.startsWith("0.00"));
                }
                
                if (firstSlicePoint && isActivePoint) {
                    if (xS < 20) {
                        g.drawString(label, xPos + 10, yPos - 5);
                        g.drawString("total: " + totalDistanceString, xPos + 10, yPos - 18);
                        if (displaySegmentDistance)
                        	g.drawString("segment: " + distanceString, xPos + 10, yPos - 31);


                    } else {
                        g.drawString(label, xPos - 15, yPos - 5);
                        g.drawString("total: " + totalDistanceString, xPos - 15, yPos - 18);
                        if (displaySegmentDistance)
                    	    g.drawString("segment: " + distanceString, xPos - 15, yPos - 31);

                    }
                } else if (firstSlicePoint) {
                    if (xS < 20) {
                        g.drawString(label, xPos + 10, yPos - 5);
                        g.drawString("total: " + totalDistanceString, xPos + 10, yPos - 18);
                    } else {
                        g.drawString(label, xPos - 15, yPos - 5);
                        g.drawString("total: " + totalDistanceString, xPos - 15, yPos - 18);
                    }
                } else if (isActivePoint) {
                    if (xS < 20) {
                        g.drawString(label, xPos + 10, yPos - 5);
                                                
                        if (displaySegmentDistance)
                        	g.drawString("segment: " + distanceString, xPos + 10, yPos - 18);
                    }
                    else {
                        g.drawString(label, xPos - 15, yPos - 5);
                        if (displaySegmentDistance)
                        	g.drawString("segment: " + distanceString, xPos - 15, yPos - 18);
                    }
                } else {
                    if (xS < 20) {
                        g.drawString(label, xPos + 10, yPos - 5);
                    } else {
                        g.drawString(label, xPos - 15, yPos - 5);
                    }
                }
            } else {
                g.setFont(MipavUtil.font12);
                if (xS < 20) {
                    g.drawString(label, xS + 10, yS);
                } else {
                    g.drawString(label, xS - 15, yS - 5);
                }

            }

    }


    }
    /**
     *  Draws point VOI into the given graphics point for axial configuration in ViewJFrameTriImage.
     *  @param g             graphics to paint in
     *  @param x             the x coordinate
     *  @param y             the y coordinate
     *  @param positionLabel the position in FileCoordinates
     */
    public void drawAxialSelf( Graphics g, int x, int y, Vector3f positionLabel ) {
        int xPos, yPos;

        if ( g == null ) {
            MipavUtil.displayError( "VOIPoint.drawAxialSelf: grapics = null" );
            return;
        }

        doName = (Preferences.is(Preferences.PREF_SHOW_VOI_NAME) && name != null);

        //Create the string that will be drawn for label and name
        if (doName) {
            str = new String(name + ": (" +
                             positionLabel.X + "," +
                             positionLabel.Y + ")" );
        }
        else if (label != null) {
            str = new String(label + ": (" +
                             positionLabel.X + "," +
                             positionLabel.Y + ")" );
        }
        else {
            str = new String("(" +
                             positionLabel.X + "," +
                             positionLabel.Y + ")" );
        }

        g.drawLine( x, y - 5, x, y + 5 );
        g.drawLine( x - 5, y, x + 5, y );
        if ( active == false ) {

            if (doName) {
                if ( x < 20) {
                    g.drawString( name, x + 10, y );
                } else {
                    g.drawString( name, x - 15, y - 5 );
                }
            } else if (label != null){
                if ( x < 20) {
                    g.drawString( label, x + 10, y );
                } else {
                    g.drawString( label, x - 15, y - 5 );
                }
            }
        }
        //else active is true
        else {
            g.setColor( Color.black );
            g.fillRect( (int) ( x - 1.5 ), (int) ( y - 1.5 ), 4, 4 );
            g.setColor( Color.white );
            g.drawRect( (int) ( x - 1.5 ), (int) ( y - 1.5 ), 4, 4 );
            g.setFont( MipavUtil.font12 );
            g.setColor( Color.yellow );

            xPos = x;
            yPos = y;
            if ( str != null ) {
                if ( x < 70 ) {
                    xPos += 10;
                } else {
                    xPos -= 60;
                }

                if ( y < 30 ) {
                    yPos += 20;
                } else {
                    yPos -= 10;
                }

                g.drawString( str, xPos, yPos );
            }
        }
    }


    /**
     *  Moves the point to the new location. Bounds checking is performed
     *  @param xM    amount in pixels to move the line in the x direction
     *  @param yM    amount in pixels to move the line in the y direction
     *  @param zM    amount in pixels to move the line in the z direction
     *  @param xDim  x dimension maximum
     *  @param yDim  y dimension maximum
     *  @param zDim  z dimension maximum
     */
    public void moveVOIPoint( int xM, int yM, int zM, int xDim, int yDim, int zDim ) {

        Vector3f pt = exportPoint();
        if ( pt.X + xM >= xDim || pt.X + xM < 0 ) {
            return;
        }
        if ( pt.Y + yM >= yDim || pt.Y + yM < 0 ) {
            return;
        }
        if ( pt.Z + zM >= zDim || pt.Z + zM < 0 ) {
            return;
        }
        pt.X = pt.X + xM;
        pt.Y = pt.Y + yM;
        pt.Z = pt.Z + zM;
    }


    public void movePt( int x, int y ) {
        ((Vector3f) (elementAt(0))).X = x;
        ((Vector3f) (elementAt(0))).Y = y;
    }

    /**
     * Does nothing now
     * @param direction int
     * @param xDim int
     * @param yDim int
     */
    public void moveActivePt( int direction, int xDim, int yDim ) {

        int x = 0;
        int y = 0;

        switch (direction) {
            case UP:
                y -= 1;
                break;
            case LEFT:
                x -= 1;
                break;
            case DOWN:
                y += 1;
                break;
            case RIGHT:
                x += 1;
                break;
            default:
                return;
        }
        moveVOIPoint(x, y, 0, xDim, yDim, Integer.MAX_VALUE);
    }

    /**
     * does nothing now
     * @param direction int
     */
    public void cycleActivePt( int direction ) { }


    /**
     *  Sets the point at (xP,yP,zP). Bounds checking is performed
     *  @param xP    x location
     *  @param yP    y location
     *  @param zP    z location
     *  @param xDim  x dimension maximum
     *  @param yDim  y dimension maximum
     *  @param zDim  z dimension maximum
     */
    public void locateVOIPoint( int xP, int yP, int zP, int xDim, int yDim, int zDim ) {
        Vector3f pt = exportPoint();
        if ( xP >= xDim || xP < 0 ) {
            return;
        }
        if ( yP >= yDim || yP < 0 ) {
            return;
        }
        if ( zP >= zDim || zP < 0 ) {
            return;
        }
        pt.X = xP;
        pt.Y = yP;
        pt.Z = zP;
    }

    /**
     *   Translates the point of the VOI to a new location
     *   @param xT   amount to translate in the x direction
     *   @param yT   amount to translate in the y direction
     *   @param zT   amount to translate in the z direction
     */
    public void translate( float xT, float yT, float zT ) {

        Vector3f pt = exportPoint();
        pt.X = pt.X + xT;
        pt.Y = pt.Y + yT;
        pt.Z = pt.Z + zT;
    }

    /**
     *   This method gets the coordinates of the point
     *   @param coord     a float array, in which the first element is the
     *                    x coordinate and the second element is the
     *                    y coordinate
     */
    public void getCoordinates( float[] coord ) {
        Vector3f pt = exportPoint();
        coord[0] = pt.X;
        coord[1] = pt.Y;
    }


    /**
     * Tells the drawSelf how to draw each point
     * @param isFirst boolean is this the very first point of the polyline slice structure
     * @param isActPt boolean is this the active point (for drawing as green and showing segment length)
     * @param totalDistance String the total distance of the polyline slice
     * @param dist String the segment (active point to next point) distance of the polyline slice
     */
    public void setFirstPoint(boolean isFirst, boolean isActPt, String totalDistance, String dist) {
        this.firstSlicePoint = isFirst;
        this.isActivePoint = isActPt;

        this.totalDistanceString = totalDistance;
        this.distanceString = dist;
    }

    /**
     * Updates the Label for VOI Points label += num to add (for adding/removing points)
     * @param numToAdd int number to add to the label (can be negative)
     */
    public void incrementLabel( int numToAdd ) {
        try {
            int labelNum = Integer.parseInt(label);
            labelNum += numToAdd;
            label = Integer.toString(labelNum);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

}
