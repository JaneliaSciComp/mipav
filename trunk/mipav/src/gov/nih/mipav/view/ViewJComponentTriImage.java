package gov.nih.mipav.view;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.*;

import java.awt.event.*;
import java.awt.*;
import java.io.IOException;
import java.util.*;
import javax.swing.*;

/**
 * One of the component images that make up the triplanar view (AXIAL, SAGITTAL, and CORONAL views).
 * <p>
 * Notes:<br>
 * The protractor does not show up in ViewJComponentEditImage because it is
 * not registered.  If newVOI is hit, then the VOI must be registered for the points to show up
 * in EditImage.
 * <p>
 * For the ViewJFrameTriImage addPoint command, that the ViewJComponentEditImage mode is set
 * to DEFAULT while the ViewJComponentTriImage mode is set to POINT_VOI.  For the
 * Protractor command, the ViewJComponentTriImage mode is set to PROTRACTOR while the
 * ViewJComponentEditImage mode is set to DEFAULT.  For the Line command, the
 * ViewJComponentTriImage mode is set to LINE while the ViewJComponentEditImage mode is
 * set to DEFAULT.  For the ViewJFrameTriImage center command, the ViewJComponentEditImage
 * mode is set to DEFAULT while the ViewJComponentTriImage mode is set to CENTER_VOI.
 * For the boundingBox command, the ViewJComponentEdit image mode is set to DEFAULT
 * while the ViewJComponentTriImage mode is set to CUBE_BOUNDS.  For the traverse
 * command, both EditImage and TriImage have their modes set to DEFAULT.  NEW_VOI,
 * PAINT_VOI, DROPPER_PAINT, ERASER_PAINT, and PAINT_CAN are set in both EditImage and TriImage.
 *
 * @author William Gandler
 * @see    ViewJFrameTriImage
 * @see    ViewJComponentDualTriImage
 */
public class ViewJComponentTriImage extends ViewJComponentEditImage implements MouseWheelListener,
    ActionListener
{
    /**
     * The tri image frame of which this object is a component.
     */
    private ViewJFrameTriImage triImageFrame;

    /**
     * This indicates which of the 3 tri-image components we are currently in; either AXIAL, SAGITTAL, or CORONAL.
     */
    private int triComponentOrientation;

    /**
     * The x and y coordinate of the cursor position in screen space of this component.
     */
    private Point screenPt = new Point(0, 0);

    /**
     * Reordered resolutions according to orientation.  These are the resolutions of the component image being displayed.
     * <code>res[0]</code> is the horizontal resolution of the component.  <code>res[1]</code> is the vertical resolution of the component.<br>
     * If this is the AXIAL component, <code>xy0</code>, <code>xy1</code> are used to fill <code>res</code> with the resolutions of <code>imageA</code>.<br>
     * If this is the CORONAL component, <code>xz0</code>, <code>xz1</code> are used to fill <code>res</code> with the resolutions of <code>imageA</code>.<br>
     * If this is the SAGITTAL component, <code>yz0</code>, <code>yz1</code> are used to fill <code>res</code> with the resolutions of <code>imageA</code>.
     */
    private float[] res = new float[3];

    /**
     * Reordered units of measure according to orientation.  These are the units of the component image being displayed.
     * <code>unitsOfMeasure[0]</code> is the horizontal units of the component.
     * <code>unitsOfMeasure[1]</code> is the vertical units of the component.
     * <code>unitsOfMeasure[2]</code> is the units through the diplayed component image plane (the not-displayed dimension).<br>
     * If this is the AXIAL component, <code>xy0</code>, <code>xy1</code>, <code>xy2</code> are used to fill <code>unitsOfMeasure</code> with the units of <code>imageA</code>.<br>
     * If this is the CORONAL component, <code>xz0</code>, <code>xz1</code>, <code>xz2</code> are used to fill <code>unitsOfMeasure</code> with the units of <code>imageA</code>.<br>
     * If this is the SAGITTAL component, <code>yz0</code>, <code>yz1</code>, <code>yz2</code> are used to fill <code>unitsOfMeasure</code> with the units of <code>imageA</code>.
     */
    private int[] unitsOfMeasure = new int[3];

    /**
     * Whether IMAGE_A, IMAGE_B, or BOTH should be transformed and have paint commit changes applied to them.
     */
    private int imagesDone = IMAGE_A;

    /**
     * If true, show XY, XZ, or ZY orientation axes in a corner of the component.
     */
    private boolean showAxes = true;

    /**
     * If true, show crosshairs indicating the currently position within the component.
     */
    private boolean showCrosshairs = true;

    /**
     * Whether to show the center of rotation point in the volume and allow the user to change it.
     * <code>doCenter</code> is set true by center command and set false by DEFAULT and CUBE_BOUNDS modes.
     */
    private boolean doCenter = false;

    /**
     * VOI line used to measure distance and show intensity value graphs.
     */
    private VOI intensityLine = null;

    /**
     * Together with <code>moveProtractor</code>, ensures that the user can either move
     * the protractor or the line VOI, but not both at the same time.  Which one can
     * be moved depends on which one the mouse is closer to.
     * @see #moveProtractor
     */
    private boolean moveLine = false;
    private boolean moveLineEndpoint = false;

    /**
     * The protractor VOI.
     */
    private VOI voiProtractor = null;

    /**
     * Together with <code>moveLine</code>, ensures that the user can either move
     * the protractor or the line VOI, but not both at the same time.  Which one can
     * be moved depends on which one the mouse is closer to.
     * @see #moveLine
     */
    private boolean moveProtractor = false;

    /**
     * Protractor angle, ranging from -180 to 180 degrees.
     */
    private double theta = 0.0;

    /**
     * Specifies whether the user wants to show the cropping / paint bounds rectangle.
     */
    private boolean showBoundingRect = false;

    /**
     * <code>anchorPt</code> is used to determine movements.  <code>mousePressed()</code>
     * establishes the coordinates of <code>anchorPt</code>.  <code>mouseDragged()</code>
     * calculates distance from the <code>anchorPt</code> to the present location and
     * uses this distance to move an object.  Then it sets <code>anchorPt</code> to the
     * new location to repeat the process.
     */
    private Point anchorPt = new Point(0, 0);

    /**
     * In mousePressed, in mode == DEFAULT for a VOI point, the z value is saved
     * as lastZOrg.  In mouseDragged, in mode == MOVE_VOIPOINT the points at
     * the lastZOrg value can be removed and replaced with points at the new
     * z value.  The new z value is then saved in lastZOrg.  The z values are
     * all in image volume space.
     */
    private int lastZOrg;

    // color of the crosshairs
    private Color xColor, yColor, zColor;

    /**
     * Whether to show the talairach grid on the component.
     */
    private boolean showTalairachGrid = false;

    /**
     * Whether to show the talairach gridmarkers on the component.
     */
    private boolean showTalairachGridmarkers = false;

    /**
     * AC-PC: Superior Edge reference point.  Used for conversions by the dialogs that create new AC-PC images.
     */
    public static final int SUPERIOR_EDGE = 0;

    /**
     * AC-PC: Posterior Margin reference point.  Used for conversions by the dialogs that create new AC-PC images.
     */
    public static final int POSTERIOR_MARGIN = 1;

    /**
     * AC-PC: Inferior Edge reference point.  Used for conversions by the dialogs that create new AC-PC images.
     */
    public static final int INFERIOR_EDGE = 2;

    /**
     * AC-PC: First midsagittal reference point.  Used for conversions by the dialogs that create new AC-PC images.
     */
    public static final int FIRST_PT = 3;

    /**
     * AC-PC: Another midsagittal reference point.  Used for conversions by the dialogs that create new AC-PC images.
     */
    public static final int ANOTHER_PT = 4;

    /**
     * Talairach: Anterior reference point.  Used for conversions by the dialogs that create new Talairach images.
     */
    public static final int ANTERIOR_PT = 5;

    /**
     * Talairach: Posterior reference point.  Used for conversions by the dialogs that create new Talairach images.
     */
    public static final int POSTERIOR_PT = 6;

    /**
     * Talairach: Superior reference point.  Used for conversions by the dialogs that create new Talairach images.
     */
    public static final int SUPERIOR_PT = 7;

    /**
     * Talairach: Inferior reference point.  Used for conversions by the dialogs that create new Talairach images.
     */
    public static final int INFERIOR_PT = 8;

    /**
     * Talairach: Left reference point.  Used for conversions by the dialogs that create new Talairach images.
     */
    public static final int LEFT_PT = 9;

    /**
     * Talairach: Right reference point.  Used for conversions by the dialogs that create new Talairach images.
     */
    public static final int RIGHT_PT = 10;

    // used in the popup menu when the user right-clicks over a voi intensity line
    public static final String DELETE_INTENSITY_LINE = "delete_inensity_line";
    public static final String SHOW_INTENSITY_GRAPH = "show_intensity_graph";
    protected boolean intensityLineVisible = false;
    protected boolean protractorVisible = false;
    protected boolean dragCenterPt = false;

    private int dragBBpt = -1; // represents the index of the bounding box point that is currently being dragged

    private Color talairachMajorLineColor = new Color(253, 253, 253);
    private Color talairachMinorLineColor = new Color(213, 178, 183);


    /***
     * The TriImage component constructor.  Sets up the initial slice to show,
     * the placement of the crosshairs, updates the position labels, and the bounding box locations.
     * <p>
     * Note that reordering occurs in paintBuffer and pixBuffer, but reordering does not
     * occur in imageBuffer.  Thus, for the ZY orientation in hasOrientation imageDim.width and
     * imageDim.height are swapped in paintBuffer and pixBuffer, but not in imageBuffer.
     *
     * @param _frame               frame where component will be displayed
     * @param _imageA              Model of the image that will be displayed
     * @param _LUTa                LUT used to display imageA
     * @param imgBufferA           storage buffer used to display imageA
     * @param _imageB              Model of the image that will be displayed
     * @param _LUTb                LUT used to display imageB
     * @param imgBufferB           storage buffer used to display imageB
     * @param pixelBuffer          storage buffer used to build a displayable image
     * @param zoom                 initial magnification of the image
     * @param extents              initial display dimensions of the image
     * @param logMagDisplay        display log magnitude of the image
     * @param _orientation         display orientation of the image
     * @param _hasOrientation  true if image is known to be in axial or standard dicom orientation
     *                             and is displayed in ViewJFrameTriImage
     * @param _orient              a 3 integer array containing the orientation of each axis
     */
    public ViewJComponentTriImage(ViewJFrameBase _frame,
                                  ModelImage _imageA, ModelLUT _LUTa, float[] imgBufferA,
                                  ModelImage _imageB, ModelLUT _LUTb, float[] imgBufferB,
                                  int[] pixelBuffer, float zoom, int[] extents, boolean logMagDisplay,
                                  int _triComponentOrientation, boolean _hasOrientation, int[] _orient)
    {

        super(_frame, _imageA, _LUTa, imgBufferA, _imageB, _LUTb, imgBufferB, pixelBuffer, zoom, extents, logMagDisplay,
              _triComponentOrientation, _hasOrientation, _orient);

        triImageFrame = (ViewJFrameTriImage) frame;

        triComponentOrientation = _triComponentOrientation;

        removeMouseListener(popup);
        removeMouseListener(popupPt);

        res[0] = Math.abs(imageActive.getFileInfo(0).getResolutions()[axisOrder[0]]);
        res[1] = Math.abs(imageActive.getFileInfo(0).getResolutions()[axisOrder[1]]);
        res[2] = Math.abs(imageActive.getFileInfo(0).getResolutions()[axisOrder[2]]);

        if ( (res[0] == 0.0f) || (res[1] == 0.0f) || (res[2] == 0.0f))
        {
            res[0] = 1.0f;
            res[1] = 1.0f;
            res[2] = 1.0f;
        }
        unitsOfMeasure[0] = imageActive.getFileInfo(0).getUnitsOfMeasure()[axisOrder[0]];
        unitsOfMeasure[1] = imageActive.getFileInfo(0).getUnitsOfMeasure()[axisOrder[1]];
        unitsOfMeasure[2] = imageActive.getFileInfo(0).getUnitsOfMeasure()[axisOrder[2]];

        removeMouseWheelListener(this); // remove listener from superclass
        addMouseWheelListener( (ViewJComponentTriImage)this);

        if (orientation == AXIAL)
        {
            xColor = Color.yellow;
            yColor = Color.green;
            zColor = Color.red;
        }
        else if (orientation == CORONAL)
        {
            xColor = Color.yellow;
            yColor = Color.red;
            zColor = Color.green;
        }
        else // SAGITTAL
        {
            if (hasOrientation)
            {
                xColor = Color.green;
                yColor = Color.red;
            }
            else
            {
                xColor = Color.red;
                yColor = Color.green;
            }

            zColor = Color.yellow;
        }
    }

    /**
     * Sets whether IMAGEA, IMAGEB, or BOTH should be transformed and have paint commit changes applied to them.
     * @param imagesDone  how to transform the image(s) and treat paint changes
     */
    public void setImagesDone(int imagesDone)
    {
        this.imagesDone = imagesDone;
    }

    /**
     * Sets whether axes (either anatomical or x,y,z if no orientation info) are
     * shown or not in one of the corners of the component.
     * @param doShowAxes  whether to show the axes
     */
    public void displayAxes(boolean doShowAxes)
    {
        this.showAxes = doShowAxes;
    }

    /**
     * Sets whether or not crosshairs are shown in the images.
     * @param doShowCrosshairs  whether to show the tri-image crosshairs
     */
    public void displayXHairs(boolean doShowCrosshairs)
    {
        this.showCrosshairs = doShowCrosshairs;
    }

    /**
     *  Sets whether or not lines dividing the Talairach regions are shown.
     *  @param doShowTalairachGrid  whether to show the talairach region grid
     */
    public void showTalairachGrid(boolean doShowTalairachGrid)
    {
        this.showTalairachGrid = doShowTalairachGrid;
    }

    /**
     * Sets whether the talairach grid markers will be displayed
     * @param doShowTalairachGridmarkers  boolean show talairach grid markers
     */
    public void showTalairachGridmarkers(boolean doShowTalairachGridmarkers)
    {
        this.showTalairachGridmarkers = doShowTalairachGridmarkers;
    }

    /**
     * Changes the slices that this component should display.
     * @param x  which x slice to show
     * @param y  which y slice to show
     * @param z  which z slice to show
     * @see #xSlice
     * @see #ySlice
     * @see #zSlice
     */
    public void updateCrosshairPosition(int x, int y)
    {
        screenPt.x = x;
        screenPt.y = y;
    }

    /**
     * Constructs and initializes one of the 3 protractors, depending on which component this is.
     */
    public void makeProtractor()
    {
        try
        {
            int j;
            int[] x = null;
            int[] y = null;
            int[] z = null;

            x = new int[3];
            y = new int[3];
            z = new int[3];

            if (voiProtractor != null)
            {
                imageActive.notifyImageDisplayListeners();
                return;
            }

            if (triComponentOrientation == AXIAL || !hasOrientation)
            {
                voiProtractor = new VOI( (short) imageActive.getVOIs().size(), "protractor.voi",
                                        imageActive.getExtents()[axisOrder[2]], VOI.PROTRACTOR, 0.0f); // 0.0f for first segment red hue

            }
            else if (triComponentOrientation == CORONAL)
            {
                // 1.0f/3.0f for first segment green hue
                voiProtractor = new VOI( (short) imageActive.getVOIs().size(), "protractor.voi",
                                        imageActive.getExtents()[axisOrder[2]], VOI.PROTRACTOR, 0.3333f);
            }
            else
            {
                // triComponentOrientation == SAGITTAL
                voiProtractor = new VOI( (short) imageActive.getVOIs().size(), "protractor.voi",
                                        imageActive.getExtents()[axisOrder[2]], VOI.PROTRACTOR, 1.0f / 6.0f);
            }

            for (j = 0; j < imageActive.getExtents()[axisOrder[2]]; j++)
            {
                x[0] = 3 * imageDim.width / 8;
                x[1] = 4 * imageDim.width / 8;
                x[2] = 5 * imageDim.width / 8;
                y[0] = (imageDim.height - 1) / 2;
                y[1] = y[0];
                y[2] = y[0];
                z[0] = j;
                z[1] = j;
                z[2] = j;
                if (voiProtractor != null)
                {
                    voiProtractor.importCurve(x, y, z, j);
                }
                ( (VOIProtractor) (voiProtractor.getCurves()[j].elementAt(0))).setSnap(true);
                if (voiProtractor != null)
                {
                    ( (VOIProtractor) (voiProtractor.getCurves()[j].elementAt(0))).setActive(true);
                }
            } // end of for (j = 0; j < imageActive.getExtents()[idx2]; j++)
            if (voiProtractor != null)
            {
                voiProtractor.setXYDim(imageDim.width, imageDim.height);
            }
            if (voiProtractor != null)
            {
                voiProtractor.setActive(true);
            }
            repaint();
        }
        catch (OutOfMemoryError error)
        {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentTriImage.makeProtractor");
            setMode(DEFAULT);
            return;
        }
    }

    public void setProtractorVisible(boolean visible)
    {
        protractorVisible = visible;
    }

    public boolean isProtractorVisible()
    {
        return protractorVisible;
    }

    /**
     * Retrieves the center point from the frame and shows a slice intersecting the center point.
     */
    public void makeCenter()
    {
        int [] center = new int[3];
        doCenter = true;

        center[0] = triImageFrame.getCenter()[0];
        center[1] = triImageFrame.getCenter()[1];
        center[2] = triImageFrame.getCenter()[2];

        setSlice(center[axisOrder[2]]);

        if (orientation == AXIAL)
        {
            if (axisFlip[2]) slice = imageActive.getExtents()[axisOrder[2]] - slice - 1;
            triImageFrame.setAxialComponentSlice(slice);
        }
        else if (orientation == SAGITTAL)
        {
            if (axisFlip[2]) slice = imageActive.getExtents()[axisOrder[2]] - slice - 1;
            triImageFrame.setSagittalComponentSlice(slice);
        }
        else
        {
            if (axisFlip[2]) slice = imageActive.getExtents()[axisOrder[2]] - slice - 1;
            triImageFrame.setCoronalComponentSlice(slice);
        }
    }

    /**
     * Sets whether the center of rotation point should be displayed and the user should be allowed to change it.
     * @param doCenter  whether the center of rotation point should be displayed and the user should be allowed to change it
     */
    public void setDoCenter(boolean doCenter)
    {
        this.doCenter = doCenter;
    }

    /**
     * Accessor that returns the protractor angle
     * @return  the protractor angle
     */
    public double getTheta()
    {
        return theta;
    }

    /**
     * Calls <code>paintComponent</code> - reduces flicker.
     * @param g  graphics to draw with
     */
    public void update(Graphics g)
    {
        this.paintComponent(g);
    }

    /**
     * Paints the image, optionally draws the axis, the center of rotation point, the talairach grid, etc., and calls drawSelf for all VOIs.
     * @param graphics  graphics to draw with
     */
    public void paintComponent(Graphics graphics)
    {
        if (graphics == null)
        {
            Preferences.debug("Cannot paint image because graphics context is null\n", Preferences.DEBUG_MINOR);
            return;
        }

        if (img == null)
        {
            importImage(cleanImageBufferA);
            importImageB(cleanImageBufferB);
        }

        Image offscreenImage = null;
        Graphics2D offscreenGraphics2d = null;

        if (isDisplayable())
        {
            int zoomedWidth = Math.round(zoomX * img.getWidth(this) * resolutionX);
            int zoomedHeight = Math.round(zoomY * img.getHeight(this) * resolutionY);

            offscreenImage = createImage(zoomedWidth, zoomedHeight);
            offscreenGraphics2d = (Graphics2D) offscreenImage.getGraphics();
        }
        else
        {
            return;
        }

        super.paintComponent(offscreenGraphics2d);

        if (triComponentOrientation == AXIAL)
        {
            if (showBoundingRect)
            {
                drawBoundingRect_AXIAL(offscreenGraphics2d);
            }

            if (showAxes)
            {
                drawAxes_AXIAL(offscreenGraphics2d);
            }

            if (showTalairachGrid)
            {
                drawTalairachGrid_AXIAL(offscreenGraphics2d);
            }

        } // end of if (triComponentOrientation == AXIAL)
        else if (triComponentOrientation == CORONAL)
        {
            if (showBoundingRect)
            {
                drawBoundingRect_CORONAL(offscreenGraphics2d);
            }

            if (showAxes)
            {
                drawAxes_CORONAL(offscreenGraphics2d);
            }

            if (showTalairachGrid)
            {
                drawTalairachGrid_CORONAL(offscreenGraphics2d);
            }

        } // end of else if (triComponentOrientation == CORONAL)
        else if (triComponentOrientation == SAGITTAL)
        {
            if (showBoundingRect)
            {
                drawBoundingRect_SAGITTAL(offscreenGraphics2d);
            }

            if (showAxes)
            {
                drawAxes_SAGITTAL(offscreenGraphics2d);
            } // if (showAxes)

            if (showTalairachGrid)
            {
                drawTalairachGrid_SAGITTAL(offscreenGraphics2d);

            } // if (showTalairach)
        } // end of else if (triComponentOrientation == SAGITTAL)

        drawCrosshairs(offscreenGraphics2d);

        if (protractorVisible)
        {
            drawProtractor(offscreenGraphics2d);
        }

        if (intensityLineVisible)
        {
            drawVOIIntensityLine(offscreenGraphics2d);
        }

        if ( (doCenter) && (getCurrentTriImageSlice() == triImageFrame.getCenter()[axisOrder[2]]) ) {
            drawCenterMark(offscreenGraphics2d);
        }

        drawTriPlanarVOIs(offscreenGraphics2d);

        offscreenGraphics2d.dispose();

        graphics.drawImage(offscreenImage, 0, 0, null);
    }

    /**
     * This method creates a buffer that will be used to make an image of the painted area on-screen.
     * @param paintImageBuffer int[] the buffer to fill that will eventually be used to create the paint image
     */
    protected void makePaintImage(int[] paintImageBuffer)
    {
        // get the color of the paint the user has selected

        int color = getSelectedPaintColor();
        float opacity = frame.getControls().getTools().getOpacity();
        int opacityInt = (int) (opacity * 255);
        opacityInt = opacityInt << 24;

        if (slice >= 0)
        {
            int[] extents = getTriImageExtents();
            //boolean [] invert = getTriImageReversal();
            int[] iterFactors = getVolumeIterationFactors();
            int iZIndex = getCurrentTriImageSlice();

            for (int iX = 0; iX < extents[0]; iX++)
            {
                for (int iY = 0; iY < extents[1]; iY++)
                {
                    int iXIndex = iX;
                    int iYIndex = iY;
                    if (axisFlip[0])
                    {
                        iXIndex = (extents[0] - 1) - iX;
                    }
                    if (axisFlip[1])
                    {
                        iYIndex = (extents[1] - 1) - iY;
                    }
                    int iIndex = (iZIndex * iterFactors[2]) + (iYIndex * iterFactors[1]) + (iXIndex * iterFactors[0]);

                    if (paintBitmap.get(iIndex))
                    {
                        color = color & 0x00ffffff;
                        paintImageBuffer[iX + iY * extents[0]] = color | opacityInt;
                    }
                }
            }
        }
    }

    /**
     * Draws the crosshairs for this oriented tri-planar component.
     * @param offscreenGraphics2d  the graphics context to draw with
     */
    private void drawCrosshairs(Graphics2D offscreenGraphics2d)
    {
        drawCrosshairStubs(offscreenGraphics2d);

        if (showCrosshairs)
        {
            drawCrosshairLines(offscreenGraphics2d);
        }
    }

    /**
     * Convenience method called by paintComponent(). Inserted here for simplicity's sake.
     * @param offscreenGraphics2d Graphics2D
     */
    private void drawCenterMark(Graphics2D offscreenGraphics2d) {

        Point pt = getScreenCoordinates(new Point3D(triImageFrame.getCenter()[0],
                                        triImageFrame.getCenter()[1],
                                        triImageFrame.getCenter()[2]));

    //pt = getScreenCoordinates(getTriImagePosition(pt.x, pt.y));

        offscreenGraphics2d.setColor(Color.yellow);
        int centerXD = pt.x;
        int centerYD = pt.y;
        offscreenGraphics2d.drawLine(centerXD, centerYD - 5, centerXD, centerYD + 5);
        offscreenGraphics2d.drawLine(centerXD - 5, centerYD, centerXD + 5, centerYD);
        offscreenGraphics2d.setColor(Color.black);
        offscreenGraphics2d.drawLine(centerXD - 1, centerYD - 5, centerXD - 1, centerYD + 5);
        offscreenGraphics2d.drawLine(centerXD + 1, centerYD - 5, centerXD + 1, centerYD + 5);
        offscreenGraphics2d.drawLine(centerXD - 5, centerYD - 1, centerXD + 5, centerYD - 1);
        offscreenGraphics2d.drawLine(centerXD - 5, centerYD + 1, centerXD + 5, centerYD + 1);
    }

    /**
     * Convenience method called by drawCrosshairsXY(). Inserted here for simplicity's sake.
     * @param offscreenGraphics2d Graphics2D
     */
    private void drawCrosshairLines(Graphics2D offscreenGraphics2d) {

        //This snaps the crosshair to the voxel boundary
        Point pt = getScreenCoordinates(getTriImagePosition(screenPt.x, screenPt.y));

        offscreenGraphics2d.setColor(xColor);
        offscreenGraphics2d.drawLine(pt.x, 0, pt.x, pt.y - 10);
        offscreenGraphics2d.drawLine(pt.x, getSize().height, pt.x, pt.y + 10);

        offscreenGraphics2d.setColor(yColor);
        offscreenGraphics2d.drawLine(0, pt.y, pt.x - 10, pt.y);
        offscreenGraphics2d.drawLine(getSize().width, pt.y, pt.x + 10, pt.y);
    }

    /**
     * Convenience method called by drawCrosshairs. Inserted here for simplicity's sake.
     * @param offscreenGraphics2d Graphics2D
     */
    private void drawCrosshairStubs(Graphics2D offscreenGraphics2d)
    {
        //This snaps the crosshair to the voxel boundary
        Point pt = getScreenCoordinates(getTriImagePosition(screenPt.x, screenPt.y));

        // border
        offscreenGraphics2d.setColor(zColor);
        offscreenGraphics2d.drawRect(0, 0, getSize().width - 1, getSize().height - 1);

        // x stubs
        offscreenGraphics2d.setColor(xColor);
        offscreenGraphics2d.drawLine(pt.x, 0, pt.x, 10);
        offscreenGraphics2d.drawLine(pt.x, getSize().height, pt.x, getSize().height - 10);

        // y stubs
        offscreenGraphics2d.setColor(yColor);
        offscreenGraphics2d.drawLine(0, pt.y, 10, pt.y);
        offscreenGraphics2d.drawLine(getSize().width, pt.y, getSize().width - 10, pt.y);
    }

    private void drawVOIIntensityLine(Graphics2D offscreenGraphics2d)
    {
        if (intensityLine != null)
        {
            intensityLine.setActive(true);
            ( (VOILine) (intensityLine.getCurves()[slice].elementAt(0))).setActive(true);
            intensityLine.drawSelf(getZoomX(), getZoomY(), resolutionX, resolutionY, 0f, 0f, res, unitsOfMeasure, slice,
                                   triComponentOrientation, offscreenGraphics2d);
        }
    }

    private void drawProtractor(Graphics2D offscreenGraphics2d)
    {
        if (voiProtractor != null)
        {
            voiProtractor.drawSelf(getZoomX(), getZoomY(), resolutionX, resolutionY, 0f, 0f, res, unitsOfMeasure, slice,
                                   triComponentOrientation, offscreenGraphics2d);
        }
    }

    /**
     * Convenience method called by paintComponent(). Inserted here for simplicity's sake.
     * @param offscreenGraphics2d Graphics2D
     */
    private void drawTalairachGrid_SAGITTAL(Graphics2D offscreenGraphics2d)
    {
        Stroke defaultStroke = offscreenGraphics2d.getStroke();
        Stroke dashedStroke = getDashedStroke();

        int ySliceT;
        int ySliceT2;
        int zSliceT;
        int yA14, yA24, yA34;
        int yP14, yP24, yP34;
        int zI04, zI14, zI24, zI34;
        int zS18, zS28, zS38, zS48, zS58, zS68, zS78;

        // slice separating inferior from superior is usually put ViewJFrameTriImage.ATLAS_BBOX_INF_NEW or
        // 65 mm. from z = 0 at the inferior of the image.
        // In case the z dimension = (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW + ViewJFrameTriImage.ATLAS_BBOX_SUP + 1)/resZ
        // Sometimes it is put ViewJFrameTriImage.ATLAS_BBOX_INF or 55. mm from z = 0 at the inferior of
        // the image. In this case the z dimension =
        // (ViewJFrameTriImage.ATLAS_BBOX_INF + ViewJFrameTriImage.ATLAS_BBOX_SUP + 1)/resZ
        if (imageActive.getExtents()[2] * imageActive.getFileInfo(0).getResolutions()[2]
            > (ViewJFrameTriImage.ATLAS_BBOX_INF + ViewJFrameTriImage.ATLAS_BBOX_SUP + 6))
        {
            zSliceT = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - ViewJFrameTriImage.ATLAS_BBOX_INF_NEW * zoomY);
            zI04 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW - ViewJFrameTriImage.ATLAS_BBOX_INF)
                * zoomY);
            zI14 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW - ViewJFrameTriImage.ATLAS_BBOX_INF
                   + ViewJFrameTriImage.ATLAS_BBOX_INF / 4)
                * zoomY);
            zI24 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW - ViewJFrameTriImage.ATLAS_BBOX_INF
                   + ViewJFrameTriImage.ATLAS_BBOX_INF / 2)
                * zoomY);
            zI34 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW - ViewJFrameTriImage.ATLAS_BBOX_INF
                   + ViewJFrameTriImage.ATLAS_BBOX_INF * 3 / 4)
                * zoomY);
            zS18 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW + ViewJFrameTriImage.ATLAS_BBOX_SUP / 8)
                * zoomY);
            zS28 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW + ViewJFrameTriImage.ATLAS_BBOX_SUP / 4)
                * zoomY);
            zS38 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW
                   + ViewJFrameTriImage.ATLAS_BBOX_SUP * 3 / 8)
                * zoomY);
            zS48 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW + ViewJFrameTriImage.ATLAS_BBOX_SUP / 2)
                * zoomY);
            zS58 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW
                   + ViewJFrameTriImage.ATLAS_BBOX_SUP * 5 / 8)
                * zoomY);
            zS68 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW
                   + ViewJFrameTriImage.ATLAS_BBOX_SUP * 3 / 4)
                * zoomY);
            zS78 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW
                   + ViewJFrameTriImage.ATLAS_BBOX_SUP * 7 / 8)
                * zoomY);
        }
        else
        {
            zSliceT = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - ViewJFrameTriImage.ATLAS_BBOX_INF * zoomY);
            zI04 = -1;
            zI14 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - ViewJFrameTriImage.ATLAS_BBOX_INF * zoomY / 4);
            zI24 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - ViewJFrameTriImage.ATLAS_BBOX_INF * zoomY / 2);
            zI34 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - ViewJFrameTriImage.ATLAS_BBOX_INF * zoomY * 3 / 4);
            zS18 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF + ViewJFrameTriImage.ATLAS_BBOX_SUP / 8)
                * zoomY);
            zS28 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF + ViewJFrameTriImage.ATLAS_BBOX_SUP / 4)
                * zoomY);
            zS38 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF + ViewJFrameTriImage.ATLAS_BBOX_SUP * 3 / 8)
                * zoomY);
            zS48 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF + ViewJFrameTriImage.ATLAS_BBOX_SUP / 2)
                * zoomX);
            zS58 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF + ViewJFrameTriImage.ATLAS_BBOX_SUP * 5 / 8)
                * zoomY);
            zS68 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF + ViewJFrameTriImage.ATLAS_BBOX_SUP * 3 / 4)
                * zoomY);
            zS78 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF + ViewJFrameTriImage.ATLAS_BBOX_SUP * 7 / 8)
                * zoomY);
        }
        // slice separating anterior from median is always put ViewJFrameTriImage.ATLAS_BBOX_ANT or 80 mm.
        // from y = 0 at the anterior of image
        // slice separating median from posterior is always put
        // ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC or 103 mm. from the anterior of the image
        ySliceT = MipavMath.round(
            ViewJFrameTriImage.ATLAS_BBOX_ANT * zoomX);
        yA14 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_ANT * zoomX / 4);
        yA24 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_ANT * zoomX / 2);
        yA34 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_ANT * zoomX * 3 / 4);
        ySliceT2 = MipavMath.round( (ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC) *
                                   zoomX);
        yP14 = MipavMath.round(
            (ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC
             + (ViewJFrameTriImage.ATLAS_BBOX_POS - ViewJFrameTriImage.ATLAS_AC_TO_PC) / 4)
            * zoomX);
        yP24 = MipavMath.round(
            (ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC
             + (ViewJFrameTriImage.ATLAS_BBOX_POS - ViewJFrameTriImage.ATLAS_AC_TO_PC) / 2)
            * zoomX);
        yP34 = MipavMath.round(
            (ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC
             + (ViewJFrameTriImage.ATLAS_BBOX_POS - ViewJFrameTriImage.ATLAS_AC_TO_PC) * 3 / 4)
            * zoomX);

        // at pilou's request, this logic draws two lines, evenly spaced through the e-box of the talairach display
        int eBoxOneThird = (int) ((ySliceT2 - ySliceT) * 0.33f) + ySliceT;
        int eBoxTwoThirds = (int) ((ySliceT2 - ySliceT) * 0.67f) + ySliceT;

        offscreenGraphics2d.setColor(talairachMinorLineColor);
        offscreenGraphics2d.setStroke(dashedStroke);
        offscreenGraphics2d.drawLine(eBoxOneThird, 0, eBoxOneThird, getSize().height);
        offscreenGraphics2d.drawLine(eBoxTwoThirds, 0, eBoxTwoThirds, getSize().height);

        offscreenGraphics2d.setColor(talairachMajorLineColor);
        offscreenGraphics2d.setStroke(defaultStroke);
        offscreenGraphics2d.drawLine(0, zSliceT, getSize().width, zSliceT);
        offscreenGraphics2d.drawLine(ySliceT, 0, ySliceT, getSize().height);
        offscreenGraphics2d.drawLine(ySliceT2, 0, ySliceT2, getSize().height);

        offscreenGraphics2d.setColor(talairachMinorLineColor);
        offscreenGraphics2d.setStroke(dashedStroke);
        if (zI04 >= 0)
        {
            offscreenGraphics2d.drawLine(0, zI04, getSize().width, zI04);
        }
        offscreenGraphics2d.drawLine(0, zI14, getSize().width, zI14);
        offscreenGraphics2d.drawLine(0, zI24, getSize().width, zI24);
        offscreenGraphics2d.drawLine(0, zI34, getSize().width, zI34);
        offscreenGraphics2d.drawLine(0, zS18, getSize().width, zS18);
        offscreenGraphics2d.drawLine(0, zS28, getSize().width, zS28);
        offscreenGraphics2d.drawLine(0, zS38, getSize().width, zS38);
        offscreenGraphics2d.drawLine(0, zS48, getSize().width, zS48);
        offscreenGraphics2d.drawLine(0, zS58, getSize().width, zS58);
        offscreenGraphics2d.drawLine(0, zS68, getSize().width, zS68);
        offscreenGraphics2d.drawLine(0, zS78, getSize().width, zS78);

        if (showTalairachGridmarkers)
        {
            if (zI04 >= 0)
            {
                offscreenGraphics2d.setColor(talairachMajorLineColor);
                offscreenGraphics2d.setStroke(defaultStroke);
                drawStringBW("13", offscreenGraphics2d, getSize().width - 13,
                             ( (getSize().height - zI04) / 2) + zI04 + 6);
                drawStringBW("12", offscreenGraphics2d, getSize().width - 13, ( (zI04 - zI14) / 2) + zI14 + 6);
                drawStringBW("11", offscreenGraphics2d, getSize().width - 13, ( (zI14 - zI24) / 2) + zI24 + 6);
                drawStringBW("10", offscreenGraphics2d, getSize().width - 13, ( (zI24 - zI34) / 2) + zI34 + 6);
                drawStringBW("9", offscreenGraphics2d, getSize().width - 9,
                             ( (zI34 - zSliceT) / 2) + zSliceT + 6);
                drawStringBW("8", offscreenGraphics2d, getSize().width - 9, ( (zSliceT - zS18) / 2) + zS18 + 6);
                drawStringBW("7", offscreenGraphics2d, getSize().width - 9, ( (zS18 - zS28) / 2) + zS28 + 6);
                drawStringBW("6", offscreenGraphics2d, getSize().width - 9, ( (zS28 - zS38) / 2) + zS38 + 6);
                drawStringBW("5", offscreenGraphics2d, getSize().width - 9, ( (zS38 - zS48) / 2) + zS48 + 6);
                drawStringBW("4", offscreenGraphics2d, getSize().width - 9, ( (zS48 - zS58) / 2) + zS58 + 6);
                drawStringBW("3", offscreenGraphics2d, getSize().width - 9, ( (zS58 - zS68) / 2) + zS68 + 6);
                drawStringBW("2", offscreenGraphics2d, getSize().width - 9, ( (zS68 - zS78) / 2) + zS78 + 6);
                drawStringBW("1", offscreenGraphics2d, getSize().width - 9, zS78 / 2 + 6);
            }
        }

        offscreenGraphics2d.setColor(talairachMinorLineColor);
        offscreenGraphics2d.setStroke(dashedStroke);
        offscreenGraphics2d.drawLine(yA14, 0, yA14, getSize().height);
        offscreenGraphics2d.drawLine(yA24, 0, yA24, getSize().height);
        offscreenGraphics2d.drawLine(yA34, 0, yA34, getSize().height);
        offscreenGraphics2d.drawLine(yP14, 0, yP14, getSize().height);
        offscreenGraphics2d.drawLine(yP24, 0, yP24, getSize().height);
        offscreenGraphics2d.drawLine(yP34, 0, yP34, getSize().height);

        offscreenGraphics2d.setStroke(defaultStroke);

        if (showTalairachGridmarkers)
        {
            offscreenGraphics2d.setColor(talairachMajorLineColor);
            drawStringBW("A", offscreenGraphics2d, yA14 / 2, getSize().height - 2);
            drawStringBW("B", offscreenGraphics2d, ( (yA24 - yA14) / 2) + yA14, getSize().height - 2);
            drawStringBW("C", offscreenGraphics2d, ( (yA34 - yA24) / 2) + yA24, getSize().height - 2);
            drawStringBW("D", offscreenGraphics2d, ( (ySliceT - yA34) / 2) + yA34, getSize().height - 2);
            drawStringBW("E", offscreenGraphics2d, ( (ySliceT2 - ySliceT) / 2) + ySliceT, getSize().height - 2);
            drawStringBW("F", offscreenGraphics2d, ( (yP14 - ySliceT2) / 2) + ySliceT2, getSize().height - 2);
            drawStringBW("G", offscreenGraphics2d, ( (yP24 - yP14) / 2) + yP14, getSize().height - 2);
            drawStringBW("H", offscreenGraphics2d, ( (yP34 - yP24) / 2) + yP24, getSize().height - 2);
            drawStringBW("I", offscreenGraphics2d, ( (getSize().width - yP34) / 2) + yP34, getSize().height - 2);
        }
    }

    /**
     * Convenience method called by paintComponent(). Inserted here for simplicity's sake.
     * @param offscreenGraphics2d Graphics2D
     */
    private void drawTalairachGrid_AXIAL(Graphics2D offscreenGraphics2d)
    {
        Stroke defaultStroke = offscreenGraphics2d.getStroke();
        Stroke dashedStroke = getDashedStroke();

        // slice separating right from left is always put ViewJFrameTriImage.ATLAS_BBOX_LAT or 80 mm. from
        // x = 0 at the right of the image
        int xSliceT = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX);
        int x14 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX / 4);
        int x24 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX / 2);
        int x34 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX * 3 / 4);
        int x54 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX * 5 / 4);
        int x64 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX * 3 / 2);
        int x74 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX * 7 / 4);
        // slice separating anterior from median is always put ViewJFrameTriImage.ATLAS_BBOX_ANT or 80 mm.
        // from y = 0 at the anterior of image
        // slice separating median from posterior is always put
        // ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC or 103 mm. from the anterior of the image
        int ySliceT = MipavMath.round(
            ViewJFrameTriImage.ATLAS_BBOX_ANT * zoomY);
        int yA14 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_ANT * zoomY / 4);
        int yA24 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_ANT * zoomY / 2);
        int yA34 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_ANT * zoomY * 3 / 4);
        int ySliceT2 = MipavMath.round( (ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC) *
                                       zoomY);
        int yP14 = MipavMath.round(
            (ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC
             + (ViewJFrameTriImage.ATLAS_BBOX_POS - ViewJFrameTriImage.ATLAS_AC_TO_PC) / 4)
            * zoomY);
        int yP24 = MipavMath.round(
            (ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC
             + (ViewJFrameTriImage.ATLAS_BBOX_POS - ViewJFrameTriImage.ATLAS_AC_TO_PC) / 2)
            * zoomY);
        int yP34 = MipavMath.round(
            (ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC
             + (ViewJFrameTriImage.ATLAS_BBOX_POS - ViewJFrameTriImage.ATLAS_AC_TO_PC) * 3 / 4)
            * zoomY);

        offscreenGraphics2d.setColor(talairachMajorLineColor);
        offscreenGraphics2d.drawLine(xSliceT, 0, xSliceT, getSize().height);
        offscreenGraphics2d.drawLine(0, ySliceT, getSize().width, ySliceT);
        offscreenGraphics2d.drawLine(0, ySliceT2, getSize().width, ySliceT2);

        // at pilou's request, this logic draws two lines, evenly spaced through the e-box of the talairach display
        int eBoxOneThird = (int) ((ySliceT2 - ySliceT) * 0.33) + ySliceT;
        int eBoxTwoThirds = (int) ((ySliceT2 - ySliceT) * 0.67) + ySliceT;
        offscreenGraphics2d.setColor(talairachMinorLineColor);
        offscreenGraphics2d.setStroke(dashedStroke);
        offscreenGraphics2d.drawLine(0, eBoxOneThird, getSize().width, eBoxOneThird);
        offscreenGraphics2d.drawLine(0, eBoxTwoThirds, getSize().width, eBoxTwoThirds);

        //gray lines
        offscreenGraphics2d.drawLine(x14, 0, x14, getSize().height);
        offscreenGraphics2d.drawLine(x24, 0, x24, getSize().height);
        offscreenGraphics2d.drawLine(x34, 0, x34, getSize().height);
        offscreenGraphics2d.drawLine(x54, 0, x54, getSize().height);
        offscreenGraphics2d.drawLine(x64, 0, x64, getSize().height);
        offscreenGraphics2d.drawLine(x74, 0, x74, getSize().height);

        if (showTalairachGridmarkers)
        {
            offscreenGraphics2d.setColor(talairachMajorLineColor);
            offscreenGraphics2d.setStroke(defaultStroke);
            drawStringBW("d", offscreenGraphics2d, x14 / 2, getSize().height - 2);
            drawStringBW("c", offscreenGraphics2d, ( (x24 - x14) / 2) + x14, getSize().height - 2);
            drawStringBW("b", offscreenGraphics2d, ( (x34 - x24) / 2) + x24, getSize().height - 2);
            drawStringBW("a", offscreenGraphics2d, ( (xSliceT - x34) / 2) + x34, getSize().height - 2);

            drawStringBW("a", offscreenGraphics2d, ( (x54 - xSliceT) / 2) + xSliceT, getSize().height - 2);
            drawStringBW("b", offscreenGraphics2d, ( (x64 - x54) / 2) + x54, getSize().height - 2);
            drawStringBW("c", offscreenGraphics2d, ( (x74 - x64) / 2) + x64, getSize().height - 2);
            drawStringBW("d", offscreenGraphics2d, ( (getSize().width - x74) / 2) + x74, getSize().height - 2);
        }

        offscreenGraphics2d.setColor(talairachMinorLineColor);
        offscreenGraphics2d.setStroke(dashedStroke);
        offscreenGraphics2d.drawLine(0, yA14, getSize().width, yA14);
        offscreenGraphics2d.drawLine(0, yA24, getSize().width, yA24);
        offscreenGraphics2d.drawLine(0, yA34, getSize().width, yA34);
        offscreenGraphics2d.drawLine(0, yP14, getSize().width, yP14);
        offscreenGraphics2d.drawLine(0, yP24, getSize().width, yP24);
        offscreenGraphics2d.drawLine(0, yP34, getSize().width, yP34);

        offscreenGraphics2d.setStroke(defaultStroke);

        if (showTalairachGridmarkers)
        {
            offscreenGraphics2d.setColor(talairachMajorLineColor);
            drawStringBW("A", offscreenGraphics2d, getSize().width - 9, yA14 / 2);
            drawStringBW("B", offscreenGraphics2d, getSize().width - 9, ( (yA24 - yA14) / 2) + yA14);
            drawStringBW("C", offscreenGraphics2d, getSize().width - 9, ( (yA34 - yA24) / 2) + yA24);
            drawStringBW("D", offscreenGraphics2d, getSize().width - 9, ( (ySliceT - yA34) / 2) + yA34);
            drawStringBW("E", offscreenGraphics2d, getSize().width - 9, ( (ySliceT2 - ySliceT) / 2) + ySliceT);
            drawStringBW("F", offscreenGraphics2d, getSize().width - 9, ( (yP14 - ySliceT2) / 2) + ySliceT2);
            drawStringBW("G", offscreenGraphics2d, getSize().width - 9, ( (yP24 - yP14) / 2) + yP14);
            drawStringBW("H", offscreenGraphics2d, getSize().width - 9, ( (yP34 - yP24) / 2) + yP24);
            drawStringBW("I", offscreenGraphics2d, getSize().width - 9, ( (getSize().height - yP34) / 2) + yP34);
        }
    }

    /**
     * Convenience method called by paintComponent(). Inserted here for simplicity's sake.
     * @param offscreenGraphics2d Graphics2D
     */
    private void drawTalairachGrid_CORONAL(Graphics2D offscreenGraphics2d)
    {
        Stroke defaultStroke = offscreenGraphics2d.getStroke();
        Stroke dashedStroke = getDashedStroke();

        // slice separating right from left is always put ViewJFrameTriImage.ATLAS_BBOX_LAT or 80 mm. from
        // x = 0 at the right of the image
        int xSliceT = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX);
        int zSliceT;

        int x14 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX / 4);
        int x24 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX / 2);
        int x34 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX * 3 / 4);
        int x54 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX * 5 / 4);
        int x64 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX * 3 / 2);
        int x74 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX * 7 / 4);

        int zI04, zI14, zI24, zI34;
        int zS18, zS28, zS38, zS48, zS58, zS68, zS78;

        // slice separating inferior from superior is usually put ViewJFrameTriImage.ATLAS_BBOX_INF_NEW or
        // 65 mm. from z = 0 at the inferior of the image.
        // In case the z dimension = (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW + ViewJFrameTriImage.ATLAS_BBOX_SUP + 1)/resZ
        // Sometimes it is put ViewJFrameTriImage.ATLAS_BBOX_INF or 55. mm from z = 0 at the inferior of
        // the image. In this case the z dimension =
        // (ViewJFrameTriImage.ATLAS_BBOX_INF + ViewJFrameTriImage.ATLAS_BBOX_SUP + 1)/resZ
        if (imageActive.getExtents()[2] * imageActive.getFileInfo(0).getResolutions()[2]
            > (ViewJFrameTriImage.ATLAS_BBOX_INF + ViewJFrameTriImage.ATLAS_BBOX_SUP + 6))
        {
            zSliceT = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW * zoomY));
            zI04 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - ( (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW - ViewJFrameTriImage.ATLAS_BBOX_INF)
                   * zoomY));
            zI14 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW - ViewJFrameTriImage.ATLAS_BBOX_INF
                   + ViewJFrameTriImage.ATLAS_BBOX_INF / 4)
                * zoomY);
            zI24 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW - ViewJFrameTriImage.ATLAS_BBOX_INF
                   + ViewJFrameTriImage.ATLAS_BBOX_INF / 2)
                * zoomY);
            zI34 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW - ViewJFrameTriImage.ATLAS_BBOX_INF
                   + ViewJFrameTriImage.ATLAS_BBOX_INF * 3 / 4)
                * zoomY);
            zS18 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW + ViewJFrameTriImage.ATLAS_BBOX_SUP / 8)
                * zoomY);
            zS28 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW + ViewJFrameTriImage.ATLAS_BBOX_SUP / 4)
                * zoomY);
            zS38 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW
                   + ViewJFrameTriImage.ATLAS_BBOX_SUP * 3 / 8)
                * zoomY);
            zS48 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW + ViewJFrameTriImage.ATLAS_BBOX_SUP / 2)
                * zoomY);
            zS58 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW
                   + ViewJFrameTriImage.ATLAS_BBOX_SUP * 5 / 8)
                * zoomY);
            zS68 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW
                   + ViewJFrameTriImage.ATLAS_BBOX_SUP * 3 / 4)
                * zoomY);
            zS78 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW
                   + ViewJFrameTriImage.ATLAS_BBOX_SUP * 7 / 8)
                * zoomY);
        }
        else
        {
            zSliceT = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - ViewJFrameTriImage.ATLAS_BBOX_INF * zoomY);
            zI04 = -1;
            zI14 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - ViewJFrameTriImage.ATLAS_BBOX_INF * zoomY / 4);
            zI24 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - ViewJFrameTriImage.ATLAS_BBOX_INF * zoomY / 2);
            zI34 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - ViewJFrameTriImage.ATLAS_BBOX_INF * zoomY * 3 / 4);
            zS18 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF + ViewJFrameTriImage.ATLAS_BBOX_SUP / 8)
                * zoomY);
            zS28 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF + ViewJFrameTriImage.ATLAS_BBOX_SUP / 4)
                * zoomY);
            zS38 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF + ViewJFrameTriImage.ATLAS_BBOX_SUP * 3 / 8)
                * zoomY);
            zS48 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF + ViewJFrameTriImage.ATLAS_BBOX_SUP / 2)
                * zoomY);
            zS58 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF + ViewJFrameTriImage.ATLAS_BBOX_SUP * 5 / 8)
                * zoomY);
            zS68 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF + ViewJFrameTriImage.ATLAS_BBOX_SUP * 3 / 4)
                * zoomY);
            zS78 = MipavMath.round(
                (imageActive.getExtents()[2] - 1) * zoomY * resolutionY
                - (ViewJFrameTriImage.ATLAS_BBOX_INF + ViewJFrameTriImage.ATLAS_BBOX_SUP * 7 / 8)
                * zoomY);
        }

        offscreenGraphics2d.setColor(talairachMajorLineColor);
        offscreenGraphics2d.drawLine(xSliceT, 0, xSliceT, getSize().height);
        offscreenGraphics2d.drawLine(0, zSliceT, getSize().width, zSliceT);

        offscreenGraphics2d.setColor(talairachMinorLineColor);
        offscreenGraphics2d.setStroke(dashedStroke);
        offscreenGraphics2d.drawLine(x14, 0, x14, getSize().height);
        offscreenGraphics2d.drawLine(x24, 0, x24, getSize().height);
        offscreenGraphics2d.drawLine(x34, 0, x34, getSize().height);
        offscreenGraphics2d.drawLine(x54, 0, x54, getSize().height);
        offscreenGraphics2d.drawLine(x64, 0, x64, getSize().height);
        offscreenGraphics2d.drawLine(x74, 0, x74, getSize().height);

        if (showTalairachGridmarkers)
        {
            offscreenGraphics2d.setColor(talairachMajorLineColor);
            offscreenGraphics2d.setStroke(defaultStroke);
            drawStringBW("d", offscreenGraphics2d, x14 / 2, getSize().height - 2);
            drawStringBW("c", offscreenGraphics2d, ( (x24 - x14) / 2) + x14, getSize().height - 2);
            drawStringBW("b", offscreenGraphics2d, ( (x34 - x24) / 2) + x24, getSize().height - 2);
            drawStringBW("a", offscreenGraphics2d, ( (xSliceT - x34) / 2) + x34, getSize().height - 2);

            drawStringBW("a", offscreenGraphics2d, ( (x54 - xSliceT) / 2) + xSliceT, getSize().height - 2);
            drawStringBW("b", offscreenGraphics2d, ( (x64 - x54) / 2) + x54, getSize().height - 2);
            drawStringBW("c", offscreenGraphics2d, ( (x74 - x64) / 2) + x64, getSize().height - 2);
            drawStringBW("d", offscreenGraphics2d, ( (getSize().width - x74) / 2) + x74, getSize().height - 2);
        }

        offscreenGraphics2d.setColor(talairachMinorLineColor);
        offscreenGraphics2d.setStroke(dashedStroke);
        if (zI04 >= 0)
        {
            offscreenGraphics2d.drawLine(0, zI04, getSize().width, zI04);
        }
        offscreenGraphics2d.drawLine(0, zI14, getSize().width, zI14);
        offscreenGraphics2d.drawLine(0, zI24, getSize().width, zI24);
        offscreenGraphics2d.drawLine(0, zI34, getSize().width, zI34);
        offscreenGraphics2d.drawLine(0, zS18, getSize().width, zS18);
        offscreenGraphics2d.drawLine(0, zS28, getSize().width, zS28);
        offscreenGraphics2d.drawLine(0, zS38, getSize().width, zS38);
        offscreenGraphics2d.drawLine(0, zS48, getSize().width, zS48);
        offscreenGraphics2d.drawLine(0, zS58, getSize().width, zS58);
        offscreenGraphics2d.drawLine(0, zS68, getSize().width, zS68);
        offscreenGraphics2d.drawLine(0, zS78, getSize().width, zS78);

        offscreenGraphics2d.setStroke(defaultStroke);

        if (showTalairachGridmarkers)
        {
            if (zI04 >= 0)
            {
                offscreenGraphics2d.setColor(talairachMajorLineColor);
                drawStringBW("13", offscreenGraphics2d, getSize().width - 13,
                             ( (getSize().height - zI04) / 2) + zI04 + 6);
                drawStringBW("12", offscreenGraphics2d, getSize().width - 13, ( (zI04 - zI14) / 2) + zI14 + 6);
                drawStringBW("11", offscreenGraphics2d, getSize().width - 13, ( (zI14 - zI24) / 2) + zI24 + 6);
                drawStringBW("10", offscreenGraphics2d, getSize().width - 13, ( (zI24 - zI34) / 2) + zI34 + 6);
                drawStringBW("9", offscreenGraphics2d, getSize().width - 9,
                             ( (zI34 - zSliceT) / 2) + zSliceT + 6);
                drawStringBW("8", offscreenGraphics2d, getSize().width - 9, ( (zSliceT - zS18) / 2) + zS18 + 6);
                drawStringBW("7", offscreenGraphics2d, getSize().width - 9, ( (zS18 - zS28) / 2) + zS28 + 6);
                drawStringBW("6", offscreenGraphics2d, getSize().width - 9, ( (zS28 - zS38) / 2) + zS38 + 6);
                drawStringBW("5", offscreenGraphics2d, getSize().width - 9, ( (zS38 - zS48) / 2) + zS48 + 6);
                drawStringBW("4", offscreenGraphics2d, getSize().width - 9, ( (zS48 - zS58) / 2) + zS58 + 6);
                drawStringBW("3", offscreenGraphics2d, getSize().width - 9, ( (zS58 - zS68) / 2) + zS68 + 6);
                drawStringBW("2", offscreenGraphics2d, getSize().width - 9, ( (zS68 - zS78) / 2) + zS78 + 6);
                drawStringBW("1", offscreenGraphics2d, getSize().width - 9, zS78 / 2 + 6);
            }
        }
    }

    private BasicStroke getDashedStroke()
    {
        BasicStroke stroke = new BasicStroke(1.0f, BasicStroke.CAP_BUTT,
                                             BasicStroke.JOIN_MITER,
                                             1.0f, new float[] {1.0f}, 0.0f);

        return stroke;
    }


    /**
     * Convenience method called by paintComponent(). Inserted here for simplicity's sake.
     * @param offscreenGraphics2d Graphics2D
     */
    private void drawAxes_SAGITTAL(Graphics2D offscreenGraphics2d)
    {
        if (hasOrientation)
        {
            offscreenGraphics2d.setColor(xColor);
            int componentHeight = getSize().height;
            offscreenGraphics2d.drawString("P", 45, componentHeight - 6);
            offscreenGraphics2d.drawLine(10, componentHeight - 12, 39, componentHeight - 12);
            offscreenGraphics2d.drawLine(10, componentHeight - 11, 40, componentHeight - 11);
            offscreenGraphics2d.drawLine(10, componentHeight - 10, 39, componentHeight - 10);
            offscreenGraphics2d.drawLine(35, componentHeight - 16, 40, componentHeight - 11);
            offscreenGraphics2d.drawLine(35, componentHeight - 6, 40, componentHeight - 11);

            offscreenGraphics2d.setColor(yColor);
            offscreenGraphics2d.drawString("S", 10, componentHeight - 46);
            offscreenGraphics2d.drawLine(9, componentHeight - 11, 9, componentHeight - 40);
            offscreenGraphics2d.drawLine(10, componentHeight - 11, 10, componentHeight - 41);
            offscreenGraphics2d.drawLine(11, componentHeight - 11, 11, componentHeight - 40);
            offscreenGraphics2d.drawLine(5, componentHeight - 36, 10, componentHeight - 41);
            offscreenGraphics2d.drawLine(15, componentHeight - 36, 10, componentHeight - 41);
        }
        else
        {
            offscreenGraphics2d.setColor(xColor);
            offscreenGraphics2d.drawString("Z", 45, 15);
            offscreenGraphics2d.drawLine(10, 9, 39, 9);
            offscreenGraphics2d.drawLine(10, 10, 40, 10);
            offscreenGraphics2d.drawLine(10, 11, 39, 11);
            offscreenGraphics2d.drawLine(35, 5, 40, 10);
            offscreenGraphics2d.drawLine(35, 15, 40, 10);

            offscreenGraphics2d.setColor(yColor);
            offscreenGraphics2d.drawString("Y", 10, 55);
            offscreenGraphics2d.drawLine(9, 10, 9, 39);
            offscreenGraphics2d.drawLine(10, 10, 10, 40);
            offscreenGraphics2d.drawLine(11, 10, 11, 39);
            offscreenGraphics2d.drawLine(5, 35, 10, 40);
            offscreenGraphics2d.drawLine(15, 35, 10, 40);
        }
    }

    /**
     * Convenience method called by paintComponent(). Inserted here for simplicity's sake.
     * @param offscreenGraphics2d Graphics2D
     */
    private void drawAxes_AXIAL(Graphics2D offscreenGraphics2d)
    {
        offscreenGraphics2d.setColor(xColor);
        if (hasOrientation)
        {
            offscreenGraphics2d.drawString("L", 45, 15);
        }
        else
        {
            offscreenGraphics2d.drawString("X", 45, 15);
        }

        offscreenGraphics2d.drawLine(10, 9, 39, 9);
        offscreenGraphics2d.drawLine(10, 10, 40, 10);
        offscreenGraphics2d.drawLine(10, 11, 39, 11);
        offscreenGraphics2d.drawLine(35, 5, 40, 10);
        offscreenGraphics2d.drawLine(35, 15, 40, 10);

        offscreenGraphics2d.setColor(yColor);
        if (hasOrientation)
        {
            offscreenGraphics2d.drawString("P", 10, 55);
        }
        else
        {
            offscreenGraphics2d.drawString("Y", 10, 55);
        }

        offscreenGraphics2d.drawLine(9, 10, 9, 39);
        offscreenGraphics2d.drawLine(10, 10, 10, 40);
        offscreenGraphics2d.drawLine(11, 10, 11, 39);
        offscreenGraphics2d.drawLine(5, 35, 10, 40);
        offscreenGraphics2d.drawLine(15, 35, 10, 40);
    }

    /**
     * Convenience method called by paintComponent(). Inserted here for simplicity's sake.
     * @param offscreenGraphics2d Graphics2D
     */
    private void drawAxes_CORONAL(Graphics2D offscreenGraphics2d)
    {
        if (hasOrientation)
        {
            offscreenGraphics2d.setColor(xColor);
            int componentHeight = getSize().height;
            offscreenGraphics2d.drawString("L", 45, componentHeight - 6);
            offscreenGraphics2d.drawLine(10, componentHeight - 12, 39, componentHeight - 12);
            offscreenGraphics2d.drawLine(10, componentHeight - 11, 40, componentHeight - 11);
            offscreenGraphics2d.drawLine(10, componentHeight - 10, 39, componentHeight - 10);
            offscreenGraphics2d.drawLine(35, componentHeight - 16, 40, componentHeight - 11);
            offscreenGraphics2d.drawLine(35, componentHeight - 6, 40, componentHeight - 11);

            offscreenGraphics2d.setColor(yColor);
            offscreenGraphics2d.drawString("S", 10, componentHeight - 46);
            offscreenGraphics2d.drawLine(9, componentHeight - 11, 9, componentHeight - 40);
            offscreenGraphics2d.drawLine(10, componentHeight - 11, 10, componentHeight - 41);
            offscreenGraphics2d.drawLine(11, componentHeight - 11, 11, componentHeight - 40);
            offscreenGraphics2d.drawLine(5, componentHeight - 36, 10, componentHeight - 41);
            offscreenGraphics2d.drawLine(15, componentHeight - 36, 10, componentHeight - 41);
        }
        else
        { // not known to be axial ordering
            offscreenGraphics2d.setColor(xColor);
            offscreenGraphics2d.drawString("X", 45, 15);
            offscreenGraphics2d.drawLine(10, 9, 39, 9);
            offscreenGraphics2d.drawLine(10, 10, 40, 10);
            offscreenGraphics2d.drawLine(10, 11, 39, 11);
            offscreenGraphics2d.drawLine(35, 5, 40, 10);
            offscreenGraphics2d.drawLine(35, 15, 40, 10);

            offscreenGraphics2d.setColor(yColor);
            offscreenGraphics2d.drawString("Z", 10, 55);
            offscreenGraphics2d.drawLine(9, 10, 9, 39);
            offscreenGraphics2d.drawLine(10, 10, 10, 40);
            offscreenGraphics2d.drawLine(11, 10, 11, 39);
            offscreenGraphics2d.drawLine(5, 35, 10, 40);
            offscreenGraphics2d.drawLine(15, 35, 10, 40);
        } // else not known to be axial ordering
    }

    /**
     * The following is a utlity method for drawing VOIs during this object's paintComponent method.
     * It was factored out of paintComponent for simplicity's sake. -- lorsino
     *
     * @param offscreenGraphics2d the graphics context to draw in
     * @param yInvert whether to invert the y-coordinate
     */
    private void drawTriPlanarVOIs(Graphics2D offscreenGraphics2d)
    {
        ViewVOIVector VOIs = (ViewVOIVector) imageA.getVOIs().clone();

        if (this == triImageFrame.getTriImage(ViewJFrameTriImage.AXIAL_AB) ||
            this == triImageFrame.getTriImage(ViewJFrameTriImage.SAGITTAL_AB) ||
            this == triImageFrame.getTriImage(ViewJFrameTriImage.CORONAL_AB))
        {
            if (imageB != null)
            {
                VOIs.addAll((ViewVOIVector) imageB.getVOIs().clone());
            }
        }


        if (VOIs != null)
        {
            int nVOI = VOIs.size();
            for (int i = nVOI - 1; i >= 0; i--)
            {
                if (VOIs.VOIAt(i).getCurveType() == VOI.POINT)
                {
                    for (int k = 0; k < imageActive.getExtents()[2]; k++)
                    {
                        Point3Df[] voiPoints = VOIs.VOIAt(i).exportPoints(k);

                        for (int j = 0; j < voiPoints.length; j++)
                        {
                            Point pt = getScreenCoordinates(new Point3D((int) voiPoints[j].x,
                                        (int) voiPoints[j].y, (int) voiPoints[j].z));

                            int flipSlice = slice;
                            if (axisFlip[2] == true)
                            {
                                flipSlice = imageActive.getExtents()[axisOrder[2]] - slice - 1;
                            }

                            int [] orderedVOIPoint = new int [] {(int) voiPoints[j].x, (int) voiPoints[j].y, (int) voiPoints[j].z};

                            if ( (!(pt.x == -1 && pt.y == -1)) && orderedVOIPoint[axisOrder[2]] == flipSlice )
                            {
                                offscreenGraphics2d.setColor(VOIs.VOIAt(i).getColor());
                                if (hasOrientation)
                                {
                                    Point label = getVOILabel( (int) voiPoints[j].x, (int) voiPoints[j].y,
                                        (int) voiPoints[j].z);

                                    ( (VOIPoint) (VOIs.VOIAt(i).getCurves()[k].elementAt(j))).drawAxialSelf(
                                        offscreenGraphics2d, pt.x, pt.y,
                                        label.x, label.y);
                                } // if (hasOrientation)
                                else
                                {
                                    ( (VOIPoint) (VOIs.VOIAt(i).getCurves()[k].elementAt(j))).drawSelf(zoomX,
                                        zoomY, resolutionX, resolutionY, 0f, 0f, res, unitsOfMeasure,
                                        triComponentOrientation, offscreenGraphics2d, true);
                                }
                            }
                        }
                    }
                }
            }
        } // if (VOIs != null)
    }

    /**
     * Draws the cropping rectangle in the ZY plane.
     * @param graphics  the graphics object to draw with
     */
    private void drawBoundingRect_SAGITTAL(Graphics graphics)
    {
        graphics.setColor(Color.red.darker());

        int [] indices = new int[4];

        if (getOriginalOrientation() == CORONAL)
        {
            indices[0] = ViewJFrameTriImage.UPPER_LEFT_BACK;
            indices[1] = ViewJFrameTriImage.UPPER_LEFT_FRONT;
            indices[2] = ViewJFrameTriImage.LOWER_LEFT_FRONT;
            indices[3] = ViewJFrameTriImage.LOWER_LEFT_BACK;
        }
        else if (getOriginalOrientation() == AXIAL)
        {
            indices[0] = ViewJFrameTriImage.UPPER_LEFT_BACK;
            indices[1] = ViewJFrameTriImage.UPPER_LEFT_FRONT;
            indices[2] = ViewJFrameTriImage.LOWER_LEFT_FRONT;
            indices[3] = ViewJFrameTriImage.LOWER_LEFT_BACK;
        }
        else if (getOriginalOrientation() == SAGITTAL)
        {
            indices[0] = ViewJFrameTriImage.UPPER_LEFT_FRONT;
            indices[1] = ViewJFrameTriImage.UPPER_RIGHT_FRONT;
            indices[2] = ViewJFrameTriImage.LOWER_RIGHT_FRONT;
            indices[3] = ViewJFrameTriImage.LOWER_LEFT_FRONT;
        }
        else if (!hasOrientation)
        {
            indices[0] = ViewJFrameTriImage.UPPER_LEFT_BACK;
            indices[1] = ViewJFrameTriImage.UPPER_LEFT_FRONT;
            indices[2] = ViewJFrameTriImage.LOWER_LEFT_FRONT;
            indices[3] = ViewJFrameTriImage.LOWER_LEFT_BACK;
        }

        Point3D [] boundingBoxPoints = triImageFrame.getBoundingBoxPoints();

        Point screenPoint1 = getScreenCoordinates(boundingBoxPoints[indices[0]]);
        Point screenPoint2 = getScreenCoordinates(boundingBoxPoints[indices[1]]);
        Point screenPoint3 = getScreenCoordinates(boundingBoxPoints[indices[2]]);
        Point screenPoint4 = getScreenCoordinates(boundingBoxPoints[indices[3]]);

        graphics.drawLine(screenPoint1.x, screenPoint1.y, screenPoint2.x, screenPoint2.y);
        graphics.drawLine(screenPoint2.x, screenPoint2.y, screenPoint3.x, screenPoint3.y);
        graphics.drawLine(screenPoint3.x, screenPoint3.y, screenPoint4.x, screenPoint4.y);
        graphics.drawLine(screenPoint4.x, screenPoint4.y, screenPoint1.x, screenPoint1.y);

        graphics.fillRect(screenPoint1.x - 2, screenPoint1.y - 2, 4, 4);
        graphics.fillRect(screenPoint2.x - 2, screenPoint2.y - 2, 4, 4);
        graphics.fillRect(screenPoint3.x - 2, screenPoint3.y - 2, 4, 4);
        graphics.fillRect(screenPoint4.x - 2, screenPoint4.y - 2, 4, 4);

        if (true) return;


        /*String widthString;
        String heightString;
        String measuredWidthString;
        String measuredHeightString;
        float measuredWidth;
        float measuredHeight;
        String xUnitsString;
        String yUnitsString;
        int width;
        int height;
        int lowDY;
        int highDY;
        int lowDZ;
        int highDZ;

        graphics.setColor(Color.red.darker());



        width = (int) (highZ - lowZ);
        height = (int) (highY - lowY);

        widthString = String.valueOf(width);
        heightString = String.valueOf(height);

        if (hasOrientation)
        {
            measuredWidth = (highZ - lowZ) * res[1];
            measuredHeight = (highY - lowY) * res[0];
        }
        else
        {
            measuredWidth = (highZ - lowZ) * res[0];
            measuredHeight = (highY - lowY) * res[1];
        }

        xUnitsString = FileInfoBase.getUnitsOfMeasureAbbrevStr(unitsOfMeasure[0]);
        yUnitsString = FileInfoBase.getUnitsOfMeasureAbbrevStr(unitsOfMeasure[1]);

        measuredWidthString = String.valueOf(measuredWidth) + " " + xUnitsString;
        measuredHeightString = String.valueOf(measuredHeight) + " " + yUnitsString;

        graphics.setColor(Color.black);
        if (!hasOrientation)
        {
            if ( (lowDY - 45) < 0)
            {
                graphics.drawString(measuredWidthString, (lowDZ + (highDZ - lowDZ) / 2) - 20, lowDY + 21);
                graphics.drawString(measuredWidthString, (lowDZ + (highDZ - lowDZ) / 2) - 20, lowDY + 19);
                graphics.drawString(measuredWidthString, (lowDZ + (highDZ - lowDZ) / 2) - 21, lowDY + 20);
                graphics.drawString(measuredWidthString, (lowDZ + (highDZ - lowDZ) / 2) - 19, lowDY + 20);
                graphics.drawString(widthString, (lowDZ + (highDZ - lowDZ) / 2) - 20, lowDY + 36);
                graphics.drawString(widthString, (lowDZ + (highDZ - lowDZ) / 2) - 20, lowDY + 34);
                graphics.drawString(widthString, (lowDZ + (highDZ - lowDZ) / 2) - 21, lowDY + 35);
                graphics.drawString(widthString, (lowDZ + (highDZ - lowDZ) / 2) - 19, lowDY + 35);

                graphics.setColor(Color.white);
                graphics.drawString(measuredWidthString, (lowDZ + (highDZ - lowDZ) / 2) - 20, lowDY + 20);
                graphics.drawString(widthString, (lowDZ + (highDZ - lowDZ) / 2) - 20, lowDY + 35);
            }
            else
            {
                graphics.drawString(measuredWidthString, (lowDZ + (highDZ - lowDZ) / 2) - 20, lowDY - 24);
                graphics.drawString(measuredWidthString, (lowDZ + (highDZ - lowDZ) / 2) - 20, lowDY - 26);
                graphics.drawString(measuredWidthString, (lowDZ + (highDZ - lowDZ) / 2) - 21, lowDY - 25);
                graphics.drawString(measuredWidthString, (lowDZ + (highDZ - lowDZ) / 2) - 19, lowDY - 25);
                graphics.drawString(widthString, (lowDZ + (highDZ - lowDZ) / 2) - 20, lowDY - 9);
                graphics.drawString(widthString, (lowDZ + (highDZ - lowDZ) / 2) - 20, lowDY - 11);
                graphics.drawString(widthString, (lowDZ + (highDZ - lowDZ) / 2) - 21, lowDY - 10);
                graphics.drawString(widthString, (lowDZ + (highDZ - lowDZ) / 2) - 19, lowDY - 10);

                graphics.setColor(Color.white);
                graphics.drawString(measuredWidthString, (lowDZ + (highDZ - lowDZ) / 2) - 20, lowDY - 25);
                graphics.drawString(widthString, (lowDZ + (highDZ - lowDZ) / 2) - 20, lowDY - 10);
            }

            graphics.setColor(Color.black);
            if ( (lowDZ - 40) < 0)
            {
                graphics.drawString(measuredHeightString, lowDZ + 10, lowDY + 10 + (highDY - lowDY) / 2 + 1);
                graphics.drawString(measuredHeightString, lowDZ + 10, lowDY + 10 + (highDY - lowDY) / 2 - 1);
                graphics.drawString(measuredHeightString, lowDZ + 9, lowDY + 10 + (highDY - lowDY) / 2);
                graphics.drawString(measuredHeightString, lowDZ + 11, lowDY + 10 + (highDY - lowDY) / 2);
                graphics.drawString(heightString, lowDZ + 10, lowDY + 25 + (highDY - lowDY) / 2 + 1);
                graphics.drawString(heightString, lowDZ + 10, lowDY + 25 + (highDY - lowDY) / 2 - 1);
                graphics.drawString(heightString, lowDZ + 9, lowDY + 25 + (highDY - lowDY) / 2);
                graphics.drawString(heightString, lowDZ + 11, lowDY + 25 + (highDY - lowDY) / 2);

                graphics.setColor(Color.white);
                graphics.drawString(measuredHeightString, lowDZ + 10, lowDY + 10 + (highDY - lowDY) / 2);
                graphics.drawString(heightString, lowDZ + 10, lowDY + 25 + (highDY - lowDY) / 2);
            }
            else
            {
                graphics.drawString(measuredHeightString, lowDZ - 35, lowDY + 10 + (highDY - lowDY) / 2 + 1);
                graphics.drawString(measuredHeightString, lowDZ - 35, lowDY + 10 + (highDY - lowDY) / 2 - 1);
                graphics.drawString(measuredHeightString, lowDZ - 36, lowDY + 10 + (highDY - lowDY) / 2);
                graphics.drawString(measuredHeightString, lowDZ - 34, lowDY + 10 + (highDY - lowDY) / 2);
                graphics.drawString(heightString, lowDZ - 35, lowDY + 25 + (highDY - lowDY) / 2 + 1);
                graphics.drawString(heightString, lowDZ - 35, lowDY + 25 + (highDY - lowDY) / 2 - 1);
                graphics.drawString(heightString, lowDZ - 36, lowDY + 25 + (highDY - lowDY) / 2);
                graphics.drawString(heightString, lowDZ - 34, lowDY + 25 + (highDY - lowDY) / 2);

                graphics.setColor(Color.white);
                graphics.drawString(measuredHeightString, lowDZ - 35, lowDY + 10 + (highDY - lowDY) / 2);
                graphics.drawString(heightString, lowDZ - 35, lowDY + 25 + (highDY - lowDY) / 2);
            }
        } // if (!hasOrientation)
        else
        { // hasOrientation
            if ( (highDY - 45) < 0)
            {
                graphics.drawString(measuredHeightString, (lowDZ + (highDZ - lowDZ) / 2) - 20, highDY + 21);
                graphics.drawString(measuredHeightString, (lowDZ + (highDZ - lowDZ) / 2) - 20, highDY + 19);
                graphics.drawString(measuredHeightString, (lowDZ + (highDZ - lowDZ) / 2) - 21, highDY + 20);
                graphics.drawString(measuredHeightString, (lowDZ + (highDZ - lowDZ) / 2) - 19, highDY + 20);
                graphics.drawString(heightString, (lowDZ + (highDZ - lowDZ) / 2) - 20, highDY + 36);
                graphics.drawString(heightString, (lowDZ + (highDZ - lowDZ) / 2) - 20, highDY + 34);
                graphics.drawString(heightString, (lowDZ + (highDZ - lowDZ) / 2) - 21, highDY + 35);
                graphics.drawString(heightString, (lowDZ + (highDZ - lowDZ) / 2) - 19, highDY + 35);

                graphics.setColor(Color.white);
                graphics.drawString(measuredHeightString, (lowDZ + (highDZ - lowDZ) / 2) - 20, highDY + 20);
                graphics.drawString(heightString, (lowDZ + (highDZ - lowDZ) / 2) - 20, highDY + 35);
            }
            else
            {
                graphics.drawString(measuredHeightString, (lowDZ + (highDZ - lowDZ) / 2) - 20, highDY - 24);
                graphics.drawString(measuredHeightString, (lowDZ + (highDZ - lowDZ) / 2) - 20, highDY - 26);
                graphics.drawString(measuredHeightString, (lowDZ + (highDZ - lowDZ) / 2) - 21, highDY - 25);
                graphics.drawString(measuredHeightString, (lowDZ + (highDZ - lowDZ) / 2) - 19, highDY - 25);
                graphics.drawString(heightString, (lowDZ + (highDZ - lowDZ) / 2) - 20, highDY - 9);
                graphics.drawString(heightString, (lowDZ + (highDZ - lowDZ) / 2) - 20, highDY - 11);
                graphics.drawString(heightString, (lowDZ + (highDZ - lowDZ) / 2) - 21, highDY - 10);
                graphics.drawString(heightString, (lowDZ + (highDZ - lowDZ) / 2) - 19, highDY - 10);

                graphics.setColor(Color.white);
                graphics.drawString(measuredHeightString, (lowDZ + (highDZ - lowDZ) / 2) - 20, highDY - 25);
                graphics.drawString(heightString, (lowDZ + (highDZ - lowDZ) / 2) - 20, highDY - 10);
            }

            graphics.setColor(Color.black);
            if ( (lowDZ - 40) < 0)
            {
                graphics.drawString(measuredWidthString, lowDZ + 10, highDY + 10 + (lowDY - highDY) / 2 + 1);
                graphics.drawString(measuredWidthString, lowDZ + 10, highDY + 10 + (lowDY - highDY) / 2 - 1);
                graphics.drawString(measuredWidthString, lowDZ + 9, highDY + 10 + (lowDY - highDY) / 2);
                graphics.drawString(measuredWidthString, lowDZ + 11, highDY + 10 + (lowDY - highDY) / 2);
                graphics.drawString(widthString, lowDZ + 10, highDY + 25 + (lowDY - highDY) / 2 + 1);
                graphics.drawString(widthString, lowDZ + 10, highDY + 25 + (lowDY - highDY) / 2 - 1);
                graphics.drawString(widthString, lowDZ + 9, highDY + 25 + (lowDY - highDY) / 2);
                graphics.drawString(widthString, lowDZ + 11, highDY + 25 + (lowDY - highDY) / 2);

                graphics.setColor(Color.white);
                graphics.drawString(measuredWidthString, lowDZ + 10, highDY + 10 + (lowDY - highDY) / 2);
                graphics.drawString(widthString, lowDZ + 10, highDY + 25 + (lowDY - highDY) / 2);
            }
            else
            {
                graphics.drawString(measuredWidthString, lowDZ - 35, highDY + 10 + (lowDY - highDY) / 2 + 1);
                graphics.drawString(measuredWidthString, lowDZ - 35, highDY + 10 + (lowDY - highDY) / 2 - 1);
                graphics.drawString(measuredWidthString, lowDZ - 36, highDY + 10 + (lowDY - highDY) / 2);
                graphics.drawString(measuredWidthString, lowDZ - 34, highDY + 10 + (lowDY - highDY) / 2);
                graphics.drawString(widthString, lowDZ - 35, highDY + 25 + (lowDY - highDY) / 2 + 1);
                graphics.drawString(widthString, lowDZ - 35, highDY + 25 + (lowDY - highDY) / 2 - 1);
                graphics.drawString(widthString, lowDZ - 36, highDY + 25 + (lowDY - highDY) / 2);
                graphics.drawString(widthString, lowDZ - 34, highDY + 25 + (lowDY - highDY) / 2);

                graphics.setColor(Color.white);
                graphics.drawString(measuredWidthString, lowDZ - 35, highDY + 10 + (lowDY - highDY) / 2);
                graphics.drawString(widthString, lowDZ - 35, highDY + 25 + (lowDY - highDY) / 2);
            }
        } // else hasOrientation*/
    }

    /**
     * Draws the cropping rectangle in the XZ plane.
     * @param graphics  the graphics object to draw with
     */
    private void drawBoundingRect_CORONAL(Graphics graphics)
    {
        graphics.setColor(Color.red.darker());

        int [] indices = new int[4];

        if (getOriginalOrientation() == CORONAL)
        {
            indices[0] = ViewJFrameTriImage.UPPER_LEFT_FRONT;
            indices[1] = ViewJFrameTriImage.UPPER_RIGHT_FRONT;
            indices[2] = ViewJFrameTriImage.LOWER_RIGHT_FRONT;
            indices[3] = ViewJFrameTriImage.LOWER_LEFT_FRONT;
        }
        else if (getOriginalOrientation() == AXIAL)
        {
            indices[0] = ViewJFrameTriImage.UPPER_LEFT_BACK;
            indices[1] = ViewJFrameTriImage.UPPER_RIGHT_BACK;
            indices[2] = ViewJFrameTriImage.UPPER_RIGHT_FRONT;
            indices[3] = ViewJFrameTriImage.UPPER_LEFT_FRONT;
        }
        else if (getOriginalOrientation() == SAGITTAL)
        {
            indices[0] = ViewJFrameTriImage.UPPER_LEFT_BACK;
            indices[1] = ViewJFrameTriImage.UPPER_LEFT_FRONT;
            indices[2] = ViewJFrameTriImage.LOWER_LEFT_FRONT;
            indices[3] = ViewJFrameTriImage.LOWER_LEFT_BACK;
        }
        else if (!hasOrientation)
        {
            indices[0] = ViewJFrameTriImage.UPPER_LEFT_BACK;
            indices[1] = ViewJFrameTriImage.UPPER_RIGHT_BACK;
            indices[2] = ViewJFrameTriImage.LOWER_RIGHT_FRONT;
            indices[3] = ViewJFrameTriImage.UPPER_LEFT_FRONT;
        }

        Point3D [] boundingBoxPoints = triImageFrame.getBoundingBoxPoints();

        Point screenPoint1 = getScreenCoordinates(boundingBoxPoints[indices[0]]);
        Point screenPoint2 = getScreenCoordinates(boundingBoxPoints[indices[1]]);
        Point screenPoint3 = getScreenCoordinates(boundingBoxPoints[indices[2]]);
        Point screenPoint4 = getScreenCoordinates(boundingBoxPoints[indices[3]]);

        graphics.drawLine(screenPoint1.x, screenPoint1.y, screenPoint2.x, screenPoint2.y);
        graphics.drawLine(screenPoint2.x, screenPoint2.y, screenPoint3.x, screenPoint3.y);
        graphics.drawLine(screenPoint3.x, screenPoint3.y, screenPoint4.x, screenPoint4.y);
        graphics.drawLine(screenPoint4.x, screenPoint4.y, screenPoint1.x, screenPoint1.y);

        graphics.fillRect(screenPoint1.x - 2, screenPoint1.y - 2, 4, 4);
        graphics.fillRect(screenPoint2.x - 2, screenPoint2.y - 2, 4, 4);
        graphics.fillRect(screenPoint3.x - 2, screenPoint3.y - 2, 4, 4);
        graphics.fillRect(screenPoint4.x - 2, screenPoint4.y - 2, 4, 4);

        if (true) return;
/*
        int lowDX;
        int highDX;
        int lowDZ;
        int highDZ;

        graphics.setColor(Color.red.darker());


        // not doing - 1 on reversal, but i don't quite know why (and my head hurts too much to think about it more)
        switch (axisOrder[0])
        {
            case 0:
                if (axisFlip[0])
                {
                    lowDX = (int) ( (imageActive.getExtents()[axisOrder[0]] - lowX) * zoomX * resolutionX);
                    highDX = (int) ( (imageActive.getExtents()[axisOrder[0]] - highX) * zoomX * resolutionX);
                }
                else
                {
                    lowDX = (int) (lowX * zoomX * resolutionX);
                    highDX = (int) (highX * zoomX * resolutionX);
                }
                break;
            case 1:
                if (axisFlip[0])
                {
                    lowDX = (int) ( (imageActive.getExtents()[axisOrder[0]] - lowY) * zoomX * resolutionX);
                    highDX = (int) ( (imageActive.getExtents()[axisOrder[0]] - highY) * zoomX * resolutionX);
                }
                else
                {
                    lowDX = (int) (lowY * zoomX * resolutionX);
                    highDX = (int) (highY * zoomX * resolutionX);
                }
                break;
            case 2:
            default:
                if (axisFlip[0])
                {
                    lowDX = (int) ( (imageActive.getExtents()[axisOrder[0]] - lowZ) * zoomX * resolutionX);
                    highDX = (int) ( (imageActive.getExtents()[axisOrder[0]] - highZ) * zoomX * resolutionX);
                }
                else
                {
                    lowDX = (int) (lowZ * zoomX * resolutionX);
                    highDX = (int) (highZ * zoomX * resolutionX);
                }
                break;
        }
        switch (axisOrder[1])
        {
            case 0:
                if (axisFlip[1])
                {
                    lowDZ = (int) ( (imageActive.getExtents()[axisOrder[1]] - lowX) * zoomY * resolutionY);
                    highDZ = (int) ( (imageActive.getExtents()[axisOrder[1]] - highX) * zoomY * resolutionY);
                }
                else
                {
                    lowDZ = (int) (lowX * zoomY * resolutionY);
                    highDZ = (int) (highX * zoomY * resolutionY);
                }
                break;
            case 1:
                if (axisFlip[1])
                {
                    lowDZ = (int) ( (imageActive.getExtents()[axisOrder[1]] - lowY) * zoomY * resolutionY);
                    highDZ = (int) ( (imageActive.getExtents()[axisOrder[1]] - highY) * zoomY * resolutionY);
                }
                else
                {
                    lowDZ = (int) (lowY * zoomY * resolutionY);
                    highDZ = (int) (highY * zoomY * resolutionY);
                }
                break;
            case 2:
            default:
                if (axisFlip[1])
                {
                    lowDZ = (int) ( (imageActive.getExtents()[axisOrder[1]] - lowZ) * zoomY * resolutionY);
                    highDZ = (int) ( (imageActive.getExtents()[axisOrder[1]] - highZ) * zoomY * resolutionY);
                }
                else
                {
                    lowDZ = (int) (lowZ * zoomY * resolutionY);
                    highDZ = (int) (highZ * zoomY * resolutionY);
                }
                break;
        }

        graphics.drawLine(lowDX, lowDZ, highDX, lowDZ);
        graphics.drawLine(highDX, lowDZ, highDX, highDZ);
        graphics.drawLine(highDX, highDZ, lowDX, highDZ);
        graphics.drawLine(lowDX, highDZ, lowDX, lowDZ);
        graphics.fillRect(lowDX - 2, lowDZ - 2, 5, 5);
        graphics.fillRect(highDX - 2, lowDZ - 2, 5, 5);
        graphics.fillRect(highDX - 2, highDZ - 2, 5, 5);
        graphics.fillRect(lowDX - 2, highDZ - 2, 5, 5);

        // display the height/width of the bounding box above (or below) the top
        // midpoint and to the right of (or left of) the right midpoint

        int width = (int) (highX - lowX);
        int height = (int) (highZ - lowZ);

        String widthString = String.valueOf(width);
        String heightString = String.valueOf(height);

        float measuredWidth = (highX - lowX) * res[0];
        float measuredHeight = (highZ - lowZ) * res[1];

        String xUnitsString = FileInfoBase.getUnitsOfMeasureAbbrevStr(unitsOfMeasure[0]);
        String yUnitsString = FileInfoBase.getUnitsOfMeasureAbbrevStr(unitsOfMeasure[1]);

        String measuredWidthString = String.valueOf(measuredWidth) + " " + xUnitsString;
        String measuredHeightString = String.valueOf(measuredHeight) + " " + yUnitsString;

        graphics.setColor(Color.black);

        if (!hasOrientation)
        {
            if ( (lowDZ - 45) < 0)
            {
                graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDZ + 21);
                graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDZ + 19);
                graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 21, lowDZ + 20);
                graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 19, lowDZ + 20);
                graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDZ + 36);
                graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDZ + 34);
                graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 21, lowDZ + 35);
                graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 19, lowDZ + 35);

                graphics.setColor(Color.white);
                graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDZ + 20);
                graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDZ + 35);
            }
            else
            {
                graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDZ - 24);
                graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDZ - 26);
                graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 21, lowDZ - 25);
                graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 19, lowDZ - 25);
                graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDZ - 9);
                graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDZ - 11);
                graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 21, lowDZ - 10);
                graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 19, lowDZ - 10);

                graphics.setColor(Color.white);
                graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDZ - 25);
                graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDZ - 10);
            }

            graphics.setColor(Color.black);
            if ( (lowDX - 40) < 0)
            {
                graphics.drawString(measuredHeightString, lowDX + 10, lowDZ + 10 + (highDZ - lowDZ) / 2 + 1);
                graphics.drawString(measuredHeightString, lowDX + 10, lowDZ + 10 + (highDZ - lowDZ) / 2 - 1);
                graphics.drawString(measuredHeightString, lowDX + 9, lowDZ + 10 + (highDZ - lowDZ) / 2);
                graphics.drawString(measuredHeightString, lowDX + 11, lowDZ + 10 + (highDZ - lowDZ) / 2);
                graphics.drawString(heightString, lowDX + 10, lowDZ + 25 + (highDZ - lowDZ) / 2 + 1);
                graphics.drawString(heightString, lowDX + 10, lowDZ + 25 + (highDZ - lowDZ) / 2 - 1);
                graphics.drawString(heightString, lowDX + 9, lowDZ + 25 + (highDZ - lowDZ) / 2);
                graphics.drawString(heightString, lowDX + 11, lowDZ + 25 + (highDZ - lowDZ) / 2);

                graphics.setColor(Color.white);
                graphics.drawString(measuredHeightString, lowDX + 10, lowDZ + 10 + (highDZ - lowDZ) / 2);
                graphics.drawString(heightString, lowDX + 10, lowDZ + 25 + (highDZ - lowDZ) / 2);
            }
            else
            {
                graphics.drawString(measuredHeightString, lowDX - 35, lowDZ + 10 + (highDZ - lowDZ) / 2 + 1);
                graphics.drawString(measuredHeightString, lowDX - 35, lowDZ + 10 + (highDZ - lowDZ) / 2 - 1);
                graphics.drawString(measuredHeightString, lowDX - 36, lowDZ + 10 + (highDZ - lowDZ) / 2);
                graphics.drawString(measuredHeightString, lowDX - 34, lowDZ + 10 + (highDZ - lowDZ) / 2);
                graphics.drawString(heightString, lowDX - 35, lowDZ + 25 + (highDZ - lowDZ) / 2 + 1);
                graphics.drawString(heightString, lowDX - 35, lowDZ + 25 + (highDZ - lowDZ) / 2 - 1);
                graphics.drawString(heightString, lowDX - 36, lowDZ + 25 + (highDZ - lowDZ) / 2);
                graphics.drawString(heightString, lowDX - 34, lowDZ + 25 + (highDZ - lowDZ) / 2);

                graphics.setColor(Color.white);
                graphics.drawString(measuredHeightString, lowDX - 35, lowDZ + 10 + (highDZ - lowDZ) / 2);
                graphics.drawString(heightString, lowDX - 35, lowDZ + 25 + (highDZ - lowDZ) / 2);
            }
        } // if (!hasOrientation)
        else
        { // hasOrientation
            if ( (highDZ - 45) < 0)
            {
                graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 20, highDZ + 21);
                graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 20, highDZ + 19);
                graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 21, highDZ + 20);
                graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 19, highDZ + 20);
                graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 20, highDZ + 36);
                graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 20, highDZ + 34);
                graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 21, highDZ + 35);
                graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 19, highDZ + 35);

                graphics.setColor(Color.white);
                graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 20, highDZ + 20);
                graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 20, highDZ + 35);
            }
            else
            {
                graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 20, highDZ - 24);
                graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 20, highDZ - 26);
                graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 21, highDZ - 25);
                graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 19, highDZ - 25);
                graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 20, highDZ - 9);
                graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 20, highDZ - 11);
                graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 21, highDZ - 10);
                graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 19, highDZ - 10);

                graphics.setColor(Color.white);
                graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 20, highDZ - 25);
                graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 20, highDZ - 10);
            }

            graphics.setColor(Color.black);
            if ( (lowDX - 40) < 0)
            {
                graphics.drawString(measuredHeightString, lowDX + 10, highDZ + 10 + (lowDZ - highDZ) / 2 + 1);
                graphics.drawString(measuredHeightString, lowDX + 10, highDZ + 10 + (lowDZ - highDZ) / 2 - 1);
                graphics.drawString(measuredHeightString, lowDX + 9, highDZ + 10 + (lowDZ - highDZ) / 2);
                graphics.drawString(measuredHeightString, lowDX + 11, highDZ + 10 + (lowDZ - highDZ) / 2);
                graphics.drawString(heightString, lowDX + 10, highDZ + 25 + (lowDZ - highDZ) / 2 + 1);
                graphics.drawString(heightString, lowDX + 10, highDZ + 25 + (lowDZ - highDZ) / 2 - 1);
                graphics.drawString(heightString, lowDX + 9, highDZ + 25 + (lowDZ - highDZ) / 2);
                graphics.drawString(heightString, lowDX + 11, highZ + 25 + (lowDZ - highDZ) / 2);

                graphics.setColor(Color.white);
                graphics.drawString(measuredHeightString, lowDX + 10, highDZ + 10 + (lowDZ - highDZ) / 2);
                graphics.drawString(heightString, lowDX + 10, highDZ + 25 + (lowDZ - highDZ) / 2);
            }
            else
            {
                graphics.drawString(measuredHeightString, lowDX - 35, highDZ + 10 + (lowDZ - highDZ) / 2 + 1);
                graphics.drawString(measuredHeightString, lowDX - 35, highDZ + 10 + (lowDZ - highDZ) / 2 - 1);
                graphics.drawString(measuredHeightString, lowDX - 36, highDZ + 10 + (lowDZ - highDZ) / 2);
                graphics.drawString(measuredHeightString, lowDX - 34, highDZ + 10 + (lowDZ - highDZ) / 2);
                graphics.drawString(heightString, lowDX - 35, highDZ + 25 + (lowDZ - highDZ) / 2 + 1);
                graphics.drawString(heightString, lowDX - 35, highDZ + 25 + (lowDZ - highDZ) / 2 - 1);
                graphics.drawString(heightString, lowDX - 36, highDZ + 25 + (lowDZ - highDZ) / 2);
                graphics.drawString(heightString, lowDX - 34, highDZ + 25 + (lowDZ - highDZ) / 2);

                graphics.setColor(Color.white);
                graphics.drawString(measuredHeightString, lowDX - 35, highDZ + 10 + (lowDZ - highDZ) / 2);
                graphics.drawString(heightString, lowDX - 35, highDZ + 25 + (lowDZ - highDZ) / 2);
            }
        } // hasOrientation*/
    }

    /**
     * Draws the cropping rectangle in the XY plane.
     * @param graphics  the graphics object to draw with
     */
    private void drawBoundingRect_AXIAL(Graphics graphics)
    {
        graphics.setColor(Color.red.darker());

        int [] indices = new int[4];

        if (getOriginalOrientation() == CORONAL)
        {
            indices[0] = ViewJFrameTriImage.UPPER_LEFT_BACK;
            indices[1] = ViewJFrameTriImage.UPPER_RIGHT_BACK;
            indices[2] = ViewJFrameTriImage.UPPER_RIGHT_FRONT;
            indices[3] = ViewJFrameTriImage.UPPER_LEFT_FRONT;
        }
        else if (getOriginalOrientation() == AXIAL)
        {
            indices[0] = ViewJFrameTriImage.UPPER_LEFT_FRONT;
            indices[1] = ViewJFrameTriImage.UPPER_RIGHT_FRONT;
            indices[2] = ViewJFrameTriImage.LOWER_RIGHT_FRONT;
            indices[3] = ViewJFrameTriImage.LOWER_LEFT_FRONT;
        }
        else if (getOriginalOrientation() == SAGITTAL)
        {
            indices[0] = ViewJFrameTriImage.UPPER_LEFT_BACK;
            indices[1] = ViewJFrameTriImage.UPPER_LEFT_FRONT;
            indices[2] = ViewJFrameTriImage.LOWER_LEFT_FRONT;
            indices[3] = ViewJFrameTriImage.LOWER_LEFT_BACK;
        }
        else if (!hasOrientation)
        {
            indices[0] = ViewJFrameTriImage.UPPER_LEFT_FRONT;
            indices[1] = ViewJFrameTriImage.UPPER_RIGHT_FRONT;
            indices[2] = ViewJFrameTriImage.LOWER_RIGHT_FRONT;
            indices[3] = ViewJFrameTriImage.LOWER_LEFT_FRONT;
        }

        Point3D [] boundingBoxPoints = triImageFrame.getBoundingBoxPoints();

        Point screenPoint1 = getScreenCoordinates(boundingBoxPoints[indices[0]]);
        Point screenPoint2 = getScreenCoordinates(boundingBoxPoints[indices[1]]);
        Point screenPoint3 = getScreenCoordinates(boundingBoxPoints[indices[2]]);
        Point screenPoint4 = getScreenCoordinates(boundingBoxPoints[indices[3]]);

        graphics.drawLine(screenPoint1.x, screenPoint1.y, screenPoint2.x, screenPoint2.y);
        graphics.drawLine(screenPoint2.x, screenPoint2.y, screenPoint3.x, screenPoint3.y);
        graphics.drawLine(screenPoint3.x, screenPoint3.y, screenPoint4.x, screenPoint4.y);
        graphics.drawLine(screenPoint4.x, screenPoint4.y, screenPoint1.x, screenPoint1.y);

        graphics.fillRect(screenPoint1.x - 2, screenPoint1.y - 2, 4, 4);
        graphics.fillRect(screenPoint2.x - 2, screenPoint2.y - 2, 4, 4);
        graphics.fillRect(screenPoint3.x - 2, screenPoint3.y - 2, 4, 4);
        graphics.fillRect(screenPoint4.x - 2, screenPoint4.y - 2, 4, 4);

        if (true) return;

/*
        int lowDX;
        int highDX;
        int lowDY;
        int highDY;

        switch (axisOrder[0])
        {
            case 0:
                if (axisFlip[0])
                {
                    lowDX = (int) ( (imageActive.getExtents()[axisOrder[0]] - lowX) * zoomX * resolutionX);
                    highDX = (int) ( (imageActive.getExtents()[axisOrder[0]] - highX) * zoomX * resolutionX);
                }
                else
                {
                    lowDX = (int) (lowX * zoomX * resolutionX);
                    highDX = (int) (highX * zoomX * resolutionX);
                }
                break;
            case 1:
                if (axisFlip[0])
                {
                    lowDX = (int) ( (imageActive.getExtents()[axisOrder[0]] - lowY) * zoomX * resolutionX);
                    highDX = (int) ( (imageActive.getExtents()[axisOrder[0]] - highY) * zoomX * resolutionX);
                }
                else
                {
                    lowDX = (int) (lowY * zoomX * resolutionX);
                    highDX = (int) (highY * zoomX * resolutionX);
                }
                break;
            case 2:
            default:
                if (axisFlip[0])
                {
                    lowDX = (int) ( (imageActive.getExtents()[axisOrder[0]] - lowZ) * zoomX * resolutionX);
                    highDX = (int) ( (imageActive.getExtents()[axisOrder[0]] - highZ) * zoomX * resolutionX);
                }
                else
                {
                    lowDX = (int) (lowZ * zoomX * resolutionX);
                    highDX = (int) (highZ * zoomX * resolutionX);
                }
                break;
        }
        switch (axisOrder[1])
        {
            case 0:
                if (axisFlip[1])
                {
                    lowDY = (int) ( (imageActive.getExtents()[axisOrder[1]] - lowX) * zoomY * resolutionY);
                    highDY = (int) ( (imageActive.getExtents()[axisOrder[1]] - highX) * zoomY * resolutionY);
                }
                else
                {
                    lowDY = (int) (lowX * zoomY * resolutionY);
                    highDY = (int) (highX * zoomY * resolutionY);
                }
                break;
            case 1:
                if (axisFlip[1])
                {
                    lowDY = (int) ( (imageActive.getExtents()[axisOrder[1]] - lowY) * zoomY * resolutionY);
                    highDY = (int) ( (imageActive.getExtents()[axisOrder[1]] - highY) * zoomY * resolutionY);
                }
                else
                {
                    lowDY = (int) (lowY * zoomY * resolutionY);
                    highDY = (int) (highY * zoomY * resolutionY);
                }
                break;
            case 2:
            default:
                if (axisFlip[1])
                {
                    lowDY = (int) ( (imageActive.getExtents()[axisOrder[1]] - lowZ) * zoomY * resolutionY);
                    highDY = (int) ( (imageActive.getExtents()[axisOrder[1]] - highZ) * zoomY * resolutionY);
                }
                else
                {
                    lowDY = (int) (lowZ * zoomY * resolutionY);
                    highDY = (int) (highZ * zoomY * resolutionY);
                }
                break;
        }

        graphics.setColor(Color.red.darker());

        graphics.drawLine(lowDX, lowDY, highDX, lowDY);
        graphics.drawLine(highDX, lowDY, highDX, highDY);
        graphics.drawLine(highDX, highDY, lowDX, highDY);
        graphics.drawLine(lowDX, highDY, lowDX, lowDY);
        graphics.fillRect(lowDX - 2, lowDY - 2, 5, 5);
        graphics.fillRect(highDX - 2, lowDY - 2, 5, 5);
        graphics.fillRect(highDX - 2, highDY - 2, 5, 5);
        graphics.fillRect(lowDX - 2, highDY - 2, 5, 5);

        // display the height/width of the bounding box above (or below) the top
        // midpoint and to the right of (or left of) the right midpoint
        int width = (int) (highX - lowX);
        int height = (int) (highY - lowY);

        String widthString = String.valueOf(width);
        String heightString = String.valueOf(height);

        float measuredWidth = (highX - lowX) * res[0];
        float measuredHeight = (highY - lowY) * res[1];

        String xUnitsString = FileInfoBase.getUnitsOfMeasureAbbrevStr(unitsOfMeasure[0]);
        String yUnitsString = FileInfoBase.getUnitsOfMeasureAbbrevStr(unitsOfMeasure[1]);

        String measuredWidthString = String.valueOf(measuredWidth) + " " + xUnitsString;
        String measuredHeightString = String.valueOf(measuredHeight) + " " + yUnitsString;

        graphics.setColor(Color.black);

        if ( (lowDY - 45) < 0)
        {
            graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDY + 21);
            graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDY + 19);
            graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 21, lowDY + 20);
            graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 19, lowDY + 20);
            graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDY + 36);
            graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDY + 34);
            graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 21, lowDY + 35);
            graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 19, lowDY + 35);

            graphics.setColor(Color.white);
            graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDY + 20);
            graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDY + 35);
        }
        else
        {
            graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDY - 24);
            graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDY - 26);
            graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 21, lowDY - 25);
            graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 19, lowDY - 25);
            graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDY - 9);
            graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDY - 11);
            graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 21, lowDY - 10);
            graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 19, lowDY - 10);

            graphics.setColor(Color.white);
            graphics.drawString(measuredWidthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDY - 25);
            graphics.drawString(widthString, (lowDX + (highDX - lowDX) / 2) - 20, lowDY - 10);
        }

        graphics.setColor(Color.black);
        if ( (lowDX - 40) < 0)
        {
            graphics.drawString(measuredHeightString, lowDX + 10, lowDY + 10 + (highDY - lowDY) / 2 + 1);
            graphics.drawString(measuredHeightString, lowDX + 10, lowDY + 10 + (highDY - lowDY) / 2 - 1);
            graphics.drawString(measuredHeightString, lowDX + 9, lowDY + 10 + (highDY - lowDY) / 2);
            graphics.drawString(measuredHeightString, lowDX + 11, lowDY + 10 + (highDY - lowDY) / 2);
            graphics.drawString(heightString, lowDX + 10, lowDY + 25 + (highDY - lowDY) / 2 + 1);
            graphics.drawString(heightString, lowDX + 10, lowDY + 25 + (highDY - lowDY) / 2 - 1);
            graphics.drawString(heightString, lowDX + 9, lowDY + 25 + (highDY - lowDY) / 2);
            graphics.drawString(heightString, lowDX + 11, lowDY + 25 + (highDY - lowDY) / 2);

            graphics.setColor(Color.white);
            graphics.drawString(measuredHeightString, lowDX + 10, lowDY + 10 + (highDY - lowDY) / 2);
            graphics.drawString(heightString, lowDX + 10, lowDY + 25 + (highDY - lowDY) / 2);
        }
        else
        {
            graphics.drawString(measuredHeightString, lowDX - 35, lowDY + 10 + (highDY - lowDY) / 2 + 1);
            graphics.drawString(measuredHeightString, lowDX - 35, lowDY + 10 + (highDY - lowDY) / 2 - 1);
            graphics.drawString(measuredHeightString, lowDX - 36, lowDY + 10 + (highDY - lowDY) / 2);
            graphics.drawString(measuredHeightString, lowDX - 34, lowDY + 10 + (highDY - lowDY) / 2);
            graphics.drawString(heightString, lowDX - 35, lowDY + 25 + (highDY - lowDY) / 2 + 1);
            graphics.drawString(heightString, lowDX - 35, lowDY + 25 + (highDY - lowDY) / 2 - 1);
            graphics.drawString(heightString, lowDX - 36, lowDY + 25 + (highDY - lowDY) / 2);
            graphics.drawString(heightString, lowDX - 34, lowDY + 25 + (highDY - lowDY) / 2);

            graphics.setColor(Color.white);
            graphics.drawString(measuredHeightString, lowDX - 35, lowDY + 10 + (highDY - lowDY) / 2);
            graphics.drawString(heightString, lowDX - 35, lowDY + 25 + (highDY - lowDY) / 2);
        }*/
    }

    /**
     * Mouse entry handler: tells the parent tri-image frame about the current component triComponentOrientation.
     * @param mouseEvent  event that triggers this function
     */
    public void mouseEntered(MouseEvent mouseEvent)
    {
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();
        triImageFrame.setOrientation(triComponentOrientation);
    }

    /**
     * Mouse exit handler: repaints the image to get rid of the cursor if we are in a mode that custom draws the mouse cursor.
     * @param mouseEvent  event that triggered function
     */
    public void mouseExited(MouseEvent mouseEvent)
    {
        lastMouseX = OUT_OF_BOUNDS;
        lastMouseY = OUT_OF_BOUNDS;
        if (mode == PAINT_VOI || mode == ERASER_PAINT)
        {
            paintComponent(getGraphics());
        }
    }

    public void mouseClicked(MouseEvent mouseEvent)
    { }

    /**
     * A mouse-pressed event.  Sets the mode of the program depending on the cursor mode.  If the mode is move,
     * activates the contour or line and enables the delete button.
     * @param mouseEvent  event that triggered this function
     */
    public void mousePressed(MouseEvent mouseEvent)
    {
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();

        int xS = getScaledX(mouseEvent.getX()); // zoomed x.  Used as cursor
        int yS = getScaledY(mouseEvent.getY()); // zoomed y.  Used as cursor

        if (xS < 0 || xS >= imageDim.width || yS < 0 || yS >= imageDim.height)
        {
            return;
        }

        if (mode == POINT_VOI)
        {
            return;
        }

        if (mouseEvent.getModifiers() == MouseEvent.BUTTON1_MASK)
        {
            if (intensityLine != null && intensityLineVisible)
            {
                intensityLine.setActive(true);
                ( (VOILine) (intensityLine.getCurves()[slice].elementAt(0))).setActive(true);

                if (intensityLine.nearLinePoint(mouseEvent.getX(), mouseEvent.getY(), slice, 0, getZoomX(), resolutionX,
                                                resolutionY))
                {
                    moveLineEndpoint = true;
                    return;
                }
                else
                {
                    moveLineEndpoint = false;
                }
            }

            if (intensityLine != null && intensityLineVisible)
            {
                if (intensityLine.nearLine(xS, yS, slice))
                {
                    anchorPt.x = xS;
                    anchorPt.y = yS;
                    moveLine = true;
                    return;
                }
                else
                {
                    moveLine = false;
                }
            }

            if (doCenter)
            {
                // TODO: include logic to ensure we're on the proper slice...

                Point3D volumePoint3d = getVolumePosition(xS, yS, slice);
                int[] volumePoint = new int[]
                    {volumePoint3d.x, volumePoint3d.y, volumePoint3d.z};

                if (Math.abs(triImageFrame.getCenter()[axisOrder[0]] - volumePoint[axisOrder[0]]) < 6 &&
                    Math.abs(triImageFrame.getCenter()[axisOrder[1]] - volumePoint[axisOrder[1]]) < 6)
                {
                    dragCenterPt = true;
                }
                else
                {
                    dragCenterPt = false;
                }
             }


            if (mode == DEFAULT)
            {
                handleVOIProcessing(mouseEvent);
            }
        }

        if (mouseEvent.getModifiers() == MouseEvent.BUTTON3_MASK)
        {
            handleMouseButton3PressedEvent(mouseEvent);
            return;
        } // if (mouseEvent.getModifiers() == MouseEvent.BUTTON3_MASK)
        else if (mode == LINE && intensityLine == null)
        {
            anchorPt.setLocation(xS, yS);

            return;
        } // else if (mode == LINE)
        else if (mode == MOVE)
        {
            anchorPt.setLocation(xS, yS); // For use in dragging VOIs
            if (voiProtractor != null && voiProtractor.nearLine(xS, yS, slice) && moveProtractor)
            {
                voiProtractor.setActive(true);
                ( (VOIProtractor) (voiProtractor.getCurves()[slice].elementAt(0))).setActive(true);
            }
            else
            {
                setMode(DEFAULT);
            }
            return;
            // do not do a notifyImageDisplayListeners in mode MOVE or VOISpecial labels will disappear
        } // end of if (mode == MOVE)
        /*else if (mode == CUBE_BOUNDS)
        {
            if ( (triComponentOrientation == AXIAL || !hasOrientation) &&
                (slice >= lowZ) && (slice <= highZ))
            {
                if (nearBoundsPoint(mouseEvent.getX(), mouseEvent.getY(), lowX, lowY))
                {
                    doxyC0 = true;
                    return;
                }
                if (nearBoundsPoint(mouseEvent.getX(), mouseEvent.getY(), highX, lowY))
                {
                    doxyC1 = true;
                    return;
                }
                if (nearBoundsPoint(mouseEvent.getX(), mouseEvent.getY(), highX, highY))
                {
                    doxyC2 = true;
                    return;
                }
                if (nearBoundsPoint(mouseEvent.getX(), mouseEvent.getY(), lowX, highY))
                {
                    doxyC3 = true;
                    return;
                }
            } // if ((triComponentOrientation == XY) && (zSlice >= lowZ) && (zSlice <= highZ))
            else if ( (triComponentOrientation == CORONAL || !hasOrientation) && (screenPt.y >= lowY) && (screenPt.y <= highY))
            {
                if (hasOrientation)
                {
                    nearZ = MipavMath.round(
                        (imageActive.getExtents()[axisOrder[1]] - 1) * getZoomY() * resolutionY - mouseEvent.getY());
                }
                else
                {
                    nearZ = mouseEvent.getY();
                }
                if (nearBoundsPoint(mouseEvent.getX(), nearZ, lowX, lowZ))
                {
                    doxzC0 = true;
                    return;
                }
                if (nearBoundsPoint(mouseEvent.getX(), nearZ, highX, lowZ))
                {
                    doxzC1 = true;
                    return;
                }
                if (nearBoundsPoint(mouseEvent.getX(), nearZ, highX, highZ))
                {
                    doxzC2 = true;
                    return;
                }
                if (nearBoundsPoint(mouseEvent.getX(), nearZ, lowX, highZ))
                {
                    doxzC3 = true;
                    return;
                }
            } // else if ((triComponentOrientation == XZ) && (screenPt.y >= lowY) && (screenPt.y <= highY))
            else if ( (triComponentOrientation == SAGITTAL || !hasOrientation) && (screenPt.x >= lowX) && (screenPt.x <= highX))
            {
                if (hasOrientation)
                {
                    nearZ = MipavMath.round(
                        (imageActive.getExtents()[axisOrder[1]] - 1) * getZoomY() * resolutionY - mouseEvent.getY());
                    nearY = mouseEvent.getX();
                }
                else
                {
                    nearZ = mouseEvent.getX();
                    nearY = mouseEvent.getY();
                }
                if (nearBoundsPoint(nearZ, nearY, lowZ, lowY))
                {
                    dozyC0 = true;
                    return;
                }
                if (nearBoundsPoint(nearZ, nearY, highZ, lowY))
                {
                    dozyC1 = true;
                    return;
                }
                if (nearBoundsPoint(nearZ, nearY, highZ, highY))
                {
                    dozyC2 = true;
                    return;
                }
                if (nearBoundsPoint(nearZ, nearY, lowZ, highY))
                {
                    dozyC3 = true;
                    return;
                }
            } // else if ((triComponentOrientation == ZY) && (screenPt.x >= lowX) && (screenPt.x <= highX))
        }*/ // else if (mode == CUBE_BOUNDS)
        else if (mode == PAINT_VOI)
        {
            boolean isLeftMouseButtonDown = mouseEvent.getModifiers() == MouseEvent.BUTTON1_MASK;
            updatePaintBitmap(isLeftMouseButtonDown, xS, yS);
            if (Preferences.is(Preferences.PREF_FAST_TRIPLANAR_REPAINT))
            {
                repaint();
                return;
            }
        } // end of else if (mode == PAINT_VOI)
        else if (mode == ERASER_PAINT)
        {
            updatePaintBitmap(false, xS, yS);
            if (Preferences.is(Preferences.PREF_FAST_TRIPLANAR_REPAINT))
            {
                repaint();
                return;
            }
        } // end of else if (mode == ERASER_PAINT)
        else if (mode == DROPPER_PAINT)
        {
            if (imagesDone == IMAGE_A)
            {
                intensityDropper = imageBufferA[yS * imageDim.width + xS];
            }
            else
            {
                intensityDropper = imageBufferB[yS * imageDim.width + xS];
            }

            if (triComponentOrientation == AXIAL || !hasOrientation)
            {
                triImageFrame.setXZIntensityDropper(intensityDropper);
                triImageFrame.setZYIntensityDropper(intensityDropper);
            }
            else if (triComponentOrientation == CORONAL)
            {
                triImageFrame.setXYIntensityDropper(intensityDropper);
                triImageFrame.setZYIntensityDropper(intensityDropper);
            }
            else
            {
                triImageFrame.setXYIntensityDropper(intensityDropper);
                triImageFrame.setXZIntensityDropper(intensityDropper);
            }
            triImageFrame.setIntensityPaintName(intensityDropper);
            imageActive.notifyImageDisplayListeners(null, true);
        } // end of else if (mode == DROPPER_PAINT)

        frame.updateImages();

        return;
    }

    private void handleMouseButton3PressedEvent(MouseEvent mouseEvent)
    {
        if (intensityLineVisible == true)
        {
            int xSOrg = getScaledX(mouseEvent.getX());
            int ySOrg = getScaledY(mouseEvent.getY());

            if (intensityLine != null &&
                ( ( (VOILine) intensityLine.getCurves()[slice].elementAt(0))).nearLine(xSOrg, ySOrg))
            {
                // build VOI intensity popup menu
                JPopupMenu popupMenu = new JPopupMenu();
                JMenuItem menuItem = new JMenuItem("Show intensity graph");
                popupMenu.add(menuItem);
                menuItem.addActionListener(this);
                menuItem.setActionCommand(SHOW_INTENSITY_GRAPH);
                menuItem = new JMenuItem("Delete this intensity line");
                popupMenu.add(menuItem);
                menuItem.addActionListener(this);
                menuItem.setActionCommand(DELETE_INTENSITY_LINE);
                popupMenu.show(this, mouseEvent.getX(), mouseEvent.getY());
            }
        }
    }

    private void showIntensityGraph(int idx0, int idx1)
    {
        ViewJFrameGraph lineGraph;
        int length;
        int c;
        int pts;
        int m;
        float[] lineX, lineY, lineZ;
        float[][] rgbPositions = null;
        float[][] rgbIntensities = null;
        float[][] rgbPos = null;
        float[][] rgbInten = null;
        float[] pos = null;
        float[] inten = null;
        float[] position = null;
        float[] intensity = null;

        lineX = new float[2];
        lineY = new float[2];
        lineZ = new float[2];
        intensityLine.exportArrays(lineX, lineY, lineZ, slice, 0);
        if (imageActive.isColorImage() == true)
        {
            length = (int) (Math.sqrt(
                ( (lineX[1] - lineX[0]) * (lineX[1] - lineX[0]))
                + ( (lineY[1] - lineY[0]) * (lineY[1] - lineY[0]))));
            rgbPositions = new float[3][length * 2 + 1];
            rgbIntensities = new float[3][length * 2 + 1];
            for (c = 0; c < 3; c++)
            {
                pts = ( (VOILine) (intensityLine.getCurves()[slice].elementAt(0))).findPositionAndIntensityRGB(
                    rgbPositions[c], rgbIntensities[c], c, getActiveImageBuffer(), res,
                    getActiveImage().getExtents()[idx0], getActiveImage().getExtents()[idx1]);
                if (c == 0)
                {
                    rgbPos = new float[3][pts];
                    rgbInten = new float[3][pts];
                }

                for (m = 0; m < pts; m++)
                {
                    rgbPos[c][m] = rgbPositions[c][m];
                    rgbInten[c][m] = rgbIntensities[c][m];
                }
            }
            if (intensityLine.getContourGraph() == null)
            {
                ViewJFrameGraph contourGraph = new ViewJFrameGraph(rgbPos, rgbInten, "Intensity Graph", intensityLine,
                    FileInfoBase.getUnitsOfMeasureAbbrevStr(
                        imageActive.getFileInfo(0).getUnitsOfMeasure(idx0)));

                contourGraph.setDefaultDirectory(getActiveImage().getUserInterface().getDefaultDirectory());
                contourGraph.setVisible(true);
                intensityLine.setContourGraph(contourGraph);
                contourGraph.setVOI(intensityLine);
            }
            else
            {
                intensityLine.getContourGraph().setUnitsInLabel(
                    FileInfoBase.getUnitsOfMeasureAbbrevStr(
                        imageActive.getFileInfo(0).getUnitsOfMeasure(idx0)));
                intensityLine.getContourGraph().saveNewFunction(rgbPos, rgbInten, 0);
            }
        }
        else
        {
            length = (int) (Math.sqrt(
                ( (lineX[1] - lineX[0]) * (lineX[1] - lineX[0]))
                + ( (lineY[1] - lineY[0]) * (lineY[1] - lineY[0]))));
            position = new float[length * 2 + 1];
            intensity = new float[length * 2 + 1];
            pts = intensityLine.findPositionAndIntensity(slice, 0, position, intensity, imageBufferActive, res,
                imageActive.getExtents()[idx0], imageActive.getExtents()[idx1]);
            pos = new float[pts];
            inten = new float[pts];

            for (m = 0; m < pts; m++)
            {
                pos[m] = position[m];
                inten[m] = intensity[m];
            }
            if (intensityLine.getContourGraph() == null)
            {
                lineGraph = new ViewJFrameGraph(pos, inten, "Line VOI Graph", intensityLine,
                                                FileInfoBase.getUnitsOfMeasureAbbrevStr(
                    imageActive.getFileInfo(0).getUnitsOfMeasure(idx0)));
                lineGraph.setDefaultDirectory(imageActive.getUserInterface().getDefaultDirectory());
                lineGraph.setVisible(true);
                intensityLine.setContourGraph(lineGraph);
                lineGraph.setVOI(intensityLine);
            }
            else
            {
                intensityLine.getContourGraph().setUnitsInLabel(
                    FileInfoBase.getUnitsOfMeasureAbbrevStr(
                        imageActive.getFileInfo(0).getUnitsOfMeasure(idx0)));
                intensityLine.getContourGraph().replaceFunction(pos, inten, intensityLine, 0);
            }
        }
    }

    /**
     * Convenience method called by mousePressed(). Inserted here for simplicity's sake because
     * this method is quite long. Handles the mouse pressed event when in the DEFAULT mode.
     * @param offscreenGraphics2d Graphics2D
     */
    private void handleVOIProcessing(MouseEvent mouseEvent)
    {
        int j;
        int k;
        Point3Df pt;

        ViewVOIVector VOIs = imageActive.getVOIs();

        if (VOIs == null)
        {
            return;
        }

        int nVOI = VOIs.size();

        for (int i = 0; i < nVOI; i++)
        {
            VOIs.VOIAt(i).setAllActive(false); // deactivate all other VOIs
        }

        int originalOrientation = getOriginalOrientation();

        for (int i = 0; i < nVOI; i++)
        {
            if (VOIs.VOIAt(i).getCurveType() == VOI.POINT)
            { // curve type is Point
                //Point3D mousePt = getVolumePosition(xSOrg, ySOrg, slice);
                Point3D mousePt = getTriImagePosition(mouseEvent.getX(), mouseEvent.getY());
                int xOrg = mousePt.x;
                int yOrg = mousePt.y;
                int zOrg = mousePt.z;

                pt = null;
                Point3Df[] voiPoints;
                if (originalOrientation == AXIAL || originalOrientation == NA)
                {
                    voiPoints = VOIs.VOIAt(i).exportPoints(zOrg);
                    for (j = 0; j < voiPoints.length; j++)
                    {
                        if ( ( (VOIPoint) (VOIs.VOIAt(i).getCurves()[zOrg].elementAt(j))).nearPointInPlane(xOrg,
                            yOrg, zOrg, AXIAL))
                        {
                            VOIs.VOIAt(i).setActive(true);
                            ( (VOIPoint) (VOIs.VOIAt(i).getCurves()[zOrg].elementAt(j))).setActive(true);
                            voiID = VOIs.VOIAt(i).getID();

                            pt = ( (VOIPoint) (VOIs.VOIAt(i).getCurves()[zOrg].elementAt(j))).exportPoint();
                        }
                    }
                } // if (originalOrientation == AXIAL || originalOrientation == NA)
                else if (originalOrientation == CORONAL)
                {
                    for (k = 0; k < imageActive.getExtents()[2]; k++)
                    {
                        voiPoints = VOIs.VOIAt(i).exportPoints(k);
                        for (j = 0; j < voiPoints.length; j++)
                        {
                            if ( ( (VOIPoint) (VOIs.VOIAt(i).getCurves()[k].elementAt(j))).nearPointInPlane(xOrg,
                                zOrg, yOrg, CORONAL))
                            {
                                VOIs.VOIAt(i).setActive(true);
                                ( (VOIPoint) (VOIs.VOIAt(i).getCurves()[k].elementAt(j))).setActive(true);
                                voiID = VOIs.VOIAt(i).getID();
                                pt = ( (VOIPoint) (VOIs.VOIAt(i).getCurves()[k].elementAt(j))).exportPoint();
                            }
                        }
                    }
                } // else if (originalOrientation == CORONAL)
                else if (originalOrientation == SAGITTAL)
                {
                    for (k = 0; k < imageActive.getExtents()[2]; k++)
                    {
                        voiPoints = VOIs.VOIAt(i).exportPoints(k);
                        for (j = 0; j < voiPoints.length; j++)
                        {
                            if ( ( (VOIPoint) (VOIs.VOIAt(i).getCurves()[k].elementAt(j))).nearPointInPlane(zOrg,
                                yOrg, xOrg, SAGITTAL))
                            {
                                VOIs.VOIAt(i).setActive(true);
                                ( (VOIPoint) (VOIs.VOIAt(i).getCurves()[k].elementAt(j))).setActive(true);
                                voiID = VOIs.VOIAt(i).getID();
                                pt = ( (VOIPoint) (VOIs.VOIAt(i).getCurves()[k].elementAt(j))).exportPoint();
                            }
                        }
                    }
                } // else if (originalOrientation == SAGITTAL)


                if (pt != null)
                {
                    Point3D volumePt = getVolumePosition( (int) pt.x, (int) pt.y, (int) pt.z);

                    anchorPt.setLocation(volumePt.x, volumePt.y);

                    lastZOrg = zOrg;
                    setMode(MOVE_VOIPOINT);
                    imageActive.notifyImageDisplayListeners();
                }

            } // if (VOIs.VOIAt(i).getCurveType() == VOI.POINT)
        } // for (i = 0; i < nVOI; i++)
    }

    /**
     *  A mouse-released event.  This function sets up and draws the VOI according to the mode.
     *  @param mouseEvent  event that triggered this function
     */
    public void mouseReleased(MouseEvent mouseEvent)
    {
        dragBBpt = -1;
        int xOrg = 0;
        int yOrg = 0;
        int zOrg = 0;
        int i, j, k;
        int nVOI;
        boolean doNewVOI;
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();

        int xS = getScaledX(mouseEvent.getX()); // zoomed x.  Used as cursor
        int yS = getScaledY(mouseEvent.getY()); // zoomed y.  Used as cursor

        if (xS < 0 || xS >= imageDim.width || yS < 0 || yS >= imageDim.height)
        {
            return;
        }

        moveLine = false;
        moveLineEndpoint = false;

        if (mode == PAINT_VOI || mode == ERASER_PAINT)
        {
            imageActive.notifyImageDisplayListeners();
        }
        else if (mode == MOVE_POINT)
        {
            if (voiProtractor != null && moveProtractor)
            {
                float[] x = new float[3];
                float[] y = new float[3];
                float[] z = new float[3];

                voiProtractor.exportArrays(x, y, z, slice);

                theta = ( (VOIProtractor) (voiProtractor.getCurves()[slice].elementAt(0))).getTheta2();

                for (j = 0; j < imageActive.getExtents()[axisOrder[2]]; j++)
                {
                    voiProtractor.removeCurves(j);
                    z[0] = j;
                    z[1] = j;
                    z[2] = j;
                    voiProtractor.importCurve(x, y, z, j);
                    //( (VOIProtractor) (voiProtractor.getCurves()[j].elementAt(0))).setSnap(true);
                }

                ( (VOIProtractor) (voiProtractor.getCurves()[slice].elementAt(0))).setActive(true);
                voiProtractor.setActive(true);
                repaint();

                // the call below is needed because nearOuterPoint actually changes the state of the
                // voiProtractor. this prevents an exception that happened when the user dragged the
                // outer point of the protractor and released the mouse, and then re-dragged it without
                // moving the mouse position again
                voiProtractor.nearOuterPoint(mouseEvent.getX(), mouseEvent.getY(), slice, 0, getZoomX(), resolutionX,
                                             resolutionY);
            }
            if (intensityLine != null)
            {
                float[] x = new float[2];
                float[] y = new float[2];
                float[] z = new float[2];

                intensityLine.exportArrays(x, y, z, slice);
                for (j = 0; j < imageActive.getExtents()[axisOrder[2]]; j++)
                {
                    intensityLine.removeCurves(j);
                    z[0] = j;
                    z[1] = j;
                    intensityLine.importCurve(x, y, z, j);
                }
                intensityLine.setXYDim(imageDim.width, imageDim.height);
            }

            return;
        } // else if (mode == MOVE_POINT)
        else if (mode == MOVE_VOIPOINT)
        {
            setMode(DEFAULT);
            imageActive.notifyImageDisplayListeners(null, true);
        }
        else if (mode == CUBE_BOUNDS)
        {
            setCursor(crosshairCursor);
        } // else if (mode == CUBE_BOUNDS)
        else if (mode == PAINT_CAN)
        {
            Point3D mousePt = getVolumePosition(xS, yS, slice);

            xPG = (short) mousePt.x;
            yPG = (short) mousePt.y;
            zPG = (short) mousePt.z;

            if (imageActive.isColorImage()) {
                int index = 4 * (yS + imageActive.getExtents()[0] + xS);
                seedValR = imageBufferActive[index + 1];
                seedValG = imageBufferActive[index + 2];
                seedValB = imageBufferActive[index + 3];
                regionGrow( (short) xS, (short) yS, (short) slice, seedValR,
                           seedValG, seedValB, null, true);
            }
            else {
                seedVal = imageBufferActive[yS * imageActive.getExtents()[0] + xS];
                regionGrow( (short) xS, (short) yS, (short) slice, seedVal, null, true);
            }

        } // end of else if (mode == PAINT_CAN)
        else if (mode == POINT_VOI)
        {
            if ( (mouseEvent.getModifiers() & MouseEvent.BUTTON1_MASK) != 0)
            {
                Point3D mousePt = getVolumePosition(xS, yS, slice);
                xOrg = mousePt.x;
                yOrg = mousePt.y;
                zOrg = mousePt.z;

                doNewVOI = true;
                ViewVOIVector VOIs = imageActive.getVOIs();

                nVOI = VOIs.size();
                for (i = nVOI - 1; i >= 0; i--)
                {
                    if (VOIs.VOIAt(i).getCurveType() == VOI.POINT)
                    {
                        doNewVOI = false;
                    }
                }
                float[] x = new float[1];
                float[] y = new float[1];
                float[] z = new float[1];

                // voiID is a protected variable set to -1 by NEW_VOI in ViewJComponentEditImage
                if ( (voiID == -1) || (doNewVOI))
                { // create new VOI
                    VOI newPointVOI;
                    try
                    {
                        voiID = imageActive.getVOIs().size();
                        newPointVOI = new VOI( (short) imageActive.getVOIs().size(), "point3D_" + (voiID + 1),
                                              imageActive.getExtents()[2], VOI.POINT, -1.0f);
                        x[0] = xOrg;
                        y[0] = yOrg;
                        z[0] = zOrg;
                        newPointVOI.importCurve(x, y, z, zOrg);
                    }
                    catch (OutOfMemoryError error)
                    {
                        System.gc();
                        MipavUtil.displayError("Out of memory: ComponentTriImage.mouseReleased");
                        setMode(DEFAULT);
                        return;
                    }
                    // lastPointVOI is a protected variable in ViewJComponentEditImage
                    lastPointVOI = voiID;

                    if (this == triImageFrame.getTriImage(ViewJFrameTriImage.AXIAL_AB) ||
                        this == triImageFrame.getTriImage(ViewJFrameTriImage.SAGITTAL_AB) ||
                        this == triImageFrame.getTriImage(ViewJFrameTriImage.CORONAL_AB))
                    {
                        // was pressed on a composite image
                        int selectedImage = triImageFrame.getSelectedImage();

                        if (selectedImage == ViewJComponentBase.IMAGE_A || selectedImage == ViewJComponentBase.BOTH)
                        {
                            imageA.registerVOI(newPointVOI);
                            //System.out.println("registering A");
                        }

                        if (selectedImage == ViewJComponentBase.IMAGE_B || selectedImage == ViewJComponentBase.BOTH)
                        {
                            if (imageB != null) // should never be null, but just in case
                            {
                                //System.out.println("registering B");
                                imageB.registerVOI(newPointVOI);
                            }
                        }
                    }
                    else
                    {
                        imageA.registerVOI(newPointVOI);
                    }

                    newPointVOI.setActive(true);
                    ( (VOIPoint) (VOIs.VOIAt(voiID).getCurves()[ (int) z[0]].elementAt(0))).setActive(true);
                    frame.updateImages();
                    triImageFrame.updatevoiID(voiID);
                    triImageFrame.setTraverseButton();
                    if (mouseEvent.isShiftDown() != true)
                    {
                        triImageFrame.setDefault();
                    }
                } // end of if ((voiID == -1) || (doNewVOI))
                else
                { // add point to existing VOI
                    int index;

                    x[0] = xOrg;
                    y[0] = yOrg;
                    z[0] = zOrg;

                    for (i = 0; i < nVOI; i++)
                    {
                        if (VOIs.VOIAt(i).getID() == voiID)
                        {
                            if (VOIs.VOIAt(i).getCurveType() == VOI.POINT)
                            {
                                VOIs.VOIAt(i).importCurve(x, y, z, zOrg);
                                break;
                            }
                            else
                            {
                                MipavUtil.displayError("Can't add point VOI to other VOI structure.");
                                return;
                            }
                        }
                    }

                    /*int end = imageActive.getExtents()[2];
                    for (j = 0; j < end; j++)
                    {
                        index = VOIs.VOIAt(i).getCurves()[j].size();
                        for (k = 0; k < index; k++)
                        {
                            ( (VOIPoint) (VOIs.VOIAt(i).getCurves()[j].elementAt(k))).setActive(false);
                        }
                    }*/

                    index = VOIs.VOIAt(i).getCurves()[ (int) z[0]].size();
                    ( (VOIPoint) (VOIs.VOIAt(i).getCurves()[ (int) z[0]].elementAt(index - 1))).setActive(true);
                    frame.updateImages();
                    triImageFrame.setTraverseButton();
                    triImageFrame.updatevoiID(voiID);
                    if (mouseEvent.isShiftDown() != true)
                    {
                        triImageFrame.setDefault();
                    }
                    return;
                } // end of else add point to existing VOI
            } // end of if ((mouseEvent.getModifiers() & mouseEvent.BUTTON1_MASK) != 0)
        } // end of else if (mode == POINT_VOI)

        return;
    }

    /**
     * A mouse-dragged event.
     * @param mouseEvent  event that triggered function
     */
    public void mouseDragged(MouseEvent mouseEvent)
    {
        int i, j;
        String pointString;
        int distX, distY;
        int nVOI;
        ViewVOIVector VOIs = imageActive.getVOIs();
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();

        int xS = getScaledX(mouseEvent.getX());
        int yS = getScaledY(mouseEvent.getY());

        if (xS < 0)
        {
            xS = 0;
        }
        if (yS < 0)
        {
            yS = 0;
        }

        if (xS > imageDim.width - 1)
        {
            xS = imageDim.width - 1;
        }
        if (yS > imageDim.height - 1)
        {
            yS = imageDim.height - 1;
        }

        if (moveLineEndpoint) // if user is dragging the intensityLineEndpoint
        {
            if (intensityLine != null && intensityLine.isVisible())
            {
                intensityLine.rubberbandVOI(xS, yS, slice, imageDim.width, imageDim.height, false);
                repaint();
            }

            return;
        }

        if (moveLine) // if user is moving the intensityLine
        {
            if (intensityLine != null && intensityLine.isVisible())
            {
                distX = xS - anchorPt.x; // distance from original to cursor
                distY = yS - anchorPt.y;

                intensityLine.setActive(true);
                ( (VOILine) (intensityLine.getCurves()[slice].elementAt(0))).setActive(true);

                for (j = 0; j < imageActive.getExtents()[axisOrder[2]]; j++)
                {
                    intensityLine.moveVOI(j, imageDim.width, imageDim.height, 0, distX, distY, 0);
                }

                anchorPt.x = xS;
                anchorPt.y = yS;

                repaint();
            }
            return;
        }

        if (dragCenterPt) // if user is moving the center point
        {
            if (doCenter)
            {
                setCursor(blankCursor);
                Point3D point3d = getVolumePosition(xS, yS, slice);

                triImageFrame.setVolumeCenter(point3d);

                float zoomer = 1;
                if (resolutionY == resolutionX) {
                    zoomer = Math.max(res[2], res[0]) / Math.min(res[2], res[0]);
                }

                float flipper = slice;
                // Because the upper left hand of the image is the origin and therefore the y coordinate must be inverted.
                if (hasOrientation && (triComponentOrientation == AXIAL)) {
                    flipper = (imageActive.getExtents()[axisOrder[2]] - 1) - flipper;
                }

                flipper = flipper * zoomer * getZoomY();
                triImageFrame.setCrosshairs(mouseEvent.getX(), mouseEvent.getY(),
                                            (int) ( flipper + (0.5f * zoomer) ),
                                            triComponentOrientation);

                // Because the upper left hand of the image is the origin and therefore the y coordinate must be inverted.
                if (hasOrientation && (triComponentOrientation == SAGITTAL ||
                                       triComponentOrientation == CORONAL))
                {
                    yS = imageActive.getExtents()[axisOrder[1]] - yS - 1;
                }

                triImageFrame.setDisplaySlices(xS, yS, slice, triComponentOrientation);

                frame.updateImages();
                return;
            }
        }

        if (mode == DEFAULT || mode == MOVE_VOIPOINT || mode == PROTRACTOR)
        {
            // Hides the cursor during dragging so it doesn't get in the way.
            if (showCrosshairs == true)
            {
                setCursor(blankCursor);
            }
            updateFrameLabels(frame, xS, yS);

            float zoomer = 1;
            if (resolutionY == resolutionX) {
                zoomer = Math.max(res[2], res[0]) / Math.min(res[2], res[0]);
            }

            float flipper = slice;
            // Because the upper left hand of the image is the origin and therefore the y coordinate must be inverted.
            if (hasOrientation && (triComponentOrientation == AXIAL))
            {
                flipper = (imageActive.getExtents()[axisOrder[2]]-1) - flipper;
            }

            flipper = flipper * zoomer * getZoomY();
            triImageFrame.setCrosshairs(mouseEvent.getX(), mouseEvent.getY(),
                                       (int) ( flipper + (0.5f * zoomer) ), triComponentOrientation);

            // Because the upper left hand of the image is the origin and therefore the y coordinate must be inverted.
            if (hasOrientation && (triComponentOrientation == SAGITTAL || triComponentOrientation == CORONAL))
            {
                yS = imageActive.getExtents()[axisOrder[1]] - yS - 1;
            }
            triImageFrame.setDisplaySlices(xS, yS, slice, triComponentOrientation);

            if (triImageFrame.linkTriFrame != null)
            {
                triImageFrame.linkTriFrame.setSlices(xS, yS, slice);
                triImageFrame.linkTriFrame.updateImages(true);
            }
        } // if (mode == DEFAULT || mode == MOVE_VOIPOINT || mode == CUBE_BOUNDS || mode == PROTRACTOR)

        else if (mode == MOVE)
        {
            distX = xS - anchorPt.x; // distance from original to cursor
            distY = yS - anchorPt.y;

            if (voiProtractor != null && voiProtractor.isVisible() && moveProtractor)
            {
                for (j = 0; j < imageActive.getExtents()[axisOrder[2]]; j++)
                {
                    voiProtractor.moveVOI(j, imageDim.width, imageDim.height, 0, distX, distY, 0);
                }
            }

            if (intensityLine != null && intensityLine.isVisible() && moveLine)
            {
                for (j = 0; j < imageActive.getExtents()[axisOrder[2]]; j++)
                {
                    intensityLine.moveVOI(j, imageDim.width, imageDim.height, 0, distX, distY, 0);
                }
            }

            anchorPt.x = xS;
            anchorPt.y = yS;

        } // end of else if (mode == MOVE)

        if (mode == MOVE_VOIPOINT)
        {
            nVOI = VOIs.size();
            boolean found = false;
            for (i = 0; ( (i < nVOI) && (!found)); i++)
            {
                if (VOIs.VOIAt(i).isActive() && VOIs.VOIAt(i).isVisible())
                {
                    if (VOIs.VOIAt(i).getCurveType() == VOI.POINT)
                    {
                        Point3D mousePt = getVolumePosition(xS, yS, slice);

                        found = true;

                        // the reason for this k = lastZOrg-1 loop is because the VOI point lies right on
                        // the slice edge. if this loop was not present, the user could only grab the
                        // point from side, which is confusing since the point is drawn on the
                        // edge of the slice. the loop tests both sides of the VOI point (both
                        // surrounding slices) and thus makes it more easier to grab the point
                        for (int k = lastZOrg-1; k <= lastZOrg; k++)
                        {
                            Point3Df[] voiPoints = VOIs.VOIAt(i).exportPoints(k);
                            for (j = 0; j < voiPoints.length; j++)
                            {
                                if ( ( (VOIPoint) (VOIs.VOIAt(i).getCurves()[k].elementAt(j))).isActive())
                                {
                                    if (! ( (VOIPoint) (VOIs.VOIAt(i).getCurves()[k].elementAt(j))).isFixed())
                                    {
                                        pointString = ( (VOIPoint) (VOIs.VOIAt(i).getCurves()[k].elementAt(j))).
                                            getLabel();
                                        VOIs.VOIAt(i).getCurves()[k].removeElementAt(j);
                                        int[] x = new int[1];
                                        int[] y = new int[1];
                                        int[] z = new int[1];

                                        x[0] = mousePt.x;
                                        y[0] = mousePt.y;
                                        z[0] = mousePt.z;
                                        VOIBase pt = new VOIPoint();

                                        // Because the upper left hand of the image is the origin and therefore the y coordinate must be inverted.
                                        if (hasOrientation && (triComponentOrientation == SAGITTAL ||
                                                               triComponentOrientation == CORONAL))
                                        {
                                            y[0] = imageActive.getExtents()[axisOrder[1]] - y[0] - 1;
                                        }

                                        pt.importArrays(x, y, z, x.length);
                                        VOIs.VOIAt(i).getCurves()[mousePt.z].addElement(pt);
                                        ( (VOIPoint) (VOIs.VOIAt(i).getCurves()[mousePt.z].lastElement())).setLabel(
                                            pointString);
                                        ( (VOIPoint) (VOIs.VOIAt(i).getCurves()[mousePt.z].lastElement())).setActive(true);
                                        lastZOrg = mousePt.z;
                                    }
                                }
                            }
                        }
                    }
                }
            }

            anchorPt.x = xS;
            anchorPt.y = yS;
            frame.updateImages();
            return;
        } // end of else if (mode == MOVE_VOIPOINT)
        else if (mode == CUBE_BOUNDS)
        {
            // get array representing the 8 corners of the bounding box
            Point3D [] boundingBoxPoints = triImageFrame.getBoundingBoxPoints();

            // get the volume position of the mouse event
            Point3D volumeMousePoint = getTriImagePosition(mouseEvent.getX(), mouseEvent.getY());

            // if this is the image's original orientation
            if (axisOrder[2] == 2)
            {
                // if we are not already dragging a point, see if the mouse event is near one of the corners
                if (dragBBpt == -1)
                {
                    for (i = 0; i < boundingBoxPoints.length; i++)
                    {
                        if (Math.abs(boundingBoxPoints[i].x - volumeMousePoint.x) < 5 &&
                            Math.abs(boundingBoxPoints[i].y - volumeMousePoint.y) < 5)
                        {
                            // if we are dragging near a box corner, set 'dragBBpt' to indicate that point
                            dragBBpt = i;
                            break;
                        }
                    }

                }

                // if a point is being dragged
                if (dragBBpt != -1)
                {
                    // create Vector to hold points that are coplanar
                    Vector coplanarPoints = new Vector();

                    // find other points that are coplanar with this one
                    for (i = 0; i < boundingBoxPoints.length; i++)
                    {
                        if (i == dragBBpt)
                        {
                            continue;
                        }

                        Point3D potentiallyCoplanarPoint = boundingBoxPoints[i];

                        // test the potentially coplanar point to see if it is, in fact, coplanar
                        if (boundingBoxPoints[dragBBpt].x == potentiallyCoplanarPoint.x ||
                            boundingBoxPoints[dragBBpt].y == potentiallyCoplanarPoint.y)
                        {
                            coplanarPoints.add(potentiallyCoplanarPoint);
                        }
                    }

                    // for each coplanar point
                    for (i = 0; i < coplanarPoints.size(); i++)
                    {
                        Point3D commonPoint = (Point3D) coplanarPoints.elementAt(i);

                        // set coplanar dimensions equal, therefore equalizing the point on a common plane
                        if (boundingBoxPoints[dragBBpt].x == commonPoint.x)
                        {
                            commonPoint.x = volumeMousePoint.x;
                        }

                        // set coplanar dimensions equal, therefore equalizing the point on a common plane
                        if (boundingBoxPoints[dragBBpt].y == commonPoint.y)
                        {
                            commonPoint.y = volumeMousePoint.y;
                        }
                    }

                    // finally, set dragged point to new mouse value
                    boundingBoxPoints[dragBBpt].x = volumeMousePoint.x;
                    boundingBoxPoints[dragBBpt].y = volumeMousePoint.y;

                }
            }

            // the following 2 code sections are the same as the one above, except they have
            // been modified for their individual orientations (i.e. point.x becomes point.z, etc)
            if (axisOrder[2] == 1)
            {
                if (dragBBpt == -1)
                {
                    for (i = 0; i < boundingBoxPoints.length; i++)
                    {
                        if (Math.abs(boundingBoxPoints[i].x - volumeMousePoint.x) < 5 &&
                            Math.abs(boundingBoxPoints[i].z - volumeMousePoint.z) < 5)
                        {
                            dragBBpt = i;
                            break;
                        }
                    }

                }

                if (dragBBpt != -1)
                {
                    Vector commonPoints = new Vector();

                    // find other points that correlate with this one
                    for (i = 0; i < boundingBoxPoints.length; i++)
                    {
                        if (i == dragBBpt)
                        {
                            continue;
                        }

                        Point3D potentiallyCommonPoint = boundingBoxPoints[i];

                        if (boundingBoxPoints[dragBBpt].x == potentiallyCommonPoint.x ||
                            boundingBoxPoints[dragBBpt].z == potentiallyCommonPoint.z)
                        {
                            commonPoints.add(potentiallyCommonPoint);
                        }
                    }

                    for (i = 0; i < commonPoints.size(); i++)
                    {
                        Point3D commonPoint = (Point3D) commonPoints.elementAt(i);

                        if (boundingBoxPoints[dragBBpt].x == commonPoint.x)
                        {
                            commonPoint.x = volumeMousePoint.x;
                        }

                        if (boundingBoxPoints[dragBBpt].z == commonPoint.z)
                        {
                            commonPoint.z = volumeMousePoint.z;
                        }
                    }

                    boundingBoxPoints[dragBBpt].x = volumeMousePoint.x;
                    boundingBoxPoints[dragBBpt].z = volumeMousePoint.z;

                }
            }

            if (axisOrder[2] == 0)
            {
                if (dragBBpt == -1)
                {
                    for (i = 0; i < boundingBoxPoints.length; i++)
                    {
                        if (Math.abs(boundingBoxPoints[i].y - volumeMousePoint.y) < 5 &&
                            Math.abs(boundingBoxPoints[i].z - volumeMousePoint.z) < 5)
                        {
                            dragBBpt = i;
                            break;
                        }
                    }

                }

                if (dragBBpt != -1)
                {
                    Vector commonPoints = new Vector();

                    // find other points that correlate with this one
                    for (i = 0; i < boundingBoxPoints.length; i++)
                    {
                        if (i == dragBBpt)
                        {
                            continue;
                        }

                        Point3D potentiallyCommonPoint = boundingBoxPoints[i];

                        if (boundingBoxPoints[dragBBpt].y == potentiallyCommonPoint.y ||
                            boundingBoxPoints[dragBBpt].z == potentiallyCommonPoint.z)
                        {
                            commonPoints.add(potentiallyCommonPoint);
                        }
                    }

                    for (i = 0; i < commonPoints.size(); i++)
                    {
                        Point3D commonPoint = (Point3D) commonPoints.elementAt(i);

                        if (boundingBoxPoints[dragBBpt].y == commonPoint.y)
                        {
                            commonPoint.y = volumeMousePoint.y;
                        }

                        if (boundingBoxPoints[dragBBpt].z == commonPoint.z)
                        {
                            commonPoint.z = volumeMousePoint.z;
                        }
                    }

                    boundingBoxPoints[dragBBpt].y = volumeMousePoint.y;
                    boundingBoxPoints[dragBBpt].z = volumeMousePoint.z;

                }
            }


        }

        else if (mode == LINE &&
                 intensityLine == null &&
                 intensityLineVisible &&
                 ( (anchorPt.x != getScaledX(mouseEvent.getX())) ||
                  (anchorPt.y != getScaledY(mouseEvent.getY()))))
        {

            int[] x = null;
            int[] y = null;
            int[] z = null;

            try
            {
                x = new int[2];
                y = new int[2];
                z = new int[2];
            }
            catch (OutOfMemoryError error)
            {
                System.gc();
                MipavUtil.displayError("Out of memory: ComponentTriImage.mouseReleased");
                setMode(DEFAULT);
                return;
            }

            VOIs = imageActive.getVOIs(); // Get the VOIs from the active image.

            if (triComponentOrientation == AXIAL || !hasOrientation)
            {
                intensityLine = new VOI( (short) imageActive.getVOIs().size(), "xyline.voi",
                                        imageActive.getExtents()[axisOrder[2]], VOI.LINE, 0.0f); // 0.0f for first segment red hue
            }
            else if (triComponentOrientation == CORONAL)
            {
                // 1.0f/3.0f for first segment green hue
                intensityLine = new VOI( (short) imageActive.getVOIs().size(), "xzline.voi",
                                        imageActive.getExtents()[axisOrder[2]], VOI.LINE, 0.3333f);
            }
            else // (triComponentOrientation == SAGITTAL)
            {
                intensityLine = new VOI( (short) imageActive.getVOIs().size(), "zyline.voi",
                                        imageActive.getExtents()[axisOrder[2]], VOI.LINE, 1.0f / 6.0f);
            }

            for (j = 0; j < imageActive.getExtents()[axisOrder[2]]; j++)
            {
                x[0] = anchorPt.x;
                x[1] = xS;
                y[0] = anchorPt.y;
                y[1] = yS;
                z[0] = j;
                z[1] = j;
                intensityLine.importCurve(x, y, z, j);
                ( (VOILine) (intensityLine.getCurves()[j].elementAt(0))).setActive(true);
            }
            intensityLine.setXYDim(imageDim.width, imageDim.height);
            intensityLine.setActive(true);
            repaint();
            if (intensityLine.nearLinePoint(MipavMath.round(xS * getZoomX() * resolutionX),
                                            MipavMath.round(yS * getZoomY() * resolutionY),
                                            slice, 0, getZoomX(), resolutionX, resolutionY))
            {
                setMode(MOVE_POINT);
                moveProtractor = false;
            }
        } // else if ((mode == LINE) && (startLine) &&
        // ((anchorPt.x != xSOrg) || (anchorPt.y != ySOrg)))
        else if (mode == MOVE_POINT)
        {

            if (voiProtractor != null && voiProtractor.isActive() && moveProtractor)
            {
                voiProtractor.rubberbandVOI(xS, yS, slice, imageDim.width, imageDim.height, false);
            }

            if (intensityLine != null)
            {
                intensityLine.rubberbandVOI(xS, yS, slice, imageDim.width, imageDim.height, false);
            }
        } // end of else if (mode == MOVE_POINT)
        else if (mode == PAINT_VOI)
        {
            boolean isLeftMouseButtonDown = mouseEvent.getModifiers() == MouseEvent.BUTTON1_MASK;
            updatePaintBitmap(isLeftMouseButtonDown, xS, yS);
            if (Preferences.is(Preferences.PREF_FAST_TRIPLANAR_REPAINT))
            {
                repaint();
                return;
            }
        } // end of else if (mode == PAINT_VOI)
        else if (mode == ERASER_PAINT)
        {
            updatePaintBitmap(false, xS, yS);
            if (Preferences.is(Preferences.PREF_FAST_TRIPLANAR_REPAINT))
            {
                repaint();
                return;
            }
        } // end of else if (mode == ERASER_PAINT)
        else if (mode == DROPPER_PAINT)
        {
            if (imagesDone == IMAGE_A)
            {
                intensityDropper = imageBufferA[yS * imageDim.width + xS];
            }
            else
            {
                intensityDropper = imageBufferB[yS * imageDim.width + xS];
            }
            if (triComponentOrientation == AXIAL || !hasOrientation)
            {
                triImageFrame.setXZIntensityDropper(intensityDropper);
                triImageFrame.setZYIntensityDropper(intensityDropper);
            }
            else if (triComponentOrientation == CORONAL)
            {
                triImageFrame.setXYIntensityDropper(intensityDropper);
                triImageFrame.setZYIntensityDropper(intensityDropper);
            }
            else
            {
                triImageFrame.setXYIntensityDropper(intensityDropper);
                triImageFrame.setXZIntensityDropper(intensityDropper);
            }
            triImageFrame.setIntensityPaintName(intensityDropper);

            return;
        }

        frame.updateImages();
    }

    public void setMode(int newMode)
    {
        if (newMode == MOVE_POINT)
        {
            this.mode = newMode;
            rubberband.setActive(false);
            setCursor(crosshairCursor);
        }
        else if (newMode == LINE)
        {
            if (intensityLine != null)
            {
                return;
            }
        }
        else
        {
            super.setMode(newMode);
        }

        setCursor(crosshairCursor);
    }

    /**
     * A mouse event.  If the mode is level set, draws level sets as user moves mouse.  Otherwise, changes
     * the cursor depending on where the mouse is in relation to the VOI.
     * @param mouseEvent  event that triggered the function
     */
    public void mouseMoved(MouseEvent mouseEvent)
    {
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();

        int x = mouseEvent.getX();
        int y = mouseEvent.getY();

        int xS = getScaledX(mouseEvent.getX());
        int yS = getScaledY(mouseEvent.getY());

        if (xS < 0 || xS >= imageDim.width || // Check to ensure point is within image bounds
            yS < 0 || yS >= imageDim.height)
        {
            return;
        }

        if (intensityLine == null && intensityLineVisible)
        {
            // if we get here, it means the intensity line button is pressed, but there is no line present
            mode = LINE;
            return;
        }

        if (mode == POINT_VOI || mode == DROPPER_PAINT || mode == CUBE_BOUNDS)
        {
            return;
        }

        if (mode == PAINT_VOI || mode == ERASER_PAINT)
        {
            repaint();
            return;
        }

        if (mode == PAINT_CAN)
        {
            if (growDialog != null)
            {
                // the "+ 1" is there to make the displayed coordinates 1-based instead of 0-based
                Point3D point = getVolumePosition(xS + 1, yS + 1, slice + 1);

                if (triComponentOrientation == AXIAL || !hasOrientation)
                {
                    growDialog.setPositionText(
                        "  X: " + String.valueOf(point.x) + " Y: " + String.valueOf(point.y) + " Z: "
                        + String.valueOf(point.z) + "  Intensity:  "
                        + String.valueOf(imageBufferActive[yS * imageDim.width + xS]));
                }
                else if (triComponentOrientation == CORONAL)
                {
                    growDialog.setPositionText(
                        "  X: " + String.valueOf(point.x) + " Y: " + String.valueOf(point.y) + " Z: "
                        + String.valueOf(point.z) + "  Intensity:  "
                        + String.valueOf(imageBufferActive[yS * imageDim.width + xS]));
                }
                else
                { // triComponentOrientation == SAGITTAL
                    growDialog.setPositionText(
                        "  X: " + String.valueOf(point.x) + " Y: " + String.valueOf(point.y) + " Z: "
                        + String.valueOf(point.z) + "  Intensity:  "
                        + String.valueOf(imageBufferActive[yS * imageDim.width + xS]));
                }
            }
            return;
        }

        if (voiProtractor != null)
        {
            if (voiProtractor.nearOuterPoint(x, y, slice, 0, getZoomX(), resolutionX, resolutionY))
            {
                setMode(MOVE_POINT);
                moveProtractor = true;
            }
            else if (voiProtractor.isVisible() &&
                     voiProtractor.nearLine(xS, yS, slice))
            {
                setMode(MOVE);
                moveProtractor = true;
            }
            return;
        }

        else if (mode == LINE || mode == MOVE_POINT)
        {
            return;
        }
        else
        {
            setMode(DEFAULT);
            return;
        } // else
    }

    /**
     * Returns true if mouse point is close to bounds point.
     * @param mouseX   the x component of the mouse click within the component
     * @param mouseY   the y component of the mouse click within the component
     * @param boundsX  the x component of the bounds point to check against
     * @param boundsY  the y component of the bounds point to check against
     * @return         true if mouse point is close to bounds point
     */
    public boolean nearBoundsPoint(int mouseX, int mouseY, int boundsX, int boundsY)
    {
        float boundsXD, boundsYD;
        double dist;

        /*if ( (triComponentOrientation == SAGITTAL) && (hasOrientation))
                 {
            boundsXD = boundsX * zoomY * resolutionY;
            boundsYD = boundsY * zoomX * resolutionX;
                 }
                 else
                 {
            boundsXD = boundsX * zoomX * resolutionX;
            boundsYD = boundsY * zoomY * resolutionY;
                 }*/
        Point3D pt = getTriImagePosition(mouseX, mouseY);
        mouseX = pt.x;
        mouseY = pt.y;
        int mouseZ = pt.z;

        dist = Math.sqrt( (mouseX - boundsX) * (mouseX - boundsX) + (mouseY - boundsY) * (mouseY - boundsY));

        //dist = Math.sqrt( (mouseX - boundsXD) * (mouseX - boundsXD) + (mouseY - boundsYD) * (mouseY - boundsYD));
        if (dist < 3.0)
        {
            return true;
        }
        else
        {
            return false;
        }
    }

    /**
     * Clean up memory used by the component.
     * @throws Throwable  if there is a problem encountered during memory clean-up
     * @see #disposeLocal(boolean)
     */
    protected void finalize() throws Throwable
    {
        disposeLocal(true);
        super.finalize();
    }

    /**
     * Sets all variables to null, disposes, and garbage collects (sometimes).
     * @param flag  if true garbage collector should be called.
     */
    public void disposeLocal(boolean flag)
    {
        voiProtractor = null;
        intensityLine = null;

        anchorPt = null;

        if (flag == true)
        {
            super.disposeLocal(true);
        }
    }

    /**
     * Set a talairach / ACPC reference point.
     * @param pointType  the reference point type
     * @param pt         the point position (in image volume space)
     */
    public void setReferenceXY(int pointType, Point3Df pt)
    {
        float[] x = new float[1];
        float[] y = new float[1];
        float[] z = new float[1];
        VOI newPointVOI;

        if (triComponentOrientation == AXIAL || !hasOrientation)
        {
            switch (axisOrder[0])
            {
                case 0:
                    x[0] = pt.x;
                    if (axisFlip[0])
                    {
                        x[0] = imageActive.getExtents()[0] - x[0] - 1;
                    }
                    break;

                case 1:
                    y[0] = pt.x;
                    if (axisFlip[0])
                    {
                        y[0] = imageActive.getExtents()[1] - y[0] - 1;
                    }
                    break;

                case 2:
                    z[0] = pt.x;
                    if (axisFlip[0])
                    {
                        z[0] = imageActive.getExtents()[2] - z[0] - 1;
                    }
                    break;
            }

            switch (axisOrder[1])
            {
                case 0:
                    x[0] = pt.y;
                    if (axisFlip[1])
                    {
                        x[0] = imageActive.getExtents()[0] - x[0] - 1;
                    }
                    break;

                case 1:
                    y[0] = pt.y;
                    if (axisFlip[1])
                    {
                        y[0] = imageActive.getExtents()[1] - y[0] - 1;
                    }
                    break;

                case 2:
                    z[0] = pt.y;
                    if (axisFlip[1])
                    {
                        z[0] = imageActive.getExtents()[2] - z[0] - 1;
                    }
                    break;
            }

            switch (axisOrder[2])
            {
                case 0:
                    x[0] = pt.z;
                    if (axisFlip[2])
                    {
                        x[0] = imageActive.getExtents()[0] - x[0] - 1;
                    }
                    break;

                case 1:
                    y[0] = pt.z;
                    if (axisFlip[2])
                    {
                        y[0] = imageActive.getExtents()[1] - y[0] - 1;
                    }
                    break;

                case 2:
                    z[0] = pt.z;
                    if (axisFlip[2])
                    {
                        z[0] = imageActive.getExtents()[2] - z[0] - 1;
                    }
                    break;
            }
        } // if (triComponentOrientation == XY)

        try
        {
            voiID = imageActive.getVOIs().size();
            newPointVOI = new VOI( (short) imageActive.getVOIs().size(), "point3D_" + (voiID + 1),
                                  imageActive.getExtents()[2],
                                  VOI.POINT, -1.0f);
            newPointVOI.importCurve(x, y, z, (int) z[0]);
        }
        catch (OutOfMemoryError error)
        {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentTriImage.setReferenceXY");
            setMode(DEFAULT);
            return;
        }
        // lastPointVOI is a protected variable in ViewJComponentEditImage
        lastPointVOI = voiID;
        imageActive.registerVOI(newPointVOI);
        triImageFrame.updatevoiID(voiID);
        ( (VOIPoint) (newPointVOI.getCurves()[ (int) z[0]].elementAt(0))).setFixed(true);

        switch (pointType)
        {
            case SUPERIOR_EDGE:
                ( (VOIPoint) (newPointVOI.getCurves()[ (int) z[0]].elementAt(0))).setLabel("ACS");
                break;

            case POSTERIOR_MARGIN:
                ( (VOIPoint) (newPointVOI.getCurves()[ (int) z[0]].elementAt(0))).setLabel("ACP");
                break;

            case INFERIOR_EDGE:
                ( (VOIPoint) (newPointVOI.getCurves()[ (int) z[0]].elementAt(0))).setLabel("PC");
                break;

            case FIRST_PT:
                ( (VOIPoint) (newPointVOI.getCurves()[ (int) z[0]].elementAt(0))).setLabel("MS1");
                break;

            case ANOTHER_PT:
                ( (VOIPoint) (newPointVOI.getCurves()[ (int) z[0]].elementAt(0))).setLabel("MS2");
                break;

            case ANTERIOR_PT:
                ( (VOIPoint) (newPointVOI.getCurves()[ (int) z[0]].elementAt(0))).setLabel("A");
                break;

            case POSTERIOR_PT:
                ( (VOIPoint) (newPointVOI.getCurves()[ (int) z[0]].elementAt(0))).setLabel("P");
                break;

            case SUPERIOR_PT:
                ( (VOIPoint) (newPointVOI.getCurves()[ (int) z[0]].elementAt(0))).setLabel("S");
                break;

            case INFERIOR_PT:
                ( (VOIPoint) (newPointVOI.getCurves()[ (int) z[0]].elementAt(0))).setLabel("I");
                break;

            case LEFT_PT:
                ( (VOIPoint) (newPointVOI.getCurves()[ (int) z[0]].elementAt(0))).setLabel("L");
                break;

            case RIGHT_PT:
                ( (VOIPoint) (newPointVOI.getCurves()[ (int) z[0]].elementAt(0))).setLabel("R");
                break;
        }
        frame.updateImages();
    }

    /**
     * Remove a talairach / ACPC reference point.
     * @param label  the label of the reference point to remove
     * @return       true if the requested reference point was found and removed
     */
    public boolean removeReference(String label)
    {
        String label2;
        boolean found = false;
        int i, j, k;
        ViewVOIVector VOIs = imageActive.getVOIs();
        int nVOI = VOIs.size();

        if (nVOI == 0)
        {
            return false; // no point present
        }
        for (i = 0; (i < nVOI) && (!found); i++)
        {
            if (VOIs.VOIAt(i).getCurveType() == VOI.POINT)
            {
                for (k = 0; (k < imageActive.getExtents()[2]) && (!found); k++)
                {
                    Point3Df[] voiPoints = VOIs.VOIAt(i).exportPoints(k);
                    for (j = 0; (j < voiPoints.length) && (!found); j++)
                    {
                        label2 = ( (VOIPoint) (VOIs.VOIAt(i).getCurves()[k].elementAt(j))).getLabel();
                        if (label.equals(label2))
                        {
                            VOIs.VOIAt(i).getCurves()[k].removeElementAt(j);
                            found = true;
                        }
                    }
                }
            }
        }
        frame.updateImages();
        return found;
    }

    /**
     * Delete all VOIs in the active image.
     */
    public void deleteAllVOIs()
    {
        int i;
        int nVOI;

        ViewVOIVector VOIs = imageActive.getVOIs();

        nVOI = VOIs.size();
        if (nVOI == 0)
        {
            return;
        }
        for (i = nVOI - 1; i >= 0; i--)
        {
            VOIs.removeElementAt(i);
        }
        voiID = -1;
        imageActive.notifyImageDisplayListeners(null, true);
    }

    /**
     * Delete the selected contour of a VOI in the active image.
     */
    public void deleteSelectedContours()
    {
        int i, s, nVOI;

        ViewVOIVector VOIs = imageActive.getVOIs();

        nVOI = VOIs.size();
        if (nVOI == 0)
        {
            return;
        }

        for (i = 0; i < nVOI; i++)
        {
            if (VOIs.VOIAt(i).isActive() == true)
            {
                break;
            } // Set i
        }
        if (i == nVOI)
        {
            MipavUtil.displayError("VOI must be selected.");
            return; // No VOI to delete
        }

        if (imageActive.getNDims() == 2)
        {
            deleteContour(VOIs.VOIAt(i), 0);
        }
        else if (imageActive.getNDims() >= 3)
        {
            for (s = 0; s < imageActive.getExtents()[2]; s++)
            {
                deleteContour(VOIs.VOIAt(i), s);
            }
        }

        if (VOIs.VOIAt(i).isEmpty() == true)
        {
            imageActive.unregisterVOI(VOIs.VOIAt(i));
            voiID = -1;
        }

        imageActive.notifyImageDisplayListeners(null, true);
    }

    /**
     * Change the color for the x crosshair.
     * @param c  the new crosshair color
     */
    public void setXSliceHairColor(Color c)
    {
        xColor = c;
    }

    /**
     * Change the color for the y crosshair.
     * @param c  the new crosshair color
     */
    public void setYSliceHairColor(Color c)
    {
        yColor = c;
    }

    /**
     * Change the color for the z crosshair.
     * @param c  the new crosshair color
     */
    public void setZSliceHairColor(Color c)
    {
        zColor = c;
    }

    /**
     * Get the color for the screenPt.x crosshair.
     * @return  the x crosshair color
     */
    public Color getXSliceHairColor()
    {
        return xColor;
    }

    /**
     * Get the color for the ySlice crosshair.
     * @return  the y crosshair color
     */
    public Color getYSliceHairColor()
    {
        return yColor;
    }

    /**
     * Get the color for the zSlice crosshair.
     * @return  the z crosshair color
     */
    public Color getZSliceHairColor()
    {
        return zColor;
    }

    /**
     * Accepts a point in volume space and converts it to screen space.
     * @param pt Point3D the point in volume space
     * @return Point
     */
    public Point getScreenCoordinates(Point3D point3d)
    {
        int x = 0, y = 0;

        switch (axisOrder[0])
        {
            case 0:
                x = point3d.x;
                if (axisFlip[0])
                {
                    x = (imageActive.getExtents()[0] - point3d.x - 1);
                }
                break;

            case 1:
                x = point3d.y;
                if (axisFlip[0])
                {
                    x = (imageActive.getExtents()[1] - point3d.y - 1);
                }
                break;

            case 2:
                x = point3d.z;
                if (axisFlip[0])
                {
                    x = (imageActive.getExtents()[2] - point3d.z - 1);
                }
                break;
        }

        switch (axisOrder[1])
        {
            case 0:
                y = point3d.x;
                if (axisFlip[1])
                {
                    y = (imageActive.getExtents()[0] - point3d.x - 1);
                }
                break;

            case 1:
                y = point3d.y;
                if (axisFlip[1])
                {
                    y = (imageActive.getExtents()[1] - point3d.y - 1);
                }
                break;

            case 2:
                y = point3d.z;
                if (axisFlip[1])
                {
                    y = (imageActive.getExtents()[2] - point3d.z - 1);
                }
                break;
        }

        x = (int) (x * getZoomX() * resolutionX);
        y = (int) (y * getZoomY() * resolutionY);

        return new Point(x, y);
    }

    /**
     * Translate a point from screen space into image volume space.
     * Assumes input parameters are in screen space (includes zoom and voxel resolutions).
     * @param x  the x screen coordinate
     * @param y  the y screen coordinate
     * @return   the point position within the volume
     */
    public final Point3D getTriImagePosition(int x, int y)
    {
        x = (int) (x / (getZoomX() * getResolutionX()));
        y = (int) (y / (getZoomY() * getResolutionY()));

        return getVolumePosition(x, y, slice);
    }

    /**
     * Translate a point on the x-y tri-image component into image volume space.
     * Assumes input parameters have zoom and voxel resolution already factored out.
     * @param x  x value of the point within the component
     * @param y  y value of the point within the component
     * @param z  the z coordinate ( usually == slice ) (the out-of-component dimension)
     * @return   the point translated into the image volume
     */
    public final Point3D getVolumePosition(int x, int y, int z)
    {
        Point3D pt = new Point3D();

        switch (axisOrder[0])
        {
            case 0:
                pt.x = x;
                if (axisFlip[0])
                {
                    pt.x = imageActive.getExtents()[0] - pt.x - 1;
                }
                break;

            case 1:
                pt.y = x;
                if (axisFlip[0])
                {
                    pt.y = imageActive.getExtents()[1] - pt.y - 1;
                }
                break;

            case 2:
                pt.z = x;
                if (axisFlip[0])
                {
                    pt.z = imageActive.getExtents()[2] - pt.z - 1;
                }
                break;
        }

        switch (axisOrder[1])
        {
            case 0:
                pt.x = y;
                if (axisFlip[1])
                {
                    pt.x = imageActive.getExtents()[0] - pt.x - 1;
                }
                break;

            case 1:
                pt.y = y;
                if (axisFlip[1])
                {
                    pt.y = imageActive.getExtents()[1] - pt.y - 1;
                }
                break;

            case 2:
                pt.z = y;
                if (axisFlip[1])
                {
                    pt.z = imageActive.getExtents()[2] - pt.z - 1;
                }
                break;
        }

        switch (axisOrder[2])
        {
            case 0:
                pt.x = z;
                if (axisFlip[2])
                {
                    pt.x = imageActive.getExtents()[0] - pt.x - 1;
                }
                break;

            case 1:
                pt.y = z;
                if (axisFlip[2])
                {
                    pt.y = imageActive.getExtents()[1] - pt.y - 1;
                }
                break;

            case 2:
                pt.z = z;
                if (axisFlip[2])
                {
                    pt.z = imageActive.getExtents()[2] - pt.z - 1;
                }
                break;
        }

        return pt;
    }

    /**
     * Gets the original triComponentOrientation.
     * @return  the original triComponentOrientation
     */
    public final int getOriginalOrientation()
    {
        int originalOrientation = NA;

        if (!hasOrientation)
        {
            return NA;
        }

        if (orientation == CORONAL)
        {
            switch (axisOrder[2])
            {
                case 0:
                    originalOrientation = SAGITTAL;
                    break;

                case 1:
                    originalOrientation = AXIAL;
                    break;

                case 2:
                    originalOrientation = CORONAL;
                    break;
            }
        }

        if (orientation == SAGITTAL)
        {
            switch (axisOrder[2])
            {
                case 0:
                    originalOrientation = CORONAL;
                    break;

                case 1:
                    originalOrientation = AXIAL;
                    break;

                case 2:
                    originalOrientation = SAGITTAL;
                    break;
            }
        }

        if (orientation == AXIAL)
        {
            switch (axisOrder[2])
            {
                case 0:
                    originalOrientation = SAGITTAL;
                    break;

                case 1:
                    originalOrientation = CORONAL;
                    break;

                case 2:
                    originalOrientation = AXIAL;
                    break;
            }
        }

        return originalOrientation;
    }

    /**
     * Updates the TriImage frame position labels based on the currently shown slices in this component.
     * @param triFrame  the frame to set the position labels of
     * @param xS        the x coordinate within the tri-image component
     * @param yS        the y coordinate within the tri-image component
     */
    public Point3D updateFrameLabels(ViewJFrameBase triFrame, int xS, int yS)
    {
        Point3D point = getVolumePosition(xS, yS, slice);
        ( (ViewJFrameTriImage) triFrame).setPositionLabels(point.x, point.y, point.z);

        return point;
    }

    /**
     * Gets the label for a point VOI (reordered to show the proper two dimensions which are being shown in the tri-image component).
     * @param x  x value of the volume space coordinate
     * @param y  y value of the volume space coordinate
     * @param z  z value of the volume space coordinate
     * @return   the label that the point should have
     */
    public Point getVOILabel(int x, int y, int z)
    {
        Point label = new Point( -1, -1);

        switch (axisOrder[0])
        {
            case 0:
                label.x = x;
                break;
            case 1:
                label.x = y;
                break;
            case 2:
                label.x = z;
                break;
        }
        switch (axisOrder[1])
        {
            case 0:
                label.y = x;
                break;
            case 1:
                label.y = y;
                break;
            case 2:
                label.y = z;
                break;
        }

        return label;
    }

    /**
     * Get the factors needed to iterate through the image volume using 0-based indexes.
     * Can be multiplied against iterators to retreive the index into the image volume data.
     * @return  the steps needed to iterate along the dimensions of the backing image volume data
     */
    protected int[] getVolumeIterationFactors()
    {
        int[] iterationFactors = new int[3];

        if (axisOrder[0] == 0)
        {
            iterationFactors[0] = 1;
        }
        else if (axisOrder[0] == 1)
        {
            iterationFactors[0] = imageActive.getExtents()[0];
        }
        else
        { // axisOrder[0] == 2
            iterationFactors[0] = imageActive.getSliceSize();
        }

        if (axisOrder[1] == 0)
        {
            iterationFactors[1] = 1;
        }
        else if (axisOrder[1] == 1)
        {
            iterationFactors[1] = imageActive.getExtents()[0];
        }
        else
        { // axisOrder[1] == 2
            iterationFactors[1] = imageActive.getSliceSize();
        }

        if (axisOrder[2] == 0)
        {
            iterationFactors[2] = 1;
        }
        else if (axisOrder[2] == 1)
        {
            iterationFactors[2] = imageActive.getExtents()[0];
        }
        else
        { // axisOrder[2] == 2
            iterationFactors[2] = imageActive.getSliceSize();
        }

        return iterationFactors;
    }

    /**
     * Get the points which define the bounding cube.
     * @return CubeBounds  the bounding cube's bounds (in image volume space)
     */
    public CubeBounds getBoundedVolume()
    {
        // no translation needed since the low and high vars are already in volume space (always x,y,z)

        Point3D [] boundingBoxPoints = triImageFrame.getBoundingBoxPoints();

        return new CubeBounds(boundingBoxPoints[ViewJFrameTriImage.UPPER_RIGHT_FRONT].x,
                              boundingBoxPoints[ViewJFrameTriImage.UPPER_LEFT_FRONT].x,
                              boundingBoxPoints[ViewJFrameTriImage.LOWER_RIGHT_FRONT].y,
                              boundingBoxPoints[ViewJFrameTriImage.UPPER_RIGHT_FRONT].y,
                              boundingBoxPoints[ViewJFrameTriImage.UPPER_RIGHT_BACK].z,
                              boundingBoxPoints[ViewJFrameTriImage.UPPER_RIGHT_FRONT].z);
    }

    /**
     * Returns whether the bounding rectangle should be drawn.
     * @return  whether the bounding rectangle should be drawn
     */
    public boolean isShowBoundingRect()
    {
        return showBoundingRect;
    }

    /**
     * Sets whether the bounding rectangle should be drawn.
     * @param flag  whether the bounding rectangle should be drawn
     */
    public void setShowBoundingRect(boolean flag)
    {
        showBoundingRect = flag;
    }

    /**
     * Mouse wheel movement handler, used to change the displayed slice in the component.
     * @param event  the mouse wheel rotation event
     */
    public void mouseWheelMoved(MouseWheelEvent event)
    {
        slice += ( -event.getWheelRotation());

        if (slice < 0)
        {
            slice = 0;
        }
        if (slice >= imageActive.getExtents()[axisOrder[2]])
        {
            slice = imageActive.getExtents()[axisOrder[2]] - 1;
        }

        Point3D newLabel = getTriImagePosition(screenPt.x, screenPt.y);
        triImageFrame.setPositionLabels(newLabel.x, newLabel.y, newLabel.z);

        if (orientation == AXIAL)
        {
            triImageFrame.setAxialComponentSlice(slice);
        }
        else if (orientation == SAGITTAL)
        {
            triImageFrame.setSagittalComponentSlice(slice);
        }
        else
        {
            triImageFrame.setCoronalComponentSlice(slice);
        }

        if (resolutionX == resolutionY)
        {
            triImageFrame.setCrosshairs(screenPt.x, screenPt.y,
                                        (int) (slice * (res[2] / res[1]) * getZoomY()),
                                        triComponentOrientation);
        }
        else
        {
            triImageFrame.setCrosshairs(screenPt.x, screenPt.y,
                                        (int) (slice * getZoomY()),
                                        triComponentOrientation);
        }

        triImageFrame.updateImages();
    }

    /**
     * Updates the image volume's paint bitmap based on the current position of the mouse in the component,
     * called after a mouse action indicates the user wants part of the image either painted or erased.
     * @param paintPixels  if true, the pixels under the paint brush should be painted. if false, they are cleared of any paint
     * @param x            the x position of the mouse, adjusted for zoom and resolutions
     * @param y            the y position of the mouse, adjusted for zoom and resolutions
     */
    protected void updatePaintBitmap(boolean paintPixels, int x, int y)
    {
        int brushSize = getBrushSize();
        int hBrushSize = getHBrushSize();

        int jMin = Math.max(y - hBrushSize, 0);
        int jMax = Math.min(y - hBrushSize + brushSize - 1, imageDim.height - 1);
        int iMin = Math.max(x - hBrushSize, 0);
        int iMax = Math.min(x - hBrushSize + brushSize - 1, imageDim.width - 1);

        int i, j, ri, rj, index;

        int volSlice = getCurrentTriImageSlice();

        int[] iterFactors = getVolumeIterationFactors();
        if (triComponentOrientation == AXIAL || !hasOrientation)
        {
            for (j = jMin; j <= jMax; j++)
            {
                if (axisFlip[1])
                {
                    rj = imageActive.getExtents()[axisOrder[1]] - 1 - j;
                }
                else
                {
                    rj = j;
                }
                for (i = iMin; i <= iMax; i++)
                {
                    if (axisFlip[0])
                    {
                        ri = imageActive.getExtents()[axisOrder[0]] - 1 - i;
                    }
                    else
                    {
                        ri = i;
                    }
                    index = iterFactors[0] * ri + iterFactors[1] * rj + iterFactors[2] * volSlice;
                    if (paintPixels)
                    {
                        paintBitmap.set(index);
                    }
                    else
                    {
                        paintBitmap.clear(index);
                    }
                }
            }
        } // end of if (triComponentOrientation == XY)
        else if (triComponentOrientation == CORONAL)
        {
            for (j = jMin; j <= jMax; j++)
            {
                if (axisFlip[1])
                {
                    rj = imageActive.getExtents()[axisOrder[1]] - 1 - j;
                }
                else
                {
                    rj = j;
                }
                for (i = iMin; i <= iMax; i++)
                {
                    if (axisFlip[0])
                    {
                        ri = imageActive.getExtents()[axisOrder[0]] - 1 - i;
                    }
                    else
                    {
                        ri = i;
                    }
                    index = iterFactors[0] * ri + iterFactors[1] * rj + iterFactors[2] * volSlice;
                    if (paintPixels)
                    {
                        paintBitmap.set(index);
                    }
                    else
                    {
                        paintBitmap.clear(index);
                    }
                }
            }
        } // end of else if (triComponentOrientation == XZ)
        else
        { // triComponentOrientation == ZY
            for (j = jMin; j <= jMax; j++)
            {
                if (axisFlip[1])
                {
                    rj = imageActive.getExtents()[axisOrder[1]] - 1 - j;
                }
                else
                {
                    rj = j;
                }
                for (i = iMin; i <= iMax; i++)
                {
                    if (axisFlip[0])
                    {
                        ri = imageActive.getExtents()[axisOrder[0]] - 1 - i;
                    }
                    else
                    {
                        ri = i;
                    }
                    index = iterFactors[0] * ri + iterFactors[1] * rj + iterFactors[2] * volSlice;
                    if (paintPixels)
                    {
                        paintBitmap.set(index);
                    }
                    else
                    {
                        paintBitmap.clear(index);
                    }
                }
            }
        } // end of else for triComponentOrientation == ZY

        if (imageActive.getType() == ModelStorageBase.COMPLEX)
        {
            int yFactor, zFactor;
            int temp, sliceC, offset, st;

            temp = iMin;
            iMin = imageDim.width - Math.max(iMax, 1);
            iMax = imageDim.width - Math.max(temp, 1);
            temp = jMin;
            jMin = imageDim.height - Math.max(jMax, 1);
            jMax = imageDim.height - Math.max(temp, 1);
            sliceC = imageActive.getExtents()[2] - Math.max(slice, 1);
            if (triComponentOrientation == AXIAL || !hasOrientation)
            {
                offset = imageDim.width * imageDim.height * sliceC;
                for (j = jMin; j <= jMax; j++)
                {
                    for (i = iMin; i <= iMax; i++)
                    {
                        st = j * imageDim.width + i;
                        if (paintPixels)
                        {
                            paintBitmap.set(offset + st);
                        }
                        else
                        {
                            paintBitmap.clear(offset + st);
                        }
                    }
                }
            } // end of if (triComponentOrientation == XY)
            else if (triComponentOrientation == CORONAL)
            {
                offset = imageActive.getExtents()[0] * sliceC;
                zFactor = imageActive.getSliceSize();
                for (j = jMin; j <= jMax; j++)
                {
                    for (i = iMin; i <= iMax; i++)
                    {
                        if (hasOrientation)
                        {
                            index = zFactor * (imageActive.getExtents()[2] - 1 - j) + offset + i;
                        }
                        else
                        {
                            index = zFactor * j + offset + i;
                        }
                        if (paintPixels)
                        {
                            paintBitmap.set(index);
                        }
                        else
                        {
                            paintBitmap.clear(index);
                        }
                    }
                }
            } // end of else if (triComponentOrientation == XZ)
            else
            { // triComponentOrientation == ZY
                zFactor = imageActive.getSliceSize();
                yFactor = imageActive.getExtents()[0];
                for (j = jMin; j <= jMax; j++)
                {
                    for (i = iMin; i <= iMax; i++)
                    {
                        if (hasOrientation)
                        {
                            index = zFactor * (imageActive.getExtents()[2] - 1 - j) + yFactor * i + sliceC;
                        }
                        else
                        {
                            index = zFactor * i + yFactor * j + sliceC;
                        }
                        if (paintPixels)
                        {
                            paintBitmap.set(index);
                        }
                        else
                        {
                            paintBitmap.clear(index);
                        }
                    }
                }
            } // end of else for triComponentOrientation == ZY
        } // if (imageActive.getType() == ModelStorageBase.COMPLEX)

        triImageFrame.updatePaint(paintBitmap);
    }

    /**
     * Gets the current out-of-component slice the component is displaying in volume space (ie, post reversal, if needed).
     * @return  the currently displayed slice in this component
     */
    protected final int getCurrentTriImageSlice()
    {
        int volumeSlice;
        if (axisFlip[2])
        {
            volumeSlice = imageActive.getExtents()[axisOrder[2]] - 1 - slice;
        }
        else
        {
            volumeSlice = slice;
        }

        return volumeSlice;
    }

    /**
     * Returns the extents of the tri planar component (in the component's order, not the image volume's).
     * @return the extents of the tri image component
     */
    protected final int[] getTriImageExtents()
    {
        int[] extents = new int[3];

        extents[0] = imageActive.getExtents()[axisOrder[0]];
        extents[1] = imageActive.getExtents()[axisOrder[1]];
        extents[2] = imageActive.getExtents()[axisOrder[2]];

        return extents;
    }

    /**
     *  Shows the image and the VOI(s) - used in ViewJFrameTriImage to get the proper
     *  triComponentOrientation of the image displaye (i.e. DICOM)
     *  @param tSlice     t (time) slice to show
     *  @param zSlice     z slice to show
     *  @param _LUTa      LUTa - to change to new LUT for imageA else null
     *  @param _LUTb      LUTb - to change to new LUT for imageB else null
     *  @param forceShow  forces this method to import image and recalculate java image
     *  @param _interpMode image interpolation method (Nearest or Smooth)
     *  @return           boolean to indicate if the show was successful
     */
    public boolean showUsingOrientation(int tSlice, int zSlice, ModelLUT _LUTa, ModelLUT _LUTb,
                                        boolean forceShow, int _interpMode)
    {
        int lutHeightA = 0;
        int index;
        float RGB_LUTa[][] = null, RGB_LUTb[][] = null;
        int iRGB_LUTa[][] = null, iRGB_LUTb[][] = null;
        int Ra, Ga, Ba;
        int indexA, indexB;
        int pix;
        int i, j;

        try
        {
            time = System.currentTimeMillis();

            if (_interpMode > -1)
            {
                setInterpolationMode(_interpMode);
            }

            if (imageA.isColorImage() == true)
            {
                // call the show method for displaying RGB images
                return (showUsingOrientation(tSlice, zSlice, forceShow));
            }

            if (imageA == null)
            {
                return false;
            }
            if (LUTa == null && _LUTb == null)
            {
                return false;
            }
            if (_LUTa != null)
            {
                LUTa = _LUTa;
            }
            if (imageB != null && _LUTb != null)
            {
                LUTb = _LUTb;
            }

            lutHeightA = LUTa.getExtents()[1];

            if (lutHeightA != lutBufferRemapped.length)
            {
                lutBufferRemapped = new int[lutHeightA];
            }

            if (imageB != null)
            {
                RGB_LUTa = LUTa.exportRGB_LUT(true);
                RGB_LUTb = LUTb.exportRGB_LUT(true);
                iRGB_LUTa = new int[3][RGB_LUTa[0].length];
                iRGB_LUTb = new int[3][RGB_LUTb[0].length];

                for (int c = 0; c < RGB_LUTa[0].length; c++)
                {
                    iRGB_LUTa[0][c] = (int) (RGB_LUTa[0][c] + 0.5f);
                    iRGB_LUTb[0][c] = (int) (RGB_LUTb[0][c] + 0.5f);
                    iRGB_LUTa[1][c] = (int) (RGB_LUTa[1][c] + 0.5f);
                    iRGB_LUTb[1][c] = (int) (RGB_LUTb[1][c] + 0.5f);
                    iRGB_LUTa[2][c] = (int) (RGB_LUTa[2][c] + 0.5f);
                    iRGB_LUTb[2][c] = (int) (RGB_LUTb[2][c] + 0.5f);
                }
            }
            else
            {
                LUTa.exportIndexedLUT(lutBufferRemapped);
            }

            if ( (triComponentOrientation == AXIAL))
            {
                if (slice != zSlice || timeSlice != tSlice || forceShow == true)
                {
                    slice = zSlice;
                    timeSlice = tSlice;
                    if (imageA.getNDims() < 4)
                    {
                        timeSliceA = 0;
                    }
                    else
                    {
                        timeSliceA = timeSlice;
                    }
                    if ( (imageB != null) && (imageB.getNDims() < 4))
                    {
                        timeSliceB = 0;
                    }
                    else
                    {
                        timeSliceB = timeSlice;
                    }

                    fillImageBuffer(showAxis, zSlice);

                } // end of if ( slice != zSlice || timeSlice != tSlice || forceShow == true)

                if (imageB == null)
                {
                    pix = 0;
                    TransferFunction tf_imgA = LUTa.getTransferFunction();

                    for (j = 0; j < imageExtents[axisOrder[1]]; j++)
                    {
                        for (i = 0; i < imageExtents[axisOrder[0]]; i++)
                        {
                            index = j * imageExtents[axisOrder[0]] + i;
                            pix = (int) (tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5f);
                            try
                            {
                                cleanImageBufferA[index] = lutBufferRemapped[pix];
                            }
                            catch (ArrayIndexOutOfBoundsException e)
                            {
                                Preferences.debug("error = " + e + "\n");
                                Preferences.debug("index = " + index + " pix = " + pix + "\n");
                            }
                        } // for (i = 0; i < imageExtents[xy0]; i++)
                    } // for (j = 0; j < imageExtents[xy1]; j++)
                } // if (imageB == null)
                else if (imageB != null)
                { // imageB != null
                    indexA = indexB = 0;
                    TransferFunction tf_imgA = LUTa.getTransferFunction();
                    TransferFunction tf_imgB = LUTb.getTransferFunction();

                    for (j = 0; j < imageExtents[axisOrder[1]]; j++)
                    {
                        for (i = 0; i < imageExtents[axisOrder[0]]; i++)
                        {
                            index = j * imageExtents[axisOrder[0]] + i;

                            indexA = (int) (tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5f);
                            indexB = (int) (tf_imgB.getRemappedValue(imageBufferB[index], 256) + 0.5f);

                            Ra = iRGB_LUTa[0][indexA];
                            Ga = iRGB_LUTa[1][indexA];
                            Ba = iRGB_LUTa[2][indexA];

                            pix = 0xff000000 | (Ra << 16) | (Ga << 8) | Ba;
                            cleanImageBufferA[index] = pix;
                            cleanImageBufferB[index] = (0xff000000) | (int) (RGB_LUTb[0][indexB]) << 16
                                | (int) (RGB_LUTb[1][indexB]) << 8 | (int) (RGB_LUTb[2][indexB]);

                        } // for (i = 0; i < imageExtents[xy0]; i++)
                    } // for (j = 0; j < imageExtents[xy1]; j++)
                } // else for imageB != null
            } // if ((triComponentOrientation == XY) || (!hasOrientation))
            else if (triComponentOrientation == CORONAL)
            {
                if (slice != zSlice || timeSlice != tSlice || forceShow == true)
                {
                    slice = zSlice;
                    timeSlice = tSlice;
                    if (imageA.getNDims() < 4)
                    {
                        timeSliceA = 0;
                    }
                    else
                    {
                        timeSliceA = timeSlice;
                    }
                    if ( (imageB != null) && (imageB.getNDims() < 4))
                    {
                        timeSliceB = 0;
                    }
                    else
                    {
                        timeSliceB = timeSlice;
                    }
                    fillImageBuffer(showAxis, zSlice);

                } // end of if ( slice != zSlice || timeSlice != tSlice || forceShow == true)
                if (imageB == null)
                {
                    pix = 0;
                    TransferFunction tf_imgA = LUTa.getTransferFunction();

                    for (j = 0; j < imageExtents[axisOrder[1]]; j++)
                    {
                        for (i = 0; i < imageExtents[axisOrder[0]]; i++)
                        {
                            index = j * imageExtents[axisOrder[0]] + i;

                            pix = (int) (tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5f);

                            pixBuffer[index] = lutBufferRemapped[pix];
                        } // end of for (i = 0; i < imageExtents[xz0]; i++)
                    } // end of for (j = 0; j < imageExtents[xz1]; j++)
                } // end of if (imageB == null)
                else if (imageB != null)
                { // imageB != null
                    indexA = indexB = 0;
                    TransferFunction tf_imgA = LUTa.getTransferFunction();
                    TransferFunction tf_imgB = LUTb.getTransferFunction();

                    for (j = 0; j < imageExtents[axisOrder[1]]; j++)
                    {
                        for (i = 0; i < imageExtents[axisOrder[0]]; i++)
                        {
                            index = j * imageExtents[axisOrder[0]] + i;

                            indexA = (int) (tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5f);
                            indexB = (int) (tf_imgB.getRemappedValue(imageBufferB[index], 256) + 0.5f);

                            Ra = iRGB_LUTa[0][indexA];
                            Ga = iRGB_LUTa[1][indexA];
                            Ba = iRGB_LUTa[2][indexA];

                            pix = 0xff000000 | (Ra << 16) | (Ga << 8) | Ba;
                            cleanImageBufferA[index] = pix;
                            cleanImageBufferB[index] = (0xff000000) | (int) (RGB_LUTb[0][indexB]) << 16
                                | (int) (RGB_LUTb[1][indexB]) << 8 | (int) (RGB_LUTb[2][indexB]);
                        } // end of for (i = 0; i < imageExtents[xz0]; i++)
                    } // end of for (j = 0; j < imageExtents[xz1]; j++)
                } // end of else for imageB != null
            } // end of else if (triComponentOrientation == XZ)
            else
            { // triComponentOrientation == ZY
                if (slice != zSlice || timeSlice != tSlice || forceShow == true)
                {
                    slice = zSlice;
                    timeSlice = tSlice;
                    if (imageA.getNDims() < 4)
                    {
                        timeSliceA = 0;
                    }
                    else
                    {
                        timeSliceA = timeSlice;
                    }
                    if ( (imageB != null) && (imageB.getNDims() < 4))
                    {
                        timeSliceB = 0;
                    }
                    else
                    {
                        timeSliceB = timeSlice;
                    }

                    fillImageBuffer(showAxis, zSlice);

                } // end of if ( slice != zSlice || timeSlice != tSlice || forceShow == true)
                if (imageB == null)
                {
                    pix = 0;
                    TransferFunction tf_imgA = LUTa.getTransferFunction();

                    for (j = 0; j < imageExtents[axisOrder[1]]; j++)
                    {
                        for (i = 0; i < imageExtents[axisOrder[0]]; i++)
                        {
                            index = j * imageExtents[axisOrder[0]] + i;
                            pix = (int) (tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5f);
                            cleanImageBufferA[index] = lutBufferRemapped[pix];
                        } // end of for (i = 0; i < imageExtents[yz0]; i++)
                    } // end of for (j = 0; j < imageExtents[yz1]; j++)
                } // end of if (imageB == null)
                else if (imageB != null)
                { // imageB != null
                    indexA = indexB = 0;
                    TransferFunction tf_imgA = LUTa.getTransferFunction();
                    TransferFunction tf_imgB = LUTb.getTransferFunction();

                    for (j = 0; j < imageExtents[axisOrder[1]]; j++)
                    {
                        for (i = 0; i < imageExtents[axisOrder[0]]; i++)
                        {
                            index = j * imageExtents[axisOrder[0]] + i;

                            indexA = (int) (tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5f);
                            indexB = (int) (tf_imgB.getRemappedValue(imageBufferB[index], 256) + 0.5f);

                            Ra = iRGB_LUTa[0][indexA];
                            Ga = iRGB_LUTa[1][indexA];
                            Ba = iRGB_LUTa[2][indexA];

                            pix = 0xff000000 | (Ra << 16) | (Ga << 8) | Ba;
                            cleanImageBufferA[index] = pix;
                            cleanImageBufferB[index] = (0xff000000) | (int) (RGB_LUTb[0][indexB]) << 16
                                | (int) (RGB_LUTb[1][indexB]) << 8 | (int) (RGB_LUTb[2][indexB]);
                        } // end of for (i = 0; i < imageExtents[yz0]; i++)
                    } // end of for (j = 0; j < imageExtents[yz1]; j++)
                } // end of else for imageB != null
            } // end of else for triComponentOrientation == ZY

            setSliceString(String.valueOf(slice + 1));

            repaint();
            time = System.currentTimeMillis() - time;
        }
        catch (OutOfMemoryError oome)
        {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.show");
            return false;
        }

        return true;
    } // show(int tSlice, int zSlice, ModelLUT _LUTa, ModelLUT _LUTb, boolean forceShow,

    /**
     *  For generating the display of 1 or 2 RGB images
     *  @param tSlice     t (time) slice to show
     *  @param zSlice     z slice to show
     *  @param forceShow  forces this method to import image and recalculate java image
     *  @return           boolean to indicate if the show was successful
     */
    public boolean showUsingOrientation(int tSlice, int zSlice, boolean forceShow)
    {
        // Note that alphaBlending is applied with 1 component taken as zero if both components
        // are not present -for example, if either imageA or imageB but not both has red, then
        // the red component is alphaBlended with zero.

        int i, j;
        int ind4, index;
        int Ra, Ga, Ba, Rb, Gb, Bb;
        int pixValue;
        float redMapped, greenMapped, blueMapped;
        int[] RGBIndexBufferA = null;
        int[] RGBIndexBufferB = null;

        if (RGBTA != null)
        {
            RGBIndexBufferA = RGBTA.exportIndexedRGB();
        }

        if ( (imageB != null) && (RGBTB != null))
        {
            RGBIndexBufferB = RGBTB.exportIndexedRGB();
        }

        if (triComponentOrientation == AXIAL)
        {
            if (slice != zSlice || timeSlice != tSlice || forceShow == true)
            {
                slice = zSlice;
                timeSlice = tSlice;
                if (imageA.getNDims() < 4)
                {
                    timeSliceA = 0;
                }
                else
                {
                    timeSliceA = timeSlice;
                }
                if ( (imageB != null) && (imageB.getNDims() < 4))
                {
                    timeSliceB = 0;
                }
                else
                {
                    timeSliceB = timeSlice;
                }

                fillImageColorBuffer(showAxis, zSlice);
            } // end of if ( slice != zSlice || timeSlice != tSlice || forceShow == true)

            if (imageB == null)
            {
                for (j = 0; j < imageExtents[axisOrder[1]]; j++)
                {
                    for (i = 0; i < imageExtents[axisOrder[0]]; i++)
                    {
                        ind4 = j * imageExtents[axisOrder[0]] + i;
                        index = 4 * ind4;

                        if (RGBTA != null)
                        {
                            if (RGBTA.getROn())
                            {
                                redMapped = (RGBIndexBufferA[ (int) imageBufferA[index + 1]] & 0x00ff0000) >> 16;
                            }
                            else
                            {
                                redMapped = 0;
                            }
                            if (RGBTA.getGOn())
                            {
                                greenMapped = (RGBIndexBufferA[ (int) imageBufferA[index + 2]] & 0x0000ff00) >> 8;
                            }
                            else
                            {
                                greenMapped = 0;
                            }
                            if (RGBTA.getBOn())
                            {
                                blueMapped = (RGBIndexBufferA[ (int) imageBufferA[index + 3]] & 0x000000ff);
                            }
                            else
                            {
                                blueMapped = 0;
                            }
                        } // if (RGBTA != null)
                        else
                        {
                            redMapped = imageBufferA[index + 1];
                            greenMapped = imageBufferA[index + 2];
                            blueMapped = imageBufferA[index + 3];
                        }

                        pixValue = 0xff000000
                            | ( ( (int) (redMapped) << 16)
                               | ( ( (int) (greenMapped) << 8) | ( (int) (blueMapped))));
                        cleanImageBufferA[ind4] = pixValue;
                    } // for (i = 0; i < imageExtents[xy0]; i++)
                } // for (j = 0; j < imageExtents[xy1]; j++)
            } // if (imageB == null )
            else
            { // imageB != null
                for (j = 0; j < imageExtents[axisOrder[1]]; j++)
                {
                    for (i = 0; i < imageExtents[axisOrder[0]]; i++)
                    {
                        ind4 = j * imageExtents[axisOrder[0]] + i;
                        index = 4 * ind4;

                        if ( (RGBTA != null) && (RGBTB != null))
                        {
                            if (RGBTA.getROn())
                            {
                                Ra = (RGBIndexBufferA[ (int) imageBufferA[index + 1]] & 0x00ff0000) >> 16;
                            }
                            else
                            {
                                Ra = 0;
                            }
                            if (RGBTA.getGOn())
                            {
                                Rb = (RGBIndexBufferB[ (int) imageBufferB[index + 1]] & 0x00ff0000) >> 16;
                            }
                            else
                            {
                                Rb = 0;
                            }
                            if (RGBTA.getBOn())
                            {
                                Ga = (RGBIndexBufferA[ (int) imageBufferA[index + 2]] & 0x0000ff00) >> 8;
                            }
                            else
                            {
                                Ga = 0;
                            }
                            if (RGBTB.getROn())
                            {
                                Gb = (RGBIndexBufferB[ (int) imageBufferB[index + 2]] & 0x0000ff00) >> 8;
                            }
                            else
                            {
                                Gb = 0;
                            }
                            if (RGBTB.getGOn())
                            {
                                Ba = (RGBIndexBufferA[ (int) imageBufferA[index + 3]] & 0x000000ff);
                            }
                            else
                            {
                                Ba = 0;
                            }
                            if (RGBTB.getBOn())
                            {
                                Bb = (RGBIndexBufferB[ (int) imageBufferB[index + 3]] & 0x000000ff);
                            }
                            else
                            {
                                Bb = 0;
                            }
                        } // if ((RGBTA != null) && (RGBTB != null))
                        else
                        {
                            Ra = (int) imageBufferA[index + 1];
                            Rb = (int) imageBufferB[index + 1];
                            Ga = (int) imageBufferA[index + 2];
                            Gb = (int) imageBufferB[index + 2];
                            Ba = (int) imageBufferA[index + 3];
                            Bb = (int) imageBufferB[index + 3];
                        }

                        pixValue = 0xff000000 | (Ra << 16) | (Ga << 8) | Ba;
                        cleanImageBufferA[ind4] = pixValue;
                        cleanImageBufferB[ind4] = 0xff000000 | (Rb << 16) | (Gb << 8) | Bb;
                    } // for (i = 0; i < imageExtents[xy0]; i++)
                } // for (j = 0; j < imageExtents[xy1]; j++)
            } // else for imageB != null
        } // if ((triComponentOrientation == XY) || (!hasOrientation))
        else if (triComponentOrientation == CORONAL)
        {
            if (slice != zSlice || timeSlice != tSlice || forceShow == true)
            {
                slice = zSlice;
                timeSlice = tSlice;
                if (imageA.getNDims() < 4)
                {
                    timeSliceA = 0;
                }
                else
                {
                    timeSliceA = timeSlice;
                }
                if ( (imageB != null) && (imageB.getNDims() < 4))
                {
                    timeSliceB = 0;
                }
                else
                {
                    timeSliceB = timeSlice;
                }

                fillImageColorBuffer(showAxis, zSlice);
            } // end of if ( slice != zSlice || timeSlice != tSlice || forceShow == true)

            if (imageB == null)
            {
                for (j = 0; j < imageExtents[axisOrder[1]]; j++)
                {
                    for (i = 0; i < imageExtents[axisOrder[0]]; i++)
                    {
                        ind4 = j * imageExtents[axisOrder[0]] + i;
                        index = 4 * ind4;

                        if (RGBTA != null)
                        {
                            if (RGBTA.getROn())
                            {
                                redMapped = (RGBIndexBufferA[ (int) imageBufferA[index + 1]] & 0x00ff0000) >> 16;
                            }
                            else
                            {
                                redMapped = 0;
                            }
                            if (RGBTA.getGOn())
                            {
                                greenMapped = (RGBIndexBufferA[ (int) imageBufferA[index + 2]] & 0x0000ff00) >> 8;
                            }
                            else
                            {
                                greenMapped = 0;
                            }
                            if (RGBTA.getBOn())
                            {
                                blueMapped = (RGBIndexBufferA[ (int) imageBufferA[index + 3]] & 0x000000ff);
                            }
                            else
                            {
                                blueMapped = 0;
                            }
                        } // end of if (RGBTA != null)
                        else
                        {
                            redMapped = imageBufferA[index + 1];
                            greenMapped = imageBufferA[index + 2];
                            blueMapped = imageBufferA[index + 3];
                        }

                        pixValue = 0xff000000
                            | ( ( (int) (redMapped) << 16)
                               | ( ( (int) (greenMapped) << 8) | ( (int) (blueMapped))));
                        cleanImageBufferA[ind4] = pixValue;

                    } // for (i = 0; i < imageExtents[xz0]; i++)
                } // for (j = 0; j < imageExtents[xz1]; j++)
            } // if (imageB == null )
            else
            { // imageB != null
                for (j = 0; j < imageExtents[axisOrder[1]]; j++)
                {
                    for (i = 0; i < imageExtents[axisOrder[0]]; i++)
                    {
                        ind4 = j * imageExtents[axisOrder[0]] + i;
                        index = 4 * ind4;

                        if ( (RGBTA != null) && (RGBTB != null))
                        {
                            if (RGBTA.getROn())
                            {
                                Ra = (RGBIndexBufferA[ (int) imageBufferA[index + 1]] & 0x00ff0000) >> 16;
                            }
                            else
                            {
                                Ra = 0;
                            }
                            if (RGBTA.getGOn())
                            {
                                Rb = (RGBIndexBufferB[ (int) imageBufferB[index + 1]] & 0x00ff0000) >> 16;
                            }
                            else
                            {
                                Rb = 0;
                            }
                            if (RGBTA.getBOn())
                            {
                                Ga = (RGBIndexBufferA[ (int) imageBufferA[index + 2]] & 0x0000ff00) >> 8;
                            }
                            else
                            {
                                Ga = 0;
                            }
                            if (RGBTB.getROn())
                            {
                                Gb = (RGBIndexBufferB[ (int) imageBufferB[index + 2]] & 0x0000ff00) >> 8;
                            }
                            else
                            {
                                Gb = 0;
                            }
                            if (RGBTB.getGOn())
                            {
                                Ba = (RGBIndexBufferA[ (int) imageBufferA[index + 3]] & 0x000000ff);
                            }
                            else
                            {
                                Ba = 0;
                            }
                            if (RGBTB.getBOn())
                            {
                                Bb = (RGBIndexBufferB[ (int) imageBufferB[index + 3]] & 0x000000ff);
                            }
                            else
                            {
                                Bb = 0;
                            }
                        } // end of if ((RGBTA != null) && (RGBTB != null))
                        else
                        {
                            Ra = (int) imageBufferA[index + 1];
                            Rb = (int) imageBufferB[index + 1];
                            Ga = (int) imageBufferA[index + 2];
                            Gb = (int) imageBufferB[index + 2];
                            Ba = (int) imageBufferA[index + 3];
                            Bb = (int) imageBufferB[index + 3];
                        }

                        pixValue = 0xff000000 | (Ra << 16) | (Ga << 8) | Ba;
                        cleanImageBufferA[ind4] = pixValue;
                        cleanImageBufferB[ind4] = 0xff000000 | (Rb << 16) | (Gb << 8) | Bb;
                    } // for (i = 0; i < imageExtents[xz0]; i++)
                } // for (j = 0; j < imageExtents[xz1]; j++)
            } // else for imageB != null
        } // else if (triComponentOrientation == XZ)
        else
        { // for triComponentOrientation == ZY
            if (slice != zSlice || timeSlice != tSlice || forceShow == true)
            {
                slice = zSlice;
                timeSlice = tSlice;
                if (imageA.getNDims() < 4)
                {
                    timeSliceA = 0;
                }
                else
                {
                    timeSliceA = timeSlice;
                }
                if ( (imageB != null) && (imageB.getNDims() < 4))
                {
                    timeSliceB = 0;
                }
                else
                {
                    timeSliceB = timeSlice;
                }
                fillImageColorBuffer(showAxis, zSlice);
            } // end of if ( slice != zSlice || timeSlice !=   || forceShow == true)

            if (imageB == null)
            {
                for (j = 0; j < imageExtents[axisOrder[1]]; j++)
                {
                    for (i = 0; i < imageExtents[axisOrder[0]]; i++)
                    {
                        ind4 = j * imageExtents[axisOrder[0]] + i;
                        index = 4 * ind4;

                        if (RGBTA != null)
                        {
                            if (RGBTA.getROn())
                            {
                                redMapped = (RGBIndexBufferA[ (int) imageBufferA[index + 1]] & 0x00ff0000) >> 16;
                            }
                            else
                            {
                                redMapped = 0;
                            }
                            if (RGBTA.getGOn())
                            {
                                greenMapped = (RGBIndexBufferA[ (int) imageBufferA[index + 2]] & 0x0000ff00) >> 8;
                            }
                            else
                            {
                                greenMapped = 0;
                            }
                            if (RGBTA.getBOn())
                            {
                                blueMapped = (RGBIndexBufferA[ (int) imageBufferA[index + 3]] & 0x000000ff);
                            }
                            else
                            {
                                blueMapped = 0;
                            }
                        } // end of if (RGBTA != null)
                        else
                        {
                            redMapped = imageBufferA[index + 1];
                            greenMapped = imageBufferA[index + 2];
                            blueMapped = imageBufferA[index + 3];
                        }

                        pixValue = 0xff000000
                            | ( ( (int) (redMapped) << 16)
                               | ( ( (int) (greenMapped) << 8) | ( (int) (blueMapped))));
                        cleanImageBufferA[ind4] = pixValue;
                    } // for (i = 0; i < imageExtents[yz0]; i++)
                } // for (j = 0; j < imageExtents[yz1]; j++)
            } // if (imageB == null )
            else
            { // imageB != null
                for (j = 0; j < imageExtents[axisOrder[1]]; j++)
                {
                    for (i = 0; i < imageExtents[axisOrder[0]]; i++)
                    {
                        ind4 = j * imageExtents[axisOrder[0]] + i;
                        index = 4 * ind4;

                        if ( (RGBTA != null) && (RGBTB != null))
                        {
                            if (RGBTA.getROn())
                            {
                                Ra = (RGBIndexBufferA[ (int) imageBufferA[index + 1]] & 0x00ff0000) >> 16;
                            }
                            else
                            {
                                Ra = 0;
                            }
                            if (RGBTA.getGOn())
                            {
                                Rb = (RGBIndexBufferB[ (int) imageBufferB[index + 1]] & 0x00ff0000) >> 16;
                            }
                            else
                            {
                                Rb = 0;
                            }
                            if (RGBTA.getBOn())
                            {
                                Ga = (RGBIndexBufferA[ (int) imageBufferA[index + 2]] & 0x0000ff00) >> 8;
                            }
                            else
                            {
                                Ga = 0;
                            }
                            if (RGBTA.getROn())
                            {
                                Gb = (RGBIndexBufferB[ (int) imageBufferB[index + 2]] & 0x0000ff00) >> 8;
                            }
                            else
                            {
                                Gb = 0;
                            }
                            if (RGBTA.getGOn())
                            {
                                Ba = (RGBIndexBufferA[ (int) imageBufferA[index + 3]] & 0x000000ff);
                            }
                            else
                            {
                                Ba = 0;
                            }
                            if (RGBTA.getBOn())
                            {
                                Bb = (RGBIndexBufferB[ (int) imageBufferB[index + 3]] & 0x000000ff);
                            }
                            else
                            {
                                Bb = 0;
                            }
                        } // if ((RGBTA != null) && (RGBTB != null))
                        else
                        {
                            Ra = (int) imageBufferA[index + 1];
                            Rb = (int) imageBufferB[index + 1];
                            Ga = (int) imageBufferA[index + 2];
                            Gb = (int) imageBufferB[index + 2];
                            Ba = (int) imageBufferA[index + 3];
                            Bb = (int) imageBufferB[index + 3];
                        }

                        pixValue = 0xff000000 | (Ra << 16) | (Ga << 8) | Ba;
                        cleanImageBufferA[ind4] = pixValue;
                        cleanImageBufferB[ind4] = 0xff000000 | (Rb << 16) | (Gb << 8) | Bb;
                    } // for (i = 0; i < imageExtents[yz0]; i++)
                } // for (j = 0; j < imageExtents[yz1]; j++)
            } // else for imageB != null
        } // else for triComponentOrientation == ZY

        time = System.currentTimeMillis() - time;

        setSliceString(String.valueOf(slice + 1));

        repaint();
        return true;
    }

    private void fillImageBuffer(int show, int slice)
    {
        int xDim, yDim, zDim;

        xDim = imageA.getExtents()[0];
        yDim = imageA.getExtents()[1];
        zDim = imageA.getExtents()[2];

        try
        {
            switch (show)
            {
                case SHOW012:
                    imageA.exportSliceXY(timeSliceA * zDim + slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceXY(timeSliceB * zDim + slice, imageBufferB);
                    }
                    break;

                case SHOW012R:
                    imageA.exportSliceXY(timeSliceA * zDim + (zDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceXY(timeSliceB * zDim + (zDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW01R2:
                    imageA.exportDataR1( (timeSliceA * zDim + slice) * xDim * yDim, xDim, yDim, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataR1( (timeSliceB * zDim + slice) * xDim * yDim, xDim, yDim, imageBufferB);
                    }
                    break;

                case SHOW01R2R:
                    imageA.exportDataR1( (timeSliceA * zDim + (zDim - 1 - slice)) * xDim * yDim, xDim, yDim,
                                        imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataR1( (timeSliceB * zDim + (zDim - 1 - slice)) * xDim * yDim, xDim, yDim,
                                            imageBufferB);
                    }
                    break;

                case SHOW0R12:

                    imageA.exportDataR0( (timeSliceA * zDim + slice) * xDim * yDim, xDim, yDim, imageBufferA);

                    if (imageB != null)
                    {
                        imageB.exportDataR0( (timeSliceB * zDim + slice) * xDim * yDim, xDim, yDim, imageBufferB);
                    }
                    break;

                case SHOW0R12R:
                    imageA.exportDataR0( (timeSliceA * zDim + (zDim - 1 - slice)) * xDim * yDim, xDim, yDim,
                                        imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataR0( (timeSliceB * zDim + (zDim - 1 - slice)) * xDim * yDim, xDim, yDim,
                                            imageBufferB);
                    }
                    break;

                case SHOW0R1R2:

                    imageA.exportDataR0R1( (timeSliceA * zDim + slice) * xDim * yDim, xDim, yDim, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataR0R1( (timeSliceB * zDim + slice) * xDim * yDim, xDim, yDim, imageBufferB);
                    }
                    break;

                case SHOW0R1R2R:
                    imageA.exportDataR0R1( (timeSliceA * zDim + (zDim - 1 - slice)) * xDim * yDim, xDim, yDim,
                                          imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataR0R1( (timeSliceB * zDim + (zDim - 1 - slice)) * xDim * yDim, xDim, yDim,
                                              imageBufferB);
                    }
                    break;

                case SHOW102:

                    imageA.exportDataXD( (timeSliceA * zDim + slice) * xDim * yDim, xDim, yDim, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataXD( (timeSliceB * zDim + slice) * xDim * yDim, xDim, yDim, imageBufferB);
                    }
                    break;

                case SHOW102R:

                    imageA.exportDataXD( (timeSliceA * zDim + (zDim - 1 - slice)) * xDim * yDim, xDim, yDim,
                                        imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataXD( (timeSliceB * zDim + (zDim - 1 - slice)) * xDim * yDim, xDim, yDim,
                                            imageBufferB);
                    }
                    break;

                case SHOW10R2:

                    imageA.exportDataXDR0( (timeSliceA * zDim + slice) * xDim * yDim, xDim, yDim, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataXDR0( (timeSliceB * zDim + slice) * xDim * yDim, xDim, yDim, imageBufferB);
                    }
                    break;

                case SHOW10R2R:
                    imageA.exportDataXDR0( (timeSliceA * zDim + (zDim - 1 - slice)) * xDim * yDim, xDim, yDim,
                                          imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataXDR0( (timeSliceB * zDim + (zDim - 1 - slice)) * xDim * yDim, xDim, yDim,
                                              imageBufferB);
                    }
                    break;

                case SHOW1R02:

                    imageA.exportDataXDR1( (timeSliceA * zDim + slice) * xDim * yDim, xDim, yDim, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataXDR1( (timeSliceB * zDim + slice) * xDim * yDim, xDim, yDim, imageBufferB);
                    }
                    break;

                case SHOW1R02R:

                    imageA.exportDataXDR1( (timeSliceA * zDim + (zDim - 1 - slice)) * xDim * yDim, xDim, yDim,
                                          imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataXDR1( (timeSliceB * zDim + (zDim - 1 - slice)) * xDim * yDim, xDim, yDim,
                                              imageBufferB);
                    }
                    break;

                case SHOW1R0R2:
                    imageA.exportDataXDR0R1( (timeSliceA * zDim + slice) * xDim * yDim, xDim, yDim, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataXDR0R1( (timeSliceB * zDim + slice) * xDim * yDim, xDim, yDim, imageBufferB);
                    }
                    break;

                case SHOW1R0R2R:
                    imageA.exportDataXDR0R1( (timeSliceA * zDim + (zDim - 1 - slice)) * xDim * yDim, xDim, yDim,
                                            imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataXDR0R1( (timeSliceB * zDim + (zDim - 1 - slice)) * xDim * yDim, xDim, yDim,
                                                imageBufferB);
                    }
                    break;

                case SHOW021:

                    // z was y in the original image
                    imageA.exportSliceXZ(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceXZ(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW021R:

                    imageA.exportSliceXZ(timeSliceA, (yDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceXZ(timeSliceB, (yDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW02R1:

                    imageA.exportSliceXZR(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceXZR(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW02R1R:

                    imageA.exportSliceXZR(timeSliceA, (yDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceXZR(timeSliceB, (yDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW0R21:

                    imageA.exportSliceXRZ(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceXRZ(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW0R21R:

                    imageA.exportSliceXRZ(timeSliceA, (yDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceXRZ(timeSliceB, (yDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW0R2R1:

                    imageA.exportSliceXRZR(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceXRZR(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW0R2R1R:

                    imageA.exportSliceXRZR(timeSliceA, (yDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceXRZR(timeSliceB, (yDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW201:

                    imageA.exportSliceZX(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceZX(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW201R:

                    imageA.exportSliceZX(timeSliceA, (yDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceZX(timeSliceB, (yDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW20R1:

                    imageA.exportSliceZXR(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceZXR(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW20R1R:

                    imageA.exportSliceZXR(timeSliceA, (yDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceZXR(timeSliceB, (yDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW2R01:

                    imageA.exportSliceZRX(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceZRX(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW2R01R:

                    imageA.exportSliceZRX(timeSliceA, (yDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceZRX(timeSliceB, (yDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW2R0R1:

                    imageA.exportSliceZRXR(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceZRXR(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW2R0R1R:

                    imageA.exportSliceZRXR(timeSliceA, (yDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceZRXR(timeSliceB, (yDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW120:

                    imageA.exportSliceYZ(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceYZ(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW120R:

                    imageA.exportSliceYZ(timeSliceA, (xDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceYZ(timeSliceB, (xDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW12R0:

                    imageA.exportSliceYZR(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceYZR(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW12R0R:

                    imageA.exportSliceYZR(timeSliceA, (xDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceYZR(timeSliceB, (xDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW1R20:

                    imageA.exportSliceYRZ(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceYRZ(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW1R20R:

                    imageA.exportSliceYRZ(timeSliceA, (xDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceYRZ(timeSliceB, (xDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW1R2R0:

                    imageA.exportSliceYRZR(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceYRZR(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW1R2R0R:

                    imageA.exportSliceYRZR(timeSliceA, (xDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceYRZR(timeSliceB, (xDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW210:

                    imageA.exportSliceZY(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceZY(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW210R:

                    imageA.exportSliceZY(timeSliceA, (xDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceZY(timeSliceB, (xDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW21R0:

                    imageA.exportSliceZYR(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceZYR(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW21R0R:

                    imageA.exportSliceZYR(timeSliceA, (xDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceZYR(timeSliceB, (xDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW2R10:

                    imageA.exportSliceZRY(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceZRY(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW2R10R:

                    imageA.exportSliceZRY(timeSliceA, (xDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceZRY(timeSliceB, (xDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW2R1R0:

                    imageA.exportSliceZRYR(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceZRYR(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW2R1R0R:

                    imageA.exportSliceZRYR(timeSliceA, (xDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportSliceZRYR(timeSliceB, (xDim - 1 - slice), imageBufferB);
                    }
                    break;
            } // end switch(show)
        } // end try
        catch (IOException error)
        {
            MipavUtil.displayError("" + error); // Need to fix this
            error.printStackTrace();
            return;
        }
    }

    private void fillImageColorBuffer(int show, int slice)
    {

        int xDim, yDim, zDim;

        xDim = imageA.getExtents()[0];
        yDim = imageA.getExtents()[1];
        zDim = imageA.getExtents()[2];

        try
        {
            switch (show)
            {
                case SHOW012:
                    imageA.exportData( (timeSliceA * zDim + slice) * 4 * xDim * yDim, 4 * xDim, yDim, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportData( (timeSliceB * zDim + slice) * 4 * xDim * yDim, 4 * xDim, yDim, imageBufferB);
                    }
                    break;

                case SHOW012R:
                    imageA.exportData( (timeSliceA * zDim + (zDim - 1 - slice)) * 4 * xDim * yDim, 4 * xDim, yDim,
                                      imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportData( (timeSliceB * zDim + (zDim - 1 - slice)) * 4 * xDim * yDim, 4 * xDim, yDim,
                                          imageBufferB);
                    }
                    break;

                case SHOW01R2:
                    imageA.exportDataR1( (timeSliceA * zDim + slice) * 4 * xDim * yDim, 4 * xDim, yDim, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataR1( (timeSliceB * zDim + slice) * 4 * xDim * yDim, 4 * xDim, yDim,
                                            imageBufferB);
                    }
                    break;

                case SHOW01R2R:
                    imageA.exportDataR1( (timeSliceA * zDim + (zDim - 1 - slice)) * 4 * xDim * yDim, 4 * xDim, yDim,
                                        imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataR1( (timeSliceB * zDim + (zDim - 1 - slice)) * 4 * xDim * yDim, 4 * xDim, yDim,
                                            imageBufferB);
                    }
                    break;

                case SHOW0R12:
                    imageA.exportRGBDataR0( (timeSliceA * zDim + slice) * 4 * xDim * yDim, 4 * xDim, yDim, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBDataR0( (timeSliceB * zDim + slice) * 4 * xDim * yDim, 4 * xDim, yDim,
                                               imageBufferB);
                    }
                    break;

                case SHOW0R12R:
                    imageA.exportRGBDataR0( (timeSliceA * zDim + (zDim - 1 - slice)) * 4 * xDim * yDim, 4 * xDim, yDim,
                                           imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBDataR0( (timeSliceB * zDim + (zDim - 1 - slice)) * 4 * xDim * yDim, 4 * xDim,
                                               yDim, imageBufferB);
                    }
                    break;

                case SHOW0R1R2:
                    imageA.exportRGBDataR0R1( (timeSliceA * zDim + slice) * 4 * xDim * yDim, 4 * xDim, yDim,
                                             imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBDataR0R1( (timeSliceB * zDim + slice) * 4 * xDim * yDim, 4 * xDim, yDim,
                                                 imageBufferB);
                    }
                    break;

                case SHOW0R1R2R:
                    imageA.exportRGBDataR0R1( (timeSliceA * zDim + (zDim - 1 - slice)) * 4 * xDim * yDim, 4 * xDim,
                                             yDim,
                                             imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBDataR0R1( (timeSliceB * zDim + (zDim - 1 - slice)) * 4 * xDim * yDim, 4 * xDim,
                                                 yDim, imageBufferB);
                    }
                    break;

                case SHOW102:
                    imageA.exportDataXD( (timeSliceA * zDim + slice) * 4 * xDim * yDim, 4 * xDim, yDim, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataXD( (timeSliceB * zDim + slice) * 4 * xDim * yDim, 4 * xDim, yDim,
                                            imageBufferB);
                    }
                    break;

                case SHOW102R:
                    imageA.exportDataXD( (timeSliceA * zDim + (zDim - 1 - slice)) * 4 * xDim * yDim, 4 * xDim, yDim,
                                        imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataXD( (timeSliceB * zDim + (zDim - 1 - slice)) * 4 * xDim * yDim, 4 * xDim, yDim,
                                            imageBufferB);
                    }
                    break;

                case SHOW10R2:
                    imageA.exportDataXDR0( (timeSliceA * zDim + slice) * 4 * xDim * yDim, 4 * xDim, yDim, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataXDR0( (timeSliceB * zDim + slice) * 4 * xDim * yDim, 4 * xDim, yDim,
                                              imageBufferB);
                    }
                    break;

                case SHOW10R2R:
                    imageA.exportDataXDR0( (timeSliceA * zDim + (zDim - 1 - slice)) * 4 * xDim * yDim, 4 * xDim, yDim,
                                          imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataXDR0( (timeSliceB * zDim + (zDim - 1 - slice)) * 4 * xDim * yDim, 4 * xDim,
                                              yDim, imageBufferB);
                    }
                    break;

                case SHOW1R02:
                    imageA.exportDataXDR1( (timeSliceA * zDim + slice) * 4 * xDim * yDim, 4 * xDim, yDim, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataXDR1( (timeSliceB * zDim + slice) * 4 * xDim * yDim, 4 * xDim, yDim,
                                              imageBufferB);
                    }
                    break;

                case SHOW1R02R:
                    imageA.exportDataXDR1( (timeSliceA * zDim + (zDim - 1 - slice)) * 4 * xDim * yDim, 4 * xDim, yDim,
                                          imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataXDR1( (timeSliceB * zDim + (zDim - 1 - slice)) * 4 * xDim * yDim, 4 * xDim,
                                              yDim, imageBufferB);
                    }
                    break;

                case SHOW1R0R2:
                    imageA.exportDataXDR0R1( (timeSliceA * zDim + slice) * 4 * xDim * yDim, 4 * xDim, yDim,
                                            imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataXDR0R1( (timeSliceB * zDim + slice) * 4 * xDim * yDim, 4 * xDim, yDim,
                                                imageBufferB);
                    }
                    break;

                case SHOW1R0R2R:
                    imageA.exportDataXDR0R1( (timeSliceA * zDim + (zDim - 1 - slice)) * 4 * xDim * yDim, 4 * xDim, yDim,
                                            imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportDataXDR0R1( (timeSliceB * zDim + (zDim - 1 - slice)) * 4 * xDim * yDim, 4 * xDim,
                                                yDim, imageBufferB);
                    }
                    break;

                case SHOW021:

                    // z was y in the original image
                    imageA.exportRGBSliceXZ(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceXZ(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW021R:
                    imageA.exportRGBSliceXZ(timeSliceA, (yDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceXZ(timeSliceB, (yDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW02R1:
                    imageA.exportRGBSliceXZR(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceXZR(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW02R1R:
                    imageA.exportRGBSliceXZR(timeSliceA, (yDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceXZR(timeSliceB, (yDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW0R21:
                    imageA.exportRGBSliceXRZ(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceXRZ(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW0R21R:
                    imageA.exportRGBSliceXRZ(timeSliceA, (yDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceXRZ(timeSliceB, (yDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW0R2R1:
                    imageA.exportRGBSliceXRZR(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceXRZR(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW0R2R1R:
                    imageA.exportRGBSliceXRZR(timeSliceA, (yDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceXRZR(timeSliceB, (yDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW201:
                    imageA.exportRGBSliceZX(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceZX(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW201R:
                    imageA.exportRGBSliceZX(timeSliceA, (yDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceZX(timeSliceB, (yDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW20R1:
                    imageA.exportRGBSliceZXR(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceZXR(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW20R1R:
                    imageA.exportRGBSliceZXR(timeSliceA, (yDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceZXR(timeSliceB, (yDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW2R01:
                    imageA.exportRGBSliceZRX(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceZRX(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW2R01R:
                    imageA.exportRGBSliceZRX(timeSliceA, (yDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceZRX(timeSliceB, (yDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW2R0R1:
                    imageA.exportRGBSliceZRXR(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceZRXR(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW2R0R1R:
                    imageA.exportRGBSliceZRXR(timeSliceA, (yDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceZRXR(timeSliceB, (yDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW120:
                    imageA.exportRGBSliceYZ(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceYZ(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW120R:
                    imageA.exportRGBSliceYZ(timeSliceA, (xDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceYZ(timeSliceB, (xDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW12R0:
                    imageA.exportRGBSliceYZR(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceYZR(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW12R0R:
                    imageA.exportRGBSliceYZR(timeSliceA, (xDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceYZR(timeSliceB, (xDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW1R20:
                    imageA.exportRGBSliceYRZ(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceYRZ(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW1R20R:
                    imageA.exportRGBSliceYRZ(timeSliceA, (xDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceYRZ(timeSliceB, (xDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW1R2R0:
                    imageA.exportRGBSliceYRZR(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceYRZR(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW1R2R0R:
                    imageA.exportRGBSliceYRZR(timeSliceA, (xDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceYRZR(timeSliceB, (xDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW210:
                    imageA.exportRGBSliceZY(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceZY(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW210R:
                    imageA.exportRGBSliceZY(timeSliceA, (xDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceZY(timeSliceB, (xDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW21R0:
                    imageA.exportRGBSliceZYR(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceZYR(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW21R0R:
                    imageA.exportRGBSliceZYR(timeSliceA, (xDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceZYR(timeSliceB, (xDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW2R10:
                    imageA.exportRGBSliceZRY(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceZRY(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW2R10R:
                    imageA.exportRGBSliceZRY(timeSliceA, (xDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceZRY(timeSliceB, (xDim - 1 - slice), imageBufferB);
                    }
                    break;

                case SHOW2R1R0:
                    imageA.exportRGBSliceZRYR(timeSliceA, slice, imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceZRYR(timeSliceB, slice, imageBufferB);
                    }
                    break;

                case SHOW2R1R0R:
                    imageA.exportRGBSliceZRYR(timeSliceA, (xDim - 1 - slice), imageBufferA);
                    if (imageB != null)
                    {
                        imageB.exportRGBSliceZRYR(timeSliceB, (xDim - 1 - slice), imageBufferB);
                    }
                    break;
            } // end switch(showXY)
        }
        catch (IOException error)
        {
            MipavUtil.displayError("" + error);
            return;
        }

    }

    public void actionPerformed(ActionEvent event)
    {
        String command = event.getActionCommand();

        if (command.equals(DELETE_INTENSITY_LINE)) // handling the popup menu for the VOI intensity line
        {
            intensityLine = null;
            repaint();
        }
        else if (command.equals(SHOW_INTENSITY_GRAPH)) // handling the popup menu for the VOI intensity line
        {
            showIntensityGraph(axisOrder[0], axisOrder[1]);
        }
    }

    public void setIntensityLineVisible(boolean visible)
    {
        intensityLineVisible = visible;
    }

    public boolean isIntensityLineVisible()
    {
        return intensityLineVisible;
    }
}
