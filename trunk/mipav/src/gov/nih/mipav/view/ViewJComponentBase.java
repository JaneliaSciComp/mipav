package gov.nih.mipav.view;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.util.MipavMath;
import gov.nih.mipav.model.structures.*;

import java.util.*;
import java.awt.*;
import java.awt.image.*;
import javax.swing.*;

/**
 * Abstract class used for displaying images in the program MIPAV.
 *
 * @version  1.0 August 31, 1999
 * @author   Matthew J. McAuliffe, Ph.D.
 *
 *           <p>$Logfile: /mipav/src/gov/nih/mipav/view/ViewJComponentBase.java $ $Revision: 135 $ $Date: 1/30/06 2:25p
 *           $</p>
 */
public abstract class ViewJComponentBase extends JComponent {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

	/** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1024094346997939797L;

    /** DOCUMENT ME! */
    public static final int SMOOTH = 2;

    /** Image interpolation only done for image A */
    public static final int INTERPOLATE_A = 1;

    /** Image interpolation only done for image B */
    public static final int INTERPOLATE_B = 2;

    /** Image interpolation only done for both images A and B */
    public static final int INTERPOLATE_BOTH = 3;

    /** Nearest neighbor interpolation applied to both images */
    public static final int NEAREST_BOTH = 6;

    /** DOCUMENT ME! */
    public static final int IMAGE_A = 0;

    /** DOCUMENT ME! */
    public static final int IMAGE_B = 1;

    /** DOCUMENT ME! */
    public static final int BOTH = 2;

    /** Used to describe cursor mode, this means no VOIs will be drawn. */
    public static final int DEFAULT = 0;

    /** DOCUMENT ME! */
    public static final int SELECT = 1;

    /** DOCUMENT ME! */
    public static final int MOVE = 2;

    /** Indicates the MIPAV cursor is in slice rectangle VOI mode */
    public static final int RECTANGLE = 3;

    /** Indicates the MIPAV cursor is in volume rectangle VOI mode */
    public static final int RECTANGLE3D = 4;

    /** Indicates the MIPAV cursor is in levelset VOI mode */
    public static final int LEVELSET = 6;

    /** DOCUMENT ME! */
    public static final int MOVE_POINT = 7;

    /** DOCUMENT ME! */
    public static final int NEW_POINT = 8;

    /** DOCUMENT ME! */
    public static final int DELETE_POINT = 9;

    /** DOCUMENT ME! */
    public static final int WAND = 10;

    /** Indicates the MIPAV cursor is in ellipse VOI mode */
    public static final int ELLIPSE = 11;

    /** Indicates the MIPAV cursor is in 2D line VOI mode */
    public static final int LINE = 12;

    /** Indicates the MIPAV cursor is in polyline VOI mode */
    public static final int POLYLINE = 13;

    /** Indicates that a VOI has just been created, is not related to the various
     * possible VOI modes that the handler deals with*/
    public static final int NEW_VOI = 14;

    /** DOCUMENT ME! */
    public static final int RETRACE = 15;

    /** Indicates the MIPAV cursor is in point VOI mode */
    public static final int POINT_VOI = 16;

    /** Indicates the MIPAV cursor is in paint VOI mode */
    public static final int PAINT_VOI = 17;

    /** Indicates the cursor is in paint can mode  */
    public static final int PAINT_CAN = 18;

    /** This mode is an active VOI mode used by the VOI Handler */
    public static final int DROPPER_PAINT = 19;

    /** This mode is an active VOI mode used by the VOI Handler  */
    public static final int ERASER_PAINT = 20;

    /** DOCUMENT ME! */
    public static final int MAG_REGION = 21;

    /** DOCUMENT ME! */
    public static final int WIN_REGION = 22;

    /** This mode is used by the VOI Handler to determine whether VOIs will be drawn.
     * A quick lut is implemented in MIPAV as a rectangle VOI */
    public static final int QUICK_LUT = 23;

    /** This mode is an active VOI mode used by the VOI Handler  */
    public static final int PROTRACTOR = 24;

    /** Indicates the VOI cursor is in translate mode */
    public static final int ROTATE = 25;

    /** Indicates the VOI cursor is in translate mode */
    public static final int TRANSLATE = 26;

    /** DOCUMENT ME! */
    public static final int MOVE_VOIPOINT = 27;

    /** DOCUMENT ME! */
    public static final int CENTER_VOI = 28;

    /** DOCUMENT ME! */
    public static final int CUBE_BOUNDS = 29;

    /** Indicates the MIPAV cursor is in livewire VOI mode */
    public static final int LIVEWIRE = 30;

    /** DOCUMENT ME! */
    public static final int PAINT_VASC = 31;

    /** Indicates the MIPAV cursor is in annotation VOI mode */
    public static final int ANNOTATION = 32;

    /** DOCUMENT ME! */
    public static final int PROBE = 33;

    /** DOCUMENT ME! */
    public static final int MOVE_INTERSECTION_POINT = 34;

    /** Indicates the cursor is in zoom in mode */
    public static final int ZOOMING_IN = 35;

    /** Indicates the cursor is in zoom out mode */
    public static final int ZOOMING_OUT = 36;

    /** Indicates the MIPAV cursor is in polyline slice VOI mode */
    public static final int POLYLINE_SLICE_VOI = 37;

    /** Indicates the MIPAV cursor is in split VOI mode */
    public static final int SPLIT_VOI = 38;
    
    /** Indicates the cursor is in 3D voi drawing mode while in tri-planar view */
    public static final int VOI_3D = 39;
    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected Dimension imageDim = null;

    /** DOCUMENT ME! */
    protected Image img; // always magnification of 1;

    /** DOCUMENT ME! */
    protected Image imgB;

    /** Interpolation mode for image display */
    protected int interpMode = NEAREST_BOTH;

    /** DOCUMENT ME! */
    protected MemoryImageSource memImageA = null;

    /** DOCUMENT ME! */
    protected MemoryImageSource memImageB = null;

    /**
     * resolutionX and Y are used to correct difference in intra and inter
     * plane voxel resolution These represent the aspect ratio of the image
     * These are NOT to be confused with the pixel resolutions.
     */
    protected float resolutionX = 1;

    /** DOCUMENT ME! */
    protected float resolutionY = 1;

    /** DOCUMENT ME! */
    protected boolean showSliceNumber = true;

    /** DOCUMENT ME! */
    protected String sliceString;

    /** DOCUMENT ME! */
    protected Color textColor = new Color(240, 240, 0);

    /** DOCUMENT ME! */
    protected float zoomX = 1;

    /** DOCUMENT ME! */
    protected float zoomY = 1;

    /** DOCUMENT ME! */
    protected int OUT_OF_BOUNDS = -9999;

    /** used by the repaintPaintBrushCursorFast method */
    protected int lastMouseX = OUT_OF_BOUNDS;

    /** used by the repaintPaintBrushCursorFast method */
    protected int lastMouseY = OUT_OF_BOUNDS;

    //~ Constructors ---------------------------------------------------------------------------------------------------
    /**
     * creates object of size defined by width & height.
     *
     * @param  compDim  width and height of component
     */
    public ViewJComponentBase(Dimension compDim) {
        img = null;
        imageDim = compDim;
        sliceString = "0";
        setSize(imageDim.width, imageDim.height);
        setDoubleBuffered(false);
    }

    /**
     * creates object of size defined by width & height.
     *
     * @param extents the image width and height
     * @param _imageA the image that this ViewJComponentBase is a view of
     */
    public ViewJComponentBase( int width, int height, ModelImage _imageA ) {
        img = null;
        imageDim = new Dimension( width, height );
        sliceString = "0";
        setSize(imageDim.width, imageDim.height);
        setDoubleBuffered(false);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  flag  DOCUMENT ME!
     */
    public void dispose(boolean flag) {
        disposeLocal();
    }

    /**
     * Clean up some resources!
     */
    public void disposeLocal() {

        if (img != null) {
            img.flush();
            img = null;
        }

        if (imgB != null) {
            imgB.flush();
            imgB = null;
        }

        imageDim = null;
        
        
        
        memImageA = null;
        memImageB = null;
        textColor = null;
        sliceString = null;
    }

    /**
     * Gets the Java image.
     *
     * @return  Java image
     *
     * @see     Image
     */
    public Image getImage() {
        return img;
    }

    /**
     * Gets the interpolation mode.
     *
     * @return  returns the interpolation mode
     */
    public int getInterpMode() {
        return interpMode;
    }

    /**
     * Get the x position of the last place we drew something from a mouse event.
     *
     * @return  the x coordinate of the last mouse event
     */
    public int getLastMouseX() {
        return lastMouseX;
    }

    /**
     * Get the y position of the last place we drew something from a mouse event.
     *
     * @return  the y coordinate of the last mouse event
     */
    public int getLastMouseY() {
        return lastMouseY;
    }

    /**
     * size set to object size.
     *
     * @return  dimension with the size
     */
    public Dimension getPreferredSize() {

        try {
            return new Dimension(Math.round(zoomX * imageDim.width * resolutionX),
                                 Math.round(zoomY * imageDim.height * resolutionY));
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentBase.getPreferredSize");

            return null;
        }
    }

    /**
     * Resolution correction factor in the x - dimension.
     *
     * @return  correction in the x - dimension
     */
    public float getResolutionX() {
        return resolutionX;
    }

    /**
     * Resolution correction factor in the y - dimension.
     *
     * @return  correction in the y - dimension
     */
    public float getResolutionY() {
        return resolutionY;
    }

    /**
     * Converts the screen cursor x-coordinate to scaled version using the
     * image's pixel resolution and zoom.
     *
     * @param   x  the cursor's x-coordinate
     *
     * @return  x-coordinate in image space
     */
    public int getScaledX(int x) {
        return (int) (x / (zoomX * resolutionX));
    }

    /**
     * Converts the screen cursor y-coordinate to scaled version using the
     * image's pixel resolution and zoom.
     *
     * @param   y  the cursor's y-coordinate
     *
     * @return  x-coordinate in image space
     */
    public int getScaledY(int y) {
        return (int) (y / (zoomY * resolutionY));
    }

    /**
     * LocalToScreen converts a point from local coordinates to screen coordinates.
     * @param localPoint a 3D point, where x,y represent the point in local
     * slice coordinates and the z value is the slice value.
     * @param screenPoint the transfromed localPoint, scaled with the image
     * pixel resolution and zoom.
     */
    public void LocalToScreen( Vector3f localPoint, Vector3f screenPoint )
    {
        screenPoint.X = Math.round(localPoint.X * ( zoomX * resolutionX ));
        screenPoint.Y = Math.round(localPoint.Y * ( zoomY * resolutionY ));
        /* store but do not transform the z value (z = slice displayed) */
        screenPoint.Z = Math.round(localPoint.Z);
    }

    /**
     * ScreenToLocal converts a point from screen coordinates to local coordinates.
     * @param screenPoint a 3D point, where x,y represent the point in local
     * slice coordinates and the z value is the slice value. screenPoint is
     * usually taken from the MouseEvent point.
     * @param localPoint the transfromed point in local slice
     * coordinates. The z-value represents the current slice.
     */
    public void ScreenToLocal( Vector3f screenPoint, Vector3f localPoint )
    {
        localPoint.X = Math.round(screenPoint.X / ( zoomX * resolutionX ));
        localPoint.Y = Math.round(screenPoint.Y / ( zoomY * resolutionY ));
        /* store but do not transform the z value (z = slice displayed) */
        localPoint.Z = Math.round(screenPoint.Z);
    }


    /**
     * Returns whether to enable the showIntensity checkbox for mag. box
     *
     * @param   g             DOCUMENT ME!
     * @param   width         DOCUMENT ME!
     * @param   height        DOCUMENT ME!
     * @param   mag           DOCUMENT ME!
     * @param   imageType     DOCUMENT ME!
     * @param   minIntensity  DOCUMENT ME!
     * @param   maxIntensity  DOCUMENT ME!
     *
     * @return  whether to enable showIntensity checkbox
     */
    public boolean getShowMagIntensityEnabled(Graphics g, int width, int height, float mag, int imageType,
                                              double minIntensity, double maxIntensity) {

        // ****need to remove later
        if ((g == null) || (img == null)) {
            return false;
        }

        if (zoomX >= 2) {

            while (((Math.round(width / zoomX) - (width / zoomX)) != 0) ||
                       ((Math.round(width / zoomX / 2.0f) - (width / zoomX / 2.0f)) != 0)) {
                width++;
            }
        }

        height = width;

        int sIWidth = (int) (width / mag);
        float xwidth = (float) width / ((int) (sIWidth / 2) + (int) (sIWidth / 2));
        float yheight = (float) height / ((int) (sIWidth / 2) + (int) (sIWidth / 2));

        int fontHeight = g.getFontMetrics(g.getFont()).getHeight();
        int minStrWidth = g.getFontMetrics(g.getFont()).stringWidth(Integer.toString((int) minIntensity));
        int maxStrWidth = g.getFontMetrics(g.getFont()).stringWidth(Integer.toString((int) maxIntensity));

        if (minStrWidth > maxStrWidth) {
            maxStrWidth = minStrWidth;
        }

        int maxCharWidth = g.getFontMetrics(g.getFont()).charWidth('8');

        if ((((imageType == ModelImage.FLOAT) || (imageType == ModelImage.DOUBLE) ||
                  (imageType == ModelImage.COMPLEX) || (imageType == ModelImage.DCOMPLEX) ||
                  (imageType == ModelImage.ARGB) || (imageType == ModelImage.ARGB_USHORT)) ||
                  (imageType == ModelImage.ARGB_FLOAT) &&
                 ((maxStrWidth < (xwidth - 1 - (2 * maxCharWidth))) && (fontHeight < (yheight - 1)))) ||
                (((imageType != ModelImage.FLOAT) && (imageType != ModelImage.DOUBLE) &&
                      (imageType != ModelImage.COMPLEX) && (imageType != ModelImage.DCOMPLEX) &&
                      (imageType != ModelImage.ARGB) && (imageType != ModelImage.ARGB_USHORT) &&
                      (imageType != ModelImage.ARGB_FLOAT)) &&
                     ((maxStrWidth < (xwidth - 1)) && (fontHeight < (yheight - 1))))) {

            return true;

        } else {
            return false;
        }
    }

    /**
     * gets the size of the object taking into account the zoom.
     *
     * @param   wh  DOCUMENT ME!
     *
     * @return  dimension with the size
     */
    public Dimension getSize(Dimension wh) {

        try {

            if (wh == null) {
                return new Dimension(Math.round(zoomX * imageDim.width * resolutionX),
                                     Math.round(zoomY * imageDim.height * resolutionY));
            } else {
                wh.setSize(Math.round(zoomX * imageDim.width * resolutionX),
                           Math.round(zoomY * imageDim.height * resolutionY));

                return wh;
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentBase.getSize");

            return null;
        }
    }

    /**
     * Magnification in the x - dimension.
     *
     * @return  magnificaiton in the x - dimension
     */
    public float getZoomX() {
        return zoomX;
    }

    /**
     * Magnification in the y - dimension.
     *
     * @return  magnificaiton in the y - dimension
     */
    public float getZoomY() {
        return zoomY;
    }

    /**
     * Creates a Image object from an array of ints that have been formatted (packed) properly (i.e. aRGB)
     *
     * @param  data  Data (image) to be displayed that has been formatted (packed) properly (i.e. aRGB)
     */
    public void importImage(int[] data) {

        try {

            if (memImageA == null) {
                memImageA = new MemoryImageSource(imageDim.width, imageDim.height, data, 0, imageDim.width);
                img = createImage(memImageA);
            } else {
                memImageA.newPixels(data, ColorModel.getRGBdefault(), 0, imageDim.width);
                img.flush();
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentBase.importImage.");
        }
    }

    /**
     * Creates a Image object from an array of ints that have been formatted (packed) properly (i.e. aRGB) for the
     * magnifier in image B
     *
     * @param  dataB  Data (imageB) to be displayed that has been formatted (packed) properly (i.e. aRGB)
     */
    public void importImageB(int[] dataB) {

        try {

            if (memImageB == null) {
                memImageB = new MemoryImageSource(imageDim.width, imageDim.height, dataB, 0, imageDim.width);
                imgB = createImage(memImageB);
            } else {
                memImageB.newPixels(dataB, ColorModel.getRGBdefault(), 0, imageDim.width);
                imgB.flush();
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            Preferences.debug("Out of memory: ComponentBase.importImageB.");
        }

    }

    /**
     * Paints the image and border.
     *
     * @param  g  Graphics handle
     */
    public void paintComponent(Graphics g) {

        /**
         *  this method may not be needed anymore, since it has been refactored into ViewJComponentEditImage
         */

        try {

            if (g == null) {
                return;
            }

            // setDebugGraphicsOptions(DebugGraphics.LOG_OPTION);
            // setDebugGraphicsOptions(DebugGraphics.FLASH_OPTION);
            // setDebugGraphicsOptions(DebugGraphics.BUFFERED_OPTION);
            if (img != null) {

                g.setClip(getVisibleRect());

                if (interpMode == SMOOTH) {
                    g.drawImage(img, 0, 0, null);
                } else {
                    g.drawImage(img, 0, 0, Math.round(zoomX * img.getWidth(this) * resolutionX),
                                Math.round(zoomY * img.getHeight(this) * resolutionY), 0, 0, img.getWidth(this),
                                img.getHeight(this), null);
                }

                g.setFont(MipavUtil.font12);

                if ((((int) ((zoomX * imageDim.width) + 0.5) - 40) > 0) && (sliceString != null) &&
                        (showSliceNumber == true)) {
                    g.setColor(Color.black);
                    g.drawString(sliceString, 5, (int) ((zoomY * resolutionY * imageDim.height) + 0.5) - 5);
                    g.drawString(sliceString, 5, (int) ((zoomY * resolutionY * imageDim.height) + 0.5) - 6);
                    g.drawString(sliceString, 5, (int) ((zoomY * resolutionY * imageDim.height) + 0.5) - 4);
                    g.drawString(sliceString, 6, (int) ((zoomY * resolutionY * imageDim.height) + 0.5) - 5);
                    g.drawString(sliceString, 4, (int) ((zoomY * resolutionY * imageDim.height) + 0.5) - 5);
                    g.setColor(Color.white);
                    g.drawString(sliceString, 5, (int) ((zoomY * resolutionY * imageDim.height) + 0.5) - 5);
                }

            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentBase.paintComponent.");
        }
    }

    /**
     * Paints a box over the image centered about the cursor.
     *
     * @param  g       graphics component
     * @param  xNew    x coord in image
     * @param  yNew    y coord in image
     * @param  width   width of the cursor window in pixels
     * @param  height  height of the cursor window in pixels
     */
    public void paintCursorBoxComponent(Graphics g, int xNew, int yNew, int width, int height) {

        if (g == null) {
            return;
        }

        if ((xNew == lastMouseX) && (yNew == lastMouseY)) {
            return;
        }

        if ((lastMouseX == 0) && (lastMouseY == 0)) {
            lastMouseX = xNew;
            lastMouseY = yNew;

            return;
        }

        int xNew0 = xNew - MipavMath.round(0.5f * width);
        int yNew0 = yNew - MipavMath.round(0.5f * height);

        if ((width == (int) zoomX) && (height == (int) zoomY)) // if we are painting using the 1-pixel brush
        {

            // not sure why, exactly, the +0.5f is needed
            // but it seems to make it work --- lorsino
            xNew0 = MipavMath.round((xNew0 / getZoomX() * resolutionX) + 0.5f);
            xNew0 = MipavMath.round((xNew0 * getZoomX() * resolutionX) + 0.5f);
            yNew0 = MipavMath.round((yNew0 / getZoomY() * resolutionY) + 0.5f);
            yNew0 = MipavMath.round((yNew0 * getZoomY() * resolutionY) + 0.5f);
        } else {
            xNew0 = MipavMath.round(xNew0 / getZoomX() * resolutionX);
            xNew0 = MipavMath.round(xNew0 * getZoomX() * resolutionX);
            yNew0 = MipavMath.round(yNew0 / getZoomY() * resolutionY);
            yNew0 = MipavMath.round(yNew0 * getZoomY() * resolutionY);
        }

        this.paintComponent(g);

        g.setColor(Color.red.darker());
        g.drawRect(xNew0, yNew0, width - 1, height - 1);

        lastMouseX = xNew0;
        lastMouseY = yNew0;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  graphics  DOCUMENT ME!
     * @param  xNew      DOCUMENT ME!
     * @param  yNew      DOCUMENT ME!
     * @param  width     DOCUMENT ME!
     * @param  height    DOCUMENT ME!
     * @param  mag       DOCUMENT ME!
     */
    public void paintWindowComponent(Graphics graphics, int xNew, int yNew, int width, int height, float mag) {
        paintWindowComponent(graphics, xNew, yNew, width, height, mag, imgB);
    }

    /**
     * Paints a image B in a window over the image centered about the cursor.
     *
     * @param  graphics   graphics component
     * @param  xNew       x coord in image
     * @param  yNew       y coord in image
     * @param  width      width of the magnification window in pixels in unit zoom
     * @param  height     height of the magnification window in pixels in unit zoom
     * @param  mag        magnification of the zoom window
     * @param  drawImage  DOCUMENT ME!
     */
    public void paintWindowComponent(Graphics graphics, int xNew, int yNew, int width, int height, float mag,
                                     Image drawImage) {
        int xNewO, yNewO;
        int x1, y1, xw1, yh1;
        int x2, y2;

        if (graphics == null) {
            return;
        }

        if (zoomX >= 2) {

            while (((Math.round(width / zoomX) - (width / zoomX)) != 0) ||
                       ((Math.round(width / zoomX / 2.0f) - (width / zoomX / 2.0f)) != 0)) {
                width++;
            }
        }

        height = width;

        xNew = (int) (((int) (xNew / (float) zoomX) * zoomX) + 0.5);
        yNew = (int) (((int) (yNew / (float) zoomY) * zoomY) + 0.5);

        int sIWidth = (int) (width / mag);
        int sIHeight = (int) (height / mag);

        xNewO = xNew - (int) (0.5f * width);
        yNewO = yNew - (int) (0.5f * height);

        int sX = (int) (xNew / zoomX);
        int sY = (int) (yNew / zoomY);

        if ((sX - (int) (sIWidth / 2)) < 0) {
            return;
        }

        if ((sY - (int) (sIHeight / 2)) < 0) {
            return;
        }

        // Draw zoomed portion of window
        x2 = sX - (int) (sIWidth / 2);
        x1 = xNewO;
        xw1 = width + xNewO;
        y2 = sY - (int) (sIHeight / 2);
        y1 = yNewO;
        yh1 = height + yNewO;

        // add code to build imageWindow
        graphics.drawImage(drawImage, x1, y1, xw1, yh1, x2, y2, sX + (int) (sIWidth / 2), sY + (int) (sIHeight / 2),
                           this);
    }



    /**
     * Sets the interpolation mode.
     *
     * @param  mode  the interpolation mode (i.e. SMOOTH, NEAREST)
     */
    public void setInterpolationMode(int mode) {
        interpMode = mode;
    }

    /**
     * Sets the resolution correction factor in both x and y directions.
     *
     * @param  rX  resolution correction factor in the x direction
     * @param  rY  resolution correction factor in the y direction
     */
    public void setResolutions(float rX, float rY) {
        resolutionX = rX;
        resolutionY = rY;
        setSize(Math.round(zoomX * imageDim.width * rX), Math.round(zoomY * imageDim.height * rY));
    }

    /**
     * Sets whether the slice number is shown.
     *
     * @param  flag  if true show slice number
     */
    public void setShowSliceNumber(boolean flag) {
        showSliceNumber = flag;
    }

    /**
     * Sets the string painted on the lower left.
     *
     * @param  str  str that is painted on the lower left of image
     */
    public void setSliceString(String str) {
        sliceString = str;
    }

    /**
     * Sets the text to the desired color.
     *
     * @param  color  color of text
     */
    public void setTextColor(Color color) {
        textColor = color;
    }

    /**
     * Sets the magnification in both x and y directions.
     *
     * @param  zX  zoom in the x direction
     * @param  zY  zoom in the y direction
     */
    public void setZoom(float zX, float zY) {
        zoomX = zX;
        zoomY = zY;
        setSize(Math.round(zoomX * imageDim.width * resolutionX), Math.round(zoomY * imageDim.height * resolutionY));
    }

    /**
     * Calls paint without erasing background - this reduces flicker!
     *
     * @param  g  Graphics handle
     */
    public void update(Graphics g) {
        paintComponent(g);
    }

    /**
     * Clean up some resources!
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
    }


    /**
     * This method creates a buffer that will be used to make an image of the
     * painted area on-screen.
     *
     * @param paintImageBuffer int[] the buffer to fill that will make the
     * paint image
     * @param paintBitmap the bit map representing the painted pixels
     * @param paintNumberMap short[] of numbers for different paints
     * @param slice the current slice to paint if this is a 3D image
     * @param frame the ViewJFrameBase containing the painted component.
     * @param b2D when true this is a 2D image component, when false it is
     * greater than 2D
     */
    protected void makePaintImage(int[] paintImageBuffer, BitSet paintBitmap, short[] paintNumberMap, 
                                  int slice, ViewJFrameBase frame,
                                  boolean b2D )
    {
        int colorArray[] = null;
        if ( paintBitmap.cardinality() <= 0 )
        {
            // Nothing to paint...
            return;
        }
        
        // get the color of the paint the user has selected
        int color = getSelectedPaintColor( frame );
        if (paintNumberMap != null) {
            colorArray = new int[37];
            colorArray[0] = color;
            // paintNumberMap[j] = 0 if paintNumberMap is not given a mask value.
            // paintNumberMap[j] could be 0 with bitMap set or not set.
            // To handle this case colorArray[0] has the original color.
            // paintNumberMap[j] >= 1 if bitMap set and paintNumberMap[j] is given a mask value.
            // 36 different colors in 1 thru 36 with selected color in 1.
            colorArray[1] = color;
            int index = 2;
            int colorNumber;
            int i = 0;
            while (index <= 36) {
                float hue = (float)(((i * 35) % 360)/ 360.0);
                i++;
                colorNumber = Color.getHSBColor(hue, 1.0f, 1.0f).getRGB();
                if (colorNumber != color) {
                    colorArray[index++] = colorNumber;  
                }
            }
            
        }
        float opacity = 0.3f;

        try {
            opacity = frame.getControls().getTools().getOpacity();
        } catch (Exception e) {
            /* do nothing, opacity defaults to 0.3f */
        } // should be changed later to a more elegant solution, since this always fails when 'frame' is a
          // ViewJFrameLightBox

        int opacityInt = (int) (opacity * 255);
        opacityInt = opacityInt << 24;

        // this loop converts the paint mask from a BitSet into an image
        // buffer that will be drawn on-screen
        
        if ( b2D )
        {
            // for 2D images
            for (int j = 0; j < paintBitmap.length(); j++) {

                if (paintBitmap.get(j) == true) {
                    if (paintNumberMap != null) {
                        color = colorArray[paintNumberMap[j]] & 0x00ffffff;
                    }
                    else {
                        color = color & 0x00ffffff;
                    }
                    paintImageBuffer[j] = color | opacityInt;
                }
            }
        } else if (slice >= 0) {

            // for 3D images
            int j = slice * imageDim.width * imageDim.height;
            int numPixelsInOneSlice = imageDim.width * imageDim.height;
            int offset = numPixelsInOneSlice * slice;
            int numIterations = numPixelsInOneSlice + offset;

            for (; j < numIterations; j++) {

                if (paintBitmap.get(j) == true) {
                    if (paintNumberMap != null) {
                        color = colorArray[paintNumberMap[j]] & 0x00ffffff;
                    }
                    else {
                        color = color & 0x00ffffff;
                    }
                    paintImageBuffer[j - offset] = color | opacityInt;
                }
            }
        }    
    }

    /**
     * This method adds an opaque border to the painted area on-screen.
     *
     * @param paintImageBuffer int[] the buffer the paint image will be
     * created from
     * @param paintBitmap the bit map representing the painted pixels
     * @param slice the current slice to paint if this is a 3D image
     * @param frame the ViewJFrameBase containing the painted component.
     */
    protected void makePaintBitmapBorder(int[] paintImageBuffer, BitSet paintBitmap,
                                         int slice, ViewJFrameBase frame )
    {
        int i, j;
        int idx;
        int xDim = imageDim.width;
        int yDim = imageDim.height;

        // Top row of image
        for (i = 0; i < (xDim - 1); i++) {

            if (paintImageBuffer[i] != 0) {
                paintImageBuffer[i] = paintImageBuffer[i] | 0xff000000;
            }
        }

        // Bottom row of image
        for (i = (yDim - 1) * xDim; i < (xDim * yDim); i++) {

            if (paintImageBuffer[i] != 0) {
                paintImageBuffer[i] = paintImageBuffer[i] | 0xff000000;
            }
        }

        // Left column of image
        for (i = 0; i < (xDim * yDim); i += xDim) {

            if (paintImageBuffer[i] != 0) {
                paintImageBuffer[i] = paintImageBuffer[i] | 0xff000000;
            }
        }

        // Right column of image
        for (i = (xDim - 1); i < (xDim * yDim); i += xDim) {

            if (paintImageBuffer[i] != 0) {
                paintImageBuffer[i] = paintImageBuffer[i] | 0xff000000;
            }
        }

        for (j = 1; j < (imageDim.height - 1); j++) {

            for (i = 1; i < (imageDim.width - 1); i++) {

                idx = (j * imageDim.width) + i;

                int left = idx - 1;
                int right = idx + 1;
                int above = idx - imageDim.width*1;
                int below = idx + imageDim.width*1;
                int northwest = above - 1;
                int northeast = above + 1;
                int southeast = below + 1;
                int southwest = below - 1;

                if (paintImageBuffer[idx] != 0) {

                    if (paintImageBuffer[above] == 0) {
                        paintImageBuffer[idx] = paintImageBuffer[idx] | 0xff000000;

                        continue;
                    }

                    if (paintImageBuffer[below] == 0) {
                        paintImageBuffer[idx] = paintImageBuffer[idx] | 0xff000000;

                        continue;
                    }

                    if (paintImageBuffer[left] == 0) {
                        paintImageBuffer[idx] = paintImageBuffer[idx] | 0xff000000;

                        continue;
                    }

                    if (paintImageBuffer[right] == 0) {
                        paintImageBuffer[idx] = paintImageBuffer[idx] | 0xff000000;

                        continue;
                    }

                    if (paintImageBuffer[northwest] == 0) {
                        paintImageBuffer[idx] = paintImageBuffer[idx] | 0xff000000;

                        continue;
                    }

                    if (paintImageBuffer[northeast] == 0) {
                        paintImageBuffer[idx] = paintImageBuffer[idx] | 0xff000000;

                        continue;
                    }

                    if (paintImageBuffer[southeast] == 0) {
                        paintImageBuffer[idx] = paintImageBuffer[idx] | 0xff000000;

                        continue;
                    }

                    if (paintImageBuffer[southwest] == 0) {
                        paintImageBuffer[idx] = paintImageBuffer[idx] | 0xff000000;

                        continue;
                    }
                }
            }
        }
    }

    /**
     * Gets the color of the paint the user has selected from the paint
     * toolbar.
     *
     * @param vjfb the ViewJFrameBase object that contains the paint toolbar.
     * @return int the color of the paint selected by the user, represented as
     * a packed integer
     */
    public int getSelectedPaintColor( ViewJFrameBase vjfb )
    {
        try {
            ViewControlsImage vci = vjfb.getControls();
            ViewToolBarBuilder vtbb = vci.getTools();
            Color rgbColorObj = vtbb.getPaintColor();

            return rgbColorObj.getRGB();
        } catch (Throwable t) {
            return 0xffff0000; // default to red if exception is thrown
        }
    }

    
}
