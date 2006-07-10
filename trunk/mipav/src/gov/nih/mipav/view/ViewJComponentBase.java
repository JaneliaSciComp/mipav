package gov.nih.mipav.view;


import gov.nih.mipav.*;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.awt.*;
import java.awt.image.*;

import java.text.*;

import javax.swing.*;
import javax.swing.event.*;


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
    public static final int NEAREST = 0;

    /** DOCUMENT ME! */
    public static final int BILINEAR = 1;

    /** DOCUMENT ME! */
    public static final int SMOOTH = 2;

    /** DOCUMENT ME! */
    public static final int INTERPOLATE_A = 1;

    /** DOCUMENT ME! */
    public static final int INTERPOLATE_B = 2;

    /** DOCUMENT ME! */
    public static final int INTERPOLATE_BOTH = 3;

    /** DOCUMENT ME! */
    public static final int NEAREST_BOTH = 6;

    /** MipavCoordinateSystems upgrade: TODO: refactor AXIAL, SAGITTAL,
     * CORONAL constant defintions into one place so that all refs are
     * consistent: */
    /** DOCUMENT ME! */
    //public static final int AXIAL = 0;
    //FileInfoBase.AXIAL = 0
    public static final int AXIAL = FileInfoBase.AXIAL;

    /** DOCUMENT ME! */
    //public static final int SAGITTAL = 1;
    //FileInfoBase.SAGITTAL = 2
    public static final int SAGITTAL = FileInfoBase.SAGITTAL;

    /** DOCUMENT ME! */
    //public static final int CORONAL = 2;
    //FileInfoBase.CORONAL = 1
    public static final int CORONAL = FileInfoBase.CORONAL;

    /** DOCUMENT ME! */
    //public static final int NA = 3;
    public static final int NA = FileInfoBase.UNKNOWN_ORIENT;

    /** DOCUMENT ME! */
    public static final int IMAGE_A = 0;

    /** DOCUMENT ME! */
    public static final int IMAGE_B = 1;

    /** DOCUMENT ME! */
    public static final int BOTH = 2;

    /** Used to describe cursor mode. */
    public static final int DEFAULT = 0;

    /** DOCUMENT ME! */
    public static final int SELECT = 1;

    /** DOCUMENT ME! */
    public static final int MOVE = 2;

    /** DOCUMENT ME! */
    public static final int RECTANGLE = 3;

    /** DOCUMENT ME! */
    public static final int RECTANGLE3D = 4;

    /** DOCUMENT ME! */
    public static final int LEVELSET = 6;

    /** DOCUMENT ME! */
    public static final int MOVE_POINT = 7;

    /** DOCUMENT ME! */
    public static final int NEW_POINT = 8;

    /** DOCUMENT ME! */
    public static final int DELETE_POINT = 9;

    /** DOCUMENT ME! */
    public static final int WAND = 10;

    /** DOCUMENT ME! */
    public static final int ELLIPSE = 11;

    /** DOCUMENT ME! */
    public static final int LINE = 12;

    /** DOCUMENT ME! */
    public static final int POLYLINE = 13;

    /** DOCUMENT ME! */
    public static final int NEW_VOI = 14;

    /** DOCUMENT ME! */
    public static final int RETRACE = 15;

    /** DOCUMENT ME! */
    public static final int POINT_VOI = 16;

    /** DOCUMENT ME! */
    public static final int PAINT_VOI = 17;

    /** DOCUMENT ME! */
    public static final int PAINT_CAN = 18;

    /** DOCUMENT ME! */
    public static final int DROPPER_PAINT = 19;

    /** DOCUMENT ME! */
    public static final int ERASER_PAINT = 20;

    /** DOCUMENT ME! */
    public static final int MAG_REGION = 21;

    /** DOCUMENT ME! */
    public static final int WIN_REGION = 22;

    /** DOCUMENT ME! */
    public static final int QUICK_LUT = 23;

    /** DOCUMENT ME! */
    public static final int PROTRACTOR = 24;

    /** DOCUMENT ME! */
    public static final int ROTATE = 25;

    /** DOCUMENT ME! */
    public static final int TRANSLATE = 26;

    /** DOCUMENT ME! */
    public static final int MOVE_VOIPOINT = 27;

    /** DOCUMENT ME! */
    public static final int CENTER_VOI = 28;

    /** DOCUMENT ME! */
    public static final int CUBE_BOUNDS = 29;

    /** DOCUMENT ME! */
    public static final int LIVEWIRE = 30;

    /** DOCUMENT ME! */
    public static final int PAINT_VASC = 31;

    /** DOCUMENT ME! */
    public static final int ANNOTATION = 32;

    /** DOCUMENT ME! */
    public static final int PROBE = 33;

    /** DOCUMENT ME! */
    public static final int MOVE_INTERSECTION_POINT = 34;

    /** DOCUMENT ME! */
    public static final int ZOOMING_IN = 35;

    /** DOCUMENT ME! */
    public static final int ZOOMING_OUT = 36;

    /** DOCUMENT ME! */
    public static final int POLYLINE_SLICE_VOI = 37;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected Dimension imageDim = null;

    /** DOCUMENT ME! */
    protected Image img; // always magnification of 1;

    /** DOCUMENT ME! */
    protected Image imgB;

    /** DOCUMENT ME! */
    protected int interpMode = NEAREST;

    /** DOCUMENT ME! */
    protected MemoryImageSource memImage = null;

    /** DOCUMENT ME! */
    protected MemoryImageSource memImageB = null;

    /**
     * resolutionX and Y are used to correct difference in intra and inter plane voxel resolution These represent the
     * aspect ratio of the image These are NOT to be confused with the pixel resolutions.
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
    private short[] imDataB;

    /** DOCUMENT ME! */
    private short[] imDataG;

    /** DOCUMENT ME! */
    private short[] imDataR;

    /** DOCUMENT ME! */
    private int[] imgData; // here for posterity...these arrays are used in the smooth image method, which we may or
                           // may not use in the future

    /** DOCUMENT ME! */
    private int[] imgDataB;

    /** DOCUMENT ME! */
    private int xOld, yOld;

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
     * @param orientation the Patient-Coordinates orientation from which to
     * view the data
     * @param _imageA, the image that this ViewJComponentBase is a view of
     */
    public ViewJComponentBase( int orientation, ModelImage _imageA ) {
        img = null;
        /* MipavCoordinateSystems upgrade TODO: */
        imageDim = new Dimension( _imageA.getWidth( orientation ),
                                  _imageA.getHeight( orientation ) );
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
        memImage = null;
        memImageB = null;
        textColor = null;
        sliceString = null;
        imgData = null;
        imgDataB = null;
        imDataR = imDataG = imDataB = null;
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
        return xOld;
    }

    /**
     * Get the y position of the last place we drew something from a mouse event.
     *
     * @return  the y coordinate of the last mouse event
     */
    public int getLastMouseY() {
        return yOld;
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
                  (imageType == ModelImage.COMPLEX) || (imageType == ModelImage.ARGB) ||
                  (imageType == ModelImage.ARGB_USHORT)) &&
                 ((maxStrWidth < (xwidth - 1 - (2 * maxCharWidth))) && (fontHeight < (yheight - 1)))) ||
                (((imageType != ModelImage.FLOAT) && (imageType != ModelImage.DOUBLE) &&
                      (imageType != ModelImage.COMPLEX) && (imageType != ModelImage.ARGB) &&
                      (imageType != ModelImage.ARGB_USHORT)) &&
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

            if (memImage == null) {
                memImage = new MemoryImageSource(imageDim.width, imageDim.height, data, 0, imageDim.width);
                img = createImage(memImage);
            } else {
                memImage.newPixels(data, ColorModel.getRGBdefault(), 0, imageDim.width);
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

        if ((xNew == xOld) && (yNew == yOld)) {
            return;
        }

        if ((xOld == 0) && (yOld == 0)) {
            xOld = xNew;
            yOld = yNew;

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

        xOld = xNew0;
        yOld = yNew0;
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
     * smoothImage - here for posterity. the smoothing is now done by the graphics hardware in ViewJComponentEditImage's
     * paintComponent method
     *
     * @param  inData  DOCUMENT ME!
     */
    private void smoothImage(int[] inData) {

        int i, j, index;
        int rgb;
        int xDim = imageDim.width;
        int yDim = imageDim.height;
        int xDim1 = imageDim.width - 1;
        int yDim1 = imageDim.height - 1;
        int zoomXDim = Math.round(xDim * zoomX * resolutionX);
        int zoomYDim = Math.round(yDim * zoomY * resolutionY);
        int imgLength = xDim * yDim;
        float xfNew, yfNew;
        int intX, intY;
        float x1, x2;
        float dx, dy, dy1, diff;
        int position, position1, positionXDim, positionXDimp1;
        int red, green, blue;

        if (inData == null) {
            return;
        }

        try {

            if ((imgData == null) || (imDataR == null) || (imDataG == null) || (imDataB == null) ||
                    ((Math.round(zoomX * imageDim.width * resolutionX) *
                          Math.round(zoomY * imageDim.height * resolutionY)) != imgData.length)) {
                imgData = null;
                imDataR = imDataG = imDataB = null;
                System.gc();
                imgData = new int[Math.round(zoomX * imageDim.width * resolutionX) *
                              Math.round(zoomY * imageDim.height * resolutionY)];
                imDataR = new short[imageDim.width * imageDim.height];
                imDataG = new short[imageDim.width * imageDim.height];
                imDataB = new short[imageDim.width * imageDim.height];
            }
        } catch (OutOfMemoryError error) {

            // add error message
            return;
        }

        for (i = 0; i < imgLength; i++) {
            rgb = inData[i];
            imDataR[i] = (short) ((rgb >> 16) & 0xff);
            imDataG[i] = (short) ((rgb >> 8) & 0xff);
            imDataB[i] = (short) (rgb & 0xff);
        }

        int rP = 0, gP = 0, bP = 0;

        if (this instanceof ViewJComponentEditImage) {
            rP = ((ViewJComponentEditImage) this).frame.getControls().getTools().getPaintColor().getRed();
            gP = ((ViewJComponentEditImage) this).frame.getControls().getTools().getPaintColor().getGreen();
            bP = ((ViewJComponentEditImage) this).frame.getControls().getTools().getPaintColor().getBlue();
        }

        int endX = zoomXDim;
        int endY = zoomYDim;
        float scaleX = 1 / (zoomX * resolutionX);
        float scaleY = 1 / (zoomY * resolutionY);
        int offset = Math.round((zoomY * resolutionY * zoomXDim * 0.5f) + (zoomX * resolutionX * 0.5f));

        if (((zoomX * resolutionX) != 1) || ((zoomY * resolutionY) != 1)) {

            for (j = 0; j < endY; j++) {
                index = (j * zoomXDim) + offset;
                yfNew = j * scaleY;

                for (i = 0; i < endX; i++) {
                    xfNew = i * scaleX;

                    if ((xfNew < xDim1) && (yfNew < yDim1)) {
                        intX = (int) xfNew;
                        intY = (int) yfNew;

                        dx = xfNew - intX;
                        dy = yfNew - intY;
                        dy1 = 1 - dy;
                        diff = 1 - dx;
                        position = (intY * xDim) + intX;
                        position1 = position + 1;
                        positionXDim = position + xDim;
                        positionXDimp1 = positionXDim + 1;

                        x1 = (diff * imDataR[position]) + (dx * imDataR[position1]);
                        x2 = (diff * imDataR[positionXDim]) + (dx * imDataR[positionXDimp1]);

                        red = (int) ((dy1 * x1) + (dy * x2));

                        x1 = (diff * imDataG[position]) + (dx * imDataG[position1]);
                        x2 = (diff * imDataG[positionXDim]) + (dx * imDataG[positionXDimp1]);

                        green = (int) ((dy1 * x1) + (dy * x2));

                        x1 = (diff * imDataB[position]) + (dx * imDataB[position1]);
                        x2 = (diff * imDataB[positionXDim]) + (dx * imDataB[positionXDimp1]);

                        blue = (int) ((dy1 * x1) + (dy * x2));
                        imgData[index] = (0xff000000) | (red << 16) | (green << 8) | blue;

                        if ((imDataR[position] == rP) && (imDataG[position] == gP) && (imDataB[position] == bP)) {
                            red = rP;
                            green = gP;
                            blue = bP;
                            imgData[index - offset] = (0xff000000) | (red << 16) | (green << 8) | blue;
                        }
                    }

                    index++;
                }
            }
        } else {

            for (i = 0; i < imgLength; i++) {
                imgData[i] = inData[i];
            }
        }

        if ((memImage == null) || (img == null) || ((img.getWidth(null) * img.getHeight(null)) != imgData.length)) {
            memImage = new MemoryImageSource(zoomXDim, zoomYDim, imgData, 0, zoomXDim);
            img = createImage(memImage);
        } else {
            memImage.newPixels(imgData, ColorModel.getRGBdefault(), 0,
                               Math.round(imageDim.width * zoomX * resolutionX));
            img.flush();
        }

    }

    /**
     * smoothImage - here for posterity. the smoothing is now done by the graphics hardware in ViewJComponentEditImage's
     * paintComponent method
     *
     * @param  inData  DOCUMENT ME!
     */
    private void smoothImageB(int[] inData) {
        int i, j, index;
        int rgb;
        int xDim = imageDim.width;
        int yDim = imageDim.height;
        int xDim1 = imageDim.width - 1;
        int yDim1 = imageDim.height - 1;
        int zoomXDim = Math.round(xDim * zoomX * resolutionX);
        int zoomYDim = Math.round(yDim * zoomY * resolutionY);
        int imgLength = xDim * yDim;
        float xfNew, yfNew;
        int intX, intY;
        float x1, x2;
        float dx, dy, dy1, diff;
        int position, position1, positionXDim, positionXDimp1;
        int red, green, blue;

        if (inData == null) {
            return;
        }

        try {

            if ((imgDataB == null) || (imDataR == null) || (imDataG == null) || (imDataB == null) ||
                    ((Math.round(zoomX * imageDim.width * resolutionX) *
                          Math.round(zoomY * imageDim.height * resolutionY)) != imgDataB.length)) {

                imgDataB = null;
                imDataR = imDataG = imDataB = null;
                System.gc();
                imgDataB = new int[Math.round(zoomX * imageDim.width * resolutionX) *
                               Math.round(zoomY * imageDim.height * resolutionY)];
                imDataR = new short[imageDim.width * imageDim.height];
                imDataG = new short[imageDim.width * imageDim.height];
                imDataB = new short[imageDim.width * imageDim.height];
            }
        } catch (OutOfMemoryError error) {

            // add error message
            return;
        }

        for (i = 0; i < imgLength; i++) {
            rgb = inData[i];
            imDataR[i] = (short) ((rgb >> 16) & 0xff);
            imDataG[i] = (short) ((rgb >> 8) & 0xff);
            imDataB[i] = (short) (rgb & 0xff);
        }

        int rP = 0, gP = 0, bP = 0;

        if (this instanceof ViewJComponentEditImage) {
            rP = ((ViewJComponentEditImage) this).frame.getControls().getTools().getPaintColor().getRed();
            gP = ((ViewJComponentEditImage) this).frame.getControls().getTools().getPaintColor().getGreen();
            bP = ((ViewJComponentEditImage) this).frame.getControls().getTools().getPaintColor().getBlue();
        }

        int endX = zoomXDim;
        int endY = zoomYDim;
        float scaleX = 1 / (zoomX * resolutionX);
        float scaleY = 1 / (zoomY * resolutionY);
        int offset = Math.round((zoomY * resolutionY * zoomXDim * 0.5f) + (zoomX * resolutionX * 0.5f));

        if (((zoomX * resolutionX) != 1) || ((zoomY * resolutionY) != 1)) {

            for (j = 0; j < endY; j++) {
                index = (j * zoomXDim) + offset;
                yfNew = j * scaleY;

                for (i = 0; i < endX; i++) {
                    xfNew = i * scaleX;

                    if ((xfNew < xDim1) && (yfNew < yDim1)) {
                        intX = (int) xfNew;
                        intY = (int) yfNew;

                        dx = xfNew - intX;
                        dy = yfNew - intY;
                        dy1 = 1 - dy;
                        diff = 1 - dx;
                        position = (intY * xDim) + intX;
                        position1 = position + 1;
                        positionXDim = position + xDim;
                        positionXDimp1 = positionXDim + 1;

                        x1 = (diff * imDataR[position]) + (dx * imDataR[position1]);
                        x2 = (diff * imDataR[positionXDim]) + (dx * imDataR[positionXDimp1]);

                        red = (int) ((dy1 * x1) + (dy * x2));

                        x1 = (diff * imDataG[position]) + (dx * imDataG[position1]);
                        x2 = (diff * imDataG[positionXDim]) + (dx * imDataG[positionXDimp1]);

                        green = (int) ((dy1 * x1) + (dy * x2));

                        x1 = (diff * imDataB[position]) + (dx * imDataB[position1]);
                        x2 = (diff * imDataB[positionXDim]) + (dx * imDataB[positionXDimp1]);

                        blue = (int) ((dy1 * x1) + (dy * x2));

                        imgDataB[index] = (0xff000000) | (red << 16) | (green << 8) | blue;

                        if ((imDataR[position] == rP) && (imDataG[position] == gP) && (imDataB[position] == bP)) {
                            red = rP;
                            green = gP;
                            blue = bP;
                            imgDataB[index - offset] = (0xff000000) | (red << 16) | (green << 8) | blue;
                        }
                    }

                    index++;
                }
            }
        } else {

            for (i = 0; i < imgLength; i++) {
                imgDataB[i] = inData[i];
            }
        }

        if ((memImageB == null) || (imgB == null) || ((img.getWidth(null) * img.getHeight(null)) != imgDataB.length)) {
            memImageB = new MemoryImageSource(zoomXDim, zoomYDim, imgDataB, 0, zoomXDim);
            imgB = createImage(memImageB);
        } else {
            memImageB.newPixels(imgDataB, ColorModel.getRGBdefault(), 0,
                                Math.round(imageDim.width * zoomX * resolutionX));
            imgB.flush();
        }
    }
}
