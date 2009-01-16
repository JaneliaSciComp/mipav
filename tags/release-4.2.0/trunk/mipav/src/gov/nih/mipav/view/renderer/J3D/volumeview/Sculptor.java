package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;

import javax.media.j3d.*;

import javax.swing.*;


/**
 * Volume Sculpturing allows the user to draw a region on the screen in the volume render view, and to remove the parts
 * of the volume covered by that region. The user outlines a region by holding the left mouse button down and drawing
 * directly on the screen. Volume Sculpturing works in both the Raycast and Shearwarp volume render views through the
 * VolumeSculptor.java implementation and on the VolumeTexture render views through the TextureSculptor.java
 * implemetation.
 *
 * <p>The user draws an outline on the screen, and defines the cut-away or sculpture region. If the user chooses to
 * apply the sculpture region to the volume, the voxels defined by an extruded volume of the cut-away region are
 * removed. This is done by projecting the voxels in the volume into screen space and determining which voxels fall
 * inside the sculpture region. Voxels that are inside the sculpture region are set to the minimum voxel value, which is
 * rendered as transparent. The user may also undo volume sculpturing, returning the volume data to its original values.
 * </p>
 *
 * <p>Drawing the Sculpt Region on the Screen</p>
 *
 * <p>The VolumeSculptor class implements both MouseListener and MouseMotionListener interfaces. Drawing is done through
 * the mousePressed, mouseReleased, and mouseDragged functions.</p>
 *
 * <p>When sculpting is enabled, the viewing transformations (rotation, scale, translation) are disabled, so the left
 * mouse button can be used for drawing on the screen instead of rotating the volume. Second, the view canvas is
 * captured and stored. When the left mouse button is pressed, the VolumeSculptor class captures and stores all
 * subsequent mouse positions, and draws them directly into the canvas image, connecting the mouse positions by drawing
 * 2D lines on the image. On a mouseReleased event the outline is closed by connecting the first and last recorded mouse
 * positions and the outline region is filled in. The line drawing and polygon filling functions are implemented as 2D
 * scan-conversion algorithms and are drawn directly by the VolumeSculptor onto the canvas image. This implementation
 * allows concave and self-intersecting outlines.</p>
 *
 * <p>Multiple outlines may be drawn on the canvas image before applying the sculpt regions to the volume. Because the
 * sculpt regions are in image space, to apply the sculpt region to the volume, it is simply necessary to project the
 * volume voxels onto the canvas image. Voxels that project onto blue pixels in the canvas image are removed, and voxels
 * that project to non-masked pixels are unaltered.</p>
 *
 * <p>Removing Sculpted Voxels</p>
 *
 * <p>The process of applying the sculpt region to the volume data and removing the voxels that fall inside the sculpt
 * region is broken down into three steps. First, the ModelImage data is retrieved and the center and spacing of the
 * data is determined. Second, the viewing transformations are calculated. Third, the voxels are mapped into screen
 * space and the location of the voxel is compared to the corresponding pixel. If the voxel maps to a pixel inside the
 * sculpt region its value is set to the minimum voxel value.</p>
 *
 * @author  Alexandra Bokinsky, Ph.D. Under contract from Magic Software.
 * @see     ViewJFrameVolumeView
 * @see     RayCastVolumeRenderer
 * @see     ShearWarpVolumeRenderer
 */
public abstract class Sculptor implements MouseMotionListener, MouseListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static int LINES = 0;

    /** DOCUMENT ME! */
    public static int RECTANGLE = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected double dAMinAlpha = Float.MAX_VALUE;

    /** DOCUMENT ME! */
    protected double dAMinBlue = Float.MAX_VALUE;

    /** DOCUMENT ME! */
    protected double dAMinGreen = Float.MAX_VALUE;

    /** DOCUMENT ME! */
    protected double dAMinRed = Float.MAX_VALUE;

    /** DOCUMENT ME! */
    protected double dBMinAlpha = Float.MAX_VALUE;

    /** DOCUMENT ME! */
    protected double dBMinBlue = Float.MAX_VALUE;

    /** DOCUMENT ME! */
    protected double dBMinGreen = Float.MAX_VALUE;

    /** DOCUMENT ME! */
    protected double dBMinRed = Float.MAX_VALUE;

    /** DOCUMENT ME! */
    protected float fImageBMin = Float.MAX_VALUE;

    /** Min and Max image intensity and color values:. */
    protected float fImageMin = Float.MAX_VALUE;

    /** Backup of the data for undo:. */
    protected int[] m_aiImageA_backup;

    /** DOCUMENT ME! */
    protected int[] m_aiImageB_backup;

    /** Mouse positions in x,y for drawing the sculpt region:. */
    protected int[] m_aiXPoints;

    /** DOCUMENT ME! */
    protected int[] m_aiYPoints;

    /** m_bMousePressed is set to true when the mousePressed callback is activated and the left mouse button is down. */
    protected boolean m_bMousePressed;

    /** DOCUMENT ME! */
    protected boolean m_bSculptDrawn = false;

    /** m_bSculptEnabled is turned on when the user presses the "Draw Sculpt Outline" button. */
    protected boolean m_bSculptEnabled = false;

    /** The sculpt region color. */
    protected int m_iColorSculpt = 0xff0000ff;

    /** DOCUMENT ME! */
    protected int m_iFirstX;

    /** DOCUMENT ME! */
    protected int m_iFirstY;

    /** DOCUMENT ME! */
    protected int m_iLastX;

    /** DOCUMENT ME! */
    protected int m_iLastY;

    /** DOCUMENT ME! */
    protected int m_iNumberPoints;

    /** DOCUMENT ME! */
    protected int m_iOriginalHeight;

    /** The original canvas size. This is used if the ViewJFrameVolumeView window is resized by the user. */
    protected int m_iOriginalWidth;

    /** Previous and first mouse positions:. */
    protected int m_iPreviousX;

    /** DOCUMENT ME! */
    protected int m_iPreviousY;

    /** DOCUMENT ME! */
    protected int m_iSculptImageHeight;

    /** Size of the m_kSculptImage, when it is captured. */
    protected int m_iSculptImageWidth;

    /** DOCUMENT ME! */
    protected int m_iXMax;

    /** The min and max x,y mouse values:. */
    protected int m_iXMin;

    /** DOCUMENT ME! */
    protected int m_iYMax;

    /** DOCUMENT ME! */
    protected int m_iYMin;

    /** Canvas3D reference for drawing :. */
    protected Canvas3D m_kCanvas3D;

    /** Green Progress bar at the top right hand corner of the ViewJFrameVolumeView window. */
    protected JProgressBar m_kProgress;

    /** DOCUMENT ME! */
    protected BufferedImage m_kSavedImage;

    /**
     * m_kSculptImage is a screen shot of the canvas image. The sculpt region is drawn directly into m_kSculptImage. The
     * outline is filled by alpha-blending the Sculpt region color with the m_kSculptImage backgound image.
     * m_kSculptImageOpaque is filled with solid color, but not displayed, it is used to test whether voxels are inside
     * the sculpt region.
     */
    protected BufferedImage m_kSculptImage;

    /** DOCUMENT ME! */
    protected BufferedImage m_kSculptImageOpaque;

    /** Shape of the drawing rectangle. */
    private int drawShape = LINES;

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * applySculpt: abstract function, implementation depends on whether the instance is VolumeTextureSculptor or
     * VolumeSculptor.
     *
     * @return  DOCUMENT ME!
     */
    public abstract boolean applySculpt();

    /**
     * undoSculpt: abstract function, implementation depends on whether the instance is VolumeTextureSculptor or
     * VolumeSculptor.
     */
    public abstract void undoSculpt();

    /**
     * DOCUMENT ME!
     */
    public abstract void update();

    /**
     * clearSculpt: called by ViewJFrameVolumeView when the user presses the "Clear Ouline" button, clearing the sculpt
     * outline from the canvas image. The function disables sculpting and reactivates the mouse events for the
     * m_kVolumeRenderer.
     */
    public void clearSculpt() {
        m_bSculptEnabled = false;
        m_bSculptDrawn = false;
        m_iNumberPoints = 0;
    }

    /**
     * Sets all variables to null, disposes, and garbage collects.
     *
     * @param  flag  DOCUMENT ME!
     */
    public void disposeLocal(boolean flag) {

        if (m_kSculptImage != null) {
            m_kSculptImage.flush();
            m_kSculptImage = null;
        }

        if (m_kSculptImageOpaque != null) {
            m_kSculptImageOpaque.flush();
            m_kSculptImageOpaque = null;
        }

        if (m_kSavedImage != null) {
            m_kSavedImage.flush();
            m_kSavedImage = null;
        }

        m_aiImageA_backup = null;
        m_aiImageB_backup = null;

        m_aiXPoints = null;
        m_aiYPoints = null;
    }

    /**
     * enableSculpt: called by the ViewJFrameVolumeView object when the Draw Sculpt button is pressed. This function
     * deactivates the m_kVolumeRenderer's mouse response, so the mouse can be used to draw the sculpt outline. It also
     * allocates and initializes the m_iSculptImage buffer for drawing.
     *
     * @param  bEnabled  DOCUMENT ME!
     */
    public void enableSculpt(boolean bEnabled) {
        m_bSculptEnabled = bEnabled;

        if (m_bSculptEnabled == false) {
            return;
        }

        m_iNumberPoints = 0;

        /* Get the current size of the canvas */
        m_iSculptImageWidth = m_kCanvas3D.getWidth();
        m_iSculptImageHeight = m_kCanvas3D.getHeight();

        if ((m_iSculptImageWidth <= 0) || (m_iSculptImageHeight <= 0)) {
            return; // Dual Panel mode, both are enabled, but only one panel
                    // is visible
        }

        /* Allocate the m_kSculptImage, and m_kSculptImageOpaque and read the
         * canvas image into the buffers.
         */
        m_kSculptImage = null;
        m_kSculptImage = new BufferedImage(m_iSculptImageWidth, m_iSculptImageHeight, BufferedImage.TYPE_INT_ARGB);

        m_kSculptImageOpaque = null;
        m_kSculptImageOpaque = new BufferedImage(m_iSculptImageWidth, m_iSculptImageHeight,
                                                 BufferedImage.TYPE_INT_ARGB);
        m_kSavedImage = null;
        m_kSavedImage = new BufferedImage(m_iSculptImageWidth, m_iSculptImageHeight, BufferedImage.TYPE_INT_ARGB);
        getFrameBuffer();
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean getEnable() {
        return m_bSculptEnabled;
    }

    /**
     * Initialize the Mouse events, store the progress bar, and get the original canvas widths. Disable sculpting and
     * drawing with the mouse. Note - this function should be called once per instance only, as it sets up the
     * MouseMotionListener and MouseListener.
     *
     * @param  iSculptWidth   DOCUMENT ME!
     * @param  iSculptHeight  DOCUMENT ME!
     */
    public void initVolumeSculptor(int iSculptWidth, int iSculptHeight) {

        m_kCanvas3D.addMouseMotionListener(this);
        m_kCanvas3D.addMouseListener(this);

        m_kProgress = ViewJFrameVolumeView.getRendererProgressBar();

        m_iOriginalWidth = iSculptWidth;
        m_iOriginalHeight = iSculptHeight;

        m_bSculptEnabled = false;
        m_bMousePressed = false;
    }

    /**
     * invertSculpt: called by ViewJFrameVolumeView when the user presses the "Invert Sculpt Region" button, inverting
     * the sculpt region.
     */
    public void invertSculpt() {

        if (m_bSculptDrawn == false) {
            return;
        }

        int iImageColor = 0;
        int iNewColor = 0;

        /* Loop over the pixels in the sculpt buffer: */
        for (int iX = 0; iX < m_iSculptImageWidth; iX++) {

            for (int iY = 0; iY < m_iSculptImageHeight; iY++) {

                /* If the pixel is inside the sculpt region, then change it to
                 * the saved image color: */
                if (m_kSculptImageOpaque.getRGB(iX, iY) == m_iColorSculpt) {
                    iImageColor = m_kSavedImage.getRGB(iX, iY);
                    m_kSculptImageOpaque.setRGB(iX, iY, iImageColor);
                    m_kSculptImage.setRGB(iX, iY, iImageColor);
                } else {

                    /* If the pixel is outside the sculpt, use the original
                     * saved image to blend in the sculpt color for the SculptImage, and use the m_iColorSculpt for the
                     * Opaque
                     * image: */
                    iImageColor = m_kSavedImage.getRGB(iX, iY);
                    iNewColor = blendColor(iImageColor, m_iColorSculpt);
                    m_kSculptImage.setRGB(iX, iY, iNewColor);
                    m_kSculptImageOpaque.setRGB(iX, iY, m_iColorSculpt);
                }
            }
        }

        /* Draw update image: */
        int iCanvasHeight = m_kCanvas3D.getHeight();
        int iCanvasWidth = m_kCanvas3D.getWidth();
        Graphics kGraphics = m_kCanvas3D.getGraphics();
        kGraphics.drawImage(m_kSculptImage, 0, 0, m_iSculptImageWidth, m_iSculptImageHeight, 0, 0, iCanvasWidth,
                            iCanvasHeight, new Color(0, 0, 0, 0), m_kCanvas3D);
    }

    /**
     * One of the overrides necessary to be a MouseListener. This function is invoked when a button has been pressed and
     * released.
     *
     * @param  kEvent  the mouse event generated by a mouse clicked
     */
    public void mouseClicked(MouseEvent kEvent) { /* stub */
    }


    /**
     * Invoked when the mouse is dragged while a button is held down. If this occurs while sculpting is enabled, then
     * the mouse positions are stored and the sculpt outline is drawn on the canvas, using the previously recorded mouse
     * position and the current mouse position as the start and end points of a 2D line on the canvas.
     *
     * @param  kEvent  the mouse event generated by a mouse drag
     */
    public void mouseDragged(MouseEvent kEvent) {

        if (m_bSculptEnabled && m_bMousePressed) {
            int iX = kEvent.getX();
            int iY = kEvent.getY();

            if ((iX >= 0) && (iX < m_iSculptImageWidth) && (iY >= 0) && (iY < m_iSculptImageHeight)) {

                if (drawShape == LINES) {

                    /* If the mouse position isn't the same as the last mouse
                     * position, then add it to the point list, draw a line from
                     * the last point to this point, and update min,max. */
                    if ((iX != m_iPreviousX) || (iY != m_iPreviousY)) {
                        m_aiXPoints[m_iNumberPoints] = iX;
                        m_aiYPoints[m_iNumberPoints] = iY;
                        m_iNumberPoints++;
                        line(iX, iY, m_iPreviousX, m_iPreviousY);
                    }

                    m_iPreviousX = iX;
                    m_iPreviousY = iY;

                    if (iX < m_iXMin) {

                        m_iXMin = iX;
                    }

                    if (iX > m_iXMax) {

                        m_iXMax = iX;
                    }

                    if (iY < m_iYMin) {

                        m_iYMin = iY;
                    }

                    if (iY > m_iYMax) {

                        m_iYMax = iY;
                    }

                }

                /* Draw update image: */
                int iCanvasHeight = m_kCanvas3D.getHeight();
                int iCanvasWidth = m_kCanvas3D.getWidth();
                Graphics kGraphics = m_kCanvas3D.getGraphics();

                kGraphics.drawImage(m_kSculptImage, 0, 0, m_iSculptImageWidth, m_iSculptImageHeight, 0, 0, iCanvasWidth,
                                    iCanvasHeight, new Color(0, 0, 0, 0), m_kCanvas3D);

                if (drawShape == RECTANGLE) {

                    if (m_iFirstX < iX) {
                        m_iXMin = m_iFirstX;
                    } else {
                        m_iXMin = iX;
                    }

                    if (m_iFirstX > iX) {
                        m_iXMax = m_iFirstX;
                    } else {
                        m_iXMax = iX;
                    }

                    if (m_iFirstY < iY) {
                        m_iYMin = m_iFirstY;
                    } else {
                        m_iYMin = iY;
                    }

                    if (m_iFirstY > iY) {
                        m_iYMax = m_iFirstY;
                    } else {
                        m_iYMax = iY;
                    }

                    kGraphics.setColor(Color.cyan);
                    kGraphics.drawLine(m_iXMin, m_iYMax, m_iXMax, m_iYMax);
                    kGraphics.drawLine(m_iXMax, m_iYMax, m_iXMax, m_iYMin);
                    kGraphics.drawLine(m_iXMax, m_iYMin, m_iXMin, m_iYMin);
                    kGraphics.drawLine(m_iXMin, m_iYMin, m_iXMin, m_iYMax);
                }
            }
        }
    }

    /**
     * One of the overrides necessary to be a MouseListener. Invoked when the mouse enters a component.
     *
     * @param  kEvent  the mouse event generated by a mouse entered
     */
    public void mouseEntered(MouseEvent kEvent) { }

    /**
     * Invoked when the mouse leaves a component. This function captures the mouseExited event when the button is
     * pressed and the sculpt outline is being drawn, so that the outline is contained within the bounds of the canvas.
     *
     * @param  kEvent  the mouse event generated by a mouse exit
     */
    public void mouseExited(MouseEvent kEvent) {

        if (m_bSculptEnabled && m_bMousePressed) {
            processMouseReleased(m_iPreviousX, m_iPreviousY);
            m_bMousePressed = false;
        }
    }

    /**
     * One of the overrides necessary to be a MouseMotionListener. Invoked when the mouse is moved, but no buttons are
     * pressed.
     *
     * @param  kEvent  the event generated by a mouse movement
     */
    public void mouseMoved(MouseEvent kEvent) { }


    /**
     * Invoked when a mouse button is pressed. When the left mouse button is pressed, and sculpting is enabled, then the
     * user is beginning to outline the sculpt region. This function initializes the outline drawing, and stores the
     * first point(x,y) so the outline can be closed on mouseReleased. The first and following mouse positions are drawn
     * in outline form using current and previously (last recorded) positions as the start and end points of a 2D line
     * on the canvas.
     *
     * @param  kEvent  the mouse event generated by a mouse press
     */
    public void mousePressed(MouseEvent kEvent) {

        if (m_bSculptEnabled && (kEvent.getButton() == MouseEvent.BUTTON1)) {
            m_bMousePressed = true;

            /* Get the x,y position of the mouse: */
            int iX = kEvent.getX();
            int iY = kEvent.getY();

            /* Store first point in the previous point */
            m_iPreviousX = iX;
            m_iPreviousY = iY;

            /* Store first point */
            m_iFirstX = iX;
            m_iFirstY = iY;

            /* Initialize the min and max points */
            m_iXMin = iX;
            m_iXMax = iX;
            m_iYMin = iY;
            m_iYMax = iY;

            m_aiXPoints = null;
            m_aiXPoints = new int[m_iSculptImageWidth * m_iSculptImageHeight];
            m_aiYPoints = null;
            m_aiYPoints = new int[m_iSculptImageWidth * m_iSculptImageHeight];

            m_aiXPoints[m_iNumberPoints] = iX;
            m_aiYPoints[m_iNumberPoints] = iY;
            m_iNumberPoints++;

            /* If the point is within the SculptImage bounds, then draw the
             * point on the canvas */
            if ((iX >= 0) && (iX < m_iSculptImageWidth) && (iY >= 0) && (iY < m_iSculptImageHeight)) {
                m_kSculptImage.setRGB(iX, iY, m_iColorSculpt);

                /* Draw the modified image: */
                int iCanvasHeight = m_kCanvas3D.getHeight();
                int iCanvasWidth = m_kCanvas3D.getWidth();
                Graphics kGraphics = m_kCanvas3D.getGraphics();
                kGraphics.drawImage(m_kSculptImage, 0, 0, m_iSculptImageWidth, m_iSculptImageHeight, 0, 0, iCanvasWidth,
                                    iCanvasHeight, new Color(0, 0, 0, 0), m_kCanvas3D);
            }
        }
    }

    /**
     * Invoked when a mouse button is released. If this happens while drawing the sculpt outline, the mouse release
     * indicates that the outline is complete. The function closes the outline by connecting the first and last points,
     * and then calls processMouseReleased(), which fills the outline.
     *
     * @param  kEvent  the mouse event generated by a mouse release
     */
    public void mouseReleased(MouseEvent kEvent) {

        if (m_bSculptEnabled && m_bMousePressed && (kEvent.getButton() == MouseEvent.BUTTON1)) {
            m_bMousePressed = false;

            /* Get the x,y position of the mouse: */
            int iX = kEvent.getX();
            int iY = kEvent.getY();

            if (drawShape == RECTANGLE) {
                m_iLastX = iX;
                m_iLastY = iY;

                if (m_iFirstX < iX) {
                    m_iXMin = m_iFirstX;
                } else {
                    m_iXMin = iX;
                }

                if (m_iFirstX > iX) {
                    m_iXMax = m_iFirstX;
                } else {
                    m_iXMax = iX;
                }

                if (m_iFirstY < iY) {
                    m_iYMin = m_iFirstY;
                } else {
                    m_iYMin = iY;
                }

                if (m_iFirstY > iY) {
                    m_iYMax = m_iFirstY;
                } else {
                    m_iYMax = iY;
                }

                drawRectangleArea();
            } else if (drawShape == LINES) {
                /* If the mouse position isn't the same as the last mouse
                 * position, then add it to the point list, draw a line from the
                 * last point to this point, and update min,max. */

                if ((iX != m_iPreviousX) || (iY != m_iPreviousY)) {
                    m_aiXPoints[m_iNumberPoints] = iX;
                    m_aiYPoints[m_iNumberPoints] = iY;
                    m_iNumberPoints++;
                    line(iX, iY, m_iPreviousX, m_iPreviousY);

                    if (iX < m_iXMin) {
                        m_iXMin = iX;
                    }

                    if (iX > m_iXMax) {
                        m_iXMax = iX;
                    }

                    if (iY < m_iYMin) {
                        m_iYMin = iY;
                    }

                    if (iY > m_iYMax) {
                        m_iYMax = iY;
                    }
                }
            }

            // Fill the sculpt outline:
            processMouseReleased(iX, iY);

        }
    }

    /**
     * Set the shape of the drawing sculptor.
     *
     * @param  shape  0 for LINES, 1 for RECTANGLE
     */
    public void setDrawingShape(int shape) {
        drawShape = shape;
    }

    /**
     * Called by the TextureSculptor or VolumeSculptor objects. The function stores the original volume data back to the
     * original values in the m_aiImage_backup data members.
     *
     * @param  kImageA  DOCUMENT ME!
     * @param  kImageB  DOCUMENT ME!
     */
    protected void backupImage(ModelImage kImageA, ModelImage kImageB) {

        /* The size of the ModelImage for indexing: */
        int iXBound = kImageA.getExtents()[0];
        int iYBound = kImageA.getExtents()[1];
        int iZBound = kImageA.getExtents()[2];
        int iSliceSize = iXBound * iYBound;

        int iColor = 1;

        if (kImageA.isColorImage()) {
            iColor = 4;
        }

        if (m_aiImageA_backup == null) {
            m_aiImageA_backup = new int[iXBound * iYBound * iZBound * iColor];

            if (kImageB != null) {
                iColor = 1;

                if (kImageB.isColorImage()) {
                    iColor = 4;
                }

                m_aiImageB_backup = new int[iXBound * iYBound * iZBound * iColor];
            }
        }

        for (int iZ = 0; iZ < iZBound; iZ++) {

            for (int iY = 0; iY < iYBound; iY++) {

                for (int iX = 0; iX < iXBound; iX++) {
                    int iIndex = (iZ * iSliceSize) + (iY * iXBound) + iX;

                    /* Store the volume data in the backup */
                    if (kImageA.isColorImage()) {

                        for (int iC = 0; iC < iColor; iC++) {
                            m_aiImageA_backup[(iIndex * iColor) + iC] = kImageA.getInt((iIndex * iColor) + iC);
                        }
                    } else {
                        m_aiImageA_backup[iIndex] = kImageA.getInt(iIndex);
                    }

                    if (kImageB != null) {

                        if (kImageB.isColorImage()) {

                            for (int iC = 0; iC < iColor; iC++) {
                                m_aiImageB_backup[(iIndex * iColor) + iC] = kImageB.getInt((iIndex * iColor) + iC);
                            }
                        } else {
                            m_aiImageB_backup[iIndex] = kImageB.getInt(iIndex);
                        }
                    }
                }
            }
        }
    }

    /**
     * Called by the TextureSculptor or VolumeSculptor objects. The function stores the original volume data back to the
     * original values in the m_aiImage_backup data members.
     *
     * @param  kImageA  DOCUMENT ME!
     * @param  kImageB  DOCUMENT ME!
     * @param  iIndex   DOCUMENT ME!
     */
    protected void backupImage(ModelImage kImageA, ModelImage kImageB, int iIndex) {

        /* The size of the ModelImage for indexing: */
        int iXBound = kImageA.getExtents()[0];
        int iYBound = kImageA.getExtents()[1];
        int iZBound = kImageA.getExtents()[2];
        int iSliceSize = iXBound * iYBound;

        int iColor = 1;

        if (kImageA.isColorImage()) {
            iColor = 4;
        }

        if (m_aiImageA_backup == null) {
            m_aiImageA_backup = new int[iXBound * iYBound * iZBound * iColor];

            if (kImageB != null) {
                iColor = 1;

                if (kImageB.isColorImage()) {
                    iColor = 4;
                }

                m_aiImageB_backup = new int[iXBound * iYBound * iZBound * iColor];
            }
        }

        /* Store the volume data in the backup */
        if (kImageA.isColorImage()) {

            for (int iC = 0; iC < iColor; iC++) {
                m_aiImageA_backup[(iIndex * iColor) + iC] = kImageA.getInt((iIndex * iColor) + iC);
            }
        } else {
            m_aiImageA_backup[iIndex] = kImageA.getInt(iIndex);
        }

        if (kImageB != null) {

            if (kImageB.isColorImage()) {

                for (int iC = 0; iC < iColor; iC++) {
                    m_aiImageB_backup[(iIndex * iColor) + iC] = kImageB.getInt((iIndex * iColor) + iC);
                }
            } else {
                m_aiImageB_backup[iIndex] = kImageB.getInt(iIndex);
            }
        }
    }

    /**
     * blendColor: blends two colors in ARGB format from the BufferedImage class, using an alpha value of 0.5.
     *
     * @param   iImageColor  DOCUMENT ME!
     * @param   iBlendColor  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected int blendColor(int iImageColor, int iBlendColor) {
        int iImageRed = ((iImageColor >> 16) & 0xFF);
        int iImageGreen = ((iImageColor >> 8) & 0xFF);
        int iImageBlue = (iImageColor & 0xFF);
        iImageRed = iImageRed >> 1;
        iImageGreen = iImageGreen >> 1;
        iImageBlue = iImageBlue >> 1;

        int iBlendRed = ((iBlendColor >> 16) & 0xFF);
        int iBlendGreen = ((iBlendColor >> 8) & 0xFF);
        int iBlendBlue = (iBlendColor & 0xFF);

        iBlendRed = iBlendRed >> 1;
        iBlendGreen = iBlendGreen >> 1;
        iBlendBlue = iBlendBlue >> 1;

        int iRed = (iImageRed + iBlendRed);
        int iGreen = (iImageGreen + iBlendGreen);
        int iBlue = (iImageBlue + iBlendBlue);

        int iNewColor = (iRed & 0x00ff0000) | (iGreen & 0x0000ff00) | (iBlue & 0x000000ff);
        iNewColor = (iNewColor | 0xff000000);

        return iNewColor;
    }

    /**
     * Called by the TextureSculptor or VolumeSculptor objects. The function calculates the mimumum and maximum
     * intensity and color values in the images.
     *
     * @param  kImageA  DOCUMENT ME!
     * @param  kImageB  DOCUMENT ME!
     */
    protected void calculateMinMaxValues(ModelImage kImageA, ModelImage kImageB) {

        /* When the volume is sculpted, the removed voxels are set to the
         * lowest (zero-value) in the volume. This step calculates the minimum
         * value in the ModelImage volume data:*/
        fImageMin = Float.MAX_VALUE;
        fImageBMin = Float.MAX_VALUE;

        dAMinAlpha = Float.MAX_VALUE;
        dAMinRed = Float.MAX_VALUE;
        dAMinGreen = Float.MAX_VALUE;
        dAMinBlue = Float.MAX_VALUE;

        dBMinAlpha = Float.MAX_VALUE;
        dBMinRed = Float.MAX_VALUE;
        dBMinGreen = Float.MAX_VALUE;
        dBMinBlue = Float.MAX_VALUE;

        /* The size of the ModelImage for indexing: */
        int iXBound = kImageA.getExtents()[0];
        int iYBound = kImageA.getExtents()[1];
        int iZBound = kImageA.getExtents()[2];
        int iSliceSize = iXBound * iYBound;
        int iSize = iZBound * iYBound * iXBound;

        /* iMod is used for the progress bar */
        int iMod = iYBound / 10;

        for (int iY = 0; iY < iYBound; iY++) {
            int iIndexY = iY * iXBound;

            /* Update progress bar: */
            if (((iY % iMod) == 0) || (iY == (iYBound - 1))) {

                if (iY == (iYBound - 1)) {
                    m_kProgress.setValue(100);
                } else {
                    m_kProgress.setValue(Math.round((float) (iY) / (iYBound - 1) * 100));
                    m_kProgress.update(m_kProgress.getGraphics());
                }
            }

            for (int iX = 0; iX < iXBound; iX++) {

                for (int iZ = 0; iZ < iZBound; iZ++) {
                    int iIndexZ = iZ * iSliceSize;
                    int iIndex = iIndexZ + iIndexY + iX;

                    /* get the min value for ModelImageA */
                    if (kImageA.isColorImage()) {

                        if (kImageA.getDouble((iIndex * 4) + 0) < dAMinAlpha) {
                            dAMinAlpha = kImageA.getDouble((iIndex * 4) + 0);
                        }

                        if (kImageA.getDouble((iIndex * 4) + 1) < dAMinRed) {
                            dAMinRed = kImageA.getDouble((iIndex * 4) + 1);
                        }

                        if (kImageA.getDouble((iIndex * 4) + 2) < dAMinGreen) {
                            dAMinGreen = kImageA.getDouble((iIndex * 4) + 2);
                        }

                        if (kImageA.getDouble((iIndex * 4) + 3) < dAMinBlue) {
                            dAMinBlue = kImageA.getDouble((iIndex * 4) + 3);
                        }
                    } else if (kImageA.getFloat(iIndex) < fImageMin) {
                        fImageMin = kImageA.getFloat(iIndex);
                    }

                    /* get the min value for ModelImageB */
                    if (kImageB != null) {

                        if (kImageB.isColorImage()) {

                            if (kImageB.getDouble((iIndex * 4) + 0) < dBMinAlpha) {
                                dBMinAlpha = kImageB.getDouble((iIndex * 4) + 0);
                            }

                            if (kImageB.getDouble((iIndex * 4) + 1) < dBMinRed) {
                                dBMinRed = kImageB.getDouble((iIndex * 4) + 1);
                            }

                            if (kImageB.getDouble((iIndex * 4) + 2) < dBMinGreen) {
                                dBMinGreen = kImageB.getDouble((iIndex * 4) + 2);
                            }

                            if (kImageB.getDouble((iIndex * 4) + 3) < dBMinBlue) {
                                dBMinBlue = kImageB.getDouble((iIndex * 4) + 3);
                            }
                        } else if (kImageB.getFloat(iIndex) < fImageBMin) {
                            fImageBMin = kImageB.getFloat(iIndex);
                        }
                    }
                }
            }
        }
    }

    /**
     * Draw rectangle shape object.
     */
    protected void drawRectangleArea() {

        // /*
        if (!(m_iLastX < m_iFirstX)) {
            line(m_iXMin, m_iYMax, m_iXMax, m_iYMax);
            m_aiXPoints[m_iNumberPoints] = m_iXMin;
            m_aiYPoints[m_iNumberPoints] = m_iYMax;
            m_iNumberPoints++;
            m_aiXPoints[m_iNumberPoints] = m_iXMax;
            m_aiYPoints[m_iNumberPoints] = m_iYMax;
            m_iNumberPoints++;
        }

        line(m_iXMax, m_iYMax, m_iXMax, m_iYMin);
        m_aiXPoints[m_iNumberPoints] = m_iXMax;
        m_aiYPoints[m_iNumberPoints] = m_iYMax;
        m_iNumberPoints++;
        m_aiXPoints[m_iNumberPoints] = m_iXMax;
        m_aiYPoints[m_iNumberPoints] = m_iYMin;
        m_iNumberPoints++;

        line(m_iXMax, m_iYMin, m_iXMin, m_iYMin);
        m_aiXPoints[m_iNumberPoints] = m_iXMax;
        m_aiYPoints[m_iNumberPoints] = m_iYMin;
        m_iNumberPoints++;
        m_aiXPoints[m_iNumberPoints] = m_iXMin;
        m_aiYPoints[m_iNumberPoints] = m_iYMin;
        m_iNumberPoints++;

        line(m_iXMin, m_iYMin, m_iXMin, m_iYMax);
        m_aiXPoints[m_iNumberPoints] = m_iXMin;
        m_aiYPoints[m_iNumberPoints] = m_iYMin;
        m_iNumberPoints++;
        m_aiXPoints[m_iNumberPoints] = m_iXMin;
        m_aiYPoints[m_iNumberPoints] = m_iYMax;
        m_iNumberPoints++;

        if ((m_iLastX < m_iFirstX)) {
            line(m_iXMin, m_iYMax, m_iXMax, m_iYMax);
            m_aiXPoints[m_iNumberPoints] = m_iXMin;
            m_aiYPoints[m_iNumberPoints] = m_iYMax;
            m_iNumberPoints++;
            m_aiXPoints[m_iNumberPoints] = m_iXMax;
            m_aiYPoints[m_iNumberPoints] = m_iYMax;
            m_iNumberPoints++;
        }

    }

    /**
     * fill: fill the sculpt outline drawn by the user. Pixels are determined to be inside or outside the sculpt region
     * based on the parameters, aaiCrossingPoints and aiNumCrossings, using a scan-conversion algorithm that traverses
     * each row and column of the bounding box of the sculpt region coloring inside points as it goes.
     *
     * @param  aaiCrossingPoints  DOCUMENT ME!
     * @param  aiNumCrossings     DOCUMENT ME!
     */
    protected void fill(int[][] aaiCrossingPoints, int[] aiNumCrossings) {
        int iColumn = 0;

        /* Loop over the width of the sculpt region bounding-box: */
        for (int iX = m_iXMin; iX < m_iXMax; iX++) {
            boolean bInside = false;

            /* Loop over the height of the sculpt region bounding-box: */
            for (int iY = m_iYMin; iY < m_iYMax; iY++) {

                /* loop over each crossing point for this column: */
                for (int iCross = 0; iCross < aiNumCrossings[iColumn]; iCross++) {

                    if (iY == aaiCrossingPoints[iColumn][iCross]) {

                        /* Each time an edge is cross the point alternates
                         * from outside to inside: */
                        bInside = !bInside;
                    }
                }

                if (bInside == true) {

                    /* The current pixel is inside the sculpt region.  Get the
                     * image color from the canvas image and alpha-blend the sculpt color ontop, storing the result in
                     * the canvas image.
                     */
                    int iImageColor = m_kSculptImage.getRGB(iX, iY);
                    int iNewColor = blendColor(iImageColor, m_iColorSculpt);
                    m_kSculptImage.setRGB(iX, iY, iNewColor);

                    /* The un-blended sculpt color is stored in the opaque
                     * image for point-polygon test later when the sculpt region is applied to the volume.
                     */
                    m_kSculptImageOpaque.setRGB(iX, iY, m_iColorSculpt);
                    m_bSculptDrawn = true;
                }
            }

            iColumn++;
        }
    }


    /**
     * Calls disposeLocal.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        this.disposeLocal(false);
        super.finalize();
    }


    /**
     * Grab the canvas image, and copy into the m_kSculptImage data members.
     *
     * @return  DOCUMENT ME!
     */
    protected boolean getFrameBuffer() {
        int[] pixels;
        int xDim, yDim;
        Robot robot;

        Dimension d = new Dimension();
        Point p = new Point();

        p.x = 0;
        p.y = 0;
        SwingUtilities.convertPointToScreen(p, m_kCanvas3D);

        d.width = m_kCanvas3D.getWidth();
        d.height = m_kCanvas3D.getHeight();

        Rectangle currentRectangle = new Rectangle(p, d);

        /* Using Robot to capture image rectangle and transfering image into
         * the pixel buffer.
         */
        try {
            robot = new Robot();

            Image imagePix = robot.createScreenCapture(currentRectangle);

            xDim = currentRectangle.width;
            yDim = currentRectangle.height;
            xDim = xDim - (xDim % 4);
            yDim = yDim - (yDim % 4);
            pixels = new int[xDim * yDim];

            PixelGrabber pgTest = new PixelGrabber(imagePix, 0, 0, xDim, yDim, pixels, 0, xDim);

            pgTest.grabPixels();
            imagePix = null;
            pgTest = null;
        } catch (InterruptedException e) {
            Preferences.debug("Interrupted waiting for pixels!");

            return false;
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("VolumeSculpt: unable to allocate enough memory for RGB image");

            return false;
        } catch (AWTException error) {
            MipavUtil.displayError("Platform doesn't support screen capture.");

            return false;
        }

        int sculptBG = 0;

        for (int y = 0; y < yDim; y++) {

            for (int x = 0; x < xDim; x++) {
                sculptBG = 0xffffffff & pixels[(y * xDim) + x];
                sculptBG = 0xff000000 | sculptBG;
                m_kSculptImage.setRGB(x, y, sculptBG);
                m_kSculptImageOpaque.setRGB(x, y, sculptBG);
                m_kSavedImage.setRGB(x, y, sculptBG);
            }
        }

        pixels = null;
        robot = null;

        return true;
    }

    /**
     * This function draws a 2D line on the canvas image -- ontop of the currently rendered image. To implement the line
     * drawing I use the midpoint line algorithm, in Foley and van Dam, originally Bresenham's algorithm. The first part
     * of the function sets up the step sizes for x and y depending on what type of line is being drawn. The second part
     * loops over the line, drawing pixels into the canvas image.
     *
     * @param  iX0  DOCUMENT ME!
     * @param  iY0  DOCUMENT ME!
     * @param  iX1  DOCUMENT ME!
     * @param  iY1  DOCUMENT ME!
     */
    protected void line(int iX0, int iY0, int iX1, int iY1) {

        if ((iX0 == iX1) && (iY0 == iY1)) {
            return;
        }

        /* incremental steps along the x, and y-directions */
        int iXStep, iYStep;

        /* The slope of the line in the y-direction: */
        int iSlopeY = (iY1 - iY0);

        if (iSlopeY >= 0) {

            /* The slope is positive in the y direction. */
            iYStep = 1;
        } else {

            /* The slope is negative in the y direction */
            iSlopeY = -iSlopeY;
            iYStep = -1;
        }

        /* The slope of the line in the x-direction: */
        int iSlopeX = (iX1 - iX0);

        if (iSlopeX >= 0) {

            /* The slope is positive in the x direction */
            iXStep = 1;
        } else {

            /* The slope is negative in the x direction  */
            iSlopeX = -iSlopeX;
            iXStep = -1;
        }

        /* Iterators iX,iY: initialize to the first x,y position of the
         * line: */
        int iX = iX0;
        int iY = iY0;

        /* partial increments for x, and y */
        int iD, iIncrE, iIncrNE;

        /* The line is vertical.*/
        if (iSlopeX == 0) {
            m_kSculptImage.setRGB(iX, iY, m_iColorSculpt);
            iY = iY + iYStep;

            while (iY != iY1) {
                m_kSculptImage.setRGB(iX, iY, m_iColorSculpt);
                iY = iY + iYStep;
            }
        }
        /* The line is horixontal. */
        else if (iSlopeY == 0) {
            m_kSculptImage.setRGB(iX, iY, m_iColorSculpt);
            iX = iX + iXStep;

            while (iX != iX1) {
                m_kSculptImage.setRGB(iX, iY, m_iColorSculpt);
                iX = iX + iXStep;
            }
        }

        /* The line has a slope <= -1 or between 1 and 0  */
        else if (iSlopeY < iSlopeX) {
            iD = (2 * iSlopeY) - iSlopeX;
            iIncrE = iSlopeY * 2;
            iIncrNE = 2 * (iSlopeY - iSlopeX);

            /* Draw the line into the frame buffer until the index reaches
             * the greatest point in the line:  */
            m_kSculptImage.setRGB(iX, iY, m_iColorSculpt);
            iX = iX + iXStep;

            if (iD > 0) {
                iD = iD + iIncrNE;
                iY = iY + iYStep;
            } else {
                iD = iD + iIncrE;
            }

            while ((iX != iX1) || (iY != iY1)) {
                m_kSculptImage.setRGB(iX, iY, m_iColorSculpt);
                iX = iX + iXStep;

                if (iD > 0) {
                    iD = iD + iIncrNE;
                    iY = iY + iYStep;
                } else {
                    iD = iD + iIncrE;
                }
            }
        }

        /* The line has a slope >= 1 or between -1 and 0 */
        else {
            iD = (2 * iSlopeX) - iSlopeY;
            iIncrE = iSlopeX * 2;
            iIncrNE = 2 * (iSlopeX - iSlopeY);

            /* Draw the line into the frame buffer until the index reaches
             * the greatest point in the line */
            m_kSculptImage.setRGB(iX, iY, m_iColorSculpt);
            iY = iY + iYStep;

            if (iD > 0) {
                iX = iX + iXStep;
                iD = iD + iIncrNE;
            } else {
                iD = iD + iIncrE;
            }

            while ((iX != iX1) || (iY != iY1)) {
                m_kSculptImage.setRGB(iX, iY, m_iColorSculpt);
                iY = iY + iYStep;

                if (iD > 0) {
                    iX = iX + iXStep;
                    iD = iD + iIncrNE;
                } else {
                    iD = iD + iIncrE;
                }
            }
        }
    }

    /**
     * This function computes the set of spans indicated by column crossings for the sculpt outline drawn by the user,
     * by doing a polygon scan conversion in gridded space. The outline must be closed with last point = first point.
     *
     * @param  aaiCrossingPoints  DOCUMENT ME!
     * @param  aiNumCrossings     DOCUMENT ME!
     */
    protected void outlineRegion(int[][] aaiCrossingPoints, int[] aiNumCrossings) {

        /*
         * nudge the vertices off of the exact integer coords by a factor of 0.1 to avoid vertices on pixel centers,
         * which would create spans of zero length
         */
        double dNudge = 0.1;
        double[][][] aaadEdgeList = new double[m_iNumberPoints][2][2];

        for (int iPoint = 0; iPoint < (m_iNumberPoints - 1); iPoint++) {
            aaadEdgeList[iPoint][0][0] = m_aiXPoints[iPoint] - dNudge;
            aaadEdgeList[iPoint][0][1] = m_aiYPoints[iPoint] - dNudge;
            aaadEdgeList[iPoint][1][0] = m_aiXPoints[iPoint + 1] - dNudge;
            aaadEdgeList[iPoint][1][1] = m_aiYPoints[iPoint + 1] - dNudge;
        }

        /*
         * Compute the crossing points for this column and produce spans.
         */
        for (int iColumn = m_iXMin; iColumn <= m_iXMax; iColumn++) {
            int iIndex = iColumn - m_iXMin;

            /* for each edge, figure out if it crosses this column and add its
             * crossing point to the list if so. */
            aiNumCrossings[iIndex] = 0;

            for (int iPoint = 0; iPoint < (m_iNumberPoints - 1); iPoint++) {
                double dX0 = aaadEdgeList[iPoint][0][0];
                double dX1 = aaadEdgeList[iPoint][1][0];
                double dY0 = aaadEdgeList[iPoint][0][1];
                double dY1 = aaadEdgeList[iPoint][1][1];
                double dMinX = (dX0 <= dX1) ? dX0 : dX1;
                double dMaxX = (dX0 > dX1) ? dX0 : dX1;

                if ((dMinX < iColumn) && (dMaxX > iColumn)) {

                    /* The edge crosses this column, so compute the
                     * intersection.
                     */
                    double dDX = dX1 - dX0;
                    double dDY = dY1 - dY0;
                    double dM = (dDX == 0) ? 0 : (dDY / dDX);
                    double dB = (dDX == 0) ? 0 : (((dX1 * dY0) - (dY1 * dX0)) / dDX);

                    double dYCross = (dM * iColumn) + dB;
                    double dRound = 0.5;
                    aaiCrossingPoints[iIndex][aiNumCrossings[iIndex]] = (dYCross < 0) ? (int) (dYCross - dRound)
                                                                                      : (int) (dYCross + dRound);
                    aiNumCrossings[iIndex]++;
                }
            }

            /* sort the set of crossings for this column: */
            sortCrossingPoints(aaiCrossingPoints[iIndex], aiNumCrossings[iIndex]);
        }

        aaadEdgeList = null;
    }

    /**
     * Called when the left mouse button is released, indicating that the sculpt outline is complete, or when the
     * outline is being drawn and the mouse is fragged outside the canvas area. The function closes the outline, using
     * the first and last recorded mouse positions. It then fills the outline, by blending the fill color with the
     * background data on the canvas. Filling is done as a 2D polygon scan conversion so that any closed loop, including
     * concave and self-intersecting loops, can be processed accurately.
     *
     * @param  iX  int, the x position of the mouse recorded when the mouse button was released.
     * @param  iY  int, the y position of the mouse recorded when the mouse button was released.
     */
    protected void processMouseReleased(int iX, int iY) {

        /* Close the sculpt outline by connecting the last point input: iX,iY
         * to the first recorded point: m_iFirstX,m_iFirstY.
         */
        if (drawShape == LINES) {
            line(iX, iY, m_iFirstX, m_iFirstY);

            /* Store the first point redundantly in the point array: */
            m_aiXPoints[m_iNumberPoints] = m_iFirstX;
            m_aiYPoints[m_iNumberPoints] = m_iFirstY;
            m_iNumberPoints++;
        }

        int[][] aaiCrossingPoints = new int[m_iXMax - m_iXMin + 1][];
        int[] aiNumCrossings = new int[m_iXMax - m_iXMin + 1];

        for (int i = 0; i < (m_iXMax - m_iXMin + 1); i++) {
            aaiCrossingPoints[i] = new int[m_iNumberPoints];
        }

        outlineRegion(aaiCrossingPoints, aiNumCrossings);
        fill(aaiCrossingPoints, aiNumCrossings);
        m_iNumberPoints = 0;

        /* Draw the modified image: */
        int iCanvasHeight = m_kCanvas3D.getHeight();
        int iCanvasWidth = m_kCanvas3D.getWidth();
        Graphics kGraphics = m_kCanvas3D.getGraphics();

        // m_kCanvas3D.addNotify();
        for (int m = 0; m < 1 /* 20 */; m++) {
            kGraphics.drawImage(m_kSculptImage, 0, 0, m_iSculptImageWidth, m_iSculptImageHeight, 0, 0, iCanvasWidth,
                                iCanvasHeight, new Color(0, 0, 0, 0), m_kCanvas3D);
        }

        aaiCrossingPoints = null;
        aiNumCrossings = null;
    }

    /**
     * Called by the TextureSculptor or VolumeSculptor objects. The function sculpts the original volume data by setting
     * the value at input iIndex to the mimimum values calculated in CalculateMinMaxValues.
     *
     * @param  kImageA  DOCUMENT ME!
     * @param  kImageB  DOCUMENT ME!
     * @param  iIndex   DOCUMENT ME!
     */
    protected void sculptImage(ModelImage kImageA, ModelImage kImageB, int iIndex) {

        /* set the voxel value to zero */
        if (kImageA.isColorImage()) {
            kImageA.set((iIndex * 4) + 0, dAMinAlpha);
            kImageA.set((iIndex * 4) + 1, dAMinRed);
            kImageA.set((iIndex * 4) + 2, dAMinGreen);
            kImageA.set((iIndex * 4) + 3, dAMinBlue);
        } else {
            kImageA.set(iIndex, fImageMin);
        }

        if (kImageB != null) {

            if (kImageB.isColorImage()) {
                kImageB.set((iIndex * 4) + 0, dBMinAlpha);
                kImageB.set((iIndex * 4) + 1, dBMinRed);
                kImageB.set((iIndex * 4) + 2, dBMinGreen);
                kImageB.set((iIndex * 4) + 3, dBMinBlue);
            } else {
                kImageB.set(iIndex, fImageBMin);
            }
        }
    }

    /**
     * Sorts the edge crossing points in place.
     *
     * @param  aiList        DOCUMENT ME!
     * @param  iNumElements  DOCUMENT ME!
     */
    protected void sortCrossingPoints(int[] aiList, int iNumElements) {
        boolean bDidSwap = true;

        while (bDidSwap) {
            bDidSwap = false;

            for (int iPoint = 0; iPoint < (iNumElements - 1); iPoint++) {

                if (aiList[iPoint] > aiList[iPoint + 1]) {
                    int iTmp = aiList[iPoint];
                    aiList[iPoint] = aiList[iPoint + 1];
                    aiList[iPoint + 1] = iTmp;
                    bDidSwap = true;
                }
            }
        }
    }

    /**
     * Called by the TextureSculptor or VolumeSculptor objects. The function resets the volume data back to the original
     * values, using the data stored in the m_aiImage_backup data members.
     *
     * @param  kImageA  DOCUMENT ME!
     * @param  kImageB  DOCUMENT ME!
     */
    protected void undoSculpt(ModelImage kImageA, ModelImage kImageB) {

        if (m_aiImageA_backup == null) {
            return;
        }

        /* The extents of the ModelImages: */
        int iXBound = kImageA.getExtents()[0];
        int iYBound = kImageA.getExtents()[1];
        int iZBound = kImageA.getExtents()[2];
        int iSliceSize = iXBound * iYBound;
        int iSize = iXBound * iYBound * iZBound;

        /* Reset the ModelImage to the original values */
        int iMod = iYBound / 10;

        for (int iY = 0; iY < iYBound; iY++) {

            /* Update progress bar: */
            if (((iY % iMod) == 0) || (iY == (iYBound - 1))) {

                if (iY == (iYBound - 1)) {
                    m_kProgress.setValue(100);
                } else {
                    m_kProgress.setValue(Math.round((float) (iY) / (iYBound - 1) * 100));
                    m_kProgress.update(m_kProgress.getGraphics());
                }
            }

            int iIndexY = iY * iXBound;

            for (int iX = 0; iX < iXBound; iX++) {

                for (int iZ = 0; iZ < iZBound; iZ++) {
                    int iIndexZ = iZ * iSliceSize;
                    int iIndex = iIndexZ + iIndexY + iX;

                    if (kImageA.isColorImage()) {

                        for (int iColor = 0; iColor < 4; iColor++) {
                            kImageA.set((iIndex * 4) + iColor, m_aiImageA_backup[(iIndex * 4) + iColor]);
                        }
                    } else {
                        kImageA.set(iIndex, m_aiImageA_backup[iIndex]);
                    }

                    if (kImageB != null) {

                        if (kImageB.isColorImage()) {

                            for (int iColor = 0; iColor < 4; iColor++) {
                                kImageB.set((iIndex * 4) + iColor, m_aiImageB_backup[(iIndex * 4) + iColor]);
                            }
                        } else {
                            kImageB.set(iIndex, m_aiImageB_backup[iIndex]);
                        }
                    }
                }
            }
        }

        /* Free the backup variables */
        m_aiImageA_backup = null;
        m_aiImageB_backup = null;
    }
}
