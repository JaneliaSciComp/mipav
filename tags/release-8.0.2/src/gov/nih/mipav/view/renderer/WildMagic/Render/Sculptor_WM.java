package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewFileChooserBase;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.io.File;

import javax.media.opengl.awt.GLCanvas;
import javax.swing.JFileChooser;

import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Vector4f;

/*
import com.jogamp.newt.Window;
import com.jogamp.newt.event.KeyEvent;
import com.jogamp.newt.event.KeyListener;
import com.jogamp.newt.event.MouseAdapter;
import com.jogamp.newt.event.MouseEvent;
import com.jogamp.newt.event.MouseListener;
import com.jogamp.newt.event.awt.AWTMouseAdapter;
import com.jogamp.newt.opengl.GLWindow;
*/

/**
 * Volume Sculpturing allows the user to draw a region on the screen in the volume render view, and to remove the parts
 * of the volume covered by that region. The user outlines a region by holding the left mouse button down and drawing
 * directly on the screen. 
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
 * @author  Alexandra Bokinsky, Ph.D. Under contract from GeometricTools, Inc.
 */
public class Sculptor_WM implements MouseMotionListener, MouseListener {

    /** Free-hand sculpting */
    public static int LINES = 0;

    /** rectangle sculpt */
    public static int RECTANGLE = 1;

    /** Mouse positions in x,y for drawing the sculpt region:. */
    protected int[] m_aiXPoints;

    /** Mouse positions in x,y for drawing the sculpt region:. */
    protected int[] m_aiYPoints;

    /** m_bMousePressed is set to true when the mousePressed callback is activated and the left mouse button is down. */
    protected boolean m_bMousePressed;

    /** True when a sculpt region has been drawn. */
    protected boolean m_bSculptDrawn = false;

    /** m_bSculptEnabled is turned on when the user presses the "Draw Sculpt Outline" button. */
    protected boolean m_bSculptEnabled = false;

    /** The sculpt region color. */
    protected int[] m_aiColorSculpt = new int[] { 0, 0, 255 };

    /** First x mouse position */
    protected int m_iFirstX;

    /** First y mouse position */
    protected int m_iFirstY;

    /** Last x mouse position */
    protected int m_iLastX;

    /** Last y mouse position */
    protected int m_iLastY;

    /** Number of points in the free-hand line. */
    protected int m_iNumberPoints;

    /** Previous and first mouse positions:. */
    protected int m_iPreviousX;

    /** Previous and first mouse positions:. */
    protected int m_iPreviousY;

    /** sculpt image height */
    protected int m_iSculptImageHeight;

    /** sculpt image width */
    protected int m_iSculptImageWidth;

    /** The min and max x,y mouse values */
    protected int m_iXMax;

    /** The min and max x,y mouse values */
    protected int m_iXMin;

    /** The min and max x,y mouse values */
    protected int m_iYMax;

    /** The min and max x,y mouse values */
    protected int m_iYMin;

    /** Canvas reference for drawing :. */
    protected GLCanvas m_kCanvas;

    /** Backup image */
    protected byte[] m_aucSavedImage;

    /**
     * m_kSculptImage is a screen shot of the canvas image. The sculpt region is drawn directly into m_kSculptImage. The
     * outline is filled by alpha-blending the Sculpt region color with the m_kSculptImage backgound image.
     */
    protected byte[] m_aucSculptImage = null;

    /** Shape of the drawing rectangle. */
    private int drawShape = LINES;

    /** ModelImage A */
    private ModelImage m_kImageA = null;
    /** Backup of ModelImage A */
    private ModelImage m_kImageBackupA = null;

    /** ModelImage B */
    private ModelImage m_kImageB = null;
    /** Backup of ModelImage B */
    private ModelImage m_kImageBackupB = null;

    /**  World-view-project matrix for applying the sculpt to the volume. */
    private Matrix4f m_kWVPMatrix = null;
    /** Texture data for ModelImage A in byte */
    private byte[] m_aucTextureImageDataA = null;

    /** Texture data for ModelImage B in byte */
    private byte[] m_aucTextureImageDataB = null;
    
    /**
     * Constructor.
     * @param kCanvas the canvas to draw the sculpt image on.
     */
    public Sculptor_WM( GLCanvas kCanvas )
    {
        m_kCanvas = kCanvas;
        initVolumeSculptor();
    }

    /**
     * Apply the sculpt region to the volume data.
     * @return true if the volume has changed, false indicates no change.
     */
    public boolean applySculpt(int iTSlice) { 
        /* Disable drawing the sculpt region outline: */
        m_bSculptEnabled = false;

        boolean bVolumeChanged = false;

        if (m_bSculptDrawn == false) {
            return false;
        }
        
        if ( m_kImageA == null ) {
            return false;
        }
        if ( m_kWVPMatrix == null ) {
            return false;
        }

        if ( (m_aucTextureImageDataA == null) )
        {
            return false;
        }

        int iXBound = m_kImageA.getExtents()[0];
        int iYBound = m_kImageA.getExtents()[1];
        int iZBound = m_kImageA.getExtents()[2];
        int iSliceSize = iXBound * iYBound;
        int iVolumeSize = iSliceSize * iZBound;

        float fMaxX = (iXBound - 1) * m_kImageA.getFileInfo(0).getResolutions()[0];
        float fMaxY = (iYBound - 1) * m_kImageA.getFileInfo(0).getResolutions()[1];
        float fMaxZ = (iZBound - 1) * m_kImageA.getFileInfo(0).getResolutions()[2];

        float fMax = fMaxX;
        if (fMaxY > fMax) {
            fMax = fMaxY;
        }
        if (fMaxZ > fMax) {
            fMax = fMaxZ;
        }
        float fX = fMaxX/fMax;
        float fY = fMaxY/fMax;
        float fZ = fMaxZ/fMax;


        Vector4f kIn = new Vector4f(0,0,0,1);
        Vector4f kOut = new Vector4f();
        for (int iZ = 0; iZ < iZBound; iZ++)
        {
            for (int iY = 0; iY < iYBound; iY++)
            {
                for (int iX = 0; iX < iXBound; iX++)
                {
                    kIn.X = ((float)iX/(float)iXBound)*fX;
                    kIn.Y = ((float)iY/(float)iYBound)*fY;
                    kIn.Z = ((float)iZ/(float)iZBound)*fZ;

                    m_kWVPMatrix.multLeft( kIn, kOut);
                    kOut.scale(1.0f/kOut.W);

                    int iXIndex = (int)((m_iSculptImageWidth-1) * (1 + kOut.X)/2.0f);
                    int iYIndex = (int)((m_iSculptImageHeight-1) * (1 - kOut.Y)/2.0f);

                    int iTextureIndex = iZ * iSliceSize + (iY * iXBound) + iX;
                    int iDataIndex = iZ * iSliceSize + (iY * iXBound) + iX;
                    iDataIndex += (iTSlice * iVolumeSize);
                    
                    backupData( m_kImageA, m_kImageBackupA, iDataIndex );
                    if ( m_kImageB != null )
                    {
                        backupData( m_kImageB, m_kImageBackupB, iDataIndex );
                    }
                    if ( (iXIndex >= 0) && (iXIndex < m_iSculptImageWidth) &&
                         (iYIndex >= 0) && (iYIndex < m_iSculptImageHeight) &&
                         getSculpt( iXIndex, iYIndex ) )
                    {
                        bVolumeChanged = true;
                        
                        sculptData( m_kImageA, iDataIndex, iTextureIndex, m_aucTextureImageDataA );

                        if ( m_kImageB != null )
                        {
                            sculptData( m_kImageB, iDataIndex, iTextureIndex, m_aucTextureImageDataB );
                        }
                    }
                }
            }
        }
        
        kIn = null;
        return bVolumeChanged;
    }

    /**
     * Clears the sculpt image.
     */
    public void clearSculpt() {
        m_bSculptEnabled = false;
        m_bSculptDrawn = false;
        m_iNumberPoints = 0;
        backupSculptImage();
    }

    /**
     * Memory cleanup.
     */
    public void disposeLocal()
    {
        m_kCanvas.removeMouseMotionListener(this);
        m_kCanvas.removeMouseListener(this);

        if (m_aucSculptImage != null) {
            m_aucSculptImage = null;
        }

        if (m_aucSavedImage != null) {
            m_aucSavedImage = null;
        }

        m_aiColorSculpt = null;
        m_aiXPoints = null;
        m_aiYPoints = null;

        m_kImageA = null;
        if ( m_kImageBackupA != null )
        {
            m_kImageBackupA.disposeLocal(true);
            m_kImageBackupA = null;
        }
        m_kImageB = null;
        if ( m_kImageBackupB != null )
        {
            m_kImageBackupB.disposeLocal(true);
            m_kImageBackupB = null;
        }

        m_kWVPMatrix = null;
        m_aucTextureImageDataA = null;
        m_aucTextureImageDataB = null;
   }

    /**
     * Turn sculpt drawing on/off.
     * @param bEnabled turns sculpting on/off.
     */
    public void enableSculpt(boolean bEnabled) {
        m_bSculptEnabled = bEnabled;

        if (m_bSculptEnabled == false) {
            m_aucSculptImage = null;
            m_aucSavedImage = null;
            return;
        }

        m_iNumberPoints = 0;

        /* Get the current size of the canvas */
        m_iSculptImageWidth = m_kCanvas.getWidth();
        m_iSculptImageHeight = m_kCanvas.getHeight();

        if ((m_iSculptImageWidth <= 0) || (m_iSculptImageHeight <= 0)) {
            return; // Dual Panel mode, both are enabled, but only one panel
                    // is visible
        }

        /* Allocate the m_kSculptImage */
        m_aucSculptImage = new byte[m_iSculptImageWidth * m_iSculptImageHeight *4];
        m_aucSavedImage = new byte[m_iSculptImageWidth * m_iSculptImageHeight *4];
    }
    
    /**
     * Return the sculpt enabled status.
     * @return sculpt enabled status.
     */
    public boolean getEnable() {
        return m_bSculptEnabled;
    }

    /**
     * Return sculpt image for display.
     * @return byte[] containing the sculpt image.
     */
    public byte[] getSculptImage()
    {
        return m_aucSculptImage;
    }

    /**
     * Initialize the Mouse listener. Note - this function should be called once per instance only, as it sets up the
     * MouseMotionListener and MouseListener.
     */
    public void initVolumeSculptor() {

        m_kCanvas.addMouseMotionListener(this);
        m_kCanvas.addMouseListener(this);

        m_bSculptEnabled = false;
        m_bMousePressed = false;
    }

    /**
     * Inverts the sculpt image.
     */
    public void invertSculpt() {

        if (m_bSculptDrawn == false) {
            return;
        }


        /* Loop over the pixels in the sculpt buffer: */
        for (int iY = 0; iY < m_iSculptImageHeight; iY++)
        {
            for (int iX = 0; iX < m_iSculptImageWidth; iX++)
            {
                int index = (iY * m_iSculptImageWidth + iX)*4;

                /* If the pixel is inside the sculpt region, then change it to
                 * the saved image color: */
                if ( (m_aucSculptImage[index + 0] == (byte)0) &&
                     (m_aucSculptImage[index + 1] == (byte)0) &&
                     (m_aucSculptImage[index + 2] == (byte)0)    )
                {
                    invertSculpt( iX, iY, m_aiColorSculpt );
                }
                else {
                    invertSculpt( iX, iY, null );
                }
            }
        }
    }

    /**
     * Return true if the sculpt image has been drawn.
     * @return true if the sculpt image has been drawn.
     */
    public boolean IsSculptDrawn()
    {
        return m_bSculptDrawn;
    }

    /* (non-Javadoc)
     * @see java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
     */
    @Override
	public void mouseClicked(MouseEvent kEvent) {}


    /**
     * Invoked when the mouse is dragged while a button is held down. If this occurs while sculpting is enabled, then
     * the mouse positions are stored and the sculpt outline is drawn on the canvas, using the previously recorded mouse
     * position and the current mouse position as the start and end points of a 2D line on the canvas.
     *
     * @param  kEvent  the mouse event generated by a mouse drag
     */
    @Override
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
                        line(iX, iY, m_iPreviousX, m_iPreviousY, m_aiColorSculpt);
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
                if (drawShape == RECTANGLE) {

                    line(m_iXMin, m_iYMax, m_iXMax, m_iYMax, null);
                    line(m_iXMax, m_iYMax, m_iXMax, m_iYMin, null);
                    line(m_iXMax, m_iYMin, m_iXMin, m_iYMin, null);
                    line(m_iXMin, m_iYMin, m_iXMin, m_iYMax, null);

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
                    line(m_iXMin, m_iYMax, m_iXMax, m_iYMax, m_aiColorSculpt);
                    line(m_iXMax, m_iYMax, m_iXMax, m_iYMin, m_aiColorSculpt);
                    line(m_iXMax, m_iYMin, m_iXMin, m_iYMin, m_aiColorSculpt);
                    line(m_iXMin, m_iYMin, m_iXMin, m_iYMax, m_aiColorSculpt);
                }
            }
        }
    }

    /* (non-Javadoc)
     * @see java.awt.event.MouseListener#mouseEntered(java.awt.event.MouseEvent)
     */
    @Override
	public void mouseEntered(MouseEvent kEvent) { }

    /**
     * Invoked when the mouse leaves a component. This function captures the mouseExited event when the button is
     * pressed and the sculpt outline is being drawn, so that the outline is contained within the bounds of the canvas.
     *
     * @param  kEvent  the mouse event generated by a mouse exit
     */
    @Override
	public void mouseExited(MouseEvent kEvent) {

        if (m_bSculptEnabled && m_bMousePressed) {
            processMouseReleased(m_iPreviousX, m_iPreviousY);
            m_bMousePressed = false;
        }
    }

    /* (non-Javadoc)
     * @see java.awt.event.MouseMotionListener#mouseMoved(java.awt.event.MouseEvent)
     */
    @Override
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
    @Override
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
                setSculpt( iX, iY, m_aiColorSculpt );
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
    @Override
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
                    line(iX, iY, m_iPreviousX, m_iPreviousY, m_aiColorSculpt);

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
     * Creates save dialog so that the image can be saved // This should be moved to imageModel.save();
     *
     * @param   options     File-write options.
     * @param   filterType  only used if >= 0
     *
     * @return  true on successful write, false otherwise.
     */
    public boolean save(FileWriteOptions options, int filterType) {
        String fileName = null;
        String extension = null;
        String directory = null;
        String suffix = null;
        int fileType = FileUtility.UNDEFINED;
        ModelImage img = null;
        ViewImageFileFilter vFilter = null;

        int i;

        if (m_kImageA != null) {
            img = m_kImageA;
        } else {
            return false;
        }

        if (options.isSaveAs()) {

            // save into its own subdirectory when on SaveAs.
            // (preferrably used in multi-file formats., ie DICOM)
            options.setSaveInSubdirectory(true);

            if (options.isSet()) {
                fileName = options.getFileName();
                directory = options.getFileDirectory();
            } else {

                try {
                    ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, true);

                
                        JFileChooser chooser = fileChooser.getFileChooser();

                        // chooser.setName("Save image as");
                        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
                        } else {
                            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
                        }

                        if (filterType >= 0) {
                            chooser.addChoosableFileFilter(new ViewImageFileFilter(filterType));
                        } else {
                            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.ALL));
                            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
                            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
                        }

                        int returnVal = chooser.showSaveDialog(null);

                        if (returnVal == JFileChooser.APPROVE_OPTION) {
                            fileName = chooser.getSelectedFile().getName();

                            if (filterType >= 0) {
                                i = fileName.lastIndexOf('.');

                                if ((i > 0) && (i < (fileName.length() - 1))) {
                                    extension = fileName.substring(i + 1).toLowerCase();
                                    vFilter = new ViewImageFileFilter(filterType);

                                    if (!vFilter.accept(extension)) {
                                        MipavUtil.displayError("Extension does not match filter type");

                                        return false;
                                    }
                                } // if ( i > 0 && i < fileName.length() - 1 )
                                else if (i < 0) {

                                    switch (filterType) {

                                        case ViewImageFileFilter.AVI:
                                            fileName = fileName + ".avi";
                                            break;

                                        case ViewImageFileFilter.VOI:
                                            fileName = fileName + ".voi";
                                            break;

                                        case ViewImageFileFilter.FUNCT:
                                            fileName = fileName + ".fun";
                                            break;

                                        case ViewImageFileFilter.LUT:
                                            fileName = fileName + ".lut";
                                            break;

                                        case ViewImageFileFilter.PLOT:
                                            fileName = fileName + ".plt";
                                            break;

                                        case ViewImageFileFilter.CLASS:
                                            fileName = fileName + ".class";
                                            break;

                                        case ViewImageFileFilter.SCRIPT:
                                            fileName = fileName + ".sct";
                                            break;

                                        case ViewImageFileFilter.SURFACE:
                                            fileName = fileName + ".sur";
                                            break;

                                        case ViewImageFileFilter.FREESURFER:
                                            fileName = fileName + ".asc";
                                            break;
                                    }
                                } // else if (i < 0)
                            } // if (filterType >= 0)

                            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                            ViewUserInterface.getReference().setDefaultDirectory(directory);
                        } else {
                            return false;
                        }
                    
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: ViewJFrameBase.save");
                    Preferences.debug("Out of memory: ViewJFrameBase.save\n", Preferences.DEBUG_COMMS);

                    return false;
                }
            }

        } else {
            fileName = img.getFileInfo(0).getFileName();
            directory = img.getFileInfo(0).getFileDirectory();
        }

        options.setFileName(fileName);
        options.setFileDirectory(directory);

        if (!options.isSaveAs()) {

            if (img.getNDims() == 3) {
                options.setBeginSlice(0);
                options.setEndSlice(img.getExtents()[2] - 1);
            } else if (img.getNDims() == 4) {
                options.setBeginSlice(0);
                options.setEndSlice(img.getExtents()[2] - 1);
                options.setBeginTime(0);
                options.setEndTime(img.getExtents()[3] - 1);
            }
        }

        if (fileName != null) {
            FileIO fileIO = new FileIO();
            fileIO.writeImage(img, options);
        }

        // if the SaveAllOnSave preference flag is set, then
        // save all the files associated with this image (VOIs, LUTs, etc.)
        if (Preferences.is(Preferences.PREF_SAVE_ALL_ON_SAVE)) {

            // Since the options may have changed the filename
            // and the directory --- get new fileName and directory
            // from options
            String fName = options.getFileName(); // if you use the name from img, then DICOM has funny names
            String filebase;
            int ind = fName.lastIndexOf(".");

            if (ind > 0) {
                filebase = fName.substring(0, fName.lastIndexOf("."));
            } else {
                filebase = new String(fName);
            }

            if (options.getFileType() == FileUtility.DICOM) {
                int newIndex = filebase.length();

                for (i = filebase.length() - 1; i >= 0; i--) {
                    char myChar = filebase.charAt(i);

                    if (Character.isDigit(myChar)) {
                        newIndex = i;
                    } else {
                        break;
                    } // as soon as something is NOT a digit, leave loop
                }

                if (newIndex > 0) {
                    filebase = filebase.substring(0, newIndex);
                }
            }
        }

        // set the new fileName and directory in the fileInfo for the img -- so that it's
        // updated correctly in memory as well -- don't move this before the saveAllOnSave loop --
        // that needs to look at the former settings!
        FileInfoBase[] fileInfo = img.getFileInfo();

        suffix = FileUtility.getExtension(fileName);
        fileType = FileUtility.getFileType(fileName, directory, false);


        // now, get rid of any numbers at the end of the name (these
        // are part of the dicom file name, but we only want the 'base'
        // part of the name
        String baseName = new String(fileName);

        if (fileType == FileUtility.DICOM) {
            int index = fileName.lastIndexOf(".");

            if (index > 0) {
                baseName = fileName.substring(0, index);
            }

            int newIndex = baseName.length();

            for (i = baseName.length() - 1; i >= 0; i--) {
                char myChar = baseName.charAt(i);

                if (Character.isDigit(myChar)) {
                    newIndex = i;
                } else {
                    break;
                } // as soon as something is NOT a digit, leave loop
            }

            if (newIndex > 0) {
                baseName = baseName.substring(0, newIndex);
            }

            fileName = new String(baseName + ".dcm");

            if (!directory.endsWith(baseName)) {
                directory = new String(directory + baseName + File.separator);
            }
        }

        for (i = 0; i < fileInfo.length; i++) {
            fileInfo[i].setFileDirectory(directory);

            if (fileType == FileUtility.DICOM) {
                fileInfo[i].setFileName(baseName + (i + 1) + ".dcm");
            } else {
                fileInfo[i].setFileName(fileName);
            }

            fileInfo[i].setFileSuffix(suffix);
            // fileInfo[i].setFileFormat (fileType);
        }

        return true;
    }

    /**
     * Set the shape of the drawing sculptor.
     *
     * @param  shape  0 for LINES, 1 for RECTANGLE
     */
    public void setDrawingShape(int shape) {
        drawShape = shape;
        if ( drawShape == RECTANGLE )
        {
            backupSculptImage();
        }
    }

    /**
     * Set the ModelImage data
     * @param kImageA ModelImage A
     * @param kImageB ModelImage B (or null)
     */
    public void setImage( ModelImage kImageA, ModelImage kImageB )
    {
        m_kImageA = kImageA;
        if ( m_kImageBackupA != null )
        {
            m_kImageBackupA.disposeLocal(true);
            m_kImageBackupA = null;
        }
        m_kImageBackupA = (ModelImage)m_kImageA.clone("sculpt_backupA");
        if ( kImageB != null )
        {
            m_kImageB = kImageB;
            if ( m_kImageBackupB != null )
            {
                m_kImageBackupB.disposeLocal(true);
                m_kImageBackupB = null;
            }
            m_kImageBackupB = (ModelImage)m_kImageB.clone("sculpt_backupB");
        }
    }

    public void setSculptDrawn(boolean value)
    {
        m_bSculptDrawn = value;
    }

    public void setSculptImage(byte[] abImage)
    {
        m_aucSculptImage = abImage;
    }

    /**
     * Set the texture data byte[] for ModelImage A
     * @param data the texture data byte[] for ModelImage A
     */
    public void setTextureImageDataA( byte[] data )
    {
        m_aucTextureImageDataA = data;
    }


    /**
     * Set the texture data byte[] for ModelImage B
     * @param data the texture data byte[] for ModelImage B
     */
    public void setTextureImageDataB( byte[] data )
    {
        m_aucTextureImageDataB = data;
    }

    /**
     * Set the World-view-projection matrix from the renderer. Used to apply the sculpt image to the volume data.
     * @param kMatrix World-view-projection matrix
     */
    public void setWVPMatrix( Matrix4f kMatrix )
    {
        m_kWVPMatrix = kMatrix;
    }

    /**
     * Undo the sculpt.
     */
    public void undoSculpt(int iTSlice)
    {
        float fImageMaxA = (float)m_kImageA.getMax();
        float fImageMinA = (float)m_kImageA.getMin();
        int iXBound = m_kImageA.getExtents()[0];
        int iYBound = m_kImageA.getExtents()[1];
        int iZBound = m_kImageA.getExtents()[2];
        int iSliceSize = iXBound * iYBound;
        int iVolumeSize = iSliceSize * iZBound;

        float fImageMaxB = 0, fImageMinB = 0;
        if ( m_kImageB != null )
        {
            fImageMaxB = (float)m_kImageB.getMax();
            fImageMinB = (float)m_kImageB.getMin();
        }

        for (int iZ = 0; iZ < iZBound; iZ++)
        {
            for (int iY = 0; iY < iYBound; iY++)
            {
                for (int iX = 0; iX < iXBound; iX++)
                {
                    int iDataIndex = iZ * (iSliceSize) + (iY * iXBound) + iX;
                    int iVolIndex = iDataIndex + (iTSlice * iVolumeSize);
                    iDataIndex += (iTSlice * iVolumeSize);
                    backupData( m_kImageBackupA, m_kImageA, iDataIndex );
                    if ( m_kImageA.isColorImage() )
                    {
                        if ( m_aucTextureImageDataA != null )
                        {
                            m_aucTextureImageDataA[iDataIndex*4 +0] = m_kImageA.get(iVolIndex*4 +1).byteValue();
                            m_aucTextureImageDataA[iDataIndex*4 +1] = m_kImageA.get(iVolIndex*4 +2).byteValue();
                            m_aucTextureImageDataA[iDataIndex*4 +2] = m_kImageA.get(iVolIndex*4 +3).byteValue();
                            m_aucTextureImageDataA[iDataIndex*4 +3] = (byte)255;
                        }
                        else
                        {}
                    }
                    
                    else
                    {
                        float fValue = m_kImageA.get(iVolIndex).floatValue();
                        fValue = (fValue - fImageMinA)/(fImageMaxA - fImageMinA);
                        byte bValue = (byte)(255.0f * fValue);
                        if ( m_aucTextureImageDataA != null )
                        {
                            m_aucTextureImageDataA[iDataIndex*4 +0] = bValue;
                        }
                    }

                    if ( m_kImageB != null )
                    {
                        backupData( m_kImageBackupB, m_kImageB, iDataIndex );
                        if ( m_kImageB.isColorImage() )
                        {
                            if ( m_aucTextureImageDataB != null )
                            {
                                m_aucTextureImageDataB[iDataIndex*4 +0] = m_kImageB.get(iVolIndex*4 +1).byteValue();
                                m_aucTextureImageDataB[iDataIndex*4 +1] = m_kImageB.get(iVolIndex*4 +2).byteValue();
                                m_aucTextureImageDataB[iDataIndex*4 +2] = m_kImageB.get(iVolIndex*4 +3).byteValue();
                                m_aucTextureImageDataB[iDataIndex*4 +3] = (byte)255;
                            }
                        }
                        else
                        {
                            float fValue = m_kImageB.get(iVolIndex).floatValue();
                            fValue = (fValue - fImageMinB)/(fImageMaxB - fImageMinB);
                            byte bValue = (byte)(255.0f * fValue);
                            if ( m_aucTextureImageDataB != null )
                            {
                                m_aucTextureImageDataB[iDataIndex*4 +0] = bValue;
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Backup ModelImage data.
     * @param kImage original ModelImage
     * @param kImageBackup backup of ModelImage
     * @param iDataIndex index of the position in the ModelImage to backup
     */
    private void backupData( ModelImage kImage, ModelImage kImageBackup, int iDataIndex )
    {
        if ( !kImage.isColorImage() )
        {
            kImageBackup.set(iDataIndex, kImage.get(iDataIndex));
        }
        else
        {
            for ( int c = 0; c < 4; c++ )
            {
                kImageBackup.set(iDataIndex * 4 + c, kImage.get(iDataIndex * 4 + c));
            }
        }
    }


    /**
     * Backup the sculpt image.
     */
    private void backupSculptImage()
    {
        /* Loop over the pixels in the sculpt buffer: */
        for (int iY = 0; iY < m_iSculptImageHeight; iY++)
        {
            for (int iX = 0; iX < m_iSculptImageWidth; iX++)
            {
                int index = (iY * m_iSculptImageWidth + iX)*4;
                m_aucSavedImage[index + 0] = m_aucSculptImage[index + 0];
                m_aucSavedImage[index + 1] = m_aucSculptImage[index + 1];
                m_aucSavedImage[index + 2] = m_aucSculptImage[index + 2];
                m_aucSavedImage[index + 3] = m_aucSculptImage[index + 3];
            }
        }
    }

    /**
     * Get the sculpt on/off at the position.
     * @param iX x-position
     * @param iY y-position
     * @return true if the position should be sculpted, false otherwise.
     */
    private boolean getSculpt( int iX, int iY )
    {
        int index = (iY * m_iSculptImageWidth + iX)*4;
        if ( (m_aucSculptImage[index + 0] == 0) &&
             (m_aucSculptImage[index + 1] == 0) &&
             (m_aucSculptImage[index + 2] == 0) &&
             (m_aucSculptImage[index + 3] == 0)     )
        {
            return false;
        }
        return true;                
    }

    /**
     * Invert the sculpt image at the position
     * @param iX x-position
     * @param iY y-position
     * @param aiColors sculpt color.
     */
    private void invertSculpt( int iX, int iY, int[] aiColors )
    {
        int index = (iY * m_iSculptImageWidth + iX)*4;
        if ( aiColors != null )
        {
            m_aucSculptImage[index + 0] = (byte)aiColors[0];
            m_aucSculptImage[index + 1] = (byte)aiColors[1];
            m_aucSculptImage[index + 2] = (byte)aiColors[2];
            m_aucSculptImage[index + 3] = (byte)200;
        }
        else
        {
            m_aucSculptImage[index + 0] = (byte)0;
            m_aucSculptImage[index + 1] = (byte)0;
            m_aucSculptImage[index + 2] = (byte)0;
            m_aucSculptImage[index + 3] = (byte)0;
        }
        m_bSculptDrawn = true;
    }

    /**
     * Sculpt the ModelImage and Textures at the index.
     * @param kImage ModelImage
     * @param iDataIndex index to sculpt
     * @param aucTextureImageData byte[] Texture
     * @param afTextureImageData float[] Texture
     */
    private void sculptData( ModelImage kImage, int iDataIndex, int iTextureIndex,
                             byte[] aucTextureImageData )
    {
        if ( !kImage.isColorImage() )
        {
            kImage.set(iDataIndex, (float)kImage.getMin());
        }
        else
        {
            kImage.set(iDataIndex*4 + 1, (float)kImage.getMinR());
            kImage.set(iDataIndex*4 + 2, (float)kImage.getMinG());
            kImage.set(iDataIndex*4 + 3, (float)kImage.getMinB());
        }
        
        if ( aucTextureImageData != null )
        {
            if ( kImage.isColorImage() )
            {
                aucTextureImageData[iTextureIndex * 4 + 0] = (byte)0;
                aucTextureImageData[iTextureIndex * 4 + 1] = (byte)0;
                aucTextureImageData[iTextureIndex * 4 + 2] = (byte)0;
                aucTextureImageData[iTextureIndex * 4 + 3] = (byte)0;
            }
            else
            {
                aucTextureImageData[iTextureIndex * 4 + 0] = (byte)0;
            }
        }

    }

    /**
     * Set the sculpt image
     * @param iX x-position
     * @param iY y-position
     * @param aiColors sculpt color
     */
    private void setSculpt( int iX, int iY, int[] aiColors )
    {
        int index = (iY * m_iSculptImageWidth + iX)*4;
        if ( aiColors != null )
        {
            m_aucSculptImage[index + 0] = (byte)aiColors[0];
            m_aucSculptImage[index + 1] = (byte)aiColors[1];
            m_aucSculptImage[index + 2] = (byte)aiColors[2];
            m_aucSculptImage[index + 3] = (byte)200;
        }
        else
        {
            if ( drawShape == RECTANGLE )
            {
                m_aucSculptImage[index + 0] = m_aucSavedImage[index + 0];
                m_aucSculptImage[index + 1] = m_aucSavedImage[index + 1];
                m_aucSculptImage[index + 2] = m_aucSavedImage[index + 2];
                m_aucSculptImage[index + 3] = m_aucSavedImage[index + 3];
            }
            else
            {
                m_aucSculptImage[index + 0] = (byte)0;
                m_aucSculptImage[index + 1] = (byte)0;
                m_aucSculptImage[index + 2] = (byte)0;
                m_aucSculptImage[index + 3] = (byte)0;
            }
        }
        m_bSculptDrawn = true;
    }

    /**
     * Draw rectangle shape object.
     */
    protected void drawRectangleArea() {

        if (!(m_iLastX < m_iFirstX)) {
            line(m_iXMin, m_iYMax, m_iXMax, m_iYMax, m_aiColorSculpt);
            m_aiXPoints[m_iNumberPoints] = m_iXMin;
            m_aiYPoints[m_iNumberPoints] = m_iYMax;
            m_iNumberPoints++;
            m_aiXPoints[m_iNumberPoints] = m_iXMax;
            m_aiYPoints[m_iNumberPoints] = m_iYMax;
            m_iNumberPoints++;
        }

        line(m_iXMax, m_iYMax, m_iXMax, m_iYMin, m_aiColorSculpt);
        m_aiXPoints[m_iNumberPoints] = m_iXMax;
        m_aiYPoints[m_iNumberPoints] = m_iYMax;
        m_iNumberPoints++;
        m_aiXPoints[m_iNumberPoints] = m_iXMax;
        m_aiYPoints[m_iNumberPoints] = m_iYMin;
        m_iNumberPoints++;

        line(m_iXMax, m_iYMin, m_iXMin, m_iYMin, m_aiColorSculpt);
        m_aiXPoints[m_iNumberPoints] = m_iXMax;
        m_aiYPoints[m_iNumberPoints] = m_iYMin;
        m_iNumberPoints++;
        m_aiXPoints[m_iNumberPoints] = m_iXMin;
        m_aiYPoints[m_iNumberPoints] = m_iYMin;
        m_iNumberPoints++;

        line(m_iXMin, m_iYMin, m_iXMin, m_iYMax, m_aiColorSculpt);
        m_aiXPoints[m_iNumberPoints] = m_iXMin;
        m_aiYPoints[m_iNumberPoints] = m_iYMin;
        m_iNumberPoints++;
        m_aiXPoints[m_iNumberPoints] = m_iXMin;
        m_aiYPoints[m_iNumberPoints] = m_iYMax;
        m_iNumberPoints++;

        if ((m_iLastX < m_iFirstX)) {
            line(m_iXMin, m_iYMax, m_iXMax, m_iYMax, m_aiColorSculpt);
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
                    setSculpt( iX, iY, m_aiColorSculpt );
                }
            }

            iColumn++;
        }
    }

    /**
     * This function draws a 2D line on the canvas image -- on top of the currently rendered image. To implement the line
     * drawing I use the midpoint line algorithm, in Foley and van Dam, originally Bresenham's algorithm. The first part
     * of the function sets up the step sizes for x and y depending on what type of line is being drawn. The second part
     * loops over the line, drawing pixels into the canvas image.
     *
     * @param  iX0  mouse x0
     * @param  iY0  mouse y0
     * @param  iX1  mouse x1
     * @param  iY1  mouse y1
     * @param aiColors line color.
     */
    protected void line(int iX0, int iY0, int iX1, int iY1, int[] aiColors ) {

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
            setSculpt( iX, iY, aiColors );

            iY = iY + iYStep;

            while (iY != iY1) {
                setSculpt( iX, iY, aiColors );

                iY = iY + iYStep;
            }
        }
        /* The line is horixontal. */
        else if (iSlopeY == 0) {
            setSculpt( iX, iY, aiColors );

            iX = iX + iXStep;

            while (iX != iX1) {
                setSculpt( iX, iY, aiColors );

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
            setSculpt( iX, iY, aiColors );

            iX = iX + iXStep;

            if (iD > 0) {
                iD = iD + iIncrNE;
                iY = iY + iYStep;
            } else {
                iD = iD + iIncrE;
            }

            while ((iX != iX1) || (iY != iY1)) {
                setSculpt( iX, iY, aiColors );

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
            setSculpt( iX, iY, aiColors );

            iY = iY + iYStep;

            if (iD > 0) {
                iX = iX + iXStep;
                iD = iD + iIncrNE;
            } else {
                iD = iD + iIncrE;
            }

            while ((iX != iX1) || (iY != iY1)) {
                setSculpt( iX, iY, aiColors );

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
     * outline is being drawn and the mouse is dragged outside the canvas area. The function closes the outline, using
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
            line(iX, iY, m_iFirstX, m_iFirstY, m_aiColorSculpt);

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

        aaiCrossingPoints = null;
        aiNumCrossings = null;

        backupSculptImage();
    }

    /**
     * Sorts the edge crossing points in place.
     *
     * @param  aiList        list of positions
     * @param  iNumElements  number of positions.
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
}
