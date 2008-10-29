package gov.nih.mipav.view.renderer.WildMagic;

import gov.nih.mipav.MipavCoordinateSystems;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.WindowLevel;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeSlices;

import java.awt.Cursor;
import java.awt.Image;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.io.FileNotFoundException;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLCanvas;
import javax.media.opengl.GLEventListener;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.Rendering.Camera;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.sun.opengl.util.Animator;



/**
 * Class PlaneRenderWM: renders a single dimension of the ModelImage data as a
 * texture-mapped polygon. The PlaneRenderWM class keeps track of whether it is
 * rendering the Axial, Sagittal, or Coronal view of the data.
 *
 */
public class PlaneRender_WM extends GPURenderBase
    implements GLEventListener
{

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2025132936439496099L;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    /** Camera Locations, for rendering the different Axial, Sagittal and Coronal views. */
    Vector3f[] m_akCLoc = { new Vector3f(-1.0f,0.0f,0.0f), new Vector3f(0.0f,-1.0f,0.0f), new Vector3f(0.0f,0.0f,-1.0f) };
    /** Camera Direction, UP, and Right vectors, for rendering the different Axial, Sagittal and Coronal views. */
    Vector3f[] m_akCoords = { new Vector3f(Vector3f.UNIT_X), new Vector3f(Vector3f.UNIT_Y), new Vector3f(Vector3f.UNIT_Z) };
    
    /** Actual image orientation. */
    protected boolean m_bPatientOrientation = true;
    /** Which dimension of the ModelImage to render. */
    protected int m_iPlaneOrientation = 0;

    /** Window-level interface. */
    protected WindowLevel m_kWinLevel;

    /** The image dimensions in x,y,z:. */
    private int[] m_aiLocalImageExtents;

    /** Set of colors used to draw the axis labels. */
    private ColorRGB[][] m_aakColors = { { new ColorRGB(1, 1, 0), new ColorRGB(0, 1, 0), new ColorRGB(1, 0, 0) },
                                         { new ColorRGB(1, 1, 0), new ColorRGB(1, 0, 0), new ColorRGB(0, 1, 0) },
                                         { new ColorRGB(0, 1, 0), new ColorRGB(1, 0, 0), new ColorRGB(1, 1, 0) } };

    /** Axis labels color assignments.  */
    private int[][] m_aaiColorSwap = { { 2, 1, 0 }, { 1, 2, 0 }, { 1, 0, 2 } };

    /** when true, the axis labels (P-> L-> S->) will be drawn */
    private boolean m_bDrawAxes = true;

    /** Turns on drawing of the X,Y bars and the Axis labels:. */
    private boolean m_bDrawXHairs = true;

    /** Change the mouse cursor with the first mouseDrag event */
    private boolean m_bFirstDrag = true;

    /** True when the left mouse has been pressed, set to false when the left
     * mouse button is released. */
    private boolean m_bLeftMousePressed = false;

    /** Flag indicating if the right mouse button is currently pressed
     * down: */
    private boolean m_bRightMousePressed = false;

    /** The current active lookup table: */
    private ModelStorageBase m_kActiveLookupTable;

    /** lower x-bound of the texture-mapped polygon: */
    private float m_fX0;

    /** upper x-bound of the texture-mapped polygon: */
    private float m_fX1;

    /** Numbers dictating the size of the plane based on the extents and
     * resolutions of the image. */
    private float m_fXBox, m_fYBox, m_fMaxBox;

    /** Width of the texture-mapped polygon: */
    private float m_fXRange;

    /** X direction mouse translation. */
    private float m_fXTranslate = 0.0f;

    /** lower y-bound of the texture-mapped polygon: */
    private float m_fY0;

    /** upper y-bound of the texture-mapped polygon: */
    private float m_fY1;

    /** Height of the texture-mapped polygon: */
    private float m_fYRange;

    /** Y direction mouse translation. */
    private float m_fYTranslate = 0.0f;

    /** Image scaling from Zoom:. */
    private float m_fZoomScale = 1.0f;
    /** Which slice is currently displayed in the XY plane. */
    private int m_iSlice;
    /** Model coordinates for current cursor position. */
    private float[] m_afModelPosition = new float[3];

    /** Current active image for manipulating the LUT by dragging with the
     * right-mouse down. */
    private ModelImage m_kActiveImage;
    /** x-axis label: */
    private String m_kLabelX = new String("X");
    private String m_kLabelXDisplay = new String("X");

    /** y-axis label: */
    private String m_kLabelY = new String("Y");

    /** x-axis arrow */
    private TriMesh[] m_kXArrow;
    /** y-axis arrow */
    private TriMesh[] m_kYArrow;
    
    /** Drawing the axis arrows in screen-space. */
    private Camera m_spkScreenCamera;
    private int m_iLabelX_SpacingX;
    private int m_iLabelX_SpacingY;
    private int m_iLabelY_SpacingX;
    private int m_iLabelY_SpacingY;
    private boolean m_bUpdateSpacing = false;

    /** ModelImage axis orientation. */
    private int[] m_aiAxisOrder;
    /** ModelImage axis flip. */
    private boolean[] m_abAxisFlip;
    /** Default field of view, changes with mouse zoom. */
    private float m_fUpFOV = 65f;
    /** For zooming with the mouse. */
    private float m_fMouseY;


    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default PlaneRender interface.
     */
    public PlaneRender_WM ()
    {
        super();
    }

    /**
     * @param kParent
     * @param kAnimator
     * @param kVolumeImageA
     * @param iPlane
     */
    public PlaneRender_WM(VolumeTriPlanarInterface kParent, Animator kAnimator, 
                          VolumeImage kVolumeImageA,
                          int iPlane)
    {
        super();
        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                                          m_eBuffering, m_eMultisampling,
                                           m_iWidth, m_iHeight );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );   

        m_kAnimator = kAnimator;
        m_kVolumeImageA = kVolumeImageA;
        m_kParent = kParent;
        m_iPlaneOrientation = iPlane;

        setOrientation();
        m_kWinLevel = new WindowLevel();
        
        ModelImage kImageA = m_kVolumeImageA.GetImage();
        float fMaxX = (kImageA.getExtents()[0] - 1) * kImageA.getFileInfo(0).getResolutions()[0];
        float fMaxY = (kImageA.getExtents()[1] - 1) * kImageA.getFileInfo(0).getResolutions()[1];
        float fMaxZ = (kImageA.getExtents()[2] - 1) * kImageA.getFileInfo(0).getResolutions()[2];

        m_fMax = fMaxX;
        if (fMaxY > m_fMax) {
            m_fMax = fMaxY;
        }
        if (fMaxZ > m_fMax) {
            m_fMax = fMaxZ;
        }
        m_fX = fMaxX/m_fMax;
        m_fY = fMaxY/m_fMax;
        m_fZ = fMaxZ/m_fMax;
    }
    
    /**
     * Adds the VolumeSlices object to the display list for rendering.
     * @param kVolumeSlice.
     */
    public void addSlices( VolumeSlices kVolumeSlice  )
    {
        m_kDisplayList.add(kVolumeSlice);
    }

    /**
     * Closes the frame.
     */
    public void close() {
        disposeLocal();
    }

    
    /* (non-Javadoc)
     * @see javax.media.opengl.GLEventListener#display(javax.media.opengl.GLAutoDrawable)
     */
    public void display(GLAutoDrawable arg0) {
        if ( !m_bModified )
        {
            return;
        }
        
        if ( m_kVolumeImageA == null ) {
        	return;
        }
        if ( !m_bInit )
        {
            init(arg0);
        }
        m_bModified = false;   
        m_pkRenderer.ClearBuffers();
        if (m_pkRenderer.BeginScene())
        {
            for ( int i = 0; i < m_kDisplayList.size(); i++ )
            {
                boolean bDisplaySave = m_kDisplayList.get(i).GetDisplay();
                Matrix3f kSave = new Matrix3f(m_kDisplayList.get(i).GetScene().Local.GetRotate());
                m_kDisplayList.get(i).GetScene().Local.SetRotateCopy(Matrix3f.IDENTITY);
                m_kDisplayList.get(i).SetDisplay(true);
                
                float fBlend = 1;
                boolean[] bShowBoundingBox = new boolean[]{ true, true, true };
                VolumeSlices kSlices = null;
                if ( m_kDisplayList.get(i) instanceof VolumeSlices )
                {
                    kSlices = (VolumeSlices)m_kDisplayList.get(i);
                    fBlend = kSlices.GetSliceOpacity(m_iPlaneOrientation);
                    kSlices.SetSliceOpacity(m_iPlaneOrientation, 1);
                    kSlices.ShowSurface(true);
                    for ( int j = 0; j < 3; j++ )
                    {
                        bShowBoundingBox[j] = kSlices.GetShowBoundingBox(j);
                        kSlices.ShowBoundingBox(j, m_bDrawXHairs);
                    }
                }

                m_kDisplayList.get(i).Render( m_pkRenderer, m_kCuller );

                m_kDisplayList.get(i).GetScene().Local.SetRotateCopy(kSave);
                m_kDisplayList.get(i).SetDisplay(bDisplaySave);

                if ( kSlices != null )
                {
                    kSlices.SetSliceOpacity(m_iPlaneOrientation, fBlend);     
                    for ( int j = 0; j < 3; j++ )
                    {
                        kSlices.ShowBoundingBox(j, bShowBoundingBox[j]);
                    }
                    kSlices.ShowSurface(false);
                }
            }
            drawAxes();
            m_pkRenderer.EndScene();
        }
        m_pkRenderer.DisplayBackBuffer();

    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.GPURenderBase#displayChanged(javax.media.opengl.GLAutoDrawable, boolean, boolean)
     */
    public void displayChanged(GLAutoDrawable arg0, boolean arg1, boolean arg2)
    {
        m_bModified = true;
    }   
    
    /**
     * Clean memory.
     */
    public void disposeLocal() {
        for (int i = 0; i < 3; i++)
        {
            for (int j = 0; j < 3; j++)
            {
                m_aakColors[i][j] = null;
            }
            m_aakColors[i] = null;
        }
        m_aakColors = null;

        m_kLabelX = null;
        m_kLabelY = null;
    }
        
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.GPURenderBase#GetCanvas()
     */
    public GLCanvas GetCanvas()
    {
        return ((OpenGLRenderer)m_pkRenderer).GetCanvas();
    }

    /* (non-Javadoc)
     * @see javax.media.opengl.GLEventListener#init(javax.media.opengl.GLAutoDrawable)
     */
    public void init(GLAutoDrawable arg0) {
    	if ( m_kVolumeImageA == null ) {
            return;
        }

        m_bInit = true;
        
        arg0.setAutoSwapBufferMode( false );

        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        ((OpenGLRenderer)m_pkRenderer).InitializeState();

        super.OnInitialize();

        // set up camera
        m_spkCamera.SetFrustum(60.0f,1.0f,0.1f,100.0f);
        Vector3f kCLoc = new Vector3f(m_akCLoc[m_aiAxisOrder[2]]);
        Vector3f kCDir = new Vector3f(m_akCoords[m_aiAxisOrder[2]]);
        Vector3f kCUp = new Vector3f(m_akCoords[m_aiAxisOrder[1]]);
        Vector3f kCRight = new Vector3f(m_akCoords[m_aiAxisOrder[0]]);
        if ( m_abAxisFlip[2] )
        {
            kCLoc.Scale(-1);
            kCDir.Scale(-1);
        }
        if ( m_abAxisFlip[1] )
        {
            kCUp.Scale(-1);
        }
        if ( m_abAxisFlip[0] )
        {
            kCRight.Scale(-1);
        }
        //invert y-axis
        kCUp.Scale(-1);
        m_spkCamera.SetFrame( kCLoc, kCDir, kCUp, kCRight );
        CreateScene();

        // initial culling of scene
        m_kCuller.SetCamera(m_spkCamera);

        InitializeCameraMotion(.05f,0.001f);
        InitializeObjectMotion(m_spkScene);

        m_kAnimator.add( GetCanvas() );
    }
    
    /* (non-Javadoc)
     * @see WildMagic.LibApplications.OpenGLApplication.JavaApplication3D#mouseDragged(java.awt.event.MouseEvent)
     */
    public void mouseDragged(MouseEvent kEvent) {
        //super.mouseDragged(kEvent);
        
        /* If the right mouse button is pressed and
         * dragged. processRightMouseDrag updates the HistoLUT window and
         * level (contrast and brightness) */

        if (m_bRightMousePressed && !kEvent.isShiftDown()) {
            processRightMouseDrag(kEvent);
        }

        /* Dragging the mouse with the left-mouse button held down changes the
         * positions of the X and Y cross bars, and therefor the ZSlice positions of the associated PlaneRenderWM objects
         * and the TriPlanar Surface. The new positions are calculated and passed onto the parent frame.
         */

        else if (m_bLeftMousePressed && !kEvent.isShiftDown()) {
            processLeftMouseDrag(kEvent);
        }
        else
        {
            if ( kEvent.getY() < m_fMouseY )
            {
                m_fUpFOV += 2.0f;
            }
            else if ( kEvent.getY() > m_fMouseY )
            {
                m_fUpFOV -= 2.0f;
            }
            m_fMouseY = kEvent.getY();
            m_spkCamera.SetFrustum(m_fUpFOV,m_iWidth/(float)m_iHeight,1f,5.0f);
            m_pkRenderer.OnFrustumChange();
            m_bModified = true;
        }

    }

    /* (non-Javadoc)
     * @see WildMagic.LibApplications.OpenGLApplication.JavaApplication3D#mousePressed(java.awt.event.MouseEvent)
     */
    public void mousePressed(MouseEvent kEvent) {
        super.mousePressed(kEvent);
        /* If the button pressed is the left mouse button: */
        if ((kEvent.getButton() == MouseEvent.BUTTON1) && !kEvent.isShiftDown()) {
            m_bLeftMousePressed = true;
        }

        if ((kEvent.getButton() == MouseEvent.BUTTON3) && !kEvent.isShiftDown()) {
            m_bRightMousePressed = true;
        }
        m_fMouseY = kEvent.getY();
    }

    /* (non-Javadoc)
     * @see WildMagic.LibApplications.OpenGLApplication.JavaApplication3D#mouseReleased(java.awt.event.MouseEvent)
     */
    public void mouseReleased(MouseEvent kEvent) {

        super.mousePressed(kEvent);
        /* If the button pressed is the left mouse button turn off the
         * m_bLeftMousePressed flag: */
        if ((kEvent.getButton() == MouseEvent.BUTTON3) && !kEvent.isShiftDown()) {
            m_bRightMousePressed = false;
        }

        if ((kEvent.getButton() == MouseEvent.BUTTON1) && !kEvent.isShiftDown()) {
            processLeftMouseDrag( kEvent );
            m_bLeftMousePressed = false;
        }
        m_bFirstDrag = true;
        m_kParent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.GPURenderBase#reshape(javax.media.opengl.GLAutoDrawable, int, int, int, int)
     */
    public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight) {
    	if ( m_kVolumeImageA == null ) {
        	return;
        }
    	
        if (iWidth > 0 && iHeight > 0)
        {            
            if ( m_bUpdateSpacing )
            {
                m_iLabelX_SpacingX *= (float)iWidth/(float)m_iWidth;
                m_iLabelX_SpacingY *= (float)iHeight/(float)m_iHeight;
                m_iLabelY_SpacingX *= (float)iWidth/(float)m_iWidth;
                m_iLabelY_SpacingY *= (float)iHeight/(float)m_iHeight;
            }
            m_bUpdateSpacing = true;
            
            
            if (m_pkRenderer != null)
            {
                m_pkRenderer.Resize(iWidth,iHeight);
            }
            
            m_iWidth = iWidth;
            m_iHeight = iHeight;
            m_bModified = true;
            m_spkCamera.Perspective = false;
            m_spkCamera.SetFrustum(m_fUpFOV,m_iWidth/(float)m_iHeight,1f,5.0f);
            m_pkRenderer.OnFrustumChange();
        }
        
    }

    /**
     * Sets the background color for the frame and rendered image.
     *
     * @param  color  RGBA color to use as the background color.
     */
    public void setBackgroundColor( ColorRGBA kColor )
    {
        m_kBackgroundColor = kColor;
        return;
    }

    /**
     * setCenter sets the cursor and slice position for this PlaneRenderWM
     * object, based on the 3D location of the three intersecting ModelImage
     * planes.
     * @param center, the 3D center in FileCoordinates of the three
     * intersecting ModelImage planes.
     */
    public void setCenter( Vector3f center )
    {
        ModelImage kImage = m_kVolumeImageA.GetImage();
        m_bModified = true;
        m_afModelPosition[0] = center.X/(kImage.getExtents()[0] -1);
        m_afModelPosition[1] = center.Y/(kImage.getExtents()[1] -1);
        m_afModelPosition[2] = center.Z/(kImage.getExtents()[2] -1);
        Vector3f patientPt = new Vector3f();
        MipavCoordinateSystems.fileToPatient( center, patientPt, kImage, m_iPlaneOrientation );
        setSlice( patientPt.Z );
    }

    /**
     * Causes re-display.
     * @param bModified
     */
    public void SetModified ( boolean bModified )
    {
        m_bModified = bModified;
    }

    /**
     * Sets the view to Radiological (true) or Neurological (false) view.
     * @param bOn
     */
    public void setRadiologicalView( boolean bOn )
    {
        if ( m_iPlaneOrientation == FileInfoBase.SAGITTAL )
        {
            return;
        }
        
        Vector3f kCLoc = new Vector3f(m_akCLoc[m_aiAxisOrder[2]]);
        Vector3f kCDir = new Vector3f(m_akCoords[m_aiAxisOrder[2]]);
        Vector3f kCUp = new Vector3f(m_akCoords[m_aiAxisOrder[1]]);
        Vector3f kCRight = new Vector3f(m_akCoords[m_aiAxisOrder[0]]);
        if ( m_abAxisFlip[2] )
        {
            kCLoc.Scale(-1);
            kCDir.Scale(-1);
        }
        if ( m_abAxisFlip[1] )
        {
            kCUp.Scale(-1);
        }
        if ( m_abAxisFlip[0] )
        {
            kCRight.Scale(-1);
        }
        //invert y-axis
        kCUp.Scale(-1);
        if ( !bOn )
        {
            kCLoc.Scale(-1);
            kCDir.Scale(-1);
            kCRight.Scale(-1);
        }
        m_spkCamera.SetFrame( kCLoc, kCDir, kCUp, kCRight );
    }

    /**
     * Sets the color for the PlaneRender iView (AXIAL, SAGITTAL, CORONAL) slice.
     *
     * @param  iView  (AXIAL, SAGITTAL, CORONAL)
     * @param  kColor  the new axis color attribute.
     */
    public void setSliceHairColor(int iView, ColorRGB kColor) {
        int iX = 0;
        int iY = 1;
        m_bModified = true;

        m_aakColors[m_iPlaneOrientation][m_aaiColorSwap[m_iPlaneOrientation][iView]] = kColor;
        ColorRGB kXSliceHairColor = m_aakColors[m_iPlaneOrientation][iX];
        ColorRGB kYSliceHairColor = m_aakColors[m_iPlaneOrientation][iY];

        for ( int j = 0; j < 4; j++ )
        {
            m_kXArrow[0].VBuffer.SetColor3( 0, j, kXSliceHairColor );
            m_kYArrow[0].VBuffer.SetColor3( 0, j, kYSliceHairColor );
        }
        for ( int j = 0; j < 3; j++ )
        {
            m_kXArrow[1].VBuffer.SetColor3( 0, j, kXSliceHairColor );
            m_kYArrow[1].VBuffer.SetColor3( 0, j, kYSliceHairColor );
        }
        for ( int i = 0; i < 2; i++ )
        {
            m_kXArrow[i].VBuffer.Release();
            m_kYArrow[i].VBuffer.Release();
        }
    }

    /**
     * Turns displaying the Axis labels on or off:
     *
     * @param bShow when true display the axis lablels, when false hide the
     * axis labels
     */
    public void showAxes(boolean bShow) {
        m_bDrawAxes = bShow;
    }

    /**
     * Turns displaying the X and Y bars on or off:
     *
     * @param  bShow when true show the cross-hairs when false hide the cross-hairs
     */
    public void showXHairs(boolean bShow) {
        if ( m_bDrawXHairs != bShow )
        {
            m_bDrawXHairs = bShow;
        }
    }



    /**
     * Based on the orientation of the ModelImage, sets up the index
     * parameters, m_aiLocalImageExtents[0], m_aiLocalImageExtents[1], and
     * m_aiLocalImageExtents[2], the drawing colors for the z box, x and y
     * bars, and the invert flags.
     *
     * <p>Once setup everything is rendered into an x,y plane where x,y may be
     * any of the original x,y, or z dimensions in the original
     * ModelImage.</p>
     */
    protected void setOrientation() 
    {
        ModelImage kImage = m_kVolumeImageA.GetImage();
        m_aiAxisOrder = MipavCoordinateSystems.getAxisOrder(kImage, m_iPlaneOrientation);
        m_abAxisFlip = MipavCoordinateSystems.getAxisFlip(kImage, m_iPlaneOrientation);
        m_aiLocalImageExtents = kImage.getExtents( m_iPlaneOrientation );

        float[] afResolutions = kImage.getResolutions( 0, m_iPlaneOrientation );

        if ((afResolutions[0] == 0.0f) || (afResolutions[1] == 0.0f) || (afResolutions[2] == 0.0f)) {
            afResolutions[0] = 1.0f;
            afResolutions[1] = 1.0f;
            afResolutions[2] = 1.0f;
        }

        m_fXBox = (m_aiLocalImageExtents[0] - 1) * afResolutions[0];
        m_fYBox = (m_aiLocalImageExtents[1] - 1) * afResolutions[1];

        m_fMaxBox = m_fXBox;

        if (m_fYBox > m_fMaxBox) {
            m_fMaxBox = m_fYBox;
        }

        if ( kImage.getImageOrientation() != FileInfoBase.UNKNOWN_ORIENT )
        {
            if ((m_iPlaneOrientation == FileInfoBase.AXIAL) ||
                (m_iPlaneOrientation == FileInfoBase.CORONAL)) {
                m_kLabelX = new String("L");
            } else {
                m_kLabelX = new String("P");
            }

            if ((m_iPlaneOrientation == FileInfoBase.SAGITTAL) ||
                (m_iPlaneOrientation == FileInfoBase.CORONAL)) {
                m_kLabelY = new String("S");
            } else {
                m_kLabelY = new String("P");
            }
        }
        else
        {
            m_bPatientOrientation = false;
            if ( m_iPlaneOrientation == FileInfoBase.SAGITTAL )
            {
                m_kLabelX = new String("Z");
                m_kLabelY = new String("Y");
            }
            else if ( m_iPlaneOrientation == FileInfoBase.CORONAL )
            {
                m_kLabelX = new String("X");
                m_kLabelY = new String("Z");
            }
        }

        m_kLabelXDisplay = new String( m_kLabelX );
        if ( !m_kVolumeImageA.GetImage().getRadiologicalView() && (m_iPlaneOrientation != FileInfoBase.SAGITTAL) )
        {
            if ( !m_bPatientOrientation )
            {
                m_kLabelXDisplay = new String( "-X" );
            }
            else
            {
                m_kLabelXDisplay = new String( "R" );
            }
        }
        if ( m_iPlaneOrientation == FileInfoBase.AXIAL) 
        {
            m_iLabelX_SpacingX = 50;
            m_iLabelX_SpacingY = 20;
            m_iLabelY_SpacingX = 10;
            m_iLabelY_SpacingY = 68;
        }
        else
        {     
            m_iLabelX_SpacingX = 50;
            m_iLabelX_SpacingY = 10;
            m_iLabelY_SpacingX = 10;
            m_iLabelY_SpacingY = 55;
        }
        
    }
    
    /**
     * Creates the TriMesh data structures for the axis arrows.
     */
    private void CreateLabels()
    {
        // The screen camera is designed to map (x,y,z) in [0,1]^3 to (x',y,'z')
        // in [-1,1]^2 x [0,1].
        m_spkScreenCamera = new Camera();
        m_spkScreenCamera.Perspective = false;
        m_spkScreenCamera.SetFrustum(0.0f,1.0f,0.0f,1.0f,0.0f,1.0f);
        m_spkScreenCamera.SetFrame(Vector3f.ZERO,Vector3f.UNIT_Z,
                Vector3f.UNIT_Y,Vector3f.UNIT_X);

        m_kXArrow = new TriMesh[2];
        m_kYArrow = new TriMesh[2];

        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetCChannels(0,3);
        VertexBuffer pkVBuffer = new VertexBuffer(kAttr,4);
        pkVBuffer.SetPosition3(0, 0.05f,0.05f,0.0f);
        pkVBuffer.SetPosition3(1, 0.15f,0.05f,0.0f);
        pkVBuffer.SetPosition3(2, 0.15f,0.06f,0.0f);
        pkVBuffer.SetPosition3(3, 0.05f,0.06f,0.0f);
        pkVBuffer.SetColor3(0,0,m_aakColors[m_iPlaneOrientation][0] );
        pkVBuffer.SetColor3(0,1,m_aakColors[m_iPlaneOrientation][0] );
        pkVBuffer.SetColor3(0,2,m_aakColors[m_iPlaneOrientation][0] );
        pkVBuffer.SetColor3(0,3,m_aakColors[m_iPlaneOrientation][0] );
        IndexBuffer pkIBuffer = new IndexBuffer(6);
        int[] aiIndex = pkIBuffer.GetData();
        aiIndex[0] = 0;  aiIndex[1] = 1;  aiIndex[2] = 2;
        aiIndex[3] = 0;  aiIndex[4] = 2;  aiIndex[5] = 3;
        m_kXArrow[0] = new TriMesh( new VertexBuffer(pkVBuffer),new IndexBuffer(pkIBuffer));
        m_kXArrow[0].AttachEffect( new VertexColor3Effect() );
        m_kXArrow[0].UpdateGS();
        m_kXArrow[0].UpdateRS();
        m_pkRenderer.LoadResources(m_kXArrow[0]);

        pkVBuffer = new VertexBuffer(kAttr,3);
        pkVBuffer.SetPosition3(0, 0.15f,0.04f,0.0f);
        pkVBuffer.SetPosition3(1, 0.18f,0.055f,0.0f);
        pkVBuffer.SetPosition3(2, 0.15f,0.07f,0.0f);
        pkVBuffer.SetColor3(0,0,m_aakColors[m_iPlaneOrientation][0] );
        pkVBuffer.SetColor3(0,1,m_aakColors[m_iPlaneOrientation][0] );
        pkVBuffer.SetColor3(0,2,m_aakColors[m_iPlaneOrientation][0] );
        pkIBuffer = new IndexBuffer(3);
        aiIndex = pkIBuffer.GetData();
        aiIndex[0] = 0;  aiIndex[1] = 1;  aiIndex[2] = 2;
        m_kXArrow[1] = new TriMesh( new VertexBuffer(pkVBuffer),new IndexBuffer(pkIBuffer));
        m_kXArrow[1].AttachEffect( new VertexColor3Effect() );
        m_kXArrow[1].UpdateGS();
        m_kXArrow[1].UpdateRS();
        m_pkRenderer.LoadResources(m_kXArrow[1]);

        // YArrow:

        pkVBuffer = new VertexBuffer(kAttr,4);
        pkVBuffer.SetPosition3(0, 0.05f,0.05f,0.0f);
        pkVBuffer.SetPosition3(1, 0.06f,0.05f,0.0f);
        pkVBuffer.SetPosition3(2, 0.06f,0.15f,0.0f);
        pkVBuffer.SetPosition3(3, 0.05f,0.15f,0.0f);
        pkVBuffer.SetColor3(0,0,m_aakColors[m_iPlaneOrientation][1] );
        pkVBuffer.SetColor3(0,1,m_aakColors[m_iPlaneOrientation][1] );
        pkVBuffer.SetColor3(0,2,m_aakColors[m_iPlaneOrientation][1] );
        pkVBuffer.SetColor3(0,3,m_aakColors[m_iPlaneOrientation][1] );
        pkIBuffer = new IndexBuffer(6);
        aiIndex = pkIBuffer.GetData();
        aiIndex[0] = 0;  aiIndex[1] = 1;  aiIndex[2] = 2;
        aiIndex[3] = 0;  aiIndex[4] = 2;  aiIndex[5] = 3;
        m_kYArrow[0] = new TriMesh( new VertexBuffer(pkVBuffer),new IndexBuffer(pkIBuffer));
        m_kYArrow[0].AttachEffect( new VertexColor3Effect() );
        m_kYArrow[0].UpdateGS();
        m_kYArrow[0].UpdateRS();
        m_pkRenderer.LoadResources(m_kYArrow[0]);

        pkVBuffer = new VertexBuffer(kAttr,3);
        pkVBuffer.SetPosition3(0, 0.04f,0.15f,0.0f);
        pkVBuffer.SetPosition3(1, 0.07f,0.15f,0.0f);
        pkVBuffer.SetPosition3(2, 0.055f,0.18f,0.0f);
        pkVBuffer.SetColor3(0,0,m_aakColors[m_iPlaneOrientation][1] );
        pkVBuffer.SetColor3(0,1,m_aakColors[m_iPlaneOrientation][1] );
        pkVBuffer.SetColor3(0,2,m_aakColors[m_iPlaneOrientation][1] );
        pkIBuffer = new IndexBuffer(3);
        aiIndex = pkIBuffer.GetData();
        aiIndex[0] = 0;  aiIndex[1] = 1;  aiIndex[2] = 2;
        m_kYArrow[1] = new TriMesh( new VertexBuffer(pkVBuffer),new IndexBuffer(pkIBuffer));
        m_kYArrow[1].AttachEffect( new VertexColor3Effect() );
        m_kYArrow[1].UpdateGS();
        m_kYArrow[1].UpdateRS();
        m_pkRenderer.LoadResources(m_kYArrow[1]);

        if ( m_iPlaneOrientation == FileInfoBase.AXIAL) 
        {
            Vector3f kPosition = new Vector3f();
            Vector3f kDiff = new Vector3f( 0f, 0.9f, 0f );
            for ( int j = 0; j < 4; j++ )
            {
                m_kXArrow[0].VBuffer.GetPosition3(j, kPosition);
                kPosition.Add(kDiff);
                m_kXArrow[0].VBuffer.SetPosition3(j, kPosition );
            }
            for ( int j = 0; j < 3; j++ )
            {
                m_kXArrow[1].VBuffer.GetPosition3(j, kPosition);
                kPosition.Add(kDiff);
                m_kXArrow[1].VBuffer.SetPosition3(j, kPosition );
            }
            for ( int i = 0; i < 2; i++ )
            {
                m_kXArrow[i].VBuffer.Release();
                m_kXArrow[i].UpdateGS();
                m_kXArrow[i].UpdateRS();
                m_pkRenderer.LoadResources(m_kXArrow[i]);
            }

            pkVBuffer = m_kYArrow[0].VBuffer;
            pkVBuffer.SetPosition3(0, 0.05f,0.85f,0.0f);
            pkVBuffer.SetPosition3(1, 0.06f,0.85f,0.0f);
            pkVBuffer.SetPosition3(2, 0.06f,0.95f,0.0f);
            pkVBuffer.SetPosition3(3, 0.05f,0.95f,0.0f);
            pkVBuffer.Release();
            m_kYArrow[0].UpdateGS();
            m_kYArrow[0].UpdateRS();
            m_pkRenderer.LoadResources(m_kYArrow[0]);

            pkVBuffer = m_kYArrow[1].VBuffer;
            pkVBuffer.SetPosition3(0, 0.04f,0.85f,0.0f);
            pkVBuffer.SetPosition3(1, 0.055f,0.82f,0.0f);
            pkVBuffer.SetPosition3(2, 0.07f,0.85f,0.0f);
            pkVBuffer.Release();
            m_kYArrow[1].UpdateGS();
            m_kYArrow[1].UpdateRS();
            m_pkRenderer.LoadResources(m_kYArrow[1]);
        }
    }

    /**
     * Initializes the display parameters.
     */
    private void CreateScene ()
    {        
        m_fX0 = -m_fXBox / m_fMaxBox;
        m_fX1 = m_fXBox / m_fMaxBox;
        m_fY0 = -m_fYBox / m_fMaxBox;
        m_fY1 = m_fYBox / m_fMaxBox;
        
        m_fXRange = m_fX1 - m_fX0;
        m_fYRange = m_fY1 - m_fY0;

        m_iSlice = (m_aiLocalImageExtents[2]) / 2;
              
        CreateLabels();
    }

    /**
     * Called from the display function. Draws the axis arrows.
     */
    private void drawAxes()
    {
        if ( m_bDrawAxes )
        {     

            ColorRGBA kXSliceHairColor =
                new ColorRGBA( m_aakColors[m_iPlaneOrientation][0].R,
                               m_aakColors[m_iPlaneOrientation][0].G,
                               m_aakColors[m_iPlaneOrientation][0].B, 1.0f );

            ColorRGBA kYSliceHairColor =
                new ColorRGBA( m_aakColors[m_iPlaneOrientation][1].R,
                               m_aakColors[m_iPlaneOrientation][1].G,
                               m_aakColors[m_iPlaneOrientation][1].B, 1.0f );
            
            if ( !m_kVolumeImageA.GetImage().getRadiologicalView() && (m_iPlaneOrientation != FileInfoBase.SAGITTAL) )
            {
                if ( !m_bPatientOrientation )
                {
                    m_kLabelXDisplay = new String( "-X" );
                }
                else
                {
                    m_kLabelXDisplay = new String( "R" );
                }
            }
            else if ( m_iPlaneOrientation != FileInfoBase.SAGITTAL )
            {
                if ( !m_bPatientOrientation )
                {
                    m_kLabelXDisplay = new String( "X" );
                }
                else
                {
                    m_kLabelXDisplay = new String( "L" );
                }
            }
            if ( m_iPlaneOrientation == FileInfoBase.AXIAL) 
            {
                m_pkRenderer.Draw( m_iLabelX_SpacingX, m_iLabelX_SpacingY, kXSliceHairColor,m_kLabelXDisplay.toCharArray());
                m_pkRenderer.Draw( m_iLabelY_SpacingX, m_iLabelY_SpacingY, kYSliceHairColor,m_kLabelY.toCharArray());
            }
            else
            {
                m_pkRenderer.Draw( m_iLabelX_SpacingX, m_iHeight - m_iLabelX_SpacingY, kXSliceHairColor,m_kLabelXDisplay.toCharArray());
                m_pkRenderer.Draw( m_iLabelY_SpacingX, m_iHeight - m_iLabelY_SpacingY, kYSliceHairColor,m_kLabelY.toCharArray());               
            }
            m_pkRenderer.SetCamera(m_spkScreenCamera);  
            m_pkRenderer.Draw(m_kXArrow[0]);
            m_pkRenderer.Draw(m_kXArrow[1]);
            m_pkRenderer.Draw(m_kYArrow[0]);
            m_pkRenderer.Draw(m_kYArrow[1]);
            m_pkRenderer.SetCamera(m_spkCamera);
        }
    }

    /* Convert the position in LocalCoordinates (rendering space) into
     * PatientCoordinates:
     * @param localPt, the current point in LocalCoordinates
     * @param patientPt transformed localPt in PatientCoordinates
     */
    private void LocalToPatient( Vector3f localPt, Vector3f patientPt )
    {
        patientPt.X = localPt.X * (m_aiLocalImageExtents[0] - 1);
        patientPt.Y = localPt.Y * (m_aiLocalImageExtents[1] - 1);
        patientPt.Z = localPt.Z * (m_aiLocalImageExtents[2] - 1);
    }

    /**
     * Dragging the mouse with the left-mouse button held down changes the
     * positions of the X and Y cross bars, and therefor the ZSlice positions
     * of the associated PlaneRenderWM objects and the TriPlanar Surface. The
     * new positions are calculated and passed onto the parent frame.
     *
     * @param  kEvent  the mouse event generated by a mouse drag
     */
    private void processLeftMouseDrag(MouseEvent kEvent) {

        /* Calculate the center of the mouse in local coordineates, taking into
         * account zoom and translate: */
        Vector3f localPt = new Vector3f();
        this.ScreenToLocal(kEvent.getX(), kEvent.getY(), localPt);

        /* Tell the ViewJFrameVolumeView parent to update the other
         * PlaneRenderWMs and the SurfaceRender with the changed Z position
         * of the planes with color matching the moved bar: */
        Vector3f patientPt = new Vector3f();
        this.LocalToPatient( localPt, patientPt );
        Vector3f volumePt = new Vector3f();
        MipavCoordinateSystems.patientToFile( patientPt, volumePt, m_kVolumeImageA.GetImage(), m_iPlaneOrientation );
        m_kParent.setSliceFromPlane( volumePt );
    }
    
    /**
     * If the right mouse button is pressed and dragged. processRightMouseDrag
     * updates the HistoLUT window and level (contrast and brightness)
     *
     * @param  kEvent  the mouse event generated by a mouse drag
     */
    private void processRightMouseDrag(MouseEvent kEvent) {
        // make the LUT panel the active panel.
        m_kParent.actionPerformed(new ActionEvent(this, 0, "HistoLUT"));
        /* Get the coordinates of the mouse position in local coordinates: */
        Vector3f localPt = new Vector3f();
        this.ScreenToLocal(kEvent.getX(), kEvent.getY(), localPt);
        m_kActiveLookupTable = null;

        /* Get which image is active, either m_kImageA or m_kImageB: */
        m_kActiveImage = m_kParent.getHistoLUTActiveImage();

        if (m_kActiveImage == null) {
            m_kActiveImage = m_kParent.getHistoRGBActiveImage();
        }
        if (m_kActiveImage == null) {
            m_kActiveImage = m_kVolumeImageA.GetImage();
        }
        
        m_kActiveLookupTable = m_kParent.getActiveLookupTable(m_kActiveImage);
        
        if ( m_kWinLevel.updateWinLevel( localPt.X, localPt.Y, m_bFirstDrag, m_kActiveLookupTable, m_kActiveImage ) )
        {
            if ( m_kActiveImage == m_kVolumeImageA.GetImage() )
            {
                if ( m_kVolumeImageA.GetImage().isColorImage() )
                {
                    m_kParent.getRGBDialog().setRGBTA((ModelRGB)m_kActiveLookupTable);
                    m_kParent.getRGBDialog().update( );
                }
                else
                {
                    m_kParent.getLUTDialog().setLUTA((ModelLUT)m_kActiveLookupTable);
                }
            }
            else if ( m_kActiveImage == m_kVolumeImageB.GetImage() )
            {
                if ( m_kVolumeImageB.GetImage().isColorImage() )
                {
                    m_kParent.getRGBDialog().setRGBTB((ModelRGB)m_kActiveLookupTable);
                    m_kParent.getRGBDialog().update( );
                }
                else
                {
                    m_kParent.getLUTDialog().setLUTB((ModelLUT)m_kActiveLookupTable);
                }
            }
        }
        if (m_bFirstDrag) {
            try {
                Image kImg = MipavUtil.getIconImage("qkwinlevel.gif");
                Cursor kWinLevelCursor = Toolkit.getDefaultToolkit().createCustomCursor(kImg, new Point(12, 12),
                                                                                        "WinLevel");
                /* Set the cursor icon: */
                m_kParent.setCursor(kWinLevelCursor);
            } catch (FileNotFoundException error) { }
            m_bFirstDrag = false;
        }
    }

    /**
     * Calculate the position of the mouse in the Local Coordinates, taking
     * into account zoom and translate:
     *
     * @param iX mouse x coordinate value
     * @param iY mouse y coordinate value
     * @param kLocal mouse position in Local Coordinates
     */
    private void ScreenToLocal(int iX, int iY, Vector3f kLocal )
    {
        iX = Math.min( m_iWidth,  Math.max( 0, iX ) );
        iY = Math.min( m_iHeight, Math.max( 0, iY ) );
        float fHalfWidth = ((float) m_iWidth-1) / 2.0f;
        float fHalfHeight = ((float) m_iHeight-1) / 2.0f;

        kLocal.X = (iX - fHalfWidth) / fHalfWidth;
        kLocal.Y = (iY - fHalfHeight) / fHalfWidth;

        kLocal.X /= m_fZoomScale;
        kLocal.Y /= m_fZoomScale;

        kLocal.X -= m_fXTranslate;
        kLocal.Y -= m_fYTranslate;

        /* Bounds checking: */
        kLocal.X = Math.min( Math.max( kLocal.X, m_fX0 ), m_fX1 );
        kLocal.Y = Math.min( Math.max( kLocal.Y, m_fY0 ), m_fY1 );

        /* Normalize: */
        kLocal.X = (kLocal.X - m_fX0) / m_fXRange;
        kLocal.Y = (kLocal.Y - m_fY0) / m_fYRange;
        kLocal.Z = m_iSlice / (float)(m_aiLocalImageExtents[2] - 1);
    }

    /**
     * Sets the local slice value.
     * @param fSlice
     */
    private void setSlice(float fSlice) {
        int iSlice = (int)fSlice;

        /* Check bounds: */
        if (iSlice > (m_aiLocalImageExtents[2] - 1)) {
            iSlice = m_aiLocalImageExtents[2] - 1;
        }

        if (iSlice < 0) {
            iSlice = 0;
        }

        if (iSlice != m_iSlice) {
            m_iSlice = iSlice;
        }
    }
    
}
