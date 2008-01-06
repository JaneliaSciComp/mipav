package gov.nih.mipav.view.renderer.WildMagic;

import gov.nih.mipav.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.media.opengl.*;
import com.sun.opengl.util.*;

import gov.nih.mipav.view.WildMagic.LibApplications.OpenGLApplication.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.view.WildMagic.LibRenderers.OpenGLRenderer.*;

import java.io.FileNotFoundException;



/**
 * Class PlaneRenderWM: renders a single dimension of the ModelImage data as a
 * texture-mapped polygon. The PlaneRenderWM class keeps track of whether it is
 * rendering the Axial, Sagital, or Coronal view of the data.
 *
 * Surfaces are displayed as the intersection of the ModelTriangleMesh with
 * the rendered z-slice. 
 */
public class PlaneRender_WM extends JavaApplication3D
    implements GLEventListener
{

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2025132936439496099L;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    /** Animator object, displays scene in rendering loop (similar to GLUTMainLoop() */
    private Animator m_kAnimator;
    
    private VolumeImage m_kVolumeImageA;

    /** Set to true when init() is called: */
    private boolean m_bInit = false;
    
    /** The image dimensions in x,y,z:. */
    private int[] m_aiLocalImageExtents;

    /** Set of colors used to draw the X and Y Bars and the Z box:. */
    private ColorRGB[][] m_aakColors = { { new ColorRGB(1, 1, 0), new ColorRGB(0, 1, 0), new ColorRGB(1, 0, 0) },
                                         { new ColorRGB(1, 1, 0), new ColorRGB(1, 0, 0), new ColorRGB(0, 1, 0) },
                                         { new ColorRGB(0, 1, 0), new ColorRGB(1, 0, 0), new ColorRGB(1, 1, 0) } };

    private int[][] m_aaiColorSwap = { { 2, 1, 0 }, { 1, 2, 0 }, { 1, 0, 2 } };


    /** Bounding box polyline object: */
    //private Polyline[] m_akBoundingBox;
    private Polyline m_kBoundingBox;

    /** XBar box polyline object: */
    private Polyline[] m_akXBar;

    /** YBar box polyline object: */
    private Polyline[] m_akYBar;

    /** Vertex-color shader effect used for the polylines and the first-pass
     * rendering of the proxy-geometry:*/
    private ShaderEffect m_spkVertexColor3Shader;

    /** when true, the axis labels (P-> L-> S->) will be drawn */
    private boolean m_bDrawAxes = true;

    /** Turns on drawing of the X,Y bars and the Axis labels:. */
    private boolean m_bDrawXHairs = true;

    /** Boolean to turn on/off the RFA probe entry point selection with mouse:. */
    private boolean m_bEntryPointSelect = false;

    /** Change the mouse cursor with the first mouseDrag event */
    private boolean m_bFirstDrag = true;

    /** True when the left mouse has been pressed, set to false when the left
     * mouse button is released. */
    private boolean m_bLeftMousePressed = false;

    /** Whether to store all the data in ImageComponent2D array or not:. */
    private boolean m_bMemoryUsage;

    /** Actual image orietation. */
    private boolean m_bPatientOrientation = true;

    /** Flag indicating if the right mouse button is currently pressed
     * down: */
    private boolean m_bRightMousePressed = false;

    /** The center of the X,Y bar cross hairs, in plane coordinates:. */
    private float m_fCenterX, m_fCenterY;

    /** The current active lookup table: */
    private ModelStorageBase m_kActiveLookupTable;

    /** lower x-bound of the texture-mapped polygon: */
    private float m_fX0;

    /** upper x-bound of the texture-mapped polygon: */
    private float m_fX1;

    /** Numbers dicatating the size of the plane based on the extents and
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

    /** Y direction mouse translatioin. */
    private float m_fYTranslate = 0.0f;

    /** Image scaling from Zoom:. */
    private float m_fZoomScale = 1.0f;

    /** Which dimension of the ModelImage to render. */
    private int m_iPlaneOrientation = 0;

    /** Which slice is currently displayed in the XY plane. */
    private int m_iSlice;

    /** Current active image for manipulating the LUT by dragging with the
     * right-mouse down. */
    private ModelImage m_kActiveImage;

    /** Current image A. */
    private ModelImage m_kImageA;

    /** Current image B. */
    private ModelImage m_kImageB;

    /** x-axis label: */
    private String m_kLabelX = new String("X");

    /** y-axis label: */
    private String m_kLabelY = new String("Y");

    /** Reference to the parent frame:. */
    private VolumeViewer m_kParent;

    private WindowLevel m_kWinLevel;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new PlaneRenderWM object.
     *
     * @param  kParent  ViewJFrameVolumeView - reference to parent frame.
     * @param  kImageA  First image to display, cannot be null.
     * @param  kLUTa    LUT of the imageA (if null grayscale LUT is constructed).
     * @param  kImageB  Second loaded image, may be null.
     * @param  kLUTb    LUT of the imageB, may be null.
     * @param  kConfig  GraphicsConfiguration
     * @param  iPlane   Image dimension to be displayed.
     * @param bMemory when true store all the data in memory, when false,
     * write textues as the slices change
     */
    public PlaneRender_WM(VolumeViewer kParent, Animator kAnimator, 
                          VolumeImage kVolumeImageA, ModelImage kImageA, ModelLUT kLUTa,
                          VolumeImage kVolumeImageB, ModelImage kImageB, ModelLUT kLUTb,
                          int iPlane, boolean bMemory)
    {
        super( "PlaneRender", 0, 0, 512, 512,
               new ColorRGBA(0.0f,0.0f,0.0f,0.0f));
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
        m_bMemoryUsage = bMemory;

        m_kImageA = kImageA;
        m_kImageB = kImageB;
        m_kImageA.setImageOrder(ModelImage.IMAGE_A);

        if (m_kImageB != null) {
            m_kImageB.setImageOrder(ModelImage.IMAGE_B);
        }

        setOrientation();
        m_kWinLevel = new WindowLevel();

        /* Create the scene graph and initialize the rendering */
        //init();
    }

    public void display(GLAutoDrawable arg0) {

        if (MoveCamera())
        {
            m_kCuller.ComputeVisibleSet(m_spkScene);
        }
        
        if (MoveObject())
        {
            m_spkScene.UpdateGS();
            m_kCuller.ComputeVisibleSet(m_spkScene);
        }

        m_pkRenderer.ClearBuffers();
        if (m_pkRenderer.BeginScene())
        {          
            //System.err.println( "PlaneRender " + m_iPlaneOrientation );
            m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
            m_pkRenderer.EndScene();
        }
        m_pkRenderer.DisplayBackBuffer();
        
    }

    public void displayChanged(GLAutoDrawable arg0, boolean arg1, boolean arg2) {
        // TODO Auto-generated method stub
        
    }

    public void init(GLAutoDrawable arg0) {
        arg0.setAutoSwapBufferMode( false );

        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        ((OpenGLRenderer)m_pkRenderer).InitializeState();

        super.OnInitialize();

        // set up camera
        m_spkCamera.SetFrustum(60.0f,1.0f,0.1f,100.0f);
        Vector3f kCLoc = new Vector3f(0.0f,0.0f,2.0f);
        Vector3f kCDir = Vector3f.UNIT_Z_NEG;
        Vector3f kCUp = Vector3f.UNIT_Y;
        Vector3f kCRight = Vector3f.UNIT_X;
        m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);

        CreateScene();

        // initial update of objects
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();

        // initial culling of scene
        m_kCuller.SetCamera(m_spkCamera);
        m_kCuller.ComputeVisibleSet(m_spkScene);

        InitializeCameraMotion(0.001f,0.001f);
        InitializeObjectMotion(m_spkScene);

        //((OpenGLRenderer)m_pkRenderer).ClearDrawable( );

        m_kAnimator.add( GetCanvas() );
    }

    public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight) {
       // ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        if (iWidth > 0 && iHeight > 0)
        {
            if (m_pkRenderer != null)
            {
                m_pkRenderer.Resize(iWidth,iHeight);
            }
            
            m_iWidth = iWidth;
            m_iHeight = iHeight;
        }
    }

    public GLCanvas GetCanvas()
    {
        return ((OpenGLRenderer)m_pkRenderer).GetCanvas();
    }

    private void CreateScene ()
    {
        m_spkScene = new Node();
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetTChannels(0,3);
        StandardMesh kSM = new StandardMesh(kAttr);
        m_pkPlane = kSM.Rectangle(2,2,1.0f,1.0f);
        
        m_fX0 = -m_fXBox / m_fMaxBox;
        m_fX1 = m_fXBox / m_fMaxBox;
        m_fY0 = -m_fYBox / m_fMaxBox;
        m_fY1 = m_fYBox / m_fMaxBox;
        
        m_fXRange = m_fX1 - m_fX0;
        m_fYRange = m_fY1 - m_fY0;

        /* Center the X and Y bars: */
        m_fCenterX = (m_fX0 + m_fX1) / 2.0f;
        m_fCenterY = (m_fY0 + m_fY1) / 2.0f;

        m_iSlice = (m_aiLocalImageExtents[2]) / 2;
        
        float[][] tc = MipavCoordinateSystems.
            getPatientTextureCoordinates( new Point3Df( m_kImageA.getExtents()[0]/2.0f,
                                                        m_kImageA.getExtents()[1]/2.0f,
                                                        m_kImageA.getExtents()[2]/2.0f),
                                          m_kImageA, m_iPlaneOrientation, true);

        m_pkPlane.VBuffer.SetPosition3(0, m_fX0, m_fY0, 0);
        m_pkPlane.VBuffer.SetTCoord3(0, 2, tc[0][0], tc[0][1], tc[0][2] );

        m_pkPlane.VBuffer.SetPosition3(1, m_fX1, m_fY0, 0);
        m_pkPlane.VBuffer.SetTCoord3(0, 3, tc[1][0], tc[1][1], tc[1][2] );

        m_pkPlane.VBuffer.SetPosition3(2, m_fX0, m_fY1, 0);
        m_pkPlane.VBuffer.SetTCoord3(0, 0, tc[2][0], tc[2][1], tc[2][2] );

        m_pkPlane.VBuffer.SetPosition3(3, m_fX1, m_fY1, 0);
        m_pkPlane.VBuffer.SetTCoord3(0, 1, tc[3][0], tc[3][1], tc[3][2] );

        m_spkEffect = new VolumePlaneEffect( m_kVolumeImageA );
        m_pkPlane.AttachEffect(m_spkEffect);
        m_spkScene.AttachChild(m_pkPlane);
        
        CreateLabels();
    }

    private Node m_spkScene;
    private Culler m_kCuller = new Culler(0,0,null);
    private VolumePlaneEffect m_spkEffect;
    private TriMesh m_pkPlane = null;


    /**
     * Closes the frame.
     */
    public void close() {
        disposeLocal();
    }

    /**
     * Clean memory.
     */
    public void disposeLocal() {
        m_kImageA = null;
        m_kImageB = null;

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
        

        //m_kAnimator.stop();
   }

    /**
     * Given a point in FileCoordinates, transform the point to local
     * PatientCoordinates, and draw with a red sphere:
     *
     * @param  kPoint  RFA indicator point coordinate
     */
//    public void drawRFAPoint(Point3f kPoint) {

//         /* FileToPatient: */
//         Point3Df patientPt = new Point3Df();
//         MipavCoordinateSystems.fileToPatient( new Point3Df( kPoint.x,
//                                                             kPoint.y,
//                                                             kPoint.z ),
//                                               patientPt, m_kImageA,
//                                               m_iPlaneOrientation );
//         /* PatientToLocal: */
//         Point3Df localPt = new Point3Df();
//         this.PatientToLocal( patientPt, localPt );

//         /* If this is the first time drawing, create the BranchGroup to hold
//          * the sphere representation: */
//         if (m_kRFA_BranchGroup == null) {
//             PolygonAttributes kPolygonAttributes = new PolygonAttributes();
//             kPolygonAttributes.setCullFace(PolygonAttributes.CULL_NONE);
//             Material kMaterial = new Material( m_akColors[0], m_akColors[0], m_akColors[0],
//                                                m_akColors[0], 1.0f);
//             Appearance kAppearance = new Appearance();
//             kAppearance.setMaterial(kMaterial);
//             kAppearance.setPolygonAttributes(kPolygonAttributes);
//             Shape3D kSphere = new Sphere( 0.025f ).getShape();

//             kSphere.setAppearance( kAppearance );
//             kSphere.setPickable(false);

//             Transform3D kTransform = new Transform3D();

//             kTransform.set(new Vector3f(localPt.x, -localPt.y, -2.5f));

//             TransformGroup kTransformGroup = new TransformGroup();

//             kTransformGroup.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
//             kTransformGroup.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
//             kTransformGroup.setTransform(kTransform);
//             kTransformGroup.addChild(kSphere.cloneTree());
//             m_kRFA_BranchGroup = new BranchGroup();
//             m_kRFA_BranchGroup.setCapability(BranchGroup.ALLOW_DETACH);
//             m_kRFA_BranchGroup.setCapability(BranchGroup.ALLOW_CHILDREN_READ);
//             m_kRFA_BranchGroup.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
//             m_kRFA_BranchGroup.addChild(kTransformGroup);
//             m_kRFA_BranchGroup.compile();
//             m_kOrderedGroup.addChild(m_kRFA_BranchGroup);
//         } /* Otherwise, update the position of the existing sphere: */
//         else {
//             Transform3D kTransform = new Transform3D();

//             kTransform.set(new Vector3f(localPt.x, -localPt.y, -2.5f));

//             TransformGroup kTransformGroup =
//                 (TransformGroup) (m_kRFA_BranchGroup.getChild(0));

//             kTransformGroup.setTransform(kTransform);
//         }
//    }

    /**
     * Enable or disable target point for the RFA probe from within the plane
     * renderer:
     *
     * @param  bEnable  true enable target point, false not.
     */
    public void enableTargetPointPicking(boolean bEnable) {

//         if (m_bEntryPointSelect == bEnable) {
//             return;
//         }

//         m_bEntryPointSelect = bEnable;

//         /* If point selection is disabled, then detach the display group: */
//         if (!bEnable && (m_kRFA_BranchGroup != null)) {
//             m_kOrderedGroup.removeChild(m_kRFA_BranchGroup);
//         } /* If point selection is enabled, then draw the display group: */
//         else if (bEnable && (m_kRFA_BranchGroup != null)) {
//             m_kOrderedGroup.addChild(m_kRFA_BranchGroup);
//         }
    }

    /**
     * Accessor that returns the reference to image A.
     *
     * @return  Image A.
     */
    public ModelImage getImageA() {
        return m_kImageA;
    }

    /**
     * Accessor that returns the reference to image B.
     *
     * @return  Image B.
     */
    public ModelImage getImageB() {
        return m_kImageB;
    }


    /**
     * One of the overrides necessary to be a MouseListener. This function is
     * invoked when a mouse button is held down and the mouse is dragged in
     * the active window area.
     *
     * @param  kEvent  the mouse event generated by a mouse drag
     */
    public void mouseDragged(MouseEvent kEvent) {

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
            super.mouseDragged(kEvent);
        }
    }

    /**
     * One of the overrides necessary to be a MouseListener.
     *
     * <p>If the left mouse button is pressed, the function sets the
     * m_bLeftMousePressed to be true, and records the current canvas width
     * and height.</p>
     *
     * @param  kEvent  the mouse event generated by a mouse press
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
    }

    /**
     * One of the overrides necessary to be a MouseListener.
     *
     * @param  kEvent  the mouse event generated by a mouse release
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

//             /* If the RFA probe point is being set by the mouse, then
//              * calculate the mouse position in FileCoordinates and pass the
//              * information to the parent class: */
//             if (m_bEntryPointSelect) {

//                 /* Calculate the center of the mouse in LOCAL coordineates, taking
//                  * into account zoom and translate: */
//                 Point3Df localPt = new Point3Df();
//                 this.ScreenToLocal(kEvent.getX(), kEvent.getY(), localPt, false);
//                 Point3Df patientPt = new Point3Df();
//                 this.LocalToPatient( localPt, patientPt );
//                 Point3Df kRFAPoint = new Point3Df();
//                 MipavCoordinateSystems.patientToFile(patientPt, kRFAPoint, m_kImageA,
//                                                      m_iPlaneOrientation );
//                 /* Tell the parent to draw the RFA point: */
//                 m_kParent.
//                     drawRFAPoint( new Point3f( kRFAPoint.x, kRFAPoint.y, kRFAPoint.z ) );
//             }
        }
        m_bFirstDrag = true;
        m_kParent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
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
     * Sets the new location of the XBar.
     *
     * @param  fSlice  The new position of the XBar in plane coordinates:
     */
    private void setXBar(float fSlice) {
         fSlice = fSlice / (m_aiLocalImageExtents[0] - 1);
         m_fCenterX = (fSlice * (m_fX1 - m_fX0)) + m_fX0;
    }

    /**
     * Sets the default color for the SliceHairColor.
     *
     * @param  kColor  set the hair color to this color
     */
    public void setSliceHairColor(int iView, ColorRGB kColor) {
        int iX = 0;
        int iY = 1;
        int iZ = 2;

        m_aakColors[m_iPlaneOrientation][m_aaiColorSwap[m_iPlaneOrientation][iView]] = kColor;
        ColorRGB kXSliceHairColor = m_aakColors[m_iPlaneOrientation][iX];
        ColorRGB kYSliceHairColor = m_aakColors[m_iPlaneOrientation][iY];

        for ( int i = 0; i < 2; i++ )
        {
            for ( int j = 0; j < 2; j++ )
            {
                m_akXBar[i].VBuffer.SetColor3( 0, j, kXSliceHairColor );
                m_akYBar[i].VBuffer.SetColor3( 0, j, kYSliceHairColor );
            }
            m_akXBar[i].VBuffer.Release();
            m_akYBar[i].VBuffer.Release();
        }
        for ( int i = 0; i < 4; i++ )
        {
            m_kBoundingBox.VBuffer.SetColor3(0,i, m_aakColors[m_iPlaneOrientation][ iZ ] ) ;
        }
        m_kBoundingBox.VBuffer.Release();

    }

    /**
     * Sets the new location of the YBar.
     *
     * @param  fSlice  The new position of the YBar in plane coordinates:
     */
    private void setYBar(float fSlice) {
         fSlice = fSlice / (m_aiLocalImageExtents[1] - 1);
         m_fCenterY = (fSlice * (m_fY1 - m_fY0)) + m_fY0;
    }


    /**
     * Turns displaying the Axis labes on or off:
     *
     * @param bShow when true display the axis lables, when false hide the
     * axis labels
     */
    public void showAxes(boolean bShow) {
//         if (m_bDrawAxes != bShow) {
//             m_bDrawAxes = bShow;
//             initAxes();
//         }
    }

    /**
     * Turns displaying the X and Y bars on or off:
     *
     * @param  bShow when true show the cross-hairs when false hide the cross-hairs
     */
    public void showXHairs(boolean bShow) {
//         m_bDrawXHairs = bShow;
    }



    /**
     * Draws the Z box, the X bar and the Y bar:.
     */
    private void CreateLabels()
    {
        int iX = 0;
        int iY = 1;
        int iZ = 2;

        m_spkVertexColor3Shader = new VertexColor3Effect();
        
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);
        
        VertexBuffer kOutlineSquare = new VertexBuffer(kAttributes, 4);
        for ( int j = 0; j < 4; j++ )
        {
            kOutlineSquare.SetColor3( 0, j, m_aakColors[m_iPlaneOrientation][ iZ ] );
        }
        kOutlineSquare.SetPosition3( 0, m_fX0, m_fY0, 0 ) ;
        kOutlineSquare.SetPosition3( 1, m_fX1, m_fY0, 0 ) ;
        kOutlineSquare.SetPosition3( 2, m_fX1, m_fY1, 0 ) ;
        kOutlineSquare.SetPosition3( 3, m_fX0, m_fY1, 0 ) ;

        m_kBoundingBox = new Polyline( new VertexBuffer(kOutlineSquare), true, true );
        m_kBoundingBox.AttachEffect( m_spkVertexColor3Shader );
        m_spkScene.AttachChild(m_kBoundingBox);



        ColorRGB kXSliceHairColor = m_aakColors[m_iPlaneOrientation][ iX ];
        ColorRGB kYSliceHairColor = m_aakColors[m_iPlaneOrientation][ iY ];
//         if ( m_iPlaneOrientation == FileInfoBase.AXIAL )
//         {
//             kXSliceHairColor = m_akColors[2];
//             kYSliceHairColor = m_akColors[1];
//         }
//         else if ( m_iPlaneOrientation == FileInfoBase.SAGITTAL )
//         {
//             kXSliceHairColor = m_akColors[1];
//             kYSliceHairColor = m_akColors[0];
//         }
        
        float fScreenX = (float) m_fCenterX;
        float fScreenY = (float) -m_fCenterY;


        VertexBuffer[] akXBar = new VertexBuffer[2];
        VertexBuffer[] akYBar = new VertexBuffer[2];
        for ( int i = 0; i < 2; i++ )
        {
            akXBar[i] = new VertexBuffer(kAttributes, 2 );
            akYBar[i] = new VertexBuffer(kAttributes, 2 );
            for ( int j = 0; j < 2; j++ )
            {
                akXBar[i].SetColor3( 0, j, kXSliceHairColor );
                akYBar[i].SetColor3( 0, j, kYSliceHairColor );
            }
        }

        akXBar[0].SetPosition3(0, fScreenX, m_fY0, 0 );
        akXBar[0].SetPosition3(1, fScreenX, fScreenY - .10f, 0 );

        akXBar[1].SetPosition3(0, fScreenX, fScreenY + .10f, 0 );
        akXBar[1].SetPosition3(1, fScreenX, m_fY1, 0 );
        
        akYBar[0].SetPosition3(0, m_fX0, fScreenY, 0 );
        akYBar[0].SetPosition3(1, fScreenX - .10f, fScreenY, 0 );

        akYBar[1].SetPosition3(0, fScreenX + .10f, fScreenY, 0 );
        akYBar[1].SetPosition3(1, m_fX1, fScreenY, 0 );

        m_akXBar = new Polyline[2];   
        m_akYBar = new Polyline[2];    
        for ( int i = 0; i < 2; i++ )
        {
            m_akXBar[i] = new Polyline( new VertexBuffer(akXBar[i]), true, true );
            m_akXBar[i].AttachEffect( m_spkVertexColor3Shader );
            m_spkScene.AttachChild(m_akXBar[i]);

            m_akYBar[i] = new Polyline( new VertexBuffer(akYBar[i]), true, true );
            m_akYBar[i].AttachEffect( m_spkVertexColor3Shader );
            m_spkScene.AttachChild(m_akYBar[i]);
        }
    }

    private void UpdateBarPosition()
    {

        float fScreenX = (float) m_fCenterX;
        float fScreenY = (float) -m_fCenterY;
        m_akYBar[0].VBuffer.SetPosition3(0, m_fX0, fScreenY, 0 );
        m_akYBar[0].VBuffer.SetPosition3(1, fScreenX - .10f, fScreenY, 0 );

        m_akYBar[1].VBuffer.SetPosition3(0, fScreenX + .10f, fScreenY, 0 );
        m_akYBar[1].VBuffer.SetPosition3(1, m_fX1, fScreenY, 0 );

        m_akXBar[0].VBuffer.SetPosition3(0, fScreenX, m_fY0, 0 );
        m_akXBar[0].VBuffer.SetPosition3(1, fScreenX, fScreenY - .10f, 0 );

        m_akXBar[1].VBuffer.SetPosition3(0, fScreenX, fScreenY + .10f, 0 );
        m_akXBar[1].VBuffer.SetPosition3(1, fScreenX, m_fY1, 0 );
        
        for ( int i = 0; i < 2; i++ )
        {
            m_akXBar[i].VBuffer.Release();
            m_akYBar[i].VBuffer.Release();
        }
    }
    
    /**
     * Calculate the position of the mouse in the Local Coordinates, taking
     * into account zoom and translate:
     *
     * @param iX mouse x coordinate value
     * @param iY mouse y coordinate value
     * @param kLocal mouse position in Local Coordinates
     * @param bSetCenter if true updates the position for rendering the x-bar
     * and y-bar colored axes (for left mouse drag)
     */
    private void ScreenToLocal(int iX, int iY, Point3Df kLocal, boolean bSetCenter )
    {
        iX = (int)Math.min( m_iWidth,  Math.max( 0, iX ) );
        iY = (int)Math.min( m_iHeight, Math.max( 0, iY ) );
        float fHalfWidth = ((float) m_iWidth-1) / 2.0f;
        float fHalfHeight = ((float) m_iHeight-1) / 2.0f;

        kLocal.x = ((float) (iX - fHalfWidth)) / fHalfWidth;
        kLocal.y = ((float) (iY - fHalfHeight)) / fHalfWidth;

        kLocal.x /= m_fZoomScale;
        kLocal.y /= m_fZoomScale;

        kLocal.x -= m_fXTranslate;
        kLocal.y -= m_fYTranslate;

        /* Bounds checking: */
        kLocal.x = Math.min( Math.max( kLocal.x, m_fX0 ), m_fX1 );
        kLocal.y = Math.min( Math.max( kLocal.y, m_fY0 ), m_fY1 );

        if ( bSetCenter )
        {
            m_fCenterX = kLocal.x;
            m_fCenterY = kLocal.y;
        }

        /* Normalize: */
        kLocal.x = (kLocal.x - m_fX0) / m_fXRange;
        kLocal.y = (kLocal.y - m_fY0) / m_fYRange;
        kLocal.z = m_iSlice / (float)(m_aiLocalImageExtents[2] - 1);
    }

    /* Convert the position in LocalCoordinates (rendering space) into
     * PatientCoordinates:
     * @param localPt, the current point in LocalCoordinates
     * @param patientPt transformed localPt in PatientCoordinates
     */
    private void LocalToPatient( Point3Df localPt, Point3Df patientPt )
    {
        patientPt.x = localPt.x * (m_aiLocalImageExtents[0] - 1);
        patientPt.y = localPt.y * (m_aiLocalImageExtents[1] - 1);
        patientPt.z = localPt.z * (m_aiLocalImageExtents[2] - 1);
    }

    /**
     * Convert the position in PatientCoordinates into Local rendering
     * coordinates:
     * @param patientPt the current point in PatientCoordinates
     * @param localPt, the transformed point in LocalCoordinates
     */
    private void PatientToLocal( Point3Df patientPt, Point3Df localPt )
    {
//         localPt.x = patientPt.x / (float)(m_aiLocalImageExtents[0] - 1);
//         localPt.y = patientPt.y / (float)(m_aiLocalImageExtents[1] - 1);
//         localPt.z = patientPt.z / (float)(m_aiLocalImageExtents[2] - 1);

//         localPt.x = (localPt.x * m_fXRange) + m_fX0;
//         localPt.y = (localPt.y * m_fYRange) + m_fY0;
//         localPt.z = 1.0f;
    }


    /**
     * Initializes the Axis labels based on the ModelImage orientation. Axes are displayed with 3D text objects and
     * arrows drawn as polygons. They are colored and labeled to match the axes they represent.
     */
    private void initAxes() {

//         if (m_kTextBranchGroup != null) {
//             m_kOrderedGroup.removeChild(m_kTextBranchGroup);
//             m_kTextBranchGroup = null;
//             m_kTextTransformGroup = null;
//         }
//         if ( !m_bDrawAxes )
//         {
//             return;
//         }

//         String kLabelX = new String( m_kLabelX );
//         if ( !m_kImageA.getRadiologicalView() && (m_iPlaneOrientation != FileInfoBase.SAGITTAL) )
//         {
//             if ( !m_bPatientOrientation )
//             {
//                 kLabelX = new String( "-X" );
//             }
//             else
//             {
//                 kLabelX = new String( "R" );
//             }
//         }
//         Text3D kXText = new Text3D(new Font3D(MipavUtil.courier12B, new FontExtrusion()), kLabelX,
//                                    new Point3f(25f, 1f, 0f));

//         Text3D kYText = new Text3D(new Font3D(MipavUtil.courier12B, new FontExtrusion()), m_kLabelY,
//                                    new Point3f(-2f, -25f, 0f));

//         if ( m_bPatientOrientation &&
//             (m_iPlaneOrientation != FileInfoBase.AXIAL) ) {
//             kYText.setPosition(new Point3f(-2f, 31f, 0f));
//         }

//         QuadArray kXGeometry = new QuadArray(4, QuadArray.COORDINATES | QuadArray.COLOR_3);

//         Color3f kXSliceHairColor = m_akColors[2];
//         Color3f kYSliceHairColor = m_akColors[0];
//         if ( m_iPlaneOrientation == FileInfoBase.AXIAL )
//         {
//             kXSliceHairColor = m_akColors[2];
//             kYSliceHairColor = m_akColors[1];
//         }
//         else if ( m_iPlaneOrientation == FileInfoBase.SAGITTAL )
//         {
//             kXSliceHairColor = m_akColors[1];
//             kYSliceHairColor = m_akColors[0];
//         }

//         kXGeometry.setColor(0, kXSliceHairColor);
//         kXGeometry.setColor(1, kXSliceHairColor);
//         kXGeometry.setColor(2, kXSliceHairColor);
//         kXGeometry.setColor(3, kXSliceHairColor);
//         kXGeometry.setCoordinate(0, new Point3d(2f, 4f, 0.5f));
//         kXGeometry.setCoordinate(1, new Point3d(18f, 4f, 0.5f));
//         kXGeometry.setCoordinate(2, new Point3d(18f, 6f, 0.5f));
//         kXGeometry.setCoordinate(3, new Point3d(2f, 6f, 0.5f));

//         TriangleArray kXTri = new TriangleArray(3, TriangleArray.COORDINATES | TriangleArray.COLOR_3);

//         kXTri.setColor(0, kXSliceHairColor);
//         kXTri.setColor(1, kXSliceHairColor);
//         kXTri.setColor(2, kXSliceHairColor);
//         kXTri.setCoordinate(0, new Point3d(18f, 9f, 0.5f));
//         kXTri.setCoordinate(1, new Point3d(18f, 1f, 0.5f));
//         kXTri.setCoordinate(2, new Point3d(23f, 5f, 0.5f));

//         QuadArray kYGeometry = new QuadArray(4, QuadArray.COORDINATES | QuadArray.COLOR_3);

//         kYGeometry.setColor(0, kYSliceHairColor);
//         kYGeometry.setColor(1, kYSliceHairColor);
//         kYGeometry.setColor(2, kYSliceHairColor);
//         kYGeometry.setColor(3, kYSliceHairColor);

//         if (m_bPatientOrientation &&
//             (m_iPlaneOrientation != FileInfoBase.AXIAL)) {
//             kYGeometry.setCoordinate(0, new Point3d(2f, 4f, 0.5f));
//             kYGeometry.setCoordinate(1, new Point3d(0f, 4f, 0.5f));
//             kYGeometry.setCoordinate(2, new Point3d(0f, 22f, 0.5f));
//             kYGeometry.setCoordinate(3, new Point3d(2f, 22f, 0.5f));
//         } else {
//             kYGeometry.setCoordinate(0, new Point3d(2f, 6f, 0.5f));
//             kYGeometry.setCoordinate(1, new Point3d(0f, 6f, 0.5f));
//             kYGeometry.setCoordinate(2, new Point3d(0f, -12f, 0.5f));
//             kYGeometry.setCoordinate(3, new Point3d(2f, -12f, 0.5f));
//         }

//         TriangleArray kYTri = new TriangleArray(3, TriangleArray.COORDINATES | TriangleArray.COLOR_3);

//         kYTri.setColor(0, kYSliceHairColor);
//         kYTri.setColor(1, kYSliceHairColor);
//         kYTri.setColor(2, kYSliceHairColor);

//         if (m_bPatientOrientation &&
//             (m_iPlaneOrientation != FileInfoBase.AXIAL)) {
//             kYTri.setCoordinate(0, new Point3d(5f, 22f, 0.5f));
//             kYTri.setCoordinate(1, new Point3d(-4f, 22f, 0.5f));
//             kYTri.setCoordinate(2, new Point3d(1f, 27f, 0.5f));
//         } else {
//             kYTri.setCoordinate(0, new Point3d(5f, -12f, 0.5f));
//             kYTri.setCoordinate(1, new Point3d(-4f, -12f, 0.5f));
//             kYTri.setCoordinate(2, new Point3d(1f, -17f, 0.5f));
//         }

//         PolygonAttributes kPolygonAttributes = new PolygonAttributes();

//         kPolygonAttributes.setCullFace(PolygonAttributes.CULL_NONE);

//         Material kXMaterial = new Material( kXSliceHairColor, kXSliceHairColor, kXSliceHairColor,
//                                             kXSliceHairColor, 1.0f);
//         Material kYMaterial = new Material( kYSliceHairColor, kYSliceHairColor, kYSliceHairColor,
//                                             kYSliceHairColor, 1.0f);

//         Appearance kXAppearance = new Appearance();

//         kXAppearance.setMaterial(kXMaterial);
//         kXAppearance.setPolygonAttributes(kPolygonAttributes);

//         Appearance kYAppearance = new Appearance();

//         kYAppearance.setMaterial(kYMaterial);
//         kYAppearance.setPolygonAttributes(kPolygonAttributes);

//         Shape3D kXBox = new Shape3D(kXGeometry, kXAppearance);
//         Shape3D kXArrows = new Shape3D(kXTri, kXAppearance);
//         Shape3D kYBox = new Shape3D(kYGeometry, kYAppearance);
//         Shape3D kYArrows = new Shape3D(kYTri, kYAppearance);
//         Shape3D kXAxisLabel = new Shape3D(kXText, kXAppearance);
//         Shape3D kYAxisLabel = new Shape3D(kYText, kYAppearance);

//         float fXTrans = m_fX0 * 0.85f / m_fZoomScale;
//         float fYTrans = m_fY1 * 0.85f / m_fZoomScale;

//         if (m_bPatientOrientation &&
//             (m_iPlaneOrientation != FileInfoBase.AXIAL)) {
//             fYTrans = -fYTrans;
//         }

//         Transform3D kTextTransform = new Transform3D();
//         kTextTransform.setScale(0.01f / m_fZoomScale);
//         kTextTransform.setTranslation(new Vector3f(fXTrans, fYTrans, -2.5f));
//         m_kTextTransformGroup = new TransformGroup();
//         m_kTextTransformGroup.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
//         m_kTextTransformGroup.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
//         m_kTextTransformGroup.setCapability(TransformGroup.ALLOW_CHILDREN_WRITE);
//         m_kTextTransformGroup.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
//         m_kTextTransformGroup.setCapability(TransformGroup.ALLOW_CHILDREN_EXTEND);
//         m_kTextTransformGroup.setTransform(kTextTransform);

//         m_kTextTransformGroup.addChild(kXBox);
//         m_kTextTransformGroup.addChild(kXArrows);
//         m_kTextTransformGroup.addChild(kYBox);
//         m_kTextTransformGroup.addChild(kYArrows);
//         m_kTextTransformGroup.addChild(kXAxisLabel);
//         m_kTextTransformGroup.addChild(kYAxisLabel);

//         m_kTextBranchGroup = new BranchGroup();
//         m_kTextBranchGroup.setCapability(BranchGroup.ALLOW_DETACH);
//         m_kTextBranchGroup.addChild(m_kTextTransformGroup);
//         m_kTextBranchGroup.compile();

//         m_kOrderedGroup.addChild(m_kTextBranchGroup);
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

        /* If the RFA point is enabled, then the mouse is used to select
         * the Probe point, not to move the slice positions: */
//         if (m_bEntryPointSelect) {
//             return;
//         }

        /* Calculate the center of the mouse in local coordineates, taking into
         * account zoom and translate: */
        Point3Df localPt = new Point3Df();
        this.ScreenToLocal(kEvent.getX(), kEvent.getY(), localPt, true);

//         drawLabels();

        /* Tell the ViewJFrameVolumeView parent to update the other
         * PlaneRenderWMs and the SurfaceRender with the changed Z position
         * of the planes with color matching the moved bar: */
        Point3Df patientPt = new Point3Df();
        this.LocalToPatient( localPt, patientPt );
        Point3Df volumePt = new Point3Df();
        MipavCoordinateSystems.patientToFile( patientPt, volumePt, m_kImageA, m_iPlaneOrientation );
        m_kParent.setSliceFromPlane( volumePt );
    }

    /**
     * setCenter sets the cursor and slice position for this PlaneRenderWM
     * object, based on the 3D location of the three intersecting ModelImage
     * planes.
     * @param center, the 3D center in FileCoordinates of the three
     * intersecting ModelImage planes.
     */
    public void setCenter( Point3Df center )
    {
        float[][] tc = MipavCoordinateSystems.
            getPatientTextureCoordinates( center,
                                          m_kImageA, m_iPlaneOrientation, true);
        m_pkPlane.VBuffer.SetTCoord3(0, 2, tc[0][0], tc[0][1], tc[0][2] );

        m_pkPlane.VBuffer.SetTCoord3(0, 3, tc[1][0], tc[1][1], tc[1][2] );

        m_pkPlane.VBuffer.SetTCoord3(0, 0, tc[2][0], tc[2][1], tc[2][2] );

        m_pkPlane.VBuffer.SetTCoord3(0, 1, tc[3][0], tc[3][1], tc[3][2] );

        m_pkPlane.VBuffer.Release();

        Point3Df patientPt = new Point3Df();
        MipavCoordinateSystems.fileToPatient( center, patientPt, m_kImageA, m_iPlaneOrientation );
        setXBar( patientPt.x );
        setYBar( patientPt.y );
        setSlice( patientPt.z );
        
        UpdateBarPosition();
    }

    public void setSlice(float fSlice) {
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
    
    
    /**
     * Returns the current center point of the 3 intersecting ModelImage
     * planes in FileCoordinates.
     * @return the current volume center point in FileCoordinates
     */
//    public Point3Df getCenter()
//    {
//         return m_kPatientSlice.getCenter();
//    }

    /**
     * If the right mouse button is pressed and dragged. processRightMouseDrag
     * updates the HistoLUT window and level (contrast and brightness)
     *
     * @param  kEvent  the mouse event generated by a mouse drag
     */
    private void processRightMouseDrag(MouseEvent kEvent) {

        /* Get the coordinates of the mouse position in local coordinates: */
        Point3Df localPt = new Point3Df();
        this.ScreenToLocal(kEvent.getX(), kEvent.getY(), localPt, false);
        m_kActiveLookupTable = null;

        /* Get which image is active, either m_kImageA or m_kImageB: */
        m_kActiveImage = m_kParent.getHistoLUTActiveImage();

        if (m_kActiveImage == null) {
            m_kActiveImage = m_kParent.getHistoRGBActiveImage();
        }
        if (m_kActiveImage == null) {
            m_kActiveImage = m_kImageA;
        }
        
        m_kActiveLookupTable = m_kParent.getLUTa();
        
        if ( m_kWinLevel.updateWinLevel( localPt.x, localPt.y, m_bFirstDrag, m_kActiveLookupTable, m_kActiveImage ) )
        {
            if ( m_kActiveImage == m_kImageA )
            {
                if ( m_kImageA.isColorImage() )
                {
                    m_kParent.getRGBDialog().setRGBTA((ModelRGB)m_kActiveLookupTable);
                    m_kParent.getRGBDialog().update( );
                }
                else
                {
                    m_kParent.getLUTDialog().setLUTA((ModelLUT)m_kActiveLookupTable);
                }
            }
            else if ( m_kActiveImage == m_kImageB )
            {
                if ( m_kImageB.isColorImage() )
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
     * Based on the orientaion of the ModelImage, sets up the index
     * parameters, m_aiLocalImageExtents[0], m_aiLocalImageExtents[1], and
     * m_aiLocalImageExtents[2], the drawing colors for the z box, x and y
     * bars, and the invert flags.
     *
     * <p>Once setup everything is rendered into an x,y plane where x,y may be
     * any of the original x,y, or z dimensions in the original
     * ModelImage.</p>
     */
    private void setOrientation() {
        m_aiLocalImageExtents = m_kImageA.getExtents( m_iPlaneOrientation );

        float[] afResolutions = m_kImageA.getResolutions( 0, m_iPlaneOrientation );

        if ((afResolutions[0] == 0.0f) || (afResolutions[1] == 0.0f) || (afResolutions[2] == 0.0f)) {
            afResolutions[0] = 1.0f;
            afResolutions[1] = 1.0f;
            afResolutions[2] = 1.0f;
        }

        m_fXBox = (float) (m_aiLocalImageExtents[0] - 1) * afResolutions[0];
        m_fYBox = (float) (m_aiLocalImageExtents[1] - 1) * afResolutions[1];

        m_fMaxBox = m_fXBox;

        if (m_fYBox > m_fMaxBox) {
            m_fMaxBox = m_fYBox;
        }

        if ( m_kImageA.getImageOrientation() != FileInfoBase.UNKNOWN_ORIENT )
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
    }

}
