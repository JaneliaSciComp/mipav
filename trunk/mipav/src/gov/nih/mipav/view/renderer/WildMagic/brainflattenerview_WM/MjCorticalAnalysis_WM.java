package gov.nih.mipav.view.renderer.WildMagic.brainflattenerview_WM;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.renderer.*;
import gov.nih.mipav.view.renderer.WildMagic.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.VolumeObject;
import gov.nih.mipav.view.renderer.WildMagic.VolumeSurface;
import gov.nih.mipav.view.renderer.WildMagic.VolumeViewer;

import java.awt.*;
import java.awt.event.*;
import java.util.Vector;
import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLCanvas;
import javax.media.opengl.GLEventListener;
import javax.swing.*;
import javax.swing.event.*;
import com.sun.opengl.util.Animator;


import gov.nih.mipav.view.WildMagic.LibApplications.OpenGLApplication.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.CullState;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.Light;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.view.WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;


/**
 * Cortical analysis applet and viewer. This class contains the static 'main' method to launch the application. This
 * class is an Applet which means that it can be embedded in a browser or it can be attached to a MainFrame when
 * executed as an application.
 */
public class MjCorticalAnalysis_WM extends JavaApplication3D implements GLEventListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6130870790192175575L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int[] m_aiTriIndex = null;

    /** Colors for the picked points:. */
    private ColorRGB[] m_akPickColors = {
        new ColorRGB(1, 0, 0), new ColorRGB(0, 1, 0), new ColorRGB(0, 0, 1), new ColorRGB(1, 1, 0), new ColorRGB(0, 1, 1),
        new ColorRGB(1, 0, 1)
    };

    /** DOCUMENT ME! */
    private ColorRGBA[] m_akTriColors = null;

    /**
     * Flag set to indicate whether curvature colors are rendered on the mesh surface. If not, then average convexity
     * colors are rendered.
     */
    private boolean m_bCurvatureColors = true;

    /** Flag set once inflation processing has been initialized. */
    private boolean m_bInflationInitialized = false;

    /**
     * Mouse events. Setting mousePressed and mouseReleased explicitly when the mouse events are received has deals with
     * getting multiply mouse event notifications for the same mouse press.
     */
    private boolean m_bMousePressed = false;

    /** DOCUMENT ME! */
    private boolean m_bMouseReleased = true;

    /** Turned on when picking with the mouse is enabled:. */
    private boolean m_bPickCorrespondenceEnabled = false;

    /** DOCUMENT ME! */
    private boolean m_bPickPunctureEnabled = false;

    /** number of picked points displayed. */
    private int m_iNumPicked = 0;

    /** DOCUMENT ME! */
    private int m_iRunningNumPicked = 0;

    /** Cortical mesh to be used in rendering scene. */
    private MjCorticalMesh_WM m_kCortical = null;

    /** DOCUMENT ME! */
    private ModelLUT m_kLUTConvexity = null;

    /** local reference to ModelLUT. */
    private ModelLUT m_kLUTCurvature = null;

    /** DOCUMENT ME! */
    private MouseEvent m_kMouseEvent = null;

    /** DOCUMENT ME! */
    private JPanelBrainSurfaceFlattener_WM m_kPanel = null;

    /**
     * Geometry for the rendering of the cortical mesh is maintained here so that the scene can be updated when the mesh
     * is inflated.
     */
    private TriMesh m_kMesh = null;

    /** DOCUMENT ME! */
    private int MAX_POINTS = 6;

    /** DOCUMENT ME! */
    private int MESH = 2;

    /** DOCUMENT ME! */
    private int PLANE = 1;

    /** DOCUMENT ME! */
    private int SPHERE = 0;
    
    /** Scene-graph root node: */
    private Node m_spkScene;
    /** Culling out-of-view objects: */
    private Culler m_kCuller = new Culler(0,0,null);
    /** Culling: turns backface/frontface culling on/off: */
    private CullState m_spkCull;

    private Vector<VolumeObject> m_kDisplayList = new Vector<VolumeObject>();
    private boolean m_bSurfaceAdded = false;
    /** Scene translation, centers the scene: */
    private Vector3f m_kTranslate;
    /** Normalized volume extents: */
    private float m_fX, m_fY, m_fZ, m_fMax;
    /** Lights from JPanelLight */
    private GeneralLight[] m_akLights = null;
    
    private VolumeImage m_kVolumeImageA;
    private VolumeImage m_kVolumeImageB;
    
    /** Reference to imageA in ViewJFrameVolumeView: */
    private ModelImage m_kImageA = null;
    /** ModelLUT applied to m_kImageA */
    private ModelLUT m_kLUTa = null;
    /** ModelRGB applied to m_kImageA */
    private ModelRGB m_kRGBTa = null;

    /** Reference to ModelImage imageB in ViewJFrameVolumeView */
    private ModelImage m_kImageB = null;
    /** ModelLUT applied to m_kImageB */
    private ModelLUT m_kLUTb = null;
    /** ModelRGB applied to m_kImageB */
    private ModelRGB m_kRGBTb = null;

    /** Animator object, displays scene in rendering loop (similar to GLUTMainLoop() */
    private Animator m_kAnimator;

    private VolumeViewer m_kParent = null;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * A simple universe is created that contains a viewing platform. A canvas is created and attached to the simple
     * universe and is added to this instance The canvas is used for the 3D rendering. An initially empty scene graph
     * for the application is created and assigned to the universe.
     *
     * @param  _imageA  DOCUMENT ME!
     * @param  _imageB  DOCUMENT ME!
     * @param  _config  DOCUMENT ME!
     * @param  kParent  DOCUMENT ME!
     */
    public MjCorticalAnalysis_WM( VolumeViewer kParent, Animator kAnimator, VolumeImage kVolumeImageA, ModelImage kImageA, ModelLUT kLUTa, ModelRGB kRGBTa,
            VolumeImage kVolumeImageB, ModelImage kImageB, ModelLUT kLUTb, ModelRGB kRGBTb  ) {


        super("GPUVolumeRender",0,0,512,512, new ColorRGBA(0.0f,0.0f,0.0f,0.0f));

        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                m_eBuffering, m_eMultisampling,
                m_iWidth, m_iHeight );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );       

        m_kAnimator = kAnimator;
        m_kVolumeImageA = kVolumeImageA;
        m_kImageA = kImageA;
        m_kLUTa = kLUTa;
        m_kRGBTa = kRGBTa;

        m_kVolumeImageB = kVolumeImageB;
        m_kImageB = kImageB;
        m_kLUTb = kLUTb;
        m_kRGBTb = kRGBTb;
        m_kParent = kParent;


        m_kPanel = new JPanelBrainSurfaceFlattener_WM(this, m_kImageA, kParent);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns the GLCanvas in the m_pkRenderer object.
     * @return OpenGLRenderer.GLCanvas
     */
    public GLCanvas GetCanvas()
    {
        return ((OpenGLRenderer)m_pkRenderer).GetCanvas();
    }

    public void display(GLAutoDrawable arg0) {
        MeasureTime();

        Move();
        //Pick();

        // Draw the scene to the back buffer/
        if (m_pkRenderer.BeginScene())
        {
            m_pkRenderer.SetBackgroundColor(ColorRGBA.BLACK);
            m_pkRenderer.ClearBuffers();

            Render();
            m_pkRenderer.EndScene();
        }
        m_pkRenderer.DisplayBackBuffer();

        UpdateFrameCount();       
        
        if ( m_bSurfaceAdded )
        {
            m_bSurfaceAdded = false;
            //updateLighting( m_akLights );
            for ( int i = 0; i < m_kDisplayList.size(); i++ )
            {
                if ( m_kDisplayList.get(i) instanceof VolumeSurface )
                {
                    ((VolumeSurface)m_kDisplayList.get(i)).InitClip(new float[] { 0, 1, 0, 1, 0, 1 });
                }
            }
        }
    }

    private void Move()
    {
        if (MoveCamera())
        {
            m_kCuller.ComputeVisibleSet(m_spkScene);
        }        
        if (MoveObject())
        {
            m_spkScene.UpdateGS();
            m_kCuller.ComputeVisibleSet(m_spkScene);
            
            for ( int i = 0; i < m_kDisplayList.size(); i++ )
            {
                m_kDisplayList.get(i).GetScene().Local.SetRotateCopy(m_spkScene.Local.GetRotate());
            }
        }
    }
    private void Render()
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            m_kDisplayList.get(i).Render( m_pkRenderer, m_kCuller );
        }
    }

    
    public void displayChanged(GLAutoDrawable arg0, boolean arg1, boolean arg2) {
        // TODO Auto-generated method stub
        
    }

    public void init(GLAutoDrawable arg0) {
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        ((OpenGLRenderer)m_pkRenderer).InitializeState();
        m_pkRenderer.SetLineWidth(3);

        super.OnInitialize();

        // set up camera
        m_spkCamera.SetFrustum(60.0f,m_iWidth/(float)m_iHeight,0.01f,10.0f);
        Vector3f kCDir = new Vector3f(0.0f,0.0f,1.0f);
        Vector3f kCUp = new Vector3f(0.0f, -1.0f,0.0f);
        Vector3f kCRight = new Vector3f();
        kCDir.Cross(kCUp, kCRight);
        Vector3f kCLoc = new Vector3f(kCDir);
        kCLoc.scaleEquals(-1.4f);
        m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);

        CreateScene( arg0 );

        // initial update of objects
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();

        // initial culling of scene
        m_kCuller.SetCamera(m_spkCamera);
        m_kCuller.ComputeVisibleSet(m_spkScene);

        InitializeCameraMotion(.05f,0.001f);
        InitializeObjectMotion(m_spkScene);

        //((OpenGLRenderer)m_pkRenderer).ClearDrawable( );

        m_kAnimator.add( GetCanvas() );      
    }

    public void reshape(GLAutoDrawable arg0, int arg1, int arg2, int arg3, int arg4) {
        // TODO Auto-generated method stub
        
    }
    
    /**
     * Called by the init() function. Creates and initialized the scene-graph.
     * @param arg0, the GLCanvas
     */
    private void CreateScene (GLAutoDrawable arg0)
    {
        // Create a scene graph with the face model as the leaf node.
        m_spkScene = new Node();
        m_spkCull = new CullState();
        m_spkScene.AttachGlobalState(m_spkCull);
        
        m_kTranslate = new Vector3f(Vector3f.ZERO);

        float fMaxX = (float) (m_kImageA.getExtents()[0] - 1) * m_kImageA.getFileInfo(0).getResolutions()[0];
        float fMaxY = (float) (m_kImageA.getExtents()[1] - 1) * m_kImageA.getFileInfo(0).getResolutions()[1];
        float fMaxZ = (float) (m_kImageA.getExtents()[2] - 1) * m_kImageA.getFileInfo(0).getResolutions()[2];

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
        
        for ( int i = 0; i < m_pkRenderer.GetMaxLights(); i++ )
        {
            m_pkRenderer.SetLight( i, new Light() );
        }
    }
    
    /**
     * Called on setup, when a new triangle mesh is loaded, or when the puncture triangle is selected and the
     * "recalculate conformal" button is pressed by the user:
     */
    public void calculateConformal() {

        /* conformally map mesh to plane, sphere, and cylinder */
        /* set the user-selected puncture triangle: */
        m_kCortical.setPunctureTriangle(m_aiTriIndex);

        /* Restore previously picked triangle colors: */
        restoreTriColor();
        m_kCortical.computeConformalMapping();

        /* Setup for sphere nodes. */
        //createGeometry(m_kCortical.getSphereCoordinates(), akColor, aiConnect);

        /* Setup for plane nodes. */
        //createGeometry(m_kCortical.getCylinderCoordinates(), akColor, aiConnect);

        setupLight();

        m_iNumPicked = 0;
        m_iRunningNumPicked = 0;
    }

    /**
     * toggle between display of mean curvature-based colors and average convexity-based colors.
     *
     * @return  DOCUMENT ME!
     */
    public boolean displayConvexityColors() {

        if (m_bCurvatureColors && (null != m_kCortical.getAvrConvexity())) {
            m_bCurvatureColors = false;
            setColorsLUT();
        }

        return m_bCurvatureColors;
    }

    /**
     * toggle between display of mean curvature-based colors and average convexity-based colors.
     *
     * @return  DOCUMENT ME!
     */
    public boolean displayCurvatureColors() {

        if (!m_bCurvatureColors) {
            m_bCurvatureColors = true;
            setColorsLUT();
        }

        return m_bCurvatureColors;
    }

    /**
     * Switch to displaying the plane:
     */
    public void displayPlane() {
        //m_kSwitchDisplay.setWhichChild(PLANE);
    }

    /**
     * Switch to displaying the sphere:
     */
    public void displaySphere() {
        //m_kSwitchDisplay.setWhichChild(SPHERE);
    }

    /**
     * DOCUMENT ME!
     */
    public void disposeLocal() {

        if (m_kPanel != null) {
            m_kPanel = null;
        }

        if (m_kCortical != null) {
            m_kCortical = null;
        }

        if (m_kLUTCurvature != null) {
            m_kLUTCurvature = null;
        }

        if (m_kLUTConvexity != null) {
            m_kLUTConvexity = null;
        }

        if (m_kMouseEvent != null) {
            m_kMouseEvent = null;
        }

        if (m_akTriColors != null) {
            m_akTriColors = null;
        }

        if (m_aiTriIndex != null) {
            m_aiTriIndex = null;
        }
    }

    /**
     * Draw the user-selected point, either as a sphere on the triangle mesh, or as a black triangle, depending on which
     * picking is enabled:
     *
     * @param  kStart   DOCUMENT ME!
     * @param  aiIndex  DOCUMENT ME!
     * @param  iWhich   DOCUMENT ME!
     */
    public void drawPicked(Vector3f kStart, int[] aiIndex, int iWhich) {

        if (m_bPickCorrespondenceEnabled) {
            drawPoint(kStart, aiIndex, iWhich);
        } else if (m_bPickPunctureEnabled) {
            drawTriangle(kStart, aiIndex, iWhich);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public JPanel getMainPanel() {
        return m_kPanel.getMainPanel();
    }
    
    public JPanelBrainSurfaceFlattener_WM getPanel() {
        return m_kPanel;
    }
    
    /**
     * Resizig the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   int width
     * @param  frameHeight  int height
     */
    public void resizePanel(int panelWidth, int frameHeight) {
        m_kPanel.resizePanel(panelWidth, frameHeight);
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getMaxAverageConvexity() {
        return m_kCortical.getMaxAvrConvexity();
    }

    /**
     * return the max mean curvature:
     *
     * @return  DOCUMENT ME!
     */
    public float getMaxCurvature() {
        return m_kCortical.getMaxMeanCurvature();
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getMinAverageConvexity() {
        return m_kCortical.getMinAvrConvexity();
    }

    /**
     * return the min mean curvature:
     *
     * @return  DOCUMENT ME!
     */
    public float getMinCurvature() {
        return m_kCortical.getMinMeanCurvature();
    }


    /**
     * Gets the scene state in terms of slices visible, which slice number, etc.
     *
     * @return  A private object extension specific to the class that extends this one.
     */
    public Object getSceneState() {
        return null;
    }

    /**
     * perform inflation step and then render shapes using the average convexity colors.
     *
     * @return  DOCUMENT ME!
     */
    public boolean inflation() {

        if (!m_bInflationInitialized) {
            m_bInflationInitialized = true;
            m_kCortical.doInflation(0);
        }

        m_kCortical.doInflation(1);
        m_kCortical.doInflation(2);

        // m_bCurvatureColors = false;
        //m_kTriangleMesh.setCoordinates(0, m_kCortical.getPoints());

        return m_bCurvatureColors;
    }

    /**
     * Return whether or not picking correspondence points is enabled:
     *
     * @return  DOCUMENT ME!
     */
    public boolean isCorrespondencePickEnabled() {
        return m_bPickCorrespondenceEnabled;
    }

    /**
     * Returns whether or not picking is enabled:
     *
     * @return  DOCUMENT ME!
     */
    public boolean isPickEnabled() {
        return m_bPickCorrespondenceEnabled | m_bPickPunctureEnabled;
    }

    /**
     * Return whether or not picking the puncture triangle is enabled:
     *
     * @return  DOCUMENT ME!
     */
    public boolean isPuncturePickEnabled() {
        return m_bPickPunctureEnabled;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  kEvent  the mouse event generated by a mouse drag
     */
    public void mouseDragged(MouseEvent e)
    {
        if ( !e.isControlDown() )
        {
            super.mouseDragged(e);
        }
        //else if ( e.isControlDown() && m_bPaintEnabled )
        //{
            //m_iXPick = e.getX();
            //m_iYPick = e.getY();
            //m_bPickPending = true;
        //}
    }

    /**
     * DOCUMENT ME!
     *
     * @param  kMouseEvent  the mouse event generated by a mouse press
     */
    public void mousePressed(MouseEvent e) {
        if ( !e.isControlDown() )
        {
            super.mousePressed(e);
        }
        
        /* Only capture mouse events when drawing the geodesic curve is
         * enabled, and only when the control key is down and the left mouse
         * button is pressed. */
        //if ((m_bPickCorrespondenceEnabled | m_bPickPunctureEnabled) && e.isControlDown()) {

            /* m_bMousePressed and m_bMouseReleased are set explicitly to
             * prevent multiple mouse clicks at the same location.  If the mouse has been released, then set
             * mousePressed to true and save the location of the mouse event.
             */
            //if (m_bMouseReleased == true) {

                //if ((e.getButton() == MouseEvent.BUTTON1)) {
                 //   m_bMousePressed = true;
                 //   m_bMouseReleased = false;

                 //   m_kMouseEvent = e;
               // }
           // }
        //}

    }

    /**
     * DOCUMENT ME!
     *
     * @param  kEvent  the mouse event generated by a mouse release
     */
    public void mouseReleased(MouseEvent e) {

        super.mouseReleased(e);
//         /* If the mouse has been pressed, but not released: */
//         if (m_bMousePressed && !m_bMouseReleased) {
//             m_bMousePressed = false;
//             m_bMouseReleased = true;

//             /* If the pickCanvas is null, then do not try to pick */
//             if (m_kPickCanvas == null) {
//                 return;
//             }

//             /* Set the location for picking that was stored when the mouse was
//              * presed: */
//             m_kPickCanvas.setShapeLocation(m_kMouseEvent);

//             PickResult kPickResult = null;

//             /* Try to get the closest picked polygon, catch the
//              * javax.media.j3d.CapabilityNotSetException. */
//             try {
//                 kPickResult = m_kPickCanvas.pickClosest();
//             } catch (javax.media.j3d.CapabilityNotSetException e) {
//                 return;
//             }

//             /* If the pickResult is not null, mark the picked point and, if
//              * this is the second point in a sequence, then draw the geodesic
//              * curve. */
//             if (kPickResult != null) {

//                 /* Pick the first intersection since we executed a pick
//                  * closest. */
//                 int iClosestPick = 0;
//                 int iWhich = m_kSwitchDisplay.getWhichChild();

//                 if (iWhich == PLANE) {
//                     float fMinDistance = Float.MAX_VALUE;

//                     for (int i = 0; i < kPickResult.numIntersections(); i++) {
//                         PickIntersection kPick = kPickResult.getIntersection(i);

//                         /* Get the coordinates of the picked point on the mesh. */
//                         Vector3f kPickPoint = new Vector3f(kPick.getPointCoordinates());

//                         /* Get the triangle indices of the triangle that that
//                          * pickPoint falls in:  */
//                         int[] aiIndex = kPick.getPrimitiveCoordinateIndices();

//                         float fDistance = closestPlanePointIndex(kPickPoint, aiIndex);

//                         if (fDistance < fMinDistance) {
//                             fMinDistance = fDistance;
//                             iClosestPick = i;
//                         }
//                     }
//                 }

//                 PickIntersection kPick = kPickResult.getIntersection(iClosestPick);

//                 /* Get the coordinates of the picked point on the mesh. */
//                 Vector3f kPickPoint = new Vector3f(kPick.getPointCoordinates());

//                 /* Get the triangle indices of the triangle that that
//                  * pickPoint falls in:  */
//                 int[] aiIndex = kPick.getPrimitiveCoordinateIndices();

//                 drawPicked(kPickPoint, aiIndex, iWhich);
//             }
//         }
    }

    /**
     * Remove all points drawn on the mesh, sphere, and plane:
     */
    public void removePoints() {

        for (int i = 0; i < m_iNumPicked; i++) {
            //m_kTransformGroupSphere.removeChild(2);
            //m_kTransformGroupPlane.removeChild(2);
            //m_kTransformGroupMesh.removeChild(1);
        }

        m_iNumPicked = 0;
        m_iRunningNumPicked = 0;
    }

    /**
     * Sets the alpha blending of parameter for two image displaying.
     *
     * @param  value  amount [0,100] that is the percentage of Image A to be displayed and (1-percentage) of Image B to
     *                be displayed
     */
    public void setAlphaBlend(int value) { }

    /**
     * Used by the JPanelMouse the redisplay the recorded actions.
     *
     * @param  scene  Object
     */
    public void setGUI(Object scene) { }

    /**
     * DOCUMENT ME!
     *
     * @param  kLUT  DOCUMENT ME!
     */
    public void setLUTConvexity(ModelLUT kLUT) {

        if (m_kLUTConvexity != null) {
            m_kLUTConvexity = null;
        }

        m_kLUTConvexity = kLUT;

        setColorsLUT();
    }

    /**
     * Update the colormap based on the LUT:
     *
     * @param  kLUT  DOCUMENT ME!
     */
    public void setLUTCurvature(ModelLUT kLUT) {

        if (m_kLUTCurvature != null) {
            m_kLUTCurvature = null;
        }

        m_kLUTCurvature = kLUT;

        setColorsLUT();
    }

    /**
     * setRGBTA.
     *
     * @param  RGBT  ModelRGB
     */
    public void setRGBTA(ModelRGB RGBT) { }

    /**
     * setRGBTB.
     *
     * @param  RGBT  ModelRGB
     */
    public void setRGBTB(ModelRGB RGBT) { }

    /**
     * setSlice.
     *
     * @param  slice  int
     */
    public void setSlice(int slice) { }

    /**
     * setTimeSlice.
     *
     * @param  tSlice  int
     */
    public void setTimeSlice(int tSlice) { }

    /**
     * Perform all initialization prior to executing. Loads the sample mesh surface and sets up the rendering of the
     * scene.
     *
     * @param  kTriangleMesh  DOCUMENT ME!
     */
    public void setup(TriMesh kMesh) {

        /* reset inflation initialization: */
        m_bInflationInitialized = false;

        System.err.println( "MjAnalysis setup creating MjCorticalMesh ");
       
        m_kCortical = new MjCorticalMesh_WM(kMesh);

        System.err.println( "DONE creating MjCorticalMesh ");
        System.err.println( "Start computeMeanCurvature ");
        /* cortical mesh initializations */
        m_kCortical.computeMeanCurvature();

        System.err.println( "DONE computeMeanCurvature ");
        /* Setup for mesh: */
        //addSurface(kMesh, false);

        /* calculate the conformal mapping of mesh to the flattened plane and
         * sphere maps, and setup the display for the plane and sphere: */
        System.err.println( "Start calculateConformal ");
        calculateConformal();
        System.err.println( "DONE calculateConformal ");
    }

    
    public void addSurface(TriMesh kSurfaces, boolean bReplace)
    {
        VolumeSurface kSurface = new VolumeSurface( m_pkRenderer, m_kVolumeImageA,
                m_kTranslate,
                m_fX, m_fY, m_fZ,
                kSurfaces, bReplace );
        kSurface.SetPerPixelLighting( m_pkRenderer, true );
        kSurface.SetDisplay(true);
        m_kDisplayList.add( kSurface );

        UpdateSceneRotation();
        m_bSurfaceAdded = true;
    } 
    

    private void UpdateSceneRotation()
    {
        m_spkScene.UpdateGS();
        m_kCuller.ComputeVisibleSet(m_spkScene);
        
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            m_kDisplayList.get(i).GetScene().Local.SetRotateCopy(m_spkScene.Local.GetRotate());
        }
    }
    
    
    /**
     * Called from JPanelLight. Updates the lighting parameters.
     * @param akGLights, the set of GeneralLight objects.
     */
    public void updateLighting(GeneralLight[] akGLights )
    {
        if ( akGLights == null )
        {
            return;
        }
        m_akLights = akGLights;
        for ( int i = 0; i < akGLights.length; i++ )
        {
            String kLightType = new String("Light"+(i)+"Type");
            float[] afType = new float[]{0,0,0,0};
            //if ( i < m_pkRenderer.GetMaxLights() )
            if ( i < 4 )
            {
                if ( akGLights[i].isEnabled() )
                {
                    Light kLight = akGLights[i].createWMLight();
                    m_pkRenderer.SetLight( i, kLight );
                    if ( akGLights[i].isTypeAmbient() )
                    {
                        afType[0] = 0;
                    }
                    else if ( akGLights[i].isTypeDirectional() )
                    {
                        afType[0] = 1;
                    }
                    else if ( akGLights[i].isTypePoint() )
                    {
                        afType[0] = 2;
                    }
                    else if ( akGLights[i].isTypeSpot() )
                    {
                        afType[0] = 3;
                    }
                    for ( int j = 0; j < m_kDisplayList.size(); j++ )
                    {
                        m_kDisplayList.get(j).SetLight(kLightType, afType);
                    }

                }
                else
                {
                    m_pkRenderer.SetLight( i, new Light() );
                    afType[0] = -1;
                    for ( int j = 0; j < m_kDisplayList.size(); j++ )
                    {
                        m_kDisplayList.get(j).SetLight(kLightType, afType);
                    }
                }
            }
        }

    }


    
    /**
     * Called when the mesh is created, or when the number of latitude or longitude lines is changed by the user. When
     * the user changes the number of latitude or longitude lines, the meshes are removed from the scene and recreated.
     *
     * @param  iNumLat  DOCUMENT ME!
     * @param  iNumLon  DOCUMENT ME!
     */
    public void setupLatLon(int iNumLat, int iNumLon) {

        /* Remove existing meshes from the scene: */

        /* bias for latitude/longitude to handle z-buffer fighting */
        float fMBias = 0.01f;
        float fSBias = 0.01f;
        float fPBias = 0.01f;

        /* iNumLat latitute and iNumLon longitude per object */
        Vector3f[] akPolylinePointsMesh = new Vector3f[0];
        Vector3f[] akPolylinePointsSphere = new Vector3f[0];
        Vector3f[] akPolylinePointsPlane = new Vector3f[0];
        int iNumPoints = 1 + (2 * (int) (iNumLat / 2)) + iNumLon;
        int[] aiPointsMesh = new int[iNumPoints];
        int[] aiPointsSphere = new int[iNumPoints];
        int[] aiPointsPlane = new int[iNumPoints];
        int iPolyline = 0;

        /* compute draw latitude lines */
        for (int i = -(int) (iNumLat / 2); i <= (int) (iNumLat / 2); i++) {
            float fZNormal = i / (float) (1 + (iNumLat / 2));

            MjCorticalMesh_WM.Polylines kPolylines = m_kCortical.getLatitude(fZNormal, fMBias, fSBias, fPBias);

            /* how many points for this latitude? */
            aiPointsMesh[iPolyline] = kPolylines.akMVertex.length;
            aiPointsSphere[iPolyline] = kPolylines.akSVertex.length;
            aiPointsPlane[iPolyline] = kPolylines.akPVertex.length;
            ++iPolyline;

            /* append the vertices */
            akPolylinePointsMesh = combine(akPolylinePointsMesh, kPolylines.akMVertex);
            akPolylinePointsSphere = combine(akPolylinePointsSphere, kPolylines.akSVertex);
            akPolylinePointsPlane = combine(akPolylinePointsPlane, kPolylines.akPVertex);
        }

        /* compute longitude lines */
        for (int i = 0; i <= (iNumLon - 1); i++) {
            float fAngle = i * 2.0f * (float) Math.PI / (float) (iNumLon - 1);

            MjCorticalMesh_WM.Polylines kPolylines = m_kCortical.getLongitude(fAngle, fMBias, fSBias, fPBias);

            /* how many points for this latitude? */
            aiPointsMesh[iPolyline] = kPolylines.akMVertex.length;
            aiPointsSphere[iPolyline] = kPolylines.akSVertex.length;
            aiPointsPlane[iPolyline] = kPolylines.akPVertex.length;
            ++iPolyline;

            /* append the vertices */
            akPolylinePointsMesh = combine(akPolylinePointsMesh, kPolylines.akMVertex);
            akPolylinePointsSphere = combine(akPolylinePointsSphere, kPolylines.akSVertex);
            akPolylinePointsPlane = combine(akPolylinePointsPlane, kPolylines.akPVertex);
        }

    }

    /**
     * stateChanged.
     *
     * @param  e  ChangeEvent
     */
    public void stateChanged(ChangeEvent e) { }

    /**
     * Toggle between displaying the latitude and longitude lines:
     */
    public void toggleLatLonLines() {
    }

    /**
     * Enables picking with the mouse and drawing the curve on the mesh.
     */
    public void togglePickCorrespondence() {
        m_bPickCorrespondenceEnabled = !m_bPickCorrespondenceEnabled;
    }

    /**
     * Enables picking with the mouse and drawing the curve on the mesh.
     */
    public void togglePickPuncture() {
        m_bPickPunctureEnabled = !m_bPickPunctureEnabled;

        if (m_bPickPunctureEnabled == false) {

            /* Restore previously picked triangle colors: */
            restoreTriColor();
        }
    }

    /**
     * updateImageExtents.
     *
     * @return  boolean
     */
    public boolean updateImageExtents() {
        return false;
    }

    /**
     * updateImages.
     *
     * @return  boolean
     */
    public boolean updateImages() {
        return false;
    }

    /**
     * updateImages.
     *
     * @param   flag  boolean
     *
     * @return  boolean
     */
    public boolean updateImages(boolean flag) {
        return false;
    }

    /**
     * updateImages.
     *
     * @param   LUTa        ModelLUT
     * @param   LUTb        ModelLUT
     * @param   flag        boolean
     * @param   interpMode  int
     *
     * @return  boolean
     */
    public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean flag, int interpMode) {
        return false;
    }

    /**
     * DOCUMENT ME!
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
    }

    /**
     * DOCUMENT ME!
     *
     * @param   akPointA  DOCUMENT ME!
     * @param   akPointB  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private static Vector3f[] combine(Vector3f[] akPointA, Vector3f[] akPointB) {
        Vector3f[] akPointCombined = new Vector3f[akPointA.length + akPointB.length];
        int iIndex = 0;

        for (int i = 0; i < akPointA.length; i++) {
            akPointCombined[iIndex++] = akPointA[i];
        }

        for (int i = 0; i < akPointB.length; i++) {
            akPointCombined[iIndex++] = akPointB[i];
        }

        return akPointCombined;
    }

    /**
     * For the plane, all vertices have z-value set to zero. Because the mesh is closed, even for the plane, the edge
     * triangles wrap around the back of the mesh, and are co-planar with the forward-facing triangles (the ones that
     * display the data) -- when picking is done, the pickClosest sometimes returns the back-facing triangle. This
     * function returns the correct, visible triangle for the plane:
     *
     * @param   kStart   DOCUMENT ME!
     * @param   aiIndex  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float closestPlanePointIndex(Vector3f kStart, int[] aiIndex) {
        Vector3f[] akPlanePoints = m_kCortical.getCylinderCoordinates();

        float fDistance = Float.MAX_VALUE;
        float fMinDistance = Float.MAX_VALUE;

        for (int i = 0; i < aiIndex.length; i++) {
            fDistance = kStart.sub(akPlanePoints[aiIndex[i]]).SquaredLength();

            if (fDistance < fMinDistance) {
                fMinDistance = fDistance;
            }
        }

        return fMinDistance;
    }

    /**
     * Draw the user-selected point as a sphere on the triangle mesh:
     *
     * @param  kStart   DOCUMENT ME!
     * @param  aiIndex  DOCUMENT ME!
     * @param  iWhich   DOCUMENT ME!
     */
    private void drawPoint(Vector3f kStart, int[] aiIndex, int iWhich) {
        /*
        m_iNumPicked++;

        if (m_iNumPicked > MAX_POINTS) {
            m_kTransformGroupSphere.removeChild(2);
            m_kTransformGroupPlane.removeChild(2);
            m_kTransformGroupMesh.removeChild(1);
            m_iNumPicked--;
        }

        Shape3D kSphere = new Sphere(.05f).getShape();
        ColorRGB kColor = new ColorRGB(1f, 0f, 0f);
        Material kMaterial = new Material(m_akPickColors[m_iRunningNumPicked % MAX_POINTS], new ColorRGB(0f, 0f, 0f),
                                          m_akPickColors[m_iRunningNumPicked % MAX_POINTS],
                                          m_akPickColors[m_iRunningNumPicked % MAX_POINTS], 128.0f);
        kMaterial.setLightingEnable(true);
        m_iRunningNumPicked++;

        kSphere.getAppearance().setMaterial(kMaterial);
        kSphere.setPickable(false);

        int iVQuantity = m_kCortical.getVQuantity();
        Vector3f[] akPlanePoints = m_kCortical.getCylinderCoordinates();
        Vector3f[] akSpherePoints = m_kCortical.getSphereCoordinates();
        Vector3f[] akMeshPoints = m_kCortical.getPoints();


        float fDistance = Float.MAX_VALUE;
        float fMinDistance = Float.MAX_VALUE;
        int iDistance = 0;
*/
        /* Find the nearest triangle vertex to the picked point: */
        /*
        for (int i = 0; i < aiIndex.length; i++) {

            if (iWhich == SPHERE) {
                fDistance = kStart.distanceSquared(akSpherePoints[aiIndex[i]]);
            } else if (iWhich == PLANE) {
                fDistance = kStart.distanceSquared(akPlanePoints[aiIndex[i]]);
            } else if (iWhich == MESH) {
                fDistance = kStart.distanceSquared(akMeshPoints[aiIndex[i]]);
            }

            if (fDistance < fMinDistance) {
                fMinDistance = fDistance;
                iDistance = aiIndex[i];
            }
        }
*/
    }

    /**
     * Draw the user-selected triangle as a black triangle, save the original color so it can be restored:
     *
     * @param  kStart   DOCUMENT ME!
     * @param  aiIndex  DOCUMENT ME!
     * @param  iWhich   DOCUMENT ME!
     */
    private void drawTriangle(Vector3f kStart, int[] aiIndex, int iWhich) {
        /*
        ColorRGBA[] akColor = m_kCortical.getColors();

        // Restore previously picked triangle colors:
        restoreTriColor();

        // Set the picked triangle to black, saving it's color:
        m_akTriColors = new ColorRGBA[aiIndex.length];
        m_aiTriIndex = new int[aiIndex.length];

        for (int i = 0; i < aiIndex.length; i++) {
            m_akTriColors[i] = new ColorRGBA();
            m_akTriColors[i].x = akColor[aiIndex[i]].x;
            m_akTriColors[i].y = akColor[aiIndex[i]].y;
            m_akTriColors[i].z = akColor[aiIndex[i]].z;
            m_akTriColors[i].w = akColor[aiIndex[i]].w;

            akColor[aiIndex[i]].x = 0f;
            akColor[aiIndex[i]].y = 0f;
            akColor[aiIndex[i]].z = 0f;

            m_aiTriIndex[i] = aiIndex[i];
        }

        m_kGeometryArraySphere.setColors(0, akColor);
        m_kTriangleMesh.setColors(0, akColor);
        m_kGeometryArrayPlane.setColors(0, akColor);
        */
    }

    /**
     * Resets the previously picked triangle to it's orginal color:
     */
    private void restoreTriColor() {
/*
        if ((m_akTriColors != null) && (m_aiTriIndex != null)) {
            ColorRGBA[] akColor = m_kCortical.getColors();

            for (int i = 0; i < m_aiTriIndex.length; i++) {
                akColor[m_aiTriIndex[i]].x = m_akTriColors[i].x;
                akColor[m_aiTriIndex[i]].y = m_akTriColors[i].y;
                akColor[m_aiTriIndex[i]].z = m_akTriColors[i].z;
                akColor[m_aiTriIndex[i]].w = m_akTriColors[i].w;
            }

            m_akTriColors = null;
            m_aiTriIndex = null;

            m_kGeometryArraySphere.setColors(0, akColor);
            m_kTriangleMesh.setColors(0, akColor);
            m_kGeometryArrayPlane.setColors(0, akColor);
        }
        */
    }

    /**
     * Update the colormap based on the LUT:
     */
    private void setColorsLUT() {

        int iLutSize = 0;
        float[] afXLUTa = null;
        float[] afYLUTa = null;
        int iLutHeightA = 0;
        int[] aiLutBufferRemapped = null;

        ModelLUT kLUT;

        float[] afCArray = null;

        if (!m_bCurvatureColors && (null != m_kCortical.getAvrConvexity())) {
            afCArray = m_kCortical.getAvrConvexity();
            kLUT = m_kLUTConvexity;
        } else if (m_bCurvatureColors) {
            afCArray = m_kCortical.getMeanCurvature();
            kLUT = m_kLUTCurvature;
        } else {
            return;
        }

        iLutSize = kLUT.getTransferFunction().size();
        afXLUTa = new float[iLutSize];
        afYLUTa = new float[iLutSize];

        for (int iLIndex = 0; iLIndex < iLutSize; iLIndex++) {
            afXLUTa[iLIndex] = ((Point2Df) (kLUT.getTransferFunction().getPoint(iLIndex))).x;
            afYLUTa[iLIndex] = 255 - ((Point2Df) (kLUT.getTransferFunction().getPoint(iLIndex))).y;
        }

        iLutHeightA = kLUT.getExtents()[1];
        aiLutBufferRemapped = new int[iLutHeightA];
        kLUT.exportIndexedLUT(aiLutBufferRemapped);


        int iVQuantity = m_kCortical.getVQuantity();

        for (int i = 0; i < iVQuantity; i++) {
            float fValue = afCArray[i];
            int iPixColor = 0;

            for (int iLut = 0; iLut < (iLutSize - 1); iLut++) {

                if ((fValue >= afXLUTa[iLut]) && (fValue <= afXLUTa[iLut + 1])) {
                    float fSlope = 0;

                    if ((afXLUTa[iLut + 1] - afXLUTa[iLut]) != 0) {
                        fSlope = (afYLUTa[iLut + 1] - afYLUTa[iLut]) / (afXLUTa[iLut + 1] - afXLUTa[iLut]);
                    }

                    iPixColor = (int) (afYLUTa[iLut] + (fSlope * (fValue - afXLUTa[iLut])) + 0.5);

                    break;
                }
            }

            int iValue = aiLutBufferRemapped[iPixColor];
            float fRed = (iValue & 0x00ff0000) >> 16;
            float fGreen = (iValue & 0x0000ff00) >> 8;
            float fBlue = (iValue & 0x000000ff);
            
            m_kCortical.setColor( i, fRed / 255.0f, fGreen / 255.0f, fBlue / 255.0f );
        }

        //m_kGeometryArraySphere.setColors(0, akColor);
        //m_kTriangleMesh.setColors(0, akColor);
        //m_kGeometryArrayPlane.setColors(0, akColor);

    }

    /**
     * Sets up a direction lightsource for the scene.
     */
    private void setupLight() {
        /*
        DirectionalLight kLightDr = new DirectionalLight(true, new ColorRGB(1f, 1f, 1f), new Vector3f(0f, 0f, -1f));
        kLightDr.setInfluencingBounds(bounds);

        BranchGroup kLightBG = new BranchGroup();
        kLightBG.setCapability(BranchGroup.ALLOW_DETACH);
        kLightBG.addChild(kLightDr);
        sceneRootTG.addChild(kLightBG);
        */
    }
}
