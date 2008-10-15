package gov.nih.mipav.view.renderer.WildMagic.brainflattenerview_WM;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.renderer.WildMagic.*;
import gov.nih.mipav.view.renderer.WildMagic.Render.*;

import java.awt.event.*;
import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;
import javax.swing.*;
import com.sun.opengl.util.Animator;


import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Collision.*;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.Rendering.Light;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.SceneGraph.*;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;


/**
 * Cortical analysis applet and viewer. This class contains the static 'main' method to launch the application. This
 * class is an Applet which means that it can be embedded in a browser or it can be attached to a MainFrame when
 * executed as an application.
 */
public class CorticalAnalysisRender extends GPURenderBase implements GLEventListener {

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
    private JPanelBrainSurfaceFlattener_WM m_kPanel = null;

    /** DOCUMENT ME! */
    private int MAX_POINTS = 6;

    private boolean m_bFirst = true;

    private VolumeSurface m_kSphere = null;
    private VolumeSurface m_kCylinder = null;
    private Node m_kSphereLines = null;
    private Node m_kCylinderLines = null;
    private Node m_kMeshLines = null;
    private VertexColor3Effect m_kPolylineShader;
    private boolean m_bDisplayLines = true;
    
    private Node m_kSpherePoints = null;
    private Node m_kCylinderPoints = null;
    private Node m_kMeshPoints = null;
    
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
    public CorticalAnalysisRender( VolumeTriPlanarInterface kParent, Animator kAnimator, VolumeImage kVolumeImageA, ModelImage kImageA, ModelLUT kLUTa, ModelRGB kRGBTa,
            VolumeImage kVolumeImageB, ModelImage kImageB, ModelLUT kLUTb, ModelRGB kRGBTb  ) {


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

    public void display(GLAutoDrawable arg0)
    {
        if ( !m_bInit )
        {
            init(arg0);
        }
        MeasureTime();

        Move();
        Pick();

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
        
        if ( m_bSurfaceUpdate )
        {
            m_bSurfaceUpdate = false;

            UpdateSceneRotation();
            for ( int i = 0; i < m_kDisplayList.size(); i++ )
            {
                if ( m_kDisplayList.get(i) instanceof VolumeSurface )
                {
                    ((VolumeSurface)m_kDisplayList.get(i)).SetPerPixelLighting( m_pkRenderer, true );
                }
            }            
            updateLighting( m_akLights );
        }
        if ( m_bFirst )
        {
            m_bFirst = false;
            displayPlane();
        }
    }

    private void Render()
    {
        m_kCuller.ComputeVisibleSet(m_spkScene);
        m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            m_kDisplayList.get(i).Render( m_pkRenderer, m_kCuller );
        }
    }

    protected void Pick()
    {
        Vector3f kPos = new Vector3f(0,0,10);
        Vector3f kDir = new Vector3f(0,0,1);  // the pick ray

        if (m_bPickPending)
        {
            if (m_spkCamera.GetPickRay(m_iXPick,m_iYPick,GetWidth(),
                                       GetHeight(),kPos,kDir))
            {
                m_bPickPending = false;
                for ( int i = 0; i < m_kDisplayList.size(); i++ )
                {
                    if ( m_kDisplayList.get(i).GetPickable() )
                    {
                        m_kPicker.Execute(m_kDisplayList.get(i).GetScene(),kPos,kDir,0.0f,
                                          Float.MAX_VALUE);
                        if (m_kPicker.Records.size() > 0)
                        {
                            int iClosestPick = 0;
                            PickRecord kPicked = null;
                            if (m_kDisplayList.get(i) == m_kCylinder )
                            {
                                float fMinDistance = Float.MAX_VALUE;

                                for (int j = 0; j < m_kPicker.Records.size(); j++)
                                {
                                    float fDistance = closestPlanePointIndex(m_kPicker.Records.elementAt(j));

                                    if (fDistance < fMinDistance) {
                                        fMinDistance = fDistance;
                                        iClosestPick = j;
                                    }
                                }
                                kPicked = m_kPicker.Records.elementAt(iClosestPick);
                            }
                            else
                            {
                                kPicked = m_kPicker.GetClosestNonnegative();
                            }
                            if ( kPicked != null )
                            {
                                drawPicked( kPicked.iV0, kPicked.iV1, kPicked.iV2 );
                            }
                        }
                    }
                }
            }
        }
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
        kCRight.Cross( kCDir, kCUp );
        Vector3f kCLoc = new Vector3f(kCDir);
        kCLoc.Scale(-1.4f);
        m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);

        CreateScene();

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
        m_bInit = true;
    }

    /**
     * Called by the init() function. Creates and initialized the scene-graph.
     * @param arg0, the GLCanvas
     */
    private void CreateScene ()
    {
        // Create a scene graph with the face model as the leaf node.
        m_spkScene = new Node();
        m_spkCull = new CullState();
        m_spkScene.AttachGlobalState(m_spkCull);
        
        m_kTranslate = new Vector3f(Vector3f.ZERO);

        float fMaxX = (m_kImageA.getExtents()[0] - 1) * m_kImageA.getFileInfo(0).getResolutions()[0];
        float fMaxY = (m_kImageA.getExtents()[1] - 1) * m_kImageA.getFileInfo(0).getResolutions()[1];
        float fMaxZ = (m_kImageA.getExtents()[2] - 1) * m_kImageA.getFileInfo(0).getResolutions()[2];

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

        if ( m_kSphere != null )
        {
            m_kDisplayList.remove( m_kSphere );
            m_kSphere.dispose();
            m_kSphere = null;
        }
        if ( m_kCylinder != null )
        {
            m_kDisplayList.remove( m_kCylinder );
            m_kCylinder.dispose();
            m_kCylinder = null;
        }
        
        /* Setup for sphere nodes. */
        m_kSphere = addSurface( m_kCortical.getSphere(), false );
        /* Setup for plane nodes. */
        m_kCylinder = addSurface( m_kCortical.getCylinder(), false );
        m_kCylinder.SetBackface(true);
        
        if ( !m_bFirst )
        {
            m_pkRenderer.LoadResources( m_kSphere.GetMesh() );
            m_pkRenderer.LoadResources( m_kCylinder.GetMesh() );
        }
        
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
    public void displayPlane()
    {
        m_kSphere.SetDisplay(false);
        m_kCylinder.SetDisplay(true);
        m_spkScene.DetachChild( m_kSphereLines );
        m_spkScene.DetachChild( m_kCylinderLines );
        m_spkScene.DetachChild( m_kSpherePoints );
        m_spkScene.DetachChild( m_kCylinderPoints );
        if ( m_bDisplayLines )
        {
            m_spkScene.AttachChild( m_kCylinderLines );
        }
        m_spkScene.AttachChild( m_kCylinderPoints );
        m_spkScene.UpdateGS();
    }

    /**
     * Switch to displaying the sphere:
     */
    public void displaySphere()
    {
        m_kSphere.SetDisplay(true);
        m_kCylinder.SetDisplay(false);
        m_spkScene.DetachChild( m_kSphereLines );
        m_spkScene.DetachChild( m_kCylinderLines );
        m_spkScene.DetachChild( m_kSpherePoints );
        m_spkScene.DetachChild( m_kCylinderPoints );
        if ( m_bDisplayLines )
        {
            m_spkScene.AttachChild( m_kSphereLines );
        }
        m_spkScene.AttachChild( m_kSpherePoints );
        m_spkScene.UpdateGS();
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
    public void drawPicked(int iV0, int iV1, int iV2) {

        if (m_bPickCorrespondenceEnabled) {
            drawPoint(iV0);
        } else if (m_bPickPunctureEnabled) {
            drawTriangle(iV0, iV1, iV2);
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

        m_kCortical.updateMesh( true );
        
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
        super.mouseDragged(e);
        if ((m_bPickCorrespondenceEnabled | m_bPickPunctureEnabled) && e.isControlDown())
        {
            m_iXPick = e.getX();
            m_iYPick = e.getY();
            m_bPickPending = true;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  kMouseEvent  the mouse event generated by a mouse press
     */
    public void mousePressed(MouseEvent e) {
        super.mousePressed(e);
        
        /* Only capture mouse events when enabled, and only when the control key is down and the left mouse
         * button is pressed. */
        if ((m_bPickCorrespondenceEnabled | m_bPickPunctureEnabled) && e.isControlDown()) {

            /* m_bMousePressed and m_bMouseReleased are set explicitly to
             * prevent multiple mouse clicks at the same location.  If the mouse has been released, then set
             * mousePressed to true and save the location of the mouse event.
             */
            if ((e.getButton() == MouseEvent.BUTTON1)) {
                m_iXPick = e.getX();
                m_iYPick = e.getY();
                m_bPickPending = true;
            }
        }

    }

    /**
     * Remove all points drawn on the mesh, sphere, and plane:
     */
    public void removePoints() {

        m_kSpherePoints.DetachAllChildren();
        m_kCylinderPoints.DetachAllChildren();
        m_kMeshPoints.DetachAllChildren();
        m_spkScene.UpdateGS();
        m_iNumPicked = 0;
        m_iRunningNumPicked = 0;
    }

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
     * Perform all initialization prior to executing. Loads the sample mesh surface and sets up the rendering of the
     * scene.
     *
     * @param  kTriangleMesh  DOCUMENT ME!
     */
    public boolean setup(TriMesh kMesh ) {

        /* reset inflation initialization: */
        m_bInflationInitialized = false;
        m_kCortical = new MjCorticalMesh_WM(kMesh);
        if ( m_kCortical.CheckManifold() )
        {
        	/* cortical mesh initializations */
        	m_kCortical.computeMeanCurvature();
        	/* calculate the conformal mapping of mesh to the flattened plane and
        	 * sphere maps, and setup the display for the plane and sphere: */
        	calculateConformal();
        	return true;
        }
        return false;
    }

    
    public VolumeSurface addSurface(TriMesh kSurfaces, boolean bReplace)
    {
        VolumeSurface kSurface = new VolumeSurface( m_pkRenderer, m_kVolumeImageA, m_kVolumeImageB,
                m_kTranslate,
                m_fX, m_fY, m_fZ,
                kSurfaces, bReplace );
        kSurface.SetPickable(true);
        kSurface.SetDisplay(true);
        //kSurface.SetPolygonMode( true, WireframeState.FillMode.FM_LINE );
        m_kDisplayList.add( kSurface );
        m_bSurfaceUpdate = true;
        return kSurface;
    } 
    
    public Node getMeshLines()
    {
        return m_kMeshLines;
    }
    
    /**
     * Called from JPanelLight. Updates the lighting parameters.
     * @param akGLights, the set of GeneralLight objects.
     */
    public void updateLighting(Light[] akGLights )
    {
        m_akLights = akGLights;
        if ( !m_bInit )
        {
            return;
        }
        if ( akGLights == null )
        {
            return;
        }
        
        for ( int i = 0; i < akGLights.length; i++ )
        {
            String kLightType = new String("Light"+(i)+"Type");
            float[] afType = new float[]{0,0,0,0};
            //if ( i < m_pkRenderer.GetMaxLights() )
            if ( i < 4 )
            {
                if ( akGLights[i].On )
                {
                    Light kLight = akGLights[i];
                    m_pkRenderer.SetLight( i, kLight );
                    if ( akGLights[i].Type == Light.LightType.LT_AMBIENT )
                    {
                        afType[0] = 0;
                    }
                    else if ( akGLights[i].Type == Light.LightType.LT_DIRECTIONAL )
                    {
                        afType[0] = 1;
                    }
                    else if ( akGLights[i].Type == Light.LightType.LT_POINT )
                    {
                        afType[0] = 2;
                    }
                    else if ( akGLights[i].Type == Light.LightType.LT_SPOT )
                    {
                        afType[0] = 3;
                    }
                    for ( int j = 0; j < m_kDisplayList.size(); j++ )
                    {
                        m_kDisplayList.get(j).SetLight(kLightType, afType);
                    }

                    if ( (m_kSpherePoints != null) && (m_kCylinderPoints != null) && (m_kMeshPoints != null) )
                    {
                        for ( int k = 0; k < m_kSpherePoints.GetQuantity(); k++ )
                        {
                            ((MipavLightingEffect)m_kSpherePoints.GetChild(k).GetEffect(0)).SetLight(kLightType, afType);
                            ((MipavLightingEffect)m_kCylinderPoints.GetChild(k).GetEffect(0)).SetLight(kLightType, afType);
                            ((MipavLightingEffect)m_kMeshPoints.GetChild(k).GetEffect(0)).SetLight(kLightType, afType);
                        }
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
                    if ( (m_kSpherePoints != null) && (m_kCylinderPoints != null) && (m_kMeshPoints != null) )
                    {
                        for ( int k = 0; k < m_kSpherePoints.GetQuantity(); k++ )
                        {
                            ((MipavLightingEffect)m_kSpherePoints.GetChild(k).GetEffect(0)).SetLight(kLightType, afType);
                            ((MipavLightingEffect)m_kCylinderPoints.GetChild(k).GetEffect(0)).SetLight(kLightType, afType);
                            ((MipavLightingEffect)m_kMeshPoints.GetChild(k).GetEffect(0)).SetLight(kLightType, afType);
                        }
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

        if ( m_kSphereLines == null )
        {
            m_kSphereLines = new Node();
        }
        if ( m_kCylinderLines == null )
        {
            m_kCylinderLines = new Node();
        }
        if ( m_kMeshLines == null )
        {
            m_kMeshLines = new Node();
        }
        
        m_kSphereLines.DetachAllChildren();
        m_kCylinderLines.DetachAllChildren();
        m_kMeshLines.DetachAllChildren();
        
        if ( m_kPolylineShader == null )
        {
            m_kPolylineShader = new VertexColor3Effect();
        }
        
        /* bias for latitude/longitude to handle z-buffer fighting */
        float fMBias = 0.01f;
        float fSBias = 0.01f;
        float fPBias = 0.01f;

        /* compute draw latitude lines */
        for (int i = -(iNumLat / 2); i <= (iNumLat / 2); i++) {
            float fZNormal = i / (float) (1 + (iNumLat / 2));

            MjCorticalMesh_WM.Polylines kPolylines = m_kCortical.getLatitude(fZNormal, fMBias, fSBias, fPBias);
            Polyline kMeshLat = new Polyline( kPolylines.kMVertex, false, true );  
            kMeshLat.AttachEffect( m_kPolylineShader );
            m_kMeshLines.AttachChild( kMeshLat );
            
            Polyline kSphereLat = new Polyline( kPolylines.kSVertex, false, true );  
            kSphereLat.AttachEffect( m_kPolylineShader );
            m_kSphereLines.AttachChild( kSphereLat );
            
            Polyline kPlaneLat = new Polyline( kPolylines.kPVertex, false, true );  
            kPlaneLat.AttachEffect( m_kPolylineShader );
            m_kCylinderLines.AttachChild( kPlaneLat );
        }
        /* compute longitude lines */
        for (int i = 0; i <= (iNumLon - 1); i++) {
            float fAngle = i * 2.0f * (float) Math.PI / (iNumLon - 1);

            MjCorticalMesh_WM.Polylines kPolylines = m_kCortical.getLongitude(fAngle, fMBias, fSBias, fPBias);
            Polyline kMeshLon = new Polyline( kPolylines.kMVertex, false, true );  
            kMeshLon.AttachEffect( m_kPolylineShader );
            m_kMeshLines.AttachChild( kMeshLon );
            
            Polyline kSphereLon = new Polyline( kPolylines.kSVertex, false, true );  
            kSphereLon.AttachEffect( m_kPolylineShader );
            m_kSphereLines.AttachChild( kSphereLon );
            
            Polyline kPlaneLon = new Polyline( kPolylines.kPVertex, false, true );  
            kPlaneLon.AttachEffect( m_kPolylineShader );
            m_kCylinderLines.AttachChild( kPlaneLon );
        }
        if ( m_spkScene != null )
        {
            m_spkScene.UpdateGS();            
        }
    }

    /**
     * Toggle between displaying the latitude and longitude lines:
     */
    public void toggleLatLonLines( boolean bDisplay )
    {
        if ( m_bDisplayLines == bDisplay )
        {
            return;
        }
        m_bDisplayLines = bDisplay;
        if ( !bDisplay )
        {
            if ( m_kSphere.GetDisplay() )
            {
                m_spkScene.DetachChild( m_kSphereLines );
            }
            if ( m_kCylinder.GetDisplay() )
            {
                m_spkScene.DetachChild( m_kCylinderLines );
            }
        }
        else
        {
            if ( m_kSphere.GetDisplay() )
            {
                m_spkScene.AttachChild( m_kSphereLines );
            }
            if ( m_kCylinder.GetDisplay() )
            {
                m_spkScene.AttachChild( m_kCylinderLines );
            }
        }
        m_spkScene.UpdateGS();
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
     * DOCUMENT ME!
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
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
    private float closestPlanePointIndex( PickRecord kPickPoint )
    {     
        Vector3f kP0 = m_kCortical.getCylinder().VBuffer.GetPosition3( kPickPoint.iV0 );
        Vector3f kP1 = m_kCortical.getCylinder().VBuffer.GetPosition3( kPickPoint.iV1 );
        Vector3f kP2 = m_kCortical.getCylinder().VBuffer.GetPosition3( kPickPoint.iV2 );
        Vector3f kP0mB0 = new Vector3f();
        kP0mB0.Scale( kPickPoint.B0, kP0 );
        Vector3f kP1mB1 = new Vector3f();
        kP1mB1.Scale( kPickPoint.B1, kP1 );
        Vector3f kP2mB2 = new Vector3f();
        kP2mB2.Scale( kPickPoint.B2, kP2 );
        Vector3f kPoint = new Vector3f();
        kPoint.Add( kP0mB0, kP1mB1 );
        kPoint.Add( kP2mB2 );
        
        float fDistance = Float.MAX_VALUE;
        float fMinDistance = Float.MAX_VALUE;

        Vector3f kDiff = new Vector3f();
        kDiff.Sub( kPoint, kP0 );
        fDistance = kDiff.SquaredLength();
        if (fDistance < fMinDistance) {
            fMinDistance = fDistance;
        }

        kDiff.Sub( kPoint, kP1 );
        fDistance = kDiff.SquaredLength();
        if (fDistance < fMinDistance) {
            fMinDistance = fDistance;
        }
        kDiff.Sub( kPoint, kP2 );
        fDistance = kDiff.SquaredLength();
        if (fDistance < fMinDistance) {
            fMinDistance = fDistance;
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
    private void drawPoint( int iIndex )
    {
        if ( m_kSpherePoints == null )
        {
            m_kSpherePoints = new Node();
            if ( m_kSphere.GetDisplay() )
            {
               m_spkScene.AttachChild( m_kSpherePoints );
            }
        }
        if ( m_kCylinderPoints == null )
        {
            m_kCylinderPoints = new Node();
            if ( m_kCylinder.GetDisplay() )
            {
                m_spkScene.AttachChild( m_kCylinderPoints );            
            }
        }
        if ( m_kMeshPoints == null )
        {
            m_kMeshPoints = new Node();
            m_kParent.addNode( m_kMeshPoints );
        }
        if (m_iNumPicked >= MAX_POINTS) {
            m_kSpherePoints.DetachChildAt(0);
            m_kCylinderPoints.DetachChildAt(0);
            m_kMeshPoints.DetachChildAt(0);
            m_iNumPicked--;
        }
        Vector3f kSphereTranslate = new Vector3f();
        m_kCortical.getSphere().VBuffer.GetPosition3( iIndex, kSphereTranslate );

        Vector3f kCylinderTranslate = new Vector3f();
        m_kCortical.getCylinder().VBuffer.GetPosition3( iIndex, kCylinderTranslate );
        
        Vector3f kMeshTranslate = new Vector3f();
        m_kCortical.getMesh().VBuffer.GetPosition3( iIndex, kMeshTranslate );
        
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetNChannels(3);
        StandardMesh kSM = new StandardMesh(kAttr);
        TriMesh kSphere1 = kSM.Sphere(64,64,.05f);
        kSphere1.Local.SetTranslate( kSphereTranslate );


        MaterialState kMaterial = new MaterialState();
        kMaterial.Emissive.Copy(ColorRGB.BLACK);
        kMaterial.Ambient.Set(.2f, .2f, .2f );
        kMaterial.Diffuse.Copy(m_akPickColors[m_iRunningNumPicked % MAX_POINTS]);
        kMaterial.Specular.Copy(ColorRGB.WHITE);
        kMaterial.Shininess = 83.2f;
        kSphere1.AttachGlobalState(kMaterial);
        kSphere1.AttachEffect(new MipavLightingEffect(true));
        m_kSpherePoints.AttachChild(kSphere1);
        

        TriMesh kSphere2 = kSM.Sphere(64,64,.05f);
        kSphere2.Local.SetTranslate( kCylinderTranslate );
        kSphere2.AttachGlobalState(kMaterial);
        kSphere2.AttachEffect(new MipavLightingEffect(true));
        m_kCylinderPoints.AttachChild(kSphere2);        

        TriMesh kSphere3 = kSM.Sphere(64,64,.015f);
        kSphere3.Local.SetTranslate( kMeshTranslate );
        kSphere3.AttachGlobalState(kMaterial);
        kSphere3.AttachEffect(new MipavLightingEffect(true));
        m_kMeshPoints.AttachChild(kSphere3);
                
        m_spkScene.UpdateGS();
        m_kMeshPoints.UpdateGS();
        m_iNumPicked++;
        m_iRunningNumPicked++;
        
        m_pkRenderer.LoadResources(kSphere1);
        m_pkRenderer.LoadResources(kSphere2);
        m_pkRenderer.LoadResources(kSphere3);
        updateLighting( m_akLights );

        m_kSpherePoints.UpdateRS();
        m_kCylinderPoints.UpdateRS();
        m_kMeshPoints.UpdateRS();
    }

    /**
     * Draw the user-selected triangle as a black triangle, save the original color so it can be restored:
     *
     * @param  kStart   DOCUMENT ME!
     * @param  aiIndex  DOCUMENT ME!
     * @param  iWhich   DOCUMENT ME!
     */
    private void drawTriangle(int iV0, int iV1, int iV2)
    {
        // Restore previously picked triangle colors:
        restoreTriColor();

        // Set the picked triangle to black, saving it's color:
        m_akTriColors = new ColorRGBA[3];
        m_aiTriIndex = new int[]{ iV0, iV1, iV2 };

        for (int i = 0; i < 3; i++) {
            m_akTriColors[i] = new ColorRGBA();

            m_kCortical.getMesh().VBuffer.GetColor4( 0, m_aiTriIndex[i], m_akTriColors[i] );
            m_kCortical.getSphere().VBuffer.GetColor4( 0, m_aiTriIndex[i], m_akTriColors[i] );
            m_kCortical.getCylinder().VBuffer.GetColor4( 0, m_aiTriIndex[i], m_akTriColors[i] );

            m_kCortical.getMesh().VBuffer.SetColor4( 0, m_aiTriIndex[i], 0f, 0f, 0f, 0f );
            m_kCortical.getSphere().VBuffer.SetColor4( 0, m_aiTriIndex[i], 0f, 0f, 0f, 0f );
            m_kCortical.getCylinder().VBuffer.SetColor4( 0, m_aiTriIndex[i], 0f, 0f, 0f, 0f );
        }


        m_kCortical.getMesh().VBuffer.Release();
        m_kCortical.getSphere().VBuffer.Release();
        m_kCortical.getCylinder().VBuffer.Release();
    }

    /**
     * Resets the previously picked triangle to it's orginal color:
     */
    private void restoreTriColor() {

        if ((m_akTriColors != null) && (m_aiTriIndex != null)) {

            for (int i = 0; i < m_aiTriIndex.length; i++) {
                m_kCortical.getMesh().VBuffer.SetColor4( 0, m_aiTriIndex[i], m_akTriColors[i] );
                m_kCortical.getSphere().VBuffer.SetColor4( 0, m_aiTriIndex[i], m_akTriColors[i] );
                m_kCortical.getCylinder().VBuffer.SetColor4( 0, m_aiTriIndex[i], m_akTriColors[i] );
            }

            m_akTriColors = null;
            m_aiTriIndex = null;

            m_kCortical.getMesh().VBuffer.Release();
            m_kCortical.getSphere().VBuffer.Release();
            m_kCortical.getCylinder().VBuffer.Release();
        }
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
            afXLUTa[iLIndex] = kLUT.getTransferFunction().getPoint(iLIndex).X;
            afYLUTa[iLIndex] = 255 - kLUT.getTransferFunction().getPoint(iLIndex).Y;
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
                    //System.err.println( iLut + " " + fSlope + " " + iPixColor );
                    break;
                }
            }

            int iValue = aiLutBufferRemapped[iPixColor];
            float fRed = (iValue & 0x00ff0000) >> 16;
            float fGreen = (iValue & 0x0000ff00) >> 8;
            float fBlue = (iValue & 0x000000ff);
            fRed /= 255.0f;
            fGreen /= 255.0f;
            fBlue /= 255.0f;
            
            m_kCortical.setColor( i, fRed, fGreen, fBlue );
            m_kSphere.GetMesh().VBuffer.SetColor3( 0, i, fRed, fGreen, fBlue );
            m_kCylinder.GetMesh().VBuffer.SetColor3( 0, i, fRed, fGreen, fBlue );
        }
        m_kCortical.updateMesh(false);
        m_kSphere.GetMesh().VBuffer.Release();
        m_kCylinder.GetMesh().VBuffer.Release();
    }
}
