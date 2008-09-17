package gov.nih.mipav.view.renderer.WildMagic.flythroughview;


import gov.nih.mipav.MipavCoordinateSystems;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.renderer.flythroughview.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.flythruview.JPanelVirtualEndoscopySetup;
import gov.nih.mipav.view.renderer.WildMagic.*;
import gov.nih.mipav.view.renderer.WildMagic.Render.*;
import gov.nih.mipav.view.renderer.flythroughview.FlyPathGraphCurve;
import gov.nih.mipav.view.renderer.flythroughview.ModelImage3DLayout;
import gov.nih.mipav.view.renderer.flythroughview.Skeleton3D;

import java.awt.event.*;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;
import com.sun.opengl.util.Animator;

import WildMagic.LibFoundation.Curves.Curve3f;
import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Collision.PickRecord;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.Rendering.GlobalState;
import WildMagic.LibGraphics.Rendering.Light;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.SceneGraph.*;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;


public class FlyThroughRender extends GPURenderBase implements FlyPathBehavior_WM.Callback, GLEventListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6130870790192175575L;

    ModelImage3DLayout m_kVolumeLayout = null;
    Skeleton3D m_kSkeleton = null;
    FlyPathGraphSamples m_kFlyPathGraphSamples = null;
    FlyPathGraphCurve m_kFlyPathGraphCurve = null;
    FlyPathBehavior_WM m_kFlyPathBehavior = null; 
    FlyPathAnnotateList_WM m_kAnnotateList = null; 
    ModelImage m_kMaskImage = null;
    /** DOCUMENT ME! */
    private int[] m_aiBranchIndexUnvisitedMax = null;
    private ColorRGB m_kNormalColorPathVisited = new ColorRGB(0.0f, 1.0f, 0.0f);
    private ColorRGB m_kNormalColorPathUnvisited = new ColorRGB(0.0f, 0.25f, 0.0f);

    /** This is the range of path samples for each branch that are unvisited. */
    private int[] m_aiBranchIndexUnvisitedMin = null;
    //private VolumeSurface m_kSurface = null;
    private TriMesh m_kSurface = null;
    private Light m_kLight = null;
    private SetupOptions m_kOptions = new SetupOptions();
    /** This is the control frame which may need to be updated as the view changes. */
    private JPanelVirtualEndoscopySetup_WM m_kControlFrame = null;
    private Node m_kRotation = new Node();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    public FlyThroughRender( VolumeTriPlanarInterface kParent, Animator kAnimator, VolumeImage kVolumeImageA, ModelImage kImageA, ModelLUT kLUTa, ModelRGB kRGBTa,
            VolumeImage kVolumeImageB, ModelImage kImageB, ModelLUT kLUTb, ModelRGB kRGBTb  )
    {
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

        if ( m_bSurfaceAdded )
        {
            m_bSurfaceAdded = false;
            //((SurfaceLightingEffect)m_kSurface.GetEffect(0)).SetPerPixelLighting(m_pkRenderer, true);
            updateLighting( m_akLights );
        }
    }

    protected void Move()
    {
        if (MoveCamera())
        {
            m_kCuller.ComputeVisibleSet(m_spkScene);
        }        
        if (MoveObject())
        {
            m_spkScene.UpdateGS();
            m_kCuller.ComputeVisibleSet(m_spkScene);
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
                            PickRecord kRecord = m_kPicker.GetClosestNonnegative();
                            //m_kParent.PickCorrespondence( kRecord.iV0, kRecord.iV1, kRecord.iV2 );
                        }
                    }
                }
            }
        }
    }
    
    private void Render()
    {
        m_kCuller.ComputeVisibleSet(m_spkScene);
        m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
    }

    /**
     * Called from JPanelLight. Updates the lighting parameters.
     * @param akGLights, the set of GeneralLight objects.
     */
    public void updateLighting(Light[] akGLights )
    {
        if ( m_bInit && (m_kLight == null) )
        {
            m_kLight = new Light(Light.LightType.LT_POINT);
            m_kLight.Diffuse.Set(1f, 1f, 1f);
            m_pkRenderer.SetLight( 0, m_kLight );
            String kLightType = new String("Light0Type");
            float[] afType = new float[]{2,0,0,0};
            ((SurfaceLightingEffect)m_kSurface.GetEffect(0)).SetLight(kLightType, afType);
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

        CreateScene( arg0 );

        // initial update of objects
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();

        // initial culling of scene
        m_kCuller.SetCamera(m_spkCamera);
        //m_kCuller.ComputeVisibleSet(m_spkScene);

        InitializeCameraMotion(.05f,0.001f);
        InitializeObjectMotion(m_kRotation);

        //((OpenGLRenderer)m_pkRenderer).ClearDrawable( );

        m_kAnimator.add( GetCanvas() );      
        m_bInit = true;
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
        //m_spkCull.FrontFace = FrontMode.FT_CW; 
        m_spkScene.AttachGlobalState(m_spkCull);
        //m_spkScene.AttachChild(m_kSurface.GetMesh());
        m_spkScene.AttachChild(m_kSurface);

        m_kTranslate = new Vector3f(Vector3f.ZERO);      
        for ( int i = 0; i < m_pkRenderer.GetMaxLights(); i++ )
        {
            m_pkRenderer.SetLight( i, new Light() );
        }
        setupRender(m_kImageA, m_kOptions);
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
        if (e.isControlDown()) {

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

    public void mouseDragged(MouseEvent e)
    {
        super.mouseDragged(e);
        Matrix3f kRotate = m_kRotation.Local.GetRotate();
        //kRotate.Inverse();
        Vector3f kUp = new Vector3f();
        Vector3f kDir = new Vector3f();
        Vector3f kRight = new Vector3f();
        

        Vector3f kCUp = m_kFlyPathBehavior.getViewUp();
        kCUp.Normalize();
        Vector3f kCDir = m_kFlyPathBehavior.getViewDirection();
        kCDir.Normalize();
        Vector3f kCRight = new Vector3f();
        kCRight.UnitCross( kCDir, kCUp );
        
        kRotate.Mult( kCUp, kUp );
        kRotate.Mult( kCDir, kDir );
        kRotate.Mult( kCRight, kRight );
        m_spkCamera.SetFrame( m_spkCamera.GetLocation(), kDir, kUp, kRight);
        if ( m_kControlFrame != null )
        {
            m_kControlFrame.updateOrientation(kRotate);
        }
    }

    public void addSurface(TriMesh kSurfaces)
    {
        /*
        m_kSurface =
            new VolumeSurface( m_pkRenderer, m_kVolumeImageA,
                    m_kTranslate,
                    m_fX, m_fY, m_fZ,
                    new TriMesh(kSurfaces), false );
        m_kSurface.SetDisplay(true);
         */

        m_kSurface = new TriMesh(kSurfaces);
        StandardMesh.ReverseTriangleOrder(m_kSurface.GetTriangleQuantity(), m_kSurface.IBuffer.GetData());
        boolean bHasMaterial = true;
        MaterialState kMaterial = (MaterialState)m_kSurface.GetGlobalState( GlobalState.StateType.MATERIAL );
        if ( kMaterial == null )
        {
            bHasMaterial = false;
            kMaterial = new MaterialState();
            kMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
            kMaterial.Ambient = new ColorRGB(0.2f,0.2f,0.2f);
            kMaterial.Diffuse = new ColorRGB(ColorRGB.WHITE);
            kMaterial.Specular = new ColorRGB(ColorRGB.WHITE);
            kMaterial.Shininess = 32f;
        }
        SurfaceLightingEffect kLightShader = new SurfaceLightingEffect( m_kVolumeImageA );

        if ( !bHasMaterial )
        {
            m_kSurface.AttachGlobalState(kMaterial);
        }
        m_kSurface.AttachEffect(kLightShader);
        m_kSurface.UpdateRS();
        m_kSurface.UpdateMS();
        m_bSurfaceAdded = true;
    }

    /**
     * Setup flythru renderer.
     *
     * @param  _kImage   original model image reference.
     * @param  kOptions  setup options reference.
     */
    public void setupRender(ModelImage kImage, SetupOptions kOptions) {
        m_kMaskImage = kImage;
        m_kOptions = kOptions;

        if ( !m_bInit ) 
        {
            return;
        }

        // Setup layout of 3D image for mapping sample coordinates
        // to real coordinates.
        int[] aiExtents = m_kMaskImage.getExtents();
        float[] afResolutions = m_kMaskImage.getFileInfo(0).getResolutions();
        float[] afOrigins = m_kMaskImage.getFileInfo(0).getOrigin();
        int[] aiDirections = m_kMaskImage.getFileInfo(0).getAxisDirection();

        m_kVolumeLayout = new ModelImage3DLayout(aiExtents[0], aiExtents[1], aiExtents[2],
                aiDirections[0] * (afResolutions[0]),
                aiDirections[1] * (afResolutions[1]),
                aiDirections[2] * (afResolutions[2]), afOrigins[0], afOrigins[1],
                afOrigins[2]);

        // Perform the skeletonization of the input image.
        // Extract the centerline curve.
        m_kSkeleton = new Skeleton3D(m_kMaskImage, m_kVolumeLayout);

        m_kFlyPathGraphSamples = m_kSkeleton.getPathGraph(m_kOptions.m_iMaxBranches, m_kOptions.m_fMinBranchLength);

        m_kFlyPathGraphCurve = new FlyPathGraphCurve(m_kFlyPathGraphSamples, 0.07f, 2);

        // Default geometry/appearance properties.
        // Set the radius based on the maximum distance of "inside"
        // volume samples to the surface.
        float fRadius = 0.003f * m_kSkeleton.getMaxBoundaryDistance();

        // Create the node to render the branch paths and their connections.
        int iNumBranches = m_kFlyPathGraphCurve.getNumBranches();
        m_aiBranchIndexUnvisitedMin = new int[iNumBranches];
        m_aiBranchIndexUnvisitedMax = new int[iNumBranches];
        for (int iBranch = 0; iBranch < iNumBranches; iBranch++) {
            Polyline kGeometryBranchPath = createBranchPathGeometryScaled(iBranch);
            kGeometryBranchPath.AttachEffect( new VertexColor3Effect() );
            m_spkScene.AttachChild(kGeometryBranchPath);

            m_aiBranchIndexUnvisitedMin[iBranch] = 0;
            m_aiBranchIndexUnvisitedMax[iBranch] = kGeometryBranchPath.VBuffer.GetVertexQuantity() - 1;

            m_kParent.addGeometry(new Polyline(kGeometryBranchPath.VBuffer, false, true));
        }

        // Create the node to render the annotation points as they are added.
        // Create default shape to render and create list to store all
        // of the annotations.
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetNChannels(3);
        kAttr.SetCChannels(0, 3);
        StandardMesh kSM = new StandardMesh(kAttr);
        m_kAnnotateList = new FlyPathAnnotateList_WM();
        m_kAnnotateList.setDefaultShape(kSM.Sphere(10, 10, fRadius));

        TriMesh kSpherePosition = kSM.Sphere(10, 10, fRadius);
        for ( int i = 0; i < kSpherePosition.VBuffer.GetVertexQuantity(); i++ )
        {
            kSpherePosition.VBuffer.SetColor3(0, i, 1, 0, 0 );
        }
        kSpherePosition.SetName( "FlyThrough" );
        m_kParent.addGeometry( kSpherePosition );

        // Setup behavior to handle flying down the path and
        // looking around.
        m_kFlyPathBehavior = new FlyPathBehavior_WM(m_kFlyPathGraphCurve, m_kAnnotateList, this);
        m_kFlyPathBehavior.setupCallback(this);
    }


    /**
     * DOCUMENT ME!
     *
     * @param  _control  DOCUMENT ME!
     */
    public void setupRenderControl(JPanelVirtualEndoscopySetup_WM _control) {
        m_kControlFrame = _control;
    }

    /**
     * Get the geometry to be used for rendering the path of the specified branch.
     *
     * @param   iBranch  Index which identifies the branch.
     *
     * @return  LineStripArray instance that can be attached to a Shape3D node for rendering.
     *
     *          <p>The LineStripArray coordinates are scaled to match the ModelTriangleMesh in JPanelSurface.</p>
     */
    private Polyline createBranchPathGeometryScaled(int iBranch) {
        int iNumVertex = 500;
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);
        VertexBuffer kVBuffer = new VertexBuffer(kAttributes, iNumVertex);


        Curve3f kCurve = m_kFlyPathGraphCurve.getCurvePosition(iBranch);
        float fStep = kCurve.GetTotalLength() / (iNumVertex - 1);

        int[] aiExtents = m_kMaskImage.getExtents();
        float[] afResolutions = m_kMaskImage.getFileInfo(0).getResolutions();
        float[] afOrigins = m_kMaskImage.getFileInfo(0).getOrigin();
        int[] aiModelDirection = MipavCoordinateSystems.getModelDirections(m_kMaskImage);
        float[] aiDirections = new float[] { (int)aiModelDirection[0], (int)aiModelDirection[1], (int)aiModelDirection[2]}; 
        //int[] aiDirections = m_kMaskImage.getFileInfo(0).getAxisDirection();

        int xDim = aiExtents[0];
        int yDim = aiExtents[1];
        int zDim = aiExtents[2];

        float xBox = (xDim - 1) * afResolutions[0];
        float yBox = (yDim - 1) * afResolutions[1];
        float zBox = (zDim - 1) * afResolutions[2];
        float maxBox = Math.max(xBox, Math.max(yBox, zBox));

        for (int iPoint = 0; iPoint < iNumVertex; ++iPoint) {
            float fDist = iPoint * fStep;
            float fTime = kCurve.GetTime(fDist, 100, 1e-02f);      
            Vector3f kPoint = kCurve.GetPosition(fTime);

            kPoint.X = ((2.0f * (kPoint.X - afOrigins[0]) / aiDirections[0]) -
                    xBox) / (2.0f*maxBox);
            kPoint.Y = ((2.0f * (kPoint.Y - afOrigins[1]) / aiDirections[1]) -
                    yBox) / (2.0f*maxBox);
            kPoint.Z = ((2.0f * (kPoint.Z - afOrigins[2]) / aiDirections[2]) -
                    zBox) / (2.0f*maxBox);
            kVBuffer.SetPosition3(iPoint, kPoint);
            kVBuffer.SetColor3(0, iPoint, m_kNormalColorPathUnvisited);
        }

        return new Polyline( kVBuffer, false, true );
    }


    /**
     * Scaled coordinates for the current position along the path for viewing.
     *
     * @return  Point3f A new instance created which contains the path position coordinates, scaled to match the
     *          ModelTriangleMesh in JPanelSurface.
     */
    public Vector3f getPositionScaled( Vector3f kPoint) {

        int[] aiExtents = m_kMaskImage.getExtents();
        float[] afResolutions = m_kMaskImage.getFileInfo(0).getResolutions();
        float[] afOrigins = m_kMaskImage.getFileInfo(0).getOrigin();
        int[] aiDirections = m_kMaskImage.getFileInfo(0).getAxisDirection();

        int xDim = aiExtents[0];
        int yDim = aiExtents[1];
        int zDim = aiExtents[2];

        float xBox = (xDim - 1) * afResolutions[0];
        float yBox = (yDim - 1) * afResolutions[1];
        float zBox = (zDim - 1) * afResolutions[2];
        float maxBox = Math.max(xBox, Math.max(yBox, zBox));
        Vector3f kPointScaled = new Vector3f();
        kPointScaled.X = ((2.0f * (kPoint.X - afOrigins[0]) / aiDirections[0]) -
                xBox) / (2.0f*maxBox);
        kPointScaled.Y = ((2.0f * (kPoint.Y - afOrigins[1]) / aiDirections[1]) -
                yBox) / (2.0f*maxBox);
        kPointScaled.Z = ((2.0f * (kPoint.Z - afOrigins[2]) / aiDirections[2]) -
                zBox) / (2.0f*maxBox);
        return kPointScaled;
    }
    /**
     * Called any time the position along the current curve changes and the color of the curve needs to change to show
     * what has been visited.
     */
    protected void resetRenderBranchPath() {

        // Access the GeometryStripArray-derived instance for the rendering
        // of the specified branch path.
        int iBranch = m_kFlyPathBehavior.getBranchIndex();
        Polyline kLine = (Polyline)m_spkScene.GetChild(iBranch+1);

        // How many points are in the array?
        int iNumPoints = kLine.VBuffer.GetVertexQuantity();

        // What is the range of samples to be colored as unvisited.
        int iUnvisitedMin = (int) (m_kFlyPathBehavior.getBranchDistUnvisitedMin() * (iNumPoints - 1));
        int iUnvisitedMax = (int) (m_kFlyPathBehavior.getBranchDistUnvisitedMax() * (iNumPoints - 1));

        // Initially, all paths were marked as being unvisited, and since
        // you cannot "unvisit" a point along a path, we just need to go
        // through and mark each node as visited or not.  Update any newly
        // visited points.
        for (int iIndex = m_aiBranchIndexUnvisitedMin[iBranch]; iIndex < iUnvisitedMin; iIndex++) {
            kLine.VBuffer.SetColor3(0, iIndex, m_kNormalColorPathVisited);
        }

        for (int iIndex = m_aiBranchIndexUnvisitedMax[iBranch]; iIndex > iUnvisitedMax; iIndex--) {
            kLine.VBuffer.SetColor3(0, iIndex, m_kNormalColorPathVisited);
        }

        // Save the new range of unvisited range of the path.
        m_aiBranchIndexUnvisitedMin[iBranch] = iUnvisitedMin;
        m_aiBranchIndexUnvisitedMax[iBranch] = iUnvisitedMax;

        kLine.VBuffer.Release();
    }



    public static class SetupOptions {

        /**
         * Percentage of path samples points to use for the number of control points for a BSpline curve fit. A larger
         * number may create a curve which oscilates more. A smaller number may create a curve which is too smooth to
         * even be close to the original path samples.
         */
        public float m_fFractionNumControlPoints = 0.07f;

        /** Minimum length of branch to extract. */
        public float m_fMinBranchLength = 100.0f;

        /** Maximum number of branches to extract. */
        public int m_iMaxBranches = 1;

        /**
         * The number of samples which define each branch path is reduced by this factor for the purpose of speeding up
         * the segmentation of the surface.
         */
        public int m_iSegmentSurfaceBranchSamplesReductionFactor = 2;
    }




    /**
     * Part of the KeyListener interface. Pressing 'b' toggles displaying the
     * proxy-geometry versus the ray-traced volume.
     * @param e, the key event.
     */
    public void keyPressed(KeyEvent e) {
        int iKeyCode = e.getKeyCode();
        if ( (iKeyCode != KeyEvent.VK_UP) && (iKeyCode != KeyEvent.VK_DOWN) )
        {
            super.keyPressed(e);
        }
    }


    public void viewChanged(FlyPathBehavior_WM behavior, int iEvent)
    {
        if ( m_kFlyPathBehavior != behavior )
        {
            return;
        }
        if (FlyPathBehavior_WM.EVENT_RESET_ORIENTATION == 
            (FlyPathBehavior_WM.EVENT_RESET_ORIENTATION & iEvent))
        {       
            m_kRotation.Local.SetRotateCopy(Matrix3f.IDENTITY);
            if ( m_kControlFrame != null )
            {
                m_kControlFrame.updateOrientation(m_kRotation.Local.GetRotate());
            }
        }  

        if ( m_kControlFrame != null )
        {
            m_kControlFrame.updatePosition(behavior);
        }
        Vector3f kCDir = behavior.getViewDirection();
        kCDir.Normalize();
        Vector3f kCUp = behavior.getViewUp();
        kCUp.Normalize();
        Vector3f kCLoc = behavior.getViewPoint();
        Vector3f kCRight = new Vector3f();
        kCRight.UnitCross( kCDir, kCUp );
        Vector3f kPositionScaled = getPositionScaled(kCLoc);
        m_spkCamera.SetFrame(kPositionScaled,kCDir,kCUp,kCRight);

        if ( m_kLight != null )
        {
            m_kLight.Position.Copy(kPositionScaled);
            m_kLight.DVector.Copy(kCDir);
        }
        Vector3f kVolumePt = m_kVolumeLayout.getSamplePoint(kCLoc.X, kCLoc.Y, kCLoc.Z);
        kVolumePt.X *= m_kMaskImage.getExtents()[0];
        kVolumePt.Y *= m_kMaskImage.getExtents()[1];
        kVolumePt.Z *= m_kMaskImage.getExtents()[2];
        m_kParent.setSliceFromPlane(kVolumePt);
        m_kParent.translateSurface( "FlyThrough", kPositionScaled );

        resetRenderBranchPath();


    }
    
    public Vector3f getCameraDirection()
    {
        return new Vector3f( m_spkCamera.GetDVector() );
    }


    public Matrix4f getWVMatrix()
    {
        //m_pkRenderer.SetGeometry(m_kSurface.GetMesh());
        m_pkRenderer.SetGeometry(m_kSurface);
        float[] afData = new float[16];
        m_pkRenderer.SetConstantWVMatrix (0, afData);
        m_pkRenderer.SetGeometry(null);
        return new Matrix4f(afData, true);
    }

}
