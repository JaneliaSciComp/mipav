package gov.nih.mipav.view.renderer.WildMagic.flythroughview;


import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.util.MipavInitGPU;
import gov.nih.mipav.view.renderer.WildMagic.GPURenderBase;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;
import gov.nih.mipav.view.renderer.WildMagic.Render.SurfaceLightingEffect;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.brainflattenerview_WM.MjCorticalMesh_WM;
import gov.nih.mipav.view.renderer.flythroughview.FlyPathGraphCurve;
import gov.nih.mipav.view.renderer.flythroughview.FlyPathGraphSamples;
import gov.nih.mipav.view.renderer.flythroughview.FlyThroughRenderInterface;
import gov.nih.mipav.view.renderer.flythroughview.ModelImage3DLayout;
import gov.nih.mipav.view.renderer.flythroughview.Skeleton3D;

import java.awt.Canvas;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;

import WildMagic.LibFoundation.Curves.Curve3f;
import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Collision.PickRecord;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.Rendering.GlobalState;
import WildMagic.LibGraphics.Rendering.Light;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.jogamp.opengl.util.Animator;

public class FlyThroughRender extends GPURenderBase implements FlyThroughRenderInterface, FlyPathBehavior_WM.Callback, GLEventListener {

    public static class SetupOptions {

        /**
         * Percentage of path samples points to use for the number of control points for a BSpline curve fit. A larger
         * number may create a curve which oscillates more. A smaller number may create a curve which is too smooth to
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

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6130870790192175575L;

	/** Parent user-interface and display frame. */
    protected VolumeTriPlanarInterface m_kParent = null;
    
    /** 3D Layout of the ModelImage. */
    private ModelImage3DLayout m_kVolumeLayout = null;
    /** 3D Skeleton of the ModelImage. */
    private Skeleton3D m_kSkeleton = null;
    /** Fly path curve. */
    private FlyPathGraphCurve m_kFlyPathGraphCurve = null; 
    /** Fly path behavior. */
    private FlyPathBehavior_WM m_kFlyPathBehavior = null;
    /** Annotations along the fly path. */
    private FlyPathAnnotateList_WM m_kAnnotateList = null; 
    /** ModelImage used to generate the fly path. */
    private ModelImage m_kMaskImage = null;
    /** Keep track of the last branch that was selected (at a branch point). */
    private int m_iLastSelectedBranchIndex = -1;
    /** This is the range of path samples for each branch that are unvisited. */
    private int[] m_aiBranchIndexUnvisitedMax = null;
    /** Color for the parts of the path that have been visited (Light Green) */
    private ColorRGB m_kNormalColorPathVisited = new ColorRGB(0.0f, 1.0f, 0.0f);
    /** Color for the parts of the path that have not yet been visited (Dark Green) */
    private ColorRGB m_kNormalColorPathUnvisited = new ColorRGB(0.0f, 0.25f, 0.0f);
    /** Color for selecting previously-visited parts of the path at a branch (Red) */
    private ColorRGB m_kSelectColorPathVisited = new ColorRGB(1.0f, 0.0f, 0.0f);
    /** Color for selecting previously-unvisited parts of the path at a branch (Dark Red) */
    private ColorRGB m_kSelectColorPathUnvisited = new ColorRGB(0.25f, 0.0f, 0.0f);
    /** This is the range of path samples for each branch that are unvisited. */
    private int[] m_aiBranchIndexUnvisitedMin = null;
    /** The surface to fly through. */
    private TriMesh m_kSurface = null;
    /** Lighting in the scene. */
    private Light m_kLight = null;
    /**  Setup options. */
    private SetupOptions m_kOptions = new SetupOptions();
    /** This is the control frame which may need to be updated as the view changes. */
    private JPanelVirtualEndoscopySetup_WM m_kControlFrame = null;
    /** Mouse rotation. */
    private Node m_kRotation = new Node();
    /** Calculating the mesh curvatures. */
    private MjCorticalMesh_WM m_kCortical = null;
    /** Center of Mesh */
    private Vector3f m_kCenter = new Vector3f();
    /** kMean curvature LUT. Pseudo color look up table. */
    private ModelLUT m_kMeanCurvaturesLUT = null;
    /** Lighting effect. */
    private SurfaceLightingEffect m_kLightShader;
    /** Original surface color for undoing the pseudo-color curvature map. */
    private ColorRGB[] m_akColorBackup;



    /**
     * Construct the FlyThroughRenderer.
     * @param kParent parent user-interface and frame.
     * @param kAnimator animator used to display the canvas.
     * @param kVolumeImageA volume data and textures for ModelImage A.
     * @param kVolumeImageB volume data and textures for ModelImage B.
     */
    public FlyThroughRender( VolumeTriPlanarInterface kParent, Animator kAnimator, VolumeImage kVolumeImageA, 
                             VolumeImage kVolumeImageB, Vector3f kTranslate  )
    {
        super();
        m_kTranslate.copy(kTranslate);

        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                                           m_eBuffering, m_eMultisampling,
                                           m_iWidth, m_iHeight );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this ); 
        m_pkRenderer.SetExternalDir(MipavInitGPU.getExternalDirs());      

        m_kAnimator = kAnimator;
        m_kVolumeImageA = kVolumeImageA;
        m_kVolumeImageB = kVolumeImageB;
        m_kParent = kParent;

        m_fX = m_kVolumeImageA.GetScaleX();
        m_fY = m_kVolumeImageA.GetScaleY();
        m_fZ = m_kVolumeImageA.GetScaleZ();
    }

    /**
     * Add the fly-through surface.
     * @param kSurface the TriMesh surface to fly through.
     */
    public void addSurface(TriMesh kSurface, Vector3f kCenter)
    {
        m_kCenter.copy(kCenter);
        m_kSurface = new TriMesh(kSurface);
        m_kSurface.AttachGlobalState(kSurface.GetGlobalState( GlobalState.StateType.MATERIAL ));
        SurfaceLightingEffect kLightShader = new SurfaceLightingEffect( m_kVolumeImageA, false );
        m_kLightShader = new SurfaceLightingEffect( m_kVolumeImageA, false );
        m_kSurface.AttachEffect(kLightShader);
        m_kSurface.UpdateRS();
        m_kSurface.UpdateMS();
        m_bSurfaceUpdate = true;
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.flythroughview.FlyThroughRenderInterface#autoRun()
     */
    @Override
	public void autoRun() {
        m_kFlyPathBehavior.autoRun();        
    }

    /* (non-Javadoc)
     * @see javax.media.opengl.GLEventListener#display(javax.media.opengl.GLAutoDrawable)
     */
    @Override
	public void display(GLAutoDrawable arg0)
    {      
        if ( !m_bInit )
        {
            init(arg0);
        }
        MeasureTime();

        Move();

        // Draw the scene to the back buffer/
        if (m_pkRenderer.BeginScene())
        {
            m_pkRenderer.SetBackgroundColor(ColorRGBA.BLACK);
            m_pkRenderer.ClearBuffers();
            Render();
            m_pkRenderer.EndScene();
        }
        if ( m_bSnapshot )
        {
            writeImage();
        }
        m_pkRenderer.DisplayBackBuffer();

        UpdateFrameCount();       

        if ( m_bSurfaceUpdate )
        {
            m_bSurfaceUpdate = false;
            //((SurfaceLightingEffect)m_kSurface.GetEffect(0)).SetPerPixelLighting(m_pkRenderer, true);
            updateLighting( m_akLights );
            GetCanvas().display();
        }
    }

    /**
     * Turn curvature-based color map on/off.
     * @param bOn turns curvature-based color map on when true and off when false.
     */
    public void doPseudoColor( boolean bOn )
    {
        if ( bOn )
        {
            if ( m_kCortical == null )
            {
                m_kCortical = new MjCorticalMesh_WM(m_kSurface, m_kCenter);
                m_kCortical.computeMeanCurvature();
            }

            float[] afMeanCurvatures = m_kCortical.getMeanCurvature();
            int iNumVertices = afMeanCurvatures.length;
            double dNumVertices = iNumVertices;

            // Compute the mean and standard deviation of the mean curvatures.
            double dMeanCurvatureSum = 0.0f;
            double dMeanCurvatureSum2 = 0.0f;

            for (int iVertex = 0; iVertex < iNumVertices; ++iVertex) {
                double dMeanCurvature = afMeanCurvatures[iVertex];

                dMeanCurvatureSum += dMeanCurvature;
                dMeanCurvatureSum2 += dMeanCurvature * dMeanCurvature;
            }

            double dMeanCurvatureMean = dMeanCurvatureSum / dNumVertices;
            double dMeanCurvatureStddev = Math.sqrt((dMeanCurvatureSum2 -
                                                     (dNumVertices * dMeanCurvatureMean * dMeanCurvatureMean)) /
                                                    (dNumVertices - 1.0));

            // Make the range of curvature values to be mapped symmetrical.
            double dMeanCurvatureAbsMax = Math.max(Math.abs(dMeanCurvatureMean - (2.0 * dMeanCurvatureStddev)),
                                                   Math.abs(dMeanCurvatureMean + (2.0 * dMeanCurvatureStddev)));

            // Map the mean curvatures values to pseudocolors.
            int[] aiDimExtentsLUT = new int[] { 4, 256 };

            m_kMeanCurvaturesLUT = new ModelLUT(ModelLUT.SPECTRUM, 256, aiDimExtentsLUT);
            m_kMeanCurvaturesLUT.invertLUT();
            m_kMeanCurvaturesLUT.resetTransferLine(-(float) dMeanCurvatureAbsMax, +(float) dMeanCurvatureAbsMax);


            int iLutSize = 0;
            float[] afXLUTa = null;
            float[] afYLUTa = null;
            int iLutHeightA = 0;
            int[] aiLutBufferRemapped = null;


            iLutSize = m_kMeanCurvaturesLUT.getTransferFunction().size();
            afXLUTa = new float[iLutSize];
            afYLUTa = new float[iLutSize];

            for (int iLIndex = 0; iLIndex < iLutSize; iLIndex++) {
                afXLUTa[iLIndex] = m_kMeanCurvaturesLUT.getTransferFunction().getPoint(iLIndex).X;
                afYLUTa[iLIndex] = 255 - m_kMeanCurvaturesLUT.getTransferFunction().getPoint(iLIndex).Y;
            }

            iLutHeightA = m_kMeanCurvaturesLUT.getExtents()[1];
            aiLutBufferRemapped = new int[iLutHeightA];
            m_kMeanCurvaturesLUT.exportIndexedLUT(aiLutBufferRemapped);


            int iVQuantity = m_kSurface.VBuffer.GetVertexQuantity();
            m_akColorBackup = new ColorRGB[iVQuantity];
            for (int i = 0; i < iVQuantity; i++) {
                float fValue = afMeanCurvatures[i];
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

                m_akColorBackup[i] = new ColorRGB();
                m_kSurface.VBuffer.GetColor3(0, i, m_akColorBackup[i]);
                m_kSurface.VBuffer.SetColor3( 0, i, fRed, fGreen, fBlue );
            }
            m_kSurface.Reload(true);
        }
        else if ( m_akColorBackup != null )
        {
            int iVQuantity = m_kSurface.VBuffer.GetVertexQuantity();
            for (int i = 0; i < iVQuantity; i++) {
                m_kSurface.VBuffer.SetColor3(0, i, m_akColorBackup[i]);
            }
            m_kSurface.Reload(true);
        }
        GetCanvas().display();
    }


    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.flythroughview.FlyThroughRenderInterface#getBranchState()
     */
    @Override
	public Object getBranchState() {
        return m_kFlyPathBehavior.getBranchState();
    }

    /**
     * Return the camera direction vector.
     * @return camera direction vector.
     */
    public Vector3f getCameraDirection()
    {
        return new Vector3f( m_spkCamera.GetDVector() );
    }

    /**
     * Return the camera up vector.
     * @return camera up vector.
     */
    public Vector3f getCameraUp()
    {
        return new Vector3f( m_spkCamera.GetUVector() );
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.flythroughview.FlyThroughRenderInterface#getCanvas()
     */
    @Override
	public Canvas getCanvas() {
        return GetCanvas();
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.flythroughview.FlyThroughRenderInterface#getHeight()
     */
    @Override
	public int getHeight() {
        return m_pkRenderer.GetHeight();
    }

    /**
     * Scaled coordinates for the current position along the path for viewing.
     * @param   Point in normalized path coordinates.
     * @return  Point3f A new instance created which contains the path position coordinates, scaled to match the
     *          TriMesh in JPanelSurface.
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
     * Scaled coordinates for the current position along the path for viewing.
     * @param  The path position coordinates, scaled to match the TriMesh.
     * @return   Point in normalized path coordinates.
     */
    public Vector3f getPositionUnScaled( Vector3f kPoint) {

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
        kPointScaled.X  = (((kPoint.X * (2.0f*maxBox) + xBox) * aiDirections[0]) / 2.0f) + afOrigins[0];
        kPointScaled.Y  = (((kPoint.Y * (2.0f*maxBox) + yBox) * aiDirections[1]) / 2.0f) + afOrigins[1];
        kPointScaled.Z  = (((kPoint.Z * (2.0f*maxBox) + zBox) * aiDirections[2]) / 2.0f) + afOrigins[2];
        return kPointScaled;
    }


    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.flythroughview.FlyThroughRenderInterface#getWidth()
     */
    @Override
	public int getWidth() {
        return m_pkRenderer.GetWidth();
    }


    /* (non-Javadoc)
     * @see javax.media.opengl.GLEventListener#init(javax.media.opengl.GLAutoDrawable)
     */
    @Override
	public void init(GLAutoDrawable arg0) {      
        if ( m_pkRenderer != null )
        {
            ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        }
        m_pkRenderer.InitializeState();
        m_pkRenderer.SetLineWidth(3);

        super.OnInitialize();

        // set up camera
        m_spkCamera.SetFrustum(60.0f,m_iWidth/(float)m_iHeight,0.01f,10.0f);
        Vector3f kCDir = new Vector3f(0.0f,0.0f,1.0f);
        Vector3f kCUp = new Vector3f(0.0f, -1.0f,0.0f);
        Vector3f kCRight = Vector3f.cross( kCDir, kCUp );
        Vector3f kCLoc = Vector3f.scale(-1.4f, kCDir);
        m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);

        CreateScene();

        // initial update of objects
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();

        // initial culling of scene
        m_kCuller.SetCamera(m_spkCamera);

        InitializeCameraMotion(.05f,0.001f);
        InitializeObjectMotion(m_kRotation);

        m_kAnimator.add( GetCanvas() );      
        m_bInit = true;
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.GPURenderBase#keyPressed(java.awt.event.KeyEvent)
     */
    @Override
	public void keyPressed(KeyEvent e) {
        int iKeyCode = e.getKeyCode();
        if ( (iKeyCode != KeyEvent.VK_UP) && (iKeyCode != KeyEvent.VK_DOWN) )
        {
            super.keyPressed(e);
        }
    }


    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.flythroughview.FlyThroughRenderInterface#makeMove(java.lang.String)
     */
    @Override
	public void makeMove(String cmd) {
        m_kFlyPathBehavior.move(cmd);
    }


    /* (non-Javadoc)
     * @see WildMagic.LibApplications.OpenGLApplication.JavaApplication3D#mouseDragged(java.awt.event.MouseEvent)
     */
    @Override
	public void mouseDragged(MouseEvent e)
    {
        super.mouseDragged(e);
        updateCamera();
    }

    /* (non-Javadoc)
     * @see WildMagic.LibApplications.OpenGLApplication.JavaApplication3D#mousePressed(java.awt.event.MouseEvent)
     */
    @Override
	public void mousePressed(MouseEvent e) {
        super.mousePressed(e);

        /* Only capture mouse events when enabled, and only when the control key is down and the left mouse
         * button is pressed. */
        if (e.isShiftDown()) {

            /* m_bMousePressed and m_bMouseReleased are set explicitly to
             * prevent multiple mouse clicks at the same location.  If the mouse has been released, then set
             * mousePressed to true and save the location of the mouse event.
             */
            if ((e.getButton() == MouseEvent.BUTTON1)) {
                m_iXPick = e.getX();
                m_iYPick = e.getY();
                m_bPickPending = true;
                Pick();
                GetCanvas().display();
            }
        }

    } 


    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.flythroughview.FlyThroughRenderInterface#setCurrentState(java.lang.Object)
     */
    @Override
	public void setCurrentState(Object _state) {
        m_kFlyPathBehavior.setBranch(_state);
    }

    /**
     * Setup flythru renderer.
     *
     * @param  _kImage   original model image reference.
     * @param  kOptions  setup options reference.
     */
    public void setupRender(ModelImage kImage, SetupOptions kOptions) {
        m_kMaskImage = resizeImage( kImage );
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

        FlyPathGraphSamples kFlyPathGraphSamples = m_kSkeleton.getPathGraph(m_kOptions.m_iMaxBranches, m_kOptions.m_fMinBranchLength);

        m_kFlyPathGraphCurve = new FlyPathGraphCurve(kFlyPathGraphSamples, 0.07f, 2);

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
            Polyline kPoly = new Polyline(kGeometryBranchPath.VBuffer, false, true);
            kPoly.AttachEffect( new VertexColor3Effect() );
            Node kPolyNode = new Node();
            kPolyNode.AttachChild(kPoly);
            m_kParent.addNode( kPolyNode );

            m_aiBranchIndexUnvisitedMin[iBranch] = 0;
            m_aiBranchIndexUnvisitedMax[iBranch] = kGeometryBranchPath.VBuffer.GetVertexQuantity() - 1;

            kGeometryBranchPath = createBranchConnectGeometryScaled(iBranch);
            kGeometryBranchPath.AttachEffect( new VertexColor3Effect() );
            m_spkScene.AttachChild(kGeometryBranchPath);
            kPoly = new Polyline(kGeometryBranchPath.VBuffer, false, true);
            kPoly.AttachEffect( new VertexColor3Effect() );
            kPolyNode = new Node();
            kPolyNode.AttachChild(kPoly);
            m_kParent.addNode( kPolyNode );
        }
        m_spkScene.UpdateGS();

        m_kAnnotateList = new FlyPathAnnotateList_WM();

        // Create the node to render the annotation points as they are added.
        // Create default shape to render and create list to store all
        // of the annotations.
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetNChannels(3);
        kAttr.SetCChannels(0, 3);
        StandardMesh kSM = new StandardMesh(kAttr);

        TriMesh kSpherePosition = kSM.Sphere(10, 10, fRadius);
        for ( int i = 0; i < kSpherePosition.VBuffer.GetVertexQuantity(); i++ )
        {
            kSpherePosition.VBuffer.SetColor3(0, i, 1, 0, 0 );
        }
        kSpherePosition.AttachEffect( new VertexColor3Effect() );
        Node kSphereNode = new Node();
        kSphereNode.AttachChild(kSpherePosition);
        kSphereNode.SetName( "FlyThrough" );
        m_kParent.addNode( kSphereNode );

        // Setup behavior to handle flying down the path and
        // looking around.
        m_kFlyPathBehavior = new FlyPathBehavior_WM(m_kFlyPathGraphCurve, m_kAnnotateList, this);
        m_kFlyPathBehavior.setupCallback(this);
    }

    /**
     * Set the JPanelVirtualEndoscopySetup user-interface.
     * @param _control the JPanelVirtualEndoscopySetup user-interface.
     */
    public void setupRenderControl(JPanelVirtualEndoscopySetup_WM _control) {
        m_kControlFrame = _control;
    }

    /**
     * Update the representation of the current viewpoint in the Volume Tri-Planar renderer.
     */
    public void update()
    {                
        Vector3f kPositionScaled = m_spkCamera.GetLocation();
        m_kParent.translateSurface( "FlyThrough", kPositionScaled );
        Vector3f kVolumePt3 = m_kParent.getTranslateSurface( "FlyThrough" );
        kVolumePt3.sub( m_kTranslate );
        kVolumePt3.X *= 1.0f/m_fX;
        kVolumePt3.Y *= 1.0f/m_fY;
        kVolumePt3.Z *= 1.0f/m_fZ;
        kVolumePt3.X *= (m_kMaskImage.getExtents()[0]-1);
        kVolumePt3.Y *= (m_kMaskImage.getExtents()[1]-1);
        kVolumePt3.Z *= (m_kMaskImage.getExtents()[2]-1);
        m_kParent.setSliceFromPlane(kVolumePt3);
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.GPURenderBase#updateLighting(WildMagic.LibGraphics.Rendering.Light[])
     */
    @Override
	public void updateLighting(Light[] akGLights )
    {
        if ( m_bInit )
        {
            if ( m_kLight == null )
            {
                m_kLight = new Light(Light.LightType.LT_POINT);
                m_kLight.Ambient.Set(.1f, .1f, .1f);
                m_kLight.Diffuse.Set(1f, 1f, 1f);
                m_kLight.Specular.Set(1f, 1f, 1f);
                m_pkRenderer.SetLight( 0, m_kLight );
            }
            String kLightType = new String("Light0Type");
            ((SurfaceLightingEffect)m_kSurface.GetEffect(0)).SetPerPixelLighting(m_pkRenderer, true);
            ((SurfaceLightingEffect)m_kSurface.GetEffect(0)).SetLight(kLightType, new float[]{2,-1,-1,-1});
            ((SurfaceLightingEffect)m_kSurface.GetEffect(0)).SetReverseFace(1);
            m_kLightShader.SetLight(kLightType, new float[]{2,-1,-1,-1});
        }
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.flythroughview.FlyPathBehavior_WM.Callback#viewChanged(gov.nih.mipav.view.renderer.WildMagic.flythroughview.FlyPathBehavior_WM, int)
     */
    @Override
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
        kCDir.normalize();
        Vector3f kCUp = behavior.getViewUp();
        kCUp.normalize();
        Vector3f kCLoc = behavior.getViewPoint();
        Vector3f kCRight = Vector3f.unitCross( kCDir, kCUp );
        Vector3f kPositionScaled = getPositionScaled(kCLoc);
        m_spkCamera.SetFrame(kPositionScaled,kCDir,kCUp,kCRight);

        if ( m_kLight != null )
        {
            m_kLight.Position.copy(kPositionScaled);
            m_kLight.DVector.copy(kCDir);
        }

        resetRenderBranchPath();

        if (FlyPathBehavior_WM.EVENT_CHANGE_BRANCH == (FlyPathBehavior_WM.EVENT_CHANGE_BRANCH & iEvent)) {
            resetRenderBranchConnect();
        }
        if ( m_kControlFrame.getContinuousUpdate() )
        {
            update();
            m_kParent.displayAll();
        }
        else
        {
            GetCanvas().display();
        }

    }

    /**
     * Create a new Annotation point at the position specified.
     * @param kPosition position of new annotation point.
     */
    private void createAnnotatePoint(Vector3f kPosition)
    {
        float fRadius = 0.0003f * m_kSkeleton.getMaxBoundaryDistance();
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetNChannels(3);
        kAttr.SetCChannels(0, 3);
        StandardMesh kSM = new StandardMesh(kAttr);
        TriMesh kMesh = kSM.Sphere(10, 10, fRadius);
        kMesh.Local.SetTranslate(kPosition);
        kMesh.AttachEffect( m_kLightShader );
        MaterialState kMaterial = new MaterialState();
        kMaterial.Ambient.Set(0.1f, 0.1f, 0.1f);
        kMaterial.Diffuse.Set(0.1f, 1.0f, 0.1f);
        kMaterial.Specular.Set(1.0f, 1.0f, 1.0f);
        kMaterial.Shininess = 10.0f;
        kMesh.AttachGlobalState(kMaterial);
        kMesh.UpdateGS();
        kMesh.UpdateRS();
        kMesh.UpdateMS();
        m_spkScene.AttachChild(kMesh);
        m_spkScene.UpdateGS();
        m_bSurfaceUpdate = true;
    }

    /**
     * Get the geometry to be used for rendering the connection of the specified branch to its parent branch.
     * @param   iBranch  Index which identifies the branch.
     * @return  Polyline instance that can be attached to a scene-graph Node for rendering
     */
    private Polyline createBranchConnectGeometryScaled(int iBranch) {
        int[] aiExtents = m_kMaskImage.getExtents();
        float[] afResolutions = m_kMaskImage.getFileInfo(0).getResolutions();
        float[] afOrigins = m_kMaskImage.getFileInfo(0).getOrigin();
        int[] aiModelDirection = MipavCoordinateSystems.getModelDirections(m_kMaskImage);
        float[] aiDirections = new float[] { aiModelDirection[0], aiModelDirection[1], aiModelDirection[2]}; 
        //int[] aiDirections = m_kMaskImage.getFileInfo(0).getAxisDirection();

        int xDim = aiExtents[0];
        int yDim = aiExtents[1];
        int zDim = aiExtents[2];

        float xBox = (xDim - 1) * afResolutions[0];
        float yBox = (yDim - 1) * afResolutions[1];
        float zBox = (zDim - 1) * afResolutions[2];
        float maxBox = Math.max(xBox, Math.max(yBox, zBox));

        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);
        VertexBuffer kVBuffer = new VertexBuffer(kAttributes, 2);

        // Get the index of the parent.
        int iBranchParent = m_kFlyPathGraphCurve.getBranchParentIndex(iBranch);

        // Set the point for the start of the child branch.
        Curve3f kCurveChild = m_kFlyPathGraphCurve.getCurvePosition(iBranch);
        float fTimeChild = kCurveChild.GetTime(0.0f, 100, 1e-02f);

        Vector3f kPoint = kCurveChild.GetPosition(fTimeChild);
        kPoint.X = ((2.0f * (kPoint.X - afOrigins[0]) / aiDirections[0]) -
                    xBox) / (2.0f*maxBox);
        kPoint.Y = ((2.0f * (kPoint.Y - afOrigins[1]) / aiDirections[1]) -
                    yBox) / (2.0f*maxBox);
        kPoint.Z = ((2.0f * (kPoint.Z - afOrigins[2]) / aiDirections[2]) -
                    zBox) / (2.0f*maxBox);
        kVBuffer.SetPosition3(0, kPoint);
        kVBuffer.SetColor3(0, 0, m_kNormalColorPathUnvisited);

        // If there is no parent, then replicate the child start point
        if (iBranchParent < 0) {
            kPoint = kCurveChild.GetPosition(fTimeChild);
            kPoint.X = ((2.0f * (kPoint.X - afOrigins[0]) / aiDirections[0]) -
                        xBox) / (2.0f*maxBox);
            kPoint.Y = ((2.0f * (kPoint.Y - afOrigins[1]) / aiDirections[1]) -
                        yBox) / (2.0f*maxBox);
            kPoint.Z = ((2.0f * (kPoint.Z - afOrigins[2]) / aiDirections[2]) -
                        zBox) / (2.0f*maxBox);
            kVBuffer.SetPosition3(1, kPoint);
            kVBuffer.SetColor3(0, 1, m_kNormalColorPathUnvisited);
        } // Otherwise, set the point for the position along the parent

        // branch where the child branch starts.
        else {
            float fParentNormalizedDist = m_kFlyPathGraphCurve.getBranchParentNormalizedDist(iBranch);
            Curve3f kCurveParent = m_kFlyPathGraphCurve.getCurvePosition(iBranchParent);
            float fTimeParent = kCurveParent.GetTime(kCurveParent.GetTotalLength() * fParentNormalizedDist, 100,
                                                     1e-02f);
            kPoint = kCurveParent.GetPosition(fTimeParent); 
            kPoint.X = ((2.0f * (kPoint.X - afOrigins[0]) / aiDirections[0]) -
                        xBox) / (2.0f*maxBox);
            kPoint.Y = ((2.0f * (kPoint.Y - afOrigins[1]) / aiDirections[1]) -
                        yBox) / (2.0f*maxBox);
            kPoint.Z = ((2.0f * (kPoint.Z - afOrigins[2]) / aiDirections[2]) -
                        zBox) / (2.0f*maxBox);
            kVBuffer.SetPosition3(1, kPoint);
            kVBuffer.SetColor3(0, 1, m_kNormalColorPathUnvisited);
        }

        return new Polyline( kVBuffer, false, true );
    }

    /**
     * Get the geometry to be used for rendering the path of the specified branch.
     * @param   iBranch  Index which identifies the branch.
     * @return  Polyline instance that can be attached to a scene-graph Node for rendering
     *          <p>The  coordinates are scaled to match the TriMesh in JPanelSurface.</p>
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
        float[] aiDirections = new float[] { aiModelDirection[0], aiModelDirection[1], aiModelDirection[2]}; 
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
     * Called by the init() function. Creates and initialized the scene-graph.
     * @param arg0 the GLCanvas
     */
    private void CreateScene ()
    {
        // Create a scene graph with the face model as the leaf node.
        m_spkScene = new Node();
        m_spkCull = new CullState();
        m_spkCull.FrontFace = CullState.FrontMode.FT_CW; 
        m_spkCull.Enabled = false;
        m_spkScene.AttachGlobalState(m_spkCull);
        m_spkScene.AttachChild(m_kSurface);
 
        for ( int i = 0; i < m_pkRenderer.GetMaxLights(); i++ )
        {
            m_pkRenderer.SetLight( i, new Light() );
        }
        setupRender(m_kVolumeImageA.GetImage(), m_kOptions);
    }

    /**
     * Render the scene-graph.
     */
    private void Render()
    {
        m_kCuller.ComputeVisibleSet(m_spkScene);
        m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
    }

    private ModelImage resizeImage( ModelImage kInput )
    {        
        int[] aiNewExtents = kInput.getExtents();
        int[] aiExtents = m_kVolumeImageA.GetImage().getExtents();
        if ( (aiNewExtents[0] == aiExtents[0]) && 
                (aiNewExtents[1] == aiExtents[1]) && (aiNewExtents[2] == aiExtents[2]) )
        {
            return kInput;
        }
        float[] afRes = m_kVolumeImageA.GetImage().getResolutions(0);
        AlgorithmTransform transformFunct = 
            new AlgorithmTransform(kInput, new TransMatrix(4), AlgorithmTransform.TRILINEAR, 
                    afRes[0], afRes[1], afRes[2], 
                    aiExtents[0], aiExtents[1], aiExtents[2], false,
                true, false);


        transformFunct.setRunningInSeparateThread(false);
        transformFunct.run();
        ModelImage kResult = null;
        if (transformFunct.isCompleted() == true) 
        {
            kResult = transformFunct.getTransformedImage();
            kResult.calcMinMax();
        }
        transformFunct.finalize();
        transformFunct = null;
        return kResult;
    }

    /**
     * Update the camera location and view direction. Called from mouse dragged. Implements "inside looking out" object rotation.
     */
    private void updateCamera()
    {
        Matrix3f kRotate = m_kRotation.Local.GetRotate();
        Vector3f kUp = new Vector3f();
        Vector3f kDir = new Vector3f();
        Vector3f kRight = new Vector3f();

        if ( m_kFlyPathBehavior != null ) {

            Vector3f kCUp = m_kFlyPathBehavior.getViewUp();
            kCUp.normalize();
            Vector3f kCDir = m_kFlyPathBehavior.getViewDirection();
            kCDir.normalize();
            Vector3f kCRight = Vector3f.unitCross( kCDir, kCUp );

            kRotate.mult( kCUp, kUp );
            kRotate.mult( kCDir, kDir );
            kRotate.mult( kCRight, kRight );
            m_spkCamera.SetFrame( m_spkCamera.GetLocation(), kDir, kUp, kRight);
            if ( m_kControlFrame != null )
            {
                m_kControlFrame.updateOrientation(kRotate);
            }
            GetCanvas().display();
        }
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.GPURenderBase#Move()
     */
    @Override
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

    /**
     * Picking support for adding Annotation points to the scene.
     */
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
                m_kPicker.Execute(m_spkScene,kPos,kDir,0.0f,
                                  Float.MAX_VALUE);
                if (m_kPicker.Records.size() > 0)
                {
                    PickRecord kRecord = m_kPicker.GetClosestToZero();
                    // Get the normal vector for the picked point.
                    Vector3f kN0 = m_kSurface.VBuffer.GetNormal3( kRecord.iV0 );
                    kN0.scale(kRecord.B0);
                    Vector3f kN1 = m_kSurface.VBuffer.GetNormal3( kRecord.iV1 );
                    kN1.scale( kRecord.B1);
                    Vector3f kN2 = m_kSurface.VBuffer.GetNormal3( kRecord.iV2 );
                    kN2.scale( kRecord.B2 );
                    Vector3f kNormal = Vector3f.add( kN0, kN1 );
                    kNormal.add( kN2 ).normalize();

                    // Get picked point.
                    Vector3f kP0 = m_kSurface.VBuffer.GetPosition3( kRecord.iV0 );
                    kP0.scale(kRecord.B0);
                    Vector3f kP1 = m_kSurface.VBuffer.GetPosition3( kRecord.iV1 );
                    kP1.scale( kRecord.B1);
                    Vector3f kP2 = m_kSurface.VBuffer.GetPosition3( kRecord.iV2 );
                    kP2.scale( kRecord.B2 );
                    Vector3f kPoint = Vector3f.add( kP0, kP1 );
                    kPoint.add( kP2 );

                    createAnnotatePoint(kPoint);

                    // Get vector from current path position to the picked
                    // point.  This vector and the normal vector must be
                    // pointing in opposite directions.  If not, then the
                    // normal vector needs to be negated since the vertex
                    // ordering for the triangle mesh is not guaranteed.
                    Vector3f kV = Vector3f.sub(kPoint, m_spkCamera.GetLocation());

                    if (kV.dot(kNormal) > 0.0f) {
                        kNormal.neg();
                    }

                    // Add the point to the annotation list.
                    m_kAnnotateList.addItem(m_kFlyPathBehavior.getBranchIndex(),
                                            m_kFlyPathBehavior.getNormalizedPathDistance(),
                                            m_kFlyPathBehavior.isPathMoveForward(), kPoint, kNormal,
                                            getPositionUnScaled(m_spkCamera.GetLocation()), m_spkCamera.GetDVector(), 
                                            m_spkCamera.GetUVector(), m_spkCamera.GetRVector() );
                }
            }
        }
    }

    /**
     * Called any time a change has been made to a new branch so that the connection between the branch and its parent
     * can be rendered.
     */
    protected void resetRenderBranchConnect() {

        // Access the GeometryStripArray-derived instance for the rendering
        // of the specified branch path.
        int iBranch = m_kFlyPathBehavior.getBranchIndex();
        Polyline kLine = (Polyline)m_spkScene.GetChild(iBranch*2+1+1);

        // Mark the color as being visited.
        kLine.VBuffer.SetColor3(0, 0, m_kNormalColorPathVisited);
        kLine.VBuffer.SetColor3(0, 1, m_kNormalColorPathVisited);

        // Are we at a branch point and is there a currently selected branch,
        // or a previous one?
        int iSelectedBranch = m_kFlyPathBehavior.getSelectedBranchIndex();

        if ((-1 != iSelectedBranch) || (-1 != m_iLastSelectedBranchIndex)) {

            // If a branch was selected and it is different from
            // the one currently selected, then reset all of its colors
            // to normal.
            if ((-1 != m_iLastSelectedBranchIndex) && (m_iLastSelectedBranchIndex != iSelectedBranch)) {
                iBranch = m_iLastSelectedBranchIndex;
                kLine = (Polyline)m_spkScene.GetChild(iBranch*2+1);

                // How many points are in the array?
                int iNumPoints = kLine.VBuffer.GetVertexQuantity();
                int iIndex = 0;

                while (iIndex < m_aiBranchIndexUnvisitedMin[iBranch]) {
                    kLine.VBuffer.SetColor3(0, iIndex++, m_kNormalColorPathVisited);
                }

                while (iIndex <= m_aiBranchIndexUnvisitedMax[iBranch]) {
                    kLine.VBuffer.SetColor3(0, iIndex++, m_kNormalColorPathUnvisited);
                }

                while (iIndex < iNumPoints) {
                    kLine.VBuffer.SetColor3(0, iIndex++, m_kNormalColorPathVisited);
                }
            }

            // If this branch is selected, then reset the colors
            // for the entire path as being selected.  Redraw entire
            // branch just in case one of the unvisited limits changed.
            if (-1 != iSelectedBranch) {

                // Access the GeometryStripArray-derived instance for the rendering
                // of the specified branch path.
                iBranch = m_kFlyPathBehavior.getBranchIndex();
                kLine = (Polyline)m_spkScene.GetChild(iBranch*2+1);
                int iNumPoints = kLine.VBuffer.GetVertexQuantity();

                // Convert the current branch point position to curve
                // sample index for rendering.
                int iBranchPoint = (int) (iNumPoints * m_kFlyPathBehavior.getNormalizedPathDistance());

                // What is the range of samples to be colored as unvisited.
                int iUnvisitedMin = m_aiBranchIndexUnvisitedMin[iBranch];
                int iUnvisitedMax = m_aiBranchIndexUnvisitedMax[iBranch];

                // Heading start-to-end?
                if (m_kFlyPathBehavior.isPathMoveForward()) {

                    for (int i = iBranchPoint; i < iNumPoints; i++) {

                        if ((iUnvisitedMin <= i) && (i <= iUnvisitedMax)) {
                            kLine.VBuffer.SetColor3(0, i, m_kSelectColorPathUnvisited);

                        } else {
                            kLine.VBuffer.SetColor3(0, i, m_kSelectColorPathVisited);
                        }
                    }
                } // Heading end-to-start?
                else {

                    for (int i = iBranchPoint; i >= 0; i--) {

                        if ((iUnvisitedMin <= i) && (i <= iUnvisitedMax)) {
                            kLine.VBuffer.SetColor3(0, i, m_kSelectColorPathUnvisited);

                        } else {
                            kLine.VBuffer.SetColor3(0, i, m_kSelectColorPathVisited);
                        }
                    }
                }
            }
        }

        m_iLastSelectedBranchIndex = iSelectedBranch;
    }



    /**
     * Called any time the position along the current curve changes and the color of the curve needs to change to show
     * what has been visited.
     */
    protected void resetRenderBranchPath() {

        // Access the GeometryStripArray-derived instance for the rendering
        // of the specified branch path.
        int iBranch = m_kFlyPathBehavior.getBranchIndex();
        Polyline kLine = (Polyline)m_spkScene.GetChild(iBranch*2+1);

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

        kLine.Reload(true);
    }

}
