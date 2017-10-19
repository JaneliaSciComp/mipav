package gov.nih.mipav.view.renderer.volumeview;

import javax.media.opengl.*;
import com.sun.opengl.util.*;
import java.awt.event.*;
import java.util.Iterator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Vector;

import gov.nih.mipav.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.WildMagic.LibApplications.OpenGLApplication.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;
import gov.nih.mipav.view.WildMagic.LibRenderers.OpenGLRenderer.*;

import gov.nih.mipav.view.renderer.*;
import gov.nih.mipav.view.renderer.surfaceview.JPanelDisplay;
import gov.nih.mipav.view.renderer.volumeview.*;


/**
 * 
 * GPUVolumeRender and VolumeShaderEffect implement ray-cast volume shading.
 *
 * GPUVolumeRender applies a VolumeShaderEffect to the proxy geometry. The
 * VolumeShaderEffect contains the shader programs, texture images, and shader
 * UserConstant parameters needed for producing the ray-traced images. It
 * extends the ShaderEffect class in Wild Magic.  Volume rendering with proxy
 * geometry is presented in chapters 39 and 40 of GPU Gems: Programming
 * Techniques, Tips, and Tricks for Real-Time Graphics by Randima
 * Fernando. The volume shaders need to compute the start- and end-points of
 * the ray to trace in the 3D volume texture. The proxy geometry provides a
 * way of generating this information.
 * 
 * In the GPUVolumeRender class, the proxy geometry is a cube with x,y,z
 * dimensions based on the ModelImage data. During every rendering pass, cube
 * proxy-geometry is rendered twice. The first rendering pass renders to an
 * off-screen buffer, creating a texture image named SceneImage, which is used
 * during the second rendering pass.
 * 
 * In the first rendering pass the cube is rendered with the vertex colors set
 * equal to the texture coordinates, and with all front-facing polygons
 * removed. The resulting texture image, SceneImage shows the back faces of
 * the cube, where the color represents the texture coordinate of that
 * pixel the cube face.
 *
 * The SceneImage texture is passed to the volume pixel-shader on the second
 * rendering pass. The front-facing polygons of the cube are rendered, and the
 * texture-coordinates for each pixel calculated in the vertex-shader. The
 * pixel-shader thus has both the texture-coordinates of the front-facing
 * polygons and the back-facing polygons and can calculate a ray through the
 * volume in texture coordinates to trace.
 */
public class GPUVolumeRenderWM extends JavaApplication3DWM
implements GLEventListener, KeyListener, MouseMotionListener
{
	
	 /** Volume opacity control dialog. */
    private JPanelVolOpacityBaseWM volOpacityPanel;
    /** Dialog to turn bounding box of surface renderer on and off, and to change the color of the frame. */
    private JPanelDisplayWM boxPanel;
    /** Dialog to turn the clipping palne box on and off. */
    private JPanelClipWM clipPanel;
    /** Resolutions of image A. */
    private float[] resols = new float[4];
    /** Numbers dicatating the sizes of the planes based on the extents and
     * resolutions of the image. */
    private float xBox, yBox, zBox, maxBox;
    /** Sculptor panel. */
    private JPanelSculptorWM sculptorPanel;
    // Light Interface:
    /** Light dialog for when the user clicks the light button. */
    private JPanelLightsWM m_kLightsControl;
    
    /**
     * Constructs a new GPUVolumeRender object.
     * @param kImageA ModelImage A
     * @param kLUTa, LUT for ModelImage A
     * @param kRGBTa, RGB lookup table for ModelImage A
     * @param kImageB ModelImage B
     * @param kLUTb, LUT for ModelImage B
     * @param kRGBTb, RGB lookup table for ModelImage B
     */
    public GPUVolumeRenderWM( ModelImage kImageA, ModelLUT kLUTa, ModelRGB kRGBTa,
            ModelImage kImageB, ModelLUT kLUTb, ModelRGB kRGBTb, ViewJFrameVolumeViewWM parentFrame  )
    {

        super("GPUVolumeRender",0,0,512,512, new ColorRGBA(0.0f,0.0f,0.0f,0.0f), parentFrame);

        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                m_eBuffering, m_eMultisampling,
                m_iWidth, m_iHeight );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );       

        ImageCatalog.SetActive( new ImageCatalog("Main", System.getProperties().getProperty("user.dir")) );      
        VertexProgramCatalog.SetActive(new VertexProgramCatalog("Main", System.getProperties().getProperty("user.dir")));       
        PixelProgramCatalog.SetActive(new PixelProgramCatalog("Main", System.getProperties().getProperty("user.dir")));
        CompiledProgramCatalog.SetActive(new CompiledProgramCatalog());
        
        m_kImageA = kImageA;
        m_kLUTa = kLUTa;
        m_kRGBTa = kRGBTa;

        m_kImageB = kImageB;
        m_kLUTb = kLUTb;
        m_kRGBTb = kRGBTb;

        m_iDimX = m_kImageA.getExtents()[0];
        m_iDimY = m_kImageA.getExtents()[1];
        m_iDimZ = m_kImageA.getExtents()[2];
        m_fScale = 1.0f/(float)(Math.max(m_iDimX,Math.max(m_iDimY,m_iDimZ)));
        m_iLen = m_iDimX*m_iDimY*m_iDimZ;
        
        resols = m_kImageA.getResolutions( 0 );
        for ( int i = 0; i < 3; i++ ) {
            resols[i] = Math.abs(resols[i]);
        }

        if ((resols[0] == 0.0f) || (resols[1] == 0.0f) || (resols[2] == 0.0f)) {
            resols[0] = 1.0f;
            resols[1] = 1.0f;
            resols[2] = 1.0f;
        }
        
        xBox = (m_iDimX - 1) * resols[0];
        yBox = (m_iDimY - 1) * resols[1];
        zBox = (m_iDimZ - 1) * resols[2];
        maxBox = xBox;

        if (yBox > maxBox) {
            maxBox = yBox;
        }

        if (zBox > maxBox) {
            maxBox = zBox;
        }


        // Normalize the size
        // xBox range between 0 - 1.
        xBox = xBox / maxBox;
        yBox = yBox / maxBox;
        zBox = zBox / maxBox;
        
        
        if (m_kImageA.isColorImage()) {
            volOpacityPanel = new JPanelVolOpacityRGBWM(this, m_kImageA, m_kImageB);
        } else {
            volOpacityPanel = new JPanelVolOpacityWM(this, m_kImageA, m_kImageB);
        }
        boxPanel = new JPanelDisplayWM(this);
        clipPanel = new JPanelClipWM(this, xBox, yBox, zBox);
        sculptorPanel = new JPanelSculptorWM(this);
        m_kLightsControl = new JPanelLightsWM(this);
        

    }

    /**
     * Return volume opacity control Dialog from parent frame.
     *
     * @return  volOpacityPanel volume opacity dialog box.
     */
    public JPanelVolOpacityBaseWM getVolOpacityPanel() {
        return volOpacityPanel;
    }

    /**
     * Return the surface light control dialog box.
     *
     * @return  lights Surface light dialog box
     */
    public JPanelLightsWM getLightPanel() {
        return m_kLightsControl;
    }
    
    /**
     * Return the diaplay dialog.
     *
     * @return  boxPanel Display dialog.
     */
    public JPanelDisplayWM getDisplayPanel() {
        return boxPanel;
    }

    /**
     * Return clipPanel from parent frame.
     *
     * @return  clipPanel Clip Dialog box.
     */
    public JPanelClipWM getClipPanel() {
        return clipPanel;
    }
    
    /**
     * Return the sculptor panel.
     *
     * @return  JPanelSculptor sculptor control panel.
     */
    public JPanelSculptorWM getSculptorPanel() {
        return sculptorPanel;
    }
    
    /**
     * memory cleanup.
     */
    public void dispose()
    {
        //((OpenGLRenderer)m_pkRenderer).SetDrawable( GetCanvas() );

        if ( m_spkScene != null )
        {
            m_spkScene.dispose();
            m_spkScene = null;
        }
        if ( m_spkWireframe != null )
        {
            m_spkWireframe.dispose();
            m_spkWireframe = null;
        }
        if ( m_spkCull != null )
        {
            m_spkCull.dispose();
            m_spkCull = null;
        }
        if ( m_kCuller != null )
        {
            m_kCuller.dispose();
            m_kCuller = null;
        }
        for ( int i = 0; i < 6; i++ )
        {
            m_akBoundingBox[i].dispose();
            m_akBoundingBox[i] = null;
            m_akOrientationCube[i].dispose();
            m_akOrientationCube[i] = null;
            m_akPolyline[i].dispose();
            m_akPolyline[i] = null;
        }
        m_akBoundingBox = null;
        m_akOrientationCube = null;
        m_akPolyline = null;

        m_aakAxisFiles = null;
        if ( m_kCubeTranslate != null )
        {
            m_kCubeTranslate.dispose();
            m_kCubeTranslate = null;
        }
        if ( m_kClipArb != null )
        {
            m_kClipArb.dispose();
            m_kClipArb = null;
        }
        if ( m_kClipEye != null )
        {
            m_kClipEye.dispose();
            m_kClipEye = null;
        }
        if ( m_kClipEyeInv != null )
        {
            m_kClipEyeInv.dispose();
            m_kClipEyeInv = null;
        }
        m_abDisplayPolyline = null;
        if ( m_kVolumeShaderEffect != null )
        {
            m_kVolumeShaderEffect.dispose();
            m_kVolumeShaderEffect = null;
        }
        m_kImageA = null;
        m_kLUTa = null;
        m_kRGBTa = null;
        m_kImageB = null;
        m_kLUTb = null;
        m_kRGBTb = null;

        if ( m_spkScreenCamera != null )
        {
            m_spkScreenCamera.dispose();
            m_spkScreenCamera = null;
        }
        if ( m_spkEyeCamera != null )
        {
            m_spkEyeCamera.dispose();
            m_spkEyeCamera = null;
        }
        if ( m_spkScenePolygon != null )
        {
            m_spkScenePolygon.dispose();
            m_spkScenePolygon = null;
        }
        if ( m_spkSceneImage != null )
        {
            m_spkSceneImage.dispose();
            m_spkSceneImage = null;
        }
        if ( m_pkSceneTarget != null )
        {
            m_pkSceneTarget.dispose();
            m_pkSceneTarget = null;
        }
        if ( m_pkPBuffer != null )
        {
            m_pkPBuffer.dispose();
            m_pkPBuffer = null;
        }

        if ( m_spkVertexColor3Shader != null )
        {
            m_spkVertexColor3Shader.dispose();
            m_spkVertexColor3Shader = null;
        }

        if ( m_kTranslate != null )
        {
            m_kTranslate.dispose();
            m_kTranslate = null;
        }
        if ( m_kArbitraryClip != null )
        {
            m_kArbitraryClip.dispose();
            m_kArbitraryClip = null;
        }
        if ( m_kMaterial != null )
        {
            m_kMaterial.dispose();
            m_kMaterial = null;
        }
        if ( m_kMesh != null )
        {
            m_kMesh.dispose();
            m_kMesh = null;
        }
        m_akLights = null;

        if ( m_kSculptor != null )
        { 
            m_kSculptor.disposeLocal();
            m_kSculptor = null;
        }
        m_kLUTa = null;
        for ( int i = 0; i < 4; i++ )
        {
            m_akTransfer[i] = null;
        }
        if ( m_kShaderParamsWindow != null )
        {
            m_kShaderParamsWindow.close();
        }
        super.dispose();

        m_kAnimator.stop();
        m_kAnimator = null;

        System.gc();
    }

    public ModelImage getImage()
    {
        return m_kImageA;
    }

    public ModelImage getImageB()
    {
        return m_kImageB;
    }
    
    /**
     * Part of the GLEventListener interface.
     * display is called by the Animator object.
     * @param arg0, the GLAutoDrawable (GLCanvas) to display.
     */
    public void display(GLAutoDrawable arg0) {
        if ( !m_bVisible )
        {
            return;
        }

        //((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        MeasureTime();

        if (MoveCamera())
        {
            m_kCuller.ComputeVisibleSet(m_spkScene);
        }        
        if (MoveObject())
        {
            m_spkScene.UpdateGS();
            m_kCuller.ComputeVisibleSet(m_spkScene);
        }

        // Draw the scene to the back buffer/
        if (m_pkRenderer.BeginScene())
        {
            if ( m_bDisplayAllEllipsoids )
            {
                m_pkRenderer.SetBackgroundColor(m_kBackgroundColor);
                m_pkRenderer.ClearBuffers();

                DisplayAllEllipsoids();
            }
            else
            {
                m_pkPBuffer.SetDrawable( arg0 );

                if ( !m_bDisplaySecond )
                {
                    // First rendering pass:
                    // Draw the proxy geometry to a color buffer, to generate the
                    // back-facing texture-coordinates:
                    m_kMesh.DetachAllEffects();
                    m_kMesh.AttachEffect( m_spkVertexColor3Shader );
                    m_kCuller.ComputeVisibleSet(m_spkScene);
                    // Enable rendering to the PBuffer:
                    m_pkRenderer.SetBackgroundColor(ColorRGBA.BLACK);
                    m_pkRenderer.ClearBuffers();
                    // Cull front-facing polygons:
                    m_spkCull.CullFace = CullState.CullMode.CT_FRONT;
                    m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
                    // Undo culling:
                    m_spkCull.CullFace = CullState.CullMode.CT_BACK;

                    // Displays any geometry that is inside the volume. In
                    // this case the geometry is the fiber-bundle tracts
                    // for the brain. The tracts are displayed with the
                    // colors corresponding to the positions -- thus the
                    // ray tracing will stop at the interseced geometry
                    // instead of the far wall of the proxy cube. The
                    // intersecting geometry must be rendered twice, once
                    // into the pbuffer to use for the ray-tracing bounds,
                    // and once to display it.
                    if ( m_bDisplayTract && (m_kTracts != null) )
                    {
                        DisplayTract(m_spkVertexColor3Shader );
                    }
                    else
                    {
                        m_spkAlpha.BlendEnabled = false;
                    }


                }
                else
                {
                    // First rendering pass:
                    // Draw the proxy geometry to a color buffer, to generate the
                    // back-facing texture-coordinates:
                    m_kMesh.DetachAllEffects();
                    m_kMesh.AttachEffect( m_spkVertexColor3Shader );
                    m_kCuller.ComputeVisibleSet(m_spkScene);
                    // Enable rendering to the PBuffer:
                    m_pkPBuffer.Enable();
                    m_pkRenderer.SetBackgroundColor(ColorRGBA.BLACK);
                    m_pkRenderer.ClearBuffers();
                    // Cull front-facing polygons:
                    m_spkCull.CullFace = CullState.CullMode.CT_FRONT;
                    m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
                    // Undo culling:
                    m_spkCull.CullFace = CullState.CullMode.CT_BACK;

                    // Displays any geometry that is inside the volume. In
                    // this case the geometry is the fiber-bundle tracts
                    // for the brain. The tracts are displayed with the
                    // colors corresponding to the positions -- thus the
                    // ray tracing will stop at the interseced geometry
                    // instead of the far wall of the proxy cube. The
                    // intersecting geometry must be rendered twice, once
                    // into the pbuffer to use for the ray-tracing bounds,
                    // and once to display it.
                    if ( (m_bDisplayEllipsoids || m_bDisplayTract) && (m_kTracts != null) )
                    {
                        DisplayTract(m_spkVertexColor3Shader);
                    }


                    // Disable the PBuffer
                    m_pkPBuffer.Disable();


                    // Second rendering pass:
                    // Draw the proxy grometry with the volume ray-tracing shader:
                    m_kMesh.DetachAllEffects();
                    m_kMesh.AttachEffect( m_kVolumeShaderEffect );
                    m_kCuller.ComputeVisibleSet(m_spkScene);
                    m_pkRenderer.SetBackgroundColor(m_kBackgroundColor);
                    m_pkRenderer.ClearBuffers();

                    // Displays any geometry that is inside the volume. In
                    // this case the geometry is the fiber-bundle tracts
                    // for the brain. This is the second display, and the
                    // tracts are shown with the non-position dependant
                    // colors.
                    if ( m_bDisplayEllipsoids )
                    {
                        DisplayEllipsoids();
                    }
                    else if ( m_bDisplayTract && (m_kTracts != null ) )
                    {
                        DisplayTract(null);
                    }
                    else
                    {
                        m_spkAlpha.BlendEnabled = false;
                    }

                    m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());

                    if ( m_bDisplayClipEye || m_bDisplayClipEyeInv )
                    {
                        m_spkEyeCamera.SetLocation(m_spkCamera.GetLocation());
                        m_pkRenderer.SetCamera(m_spkEyeCamera);
                        if ( m_bDisplayClipEye )
                        {
                            m_pkRenderer.Draw(m_kClipEye);
                        }
                        if ( m_bDisplayClipEyeInv )
                        {
                            m_pkRenderer.Draw(m_kClipEyeInv);
                        }
                        m_pkRenderer.SetCamera(m_spkCamera);
                    }

                    if ( m_bDisplayOrientationCube )
                    {
                        for ( int i = 0; i < 6; i++ )
                        {
                            m_akOrientationCube[i].Local.SetRotate(m_spkScene.Local.GetRotate());
                            m_akOrientationCube[i].UpdateGS();
                            m_akOrientationCube[i].UpdateRS();
                            m_pkRenderer.LoadResources(m_akOrientationCube[i]);
                            m_pkRenderer.Draw(m_akOrientationCube[i]);
                        }
                    }

                    /*
                    // Draw screne polygon:
                    m_pkRenderer.SetCamera(m_spkScreenCamera);
                    m_pkRenderer.Draw(m_spkScenePolygon);
                     */
                    //Draw frame rate:
                    m_pkRenderer.SetCamera(m_spkCamera);
                    //DrawFrameRate(8,16,ColorRGBA.WHITE);


                    if ( (m_kSculptor != null) && m_kSculptor.IsSculptDrawn() )
                    {
                        m_pkRenderer.Draw( m_kSculptor.getSculptImage() );
                    }
                }            
            }
            m_pkRenderer.EndScene();
        }
        m_pkRenderer.DisplayBackBuffer();

        UpdateFrameCount();
    }

    /**
     * Part of the GLEventListener interface.
     */
    public void displayChanged(GLAutoDrawable arg0, boolean arg1, boolean arg2) {}

    /**
     * Part of the GLEventListener interface. Init is called once when the
     * GLCanvas is first displayed. Called again if the GLCanvas is removed
     * from the frame and added to another window or panel. Initializes the
     * display.
     * @param arg0, GLCanvas
     */
    public void init(GLAutoDrawable arg0) {
        if ( m_bInit )
        {
            if ( !m_kAnimator.isAnimating() )
            {
                m_kAnimator.start();
            }        

            return;
        }
        m_bInit = true;

        arg0.setAutoSwapBufferMode( false );

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

        m_kAnimator = new Animator( GetCanvas() );
        m_kAnimator.setRunAsFastAsPossible(true); 
        m_kAnimator.start();
    }

    /**
     * Part of the GLEventListener interface. Called when the GLCanvas is resized.
     * @param arg0, the GLCanvas
     * @param iX, the new x-position in screen coordinates.
     * @param iY, the new y-position in screen coordinates.
     * @param iWidth, the new width
     * @param iHeight, the new height
     */
    public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight) {
        if (iWidth > 0 && iHeight > 0)
        {
            m_iWidth = iWidth;
            m_iHeight = iHeight;
            if ( m_spkCamera.Perspective )
            {
                m_spkCamera.SetFrustum(60.0f,m_iWidth/(float)m_iHeight,0.01f,10.0f);
            }
            else
            {
                m_spkCamera.SetFrustum(60.0f,m_iWidth/(float)m_iHeight,1f,10.0f);
            }

            if (m_pkRenderer != null)
            {
                //((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
                m_pkRenderer.Resize(iWidth,iHeight);
                //((OpenGLRenderer)m_pkRenderer).ClearDrawable( );
            }
        }
    }

    /**
     * Returns the GLCanvas in the m_pkRenderer object.
     * @return OpenGLRenderer.GLCanvas
     */
    public GLCanvas GetCanvas()
    {
        return ((OpenGLRenderer)m_pkRenderer).GetCanvas();
    }

    /**
     * Called by the init() function. Creates and initialized the scene-graph.
     * @param arg0, the GLCanvas
     */
    private void CreateScene (GLAutoDrawable arg0)
    {
        // The screen camera is designed to map (x,y,z) in [0,1]^3 to (x',y,'z')
        // in [-1,1]^2 x [0,1].
        m_spkScreenCamera = new Camera();
        m_spkScreenCamera.Perspective = false;
        m_spkScreenCamera.SetFrustum(0.0f,1.0f,0.0f,1.0f,0.0f,1.0f);
        m_spkScreenCamera.SetFrame(Vector3f.ZERO,Vector3f.UNIT_Z,
                Vector3f.UNIT_Y,Vector3f.UNIT_X);

        // Create a scene graph with the face model as the leaf node.
        m_spkScene = new Node();
        CreateBox();
        m_spkScene.AttachChild( m_kMesh );
        m_spkWireframe = new WireframeState();
        m_spkScene.AttachGlobalState(m_spkWireframe);
        m_spkCull = new CullState();
        m_spkScene.AttachGlobalState(m_spkCull);
        m_spkAlpha = new AlphaState();
        m_spkAlpha.BlendEnabled = false;
        m_spkAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE_MINUS_DST_COLOR;
        m_spkAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
        m_spkScene.AttachGlobalState(m_spkAlpha);

        // Create a screen polygon to use as the RGBA render target.
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetTChannels(0,2);
        VertexBuffer pkVBuffer = new VertexBuffer(kAttr,4);
        pkVBuffer.SetPosition3(0,0.0f,0.0f,0.0f) ;
        pkVBuffer.SetPosition3(1,0.2f,0.0f,0.0f) ;
        pkVBuffer.SetPosition3(2,0.2f,0.2f,0.0f) ;
        pkVBuffer.SetPosition3(3,0.0f,0.2f,0.0f) ;
        pkVBuffer.SetTCoord2(0,0,0.0f,0.0f) ;
        pkVBuffer.SetTCoord2(0,1,1.0f,0.0f) ;
        pkVBuffer.SetTCoord2(0,2,1.0f,1.0f) ;
        pkVBuffer.SetTCoord2(0,3,0.0f,1.0f) ;
        IndexBuffer pkIBuffer = new IndexBuffer(6);
        int[] aiIndex = pkIBuffer.GetData();
        aiIndex[0] = 0;  aiIndex[1] = 1;  aiIndex[2] = 2;
        aiIndex[3] = 0;  aiIndex[4] = 2;  aiIndex[5] = 3;
        m_spkScenePolygon = new TriMesh(pkVBuffer,pkIBuffer);

        // Create a red image for the purposes of debugging.  The image itself
        // should not show up because the frame-buffer object writes to the
        // texture memory.  But if this fails, the red image should appear.
        byte[] aucData = new byte[4*m_iWidth*m_iHeight];
        for (int i = 0; i < m_iWidth*m_iHeight; i++)
        {
            aucData[i++] = (byte)0xFF;
            aucData[i++] = 0x00;
            aucData[i++] = 0x00;
            aucData[i++] = (byte)0xFF;
        }
        m_spkSceneImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA8888,m_iWidth,m_iHeight,aucData,
        "SceneImage");

        // Create the texture effect for the scene polygon.  The resources are
        // loaded so that the scene target texture has everything needed for
        // FrameBuffer::Create to succeed.
        TextureEffect pkEffect = new TextureEffect("SceneImage");
        m_pkSceneTarget = pkEffect.GetPTexture(0,0);
        m_pkSceneTarget.SetFilterType(Texture.FilterType.NEAREST);
        m_pkSceneTarget.SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_pkSceneTarget.SetWrapType(1,Texture.WrapType.CLAMP_BORDER);

        m_pkSceneTarget.SetOffscreenTexture(true);
        m_spkScenePolygon.AttachEffect(pkEffect);
        m_spkScenePolygon.UpdateGS();
        m_spkScenePolygon.UpdateRS();
        m_pkRenderer.LoadResources(m_spkScenePolygon);


        // Create the RGBA frame-buffer object to be bound to the scene polygon.
        m_pkPBuffer = new OpenGLFrameBuffer(m_eFormat,m_eDepth,m_eStencil,
                m_eBuffering,m_eMultisampling,m_pkRenderer,m_pkSceneTarget,arg0);
        assert(m_pkPBuffer != null);

        m_spkScene.UpdateGS();
        m_kTranslate = new Vector3f( m_spkScene.WorldBound.GetCenter() );
        m_kTranslate.negEquals();
        m_spkScene.GetChild(0).Local.SetTranslate( m_kTranslate );

        int iNumImages = 1;
        if ( m_kImageB != null )
        {
            iNumImages = 2;
        }
        m_kVolumeShaderEffect = new VolumeShaderEffect(m_kImageA, m_kLUTa, m_kRGBTa, 
                m_kImageB, m_kLUTb, m_kRGBTb, 
                m_pkSceneTarget);
        m_pkRenderer.LoadResources(m_kVolumeShaderEffect);
        m_kVolumeShaderEffect.SetPassQuantity(1);
        m_kVolumeShaderEffect.MIPMode(m_pkRenderer);
        m_kVolumeShaderEffect.Blend(0.5f);

        for ( int i = 0; i < 4; i++ )
        {
            if ( m_akTransfer[i] != null )
            {
                updateImages(i, m_akTransfer[i]);
            }
        }
        if ( m_kRGBTa != null )
        {
            setRGBT( m_kRGBTa, 0 );
        }
        if ( m_kRGBTb != null )
        {
            setRGBT( m_kRGBTb, 1 );
        }

        int iXBound = m_kImageA.getExtents()[0];
        int iYBound = m_kImageA.getExtents()[1];
        int iZBound = m_kImageA.getExtents()[2];
        InitDisplay(iXBound,iYBound,iZBound);

        for ( int i = 0; i < m_pkRenderer.GetMaxLights(); i++ )
        {
            m_pkRenderer.SetLight( i, new Light() );
        }
    }

    /**
     * Called when the opacity transfer function changes.
     * @param iImage, the image the transfer function modifies (ImageA = 0,
     * ImageB = 1, ImageAGM = 2, ImageBGM = 3)
     * @param kTransfer, the new transfer function
     * @return returns if the image has been modified.
     */
    public boolean updateImages(int iImage, TransferFunction kTransfer)
    {
        if ( m_kVolumeShaderEffect == null )
        {
            m_akTransfer[iImage] = kTransfer;
            return false;
        }
        m_kVolumeShaderEffect.UpdateImages(kTransfer, iImage);
        return true;
    }

    /**
     * This methods calls corresponding render to update images with LUT changes.
     *
     * @param   LUTa        LUT used to update imageA
     * @param   LUTb        LUT used to update imageB
     *
     * @return  boolean confirming successful update
     */
    public void updateImages(ModelLUT kLUTa, ModelLUT kLUTb)
    {
        m_kVolumeShaderEffect.UpdateImages(kLUTa, kLUTb);
    }

    /**
     * Called by CreateScene. Creates the bounding-box proxy geometry scene
     * node.
     */
    private void CreateBox ()
    {
        int iXBound = m_kImageA.getExtents()[0];
        int iYBound = m_kImageA.getExtents()[1];
        int iZBound = m_kImageA.getExtents()[2];
        Box(iXBound,iYBound,iZBound);
        m_spkVertexColor3Shader = new VertexColor3Effect();
    }

    /**
     * Called by CreateBox. Creates the bounding-box proxy geometry (VertexBuffer, IndexBuffer).
     * @param iXBound image x-extent.
     * @param iYBound image y-extent.
     * @param iZBound image z-extent.
     * @return TriMesh, new geometry.
     */
    private TriMesh Box (int iXBound, int iYBound, int iZBound)
    {
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetCChannels(0,3);
        kAttr.SetTChannels(0,3);
        kAttr.SetTChannels(1,3);

        float fMaxX = (float) (iXBound - 1) * m_kImageA.getFileInfo(0).getResolutions()[0];
        float fMaxY = (float) (iYBound - 1) * m_kImageA.getFileInfo(0).getResolutions()[1];
        float fMaxZ = (float) (iZBound - 1) * m_kImageA.getFileInfo(0).getResolutions()[2];

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

        int iVQuantity = 24;
        int iTQuantity = 12;
        VertexBuffer pkVB = new VertexBuffer(kAttr,iVQuantity);
        IndexBuffer pkIB = new IndexBuffer(3*iTQuantity);

        // generate connectivity (outside view)
        int i = 0;
        int[] aiIndex = pkIB.GetData();

        // generate geometry
        // front
        pkVB.SetPosition3(0,0,0,0);
        pkVB.SetPosition3(1,m_fX,0,0);
        pkVB.SetPosition3(2,m_fX,m_fY,0);
        pkVB.SetPosition3(3,0,m_fY,0);
        pkVB.SetColor3(0,0,0,0,0);
        pkVB.SetColor3(0,1,1,0,0);
        pkVB.SetColor3(0,2,1,1,0);
        pkVB.SetColor3(0,3,0,1,0);
        aiIndex[i++] = 0;  aiIndex[i++] = 2;  aiIndex[i++] = 1;
        aiIndex[i++] = 0;  aiIndex[i++] = 3;  aiIndex[i++] = 2;

        // back
        pkVB.SetPosition3(4,0,0,m_fZ);
        pkVB.SetPosition3(5,m_fX,0,m_fZ);
        pkVB.SetPosition3(6,m_fX,m_fY,m_fZ);
        pkVB.SetPosition3(7,0,m_fY,m_fZ);
        pkVB.SetColor3(0,4,0,0,1);
        pkVB.SetColor3(0,5,1,0,1);
        pkVB.SetColor3(0,6,1,1,1);
        pkVB.SetColor3(0,7,0,1,1);
        aiIndex[i++] = 4;  aiIndex[i++] = 5;  aiIndex[i++] = 6;
        aiIndex[i++] = 4;  aiIndex[i++] = 6;  aiIndex[i++] = 7;

        // top
        pkVB.SetPosition3(8,0,m_fY,0);
        pkVB.SetPosition3(9,m_fX,m_fY,0);
        pkVB.SetPosition3(10,m_fX,m_fY,m_fZ);
        pkVB.SetPosition3(11,0,m_fY,m_fZ);
        pkVB.SetColor3(0,8,0,1,0);
        pkVB.SetColor3(0,9,1,1,0);
        pkVB.SetColor3(0,10,1,1,1);
        pkVB.SetColor3(0,11,0,1,1);
        aiIndex[i++] = 8;  aiIndex[i++] = 10;  aiIndex[i++] = 9;
        aiIndex[i++] = 8;  aiIndex[i++] = 11;  aiIndex[i++] = 10;

        // bottom
        pkVB.SetPosition3(12,0,0,0);
        pkVB.SetPosition3(13,m_fX,0,0);
        pkVB.SetPosition3(14,m_fX,0,m_fZ);
        pkVB.SetPosition3(15,0,0,m_fZ);
        pkVB.SetColor3(0,12,0,0,0);
        pkVB.SetColor3(0,13,1,0,0);
        pkVB.SetColor3(0,14,1,0,1);
        pkVB.SetColor3(0,15,0,0,1);
        aiIndex[i++] = 12;  aiIndex[i++] = 13;  aiIndex[i++] = 14;
        aiIndex[i++] = 12;  aiIndex[i++] = 14;  aiIndex[i++] = 15;

        // right
        pkVB.SetPosition3(16,m_fX,0,0);
        pkVB.SetPosition3(17,m_fX,m_fY,0);
        pkVB.SetPosition3(18,m_fX,m_fY,m_fZ);
        pkVB.SetPosition3(19,m_fX,0,m_fZ);
        pkVB.SetColor3(0,16,1,0,0);
        pkVB.SetColor3(0,17,1,1,0);
        pkVB.SetColor3(0,18,1,1,1);
        pkVB.SetColor3(0,19,1,0,1);
        aiIndex[i++] = 16;  aiIndex[i++] = 17;  aiIndex[i++] = 18;
        aiIndex[i++] = 16;  aiIndex[i++] = 18;  aiIndex[i++] = 19;

        // left
        pkVB.SetPosition3(20,0,0,0);
        pkVB.SetPosition3(21,0,m_fY,0);
        pkVB.SetPosition3(22,0,m_fY,m_fZ);
        pkVB.SetPosition3(23,0,0,m_fZ);
        pkVB.SetColor3(0,20,0,0,0);
        pkVB.SetColor3(0,21,0,1,0);
        pkVB.SetColor3(0,22,0,1,1);
        pkVB.SetColor3(0,23,0,0,1);
        aiIndex[i++] = 20;  aiIndex[i++] = 22;  aiIndex[i++] = 21;
        aiIndex[i++] = 20;  aiIndex[i++] = 23;  aiIndex[i++] = 22;

        if (kAttr.GetMaxTCoords() > 0)
        {
            for (int iUnit = 0; iUnit < kAttr.GetMaxTCoords(); iUnit++)
            {
                if (kAttr.HasTCoord(iUnit))
                {
                    pkVB.SetTCoord3(iUnit,0,0,0,0);
                    pkVB.SetTCoord3(iUnit,1,1,0,0);
                    pkVB.SetTCoord3(iUnit,2,1,1,0);
                    pkVB.SetTCoord3(iUnit,3,0,1,0);
                    pkVB.SetTCoord3(iUnit,4,0,0,1);
                    pkVB.SetTCoord3(iUnit,5,1,0,1);
                    pkVB.SetTCoord3(iUnit,6,1,1,1);
                    pkVB.SetTCoord3(iUnit,7,0,1,1);
                    pkVB.SetTCoord3(iUnit,8,0,1,0);
                    pkVB.SetTCoord3(iUnit,9,1,1,0);
                    pkVB.SetTCoord3(iUnit,10,1,1,1);
                    pkVB.SetTCoord3(iUnit,11,0,1,1);
                    pkVB.SetTCoord3(iUnit,12,0,0,0);
                    pkVB.SetTCoord3(iUnit,13,1,0,0);
                    pkVB.SetTCoord3(iUnit,14,1,0,1);
                    pkVB.SetTCoord3(iUnit,15,0,0,1);
                    pkVB.SetTCoord3(iUnit,16,1,0,0);
                    pkVB.SetTCoord3(iUnit,17,1,1,0);
                    pkVB.SetTCoord3(iUnit,18,1,1,1);
                    pkVB.SetTCoord3(iUnit,19,1,0,1);
                    pkVB.SetTCoord3(iUnit,20,0,0,0);
                    pkVB.SetTCoord3(iUnit,21,0,1,0);
                    pkVB.SetTCoord3(iUnit,22,0,1,1);
                    pkVB.SetTCoord3(iUnit,23,0,0,1);
                }
            }
        }
        m_kMesh = new TriMesh(pkVB,pkIB);

        // polished gold
        m_kMaterial = new MaterialState();
        m_kMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
        m_kMaterial.Ambient = new ColorRGB(0.1f,0.1f,0.1f);
        m_kMaterial.Diffuse = new ColorRGB(1f,1f,1f);
        m_kMaterial.Specular = new ColorRGB(1f,1f,1f);
        m_kMaterial.Shininess = 64f;
        m_kMesh.AttachGlobalState(m_kMaterial);
        m_kMesh.UpdateMS(true);
        return m_kMesh;
    }

    /**
     * Part of the KeyListener interface. Pressing 'b' toggles displaying the
     * proxy-geometry versus the ray-traced volume.
     * @param e, the key event.
     */
    public void keyPressed(KeyEvent e) {
        char ucKey = e.getKeyChar();
        super.keyPressed(e);
        switch (ucKey)
        {
        case 's':
        case 'S':
            TestStreaming(m_spkScene,"VolumeTextures.wmof");
            return;
        case 'b':
            m_bDisplaySecond = !m_bDisplaySecond;
            return;
        case 'v':
            m_bDisplayEllipsoids = !m_bDisplayEllipsoids;
            return;
        case 'd':
            m_iEllipsoidMod--;
            if ( m_iEllipsoidMod < 1 )
            {
                m_iEllipsoidMod = 1;
            }
            return;
        case 'f':
            m_iEllipsoidMod++;
            if ( m_iEllipsoidMod > 100 )
            {
                m_iEllipsoidMod = 100;
            }
            return;
        case 'c':
            System.err.println(m_iActive);
            if (m_iActive == 0)
            {
                m_spkAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
                m_spkAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
                m_iActive = 1;
            }
            else if (m_iActive == 1)
            {
                // soft additive
                m_spkAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE_MINUS_DST_COLOR;
                m_spkAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
                m_iActive = 2;
            }
            else
            {
                // multiplicative
                m_spkAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_DST_COLOR;
                m_spkAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ZERO;
                m_iActive = 0;
            }
            return;
        }
        return;
    }

    /** 
     * Changes the projection to Orthographic Projection.
     */
    public void setOrthographicProjection()
    {
        m_spkCamera.Perspective = false;
        m_spkCamera.SetFrustum(60.0f,m_iWidth/(float)m_iHeight,1f,10.0f);
        m_pkRenderer.OnFrustumChange();
    }

    /** 
     * Changes the projection to Perspective Projection.
     */
    public void setPerspectiveProjection()
    {
        m_spkCamera.Perspective = true;
        m_spkCamera.SetFrustum(60.0f,m_iWidth/(float)m_iHeight,0.01f,10.0f);
        m_pkRenderer.OnFrustumChange();
    }

    /**
     * Returns true when sculpting is enabled.
     * @return true when sculpting is enabled, false otherwise.
     */
    public boolean getSculptEnabled()
    {
        if ( m_kSculptor == null )
        {
            return false;
        }
        return m_kSculptor.getEnable();
    }

    /**
     * Enables and disables sculpting.
     * @param bSculpt, true to enable sculpting, false to disable.
     */
    public void enableSculpt( boolean bSculpt )
    {
        if ( m_kSculptor == null )
        {
            m_kSculptor = new SculptorWm( ((OpenGLRenderer)m_pkRenderer).GetCanvas() );
            m_kSculptor.setImage(m_kImageA, m_kImageB);
        }
        m_kSculptor.enableSculpt(bSculpt);
    }

    /**
     * Invert the sculpt region.
     */
    public void invertSculpt()
    {
        if ( m_kSculptor != null )
        {
            m_kSculptor.invertSculpt();
        }
    }

    /**
     * Clear the sculpt region.
     */
    public void clearSculpt()
    {
        if ( m_kSculptor != null )
        {
            m_kSculptor.clearSculpt();
        }
    }

    /**
     * Undo applying the sculpt region to the volume.
     */
    public void undoSculpt()
    {
        if ( m_kSculptor == null )
        {
            return;
        }

        if ( m_kVolumeShaderEffect.GetVolumeTargetA().GetImage().GetData() != null )
        {
            m_kSculptor.setTextureImageDataA( m_kVolumeShaderEffect.GetVolumeTargetA().GetImage().GetData() );
        }
        else
        {
            m_kSculptor.setTextureImageFloatDataA( m_kVolumeShaderEffect.GetVolumeTargetA().GetImage().GetFloatData() );
        }
        if ( m_kImageB != null )
        {
            if ( m_kVolumeShaderEffect.GetVolumeTargetB().GetImage().GetData() != null )
            {
                m_kSculptor.setTextureImageDataB( m_kVolumeShaderEffect.GetVolumeTargetB().GetImage().GetData() );
            }
            else
            {
                m_kSculptor.setTextureImageFloatDataB( m_kVolumeShaderEffect.GetVolumeTargetB().GetImage().GetFloatData() );
            }
        }
        m_kSculptor.undoSculpt();
        m_kVolumeShaderEffect.ReleaseVolume();
    }

    /**
     * Apply the sculpt region to the volume.
     */
    public void applySculpt()
    {
        if ( m_kSculptor == null )
        {
            return;
        }

        float[] afData = new float[16];
        m_pkRenderer.SetConstantWVPMatrix (0, afData);
        m_kSculptor.setWVPMatrix(new Matrix4f(afData, true));
        if ( m_kVolumeShaderEffect.GetVolumeTargetA().GetImage().GetData() != null )
        {
            m_kSculptor.setTextureImageDataA( m_kVolumeShaderEffect.GetVolumeTargetA().GetImage().GetData() );
        }
        else
        {
            m_kSculptor.setTextureImageFloatDataA( m_kVolumeShaderEffect.GetVolumeTargetA().GetImage().GetFloatData() );
        }
        if ( m_kImageB != null )
        {
            if ( m_kVolumeShaderEffect.GetVolumeTargetB().GetImage().GetData() != null )
            {
                m_kSculptor.setTextureImageDataB( m_kVolumeShaderEffect.GetVolumeTargetB().GetImage().GetData() );
            }
            else
            {
                m_kSculptor.setTextureImageFloatDataB( m_kVolumeShaderEffect.GetVolumeTargetB().GetImage().GetFloatData() );
            }
        }
        if ( m_kSculptor.applySculpt() )
        {
            m_kVolumeShaderEffect.ReleaseVolume();
            m_kSculptor.clearSculpt();
        }
    }

    /**
     * Save the sculpted volume.
     * @param options, file writing options
     * @param filtertype, 
     * @return boolean, true on sucess.
     */
    public boolean save(FileWriteOptions options, int filterType)
    {
        if ( m_kSculptor == null )
        {
            return false;
        }
        return m_kSculptor.save(options, filterType);
    }

    /**
     * Sets the sculpt drawing shape.
     * @param shape, (0 = free-hand, 1 = rectangular)
     */
    public void setDrawingShape(int shape)
    {
        if ( m_kSculptor == null )
        {
            return;
        }
        m_kSculptor.setDrawingShape(shape);
    }

    /**
     * Causes the VolumeShader to update the copy of the ModelImage on the
     * GPU.
     * @param kImage, the new image.
     */
    public void updateData( ModelImage kImage )
    {
        m_kImageA = kImage;
        if ( m_kSculptor != null )
        {
            m_kSculptor.setImage(m_kImageA, m_kImageB);
        }
        if ( m_kVolumeShaderEffect != null )
        {
            m_kVolumeShaderEffect.UpdateData(kImage, 0);
        }
    }

    /**
     * Initializes the objects in the scene other than the ray-traced
     * volume. Bounding cube, clip planes, orientation cube, etc.
     * @param iXBound image x-extent.
     * @param iYBound image y-extent.
     * @param iZBound image z-extent.
     */
    private void InitDisplay(int iXBound, int iYBound, int iZBound)
    {
        m_akPolyline = new Polyline[6];
        m_akBoundingBox = new Polyline[6];
        m_akOrientationCube = new TriMesh[6];
        IndexBuffer kIndexBuffer = new IndexBuffer(6);
        int[] aiIndexData = kIndexBuffer.GetData();
        aiIndexData[0] = 0;
        aiIndexData[1] = 1;
        aiIndexData[2] = 2;
        aiIndexData[3] = 0;
        aiIndexData[4] = 2;
        aiIndexData[5] = 3;

        InitCubicTextures();

        m_kCubeTranslate = new Vector3f( -1.5f, 1f, 1.5f );
        Vector3f kCubeScale = new Vector3f( .5f, .5f, .5f );

        float fMaxX = (float) (iXBound - 1) * m_kImageA.getFileInfo(0).getResolutions()[0];
        float fMaxY = (float) (iYBound - 1) * m_kImageA.getFileInfo(0).getResolutions()[1];
        float fMaxZ = (float) (iZBound - 1) * m_kImageA.getFileInfo(0).getResolutions()[2];

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

        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);
        kAttributes.SetTChannels(0,2);

        VertexBuffer[] akOutlineSquare = new VertexBuffer[6];
        for ( int i = 0; i < 6; i++ )
        {
            akOutlineSquare[i] = new VertexBuffer(kAttributes, 4 );
            for ( int j = 0; j < 4; j++ )
            {
                akOutlineSquare[i].SetColor3( 0, j, 1, 0, 0 ) ;
            }

            akOutlineSquare[i].SetTCoord2( 0, 0, 1, 1 ) ;
            akOutlineSquare[i].SetTCoord2( 0, 1, 0, 1 ) ;
            akOutlineSquare[i].SetTCoord2( 0, 2, 0, 0 ) ;
            akOutlineSquare[i].SetTCoord2( 0, 3, 1, 0 ) ;
        }
        // neg x clipping:
        akOutlineSquare[0].SetPosition3( 0, 0, 0, 0 ) ;
        akOutlineSquare[0].SetPosition3( 1, 0, 0, m_fZ ) ;
        akOutlineSquare[0].SetPosition3( 2, 0, m_fY, m_fZ ) ;
        akOutlineSquare[0].SetPosition3( 3, 0, m_fY, 0 ) ;

        // pos x clipping:
        akOutlineSquare[1].SetPosition3( 0, m_fX, 0, m_fZ ) ;
        akOutlineSquare[1].SetPosition3( 1, m_fX, 0, 0 ) ;
        akOutlineSquare[1].SetPosition3( 2, m_fX, m_fY, 0 ) ;
        akOutlineSquare[1].SetPosition3( 3, m_fX, m_fY, m_fZ ) ;

        // neg y clipping:
        akOutlineSquare[2].SetPosition3( 0, m_fX, 0, m_fZ ) ;
        akOutlineSquare[2].SetPosition3( 1, 0, 0, m_fZ ) ;
        akOutlineSquare[2].SetPosition3( 2, 0, 0, 0 ) ;
        akOutlineSquare[2].SetPosition3( 3, m_fX, 0, 0 ) ;
        // pos y clipping:
        akOutlineSquare[3].SetPosition3( 0, m_fX, m_fY, 0 ) ;
        akOutlineSquare[3].SetPosition3( 1, 0, m_fY, 0 ) ;
        akOutlineSquare[3].SetPosition3( 2, 0, m_fY, m_fZ ) ;
        akOutlineSquare[3].SetPosition3( 3, m_fX, m_fY, m_fZ ) ;

        // neg z clipping:
        akOutlineSquare[4].SetPosition3( 0, m_fX, 0, 0 ) ;
        akOutlineSquare[4].SetPosition3( 1, 0, 0, 0 ) ;
        akOutlineSquare[4].SetPosition3( 2, 0, m_fY, 0 ) ;
        akOutlineSquare[4].SetPosition3( 3, m_fX, m_fY, 0 ) ;

        // pos z clipping:
        akOutlineSquare[5].SetPosition3( 0, 0, 0, m_fZ ) ;
        akOutlineSquare[5].SetPosition3( 1, m_fX, 0, m_fZ ) ;
        akOutlineSquare[5].SetPosition3( 2, m_fX, m_fY, m_fZ ) ;
        akOutlineSquare[5].SetPosition3( 3, 0, m_fY, m_fZ ) ;

        Vector3f kHalf = new Vector3f( m_fX/2.0f, m_fY/2.0f, m_fZ/2.0f );
        Vector3f kPos = new Vector3f();

        for ( int i = 0; i < 6; i++ )
        {
            m_akPolyline[i] = new Polyline( new VertexBuffer(akOutlineSquare[i]), true, true );
            m_akPolyline[i].AttachEffect( m_spkVertexColor3Shader );
            m_akPolyline[i].Local.SetTranslate(m_kTranslate);

            m_akBoundingBox[i] = new Polyline( new VertexBuffer(akOutlineSquare[i]), true, true );
            m_akBoundingBox[i].AttachEffect( m_spkVertexColor3Shader );
            m_akBoundingBox[i].Local.SetTranslate(m_kTranslate);

            for ( int j = 0; j < 4; j++ )
            {
                akOutlineSquare[i].GetPosition3( j, kPos );
                kPos.subEquals(kHalf);
                akOutlineSquare[i].SetPosition3( j, kPos );
            }
            m_akOrientationCube[i] = new TriMesh( new VertexBuffer(akOutlineSquare[i]), kIndexBuffer );
            m_akOrientationCube[i].AttachEffect( new TextureEffect( m_aakAxisFiles[i] ) );
            m_akOrientationCube[i].Local.SetTranslate(m_kCubeTranslate);
            m_akOrientationCube[i].Local.SetScale(kCubeScale);
            m_akOrientationCube[i].UpdateGS();
            m_akOrientationCube[i].UpdateRS();
            m_pkRenderer.LoadResources(m_akOrientationCube[i]);
        }
        kPos = null;

        VertexBuffer kOutlineSquare = new VertexBuffer( kAttributes, 4);
        // arbitrary clipping:
        for ( int i = 0; i < 4; i++ )
        {
            kOutlineSquare.SetColor3( 0, i, 1, 0, 0 ) ;
        }
        kOutlineSquare.SetPosition3( 0, 0, 0, 0 ) ;
        kOutlineSquare.SetPosition3( 1, 0, 0, m_fZ ) ;
        kOutlineSquare.SetPosition3( 2, 0, m_fY, m_fZ ) ;
        kOutlineSquare.SetPosition3( 3, 0, m_fY, 0 ) ;
        m_kClipArb = new Polyline( new VertexBuffer(kOutlineSquare), true, true );
        m_kClipArb.AttachEffect( m_spkVertexColor3Shader );
        m_kClipArb.Local.SetTranslate(m_kTranslate);
        m_kArbRotate.AttachChild( m_kClipArb );

        // eye clipping:
        // set up camera
        m_spkEyeCamera = new Camera();
        m_spkEyeCamera.SetFrustum(-0.55f,0.55f,-0.4125f,0.4125f,1.0f,1000.0f);
        Vector3f kCDir = new Vector3f(0.0f,0.0f,1.0f);
        Vector3f kCUp = new Vector3f(0.0f,-1.0f,0.0f);
        Vector3f kCRight = new Vector3f();
        kCDir.Cross(kCUp, kCRight);
        Vector3f kCLoc = new Vector3f(kCDir);
        kCLoc.scaleEquals(-4.0f);
        m_spkEyeCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);

        for ( int i = 0; i < 4; i++ )
        {
            kOutlineSquare.SetColor3( 0, i, 1, 0, 0 ) ;
        }
        kOutlineSquare.SetPosition3( 0, -.2f, -.2f, m_fZ ) ;
        kOutlineSquare.SetPosition3( 1, m_fX +.2f, -.2f, m_fZ ) ;
        kOutlineSquare.SetPosition3( 2, m_fX +.2f, m_fY +.2f, m_fZ ) ;
        kOutlineSquare.SetPosition3( 3, -.2f, m_fY +.2f, m_fZ ) ;
        m_kClipEye = new Polyline( new VertexBuffer(kOutlineSquare), true, true );
        m_kClipEye.Local.SetTranslate(m_kTranslate);
        m_kClipEye.AttachEffect( m_spkVertexColor3Shader );
        m_kClipEye.UpdateGS();
        m_kClipEye.UpdateRS();
        m_pkRenderer.LoadResources(m_kClipEye);

        for ( int i = 0; i < 4; i++ )
        {
            kOutlineSquare.SetColor3( 0, i, 1, 0, 0 ) ;
        }
        kOutlineSquare.SetPosition3( 0, -.2f, -.2f, 1.0f ) ;
        kOutlineSquare.SetPosition3( 1, m_fX +.2f, -.2f, 1.0f ) ;
        kOutlineSquare.SetPosition3( 2, m_fX +.2f, m_fY +.2f, 1.0f ) ;
        kOutlineSquare.SetPosition3( 3, -.2f, m_fY +.2f, 1.0f ) ;
        m_kClipEyeInv = new Polyline( new VertexBuffer(kOutlineSquare), true, true );
        m_kClipEyeInv.Local.SetTranslate(m_kTranslate);
        m_kClipEyeInv.AttachEffect( m_spkVertexColor3Shader );
        m_kClipEyeInv.UpdateGS();
        m_kClipEyeInv.UpdateRS();
        m_pkRenderer.LoadResources(m_kClipEyeInv);

        m_kVolumeShaderEffect.InitClip(new float[] { 0, 1, 0, 1, 0, 1 });
    }

    /**
     * Called from JPanelClip. Sets the axis-aligned clip plane.
     * @param iWhich, the clip plane to set.
     * @param fValue, the new position of the clip plane (the same value as
     * the slider in JPanelClip).
     */
    public void setClipPlane( int iWhich, float fValue )
    {
        if ( iWhich < 2 )
        {
            fValue /= (m_kImageA.getExtents()[0] -1);
        }
        else if ( iWhich < 4 )
        {
            fValue /= (m_kImageA.getExtents()[1]-1);
        }
        else
        {
            fValue /= (m_kImageA.getExtents()[2] -1);
        }
        for ( int i = 0; i < 4; i++ )
        {
            if ( iWhich < 2 )
            {
                m_akPolyline[iWhich].VBuffer.SetPosition3(i, 
                        fValue,
                        m_akPolyline[iWhich].VBuffer.GetPosition3fY(i),     
                        m_akPolyline[iWhich].VBuffer.GetPosition3fZ(i));
            }
            else if ( iWhich < 4 )
            {
                m_akPolyline[iWhich].VBuffer.SetPosition3(i, 
                        m_akPolyline[iWhich].VBuffer.GetPosition3fX(i),     
                        fValue,
                        m_akPolyline[iWhich].VBuffer.GetPosition3fZ(i));
            }
            else
            {
                m_akPolyline[iWhich].VBuffer.SetPosition3(i, 
                        m_akPolyline[iWhich].VBuffer.GetPosition3fX(i),     
                        m_akPolyline[iWhich].VBuffer.GetPosition3fY(i),
                        fValue);
            }
        }
        m_akPolyline[iWhich].VBuffer.Release();

        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();

        float[] data = new float[4];
        data[0] = fValue;
        m_kVolumeShaderEffect.SetClip(iWhich, data);
    }

    /**
     * Called from JPanelClip. Sets the axis-aligned clip plane display on/off and color.
     * @param iWhich, the clip plane to set.
     * @param bDisplay on/off.
     * @param kColor, the clipping plane color.
     */
    public void displayClipPlane( int iWhich, boolean bDisplay, ColorRGB kColor )
    {
        setClipPlaneColor( iWhich, kColor );
        displayClipPlane( iWhich, bDisplay );
    }

    /**
     * Called from JPanelClip. Sets the axis-aligned clip plane display on/off.
     * @param iWhich, the clip plane to set.
     * @param bDisplay on/off.
     */
    public void displayClipPlane( int iWhich, boolean bDisplay )
    {
        if ( bDisplay != m_abDisplayPolyline[iWhich] )
        {
            m_abDisplayPolyline[iWhich] = bDisplay;
            if ( bDisplay )
            {
                m_spkScene.AttachChild(m_akPolyline[iWhich]);
            }
            else
            {
                m_spkScene.DetachChild(m_akPolyline[iWhich]);
            }
        }
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();
    }

    /**
     * Called from JPanelDisplay. Sets the bounding box display on/off.
     * @param bDisplay on/off.
     */
    public void DisplayBoundingBox( boolean bDisplay )
    {
        if ( bDisplay != m_bDisplayBoundingBox )
        {
            m_bDisplayBoundingBox = bDisplay;
            if ( bDisplay )
            {
                for ( int i = 0; i < 6; i++ )
                {
                    m_spkScene.AttachChild(m_akBoundingBox[i]);
                }
            }
            else
            {
                for ( int i = 0; i < 6; i++ )
                {
                    m_spkScene.DetachChild(m_akBoundingBox[i]);
                }
            }
        }
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();
    }

    /**
     * Called from JPanelDisplay. Sets the orientation cube display on/off.
     * @param bDisplay on/off.
     */
    public void DisplayOrientationCube( boolean bDisplay )
    {
        m_bDisplayOrientationCube = bDisplay;
    }

    /**
     * Called from JPanelDisplay. Sets the bounding box color.
     * @param kColor bounding box color.
     */
    public void SetBoundingBoxColor( ColorRGB kColor )
    {
        for ( int i = 0; i < 6; i++ )
        {
            for ( int j = 0; j < 4; j++ )
            {
                m_akBoundingBox[i].VBuffer.SetColor3( 0, j, kColor );
            }
            m_akBoundingBox[i].VBuffer.Release();
        }
    }

    /**
     * Enables the axis-aligned clipping planes.
     * @param iWhich, the clip plane to enable.
     * @param bEnable clipping enabled
     * @param bDisplay on/off.
     */
    public void enableClipPlane( int iWhich, boolean bEnable, boolean bDisplay )
    {
        displayClipPlane( iWhich, bDisplay );

        float fValue = 0;
        if ( bEnable )
        {
            if ( iWhich < 2 )
            {
                fValue = m_akPolyline[iWhich].VBuffer.GetPosition3fX( 0 );
            }
            else if ( iWhich < 4 )
            {
                fValue = m_akPolyline[iWhich].VBuffer.GetPosition3fY( 0 );
            }
            else
            {
                fValue = m_akPolyline[iWhich].VBuffer.GetPosition3fZ( 0 );
            }
        }
        else
        { 
            if ( iWhich%2 == 0 )
                fValue = 0;
            else
                fValue = 1;
        }

        float[] data = new float[4];
        data[0] = fValue;
        m_kVolumeShaderEffect.SetClip(iWhich,data);
    }

    /**
     * Enables the eye clip plane.
     * @param bEnable clipping enabled
     * @param bDisplay on/off.
     * @param kColor, the eye clip plane color.
     */
    public void enableEyeClipPlane( boolean bEnable, boolean bDisplay, ColorRGB kColor )
    {
        setEyeColor(kColor);
        m_bDisplayClipEye = bDisplay;
        if ( !bEnable )
        {
            setEyeClipPlane(0, bDisplay);
            setEyeInvClipPlane(m_kImageA.getExtents()[2] - 1, bDisplay);
        }
    }

    /**
     * Enables the inverse-eye clip plane.
     * @param bEnable clipping enabled
     * @param bDisplay on/off.
     * @param kColor, the inverse-eye clip plane color.
     */
    public void enableEyeInvClipPlane( boolean bEnable, boolean bDisplay, ColorRGB kColor )
    {
        setEyeInvColor(kColor);
        m_bDisplayClipEyeInv = bDisplay;
        if ( !bEnable )
        {
            setEyeInvClipPlane(m_kImageA.getExtents()[2] - 1, bDisplay);
            setEyeClipPlane(0, bDisplay);
        }
    }

    /**
     * Sets the eye clip plane position.
     * @param f4 clip position (same value as sSlice in JPanelClip)
     * @param bDisplay on/off.
     */
    public void setEyeClipPlane( float f4, boolean bDisplay )
    {
        f4 /= (m_kImageA.getExtents()[2] -1);
        Vector4f kEyeClipV = new Vector4f(0,0,1,f4);
        float[] afEquation = new float[4];
        afEquation[0] = kEyeClipV.X();
        afEquation[1] = kEyeClipV.Y();
        afEquation[2] = kEyeClipV.Z();;
        afEquation[3] = kEyeClipV.W();
        float fZ = kEyeClipV.W() * m_fZ;

        m_kClipEye.VBuffer.SetPosition3( 0, -.2f, -.2f, fZ ) ;
        m_kClipEye.VBuffer.SetPosition3( 1, m_fX +.2f, -.2f, fZ ) ;
        m_kClipEye.VBuffer.SetPosition3( 2, m_fX +.2f, m_fY +.2f, fZ ) ;
        m_kClipEye.VBuffer.SetPosition3( 3, -.2f, m_fY +.2f, fZ ) ;
        m_kClipEye.VBuffer.Release();

        m_kClipEye.UpdateGS();
        m_kClipEye.UpdateRS();

        m_kVolumeShaderEffect.SetClipEye(afEquation);
        m_bDisplayClipEye = bDisplay;
    }

    /**
     * Sets the inverse-eye clip plane position.
     * @param f4 clip position (same value as sSliceInv in JPanelClip)
     * @param bDisplay on/off.
     */
    public void setEyeInvClipPlane( float f4, boolean bDisplay )
    {
        f4 /= (m_kImageA.getExtents()[2] -1);
        Vector4f kEyeClipV = new Vector4f(0,0,1,f4);
        float[] afEquation = new float[4];
        afEquation[0] = kEyeClipV.X();
        afEquation[1] = kEyeClipV.Y();
        afEquation[2] = kEyeClipV.Z();;
        afEquation[3] = kEyeClipV.W();
        float fZ = kEyeClipV.W() * m_fZ;

        m_kClipEyeInv.VBuffer.SetPosition3( 0, -.2f, -.2f, fZ ) ;
        m_kClipEyeInv.VBuffer.SetPosition3( 1, m_fX +.2f, -.2f, fZ ) ;
        m_kClipEyeInv.VBuffer.SetPosition3( 2, m_fX +.2f, m_fY +.2f, fZ ) ;
        m_kClipEyeInv.VBuffer.SetPosition3( 3, -.2f, m_fY +.2f, fZ ) ;
        m_kClipEyeInv.VBuffer.Release();

        m_kClipEyeInv.UpdateGS();
        m_kClipEyeInv.UpdateRS();

        m_kVolumeShaderEffect.SetClipEyeInv(afEquation);
        m_bDisplayClipEyeInv = bDisplay;
    }


    /**
     * Enables the arbitrary clip plane position.
     * @param bEnable clipping enabled
     * @param bDisplay on/off.
     * @param kColor, the arbitrary clip plane color.
     */
    public void enableArbitraryClipPlane( boolean bEnable, boolean bDisplay, ColorRGB kColor )
    {
        setArbColor(kColor);
        displayArbitraryClipPlane(bDisplay);
        if ( !bEnable )
        {
            setArbitraryClipPlane((float)(m_kImageA.getExtents()[0]-1));
        }
    }

    /**
     * Displays the arbitrary clip plane position.
     * @param bDisplay on/off.
     */
    public void displayArbitraryClipPlane( boolean bDisplay )
    {
        if ( m_bDisplayClipArb != bDisplay )
        {
            m_bDisplayClipArb = bDisplay;
            if ( m_bDisplayClipArb )
            {
                m_spkScene.AttachChild(m_kArbRotate); //m_kClipArb);
            }
            else
            {
                m_spkScene.DetachChild(m_kArbRotate); //m_kClipArb);
            }
            m_spkScene.UpdateGS();
            m_spkScene.UpdateRS();
        }
    }


    /**
     * Enables the arbitrary clip plane position.
     * @param f4 clip position (same value as aSlice in JPanelClip)
     */
    public void setArbitraryClipPlane( float f4 )
    {
        f4 /= (m_kImageA.getExtents()[0] -1);
        m_kArbitraryClip = new Vector4f(1,0,0,f4);
        doClip();
    }

    /**
     * Calculates the rotation for the arbitrary clip plane.
     */
    private void doClip() 
    {        
        // update position of the bounding box:
        Vector3f kTranslate = new Vector3f( m_kArbitraryClip.W() * m_fX, 0, 0 );
        kTranslate.addEquals(m_kTranslate);
        m_kClipArb.Local.SetTranslate( kTranslate );

        // Rotate normal vector:
        Matrix3f kClipRotate = m_kArbRotate.Local.GetRotate();
        Vector3f kNormal = new Vector3f( m_kArbitraryClip.X(), m_kArbitraryClip.Y(), m_kArbitraryClip.Z() );
        kNormal = kClipRotate.mult(kNormal);
        kNormal.Normalize();

        float[] afEquation = new float[4];
        afEquation[0] = kNormal.X();
        afEquation[1] = kNormal.Y();
        afEquation[2] = kNormal.Z();
        afEquation[3] = m_kArbitraryClip.W();

        Vector3f kPos = new Vector3f( m_kArbitraryClip.W(), 0, 0 );
        kPos.subEquals( new Vector3f( .5f, .5f, .5f ));
        kPos = kClipRotate.mult(kPos);
        kPos.addEquals( new Vector3f( .5f, .5f, .5f ));
        afEquation[3] = kNormal.Dot(kPos);

        // Update shader with rotated normal and distance:
        m_kVolumeShaderEffect.SetClipArb(afEquation);
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();
    }

    /**
     * Sets the axis-aligned clip plane color.
     * @param iWhich, one of the 6 clip planes
     * @param kColor, the new color.
     */
    public void setClipPlaneColor( int iWhich, ColorRGB kColor )
    {
        kColor.R( (float)(kColor.R()/255.0) );
        kColor.G( (float)(kColor.G()/255.0) );
        kColor.B( (float)(kColor.B()/255.0) );

        for ( int i = 0; i < 4; i++ )
        {
            m_akPolyline[iWhich].VBuffer.SetColor3( 0, i, kColor );
        }
        m_akPolyline[iWhich].VBuffer.Release();
    }

    /**
     * Sets the eye clip plane color.
     * @param kColor, the new color.
     */
    public void setEyeColor( ColorRGB kColor )
    {
        kColor.R( (float)(kColor.R()/255.0) );
        kColor.G( (float)(kColor.G()/255.0) );
        kColor.B( (float)(kColor.B()/255.0) );
        for ( int i = 0; i < 4; i++ )
        {
            m_kClipEye.VBuffer.SetColor3( 0, i, kColor );
        }
        m_kClipEye.VBuffer.Release();
    }

    /**
     * Sets the arbitrary clip plane color.
     * @param kColor, the new color.
     */
    public void setArbColor( ColorRGB kColor )
    {
        kColor.R( (float)(kColor.R()/255.0) );
        kColor.G( (float)(kColor.G()/255.0) );
        kColor.B( (float)(kColor.B()/255.0) );
        for ( int i = 0; i < 4; i++ )
        {
            m_kClipArb.VBuffer.SetColor3( 0, i, kColor );
        }
        m_kClipArb.VBuffer.Release();
    }

    /**
     * Sets the inverse-eye clip plane color.
     * @param kColor, the new color.
     */
    public void setEyeInvColor( ColorRGB kColor )
    {
        kColor.R( (float)(kColor.R()/255.0) );
        kColor.G( (float)(kColor.G()/255.0) );
        kColor.B( (float)(kColor.B()/255.0) );
        for ( int i = 0; i < 4; i++ )
        {
            m_kClipEyeInv.VBuffer.SetColor3( 0, i, kColor );
        }
        m_kClipEyeInv.VBuffer.Release();
    }

    /** Rotates the object with a virtual trackball:
     * @param e, the MouseEvent
     */
    public void mousePressed(MouseEvent e)
    {
        if ( !e.isControlDown() )
        {
            super.mousePressed(e);
        }
        else if ( m_bDisplayClipArb )
        {
            InitializeObjectMotion(m_kArbRotate);
            super.mousePressed(e);
            InitializeObjectMotion(m_spkScene);
        }
    }


    /** Rotates the object with a virtual trackball:
     * @param e, the MouseEvent
     */
    public void mouseDragged(MouseEvent e)
    {
        if ( !getSculptEnabled() )
        {
            if ( !e.isControlDown() )
            {
                super.mouseDragged(e);
            }
            else if ( m_bDisplayClipArb )
            {
                InitializeObjectMotion(m_kArbRotate);
                super.mouseDragged(e);
                InitializeObjectMotion(m_spkScene);
                doClip();
            }
        }
    }

    /**
     * Called when the tri-planar view is rotated, so the matching rotation
     * can be applied to the volume
     * @param data, the rotation matrix.
     */
    public void transformUpdate( float[] data )
    {
        Matrix3f kRotate = new Matrix3f( -1, 0, 0,
                0,  1, 0,
                0,  0, 1 );
        Matrix3f kIn = new Matrix3f(data[0], data[1], data[2],
                data[4], data[5], data[6],
                data[8], data[9], data[10]);
        kIn.multEquals(kRotate);
        kRotate.multEquals(kIn);

        m_spkMotionObject.Local.SetRotateCopy(kRotate);
        kIn = null;
        kRotate = null;
        m_spkScene.UpdateGS();
    }

    /**
     * Called by the ApplicationGUI. Causes the current shader to be reloaded
     * from file, compiled and applied to the proxy-geometry.
     */
    public void reloadShaders()
    {
        m_kVolumeShaderEffect.Reload( m_pkRenderer );
        updateLighting(m_akLights);
    }

    /**
     * Sets the currently visible flag. Used when the GLCanvas is removed from
     * the display panel or frame.
     * @param bVisible, set to false when the GLCanvas container is not displayed.
     */
    public void setVisible( boolean bVisible )
    {
        m_bVisible = bVisible;
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
        if ( m_bInit )
        {
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

                        m_kVolumeShaderEffect.SetLight(kLightType, afType);
                    }
                    else
                    {
                        m_pkRenderer.SetLight( i, new Light() );
                        afType[0] = -1;
                        m_kVolumeShaderEffect.SetLight(kLightType, afType);
                    }
                }
            }
        }
    }

    /**
     * Called from the AdvancedMaterialProperties dialog. Sets the material
     * properties for the VolumeShaderSUR (Surface and Composite Surface
     * volume shaders.)
     * @param kMaterial, new material properties for the surface mode.
     */
    public void SetMaterialState( MaterialState kMaterial )
    {
        m_kMesh.DetachGlobalState(GlobalState.StateType.MATERIAL);
        m_kMaterial = kMaterial;
        m_kMesh.AttachGlobalState(m_kMaterial);
        m_kMesh.UpdateMS(true);
        m_kMesh.UpdateRS();
    }

    /**
     * Called from the JPanelDisplay dialog. Gets the material properties for
     * the VolumeShaderSUR (Surface and Composite Surface volume shaders.)
     * @return material properties for the surface mode.
     */
    public MaterialState GetMaterialState( )
    {
        return m_kMaterial;
    }

    /**
     * Sets the color lut for the ModelImage image.
     * @param RGBT, new ModelRGB
     * @param iImage, the image it applies to.
     */
    public void setRGBT(ModelRGB RGBT, int iImage)
    {
        if ( m_kVolumeShaderEffect == null )
        {
            m_kRGBTa = RGBT;
            return;
        }
        m_kVolumeShaderEffect.SetRGBT( RGBT, iImage );
    }

    /**
     * Display the volume in MIP mode.
     */
    public void MIPMode()
    {
        m_kVolumeShaderEffect.MIPMode(m_pkRenderer);
        ResetShaderParamsWindow();
    }

    /**
     * Display the volume in DDR mode.
     */
    public void DDRMode()
    {
        m_kVolumeShaderEffect.DDRMode(m_pkRenderer);
        ResetShaderParamsWindow();
    }

    /**
     * Display the volume in Composite mode.
     */
    public void CMPMode()
    {
        m_kVolumeShaderEffect.CMPMode(m_pkRenderer);
        ResetShaderParamsWindow();
    }

    /**
     * Display the volume in Composite Surface mode.
     */
    public void SURMode()
    {
        m_kVolumeShaderEffect.SURMode(m_pkRenderer);
        ResetShaderParamsWindow();
    }

    /**
     * Display the volume in Surface mode.
     */
    public void SURFASTMode()
    {
        m_kVolumeShaderEffect.SURFASTMode(m_pkRenderer);
        ResetShaderParamsWindow();
    }

    /**
     * Closes the shader parameters window.
     */
    private void ResetShaderParamsWindow()
    {
        if ( m_kShaderParamsWindow != null )
        {
            m_kShaderParamsWindow.close();
        }
    }

    /**
     * Sets blending between imageA and imageB.
     * @param fValue, the blend value (0-1)
     */
    public void Blend( float fValue )
    {
        m_kVolumeShaderEffect.Blend(fValue);
    }

    /**
     * Sets the raytracing steps size.
     * @param fValue, the steps value (0-450)
     */
    public void StepsSize( float fValue )
    {
        m_kVolumeShaderEffect.setSteps(fValue);
    }

    /**
     * Launches the ApplicationGUI window displaying the currently-loaded
     * shader parameters.
     */
    public void displayShaderParameters()
    {
        if ( m_kShaderParamsWindow == null )
        {
            m_kShaderParamsWindow = new ApplicationGUI();
            m_kShaderParamsWindow.setParent(this);
        }
        m_kShaderParamsWindow.close();
        m_kShaderParamsWindow.AddUserVariables(m_kVolumeShaderEffect.GetPProgram());
        m_kShaderParamsWindow.Display();
        m_kShaderParamsWindow.setParent(this);
    }

    /**
     * Sets the background color.
     * @param kColor, new background color.
     */
    public void SetBackgroundColor( ColorRGBA kColor )
    {
        m_kBackgroundColor = kColor;
        m_kVolumeShaderEffect.SetBackgroundColor( kColor );
    }


    /**
     * Create the rotation control cubic box. A cube representing the image
     * orientation, with labels painted on the cube faces showing which axis
     * corresponds to which axis in patient coordinates.
     */
    private void InitCubicTextures() {

        /* Read the axis strings from the FileInfo data structure: */
        String[] akAxisLabels = new String[3];
        int[] axisOrientation = MipavCoordinateSystems.getAxisOrientation(m_kImageA); 
        if ( axisOrientation != null )
        {
            for ( int i = 0; i < 3; i++ )
            {
                akAxisLabels[i] = FileInfoBase.getAxisOrientationStr( axisOrientation[i] ).toLowerCase();
                // System.out.println(akAxisLabels[i]);
                /* The file name correspond to the axis strings, read the file
                 * names from the axis strings: */
                m_aakAxisFiles[i*2 +0] = new String( String.valueOf( akAxisLabels[i].charAt(0) ) );
                m_aakAxisFiles[i*2 +1] = new String( String.valueOf( akAxisLabels[i].charAt( akAxisLabels[i].lastIndexOf( " " ) + 1 ) ) );
            }
        }
    }

    /**
     * Enables/Disables Gradient Magnitude filter.
     * @param bShow, gradient magnitude filter on/off
     */
    public void SetGradientMagnitude(boolean bShow)
    {
        if ( m_kVolumeShaderEffect != null )
        {
            m_kVolumeShaderEffect.SetGradientMagnitude(bShow);
        }
    }

    /**
     * Enables/Disables self-shadowing in the Surface mode.
     * @param bShadow, shadow on/off.
     */
    public void SelfShadow(boolean bShadow)
    {
        if ( m_kVolumeShaderEffect != null )
        {
            m_kVolumeShaderEffect.SelfShadow(bShadow);
        }
    }

    /** Add a polyline to the display. Used to display fiber tract bundles.
     * @param kLine, new polyline to display.
     */
    public void addPolyline( Polyline kLine, int iGroup )
    {
        if ( m_kTracts == null )
        {
            m_kTracts = new HashMap<Integer,Node>();
            m_kEllipsoids = new HashMap<Integer,Vector<int[]>>();
            m_kShaders = new HashMap<Integer,ShaderEffect>();
            m_kEllipseConstantColor = new HashMap<Integer,ColorRGB>();
            m_bDisplayTract = true;
        }


        int[] aiEllipsoids = new int[kLine.VBuffer.GetVertexQuantity()];
        for ( int i = 0; i < kLine.VBuffer.GetVertexQuantity(); i++ )
        {
            int iX = (int)((kLine.VBuffer.GetPosition3fX(i) +.5f) * m_iDimX);
            int iY = (int)((kLine.VBuffer.GetPosition3fY(i) +.5f) * m_iDimY);
            int iZ = (int)((kLine.VBuffer.GetPosition3fZ(i) +.5f) * m_iDimZ);
            int iIndex = iZ * m_iDimY * m_iDimX + iY * m_iDimX + iX;

            if ( m_kEigenVectors != null )
            {
                if (  m_kEigenVectors.get( new Integer(iIndex) ) != null )
                {
                    aiEllipsoids[i] = iIndex;
                }
                else
                {
                    aiEllipsoids[i] = -1;
                }
            }
        }

        kLine.Local.SetScale( new Vector3f( m_fX, m_fY, m_fZ ) );

        Node kTractNode = null;
        Integer iIGroup = new Integer(iGroup);
        if ( m_kTracts.containsKey( iIGroup ) )
        {
            kTractNode = m_kTracts.get(iIGroup);
            kTractNode.AttachChild(kLine);
            kTractNode.UpdateGS();
            kTractNode.UpdateRS();

            Vector<int[]> kEllipseVector = m_kEllipsoids.get(iIGroup);
            kEllipseVector.add(aiEllipsoids);
        }
        if ( kTractNode == null )
        {
            kTractNode = new Node();
            kTractNode.AttachChild(kLine);
            kTractNode.UpdateGS();
            kTractNode.UpdateRS();
            m_kTracts.put( new Integer(iIGroup), kTractNode );

            Vector<int[]> kEllipseVector = new Vector<int[]>();
            kEllipseVector.add(aiEllipsoids);
            m_kEllipsoids.put( new Integer(iIGroup), kEllipseVector );

            String kShaderName = new String( "ConstantColor" );
            VertexColor3Effect kPolylineShader = new VertexColor3Effect( kShaderName, true );
            m_kShaders.put( new Integer(iIGroup), kPolylineShader );
        }
    }

    /** Remove all polylines from the display. */
    public void removeAllPolylines(  )
    {
        if ( m_kTracts == null )
        {
            return;
        }

        Iterator kIterator = m_kTracts.keySet().iterator();
        while ( kIterator.hasNext() )
        {
            Integer iKey = (Integer)kIterator.next();
            Node kTractNode = m_kTracts.get(iKey);
            for ( int i = 0; i < kTractNode.GetQuantity(); i++ )
            {
                Polyline kTract = (Polyline)kTractNode.DetachChildAt(i);
                if ( kTract != null )
                {
                    kTract.DetachAllEffects();
                    kTract.dispose();
                }
            }
            kTractNode.UpdateGS();
            kTractNode.UpdateRS();
            kTractNode.dispose();
            kTractNode = null;
        }
        m_kTracts.clear();
        m_kTracts = null;

        kIterator = m_kShaders.keySet().iterator();
        while ( kIterator.hasNext() )
        {
            Integer iKey = (Integer)kIterator.next();
            ShaderEffect kShader = m_kShaders.get(iKey);
            kShader.dispose();
            kShader = null;
        }
        m_kShaders.clear();
        m_kShaders = null;

        kIterator = m_kEllipsoids.keySet().iterator();
        while ( kIterator.hasNext() )
        {
            Integer iKey = (Integer)kIterator.next();
            Vector<int[]> kEllipseVector = m_kEllipsoids.get(iKey);
            if ( kEllipseVector != null )
            {
                kEllipseVector.clear();
            }
        }
        m_kEllipsoids.clear();
        m_kEllipsoids = null;
        

        m_bDisplayTract = false;
    }

    /** 
     */
    public void removePolyline( int iGroup )
    {
        Integer kGroup = new Integer(iGroup);
        Node kTractNode = m_kTracts.remove(kGroup);
        for ( int i = 0; i < kTractNode.GetQuantity(); i++ )
        {
            Polyline kTract = (Polyline)kTractNode.DetachChildAt(i);
            if ( kTract != null )
            {
                kTract.DetachAllEffects();
                kTract.dispose();
            }
        }
        kTractNode.UpdateGS();
        kTractNode.UpdateRS();
        kTractNode.dispose();
        kTractNode = null;

        Vector<int[]> kEllipseVector = m_kEllipsoids.remove(kGroup);
        if ( kEllipseVector != null )
        {
            kEllipseVector.clear();
        }

        ShaderEffect kShader = m_kShaders.remove(kGroup);
        if ( kShader != null )
        {
            kShader.dispose();
        }
    }

    /** 
     */
    public void setPolylineColor( int iGroup, ColorRGB kColor )
    {
        Integer kKey = new Integer(iGroup);
        ShaderEffect kShader = m_kShaders.get(kKey);
        if ( kShader == null )
        {
            return;
        }
        Program pkProgram = kShader.GetVProgram(0);
        if ( pkProgram == null )
        {
            return;
        }
        if ( kColor == null )
        {
            if ( pkProgram.GetUC("UseConstantColor") != null )
            {
                pkProgram.GetUC("UseConstantColor").SetDataSource(new float[] {0,0,0,0});
            }

            m_kEllipseConstantColor.remove( kKey );
            m_kEllipseConstantColor.put( kKey, null );
        }
        else
        {
            if ( pkProgram.GetUC("ConstantColor") != null )
            {
                pkProgram.GetUC("ConstantColor").SetDataSource(new float[] { kColor.R(), kColor.G(), kColor.B(), 1f } );
            }
            if ( pkProgram.GetUC("UseConstantColor") != null )
            {
                pkProgram.GetUC("UseConstantColor").SetDataSource(new float[] {1,0,0,0});
            }

            m_kEllipseConstantColor.remove( kKey );
            m_kEllipseConstantColor.put( kKey, kColor );
        }
    }

    public void setEllipseColor( ShaderEffect kShader, ColorRGB kColor )
    {
        if ( kShader == null )
        {
            return;
        }
        Program pkProgram = kShader.GetVProgram(0);
        if ( pkProgram == null )
        {
            return;
        }
        if ( kColor == null )
        {
            if ( pkProgram.GetUC("UseConstantColor") != null )
            {
                pkProgram.GetUC("UseConstantColor").SetDataSource(new float[] {0,0,0,0});
            }
        }
        else
        {
            if ( pkProgram.GetUC("ConstantColor") != null )
            {
                pkProgram.GetUC("ConstantColor").SetDataSource(new float[] { kColor.R(), kColor.G(), kColor.B(), 1f } );
            }
            if ( pkProgram.GetUC("UseConstantColor") != null )
            {
                pkProgram.GetUC("UseConstantColor").SetDataSource(new float[] {1,0,0,0});
            }
        }
    }


    public void setDTIImage( ModelImage kDTIImage )
    {
        float[] newRes = m_kImageA.getResolutions(0);
        int[] volExtents = m_kImageA.getExtents();
        AlgorithmTransform transformFunct = new AlgorithmTransform(kDTIImage, new TransMatrix(4), AlgorithmTransform.NEAREST_NEIGHBOR,
                                                                   newRes[0], newRes[1], newRes[2],
                                                                   volExtents[0], volExtents[1], volExtents[2],
                                                                   false, true, false);
        transformFunct.setRunningInSeparateThread(false);
        transformFunct.run();

        if (transformFunct.isCompleted() == false) {
            transformFunct.finalize();
            transformFunct = null;
        }
        kDTIImage.disposeLocal();
        kDTIImage = null;

        kDTIImage = transformFunct.getTransformedImage();
        kDTIImage.calcMinMax();

        if (transformFunct != null) {
            transformFunct.disposeLocal();
        }
        transformFunct = null;

        ViewJProgressBar kProgressBar = new ViewJProgressBar("Calculating ellipse transforms", "", 0, m_iLen, true);
        
        m_kEigenVectors =
            new HashMap<Integer,Transformation>();
        Matrix3f kMatrix = new Matrix3f();;
        float[] afTensorData = new float[6];
        Matrix3f kEigenValues = new Matrix3f();
        float fLambda1;
        float fLambda2;
        float fLambda3;
        Vector3f kV1 = new Vector3f();
        Vector3f kV2 = new Vector3f();
        Vector3f kV3 = new Vector3f();

        for ( int i = 0; i < m_iLen; i++ )
        {
            boolean bAllZero = true;


            for ( int j = 0; j < 6; j++ )
            {
                afTensorData[j] = kDTIImage.getFloat(i + j*m_iLen);
                if ( afTensorData[j] != 0 )
                {
                    bAllZero = false;
                }
            }
            if ( !bAllZero )
            {
                kMatrix.SetData( afTensorData[0], afTensorData[3], afTensorData[4],
                        afTensorData[3], afTensorData[1], afTensorData[5], 
                        afTensorData[4], afTensorData[5], afTensorData[2] );

                Matrix3f.EigenDecomposition( kMatrix, kEigenValues );
                fLambda1 = kEigenValues.GetData(2,2);
                fLambda2 = kEigenValues.GetData(1,1);
                fLambda3 = kEigenValues.GetData(0,0);
                kMatrix.GetColumn(2,kV1);
                kMatrix.GetColumn(1,kV2);
                kMatrix.GetColumn(0,kV3);

                kV1.Normalize();
                kV2.Normalize();
                kV3.Normalize();

                kMatrix.SetColumn(0,kV1);
                kMatrix.SetColumn(1,kV2);
                kMatrix.SetColumn(2,kV3);

                if ( (fLambda1 == fLambda2) && (fLambda1 == fLambda3) )
                {}
                else
                {
                    Transformation kTransform = new Transformation();
                    kTransform.SetMatrix(new Matrix3f(kMatrix));
                    kTransform.SetScale( new Vector3f( fLambda1, fLambda2, fLambda3 ) );
                    m_kEigenVectors.put( new Integer(i), kTransform );

                    if ( fLambda1 > m_fScaleMax )
                    {
                        m_fScaleMax = fLambda1;
                    }
                    if ( fLambda2 > m_fScaleMax )
                    {
                        m_fScaleMax = fLambda2;
                    }
                    if ( fLambda3 > m_fScaleMax )
                    {
                        m_fScaleMax = fLambda3;
                    }
                }
            }
            if ( (i%(m_iDimX*m_iDimY)) == 0 )
            {
                kProgressBar.updateValue(i+1);
            }
        }
        kProgressBar.dispose();
        m_fScaleMax = 1.0f/m_fScaleMax;

        kDTIImage.disposeLocal();
        kDTIImage = null;

        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);

        StandardMesh kSM = new StandardMesh(kAttr);
        m_kSphere = kSM.Sphere(10,10,1f);

        m_kAllEllipsoidsShader = new MaterialEffect( );
        m_kEllipseMaterial = new MaterialState();
        m_kEllipseMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
        m_kEllipseMaterial.Ambient = new ColorRGB(0.24725f,0.2245f,0.0645f);
        m_kEllipseMaterial.Diffuse = new ColorRGB(0.34615f,0.3143f,0.0903f);
        m_kEllipseMaterial.Specular = new ColorRGB(0.797357f,0.723991f,0.208006f);
        m_kEllipseMaterial.Shininess = 83.2f;
        m_kColorEllipse = new ColorRGB(ColorRGB.BLACK);
    }

    public void setDisplayEllipsoids( boolean bDisplay )
    {
        m_bDisplayEllipsoids = bDisplay;
    }


    public void setDisplayAllEllipsoids( boolean bDisplay, int iMod, boolean bUseConstantColor, ColorRGB kColor )
    {
        m_bDisplayAllEllipsoids = bDisplay;
        //m_iEllipsoidMod = iMod;

        Program pkProgram = m_kAllEllipsoidsShader.GetVProgram(0);
        if ( pkProgram == null )
        {
            return;
        }
        if ( !bUseConstantColor || (kColor == null) )
        {
            if ( pkProgram.GetUC("UseConstantColor") != null )
            {
                pkProgram.GetUC("UseConstantColor").SetDataSource(new float[] {0,0,0,0});
            }
        }
        else if ( bUseConstantColor && (kColor != null) )
        {
            if ( pkProgram.GetUC("ConstantColor") != null )
            {
                pkProgram.GetUC("ConstantColor").SetDataSource(new float[] { kColor.R(), kColor.G(), kColor.B(), 1f } );
            }
            if ( pkProgram.GetUC("UseConstantColor") != null )
            {
                pkProgram.GetUC("UseConstantColor").SetDataSource(new float[] {1,0,0,0});
            }
        }
    }

    public void setEllipseMod( int iMod )
    {
        m_iEllipsoidMod = iMod;
    }


    private void DisplayAllEllipsoids()
    {
        if ( m_kEigenVectors == null )
        {
            return;
        }
        
        m_spkAlpha.BlendEnabled = true;
        int iCount = 0;
        int iIndex, iX, iY, iZ;
        float fX, fY, fZ;
        Integer kKey;
        float fR, fG, fB;
        Vector3f kScale;
        TriMesh kEllipse;
        Transformation kTransform;
                
        Iterator kIterator = m_kEigenVectors.keySet().iterator();
        while ( kIterator.hasNext() )
        {
            kKey = (Integer)kIterator.next();
            if ( (iCount%m_iEllipsoidMod) == 0 )
            {                           
                iIndex = kKey.intValue();
                iX = iIndex % m_iDimX;
                iIndex -= iX;
                iIndex /= m_iDimX;
                            
                iY = iIndex % m_iDimY;
                iIndex -= iY;
                iIndex /= m_iDimY;
                            
                iZ = iIndex;
                            
                // reset iIndex:
                iIndex = kKey.intValue();
                            
                if ( m_kImageA.isColorImage() )
                {
                    fR = m_kImageA.getFloat( iIndex*4 + 1 )/255.0f;
                    fG = m_kImageA.getFloat( iIndex*4 + 2 )/255.0f;
                    fB = m_kImageA.getFloat( iIndex*4 + 3 )/255.0f;
                    m_kColorEllipse.R(fR);
                    m_kColorEllipse.G(fG);
                    m_kColorEllipse.B(fB);
                }
                else
                {
                    fR = m_kImageA.getFloat( iIndex );
                    m_kColorEllipse.R(fR);
                    m_kColorEllipse.G(fR);
                    m_kColorEllipse.B(fR);
                }

                fX = (float)(iX)/(float)(m_iDimX);
                fY = (float)(iY)/(float)(m_iDimY);
                fZ = (float)(iZ)/(float)(m_iDimZ);


                m_kTScale.MakeIdentity();
                m_kTEllipse.MakeIdentity();
                m_kTRotate.MakeIdentity();

                kTransform = m_kEigenVectors.get(kKey);
            
                kScale = m_kTScale.GetScale();
                kScale.scaleEquals( m_fScaleMax * m_fScale * 2f);
                kScale.multEquals( kTransform.GetScale() );
                m_kTScale.SetScale(kScale);
                            
                m_kTEllipse.SetTranslate( fX - .5f, fY - .5f, fZ - .5f );
                m_kTEllipse.SetMatrixCopy( kTransform.GetMatrix() );

                m_kTRotate.SetRotateCopy(m_spkScene.Local.GetRotate());
                m_kTRotate.SetScale( m_fX, m_fY, m_fZ );

                kEllipse = m_kSphere;
                kEllipse.Local.MakeIdentity();
                kEllipse.Local.Product( m_kTEllipse, m_kTScale );
                kEllipse.Local.Product( m_kTRotate, kEllipse.Local );
                            
                kEllipse.DetachAllEffects();
                            
                m_kEllipseMaterial.Diffuse = m_kColorEllipse;
                kEllipse.AttachGlobalState(m_kEllipseMaterial);
                kEllipse.AttachEffect( m_spkVertexColor3Shader );
                            
                kEllipse.UpdateRS();
                kEllipse.UpdateGS();
                m_pkRenderer.Draw(kEllipse);
            }
            iCount++;
        }
    }


    private void DisplayEllipsoids( )
    {
        if ( m_kEllipsoids == null )
        {
            return;
        }
        
        m_pkRenderer.SetBackgroundColor(m_kBackgroundColor);
        m_pkRenderer.ClearBuffers();

        m_spkAlpha.BlendEnabled = true;

        Integer kKey;
        Vector<int[]> kEllipseVector;
        int[] aiEllipsoids;
        int iIndex;
        ColorRGB kColor;
        int iX,iY,iZ;
        float fX,fY,fZ;
        float fR,fG,fB;
        Vector3f kScale;
        TriMesh kEllipse;
        Transformation kTransform;
        
        Iterator kIterator = m_kEllipsoids.keySet().iterator();
        while ( kIterator.hasNext() )
        {
            kKey = (Integer)kIterator.next();
            kEllipseVector = m_kEllipsoids.get(kKey);
            for ( int i = 0; i < kEllipseVector.size(); i++ )
            {
                aiEllipsoids = kEllipseVector.get(i);
                for ( int j = 0; j < aiEllipsoids.length; j++ )
                {
                    if ( aiEllipsoids[j] != -1 )
                    {
                        iIndex = aiEllipsoids[j];

                        kColor = m_kEllipseConstantColor.get(kKey);
                        if ( kColor != null )
                        {
                            m_kColorEllipse = kColor;
                        }
                        else
                        {
                            if ( m_kImageA.isColorImage() )
                            {
                                fR = m_kImageA.getFloat( iIndex*4 + 1 )/255.0f;
                                fG = m_kImageA.getFloat( iIndex*4 + 2 )/255.0f;
                                fB = m_kImageA.getFloat( iIndex*4 + 3 )/255.0f;
                                m_kColorEllipse.R(fR);
                                m_kColorEllipse.G(fG);
                                m_kColorEllipse.B(fB);
                            }
                            else
                            {
                                fR = m_kImageA.getFloat( iIndex );
                                m_kColorEllipse.R(fR);
                                m_kColorEllipse.G(fR);
                                m_kColorEllipse.B(fR);
                            }
                        }
                        
                        iX = iIndex % m_iDimX;
                        iIndex -= iX;
                        iIndex /= m_iDimX;
                        
                        iY = iIndex % m_iDimY;
                        iIndex -= iY;
                        iIndex /= m_iDimY;
                        
                        iZ = iIndex;
                        // reset iIndex
                        iIndex = aiEllipsoids[j];
                        
                        fX = (float)(iX)/(float)(m_iDimX);
                        fY = (float)(iY)/(float)(m_iDimY);
                        fZ = (float)(iZ)/(float)(m_iDimZ);
                        
                        m_kTScale.MakeIdentity();
                        m_kTEllipse.MakeIdentity();
                        m_kTRotate.MakeIdentity();
                        
                        kTransform = m_kEigenVectors.get(new Integer(iIndex));
            
                        kScale = m_kTScale.GetScale();
                        kScale.scaleEquals( m_fScaleMax * m_fScale * 2f);
                        kScale.multEquals( kTransform.GetScale() );
                        m_kTScale.SetScale(kScale);
                        
                        m_kTEllipse.SetTranslate( fX - .5f, fY - .5f, fZ - .5f );
                        m_kTEllipse.SetMatrixCopy( kTransform.GetMatrix() );
                        
                        m_kTRotate.SetRotateCopy(m_spkScene.Local.GetRotate());
                        m_kTRotate.SetScale( m_fX, m_fY, m_fZ );
                        
                        kEllipse = m_kSphere;
                        kEllipse.Local.MakeIdentity();
                        kEllipse.Local.Product( m_kTEllipse, m_kTScale );
                        kEllipse.Local.Product( m_kTRotate, kEllipse.Local );
                        
                        kEllipse.DetachAllEffects();
                        
                        m_kEllipseMaterial.Diffuse = m_kColorEllipse;
                        kEllipse.AttachGlobalState(m_kEllipseMaterial);
                        kEllipse.AttachEffect( m_kAllEllipsoidsShader );
                        
                        kEllipse.UpdateRS();
                        kEllipse.UpdateGS();
                        m_pkRenderer.Draw(kEllipse);
                    }
                }
            }
        }
    }
    

    
    private void DisplayTract( ShaderEffect kInputShader )
    {
        Iterator kIterator = m_kTracts.keySet().iterator();
        Integer iKey;
        Node kTractNode;
        Polyline kTract;
        ShaderEffect kShader;
        
        m_spkAlpha.BlendEnabled = true;
        while ( kIterator.hasNext() )
        {
            iKey = (Integer)kIterator.next();
            kTractNode = m_kTracts.get(iKey);
            kShader = kInputShader;
            if ( kShader == null )
            {
                kShader = m_kShaders.get(iKey);
            }
            for ( int i = 0; i < kTractNode.GetQuantity(); i++ )
            {
                kTract = (Polyline)kTractNode.GetChild(i);
                kTract.Local.SetRotate(new Matrix3f(m_spkScene.Local.GetRotate()));
                kTract.DetachAllEffects();
                kTract.AttachEffect( kShader );
                kTract.UpdateGS();
                kTract.UpdateRS();
                m_pkRenderer.Draw(kTract);
            }
        }
    }

    /** Scene-graph root node: */
    private Node m_spkScene;
    /** Turns wireframe on/off: */
    private WireframeState m_spkWireframe;
    /** Alpha blending state for blending between geometry and the volume. */
    private AlphaState m_spkAlpha;
    /** Culling: turns backface/frontface culling on/off: */
    private CullState m_spkCull;
    /** Culling out-of-view objects: */
    private Culler m_kCuller = new Culler(0,0,null);
    /** Bounding box polyline object: */
    private Polyline[] m_akBoundingBox;
    /** enables/disables displaying the bounding box: */
    private boolean m_bDisplayBoundingBox = false;
    /** Orientation cube grometry: */
    private TriMesh[] m_akOrientationCube;
    /** enables/disables displaying the orientation cube: */
    private boolean m_bDisplayOrientationCube = false;
    /** Orientation cube texture names: */
    private String[] m_aakAxisFiles = new String[]{ "u", "u", "u", "u", "u", "u"};
    /** Orientation cube translation offset: */
    private Vector3f m_kCubeTranslate = Vector3f.ZERO;
    /** axis-aligned clip plane polylines: */
    private Polyline[] m_akPolyline;
    /** arbitrary clip plane polyline: */
    private Polyline m_kClipArb;
    /** eye clip plane polyline: */
    private Polyline m_kClipEye;
    /** inverse-eye clip plane polyline: */
    private Polyline m_kClipEyeInv;
    /** enables/disables displaying clip planes*/
    private boolean[] m_abDisplayPolyline = new boolean[]{false,false,false,false,false,false};
    /** VolumeShaderEffect applied to proxy-geometry: */
    private VolumeShaderEffect m_kVolumeShaderEffect = null;
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

    /** Screen camera for displaying the screen polygon: */
    private Camera m_spkScreenCamera;
    /** Screen camera for displaying the eye clip planes in screen-coordinates: */
    private Camera m_spkEyeCamera;
    /** Scene polygon displaying the first-pass rendering of the proxy-geometry: */
    private TriMesh m_spkScenePolygon;
    /** GraphicsImage with the first-pass rendering of the proxy-geometry: */
    private GraphicsImage m_spkSceneImage;
    /** Texture for the first-pass rendering of the proxy-geometry: */
    private Texture m_pkSceneTarget;
    /** Off-screen buffer the first-pass rendering is drawn into: */
    private OpenGLFrameBuffer m_pkPBuffer;

    /** Vertex-color shader effect used for the polylines and the first-pass
     * rendering of the proxy-geometry:*/
    private ShaderEffect m_spkVertexColor3Shader;

    /** Animator object, displays scene in rendering loop (similar to GLUTMainLoop() */
    private Animator m_kAnimator;

    /** New sculpting object for WM-based sculpting. */
    private SculptorWm m_kSculptor = null;

    /** Stores the transfer functions: */
    private TransferFunction[] m_akTransfer = new TransferFunction[4];

    /** Set to true when init() is called: */
    private boolean m_bInit = false;
    /** Scene translation, centers the scene: */
    private Vector3f m_kTranslate;
    /** Arbitrary clip plane equation: */
    private Vector4f m_kArbitraryClip;
    /** Enables/Disables displaying the arbitrary clip plane: */
    private boolean m_bDisplayClipArb = false;
    /** Enables/Disables displaying the eye clip plane: */
    private boolean m_bDisplayClipEye = false;
    /** Enables/Disables displaying the inverse-eye clip plane: */
    private boolean m_bDisplayClipEyeInv = false;
    /** Normalized volume extents: */
    private float m_fX, m_fY, m_fZ, m_fMax;
    /** Flag for indicating the that Java Container is visible or not: */
    private boolean m_bVisible = true;
    /** Node for rotating the arbitrary clip plane with the mouse trackball: */
    private Node m_kArbRotate = new Node();
    /** Material properties for Volume Surface (and Composite Surface) mode*/
    private MaterialState m_kMaterial;
    /** Volume proxy-geometry (cube) */
    private TriMesh m_kMesh;
    /** Window with the shader paramter interface: */
    private ApplicationGUI m_kShaderParamsWindow = null;
    /** Lights from JPanelLight */
    private GeneralLight[] m_akLights = null;
    /** Enables/Disables rendering the second pass. When disabled, the
     * back-facing polygons of the proxy-geometry are shown instead of the volume: */
    private boolean m_bDisplaySecond = true;

    /** Set to true when polyline fiber bundles are displayed. */
    private boolean m_bDisplayTract = false;
    /** For modifying the alpha-blending mode during runtime. */
    private int m_iActive = 0;

    /** Hashmap for multiple fiber bundles: */
    private HashMap<Integer,Node>  m_kTracts = null;

    /** Hashmap for multiple fiber bundles: */
    private HashMap<Integer,ShaderEffect>  m_kShaders = null;

    /** Hashmap for multiple fiber bundles: */
    private HashMap<Integer,Vector<int[]>> m_kEllipsoids = null;

    private float  m_fScaleMax = -Float.MAX_VALUE;


    private boolean m_bDisplayEllipsoids = false;
    private boolean m_bDisplayAllEllipsoids = false;
    private int m_iEllipsoidMod = 10;

    private ShaderEffect m_kAllEllipsoidsShader = null;

    private HashMap<Integer,ColorRGB> m_kEllipseConstantColor;

    private MaterialState m_kEllipseMaterial;

    private HashMap<Integer,Transformation>  m_kEigenVectors = null;

    private TriMesh m_kSphere;
    private Transformation m_kTScale = new Transformation();
    private Transformation m_kTEllipse = new Transformation();
    private Transformation m_kTRotate = new Transformation();
    
    private int m_iDimX;
    private int m_iDimY;
    private int m_iDimZ;
    private float m_fScale;
    private int m_iLen;
    private ColorRGB m_kColorEllipse;

}