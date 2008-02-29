package gov.nih.mipav.view.renderer.WildMagic;

import javax.media.opengl.*;
import com.sun.opengl.util.*;
import java.awt.event.*;
import java.util.Vector;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.renderer.volumeview.*;
import gov.nih.mipav.view.WildMagic.LibApplications.OpenGLApplication.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Collision.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.view.WildMagic.LibRenderers.OpenGLRenderer.*;

import gov.nih.mipav.view.renderer.GeneralLight;

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
public class GPUVolumeRender_WM extends JavaApplication3D
implements GLEventListener, KeyListener, MouseMotionListener
{
    /**
     * Constructs a new GPUVolumeRender object.
     * @param kImageA ModelImage A
     * @param kLUTa, LUT for ModelImage A
     * @param kRGBTa, RGB lookup table for ModelImage A
     * @param kImageB ModelImage B
     * @param kLUTb, LUT for ModelImage B
     * @param kRGBTb, RGB lookup table for ModelImage B
     */
    public GPUVolumeRender_WM( VolumeViewer kParent, Animator kAnimator, VolumeImage kVolumeImageA, ModelImage kImageA, ModelLUT kLUTa, ModelRGB kRGBTa,
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
        
        m_kRotate.FromAxisAngle(Vector3f.UNIT_Z, (float)Math.PI/18.0f);
        
        
        String class_path_key = "java.class.path";
        String class_path = System.getProperty(class_path_key);
        //System.out.println(class_path);
                   String jar_filename = null;
        for (String fn : class_path.split(";") ) {
            if (fn.endsWith("InsightToolkit.jar")) {
                jar_filename = fn;
                System.out.println("Found itk jar: " + jar_filename);
                break;
            }
        } 
    }
    
    
    public GPUVolumeRender_WM( )
    {

        super("GPUVolumeRender",0,0,512,512, new ColorRGBA(0.0f,0.0f,0.0f,0.0f));

        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                m_eBuffering, m_eMultisampling,
                m_iWidth, m_iHeight );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );       

      
       
    }
    
    
    public void loadImage( VolumeViewer kParent, Animator kAnimator, VolumeImage kVolumeImageA, ModelImage kImageA, ModelLUT kLUTa, ModelRGB kRGBTa,
            VolumeImage kVolumeImageB, ModelImage kImageB, ModelLUT kLUTb, ModelRGB kRGBTb  )
    {

    	  m_kParent = kParent;
          m_kAnimator = kAnimator;
        m_kVolumeImageA = kVolumeImageA;
        m_kImageA = kImageA;
        m_kLUTa = kLUTa;
        m_kRGBTa = kRGBTa;

        m_kVolumeImageB = kVolumeImageB;
        m_kImageB = kImageB;
        m_kLUTb = kLUTb;
        m_kRGBTb = kRGBTb;
       
        m_kRotate.FromAxisAngle(Vector3f.UNIT_Z, (float)Math.PI/18.0f);
    }
    
    /**
     * memory cleanup.
     */
    public void dispose()
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            m_kDisplayList.get(i).dispose();
        }

        if ( m_spkScene != null )
        {
            m_spkScene.dispose();
            m_spkScene = null;
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

        m_kImageA = null;
        m_kLUTa = null;
        m_kRGBTa = null;
        m_kImageB = null;
        m_kLUTb = null;
        m_kRGBTb = null;

//         if ( m_spkEyeCamera != null )
//         {
//             m_spkEyeCamera.dispose();
//             m_spkEyeCamera = null;
//         }
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


        m_akLights = null;

        m_kRotate = null;

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

        //m_kAnimator.stop();
    }

    /** Returns model image A
     * @return model image A
     */
    public ModelImage getImage()
    {
        return m_kImageA;
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
        
        if ( m_kImageA == null ) {
        	return;
        }
        
        if ( m_bFirstRender )
        {
            m_kVolumeRayCast.SetDisplay(true);   
            m_kSlices.SetDisplay(false);   
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
            
            for ( int i = 0; i < m_kDisplayList.size(); i++ )
            {
                m_kDisplayList.get(i).GetScene().Local.SetRotateCopy(m_spkScene.Local.GetRotate());
            }
        }

        if ( m_bTestFrameRate )
        {
            Matrix3f kRotate = m_spkScene.Local.GetRotate();
            kRotate.multEquals(m_kRotate);
            m_spkScene.Local.SetRotate(kRotate);
            m_spkScene.UpdateGS();
            m_kCuller.ComputeVisibleSet(m_spkScene);
            
            for ( int i = 0; i < m_kDisplayList.size(); i++ )
            {
                m_kDisplayList.get(i).GetScene().Local.SetRotateCopy(m_spkScene.Local.GetRotate());
            }
        }

        // Draw the scene to the back buffer/
        if (m_pkRenderer.BeginScene())
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
                                //System.err.println( kPos.X() + " " + kPos.Y() + " " + kPos.Z() );
                                //System.err.println( kDir.X() + " " + kDir.Y() + " " + kDir.Z() );
                                if ( m_bPaintEnabled )
                                {
                                    //System.err.println("Picked " + m_kDisplayList.get(i).getClass().getName());
                                    if ( m_bPaint )
                                    {
                                        m_kDisplayList.get(i).Paint( m_pkRenderer, m_kPicker.GetClosestNonnegative(), m_kPaintColor, m_iBrushSize );
                                    }
                                    else if ( m_bDropper || m_bPaintCan )
                                    {
                                        ColorRGBA kDropperColor = new ColorRGBA();
                                        Vector3f kPickPoint = new Vector3f();
                                        m_kDisplayList.get(i).Dropper( m_kPicker.GetClosestNonnegative(), kDropperColor, kPickPoint );
                                        m_kParent.setDropperColor( kDropperColor, kPickPoint );
                                    } 
                                    else if ( m_bErase )
                                    {
                                        m_kDisplayList.get(i).Erase( m_pkRenderer, m_kPicker.GetClosestNonnegative(), m_iBrushSize );
                                    }
                                }
                            }
                        }
                    }
                }
            }


            
            
            
            m_pkRenderer.SetBackgroundColor(ColorRGBA.BLACK);
            m_pkRenderer.ClearBuffers();

            if ( m_kVolumeRayCast.GetDisplay() )
            {
                if ( !m_bDisplaySecond )
                {
                    for ( int i = 0; i < m_kDisplayList.size(); i++ )
                    {
                        m_kDisplayList.get(i).PreRender( m_pkRenderer, m_kCuller );
                    }
                }
                else
                {
                    for ( int i = 0; i < m_kDisplayList.size(); i++ )
                    {
                        m_kDisplayList.get(i).PreRender( m_pkRenderer, m_kCuller );
                    }
                    m_kVolumeRayCast.PostPreRender();

                    m_pkRenderer.SetBackgroundColor(m_kBackgroundColor);
                    m_pkRenderer.ClearBuffers();

                    for ( int i = 1; i < m_kDisplayList.size(); i++ )
                    {
                        m_kDisplayList.get(i).Render( m_pkRenderer, m_kCuller );
                    }
                    m_kDisplayList.get(0).Render( m_pkRenderer, m_kCuller );

                    for ( int i = 1; i < m_kDisplayList.size(); i++ )
                    {
                        m_kDisplayList.get(i).PostRender( m_pkRenderer, m_kCuller );
                    }

                    //Draw frame rate:
                    m_pkRenderer.SetCamera(m_spkCamera);
                    if ( m_bTestFrameRate )
                    {
                       DrawFrameRate(8,16,ColorRGBA.WHITE);
                    }


                    if ( (m_kSculptor != null) && m_kSculptor.IsSculptDrawn() )
                    {
                        m_pkRenderer.Draw( m_kSculptor.getSculptImage() );
                    }
                }
            }
            else
            {
                m_pkRenderer.SetBackgroundColor(m_kBackgroundColor);
                m_pkRenderer.ClearBuffers();
                    
                for ( int i = 1; i < m_kDisplayList.size(); i++ )
                {
                    m_kDisplayList.get(i).Render( m_pkRenderer, m_kCuller );
                }
                for ( int i = 1; i < m_kDisplayList.size(); i++ )
                {
                    m_kDisplayList.get(i).PostRender( m_pkRenderer, m_kCuller );
                }

                //Draw frame rate:
                m_pkRenderer.SetCamera(m_spkCamera);
                if ( m_bTestFrameRate )
                {
                    DrawFrameRate(8,16,ColorRGBA.WHITE);
                }

                if ( (m_kSculptor != null) && m_kSculptor.IsSculptDrawn() )
                {
                    m_pkRenderer.Draw( m_kSculptor.getSculptImage() );
                }
            }
            m_pkRenderer.EndScene();
        }
        m_pkRenderer.DisplayBackBuffer();

        UpdateFrameCount();

        if ( m_bFirstRender )
        {
            m_bFirstRender = false;
            m_kVolumeRayCast.SetDisplay(false);   
            m_kSlices.SetDisplay(true);   
            VolumeImageViewer.main(m_kVolumeImageA);
            CMPMode();
        }
        if ( m_bSurfaceAdded )
        {
            m_bSurfaceAdded = false;
            updateLighting( m_akLights );
            for ( int i = 0; i < m_kDisplayList.size(); i++ )
            {
                if ( m_kDisplayList.get(i) instanceof VolumeSurface )
                {
                    ((VolumeSurface)m_kDisplayList.get(i)).InitClip(new float[] { 0, 1, 0, 1, 0, 1 });
                }
            }
        }
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
//         if ( m_bInit )
//         {
//             if ( !m_kAnimator.isAnimating() )
//             {
//                 m_kAnimator.start();
//             }        

//             return;
//         }
    	
    	if ( m_kImageA == null ) {
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

        m_kAnimator.add( GetCanvas() );
        //m_kAnimator = new Animator( GetCanvas() );
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
    	
    	if ( m_kImageA == null ) {
        	return;
        }
    	
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

    public Vector3f getTranslate() {
    	return m_kTranslate;
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
        /*
        m_spkAlpha = new AlphaState();
        m_spkAlpha.BlendEnabled = false;
        m_spkAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE_MINUS_DST_COLOR;
        m_spkAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
        m_spkScene.AttachGlobalState(m_spkAlpha);
        */
        m_spkVertexColor3Shader = new VertexColor3Effect();

        m_kVolumeRayCast = new VolumeRayCast( m_kVolumeImageA );
        m_kDisplayList.add(0, m_kVolumeRayCast);
        m_kVolumeRayCast.CreateScene( m_eFormat, m_eDepth, m_eStencil,
                m_eBuffering, m_eMultisampling,
                m_iWidth, m_iHeight, arg0, m_pkRenderer );

        m_kTranslate = m_kVolumeRayCast.GetTranslate();


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

        for ( int i = 0; i < 4; i++ )
        {
            if ( m_akTransfer[i] != null )
            {
                m_kVolumeImageA.UpdateImages(m_akTransfer[i], i);
            }
        }
        if ( m_kRGBTa != null )
        {
            m_kVolumeImageA.SetRGBT( m_kRGBTa, 0 );
        }
        if ( m_kRGBTb != null )
        {
            m_kVolumeImageB.SetRGBT( m_kRGBTb, 1 );
        }

        int iXBound = m_kImageA.getExtents()[0];
        int iYBound = m_kImageA.getExtents()[1];
        int iZBound = m_kImageA.getExtents()[2];
        InitDisplay(iXBound,iYBound,iZBound);

        m_kSlices = new VolumeSlices( m_kVolumeImageA, m_kTranslate, m_fX, m_fY, m_fZ );
        DisplayVolumeSlices( true );
        m_kDisplayList.add(m_kDisplayList.size(), m_kSlices);

        m_kVolumeClip = new VolumeClip( m_kVolumeImageA, m_kTranslate, m_fX, m_fY, m_fZ );
        m_kDisplayList.add( m_kDisplayList.size(), m_kVolumeClip);

        m_kVolumeBox = new VolumeBoundingBox( m_kVolumeImageA, m_kTranslate, m_fX, m_fY, m_fZ );
        m_kDisplayList.add( m_kDisplayList.size(), m_kVolumeBox);

        m_kVolumeCube = new VolumeOrientationCube( m_kVolumeImageA, m_kTranslate, m_fX, m_fY, m_fZ );
        m_kDisplayList.add( m_kDisplayList.size(), m_kVolumeCube);

        for ( int i = 0; i < m_pkRenderer.GetMaxLights(); i++ )
        {
            m_pkRenderer.SetLight( i, new Light() );
        }
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
        case 'f':
            m_bTestFrameRate = !m_bTestFrameRate;
            if ( m_bTestFrameRate )
            {
                ResetTime();
            }
            return;

        case 's':
        case 'S':
            TestStreaming(m_spkScene,"VolumeTextures.wmof");
            return;
        case 'b':
            m_bDisplaySecond = !m_bDisplaySecond;
            if ( m_kVolumeRayCast != null )
            {
                m_kVolumeRayCast.SetDisplaySecond( m_bDisplaySecond );
            }
            return;
        case 'p':
            DisplayBoundingBox( !m_kVolumeBox.GetDisplay() );
            return;
        case 'o':
            DisplayOrientationCube( !m_kVolumeCube.GetDisplay() );
            return;
        case 'q':
            setOrthographicProjection();
            return;
        case 'Q':
            setPerspectiveProjection();
            return;
            /*


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
                    */
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

        if ( m_kVolumeImageA.GetVolumeTarget().GetImage().GetData() != null )
        {
            m_kSculptor.setTextureImageDataA( m_kVolumeImageA.GetVolumeTarget().GetImage().GetData() );
        }
        else
        {
            m_kSculptor.setTextureImageFloatDataA( m_kVolumeImageA.GetVolumeTarget().GetImage().GetFloatData() );
        }
        /*
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
        */
        m_kSculptor.undoSculpt();
        m_kVolumeImageA.ReleaseVolume();
        m_kParent.setModified();
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
        if ( m_kVolumeImageA.GetVolumeTarget().GetImage().GetData() != null )
        {
            m_kSculptor.setTextureImageDataA( m_kVolumeImageA.GetVolumeTarget().GetImage().GetData() );
        }
        else
        {
            m_kSculptor.setTextureImageFloatDataA( m_kVolumeImageA.GetVolumeTarget().GetImage().GetFloatData() );
        }
        /*
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
        */
        if ( m_kSculptor.applySculpt() )
        {
            m_kVolumeImageA.ReleaseVolume();
            m_kSculptor.clearSculpt();
            m_kParent.setModified();
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
        if ( m_kVolumeImageA != null )
        {
            m_kVolumeImageA.UpdateData(kImage, 0);
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
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.InitClip(new float[] { 0, 1, 0, 1, 0, 1 });
        }
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
        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.setClipPlane(iWhich, fValue);
        }

        float[] data = new float[4];
        data[0] = fValue;
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetClip(iWhich, data);
        }  
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            {
                ((VolumeSurface)m_kDisplayList.get(i)).SetClip(iWhich,data);
            }
        }
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
        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.displayClipPlane(iWhich, bDisplay);
            m_kVolumeClip.SetDisplay(bDisplay);
        }
    }

    /**
     * Called from JPanelDisplay. Sets the bounding box display on/off.
     * @param bDisplay on/off.
     */
    public void DisplayBoundingBox( boolean bDisplay )
    {
        if ( m_kVolumeBox != null )
        {
            m_kVolumeBox.SetDisplay(bDisplay);
        }
    }

    /**
     * Called from JPanelDisplay. Sets the orientation cube display on/off.
     * @param bDisplay on/off.
     */
    public void DisplayOrientationCube( boolean bDisplay )
    {
        if ( m_kVolumeCube != null )
        {
            m_kVolumeCube.SetDisplay(bDisplay);
        }
    }

    /**
     * Called from JPanelDisplay. Sets the bounding box color.
     * @param kColor bounding box color.
     */
    public void SetBoundingBoxColor( ColorRGB kColor )
    {
        if ( m_kVolumeBox != null )
        {
            m_kVolumeBox.SetBoundingBoxColor(kColor);
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
            fValue = m_kVolumeClip.GetValue(iWhich);
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
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetClip(iWhich,data);
        }
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            {
                ((VolumeSurface)m_kDisplayList.get(i)).SetClip(iWhich,data);
            }
        }
    }

    /**
     * Enables the eye clip plane.
     * @param bEnable clipping enabled
     * @param bDisplay on/off.
     * @param kColor, the eye clip plane color.
     */
    public void enableEyeClipPlane( boolean bEnable, boolean bDisplay, ColorRGB kColor )
    {
        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.setEyeColor(kColor);
            m_kVolumeClip.DisplayEye(bDisplay);
        }
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
        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.setEyeInvColor(kColor);
            m_kVolumeClip.DisplayEyeInv(bDisplay);
        }
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
        float[] afEquation = new float[]{0,0,1,f4};
        float fZ = afEquation[3] * m_fZ;

        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.DisplayEye(bDisplay);
            m_kVolumeClip.setEyeClipPlane( fZ );
        }
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetClipEye(afEquation);
        }
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            {
                ((VolumeSurface)m_kDisplayList.get(i)).SetClipEye(afEquation);
            }
        }
    }

    /**
     * Sets the inverse-eye clip plane position.
     * @param f4 clip position (same value as sSliceInv in JPanelClip)
     * @param bDisplay on/off.
     */
    public void setEyeInvClipPlane( float f4, boolean bDisplay )
    {
        f4 /= (m_kImageA.getExtents()[2] -1);
        float[] afEquation = new float[]{0,0,1,f4};
        float fZ = afEquation[3] * m_fZ;

        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.DisplayEyeInv(bDisplay);
            m_kVolumeClip.setEyeInvClipPlane( fZ );
        }
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetClipEyeInv(afEquation);
        }
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            {
                ((VolumeSurface)m_kDisplayList.get(i)).SetClipEyeInv(afEquation);
            }
        }
    }


    /**
     * Enables the arbitrary clip plane position.
     * @param bEnable clipping enabled
     * @param bDisplay on/off.
     * @param kColor, the arbitrary clip plane color.
     */
    public void enableArbitraryClipPlane( boolean bEnable, boolean bDisplay, ColorRGB kColor )
    {
        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.setArbColor(kColor);
            m_kVolumeClip.DisplayArb(bDisplay);
        }
        if ( !bEnable )
        {
            setArbitraryClipPlane((float)(m_kImageA.getExtents()[0] -1));
        }
    }

    /**
     * Displays the arbitrary clip plane position.
     * @param bDisplay on/off.
     */
    public void displayArbitraryClipPlane( boolean bDisplay )
    {
        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.DisplayArb(bDisplay);
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
    private void doClip( ) 
    {           
        if ( m_kArbitraryClip == null )
        {
            m_kArbitraryClip = new Vector4f(1,0,0,0);            
        }
        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.SetArbPlane( m_kArbitraryClip.W() * m_fX );
        }

        // Rotate normal vector:
        Matrix3f kClipRotate = m_kVolumeClip.ArbRotate().Local.GetRotate();
        Vector3f kNormal = new Vector3f( 1,0,0 );
        kClipRotate.mult(kNormal, kNormal);
        kNormal.Normalize();

        // Scale kNormal based on the scaled volume:
        kNormal.SetData( kNormal.X() * m_fX, kNormal.Y() * m_fY, kNormal.Z() * m_fZ );
        float fLength = kNormal.Length();
        kNormal.Normalize();
        m_afArbEquation[0] = kNormal.X();
        m_afArbEquation[1] = kNormal.Y();
        m_afArbEquation[2] = kNormal.Z();

        // Calculate the distance to the plane, scaled based on the scaled kNormal:
        Vector3f kPos = new Vector3f();
        kNormal.scale( (m_kArbitraryClip.W() - 0.5f)/fLength, kPos );
        kPos.addEquals( new Vector3f( .5f, .5f, .5f ));
        m_afArbEquation[3] = kNormal.Dot(kPos);   
        
        // Update shader with rotated normal and distance:
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetClipArb(m_afArbEquation);
        }
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            {
                ((VolumeSurface)m_kDisplayList.get(i)).SetClipArb(m_afArbEquation);
            }
        }
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

        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.setClipPlaneColor(iWhich, kColor);
        }
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
        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.setEyeColor(kColor);
        }
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
        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.setArbColor(kColor);
        }
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
        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.setEyeInvColor(kColor);
        }
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
        else if ( (m_kVolumeClip != null) && (m_kVolumeClip.DisplayArb()) )
        {
            InitializeObjectMotion(m_kVolumeClip.ArbRotate());
            super.mousePressed(e);
            InitializeObjectMotion(m_spkScene);
        }
        if ( e.isControlDown() && m_bPaintEnabled )
        {
            m_iXPick = e.getX();
            m_iYPick = e.getY();
            m_bPickPending = true;
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
            else if ( e.isControlDown() && (m_kVolumeClip != null) && (m_kVolumeClip.DisplayArb()) )
            {
                InitializeObjectMotion(m_kVolumeClip.ArbRotate());
                super.mouseDragged(e);
                InitializeObjectMotion(m_spkScene);
                doClip();
            }
            else if ( e.isControlDown() && m_bPaintEnabled )
            {
                m_iXPick = e.getX();
                m_iYPick = e.getY();
                m_bPickPending = true;
            }
        }
    }

    /**
     * Called by the ApplicationGUI. Causes the current shader to be reloaded
     * from file, compiled and applied to the proxy-geometry.
     */
    public void reloadShaders()
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.ReloadVolumeShader( m_pkRenderer );
        }
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
    }

    /**
     * Called from the AdvancedMaterialProperties dialog. Sets the material
     * properties for the VolumeShaderSUR (Surface and Composite Surface
     * volume shaders.)
     * @param kMaterial, new material properties for the surface mode.
     */
    public void SetMaterialState( MaterialState kMaterial )
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetMaterialState(  kMaterial );
        }
    }

    /**
     * Called from the JPanelDisplay dialog. Gets the material properties for
     * the VolumeShaderSUR (Surface and Composite Surface volume shaders.)
     * @return material properties for the surface mode.
     */
    public MaterialState GetMaterialState( )
    {
        if ( m_kVolumeRayCast != null )
        {
            return m_kVolumeRayCast.GetMaterialState();
        }
        return null;
    }


    /**
     * Display the volume in MIP mode.
     */
    public void MIPMode()
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.MIPMode(m_pkRenderer);
        }
        ResetShaderParamsWindow();
    }

    /**
     * Display the volume in DDR mode.
     */
    public void DDRMode()
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.DDRMode(m_pkRenderer);
        }
        ResetShaderParamsWindow();
    }

    /**
     * Display the volume in Composite mode.
     */
    public void CMPMode()
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.CMPMode(m_pkRenderer);
        }
        ResetShaderParamsWindow();
    }

    /**
     * Display the volume in Composite Surface mode.
     */
    public void SURMode()
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SURMode(m_pkRenderer);
        }
        ResetShaderParamsWindow();
    }

    /**
     * Display the volume in Surface mode.
     */
    public void SURFASTMode()
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SURFASTMode(m_pkRenderer);
        }
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
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.Blend(fValue);
        }
    }

    /**
     * Sets the raytracing steps size.
     * @param fValue, the steps value (0-450)
     */
    public void StepsSize( float fValue )
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.StepsSize(fValue);
        }
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
        m_kShaderParamsWindow.AddUserVariables(m_kVolumeRayCast.GetShaderEffect().GetPProgram());
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
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetBackgroundColor( kColor );
        }
    }


    /**
     * Enables/Disables Gradient Magnitude filter.
     * @param bShow, gradient magnitude filter on/off
     */
    public void SetGradientMagnitude(boolean bShow)
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetGradientMagnitude(bShow);
        }
    }

    /**
     * Enables/Disables self-shadowing in the Surface mode.
     * @param bShadow, shadow on/off.
     */
    public void SelfShadow(boolean bShadow)
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SelfShadow(bShadow);
        }
    }

    /** Add a polyline to the display. Used to display fiber tract bundles.
     * @param kLine, new polyline to display.
     * @param iGroup, the group the polyline belongs to.
     */
    public void addPolyline( Polyline kLine, int iGroup )
    {
        if ( m_kDTIDisplay == null )
        {
            m_kDTIDisplay = new VolumeDTI( m_kVolumeImageA, m_kTranslate, m_fX, m_fY, m_fZ );
            m_kDisplayList.add(m_kDisplayList.size(), m_kDTIDisplay);
        }
        m_kDTIDisplay.addPolyline( kLine, iGroup );
        m_kDTIDisplay.SetDisplay( true );
    }

    /** 
     * Removes the specified polyline tract group.
     * @param iGroup, the group of polylines to remove.
     */
    public void removePolyline( int iGroup )
    {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.removePolyline(iGroup);
        }
        m_kDTIDisplay.SetDisplay( m_kDTIDisplay.GetDisplayTract() );
    }

    /** Sets the polyline color for the specified fiber bundle tract group. 
     * @param iGroup, the fiber bundle group to set.
     * @param kColor the new polyline color for the specified fiber bundle tract group. 
     */
    public void setPolylineColor( int iGroup, ColorRGB kColor )
    {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.setPolylineColor(iGroup, kColor);
        }
    }

    /** Returns the polyline color for the specified fiber bundle tract group. 
     * @param iGroup, the fiber bundle group to query.
     * @return the polyline color for the specified fiber bundle tract group. 
     */
    public ColorRGB getPolylineColor( int iGroup )
    {
        if ( m_kDTIDisplay != null )
        {
            return m_kDTIDisplay.getPolylineColor(iGroup);
        }
        return null;
    }


    /** Sets the DTI Image for displaying the tensors as ellipsoids.
     * @param kDTIImage.
     */
    public void setDTIImage( ModelImage kDTIImage )
    {
        if ( m_kDTIDisplay == null )
        {
            m_kDTIDisplay = new VolumeDTI( m_kVolumeImageA, m_kTranslate, m_fX, m_fY, m_fZ );
            m_kDisplayList.add(m_kDisplayList.size(), m_kDTIDisplay);
        }
        m_kDTIDisplay.setDTIImage(kDTIImage);
    }

    /** Turns on/off displaying the fiber bundle tracts with ellipsoids.
     * @param bDisplay, when true display the tracts with ellipsods.
     */
    public void setDisplayEllipsoids( boolean bDisplay )
    {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.setDisplayEllipsoids( bDisplay );
        }
    }

    /** Turns on/off displaying all the ellipsoids.
     * @param bDisplay, when true display all the ellipsods in the volume.
     */
    public void setDisplayAllEllipsoids( boolean bDisplay )
    {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.setDisplayAllEllipsoids(bDisplay);
        }
    }

    /** Set the m_iEllipsoidMod value. 
     * @param iMod, new m_iEllipsoidMod value.
     */
    public void setEllipseMod( int iMod )
    {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.setEllipseMod( iMod );
        }
    }

    public void SetCenter( Vector3f kCenter )
    {
        if ( m_kSlices != null )
        {
            m_kSlices.SetCenter( new Vector3f( (kCenter.X() / (m_kImageA.getExtents()[0] -1)),
                                               (kCenter.Y() / (m_kImageA.getExtents()[1] -1)),
                                               (kCenter.Z() / (m_kImageA.getExtents()[2] -1))  ) );

        }
    }

    public void ShowSlice( int i, boolean bShow )
    {
        if ( m_kSlices != null )
        {
            m_kSlices.ShowSlice( i, bShow );
        }
    }

    public void SetBoundingBoxColor( int i, ColorRGB kColor )
    {
        if ( m_kSlices != null )
        {
            m_kSlices.SetBoundingBoxColor( i, kColor );
        }
    }
    
    public void ShowBoundingBox( int i, boolean bShow )
    {
        if ( m_kSlices != null )
        {
            m_kSlices.ShowBoundingBox( i, bShow );
        }
    }

    public void SetSliceOpacity( int i, float fAlpha )
    {
        if ( m_kSlices != null )
        {
            m_kSlices.SetSliceOpacity( i, fAlpha );
        }
    }


    public void DisplayVolumeRaycast( boolean bDisplay )
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetDisplay(bDisplay);
        }
    }

    public void DisplayVolumeSlices( boolean bDisplay )
    {
        if ( m_kSlices != null )
        {
            m_kSlices.SetDisplay( bDisplay );
        }
    }

    public void setVolumeBlend( float fBlend )
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.setVolumeBlend(fBlend);
        }
    }

    public void resetAxisX()
    {
        m_spkScene.Local.SetRotateCopy( Matrix3f.IDENTITY );
        m_spkScene.Local.GetRotate().FromAxisAngle( Vector3f.UNIT_X, (float)Math.PI/2.0f );
        UpdateSceneRotation();
    }

    public void resetAxisY()
    {
        m_spkScene.Local.SetRotateCopy( Matrix3f.IDENTITY );
        m_spkScene.Local.GetRotate().FromAxisAngle( Vector3f.UNIT_Y, -(float)Math.PI/2.0f );
        UpdateSceneRotation();
    }

    public void resetAxis()
    {
        m_spkScene.Local.SetRotateCopy( Matrix3f.IDENTITY );
        UpdateSceneRotation();
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
    
    public void DisplayPolyline(boolean bDisplay)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            {
                m_kDisplayList.get(i).SetDisplay(bDisplay);
            }
        }
    }
    
    public void addSurface(TriMesh[] akSurfaces)
    {
        for ( int i = 0; i < akSurfaces.length; i++ )
        {
            VolumeSurface kSurface = new VolumeSurface( m_pkRenderer, m_kVolumeImageA,
                    m_kTranslate,
                    m_fX, m_fY, m_fZ,
                    akSurfaces[i] );
            kSurface.SetPerPixelLighting( m_pkRenderer, true );
            m_kDisplayList.add( kSurface );
        }
        UpdateSceneRotation();
        m_bSurfaceAdded = true;
    }    
    
    public void DisplaySurface(boolean bDisplay)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            {
                m_kDisplayList.get(i).SetDisplay(bDisplay);
            }
        }
    }

    public void setPolygonMode(String kSurfaceName, WireframeState.FillMode eMode)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    m_kDisplayList.get(i).SetPolygonMode( true, eMode );
                }
            }
        }
    }
    
    public void setPerPixelLighting(String kSurfaceName, boolean bOn)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    ((VolumeSurface)m_kDisplayList.get(i)).SetPerPixelLighting( m_pkRenderer, bOn );
                }
            }
        }
        updateLighting(m_akLights);
    }

    public void setClipping(String kSurfaceName, boolean bClip)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    ((VolumeSurface)m_kDisplayList.get(i)).SetClipping( bClip );
                }
            }
        }
    }

    public void setBackface(String kSurfaceName, boolean bOn)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    ((VolumeSurface)m_kDisplayList.get(i)).SetBackface( bOn );
                }
            }
        }
    }
    
    public void setPickable(String kSurfaceName, boolean bOn)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    m_kDisplayList.get(i).SetPickable( bOn );
                }
            }
        }
    }
    
    
    public void setSurfaceTexture(String kSurfaceName, boolean bOn, boolean bUseNewImage, boolean bUseNewLUT)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    ((VolumeSurface)m_kDisplayList.get(i)).SetSurfaceTexture( bOn, bUseNewImage, bUseNewLUT );
                }
            }
        }
    }

    /**
     * Sets blending between imageA and imageB.
     * @param fValue, the blend value (0-1)
     */
    public void Blend( String kSurfaceName, float fValue )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    m_kDisplayList.get(i).Blend( fValue );
                }
            }
        }
    }
    
    /**
     * Sets blending between imageA and imageB.
     * @param fValue, the blend value (0-1)
     */
    public void setColor( String kSurfaceName, ColorRGB kColor )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    m_kDisplayList.get(i).SetColor( kColor );
                }
            }
        }
    }
    
    /**
     * Sets blending between imageA and imageB.
     * @param fValue, the blend value (0-1)
     */
    public void removeSurface( String kSurfaceName )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    VolumeSurface kSurface = (VolumeSurface)m_kDisplayList.remove(i);
                    kSurface.dispose();
                    kSurface = null;
                }
            }
        }
    }

    
    /**
     */
    public float GetVolume( String kSurfaceName )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    return ((VolumeSurface)(m_kDisplayList.get(i))).GetVolume();
                }
            }
        }
        return 0;
    }
    
    /**
     */
    public float GetSurfaceArea( String kSurfaceName )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    return ((VolumeSurface)(m_kDisplayList.get(i))).GetSurfaceArea();
                }
            }
        }
        return 0;
    }
    
    
    /**
     */
    public MaterialState GetMaterial( String kSurfaceName )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    return ((VolumeSurface)(m_kDisplayList.get(i))).GetMaterial();
                }
            }
        }
        return null;
    }
    
    
    /**
     */
    public void SetMaterial( String kSurfaceName, MaterialState kMaterial )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    ((VolumeSurface)(m_kDisplayList.get(i))).SetMaterial(kMaterial);
                }
            }
        }
    }
    
    public GeneralLight[] GetLights()
    {
        return m_akLights;
    }
    
    
    public void enablePaint( ColorRGBA kPaintColor, int iBrushSize, boolean bEnabled, boolean bPaint, boolean bDropper, boolean bPaintCan, boolean bErase )
    {
        m_kPaintColor = kPaintColor;
        m_iBrushSize = iBrushSize;
        m_bPaintEnabled = bEnabled;
        m_bPaint = bPaint;
        m_bDropper = bDropper;
        m_bPaintCan = bPaintCan;
        m_bErase = bErase;
    }
    
    public void eraseAllPaint()
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            {
                ((VolumeSurface)(m_kDisplayList.get(i))).EraseAllPaint(m_pkRenderer);
            }
        }
    }
    
    
    public void SetLUTNew( String kSurfaceName, ModelLUT kLUT, ModelRGB kRGBT )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    ((VolumeSurface)(m_kDisplayList.get(i))).SetLUTNew(kLUT, kRGBT);
                }
            }
        }
    }
    
    
    
    public void SetImageNew( String kSurfaceName, ModelImage kImage )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    ((VolumeSurface)(m_kDisplayList.get(i))).SetImageNew(kImage);
                }
            }
        }
    }
    
    private VolumeImage m_kVolumeImageA;
    private VolumeImage m_kVolumeImageB;
    
    /** Scene-graph root node: */
    private Node m_spkScene;
    /** Turns wireframe on/off: */
    //private AlphaState m_spkAlpha;
    /** Culling: turns backface/frontface culling on/off: */
    private CullState m_spkCull;
    /** Culling out-of-view objects: */
    private Culler m_kCuller = new Culler(0,0,null);

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
    /** Normalized volume extents: */
    private float m_fX, m_fY, m_fZ, m_fMax;
    /** Flag for indicating the that Java Container is visible or not: */
    private boolean m_bVisible = true;

    /** Window with the shader paramter interface: */
    private ApplicationGUI m_kShaderParamsWindow = null;
    /** Lights from JPanelLight */
    private GeneralLight[] m_akLights = null;
    /** Enables/Disables rendering the second pass. When disabled, the
     * back-facing polygons of the proxy-geometry are shown instead of the volume: */
    private boolean m_bDisplaySecond = true;

    /** For testing the frame rate: */
    private boolean m_bTestFrameRate = false;
    /** Rotation during frame rate tests: */
    private Matrix3f m_kRotate = new Matrix3f();

    private Vector<VolumeObject> m_kDisplayList = new Vector<VolumeObject>();

    private VolumeSlices m_kSlices = null;
    private VolumeRayCast m_kVolumeRayCast = null;
    private VolumeClip m_kVolumeClip = null;
    private VolumeDTI m_kDTIDisplay = null;
    private VolumeBoundingBox m_kVolumeBox = null;
    private VolumeOrientationCube m_kVolumeCube = null;

    private boolean m_bFirstRender = true;

    private float[] m_afArbEquation = new float[4];
    private boolean m_bSurfaceAdded = false;
    private VolumeViewer m_kParent = null;
    

    private int m_iXPick = -1;
    private int m_iYPick = -1;
    private boolean m_bPickPending = false;
    private Picker m_kPicker = new Picker();

    private boolean m_bPaintEnabled = false;
    private boolean m_bPaint = false;
    private boolean m_bErase = false;
    private boolean m_bDropper = false;
    private boolean m_bPaintCan = false;
    private ColorRGBA m_kPaintColor = null;
    private int m_iBrushSize = 1;
}
