/**
 * 
 */

package gov.nih.mipav.view.renderer.volumeview;

import javax.media.opengl.*;
import com.sun.opengl.util.*;
import java.awt.event.*;

import gov.nih.mipav.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.WildMagic.LibApplications.OpenGLApplication.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;
import gov.nih.mipav.view.WildMagic.LibRenderers.OpenGLRenderer.*;

import gov.nih.mipav.view.renderer.GeneralLight;
import gov.nih.mipav.view.renderer.volumeview.*;

/**
 */
public class GPUVolumeRender extends JavaApplication3
    implements GLEventListener, KeyListener, MouseMotionListener
{
    public GPUVolumeRender( ModelImage kImageA, ModelLUT kLUTa, ModelRGB kRGBTa,
                            ModelImage kImageB, ModelLUT kLUTb, ModelRGB kRGBTb  )
    {

        super("GPUVolumeRender",0,0,512,512, new ColorRGBA(0.0f,0.0f,0.0f,0.0f));

        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                                           m_eBuffering, m_eMultisampling,
                                           m_iWidth, m_iHeight );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );       

        m_pkRenderer.SetLineWidth(3);

        ImageCatalog.SetActive( new ImageCatalog("Main") );      
        VertexProgramCatalog.SetActive(new VertexProgramCatalog("Main", System.getProperties().getProperty("user.dir")));       
        PixelProgramCatalog.SetActive(new PixelProgramCatalog("Main", System.getProperties().getProperty("user.dir")));
        
        m_kImageA = kImageA;
        m_kLUTa = kLUTa;
        m_kRGBTa = kRGBTa;

        m_kImageB = kImageB;
        m_kLUTb = kLUTb;
        m_kRGBTb = kRGBTb;

        m_kSculptor = new SculptorWm( ((OpenGLRenderer)m_pkRenderer).GetCanvas() );
        m_kSculptor.setImage(m_kImageA, m_kImageB);
    }

    public void finalize()
    {
        m_kAnimator.stop();
        m_kAnimator = null;

        //System.err.println("GPUVolumeRender: finalize()");
        m_spkScene.finalize();
        m_spkScene = null;

        try {
            m_kSculptor.finalize();
        } catch (Throwable e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        m_kSculptor = null;
        m_kLUTa = null;
        m_akTransfer[0] = null;
        m_akTransfer[1] = null;

        if ( m_kShaderParamsWindow != null )
        {
            m_kShaderParamsWindow.close();
        }

        m_kVolumeShaderEffect.finalize();
    }

    public void display(GLAutoDrawable arg0) {
        if ( !m_bVisible )
        {
            return;
        }

        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );

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
            m_pkPBuffer.SetDrawable( arg0 );

            // Draw the scene to a color buffer.
            m_kMesh.DetachAllEffects();
            m_kMesh.AttachEffect( m_spkVertexColor3Shader );
            m_kCuller.ComputeVisibleSet(m_spkScene);

            m_pkPBuffer.Enable();
            m_pkRenderer.SetBackgroundColor(ColorRGBA.BLACK);
            m_pkRenderer.ClearBuffers();
            m_spkCull.CullFace = CullState.CullMode.CT_FRONT;
            m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
            m_spkCull.CullFace = CullState.CullMode.CT_BACK;
            m_pkPBuffer.Disable();

            m_kMesh.DetachAllEffects();
            m_kMesh.AttachEffect( m_kVolumeShaderEffect );
            m_kCuller.ComputeVisibleSet(m_spkScene);

            // Draw the scene to the main window and also to a regular screen
            // polygon, placed in the lower-left corner of the main window.
            m_pkRenderer.SetBackgroundColor(m_kBackgroundColor);
            m_pkRenderer.ClearBuffers();
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
            m_pkRenderer.SetCamera(m_spkScreenCamera);
            m_pkRenderer.Draw(m_spkScenePolygon);

            m_pkRenderer.SetCamera(m_spkCamera);
            DrawFrameRate(8,16,ColorRGBA.WHITE);
            */

            if ( m_kSculptor.IsSculptDrawn() )
            {
                m_pkRenderer.Draw( m_kSculptor.getSculptImage() );
            }
            
            m_pkRenderer.EndScene();
        }
        m_pkRenderer.DisplayBackBuffer();
        
        UpdateFrameCount();
    }

    public void displayChanged(GLAutoDrawable arg0, boolean arg1, boolean arg2) {}

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

        super.OnInitialize();

        // set up camera
        m_spkCamera.SetFrustum(60.0f,m_iWidth/(float)m_iHeight,0.01f,10.0f);
        Vector3f kCDir = new Vector3f(0.0f,0.0f,1.0f);
        Vector3f kCUp = new Vector3f(0.0f, -1.0f,0.0f);
        Vector3f kCRight = kCDir.Cross(kCUp);
        Vector3f kCLoc = kCDir.scale(-1.4f);
        m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);

        CreateScene( arg0 );

        // initial update of objects
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();

        // initial culling of scene
        m_kCuller.SetCamera(m_spkCamera);
        m_kCuller.ComputeVisibleSet(m_spkScene);

        InitializeCameraMotion(.5f,0.001f);
        InitializeObjectMotion(m_spkScene);

        ((OpenGLRenderer)m_pkRenderer).ClearDrawable( );

        m_kAnimator = new Animator( GetCanvas() );
        //m_kAnimator.setRunAsFastAsPossible(true); 
        m_kAnimator.start();
    }
    
    public void stopAnimator()
    {
        if ( (m_kAnimator != null) && m_kAnimator.isAnimating() )
        {
            m_kAnimator.stop();
        }        
    }

    public void startAnimator()
    {
        m_kAnimator.start();
    }

    public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight) {
        if (iWidth > 0 && iHeight > 0)
        {
            m_iWidth = iWidth;
            m_iHeight = iHeight;
            m_spkCamera.SetFrustum(60.0f,m_iWidth/(float)m_iHeight,1.0f,1000.0f);


            if (m_pkRenderer != null)
            {
                ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
                m_pkRenderer.Resize(iWidth,iHeight);
                ((OpenGLRenderer)m_pkRenderer).ClearDrawable( );
            }
        }
    }

    public GLCanvas GetCanvas()
    {
        return ((OpenGLRenderer)m_pkRenderer).GetCanvas();
    }

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

        // Create a screen polygon to use as the RGBA render target.
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetTChannels(0,2);
        VertexBuffer pkVBuffer = new VertexBuffer(kAttr,4);
        pkVBuffer.Position3(0, new Vector3f(0.0f,0.0f,0.0f) );
        pkVBuffer.Position3(1, new Vector3f(0.2f,0.0f,0.0f) );
        pkVBuffer.Position3(2, new Vector3f(0.2f,0.2f,0.0f) );
        pkVBuffer.Position3(3, new Vector3f(0.0f,0.2f,0.0f) );
        pkVBuffer.TCoord2(0,0, new Vector2f(0.0f,0.0f) );
        pkVBuffer.TCoord2(0,1, new Vector2f(1.0f,0.0f) );
        pkVBuffer.TCoord2(0,2, new Vector2f(1.0f,1.0f) );
        pkVBuffer.TCoord2(0,3, new Vector2f(0.0f,1.0f) );
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
        m_kTranslate = new Vector3f( m_spkScene.WorldBound.GetCenter().neg() );
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
        m_kVolumeShaderEffect.MIPMode(0, m_pkRenderer);
        m_kVolumeShaderEffect.Blend(0.5f);

        for ( int i = 0; i < 2; i++ )
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
    public boolean updateImages(ModelLUT kLUTa, ModelLUT kLUTb)
    {
        return m_kVolumeShaderEffect.UpdateImages(kLUTa, kLUTb);
    }

    private void CreateBox ()
    {
         int iXBound = m_kImageA.getExtents()[0];
         int iYBound = m_kImageA.getExtents()[1];
         int iZBound = m_kImageA.getExtents()[2];
        Box(iXBound,iYBound,iZBound);
        m_spkVertexColor3Shader = new VertexColor3Effect();
    }

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
        
        System.err.println( "GPU " + m_fX + " " + m_fY + " " + m_fZ);

        int iVQuantity = 24;
        int iTQuantity = 12;
        VertexBuffer pkVB = new VertexBuffer(kAttr,iVQuantity);
        IndexBuffer pkIB = new IndexBuffer(3*iTQuantity);

        // generate connectivity (outside view)
        int i = 0;
        int[] aiIndex = pkIB.GetData();

        // generate geometry
        // front
        pkVB.Position3(0, new Vector3f(0,0,0));
        pkVB.Position3(1, new Vector3f(m_fX,0,0));
        pkVB.Position3(2, new Vector3f(m_fX,m_fY,0));
        pkVB.Position3(3, new Vector3f(0,m_fY,0));
        pkVB.Color3(0,0, new ColorRGB(0,0,0));
        pkVB.Color3(0,1, new ColorRGB(1,0,0));
        pkVB.Color3(0,2, new ColorRGB(1,1,0));
        pkVB.Color3(0,3, new ColorRGB(0,1,0));
        aiIndex[i++] = 0;  aiIndex[i++] = 2;  aiIndex[i++] = 1;
        aiIndex[i++] = 0;  aiIndex[i++] = 3;  aiIndex[i++] = 2;

        // back
        pkVB.Position3(4, new Vector3f(0,0,m_fZ));
        pkVB.Position3(5, new Vector3f(m_fX,0,m_fZ));
        pkVB.Position3(6, new Vector3f(m_fX,m_fY,m_fZ));
        pkVB.Position3(7, new Vector3f(0,m_fY,m_fZ));
        pkVB.Color3(0,4, new ColorRGB(0,0,1));
        pkVB.Color3(0,5, new ColorRGB(1,0,1));
        pkVB.Color3(0,6, new ColorRGB(1,1,1));
        pkVB.Color3(0,7, new ColorRGB(0,1,1));
        aiIndex[i++] = 4;  aiIndex[i++] = 5;  aiIndex[i++] = 6;
        aiIndex[i++] = 4;  aiIndex[i++] = 6;  aiIndex[i++] = 7;

        // top
        pkVB.Position3(8, new Vector3f(0,m_fY,0));
        pkVB.Position3(9, new Vector3f(m_fX,m_fY,0));
        pkVB.Position3(10, new Vector3f(m_fX,m_fY,m_fZ));
        pkVB.Position3(11, new Vector3f(0,m_fY,m_fZ));
        pkVB.Color3(0,8, new ColorRGB(0,1,0));
        pkVB.Color3(0,9, new ColorRGB(1,1,0));
        pkVB.Color3(0,10, new ColorRGB(1,1,1));
        pkVB.Color3(0,11, new ColorRGB(0,1,1));
        aiIndex[i++] = 8;  aiIndex[i++] = 10;  aiIndex[i++] = 9;
        aiIndex[i++] = 8;  aiIndex[i++] = 11;  aiIndex[i++] = 10;

        // bottom
        pkVB.Position3(12, new Vector3f(0,0,0));
        pkVB.Position3(13, new Vector3f(m_fX,0,0));
        pkVB.Position3(14, new Vector3f(m_fX,0,m_fZ));
        pkVB.Position3(15, new Vector3f(0,0,m_fZ));
        pkVB.Color3(0,12, new ColorRGB(0,0,0));
        pkVB.Color3(0,13, new ColorRGB(1,0,0));
        pkVB.Color3(0,14, new ColorRGB(1,0,1));
        pkVB.Color3(0,15, new ColorRGB(0,0,1));
        aiIndex[i++] = 12;  aiIndex[i++] = 13;  aiIndex[i++] = 14;
        aiIndex[i++] = 12;  aiIndex[i++] = 14;  aiIndex[i++] = 15;

        // right
        pkVB.Position3(16, new Vector3f(m_fX,0,0));
        pkVB.Position3(17, new Vector3f(m_fX,m_fY,0));
        pkVB.Position3(18, new Vector3f(m_fX,m_fY,m_fZ));
        pkVB.Position3(19, new Vector3f(m_fX,0,m_fZ));
        pkVB.Color3(0,16, new ColorRGB(1,0,0));
        pkVB.Color3(0,17, new ColorRGB(1,1,0));
        pkVB.Color3(0,18, new ColorRGB(1,1,1));
        pkVB.Color3(0,19, new ColorRGB(1,0,1));
        aiIndex[i++] = 16;  aiIndex[i++] = 17;  aiIndex[i++] = 18;
        aiIndex[i++] = 16;  aiIndex[i++] = 18;  aiIndex[i++] = 19;

        // left
        pkVB.Position3(20, new Vector3f(0,0,0));
        pkVB.Position3(21, new Vector3f(0,m_fY,0));
        pkVB.Position3(22, new Vector3f(0,m_fY,m_fZ));
        pkVB.Position3(23, new Vector3f(0,0,m_fZ));
        pkVB.Color3(0,20, new ColorRGB(0,0,0));
        pkVB.Color3(0,21, new ColorRGB(0,1,0));
        pkVB.Color3(0,22, new ColorRGB(0,1,1));
        pkVB.Color3(0,23, new ColorRGB(0,0,1));
        aiIndex[i++] = 20;  aiIndex[i++] = 22;  aiIndex[i++] = 21;
        aiIndex[i++] = 20;  aiIndex[i++] = 23;  aiIndex[i++] = 22;

        if (kAttr.GetMaxTCoords() > 0)
        {
            for (int iUnit = 0; iUnit < kAttr.GetMaxTCoords(); iUnit++)
            {
                if (kAttr.HasTCoord(iUnit))
                {
                    pkVB.TCoord3(iUnit,0, new Vector3f(0,0,0));
                    pkVB.TCoord3(iUnit,1, new Vector3f(1,0,0));
                    pkVB.TCoord3(iUnit,2, new Vector3f(1,1,0));
                    pkVB.TCoord3(iUnit,3, new Vector3f(0,1,0));
                    pkVB.TCoord3(iUnit,4, new Vector3f(0,0,1));
                    pkVB.TCoord3(iUnit,5, new Vector3f(1,0,1));
                    pkVB.TCoord3(iUnit,6, new Vector3f(1,1,1));
                    pkVB.TCoord3(iUnit,7, new Vector3f(0,1,1));
                    pkVB.TCoord3(iUnit,8, new Vector3f(0,1,0));
                    pkVB.TCoord3(iUnit,9, new Vector3f(1,1,0));
                    pkVB.TCoord3(iUnit,10, new Vector3f(1,1,1));
                    pkVB.TCoord3(iUnit,11, new Vector3f(0,1,1));
                    pkVB.TCoord3(iUnit,12, new Vector3f(0,0,0));
                    pkVB.TCoord3(iUnit,13, new Vector3f(1,0,0));
                    pkVB.TCoord3(iUnit,14, new Vector3f(1,0,1));
                    pkVB.TCoord3(iUnit,15, new Vector3f(0,0,1));
                    pkVB.TCoord3(iUnit,16, new Vector3f(1,0,0));
                    pkVB.TCoord3(iUnit,17, new Vector3f(1,1,0));
                    pkVB.TCoord3(iUnit,18, new Vector3f(1,1,1));
                    pkVB.TCoord3(iUnit,19, new Vector3f(1,0,1));
                    pkVB.TCoord3(iUnit,20, new Vector3f(0,0,0));
                    pkVB.TCoord3(iUnit,21, new Vector3f(0,1,0));
                    pkVB.TCoord3(iUnit,22, new Vector3f(0,1,1));
                    pkVB.TCoord3(iUnit,23, new Vector3f(0,0,1));
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




    public void keyPressed(KeyEvent e) {
        char ucKey = e.getKeyChar();
        
        super.keyPressed(e);
        switch (ucKey)
        {
        case 's':
        case 'S':
             TestStreaming(m_spkScene,"VolumeTextures.wmof");
             return;
        }

        return;
    }

    public void setOrthographicProjection()
    {
        m_spkCamera.Perspective = false;
        m_pkRenderer.OnFrustumChange();
    }

    public void setPerspectiveProjection()
    {
        m_spkCamera.Perspective = true;
        m_pkRenderer.OnFrustumChange();
    }

    public boolean getSculptEnabled()
    {
        return m_kSculptor.getEnable();
    }
    
    public void enableSculpt( boolean bSculpt )
    {
        m_kSculptor.enableSculpt(bSculpt);
    }

    public void invertSculpt()
    {
        m_kSculptor.invertSculpt();
    }

    public void clearSculpt()
    {
        m_kSculptor.clearSculpt();
    }
    
    public void undoSculpt()
    {
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

    public void applySculpt()
    {
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

    public boolean save(FileWriteOptions options, int filterType)
    {
        return m_kSculptor.save(options, filterType);
    }

    public void setDrawingShape(int shape)
    {
        m_kSculptor.setDrawingShape(shape);
    }

    public void updateData( ModelImage kImage )
    {
        m_kImageA = kImage;
        m_kSculptor.setImage(m_kImageA, m_kImageB);
        m_kVolumeShaderEffect.UpdateData(kImage, 0);
    }

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
                akOutlineSquare[i].Color3( 0, j, new ColorRGB( 1, 0, 0 ) );
            }

            akOutlineSquare[i].TCoord2( 0, 0, new Vector2f( 1, 1 ) );
            akOutlineSquare[i].TCoord2( 0, 1, new Vector2f( 0, 1 ) );
            akOutlineSquare[i].TCoord2( 0, 2, new Vector2f( 0, 0 ) );
            akOutlineSquare[i].TCoord2( 0, 3, new Vector2f( 1, 0 ) );
        }
        // neg x clipping:
        akOutlineSquare[0].Position3( 0, new Vector3f( 0, 0, 0 ) );
        akOutlineSquare[0].Position3( 1, new Vector3f( 0, 0, m_fZ ) );
        akOutlineSquare[0].Position3( 2, new Vector3f( 0, m_fY, m_fZ ) );
        akOutlineSquare[0].Position3( 3, new Vector3f( 0, m_fY, 0 ) );

        // pos x clipping:
        akOutlineSquare[1].Position3( 0, new Vector3f( m_fX, 0, m_fZ ) );
        akOutlineSquare[1].Position3( 1, new Vector3f( m_fX, 0, 0 ) );
        akOutlineSquare[1].Position3( 2, new Vector3f( m_fX, m_fY, 0 ) );
        akOutlineSquare[1].Position3( 3, new Vector3f( m_fX, m_fY, m_fZ ) );

        // neg y clipping:
        akOutlineSquare[2].Position3( 0, new Vector3f( m_fX, 0, m_fZ ) );
        akOutlineSquare[2].Position3( 1, new Vector3f( 0, 0, m_fZ ) );
        akOutlineSquare[2].Position3( 2, new Vector3f( 0, 0, 0 ) );
        akOutlineSquare[2].Position3( 3, new Vector3f( m_fX, 0, 0 ) );
        // pos y clipping:
        akOutlineSquare[3].Position3( 0, new Vector3f( m_fX, m_fY, 0 ) );
        akOutlineSquare[3].Position3( 1, new Vector3f( 0, m_fY, 0 ) );
        akOutlineSquare[3].Position3( 2, new Vector3f( 0, m_fY, m_fZ ) );
        akOutlineSquare[3].Position3( 3, new Vector3f( m_fX, m_fY, m_fZ ) );

        // neg z clipping:
        akOutlineSquare[4].Position3( 0, new Vector3f( m_fX, 0, 0 ) );
        akOutlineSquare[4].Position3( 1, new Vector3f( 0, 0, 0 ) );
        akOutlineSquare[4].Position3( 2, new Vector3f( 0, m_fY, 0 ) );
        akOutlineSquare[4].Position3( 3, new Vector3f( m_fX, m_fY, 0 ) );

        // pos z clipping:
        akOutlineSquare[5].Position3( 0, new Vector3f( 0, 0, m_fZ ) );
        akOutlineSquare[5].Position3( 1, new Vector3f( m_fX, 0, m_fZ ) );
        akOutlineSquare[5].Position3( 2, new Vector3f( m_fX, m_fY, m_fZ ) );
        akOutlineSquare[5].Position3( 3, new Vector3f( 0, m_fY, m_fZ ) );

        Vector3f kHalf = new Vector3f( m_fX/2.0f, m_fY/2.0f, m_fZ/2.0f );
        Vector3f kPos;

        for ( int i = 0; i < 6; i++ )
        {
            m_akPolyline[i] = new Polyline( new VertexBuffer(akOutlineSquare[i]), true, true );
            m_akPolyline[i].AttachEffect( m_spkVertexColor3Shader );
            m_akPolyline[i].Local.SetTranslate(m_kTranslate);

            m_akBoundingBox[i] = new Polyline( new VertexBuffer(akOutlineSquare[i]), true, true );
            m_akBoundingBox[i].AttachEffect( m_spkVertexColor3Shader );
            m_akBoundingBox[i].Local.SetTranslate(m_kTranslate);

            System.err.println(m_aakAxisFiles[i]);

            for ( int j = 0; j < 4; j++ )
            {
                kPos = akOutlineSquare[i].Position3( j );
                kPos.subEquals(kHalf);
                akOutlineSquare[i].Position3( j, kPos );
            }
            m_akOrientationCube[i] = new TriMesh( new VertexBuffer(akOutlineSquare[i]), kIndexBuffer );
            m_akOrientationCube[i].AttachEffect( new TextureEffect( m_aakAxisFiles[i] ) );
            m_akOrientationCube[i].Local.SetTranslate(m_kCubeTranslate);
            m_akOrientationCube[i].Local.SetScale(kCubeScale);
            m_akOrientationCube[i].UpdateGS();
            m_akOrientationCube[i].UpdateRS();
            m_pkRenderer.LoadResources(m_akOrientationCube[i]);
        }


        VertexBuffer kOutlineSquare = new VertexBuffer( kAttributes, 4);
        // arbitrary clipping:
        for ( int i = 0; i < 4; i++ )
        {
            kOutlineSquare.Color3( 0, i, new ColorRGB( 1, 0, 0 ) );
        }
        kOutlineSquare.Position3( 0, new Vector3f( 0, 0, 0 ) );
        kOutlineSquare.Position3( 1, new Vector3f( 0, 0, m_fZ ) );
        kOutlineSquare.Position3( 2, new Vector3f( 0, m_fY, m_fZ ) );
        kOutlineSquare.Position3( 3, new Vector3f( 0, m_fY, 0 ) );
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
        Vector3f kCRight = kCDir.Cross(kCUp);
        Vector3f kCLoc = kCDir.scale(-4.0f);
        m_spkEyeCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);

        for ( int i = 0; i < 4; i++ )
        {
            kOutlineSquare.Color3( 0, i, new ColorRGB( 1, 0, 0 ) );
        }
        kOutlineSquare.Position3( 0, new Vector3f( -.2f, -.2f, m_fZ ) );
        kOutlineSquare.Position3( 1, new Vector3f( m_fX +.2f, -.2f, m_fZ ) );
        kOutlineSquare.Position3( 2, new Vector3f( m_fX +.2f, m_fY +.2f, m_fZ ) );
        kOutlineSquare.Position3( 3, new Vector3f( -.2f, m_fY +.2f, m_fZ ) );
        m_kClipEye = new Polyline( new VertexBuffer(kOutlineSquare), true, true );
        m_kClipEye.Local.SetTranslate(m_kTranslate);
        m_kClipEye.AttachEffect( m_spkVertexColor3Shader );
        m_kClipEye.UpdateGS();
        m_kClipEye.UpdateRS();
        m_pkRenderer.LoadResources(m_kClipEye);

        for ( int i = 0; i < 4; i++ )
        {
            kOutlineSquare.Color3( 0, i, new ColorRGB( 1, 0, 0 ) );
        }
        kOutlineSquare.Position3( 0, new Vector3f( -.2f, -.2f, 1.0f ) );
        kOutlineSquare.Position3( 1, new Vector3f( m_fX +.2f, -.2f, 1.0f ) );
        kOutlineSquare.Position3( 2, new Vector3f( m_fX +.2f, m_fY +.2f, 1.0f ) );
        kOutlineSquare.Position3( 3, new Vector3f( -.2f, m_fY +.2f, 1.0f ) );
        m_kClipEyeInv = new Polyline( new VertexBuffer(kOutlineSquare), true, true );
        m_kClipEyeInv.Local.SetTranslate(m_kTranslate);
        m_kClipEyeInv.AttachEffect( m_spkVertexColor3Shader );
        m_kClipEyeInv.UpdateGS();
        m_kClipEyeInv.UpdateRS();
        m_pkRenderer.LoadResources(m_kClipEyeInv);

        float[] afData = 
            new float[] { 0, m_fX, 0, m_fY, 0, m_fZ };
        m_kVolumeShaderEffect.ResetClip(afData);
    }


    public void setClipPlane( int iWhich, float fValue, String cmd )
    {
        fValue -= 1;
        if ( iWhich < 2 )
        {
            fValue = fValue * m_kImageA.getFileInfo(0).getResolutions()[0];
        }
        else if ( iWhich < 4 )
        {
            fValue = fValue * m_kImageA.getFileInfo(0).getResolutions()[1];
        }
        else
        {
            fValue = fValue * m_kImageA.getFileInfo(0).getResolutions()[2];
        }
        fValue /= m_fMax;
        
        for ( int i = 0; i < 4; i++ )
        {
            Vector3f kPos = m_akPolyline[iWhich].VBuffer.Position3( i );
            if ( iWhich < 2 )
            {
                kPos.X(fValue);
            }
            else if ( iWhich < 4 )
            {
                kPos.Y(fValue);
            }
            else
            {
                kPos.Z(fValue);
            }
            m_akPolyline[iWhich].VBuffer.Position3( i, kPos );
        }
        m_akPolyline[iWhich].VBuffer.Release();

        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();

        float[] data = new float[4];
        data[0] = fValue;
        m_kVolumeShaderEffect.SetClip(iWhich, data);
    }

    public void displayClipPlane( int iWhich, boolean bDisplay, ColorRGB kColor )
    {
        setClipPlaneColor( iWhich, kColor );
        displayClipPlane( iWhich, bDisplay );
    }

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

    public void DisplayOrientationCube( boolean bDisplay )
    {
        m_bDisplayOrientationCube = bDisplay;
    }

    public void SetBoundingBoxColor( ColorRGB kColor )
    {
        for ( int i = 0; i < 6; i++ )
        {
            for ( int j = 0; j < 4; j++ )
            {
                m_akBoundingBox[i].VBuffer.Color3( 0, j, kColor );
            }
            m_akBoundingBox[i].VBuffer.Release();
        }
    }


    public boolean getDisplayClipPlane( int iWhich )
    {
        return m_abDisplayPolyline[iWhich];
    }

    public void enableClipPlane( int iWhich, boolean bEnable, boolean bDisplay )
    {
        displayClipPlane( iWhich, bDisplay );

        float fValue = 0;
        if ( bEnable )
        {
            if ( iWhich < 2 )
            {
                fValue = m_akPolyline[iWhich].VBuffer.Position3( 0 ).X();
            }
            else if ( iWhich < 4 )
            {
                fValue = m_akPolyline[iWhich].VBuffer.Position3( 0 ).Y();
            }
            else
            {
                fValue = m_akPolyline[iWhich].VBuffer.Position3( 0 ).Z();
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

    public void enableEyeClipPlane( boolean bEnable, boolean bDisplay, ColorRGB kColor )
    {
        setEyeColor(kColor);
        m_bDisplayClipEye = bDisplay;
        if ( !bEnable )
        {
            setEyeClipPlane(0, bDisplay);
        }
    }

    public void enableEyeInvClipPlane( boolean bEnable, boolean bDisplay, ColorRGB kColor )
    {
        setEyeInvColor(kColor);
        m_bDisplayClipEyeInv = bDisplay;
        if ( !bEnable )
        {
            setEyeInvClipPlane(m_kImageA.getExtents()[2] - 1, bDisplay);
        }
    }

    public void setEyeClipPlane( float f4, boolean bDisplay )
    {
        f4 -= 1;
        f4 *= m_kImageA.getResolutions(0)[2];
        f4 /= m_fMax;

        Vector4f kEyeClipV = new Vector4f(0,0,1,f4);
        
        float[] afEquation = new float[4];
        afEquation[0] = kEyeClipV.X();
        afEquation[1] = kEyeClipV.Y();
        afEquation[2] = kEyeClipV.Z();;
        afEquation[3] = kEyeClipV.W();


        float fZ = kEyeClipV.W();
        
        m_kClipEye.VBuffer.Position3( 0, new Vector3f( -.2f, -.2f, fZ ) );
        m_kClipEye.VBuffer.Position3( 1, new Vector3f( m_fX +.2f, -.2f, fZ ) );
        m_kClipEye.VBuffer.Position3( 2, new Vector3f( m_fX +.2f, m_fY +.2f, fZ ) );
        m_kClipEye.VBuffer.Position3( 3, new Vector3f( -.2f, m_fY +.2f, fZ ) );
        m_kClipEye.VBuffer.Release();

        m_kClipEye.UpdateGS();
        m_kClipEye.UpdateRS();

        m_kVolumeShaderEffect.SetClipEye(afEquation);
        m_bDisplayClipEye = bDisplay;
    }

    public void setEyeInvClipPlane( float f4, boolean bDisplay )
    {
        f4 -= 1;
        f4 *= m_kImageA.getResolutions(0)[2];
        f4 /= m_fMax;
        Vector4f kEyeClipV = new Vector4f(0,0,1,f4);

        float[] afEquation = new float[4];
        afEquation[0] = kEyeClipV.X();
        afEquation[1] = kEyeClipV.Y();
        afEquation[2] = kEyeClipV.Z();;
        afEquation[3] = kEyeClipV.W();


        float fZ = kEyeClipV.W();
        
        m_kClipEyeInv.VBuffer.Position3( 0, new Vector3f( -.2f, -.2f, fZ ) );
        m_kClipEyeInv.VBuffer.Position3( 1, new Vector3f( m_fX +.2f, -.2f, fZ ) );
        m_kClipEyeInv.VBuffer.Position3( 2, new Vector3f( m_fX +.2f, m_fY +.2f, fZ ) );
        m_kClipEyeInv.VBuffer.Position3( 3, new Vector3f( -.2f, m_fY +.2f, fZ ) );
        m_kClipEyeInv.VBuffer.Release();

        m_kClipEyeInv.UpdateGS();
        m_kClipEyeInv.UpdateRS();

        m_kVolumeShaderEffect.SetClipEyeInv(afEquation);
        m_bDisplayClipEyeInv = bDisplay;
    }

    
    public void enableArbitraryClipPlane( boolean bEnable, boolean bDisplay, ColorRGB kColor )
    {
        setArbColor(kColor);
        displayArbitraryClipPlane(bDisplay);
        if ( !bEnable )
        {
            setArbitraryClipPlane((float)(m_kImageA.getExtents()[0]-1));
        }
    }

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
        

    public void setArbitraryClipPlane( float f4 )
    {
        f4 -= 1;
        f4 *= m_kImageA.getResolutions(0)[0];
        f4 /= m_fMax;
        m_kArbitraryClip = new Vector4f(1,0,0,f4);
        doClip();
    }
    
    private void doClip() 
    {        
        // update position of the bounding box:
        m_kClipArb.Local.SetTranslate( m_kTranslate.add( new Vector3f( m_kArbitraryClip.W(), 0, 0 ) ) );

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
        //kPos.addEquals(m_kTranslate);
        kPos.subEquals( new Vector3f( .5f, .5f, .5f ));
        kPos = kClipRotate.mult(kPos);
        //kPos.subEquals( m_kTranslate );
        kPos.addEquals( new Vector3f( .5f, .5f, .5f ));
        afEquation[3] = kNormal.Dot(kPos);

        // Update shader with rotated normal and distance:
        m_kVolumeShaderEffect.SetClipArb(afEquation);
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();
    }

    public void setClipPlaneColor( int iWhich, ColorRGB kColor )
    {
        kColor.R( (float)(kColor.R()/255.0) );
        kColor.G( (float)(kColor.G()/255.0) );
        kColor.B( (float)(kColor.B()/255.0) );

        for ( int i = 0; i < 4; i++ )
        {
            m_akPolyline[iWhich].VBuffer.Color3( 0, i, kColor );
        }
        m_akPolyline[iWhich].VBuffer.Release();
    }

    public void setEyeColor( ColorRGB kColor )
    {
        kColor.R( (float)(kColor.R()/255.0) );
        kColor.G( (float)(kColor.G()/255.0) );
        kColor.B( (float)(kColor.B()/255.0) );
       for ( int i = 0; i < 4; i++ )
        {
            m_kClipEye.VBuffer.Color3( 0, i, kColor );
        }
        m_kClipEye.VBuffer.Release();
    }
    
    public void setArbColor( ColorRGB kColor )
    {
        kColor.R( (float)(kColor.R()/255.0) );
        kColor.G( (float)(kColor.G()/255.0) );
        kColor.B( (float)(kColor.B()/255.0) );
       for ( int i = 0; i < 4; i++ )
        {
           m_kClipArb.VBuffer.Color3( 0, i, kColor );
        }
       m_kClipArb.VBuffer.Release();
    }
    
    public void setEyeInvColor( ColorRGB kColor )
    {
        kColor.R( (float)(kColor.R()/255.0) );
        kColor.G( (float)(kColor.G()/255.0) );
        kColor.B( (float)(kColor.B()/255.0) );
       for ( int i = 0; i < 4; i++ )
        {
            m_kClipEyeInv.VBuffer.Color3( 0, i, kColor );
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
        if ( !m_kSculptor.getEnable() )
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

    public void transformUpdate( float[] data )
    {
        Matrix3f kRotate = new Matrix3f( -1, 0, 0,
                                         0,  1, 0,
                                         0,  0, 1 );
        Matrix3f kIn = new Matrix3f(data[0], data[1], data[2],
                                    data[4], data[5], data[6],
                                    data[8], data[9], data[10]);

        Matrix3f kLocal = kRotate.mult(kIn.mult(kRotate));

        m_spkMotionObject.Local.SetMatrix(kLocal);
        m_spkScene.UpdateGS();
    }

    public void reloadShaders()
    {
        m_kVolumeShaderEffect.Reload( m_pkRenderer );
        m_kVolumeShaderEffect.MIPMode(0, m_pkRenderer);

        m_kShaderParamsWindow.AddUserVariables(m_kVolumeShaderEffect.GetPProgram());
        m_kShaderParamsWindow.Display();
        m_kShaderParamsWindow.setParent(this);
    }

    public void setVisible( boolean bVisible )
    {
        m_bVisible = bVisible;
    }

    public void updateLighting(GeneralLight[] akGLights )
    {
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

    public void SetMaterialState( MaterialState kMaterial )
    {
        m_kMesh.DetachGlobalState(GlobalState.StateType.MATERIAL);
        m_kMaterial = kMaterial;
        m_kMesh.AttachGlobalState(m_kMaterial);
        m_kMesh.UpdateMS(true);
        m_kMesh.UpdateRS();
    }

    public void setRGBT(ModelRGB RGBT, int iImage)
    {
        if ( m_kVolumeShaderEffect == null )
        {
            m_kRGBTa = RGBT;
            return;
        }
        m_kVolumeShaderEffect.SetRGBT( RGBT, iImage );
    }

    public void MIPMode( int iImage )
    {
        m_kVolumeShaderEffect.MIPMode( iImage, m_pkRenderer );
        ResetShaderParamsWindow();
    }

    public void DDRMode( int iImage )
    {
        m_kVolumeShaderEffect.DDRMode( iImage, m_pkRenderer );
        ResetShaderParamsWindow();
    }

    public void CMPMode( int iImage )
    {
        m_kVolumeShaderEffect.CMPMode( iImage, m_pkRenderer );
        ResetShaderParamsWindow();
    }

    public void SURMode( int iImage )
    {
        m_kVolumeShaderEffect.SURMode( iImage, m_pkRenderer );
        ResetShaderParamsWindow();
    }

    public void SURFASTMode( int iImage )
    {
        m_kVolumeShaderEffect.SURFASTMode( iImage, m_pkRenderer );
        ResetShaderParamsWindow();
    }

    private void ResetShaderParamsWindow()
    {
        if ( m_kShaderParamsWindow != null )
        {
            m_kShaderParamsWindow.close();
        }
    }

    public void Blend( float fValue )
    {
        m_kVolumeShaderEffect.Blend(fValue);
    }
    
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
    
    public void SetBackgroundColor( ColorRGBA kColor )
    {
        m_kBackgroundColor = kColor;
        m_kVolumeShaderEffect.SetBackgroundColor( kColor );
    }



    /**
     * Create the rotation control cubic box.
     *
     * @return A cube representing the image orientation, with labels painted
     * on the cube faces showing which axis corresponds to which axis in
     * patient coordinates.
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
                //System.err.println( aakAxisFiles[i][0] + " " + aakAxisFiles[i][1] );
            }
        }
    }




    private Node m_spkScene;
    private WireframeState m_spkWireframe;
    private CullState m_spkCull;
    private Culler m_kCuller = new Culler(0,0,null);

    private Polyline[] m_akBoundingBox;
    private boolean m_bDisplayBoundingBox = false;

    private TriMesh[] m_akOrientationCube;
    private boolean m_bDisplayOrientationCube = false;
    private String[] m_aakAxisFiles = new String[]{ "u", "u", "u", "u", "u", "u"};
    private Vector3f m_kCubeTranslate = Vector3f.ZERO;

    private Polyline[] m_akPolyline;
    private Polyline m_kClipArb;
    private Polyline m_kClipEye;
    private Polyline m_kClipEyeInv;
    private boolean[] m_abDisplayPolyline = new boolean[]{false,false,false,false,false,false};

    private TriMesh m_spkMesh;
    private VolumeShaderEffect m_kVolumeShaderEffect = null;

    private ModelImage m_kImageA = null;
    private ModelLUT m_kLUTa = null;
    private ModelRGB m_kRGBTa = null;

    private ModelImage m_kImageB = null;
    private ModelLUT m_kLUTb = null;
    private ModelRGB m_kRGBTb = null;

    private Camera m_spkScreenCamera;
    private Camera m_spkEyeCamera;
    private TriMesh m_spkScenePolygon;
    private GraphicsImage m_spkSceneImage;
    private Texture m_pkSceneTarget;
    private OpenGLFrameBuffer m_pkPBuffer;

    private ShaderEffect m_spkVertexColor3Shader;

    private Animator m_kAnimator;

    private SculptorWm m_kSculptor;

    private TransferFunction[] m_akTransfer = new TransferFunction[2];

    private boolean m_bInit = false;
    private Vector3f m_kTranslate;
    private Vector4f m_kArbitraryClip;
    private boolean m_bDisplayClipArb = false;
    private boolean m_bDisplayClipEye = false;
    private boolean m_bDisplayClipEyeInv = false;
    private float m_fX, m_fY, m_fZ, m_fMax;

    private boolean m_bVisible = true;
    private Node m_kArbRotate = new Node();

    private MaterialState m_kMaterial;
    private TriMesh m_kMesh;
    private int m_iActive = 0;

    ApplicationGUI m_kShaderParamsWindow = null;


}
