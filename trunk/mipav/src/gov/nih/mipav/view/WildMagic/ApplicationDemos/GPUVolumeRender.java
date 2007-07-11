/**
 * 
 */

package gov.nih.mipav.view.WildMagic.ApplicationDemos;

import javax.media.opengl.*;
import com.sun.opengl.util.*;
import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.WildMagic.LibApplications.OpenGLApplication.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;
import gov.nih.mipav.view.WildMagic.LibRenderers.OpenGLRenderer.*;

import gov.nih.mipav.view.renderer.SoftwareLight;
import gov.nih.mipav.view.renderer.GeneralLight;
import gov.nih.mipav.view.renderer.volumeview.*;

/**
 */
public class GPUVolumeRender extends JavaApplication3
    implements GLEventListener, KeyListener, MouseMotionListener
{
    public GPUVolumeRender( ModelImage kImage, ModelLUT kLUTa )
    {

        super("GPUVolumeRender",0,0,512,512, new ColorRGBA(0.0f,0.0f,0.0f,0.0f));

        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                                           m_eBuffering, m_eMultisampling,
                                           m_iWidth, m_iHeight );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );       

        ImageCatalog.SetActive( new ImageCatalog("Main") );      
        VertexProgramCatalog.SetActive(new VertexProgramCatalog("Main", System.getProperties().getProperty("user.dir")));       
        PixelProgramCatalog.SetActive(new PixelProgramCatalog("Main", System.getProperties().getProperty("user.dir")));
        
        m_kImage = kImage;
        m_kLUTa = kLUTa;

        m_kSculptor = new SculptorWm( ((OpenGLRenderer)m_pkRenderer).GetCanvas() );
        m_kSculptor.setImage(m_kImage);
    }

    public void finalize()
    {
        m_kAnimator.stop();
        m_kAnimator = null;

        System.err.println("GPUVolumeRender: finalize()");
        m_spkScene.finalize();
        m_spkScene = null;

        m_spkVolume.finalize();
        m_spkVolume = null;
        m_pkVolumeTarget.finalize();
        m_pkVolumeTarget = null;

        try {
            m_kSculptor.finalize();
        } catch (Throwable e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        m_kSculptor = null;
        m_kLUTa = null;
        m_kTransfer = null;

        ApplicationGUI.TheApplicationGUI.close();

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
            m_spkVolumeNode.DetachEffect( m_kVolumeShaderEffect );
            m_spkVolumeNode.AttachEffect( m_spkVertexColor3Shader );
            m_kCuller.ComputeVisibleSet(m_spkScene);

            m_pkPBuffer.Enable();
            //m_pkRenderer.SetBackgroundColor(ColorRGBA.BLACK);
            m_pkRenderer.ClearBuffers();
            m_spkCull.CullFace = CullState.CullMode.CT_FRONT;
            m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
            m_spkCull.CullFace = CullState.CullMode.CT_BACK;
            m_pkPBuffer.Disable();

            m_spkVolumeNode.DetachEffect( m_spkVertexColor3Shader );
            m_spkVolumeNode.AttachEffect( m_kVolumeShaderEffect );
            m_kCuller.ComputeVisibleSet(m_spkScene);

            // Draw the scene to the main window and also to a regular screen
            // polygon, placed in the lower-left corner of the main window.
            //m_pkRenderer.SetBackgroundColor(new ColorRGBA(0.0f,0.0f,0.0f,0.0f));
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

            /*
            m_pkRenderer.SetCamera(m_spkScreenCamera);
            m_pkRenderer.Draw(m_spkScenePolygon);

            m_pkRenderer.SetCamera(m_spkCamera);
            */
            DrawFrameRate(8,16,ColorRGBA.WHITE);

            if ( m_kSculptor.IsSculptDrawn() )
            {
                m_pkRenderer.Draw( m_kSculptor.getSculptImage() );
            }
            
            m_pkRenderer.EndScene();
        }
        m_pkRenderer.DisplayBackBuffer();
        
        UpdateFrameCount();

        ApplicationGUI.TheApplicationGUI.setParent(this);
        ApplicationGUI.TheApplicationGUI.Display();
    }

    public void displayChanged(GLAutoDrawable arg0, boolean arg1, boolean arg2) {
        // TODO Auto-generated method stub
        System.err.println("displayChanged");
    }

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
        m_spkCamera.SetFrustum(60.0f,m_iWidth/(float)m_iHeight,1.0f,1000.0f);
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
        m_spkScene.AttachChild( m_spkVolumeNode );
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
        int i;
        for (i = 0; i < m_iWidth*m_iHeight; i++)
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

        InitLUTTexture ( m_kLUTa );
        CreateVolumeTexture();

        if ( m_kTransfer != null )
        {
            updateImages(true, m_kTransfer);
        }

        int iXBound = m_kImage.getExtents()[0];
        int iYBound = m_kImage.getExtents()[1];
        int iZBound = m_kImage.getExtents()[2];
        InitClippingPlanes(iXBound,iYBound,iZBound);

        for ( i = 0; i < m_pkRenderer.GetMaxLights(); i++ )
        {
            m_pkRenderer.SetLight( i, new Light() );
        }
    }

    private void CreateVolumeTexture ()
    {
        int iXBound = m_kImage.getExtents()[0];
        int iYBound = m_kImage.getExtents()[1];
        int iZBound = m_kImage.getExtents()[2];

        updateData(m_kImage);
        
        VertexShader pkVShader = new VertexShader("VolumeShader");
        PixelShader pkPShader = new PixelShader("VolumeShader");

        byte[] aucData = calcImageNormals();
        m_spkVolume = new GraphicsImage(GraphicsImage.FormatMode.IT_RGB888,
                                        iXBound,iYBound,iZBound,aucData,
                                        "NormalMap");
        // setup mip shader effect:
        pkPShader.SetTextureQuantity(5);
        pkPShader.SetImageName(0,"VolumeImage");
        pkPShader.GetTexture(0).SetFilterType(Texture.FilterType.LINEAR);
        pkPShader.GetTexture(0).SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        pkPShader.GetTexture(0).SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        pkPShader.GetTexture(0).SetWrapType(2,Texture.WrapType.CLAMP_BORDER);
        m_pkVolumeTarget = pkPShader.GetTexture(0);
        pkPShader.SetImageName(1,"SceneImage");
        pkPShader.SetTexture(1,m_pkSceneTarget);
        pkPShader.SetImageName(2,"ColorMap");
        m_pkColorMapTarget = pkPShader.GetTexture(2);
        pkPShader.SetImageName(3,"OpacityMap");
        m_pkOpacityMapTarget = pkPShader.GetTexture(3);

        pkPShader.SetImageName(4,"NormalMap");
        pkPShader.GetTexture(4).SetFilterType(Texture.FilterType.LINEAR);
        pkPShader.GetTexture(4).SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        pkPShader.GetTexture(4).SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        pkPShader.GetTexture(4).SetWrapType(2,Texture.WrapType.CLAMP_BORDER);
        m_pkNormalMapTarget = pkPShader.GetTexture(4);

        m_kVolumeShaderEffect = new ShaderEffect(1);
        m_kVolumeShaderEffect.SetVShader(0,pkVShader);
        m_kVolumeShaderEffect.SetPShader(0,pkPShader);

        m_pkRenderer.LoadResources(m_kVolumeShaderEffect);
        Program pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
        if ( pkProgram.GetUC("Iterations") != null)
        {
            pkProgram.GetUC("Iterations").SetDataSource(new float[]{500,0,0,0});
        }
        float[] afMIP = new float[] {1,0,0,0};
        pkProgram.GetUC("MIP").SetDataSource(afMIP);
    }

    public void MIPMode()
    {
        Program pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
        if ( pkProgram.GetUC("MIP") != null )
        {
        pkProgram.GetUC("MIP").SetDataSource(new float[] {1,0,0,0});
        }
        if ( pkProgram.GetUC("DDR") != null )
        {
        pkProgram.GetUC("DDR").SetDataSource(new float[] {0,0,0,0});
        }
        if ( pkProgram.GetUC("Composite") != null )
        {
            pkProgram.GetUC("Composite").SetDataSource(new float[] {0,0,0,0});
        }
        if ( pkProgram.GetUC("Surface") != null )
        {
            pkProgram.GetUC("Surface").SetDataSource(new float[] {0,0,0,0});
        }
    }

    public void DDRMode()
    {
        Program pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
        if ( pkProgram.GetUC("MIP") != null )
        {
        pkProgram.GetUC("MIP").SetDataSource(new float[] {0,0,0,0});
        }
        if ( pkProgram.GetUC("DDR") != null )
        {
        pkProgram.GetUC("DDR").SetDataSource(new float[] {1,0,0,0});
        }
        if ( pkProgram.GetUC("Composite") != null )
        {
            pkProgram.GetUC("Composite").SetDataSource(new float[] {0,0,0,0});
        }
        if ( pkProgram.GetUC("Surface") != null )
        {
            pkProgram.GetUC("Surface").SetDataSource(new float[] {0,0,0,0});
        }
    }

    public void CMPMode()
    {
        Program pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
        if ( pkProgram.GetUC("MIP") != null )
        {
        pkProgram.GetUC("MIP").SetDataSource(new float[] {0,0,0,0});
        }
        if ( pkProgram.GetUC("DDR") != null )
        {
        pkProgram.GetUC("DDR").SetDataSource(new float[] {1,0,0,0});
        }
        if ( pkProgram.GetUC("Composite") != null )
        {
            pkProgram.GetUC("Composite").SetDataSource(new float[] {1,0,0,0});
        }
        if ( pkProgram.GetUC("Surface") != null )
        {
            pkProgram.GetUC("Surface").SetDataSource(new float[] {0,0,0,0});
        }
    }

    public void SURMode()
    {
        Program pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
        if ( pkProgram.GetUC("MIP") != null )
        {
        pkProgram.GetUC("MIP").SetDataSource(new float[] {0,0,0,0});
        }
        if ( pkProgram.GetUC("DDR") != null )
        {
        pkProgram.GetUC("DDR").SetDataSource(new float[] {1,0,0,0});
        }
        if ( pkProgram.GetUC("Composite") != null )
        {
            pkProgram.GetUC("Composite").SetDataSource(new float[] {1,0,0,0});
        }
        if ( pkProgram.GetUC("Surface") != null )
        {
            pkProgram.GetUC("Surface").SetDataSource(new float[] {1,0,0,0});
        }
        if ( pkProgram.GetUC("specularParam") != null )
        {
            pkProgram.GetUC("specularParam").SetDataSource(new float[] {1,0,0,0});
        }
        if ( pkProgram.GetUC("shininessParam") != null )
        {
            pkProgram.GetUC("shininessParam").SetDataSource(new float[] {1,0,0,0});
        }
        if ( pkProgram.GetUC("light") != null )
        {
            pkProgram.GetUC("light").SetDataSource(new float[] {0,-5,-5,0});
        }
    }

    public void SURFASTMode()
    {
        Program pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
        if ( pkProgram.GetUC("MIP") != null )
        {
        pkProgram.GetUC("MIP").SetDataSource(new float[] {0,0,0,0});
        }
        if ( pkProgram.GetUC("DDR") != null )
        {
        pkProgram.GetUC("DDR").SetDataSource(new float[] {1,0,0,0});
        }
        if ( pkProgram.GetUC("Composite") != null )
        {
            pkProgram.GetUC("Composite").SetDataSource(new float[] {0,0,0,0});
        }
        if ( pkProgram.GetUC("Surface") != null )
        {
            pkProgram.GetUC("Surface").SetDataSource(new float[] {1,0,0,0});
        }
        if ( pkProgram.GetUC("specularParam") != null )
        {
            pkProgram.GetUC("specularParam").SetDataSource(new float[] {1,0,0,0});
        }
        if ( pkProgram.GetUC("shininessParam") != null )
        {
            pkProgram.GetUC("shininessParam").SetDataSource(new float[] {1,0,0,0});
        }
        if ( pkProgram.GetUC("light") != null )
        {
            pkProgram.GetUC("light").SetDataSource(new float[] {0,-5,-5,0});
        }
    }

    public boolean updateImages(boolean forceShow, TransferFunction kTransfer)
    {
        if ( m_spkOpacityMap == null )
        {
            m_kTransfer = kTransfer;
            return false;
        }

//         int nPts = kTransfer.size();
//         int lutHeight = 256;

//         float[] afData = new float[nPts];
//         float xMax = ((Point2Df) (kTransfer.getPoint(nPts-1))).x;
//         float xMin = ((Point2Df) (kTransfer.getPoint(0))).x;
//         float fNew;
//         for (int i = 0; i < nPts; i++) {
//             fNew = (float) (xMin + (((float) i / (nPts - 1)) * (xMax - xMin)));
//             afData[i] = kTransfer.getRemappedValue(fNew, lutHeight) + 0.5f;
//             System.err.println( afData[i] );
//         }
//         m_spkOpacityMap.SetFloatData(afData, nPts);



//         int nPts = kTransfer.size();
//         System.err.print("Opacity Transfer = ");
//         //for (int i = 0; i < nPts; i++) {
//             System.err.println(((Point2Df)(kTransfer.getPoint(1))).x + " " +
//                                ((Point2Df)(kTransfer.getPoint(1))).y );
//             //}

         int iLutHeight = 256;
         float[] afData = m_spkOpacityMap.GetFloatData();

         float fRange = m_fImageMax - m_fImageMin;
         float fStep = fRange / (float)iLutHeight;
         float fDataValue = m_fImageMin;
         for (int i = 0; i < iLutHeight; i++) {
             afData[i] = (kTransfer.getRemappedValue( fDataValue, iLutHeight )/255.0f);
//             //System.err.println(afData[i]);
             fDataValue += fStep;
         }
         m_pkOpacityMapTarget.Reload(true);
        return true;
    }

    /**
     * This methods calls corresponding render to update images with LUT changes.
     *
     * @param   LUTa        LUT used to update imageA
     * @param   LUTb        LUT used to update imageB
     * @param   forceShow   forces show to reimport image and calc. java image
     * @param   interpMode  image interpolation method (Nearest or Smooth)
     *
     * @return  boolean confirming successful update
     */
    public boolean updateImages(ModelLUT kLUTa, ModelLUT LUTb, boolean forceShow, int interpMode)
    {
        byte[] aucData = kLUTa.exportIndexedLUTMin();
        m_spkColorMap.SetData(aucData, aucData.length/4);
        m_pkColorMapTarget.Reload(true);
        return true;
    }

    public void InitLUTTexture ( ModelLUT kLUTa )
    {
        byte[] aucData = kLUTa.exportIndexedLUTMin();
        m_spkColorMap = new GraphicsImage(
                                          GraphicsImage.FormatMode.IT_RGBA8888,aucData.length/4,aucData,
                                          "ColorMap");



        int iLutHeight = 256;
        float[] afData = new float[iLutHeight];
        float fRange = m_fImageMax - m_fImageMin;
        float fStep = fRange / (float)iLutHeight;
        float fDataValue = m_fImageMin;
        for (int i = 0; i < iLutHeight; i++) {
            afData[i] = ( iLutHeight * (m_fImageMax - fDataValue) / fRange);
            fDataValue += fStep;
        }

        m_spkOpacityMap = new GraphicsImage(
                              GraphicsImage.FormatMode.IT_L8,iLutHeight,afData,
                                                                                           "OpacityMap");
    }



    private void CreateBox ()
    {
        m_spkVolumeNode = new Node();
        int iXBound = m_kImage.getExtents()[0];
        int iYBound = m_kImage.getExtents()[1];
        int iZBound = m_kImage.getExtents()[2];
        m_spkVolumeNode.AttachChild(Box(iXBound,iYBound,iZBound));
        
        m_spkVertexColor3Shader = new VertexColor3Effect();
    }

    private TriMesh Box (int iXBound, int iYBound, int iZBound)
    {
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetCChannels(0,3);
        kAttr.SetTChannels(0,3);
        //kAttr.SetTChannels(1,3);

        float fMaxX = (float) (iXBound - 1) * m_kImage.getFileInfo(0).getResolutions()[0];
        float fMaxY = (float) (iYBound - 1) * m_kImage.getFileInfo(0).getResolutions()[1];
        float fMaxZ = (float) (iZBound - 1) * m_kImage.getFileInfo(0).getResolutions()[2];

        float fMax = fMaxX;
        if (fMaxY > fMax) {
            fMax = fMaxY;
        }
        if (fMaxZ > fMax) {
            fMax = fMaxZ;
        }
        m_fX = fMaxX/fMax;
        m_fY = fMaxY/fMax;
        m_fZ = fMaxZ/fMax;


        int iVQuantity = 24;
        int iTQuantity = 12;
        VertexBuffer pkVB = new VertexBuffer(kAttr,iVQuantity);
        IndexBuffer pkIB = new IndexBuffer(3*iTQuantity);

        // generate connectivity (outside view)
        int i = 0;
        int[] aiIndex = pkIB.GetData();

        System.err.println( m_fX + " " + m_fY + " " + m_fZ );


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
        m_kMaterial.Ambient = new ColorRGB(0.24725f,0.2245f,0.0645f);
        m_kMaterial.Diffuse = new ColorRGB(0.34615f,0.3143f,0.0903f);
        m_kMaterial.Specular = new ColorRGB(0.797357f,0.723991f,0.208006f);
        m_kMaterial.Shininess = 83.2f;
        m_kMesh.AttachGlobalState(m_kMaterial);
        m_kMesh.UpdateMS(true);
        return m_kMesh;
    }


    public byte[] calcImageNormals() {

        ModelSimpleImage kValueImageA;
        float[] afData;

        // Extract image slice.
        ModelSimpleImage kSimpleImageA = new ModelSimpleImage(m_kImage, 0);

        // Convert to intensity valued image.
        if (m_kImage.isColorImage()) {
            kValueImageA = kSimpleImageA.createIntensityImage();
            afData = kValueImageA.data;
        } else {
            afData = kSimpleImageA.data;
        }
        kSimpleImageA.calcMinMax();
        float fMax = kSimpleImageA.max;
        float fMin = kSimpleImageA.min;

        kSimpleImageA = null;
        System.gc();

        // Access intensity values as a linear array.

        int iXBound = m_kImage.getExtents()[0];;
        int iYBound = m_kImage.getExtents()[1];;
        int iZBound = m_kImage.getExtents()[2];;
        int iXYBound = iXBound * iYBound;

        // normals from gradient which are computed using central finite
        // differences everywhere except forward/backward finite differences
        // are used at the edges

        float fDX = 0;
        float fDY = 0;
        float fDZ = 0;

        int iOffX = 1;
        int iOffY = iXBound;
        int iOffZ = iXBound * iYBound;
        int iX, iY, iZ;

        float[] afDataN = new float[afData.length*3];

        for (iZ = 1; iZ < (iZBound - 1); iZ++) {
            boolean bMinZ = 0 == iZ;
            boolean bMaxZ = (iZBound - 1) == iZ;

            for (iY = 1; iY < (iYBound - 1); iY++) {
                boolean bMinY = 0 == iY;
                boolean bMaxY = (iYBound - 1) == iY;
                int offset = iXBound * (iY + (iYBound * iZ));

                for (iX = 0; iX < iXBound; iX++) {
                    boolean bMinX = 0 == iX;
                    boolean bMaxX = (iXBound - 1) == iX;

                    int i = iX + offset;

                    fDX = (((bMinX ? afData[i] : afData[i - iOffX - iXBound]) -
                            (bMaxX ? afData[i] : afData[i + iOffX - iXBound])) * 0.71f) +
                            
                        (bMinX ? afData[i] : afData[i - iOffX]) - (bMaxX ? afData[i] : afData[i + iOffX]) +
                        (
                             
                         ((bMinX ? afData[i] : afData[i - iOffX + iXBound]) -
                          (bMaxX ? afData[i] : afData[i + iOffX + iXBound])) * 0.71f);
                        
                    fDY = (((bMinY ? afData[i] : afData[i - iOffY - 1]) - (bMaxY ? afData[i] : afData[i + iOffY - 1])) *
                           0.71f) +
                            
                        (bMinY ? afData[i] : afData[i - iOffY]) - (bMaxY ? afData[i] : afData[i + iOffY]) +
                        (
                             
                         ((bMinY ? afData[i] : afData[i - iOffY + 1]) - (bMaxY ? afData[i] : afData[i + iOffY + 1])) * 0.71f);
                        
                    fDZ = (((bMinZ ? afData[i] : afData[i - iOffZ - 1]) - (bMaxZ ? afData[i] : afData[i + iOffZ - 1])) *
                           0.71f) +
                            
                        (bMinZ ? afData[i] : afData[i - iOffZ]) - (bMaxZ ? afData[i] : afData[i + iOffZ]) +
                        (
                             
                         ((bMinZ ? afData[i] : afData[i - iOffZ + 1]) - (bMaxZ ? afData[i] : afData[i + iOffZ + 1])) * 0.71f);
                        
                        
                    if ((fDX != 0.0f) || (fDY != 0.0f) || (fDZ != 0.0f)) {
                        afDataN[i*3+0] = fDX;
                        afDataN[i*3+1] = fDY;
                        afDataN[i*3+2] = fDZ;
//                         acData[i*3+0] = (byte)(fDX*127 + 127);
//                         acData[i*3+1] = (byte)(fDY*127 + 127);
//                         acData[i*3+2] = (byte)(fDZ*127 + 127);
                    }
                }
            }
        }

        int[] aiNormalAverageIndex = new int[]{ 0, -1, +1, -iXBound, +iXBound, -iXYBound, +iXYBound };
        Vector3f kNormal = new Vector3f();
        Vector3f kNormalTmp = new Vector3f();
        byte[] acData = new byte[afData.length*3];
                    
        // Catch any zero-vector normals and replace them by an average of
        // neighboring normals.
        for (iZ = 1; iZ < (iZBound - 1); iZ++) {

            for (iY = 1; iY < (iYBound - 1); iY++) {
                int offset = iXBound * (iY + (iYBound * iZ));

                for (iX = 1; iX < (iXBound - 1); iX++) {
                    int i = iX + offset;

                    kNormal.copy(Vector3f.ZERO);
                    for ( int iN = 0; iN < aiNormalAverageIndex.length; iN++ )
                    {
                        int index = i + aiNormalAverageIndex[iN];
                        index *= 3;
                        kNormalTmp.X(afDataN[index + 0]);
                        kNormalTmp.Y(afDataN[index + 1]);
                        kNormalTmp.Z(afDataN[index + 2]);

                        kNormal.addEquals( kNormalTmp );
                    }
                    kNormal.Normalize();
                    acData[i*3+0] = (byte)(kNormal.X()*127 + 127);
                    acData[i*3+1] = (byte)(kNormal.Y()*127 + 127);
                    acData[i*3+2] = (byte)(kNormal.Z()*127 + 127);
                }
            }
        }
        aiNormalAverageIndex = null;
        kNormal = null;
        kNormalTmp = null;
        afDataN = null;

        return acData;
    }



    public void keyPressed(KeyEvent e) {
        char ucKey = e.getKeyChar();
        
        super.keyPressed(e);
        float[] afEquation;
        Program pkProgram;
        switch (ucKey)
        {
        case 'w':
        case 'W':
            m_spkWireframe.Enabled = !m_spkWireframe.Enabled;
            return;
        case 's':
        case 'S':
             TestStreaming(m_spkScene,"VolumeTextures.wmof");
             return;

        case 'v':
            m_kNormal.W( (float)(m_kNormal.W() + 0.1) );
            afEquation = new float[4];
            afEquation[0] = m_kNormal.X();
            afEquation[1] = m_kNormal.Y();
            afEquation[2] = m_kNormal.Z();
            afEquation[3] = m_kNormal.W();

            System.err.println( "Normal: " + 
                                m_kNormal.X() + " " +
                                m_kNormal.Y() + " " +
                                m_kNormal.Z() + " " +
                                m_kNormal.W() + " " +
                                m_kArbitraryClip.W()
                                );
            // Update shader with rotated normal and distance:
            pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
            if ( pkProgram.GetUC("clipArb") != null ) 
            {
                pkProgram.GetUC("clipArb").SetDataSource(afEquation);
            }
            m_spkScene.UpdateGS();
            m_spkScene.UpdateRS();
            return;
        case 'V':
            m_kNormal.W( (float)(m_kNormal.W() - 0.1) );
            afEquation = new float[4];
            afEquation[0] = m_kNormal.X();
            afEquation[1] = m_kNormal.Y();
            afEquation[2] = m_kNormal.Z();
            afEquation[3] = m_kNormal.W();

            System.err.println( "Normal: " + 
                                m_kNormal.X() + " " +
                                m_kNormal.Y() + " " +
                                m_kNormal.Z() + " " +
                                m_kNormal.W() + " " +
                                m_kArbitraryClip.W() );
            // Update shader with rotated normal and distance:
            pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
            if ( pkProgram.GetUC("clipArb") != null ) 
            {
                pkProgram.GetUC("clipArb").SetDataSource(afEquation);
            }
            m_spkScene.UpdateGS();
            m_spkScene.UpdateRS();
            return;

        case 'o':
            setOrthographicProjection();
            return;
        case 'p':
            setPerspectiveProjection();
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
        if ( m_pkVolumeTarget.GetImage().GetData() != null )
        {
            m_kSculptor.setTextureImageData( m_pkVolumeTarget.GetImage().GetData() );
        }
        else
        {
            m_kSculptor.setTextureImageFloatData( m_pkVolumeTarget.GetImage().GetFloatData() );
        }
        m_kSculptor.undoSculpt();
        m_pkVolumeTarget.Release();
    }

    public void applySculpt()
    {
        float[] afData = new float[16];
        m_pkRenderer.SetConstantWVPMatrix (0, afData);
        m_kSculptor.setWVPMatrix(new Matrix4f(afData, true));
        if ( m_pkVolumeTarget.GetImage().GetData() != null )
        {
            m_kSculptor.setTextureImageData( m_pkVolumeTarget.GetImage().GetData() );
        }
        else
        {
            m_kSculptor.setTextureImageFloatData( m_pkVolumeTarget.GetImage().GetFloatData() );
        }
        if ( m_kSculptor.applySculpt() )
        {
            m_pkVolumeTarget.Release();
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
        m_kImage = kImage;
        m_kSculptor.setImage(m_kImage);

        int iXBound = m_kImage.getExtents()[0];
        int iYBound = m_kImage.getExtents()[1];
        int iZBound = m_kImage.getExtents()[2];
        m_kImage.calcMinMax();
        m_fImageMax = (float)m_kImage.getMax();
        m_fImageMin = (float)m_kImage.getMin();

        if ( m_kImage.isColorImage() )
        {

            byte[] aucData = new byte[3*iXBound*iYBound*iZBound];

            int i = 0;
            for (int iZ = 0; iZ < iZBound; iZ++)
            {
                for (int iY = 0; iY < iYBound; iY++)
                {
                    for (int iX = 0; iX < iXBound; iX++)
                    {
                        aucData[i++] = (byte)(255.0f*m_kImage.getFloatC(iX,iY,iZ,0));
                        aucData[i++] = (byte)(255.0f*m_kImage.getFloatC(iX,iY,iZ,1));
                        aucData[i++] = (byte)(255.0f*m_kImage.getFloatC(iX,iY,iZ,2));
                    }
                }
            }
            if ( m_spkVolume == null )
            {
                m_spkVolume = new GraphicsImage(
                                                GraphicsImage.FormatMode.IT_RGB888,iXBound,iYBound,iZBound,aucData,
                                                "VolumeImage");
            }
            if ( m_pkVolumeTarget != null )
            {
                m_pkVolumeTarget.GetImage().SetData( aucData, iXBound, iYBound, iZBound );
                m_pkVolumeTarget.Release();
            }
        }
        else
        {
            float[] afData = new float[iXBound*iYBound*iZBound];

            int i = 0;
            for (int iZ = 0; iZ < iZBound; iZ++)
            {
                for (int iY = 0; iY < iYBound; iY++)
                {
                    for (int iX = 0; iX < iXBound; iX++)
                    {
                        float fValue = m_kImage.getFloat(iX,iY,iZ);
                        afData[i++] = (fValue - m_fImageMin)/(m_fImageMax - m_fImageMin);
                    }
                }
            }
            if ( m_spkVolume == null )
            {
                m_spkVolume = new GraphicsImage(
                                                GraphicsImage.FormatMode.IT_L8,iXBound,iYBound,iZBound,afData,
                                                "VolumeImage");
            }

            if ( m_pkVolumeTarget != null )
            {
                m_pkVolumeTarget.GetImage().SetFloatData( afData, iXBound, iYBound, iZBound );
                m_pkVolumeTarget.Release();
            }
        }
    }

    private void InitClippingPlanes(int iXBound, int iYBound, int iZBound)
    {
        m_akPolyline = new Polyline[6];

        float fMaxX = (float) (iXBound - 1) * m_kImage.getFileInfo(0).getResolutions()[0];
        float fMaxY = (float) (iYBound - 1) * m_kImage.getFileInfo(0).getResolutions()[1];
        float fMaxZ = (float) (iZBound - 1) * m_kImage.getFileInfo(0).getResolutions()[2];

        float fMax = fMaxX;
        if (fMaxY > fMax) {
            fMax = fMaxY;
        }
        if (fMaxZ > fMax) {
            fMax = fMaxZ;
        }
        m_fX = fMaxX/fMax;
        m_fY = fMaxY/fMax;
        m_fZ = fMaxZ/fMax;

        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);

        // neg x clipping:
        VertexBuffer kClipView = new VertexBuffer(kAttributes, 4 );
        for ( int i = 0; i < 4; i++ )
        {
            kClipView.Color3( 0, i, new ColorRGB( 1, 0, 0 ) );
        }
        kClipView.Position3( 0, new Vector3f( 0, 0, 0 ) );
        kClipView.Position3( 1, new Vector3f( 0, 0, m_fZ ) );
        kClipView.Position3( 2, new Vector3f( 0, m_fY, m_fZ ) );
        kClipView.Position3( 3, new Vector3f( 0, m_fY, 0 ) );
        m_akPolyline[0] = new Polyline( kClipView, true, true );
        m_akPolyline[0].AttachEffect( m_spkVertexColor3Shader );
        m_akPolyline[0].Local.SetTranslate(m_kTranslate);

        // pos x clipping:
        kClipView = new VertexBuffer(kAttributes, 4 );
        for ( int i = 0; i < 4; i++ )
        {
            kClipView.Color3( 0, i, new ColorRGB( 1, 0, 0 ) );
        }
        kClipView.Position3( 0, new Vector3f( m_fX, 0, 0 ) );
        kClipView.Position3( 1, new Vector3f( m_fX, 0, m_fZ ) );
        kClipView.Position3( 2, new Vector3f( m_fX, m_fY, m_fZ ) );
        kClipView.Position3( 3, new Vector3f( m_fX, m_fY, 0 ) );
        m_akPolyline[1] = new Polyline( kClipView, true, true );
        m_akPolyline[1].AttachEffect( m_spkVertexColor3Shader );
        m_akPolyline[1].Local.SetTranslate(m_kTranslate);

        // neg y clipping:
        kClipView = new VertexBuffer(kAttributes, 4 );
        for ( int i = 0; i < 4; i++ )
        {
            kClipView.Color3( 0, i, new ColorRGB( 1, 0, 0 ) );
        }
        kClipView.Position3( 0, new Vector3f( 0, 0, 0 ) );
        kClipView.Position3( 1, new Vector3f( 0, 0, m_fZ ) );
        kClipView.Position3( 2, new Vector3f( m_fX, 0, m_fZ ) );
        kClipView.Position3( 3, new Vector3f( m_fX, 0, 0 ) );
        m_akPolyline[2] = new Polyline( kClipView, true, true );
        m_akPolyline[2].AttachEffect( m_spkVertexColor3Shader );
        m_akPolyline[2].Local.SetTranslate(m_kTranslate);

        // pos y clipping:
        kClipView = new VertexBuffer(kAttributes, 4 );
        for ( int i = 0; i < 4; i++ )
        {
            kClipView.Color3( 0, i, new ColorRGB( 1, 0, 0 ) );
        }
        kClipView.Position3( 0, new Vector3f( 0, m_fY, 0 ) );
        kClipView.Position3( 1, new Vector3f( 0, m_fY, m_fZ ) );
        kClipView.Position3( 2, new Vector3f( m_fX, m_fY, m_fZ ) );
        kClipView.Position3( 3, new Vector3f( m_fX, m_fY, 0 ) );
        m_akPolyline[3] = new Polyline( kClipView, true, true );
        m_akPolyline[3].AttachEffect( m_spkVertexColor3Shader );
        m_akPolyline[3].Local.SetTranslate(m_kTranslate);


        // neg z clipping:
        kClipView = new VertexBuffer(kAttributes, 4 );
        for ( int i = 0; i < 4; i++ )
        {
            kClipView.Color3( 0, i, new ColorRGB( 1, 0, 0 ) );
        }
        kClipView.Position3( 0, new Vector3f( 0, 0, 0 ) );
        kClipView.Position3( 1, new Vector3f( 0, m_fY, 0 ) );
        kClipView.Position3( 2, new Vector3f( m_fX, m_fY, 0 ) );
        kClipView.Position3( 3, new Vector3f( m_fX, 0, 0 ) );
        m_akPolyline[4] = new Polyline( kClipView, true, true );
        m_akPolyline[4].AttachEffect( m_spkVertexColor3Shader );
        m_akPolyline[4].Local.SetTranslate(m_kTranslate);

        // pos z clipping:
        kClipView = new VertexBuffer(kAttributes, 4 );
        for ( int i = 0; i < 4; i++ )
        {
            kClipView.Color3( 0, i, new ColorRGB( 1, 0, 0 ) );
        }
        kClipView.Position3( 0, new Vector3f( 0, 0, m_fZ ) );
        kClipView.Position3( 1, new Vector3f( 0, m_fY, m_fZ ) );
        kClipView.Position3( 2, new Vector3f( m_fX, m_fY, m_fZ ) );
        kClipView.Position3( 3, new Vector3f( m_fX, 0, m_fZ ) );
        m_akPolyline[5] = new Polyline( kClipView, true, true );
        m_akPolyline[5].AttachEffect( m_spkVertexColor3Shader );
        m_akPolyline[5].Local.SetTranslate(m_kTranslate);

        // arbitrary clipping:
        kClipView = new VertexBuffer(kAttributes, 4 );
        for ( int i = 0; i < 4; i++ )
        {
            kClipView.Color3( 0, i, new ColorRGB( 1, 0, 0 ) );
        }
        kClipView.Position3( 0, new Vector3f( 0, 0, 0 ) );
        kClipView.Position3( 1, new Vector3f( 0, 0, m_fZ ) );
        kClipView.Position3( 2, new Vector3f( 0, m_fY, m_fZ ) );
        kClipView.Position3( 3, new Vector3f( 0, m_fY, 0 ) );
//         kClipView.Position3( 0, new Vector3f( m_fX, 0, 0 ) );
//         kClipView.Position3( 1, new Vector3f( m_fX, 0, m_fZ ) );
//         kClipView.Position3( 2, new Vector3f( m_fX, m_fY, m_fZ ) );
//         kClipView.Position3( 3, new Vector3f( m_fX, m_fY, 0 ) );

        m_kClipArb = new Polyline( kClipView, true, true );
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

        kClipView = new VertexBuffer(kAttributes, 4 );
        for ( int i = 0; i < 4; i++ )
        {
            kClipView.Color3( 0, i, new ColorRGB( 1, 0, 0 ) );
        }
        kClipView.Position3( 0, new Vector3f( -.2f, -.2f, m_fZ ) );
        kClipView.Position3( 1, new Vector3f( m_fX +.2f, -.2f, m_fZ ) );
        kClipView.Position3( 2, new Vector3f( m_fX +.2f, m_fY +.2f, m_fZ ) );
        kClipView.Position3( 3, new Vector3f( -.2f, m_fY +.2f, m_fZ ) );
        m_kClipEye = new Polyline( kClipView, true, true );
        m_kClipEye.Local.SetTranslate(m_kTranslate);
        m_kClipEye.AttachEffect( m_spkVertexColor3Shader );
        m_kClipEye.UpdateGS();
        m_kClipEye.UpdateRS();
        m_pkRenderer.LoadResources(m_kClipEye);

        kClipView = new VertexBuffer(kAttributes, 4 );
        for ( int i = 0; i < 4; i++ )
        {
            kClipView.Color3( 0, i, new ColorRGB( 1, 0, 0 ) );
        }
        kClipView.Position3( 0, new Vector3f( -.2f, -.2f, 1.0f ) );
        kClipView.Position3( 1, new Vector3f( m_fX +.2f, -.2f, 1.0f ) );
        kClipView.Position3( 2, new Vector3f( m_fX +.2f, m_fY +.2f, 1.0f ) );
        kClipView.Position3( 3, new Vector3f( -.2f, m_fY +.2f, 1.0f ) );
        m_kClipEyeInv = new Polyline( kClipView, true, true );
        m_kClipEyeInv.Local.SetTranslate(m_kTranslate);
        m_kClipEyeInv.AttachEffect( m_spkVertexColor3Shader );
        m_kClipEyeInv.UpdateGS();
        m_kClipEyeInv.UpdateRS();
        m_pkRenderer.LoadResources(m_kClipEyeInv);


        resetClip();
    }

    private void resetClip()
    {
        String[] akCmd = new String[]{ "clipXInv", "clipX", "clipYInv", "clipY", "clipZInv", "clipZ" };
        Program pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
        for ( int i = 0; i < 6; i++ )
        {
            if ( pkProgram.GetUC(akCmd[i]) != null ) 
            {
                float[] data = new float[4];
                if ( i%2 == 0 )
                    data[0] = 0;
                else
                    data[0] = 1;
                pkProgram.GetUC(akCmd[i]).SetDataSource(data);
            }       
        }
    }


    public void setClipPlane( int iWhich, float fValue, String cmd )
    {
        if ( iWhich < 2 )
        {
            fValue /= (float)m_kImage.getExtents()[0];
        }
        else if ( iWhich < 4 )
        {
            fValue /= (float)m_kImage.getExtents()[1];
        }
        else
        {
            fValue /= (float)m_kImage.getExtents()[2];
        }
        
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

        Program pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
        if ( pkProgram.GetUC(cmd) != null ) 
        {
            float[] data = new float[4];
            data[0] = fValue;
            pkProgram.GetUC(cmd).SetDataSource(data);
        }       
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

        String[] akCmd = new String[]{ "clipXInv", "clipX", "clipYInv", "clipY", "clipZInv", "clipZ" };
        Program pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
        if ( pkProgram.GetUC(akCmd[iWhich]) != null ) 
        {
            float[] data = new float[4];
            data[0] = fValue;
            pkProgram.GetUC(akCmd[iWhich]).SetDataSource(data);
        }
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
            setEyeInvClipPlane(m_kImage.getExtents()[2] - 1, bDisplay);
        }
    }

    public void setEyeClipPlane( float f4, boolean bDisplay )
    {
        Vector4f kEyeClipV = new Vector4f(0,0,1,(f4/(float)(m_kImage.getExtents()[2]-1)));
        
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

        Program pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
        if ( pkProgram.GetUC("clipEye") != null ) 
        {
            pkProgram.GetUC("clipEye").SetDataSource(afEquation);
        }

        m_bDisplayClipEye = bDisplay;
    }

    public void setEyeInvClipPlane( float f4, boolean bDisplay )
    {
        Vector4f kEyeClipV = new Vector4f(0,0,1,(f4/(float)(m_kImage.getExtents()[2]-1)));

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

        Program pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
        if ( pkProgram.GetUC("clipEyeInv") != null ) 
        {
            pkProgram.GetUC("clipEyeInv").SetDataSource(afEquation);
        }

        m_bDisplayClipEyeInv = bDisplay;
    }

    
    public void enableArbitraryClipPlane( boolean bEnable, boolean bDisplay, ColorRGB kColor )
    {
        setArbColor(kColor);
        displayArbitraryClipPlane(bDisplay);
        if ( !bEnable )
        {
            setArbitraryClipPlane((float)(m_kImage.getExtents()[0]-1));
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
        m_kArbitraryClip = new Vector4f(1,0,0,f4/(float)(m_kImage.getExtents()[0]-1));
        doClip();
    }
    
    private void doClip() 
    {

        // update position of the bounding box:
        m_kClipArb.Local.SetTranslate( m_kTranslate.add( new Vector3f( m_kArbitraryClip.W(), 0, 0 ) ) );

        Vector3f kPrePos = m_kClipArb.Local.GetTranslate();
        System.err.println( kPrePos.X() + " " +  kPrePos.Y() + " " +  kPrePos.Z() );

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

        m_kNormal = new Vector4f( kNormal.X(), kNormal.Y(), kNormal.Z(), m_kArbitraryClip.W() );
        
        
        System.err.println( "Normal: " + 
                            kNormal.X() + " " +
                            kNormal.Y() + " " +
                            kNormal.Z() + " " +
                            m_kArbitraryClip.W() );



        // Update shader with rotated normal and distance:
        Program pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
        if ( pkProgram.GetUC("clipArb") != null ) 
        {
            pkProgram.GetUC("clipArb").SetDataSource(afEquation);
        }

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
        VertexProgram kVProgram = m_kVolumeShaderEffect.GetVProgram(0);
        kVProgram.Release();
        VertexProgramCatalog.GetActive().Remove(kVProgram);

        PixelProgram kPProgram = m_kVolumeShaderEffect.GetPProgram(0);
        kPProgram.Release();
        PixelProgramCatalog.GetActive().Remove(kPProgram);

        VertexShader pkVShader = m_kVolumeShaderEffect.GetVShader(0);
        pkVShader.OnReleaseProgram();
        PixelShader pkPShader = m_kVolumeShaderEffect.GetPShader(0);
        pkPShader.OnReleaseProgram();

        m_kVolumeShaderEffect.LoadPrograms(0,m_pkRenderer.GetMaxColors(),
                              m_pkRenderer.GetMaxTCoords(),m_pkRenderer.GetMaxVShaderImages(),
                              m_pkRenderer.GetMaxPShaderImages());

        Program pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
        if ( pkProgram.GetUC("Iterations") != null)
        {
            pkProgram.GetUC("Iterations").SetDataSource(new float[]{500,0,0,0});
        }
        resetClip();
        MIPMode();
    }

    public void setVisible( boolean bVisible )
    {
        m_bVisible = bVisible;
    }

    public void updateLighting(GeneralLight[] akGLights )
    {
        if ( m_kOptionsPanel != null )
        {
            m_kOptionsPanel.updateLighting(akGLights);
        }
        if ( m_bInit )
        {
            Program pkProgram = m_kVolumeShaderEffect.GetPProgram(0);

            for ( int i = 2; i < akGLights.length; i++ )
            {
                if ( i < m_pkRenderer.GetMaxLights() )
                {
                    if ( akGLights[i].isEnabled() )
                    {
                        m_pkRenderer.SetLight( i-2, akGLights[i].createWMLight() );
                        System.err.println( "GPU updatelights " + (i-2));
                        String kLightType = new String("Light"+(i-2)+"Type");
                        float[] afType = new float[]{0,0,0,0};
                        if ( pkProgram.GetUC(kLightType) != null)
                        {
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
                            pkProgram.GetUC(kLightType).SetDataSource(afType);
                        }
                    }
                    else
                    {
                        m_pkRenderer.SetLight( i-2, new Light() );
                    }
                }
            }
        }
    }

    public JPanel getOptions()
    {
        if ( m_kOptionsPanel == null )
        {
            m_kOptionsPanel = new JPanelRenderOptionsGPU(this);
            return m_kOptionsPanel.getMainPanel();
        }
        return null;
    }

    public void SetMaterialState( MaterialState kMaterial )
    {
        m_kMesh.DetachGlobalState(GlobalState.StateType.MATERIAL);
        m_kMaterial = kMaterial;
        m_kMesh.AttachGlobalState(m_kMaterial);
        m_kMesh.UpdateMS(true);
        m_kMesh.UpdateRS();
    }


    private Node m_spkScene;
    private WireframeState m_spkWireframe;
    private CullState m_spkCull;
    private Culler m_kCuller = new Culler(0,0,null);

    private Polyline[] m_akPolyline;
    private Polyline m_kClipArb;
    private Polyline m_kClipEye;
    private Polyline m_kClipEyeInv;
    private boolean[] m_abDisplayPolyline = new boolean[]{false,false,false,false,false,false};

    private TriMesh m_spkMesh;
    private GraphicsImage m_spkVolume;
    private ShaderEffect m_kVolumeShaderEffect;

    private ModelImage m_kImage;

    private Camera m_spkScreenCamera;
    private Camera m_spkEyeCamera;
    private TriMesh m_spkScenePolygon;
    private GraphicsImage m_spkSceneImage;
    private Texture m_pkSceneTarget;
    private OpenGLFrameBuffer m_pkPBuffer;

    private Node m_spkVolumeNode;
    private ShaderEffect m_spkVertexColor3Shader;

    private Animator m_kAnimator;

    private GraphicsImage m_spkColorMap;
    private GraphicsImage m_spkOpacityMap = null;

    private ModelLUT m_kLUTa;

    private Texture m_pkVolumeTarget;
    private Texture m_pkColorMapTarget;
    private Texture m_pkOpacityMapTarget;
    private Texture m_pkNormalMapTarget;

    private float m_fImageMax;
    private float m_fImageMin;
    private SculptorWm m_kSculptor;

    private TransferFunction m_kTransfer;

    private boolean m_bInit = false;
    private Vector3f m_kTranslate;
    private Vector4f m_kArbitraryClip;
    private boolean m_bDisplayClipArb = false;
    private boolean m_bDisplayClipEye = false;
    private boolean m_bDisplayClipEyeInv = false;
    private float m_fX, m_fY, m_fZ;

    private boolean m_bVisible = true;
    private Node m_kArbRotate = new Node();
    private Vector4f m_kNormal;

    private JPanelRenderOptionsGPU m_kOptionsPanel = null;
    private MaterialState m_kMaterial;
    private TriMesh m_kMesh;
}
