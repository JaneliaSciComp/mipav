/**
 * 
 */

package gov.nih.mipav.view.WildMagic.ApplicationDemos;

import javax.media.opengl.*;
import com.sun.opengl.cg.CgGL;
import com.sun.opengl.util.*;

import java.awt.*;
import java.awt.event.*;
import java.util.Calendar;
import java.nio.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.WildMagic.LibApplications.OpenGLApplication.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Detail.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Sorting.*;
import gov.nih.mipav.view.WildMagic.LibRenderers.OpenGLRenderer.*;

import gov.nih.mipav.view.renderer.volumeview.*;

/**
 * @author Alexandra
 *
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
    }

    public void display(GLAutoDrawable arg0) {
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
            m_pkRenderer.SetBackgroundColor(ColorRGBA.WHITE);
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
            m_pkRenderer.SetBackgroundColor(new ColorRGBA(0.0f,0.0f,0.0f,0.0f));
            m_pkRenderer.ClearBuffers();

            m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());

            /*
            m_pkRenderer.SetCamera(m_spkScreenCamera);
            m_pkRenderer.Draw(m_spkScenePolygon);

            m_pkRenderer.SetCamera(m_spkCamera);
            DrawFrameRate(8,16,ColorRGBA.BLACK);
            */

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
        m_spkCamera.SetFrustum(-0.55f,0.55f,-0.4125f,0.4125f,1.0f,1000.0f);
        Vector3f kCDir = new Vector3f(0.0f,0.0f,1.0f);
        Vector3f kCUp = new Vector3f(0.0f,-1.0f,0.0f);
        Vector3f kCRight = kCDir.Cross(kCUp);
        Vector3f kCLoc = kCDir.scale(-4.0f);
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
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        if (iWidth > 0 && iHeight > 0)
        {
            if (m_pkRenderer != null)
            {
                m_pkRenderer.Resize(iWidth,iHeight);
            }
            
            m_iWidth = iWidth;
            m_iHeight = iHeight;
        }
        ((OpenGLRenderer)m_pkRenderer).ClearDrawable( );
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
    }

    private void CreateVolumeTexture ()
    {
        int iXBound = m_kImage.getExtents()[0];
        int iYBound = m_kImage.getExtents()[1];
        int iZBound = m_kImage.getExtents()[2];
        m_kImage.calcMinMax();
        m_fImageMax = (float)m_kImage.getMax();
        m_fImageMin = (float)m_kImage.getMin();

        int iColorFactor = 1;
        if ( m_kImage.isColorImage() )
        {
            iColorFactor = 3;
        }


        byte[] aucData = new byte[iColorFactor*iXBound*iYBound*iZBound];

        int i = 0;
        for (int iZ = 0; iZ < iZBound; iZ++)
        {
            for (int iY = 0; iY < iYBound; iY++)
            {
                for (int iX = 0; iX < iXBound; iX++)
                {
                    if ( m_kImage.isColorImage() )
                    {
                        aucData[i++] = (byte)(255.0f*m_kImage.getFloatC(iX,iY,iZ,0));
                        aucData[i++] = (byte)(255.0f*m_kImage.getFloatC(iX,iY,iZ,1));
                        aucData[i++] = (byte)(255.0f*m_kImage.getFloatC(iX,iY,iZ,2));
                    }
                    else
                    {
                        float fValue = m_kImage.getFloat(iX,iY,iZ);
                        fValue = (fValue - m_fImageMin)/(m_fImageMax - m_fImageMin);
                        byte bValue = (byte)(255.0f * fValue);
                        aucData[i++] = bValue;
                    }
                }
            }
        }

        if ( m_kImage.isColorImage() )
        {
            m_spkVolume = new GraphicsImage(
                              GraphicsImage.FormatMode.IT_RGB888,iXBound,iYBound,iZBound,aucData,
                                                                                       "VolumeImage");
        }
        else
        {
            m_spkVolume = new GraphicsImage(
                              GraphicsImage.FormatMode.IT_L8,iXBound,iYBound,iZBound,aucData,
                                                                                       "VolumeImage");
        }

        System.err.println( ViewUserInterface.getReference().getDefaultDirectory() );


        VertexShader pkVShader = new VertexShader("VolumeShader");
        PixelShader pkPShader = new PixelShader("VolumeShader");

        aucData = calcImageNormals();
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

        m_kVolumeShaderEffect = new ShaderEffect(1);
        m_kVolumeShaderEffect.SetVShader(0,pkVShader);
        m_kVolumeShaderEffect.SetPShader(0,pkPShader);

        m_pkRenderer.LoadResources(m_kVolumeShaderEffect);
        Program pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
        pkProgram.GetUC("stepsize").SetDataSource(m_afstepsize);
        float[] afMIP = new float[] {1,0,0,0};
        pkProgram.GetUC("MIP").SetDataSource(afMIP);
    }

    public void MIPMode()
    {
        Program pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
        float[] afMIP = new float[] {1,0,0,0};
        pkProgram.GetUC("MIP").SetDataSource(afMIP);
        float[] afDDR = new float[] {0,0,0,0};
        pkProgram.GetUC("DDR").SetDataSource(afDDR);
        float[] afComposite = new float[] {0,0,0,0};
        pkProgram.GetUC("Composite").SetDataSource(afComposite);
        float[] afSurface = new float[] {0,0,0,0};
        pkProgram.GetUC("Surface").SetDataSource(afSurface);
    }

    public void DDRMode()
    {
        Program pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
        float[] afMIP = new float[] {0,0,0,0};
        pkProgram.GetUC("MIP").SetDataSource(afMIP);
        float[] afDDR = new float[] {1,0,0,0};
        pkProgram.GetUC("DDR").SetDataSource(afDDR);
        float[] afComposite = new float[] {0,0,0,0};
        pkProgram.GetUC("Composite").SetDataSource(afComposite);
        float[] afSurface = new float[] {0,0,0,0};
        pkProgram.GetUC("Surface").SetDataSource(afSurface);
    }

    public void CMPMode()
    {
        Program pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
        float[] afMIP = new float[] {0,0,0,0};
        pkProgram.GetUC("MIP").SetDataSource(afMIP);
        float[] afDDR = new float[]{1,0,0,0};
        pkProgram.GetUC("DDR").SetDataSource(afDDR);
        float[] afComposite = new float[]{1,0,0,0};
        pkProgram.GetUC("Composite").SetDataSource(afComposite);
        float[] afSurface = new float[] {0,0,0,0};
        pkProgram.GetUC("Surface").SetDataSource(afSurface);
    }

    public void SURMode()
    {
        Program pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
        float[] afMIP = new float[] {0,0,0,0};
        pkProgram.GetUC("MIP").SetDataSource(afMIP);
        float[] afDDR = new float[]{1,0,0,0};
        pkProgram.GetUC("DDR").SetDataSource(afDDR);
        float[] afComposite = new float[]{1,0,0,0};
        pkProgram.GetUC("Composite").SetDataSource(afComposite);
        float[] afSurface = new float[] {1,0,0,0};
        pkProgram.GetUC("Surface").SetDataSource(afSurface);
        float[] afSpecular = new float[] {1,0,0,0};
        pkProgram.GetUC("specularParam").SetDataSource(afSpecular);
        float[] afShininess = new float[] {1,0,0,0};
        pkProgram.GetUC("shininessParam").SetDataSource(afShininess);
        float[] afLight = new float[] {0,-5,-5,0};
        pkProgram.GetUC("light").SetDataSource(afLight);
    }

    public void SURFASTMode()
    {
        Program pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
        float[] afMIP = new float[] {0,0,0,0};
        pkProgram.GetUC("MIP").SetDataSource(afMIP);
        float[] afDDR = new float[]{1,0,0,0};
        pkProgram.GetUC("DDR").SetDataSource(afDDR);
        float[] afComposite = new float[]{0,0,0,0};
        pkProgram.GetUC("Composite").SetDataSource(afComposite);
        float[] afSurface = new float[] {1,0,0,0};
        pkProgram.GetUC("Surface").SetDataSource(afSurface);
        float[] afSpecular = new float[] {1,0,0,0};
        pkProgram.GetUC("specularParam").SetDataSource(afSpecular);
        float[] afShininess = new float[] {1,0,0,0};
        pkProgram.GetUC("shininessParam").SetDataSource(afShininess);
        float[] afLight = new float[] {0,-5,-5,0};
        pkProgram.GetUC("light").SetDataSource(afLight);
    }

    public boolean updateImages(boolean forceShow, TransferFunction kTransfer)
    {
        if ( m_spkOpacityMap == null )
        {
            m_kTransfer = kTransfer;
            return false;
        }
        int iLutHeight = 256;
        byte[] aucData = m_spkOpacityMap.GetData();

        System.err.println( m_fImageMax + " " + m_fImageMin );

        float fRange = m_fImageMax - m_fImageMin;
        float fStep = fRange / (float)iLutHeight;
        float fDataValue = m_fImageMin;
        for (int i = 0; i < iLutHeight; i++) {
            aucData[i] = (byte)(kTransfer.getRemappedValue( fDataValue, iLutHeight ));
            fDataValue += fStep;
        }
        m_pkOpacityMapTarget.Release();
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
        int iLutHeight = kLUTa.getExtents()[1];
        int[] iLutTable = new int[iLutHeight];
        iLutTable = kLUTa.exportIndexedLUT();

        byte[] aucData = m_spkColorMap.GetData();

        int iColor;
        int count = 0;
        for (int i = 0; i < iLutHeight; i++) {
            iColor = iLutTable[i];
            aucData[count++] = (byte)( (iColor & 0x00ff0000) >> 16);
            aucData[count++] = (byte)( (iColor & 0x0000ff00) >> 8);
            aucData[count++] = (byte)(iColor & 0x000000ff);
            aucData[count++] = (byte)( (iColor & 0xff000000) >> 24);
        }
        m_pkColorMapTarget.Release();
        return true;
    }

    public void InitLUTTexture ( ModelLUT kLUTa )
    {
        int iLutHeight = kLUTa.getExtents()[1];
        int[] iLutTable = new int[iLutHeight];
        iLutTable = kLUTa.exportIndexedLUT();

        byte[] aucData = new byte[iLutHeight*4];

        int iColor;
        int count = 0;
        for (int i = 0; i < iLutHeight; i++) {
            iColor = iLutTable[i];
            aucData[count++] = (byte)( (iColor & 0x00ff0000) >> 16);
            aucData[count++] = (byte)( (iColor & 0x0000ff00) >> 8);
            aucData[count++] = (byte)(iColor & 0x000000ff);
            aucData[count++] = (byte)( (iColor & 0xff000000) >> 24);
        }

        m_spkColorMap = new GraphicsImage(
                            GraphicsImage.FormatMode.IT_RGBA8888,iLutHeight,aucData,
                                                                                       "ColorMap");




        iLutHeight = 256;
        aucData = new byte[iLutHeight];
        float fRange = m_fImageMax - m_fImageMin;
        float fStep = fRange / (float)iLutHeight;
        float fDataValue = m_fImageMin;
        for (int i = 0; i < iLutHeight; i++) {
            aucData[i] = (byte)( iLutHeight * (m_fImageMax - fDataValue) / fRange);
            fDataValue += fStep;
        }

        m_spkOpacityMap = new GraphicsImage(
                              GraphicsImage.FormatMode.IT_L8,iLutHeight,aucData,
                                                                                           "OpacityMap");
    }



    private void CreateBox ()
    {
        m_spkVolumeNode = new Node();
        int iSlices = 64;
        final int iDelta = 32;
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
        //kAttr.SetNChannels(3);
        kAttr.SetCChannels(0,3);
        kAttr.SetTChannels(0,3);
        kAttr.SetTChannels(1,3);

        int iMaxBound = iXBound;
        if ( iYBound > iMaxBound )
        {
            iMaxBound = iYBound;
        }
        if ( iZBound > iMaxBound )
        {
            iMaxBound = iZBound;
        }

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
        int iVQuantity = 24;
        int iTQuantity = 12;
        VertexBuffer pkVB = new VertexBuffer(kAttr,iVQuantity);
        IndexBuffer pkIB = new IndexBuffer(3*iTQuantity);

        // generate connectivity (outside view)
        int i = 0;
        int[] aiIndex = pkIB.GetData();

        System.err.println( fMaxX/fMax + " " + fMaxY/fMax + " " + fMaxZ/fMax );


        // generate geometry
        // front
        pkVB.Position3(0, new Vector3f(0,0,0));
        pkVB.Position3(1, new Vector3f(fMaxX/fMax,0,0));
        pkVB.Position3(2, new Vector3f(fMaxX/fMax,fMaxY/fMax,0));
        pkVB.Position3(3, new Vector3f(0,fMaxY/fMax,0));
        pkVB.Color3(0,0, new ColorRGB(0,0,0));
        pkVB.Color3(0,1, new ColorRGB(fMaxX/fMax,0,0));
        pkVB.Color3(0,2, new ColorRGB(fMaxX/fMax,fMaxY/fMax,0));
        pkVB.Color3(0,3, new ColorRGB(0,fMaxY/fMax,0));
        aiIndex[i++] = 0;  aiIndex[i++] = 2;  aiIndex[i++] = 1;
        aiIndex[i++] = 0;  aiIndex[i++] = 3;  aiIndex[i++] = 2;

        // back
        pkVB.Position3(4, new Vector3f(0,0,fMaxZ/fMax));
        pkVB.Position3(5, new Vector3f(fMaxX/fMax,0,fMaxZ/fMax));
        pkVB.Position3(6, new Vector3f(fMaxX/fMax,fMaxY/fMax,fMaxZ/fMax));
        pkVB.Position3(7, new Vector3f(0,fMaxY/fMax,fMaxZ/fMax));
        pkVB.Color3(0,4, new ColorRGB(0,0,fMaxZ/fMax));
        pkVB.Color3(0,5, new ColorRGB(fMaxX/fMax,0,fMaxZ/fMax));
        pkVB.Color3(0,6, new ColorRGB(fMaxX/fMax,fMaxY/fMax,fMaxZ/fMax));
        pkVB.Color3(0,7, new ColorRGB(0,fMaxY/fMax,fMaxZ/fMax));
        aiIndex[i++] = 4;  aiIndex[i++] = 5;  aiIndex[i++] = 6;
        aiIndex[i++] = 4;  aiIndex[i++] = 6;  aiIndex[i++] = 7;

        // top
        pkVB.Position3(8, new Vector3f(0,fMaxY/fMax,0));
        pkVB.Position3(9, new Vector3f(fMaxX/fMax,fMaxY/fMax,0));
        pkVB.Position3(10, new Vector3f(fMaxX/fMax,fMaxY/fMax,fMaxZ/fMax));
        pkVB.Position3(11, new Vector3f(0,fMaxY/fMax,fMaxZ/fMax));
        pkVB.Color3(0,8, new ColorRGB(0,fMaxY/fMax,0));
        pkVB.Color3(0,9, new ColorRGB(fMaxX/fMax,fMaxY/fMax,0));
        pkVB.Color3(0,10, new ColorRGB(fMaxX/fMax,fMaxY/fMax,fMaxZ/fMax));
        pkVB.Color3(0,11, new ColorRGB(0,fMaxY/fMax,fMaxZ/fMax));
        aiIndex[i++] = 8;  aiIndex[i++] = 10;  aiIndex[i++] = 9;
        aiIndex[i++] = 8;  aiIndex[i++] = 11;  aiIndex[i++] = 10;

        // bottom
        pkVB.Position3(12, new Vector3f(0,0,0));
        pkVB.Position3(13, new Vector3f(fMaxX/fMax,0,0));
        pkVB.Position3(14, new Vector3f(fMaxX/fMax,0,fMaxZ/fMax));
        pkVB.Position3(15, new Vector3f(0,0,fMaxZ/fMax));
        pkVB.Color3(0,12, new ColorRGB(0,0,0));
        pkVB.Color3(0,13, new ColorRGB(fMaxX/fMax,0,0));
        pkVB.Color3(0,14, new ColorRGB(fMaxX/fMax,0,fMaxZ/fMax));
        pkVB.Color3(0,15, new ColorRGB(0,0,fMaxZ/fMax));
        aiIndex[i++] = 12;  aiIndex[i++] = 13;  aiIndex[i++] = 14;
        aiIndex[i++] = 12;  aiIndex[i++] = 14;  aiIndex[i++] = 15;

        // right
        pkVB.Position3(16, new Vector3f(fMaxX/fMax,0,0));
        pkVB.Position3(17, new Vector3f(fMaxX/fMax,fMaxY/fMax,0));
        pkVB.Position3(18, new Vector3f(fMaxX/fMax,fMaxY/fMax,fMaxZ/fMax));
        pkVB.Position3(19, new Vector3f(fMaxX/fMax,0,fMaxZ/fMax));
        pkVB.Color3(0,16, new ColorRGB(fMaxX/fMax,0,0));
        pkVB.Color3(0,17, new ColorRGB(fMaxX/fMax,fMaxY/fMax,0));
        pkVB.Color3(0,18, new ColorRGB(fMaxX/fMax,fMaxY/fMax,fMaxZ/fMax));
        pkVB.Color3(0,19, new ColorRGB(fMaxX/fMax,0,fMaxZ/fMax));
        aiIndex[i++] = 16;  aiIndex[i++] = 17;  aiIndex[i++] = 18;
        aiIndex[i++] = 16;  aiIndex[i++] = 18;  aiIndex[i++] = 19;

        // left
        pkVB.Position3(20, new Vector3f(0,0,0));
        pkVB.Position3(21, new Vector3f(0,fMaxY/fMax,0));
        pkVB.Position3(22, new Vector3f(0,fMaxY/fMax,fMaxZ/fMax));
        pkVB.Position3(23, new Vector3f(0,0,fMaxZ/fMax));
        pkVB.Color3(0,20, new ColorRGB(0,0,0));
        pkVB.Color3(0,21, new ColorRGB(0,fMaxY/fMax,0));
        pkVB.Color3(0,22, new ColorRGB(0,fMaxY/fMax,fMaxZ/fMax));
        pkVB.Color3(0,23, new ColorRGB(0,0,fMaxZ/fMax));
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
        TriMesh pkMesh = new TriMesh(pkVB,pkIB);
        pkMesh.UpdateMS(true);
        return pkMesh;
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

        // Access intensity values as a linear array.
        // Initially allocate all normal vectors as the zero vector.
        Vector3f[] akNormal = new Vector3f[afData.length];

        Vector3f[] akNormalTmp = new Vector3f[afData.length];

        for (int i = 0; i < akNormalTmp.length; i++) {
            akNormal[i] = new Vector3f(0, 0, 0);
            akNormalTmp[i] = new Vector3f(0, 0, 0);
        }

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
                        akNormalTmp[i] = new Vector3f(fDX, fDY, fDZ);
                    }
                }
            }
        }

        // Catch any zero-vector normals and replace them by an average of
        // neighboring normals.
        for (iZ = 1; iZ < (iZBound - 1); iZ++) {

            for (iY = 1; iY < (iYBound - 1); iY++) {
                int offset = iXBound * (iY + (iYBound * iZ));

                for (iX = 1; iX < (iXBound - 1); iX++) {
                    int i = iX + offset;
                    akNormal[i].addEquals(akNormalTmp[i]);
                    akNormal[i].addEquals(akNormalTmp[i - 1]);
                    akNormal[i].addEquals(akNormalTmp[i + 1]);
                    akNormal[i].addEquals(akNormalTmp[i - iXBound]);
                    akNormal[i].addEquals(akNormalTmp[i + iXBound]);
                    akNormal[i].addEquals(akNormalTmp[i - iXYBound]);
                    akNormal[i].addEquals(akNormalTmp[i + iXYBound]);
                    akNormal[i].Normalize();
                }
            }
        }

        byte[] acData = new byte[akNormalTmp.length*3];
        for (int i = 0; i < akNormalTmp.length; i++) {
            acData[i*3+0] = (byte)(akNormal[i].X()*127 + 127);
            acData[i*3+1] = (byte)(akNormal[i].Y()*127 + 127);
            acData[i*3+2] = (byte)(akNormal[i].Z()*127 + 127);

            akNormalTmp[i] = null;
            akNormal[i] = null;
        }

        akNormalTmp = null;
        akNormal = null;
        kSimpleImageA = null;
        System.gc();

        return acData;
    }



    public void keyPressed(KeyEvent e) {
        char ucKey = e.getKeyChar();
        
        super.keyPressed(e);

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
        }
        return;
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
        m_kSculptor.setTextureImageData( m_pkVolumeTarget.GetImage().GetData() );
        m_kSculptor.undoSculpt();
        m_pkVolumeTarget.Release();
    }

    public void applySculpt()
    {
        float[] afData = new float[16];
        m_pkRenderer.SetConstantWVPMatrix (0, afData);
        m_kSculptor.setWVPMatrix(new Matrix4f(afData, true));
        m_kSculptor.setTextureImageData( m_pkVolumeTarget.GetImage().GetData() );
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
                System.err.println( akCmd[i] + " " + data[0] );
            }       
        }


        // arbitrary clipping:
        kClipView = new VertexBuffer(kAttributes, 4 );
        for ( int i = 0; i < 4; i++ )
        {
            kClipView.Color3( 0, i, new ColorRGB( 1, 0, 0 ) );
        }
        kClipView.Position3( 0, new Vector3f( m_fX, 0, 0 ) );
        kClipView.Position3( 1, new Vector3f( m_fX, 0, m_fZ ) );
        kClipView.Position3( 2, new Vector3f( m_fX, m_fY, m_fZ ) );
        kClipView.Position3( 3, new Vector3f( m_fX, m_fY, 0 ) );
        m_kClipArb = new Polyline( kClipView, true, true );
        m_kClipArb.AttachEffect( m_spkVertexColor3Shader );
        m_kClipArb.Local.SetTranslate(m_kTranslate);

        // eye clipping:
        kClipView = new VertexBuffer(kAttributes, 4 );
        for ( int i = 0; i < 4; i++ )
        {
            kClipView.Color3( 0, i, new ColorRGB( 1, 0, 0 ) );
        }
        kClipView.Position3( 0, new Vector3f( 0, 0, m_fZ ) );
        kClipView.Position3( 1, new Vector3f( m_fX, 0, m_fZ ) );
        kClipView.Position3( 2, new Vector3f( m_fX, m_fY, m_fZ ) );
        kClipView.Position3( 3, new Vector3f( 0, m_fY, m_fZ ) );
        m_kClipEye = new Polyline( kClipView, true, true );
        m_kClipEye.AttachEffect( m_spkVertexColor3Shader );
        m_kClipEye.Local.SetTranslate(m_kTranslate);
    }

    public void setClipPlane( int iWhich, float fValue, String cmd )
    {
        fValue = (fValue + 1)/2.0f;
        if ( iWhich != 0 && iWhich != 1)
        {
            fValue = 1 - fValue;
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
            System.err.println( cmd + " " + data[0] );
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
            System.err.println( akCmd[iWhich] + " " + data[0] );
        }
    }

    public void setEyeClipPlane( float f4 )
    {
        m_kArbitraryClip = new Vector4f(0,0,1,(f4 + 1)/2.0f);
        
        float[] afEquation = new float[4];
        afEquation[0] = m_kArbitraryClip.X();
        afEquation[1] = m_kArbitraryClip.Y();
        afEquation[2] = m_kArbitraryClip.Z();;
        afEquation[3] = m_kArbitraryClip.W();


        float fZ = m_kArbitraryClip.W();//(m_kArbitraryClip.W() + 1)/2.0f;
        
        m_kClipEye.VBuffer.Position3( 0, new Vector3f( 0, 0, fZ ) );
        m_kClipEye.VBuffer.Position3( 1, new Vector3f( m_fX, 0, fZ ) );
        m_kClipEye.VBuffer.Position3( 2, new Vector3f( m_fX, m_fY, fZ ) );
        m_kClipEye.VBuffer.Position3( 3, new Vector3f( 0, m_fY, fZ ) );
        m_kClipEye.VBuffer.Release();

        Program pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
        if ( pkProgram.GetUC("clipArb") != null ) 
        {
            pkProgram.GetUC("clipArb").SetDataSource(afEquation);
            System.err.println( "ClipArb " + afEquation[0] + " " +
                                afEquation[1] + " " +
                                afEquation[2] + " " +
                                afEquation[3] );
        }

        if ( !m_bDisplayClipEye )
        {
            m_bDisplayClipEye = true;
            m_spkScene.AttachChild(m_kClipEye);
        }

        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();

    }

    
    public void setArbitraryClipPlane( float f1, float f2, float f3, float f4 )
    {
        m_kArbitraryClip = new Vector4f(f1,f2,f3,(f4 + 1)/2.0f);
        doClip();

         if ( !m_bDisplayClipArb )
         {
            m_bDisplayClipArb = true;
             m_spkScene.AttachChild(m_kClipArb);
             m_spkScene.UpdateGS();
             m_spkScene.UpdateRS();
         }
    }
    
    private void doClip() 
    {
        Vector3f kClip = new Vector3f( m_kArbitraryClip.X(), m_kArbitraryClip.Y(), m_kArbitraryClip.Z() );
        kClip = m_kClipRotate.mult(kClip);

        float[] afEquation = new float[4];
        afEquation[0] = kClip.X();
        afEquation[1] = kClip.Y();
        afEquation[2] = kClip.Z();;
        afEquation[3] = m_kArbitraryClip.W();


        float fX = m_kArbitraryClip.W();//(m_kArbitraryClip.W() + 1)/2.0f;
        
        m_kClipArb.VBuffer.Position3( 0, new Vector3f( fX, 0, 0 ) );
        m_kClipArb.VBuffer.Position3( 1, new Vector3f( fX, 0, m_fZ ) );
        m_kClipArb.VBuffer.Position3( 2, new Vector3f( fX, m_fY, m_fZ ) );
        m_kClipArb.VBuffer.Position3( 3, new Vector3f( fX, m_fY, 0 ) );

        for ( int i = 0; i < 4; i++ )
        {
            Vector3f kPos = m_kClipArb.VBuffer.Position3( i );
            kPos = m_kClipRotate.mult( kPos );
            m_kClipArb.VBuffer.Position3( i, kPos );
        }
        m_kClipArb.VBuffer.Release();

        Program pkProgram = m_kVolumeShaderEffect.GetPProgram(0);
        if ( pkProgram.GetUC("clipArb") != null ) 
        {
            pkProgram.GetUC("clipArb").SetDataSource(afEquation);
            System.err.println( "ClipArb " + afEquation[0] + " " +
                                afEquation[1] + " " +
                                afEquation[2] + " " +
                                afEquation[3] );
        }

        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();
    }

    public void setClipPlaneColor( int iWhich, ColorRGB kColor )
    {
        for ( int i = 0; i < 4; i++ )
        {
            m_akPolyline[iWhich].VBuffer.Color3( 0, i, kColor );
        }
        m_akPolyline[iWhich].VBuffer.Release();
    }


    /** Rotates the object with a virtual trackball:
     * @param e, the MouseEvent
     */
    public void mouseDragged(MouseEvent e)
    {
        if ( !m_kSculptor.getEnable() )
        {
            super.mouseDragged(e);
        }
    }

    public void setArbitraryClipPlaneTransform( float[] data )
    {
        m_kClipRotate = new Matrix3f(data[0], data[1], data[2],
                                     data[4], data[5], data[6],
                                     data[8], data[9], data[10]);
        doClip();
    }

    public void transformUpdate( float[] data )
    {
        m_spkMotionObject.Local.
            SetMatrix(new Matrix3f(data[0], data[1], data[2],
                                   data[4], data[5], data[6],
                                   data[8], data[9], data[10]));
        m_spkScene.UpdateGS();
        m_kCuller.ComputeVisibleSet(m_spkScene);
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
        pkProgram.GetUC("stepsize").SetDataSource(m_afstepsize);
        float[] afMIP = new float[] {1,0,0,0};
        pkProgram.GetUC("MIP").SetDataSource(afMIP);

    }



    private Node m_spkScene;
    private WireframeState m_spkWireframe;
    private CullState m_spkCull;
    private Culler m_kCuller = new Culler(0,0,null);

    private Polyline[] m_akPolyline;
    private Polyline m_kClipArb;
    private Polyline m_kClipEye;
    private boolean[] m_abDisplayPolyline = new boolean[]{false,false,false,false,false,false};

    private TriMesh m_spkMesh;
    private GraphicsImage m_spkVolume;
    private ShaderEffect m_kVolumeShaderEffect;

    private ModelImage m_kImage;

    private Camera m_spkScreenCamera;
    private TriMesh m_spkScenePolygon;
    private GraphicsImage m_spkSceneImage;
    private Texture m_pkSceneTarget;
    private OpenGLFrameBuffer m_pkPBuffer;

    private Node m_spkVolumeNode;
    private ShaderEffect m_spkVertexColor3Shader;
    private float[] m_afstepsize = new float[]{0.005f,0,0,0};
    private float[] m_afCommonAlpha = new float[]{5.0f,0,0,0};
    private float[] m_afThreshold = new float[]{1,0,0,0};

    private Animator m_kAnimator;

    private GraphicsImage m_spkColorMap;
    private GraphicsImage m_spkOpacityMap = null;

    private ModelLUT m_kLUTa;

    private Texture m_pkVolumeTarget;
    private Texture m_pkColorMapTarget;
    private Texture m_pkOpacityMapTarget;

    private float m_fImageMax;
    private float m_fImageMin;
    private SculptorWm m_kSculptor;

    private TransferFunction m_kTransfer;

    private boolean m_bInit = false;
    private Vector3f m_kTranslate;
    private Vector4f m_kArbitraryClip;
    private Matrix3f m_kClipRotate = new Matrix3f(false);
    private boolean m_bDisplayClipArb = false;
    private boolean m_bDisplayClipEye = false;
    private float m_fX, m_fY, m_fZ;
}
