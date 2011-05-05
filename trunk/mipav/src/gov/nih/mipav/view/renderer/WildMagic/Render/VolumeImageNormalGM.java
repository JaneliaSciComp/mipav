package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

import java.awt.Frame;
import java.awt.event.KeyListener;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;

import WildMagic.LibGraphics.Effects.TextureEffect;
import WildMagic.LibGraphics.Rendering.GraphicsImage;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import java.awt.image.Raster;
import java.awt.image.SampleModel;

import java.awt.Frame;
import java.awt.event.WindowAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.media.opengl.*;
import javax.media.opengl.awt.GLCanvas;
import com.jogamp.opengl.util.Animator;

public class VolumeImageNormalGM extends VolumeImageViewer
    implements GLEventListener, KeyListener
{
    /**  */
    private static final long serialVersionUID = 4477999995955496036L;
    private VolumeCalcEffect m_spkEffect2;
    private Texture m_pkVolumeCalcTarget;
    private boolean m_bDisplayFirst = true;
    private boolean m_bDisplaySecond = true;
    private ModelImage m_kImage;
    private Texture m_kTexture;
    private int m_iVolume = 0;
    private String m_kImageName;
    private ModelImage m_kOutputImage = null;
    
    public VolumeImageNormalGM( GLCanvas kCanvas, VolumeTriPlanarInterface kParentFrame, VolumeImage kVolumeImage, ModelImage kImage, Texture kTexture, int iVolume, String kImageName )
    {
        super(kCanvas, kParentFrame, kVolumeImage);
        m_kImage = kImage;
        m_kTexture = kTexture;
        m_iVolume = iVolume;
        m_kImageName = kImageName;
    }
    /**
     * @param args
     */
    public static void main( GLCanvas kCanvas, VolumeTriPlanarInterface kParentFrame, VolumeImage kVolumeImage, ModelImage kImage, Texture kTexture, int iVolume, String kImageName )
    {
        VolumeImageNormalGM kWorld = new VolumeImageNormalGM(kCanvas, kParentFrame, kVolumeImage, kImage, kTexture, iVolume, kImageName);
        Frame frame = new Frame(kWorld.GetWindowTitle());
        frame.add( kWorld.GetCanvas() );
        Animator animator = new Animator( kWorld.GetCanvas() );
        // setting the frame to be undecorated removes the frame title bar and edges
        // this prevents flashing on-screen.
        frame.setUndecorated(true);
        // frame must be set to visible for the gl canvas to be properly initialized.
        frame.setVisible(true);
        frame.setBounds(0,0,
                kWorld.GetWidth(), kWorld.GetHeight() );
        //frame.setVisible(false);
        kWorld.SetAnimator(animator);
        kWorld.SetFrame(frame);
        animator.start();
    }

    public void display(GLAutoDrawable arg0) {
        if ( m_kAnimator == null )
        {
            return;
        }
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        
        while ( m_bDisplayFirst )
        {
            float fZ = ((float)m_iSlice)/(m_kImage.getExtents()[2] -1);
            UpdateSlice(fZ);;
            m_pkPlane.DetachAllEffects();
            m_pkPlane.AttachEffect(m_spkEffect);
            m_kCuller.ComputeVisibleSet(m_spkScene);
            m_pkRenderer.ClearBuffers();
            if (m_pkRenderer.BeginScene())
            {          
                m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
                m_pkRenderer.EndScene();
            }
            //m_pkRenderer.DisplayBackBuffer();
            m_pkRenderer.FrameBufferToTexSubImage3D( m_kVolumeImage.GetNormalMapTarget(), m_iSlice, false );
        	//SaveImage(m_iSlice);
            m_iSlice++; 
            if ( m_iSlice >= m_kImage.getExtents()[2])
            {
                m_iSlice = 0;
                m_bDisplayFirst = false;
                //m_kVolumeImage.CopyNormalFiles(m_iVolume, m_kOutputImage);
            }
        }
        /*
        while ( m_bDisplaySecond )
        {
            float fZ = ((float)m_iSlice)/(m_kImage.getExtents()[2] -1);
            UpdateSlice(fZ);
            m_pkPlane.DetachAllEffects();
            m_pkPlane.AttachEffect(m_spkEffect2);
            m_kCuller.ComputeVisibleSet(m_spkScene);
            m_pkRenderer.ClearBuffers();
            if (m_pkRenderer.BeginScene())
            {          
            	m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
            	m_pkRenderer.EndScene();
            	m_pkRenderer.FrameBufferToTexSubImage3D( m_kTexture, m_iSlice, false );
            	m_pkRenderer.DisplayBackBuffer();
            	//SaveImage(m_iSlice);
            }
            m_iSlice++; 
            if ( m_iSlice >= m_kImage.getExtents()[2])
            {
                m_bDisplaySecond = false;
                //m_kVolumeImage.CopyNormalFiles(m_iVolume, m_kOutputImage);
            }
        }
        */
        dispose(arg0);
    }

    public void dispose(GLAutoDrawable arg0)
    {
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        m_spkEffect2.dispose();
        m_spkEffect2 = null;
        m_pkVolumeCalcTarget.dispose();
        m_pkVolumeCalcTarget = null;
        super.dispose(arg0);
    }

    protected void CreateScene ()
    {
        CreatePlaneNode();
        m_pkPlane.VBuffer.SetTCoord3(0, 3, 1,1,0.5f);

        m_spkEffect = new VolumeCalcEffect( m_kVolumeImage, m_kVolumeImage.GetVolumeTarget(), "CalcNormalsPerSlice_Pass1");
        m_pkPlane.AttachEffect(m_spkEffect);
        m_pkRenderer.LoadResources(m_pkPlane);
        ((VolumeCalcEffect)m_spkEffect).SetStepSize(m_kVolumeImage);
        m_pkPlane.DetachAllEffects();
        
        GraphicsImage kNormalImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA8888, 
        		m_kImage.getExtents()[0], m_kImage.getExtents()[1],
        		m_kImage.getExtents()[2],
    			(byte[])null, "VolumeCalc");
        
        m_pkVolumeCalcTarget = new Texture();
        m_pkVolumeCalcTarget.SetImage(kNormalImage);
        m_pkVolumeCalcTarget.SetShared(true);
        m_pkVolumeCalcTarget.SetFilterType(Texture.FilterType.LINEAR);
        m_pkVolumeCalcTarget.SetWrapType(0, Texture.WrapType.REPEAT);
        m_pkVolumeCalcTarget.SetWrapType(1, Texture.WrapType.REPEAT);
        m_pkVolumeCalcTarget.SetWrapType(2, Texture.WrapType.REPEAT);
        
        m_spkEffect2 = new VolumeCalcEffect( m_pkVolumeCalcTarget.GetName(), m_pkVolumeCalcTarget, "CalcNormalsPerSlice_Pass2" );
        m_pkPlane.AttachEffect(m_spkEffect2);
        m_pkRenderer.LoadResources(m_pkPlane);
        m_spkEffect2.SetStepSize(m_kVolumeImage);
        m_pkPlane.DetachAllEffects();
        
    }
    
    private void SaveImage(int iZ)
    {
        Raster kRaster = ((OpenGLRenderer)m_pkRenderer).Screenshot().getData();
        int iWidth = kRaster.getWidth();
        int iHeight = kRaster.getHeight();
        if ( m_kOutputImage == null )
        {
            m_kOutputImage = new ModelImage( ModelStorageBase.ARGB, new int[]{iWidth, iHeight, m_kImage.getExtents()[2]}, m_kImageName );
        }

        DataBuffer kBuffer = kRaster.getDataBuffer();
        SampleModel kSampleModel = kRaster.getSampleModel();
        int iSize = iWidth * iHeight;
        float[] samples = new float[ kSampleModel.getNumBands() ];
        try {
            byte[] aucData = new byte[iSize * 4];
            for ( int y = 0; y < iHeight; y++)
            {
            for ( int x = 0; x < iWidth; x++)
            {
            	kSampleModel.getPixel(x,y,samples,kBuffer);
                aucData[y*iWidth + 0] = (byte)255;
                aucData[y*iWidth + 1] = (byte) samples[0];
                aucData[y*iWidth + 2] = (byte) samples[1];
                aucData[y*iWidth + 3] = (byte) samples[2];
            }
            }
            m_kOutputImage.importData( iZ * iSize * 4, aucData, false );
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
