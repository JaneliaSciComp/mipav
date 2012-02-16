package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.ViewJFrameImage;
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
            UpdateSlice(fZ);
            m_pkPlane.DetachAllEffects();
            m_pkPlane.AttachEffect(m_spkEffect);
            m_kCuller.ComputeVisibleSet(m_spkScene);
            m_pkRenderer.ClearBuffers();
            if (m_pkRenderer.BeginScene())
            {          
                m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
                m_pkRenderer.EndScene();
            }
        	SaveImage(m_iSlice);
            m_pkRenderer.FrameBufferToTexSubImage3D( m_kVolumeImage.GetNormalMapTarget(), m_iSlice );
            //m_pkRenderer.FrameBufferToTexSubImage3D( m_kVolumeImage.GetScratchTarget(), m_iSlice );
            m_iSlice++; 
            if ( m_iSlice >= m_kImage.getExtents()[2])
            {
                m_iSlice = 0;
                m_bDisplayFirst = false;
                m_bDisplaySecond = false;
                m_kVolumeImage.CopyNormalFiles(m_iVolume, m_kOutputImage);
                //m_bDisplaySecond = true;
            }
        }
        
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
            }
        	SaveImage(m_iSlice);
            m_pkRenderer.FrameBufferToTexSubImage3D( m_kVolumeImage.GetNormalMapTarget(), m_iSlice );
            m_iSlice++; 
            if ( m_iSlice >= m_kImage.getExtents()[2])
            {
                m_iSlice = 0;
                m_bDisplaySecond = false;
                m_kVolumeImage.CopyNormalFiles(m_iVolume, m_kOutputImage);
            }
        }
        
        dispose(arg0);
    }

    public void dispose(GLAutoDrawable arg0)
    {
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        m_spkEffect2 = null;
        super.dispose(arg0);
    }

    protected void CreateScene ()
    {
        CreatePlaneNode();
        m_pkPlane.VBuffer.SetTCoord3(0, 3, 1,1,0.5f);

        m_spkEffect = new VolumeCalcEffect( m_kVolumeImage, m_kVolumeImage.GetVolumeTarget(), "CalcNormalsPerSlice_Pass1", true);
        m_pkPlane.AttachEffect(m_spkEffect);
        m_pkRenderer.LoadResources(m_pkPlane);
        ((VolumeCalcEffect)m_spkEffect).SetStepSize(m_kVolumeImage);
        m_pkPlane.DetachAllEffects();
                
        m_spkEffect2 = new VolumeCalcEffect( m_kVolumeImage, m_kVolumeImage.GetScratchTarget(), "CalcNormalsPerSlice_Pass2", false );
        m_pkPlane.AttachEffect(m_spkEffect2);
        m_pkRenderer.LoadResources(m_pkPlane);
        m_spkEffect2.SetStepSize(m_kVolumeImage);
        m_pkPlane.DetachAllEffects();
        
    }
    
    private void SaveImage(int iZ)
    {
        if ( m_kOutputImage == null )
        {
            m_kOutputImage = new ModelImage( ModelStorageBase.ARGB, m_kImage.getExtents(), m_kImageName );
        }
        int iWidth = m_kImage.getExtents()[0];
        int iHeight = m_kImage.getExtents()[1];
        ByteBuffer kBuffer = m_pkRenderer.GetScreenImage( iWidth, iHeight );
        int iSize = iWidth * iHeight * 4;
        try {
            byte[] aucData = new byte[iSize];
            for ( int i = 0; i < iSize; i += 4)
            {
                aucData[i] = (byte)255;
                aucData[i+1] = kBuffer.array()[i];
                aucData[i+2] = kBuffer.array()[i+1];
                aucData[i+3] = kBuffer.array()[i+2];
            }
            m_kOutputImage.importData( iZ * iSize, aucData, false );
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
