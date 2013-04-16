package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

import java.awt.Frame;
import java.awt.event.KeyListener;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;
import javax.media.opengl.awt.GLCanvas;

import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Vector4f;

import com.jogamp.opengl.util.Animator;

public class VolumeImageCrop extends VolumeImageViewer
    implements GLEventListener, KeyListener
{
    /**  */
    private static final long serialVersionUID = -3884075404385493867L;
    /**
     * @param args
     */
    public static void main( GLCanvas kCanvas, VolumeTriPlanarInterface kParentFrame, VolumeImage kVolumeImage, VolumeClipEffect kClip, float[]  volumeViewMatrix )
    {
        VolumeImageCrop kWorld = new VolumeImageCrop(kCanvas, kParentFrame, kVolumeImage, kClip, volumeViewMatrix);
        Frame frame = new Frame(kWorld.GetWindowTitle());
        frame.add( kWorld.GetCanvas() );
         final Animator animator = new Animator( kWorld.GetCanvas() );
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
    
    private float[] m_kVolumeViewMatrix = null;
    private VolumeClipEffect m_kClipEffect = null;
    public VolumeImageCrop( GLCanvas kCanvas, VolumeTriPlanarInterface kParentFrame, VolumeImage kVolumeImage, VolumeClipEffect kClip, float[] volumeViewMatrix )
    {
        super( kCanvas, kParentFrame, kVolumeImage );

        m_kVolumeViewMatrix = new float[volumeViewMatrix.length];
        for ( int i = 0; i < volumeViewMatrix.length; i++ )
        {
        	m_kVolumeViewMatrix[i] = volumeViewMatrix[i];
        }
        m_kClipEffect = kClip;
    }

    @Override
	public void display(GLAutoDrawable arg0) {
        if ( m_kAnimator == null )
        {
            return;
        }      
        boolean bCrop = true;
        while ( bCrop )
        {
            ((VolumeCalcEffect)m_spkEffect).ResetClip();

            float fZ = ((float)m_iSlice)/(m_kVolumeImage.GetImage().getExtents()[2] -1);
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
            m_pkRenderer.FrameBufferToTexSubImage3D( m_kVolumeImage.GetVolumeTarget(), m_iSlice );
            m_iSlice++; 
            if ( m_iSlice >= m_kVolumeImage.GetImage().getExtents()[2])
            {
                bCrop = false;
                m_iSlice = 0;
                dispose(arg0);
            }
        }
    }

    @Override
	public void dispose(GLAutoDrawable arg0)
    {
        m_kClipEffect = null;
        super.dispose(arg0);
    }

    @Override
	protected void CreateScene ()
    {
        CreatePlaneNode();
        m_spkEffect = new VolumeCalcEffect( m_kVolumeImage, m_kClipEffect );
        ((VolumeCalcEffect)m_spkEffect).setVolumeMatrix(m_kVolumeViewMatrix);
        m_pkPlane.AttachEffect(m_spkEffect);
        m_pkRenderer.LoadResources(m_pkPlane);
        m_pkPlane.DetachAllEffects();            
    }
}
