package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

import java.awt.Frame;
import java.awt.event.KeyListener;
import java.util.Vector;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;

import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibGraphics.Rendering.WireframeState;

import com.sun.opengl.util.Animator;

public class VolumeImageSurfaceMask extends VolumeImageViewer
    implements GLEventListener, KeyListener
{
    private Vector<VolumeObject> m_kDisplayList = null;
    private SurfaceClipEffect m_kSurfaceClip = null;
    
    public VolumeImageSurfaceMask( VolumeTriPlanarInterface kParentFrame, VolumeImage kVolumeImage, Vector<VolumeObject> kDisplayList )
    {
        super(kParentFrame, kVolumeImage );

        m_kDisplayList = kDisplayList;
    }
    /**
     * @param args
     */
    public static void main( VolumeTriPlanarInterface kParentFrame, VolumeImage kVolumeImage, Vector<VolumeObject> kDisplayList )
    {
        VolumeImageSurfaceMask kWorld = new VolumeImageSurfaceMask(kParentFrame, kVolumeImage, kDisplayList);
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

    public void display(GLAutoDrawable arg0) {
        if ( m_kAnimator == null )
        {
            return;
        }
        boolean bSurfaceAdded = true;
        while ( bSurfaceAdded )
        {
            ModelImage kImage = m_kVolumeImage.GetImage();
            float fClipM1 = ((float)m_iSlice/(kImage.getExtents()[2] -1)) - (.5f/(kImage.getExtents()[2] -1));
            float fClipP1 = ((float)m_iSlice/(kImage.getExtents()[2] -1)) + (.5f/(kImage.getExtents()[2] -1));
            m_kSurfaceClip.SetClip( 4, fClipM1, true );
            m_kSurfaceClip.SetClip( 5, fClipP1, true );

            m_kSurfaceClip.Scale(m_kVolumeImage.GetScaleX(), m_kVolumeImage.GetScaleY(), m_kVolumeImage.GetScaleZ(), true);
            
            m_pkRenderer.ClearBuffers();
            if (m_pkRenderer.BeginScene())
            {          
                for (int i = 0; i < m_kDisplayList.size(); i++ )
                {
                    if ( m_kDisplayList.get(i) instanceof VolumeSurface )
                    {                

                        Matrix3f kSave = new Matrix3f(m_kDisplayList.get(i).GetScene().Local.GetRotate());
                        m_kDisplayList.get(i).GetScene().Local.SetRotateCopy(Matrix3f.IDENTITY);
                        
                        boolean bDisplaySave = m_kDisplayList.get(i).GetDisplay();
                        m_kDisplayList.get(i).SetDisplay(true);
                        

                        boolean bBackFaceSave = m_kDisplayList.get(i).GetBackface();
                        m_kDisplayList.get(i).SetBackface(false);

                        WireframeState.FillMode kFill = m_kDisplayList.get(i).GetPolygonMode();
                        
                        m_kDisplayList.get(i).SetPolygonMode(true, WireframeState.FillMode.FM_FILL);
                        ((VolumeSurface)m_kDisplayList.get(i)).Render( m_pkRenderer, m_kCuller, m_kSurfaceClip );
                        m_kDisplayList.get(i).SetPolygonMode(true, WireframeState.FillMode.FM_LINE);
                        ((VolumeSurface)m_kDisplayList.get(i)).Render( m_pkRenderer, m_kCuller, m_kSurfaceClip );
                        

                        m_kDisplayList.get(i).GetScene().Local.SetRotateCopy(kSave);
                        m_kDisplayList.get(i).SetDisplay(bDisplaySave);
                        m_kDisplayList.get(i).SetBackface(bBackFaceSave);
                        m_kDisplayList.get(i).SetPolygonMode(true, kFill);
                    }
                }
                m_pkRenderer.EndScene();
            }
            m_pkRenderer.FrameBufferToTexSubImage3D( m_kVolumeImage.GetSurfaceTarget(), m_iSlice, false );
            //m_pkRenderer.DisplayBackBuffer();
            m_iSlice++; 
            if ( m_iSlice >= m_kVolumeImage.GetImage().getExtents()[2])
            {
                bSurfaceAdded = false;
                m_iSlice = 0;
                //System.err.println("Done SurfaceAdded");
                m_kAnimator.stop();
                m_kAnimator.remove(arg0);
                m_kAnimator = null;
                m_kFrame.setVisible(false);
            }
        }
    }

    protected void CreateScene ()
    {
        CreatePlaneNode();

        m_kSurfaceClip = new SurfaceClipEffect( );
        m_pkPlane.AttachEffect(m_kSurfaceClip);
        m_pkRenderer.LoadResources(m_pkPlane);
        m_pkPlane.DetachAllEffects();
    }
}