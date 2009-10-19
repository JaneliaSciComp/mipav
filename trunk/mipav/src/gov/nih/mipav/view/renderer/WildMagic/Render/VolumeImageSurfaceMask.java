package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.KeyListener;
import java.util.Vector;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;

import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

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
        if ( m_pkRenderer != null )
        {
            ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        }
        boolean bSurfaceAdded = true;
        boolean bDrawSurface = false;
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
                        bDrawSurface = true;
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
                //writeImage();
            }
            m_pkRenderer.FrameBufferToTexSubImage3D( m_kVolumeImage.GetSurfaceTarget(), m_iSlice, true );
            //m_pkRenderer.DisplayBackBuffer();
            m_iSlice++; 
            if ( m_iSlice >= m_kVolumeImage.GetImage().getExtents()[2])
            {
                if ( bDrawSurface )
                {
                    ModelImage kMask = VolumeImage.CreateImageFromTexture( m_kVolumeImage.GetSurfaceTarget().GetImage(), true );
                    // The algorithm has completed and produced a new image to be displayed.
                    try {
                        new ViewJFrameImage(kMask, null, new Dimension(610, 200));
                    } catch (OutOfMemoryError error) {
                        MipavUtil.displayError("Out of memory: unable to open new frame");
                    }
                }
                bSurfaceAdded = false;
                m_iSlice = 0;
            }
        }
        dispose(arg0);
    }

    public void dispose(GLAutoDrawable arg0)
    {
        for (int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            { 
                boolean bShared = 
                    m_kDisplayList.get(i).GetMesh().VBuffer.GetShared();
                m_kDisplayList.get(i).GetMesh().VBuffer.SetShared(false);
                m_kDisplayList.get(i).GetMesh().VBuffer.Release( m_pkRenderer );
                m_kDisplayList.get(i).GetMesh().VBuffer.SetShared(bShared);
                bShared = 
                    m_kDisplayList.get(i).GetMesh().IBuffer.GetShared();
                m_kDisplayList.get(i).GetMesh().IBuffer.SetShared(false);
                m_kDisplayList.get(i).GetMesh().IBuffer.Release( m_pkRenderer );
                m_kDisplayList.get(i).GetMesh().IBuffer.SetShared(bShared);
            }
        }
        m_kSurfaceClip.dispose();
        m_kSurfaceClip = null;
        super.dispose(arg0);
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
