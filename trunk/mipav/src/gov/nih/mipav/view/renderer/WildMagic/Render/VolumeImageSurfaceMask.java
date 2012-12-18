package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.KeyListener;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.BitSet;
import java.util.Vector;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;
import javax.media.opengl.awt.GLCanvas;

import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.jogamp.opengl.util.Animator;


public class VolumeImageSurfaceMask extends VolumeImageViewer
    implements GLEventListener, KeyListener
{
    /**  */
    private static final long serialVersionUID = -6880453836501887284L;
    /**
     * @param args
     */
    public static void main( GLCanvas canvas, VolumeTriPlanarInterface kParentFrame, VolumeImage kVolumeImage, Vector<VolumeObject> kDisplayList, boolean bCreateMask )
    {
        VolumeImageSurfaceMask kWorld = new VolumeImageSurfaceMask(canvas, kParentFrame, kVolumeImage, kDisplayList, bCreateMask );
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
    private Vector<VolumeObject> m_kDisplayList = null;
    
    private SurfaceClipEffect m_kSurfaceClip = null;
    private boolean m_bCreateMaskImage = false;
    private ModelImage m_kOutputImage = null;
    private BitSet m_kOutputMask = null;
    
    public VolumeImageSurfaceMask( GLCanvas canvas, VolumeTriPlanarInterface kParentFrame, VolumeImage kVolumeImage, Vector<VolumeObject> kDisplayList, boolean bCreateMask )
    {
        super( canvas, kParentFrame, kVolumeImage );

        m_kDisplayList = kDisplayList;
        m_bCreateMaskImage = bCreateMask;
    }

    @Override
	public void display(GLAutoDrawable arg0) {
        if ( m_kAnimator == null )
        {
            return;
        }      
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
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
            }
            if ( m_bCreateMaskImage )
            {
            	SaveImage(m_iSlice);
            }
            m_pkRenderer.FrameBufferToTexSubImage3D( m_kVolumeImage.GetSurfaceTarget(), m_iSlice );
            m_iSlice++; 
            if ( m_iSlice >= m_kVolumeImage.GetImage().getExtents()[2])
            {            	
                if ( m_kOutputImage != null )
                {
                	m_kOutputImage.disposeLocal(false);
                	m_kOutputImage = null;
                }
                bSurfaceAdded = false;
                m_iSlice = 0;
            }
        }
        dispose(arg0);
    }

    @Override
	public void dispose(GLAutoDrawable arg0)
    {
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        for (int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            { 
                m_kDisplayList.get(i).GetMesh().Release( m_pkRenderer );
            }
        }
        m_kSurfaceClip.dispose();
        m_kSurfaceClip = null;
        m_kOutputImage = null;
        super.dispose(arg0);
    }

    private void SaveImage(int iZ)
    {
        if ( m_kOutputImage == null )
        {
            m_kOutputImage = new ModelImage( ModelStorageBase.ARGB, m_kVolumeImage.GetImage().getExtents(), "SurfaceMask" );
        }
        int iWidth = m_kOutputImage.getExtents()[0];
        int iHeight = m_kOutputImage.getExtents()[1];
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
    
    @Override
	protected void CreateScene ()
    {
        CreatePlaneNode();

        m_kSurfaceClip = new SurfaceClipEffect( );
        m_pkPlane.AttachEffect(m_kSurfaceClip);
        m_pkRenderer.LoadResources(m_pkPlane);
        m_pkPlane.DetachAllEffects();
    }
}
