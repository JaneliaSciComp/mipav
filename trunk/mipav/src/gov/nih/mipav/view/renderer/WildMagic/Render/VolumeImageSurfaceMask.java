package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.KeyListener;
import java.awt.image.BufferedImage;
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
    private SurfaceClipEffect[] m_kSurfaceClip;
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
        
        int surfaceCount = 0;
        for (int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            {                
            	surfaceCount++;
            }
        }
        m_kSurfaceClip = new SurfaceClipEffect[surfaceCount];
        
        
        while ( bSurfaceAdded )
        {
            ModelImage kImage = m_kVolumeImage.GetImage();
            float fClipM1 = ((float)m_iSlice/(kImage.getExtents()[2] -1)) - (.5f/(kImage.getExtents()[2] -1));
            float fClipP1 = ((float)m_iSlice/(kImage.getExtents()[2] -1)) + (.5f/(kImage.getExtents()[2] -1));

            
            m_pkRenderer.ClearBuffers();
            if (m_pkRenderer.BeginScene())
            {          
            	surfaceCount = 0;
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

                        m_kDisplayList.get(i).GetMesh().Local.SetScale( 2.0f/m_kVolumeImage.GetScaleX(), 2.0f/m_kVolumeImage.GetScaleY(), 2.0f/m_kVolumeImage.GetScaleZ());
                        m_kDisplayList.get(i).GetMesh().UpdateGS();
                        
                        
                        m_kDisplayList.get(i).SetPolygonMode(true, WireframeState.FillMode.FM_FILL);
                        ((VolumeSurface)m_kDisplayList.get(i)).Render( m_pkRenderer, m_kCuller, m_kSurfaceClip, surfaceCount, fClipM1, fClipP1 );
                        m_kDisplayList.get(i).SetPolygonMode(true, WireframeState.FillMode.FM_LINE);
                        ((VolumeSurface)m_kDisplayList.get(i)).Render( m_pkRenderer, m_kCuller, m_kSurfaceClip, surfaceCount, fClipM1, fClipP1 );
                        
                        
                        
                        m_kDisplayList.get(i).GetMesh().Local.SetScale(1, 1, 1);
                        
                        m_kDisplayList.get(i).GetScene().Local.SetRotateCopy(kSave);
                        m_kDisplayList.get(i).SetDisplay(bDisplaySave);
                        m_kDisplayList.get(i).SetBackface(bBackFaceSave);
                        m_kDisplayList.get(i).SetPolygonMode(true, kFill);
                        
                        surfaceCount++;
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
                	//m_kOutputImage.calcMinMax();
                	//new ViewJFrameImage(m_kOutputImage);
                    	
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
        int surfaceCount = 0;
        for (int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            { 
                m_kSurfaceClip[surfaceCount++].ReleaseResources( m_pkRenderer, m_kDisplayList.get(i).GetMesh() );
                m_kDisplayList.get(i).GetMesh().Release( m_pkRenderer );
            }
        }
        m_kSurfaceClip = null;
        m_kOutputImage = null;
        super.dispose(arg0);
    }

    protected void CreateScene () {}

    private void SaveImage(int iZ)
    {
        if ( m_kOutputImage == null )
        {
            m_kOutputImage = new ModelImage( ModelStorageBase.ARGB, m_kVolumeImage.GetImage().getExtents(), "SurfaceMask" );
        }
        int iWidth = m_kOutputImage.getExtents()[0];
        int iHeight = m_kOutputImage.getExtents()[1];
        BufferedImage kScreenShot = m_pkRenderer.Screenshot();
        int iSize = iWidth * iHeight * 4;
        try {
            byte[] aucData = new byte[iSize];
            for ( int y = 0; y < iHeight; y++ )
            {
            	for ( int x = 0; x < iWidth; x++ )
            	{
            		int rgb = kScreenShot.getRGB( x, y );
    				aucData[ (y*iWidth +x) * 4 + 3 ] = (byte)((rgb & 0xff000000) >> 32);
    				aucData[ (y*iWidth +x) * 4 + 0 ] = (byte)((rgb & 0x00ff0000) >> 16);
    				aucData[ (y*iWidth +x) * 4 + 1 ] = (byte)((rgb & 0x0000ff00) >> 8);
    				aucData[ (y*iWidth +x) * 4 + 2 ] = (byte)((rgb & 0x000000ff));            		
            	}
            }
            
            m_kOutputImage.importData( iZ * iSize, aucData, false );
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    
}
