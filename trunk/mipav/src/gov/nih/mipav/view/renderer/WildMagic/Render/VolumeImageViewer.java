package gov.nih.mipav.view.renderer.WildMagic.Render;

import java.awt.*;
import java.awt.event.*;
import java.util.Vector;

import javax.media.opengl.*;
import com.sun.opengl.util.*;

import WildMagic.LibApplications.OpenGLApplication.*;
import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.SceneGraph.*;
import WildMagic.LibGraphics.Rendering.*;
import WildMagic.LibRenderers.OpenGLRenderer.*;
import gov.nih.mipav.model.structures.*;

public class VolumeImageViewer extends JavaApplication3D
    implements GLEventListener, KeyListener
{
    public VolumeImageViewer( VolumeImage kVolumeImage, VolumeClipEffect kClip, Vector<VolumeObject> kDisplayList )
    {
        super( new String("VolumeImageViewer" + kVolumeImage.GetPostfix() ), 0, 0,
               kVolumeImage.GetImage().getExtents()[0],
               kVolumeImage.GetImage().getExtents()[1],
               new ColorRGBA( 0.0f,0.0f,0.0f,1.0f ) );
        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                                          m_eBuffering, m_eMultisampling,
                                           m_iWidth, m_iHeight );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
        m_kVolumeImage = kVolumeImage;
        m_kClipEffect = kClip;
        if ( kClip != null )
        {
            m_bCrop = true;
        }
        m_kDisplayList = kDisplayList;
        if ( m_kDisplayList != null )
        {
            m_bCrop = false;
            m_bSurfaceAdded = true;
        }
    }

    public void SetAnimator( Animator kAnimator )
    {
        m_kAnimator = kAnimator;
    }
    
    public void SetFrame( Frame kFrame )
    {
        m_kFrame = kFrame;
    }


    /**
     * @param args
     */
    public static void main( VolumeImage kVolumeImage, VolumeClipEffect kClip, Vector<VolumeObject> kDisplayList )
    {
        VolumeImageViewer kWorld = new VolumeImageViewer(kVolumeImage, kClip, kDisplayList);
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
         frame.setVisible(false);
         kWorld.SetAnimator(animator);
         kWorld.SetFrame(frame);
         animator.start();
    }

    public void display(GLAutoDrawable arg0) {
        while ( m_bDisplayFirst )
        {
            UpdateSlice(m_iSlice);
            m_pkPlane.DetachAllEffects();
            m_pkPlane.AttachEffect(m_spkEffect);
            m_kCuller.ComputeVisibleSet(m_spkScene);
            m_pkRenderer.ClearBuffers();
            if (m_pkRenderer.BeginScene())
            {          
                m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
                m_pkRenderer.EndScene();
            }
            m_pkRenderer.FrameBufferToTexSubImage3D( m_pkVolumeNormalTarget, m_iSlice, false );
            m_pkRenderer.DisplayBackBuffer();
            m_iSlice++; 
            if ( m_iSlice >= m_kVolumeImage.GetImage().getExtents()[2])
            {
                m_iSlice = 0;
                m_bDisplayFirst = false;
                //System.err.println("Done first pass");
            }
        }
        while ( m_bDisplaySecond )
        {
            UpdateSlice(m_iSlice);
            m_pkPlane.DetachAllEffects();
            m_pkPlane.AttachEffect(m_spkEffect2);
            m_kCuller.ComputeVisibleSet(m_spkScene);
            m_pkRenderer.ClearBuffers();
            if (m_pkRenderer.BeginScene())
            {          
                m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
                m_pkRenderer.EndScene();
            }
            m_pkRenderer.FrameBufferToTexSubImage3D( m_kVolumeImage.GetNormalMapTarget(), m_iSlice, false );
            m_pkRenderer.DisplayBackBuffer();
            m_iSlice++; 
            if ( m_iSlice >= m_kVolumeImage.GetImage().getExtents()[2])
            {
                m_bDisplaySecond = false;
                m_iSlice = 0;
                //System.err.println("Done second pass");
            }
        }
        while ( m_bCrop )
        {
            m_spkEffect3.ResetClip();
            UpdateSlice(m_iSlice);
            m_pkPlane.DetachAllEffects();
            m_pkPlane.AttachEffect(m_spkEffect3);
            m_kCuller.ComputeVisibleSet(m_spkScene);
            m_pkRenderer.ClearBuffers();
            if (m_pkRenderer.BeginScene())
            {          
                m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
                m_pkRenderer.EndScene();
            }
            m_pkRenderer.FrameBufferToTexSubImage3D( m_kVolumeImage.GetVolumeTarget(), m_iSlice, true );
            m_pkRenderer.DisplayBackBuffer();
            m_iSlice++; 
            if ( m_iSlice >= m_kVolumeImage.GetImage().getExtents()[2])
            {
                m_bCrop = false;
                m_iSlice = 0;
                //System.err.println("Done CROP");
            }
        }
        while ( m_bSurfaceAdded )
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
            m_pkRenderer.DisplayBackBuffer();
            m_iSlice++; 
            if ( m_iSlice >= m_kVolumeImage.GetImage().getExtents()[2])
            {
                m_bSurfaceAdded = false;
                m_iSlice = 0;
                System.err.println("Done SurfaceAdded");
            }
        }
        m_kAnimator.stop();
        m_kFrame.setVisible(false);
    }

    public void displayChanged(GLAutoDrawable arg0, boolean arg1, boolean arg2) { }


    public void init(GLAutoDrawable arg0) {
        arg0.setAutoSwapBufferMode( false );

        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        ((OpenGLRenderer)m_pkRenderer).InitializeState();

        super.OnInitialize();

        // set up camera
        m_spkCamera.SetFrustum(60.0f,1.0f,0.1f,100.0f);
        Vector3f kCLoc = new Vector3f(0.0f,0.0f,2.0f);
        Vector3f kCDir = Vector3f.UNIT_Z_NEG;
        Vector3f kCUp = Vector3f.UNIT_Y;
        Vector3f kCRight = Vector3f.UNIT_X;
        m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);

        m_spkCamera.Perspective = false;
        m_spkCamera.SetFrustum(90.0f,m_iWidth/(float)m_iHeight,1f,10.0f);
        m_pkRenderer.OnFrustumChange();

        CreateScene();

        // initial update of objects
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();

        // initial culling of scene
        m_kCuller.SetCamera(m_spkCamera);
        m_kCuller.ComputeVisibleSet(m_spkScene);

        InitializeCameraMotion(0.001f,0.001f);
        InitializeObjectMotion(m_spkScene);

        //((OpenGLRenderer)m_pkRenderer).ClearDrawable( );
    }

    public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight)
    {
        if ( (iWidth != m_kVolumeImage.GetImage().getExtents()[0]) || 
                (iHeight != m_kVolumeImage.GetImage().getExtents()[1])  )
        {
            return;
        }
        if (iWidth > 0 && iHeight > 0)
        {
            m_iWidth = iWidth;
            m_iHeight = iHeight;
            m_spkCamera.Perspective = false;
            m_spkCamera.SetFrustum(90.0f,m_iWidth/(float)m_iHeight,1f,10.0f);
            m_pkRenderer.OnFrustumChange();

            if (m_pkRenderer != null)
            {
                m_pkRenderer.Resize(iWidth,iHeight);
            }
            arg0.setSize(iWidth,iHeight);
        }
    }

    public GLCanvas GetCanvas()
    {
        return ((OpenGLRenderer)m_pkRenderer).GetCanvas();
    }

    private void CreateScene ()
    {
        m_spkScene = new Node();
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetTChannels(0,3);
        StandardMesh kSM = new StandardMesh(kAttributes);
        m_pkPlane = kSM.Rectangle(2,2,1.0f,1.0f);
        
        float fX0 = -1f;
        float fX1 =  1f;
        float fY0 = -1f;
        float fY1 =  1f;

        m_pkPlane.VBuffer.SetPosition3(0, fX0, fY0, 0);
        m_pkPlane.VBuffer.SetTCoord3(0, 0, 0,0,0.5f);

        m_pkPlane.VBuffer.SetPosition3(1, fX1, fY0, 0);
        m_pkPlane.VBuffer.SetTCoord3(0, 1, 1,0,0.5f);

        m_pkPlane.VBuffer.SetPosition3(2, fX0, fY1, 0);
        m_pkPlane.VBuffer.SetTCoord3(0, 2, 0,1,0.5f);

        m_pkPlane.VBuffer.SetPosition3(3, fX1, fY1, 0);
        m_pkPlane.VBuffer.SetTCoord3(0, 3, 1,1,0.5f);

        if ( !m_bCrop && !m_bSurfaceAdded )
        {
            m_spkEffect = new VolumeCalcEffect( m_kVolumeImage );
            m_pkPlane.AttachEffect(m_spkEffect);
            m_pkRenderer.LoadResources(m_pkPlane);
            m_spkEffect.SetStepSize(m_kVolumeImage);
            m_pkPlane.DetachAllEffects();

            GraphicsImage kImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGB888,m_iWidth,m_iHeight,
                    m_kVolumeImage.GetImage().getExtents()[2],(byte[])null,
                    "VolumeNormals" );
            m_pkVolumeNormalTarget = new Texture();
            m_pkVolumeNormalTarget.SetImage(kImage);
            m_spkEffect2 = new VolumeCalcEffect( "VolumeNormals", m_pkVolumeNormalTarget );
            m_pkPlane.AttachEffect(m_spkEffect2);
            m_pkRenderer.LoadResources(m_pkPlane);
            m_spkEffect2.SetStepSize(m_kVolumeImage);
            m_pkPlane.DetachAllEffects();
        }
        else if ( m_bCrop )
        {
            m_bDisplayFirst = false;
            m_bDisplaySecond = false;
            m_spkEffect3 = new VolumeCalcEffect( m_kVolumeImage, m_kClipEffect );
            m_pkPlane.AttachEffect(m_spkEffect3);
            m_pkRenderer.LoadResources(m_pkPlane);
            m_pkPlane.DetachAllEffects();            
        }
        else
        {
            m_bDisplayFirst = false;
            m_bDisplaySecond = false;   
            m_kSurfaceClip = new SurfaceClipEffect( );
            m_pkPlane.AttachEffect(m_kSurfaceClip);
            m_pkRenderer.LoadResources(m_pkPlane);
            m_pkPlane.DetachAllEffects();
        }
        m_spkScene.AttachChild(m_pkPlane);
    }
    
    private void UpdateSlice( int iZ )
    {
        float fZ = ((float)iZ)/(m_kVolumeImage.GetImage().getExtents()[2] -1);
        m_pkPlane.VBuffer.SetTCoord3(0, 0, 0,0, fZ);
        m_pkPlane.VBuffer.SetTCoord3(0, 1, 1,0, fZ);
        m_pkPlane.VBuffer.SetTCoord3(0, 2, 0,1, fZ);
        m_pkPlane.VBuffer.SetTCoord3(0, 3, 1,1, fZ);    
        m_pkPlane.VBuffer.Release();
    }

    private Node m_spkScene;
    private Culler m_kCuller = new Culler(0,0,null);
    private VolumeImage m_kVolumeImage;
    private VolumeCalcEffect m_spkEffect;
    private VolumeCalcEffect m_spkEffect2;
    private VolumeCalcEffect m_spkEffect3;
    private TriMesh m_pkPlane;
    private int m_iSlice = 0;

    /** Texture for the first-pass rendering of the proxy-geometry: */
    private Texture m_pkVolumeNormalTarget;
    private boolean m_bDisplayFirst = true;
    private boolean m_bDisplaySecond = true;
    private Animator m_kAnimator;
    private Frame m_kFrame;
    private boolean m_bCrop = false;
    private VolumeClipEffect m_kClipEffect = null;
    private Vector<VolumeObject> m_kDisplayList = null;
    private SurfaceClipEffect m_kSurfaceClip = null;
    private boolean m_bSurfaceAdded = false;
}
