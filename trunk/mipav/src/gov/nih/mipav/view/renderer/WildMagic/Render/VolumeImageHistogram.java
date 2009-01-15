package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

import java.awt.Frame;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.TextureEffect;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.sun.opengl.util.Animator;

public class VolumeImageHistogram extends VolumeImageViewer
    implements GLEventListener, KeyListener
{
    private int m_iMouseButton;
    private int m_iMouseX, m_iMouseY;
    private TriMesh m_kBottomOutline = null;
    private TriMesh m_kBottomTri;
    private LevWidgetEffect m_kBottomTriEffect = null; 
    private ColorRGBA m_kHistoColor = new ColorRGBA( 1f, 0f, 0f, 1f );
    private TriMesh m_kTopOutline;
    private TriMesh m_kTopTri;
    private TriMesh m_kUpperSphere;
    private TriMesh m_kMiddleSphere;
    private TriMesh m_kLowerSphere;
    private float m_fScale = .5f;
    
    public VolumeImageHistogram( VolumeTriPlanarInterface kParentFrame, VolumeImage kVolumeImage )
    {
        super(kParentFrame, kVolumeImage );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );  
    }
    /**
     * @param args
     */
    public static void main( VolumeTriPlanarInterface kParentFrame, VolumeImage kVolumeImage, boolean bShowFrame )
    {
        final VolumeImageHistogram kWorld = new VolumeImageHistogram(kParentFrame, kVolumeImage);
        final Frame frame = new Frame(kWorld.GetWindowTitle());
        frame.add( kWorld.GetCanvas() );
        final Animator animator = new Animator( kWorld.GetCanvas() );    
        frame.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                // Run this on another thread than the AWT event queue to
                // avoid deadlocks on shutdown on some platforms
                new Thread(new Runnable() {
                    public void run() {
                        animator.stop();
                    }
                }).start();
                frame.setVisible(false);
            }
        });
        // setting the frame to be undecorated removes the frame title bar and edges
        // this prevents flashing on-screen.
        frame.setUndecorated(!bShowFrame);
        // frame must be set to visible for the gl canvas to be properly initialized.
        frame.setVisible(true);
        frame.setBounds(0,0,
                kWorld.GetWidth(), kWorld.GetHeight() );
        frame.setVisible(bShowFrame);
        kWorld.SetAnimator(animator);
        kWorld.SetFrame(frame);
        animator.start();
    }

    public void display(GLAutoDrawable arg0) {
        if ( m_kAnimator == null )
        {
            return;
        }
        m_pkPlane.DetachAllEffects();
        m_pkPlane.AttachEffect(m_spkEffect);
        m_kCuller.ComputeVisibleSet(m_spkScene);
        m_pkRenderer.ClearBuffers();
        if (m_pkRenderer.BeginScene())
        {          
            m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
            m_pkRenderer.EndScene();
        }
        m_pkRenderer.DisplayBackBuffer();
        m_bDisplay = false;
        
        if ( m_kBottomTriEffect != null )
        {
            m_kBottomTriEffect.SetColor( m_kHistoColor );
            
            float fMidTexX = (m_kBottomTri.VBuffer.GetTCoord2fX(0,1) + m_kBottomTri.VBuffer.GetTCoord2fX(0,2) )/2.0f;
            float fMidTexY = (m_kBottomTri.VBuffer.GetTCoord2fY(0,1) + m_kBottomTri.VBuffer.GetTCoord2fY(0,2) )/2.0f;
            m_kBottomTriEffect.SetMidLine( m_kBottomTri.VBuffer.GetTCoord2fX(0,0),
                    m_kBottomTri.VBuffer.GetTCoord2fY(0,0),
                    fMidTexX, fMidTexY);
        }
    }
    

    public void init(GLAutoDrawable arg0) {
        super.init(arg0);

        m_kAnimator.add( GetCanvas() );
    }
    
    /** 
     * keyPressed callback.
     * @param kKey the KeyEvent triggering the callback.
     */
    public void keyPressed(KeyEvent kKey)
    {
        char ucKey = kKey.getKeyChar();
    }

    public void mouseDragged(MouseEvent e)
    {
        if ( m_kBottomOutline == null )
        {
            return;
        }
        //System.err.println( e.toString() );
        if ( m_iMouseButton == MouseEvent.BUTTON1 )
        {
            float fDiffX = 0;
            float fDiffY = 0;
            if ( m_iMouseX < e.getX() )
            {
                fDiffX = 1;
            }
            else if ( m_iMouseX > e.getX() )
            {
                fDiffX = -1;
            }
            if ( m_iMouseY < e.getY() )
            {
                fDiffY = -1;
            }
            else if ( m_iMouseY > e.getY() )
            {
                fDiffY = 1;
            }
            if ( (fDiffX != 0) || (fDiffY != 0) )
            {
                m_iMouseX = e.getX();
                m_iMouseY = e.getY();
                fDiffX *= 10;
                fDiffY *= 10;

                float fX = (fDiffX/m_iWidth);
                float fY = (fDiffY/m_iHeight);
                ShearTriangle( fX, fY );
            }
        }
        else if ( m_iMouseButton == MouseEvent.BUTTON2 )
        {
            if ( e.isShiftDown() )
            {
                float fY = (m_iHeight - e.getY())/(float)m_iHeight;
                fY = Math.max( 0, fY );
                fY = Math.min( 1, fY );
                //System.err.println( fY );
                ShiftMidTriangle( fY );
            }
            else
            {
                float fDiffX = 0;
                if ( m_iMouseX < e.getX() )
                {
                    fDiffX = 1;
                }
                else if ( m_iMouseX > e.getX() )
                {
                    fDiffX = -1;
                }
                if ( fDiffX != 0 )
                {
                    m_iMouseX = e.getX();
                    m_iMouseY = e.getY();
                    fDiffX *= 10;


                    float fX = (fDiffX/m_iWidth);
                    ShiftTriangle( fX );
                }
            }
        }
    }
  
    public void mousePressed(MouseEvent e)
    {
        m_iMouseButton = e.getButton();
        m_iMouseX = e.getX();
        m_iMouseY = e.getY();
        if ( (m_iMouseX >= 0) && (m_iMouseX < m_iWidth) &&
                (m_iMouseY >= 0) && (m_iMouseY < m_iHeight) )
        {
            
            m_pkPlane.DetachAllEffects();
            m_pkPlane.AttachEffect(m_spkEffect);
            m_kCuller.ComputeVisibleSet(m_spkScene);
            m_pkRenderer.ClearBuffers();
            if (m_pkRenderer.BeginScene())
            {          
                m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
                m_pkRenderer.EndScene();
            }
            
            
            ColorRGBA kPixelColor = m_pkRenderer.GetPixelColor(m_iMouseX, m_iHeight - m_iMouseY);
            //System.err.println( kPixelColor.R + " " + kPixelColor.G + " " + kPixelColor.B + " " + kPixelColor.A );
        }
                
    }
    
    public void mouseReleased(MouseEvent e)
    {
        if ( e.getButton() == MouseEvent.BUTTON3 )
        {
            if ( m_kBottomOutline == null )
            {
                float fX = (2.0f*e.getX()/m_iWidth) - 1.0f;
                float fY = (2.0f*e.getY()/m_iHeight) - 1.0f;
                CreateTriangle(fX,fY);
            }
        }
    }
    
    public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight)
    {
        System.err.println( "VolumeImageHistogram: reshape " + iWidth + " " + iHeight );
        //super.reshape(arg0, iX, iY, m_iWidth, m_iHeight );
        

        if (iWidth > 0 && iHeight > 0)
        {
            m_iWidth = iWidth;
            //m_iHeight = iHeight;
            //m_spkCamera.Perspective = false;
            //m_spkCamera.SetFrustum(90.0f,m_iWidth/(float)m_iHeight,1f,10.0f);
            //m_pkRenderer.OnFrustumChange();

            if (m_pkRenderer != null)
            {
                m_pkRenderer.Resize(iWidth,m_iHeight);
            }
            //arg0.setSize(iWidth,iHeight);

            ((OpenGLRenderer)m_pkRenderer).GetCanvas().setSize( iWidth, m_iHeight );   
        } 
    }
    
    public void setColor( ColorRGBA kColor )
    {
        m_kHistoColor = kColor;
        if ( m_kBottomTriEffect != null )
        {
            m_kBottomTriEffect.SetColor( kColor.R, kColor.G, kColor.B, kColor.A );
        }
    }
    
    protected void CreatePlaneNode()
    {
        m_spkScene = new Node();
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetTChannels(0,2);
        StandardMesh kSM = new StandardMesh(kAttributes);
        m_pkPlane = kSM.Rectangle(2,2,1.0f,1.0f);
        
        float fX0 = -1f;
        float fX1 =  1f;
        float fY0 = -1f;
        float fY1 =  1f;

        m_pkPlane.VBuffer.SetPosition3(0, fX0, fY0, 0);
        m_pkPlane.VBuffer.SetTCoord2(0, 0, 0,0);

        m_pkPlane.VBuffer.SetPosition3(1, fX1, fY0, 0);
        m_pkPlane.VBuffer.SetTCoord2(0, 1, 1,0);

        m_pkPlane.VBuffer.SetPosition3(2, fX0, fY1, 0);
        m_pkPlane.VBuffer.SetTCoord2(0, 2, 0,1);

        m_pkPlane.VBuffer.SetPosition3(3, fX1, fY1, 0);
        m_pkPlane.VBuffer.SetTCoord2(0, 3, 1,1);

        m_spkScene.AttachChild(m_pkPlane);
    }    
    protected void CreateScene ()
    {
        CreatePlaneNode();
        m_spkEffect = new TextureEffect( m_kVolumeImage.GetHisto().GetName() );
        //m_spkEffect = new VolumePlaneEffect( m_kVolumeImage, null, true );
        m_pkPlane.AttachEffect(m_spkEffect);
        m_pkRenderer.LoadResources(m_pkPlane);
        m_pkPlane.DetachAllEffects();
    }    
    
    protected void CreateTriangle(float fX, float fY)
    {
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);
        VertexBuffer kVBuffer = new VertexBuffer(kAttributes, 3);
        kVBuffer.SetPosition3(0, fX, -1, 0.1f);
        kVBuffer.SetColor3( 0, 0, 1.0f, 0.01f, 0.01f );

        kVBuffer.SetPosition3(1, fX+.2f, 0.1f, 0.1f);
        kVBuffer.SetColor3( 0, 1, 1.0f, 0.01f, 0.01f  );

        kVBuffer.SetPosition3(2, fX-.2f, 0.1f, 0.1f);
        kVBuffer.SetColor3( 0, 2, 1.0f, 0.01f, 0.01f  );
        int[] aiData = new int[]{0,1,2};
        IndexBuffer kIBuffer = new IndexBuffer(aiData);
        
        m_kBottomOutline = new TriMesh( kVBuffer, kIBuffer );
        m_kBottomOutline.AttachEffect( new VertexColor3Effect() );
        m_spkScene.AttachChild(m_kBottomOutline);
        
        kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetTChannels(0,2);
        kVBuffer = new VertexBuffer(kAttributes, 3);
        m_kBottomTri = new TriMesh( kVBuffer, kIBuffer );
        m_kBottomTriEffect = new LevWidgetEffect( m_kVolumeImage.GetHisto().GetName() );
        m_kBottomTri.AttachEffect( m_kBottomTriEffect );
        m_spkScene.AttachChild(m_kBottomTri);
        
        kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);
        StandardMesh kSM = new StandardMesh(kAttributes);
        m_kUpperSphere = kSM.Sphere(10,10,0.05f);
        for ( int i = 0; i < m_kUpperSphere.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kUpperSphere.VBuffer.SetColor3(0, i, 0f, 0f, 1f);
        }
        m_kUpperSphere.Local.SetScale((float)m_iHeight/(float)m_iWidth, 1f, 1f);
        m_kUpperSphere.AttachEffect( new VertexColor3Effect() );
        m_spkScene.AttachChild( m_kUpperSphere );

        ScaleTriangle( m_kUpperSphere, m_kBottomTri, m_kBottomOutline );

   
        
        
        
        

        Vector3f kPos0 = m_kBottomOutline.VBuffer.GetPosition3(0);
        Vector3f kPos1 = m_kBottomOutline.VBuffer.GetPosition3(1);
        Vector3f kPos2 = m_kBottomOutline.VBuffer.GetPosition3(2);
        float fNewX = m_fScale * kPos1.X + (1.0f - m_fScale) * kPos0.X;
        float fNewY = m_fScale * kPos1.Y + (1.0f - m_fScale) * kPos0.Y;
        
        kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);
        kVBuffer = new VertexBuffer(kAttributes, 3);
        kVBuffer.SetPosition3(0, fX, -1, 0.11f);
        kVBuffer.SetColor3( 0, 0, 1.0f, 0.01f, 1.0f );

        kVBuffer.SetPosition3(1, fNewX, fNewY, 0.11f);
        kVBuffer.SetColor3( 0, 1, 1.0f, 0.01f, 1.0f  );

        fNewX = m_fScale * kPos2.X + (1.0f - m_fScale) * kPos0.X;
        fNewY = m_fScale * kPos2.Y + (1.0f - m_fScale) * kPos0.Y;
        kVBuffer.SetPosition3(2, fNewX, fNewY, 0.11f);
        kVBuffer.SetColor3( 0, 2, 1.0f, 0.01f, 1.0f  );
        
        m_kTopOutline = new TriMesh( kVBuffer, kIBuffer );
        m_kTopOutline.AttachEffect( new VertexColor3Effect() );
        m_spkScene.AttachChild(m_kTopOutline);
        
        kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetTChannels(0,2);
        kVBuffer = new VertexBuffer(kAttributes, 3);
        m_kTopTri = new TriMesh( kVBuffer, kIBuffer );
        m_kTopTri.AttachEffect( new TextureEffect( m_kVolumeImage.GetHisto().GetName() ) );
        m_spkScene.AttachChild(m_kTopTri);
        
        m_kMiddleSphere = kSM.Sphere(10,10,0.05f);
        for ( int i = 0; i < m_kMiddleSphere.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kMiddleSphere.VBuffer.SetColor3(0, i, 1f, 0f, 1f);
        }
        m_kMiddleSphere.Local.SetScale((float)m_iHeight/(float)m_iWidth, 1f, 1f);
        m_kMiddleSphere.AttachEffect( new VertexColor3Effect() );
        m_spkScene.AttachChild( m_kMiddleSphere );

        ScaleTriangle( m_kMiddleSphere, m_kTopTri, m_kTopOutline );
        
        
        m_kLowerSphere = kSM.Sphere(10,10,0.05f);
        for ( int i = 0; i < m_kLowerSphere.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kLowerSphere.VBuffer.SetColor3(0, i, 0f, 1f, 0f);
        }
        m_kLowerSphere.Local.SetScale((float)m_iHeight/(float)m_iWidth, 1f, 1f);
        m_kLowerSphere.Local.SetTranslate(m_kBottomOutline.VBuffer.GetPosition3(0));
        m_kLowerSphere.AttachEffect( new VertexColor3Effect() );
        m_spkScene.AttachChild( m_kLowerSphere );
        
        m_spkScene.UpdateGS();
    }
    
    protected void ShearTriangle(float fX, float fY)
    {
        float fNewXR = m_kBottomOutline.VBuffer.GetPosition3fX(1);
        float fNewXL = m_kBottomOutline.VBuffer.GetPosition3fX(2);
        if ( fX < 0 )
        {
            if ( (fNewXL + fX) < -1 )
            {
                fX = -1 - fNewXL;
            }
        }
        if ( fX > 0 )
        {
            if ( (fNewXR + fX) >= 1 )
            {
                fX = 1 - fNewXR;
            }
        }      
        
        
        float fNewX, fNewY;
        fNewX = fNewXR;
        if ( fNewXL > -1 )
        {
            fNewX = m_kBottomOutline.VBuffer.GetPosition3fX(1) + fX;
            fNewX = Math.max( -1, fNewX );
            fNewX = Math.min( fNewX, 1 );
        }
        fNewY = m_kBottomOutline.VBuffer.GetPosition3fY(1) + fY;
        fNewY = Math.max( -1, fNewY );
        fNewY = Math.min( fNewY, 1 );
        m_kBottomOutline.VBuffer.SetPosition3( 1, fNewX, fNewY,
                m_kBottomOutline.VBuffer.GetPosition3fZ(1) );

        fNewX = fNewXL;
        if ( fNewXR < 1 )
        {
            fNewX = m_kBottomOutline.VBuffer.GetPosition3fX(2) + fX;
            fNewX = Math.max( -1, fNewX );
            fNewX = Math.min( fNewX, 1 );
        }
        fNewY = m_kBottomOutline.VBuffer.GetPosition3fY(2) + fY;
        fNewY = Math.max( -1, fNewY );
        fNewY = Math.min( fNewY, 1 );
        m_kBottomOutline.VBuffer.SetPosition3( 2, fNewX, fNewY,
                m_kBottomOutline.VBuffer.GetPosition3fZ(2) );
        
        ScaleTriangle( m_kUpperSphere, m_kBottomTri, m_kBottomOutline );        
        
        m_kBottomOutline.VBuffer.Release();
        m_kBottomTri.VBuffer.Release();
        m_kUpperSphere.VBuffer.Release();
        
       
        ShiftMidTriangle(m_fScale);
        
        m_spkScene.UpdateGS();
    }
    
    
    protected void ShiftTriangle(float fX)
    {
        
        float fNewXR = m_kBottomOutline.VBuffer.GetPosition3fX(1);
        float fNewXL = m_kBottomOutline.VBuffer.GetPosition3fX(2);
        if ( fX < 0 )
        {
            if ( (fNewXL + fX) < -1 )
            {
                fX = -1 - fNewXL;
            }
        }
        if ( fX > 0 )
        {
            if ( (fNewXR + fX) >= 1 )
            {
                fX = 1 - fNewXR;
            }
        }     
        
        float fNewX = m_kBottomOutline.VBuffer.GetPosition3fX(0) + fX;
        m_kBottomOutline.VBuffer.SetPosition3( 0, fNewX, m_kBottomOutline.VBuffer.GetPosition3fY(0),
                m_kBottomOutline.VBuffer.GetPosition3fZ(0) );
        fNewX = m_kBottomOutline.VBuffer.GetPosition3fX(1) + fX;
        m_kBottomOutline.VBuffer.SetPosition3( 1, fNewX, m_kBottomOutline.VBuffer.GetPosition3fY(1),
                m_kBottomOutline.VBuffer.GetPosition3fZ(1) );
        fNewX = m_kBottomOutline.VBuffer.GetPosition3fX(2) + fX;
        m_kBottomOutline.VBuffer.SetPosition3( 2, fNewX, m_kBottomOutline.VBuffer.GetPosition3fY(2),
                m_kBottomOutline.VBuffer.GetPosition3fZ(2) );
        
        ScaleTriangle( m_kUpperSphere, m_kBottomTri, m_kBottomOutline );        
        
        m_kBottomOutline.VBuffer.Release();
        m_kBottomTri.VBuffer.Release();
        m_kUpperSphere.VBuffer.Release();
        
        

        ShiftMidTriangle(m_fScale);

        m_kLowerSphere.Local.SetTranslate(m_kBottomOutline.VBuffer.GetPosition3(0));
        
        m_spkScene.UpdateGS();
    }
    
    private void ShiftMidTriangle( float fY )
    {
        m_fScale = fY;
        Vector3f kPos0 = m_kBottomOutline.VBuffer.GetPosition3(0);
        Vector3f kPos1 = m_kBottomOutline.VBuffer.GetPosition3(1);
        Vector3f kPos2 = m_kBottomOutline.VBuffer.GetPosition3(2);
        float fNewX = kPos0.X;
        float fNewY = kPos0.Y;
        m_kTopOutline.VBuffer.SetPosition3(0, fNewX, fNewY, 0.11f);
        
        fNewX = fY * kPos1.X + (1.0f - fY) * kPos0.X;
        fNewY = fY * kPos1.Y + (1.0f - fY) * kPos0.Y;
        m_kTopOutline.VBuffer.SetPosition3(1, fNewX, fNewY, 0.11f);

        fNewX = fY * kPos2.X + (1.0f - fY) * kPos0.X;
        fNewY = fY * kPos2.Y + (1.0f - fY) * kPos0.Y;
        m_kTopOutline.VBuffer.SetPosition3(2, fNewX, fNewY, 0.11f);
        ScaleTriangle( m_kMiddleSphere, m_kTopTri, m_kTopOutline );

        m_kLowerSphere.Local.SetTranslate(m_kBottomOutline.VBuffer.GetPosition3(0));
        m_kTopOutline.VBuffer.Release();
        m_kTopTri.VBuffer.Release();
        m_kMiddleSphere.VBuffer.Release();
        
        m_spkScene.UpdateGS();
    }
    
    
    private void ScaleTriangle( TriMesh kUpperSphere, TriMesh kTri, TriMesh kOutline )
    {
        Vector3f kPos0 = kOutline.VBuffer.GetPosition3(0);
        Vector3f kPos1 = kOutline.VBuffer.GetPosition3(1);
        Vector3f kPos2 = kOutline.VBuffer.GetPosition3(2);
        float fTransX = (kPos0.X + kPos1.X + kPos2.X)/3.0f;
        float fTransY = (kPos0.Y + kPos1.Y + kPos2.Y)/3.0f;

        float fNewX = (kPos0.X - fTransX) * .90f + fTransX;
        float fNewY = (kPos0.Y - fTransY) * .90f + fTransY;
        kTri.VBuffer.SetPosition3(0, fNewX, fNewY, 0.11f);
        float fTX = (fNewX + 1)/2.0f;
        float fTY = (fNewY + 1)/2.0f;
        kTri.VBuffer.SetTCoord2(0, 0, fTX, fTY);


        fNewX = (kPos1.X - fTransX) * .90f + fTransX;
        fNewY = (kPos1.Y - fTransY) * .90f + fTransY;
        kTri.VBuffer.SetPosition3(1, fNewX, fNewY, 0.11f);
        fTX = (fNewX + 1)/2.0f;
        fTY = (fNewY + 1)/2.0f;
        kTri.VBuffer.SetTCoord2(0, 1, fTX, fTY);

        fNewX = (kPos2.X - fTransX) * .90f + fTransX;
        fNewY = (kPos2.Y - fTransY) * .90f + fTransY;
        kTri.VBuffer.SetPosition3(2, fNewX, fNewY, 0.11f);
        fTX = (fNewX + 1)/2.0f;
        fTY = (fNewY + 1)/2.0f;
        kTri.VBuffer.SetTCoord2(0, 2, fTX, fTY);
        
        kUpperSphere.Local.SetTranslate(kPos1);
    }
}
