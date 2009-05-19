package gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer;

import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageViewer;

import java.awt.Frame;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Vector;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Collision.Picker;
import WildMagic.LibGraphics.Effects.TextureEffect;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.sun.opengl.util.Animator;

public class VolumeImageMultiDimensionalTransfer extends VolumeImageViewer
    implements GLEventListener, KeyListener
{
    public int MAX_WIDGETS = 6;
    /** For processing picking: */
    protected Picker m_kPicker = new Picker();
    private int m_iMouseButton;
    private int m_iMouseX, m_iMouseY;
    private Vector2f m_kTMin = new Vector2f(0,0);
    private Vector2f m_kTMax = new Vector2f(1,1);
    private Vector<ClassificationWidget> m_akLev = new Vector<ClassificationWidget>();
    private int m_iCurrent = -1;
    private boolean m_bPickPending = false;
    private ColorRGBA m_kCurrentColor = new ColorRGBA(ColorRGBA.WHITE);
    private boolean m_bAdded = false;
    private JInterfaceBase m_kInterface = null;
    private String m_kWidgetType = new String( "Square" );
    private boolean m_bFirstAdded = false;
    
    public VolumeImageMultiDimensionalTransfer( VolumeTriPlanarInterface kParentFrame, VolumeImage kVolumeImage )
    {
        super(kParentFrame, kVolumeImage );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );  
        m_bDisplay = true;
    }
    /**
     * @param args
     */
    public static void main( VolumeTriPlanarInterface kParentFrame, VolumeImage kVolumeImage, boolean bShowFrame )
    {
        final VolumeImageMultiDimensionalTransfer kWorld = new VolumeImageMultiDimensionalTransfer(kParentFrame, kVolumeImage);
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
        if ( !m_bDisplay )
        {
            return;
        }
        if ( m_kAnimator == null )
        {
            return;
        }
        for ( int i = 0; i < m_akLev.size(); i++ )
        {
            m_akLev.get(i).clearPicked(i == m_iCurrent);
        }
        for ( int i = 0; i < MAX_WIDGETS; i++ )
        {
            if ( i < m_akLev.size() )
            {
                m_akLev.get(i).updateDisplay();
                if ( m_kParent != null )
                {
                    m_kParent.updateLevWidgetState( m_akLev.get(i).getState(), i );
                }
            }
            else
            {
                m_kParent.updateLevWidgetState( ClassificationWidgetState.ZERO_STATE, i );
            }            
        }
        Pick();
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

        if ( m_bAdded ) 
        {
            m_bAdded = false;
            m_akLev.get(m_iCurrent).setColor(m_kCurrentColor);
            m_bFirstAdded = true;
        }
        if ( m_bFirstAdded )
        {
            m_bDisplay = false;
        }
    }
    
    public void dispose()
    {
        m_kPicker = null;
        m_kTMin = null;
        m_kTMax = null;
        m_akLev.clear();
        m_akLev = null;
        m_kCurrentColor = null;
        m_kInterface = null;
        m_kWidgetType = null;
        super.dispose(GetCanvas());
    }


    public void init(GLAutoDrawable arg0) {
        m_bDisplay = true;
        if ( m_bInit )
        {
            return;
        }
        super.init(arg0);
        m_spkCamera.SetFrustum(0, 1, 0, 1,1f,10.0f);
        m_pkRenderer.OnFrustumChange();
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();
        m_kAnimator.add( GetCanvas() );
    }
    

    /** 
     * keyPressed callback.
     * @param kKey the KeyEvent triggering the callback.
     */
    public void keyPressed(KeyEvent kKey)
    {
        char ucKey = kKey.getKeyChar();            
        int iKey = kKey.getKeyCode();
        if ( m_iCurrent != -1 )
        {
            if ( ucKey == KeyEvent.VK_DELETE )
            {
                ClassificationWidget kDeleted = m_akLev.remove(m_iCurrent);
                m_spkScene.DetachChild( kDeleted.getWidget() );
                kDeleted.dispose();
                m_iCurrent = m_akLev.size() -1;
                m_spkScene.UpdateGS();
            }
            else if ((iKey == KeyEvent.VK_RIGHT) || (iKey == KeyEvent.VK_KP_RIGHT))
            {
                m_iCurrent++;
                if ( m_iCurrent >= m_akLev.size() )
                {
                    m_iCurrent = 0;
                }
                m_kInterface.updateColorButton( m_akLev.get(m_iCurrent).getState().Color,
                        m_akLev.get(m_iCurrent).getState().BoundaryEmphasis[0] );              
                m_spkScene.DetachChild( m_akLev.get(m_iCurrent).getWidget() );
                m_spkScene.AttachChild( m_akLev.get(m_iCurrent).getWidget() );
                m_spkScene.UpdateGS();
            }
            else if ((iKey == KeyEvent.VK_LEFT) || (iKey == KeyEvent.VK_KP_LEFT))
            {
                m_iCurrent--;
                if ( m_iCurrent < 0 )
                {
                    m_iCurrent = m_akLev.size()-1;
                }
                m_kInterface.updateColorButton( m_akLev.get(m_iCurrent).getState().Color,
                        m_akLev.get(m_iCurrent).getState().BoundaryEmphasis[0] );              
                m_spkScene.DetachChild( m_akLev.get(m_iCurrent).getWidget() );
                m_spkScene.AttachChild( m_akLev.get(m_iCurrent).getWidget() );
                m_spkScene.UpdateGS();
            }
        }
        m_bDisplay = true;
        GetCanvas().display();
    }
    
    public void mouseDragged(MouseEvent e)
    {
        if ( m_iCurrent == -1 )
        {
            return;
        }
        m_akLev.get(m_iCurrent).processMouseDrag( m_iMouseX, m_iMouseY, m_iMouseButton, e );
        m_spkScene.UpdateGS();
        m_bDisplay = true;
        GetCanvas().display();
        
        m_iMouseX = e.getX();
        m_iMouseY = e.getY();
        //System.err.println( m_iMouseX + " " + m_iMouseY );
    }

    public void mousePressed(MouseEvent e)
    {
        m_iMouseButton = e.getButton();
        m_iMouseX = e.getX();
        m_iMouseY = e.getY();
        if ( (m_iMouseX >= 0) && (m_iMouseX < m_iWidth) &&
                (m_iMouseY >= 0) && (m_iMouseY < m_iHeight) )
        {          
            m_bPickPending = true;
        }  
        m_bDisplay = true;
        GetCanvas().display();
    }
  
    public void mouseReleased(MouseEvent e)
    {
        if ( e.getButton() == MouseEvent.BUTTON3 )
        {
            if ( m_akLev.size() < MAX_WIDGETS )
            {
                float fX = ((float)e.getX()/(float)m_iWidth);
                float fY = ((float)m_iHeight-(float)e.getY())/m_iHeight;
                m_iCurrent++;
                ClassificationWidget kLev = null;
                if ( m_kWidgetType.equals( "Square" ) )
                {
                    kLev = new SquareClassificationWidget(fX,fY, m_kTMin, m_kTMax, m_kVolumeImage.GetHistoName(), m_iWidth, m_iHeight);
                }
                else
                {
                    kLev = new TriangleClassificationWidget(fX,fY, m_kTMin, m_kTMax, m_kVolumeImage.GetHistoName(), m_iWidth, m_iHeight);
                }
                m_spkScene.AttachChild(  kLev.getWidget() );
                m_spkScene.UpdateGS();
                m_akLev.add(kLev);
                m_bAdded = true;
            }
        }
        m_bDisplay = true;
        GetCanvas().display();
    }
    
    public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight)
    {
        m_iWidth = 256;
        m_iHeight = 256;
        m_spkCamera.Perspective = false;
        m_spkCamera.SetFrustum(0, 1, 0, 1,1f,10.0f);
        m_pkRenderer.OnFrustumChange();
        m_pkRenderer.Resize(m_iWidth,m_iHeight);
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().setSize( m_iWidth, m_iHeight );   
        m_bDisplay = true;
    }
    
    public void setBoundary( float fAlpha )
    {
        if (  m_iCurrent != -1 )
        {
            m_akLev.get(m_iCurrent).setBoundary( fAlpha );
            m_bDisplay = true;
            GetCanvas().display();
        }
    }
    
    public void setColor( ColorRGBA kColor )
    {
        m_kCurrentColor.Copy(kColor);
        if (  m_iCurrent != -1 )
        {
            m_akLev.get(m_iCurrent).setColor( kColor );
        }
            m_bDisplay = true;
        GetCanvas().display();
    }

    
    public void SetInterface( JInterfaceBase kInterface )
    {
        m_kInterface = kInterface;
    }
    
    public void setWidget( String kWidgetType )
    {
        m_kWidgetType = new String(kWidgetType);
    }
    
    protected void CreatePlaneNode()
    {
        m_spkScene = new Node();
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetTChannels(0,2);
        kAttributes.SetCChannels(0,3);
        StandardMesh kSM = new StandardMesh(kAttributes);
        m_pkPlane = kSM.Rectangle(2,2,1.0f,1.0f);
        
        float fX0 = 0f;
        float fX1 = 1f;
        float fY0 = 0f;
        float fY1 = 1f;
        
        Vector2f[] akTCoords = m_kVolumeImage.GetHistoTCoords();
         m_kTMin.X = akTCoords[0].X;
         m_kTMin.Y = akTCoords[0].Y;
         m_kTMax.X = akTCoords[2].X;
         m_kTMax.Y = akTCoords[2].Y;
        //m_kTMin.X = 0;
        //m_kTMin.Y = 0;
        //m_kTMax.X = 1;
        //m_kTMax.Y = 1;

        //System.err.println( m_kTMin.X + " " + m_kTMin.Y + " " + m_kTMax.X + " " + m_kTMax.Y );
        
        m_pkPlane.VBuffer.SetPosition3(0, fX0, fY0, 0);
        m_pkPlane.VBuffer.SetTCoord2(0, 0, akTCoords[0]);
        m_pkPlane.VBuffer.SetColor3(0, 0, 1,0,0);

        m_pkPlane.VBuffer.SetPosition3(1, fX1, fY0, 0);
        m_pkPlane.VBuffer.SetTCoord2(0, 1, akTCoords[1]);
        m_pkPlane.VBuffer.SetColor3(0, 1, 1,0,0);

        m_pkPlane.VBuffer.SetPosition3(2, fX0, fY1, 0);
        m_pkPlane.VBuffer.SetTCoord2(0, 2, akTCoords[3]);
        m_pkPlane.VBuffer.SetColor3(0, 2, 1,0,0);

        m_pkPlane.VBuffer.SetPosition3(3, fX1, fY1, 0);
        m_pkPlane.VBuffer.SetTCoord2(0, 3, akTCoords[2]);
        m_pkPlane.VBuffer.SetColor3(0, 3, 1,0,0);

        m_spkScene.AttachChild(m_pkPlane);
    }
    
    protected void CreateScene ()
    {
        CreatePlaneNode();
        //m_spkEffect = new VertexColor3Effect();
        m_spkEffect = new TextureEffect( m_kVolumeImage.GetHistoName() );
        //m_spkEffect = new VolumePlaneEffect( m_kVolumeImage, null, true );
        m_pkPlane.AttachEffect(m_spkEffect);
        m_pkRenderer.LoadResources(m_pkPlane);
        m_pkPlane.DetachAllEffects();
    }    

    protected void Pick()
    {
        if (m_bPickPending)
        {
            float fX = ((float)m_iMouseX/(float)m_iWidth);
            float fY = ((float)m_iHeight-(float)m_iMouseY)/m_iHeight;
            Vector3f kPos = new Vector3f(fX, fY, 10);
            Vector3f kDir = new Vector3f(0, 0, -1);
            m_bPickPending = false;
            if ( m_iCurrent != -1 )
            {
                m_kPicker.Execute(m_akLev.get(m_iCurrent).getWidget(),kPos,kDir,0.0f,
                        Float.MAX_VALUE);
                if (m_kPicker.Records.size() > 0)
                {
                    m_akLev.get(m_iCurrent).setPicked( m_kPicker.Records );
                }
            }
            /*
            for ( int i = 0; i < m_akLev.size(); i++ )
            {
                m_kPicker.Execute(m_akLev.get(i).getWidget(),kPos,kDir,0.0f,
                        Float.MAX_VALUE);

                if (m_kPicker.Records.size() > 0)
                {
                    //m_akLev.get(i).setPicked( m_kPicker.Records );
                    if ( m_akLev.get(i).setPicked( m_kPicker.GetClosestNonnegative() ) )
                    {
                        if ( m_iCurrent != i )
                        {
                            m_iCurrent = i;
                            m_kInterface.updateColorButton(m_akLev.get(i).getState().Color );
                            

                            m_spkScene.DetachChild( m_akLev.get(m_iCurrent).getWidget() );
                            m_spkScene.AttachChild( m_akLev.get(m_iCurrent).getWidget() );
                            m_spkScene.UpdateGS();
                        }
                        break;
                    }
                }
            }
            */
        }
    }    
}
