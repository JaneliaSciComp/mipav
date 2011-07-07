package gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer;

import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageViewer;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.util.Vector;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;
import javax.media.opengl.awt.GLCanvas;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Collision.Picker;
import WildMagic.LibGraphics.Effects.TextureEffect;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

public class VolumeImageMultiDimensionalTransfer extends VolumeImageViewer
implements GLEventListener, KeyListener
{

	private static final long serialVersionUID = 401740242306942355L;
	public int MAX_WIDGETS = 6;
	/** For processing picking: */
	protected Picker m_kPicker = new Picker();
	private int m_iMouseButton;
	private int m_iMouseX, m_iMouseY;
	private Vector2f m_kTMin = new Vector2f(0,0);
	private Vector2f m_kTMax = new Vector2f(1,1);
	private Vector<ClassificationWidget> m_akLev = new Vector<ClassificationWidget>();
	private int m_iCurrent = -1;
	private int m_iPicked = -1;
	private boolean m_bPickPending = false;
	private ColorRGBA m_kCurrentColor = new ColorRGBA(ColorRGBA.WHITE);
	private boolean m_bAdded = false;
	private JInterfaceBase m_kInterface = null;
	private String m_kWidgetType = new String( "Square" );
	private boolean m_bFirstAdded = false;
	private boolean m_bUpdateLev = false;

	public VolumeImageMultiDimensionalTransfer( GLCanvas canvas, VolumeTriPlanarInterface kParentFrame, VolumeImage kVolumeImage )
	{
		super( canvas, kParentFrame, kVolumeImage );
		((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );       
		((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );  
		m_bDisplay = true;
	}
	/**
	 * @param args
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
	 */

	public void display()
	{
		m_bDisplay = true;
		GetCanvas().display();
	}

	@Override
	public void display(GLAutoDrawable arg0) {
		if ( !m_bDisplay )
		{
			return;
		}
		if ( m_kAnimator == null )
		{
			return;
		}      
		((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
		if ( m_bUpdateLev )
		{
			m_bUpdateLev = false;
			updateLev();
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
		//m_pkPlane.DetachAllEffects();
		//m_pkPlane.AttachEffect(m_spkEffect);
		// m_pkPlane.UpdateGS();
		m_spkScene.UpdateGS();
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
			GetCanvas().display();
		}
		if ( m_bFirstAdded )
		{
			m_bDisplay = false;
		}
	}

	@Override
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


	public int getPicked()
	{
		return m_iCurrent;
	}


	public Vector<ClassificationWidget> getWidgets()
	{
		Vector<ClassificationWidget> kWidgetList = new Vector<ClassificationWidget>();
		for ( int i = 0; i < m_akLev.size(); i++ )
		{
			ClassificationWidget kWidget = m_akLev.get(i);
			if ( kWidget instanceof SquareClassificationWidget )
			{
				kWidgetList.add( new SquareClassificationWidget((SquareClassificationWidget)kWidget) );
			}
			else if ( kWidget instanceof TriangleClassificationWidget )
			{
				kWidgetList.add( new TriangleClassificationWidget((TriangleClassificationWidget)kWidget) );
			}
		}
		return kWidgetList;
	}

	@Override
	public void init(GLAutoDrawable arg0) {

		m_bDisplay = true;
		if ( m_bInit )
		{
			return;
		}      
		((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
		super.init(arg0);


        // set up camera
        Vector3f kCLoc = new Vector3f(0.0f,0.0f,2.0f);
        Vector3f kCDir = Vector3f.UNIT_Z_NEG;
        Vector3f kCUp = Vector3f.UNIT_Y;
        Vector3f kCRight = Vector3f.UNIT_X;
        m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);
        m_spkCamera.Perspective = false;        
        m_spkCamera.SetFrustum(-1,1,-1,1,1f,10.0f);
        m_pkRenderer.OnFrustumChange();
		
		
		m_spkScene.UpdateGS();
		m_spkScene.UpdateRS();
		m_kAnimator.add( GetCanvas() );
		
		m_kCuller.SetCamera(m_spkCamera);


		//following code places a square box over the 2d histogram the first time it is shown
		if(getM_akLev().size() == 0) {
			long currentTime = System.currentTimeMillis();
			int mod = MouseEvent.BUTTON3;
			MouseEvent evt1 = new MouseEvent(GetCanvas(), MouseEvent.MOUSE_PRESSED, currentTime, mod, 127, 127, 1, false, MouseEvent.BUTTON3);
			GetCanvas().dispatchEvent(evt1);

			currentTime = System.currentTimeMillis();
			mod = MouseEvent.BUTTON3;
			MouseEvent evt2 = new MouseEvent(GetCanvas(), MouseEvent.MOUSE_RELEASED, currentTime, mod, 127, 127, 1, false, MouseEvent.BUTTON3);
			GetCanvas().dispatchEvent(evt2);

			currentTime = System.currentTimeMillis();
			mod = MouseEvent.BUTTON1;
			MouseEvent evt3 = new MouseEvent(GetCanvas(), MouseEvent.MOUSE_PRESSED, currentTime, mod, 150, 150, 1, false, MouseEvent.BUTTON1);
			//GetCanvas().dispatchEvent(evt3);

			currentTime = System.currentTimeMillis();
			mod = MouseEvent.BUTTON1;
			MouseEvent evt4 = new MouseEvent(GetCanvas(), MouseEvent.MOUSE_DRAGGED, currentTime, mod, 210, 210, 1, false, MouseEvent.BUTTON1);
			//GetCanvas().dispatchEvent(evt4);
		}
	}

	/** 
	 * keyPressed callback.
	 * @param kKey the KeyEvent triggering the callback.
	 */
	@Override
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

	@Override
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
	}

	@Override
	public void mouseMoved(MouseEvent e)
	{
		m_bDisplay = true;
		GetCanvas().display();
	}

	@Override
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

	@Override
	public void mouseReleased(MouseEvent e)
	{   
		if ( e.getButton() == MouseEvent.BUTTON3 )
		{	
			if ( m_akLev.size() < MAX_WIDGETS )
			{
				float fX = 2.0f * (((float)e.getX()/(float)m_iWidth) - .5f);
				float fY = 2.0f * (((float)m_iHeight-(float)e.getY())/m_iHeight - .5f);

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
		if ( (m_akLev.size() > 0) && (m_iCurrent != -1) )
		{
			m_akLev.get(m_iCurrent).clearPicked( );
		}
		m_bDisplay = true;
		GetCanvas().display();
	}

	@Override
	public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight)
	{      
		((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
		m_iWidth = 256;
		m_iHeight = 256;
        m_spkCamera.Perspective = false;
        m_spkCamera.SetFrustum(-1,1,-1,1,1f,10.0f);
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

	public void setPicked( int value )
	{
		m_iPicked = value;
	}

	public void setWidget( String kWidgetType )
	{
		m_kWidgetType = new String(kWidgetType);
	}

	public void setWidgets(Vector<ClassificationWidget> kWidgetList)
	{
		m_akLev = kWidgetList;
		m_bUpdateLev = true;        
	}

	private void updateLev()
	{
		for ( int i = 0; i < m_akLev.size(); i++ )
		{
			m_iCurrent++;
			ClassificationWidget kLev = m_akLev.get(i);
			m_spkScene.AttachChild( kLev.getWidget() );
			setColor( m_akLev.get(m_iCurrent).getColor() );
			m_kInterface.updateColorButton( m_akLev.get(m_iCurrent).getState().Color,
					m_akLev.get(m_iCurrent).getState().BoundaryEmphasis[0] );              
			m_spkScene.DetachChild( m_akLev.get(m_iCurrent).getWidget() );
			m_spkScene.AttachChild( m_akLev.get(m_iCurrent).getWidget() );
			m_spkScene.UpdateGS();
			m_bAdded = true;
			m_bDisplay = true;
		}
		m_iCurrent = m_iPicked;
		if ( m_bAdded )
		{
			setColor( m_akLev.get(m_iCurrent).getColor() );
			m_kInterface.updateColorButton( m_akLev.get(m_iCurrent).getState().Color,
					m_akLev.get(m_iCurrent).getState().BoundaryEmphasis[0] );              
			m_spkScene.DetachChild( m_akLev.get(m_iCurrent).getWidget() );
			m_spkScene.AttachChild( m_akLev.get(m_iCurrent).getWidget() );
			m_spkScene.UpdateGS();
		}
	}

	@Override
	protected void CreatePlaneNode()
	{
		m_spkScene = new Node();
		Attributes kAttributes = new Attributes();
		kAttributes.SetPChannels(3);
		kAttributes.SetTChannels(0,2);
		kAttributes.SetCChannels(0,3);
		StandardMesh kSM = new StandardMesh(kAttributes);
		m_pkPlane = kSM.Rectangle(2,2,1.0f,1.0f);

        float fX0 = -1f;
        float fX1 =  1f;
        float fY0 = -1f;
        float fY1 =  1f;

		Vector2f[] akTCoords = m_kVolumeImage.GetHistoTCoords();
		m_kTMin.X = akTCoords[0].X;
		m_kTMin.Y = akTCoords[0].Y;
		m_kTMax.X = akTCoords[2].X;
		m_kTMax.Y = akTCoords[2].Y;

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

	@Override
	protected void CreateScene ()
	{
		CreatePlaneNode();
		m_spkEffect = new TextureEffect( m_kVolumeImage.GetHistoName() );
		m_pkPlane.AttachEffect(m_spkEffect);
		m_pkRenderer.LoadResources(m_pkPlane);
	}    

	protected void Pick()
	{

		//Vector3f kPos = new Vector3f(0,0,10);
		//Vector3f kDir = new Vector3f(0,0,1);  // the pick ray


		if (m_bPickPending)
		{
			m_bPickPending = false;
			if ( m_iCurrent != -1 )
			{
				if ( m_akLev.get(m_iCurrent).Pick(m_iMouseX, m_iMouseY ) )
				{
					return;
				}
			}
			for ( int i = 0; i < m_akLev.size(); i++ )
			{
				if ( m_akLev.get(i).Pick(m_iMouseX, m_iMouseY ) )
				{
					m_iCurrent = i;
					m_kInterface.updateColorButton( m_akLev.get(m_iCurrent).getState().Color,
							m_akLev.get(m_iCurrent).getState().BoundaryEmphasis[0] );              
					m_spkScene.DetachChild( m_akLev.get(m_iCurrent).getWidget() );
					m_spkScene.AttachChild( m_akLev.get(m_iCurrent).getWidget() );
					m_spkScene.UpdateGS();
					return;
				}
			}
		}
	}

	public synchronized Vector<ClassificationWidget> getM_akLev() {
		return m_akLev;
	}



}
