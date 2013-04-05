package gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer;

import gov.nih.mipav.view.CustomUIBuilder;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.renderer.WildMagic.PlaneRender_WM;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRenderBase;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageViewer;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeSlices;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Vector;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;
import javax.media.opengl.awt.GLCanvas;
import javax.swing.JPanel;

import com.jogamp.opengl.util.Animator;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.TextureEffect;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.Rendering.Renderer;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

/**
 * This class implements the 2D Histogram display and user-interaction widget in the JPanelMultiDimensionalTransfer class.
 * The widget is displayed with a GLCanvas with a screen-space polygon displaying the 2D Histogram texture as the background.
 * The user can add ClassificationWidgets to the canvas and change their location and size as well as color and blending parameters.
 * This class provides the display and user-interaction for those widgets.
 * 
 * The widget parameters are then passed to the Volume Renderer GLSL program in the VolumeRayCast class where they
 * determine how the volume is displayed.
 *
 */

public class VolumeImageMultiDimensionalTransfer extends VolumeImageViewer
implements GLEventListener, KeyListener
{

	private static final long serialVersionUID = 401740242306942355L;
	/** The maximum number of widgets that can be added to the display. 
	 * More can be added, but then the Volume Renderer GLSL program should be changed to add the new widgets. */
	public int MAX_WIDGETS = 6;
	
	/** main container for this object: */
	private JPanel container;
	/** Stores which button was pressed during mouse drag. */
	private int m_iMouseButton;
	/** Stores the mouse position for picking. */
	private int m_iMouseX, m_iMouseY;
	/** The bounds on the 2D Histogram texture coordinates used to display the 2D Histogram.
	 * The texture coordinates are clamped to show only the portions of the texture that are above a threshold value. */
	private Vector2f m_kTMin = new Vector2f(0,0);
	private Vector2f m_kTMax = new Vector2f(1,1);
	/** The list of widgets displayed on the 2D Histogram. */
	private Vector<ClassificationWidget> m_akWidgets = new Vector<ClassificationWidget>();
	/** The ID of the active and previously active widget. */
	private int m_iCurrent = -1, m_iPrevious = -1;
	/** The ID of the  */
	private int m_iPicked = -1;
	/** When true there is a pending mouse pick to process. */
	private boolean m_bPickPending = false;
	/** The current color to use to create a new Widget. */
	private ColorRGBA m_kCurrentColor = new ColorRGBA(ColorRGBA.WHITE);
	/** When true a new Widget was added with the mouse. */
	private boolean m_bAdded = false;
	/** Reference to the containing JPanel. */
	private JInterfaceBase m_kInterface = null;
	/** Defines the current selected widget type. */
	private String m_kWidgetType = new String( "Circle" );
	private boolean m_bFirstAdded = false;
	private boolean m_bUpdateLev = false;
	private int m_iLUTIndex = 0;
	
	/**
	 * Create a new VolumeImageMultiDimensionalTranfer display.
	 * @param canvas The canvas to use for the display. The canvas shares a context with the VolumeTriPlanarRenderer.
	 * @param kParentFrame parent frame.
	 * @param kVolumeImage the VolumeImage with the associated 2D Histogram.
	 */
	public VolumeImageMultiDimensionalTransfer( GLCanvas canvas, VolumeTriPlanarInterface kParentFrame, VolumeImage kVolumeImage )
	{
		super( canvas, kParentFrame, kVolumeImage );
		((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );       
		((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );
		createContainer(GetCanvas());
		m_bDisplay = true;
	}
    
    
	/**
	 * Iridescence.main creates the Iridescence object and window frame to contain the GLCanvas. An Animator object is
	 * created with the GLCanvas as an argument. The Animator provides the same function as the glutMainLoop() function
	 * call commonly used in OpenGL applications.
	 */
	public static VolumeImageMultiDimensionalTransfer main(GLCanvas kCanvas, VolumeTriPlanarInterface kParent, 
            VolumeImage kVolumeImageA, boolean displayInSeparateFrame )
	{
		VolumeImageMultiDimensionalTransfer kWorld = new VolumeImageMultiDimensionalTransfer(kCanvas, kParent, kVolumeImageA);
		
		/* Animator serves the purpose of the idle function, calls display: */
    	final Animator animator = new Animator( kWorld.GetCanvas() );
        animator.setRunAsFastAsPossible(true);
        animator.start();
		if ( displayInSeparateFrame )
		{
			Frame frame = new Frame(kWorld.GetWindowTitle());
			frame.add( kWorld.GetCanvas() );
			frame.setSize(kWorld.GetCanvas().getWidth(), kWorld.GetCanvas().getHeight());
			/* Animator serves the purpose of the idle function, calls display: */
			frame.addWindowListener(new WindowAdapter() {
				@Override
				public void windowClosing(WindowEvent e) {
					// Run this on another thread than the AWT event queue to
					// avoid deadlocks on shutdown on some platforms
					new Thread(new Runnable() {
						@Override
						public void run() {
							animator.stop();
							System.exit(0);
						}
					}).start();
				}
			});
			frame.setVisible(true);
		}
        return kWorld;
	}

	
	public void clearAllWidgets()
	{
		m_iCurrent = -1;
		for ( int i = m_akWidgets.size() - 1; i >= 0; i-- )
		{
			ClassificationWidget kDeleted = m_akWidgets.remove(i);
			m_spkScene.DetachChild( kDeleted.getWidget() );
			kDeleted.dispose();
		}
		m_spkScene.UpdateGS();
		display();
	}
	
	/**
	 * Set the display to true and call GLCanvas.display;
	 */
	public void display()
	{
		m_bDisplay = true;
		GetCanvas().display();
	}
	
	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageViewer#display(javax.media.opengl.GLAutoDrawable)
	 */
	public void display(GLAutoDrawable arg0) {
		if ( !m_bInit )
		{
			init(arg0);
		}
		// return early if not displaying:
		if ( !m_bDisplay )
		{
			return;
		}
		// return early if the animator is null:
//		if ( m_kAnimator == null )
//		{
//			return;
//		}      
		// Set the drawable in the renderer:
		((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
		// if dispose, call the dispose function and return.
		if ( m_bDispose )
		{
			dispose(arg0);
			return;
		}
		// Update the widgets:
		if ( m_bUpdateLev )
		{
			m_bUpdateLev = false;
			updateLev();
		}
		// Set the current widget as selected if the current has changed:
		if ( m_iCurrent != m_iPrevious )
		{
			for ( int i = 0; i < m_akWidgets.size(); i++ )
			{
				m_akWidgets.get(i).setPicked(i == m_iCurrent);
			}
			m_iPrevious = m_iCurrent;
		}
		// Update the parent with the widget parameters so
		// the Volume Renderer GLSL program can be updated:
		for ( int i = 0; i < m_akWidgets.size(); i++ )
		{
			m_akWidgets.elementAt(i).updateDisplay();
		}		
		m_kParent.updateLevWidgetState( m_akWidgets );
		
		// Call picking to see if the selected widget has changed:
		Pick(m_pkRenderer);    
		// Draw the widgets on the 2D Histogram background"
		m_spkScene.UpdateGS();
		m_kCuller.ComputeVisibleSet(m_spkScene);
		m_pkRenderer.ClearBuffers();
		if (m_pkRenderer.BeginScene())
		{          
			m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
			m_pkRenderer.EndScene();
		}
		m_pkRenderer.DisplayBackBuffer();    

		// If a widget was added rendering needs to be called a 2nd time after setting the color:
		if ( m_bAdded ) 
		{
			m_bAdded = false;
			m_kInterface.updateColorButton( m_akWidgets.get(m_iCurrent).getState().Color,
					m_akWidgets.get(m_iCurrent).getState().BoundaryEmphasis[0] );         
			m_bFirstAdded = true;
			GetCanvas().display();
		}
		if ( m_bFirstAdded )
		{
			m_bDisplay = false;
		}
	}

	/* (non-Javadoc)
	 * @see WildMagic.LibApplications.OpenGLApplication.JavaApplication3D#dispose()
	 */
	public void dispose()
	{
		m_bDispose = true;
		GetCanvas().display();
	}

	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageViewer#dispose(javax.media.opengl.GLAutoDrawable)
	 */
	public void dispose(GLAutoDrawable arg0)
	{
		// All OpenGL objects must be disposed through the Renderer class, when
		// the OpenGL Context is active:
		for ( int i = m_akWidgets.size(); i > 0; i-- )
		{
			ClassificationWidget kWidget = m_akWidgets.remove(0);
			kWidget.getWidget().DetachAllLights();
			kWidget.getWidget().UpdateRS();
			// Releases the GPU resources:
			m_pkRenderer.ReleaseAllResources( kWidget.getWidget() );
			kWidget.dispose();
		}
		m_akWidgets = null;
		m_kTMin = null;
		m_kTMax = null;
		m_kCurrentColor = null;
		m_kInterface = null;
		m_kWidgetType = null;
		super.dispose(arg0);
	}
	
	/**
	 * Returns the container for this object. The container has a scroll pane and slider for the depth.
	 * @return
	 */
	public JPanel getContainingPanel()
	{
		return container;
	}


	public synchronized Vector<ClassificationWidget> getM_akLev() {
		return m_akWidgets;
	}

	/**
	 * Returns the ID of the currently picked Widget. 
	 * @return the ID of the currently picked Widget. 
	 */
	public int getPicked()
	{
		return m_iCurrent;
	}

	/**
	 * Returns the current Widgets.
	 * @return the current Widgets.
	 */
	public Vector<ClassificationWidget> getWidgets()
	{
		return m_akWidgets;
	}

	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageViewer#init(javax.media.opengl.GLAutoDrawable)
	 */
	public void init(GLAutoDrawable arg0) {

		m_bDisplay = true;
		if ( m_bInit )
		{
			return;
		}      
		((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
		super.init(arg0);

        // set up camera for rendering the screen-space quad:
        Vector3f kCLoc = new Vector3f(0.0f,0.0f,2.0f);
        Vector3f kCDir = Vector3f.UNIT_Z_NEG;
        Vector3f kCUp = Vector3f.UNIT_Y;
        Vector3f kCRight = Vector3f.UNIT_X;
        m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);
        m_spkCamera.Perspective = false;        
        m_spkCamera.SetFrustum(-1,1,-1,1,1f,10.0f);
        m_pkRenderer.OnFrustumChange();
		
		// Update the scene graph:
		m_spkScene.UpdateGS();
		m_spkScene.UpdateRS();
		//m_kAnimator.add( GetCanvas() );
		
		m_kCuller.SetCamera(m_spkCamera);


		//following code places a square box over the 2d histogram the first time it is shown
		if(getM_akLev().size() == 0) {
			m_kWidgetType = new String( "Square" );
			
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
			GetCanvas().dispatchEvent(evt3);

			currentTime = System.currentTimeMillis();
			mod = MouseEvent.BUTTON1;
			MouseEvent evt4 = new MouseEvent(GetCanvas(), MouseEvent.MOUSE_DRAGGED, currentTime, mod, 210, 210, 1, false, MouseEvent.BUTTON1);
			GetCanvas().dispatchEvent(evt4);
		}
	}

	/** 
	 * keyPressed callback.
	 * @param kKey the KeyEvent triggering the callback.
	 */
	public void keyPressed(KeyEvent kKey)
	{
		char ucKey = kKey.getKeyChar();            
		int iKey = kKey.getKeyCode();
		int iCurrentPrev = m_iCurrent;
		if ( m_iCurrent != -1 )
		{
			// Delete the current widget:
			if ( ucKey == KeyEvent.VK_DELETE )
			{
				ClassificationWidget kDeleted = m_akWidgets.remove(m_iCurrent);
				m_spkScene.DetachChild( kDeleted.getWidget() );
				kDeleted.dispose();
				m_iCurrent = m_akWidgets.size() -1;
				m_spkScene.UpdateGS();
			}
			else if ((iKey == KeyEvent.VK_RIGHT) || (iKey == KeyEvent.VK_KP_RIGHT))
			{
				// Change the selected widget with the arrow keys:
				m_iCurrent++;
				if ( m_iCurrent >= m_akWidgets.size() )
				{
					m_iCurrent = 0;
				}   
				m_spkScene.DetachChild( m_akWidgets.get(m_iCurrent).getWidget() );
				m_spkScene.AttachChild( m_akWidgets.get(m_iCurrent).getWidget() );
				m_spkScene.UpdateGS();
			}
			else if ((iKey == KeyEvent.VK_LEFT) || (iKey == KeyEvent.VK_KP_LEFT))
			{
				// Change the selected widget with the arrow keys:
				m_iCurrent--;
				if ( m_iCurrent < 0 )
				{
					m_iCurrent = m_akWidgets.size()-1;
				}      
				m_spkScene.DetachChild( m_akWidgets.get(m_iCurrent).getWidget() );
				m_spkScene.AttachChild( m_akWidgets.get(m_iCurrent).getWidget() );
				m_spkScene.UpdateGS();
			}
			else if ( ucKey == 'b' )
			{
		        if ( m_kCull != null )
		        {
		            m_kCull.Enabled = !m_kCull.Enabled;
		            m_kCull.CullFace = CullState.CullMode.CT_BACK;
		    		m_spkScene.UpdateGS();
		        }
				
			}
			if ( m_iCurrent >= 0 )
			{
				m_kInterface.updateColorButton( m_akWidgets.get(m_iCurrent).getState().Color,
						m_akWidgets.get(m_iCurrent).getState().BoundaryEmphasis[0] );
				m_akWidgets.get(m_iCurrent).setPicked(true);
			}
		}
		m_bDisplay = true;
		GetCanvas().display();
	}

	public void load( String fileName )
	{
        try {
            ObjectInputStream objstream;
            objstream = new ObjectInputStream(new FileInputStream(fileName));
            int size = objstream.readInt();
            for ( int i = 0; i < size; i++ )
            {
            	int type = objstream.readInt();
            	ClassificationWidget currentWidget = null;
            	if ( type == ClassificationWidgetState.Circle )
            	{
            		CircleClassificationWidget kWidget = (CircleClassificationWidget)objstream.readObject();
        			currentWidget = kWidget;
            	}
            	else if ( type == ClassificationWidgetState.Triangle )
            	{
            		TriangleClassificationWidget kWidget = (TriangleClassificationWidget)objstream.readObject();
        			currentWidget = kWidget;
            	}
            	else if ( type == ClassificationWidgetState.Square )
            	{
            		SquareClassificationWidget kWidget = (SquareClassificationWidget)objstream.readObject();
        			currentWidget = kWidget;
            	}
            	
            	if ( currentWidget != null )
            	{
            		currentWidget.setTexture( m_kVolumeImage.GetHistoTarget() );
            		ClassificationWidgetState widgetState = currentWidget.getSavedWidgetState();
            		if ( widgetState != null )
            		{
                		m_iLUTIndex = (int)widgetState.UseColorMap[0];
            			if ( m_iLUTIndex != -1 )
            			{
            				boolean bReverseLUT = widgetState.InvertLUT;

            				Texture kMap = VolumeTriPlanarRenderBase.getHistogramLUTTexture( m_iLUTIndex, bReverseLUT );

            				currentWidget.setLUT( kMap, m_iLUTIndex, bReverseLUT );
            			}	        				
        				currentWidget.setState(widgetState);
            		}
            		
            		
    				m_iCurrent++;
    				
    				// Attach the widget scene-graph to the main scene-graph:
    				m_spkScene.AttachChild( currentWidget.getWidget() );
    				m_spkScene.UpdateGS();
    				// Save the widget in the list:
    				m_akWidgets.add(currentWidget);
    				m_bAdded = true;     
            	}
            }
            objstream.close();
            m_iPicked = m_iCurrent;
    		if ( (m_akWidgets.size() > 0) && (m_iCurrent != -1) )
    		{
    			m_akWidgets.get(m_iCurrent).clearPicked( );
    		}
    		m_bDisplay = true;
    		GetCanvas().display();
    		
        } catch (final FileNotFoundException e) {
            e.printStackTrace();
        } catch (final ClassNotFoundException e) {
            e.printStackTrace();
        } catch (final IOException e) {
            e.printStackTrace();
        }
	}

	/* (non-Javadoc)
	 * @see WildMagic.LibApplications.OpenGLApplication.JavaApplication3D#mouseDragged(java.awt.event.MouseEvent)
	 */
	public void mouseDragged(MouseEvent e)
	{
		if ( m_iCurrent == -1 )
		{
			return; 
		}
		m_akWidgets.get(m_iCurrent).processMouseDrag( m_iMouseX, m_iMouseY, m_iMouseButton, e );
		m_spkScene.UpdateGS();
		m_bDisplay = true;
		GetCanvas().display();

		m_iMouseX = e.getX();
		m_iMouseY = e.getY();
	}

	/* (non-Javadoc)
	 * @see WildMagic.LibApplications.OpenGLApplication.JavaApplication3D#mouseMoved(java.awt.event.MouseEvent)
	 */
	public void mouseMoved(MouseEvent e)
	{
		m_bDisplay = true;
		GetCanvas().display();
	}

	/* (non-Javadoc)
	 * @see WildMagic.LibApplications.OpenGLApplication.JavaApplication3D#mousePressed(java.awt.event.MouseEvent)
	 */
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
	
	/* (non-Javadoc)
	 * @see WildMagic.LibApplications.OpenGLApplication.JavaApplication3D#mouseReleased(java.awt.event.MouseEvent)
	 */
	public void mouseReleased(MouseEvent e)
	{   
		if ( e.getButton() == MouseEvent.BUTTON3 )
		{	
			// Create a new Widget on right-mouse button release:
			if ( m_akWidgets.size() < MAX_WIDGETS )
			{
				m_iCurrent++;
				ClassificationWidget kLev = null;
				if ( m_kWidgetType.equals( "Circle" ) )
				{
					// new Square widget:
					kLev = new CircleClassificationWidget(e.getX(),e.getY(), m_kTMin, m_kTMax, m_kVolumeImage.GetHistoTarget(), m_iWidth, m_iHeight);
				}
				else if ( m_kWidgetType.equals( "Square" ) )
				{
					// new Square widget:
					kLev = new SquareClassificationWidget(e.getX(),e.getY(), m_kTMin, m_kTMax, m_kVolumeImage.GetHistoTarget(), m_iWidth, m_iHeight);
				}
				else
				{
					// new Triangle widget:
					kLev = new TriangleClassificationWidget(e.getX(),e.getY(), m_kTMin, m_kTMax, m_kVolumeImage.GetHistoTarget(), m_iWidth, m_iHeight);
				}
				// Attach the widget scene-graph to the main scene-graph:
				m_spkScene.AttachChild(  kLev.getWidget() );
				m_spkScene.UpdateGS();
				// Save the widget in the list:
				m_akWidgets.add(kLev);
				m_bAdded = true;
			}
		}
		if ( (m_akWidgets.size() > 0) && (m_iCurrent != -1) )
		{
			m_akWidgets.get(m_iCurrent).clearPicked( );
		}
		m_bDisplay = true;
		GetCanvas().display();
	}
	
	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageViewer#reshape(javax.media.opengl.GLAutoDrawable, int, int, int, int)
	 */
	public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight)
	{      
		((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
		// The size of this canvas is constant:
		m_iWidth = 256;
		m_iHeight = 256;
        m_spkCamera.Perspective = false;
        m_spkCamera.SetFrustum(-1,1,-1,1,1f,10.0f);
        m_pkRenderer.OnFrustumChange();
		m_pkRenderer.Resize(m_iWidth,m_iHeight);
		((OpenGLRenderer)m_pkRenderer).GetCanvas().setSize( m_iWidth, m_iHeight );   
		m_bDisplay = true;
	}

	public void save( String fileName )
	{
        try {
            ObjectOutputStream objstream;
            objstream = new ObjectOutputStream(new FileOutputStream(fileName));
			objstream.writeInt(m_akWidgets.size());
    		for ( int i = 0; i < m_akWidgets.size(); i++ )
    		{
            	int type = m_akWidgets.get(i).getType();
    			objstream.writeInt(type);
    			if ( type == ClassificationWidgetState.Circle )
    			{
            		CircleClassificationWidget kWidget = (CircleClassificationWidget)m_akWidgets.get(i);
    				objstream.writeObject(kWidget);
    			}
    			else if ( type == ClassificationWidgetState.Triangle )
    			{
            		TriangleClassificationWidget kWidget = (TriangleClassificationWidget)m_akWidgets.get(i);
    				objstream.writeObject(kWidget);
    			}
    			else if ( type == ClassificationWidgetState.Square )
    			{
            		SquareClassificationWidget kWidget = (SquareClassificationWidget)m_akWidgets.get(i);
    				objstream.writeObject(kWidget);
    			}
    		}
            objstream.close();

        } catch (final FileNotFoundException e) {
            e.printStackTrace();
        } catch (final IOException e) {
            e.printStackTrace();
        }        
	}


	public void setAlpha( float fAlpha )
	{
		m_kCurrentColor.A = fAlpha;
		if (  m_iCurrent != -1 )
		{
			m_akWidgets.get(m_iCurrent).setAlpha( fAlpha );
		}
		m_bDisplay = true;
		GetCanvas().display();
	}
	/**
	 * Sets the contribution of the 2nd derivative on the volume rendering for this widget.
	 * @param fAlpha the contribution of the 2nd derivative on the volume rendering for this widget.
	 */
	public void setBoundary( float fAlpha )
	{
		if (  m_iCurrent != -1 )
		{
			m_akWidgets.get(m_iCurrent).setBoundary( fAlpha );
			m_bDisplay = true;
			GetCanvas().display();
		}
	}

	/**
	 * Sets the color of the color transfer function for this widget.
	 * @param kColor the color of the color transfer function for this widget.
	 */
	public void setColor( ColorRGBA kColor )
	{
		m_kCurrentColor.Copy(kColor);
		if (  m_iCurrent != -1 )
		{
			m_akWidgets.get(m_iCurrent).setColor( kColor );
		}
		m_bDisplay = true;
		GetCanvas().display();
	}

	/**
	 * Sets the parent container panel:
	 * @param kInterface
	 */
	public void SetInterface( JInterfaceBase kInterface )
	{
		m_kInterface = kInterface;
	}

	/**
	 * Sets the current picked ID.
	 * @param value
	 */
	public void setPicked( int value )
	{
		m_iPicked = value;
	}

	/**
	 * Sets the widget type for new widgets.
	 * @param kWidgetType
	 */
	public void setWidget( String kWidgetType )
	{
		m_kWidgetType = new String(kWidgetType);
	}
	
	/**
	 * Sets the list of Widgets.
	 * @param kWidgetList
	 */
	public void setWidgets(Vector<ClassificationWidget> kWidgetList)
	{
		m_akWidgets = kWidgetList;
		m_bUpdateLev = true;        
	}

	public void update( String kCommand )
	{
		boolean bReverseLUT = kCommand.equals( CustomUIBuilder.PARAM_LUT_INVERT.getActionCommand() );
		if ( !bReverseLUT )
		{
			m_iLUTIndex = VolumeTriPlanarRenderBase.getHistogramLUTTextureIndex( kCommand );
		}
		else if (  m_iCurrent != -1 )
		{
			m_iLUTIndex = m_akWidgets.get(m_iCurrent).getLUTIndex();
		}
		Texture kMap = VolumeTriPlanarRenderBase.getHistogramLUTTexture( m_iLUTIndex, bReverseLUT );
		if (  m_iCurrent != -1 )
		{
			m_akWidgets.get(m_iCurrent).setLUT( kMap, m_iLUTIndex, bReverseLUT );
		}
		m_bAdded = true;     
		m_bDisplay = true;
		GetCanvas().display();
	} 
	
	/**
	 * Creates the containing JPanel that will display this object.
	 * The JPanel contains a scroll pane with both horizontal and vertical scroll bars 
	 * and a horizontal slider for changing where the slice plane intersects the volume
	 * in depth.
	 * @param imageComponent
	 */
	private void createContainer( GLCanvas canvas ) 
	{
		container = new JPanel(new BorderLayout()) {

		    public Dimension getPreferredSize() {
		        Dimension dim;

		        try {
		            dim = new Dimension(256, 256);
		        } catch (OutOfMemoryError error) {
		            MipavUtil.displayError("Out of memory: ViewJComponentGraph.getPreferredSize");

		            return null;
		        }

		        return dim;
		    }
		};
		container.add(canvas, BorderLayout.CENTER);
	}

	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageViewer#CreatePlaneNode()
	 */
	protected void CreatePlaneNode()
	{
		// Create the new scene-graph node:
		m_spkScene = new Node();
		m_kCull = new CullState();
		m_kCull.Enabled = false;
        m_spkScene.AttachGlobalState(m_kCull);
        
		// create the screen-space quad:
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

        // Set the positions and texture coordinates.
        // The texture coordinates are clamped to the input values m_kTMin and m_kTMax:
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

	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageViewer#CreateScene()
	 */
	protected void CreateScene ()
	{
		CreatePlaneNode();
		// Attach the 2D Histogram texture as a texture effect to the 
		// screen-space quad:
		m_spkEffect = new TextureEffect( m_kVolumeImage.GetHistoTarget() );
		m_pkPlane.AttachEffect(m_spkEffect);
		m_pkRenderer.LoadResources(m_pkPlane);
	}    

	/**
	 * Determine which widget is selected with the mouse/
	 */
	protected void Pick(Renderer kRenderer)
	{
		if (m_bPickPending)
		{
			m_bPickPending = false;
			if ( m_iCurrent != -1 )
			{
				// Test the currently selected widget first:
				if ( m_akWidgets.get(m_iCurrent).Pick(kRenderer, m_iMouseX, m_iMouseY ) )
				{
					return;
				}
			}
			// If the currently selected widget was not picked, 
			// attempt to find which widget, and which component of the widget is selected:
			for ( int i = 0; i < m_akWidgets.size(); i++ )
			{
				if ( m_akWidgets.get(i).Pick(kRenderer, m_iMouseX, m_iMouseY ) )
				{
					m_iCurrent = i;
					m_kInterface.updateColorButton( m_akWidgets.get(m_iCurrent).getState().Color,
							m_akWidgets.get(m_iCurrent).getState().BoundaryEmphasis[0] );              
					m_spkScene.DetachChild( m_akWidgets.get(m_iCurrent).getWidget() );
					m_spkScene.AttachChild( m_akWidgets.get(m_iCurrent).getWidget() );
					m_spkScene.UpdateGS();
					return;
				}
			}
		}
	}

	/**
	 * Updates the scene graph after a new list of widgets is added.
	 */
	private void updateLev()
	{
		for ( int i = 0; i < m_akWidgets.size(); i++ )
		{
			m_iCurrent = 0;
			ClassificationWidget kLev = m_akWidgets.get(i);
			m_spkScene.AttachChild( kLev.getWidget() );
			if ( m_akWidgets.get(m_iCurrent).getLUTIndex() == -1 )
			{
				setColor( m_akWidgets.get(m_iCurrent).getColor() );
				m_kInterface.updateColorButton( m_akWidgets.get(m_iCurrent).getState().Color,
						m_akWidgets.get(m_iCurrent).getState().BoundaryEmphasis[0] );    
			}
			m_spkScene.DetachChild( m_akWidgets.get(m_iCurrent).getWidget() );
			m_spkScene.AttachChild( m_akWidgets.get(m_iCurrent).getWidget() );
			m_spkScene.UpdateGS();
			m_bAdded = true;
			m_bDisplay = true;
		}
		m_iCurrent = m_iPicked;
		if ( m_bAdded && (m_iCurrent >= 0) && (m_iCurrent < m_akWidgets.size()) )
		{
			if ( m_akWidgets.get(m_iCurrent).getLUTIndex() == -1 )
			{
				setColor( m_akWidgets.get(m_iCurrent).getColor() );
				m_kInterface.updateColorButton( m_akWidgets.get(m_iCurrent).getState().Color,
						m_akWidgets.get(m_iCurrent).getState().BoundaryEmphasis[0] );    
			}
			m_spkScene.DetachChild( m_akWidgets.get(m_iCurrent).getWidget() );
			m_spkScene.AttachChild( m_akWidgets.get(m_iCurrent).getWidget() );
			m_spkScene.UpdateGS();
		}
	}
}
