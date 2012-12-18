
package gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer;

import java.awt.event.MouseEvent;
import java.io.IOException;
import java.io.Serializable;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.Rendering.Light;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.Rendering.Renderer;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.Spatial;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

/** 
 * This is the base class for the Multi-histogram widgets. 
 * 
 * The multi-histogram widgets are objects rendered in 2D in the Multi-histogram panel.
 * The user can manipulate the widgets shape, size, color in the 2D histogram panel.
 * This class encapsulates the rendering and user-interaction with the widgets.
 * 
 * Based on how the widgets are rendered, they determine how the volume is renderer when
 * the multi-histogram rendering is enabled. The parameters that define the widget are passed
 * to the Volume Renderer GLSL program through the ClassificationWidgetState class, that is accessed
 * through this class.
 * */
public abstract class ClassificationWidget implements Serializable
{
	/**  */
	private static final long serialVersionUID = 8150358952544808439L;
	/** Minimum texture coordinates for the 2D histogram. The 2D Histogram is truncated based on a minimum
	 * threshold value, so the entire histogram may not be used depending on the data. */
	protected Vector2f m_kTMin = new Vector2f(0,0);
	/** Maximum texture coordinates for the 2D histogram. The 2D Histogram is truncated based on a minimum
	 * threshold value, so the entire histogram may not be used depending on the data. */
	protected Vector2f m_kTMax = new Vector2f(1,1);
	/** Canvas width and height */
	protected int m_iWidth, m_iHeight;
	/** Currently picked, or active part of this widget, may be the mesh or one of the control-point spheres.
	 *  When null no part of this widget is currently active. */
	protected Spatial m_kPicked = null;
	/** The top-level node containing the widget graphic data structures (triangle meshes and transformations) */
	protected Node m_kWidget = new Node();
	/** Polyline outlines the widget in red when selected, or blue when non selected. */
	protected Polyline m_kOutline = null;
	/** The TriMesh representing the shape of the widget. */
	protected TriMesh m_kWidgetMesh;
	/** The shader effect used to render the inside color transfer function of the widget, 
	 * based on the position of the widget and the
	 * position of the middle (green) sphere control-point. */
	protected ClassificationWidgetEffect m_kWidgetEfect = null; 
	/** The upper-sphere control point of the widget, controls the shape/size of the widget */
	protected TriMesh m_kUpperSphere;
	/** The middle-sphere control point of the widget, controls the color transfer function inside the widget. */
	protected TriMesh m_kMiddleSphere;
	/** The lower-sphere control point of the widget, controls the shape/size of the widget */
	protected TriMesh m_kLowerSphere;
	/** Left bounding edge of the canvas in world coordinates. */
	protected static int LEFT_EDGE = -1;
	/** Right bounding edge of the canvas in world coordinates. */
	protected static int RIGHT_EDGE = 1;
	/** Bottom bounding edge of the canvas in world coordinates. */
	protected static int BOTTOM_EDGE = -1;
	/** Top bounding edge of the canvas in world coordinates. */
	protected static int TOP_EDGE = 1;
	/** Radius of the sphere control points of the widget in world coordinates. */
	protected static float SPHERE_RADIUS = 0.04f;
	/** Mouse offset used for dragging the widget. Stored in screen (MouseEvent) coordinates */
	protected Vector2f m_kMouseOffset;
	/** Shader effect state, for reading from disk: */
	protected ClassificationWidgetState m_kWidgetState;

	/** Default Constructor */
	public ClassificationWidget () {}

	/**
	 * Copy Constructor. Copies the input ClassificationWidget
	 * @param kWidget
	 */
	public ClassificationWidget(ClassificationWidget kWidget)
	{
		m_kTMin = new Vector2f(kWidget.m_kTMin);
		m_kTMax = new Vector2f(kWidget.m_kTMax);
		m_iWidth = kWidget.m_iWidth;
		m_iHeight = kWidget.m_iHeight;
	}

	/**
	 * Create a new ClassificationWidget. 
	 * @param kTMin minimum texture coordinates for the 2D histogram, used for the image background. (Defaults 0-1)
	 * @param kTMax maximum texture coordinates for the 2D histogram, used for the image background. (Defaults 0-1)
	 * @param iWidth canvas width (default 256)
	 * @param iHeight canvas height (default 256)
	 */
	public ClassificationWidget(Vector2f kTMin, Vector2f kTMax, int iWidth, int iHeight)
	{
		m_kTMin = kTMin;
		m_kTMax = kTMax;
		m_iWidth = iWidth;
		m_iHeight = iHeight;
		m_kMouseOffset = new Vector2f();
	}

	/**
	 * Clears the current picked widget.
	 */
	public void clearPicked( )
	{    
		m_kPicked = null;         
	}

	/**
	 * Dispose local memory. 
	 */
	public void dispose()
	{
		m_kTMin = null;
		m_kTMax = null;
		m_kPicked = null;
		m_kWidget = null;
		m_kWidgetMesh = null;
		m_kWidgetEfect = null; 
		m_kUpperSphere = null;
		m_kMiddleSphere = null;
		m_kLowerSphere = null;
		m_kMouseOffset = null;
	}

	/**
	 * Returns the widget current color from the widget shader.
	 * @return widget shader effect, constant color from the shader color transfer function.
	 */
	public ColorRGBA getColor()
	{
		ColorRGBA kColor = null;
		if ( m_kWidgetEfect != null )
		{
			kColor = m_kWidgetEfect.GetColor();
		}
		return kColor;
	}

	/**
	 * Returns the index of the color look-up table color map.
	 * @return the index of the color look-up table color map.
	 */
	public int getLUTIndex( )
	{
		if ( m_kWidgetEfect != null )
		{
			return m_kWidgetEfect.GetLUTIndex( );
		}
		return -1;
	}

	/**
	 * Returns the widget state read from file, so it can initialize a new shader effect.
	 * @return
	 */
	public ClassificationWidgetState getSavedWidgetState()
	{
		return m_kWidgetState;
	}

	/**
	 * Returns the current state of the widget shader effect.
	 * @return the current state of the widget shader effect.
	 */
	public ClassificationWidgetState getState()
	{
		return m_kWidgetEfect.getState();
	}

	/**
	 * Returns the type of widget (Circle, Triangle, Square).
	 * @return the type of widget (Circle, Triangle, Square).
	 */
	public int getType()
	{
		return m_kWidgetEfect.getState().Type;
	}

	/**
	 * Returns the Widget scene graph Node.
	 * @return the Widget scene graph Node.
	 */
	public Node getWidget()
	{
		return m_kWidget;
	}

	/**
	 * Abstract picking. Returns true if this widget, or one of it's control points was picked.
	 * @param iX current mouse x position (MouseEvent coordinates).
	 * @param iY current mouse y position (MouseEvent coordinates).
	 * @return true if this widget or one of it's control points was picked.
	 */
	public abstract boolean Pick( Renderer kRenderer, int iX, int iY );


	/**
	 * Picking. Returns true if this widget, or one of it's control points was picked.
	 * @param iX current mouse x position (MouseEvent coordinates).
	 * @param iY current mouse y position (MouseEvent coordinates).
	 * @param bPicked input parameter from the derived classes, when true the derived class widget shape was picked.
	 * @return true if this widget or one of it's control points was picked.
	 */
	public boolean Pick( Renderer kRenderer, int iX, int iY, boolean bPicked )
	{
		// convert the input MouseEvent coordinates into object- or world-coordinates:
		float fX = calcObjX(iX);
		float fY = calcObjY(iY);
		Vector3f kCenter;
		// compare the mouse position in world coordinates to the lower sphere position:
		if ( m_kLowerSphere != null )
		{
			kCenter = m_kLowerSphere.Local.GetTranslate();
			if ( (fX > (kCenter.X - SPHERE_RADIUS)) && (fX < (kCenter.X + SPHERE_RADIUS)) &&
					(fY > (kCenter.Y - SPHERE_RADIUS)) && (fY < (kCenter.Y + SPHERE_RADIUS)) )
			{
				// set the current picked widget:
				m_kPicked = m_kLowerSphere;
				//System.err.println( "Picked Lower" );
				bPicked = true;
			}
		}
		// compare the mouse position in world coordinates to the upper sphere position:
		if ( m_kUpperSphere != null )
		{
			kCenter = m_kUpperSphere.Local.GetTranslate();
			if ( (fX > (kCenter.X - SPHERE_RADIUS)) && (fX < (kCenter.X + SPHERE_RADIUS)) &&
					(fY > (kCenter.Y - SPHERE_RADIUS)) && (fY < (kCenter.Y + SPHERE_RADIUS)) )
			{
				// set the current picked widget:
				m_kPicked = m_kUpperSphere;
				//System.err.println( "Picked Upper" );
				bPicked = true;
			}
		}
		// this comparison is last, for the case when the middle sphere overlaps another control-point
		// making the middle sphere the most-likely to be picked.
		// compare the mouse position in world coordinates to the middle sphere position:
		if ( m_kMiddleSphere != null )
		{
			kCenter = m_kMiddleSphere.Local.GetTranslate();
			if ( (fX > (kCenter.X - SPHERE_RADIUS)) && (fX < (kCenter.X + SPHERE_RADIUS)) &&
					(fY > (kCenter.Y - SPHERE_RADIUS)) && (fY < (kCenter.Y + SPHERE_RADIUS)) )
			{
				// set the current picked widget:
				m_kPicked = m_kMiddleSphere;
				//System.err.println( "Picked Middle" );
				bPicked = true;
			}
		}

		// If one of the control-point spheres was picked, or if the main widget mesh
		// was picked, set the outline color of the widget to be red:
		if ( bPicked )
		{
			// When the color is being set, also calculate the current center of the widget:
			fX = 0;
			fY = 0;
			for ( int i = 0; i < m_kWidgetMesh.VBuffer.GetVertexQuantity(); i++ )
			{
				m_kWidgetMesh.VBuffer.SetColor3(0, i, 1f,0f,0f);
				fX += m_kWidgetMesh.VBuffer.GetPosition3fX(i);
				fY += m_kWidgetMesh.VBuffer.GetPosition3fY(i);
			}
			m_kWidgetMesh.Release(kRenderer);
			fX /= m_kWidgetMesh.VBuffer.GetVertexQuantity();
			fY /= m_kWidgetMesh.VBuffer.GetVertexQuantity();

			fX = calcScreenX(fX);
			fY = calcScreenY(fY);
			// Set the mouse offset as the current position of the mouse - the current center of the widget.
			// used to translate the widget with the mouse during mouse drag events.
			m_kMouseOffset.set ( fX - iX, fY - iY );
			setPicked(true);
		}
		return bPicked;
	}

	/**
	 * @param iX0ld old mouse x-position in MouseEvent coordinates.
	 * @param iYOld old mouse y-position in MouseEvent coordinates.
	 * @param iButton old mouse button.
	 * @param e current MouseEvent
	 */
	public abstract void processMouseDrag(int iX0ld, int iYOld, int iButton, MouseEvent e );

	/**
	 * Sets the alpha value of the widget.
	 * @param fAlpha
	 */
	public void setAlpha( float fAlpha )
	{
		if ( m_kWidgetEfect != null )
		{
			m_kWidgetEfect.SetAlpha( fAlpha );
		}		
	}

	/**
	 * Sets the contribution of the 2nd derivative on the volume rendering for this widget.
	 * @param fAlpha the contribution of the 2nd derivative on the volume rendering for this widget.
	 */
	public void setBoundary( float fAlpha )
	{
		if ( m_kWidgetEfect != null )
		{
			m_kWidgetEfect.setBoundary( fAlpha );
		}
	}

	/**
	 * Sets the color of the color transfer function for this widget.
	 * @param kColor the color of the color transfer function for this widget.
	 */
	public void setColor( ColorRGBA kColor )
	{
		if ( m_kWidgetEfect != null )
		{
			m_kWidgetEfect.SetColor( kColor.R, kColor.G, kColor.B, kColor.A );
		}
	}

	/**
	 * Set the color look-up table for the widget.
	 * @param kMap Texture map look-up table.
	 * @param index index of the look-up table.
	 * @param bReverse inverts the table when true.
	 */
	public void setLUT( Texture kMap, int index, boolean bReverse )
	{
		if ( m_kWidgetEfect != null )
		{
			m_kWidgetEfect.SetLUT( kMap, index, bReverse );
		}
	}

	/**
	 * Clears or sets the current picked object, sets the outline color to red when picked, blue when not selected.
	 * @param bPicked when true the widget is selected.
	 */
	public void setPicked( boolean bPicked )
	{
		for ( int i = 0; i < m_kWidgetMesh.VBuffer.GetVertexQuantity(); i++ )
		{
			if ( bPicked )
			{
				m_kWidgetMesh.VBuffer.SetColor3(0, i, 1f,0f,0f);
			}
			else 
			{
				m_kWidgetMesh.VBuffer.SetColor3(0, i, 0f,0f,.5f);       
				m_kPicked = null;         
			}
		}
		m_kWidgetMesh.Reload(true);
	}

	/**
	 * Sets the ClassificationWidgetState of the widget.
	 * @param state
	 */
	public void setState(ClassificationWidgetState state)
	{
		m_kWidgetEfect.setState(state);
	}
	
	
	/**
	 * Sets the histogram Texture map for the widget shader effect.
	 * @param kTexture
	 */
	public abstract void setTexture( Texture kTexture );


    /**
	 * Updates the ShaderEffect parameters for this widget.
	 */
	public abstract void updateDisplay();
	
	/**
	 * Calculate the world X coordinates from input MouseEvent coordinates.
	 * @param val input MouseEvent Coordinates.
	 * @return corresponding x-position in world coordinates.
	 */
	protected float calcObjX( float val )
	{
		float fX = val / m_iWidth;
		fX *= 2.0f;
		fX -= 1;
		return fX;
	}

	/**
	 * Calculate the world Y coordinates from input MouseEvent coordinates.
	 * @param val input MouseEvent Coordinates.
	 * @return corresponding y-position in world coordinates.
	 */
	protected float calcObjY( float val )
	{
		float fY = m_iHeight - val;
		fY /= m_iHeight;
		fY *= 2.0f;
		fY -= 1;
		return fY;
	}

	/**
	 * Calculate the screen X coordinates from input world coordinates.
	 * @param val input world Coordinates.
	 * @return corresponding x-position in MouseEvent coordinates.
	 */
	protected float calcScreenX( float val )
	{
		float fX = val;
		fX += 1.0; fX /= 2.0f; fX *= m_iWidth;
		return fX;
	}

	/**
	 * Calculate the screen Y coordinates from input world coordinates.
	 * @param val input world Coordinates.
	 * @return corresponding y-position in MouseEvent coordinates.
	 */
	protected float calcScreenY( float val )
	{
		float fY = val;
		fY += 1.0; fY /= 2.0f; fY *= m_iHeight; fY = m_iHeight - fY;
		return fY;
	}

	/**
	 * Calculate the X Texture Coordinates from world coordinates.
	 * The texture coordinates are scaled, based on the minimum and maximum texture coordinates of the 2D histogram texture.
	 * @param val input world Coordinates.
	 * @return corresponding x Texture coordinates.
	 */
	protected float calcTCoordX( float val )
	{
		float scaledVal = (val + 1)/2f;
		return (m_kTMin.X + (scaledVal) * (m_kTMax.X - m_kTMin.X));
	}

	/**
	 * Calculate the Y Texture Coordinates from world coordinates.
	 * The texture coordinates are scaled, based on the minimum and maximum texture coordinates of the 2D histogram texture.
	 * @param val input world Coordinates.
	 * @return corresponding y Texture coordinates.
	 */
	protected float calcTCoordY( float val )
	{
		float scaledVal = (val + 1)/2f;
		return (m_kTMin.Y + (scaledVal) * (m_kTMax.Y - m_kTMin.Y));
	}

	/**
	 * Calculate and return the current center of the widget in world coordinates.
	 * @return the current center of the widget in world coordinates.
	 */
	protected Vector2f getCenter()
	{
		float fX = 0;
		float fY = 0;
		for ( int i = 0; i < m_kWidgetMesh.VBuffer.GetVertexQuantity(); i++ )
		{
			fX += m_kWidgetMesh.VBuffer.GetPosition3fX(i);
			fY += m_kWidgetMesh.VBuffer.GetPosition3fY(i);
		}
		fX /= m_kWidgetMesh.VBuffer.GetVertexQuantity();
		fY /= m_kWidgetMesh.VBuffer.GetVertexQuantity();
		return new Vector2f( fX, fY );
	}

	/**
     * Read this object from disk:
     * @param in
     * @throws IOException
     * @throws ClassNotFoundException
	 */
	private void readObject(java.io.ObjectInputStream in)
    throws IOException, ClassNotFoundException
    {
        
		m_kWidgetState = (ClassificationWidgetState)in.readObject();
		
		m_kTMin = (Vector2f)in.readObject();
		m_kTMax = (Vector2f)in.readObject();
		m_iWidth = in.readInt();
		m_iHeight = in.readInt();
				
		m_kWidget = new Node();
		IndexBuffer kIBuffer = (IndexBuffer)in.readObject();
		VertexBuffer kVBuffer = (VertexBuffer)in.readObject();
		m_kWidgetMesh = new TriMesh( kVBuffer, kIBuffer );     
    		
        m_kWidgetMesh.SetName("BottomTri");
        m_kWidget.AttachChild(m_kWidgetMesh);

        m_kOutline = new Polyline( m_kWidgetMesh.VBuffer, true, true );
        m_kOutline.AttachEffect( new VertexColor3Effect() );
        m_kWidget.AttachChild(m_kOutline);
        

        // Create light and attach it to the Widget, it will
        // be applied to all the spheres attached to the widget:
        Light pointLight = new Light(Light.LightType.LT_POINT);
        float fValue = .90f;
        pointLight.Ambient = new ColorRGB(fValue,fValue,fValue);
        fValue = 40f;
        pointLight.Position = new Vector3f(+fValue,+fValue,+fValue);
        pointLight.Diffuse = new ColorRGB(ColorRGB.WHITE);
        pointLight.Specular = new ColorRGB(ColorRGB.WHITE);
        m_kWidget.AttachLight(pointLight);
        // Create the material to describe the shading for the sphere:
        MaterialState kMaterial = new MaterialState();
        kMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
        kMaterial.Ambient = new ColorRGB(0.2f,0.2f,0.2f);
        kMaterial.Diffuse = new ColorRGB(0f,0f,1f);
        kMaterial.Specular = new ColorRGB(0.9f,0.9f,0.9f);
        kMaterial.Shininess = 83.2f;
        
        kIBuffer = (IndexBuffer)in.readObject();
        kVBuffer = (VertexBuffer)in.readObject();
        m_kUpperSphere = new TriMesh( kVBuffer, kIBuffer );
        m_kUpperSphere.AttachGlobalState( kMaterial );
        m_kUpperSphere.SetName("UpperSphere");
        m_kWidget.AttachChild( m_kUpperSphere );
        m_kUpperSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(2));
        
        
        kMaterial = new MaterialState();
        kMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
        kMaterial.Ambient = new ColorRGB(0.2f,0.2f,0.2f);
        kMaterial.Diffuse = new ColorRGB(0f,1f,0f);
        kMaterial.Specular = new ColorRGB(0.9f,0.9f,0.9f);
        kMaterial.Shininess = 83.2f;
        kIBuffer = (IndexBuffer)in.readObject();
        kVBuffer = (VertexBuffer)in.readObject();
        m_kMiddleSphere = new TriMesh( kVBuffer, kIBuffer );
        m_kMiddleSphere.AttachGlobalState( kMaterial );
        m_kMiddleSphere.SetName("MiddleSphere");
        m_kWidget.AttachChild( m_kMiddleSphere );     

		m_kMouseOffset = new Vector2f();
    }
	
	/**
	 * Stream this object to disk.
	 * @param out
	 * @throws IOException
	 */
	private void writeObject(java.io.ObjectOutputStream out)
	throws IOException 
	{
		out.writeObject(m_kWidgetEfect.getState());
		
		out.writeObject(m_kTMin);
		out.writeObject(m_kTMax);
		out.writeInt(m_iWidth);
		out.writeInt(m_iHeight);

		out.writeObject( m_kWidgetMesh.IBuffer );
		out.writeObject( m_kWidgetMesh.VBuffer );

		out.writeObject( m_kUpperSphere.IBuffer );
		out.writeObject( m_kUpperSphere.VBuffer );

		out.writeObject( m_kMiddleSphere.IBuffer );
		out.writeObject( m_kMiddleSphere.VBuffer );
	}
}
