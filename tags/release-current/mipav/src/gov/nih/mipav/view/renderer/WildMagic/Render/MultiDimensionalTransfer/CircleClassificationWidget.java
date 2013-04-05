
package gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer;

import java.awt.event.MouseEvent;
import java.io.IOException;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.Mathf;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.Rendering.Light;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.Rendering.Renderer;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

/**
 * Creates the circle widget for the multi-histogram interface.
 * The circle consists of two control points. One controls the radius and eccentricity of the circle/ellipse.
 * The second control point centers the maximum color value within the circle/ellipse.
 */
public class CircleClassificationWidget extends ClassificationWidget
{

	private static final long serialVersionUID = -4180814068815939724L;

	/** Direction used to position the center sphere control-point out from the
	 * center of the circle/ellipse. */
	private Vector2f m_kCenterDir = new Vector2f(0,0);
	/** Distance scale used to position the center sphere control-point out from the
	 * center of the circle/ellipse. */
	private float m_fCenterScale = 0f;

	/** The radius of the circle in the x-direction. */
    private float m_fRadiusX = 0.5f;
	/** The radius of the circle in the y-direction. */
    private float m_fRadiusY = 0.5f;


    /**
	 * @param iX location in MouseEvent Coordinates.
	 * @param iY location in MouseEvent Coordinates.
	 * @param kTMin minimum texture coordinates for the 2D histogram, used for the image background. (Defaults 0-1)
	 * @param kTMax maximum texture coordinates for the 2D histogram, used for the image background. (Defaults 0-1)
	 * @param kTexture 2D Histogram Texture.
	 * @param iWidth canvas width (default 256)
	 * @param iHeight canvas height (default 256)
	 */
	public CircleClassificationWidget(int iX, int iY, Vector2f kTMin, Vector2f kTMax, Texture kTexture, int iWidth, int iHeight)
    {
        super( kTMin, kTMax, iWidth, iHeight );
        CreateCircle( iX, iY, kTexture );
    }

	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidget#Pick(int, int)
	 */
	public boolean Pick( Renderer kRenderer, int iX, int iY )
	{
		boolean bPicked = false;
		m_kPicked = null;
		// translate the MouseEvent coordinates into world coordinates.
		float fX = calcObjX(iX);
		float fY = calcObjY(iY);
		
		// get the center of the circle:
		float fCenterX = m_kWidgetMesh.VBuffer.GetPosition3fX(0);
		float fCenterY = m_kWidgetMesh.VBuffer.GetPosition3fY(0);
		
		// test if the pick point is inside the circle:
    	float diffX = fX - fCenterX;
    	float diffY = fY - fCenterY;
    	float fLength = (float)Math.sqrt( diffX*diffX + diffY*diffY );
    	float scale = (float)((m_fRadiusX * m_fRadiusY) / Math.sqrt( m_fRadiusY*m_fRadiusY * diffX*diffX + m_fRadiusX*m_fRadiusX * diffY*diffY ));
    	float newX = diffX * scale;
    	float newY = diffY * scale;
    	float fLengthEdge = (float)Math.sqrt( newX*newX + newY*newY );
		if ( fLength < fLengthEdge )
		{
			m_kPicked = m_kWidgetMesh;
			bPicked = true;
		}
		return super.Pick(kRenderer, iX,iY,bPicked);
	}
    
	/* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidget#processMouseDrag(int, int, int, java.awt.event.MouseEvent)
     */
	public void processMouseDrag(int iX0ld, int iYOld, int iButton, MouseEvent e ) 
    {        
        if ( iButton == MouseEvent.BUTTON1 )
        {
            if ( m_kPicked == m_kWidgetMesh )
            {
            	// If the circle is picked, translate it:
                ShiftCircle( e );
            }
            else if ( (m_kPicked == m_kUpperSphere) )
            {
            	// if one of the control-point is picked, scale the circle:
            	ScaleCircle( e );
            }
            else if ( m_kPicked == m_kMiddleSphere )
            {
            	// the middle control-point was picked, so shift the center control-point:
                ShiftMid(e);
            }
        }
    }
    
    /**
	 * Clears or sets the current picked object, sets the outline color to red when picked, blue when not selected.
	 * @param bPicked when true the widget is selected.
	 */
	public void setPicked( Renderer kRenderer, boolean bPicked )
	{
		for ( int i = 0; i < m_kWidgetMesh.VBuffer.GetVertexQuantity(); i++ )
		{
			if ( bPicked )
			{
				m_kWidgetMesh.VBuffer.SetColor3(0, i, 1f,0f,0f);
				if ( i < m_kOutline.VBuffer.GetVertexQuantity() )
				{
					m_kOutline.VBuffer.SetColor3(0, i, 1f,0f,0f);
				}
			}
			else 
			{
				m_kWidgetMesh.VBuffer.SetColor3(0, i, 0f,0f,.5f);       
				if ( i < m_kOutline.VBuffer.GetVertexQuantity() )
				{
					m_kOutline.VBuffer.SetColor3(0, i, 0f,0f,.5f);       
				}
				m_kPicked = null;         
			}
		}
		m_kWidgetMesh.Release(kRenderer);
		m_kOutline.Release(kRenderer);
		
        m_kWidget.DetachChild( m_kUpperSphere );
        m_kWidget.DetachChild( m_kMiddleSphere );
		if ( bPicked )
		{
	        m_kWidget.AttachChild( m_kUpperSphere );
	        m_kWidget.AttachChild( m_kMiddleSphere );
		}
	}
	

	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidget#setTexture(WildMagic.LibGraphics.Rendering.Texture)
	 */
	public void setTexture( Texture kTexture )
	{
		if ( m_kWidgetMesh != null )
		{
			// creates and attaches a new shader-effect for the widget.
			m_kWidgetEfect = new ClassificationWidgetEffect( kTexture, ClassificationWidgetState.Circle );
			m_kWidgetMesh.AttachEffect( m_kWidgetEfect );
		}
	}
	
	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidget#updateDisplay()
	 */
	public void updateDisplay()
    {
		// Update the ClassificationWidgetEffect based on the position of the circle and the mid-line control-point.
        if ( m_kWidgetEfect != null )
        {
            Vector3f kMidLine = m_kMiddleSphere.Local.GetTranslate();
            float fX1 = kMidLine.X;
            float fY1 = kMidLine.Y;
            // set mid-line in texture coordinates:
            m_kWidgetEfect.SetMidLine( calcTCoordX(fX1), calcTCoordY(fY1), calcTCoordX(fX1), calcTCoordY(fY1) );
            
            Vector2f center = getCenter();
            m_kWidgetEfect.SetCenter( calcTCoordX(center.X), calcTCoordY(center.Y) );
            
            m_kWidgetEfect.SetRadius( m_fRadiusX * m_kTMax.X / 2f, m_fRadiusY * m_kTMax.Y / 2f );

            m_kWidgetEfect.UpdateColor();
        }
    }

    /**
     * Creates the circle widget and control point:
     * @param iX mouse x-position in MouseEvent coordinates.
     * @param iY mouse y-position in MouseEvent coordinates.
     * @param kTexName 2D Histogram texture name.
     */
    protected void CreateCircle(int iX, int iY, Texture kTexture)
    {    	
        // create the TriMesh Attributes: position, color, texture:
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);
        kAttributes.SetTChannels(0,2);
        
        StandardMesh stdMesh = new StandardMesh( kAttributes );
        m_kWidgetMesh = stdMesh.Disk( 2, 64, m_fRadiusX );
        
        float x, y, z;
        // Set up the VertexBuffer positions, colors, texture coordinates:
        VertexBuffer kVBuffer = new VertexBuffer(kAttributes, m_kWidgetMesh.VBuffer.GetVertexQuantity() -1 ); 
        int index = 0;
		for ( int i = 0; i < m_kWidgetMesh.VBuffer.GetVertexQuantity(); i++ )
		{
			x = m_kWidgetMesh.VBuffer.GetPosition3fX(i);
			y = m_kWidgetMesh.VBuffer.GetPosition3fY(i);
			z = 0.1f;
			m_kWidgetMesh.VBuffer.SetPosition3(i, x, y, z );
			m_kWidgetMesh.VBuffer.SetTCoord2(0, i, calcTCoordX(x), calcTCoordY(y));
			if ( i >= 1 )
			{
				kVBuffer.SetPosition3( index, x, y, z );
				kVBuffer.SetColor3( 0, index, 1.0f, 0.01f, 0.01f );
				kVBuffer.SetTCoord2(0, index, calcTCoordX(x), calcTCoordY(y));
				index++;
			}
		}      
                
        // Create the ClassificationWidgetEffect, pass in the texture name:
        m_kWidgetEfect = new ClassificationWidgetEffect( kTexture, ClassificationWidgetState.Circle );
        m_kWidgetMesh.AttachEffect( m_kWidgetEfect );
        m_kWidgetMesh.SetName("BottomTri");
        m_kWidget.AttachChild(m_kWidgetMesh);
        
        // Outline for the circle, is different from the circle VertexBuffer (doesn't contain the center point)
        m_kOutline = new Polyline( kVBuffer, true, true );
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
        
        // Attributes for the sphere control points:
        kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetNChannels(3);
        kAttributes.SetCChannels(0,3);
        StandardMesh kSM = new StandardMesh(kAttributes);

        m_kUpperSphere = kSM.Sphere(10,10,SPHERE_RADIUS);
        m_kUpperSphere.AttachGlobalState(kMaterial);
        m_kUpperSphere.SetName("UpperSphere");
        m_kWidget.AttachChild( m_kUpperSphere );
        m_kUpperSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(1));

        // Create the material to describe the shading for the green sphere:
        kMaterial = new MaterialState();
        kMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
        kMaterial.Ambient = new ColorRGB(0.2f,0.2f,0.2f);
        kMaterial.Diffuse = new ColorRGB(0f,1f,0f);
        kMaterial.Specular = new ColorRGB(0.9f,0.9f,0.9f);
        kMaterial.Shininess = 83.2f;
        m_kMiddleSphere = kSM.Sphere(10,10,SPHERE_RADIUS);
        m_kMiddleSphere.AttachGlobalState(kMaterial);
        m_kMiddleSphere.SetName("MiddleSphere");
        m_kWidget.AttachChild( m_kMiddleSphere );

        // position the middle control-point in the center:
        m_kMiddleSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(0) );

        // Update RS updates the lighting effects:
        m_kWidget.UpdateRS();
        m_kWidget.UpdateGS();
    }
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidget#getCenter()
     */
    protected Vector2f getCenter()
	{
		float fX = m_kWidgetMesh.VBuffer.GetPosition3fX(0);
		float fY = m_kWidgetMesh.VBuffer.GetPosition3fY(0);
		return new Vector2f( fX, fY );
	}
    
    /**
     * scale the circle based on the mouse position.
     * @param e
     */
    protected void ScaleCircle(MouseEvent e)
    {
    	// Calculate the position in world-coordinates:
    	float fX = calcObjX(e.getX()); 
    	float fY = calcObjY(e.getY());
    	// clamp to the boundary of the canvas:
    	fX = Math.max( fX, LEFT_EDGE );
    	fX = Math.min( fX, RIGHT_EDGE );
    	fY = Math.min( fY, TOP_EDGE );
    	fY = Math.max( fY, BOTTOM_EDGE );
    	// get the current center of the circle:
    	Vector2f kCurrentCenter = getCenter();
        float diffX = fX - kCurrentCenter.X;
        float diffY = fY - kCurrentCenter.Y;
        
        // set the new radius of the circle:
		m_fRadiusX = Math.abs(diffX);
		m_fRadiusY = Math.abs(diffY);
		// scale the circle object:
		scaleCircle( kCurrentCenter, diffX, diffY );		
    }
    
    /**
     * Translate the circle in the 2D Histogram panel.
     * @param e MouseEvent
     */
    protected void ShiftCircle( MouseEvent e )
    {
    	// get the current center of the circle:
    	Vector2f kCurrentCenter = getCenter();

    	// get the mouse relative position:
        float fNewGCX = e.getX() + m_kMouseOffset.X;
        fNewGCX = calcObjX( fNewGCX );
        float fNewGCY = e.getY() + m_kMouseOffset.Y;
        fNewGCY = calcObjY( fNewGCY );

        // calculate the difference in the current center and the mouse position:
        float fDiffX = fNewGCX - kCurrentCenter.X;
        float fDiffY = fNewGCY - kCurrentCenter.Y;
        // translate the circle:
        shiftCircle( kCurrentCenter, fDiffX, fDiffY );
    }
    

	/**
	 * Read a new CircleClassificationWidget from the input stream.
	 * @param in input stream
	 * @throws IOException
	 * @throws ClassNotFoundException
	 */
	private void readObject(java.io.ObjectInputStream in)
    throws IOException, ClassNotFoundException
    {
		// create the TriMesh Attributes: position, color, texture:
		Attributes kAttributes = new Attributes();
		kAttributes.SetPChannels(3);
		kAttributes.SetCChannels(0,3);
		kAttributes.SetTChannels(0,2);

		float x, y, z;
		// Set up the VertexBuffer positions, colors, texture coordinates:
		// This is the VertexBuffer for the outline, not the mesh:
		VertexBuffer kVBuffer = new VertexBuffer(kAttributes, m_kWidgetMesh.VBuffer.GetVertexQuantity() -1 ); 
		int index = 0;
		for ( int i = 0; i < m_kWidgetMesh.VBuffer.GetVertexQuantity(); i++ )
		{
			x = m_kWidgetMesh.VBuffer.GetPosition3fX(i);
			y = m_kWidgetMesh.VBuffer.GetPosition3fY(i);
			z = 0.1f;
			if ( i >= 1 )
			{
				kVBuffer.SetPosition3( index, x, y, z );
				kVBuffer.SetColor3( 0, index, 1.0f, 0.01f, 0.01f );
				kVBuffer.SetTCoord2(0, index, calcTCoordX(x), calcTCoordY(y));
				index++;
			}
		}      
		// Re-create the outline of the circle:
		m_kWidget.DetachChild(m_kOutline);
		// Outline for the circle:
		m_kOutline = new Polyline( kVBuffer, true, true );
		m_kOutline.AttachEffect( new VertexColor3Effect() );
		m_kWidget.AttachChild(m_kOutline);

		// initialize the upper-sphere position:
		m_kUpperSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(1));
		// initialize the middle-sphere position:
        m_kMiddleSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(0) );
		        
		Vector2f kNewCenter = (Vector2f)in.readObject();
		m_kCenterDir = (Vector2f)in.readObject();
		m_fCenterScale = in.readFloat();
		
		m_fRadiusX = in.readFloat();
		m_fRadiusY = in.readFloat();		
		
		Vector2f kCurrentCenter = getCenter();
		// shift the circle the the last position:
		shiftCircle( kCurrentCenter, kNewCenter.X - kCurrentCenter.X, kNewCenter.Y - kCurrentCenter.Y);
		// scale the circle:
		scaleCircle( kCurrentCenter, 0, 0 );

		// reposition the upper sphere based on last position in the file:
		Vector3f upperTranslate = (Vector3f)in.readObject( );
		m_kUpperSphere.Local.SetTranslate(upperTranslate);
		// reposition the middle sphere based on last position in the file:
		Vector3f middleTranslate = (Vector3f)in.readObject( );
		m_kMiddleSphere.Local.SetTranslate(middleTranslate);
		
		// Update RS updates the lighting effects:
        m_kWidget.UpdateRS();
		// update the widget graphics state:
		m_kWidget.UpdateGS();
    }

    /**
     * Scale the circle based on the current center and the difference between
     * the current center and the mouse position.
     * @param kCurrentCenter circle current center.
     * @param diffX difference between center and mouse position.
     * @param diffY difference between center and mouse position.
     */
    private void scaleCircle(Vector2f kCurrentCenter, float diffX, float diffY )
    {
        float fInvRS = 1.0f/64f;
        float x,y,z;
        // Test that the circle will be contained within the area of the histogram.
        // If not adjust the radius values.
		for ( int i = 1; i < m_kWidgetMesh.VBuffer.GetVertexQuantity(); i++ )
		{
			float fAngle = Mathf.TWO_PI*fInvRS*i;
            float fCos = (float)Math.cos(fAngle);
            float fSin = (float)Math.sin(fAngle);
            
			x = kCurrentCenter.X + fCos * m_fRadiusX;
			y = kCurrentCenter.Y + fSin * m_fRadiusY;
			if ( x < LEFT_EDGE )
			{
				m_fRadiusX = (LEFT_EDGE - kCurrentCenter.X)/fCos;
			}
			else if ( x > RIGHT_EDGE )
			{
				m_fRadiusX = (RIGHT_EDGE - kCurrentCenter.X)/fCos;
			}
			if ( y < BOTTOM_EDGE )
			{
				m_fRadiusY = (BOTTOM_EDGE - kCurrentCenter.Y)/fSin;
			}
			else if ( y > TOP_EDGE )
			{
				m_fRadiusY = (TOP_EDGE - kCurrentCenter.Y)/fSin;
			}
		}		   
            	
    	float scale = (float)((m_fRadiusX * m_fRadiusY) / Math.sqrt( m_fRadiusY*m_fRadiusY * diffX*diffX + m_fRadiusX*m_fRadiusX * diffY*diffY ));
    	float newX = diffX * scale;
    	float newY = diffY * scale;

		z = 0.1f;
        int index = 0;
        // scale the positions of the circle, based on the x, y radius:
		for ( int i = 1; i < m_kWidgetMesh.VBuffer.GetVertexQuantity(); i++ )
		{
            float fAngle = Mathf.TWO_PI*fInvRS*i;
            float fCos = (float)Math.cos(fAngle);
            float fSin = (float)Math.sin(fAngle);
            
			x = kCurrentCenter.X + fCos * m_fRadiusX;
			y = kCurrentCenter.Y + fSin * m_fRadiusY;
			m_kWidgetMesh.VBuffer.SetPosition3(i, x, y, z );
			m_kWidgetMesh.VBuffer.SetTCoord2(0, i, calcTCoordX(x), calcTCoordY(y));
			if ( i >= 1 )
			{
				m_kOutline.VBuffer.SetPosition3( index, x, y, z );
				m_kOutline.VBuffer.SetTCoord2(0, index, calcTCoordX(x), calcTCoordY(y));
				index++;
			}
		}      
		m_kWidgetMesh.Reload(true);
		m_kOutline.Reload(true);

		// Translate the upper sphere:
        m_kUpperSphere.Local.SetTranslate( kCurrentCenter.X + newX, kCurrentCenter.Y + newY, z );
		
        // Translate the middle sphere:
        diffX = m_kCenterDir.X;
        diffY = m_kCenterDir.Y;    	
        if ( diffX != 0 && diffY != 0 )
        {
        	scale = (float)((m_fRadiusX * m_fRadiusY) / Math.sqrt( m_fRadiusY*m_fRadiusY * diffX*diffX + m_fRadiusX*m_fRadiusX * diffY*diffY ));
        	newX = diffX * scale;
        	newY = diffY * scale;

        	float edgeLength = (float)Math.sqrt( newX*newX + newY*newY );
        	m_kMiddleSphere.Local.SetTranslate( kCurrentCenter.X + m_kCenterDir.X * m_fCenterScale * edgeLength, 
        			kCurrentCenter.Y + m_kCenterDir.Y * m_fCenterScale * edgeLength, z );

        }           
        
        // update the scene graph:
        m_kWidget.UpdateGS();
    }
        
    /**
     * Translate the circle based on the current center and the difference between
     * the current center and the mouse position.
     * @param kCurrentCenter
     * @param fDiffX
     * @param fDiffY
     */
    private void shiftCircle( Vector2f kCurrentCenter, float fDiffX, float fDiffY )
    {
        if ( kCurrentCenter.X + m_fRadiusX + fDiffX > RIGHT_EDGE )
        {
        	fDiffX = RIGHT_EDGE - (kCurrentCenter.X + m_fRadiusX);
        }
        if ( kCurrentCenter.X - m_fRadiusX + fDiffX < LEFT_EDGE )
        {
        	fDiffX = LEFT_EDGE - (kCurrentCenter.X - m_fRadiusX);
        }

        // Y check that moving the circle doesn't move it out of bounds,
        // clamp if necessary:
        if ( kCurrentCenter.Y - m_fRadiusY + fDiffY  < BOTTOM_EDGE )
        {
        	fDiffY = BOTTOM_EDGE - (kCurrentCenter.Y - m_fRadiusY);
        }
        if ( kCurrentCenter.Y + m_fRadiusY + fDiffY > TOP_EDGE )
        {
        	fDiffY = TOP_EDGE - (kCurrentCenter.Y + m_fRadiusY);
        }
        
        // Move the circle: position and texture coordinates:
        float fNewX, fNewY;
        int index = 0;
        for ( int i = 0; i < m_kWidgetMesh.VBuffer.GetVertexQuantity(); i++ )
        {
            fNewX = m_kWidgetMesh.VBuffer.GetPosition3fX(i) + fDiffX;
            fNewY = m_kWidgetMesh.VBuffer.GetPosition3fY(i) + fDiffY;
            m_kWidgetMesh.VBuffer.SetPosition3( i, fNewX, fNewY,
            		m_kWidgetMesh.VBuffer.GetPosition3fZ(i) );
            m_kWidgetMesh.VBuffer.SetTCoord2(0, i, calcTCoordX(fNewX), calcTCoordY(fNewY));
            
            if ( i >= 1 )
            {
                m_kOutline.VBuffer.SetPosition3( index, fNewX, fNewY,
                		m_kWidgetMesh.VBuffer.GetPosition3fZ(i) );
                m_kOutline.VBuffer.SetTCoord2(0, index, calcTCoordX(fNewX), calcTCoordY(fNewY));
                index++;
            }
        }
        m_kWidgetMesh.Reload(true);
        m_kOutline.Reload(true);

        // Move the center sphere by translating it the same amount as the circle:
        Vector3f kTranslate = m_kMiddleSphere.Local.GetTranslate(); 
        m_kMiddleSphere.Local.SetTranslate( kTranslate.X + fDiffX, kTranslate.Y + fDiffY, kTranslate.Z );

        // Move the upper sphere by translating it the same amount as the circle:
        kTranslate = m_kUpperSphere.Local.GetTranslate(); 
        m_kUpperSphere.Local.SetTranslate( kTranslate.X + fDiffX, kTranslate.Y + fDiffY, kTranslate.Z );
        
        // Update the scene-graph
        m_kWidget.UpdateGS();
    }   

	/**
     * Moves the mid-line control-point. Repositions the mid-line of the transfer function
     * inside this widget.
     * @param e MouseEvent
     */
    private void ShiftMid( MouseEvent e )
    {      
    	// calculate the coordinates in world space:
    	float fX = calcObjX(e.getX()); 
    	float fY = calcObjY(e.getY()); 
    	Vector3f kCenter = m_kMiddleSphere.Local.GetTranslate();
    	// get the current center of the circle:
    	Vector2f kCurrentCenter = getCenter();

    	float diffX = fX - kCurrentCenter.X;
    	float diffY = fY - kCurrentCenter.Y;
    	float length = (float)Math.sqrt(diffX * diffX + diffY * diffY );
    	
    	float scale = (float)((m_fRadiusX * m_fRadiusY) / Math.sqrt( m_fRadiusY*m_fRadiusY * diffX*diffX + m_fRadiusX*m_fRadiusX * diffY*diffY ));
    	float newX = diffX * scale;
    	float newY = diffY * scale;
    	
    	float edgeLength = (float)Math.sqrt( newX*newX + newY*newY );
    	if ( length >= edgeLength )
    	{
    		kCenter.X = kCurrentCenter.X + newX *.99f;
    		kCenter.Y = kCurrentCenter.Y + newY *.99f;   		

        	diffX = kCenter.X - kCurrentCenter.X;
        	diffY = kCenter.Y - kCurrentCenter.Y;
        	length = (float)Math.sqrt(diffX * diffX + diffY * diffY );
    	}
    	else
    	{
    		kCenter.X = fX;
    		kCenter.Y = fY;
    	}

    	// update the direction axis for positioning the center sphere:
    	m_kCenterDir.X = diffX / length;
    	m_kCenterDir.Y = diffY / length;
    	// update the scale along the direction for positioning the center sphere:
    	m_fCenterScale = length / edgeLength;

    	// update the scene graph:
        m_kWidget.UpdateGS();
    }
	
    /**
     * Stream this object to disk.
     * @param out output stream.
     * @throws IOException
     */
    private void writeObject(java.io.ObjectOutputStream out)
	throws IOException 
	{		        
    	out.writeObject(getCenter());
		out.writeObject( m_kCenterDir );		
		out.writeFloat( m_fCenterScale );	
		out.writeFloat( m_fRadiusX );
		out.writeFloat( m_fRadiusY );
		out.writeObject( m_kUpperSphere.Local.GetTranslate() );
		out.writeObject( m_kMiddleSphere.Local.GetTranslate() );
	}
}
