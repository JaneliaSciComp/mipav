
package gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer;

import java.awt.event.MouseEvent;
import java.io.IOException;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.Rendering.Light;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

/**
 * This class implements a 2D Triangle-shaped ClassficiationWidget. The code implements the rendering of the widget
 * in the 2D Multi-histogram panel, as well as the user mouse-interaction with the widget.
 * 
 * The parameters that define the widget are then passed to the Volume Renderer GLSL shader program and determine
 * how the Volume is displayed. *
 */
public class TriangleClassificationWidget extends ClassificationWidget
{
    /**  */
    private static final long serialVersionUID = -3903003012355432310L;

	/** Current parameterized coordinates for the center control point in X 
	 * (used to maintain relative position when resizing the widget). */
	private float m_fCenterX = 0.5f;
	/** Current parameterized coordinates for the center control point in Y 
	 * (used to maintain relative position when resizing the widget). */
	private float m_fCenterY = 0.5f;
	protected TriMesh m_kLeftSphere;

	/**
	 * @param iX location in MouseEvent Coordinates.
	 * @param iY location in MouseEvent Coordinates.
	 * @param kTMin minimum texture coordinates for the 2D histogram, used for the image background. (Defaults 0-1)
	 * @param kTMax maximum texture coordinates for the 2D histogram, used for the image background. (Defaults 0-1)
	 * @param kTexName 2D Histogram texture name.
	 * @param iWidth canvas width (default 256)
	 * @param iHeight canvas height (default 256)
	 */
    public TriangleClassificationWidget(int iX, int iY, Vector2f kTMin, Vector2f kTMax, Texture kTexture, int iWidth, int iHeight)
    {
        super( kTMin, kTMax, iWidth, iHeight );
        CreateTriangle(iX,iY, kTexture);
    }

    /* (non-Javadoc)
	 * @see gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidget#Pick(int, int)
	 */
	public boolean Pick( int iX, int iY )
	{
		boolean bPicked = false;
		m_kPicked = null;
		// translate the MouseEvent coordinates into world coordinates.
		float fX = calcObjX(iX);
		float fY = calcObjY(iY);
		if ( insideTri( fX, fY ) )
		{
			m_kPicked = m_kWidgetMesh;
			bPicked = true;
		}
		
		if ( m_kLeftSphere != null )
		{
			Vector3f kCenter = m_kLeftSphere.Local.GetTranslate();
			if ( (fX > (kCenter.X - SPHERE_RADIUS)) && (fX < (kCenter.X + SPHERE_RADIUS)) &&
					(fY > (kCenter.Y - SPHERE_RADIUS)) && (fY < (kCenter.Y + SPHERE_RADIUS)) )
			{
				// set the current picked widget:
				m_kPicked = m_kLeftSphere;
				//System.err.println( "Picked Lower" );
				bPicked = true;
			}
		}
		
		return super.Pick(iX,iY, bPicked);
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
                ShiftTriangle(e);
            }

            else if ( m_kPicked == m_kMiddleSphere )
            {
                ShiftMidTriangle(e);
            }
            else if ( m_kPicked == m_kLowerSphere)
            {
            	ScaleTriangle(e, 0, m_kLowerSphere);
            }
            else if ( m_kPicked == m_kLeftSphere )
            {
            	ScaleTriangle(e, 2, m_kLeftSphere);
            }
            else if ( m_kPicked == m_kUpperSphere )
            {
            	ScaleTriangle(e, 1, m_kUpperSphere);
            }
        }
    }
	
	/**
	 * Clears or sets the current picked object, sets the outline color to red when picked, blue when not selected.
	 * @param bPicked when true the widget is selected.
	 */
	public void setPicked( boolean bPicked )
	{
		super.setPicked(bPicked);
        m_kWidget.DetachChild( m_kUpperSphere );
        m_kWidget.DetachChild( m_kLowerSphere );
        m_kWidget.DetachChild( m_kMiddleSphere );
        m_kWidget.DetachChild( m_kLeftSphere );
		if ( bPicked )
		{
	        m_kWidget.AttachChild( m_kUpperSphere );
	        m_kWidget.AttachChild( m_kLowerSphere );
	        m_kWidget.AttachChild( m_kMiddleSphere );
	        m_kWidget.AttachChild( m_kLeftSphere );
		}
	}
	
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidget#setTexture(WildMagic.LibGraphics.Rendering.Texture)
     */
    public void setTexture( Texture kTexture )
	{
		if ( m_kWidgetMesh != null )
		{
			m_kWidgetEfect = new ClassificationWidgetEffect( kTexture, ClassificationWidgetState.Triangle );
			m_kWidgetMesh.AttachEffect( m_kWidgetEfect );
		}
	}	    
    
    /**
     * Move the middle-control point which controls the transfer function in the triangle:
     * @param e MouseEvent
     */
    public void ShiftMidTriangle( MouseEvent e )
    {
    	float fX = calcObjX(e.getX()); 
    	float fY = calcObjY(e.getY()); 
		if ( insideTri( fX, fY ) )
		{
	        // move the center sphere:
	        m_kMiddleSphere.Local.SetTranslate( fX, fY, 0.11f );
	        // update the scene graph:
	        m_kWidget.UpdateGS();
	        
	        updateCenterXY( fX, fY );
		}
		else
		{
			// Find the intersection of the mouse with the triangle edge and move the control point:
			Vector2f temp = getCenter();
			// triangle center position:
			Vector3f p1 = new Vector3f( temp.X, temp.Y, 0 );
			// vector is mouse - triangle center
			Vector3f v1 = new Vector3f( fX - p1.X, fY - p1.Y, 0 );
			
			// Test first edge:
	        Vector3f p2 = new Vector3f( m_kWidgetMesh.VBuffer.GetPosition3fX(0), m_kWidgetMesh.VBuffer.GetPosition3fY(0), 0);
	        Vector3f v2 = new Vector3f( m_kWidgetMesh.VBuffer.GetPosition3fX(1) - m_kWidgetMesh.VBuffer.GetPosition3fX(0), 
	        		m_kWidgetMesh.VBuffer.GetPosition3fY(1) - m_kWidgetMesh.VBuffer.GetPosition3fY(0), 0);
	        Vector3f pos = computeIntersect( p1, v1, p2, v2, true );
	        if ( pos != null )
	        {
		        // move the center sphere:
		        m_kMiddleSphere.Local.SetTranslate( pos.X, pos.Y, 0.11f );
		        // update the scene graph:
		        m_kWidget.UpdateGS();	        	
		        updateCenterXY( pos.X, pos.Y );
		        return;
	        }

			// Test second edge:
	        p2 = new Vector3f( m_kWidgetMesh.VBuffer.GetPosition3fX(1), m_kWidgetMesh.VBuffer.GetPosition3fY(1), 0);
	        v2 = new Vector3f( m_kWidgetMesh.VBuffer.GetPosition3fX(2) - m_kWidgetMesh.VBuffer.GetPosition3fX(1), 
	        		m_kWidgetMesh.VBuffer.GetPosition3fY(2) - m_kWidgetMesh.VBuffer.GetPosition3fY(1), 0);
	        pos = computeIntersect( p1, v1, p2, v2, true );
	        if ( pos != null )
	        {
		        // move the center sphere:
		        m_kMiddleSphere.Local.SetTranslate( pos.X, pos.Y, 0.11f );
		        // update the scene graph:
		        m_kWidget.UpdateGS();	        	
		        updateCenterXY( pos.X, pos.Y );
		        return;
	        }

			// Test third edge:
	        p2 = new Vector3f( m_kWidgetMesh.VBuffer.GetPosition3fX(2), m_kWidgetMesh.VBuffer.GetPosition3fY(2), 0);
	        v2 = new Vector3f( m_kWidgetMesh.VBuffer.GetPosition3fX(0) - m_kWidgetMesh.VBuffer.GetPosition3fX(2), 
	        		m_kWidgetMesh.VBuffer.GetPosition3fY(0) - m_kWidgetMesh.VBuffer.GetPosition3fY(2), 0);
	        pos = computeIntersect( p1, v1, p2, v2, true );
	        if ( pos != null )
	        {
		        // move the center sphere:
		        m_kMiddleSphere.Local.SetTranslate( pos.X, pos.Y, 0.11f );
		        // update the scene graph:
		        m_kWidget.UpdateGS();	        	
		        updateCenterXY( pos.X, pos.Y );
		        return;
	        }        					
		}
    }
    
    private void updateCenterXY( float fX, float fY )
    {        
        // calculate the parameterized position on the edge based on the current width:
        Vector3f v1 = new Vector3f( fX - m_kWidgetMesh.VBuffer.GetPosition3fX(0), fY - m_kWidgetMesh.VBuffer.GetPosition3fY(0), 0);
        Vector3f v2 = new Vector3f( m_kWidgetMesh.VBuffer.GetPosition3fX(1) - m_kWidgetMesh.VBuffer.GetPosition3fX(2), 
        		m_kWidgetMesh.VBuffer.GetPosition3fY(1) - m_kWidgetMesh.VBuffer.GetPosition3fY(2), 0);
        Vector3f p1 = new Vector3f( m_kWidgetMesh.VBuffer.GetPosition3fX(0), m_kWidgetMesh.VBuffer.GetPosition3fY(0), 0);
        Vector3f p2 = new Vector3f( m_kWidgetMesh.VBuffer.GetPosition3fX(2), m_kWidgetMesh.VBuffer.GetPosition3fY(2), 0);
        Vector3f pos = computeIntersect( p1, v1, p2, v2, false );
        fX = pos.X;
        
        m_fCenterX = (fX - m_kWidgetMesh.VBuffer.GetPosition3fX(2)) / (m_kWidgetMesh.VBuffer.GetPosition3fX(1) - m_kWidgetMesh.VBuffer.GetPosition3fX(2)); 
        float fY2 = m_kWidgetMesh.VBuffer.GetPosition3fY(2) + m_fCenterX * (m_kWidgetMesh.VBuffer.GetPosition3fY(1) - m_kWidgetMesh.VBuffer.GetPosition3fY(2));
        m_fCenterY = (fY - m_kWidgetMesh.VBuffer.GetPosition3fY(0)) / (fY2 - m_kWidgetMesh.VBuffer.GetPosition3fY(0)); 
    }

	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidget#updateDisplay()
	 */
	public void updateDisplay()
    {
        if ( m_kWidgetEfect != null )
        {
    		// Update the ClassificationWidgetEffect based on the position of the triangle and the mid-line control-point.            
            Vector3f midLine = m_kMiddleSphere.Local.GetTranslate();
            m_kWidgetEfect.SetMidLine( m_kWidgetMesh.VBuffer.GetTCoord2fX(0,0), m_kWidgetMesh.VBuffer.GetTCoord2fY(0,0),
            		calcTCoordX( midLine.X ), calcTCoordY( midLine.Y ));
            
          
            float fLeftTexX1 = m_kWidgetMesh.VBuffer.GetTCoord2fX(0,2);
            float fLeftTexY1 = m_kWidgetMesh.VBuffer.GetTCoord2fY(0,2);
            // set left line in texture coordinates:
            m_kWidgetEfect.SetLeftLine( m_kWidgetMesh.VBuffer.GetTCoord2fX(0,0), m_kWidgetMesh.VBuffer.GetTCoord2fY(0,0),
                    fLeftTexX1, fLeftTexY1);

            float fRightTexX1 = m_kWidgetMesh.VBuffer.GetTCoord2fX(0,1);
            float fRightTexY1 = m_kWidgetMesh.VBuffer.GetTCoord2fY(0,1);
            // set left right in texture coordinates:
            m_kWidgetEfect.SetRightLine( m_kWidgetMesh.VBuffer.GetTCoord2fX(0,0), m_kWidgetMesh.VBuffer.GetTCoord2fY(0,0),
                    fRightTexX1, fRightTexY1);

            float fX = m_kWidgetMesh.VBuffer.GetPosition3fX(2) + m_fCenterX * (m_kWidgetMesh.VBuffer.GetPosition3fX(1) - m_kWidgetMesh.VBuffer.GetPosition3fX(2));
            float fY = m_kWidgetMesh.VBuffer.GetPosition3fY(2) + m_fCenterX * (m_kWidgetMesh.VBuffer.GetPosition3fY(1) - m_kWidgetMesh.VBuffer.GetPosition3fY(2));
            fX = calcTCoordX(fX);
            fY = calcTCoordY(fY);
            float fMidX = calcTCoordX(midLine.X);
            float fMidY = calcTCoordX(midLine.Y);
            float fX0 = m_kWidgetMesh.VBuffer.GetTCoord2fX(0,0);
            float fY0 = m_kWidgetMesh.VBuffer.GetTCoord2fY(0,0);
            float distMid = (float) Math.sqrt( (fMidX - fX0) * (fMidX - fX0) + (fMidY - fY0) * (fMidY - fY0))/2f;
            float distEdge = (float) Math.sqrt( (fX - fX0) * (fX - fX0) + (fY - fY0) * (fY - fY0))/2f;
            float fIncr = (float) ( distMid / distEdge );
            m_kWidgetEfect.SetShift( fIncr, fIncr );
            
            m_kWidgetEfect.UpdateColor();
            m_kWidgetEfect.computeUniformVariables();
        }
    }
    

	/**
	 * Determines which side of the line a point lies on, used for determining
	 * if a point is inside or outside a closed loop.
	 * @param ptAx
	 * @param ptAy
	 * @param ptBx
	 * @param ptBy
	 * @param ptCx
	 * @param ptCy
	 * @return
	 */
	protected float areaTwice(float ptAx, float ptAy, float ptBx, float ptBy, float ptCx, float ptCy) {
        return (((ptAx - ptCx) * (ptBy - ptCy)) - ((ptAy - ptCy) * (ptBx - ptCx)));
    }
    
    /**
     * Creates the triangle widget and control points:
     * @param iX mouse x-position in MouseEvent coordinates.
     * @param iY mouse y-position in MouseEvent coordinates.
     * @param kTexName 2D Histogram texture name.
     */
    protected void CreateTriangle(int iX, int iY, Texture kTexture)
    {
    	// calculate the position in world coordinates:
    	float fX = calcObjX(iX);
    	float fY = calcObjY(iY);
        float fSize = .2f;
        // create the TriMesh Attributes: position, color, texture:
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);
        kAttributes.SetTChannels(0,2);
        // Set up the VertexBuffer positions, colors, texture coordinates:
        VertexBuffer kVBuffer = new VertexBuffer(kAttributes, 3);
        kVBuffer.SetPosition3(0, fX, BOTTOM_EDGE, 0.1f);
        kVBuffer.SetColor3( 0, 0, 1.0f, 0.01f, 0.01f );
        kVBuffer.SetTCoord2(0, 0, calcTCoordX(fX), calcTCoordY(BOTTOM_EDGE));

        kVBuffer.SetPosition3(1, fX+fSize, BOTTOM_EDGE+fSize, 0.1f);
        kVBuffer.SetColor3( 0, 1, 1.0f, 0.01f, 0.01f  );
        kVBuffer.SetTCoord2(0, 1, calcTCoordX(fX+fSize), calcTCoordY(BOTTOM_EDGE+fSize));


        kVBuffer.SetPosition3(2, fX-fSize, BOTTOM_EDGE+fSize, 0.1f);
        kVBuffer.SetColor3( 0, 2, 1.0f, 0.01f, 0.01f  );
        kVBuffer.SetTCoord2(0, 2, calcTCoordX(fX-fSize), calcTCoordY(BOTTOM_EDGE+fSize));

        int[] aiData = new int[]{0,1,2,0,2,1};
        IndexBuffer kIBuffer = new IndexBuffer(aiData);
        // new triangle TriMesh:
        m_kWidgetMesh = new TriMesh( kVBuffer, kIBuffer );
        // Create the ClassificationWidgetEffect, pass in the texture name:
        m_kWidgetEfect = new ClassificationWidgetEffect( kTexture, ClassificationWidgetState.Triangle );
        m_kWidgetMesh.AttachEffect( m_kWidgetEfect );
        m_kWidgetMesh.SetName("BottomTri");
        m_kWidget.AttachChild(m_kWidgetMesh);

        // Outline for the triangle, uses the same VertexBuffer as the triangle:
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
        
        
        // Attributes for the sphere control points:
        kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);
        kAttributes.SetNChannels(3);
        StandardMesh kSM = new StandardMesh(kAttributes);
        // Sphere with radius set in parent class:
        m_kUpperSphere = kSM.Sphere(10,10,SPHERE_RADIUS);
        m_kUpperSphere.AttachGlobalState(kMaterial);
        m_kUpperSphere.SetName("UpperSphere");
        m_kWidget.AttachChild( m_kUpperSphere );
        // move the sphere to the upper-right corner of the triangle widget:
        m_kUpperSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(1));


        m_kLowerSphere = kSM.Sphere(10,10,SPHERE_RADIUS);
        m_kLowerSphere.AttachGlobalState(kMaterial);
        // move the sphere to the lower-right corner of the triangle widget:
        m_kLowerSphere.Local.SetTranslate(m_kWidgetMesh.VBuffer.GetPosition3(0));
        m_kLowerSphere.SetName("LowerSphere");
        m_kWidget.AttachChild( m_kLowerSphere );               

        m_kLeftSphere = kSM.Sphere(10,10,SPHERE_RADIUS);
        m_kLeftSphere.AttachGlobalState(kMaterial);
        m_kLeftSphere.SetName("LeftSphere");
        m_kWidget.AttachChild( m_kLeftSphere );
        // move the sphere to the upper-right corner of the triangle widget:
        m_kLeftSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(2));
        

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
        

        // position the middle control-point in the center of the right-edge of the triangle:
        // calculate the parameterized position on the edge based on the current width:
        fX = m_kWidgetMesh.VBuffer.GetPosition3fX(2) + m_fCenterX * (m_kWidgetMesh.VBuffer.GetPosition3fX(1) - m_kWidgetMesh.VBuffer.GetPosition3fX(2));
        fY = m_kWidgetMesh.VBuffer.GetPosition3fY(2) + m_fCenterX * (m_kWidgetMesh.VBuffer.GetPosition3fY(1) - m_kWidgetMesh.VBuffer.GetPosition3fY(2));

        float fX2 = m_kWidgetMesh.VBuffer.GetPosition3fX(0) + m_fCenterY * (fX - m_kWidgetMesh.VBuffer.GetPosition3fX(0));
        float fY2 = m_kWidgetMesh.VBuffer.GetPosition3fY(0) + m_fCenterY * (fY - m_kWidgetMesh.VBuffer.GetPosition3fY(0));
        m_kMiddleSphere.Local.SetTranslate( fX2, fY2, 0.11f );
                
        // Update RS updates the lighting effects:
        m_kWidget.UpdateRS();
        // update scene-graph
        m_kWidget.UpdateGS();
    }


    /**
     * Resizes the triangle using the lower-control point:
     * @param e MouseEvent
     */
    protected void ScaleTriangle(MouseEvent e, int position, TriMesh sphere )
    {
    	// Calculate the position in world-coordinates:
    	float fX = calcObjX(e.getX()); 
    	float fY = calcObjY(e.getY());  
    	
    	// clamp the position in X to the left and right edges:
    	fX = Math.min( fX, RIGHT_EDGE );
    	fX = Math.max( fX, LEFT_EDGE );
        // clamp the position in Y to the top of the triangle and the bottom screen:
    	fY = Math.min( fY, TOP_EDGE );
    	fY = Math.max( fY, BOTTOM_EDGE );
    	
    	
    	// Move triangle bottom-corner:
        m_kWidgetMesh.VBuffer.SetPosition3( position, fX, fY,
        		m_kWidgetMesh.VBuffer.GetPosition3fZ(0) );
        m_kWidgetMesh.VBuffer.SetTCoord2(0, position, calcTCoordX(fX), calcTCoordY(fY));
        m_kWidgetMesh.VBuffer.Release();
        // Reposition lower sphere:
        sphere.Local.SetTranslate( fX, fY, 0.11f );
             
        // Reposition middle sphere after scale:
        // calculate the parameterized position on the edge based on the current width:
        fX = m_kWidgetMesh.VBuffer.GetPosition3fX(2) + m_fCenterX * (m_kWidgetMesh.VBuffer.GetPosition3fX(1) - m_kWidgetMesh.VBuffer.GetPosition3fX(2));
        fY = m_kWidgetMesh.VBuffer.GetPosition3fY(2) + m_fCenterX * (m_kWidgetMesh.VBuffer.GetPosition3fY(1) - m_kWidgetMesh.VBuffer.GetPosition3fY(2));

        float fX2 = m_kWidgetMesh.VBuffer.GetPosition3fX(0) + m_fCenterY * (fX - m_kWidgetMesh.VBuffer.GetPosition3fX(0));
        float fY2 = m_kWidgetMesh.VBuffer.GetPosition3fY(0) + m_fCenterY * (fY - m_kWidgetMesh.VBuffer.GetPosition3fY(0));
        m_kMiddleSphere.Local.SetTranslate( fX2, fY2, 0.11f );
        m_kWidget.UpdateGS();
        
        
    }
    
    /**
     * Shifts the triangle along the bottom edge of the 2D Histogram, using the bottom control-point.
     * Shift only moves the triangle to the left or right, not up or down.
     * @param e MouseEvent
     */
    protected void ShiftTriangle(MouseEvent e)
    {
    	// get the current center of the triangle:
    	Vector2f kCurrentCenter = getCenter();

    	// get the mouse relative position:
        float fNewGCX = e.getX() + m_kMouseOffset.X;
        fNewGCX = calcObjX( fNewGCX );
        float fNewGCY = e.getY() + m_kMouseOffset.Y;
        fNewGCY = calcObjY( fNewGCY );

        // calculate the difference in the current center and the mouse position:
        float fDiffX = fNewGCX - kCurrentCenter.X;
        float fDiffY = fNewGCY - kCurrentCenter.Y;

        // X check that moving the triangle doesn't move it out of bounds,
        // clamp if necessary:
        float fNewXL = m_kWidgetMesh.VBuffer.GetPosition3fX(2);
        float fNewXR = m_kWidgetMesh.VBuffer.GetPosition3fX(1);
        float fNewXC = m_kWidgetMesh.VBuffer.GetPosition3fX(0);
        if ( fNewXR + fDiffX > RIGHT_EDGE )
        {
        	fDiffX = RIGHT_EDGE - fNewXR;
        }
        if ( fNewXL + fDiffX < LEFT_EDGE )
        {
        	fDiffX = LEFT_EDGE - fNewXL;
        }
        if ( fNewXC + fDiffX < LEFT_EDGE )
        {
        	fDiffX = LEFT_EDGE - fNewXC;
        }
        if ( fNewXC + fDiffX > RIGHT_EDGE )
        {
        	fDiffX = RIGHT_EDGE - fNewXC;
        }
        
        
        // Y check that moving the triangle doesn't move it out of bounds,
        // clamp if necessary:
        float fNewYB = m_kWidgetMesh.VBuffer.GetPosition3fY(0);
        float fNewYT = m_kWidgetMesh.VBuffer.GetPosition3fY(2);
        if ( fNewYB + fDiffY < BOTTOM_EDGE )
        {
        	fDiffY = BOTTOM_EDGE - fNewYB;
        }
        if ( fNewYT + fDiffY > TOP_EDGE )
        {
        	fDiffY = TOP_EDGE - fNewYT;
        }


        // Move the triangle: position and texture coordinates:
        float fNewX, fNewY;
        for ( int i = 0; i < m_kWidgetMesh.VBuffer.GetVertexQuantity(); i++ )
        {
            fNewX = m_kWidgetMesh.VBuffer.GetPosition3fX(i) + fDiffX;
            fNewY = m_kWidgetMesh.VBuffer.GetPosition3fY(i) + fDiffY;
            m_kWidgetMesh.VBuffer.SetPosition3( i, fNewX, fNewY,
            		m_kWidgetMesh.VBuffer.GetPosition3fZ(i) );
            m_kWidgetMesh.VBuffer.SetTCoord2(0, i, calcTCoordX(fNewX), calcTCoordY(fNewY));
        }
        m_kWidgetMesh.VBuffer.Release();

        // Move the upper and lower spheres based on the triangle corners:
        m_kUpperSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(1));
        m_kLowerSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(0));
        m_kLeftSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(2));

        // Move the center sphere by translating it the same amount as the triangle:
        Vector3f kTranslate = m_kMiddleSphere.Local.GetTranslate(); 
        m_kMiddleSphere.Local.SetTranslate( kTranslate.X + fDiffX, kTranslate.Y + fDiffY, kTranslate.Z );

        // Update the scene-graph
        m_kWidget.UpdateGS();
    }
    
    
    /**
     * Computes the intersection between two lines.
     * @param p0 starting position of line 0.
     * @param v0 line 0 direction vector.
     * @param p1 starting position of line 1.
     * @param v1 line 1 direction vector.
     * @return intersection point.
     */
    private Vector3f computeIntersect( Vector3f p0, Vector3f v0, Vector3f p1, Vector3f v1, boolean onLine )
    {
    	float fDet = (v1.X * v0.Y - v1.Y * v0.X);
    	float len0 = v0.X * v0.X + v0.Y * v0.Y;
    	float len1 = v1.X * v1.X + v1.Y * v1.Y;
    	if ( (fDet * fDet) < (0.00000012 * len0 * len1)) {
    		return p0;
    	}
    	float fInvDet = (float) (1.0 / fDet);
    	Vector3f diff = new Vector3f( p1.X - p0.X, p1.Y - p0.Y, 0);
    	float s = (v1.X * diff.Y - v1.Y * diff.X) * fInvDet;
    	float t = (v0.X * diff.Y - v0.Y * diff.X) * fInvDet;
    	Vector3f intersectPoint = new Vector3f( v0.X * s + p0.X, v0.Y * s + p0.Y, 0);
    	if ( !onLine || ((s >= 0) && (s <= 1) && (t >= 0) && (t <= 1)) )
    	{
    		return intersectPoint;
    	}
    	return null;
    }

    /**
     * Determines if the input position is inside the triangle widget.
     * @param fX x-position.
     * @param fY y-position.
     * @return true if inside the triangle, false otherwise.
     */
    private boolean insideTri(float fX, float fY )
    {
		float area0_1 = areaTwice( m_kWidgetMesh.VBuffer.GetPosition3fX(0), m_kWidgetMesh.VBuffer.GetPosition3fY(0),
				m_kWidgetMesh.VBuffer.GetPosition3fX(1), m_kWidgetMesh.VBuffer.GetPosition3fY(1),
				fX, fY );

		float area1_2 = areaTwice( m_kWidgetMesh.VBuffer.GetPosition3fX(1), m_kWidgetMesh.VBuffer.GetPosition3fY(1),
				m_kWidgetMesh.VBuffer.GetPosition3fX(2), m_kWidgetMesh.VBuffer.GetPosition3fY(2),
				fX, fY );
		
		float area2_0 = areaTwice( m_kWidgetMesh.VBuffer.GetPosition3fX(2), m_kWidgetMesh.VBuffer.GetPosition3fY(2),
				m_kWidgetMesh.VBuffer.GetPosition3fX(0), m_kWidgetMesh.VBuffer.GetPosition3fY(0),
				fX, fY );
		// The triangle may be clockwise sorted or counter-clockwise. If the areas are consistent
		// either all > 0 or all < 0 then the position can be considered 'inside' the triangle.
		if ( area0_1 >= 0 && area1_2 >= 0 && area2_0 >= 0 )
		{
			return true;
		}
		else if ( area0_1 <= 0 && area1_2 <= 0 && area2_0 <= 0 )
		{
			return true;
		}
		// not inside.
		return false;
    }


    /**
     * Read this object from disk.
     * @param in
     * @throws IOException
     * @throws ClassNotFoundException
     */
    private void readObject(java.io.ObjectInputStream in)
    throws IOException, ClassNotFoundException
    {        
        m_kUpperSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(1));
        
        // Create the material to describe the shading for the sphere:
        MaterialState kMaterial = new MaterialState();
        kMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
        kMaterial.Ambient = new ColorRGB(0.2f,0.2f,0.2f);
        kMaterial.Diffuse = new ColorRGB(0f,0f,1f);
        kMaterial.Specular = new ColorRGB(0.9f,0.9f,0.9f);
        kMaterial.Shininess = 83.2f;
        
		IndexBuffer kIBuffer = (IndexBuffer)in.readObject();
		VertexBuffer kVBuffer = (VertexBuffer)in.readObject();
        m_kLowerSphere = new TriMesh( kVBuffer, kIBuffer );
        m_kLowerSphere.AttachGlobalState( kMaterial );
        m_kLowerSphere.SetName("LowerSphere");
        m_kWidget.AttachChild( m_kLowerSphere );
        m_kLowerSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(0));
        
    	kIBuffer = (IndexBuffer)in.readObject();
    	kVBuffer = (VertexBuffer)in.readObject();
        m_kLeftSphere = new TriMesh( kVBuffer, kIBuffer );
        m_kLeftSphere.AttachGlobalState( kMaterial );
        m_kLeftSphere.SetName("LeftSphere");
        m_kWidget.AttachChild( m_kLeftSphere );     
        // move the sphere to the upper-right corner of the triangle widget:
        m_kLeftSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(2));

        m_fCenterX = in.readFloat();
        m_fCenterY = in.readFloat();
        // calculate the parameterized position on the edge based on the current width:
        float fX = m_kWidgetMesh.VBuffer.GetPosition3fX(2) + m_fCenterX * (m_kWidgetMesh.VBuffer.GetPosition3fX(1) - m_kWidgetMesh.VBuffer.GetPosition3fX(2));
        float fY = m_kWidgetMesh.VBuffer.GetPosition3fY(2) + m_fCenterX * (m_kWidgetMesh.VBuffer.GetPosition3fY(1) - m_kWidgetMesh.VBuffer.GetPosition3fY(2));

        float fX2 = m_kWidgetMesh.VBuffer.GetPosition3fX(0) + m_fCenterY * (fX - m_kWidgetMesh.VBuffer.GetPosition3fX(0));
        float fY2 = m_kWidgetMesh.VBuffer.GetPosition3fY(0) + m_fCenterY * (fY - m_kWidgetMesh.VBuffer.GetPosition3fY(0));
        m_kMiddleSphere.Local.SetTranslate( fX2, fY2, 0.11f );

        // Update RS updates the lighting effects:
        m_kWidget.UpdateRS();
        m_kWidget.UpdateGS();
    }
    
    /**
	 * Stream this object to disk.
	 * @param out
	 * @throws IOException
	 */
    private void writeObject(java.io.ObjectOutputStream out)
	throws IOException 
	{
		out.writeObject( m_kLowerSphere.IBuffer );
		out.writeObject( m_kLowerSphere.VBuffer );
		
		out.writeObject( m_kLeftSphere.IBuffer );
		out.writeObject( m_kLeftSphere.VBuffer );	
		
		out.writeFloat(m_fCenterX);
		out.writeFloat(m_fCenterY);		
	}

}
