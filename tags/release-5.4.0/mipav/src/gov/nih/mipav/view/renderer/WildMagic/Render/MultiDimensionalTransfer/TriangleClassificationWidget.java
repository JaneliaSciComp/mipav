
package gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer;

import java.awt.event.MouseEvent;
import java.io.IOException;

import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Node;
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

	/** Current parameterized coordinates for the transfer-function control point
	 * (used to maintain relative position when resizing the widget). */
    private float m_fCenterT = 0.5f;

	/**
	 * @param iX location in MouseEvent Coordinates.
	 * @param iY location in MouseEvent Coordinates.
	 * @param kTMin minimum texture coordinates for the 2D histogram, used for the image background. (Defaults 0-1)
	 * @param kTMax maximum texture coordinates for the 2D histogram, used for the image background. (Defaults 0-1)
	 * @param kTexName 2D Histogram texture name.
	 * @param iWidth canvas width (default 256)
	 * @param iHeight canvas height (default 256)
	 */
    public TriangleClassificationWidget(int iX, int iY, Vector2f kTMin, Vector2f kTMax, String kTexName, int iWidth, int iHeight)
    {
        super( kTMin, kTMax, iWidth, iHeight );
        CreateTriangle(iX,iY, kTexName);
    }

    /**
     * Copy constructor.
     * @param kWidget
     */
    public TriangleClassificationWidget ( TriangleClassificationWidget kWidget )
    {
        super(kWidget);
        m_kWidgetMesh = new TriMesh(new VertexBuffer(kWidget.m_kWidgetMesh.VBuffer),
                new IndexBuffer(kWidget.m_kWidgetMesh.IBuffer));        
        m_kOutline = new Polyline( m_kWidgetMesh.VBuffer, true, true );
        m_kWidgetEfect = new ClassificationWidgetEffect( kWidget.m_kWidgetEfect );         

        m_kUpperSphere = new TriMesh(new VertexBuffer(kWidget.m_kUpperSphere.VBuffer),
                new IndexBuffer(kWidget.m_kUpperSphere.IBuffer));
        m_kMiddleSphere = new TriMesh(new VertexBuffer(kWidget.m_kMiddleSphere.VBuffer),
                new IndexBuffer(kWidget.m_kMiddleSphere.IBuffer));
        m_kLowerSphere = new TriMesh(new VertexBuffer(kWidget.m_kLowerSphere.VBuffer),
                new IndexBuffer(kWidget.m_kLowerSphere.IBuffer));
    }

    @Override
	public void processMouseDrag(int iX0ld, int iYOld, int iButton, MouseEvent e )
    {
        float fXOld = ((float)iX0ld/(float)m_iWidth);
        float fYOld = ((float)m_iHeight-(float)iYOld)/m_iHeight;

        float fX = ((float)e.getX()/(float)m_iWidth);
        float fY = ((float)m_iHeight-(float)e.getY())/m_iHeight;

        if ( iButton == MouseEvent.BUTTON1 )
        {
            if ( m_kPicked == m_kWidgetMesh )
            {
                ShearTriangle(fX-fXOld, fY-fYOld);
            }

            else if ( m_kPicked == m_kMiddleSphere )
            {
                ShiftMidTriangle(e);
            }
            else if ( m_kPicked == m_kLowerSphere)
            {
                ShiftTriangle(e);
            }
            else if ( m_kPicked == m_kUpperSphere )
            {
                ScaleTriangle(e);
            }
        }
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
		
		// find the edges of the triangle at the current mouse position in world coordinates:
        float fT = (fY - m_kWidgetMesh.VBuffer.GetPosition3fY(0)) / 
        (m_kWidgetMesh.VBuffer.GetPosition3fY(2) - m_kWidgetMesh.VBuffer.GetPosition3fY(0));

        // calculate the left-edge at this height in the triangle:
        float fLeftEdge = m_kWidgetMesh.VBuffer.GetPosition3fX(0) + fT * 
        (m_kWidgetMesh.VBuffer.GetPosition3fX(2) - m_kWidgetMesh.VBuffer.GetPosition3fX(0));
        // calculate the right-edge at this height in the triangle:
        float fRightEdge = m_kWidgetMesh.VBuffer.GetPosition3fX(0) + fT * 
        (m_kWidgetMesh.VBuffer.GetPosition3fX(1) - m_kWidgetMesh.VBuffer.GetPosition3fX(0));
		// if the mouse is inside the triangle:
		if ( (fX >= fLeftEdge) && (fX <= fRightEdge) &&
				(fY >= m_kWidgetMesh.VBuffer.GetPosition3fY(0)) && (fY <= m_kWidgetMesh.VBuffer.GetPosition3fY(2)) )
		{
			m_kPicked = m_kWidgetMesh;
			bPicked = true;
		}
		return super.Pick(iX,iY, bPicked);
	}

    /**
     * Move the middle-control point which controls the transfer function in the triangle:
     * @param e MouseEvent
     */
    public void ShiftMidTriangle( MouseEvent e )
    {
    	float fY = calcObjY(e.getY()); 
    	Vector3f kCenter = m_kMiddleSphere.Local.GetTranslate();
    	// clamp the y-position to the top and bottom of the triangle:
        fY = Math.max( fY, m_kWidgetMesh.VBuffer.GetPosition3fY(0) );
        fY = Math.min( fY, m_kWidgetMesh.VBuffer.GetPosition3fY(2) );
        kCenter.Y = fY;
        // calculate the parameterized position on the edge based on the current height:
        m_fCenterT = (fY - m_kWidgetMesh.VBuffer.GetPosition3fY(0)) / 
        (m_kWidgetMesh.VBuffer.GetPosition3fY(2) - m_kWidgetMesh.VBuffer.GetPosition3fY(0));

        // find the x-position based on the parameterized position on the line:
        float fX = m_kWidgetMesh.VBuffer.GetPosition3fX(0) + m_fCenterT * 
        (m_kWidgetMesh.VBuffer.GetPosition3fX(1) - m_kWidgetMesh.VBuffer.GetPosition3fX(0));
        // move the center sphere:
        m_kMiddleSphere.Local.SetTranslate( fX, fY, 0.11f );
        // update the scene graph:
        m_kWidget.UpdateGS();
    }
    
    @Override
	public void updateDisplay()
    {
        if ( m_kWidgetEfect != null )
        {
    		// Update the ClassificationWidgetEffect based on the position of the square and the mid-line control-point.
            float fMidTexX = (m_kWidgetMesh.VBuffer.GetTCoord2fX(0,1) + m_kWidgetMesh.VBuffer.GetTCoord2fX(0,2) )/2.0f;
            float fMidTexY = (m_kWidgetMesh.VBuffer.GetTCoord2fY(0,1) + m_kWidgetMesh.VBuffer.GetTCoord2fY(0,2) )/2.0f;
            // set mid-line in texture coordinates:
            m_kWidgetEfect.SetMidLine( m_kWidgetMesh.VBuffer.GetTCoord2fX(0,0),
                    m_kWidgetMesh.VBuffer.GetTCoord2fY(0,0),
                    fMidTexX, fMidTexY);
            

            //Left Mid calc:
            Vector3f kPos0 = m_kWidgetMesh.VBuffer.GetPosition3(0);
            Vector3f kPos1 = m_kWidgetMesh.VBuffer.GetPosition3(2);

            float fNewX = m_fCenterT * kPos1.X + (1.0f - m_fCenterT) * kPos0.X;
            float fNewY = m_fCenterT * kPos1.Y + (1.0f - m_fCenterT) * kPos0.Y;
            float fLeft_TX = calcTCoordX(fNewX);
            float fLeft_TY = calcTCoordY(fNewY);

            //Right Mid calc:
            kPos0 = m_kWidgetMesh.VBuffer.GetPosition3(0);
            kPos1 = m_kWidgetMesh.VBuffer.GetPosition3(1);

            fNewX = m_fCenterT * kPos1.X + (1.0f - m_fCenterT) * kPos0.X;
            fNewY = m_fCenterT * kPos1.Y + (1.0f - m_fCenterT) * kPos0.Y;
            float fRight_TX = calcTCoordX(fNewX);
            float fRight_TY = calcTCoordY(fNewY);
            

            float fLeftTexX1 = m_kWidgetMesh.VBuffer.GetTCoord2fX(0,2);
            float fLeftTexY1 = m_kWidgetMesh.VBuffer.GetTCoord2fY(0,2);
            // set left line in texture coordinates:
            m_kWidgetEfect.SetLeftLine( fLeft_TX, fLeft_TY,
                    fLeftTexX1, fLeftTexY1);

            float fRightTexX1 = m_kWidgetMesh.VBuffer.GetTCoord2fX(0,1);
            float fRightTexY1 = m_kWidgetMesh.VBuffer.GetTCoord2fY(0,1);
            // set left right in texture coordinates:
            m_kWidgetEfect.SetRightLine( fRight_TX, fRight_TY,
                    fRightTexX1, fRightTexY1);

            m_kWidgetEfect.UpdateColor();
            m_kWidgetEfect.UpdateLUT();
            m_kWidgetEfect.computeUniformVariables();
        }
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
        m_kWidget = new Node();
        
        IndexBuffer kIBuffer = (IndexBuffer)in.readObject();
        VertexBuffer kVBuffer = (VertexBuffer)in.readObject();
        m_kWidgetMesh = new TriMesh( kVBuffer, kIBuffer );
        m_kWidgetEfect = (ClassificationWidgetEffect)in.readObject();
        m_kWidgetMesh.AttachEffect( m_kWidgetEfect );
        m_kWidgetMesh.SetName("BottomTri");
        m_kWidget.AttachChild(m_kWidgetMesh);

        m_kOutline = new Polyline( m_kWidgetMesh.VBuffer, true, true );
        m_kOutline.AttachEffect( new VertexColor3Effect() );
        m_kWidget.AttachChild(m_kOutline);

        
        kIBuffer = (IndexBuffer)in.readObject();
        kVBuffer = (VertexBuffer)in.readObject();
        m_kUpperSphere = new TriMesh( kVBuffer, kIBuffer );
        m_kUpperSphere.AttachEffect( new VertexColor3Effect() );
        m_kUpperSphere.SetName("UpperSphere");
        m_kWidget.AttachChild( m_kUpperSphere );
        m_kUpperSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(1));
    
        
        kIBuffer = (IndexBuffer)in.readObject();
        kVBuffer = (VertexBuffer)in.readObject();
        m_kLowerSphere = new TriMesh( kVBuffer, kIBuffer );
        m_kLowerSphere.AttachEffect( new VertexColor3Effect() );
        m_kLowerSphere.SetName("LowerSphere");
        m_kWidget.AttachChild( m_kLowerSphere );
        m_kLowerSphere.Local.SetTranslate(m_kWidgetMesh.VBuffer.GetPosition3(0));
        
        kIBuffer = (IndexBuffer)in.readObject();
        kVBuffer = (VertexBuffer)in.readObject();
        m_kMiddleSphere = new TriMesh( kVBuffer, kIBuffer );
        m_kMiddleSphere.AttachEffect( new VertexColor3Effect() );
        m_kMiddleSphere.SetName("MiddleSphere");
        m_kWidget.AttachChild( m_kMiddleSphere );     

        m_fCenterT = in.readFloat();
        // reposition mid-line control-point:
        float fX = m_kWidgetMesh.VBuffer.GetPosition3fX(0) + m_fCenterT * 
        (m_kWidgetMesh.VBuffer.GetPosition3fX(1) - m_kWidgetMesh.VBuffer.GetPosition3fX(0));
        float fY = m_kWidgetMesh.VBuffer.GetPosition3fY(0) + m_fCenterT * 
        (m_kWidgetMesh.VBuffer.GetPosition3fY(1) - m_kWidgetMesh.VBuffer.GetPosition3fY(0));
        m_kMiddleSphere.Local.SetTranslate( fX, fY, 0.11f );

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
		out.writeFloat(m_fCenterT);
	}
    


    /**
     * Creates the triangle widget and control points:
     * @param iX mouse x-position in MouseEvent coordinates.
     * @param iY mouse y-position in MouseEvent coordinates.
     * @param kTexName 2D Histogram texture name.
     */
    protected void CreateTriangle(int iX, int iY, String kTexName)
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

        int[] aiData = new int[]{0,1,2};
        IndexBuffer kIBuffer = new IndexBuffer(aiData);
        // new triangle TriMesh:
        m_kWidgetMesh = new TriMesh( kVBuffer, kIBuffer );
        // Create the ClassificationWidgetEffect, pass in the texture name:
        m_kWidgetEfect = new ClassificationWidgetEffect( kTexName );
        m_kWidgetMesh.AttachEffect( m_kWidgetEfect );
        m_kWidgetMesh.SetName("BottomTri");
        m_kWidget.AttachChild(m_kWidgetMesh);

        // Outline for the triangle, uses the same VertexBuffer as the square:
        m_kOutline = new Polyline( m_kWidgetMesh.VBuffer, true, true );
        m_kOutline.AttachEffect( new VertexColor3Effect() );
        m_kWidget.AttachChild(m_kOutline);


        // Attributes for the sphere control points:
        kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);
        StandardMesh kSM = new StandardMesh(kAttributes);
        // Sphere with radius set in parent class:
        m_kUpperSphere = kSM.Sphere(10,10,SPHERE_RADIUS);
        for ( int i = 0; i < m_kUpperSphere.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kUpperSphere.VBuffer.SetColor3(0, i, 0f, 0f, 1f);
        }
        m_kUpperSphere.AttachEffect( new VertexColor3Effect() );
        m_kUpperSphere.SetName("UpperSphere");
        m_kWidget.AttachChild( m_kUpperSphere );
        // move the sphere to the upper-right corner of the square widget:
        m_kUpperSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(1));


        m_kMiddleSphere = kSM.Sphere(10,10,SPHERE_RADIUS);
        for ( int i = 0; i < m_kMiddleSphere.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kMiddleSphere.VBuffer.SetColor3(0, i, 0f, 1f, 0f);
        }
        m_kMiddleSphere.AttachEffect( new VertexColor3Effect() );
        m_kMiddleSphere.SetName("MiddleSphere");
        m_kWidget.AttachChild( m_kMiddleSphere );
        

        // position the middle control-point in the center of the right-edge of the triangle:
        fX = m_kWidgetMesh.VBuffer.GetPosition3fX(0) + m_fCenterT * 
        (m_kWidgetMesh.VBuffer.GetPosition3fX(1) - m_kWidgetMesh.VBuffer.GetPosition3fX(0));
        fY = m_kWidgetMesh.VBuffer.GetPosition3fY(0) + m_fCenterT * 
        (m_kWidgetMesh.VBuffer.GetPosition3fY(1) - m_kWidgetMesh.VBuffer.GetPosition3fY(0));
        m_kMiddleSphere.Local.SetTranslate( fX, fY, 0.11f );
        


        m_kLowerSphere = kSM.Sphere(10,10,SPHERE_RADIUS);
        for ( int i = 0; i < m_kLowerSphere.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kLowerSphere.VBuffer.SetColor3(0, i, 0f, 0f, 1f);
        }
        // move the sphere to the lower-right corner of the square widget:
        m_kLowerSphere.Local.SetTranslate(m_kWidgetMesh.VBuffer.GetPosition3(0));
        m_kLowerSphere.AttachEffect( new VertexColor3Effect() );
        m_kLowerSphere.SetName("LowerSphere");
        m_kWidget.AttachChild( m_kLowerSphere );
        // update scene-graph
        m_kWidget.UpdateGS();
    }


    /**
     * Resizes the triangle without shearing, using the upper-control point:
     * @param e MouseEvent
     */
    protected void ScaleTriangle(MouseEvent e)
    {
    	// Calculate the position in world-coordinates:
    	float fX = calcObjX(e.getX()); 
    	float fY = calcObjY(e.getY());  
    	// get the current mid-line of the top edge of the triangle:
        float centerX = (m_kWidgetMesh.VBuffer.GetPosition3fX( 1 ) + m_kWidgetMesh.VBuffer.GetPosition3fX( 2 ))/2f;
        // clamp the position in X to the midline and RIGHT_EDGE:
    	fX = Math.max( fX, centerX );
    	fX = Math.min( fX, RIGHT_EDGE );
    	// clamp the position in Y to the TOP_EDGE and the bottom of the triangle:
    	fY = Math.min( fY, TOP_EDGE );
    	fY = Math.max( fY, m_kWidgetMesh.VBuffer.GetPosition3fY(0) );
    	
    	// Scaling moves the left and right triangle corners. 
    	// Bound-check the left edge to make sure the scaling clamps.
    	// If clamping, change diffX and fX.
        float diffX = fX - centerX;
        if ( (centerX - diffX) < LEFT_EDGE )
        {
        	diffX = centerX - LEFT_EDGE;
        	fX = centerX + diffX;
        }    	
    	
    	// Move triangle right-corner:
        m_kWidgetMesh.VBuffer.SetPosition3( 1, fX, fY,
        		m_kWidgetMesh.VBuffer.GetPosition3fZ(1) );
        m_kWidgetMesh.VBuffer.SetTCoord2(0, 1, calcTCoordX(fX), calcTCoordY(fY));
        
        // Move triangle left-corner:
        m_kWidgetMesh.VBuffer.SetPosition3( 2, centerX - diffX, fY,
        		m_kWidgetMesh.VBuffer.GetPosition3fZ(1) );
        m_kWidgetMesh.VBuffer.SetTCoord2(0, 2, calcTCoordX(centerX - diffX), calcTCoordY(fY));
        m_kWidgetMesh.VBuffer.Release();
     
        // Reposition upper sphere:
        m_kUpperSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(1));

        // Reposition middle sphere after scale:
        fX = m_kWidgetMesh.VBuffer.GetPosition3fX(0) + m_fCenterT * 
        (m_kWidgetMesh.VBuffer.GetPosition3fX(1) - m_kWidgetMesh.VBuffer.GetPosition3fX(0));
        fY = m_kWidgetMesh.VBuffer.GetPosition3fY(0) + m_fCenterT * 
        (m_kWidgetMesh.VBuffer.GetPosition3fY(1) - m_kWidgetMesh.VBuffer.GetPosition3fY(0));
        m_kMiddleSphere.Local.SetTranslate( fX, fY, 0.11f );
        m_kWidget.UpdateGS();
    }

    /**
     * Shears the triangle based on the mouse movement. When the user clicks and drags inside the triangle.
     * @param fX change in mouse position
     * @param fY change in mouse position.
     */
    protected void ShearTriangle(float fX, float fY)
    {
        float fNewXR = Math.max( m_kWidgetMesh.VBuffer.GetPosition3fX(1),
        		m_kWidgetMesh.VBuffer.GetPosition3fX(0) );
        float fNewXL = Math.min( m_kWidgetMesh.VBuffer.GetPosition3fX(2),
        		m_kWidgetMesh.VBuffer.GetPosition3fX(0) );
        // Clamp the the edges of the world:
        if ( fX < 0 )
        {
            if ( (fNewXL + fX) < LEFT_EDGE )
            {
                fX = LEFT_EDGE - fNewXL;
            }
        }
        if ( fX > 0 )
        {
            if ( (fNewXR + fX) >= RIGHT_EDGE )
            {
                fX = RIGHT_EDGE - fNewXR;
            }
        }    


        float fNewX, fNewY;
        fNewX = m_kWidgetMesh.VBuffer.GetPosition3fX(1) + fX;
        fNewX = Math.max( LEFT_EDGE, fNewX );
        fNewX = Math.min( fNewX, RIGHT_EDGE );
            
        fNewY = m_kWidgetMesh.VBuffer.GetPosition3fY(1) + fY;
        fNewY = Math.max( BOTTOM_EDGE, fNewY );
        fNewY = Math.min( fNewY, TOP_EDGE );
        m_kWidgetMesh.VBuffer.SetPosition3( 1, fNewX, fNewY,
        		m_kWidgetMesh.VBuffer.GetPosition3fZ(1) );
        m_kWidgetMesh.VBuffer.SetTCoord2(0, 1, calcTCoordX(fNewX), calcTCoordY(fNewY));

        
        fNewX = m_kWidgetMesh.VBuffer.GetPosition3fX(2) + fX;
        fNewX = Math.max( LEFT_EDGE, fNewX );
        fNewX = Math.min( fNewX, RIGHT_EDGE );
            
        fNewY = m_kWidgetMesh.VBuffer.GetPosition3fY(2) + fY;
        fNewY = Math.max( BOTTOM_EDGE, fNewY );
        fNewY = Math.min( fNewY, TOP_EDGE );
        m_kWidgetMesh.VBuffer.SetPosition3( 2, fNewX, fNewY,
        		m_kWidgetMesh.VBuffer.GetPosition3fZ(2) );
        m_kWidgetMesh.VBuffer.SetTCoord2(0, 2, calcTCoordX(fNewX), calcTCoordY(fNewY));
        m_kWidgetMesh.VBuffer.Release();

        // Reposition top sphere:
        m_kUpperSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(1));

        // Reposition middle sphere:
        fX = m_kWidgetMesh.VBuffer.GetPosition3fX(0) + m_fCenterT * 
        (m_kWidgetMesh.VBuffer.GetPosition3fX(1) - m_kWidgetMesh.VBuffer.GetPosition3fX(0));
        fY = m_kWidgetMesh.VBuffer.GetPosition3fY(0) + m_fCenterT * 
        (m_kWidgetMesh.VBuffer.GetPosition3fY(1) - m_kWidgetMesh.VBuffer.GetPosition3fY(0));
        m_kMiddleSphere.Local.SetTranslate( fX, fY, 0.11f );
        
        // Update the scene-graph
        m_kWidget.UpdateGS();
    }


    /**
     * Shifts the triangle along the bottom edge of the 2D Histogram, using the bottom control-point.
     * Shift only moves the triangle to the left or right, not up or down.
     * @param e MouseEvent
     */
    protected void ShiftTriangle(MouseEvent e)
    {
    	// get the current center of the square:
    	Vector2f kCurrentCenter = getCenter();

    	// get the mouse relative position:
        float fNewGCX = e.getX() + m_kMouseOffset.X;
        fNewGCX = calcObjX( fNewGCX );
        float fNewGCY = e.getY() + m_kMouseOffset.Y;
        fNewGCY = calcObjY( fNewGCY );

        // calculate the difference in the current center and the mouse position:
        float fDiffX = fNewGCX - kCurrentCenter.X;
        float fDiffY = 0;// no movement in the y-direction

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


        // Move the square: position and texture coordinates:
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

        // Move the upper and lower spheres based on the square corners:
        m_kUpperSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(1));
        m_kLowerSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(0));

        // Move the center sphere by translating it the same amount as the square:
        Vector3f kTranslate = m_kMiddleSphere.Local.GetTranslate(); 
        m_kMiddleSphere.Local.SetTranslate( kTranslate.X + fDiffX, kTranslate.Y + fDiffY, kTranslate.Z );

        // Update the scene-graph
        m_kWidget.UpdateGS();
    }

}
