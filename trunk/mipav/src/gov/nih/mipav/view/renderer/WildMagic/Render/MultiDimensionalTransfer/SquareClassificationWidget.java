
package gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer;

import java.awt.event.MouseEvent;
import java.io.IOException;

import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

/**
 * This class implements a 2D Square-shaped ClassficiationWidget. The code implements the rendering of the widget
 * in the 2D Multi-histogram panel, as well as the user mouse-interaction with the widget.
 * 
 * The parameters that define the widget are then passed to the Volume Renderer GLSL shader program and determine
 * how the Volume is displayed. *
 */
public class SquareClassificationWidget extends ClassificationWidget
{

	private static final long serialVersionUID = -4180814068815939724L;

	/** Current parameterized coordinates for the center control point in X 
	 * (used to maintain relative position when resizing the widget). */
	private float m_fCenterX = 0.5f;
	/** Current parameterized coordinates for the center control point in Y 
	 * (used to maintain relative position when resizing the widget). */
	private float m_fCenterY = 0.5f;
	
	/**
	 * @param iX location in MouseEvent Coordinates.
	 * @param iY location in MouseEvent Coordinates.
	 * @param kTMin minimum texture coordinates for the 2D histogram, used for the image background. (Defaults 0-1)
	 * @param kTMax maximum texture coordinates for the 2D histogram, used for the image background. (Defaults 0-1)
	 * @param kTexName 2D Histogram texture name.
	 * @param iWidth canvas width (default 256)
	 * @param iHeight canvas height (default 256)
	 */
	public SquareClassificationWidget(int iX, int iY, Vector2f kTMin, Vector2f kTMax, Texture kTexture, int iWidth, int iHeight)
    {
        super( kTMin, kTMax, iWidth, iHeight );
        CreateSquare( iX, iY, kTexture );
    }

    /**
     * Copy constructor.
     * @param kWidget
     */
    public SquareClassificationWidget ( SquareClassificationWidget kWidget )
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
        m_kMiddleSphere.Local.SetTranslate( new Vector3f( kWidget.m_kMiddleSphere.Local.GetTranslate()) );
        m_kLowerSphere = new TriMesh(new VertexBuffer(kWidget.m_kLowerSphere.VBuffer),
                new IndexBuffer(kWidget.m_kLowerSphere.IBuffer));
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
		// check the mouse world coordinates with the square world coordinates:
		if ( (fX >= m_kWidgetMesh.VBuffer.GetPosition3fX(0)) && (fX <= m_kWidgetMesh.VBuffer.GetPosition3fX(1)) &&
				(fY >= m_kWidgetMesh.VBuffer.GetPosition3fY(0)) && (fY <= m_kWidgetMesh.VBuffer.GetPosition3fY(2)) )
		{
			m_kPicked = m_kWidgetMesh;
			bPicked = true;
		}
		return super.Pick(iX,iY,bPicked);
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
            	// If the square is picked, translate it:
                ShiftSquare( e );
            }
            else if ( (m_kPicked == m_kLowerSphere) || (m_kPicked == m_kUpperSphere) )
            {
            	// if one of the corner control-points is picked, scale the square:
                ScaleRectangle( e, (m_kPicked == m_kLowerSphere) );
            }
            else if ( m_kPicked == m_kMiddleSphere )
            {
            	// the middle control-point was picked, so shift the mid-line:
                ShiftMid(e);
            }
        }
    }
	

	public void setTexture( Texture kTexture )
	{
		if ( m_kWidgetMesh != null )
		{
			m_kWidgetEfect = new ClassificationWidgetEffect( kTexture, ClassificationWidgetState.Square );
			m_kWidgetMesh.AttachEffect( m_kWidgetEfect );
		}
	}
	
	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidget#updateDisplay()
	 */
	public void updateDisplay()
    {
		// Update the ClassificationWidgetEffect based on the position of the square and the mid-line control-point.
        if ( m_kWidgetEfect != null )
        {
            Vector3f kMidLine = m_kMiddleSphere.Local.GetTranslate();
            // set mid-line in texture coordinates:
            //m_kWidgetEfect.SetMidLine( calcTCoordX(fX1), calcTCoordY(fY1), calcTCoordX(fX1), calcTCoordY(fY1) );
            m_kWidgetEfect.SetMidLine( calcTCoordX(kMidLine.X), m_kWidgetMesh.VBuffer.GetTCoord2fY(0,0), calcTCoordX(kMidLine.X), m_kWidgetMesh.VBuffer.GetTCoord2fY(0,3) );

            float fLeftX1 = m_kWidgetMesh.VBuffer.GetTCoord2fX(0,0);
            float fLeftY1 = m_kWidgetMesh.VBuffer.GetTCoord2fY(0,0);
            float fLeftX2 = m_kWidgetMesh.VBuffer.GetTCoord2fX(0,3);
            float fLeftY2 = m_kWidgetMesh.VBuffer.GetTCoord2fY(0,3);
            // set left line in texture coordinates:
            m_kWidgetEfect.SetLeftLine( fLeftX1, fLeftY1, fLeftX2, fLeftY2 );

            float fRightX1 = m_kWidgetMesh.VBuffer.GetTCoord2fX(0,1);
            float fRightY1 = m_kWidgetMesh.VBuffer.GetTCoord2fY(0,1);
            float fRightX2 = m_kWidgetMesh.VBuffer.GetTCoord2fX(0,2);
            float fRightY2 = m_kWidgetMesh.VBuffer.GetTCoord2fY(0,2);
            // set right-line in texture coordinates:
            m_kWidgetEfect.SetRightLine( fRightX1, fRightY1, fRightX2, fRightY2 );


            float fMidX = calcTCoordX(kMidLine.X);
            float fMidY = calcTCoordX(kMidLine.Y);
            float fIncr = (fMidY - fLeftY1) /  (fLeftY2 - fLeftY1);
            fIncr = fIncr * (fRightX1 - fLeftX1);
            float fShiftX = (fMidX - fLeftX1) / (fRightX1 - fLeftX1);
            m_kWidgetEfect.SetShift( fShiftX*fIncr, (1.0f-fShiftX)*fIncr );

            m_kWidgetEfect.UpdateColor();
            m_kWidgetEfect.computeUniformVariables();
        }
    }

    /**
     * Creates the square widget and control points:
     * @param iX mouse x-position in MouseEvent coordinates.
     * @param iY mouse y-position in MouseEvent coordinates.
     * @param kTexName 2D Histogram texture name.
     */
    protected void CreateSquare(int iX, int iY, Texture kTexture)
    {
    	// calculate the position in world coordinates:
    	float fX = calcObjX(iX);
    	float fY = calcObjY(iY);
    	// the size of the square:
        float fSize = .2f;
        // create the TriMesh Attributes: position, color, texture:
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);
        kAttributes.SetTChannels(0,2);
        IndexBuffer kIBuffer = new IndexBuffer(6);
        int[] aiIndexData = kIBuffer.GetData();
        aiIndexData[0] = 0;
        aiIndexData[1] = 1;
        aiIndexData[2] = 2;
        aiIndexData[3] = 0;
        aiIndexData[4] = 2;
        aiIndexData[5] = 3;        
        // Set up the VertexBuffer positions, colors, texture coordinates:
        VertexBuffer kVBuffer = new VertexBuffer(kAttributes, 4 );
        kVBuffer.SetPosition3(0, fX-fSize, fY-fSize, 0.1f);
        kVBuffer.SetColor3( 0, 0, 1.0f, 0.01f, 0.01f );
        kVBuffer.SetTCoord2(0, 0, calcTCoordX(fX-fSize), calcTCoordY(fY-fSize));

        kVBuffer.SetPosition3(1, fX+fSize, fY-fSize, 0.1f);
        kVBuffer.SetColor3( 0, 1, 1.0f, 0.01f, 0.01f  );
        kVBuffer.SetTCoord2(0, 1, calcTCoordX(fX+fSize), calcTCoordY(fY-fSize));

        kVBuffer.SetPosition3(2, fX+fSize, fY+fSize, 0.1f);
        kVBuffer.SetColor3( 0, 2, 0f, 0f, 1f  );
        kVBuffer.SetTCoord2(0, 2, calcTCoordX(fX+fSize), calcTCoordY(fY+fSize));

        kVBuffer.SetPosition3(3, fX-fSize, fY+fSize, 0.1f);
        kVBuffer.SetColor3( 0, 3, 1.0f, 0.01f, 0.01f  );
        kVBuffer.SetTCoord2(0, 3, calcTCoordX(fX-fSize), calcTCoordY(fY+fSize));
        
        // new square TriMesh:
        m_kWidgetMesh = new TriMesh( kVBuffer, kIBuffer );
        
        // Create the ClassificationWidgetEffect, pass in the texture name:
        m_kWidgetEfect = new ClassificationWidgetEffect( kTexture, ClassificationWidgetState.Square );
        m_kWidgetMesh.AttachEffect( m_kWidgetEfect );
        m_kWidgetMesh.SetName("BottomTri");
        m_kWidget.AttachChild(m_kWidgetMesh);
        
        // Outline for the square, uses the same VertexBuffer as the square:
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
        	// set the color:
            m_kUpperSphere.VBuffer.SetColor3(0, i, 0f, 0f, 1f);
        }
        m_kUpperSphere.AttachEffect( new VertexColor3Effect() );
        m_kUpperSphere.SetName("UpperSphere");
        m_kWidget.AttachChild( m_kUpperSphere );
        // move the sphere to the upper-right corner of the square widget:
        m_kUpperSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(2));
        

        m_kLowerSphere = kSM.Sphere(10,10,SPHERE_RADIUS);
        for ( int i = 0; i < m_kLowerSphere.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kLowerSphere.VBuffer.SetColor3(0, i, 0f, 0f, 1f);
        }
        m_kLowerSphere.AttachEffect( new VertexColor3Effect() );
        m_kLowerSphere.SetName("LowerSphere");
        m_kWidget.AttachChild( m_kLowerSphere );
        // move the sphere to the lower-right corner of the square widget:
        m_kLowerSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(1));

        m_kMiddleSphere = kSM.Sphere(10,10,SPHERE_RADIUS);
        for ( int i = 0; i < m_kMiddleSphere.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kMiddleSphere.VBuffer.SetColor3(0, i, 0f, 1f, 0f);
        }
        m_kMiddleSphere.AttachEffect( new VertexColor3Effect() );
        m_kMiddleSphere.SetName("MiddleSphere");
        m_kWidget.AttachChild( m_kMiddleSphere );

        // position the middle control-point in the center:
        float fXPos = (m_kWidgetMesh.VBuffer.GetPosition3fX(0) + m_kWidgetMesh.VBuffer.GetPosition3fX(1))/2.0f;
        float fYPos = (m_kWidgetMesh.VBuffer.GetPosition3fY(0) + m_kWidgetMesh.VBuffer.GetPosition3fY(2))/2.0f;
        float fZPos = m_kWidgetMesh.VBuffer.GetPosition3fZ(0);
        m_kMiddleSphere.Local.SetTranslate( fXPos, fYPos, fZPos );
        
        m_kWidget.UpdateGS();
    }
    
	/**
     * Scale the rectangle, based on the control points
     * @param e MouseEvent position
     * @param bLower when true the lower control point is being dragged, false when the upper control-point is dragged.
     */
    protected void ScaleRectangle(MouseEvent e, boolean bLower)
    {
    	// Calculate the position in world-coordinates:
    	float fX = calcObjX(e.getX()); 
    	float fY = calcObjY(e.getY());
    	// the x-position is clamped to the left-edge of the square and RIGHT_EDGE:
    	fX = Math.max( fX, m_kWidgetMesh.VBuffer.GetPosition3fX(0) );
    	fX = Math.min( fX, RIGHT_EDGE );
    	// Calculate the center and difference:
        float centerX = (m_kWidgetMesh.VBuffer.GetPosition3fX( 0 ) + m_kWidgetMesh.VBuffer.GetPosition3fX( 1 ))/2f;
        float centerY = (m_kWidgetMesh.VBuffer.GetPosition3fY( 0 ) + m_kWidgetMesh.VBuffer.GetPosition3fY( 2 ))/2f;
        float diffX = fX - centerX;
        float diffY = fY - centerY;
    	if ( bLower )
    	{
    		// if the lower control-point is moved clamp to the top of the square and the
    		// BOTTOM_EDGE of the world:
        	fY = Math.max( fY, BOTTOM_EDGE );
        	fY = Math.min( fY, m_kWidgetMesh.VBuffer.GetPosition3fY(2) );
        	diffY = centerY - fY;
    	}
        else
        {
        	// if the upper control-point is moved, clamp to the bottom of the square
        	// and the TOP_EDGE of the world:
        	fY = Math.min( fY, TOP_EDGE );
        	fY = Math.max( fY, m_kWidgetMesh.VBuffer.GetPosition3fY(0) );
        	diffY = fY - centerY;
        }
    	// The square is expanded equally in all directions:
    	// Check that the expansion doesn't put the square edges out of bounds
    	// and clamp if necessary:
        if ( centerX - diffX < LEFT_EDGE )
        {
        	diffX = centerX - LEFT_EDGE;
        }
        if ( centerX + diffX > RIGHT_EDGE )
        {
        	diffX = RIGHT_EDGE - centerX;
        }
        if ( centerY - diffY < BOTTOM_EDGE )
        {
        	diffY = centerY - BOTTOM_EDGE;
        }
        if ( centerY + diffY > TOP_EDGE )
        {
        	diffY = TOP_EDGE - centerY;
        }

        // re-scale the square positions and texture coordinates:
        Vector3f kPos = m_kWidgetMesh.VBuffer.GetPosition3(0);
		kPos.X = centerX - diffX;
		kPos.Y = centerY - diffY;
		m_kWidgetMesh.VBuffer.SetPosition3(0, kPos);
		m_kWidgetMesh.VBuffer.SetTCoord2(0, 0, calcTCoordX(kPos.X), calcTCoordY(kPos.Y));

		kPos.X = centerX + diffX;
		kPos.Y = centerY - diffY;
		m_kWidgetMesh.VBuffer.SetPosition3(1, kPos);
		m_kWidgetMesh.VBuffer.SetTCoord2(0, 1, calcTCoordX(kPos.X), calcTCoordY(kPos.Y));
		
		kPos.X = centerX + diffX;
		kPos.Y = centerY + diffY;
		m_kWidgetMesh.VBuffer.SetPosition3(2, kPos);
		m_kWidgetMesh.VBuffer.SetTCoord2(0, 2, calcTCoordX(kPos.X), calcTCoordY(kPos.Y));
		
		kPos.X = centerX - diffX;
		kPos.Y = centerY + diffY;
		m_kWidgetMesh.VBuffer.SetPosition3(3, kPos);
		m_kWidgetMesh.VBuffer.SetTCoord2(0, 3, calcTCoordX(kPos.X), calcTCoordY(kPos.Y));
        m_kWidgetMesh.VBuffer.Release();
		
        // Move the upper and lower spheres based on the square corners:
        m_kUpperSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(2));
        m_kLowerSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(1));

        // reposition the center sphere based on the stored relative position
        // using the parameterized coordinates:
        fX = m_kWidgetMesh.VBuffer.GetPosition3fX(0) + m_fCenterX * 
        (m_kWidgetMesh.VBuffer.GetPosition3fX(1) - m_kWidgetMesh.VBuffer.GetPosition3fX(0));
        fY = m_kWidgetMesh.VBuffer.GetPosition3fY(0) + m_fCenterY * 
        (m_kWidgetMesh.VBuffer.GetPosition3fY(2) - m_kWidgetMesh.VBuffer.GetPosition3fY(0));
        Vector3f kCenter = m_kMiddleSphere.Local.GetTranslate();
        kCenter.X = fX;
        kCenter.Y = fY;
        
        // update the scene graph:
        m_kWidget.UpdateGS();
    }
    
    /**
     * Translate the square in the 2D Histogram panel.
     * @param e MouseEvent
     */
    protected void ShiftSquare( MouseEvent e )
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
        float fDiffY = fNewGCY - kCurrentCenter.Y;

        // X check that moving the square doesn't move it out of bounds,
        // clamp if necessary:
        float fNewXL = m_kWidgetMesh.VBuffer.GetPosition3fX(0);
        float fNewXR = m_kWidgetMesh.VBuffer.GetPosition3fX(1);
        if ( fNewXR + fDiffX > RIGHT_EDGE )
        {
        	fDiffX = RIGHT_EDGE - fNewXR;
        }
        if ( fNewXL + fDiffX < LEFT_EDGE )
        {
        	fDiffX = LEFT_EDGE - fNewXL;
        }

        // Y check that moving the square doesn't move it out of bounds,
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
        m_kUpperSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(2));
        m_kLowerSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(1));

        // Move the center sphere by translating it the same amount as the square:
        Vector3f kTranslate = m_kMiddleSphere.Local.GetTranslate(); 
        m_kMiddleSphere.Local.SetTranslate( kTranslate.X + fDiffX, kTranslate.Y + fDiffY, kTranslate.Z );
        
        // Update the scene-graph
        m_kWidget.UpdateGS();
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
		IndexBuffer kIBuffer = (IndexBuffer)in.readObject();
		VertexBuffer kVBuffer = (VertexBuffer)in.readObject();
        m_kLowerSphere = new TriMesh( kVBuffer, kIBuffer );
        m_kLowerSphere.AttachEffect( new VertexColor3Effect() );
        m_kLowerSphere.SetName("LowerSphere");
        m_kWidget.AttachChild( m_kLowerSphere );
        m_kLowerSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(1));		
		
		m_fCenterX = in.readFloat();
		m_fCenterY = in.readFloat();		
        
        // reposition mid-line control-point:
        float fX = m_kWidgetMesh.VBuffer.GetPosition3fX(0) + m_fCenterX * 
        (m_kWidgetMesh.VBuffer.GetPosition3fX(1) - m_kWidgetMesh.VBuffer.GetPosition3fX(0));
        float fY = m_kWidgetMesh.VBuffer.GetPosition3fY(0) + m_fCenterY * 
        (m_kWidgetMesh.VBuffer.GetPosition3fY(2) - m_kWidgetMesh.VBuffer.GetPosition3fY(0));

        Vector3f kCenter = m_kMiddleSphere.Local.GetTranslate();
        kCenter.X = fX;
        kCenter.Y = fY;
        kCenter.Z = 0.1f;
        
        // update the scene graph:
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
    	// clamp the new position to the square edges:
        fX = Math.max( fX, m_kWidgetMesh.VBuffer.GetPosition3fX(0) );
        fX = Math.min( fX, m_kWidgetMesh.VBuffer.GetPosition3fX(1) );
        fY = Math.max( fY, m_kWidgetMesh.VBuffer.GetPosition3fY(0) );
        fY = Math.min( fY, m_kWidgetMesh.VBuffer.GetPosition3fY(2) );
        // set the center of the control-point sphere:
        kCenter.X = fX;
        kCenter.Y = fY;
        // update the parameterized value of the control-point location for repositioning after scaling.
        m_fCenterX = (fX - m_kWidgetMesh.VBuffer.GetPosition3fX(0)) / 
        (m_kWidgetMesh.VBuffer.GetPosition3fX(1) - m_kWidgetMesh.VBuffer.GetPosition3fX(0));
        m_fCenterY = (fY - m_kWidgetMesh.VBuffer.GetPosition3fY(0)) / 
        (m_kWidgetMesh.VBuffer.GetPosition3fY(2) - m_kWidgetMesh.VBuffer.GetPosition3fY(0));
        // update the scene graph:
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
		
		out.writeFloat(m_fCenterX);
		out.writeFloat(m_fCenterY);
	}   
    
}
