
package gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer;

import java.awt.event.MouseEvent;
import java.io.IOException;

import WildMagic.LibFoundation.Mathematics.Mathf;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

/**
 */
public class CircleClassificationWidget extends ClassificationWidget
{

	private static final long serialVersionUID = -4180814068815939724L;

	private Vector2f m_kCenterDir = new Vector2f(0,0);
	private float m_fCenterScale = 0f;
	

    private float m_fRadiusX = 0.5f;
    private float m_fRadiusY = 0.5f;  


	//private Vector2f m_kLowerDir = new Vector2f(1,0);
	//private float m_fLowerScale = .5f;
	//protected TriMesh[] m_kIntersectSphere = new TriMesh[2];

	/**
	 * @param iX location in MouseEvent Coordinates.
	 * @param iY location in MouseEvent Coordinates.
	 * @param kTMin minimum texture coordinates for the 2D histogram, used for the image background. (Defaults 0-1)
	 * @param kTMax maximum texture coordinates for the 2D histogram, used for the image background. (Defaults 0-1)
	 * @param kTexName 2D Histogram texture name.
	 * @param iWidth canvas width (default 256)
	 * @param iHeight canvas height (default 256)
	 */
	public CircleClassificationWidget(int iX, int iY, Vector2f kTMin, Vector2f kTMax, String kTexName, int iWidth, int iHeight)
    {
        super( kTMin, kTMax, iWidth, iHeight );
        CreateCircle( iX, iY, kTexName );
    }

    /**
     * Copy constructor.
     * @param kWidget
     */
    public CircleClassificationWidget ( CircleClassificationWidget kWidget )
    {
        super(kWidget);

        m_kWidgetMesh = new TriMesh(new VertexBuffer(kWidget.m_kWidgetMesh.VBuffer),
                new IndexBuffer(kWidget.m_kWidgetMesh.IBuffer));
        m_kOutline = new Polyline( m_kWidgetMesh.VBuffer, true, true );
        
        m_kWidgetEfect = new ClassificationWidgetEffect( kWidget.m_kWidgetEfect ); 

        m_kMiddleSphere = new TriMesh(new VertexBuffer(kWidget.m_kMiddleSphere.VBuffer),
                new IndexBuffer(kWidget.m_kMiddleSphere.IBuffer));
        m_kMiddleSphere.Local.SetTranslate( new Vector3f( kWidget.m_kMiddleSphere.Local.GetTranslate()) );
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
		m_kWidgetMesh.VBuffer.Release();
		m_kOutline.VBuffer.Release();
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
		
		float fCenterX = m_kWidgetMesh.VBuffer.GetPosition3fX(0);
		float fCenterY = m_kWidgetMesh.VBuffer.GetPosition3fY(0);
		

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
                ShiftCircle( e );
            }
            else if ( (m_kPicked == m_kUpperSphere) )
            {
            	// if one of the corner control-points is picked, scale the square:
            	ScaleCircle( e );
            }
            else if ( m_kPicked == m_kMiddleSphere )
            {
            	// the middle control-point was picked, so shift the mid-line:
                ShiftMid(e);
            }
            /*
            else if ( m_kPicked == m_kLowerSphere )
            {
            	// the middle control-point was picked, so shift the mid-line:
                ShiftLower(e);
            }
            */
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
            float fX1 = kMidLine.X;
            float fY1 = kMidLine.Y;
            // set mid-line in texture coordinates:
            m_kWidgetEfect.SetMidLine( calcTCoordX(fX1), calcTCoordY(fY1), calcTCoordX(fX1), calcTCoordY(fY1) );
            
            Vector2f center = getCenter();
            m_kWidgetEfect.SetCenter( calcTCoordX(center.X), calcTCoordY(center.Y) );
            
            m_kWidgetEfect.SetRadius( m_fRadiusX * m_kTMax.X / 2f, m_fRadiusY * m_kTMax.Y / 2f );

            m_kWidgetEfect.UpdateColor();
            m_kWidgetEfect.UpdateLUT();
        }
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

    	m_kCenterDir.X = diffX / length;
    	m_kCenterDir.Y = diffY / length;
    	m_fCenterScale = length / edgeLength;

        //calcIntersection();
    	// update the scene graph:
        m_kWidget.UpdateGS();
    }

    /*
    private void ShiftLower( MouseEvent e )
    {      
    	// calculate the coordinates in world space:
    	float fX = calcObjX(e.getX()); 
    	float fY = calcObjY(e.getY()); 
    	Vector3f kCenter = m_kLowerSphere.Local.GetTranslate();
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

    	m_kLowerDir.X = diffX / length;
    	m_kLowerDir.Y = diffY / length;
    	m_fLowerScale = length / edgeLength;

        calcIntersection();
    	// update the scene graph:
        m_kWidget.UpdateGS();
    }
    */
    
    /**
     * Creates the circle widget and control point:
     * @param iX mouse x-position in MouseEvent coordinates.
     * @param iY mouse y-position in MouseEvent coordinates.
     * @param kTexName 2D Histogram texture name.
     */
    protected void CreateCircle(int iX, int iY, String kTexName)
    {
    	// calculate the position in world coordinates:
    	float fX = calcObjX(iX);
    	float fY = calcObjY(iY);
    	
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
        m_kWidgetEfect = new ClassificationWidgetEffect( kTexName, true );
        m_kWidgetMesh.AttachEffect( m_kWidgetEfect );
        m_kWidgetMesh.SetName("BottomTri");
        m_kWidget.AttachChild(m_kWidgetMesh);
        
        // Outline for the square, uses the same VertexBuffer as the square:
        m_kOutline = new Polyline( kVBuffer, true, true );
        m_kOutline.AttachEffect( new VertexColor3Effect() );
        m_kWidget.AttachChild(m_kOutline);
        
        
        // Attributes for the sphere control points:
        kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);
        StandardMesh kSM = new StandardMesh(kAttributes);

        m_kUpperSphere = kSM.Sphere(10,10,SPHERE_RADIUS);
        for ( int i = 0; i < m_kUpperSphere.VBuffer.GetVertexQuantity(); i++ )
        {
        	m_kUpperSphere.VBuffer.SetColor3(0, i, 0f, 0f, 1f);
        }
        m_kUpperSphere.AttachEffect( new VertexColor3Effect() );
        m_kUpperSphere.SetName("UpperSphere");
        m_kWidget.AttachChild( m_kUpperSphere );
        m_kUpperSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(1));

        
        m_kMiddleSphere = kSM.Sphere(10,10,SPHERE_RADIUS);
        for ( int i = 0; i < m_kMiddleSphere.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kMiddleSphere.VBuffer.SetColor3(0, i, 0f, 1f, 0f);
        }
        m_kMiddleSphere.AttachEffect( new VertexColor3Effect() );
        m_kMiddleSphere.SetName("MiddleSphere");
        m_kWidget.AttachChild( m_kMiddleSphere );

        // position the middle control-point in the center:
        m_kMiddleSphere.Local.SetTranslate( m_kWidgetMesh.VBuffer.GetPosition3(0) );
        
        /*
        m_kLowerSphere = kSM.Sphere(10,10,SPHERE_RADIUS);
        for ( int i = 0; i < m_kLowerSphere.VBuffer.GetVertexQuantity(); i++ )
        {
        	m_kLowerSphere.VBuffer.SetColor3(0, i, 1f, 1f, 0f);
        }
        m_kLowerSphere.AttachEffect( new VertexColor3Effect() );
        m_kLowerSphere.SetName("LowerSphere");
        m_kWidget.AttachChild( m_kLowerSphere );

        // position the lower control-point in the center:
        Vector3f kPos = new Vector3f( m_kWidgetMesh.VBuffer.GetPosition3(0) );
        kPos.Add( m_kWidgetMesh.VBuffer.GetPosition3(1) );
        kPos.Scale(.5f);
        m_kLowerSphere.Local.SetTranslate( kPos );
        */
        

        /*
        m_kIntersectSphere[0] = kSM.Sphere(10,10,SPHERE_RADIUS);
        for ( int i = 0; i < m_kIntersectSphere[0].VBuffer.GetVertexQuantity(); i++ )
        {
        	m_kIntersectSphere[0].VBuffer.SetColor3(0, i, 1f, 0f, 0f);
        }
        m_kIntersectSphere[0].AttachEffect( new VertexColor3Effect() );
        m_kIntersectSphere[0].SetName("i0");
        m_kWidget.AttachChild( m_kIntersectSphere[0] );
        
        m_kIntersectSphere[1] = kSM.Sphere(10,10,SPHERE_RADIUS);
        for ( int i = 0; i < m_kIntersectSphere[1].VBuffer.GetVertexQuantity(); i++ )
        {
        	m_kIntersectSphere[1].VBuffer.SetColor3(0, i, 1f, 0f, 1f);
        }
        m_kIntersectSphere[1].AttachEffect( new VertexColor3Effect() );
        m_kIntersectSphere[1].SetName("i1");
        m_kWidget.AttachChild( m_kIntersectSphere[1] );
        calcIntersection();
        */
        m_kWidget.UpdateGS();
    }
    
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
    	// get the current center of the square:
    	Vector2f kCurrentCenter = getCenter();
        float diffX = fX - kCurrentCenter.X;
        float diffY = fY - kCurrentCenter.Y;
        

		m_fRadiusX = Math.abs(diffX);
		m_fRadiusY = Math.abs(diffY);
        

        float fInvRS = 1.0f/64f;
        float x,y,z;
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
		m_kWidgetMesh.VBuffer.Release();
		m_kOutline.VBuffer.Release();

        m_kUpperSphere.Local.SetTranslate( kCurrentCenter.X + newX, kCurrentCenter.Y + newY, z );
		

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
/*
        diffX = m_kLowerDir.X;
        diffY = m_kLowerDir.Y;    	  	
        if ( diffX != 0 && diffY != 0 )
        {
        	scale = (float)((m_fRadiusX * m_fRadiusY) / Math.sqrt( m_fRadiusY*m_fRadiusY * diffX*diffX + m_fRadiusX*m_fRadiusX * diffY*diffY ));
        	newX = diffX * scale;
        	newY = diffY * scale;

        	float edgeLength = (float)Math.sqrt( newX*newX + newY*newY );
        	m_kLowerSphere.Local.SetTranslate( kCurrentCenter.X + m_kLowerDir.X * m_fLowerScale * edgeLength, 
        			kCurrentCenter.Y + m_kLowerDir.Y * m_fLowerScale * edgeLength, z );

        }       
        calcIntersection();
        */
        // update the scene graph:
        m_kWidget.UpdateGS();
    }
    

	protected Vector2f getCenter()
	{
		float fX = m_kWidgetMesh.VBuffer.GetPosition3fX(0);
		float fY = m_kWidgetMesh.VBuffer.GetPosition3fY(0);
		return new Vector2f( fX, fY );
	}

    /**
     * Translate the circle in the 2D Histogram panel.
     * @param e MouseEvent
     */
    protected void ShiftCircle( MouseEvent e )
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

        if ( kCurrentCenter.X + m_fRadiusX + fDiffX > RIGHT_EDGE )
        {
        	fDiffX = RIGHT_EDGE - (kCurrentCenter.X + m_fRadiusX);
        }
        if ( kCurrentCenter.X - m_fRadiusX + fDiffX < LEFT_EDGE )
        {
        	fDiffX = LEFT_EDGE - (kCurrentCenter.X - m_fRadiusX);
        }

        // Y check that moving the square doesn't move it out of bounds,
        // clamp if necessary:
        if ( kCurrentCenter.Y - m_fRadiusY + fDiffY  < BOTTOM_EDGE )
        {
        	fDiffY = BOTTOM_EDGE - (kCurrentCenter.Y - m_fRadiusY);
        }
        if ( kCurrentCenter.Y + m_fRadiusY + fDiffY > TOP_EDGE )
        {
        	fDiffY = TOP_EDGE - (kCurrentCenter.Y + m_fRadiusY);
        }
        
        // Move the square: position and texture coordinates:
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
        m_kWidgetMesh.VBuffer.Release();
        m_kOutline.VBuffer.Release();

        // Move the center sphere by translating it the same amount as the square:
        Vector3f kTranslate = m_kMiddleSphere.Local.GetTranslate(); 
        m_kMiddleSphere.Local.SetTranslate( kTranslate.X + fDiffX, kTranslate.Y + fDiffY, kTranslate.Z );

        // Move the upper sphere by translating it the same amount as the square:
        kTranslate = m_kUpperSphere.Local.GetTranslate(); 
        m_kUpperSphere.Local.SetTranslate( kTranslate.X + fDiffX, kTranslate.Y + fDiffY, kTranslate.Z );

        /*
        // Move the upper sphere by translating it the same amount as the square:
        kTranslate = m_kLowerSphere.Local.GetTranslate(); 
        m_kLowerSphere.Local.SetTranslate( kTranslate.X + fDiffX, kTranslate.Y + fDiffY, kTranslate.Z );
        */
/*
        kTranslate = m_kIntersectSphere[0].Local.GetTranslate(); 
        m_kIntersectSphere[0].Local.SetTranslate( kTranslate.X + fDiffX, kTranslate.Y + fDiffY, kTranslate.Z );
        kTranslate = m_kIntersectSphere[1].Local.GetTranslate(); 
        m_kIntersectSphere[1].Local.SetTranslate( kTranslate.X + fDiffX, kTranslate.Y + fDiffY, kTranslate.Z );
        */
        // Update the scene-graph
        m_kWidget.UpdateGS();
    }   
    /*
    private void calcIntersection()
    {
    	float z = 0.1f;

    	Vector3f kMiddle = m_kMiddleSphere.Local.GetTranslate();
    	Vector3f kLower = m_kLowerSphere.Local.GetTranslate();
    	// get the current center of the circle:
    	Vector2f kCurrentCenter = getCenter();

    	Vector2f p0 = new Vector2f( kMiddle.X - kCurrentCenter.X, kMiddle.Y - kCurrentCenter.Y );
    	Vector2f p1 = new Vector2f( kLower.X - kCurrentCenter.X, kLower.Y - kCurrentCenter.Y );
    	
    	float b = m_fRadiusY;
    	float a = m_fRadiusX;
    	float slope = (p1.Y - p0.Y) / (p1.X - p0.X);
    	float intercept = p1.Y - slope * p1.X;
    	float A = b*b + a*a*slope*slope;
    	float B = 2*a*a*intercept*slope;
    	float C = a*a*intercept*intercept - b*b*a*a;
    	float r = B*B - 4*A*C;
    	if ( r >= 0 )
    	{
    		
			// solve for x values - using the quadratic equation
			float x3 = (float)(-B-Math.sqrt(r))/(2*A);
			float x4 = (float)(-B+Math.sqrt(r))/(2*A);
			// calculate y, since we know it's on the line at that point (otherwise there would be no intersection)
			float y3 = slope*x3+intercept;
			float y4 = slope*x4+intercept;				

			m_kIntersectSphere[0].Local.SetTranslate( kCurrentCenter.X + x3, kCurrentCenter.Y + y3, z );
			m_kIntersectSphere[1].Local.SetTranslate( kCurrentCenter.X + x4, kCurrentCenter.Y + y4, z );
    		
    	}
    	else
    	{
    		System.err.println( "One intersection" );
    		float x3 = (float)(-B-Math.sqrt(r))/(2*A);	
			float y3 = slope*x3+intercept;
			m_kIntersectSphere[0].Local.SetTranslate( kCurrentCenter.X + x3, kCurrentCenter.Y + y3, z );
			m_kIntersectSphere[1].Local.SetTranslate( kCurrentCenter.X + x3, kCurrentCenter.Y + y3, z );    		
    	}
    }
    */
}
