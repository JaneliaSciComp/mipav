
package gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer;

import java.awt.event.MouseEvent;
import java.io.IOException;

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

public class SquareClassificationWidget extends ClassificationWidget
{

	private static final long serialVersionUID = -4180814068815939724L;

	private float m_fCenterX = 0.5f;
	private float m_fCenterY = 0.5f;
	
	public SquareClassificationWidget(float fX, float fY, Vector2f kTMin, Vector2f kTMax, String kTexName, int iWidth, int iHeight)
    {
        super( kTMin, kTMax, iWidth, iHeight );
        CreateSquare( fX, fY, kTexName );
    }

    public SquareClassificationWidget ( SquareClassificationWidget kWidget )
    {
        super(kWidget);

        m_kBottomTri = new TriMesh(new VertexBuffer(kWidget.m_kBottomTri.VBuffer),
                new IndexBuffer(kWidget.m_kBottomTri.IBuffer));
        m_kOutline = new Polyline( m_kBottomTri.VBuffer, true, true );
        
        m_kBottomTriEffect = new ClassificationWidgetEffect( kWidget.m_kBottomTriEffect ); 

        m_kUpperSphere = new TriMesh(new VertexBuffer(kWidget.m_kUpperSphere.VBuffer),
                new IndexBuffer(kWidget.m_kUpperSphere.IBuffer));
        m_kMiddleSphere = new TriMesh(new VertexBuffer(kWidget.m_kMiddleSphere.VBuffer),
                new IndexBuffer(kWidget.m_kMiddleSphere.IBuffer));
        m_kMiddleSphere.Local.SetTranslate( new Vector3f( kWidget.m_kMiddleSphere.Local.GetTranslate()) );
        m_kLowerSphere = new TriMesh(new VertexBuffer(kWidget.m_kLowerSphere.VBuffer),
                new IndexBuffer(kWidget.m_kLowerSphere.IBuffer));
    }
    
	public boolean Pick( int iX, int iY )
	{
		boolean bPicked = false;
		m_kPicked = null;
		float fX = calcObjX(iX);
		float fY = calcObjY(iY);
		if ( (fX >= m_kBottomTri.VBuffer.GetPosition3fX(0)) && (fX <= m_kBottomTri.VBuffer.GetPosition3fX(1)) &&
				(fY >= m_kBottomTri.VBuffer.GetPosition3fY(0)) && (fY <= m_kBottomTri.VBuffer.GetPosition3fY(2)) )
		{
			m_kPicked = m_kBottomTri;
			//System.err.println( "Picked Square" );
			bPicked = true;
		}
		return super.Pick(iX,iY,bPicked);
	}
    
    @Override
	public boolean processMouseDrag(int iX0ld, int iYOld, int iButton, MouseEvent e ) 
    {        
        if ( iButton == MouseEvent.BUTTON1 )
        {
            if ( m_kPicked == m_kBottomTri )
            {
                ShiftSquare( e );
            }
            else if ( (m_kPicked == m_kLowerSphere) || (m_kPicked == m_kUpperSphere) )
            {
                ScaleRectangle( e, (m_kPicked == m_kLowerSphere) );
            }
            else if ( m_kPicked == m_kMiddleSphere )
            {
                ShiftMid(e);
            }
            return false;
        }
        return false;
    }
/*
    public void ScaleRectangle( TriMesh kShape, TriMesh kOutline )
    {
        float fThickness = 1.0f/m_iWidth;
        fThickness *= 2;
        Vector3f kPos0 = kOutline.VBuffer.GetPosition3(0);
        Vector3f kPos1 = kOutline.VBuffer.GetPosition3(1);
        Vector3f kPos2 = kOutline.VBuffer.GetPosition3(2);
        Vector3f kPos3 = kOutline.VBuffer.GetPosition3(3);

        float fNewX = kPos0.X + fThickness;
        float fNewY = kPos0.Y + fThickness;
        kShape.VBuffer.SetPosition3(0, fNewX, fNewY, 0.11f);
        float fTX = fNewX;
        float fTY = fNewY;
        fTX = m_kTMin.X + fTX * (m_kTMax.X - m_kTMin.X);
        fTY = m_kTMin.Y + fTY * (m_kTMax.Y - m_kTMin.Y);
        kShape.VBuffer.SetTCoord2(0, 0, fTX, fTY);

        fNewX = kPos1.X - fThickness;
        fNewY = kPos1.Y + fThickness;
        kShape.VBuffer.SetPosition3(1, fNewX, fNewY, 0.11f);
        fTX = fNewX;
        fTY = fNewY;
        fTX = m_kTMin.X + fTX * (m_kTMax.X - m_kTMin.X);
        fTY = m_kTMin.Y + fTY * (m_kTMax.Y - m_kTMin.Y);
        kShape.VBuffer.SetTCoord2(0, 1, fTX, fTY);

        fNewX = kPos2.X + fThickness;
        fNewY = kPos2.Y - fThickness;
        kShape.VBuffer.SetPosition3(2, fNewX, fNewY, 0.11f);
        fTX = fNewX;
        fTY = fNewY;
        fTX = m_kTMin.X + fTX * (m_kTMax.X - m_kTMin.X);
        fTY = m_kTMin.Y + fTY * (m_kTMax.Y - m_kTMin.Y);
        kShape.VBuffer.SetTCoord2(0, 2, fTX, fTY);

        fNewX = kPos3.X - fThickness;
        fNewY = kPos3.Y - fThickness;
        kShape.VBuffer.SetPosition3(3, fNewX, fNewY, 0.11f);
        fTX = fNewX;
        fTY = fNewY;
        fTX = m_kTMin.X + fTX * (m_kTMax.X - m_kTMin.X);
        fTY = m_kTMin.Y + fTY * (m_kTMax.Y - m_kTMin.Y);
        kShape.VBuffer.SetTCoord2(0, 3, fTX, fTY);

        m_kWidget.UpdateGS();
    }
*/

    @Override
	public void updateDisplay()
    {
        if ( m_kBottomTriEffect != null )
        {
            Vector3f kMidLine = m_kMiddleSphere.Local.GetTranslate();
            float fX1 = kMidLine.X;
            float fY1 = kMidLine.Y;
            float fX2 = fX1;
            float fY2 = fY1;
            m_kBottomTriEffect.SetMidLine( calcTCoordX(fX1), calcTCoordY(fY1), calcTCoordX(fX1), calcTCoordY(fY1) );

            fX1 = m_kBottomTri.VBuffer.GetTCoord2fX(0,0);
            fY1 = m_kBottomTri.VBuffer.GetTCoord2fY(0,0);
            fX2 = m_kBottomTri.VBuffer.GetTCoord2fX(0,3);
            fY2 = m_kBottomTri.VBuffer.GetTCoord2fY(0,3);
            m_kBottomTriEffect.SetLeftLine( fX1, fY1, fX2, fY2 );

            fX1 = m_kBottomTri.VBuffer.GetTCoord2fX(0,1);
            fY1 = m_kBottomTri.VBuffer.GetTCoord2fY(0,1);
            fX2 = m_kBottomTri.VBuffer.GetTCoord2fX(0,2);
            fY2 = m_kBottomTri.VBuffer.GetTCoord2fY(0,2);
            m_kBottomTriEffect.SetRightLine( fX1, fY1, fX2, fY2 );
            
            m_kBottomTriEffect.UpdateColor();
        }
    }
    


    private void readObject(java.io.ObjectInputStream in)
    throws IOException, ClassNotFoundException
    {
        m_kWidget = new Node();
        
        IndexBuffer kIBuffer = (IndexBuffer)in.readObject();
        VertexBuffer kVBuffer = (VertexBuffer)in.readObject();
        m_kBottomTri = new TriMesh( kVBuffer, kIBuffer );
        m_kBottomTriEffect = (ClassificationWidgetEffect)in.readObject();
        m_kBottomTri.AttachEffect( m_kBottomTriEffect );
        m_kBottomTri.SetName("BottomTri");
        m_kWidget.AttachChild(m_kBottomTri);

        m_kOutline = new Polyline( m_kBottomTri.VBuffer, true, true );
        m_kOutline.AttachEffect( new VertexColor3Effect() );
        m_kWidget.AttachChild(m_kOutline);
        

        kIBuffer = (IndexBuffer)in.readObject();
        kVBuffer = (VertexBuffer)in.readObject();
        m_kUpperSphere = new TriMesh( kVBuffer, kIBuffer );
        m_kUpperSphere.AttachEffect( new VertexColor3Effect() );
        m_kUpperSphere.SetName("UpperSphere");
        m_kWidget.AttachChild( m_kUpperSphere );
        m_kUpperSphere.Local.SetTranslate( m_kBottomTri.VBuffer.GetPosition3(2));

        kIBuffer = (IndexBuffer)in.readObject();
        kVBuffer = (VertexBuffer)in.readObject();
        m_kLowerSphere = new TriMesh( kVBuffer, kIBuffer );
        m_kLowerSphere.AttachEffect( new VertexColor3Effect() );
        m_kLowerSphere.SetName("LowerSphere");
        m_kWidget.AttachChild( m_kLowerSphere );
        m_kLowerSphere.Local.SetTranslate( m_kBottomTri.VBuffer.GetPosition3(1));
        
        kIBuffer = (IndexBuffer)in.readObject();
        kVBuffer = (VertexBuffer)in.readObject();
        m_kMiddleSphere = new TriMesh( kVBuffer, kIBuffer );
        m_kMiddleSphere.AttachEffect( new VertexColor3Effect() );
        m_kMiddleSphere.SetName("MiddleSphere");
        m_kWidget.AttachChild( m_kMiddleSphere );     

        Vector3f kTranslate = (Vector3f)in.readObject();
        m_kMiddleSphere.Local.SetTranslate( kTranslate );
    }
    
    private void ShiftMid( MouseEvent e )
    {      
    	float fX = calcObjX(e.getX()); 
    	float fY = calcObjY(e.getY()); 
    	Vector3f kCenter = m_kMiddleSphere.Local.GetTranslate();
        fX = Math.max( fX, m_kBottomTri.VBuffer.GetPosition3fX(0) );
        fX = Math.min( fX, m_kBottomTri.VBuffer.GetPosition3fX(1) );
        fY = Math.max( fY, m_kBottomTri.VBuffer.GetPosition3fY(0) );
        fY = Math.min( fY, m_kBottomTri.VBuffer.GetPosition3fY(2) );
        kCenter.X = fX;
        kCenter.Y = fY;
        m_fCenterX = (fX - m_kBottomTri.VBuffer.GetPosition3fX(0)) / 
        (m_kBottomTri.VBuffer.GetPosition3fX(1) - m_kBottomTri.VBuffer.GetPosition3fX(0));
        m_fCenterY = (fY - m_kBottomTri.VBuffer.GetPosition3fY(0)) / 
        (m_kBottomTri.VBuffer.GetPosition3fY(2) - m_kBottomTri.VBuffer.GetPosition3fY(0));
        m_kWidget.UpdateGS();
    }
    
    
    protected void CreateSquare(float fX, float fY, String kTexName)
    {
        float fSize = .2f;
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
        
        m_kBottomTri = new TriMesh( kVBuffer, kIBuffer );
        
        m_kBottomTriEffect = new ClassificationWidgetEffect( kTexName );
        m_kBottomTri.AttachEffect( m_kBottomTriEffect );
        m_kBottomTri.SetName("BottomTri");
        m_kWidget.AttachChild(m_kBottomTri);
        
        m_kOutline = new Polyline( m_kBottomTri.VBuffer, true, true );
        m_kOutline.AttachEffect( new VertexColor3Effect() );
        m_kWidget.AttachChild(m_kOutline);
        
        
        
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
        m_kUpperSphere.Local.SetTranslate( m_kBottomTri.VBuffer.GetPosition3(2));
        m_kUpperSphere.UpdateGS( );
        

        m_kLowerSphere = kSM.Sphere(10,10,SPHERE_RADIUS);
        for ( int i = 0; i < m_kLowerSphere.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kLowerSphere.VBuffer.SetColor3(0, i, 0f, 0f, 1f);
        }
        m_kLowerSphere.AttachEffect( new VertexColor3Effect() );
        m_kLowerSphere.SetName("LowerSphere");
        m_kWidget.AttachChild( m_kLowerSphere );
        m_kLowerSphere.Local.SetTranslate( m_kBottomTri.VBuffer.GetPosition3(1));
        m_kLowerSphere.UpdateGS( );

        m_kMiddleSphere = kSM.Sphere(10,10,SPHERE_RADIUS);
        for ( int i = 0; i < m_kMiddleSphere.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kMiddleSphere.VBuffer.SetColor3(0, i, 0f, 1f, 0f);
        }
        m_kMiddleSphere.AttachEffect( new VertexColor3Effect() );
        m_kMiddleSphere.SetName("MiddleSphere");
        m_kWidget.AttachChild( m_kMiddleSphere );
        
        float fXPos = (m_kBottomTri.VBuffer.GetPosition3fX(0) + m_kBottomTri.VBuffer.GetPosition3fX(1))/2.0f;
        float fYPos = (m_kBottomTri.VBuffer.GetPosition3fY(0) + m_kBottomTri.VBuffer.GetPosition3fY(2))/2.0f;
        float fZPos = m_kBottomTri.VBuffer.GetPosition3fZ(0);
        m_kMiddleSphere.Local.SetTranslate( fXPos, fYPos, fZPos );
        m_kMiddleSphere.UpdateGS( );
        
        m_kWidget.UpdateGS();
    }
    
    protected void ScaleRectangle(MouseEvent e, boolean bLower)
    {
    	float fX = calcObjX(e.getX()); 
    	float fY = calcObjY(e.getY()); 
    	fX = Math.max( fX, m_kBottomTri.VBuffer.GetPosition3fX(0) );
    	fX = Math.min( fX, RIGHT_EDGE );
        float centerX = (m_kBottomTri.VBuffer.GetPosition3fX( 0 ) + m_kBottomTri.VBuffer.GetPosition3fX( 1 ))/2f;
        float centerY = (m_kBottomTri.VBuffer.GetPosition3fY( 0 ) + m_kBottomTri.VBuffer.GetPosition3fY( 2 ))/2f;
        float diffX = fX - centerX;
        float diffY = fY - centerY;
    	if ( bLower )
    	{
        	fY = Math.max( fY, BOTTOM_EDGE );
        	fY = Math.min( fY, m_kBottomTri.VBuffer.GetPosition3fY(2) );
        	diffY = centerY - fY;
    	}
        else
        {
        	fY = Math.min( fY, TOP_EDGE );
        	fY = Math.max( fY, m_kBottomTri.VBuffer.GetPosition3fY(0) );
        	diffY = fY - centerY;
        }

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

        
        Vector3f kPos = m_kBottomTri.VBuffer.GetPosition3(0);
		kPos.X = centerX - diffX;
		kPos.Y = centerY - diffY;
		m_kBottomTri.VBuffer.SetPosition3(0, kPos);
		m_kBottomTri.VBuffer.SetTCoord2(0, 0, calcTCoordX(kPos.X), calcTCoordY(kPos.Y));

		kPos.X = centerX + diffX;
		kPos.Y = centerY - diffY;
		m_kBottomTri.VBuffer.SetPosition3(1, kPos);
		m_kBottomTri.VBuffer.SetTCoord2(0, 1, calcTCoordX(kPos.X), calcTCoordY(kPos.Y));
		
		kPos.X = centerX + diffX;
		kPos.Y = centerY + diffY;
		m_kBottomTri.VBuffer.SetPosition3(2, kPos);
		m_kBottomTri.VBuffer.SetTCoord2(0, 2, calcTCoordX(kPos.X), calcTCoordY(kPos.Y));
		
		kPos.X = centerX - diffX;
		kPos.Y = centerY + diffY;
		m_kBottomTri.VBuffer.SetPosition3(3, kPos);
		m_kBottomTri.VBuffer.SetTCoord2(0, 3, calcTCoordX(kPos.X), calcTCoordY(kPos.Y));
        m_kBottomTri.VBuffer.Release();
		
              
        m_kUpperSphere.Local.SetTranslate( m_kBottomTri.VBuffer.GetPosition3(2));
        m_kLowerSphere.Local.SetTranslate( m_kBottomTri.VBuffer.GetPosition3(1));

        fX = m_kBottomTri.VBuffer.GetPosition3fX(0) + m_fCenterX * 
        (m_kBottomTri.VBuffer.GetPosition3fX(1) - m_kBottomTri.VBuffer.GetPosition3fX(0));
        fY = m_kBottomTri.VBuffer.GetPosition3fY(0) + m_fCenterY * 
        (m_kBottomTri.VBuffer.GetPosition3fY(2) - m_kBottomTri.VBuffer.GetPosition3fY(0));

        Vector3f kCenter = m_kMiddleSphere.Local.GetTranslate();
        kCenter.X = fX;
        kCenter.Y = fY;
        
        m_kWidget.UpdateGS();
    }

    protected void ShiftSquare( MouseEvent e )
    {
        float fX = 0, fY = 0;
        for ( int i = 0; i < m_kBottomTri.VBuffer.GetVertexQuantity(); i++ )
        {
        	fX += m_kBottomTri.VBuffer.GetPosition3fX(i);
        	fY += m_kBottomTri.VBuffer.GetPosition3fY(i);
        }
        fX /= m_kBottomTri.VBuffer.GetVertexQuantity();
        fY /= m_kBottomTri.VBuffer.GetVertexQuantity();

        float fNewGCX = e.getX() + m_kMouseOffset.X;
        fNewGCX = calcObjX( fNewGCX );
        float fNewGCY = e.getY() + m_kMouseOffset.Y;
        fNewGCY = calcObjY( fNewGCY );

        float fDiffX = fNewGCX - fX;
        float fDiffY = fNewGCY - fY;

        float fNewXL = m_kBottomTri.VBuffer.GetPosition3fX(0);
        float fNewXR = m_kBottomTri.VBuffer.GetPosition3fX(1);
        if ( fNewXR + fDiffX > RIGHT_EDGE )
        {
        	fDiffX = RIGHT_EDGE - fNewXR;
        }
        if ( fNewXL + fDiffX < LEFT_EDGE )
        {
        	fDiffX = LEFT_EDGE - fNewXL;
        }

        float fNewYB = m_kBottomTri.VBuffer.GetPosition3fY(0);
        float fNewYT = m_kBottomTri.VBuffer.GetPosition3fY(2);
        if ( fNewYB + fDiffY < BOTTOM_EDGE )
        {
        	fDiffY = BOTTOM_EDGE - fNewYB;
        }
        if ( fNewYT + fDiffY > TOP_EDGE )
        {
        	fDiffY = TOP_EDGE - fNewYT;
        }
        
        float fNewX, fNewY;
        for ( int i = 0; i < m_kBottomTri.VBuffer.GetVertexQuantity(); i++ )
        {
            fNewX = m_kBottomTri.VBuffer.GetPosition3fX(i) + fDiffX;
            fNewY = m_kBottomTri.VBuffer.GetPosition3fY(i) + fDiffY;
            m_kBottomTri.VBuffer.SetPosition3( i, fNewX, fNewY,
            		m_kBottomTri.VBuffer.GetPosition3fZ(i) );
            m_kBottomTri.VBuffer.SetTCoord2(0, i, calcTCoordX(fNewX), calcTCoordY(fNewY));
        }
             
        m_kUpperSphere.Local.SetTranslate( m_kBottomTri.VBuffer.GetPosition3(2));
        m_kLowerSphere.Local.SetTranslate( m_kBottomTri.VBuffer.GetPosition3(1));

        Vector3f kTranslate = m_kMiddleSphere.Local.GetTranslate(); 
        m_kMiddleSphere.Local.SetTranslate( kTranslate.X + fDiffX, kTranslate.Y + fDiffY, kTranslate.Z );
        
        m_kBottomTri.VBuffer.Release();
        m_kWidget.UpdateGS();
    }   
    
}
