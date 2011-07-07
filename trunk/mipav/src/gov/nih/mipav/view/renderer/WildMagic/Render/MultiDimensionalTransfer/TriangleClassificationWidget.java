
package gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer;

import java.awt.event.MouseEvent;
import java.io.IOException;

import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.TextureEffect;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

public class TriangleClassificationWidget extends ClassificationWidget
{
    /**  */
    private static final long serialVersionUID = -3903003012355432310L;

    private float m_fCenterT = 0.5f;
    
    public TriangleClassificationWidget(float fX, float fY, Vector2f kTMin, Vector2f kTMax, String kTexName, int iWidth, int iHeight)
    {
        super( kTMin, kTMax, iWidth, iHeight );
        CreateTriangle(fX,fY, kTexName);
    }
    
    public TriangleClassificationWidget ( TriangleClassificationWidget kWidget )
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
        m_kLowerSphere = new TriMesh(new VertexBuffer(kWidget.m_kLowerSphere.VBuffer),
                new IndexBuffer(kWidget.m_kLowerSphere.IBuffer));
    }

    @Override
	public boolean processMouseDrag(int iX0ld, int iYOld, int iButton, MouseEvent e )
    {
        float fXOld = ((float)iX0ld/(float)m_iWidth);
        float fYOld = ((float)m_iHeight-(float)iYOld)/m_iHeight;

        float fX = ((float)e.getX()/(float)m_iWidth);
        float fY = ((float)m_iHeight-(float)e.getY())/m_iHeight;

        if ( iButton == MouseEvent.BUTTON1 )
        {
            if ( m_kPicked == m_kBottomTri )
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
            return false;
        }
        return false;
    }
    
	public boolean Pick( int iX, int iY )
	{
		boolean bPicked = false;
		m_kPicked = null;
		float fX = calcObjX(iX);
		float fY = calcObjY(iY);
		
        float fT = (fY - m_kBottomTri.VBuffer.GetPosition3fY(0)) / 
        (m_kBottomTri.VBuffer.GetPosition3fY(2) - m_kBottomTri.VBuffer.GetPosition3fY(0));

        float fLeftEdge = m_kBottomTri.VBuffer.GetPosition3fX(0) + fT * 
        (m_kBottomTri.VBuffer.GetPosition3fX(2) - m_kBottomTri.VBuffer.GetPosition3fX(0));
        float fRightEdge = m_kBottomTri.VBuffer.GetPosition3fX(0) + fT * 
        (m_kBottomTri.VBuffer.GetPosition3fX(1) - m_kBottomTri.VBuffer.GetPosition3fX(0));
		
		if ( (fX >= fLeftEdge) && (fX <= fRightEdge) &&
				(fY >= m_kBottomTri.VBuffer.GetPosition3fY(0)) && (fY <= m_kBottomTri.VBuffer.GetPosition3fY(2)) )
		{
			m_kPicked = m_kBottomTri;
			//System.err.println( "Picked Tri" );
			bPicked = true;
		}
		return super.Pick(iX,iY, bPicked);
	}

    public void ScaleTriangle( TriMesh kTri, TriMesh kOutline )
    {
        Vector3f kPos0 = kOutline.VBuffer.GetPosition3(0);
        Vector3f kPos1 = kOutline.VBuffer.GetPosition3(1);
        Vector3f kPos2 = kOutline.VBuffer.GetPosition3(2);
        float fTransX = (kPos0.X + kPos1.X + kPos2.X)/3.0f;
        float fTransY = (kPos0.Y + kPos1.Y + kPos2.Y)/3.0f;

        float fNewX = (kPos0.X - fTransX) * .90f + fTransX;
        float fNewY = (kPos0.Y - fTransY) * .90f + fTransY;
        kTri.VBuffer.SetPosition3(0, fNewX, fNewY, 0.11f);
        float fTX = fNewX;
        float fTY = fNewY;
        fTX = m_kTMin.X + fTX * (m_kTMax.X - m_kTMin.X);
        fTY = m_kTMin.Y + fTY * (m_kTMax.Y - m_kTMin.Y);
        kTri.VBuffer.SetTCoord2(0, 0, fTX, fTY);


        fNewX = (kPos1.X - fTransX) * .90f + fTransX;
        fNewY = (kPos1.Y - fTransY) * .90f + fTransY;
        kTri.VBuffer.SetPosition3(1, fNewX, fNewY, 0.11f);
        fTX = fNewX;
        fTY = fNewY;
        fTX = m_kTMin.X + fTX * (m_kTMax.X - m_kTMin.X);
        fTY = m_kTMin.Y + fTY * (m_kTMax.Y - m_kTMin.Y);
        kTri.VBuffer.SetTCoord2(0, 1, fTX, fTY);

        fNewX = (kPos2.X - fTransX) * .90f + fTransX;
        fNewY = (kPos2.Y - fTransY) * .90f + fTransY;
        kTri.VBuffer.SetPosition3(2, fNewX, fNewY, 0.11f);
        fTX = fNewX;
        fTY = fNewY;
        fTX = m_kTMin.X + fTX * (m_kTMax.X - m_kTMin.X);
        fTY = m_kTMin.Y + fTY * (m_kTMax.Y - m_kTMin.Y);
        kTri.VBuffer.SetTCoord2(0, 2, fTX, fTY);

        m_kWidget.UpdateGS();
    }
    
    public void ShiftMidTriangle( MouseEvent e )
    {
    	float fY = 2.0f * (((m_iHeight - e.getY()) / (float)m_iHeight) - 0.5f); 
    	Vector3f kCenter = m_kMiddleSphere.Local.GetTranslate();
        fY = Math.max( fY, m_kBottomTri.VBuffer.GetPosition3fY(0) );
        fY = Math.min( fY, m_kBottomTri.VBuffer.GetPosition3fY(2) );
        kCenter.Y = fY;
        m_fCenterT = (fY - m_kBottomTri.VBuffer.GetPosition3fY(0)) / 
        (m_kBottomTri.VBuffer.GetPosition3fY(2) - m_kBottomTri.VBuffer.GetPosition3fY(0));

        float fX = m_kBottomTri.VBuffer.GetPosition3fX(0) + m_fCenterT * 
        (m_kBottomTri.VBuffer.GetPosition3fX(1) - m_kBottomTri.VBuffer.GetPosition3fX(0));
        m_kMiddleSphere.Local.SetTranslate( fX, fY, 0.11f );
        
        m_kWidget.UpdateGS();
    }
    
    @Override
	public void updateDisplay()
    {
        if ( m_kBottomTriEffect != null )
        {
            float fMidTexX = (m_kBottomTri.VBuffer.GetTCoord2fX(0,1) + m_kBottomTri.VBuffer.GetTCoord2fX(0,2) )/2.0f;
            float fMidTexY = (m_kBottomTri.VBuffer.GetTCoord2fY(0,1) + m_kBottomTri.VBuffer.GetTCoord2fY(0,2) )/2.0f;
            m_kBottomTriEffect.SetMidLine( m_kBottomTri.VBuffer.GetTCoord2fX(0,0),
                    m_kBottomTri.VBuffer.GetTCoord2fY(0,0),
                    fMidTexX, fMidTexY);
            

            //Left Mid calc:
            Vector3f kPos0 = m_kBottomTri.VBuffer.GetPosition3(0);
            Vector3f kPos1 = m_kBottomTri.VBuffer.GetPosition3(2);

            float fNewX = m_fCenterT * kPos1.X + (1.0f - m_fCenterT) * kPos0.X;
            float fNewY = m_fCenterT * kPos1.Y + (1.0f - m_fCenterT) * kPos0.Y;
            float fLeft_TX = calcTCoordX(fNewX);
            float fLeft_TY = calcTCoordY(fNewY);

            //Right Mid calc:
            kPos0 = m_kBottomTri.VBuffer.GetPosition3(0);
            kPos1 = m_kBottomTri.VBuffer.GetPosition3(1);

            fNewX = m_fCenterT * kPos1.X + (1.0f - m_fCenterT) * kPos0.X;
            fNewY = m_fCenterT * kPos1.Y + (1.0f - m_fCenterT) * kPos0.Y;
            float fRight_TX = calcTCoordX(fNewX);
            float fRight_TY = calcTCoordY(fNewY);
            

            float fLeftTexX1 = m_kBottomTri.VBuffer.GetTCoord2fX(0,2);
            float fLeftTexY1 = m_kBottomTri.VBuffer.GetTCoord2fY(0,2);
            m_kBottomTriEffect.SetLeftLine( fLeft_TX, fLeft_TY,
                    fLeftTexX1, fLeftTexY1);

            float fRightTexX1 = m_kBottomTri.VBuffer.GetTCoord2fX(0,1);
            float fRightTexY1 = m_kBottomTri.VBuffer.GetTCoord2fY(0,1);
            m_kBottomTriEffect.SetRightLine( fRight_TX, fRight_TY,
                    fRightTexX1, fRightTexY1);
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
        m_kUpperSphere.Local.SetTranslate( m_kBottomTri.VBuffer.GetPosition3(1));
    
        
        kIBuffer = (IndexBuffer)in.readObject();
        kVBuffer = (VertexBuffer)in.readObject();
        m_kLowerSphere = new TriMesh( kVBuffer, kIBuffer );
        m_kLowerSphere.AttachEffect( new VertexColor3Effect() );
        m_kLowerSphere.SetName("LowerSphere");
        m_kWidget.AttachChild( m_kLowerSphere );
        m_kLowerSphere.Local.SetTranslate(m_kBottomTri.VBuffer.GetPosition3(0));
        
        kIBuffer = (IndexBuffer)in.readObject();
        kVBuffer = (VertexBuffer)in.readObject();
        m_kMiddleSphere = new TriMesh( kVBuffer, kIBuffer );
        m_kMiddleSphere.AttachEffect( new VertexColor3Effect() );
        m_kMiddleSphere.SetName("MiddleSphere");
        m_kWidget.AttachChild( m_kMiddleSphere );     

        Vector3f kTranslate = (Vector3f)in.readObject();
        m_kMiddleSphere.Local.SetTranslate( kTranslate );

        m_kWidget.UpdateGS();
    }



    protected void CreateTriangle(float fX, float fY, String kTexName)
    {
        float fSize = .2f;
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);
        kAttributes.SetTChannels(0,2);
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
        m_kUpperSphere.Local.SetTranslate( m_kBottomTri.VBuffer.GetPosition3(1));


        m_kMiddleSphere = kSM.Sphere(10,10,SPHERE_RADIUS);
        for ( int i = 0; i < m_kMiddleSphere.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kMiddleSphere.VBuffer.SetColor3(0, i, 0f, 1f, 0f);
        }
        m_kMiddleSphere.AttachEffect( new VertexColor3Effect() );
        m_kMiddleSphere.SetName("MiddleSphere");
        m_kWidget.AttachChild( m_kMiddleSphere );
        

        fX = m_kBottomTri.VBuffer.GetPosition3fX(0) + m_fCenterT * 
        (m_kBottomTri.VBuffer.GetPosition3fX(1) - m_kBottomTri.VBuffer.GetPosition3fX(0));
        fY = m_kBottomTri.VBuffer.GetPosition3fY(0) + m_fCenterT * 
        (m_kBottomTri.VBuffer.GetPosition3fY(1) - m_kBottomTri.VBuffer.GetPosition3fY(0));
        m_kMiddleSphere.Local.SetTranslate( fX, fY, 0.11f );
        


        m_kLowerSphere = kSM.Sphere(10,10,SPHERE_RADIUS);
        for ( int i = 0; i < m_kLowerSphere.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kLowerSphere.VBuffer.SetColor3(0, i, 0f, 0f, 1f);
        }
        m_kLowerSphere.Local.SetTranslate(m_kBottomTri.VBuffer.GetPosition3(0));
        m_kLowerSphere.AttachEffect( new VertexColor3Effect() );
        m_kLowerSphere.SetName("LowerSphere");
        m_kWidget.AttachChild( m_kLowerSphere );
        m_kWidget.UpdateGS();
    }


    protected void ScaleTriangle(MouseEvent e)
    {
    	float fX = calcObjX(e.getX()); 
    	float fY = calcObjY(e.getY());  
        float centerX = (m_kBottomTri.VBuffer.GetPosition3fX( 1 ) + m_kBottomTri.VBuffer.GetPosition3fX( 2 ))/2f;
    	fX = Math.max( fX, centerX );
    	fX = Math.min( fX, RIGHT_EDGE );
    	fY = Math.min( fY, TOP_EDGE );
    	fY = Math.max( fY, m_kBottomTri.VBuffer.GetPosition3fY(0) );
    	
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
        m_kBottomTri.VBuffer.SetPosition3( 1, fX, fY,
        		m_kBottomTri.VBuffer.GetPosition3fZ(1) );
        m_kBottomTri.VBuffer.SetTCoord2(0, 1, calcTCoordX(fX), calcTCoordY(fY));
        
        // Move triangle left-corner:
        m_kBottomTri.VBuffer.SetPosition3( 2, centerX - diffX, fY,
        		m_kBottomTri.VBuffer.GetPosition3fZ(1) );
        m_kBottomTri.VBuffer.SetTCoord2(0, 2, calcTCoordX(centerX - diffX), calcTCoordY(fY));
        m_kBottomTri.VBuffer.Release();
     
        // Reposition upper sphere:
        m_kUpperSphere.Local.SetTranslate( m_kBottomTri.VBuffer.GetPosition3(1));

        // Reposition middle sphere after scale:
        fX = m_kBottomTri.VBuffer.GetPosition3fX(0) + m_fCenterT * 
        (m_kBottomTri.VBuffer.GetPosition3fX(1) - m_kBottomTri.VBuffer.GetPosition3fX(0));
        fY = m_kBottomTri.VBuffer.GetPosition3fY(0) + m_fCenterT * 
        (m_kBottomTri.VBuffer.GetPosition3fY(1) - m_kBottomTri.VBuffer.GetPosition3fY(0));
        m_kMiddleSphere.Local.SetTranslate( fX, fY, 0.11f );
        m_kWidget.UpdateGS();
    }

    protected void ShearTriangle(float fX, float fY)
    {
        float fNewXR = Math.max( m_kBottomTri.VBuffer.GetPosition3fX(1),
        		m_kBottomTri.VBuffer.GetPosition3fX(0) );
        float fNewXL = Math.min( m_kBottomTri.VBuffer.GetPosition3fX(2),
        		m_kBottomTri.VBuffer.GetPosition3fX(0) );
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
        fNewX = m_kBottomTri.VBuffer.GetPosition3fX(1) + fX;
        fNewX = Math.max( LEFT_EDGE, fNewX );
        fNewX = Math.min( fNewX, RIGHT_EDGE );
            
        fNewY = m_kBottomTri.VBuffer.GetPosition3fY(1) + fY;
        fNewY = Math.max( BOTTOM_EDGE, fNewY );
        fNewY = Math.min( fNewY, TOP_EDGE );
        m_kBottomTri.VBuffer.SetPosition3( 1, fNewX, fNewY,
        		m_kBottomTri.VBuffer.GetPosition3fZ(1) );
        m_kBottomTri.VBuffer.SetTCoord2(0, 1, calcTCoordX(fNewX), calcTCoordY(fNewY));

        
        fNewX = m_kBottomTri.VBuffer.GetPosition3fX(2) + fX;
        fNewX = Math.max( LEFT_EDGE, fNewX );
        fNewX = Math.min( fNewX, RIGHT_EDGE );
            
        fNewY = m_kBottomTri.VBuffer.GetPosition3fY(2) + fY;
        fNewY = Math.max( BOTTOM_EDGE, fNewY );
        fNewY = Math.min( fNewY, TOP_EDGE );
        m_kBottomTri.VBuffer.SetPosition3( 2, fNewX, fNewY,
        		m_kBottomTri.VBuffer.GetPosition3fZ(2) );
        m_kBottomTri.VBuffer.SetTCoord2(0, 2, calcTCoordX(fNewX), calcTCoordY(fNewY));
        m_kBottomTri.VBuffer.Release();

        // Reposition top sphere:
        m_kUpperSphere.Local.SetTranslate( m_kBottomTri.VBuffer.GetPosition3(1));

        // Reposition middle sphere:
        fX = m_kBottomTri.VBuffer.GetPosition3fX(0) + m_fCenterT * 
        (m_kBottomTri.VBuffer.GetPosition3fX(1) - m_kBottomTri.VBuffer.GetPosition3fX(0));
        fY = m_kBottomTri.VBuffer.GetPosition3fY(0) + m_fCenterT * 
        (m_kBottomTri.VBuffer.GetPosition3fY(1) - m_kBottomTri.VBuffer.GetPosition3fY(0));
        m_kMiddleSphere.Local.SetTranslate( fX, fY, 0.11f );
        m_kWidget.UpdateGS();
    }


    protected void ShiftTriangle(MouseEvent e)
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
        float fDiffY = 0;//fNewGCY - fY;

        float fNewXL = m_kBottomTri.VBuffer.GetPosition3fX(2);
        float fNewXR = m_kBottomTri.VBuffer.GetPosition3fX(1);
        float fNewXC = m_kBottomTri.VBuffer.GetPosition3fX(0);
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
             
        m_kUpperSphere.Local.SetTranslate( m_kBottomTri.VBuffer.GetPosition3(1));
        m_kLowerSphere.Local.SetTranslate( m_kBottomTri.VBuffer.GetPosition3(0));

        Vector3f kTranslate = m_kMiddleSphere.Local.GetTranslate(); 
        m_kMiddleSphere.Local.SetTranslate( kTranslate.X + fDiffX, kTranslate.Y + fDiffY, kTranslate.Z );
        
        m_kBottomTri.VBuffer.Release();
        m_kWidget.UpdateGS();
    }

}
