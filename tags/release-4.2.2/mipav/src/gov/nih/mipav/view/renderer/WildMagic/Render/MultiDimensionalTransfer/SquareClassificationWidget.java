
package gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer;

import java.awt.event.MouseEvent;
import java.util.Vector;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Collision.PickRecord;
import WildMagic.LibGraphics.Effects.TextureEffect;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.Spatial;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

public class SquareClassificationWidget extends ClassificationWidget
{

    public SquareClassificationWidget(float fX, float fY, Vector2f kTMin, Vector2f kTMax, String kTexName, int iWidth, int iHeight)
    {
        super( kTMin, kTMax, iWidth, iHeight );
        CreateSquare( fX, fY, kTexName );
    }

    protected void CreateSquare(float fX, float fY, String kTexName)
    {
        float fSize = .1f;
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);
        StandardMesh kSM = new StandardMesh(kAttributes);
        m_kBottomOutline = kSM.Rectangle(2,2,1.0f,1.0f);

        VertexBuffer kVBuffer = m_kBottomOutline.VBuffer;
        kVBuffer.SetPosition3(0, fX-fSize, fY-fSize, 0.1f);
        kVBuffer.SetColor3( 0, 0, 1.0f, 0.01f, 0.01f );

        kVBuffer.SetPosition3(1, fX+fSize, fY-fSize, 0.1f);
        kVBuffer.SetColor3( 0, 1, 1.0f, 0.01f, 0.01f  );

        kVBuffer.SetPosition3(2, fX-fSize, fY+fSize, 0.1f);
        kVBuffer.SetColor3( 0, 2, 1.0f, 0.01f, 0.01f  );

        kVBuffer.SetPosition3(3, fX+fSize, fY+fSize, 0.1f);
        kVBuffer.SetColor3( 0, 3, 1.0f, 0.01f, 0.01f  );

        m_kBottomOutline.AttachEffect( new VertexColor3Effect() );
        m_kBottomOutline.SetName("BottomOutline");
        m_kWidget.AttachChild(m_kBottomOutline);

        kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetTChannels(0,2);
        kSM = new StandardMesh(kAttributes);
        m_kBottomTri = kSM.Rectangle(2,2,1.0f,1.0f);
        m_kBottomTriEffect = new ClassificationWidgetEffect( kTexName );
        m_kBottomTri.AttachEffect( m_kBottomTriEffect );
        m_kBottomTri.SetName("BottomTri");
        m_kWidget.AttachChild(m_kBottomTri);

        ScaleRectangle( m_kBottomTri, m_kBottomOutline );
        
        kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);
        kSM = new StandardMesh(kAttributes);
        m_kUpperSphere = kSM.Sphere(10,10,0.02f);
        for ( int i = 0; i < m_kUpperSphere.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kUpperSphere.VBuffer.SetColor3(0, i, 0f, 0f, 1f);
        }
        m_kUpperSphere.AttachEffect( new VertexColor3Effect() );
        m_kUpperSphere.SetName("UpperSphere");
        m_kWidget.AttachChild( m_kUpperSphere );
        m_kUpperSphere.Local.SetTranslate( m_kBottomTri.VBuffer.GetPosition3(3));
        

        m_kLowerSphere = kSM.Sphere(10,10,0.02f);
        for ( int i = 0; i < m_kLowerSphere.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kLowerSphere.VBuffer.SetColor3(0, i, 0f, 0f, 1f);
        }
        m_kLowerSphere.AttachEffect( new VertexColor3Effect() );
        m_kLowerSphere.SetName("LowerSphere");
        m_kWidget.AttachChild( m_kLowerSphere );
        m_kLowerSphere.Local.SetTranslate( m_kBottomTri.VBuffer.GetPosition3(1));

        m_kMiddleSphere = kSM.Sphere(10,10,0.02f);
        for ( int i = 0; i < m_kMiddleSphere.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kMiddleSphere.VBuffer.SetColor3(0, i, 0f, 1f, 0f);
        }
        //m_kMiddleSphere.Local.SetScale(.01f, .01f, .01f);
        m_kMiddleSphere.AttachEffect( new VertexColor3Effect() );
        m_kMiddleSphere.SetName("MiddleSphere");
        m_kWidget.AttachChild( m_kMiddleSphere );
        
        float fXPos = (m_kBottomTri.VBuffer.GetPosition3fX(0) + m_kBottomTri.VBuffer.GetPosition3fX(1))/2.0f;
        float fYPos = (m_kBottomTri.VBuffer.GetPosition3fY(0) + m_kBottomTri.VBuffer.GetPosition3fY(2))/2.0f;
        float fZPos = m_kBottomTri.VBuffer.GetPosition3fZ(0);
        m_kMiddleSphere.Local.SetTranslate( fXPos, fYPos, fZPos );
    }

    public void ScaleRectangle( TriMesh kShape, TriMesh kOutline )
    {
        float fThickness = 1.0f/(float)m_iWidth;
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


    public void updateDisplay()
    {
        if ( m_kBottomTriEffect != null )
        {
            Vector3f kMidLine = m_kMiddleSphere.Local.GetTranslate();
            float fX1 = kMidLine.X;
            float fY1 = kMidLine.Y;
            float fX2 = fX1;
            float fY2 = fY1;
            float fTX = m_kTMin.X + fX1 * (m_kTMax.X - m_kTMin.X);
            float fTY = m_kTMin.Y + fY1 * (m_kTMax.Y - m_kTMin.Y);
            m_kBottomTriEffect.SetMidLine( fTX, fTY, fTX, fTY );

            fX1 = m_kBottomTri.VBuffer.GetTCoord2fX(0,0);
            fY1 = m_kBottomTri.VBuffer.GetTCoord2fY(0,0);
            fX2 = m_kBottomTri.VBuffer.GetTCoord2fX(0,2);
            fY2 = m_kBottomTri.VBuffer.GetTCoord2fY(0,2);
            m_kBottomTriEffect.SetLeftLine( fX1, fY1, fX2, fY2 );

            fX1 = m_kBottomTri.VBuffer.GetTCoord2fX(0,1);
            fY1 = m_kBottomTri.VBuffer.GetTCoord2fY(0,1);
            fX2 = m_kBottomTri.VBuffer.GetTCoord2fX(0,3);
            fY2 = m_kBottomTri.VBuffer.GetTCoord2fY(0,3);
            m_kBottomTriEffect.SetRightLine( fX1, fY1, fX2, fY2 );
        }
    }
    
    protected void ShiftSquare(float fX, float fY)
    {
        
        float fNewXR = m_kBottomOutline.VBuffer.GetPosition3fX(1);
        float fNewXL = m_kBottomOutline.VBuffer.GetPosition3fX(0);
        if ( fX < 0 )
        {
            if ( (fNewXL + fX) < 0 )
            {
                fX = 0 - fNewXL;
            }
        }
        if ( fX > 0 )
        {
            if ( (fNewXR + fX) > 1 )
            {
                fX = 1 - fNewXR;
            }
        }     
        
        float fNewYU = m_kBottomOutline.VBuffer.GetPosition3fY(2);
        float fNewYL = m_kBottomOutline.VBuffer.GetPosition3fY(0);
        if ( fY < 0 )
        {
            if ( (fNewYL + fY) < 0 )
            {
                fY = 0 - fNewYL;
            }
        }
        if ( fY > 0 )
        {
            if ( (fNewYU + fY) > 1 )
            {
                fY = 1 - fNewYU;
            }
        }   
        
        float fNewX, fNewY;
        for ( int i = 0; i < m_kBottomOutline.VBuffer.GetVertexQuantity(); i++ )
        {
            fNewX = m_kBottomOutline.VBuffer.GetPosition3fX(i) + fX;
            fNewY = m_kBottomOutline.VBuffer.GetPosition3fY(i) + fY;
            m_kBottomOutline.VBuffer.SetPosition3( i, fNewX, fNewY,
                m_kBottomOutline.VBuffer.GetPosition3fZ(i) );
        }
        
        ScaleRectangle( m_kBottomTri, m_kBottomOutline );        
        m_kUpperSphere.Local.SetTranslate( m_kBottomTri.VBuffer.GetPosition3(3));
        m_kLowerSphere.Local.SetTranslate( m_kBottomTri.VBuffer.GetPosition3(1));

        Vector3f kTranslate = m_kMiddleSphere.Local.GetTranslate(); 
        m_kMiddleSphere.Local.SetTranslate( kTranslate.X + fX, kTranslate.Y + fY, kTranslate.Z );
        
        m_kBottomOutline.VBuffer.Release();
        m_kBottomTri.VBuffer.Release();
        m_kUpperSphere.VBuffer.Release();
        m_kLowerSphere.VBuffer.Release();
        m_kMiddleSphere.VBuffer.Release();
    }
    
    protected void ScaleRectangle(float fX, float fY, boolean bLower)
    {

        float fCenterX = (m_kBottomTri.VBuffer.GetPosition3fX(0) + m_kBottomTri.VBuffer.GetPosition3fX(1))/2.0f;
        float fCenterY = (m_kBottomTri.VBuffer.GetPosition3fY(0) + m_kBottomTri.VBuffer.GetPosition3fY(2))/2.0f;
        Vector3f kStart;
        Vector3f kEnd = new Vector3f();
        Vector3f kBound = new Vector3f();
        if ( bLower )
        {
            kStart = m_kLowerSphere.Local.GetTranslate();
            kBound = m_kBottomTri.VBuffer.GetPosition3(2);
        }
        else
        {
            kStart = m_kUpperSphere.Local.GetTranslate();
            kBound = m_kBottomTri.VBuffer.GetPosition3(0);
        }
        kEnd.X = kStart.X + fX;
        kEnd.Y = kStart.Y + fY;
        kEnd.Z = kStart.Z;

        if ( kEnd.X < kBound.X )
        {
            return;
        }
        if ( !bLower && (kEnd.Y < kBound.Y) )
        {
            return;
        }
        if ( bLower && (kEnd.Y > kBound.Y ) )
        {
            return;
        }
        
        float fScaleX = (kEnd.X-fCenterX)/(kStart.X-fCenterX);
        float fScaleY = (kEnd.Y-fCenterY)/(kStart.Y-fCenterY);
        float fNewX, fNewY;
        for ( int i = 0; i < 4; i++ )
        {
            fNewX = m_kBottomOutline.VBuffer.GetPosition3fX(i) - fCenterX;
            fNewY = m_kBottomOutline.VBuffer.GetPosition3fY(i) - fCenterY;
            fNewX *= fScaleX;
            fNewY *= fScaleY;
            fNewX += fCenterX;
            fNewY += fCenterY;
            fNewX = Math.max( fNewX, 0 );
            fNewX = Math.min( fNewX, 1 );
            fNewY = Math.max( fNewY, 0 );
            fNewY = Math.min( fNewY, 1 );
            m_kBottomOutline.VBuffer.SetPosition3( i, fNewX, fNewY,
                    m_kBottomOutline.VBuffer.GetPosition3fZ(i) );
        }
        
        ScaleRectangle( m_kBottomTri, m_kBottomOutline );        
        m_kUpperSphere.Local.SetTranslate( m_kBottomTri.VBuffer.GetPosition3(3));
        m_kLowerSphere.Local.SetTranslate( m_kBottomTri.VBuffer.GetPosition3(1));

        Vector3f kCenter = m_kMiddleSphere.Local.GetTranslate();
        kCenter.X -= fCenterX;
        kCenter.Y -= fCenterY;
        kCenter.X *= fScaleX;
        kCenter.Y *= fScaleY;
        kCenter.X += fCenterX;
        kCenter.Y += fCenterY;

        if ( kCenter.X < m_kBottomTri.VBuffer.GetPosition3fX(0) )
        {
            kCenter.X = m_kBottomTri.VBuffer.GetPosition3fX(0);
        }
        if ( kCenter.X > m_kBottomTri.VBuffer.GetPosition3fX(1) )
        {
            kCenter.X = m_kBottomTri.VBuffer.GetPosition3fX(1);
        }
        if ( kCenter.Y < m_kBottomTri.VBuffer.GetPosition3fY(0) )
        {
            kCenter.Y = m_kBottomTri.VBuffer.GetPosition3fY(0);
        }
        if ( kCenter.Y > m_kBottomTri.VBuffer.GetPosition3fY(2) )
        {
            kCenter.Y = m_kBottomTri.VBuffer.GetPosition3fY(2);
        }

        
        m_kBottomOutline.VBuffer.Release();
        m_kBottomTri.VBuffer.Release();
        m_kUpperSphere.VBuffer.Release();
        m_kLowerSphere.VBuffer.Release();
        m_kMiddleSphere.VBuffer.Release();
    }
    
    private void ShiftMid( float fX, float fY )
    {      
        Vector3f kCenter = m_kMiddleSphere.Local.GetTranslate();
        if ( fX < 0 )
        {
            if ( (kCenter.X + fX) < m_kBottomTri.VBuffer.GetPosition3fX(0) )
            {
                kCenter.X = m_kBottomTri.VBuffer.GetPosition3fX(0);
            }
            else
            {
                kCenter.X += fX;
            }
        }
        if ( fX > 0 )
        {
            if ( (kCenter.X + fX) > m_kBottomTri.VBuffer.GetPosition3fX(1) )
            {
                kCenter.X = m_kBottomTri.VBuffer.GetPosition3fX(1);
            }
            else
            {
                kCenter.X += fX;
            }
        }  
        if ( fY < 0 )
        {
            if ( (kCenter.Y + fY) < m_kBottomTri.VBuffer.GetPosition3fY(0) )
            {
                kCenter.Y = m_kBottomTri.VBuffer.GetPosition3fY(0);
            }
            else
            {
                kCenter.Y += fY;
            }
        }
        if ( fY > 0 )
        {
            if ( (kCenter.Y + fY) > m_kBottomTri.VBuffer.GetPosition3fY(2) )
            {
                kCenter.Y = m_kBottomTri.VBuffer.GetPosition3fY(2);
            }
            else
            {
                kCenter.Y += fY;
            }
        }
        m_kWidget.UpdateGS();
    }

    public boolean processMouseDrag(int iX0ld, int iYOld, int iButton, MouseEvent e ) 
    {        

        float fXOld = ((float)iX0ld/(float)m_iWidth);
        float fYOld = ((float)m_iHeight-(float)iYOld)/(float)m_iHeight;
        float fX = ((float)e.getX()/(float)m_iWidth);
        float fY = ((float)m_iHeight-(float)e.getY())/(float)m_iHeight;
        if ( iButton == MouseEvent.BUTTON1 )
        {
            if ( (m_kPicked == m_kBottomOutline) || (m_kPicked == m_kBottomTri) )
            {
                ShiftSquare( fX-fXOld, fY-fYOld );
            }
            else if ( (m_kPicked == m_kLowerSphere) || (m_kPicked == m_kUpperSphere) )
            {
                ScaleRectangle( fX-fXOld, fY-fYOld, (m_kPicked == m_kLowerSphere) );
            }
            else if ( m_kPicked == m_kMiddleSphere )
            {
                ShiftMid(fX-fXOld, fY-fYOld);
            }
            return false;
        }
        return false;
    }   
    
}
