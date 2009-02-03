
package gov.nih.mipav.view.renderer.WildMagic.Render;

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

public class LevWidgetTri extends LevWidget
{
    private TriMesh m_kTopOutline;
    private TriMesh m_kTopTri;


    public LevWidgetTri(float fX, float fY, Vector2f kTMin, Vector2f kTMax, String kTexName, int iWidth, int iHeight)
    {
        super( kTMin, kTMax, iWidth, iHeight );
        CreateTriangle(fX,fY, kTexName, iWidth, iHeight);
    }

    public void dispose()
    {
        m_kTopOutline = null;
        m_kTopTri = null;
    }

    public boolean processMouseDrag(int iX0ld, int iYOld, int iButton, MouseEvent e )
    {

        if ( iButton == MouseEvent.BUTTON1 )
        {
            if ( m_kPicked == m_kBottomOutline )
            {
                //ShearTriangle( fX, fY );
            }

            else if ( m_kPicked == m_kMiddleSphere )
            {
                //ShiftMidTriangle( fY );
            }
            else if ( m_kPicked == m_kLowerSphere)
            {
                //ShiftTriangle( fX );
            }
            else if ( m_kPicked == m_kUpperSphere )
            {
                //ScaleTriangle( fY );
            }
            return false;
        }
        if ( (iButton == MouseEvent.BUTTON3) && (m_kPicked == m_kBottomTri ) )
        {
            return true;
        }
        return false;
    }

    public void updateDisplay()
    {
        if ( m_kBottomTriEffect != null )
        {
            float fMidTexX = (m_kBottomTri.VBuffer.GetTCoord2fX(0,1) + m_kBottomTri.VBuffer.GetTCoord2fX(0,2) )/2.0f;
            float fMidTexY = (m_kBottomTri.VBuffer.GetTCoord2fY(0,1) + m_kBottomTri.VBuffer.GetTCoord2fY(0,2) )/2.0f;
            m_kBottomTriEffect.SetMidLine( m_kBottomTri.VBuffer.GetTCoord2fX(0,0),
                    m_kBottomTri.VBuffer.GetTCoord2fY(0,0),
                    fMidTexX, fMidTexY);

            float fLeftTexX0 = m_kTopTri.VBuffer.GetTCoord2fX(0,2);
            float fLeftTexY0 = m_kTopTri.VBuffer.GetTCoord2fY(0,2);
            float fLeftTexX1 = m_kBottomTri.VBuffer.GetTCoord2fX(0,2);
            float fLeftTexY1 = m_kBottomTri.VBuffer.GetTCoord2fY(0,2);
            m_kBottomTriEffect.SetLeftLine( fLeftTexX0, fLeftTexY0,
                    fLeftTexX1, fLeftTexY1);

            float fRightTexX0 = m_kTopTri.VBuffer.GetTCoord2fX(0,1);
            float fRightTexY0 = m_kTopTri.VBuffer.GetTCoord2fY(0,1);
            float fRightTexX1 = m_kBottomTri.VBuffer.GetTCoord2fX(0,1);
            float fRightTexY1 = m_kBottomTri.VBuffer.GetTCoord2fY(0,1);
            m_kBottomTriEffect.SetRightLine( fRightTexX0, fRightTexY0,
                    fRightTexX1, fRightTexY1);
        }
    }

    protected void CreateTriangle(float fX, float fY, String kTexName, int iWidth, int iHeight)
    {
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);
        VertexBuffer kVBuffer = new VertexBuffer(kAttributes, 3);
        kVBuffer.SetPosition3(0, fX, -1, 0.1f);
        kVBuffer.SetColor3( 0, 0, 1.0f, 0.01f, 0.01f );

        kVBuffer.SetPosition3(1, fX+.2f, 0.1f, 0.1f);
        kVBuffer.SetColor3( 0, 1, 1.0f, 0.01f, 0.01f  );

        kVBuffer.SetPosition3(2, fX-.2f, 0.1f, 0.1f);
        kVBuffer.SetColor3( 0, 2, 1.0f, 0.01f, 0.01f  );
        int[] aiData = new int[]{0,1,2};
        IndexBuffer kIBuffer = new IndexBuffer(aiData);

        m_kBottomOutline = new TriMesh( kVBuffer, kIBuffer );
        m_kBottomOutline.AttachEffect( new VertexColor3Effect() );
        m_kBottomOutline.SetName("BottomOutline");
        m_kWidget.AttachChild(m_kBottomOutline);

        kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetTChannels(0,2);
        kVBuffer = new VertexBuffer(kAttributes, 3);
        m_kBottomTri = new TriMesh( kVBuffer, kIBuffer );
        m_kBottomTriEffect = new LevWidgetEffect( kTexName );
        m_kBottomTri.AttachEffect( m_kBottomTriEffect );
        m_kBottomTri.SetName("BottomTri");
        m_kWidget.AttachChild(m_kBottomTri);

        kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);
        StandardMesh kSM = new StandardMesh(kAttributes);
        m_kUpperSphere = kSM.Sphere(10,10,0.05f);
        for ( int i = 0; i < m_kUpperSphere.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kUpperSphere.VBuffer.SetColor3(0, i, 0f, 0f, 1f);
        }
        m_kUpperSphere.Local.SetScale((float)iHeight/(float)iWidth, 1f, 1f);
        m_kUpperSphere.AttachEffect( new VertexColor3Effect() );
        m_kUpperSphere.SetName("UpperSphere");
        m_kWidget.AttachChild( m_kUpperSphere );

        ScaleTriangle( m_kUpperSphere, m_kBottomTri, m_kBottomOutline );







        Vector3f kPos0 = m_kBottomOutline.VBuffer.GetPosition3(0);
        Vector3f kPos1 = m_kBottomOutline.VBuffer.GetPosition3(1);
        Vector3f kPos2 = m_kBottomOutline.VBuffer.GetPosition3(2);
        float fNewX = m_fScale * kPos1.X + (1.0f - m_fScale) * kPos0.X;
        float fNewY = m_fScale * kPos1.Y + (1.0f - m_fScale) * kPos0.Y;

        kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);
        kVBuffer = new VertexBuffer(kAttributes, 3);
        kVBuffer.SetPosition3(0, fX, -1, 0.11f);
        kVBuffer.SetColor3( 0, 0, 1.0f, 0.01f, 1.0f );

        kVBuffer.SetPosition3(1, fNewX, fNewY, 0.11f);
        kVBuffer.SetColor3( 0, 1, 1.0f, 0.01f, 1.0f  );

        fNewX = m_fScale * kPos2.X + (1.0f - m_fScale) * kPos0.X;
        fNewY = m_fScale * kPos2.Y + (1.0f - m_fScale) * kPos0.Y;
        kVBuffer.SetPosition3(2, fNewX, fNewY, 0.11f);
        kVBuffer.SetColor3( 0, 2, 1.0f, 0.01f, 1.0f  );

        m_kTopOutline = new TriMesh( kVBuffer, kIBuffer );
        m_kTopOutline.AttachEffect( new VertexColor3Effect() );
        m_kTopOutline.SetName("TopOutline");
        m_kWidget.AttachChild(m_kTopOutline);

        kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetTChannels(0,2);
        kVBuffer = new VertexBuffer(kAttributes, 3);
        m_kTopTri = new TriMesh( kVBuffer, kIBuffer );
        m_kTopTri.AttachEffect( new TextureEffect( kTexName ) );
        m_kTopTri.SetName("TopTri");
        m_kWidget.AttachChild(m_kTopTri);

        m_kMiddleSphere = kSM.Sphere(10,10,0.05f);
        for ( int i = 0; i < m_kMiddleSphere.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kMiddleSphere.VBuffer.SetColor3(0, i, 1f, 0f, 1f);
        }
        m_kMiddleSphere.Local.SetScale((float)iHeight/(float)iWidth, 1f, 1f);
        m_kMiddleSphere.AttachEffect( new VertexColor3Effect() );
        m_kMiddleSphere.SetName("MiddleSphere");
        m_kWidget.AttachChild( m_kMiddleSphere );

        ScaleTriangle( m_kMiddleSphere, m_kTopTri, m_kTopOutline );


        m_kLowerSphere = kSM.Sphere(10,10,0.05f);
        for ( int i = 0; i < m_kLowerSphere.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kLowerSphere.VBuffer.SetColor3(0, i, 0f, 1f, 0f);
        }
        m_kLowerSphere.Local.SetScale((float)iHeight/(float)iWidth, 1f, 1f);
        m_kLowerSphere.Local.SetTranslate(m_kBottomOutline.VBuffer.GetPosition3(0));
        m_kLowerSphere.AttachEffect( new VertexColor3Effect() );
        m_kLowerSphere.SetName("LowerSphere");
        m_kWidget.AttachChild( m_kLowerSphere );
        m_kWidget.UpdateGS();
    }


    protected void ScaleTriangle(float fY)
    {
        float fNewXR = Math.min( m_kBottomOutline.VBuffer.GetPosition3fX(1) + fY,
                1);
        float fNewXL = Math.max( m_kBottomOutline.VBuffer.GetPosition3fX(2) - fY,
                -1);


        float fNewX, fNewY;
        fNewX = fNewXR;
        fNewY = m_kBottomOutline.VBuffer.GetPosition3fY(1) + fY;
        fNewY = Math.max( -1, fNewY );
        fNewY = Math.min( fNewY, 1 );
        m_kBottomOutline.VBuffer.SetPosition3( 1, fNewX, fNewY,
                m_kBottomOutline.VBuffer.GetPosition3fZ(1) );

        fNewX = fNewXL;
        fNewY = m_kBottomOutline.VBuffer.GetPosition3fY(2) + fY;
        fNewY = Math.max( -1, fNewY );
        fNewY = Math.min( fNewY, 1 );
        m_kBottomOutline.VBuffer.SetPosition3( 2, fNewX, fNewY,
                m_kBottomOutline.VBuffer.GetPosition3fZ(2) );

        ScaleTriangle( m_kUpperSphere, m_kBottomTri, m_kBottomOutline );        

        m_kBottomOutline.VBuffer.Release();
        m_kBottomTri.VBuffer.Release();
        m_kUpperSphere.VBuffer.Release();


        ShiftMidTriangle(m_fScale);
        m_kWidget.UpdateGS();
    }


    protected void ShearTriangle(float fX, float fY)
    {
        float fNewXR = Math.max( m_kBottomOutline.VBuffer.GetPosition3fX(1),
                m_kBottomOutline.VBuffer.GetPosition3fX(0) );
        float fNewXL = Math.min( m_kBottomOutline.VBuffer.GetPosition3fX(2),
                m_kBottomOutline.VBuffer.GetPosition3fX(0) );
        if ( fX < 0 )
        {
            if ( (fNewXL + fX) < -1 )
            {
                fX = -1 - fNewXL;
            }
        }
        if ( fX > 0 )
        {
            if ( (fNewXR + fX) >= 1 )
            {
                fX = 1 - fNewXR;
            }
        }    


        float fNewX, fNewY;
        fNewX = m_kBottomOutline.VBuffer.GetPosition3fX(1);
        if ( fNewXL > -1 )
        {
            fNewX = m_kBottomOutline.VBuffer.GetPosition3fX(1) + fX;
            fNewX = Math.max( -1, fNewX );
            fNewX = Math.min( fNewX, 1 );
        }
        fNewY = m_kBottomOutline.VBuffer.GetPosition3fY(1) + fY;
        fNewY = Math.max( -1, fNewY );
        fNewY = Math.min( fNewY, 1 );
        m_kBottomOutline.VBuffer.SetPosition3( 1, fNewX, fNewY,
                m_kBottomOutline.VBuffer.GetPosition3fZ(1) );

        fNewX = m_kBottomOutline.VBuffer.GetPosition3fX(2);
        if ( fNewXR < 1 )
        {
            fNewX = m_kBottomOutline.VBuffer.GetPosition3fX(2) + fX;
            fNewX = Math.max( -1, fNewX );
            fNewX = Math.min( fNewX, 1 );
        }
        fNewY = m_kBottomOutline.VBuffer.GetPosition3fY(2) + fY;
        fNewY = Math.max( -1, fNewY );
        fNewY = Math.min( fNewY, 1 );
        m_kBottomOutline.VBuffer.SetPosition3( 2, fNewX, fNewY,
                m_kBottomOutline.VBuffer.GetPosition3fZ(2) );

        ScaleTriangle( m_kUpperSphere, m_kBottomTri, m_kBottomOutline );        

        m_kBottomOutline.VBuffer.Release();
        m_kBottomTri.VBuffer.Release();
        m_kUpperSphere.VBuffer.Release();


        ShiftMidTriangle(m_fScale);
        m_kWidget.UpdateGS();
    }


    protected void ShiftTriangle(float fX)
    {

        float fNewXR = Math.max( m_kBottomOutline.VBuffer.GetPosition3fX(1),
                m_kBottomOutline.VBuffer.GetPosition3fX(0) );
        float fNewXL = Math.min( m_kBottomOutline.VBuffer.GetPosition3fX(2),
                m_kBottomOutline.VBuffer.GetPosition3fX(0) );
        if ( fX < 0 )
        {
            if ( (fNewXL + fX) < -1 )
            {
                fX = -1 - fNewXL;
            }
        }
        if ( fX > 0 )
        {
            if ( (fNewXR + fX) >= 1 )
            {
                fX = 1 - fNewXR;
            }
        }     

        float fNewX = m_kBottomOutline.VBuffer.GetPosition3fX(0) + fX;
        m_kBottomOutline.VBuffer.SetPosition3( 0, fNewX, m_kBottomOutline.VBuffer.GetPosition3fY(0),
                m_kBottomOutline.VBuffer.GetPosition3fZ(0) );
        fNewX = m_kBottomOutline.VBuffer.GetPosition3fX(1) + fX;
        m_kBottomOutline.VBuffer.SetPosition3( 1, fNewX, m_kBottomOutline.VBuffer.GetPosition3fY(1),
                m_kBottomOutline.VBuffer.GetPosition3fZ(1) );
        fNewX = m_kBottomOutline.VBuffer.GetPosition3fX(2) + fX;
        m_kBottomOutline.VBuffer.SetPosition3( 2, fNewX, m_kBottomOutline.VBuffer.GetPosition3fY(2),
                m_kBottomOutline.VBuffer.GetPosition3fZ(2) );

        ScaleTriangle( m_kUpperSphere, m_kBottomTri, m_kBottomOutline );        

        m_kBottomOutline.VBuffer.Release();
        m_kBottomTri.VBuffer.Release();
        m_kUpperSphere.VBuffer.Release();



        ShiftMidTriangle(m_fScale);

        m_kLowerSphere.Local.SetTranslate(m_kBottomOutline.VBuffer.GetPosition3(0));
        m_kWidget.UpdateGS();
    }

    public void ShiftMidTriangle( float fY )
    {
        m_fScale = fY;
        Vector3f kPos0 = m_kBottomOutline.VBuffer.GetPosition3(0);
        Vector3f kPos1 = m_kBottomOutline.VBuffer.GetPosition3(1);
        Vector3f kPos2 = m_kBottomOutline.VBuffer.GetPosition3(2);
        float fNewX = kPos0.X;
        float fNewY = kPos0.Y;
        m_kTopOutline.VBuffer.SetPosition3(0, fNewX, fNewY, 0.11f);

        fNewX = fY * kPos1.X + (1.0f - fY) * kPos0.X;
        fNewY = fY * kPos1.Y + (1.0f - fY) * kPos0.Y;
        m_kTopOutline.VBuffer.SetPosition3(1, fNewX, fNewY, 0.11f);

        fNewX = fY * kPos2.X + (1.0f - fY) * kPos0.X;
        fNewY = fY * kPos2.Y + (1.0f - fY) * kPos0.Y;
        m_kTopOutline.VBuffer.SetPosition3(2, fNewX, fNewY, 0.11f);
        ScaleTriangle( m_kMiddleSphere, m_kTopTri, m_kTopOutline );

        m_kLowerSphere.Local.SetTranslate(m_kBottomOutline.VBuffer.GetPosition3(0));
        m_kTopOutline.VBuffer.Release();
        m_kTopTri.VBuffer.Release();
        m_kMiddleSphere.VBuffer.Release();
        m_kWidget.UpdateGS();
    }


    public void ScaleTriangle( TriMesh kUpperSphere, TriMesh kTri, TriMesh kOutline )
    {
        Vector3f kPos0 = kOutline.VBuffer.GetPosition3(0);
        Vector3f kPos1 = kOutline.VBuffer.GetPosition3(1);
        Vector3f kPos2 = kOutline.VBuffer.GetPosition3(2);
        float fTransX = (kPos0.X + kPos1.X + kPos2.X)/3.0f;
        float fTransY = (kPos0.Y + kPos1.Y + kPos2.Y)/3.0f;

        float fNewX = (kPos0.X - fTransX) * .90f + fTransX;
        float fNewY = (kPos0.Y - fTransY) * .90f + fTransY;
        kTri.VBuffer.SetPosition3(0, fNewX, fNewY, 0.11f);
        float fTX = (fNewX + 1)/2.0f;
        float fTY = (fNewY + 1)/2.0f;
        fTX = m_kTMin.X + fTX * (m_kTMax.X - m_kTMin.X);
        fTY = m_kTMin.Y + fTY * (m_kTMax.Y - m_kTMin.Y);
        kTri.VBuffer.SetTCoord2(0, 0, fTX, fTY);


        fNewX = (kPos1.X - fTransX) * .90f + fTransX;
        fNewY = (kPos1.Y - fTransY) * .90f + fTransY;
        kTri.VBuffer.SetPosition3(1, fNewX, fNewY, 0.11f);
        fTX = (fNewX + 1)/2.0f;
        fTY = (fNewY + 1)/2.0f;
        fTX = m_kTMin.X + fTX * (m_kTMax.X - m_kTMin.X);
        fTY = m_kTMin.Y + fTY * (m_kTMax.Y - m_kTMin.Y);
        kTri.VBuffer.SetTCoord2(0, 1, fTX, fTY);

        fNewX = (kPos2.X - fTransX) * .90f + fTransX;
        fNewY = (kPos2.Y - fTransY) * .90f + fTransY;
        kTri.VBuffer.SetPosition3(2, fNewX, fNewY, 0.11f);
        fTX = (fNewX + 1)/2.0f;
        fTY = (fNewY + 1)/2.0f;
        fTX = m_kTMin.X + fTX * (m_kTMax.X - m_kTMin.X);
        fTY = m_kTMin.Y + fTY * (m_kTMax.Y - m_kTMin.Y);
        kTri.VBuffer.SetTCoord2(0, 2, fTX, fTY);

        kUpperSphere.Local.SetTranslate(kPos1);
        m_kWidget.UpdateGS();
    }

}
