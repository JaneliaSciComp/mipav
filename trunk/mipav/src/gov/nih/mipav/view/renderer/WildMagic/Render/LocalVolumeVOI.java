package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.view.renderer.WildMagic.PlaneRender_WM;

import java.io.Serializable;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

public class LocalVolumeVOI implements Serializable
{
    /**  */
    private static final long serialVersionUID = 7912877738874526203L;
    public Vector<String> Name = new Vector<String>();
    public PolylineVector Local = new PolylineVector();
    public PolylineVector Volume = new PolylineVector();
    private Vector3f m_kLocalPt = new Vector3f();
    private Vector3f m_kVolumePt;
    private int m_iCurrent;

    private Vector3f m_kLocalCenter = new Vector3f();
        

        
    public LocalVolumeVOI( PlaneRender_WM parent, LocalVolumeVOI kVOI )
    {
        for ( int i = 0; i < kVOI.Name.size(); i++ )
        {
            Name.add( new String(kVOI.Name.get(i)) );
        }
        for ( int i = 0; i < kVOI.Local.size(); i++ )
        {
            Polyline kLine = new Polyline();
            kLine.Copy(kVOI.Local.get(i));
            Local.add( kLine );
            Volume.add( createVolumePolyline( parent, kLine.VBuffer ) );
        }
        if ( kVOI.m_kLocalPt != null )
        {
            m_kLocalPt = new Vector3f( kVOI.m_kLocalPt );
        }
        if ( kVOI.m_kVolumePt != null )
        {
            m_kVolumePt = new Vector3f( kVOI.m_kVolumePt );
        }
        m_iCurrent = kVOI.m_iCurrent;
    }
        
    public LocalVolumeVOI( PlaneRender_WM parent, Polyline kLocal, String kName )
    {
        Name.add(kName);
        Local.add(kLocal);
        Volume.add( createVolumePolyline( parent, kLocal.VBuffer ) );
        m_iCurrent = 0;
    }

    public void add( PlaneRender_WM parent, Polyline kLocal, String kName  )
    {
        Name.add(kName);
        Local.add(kLocal);
        Volume.add( createVolumePolyline( parent, kLocal.VBuffer ) );
        m_iCurrent++;
    }
        

        
    public void addPoint( float fX, float fY, float fZ )
    {
    }

    public Polyline getLocal( int i )
    {
        return Local.get(i);
    }

    public Polyline getVolume( int i )
    {
        return Volume.get(i);
    }

    public int GetVertexQuantity(int i)
    {
        return Local.get(i).VBuffer.GetVertexQuantity();
    }



    public boolean move( PlaneRender_WM parent, Vector3f kDiff )
    {            
        boolean bUpdateVOI = false;
        kDiff.Sub( m_kLocalCenter );
            
        VertexBuffer kLocalVBuffer = Local.get(0).VBuffer;     
        VertexBuffer kVolumeVBuffer = Volume.get(0).VBuffer;     
        int iNumPoints = kLocalVBuffer.GetVertexQuantity();
        if ( iNumPoints > 0 )
        {
            for ( int i = 0; i < iNumPoints; i++ )
            {
                Vector3f kPos = kLocalVBuffer.GetPosition3( i );
                kPos.Add(kDiff);
                kLocalVBuffer.SetPosition3( i, kPos ) ;

                kPos = parent.VOIToFileCoordinates( kPos, true );
                kVolumeVBuffer.SetPosition3( i, kPos ) ;
            }
            kLocalVBuffer.Release();
            kVolumeVBuffer.Release();
            bUpdateVOI = true;
        }
        return bUpdateVOI;
    }

    public void Release()
    {
        Local.get(m_iCurrent).VBuffer.Release();
        Volume.get(m_iCurrent).VBuffer.Release();
    }

    public void setCenter( float fX, float fY, float fZ )
    {
        m_kLocalCenter.Set( fX, fY, fZ );
    }

    public void SetPosition( PlaneRender_WM parent, int iPos, float fX, float fY, float fZ )
    {
        if ( iPos < Local.get(m_iCurrent).VBuffer.GetVertexQuantity() )
        {
            m_kLocalPt.Set( fX, fY, fZ );
            Local.get(m_iCurrent).VBuffer.SetPosition3( iPos, m_kLocalPt );
            m_kVolumePt = parent.VOIToFileCoordinates( m_kLocalPt, true );
            Volume.get(m_iCurrent).VBuffer.SetPosition3( iPos, m_kVolumePt );
            //System.err.println( iPos + " " + m_kVolumePt.ToString() );
        }
    }

    public int size()
    {
        return Local.size();
    }

    public int slice(int i)
    {
        return (int)Local.get(i).VBuffer.GetPosition3(0).Z;
    }

    public void Update()
    {
        Local.get(m_iCurrent).UpdateGS();
        Local.get(m_iCurrent).UpdateRS();
        Volume.get(m_iCurrent).UpdateGS();
        Volume.get(m_iCurrent).UpdateRS();
    }


    private Polyline createVolumePolyline( PlaneRender_WM parent, VertexBuffer kVBuffer )
    {
        int iNumPoints = kVBuffer.GetVertexQuantity();
        VertexBuffer kVolumeVBuffer = new VertexBuffer( parent.getVOIAttributes(), iNumPoints );
        if ( iNumPoints > 0 )
        {
            for ( int i = 0; i < iNumPoints; i++ )
            {
                Vector3f kPos = kVBuffer.GetPosition3(i);
                kVolumeVBuffer.SetPosition3( i, parent.VOIToFileCoordinates( kPos, true ) );
                kVolumeVBuffer.SetColor3( 0, i, parent.getZColor() );
            }
        }
        Polyline kPoly = new Polyline( kVolumeVBuffer, true, true );    
        kPoly.AttachEffect( new VertexColor3Effect() );
        kPoly.AttachGlobalState(parent.getZState());
        //kPoly.UpdateGS();
        //kPoly.UpdateRS();
        return kPoly;
    }
}
