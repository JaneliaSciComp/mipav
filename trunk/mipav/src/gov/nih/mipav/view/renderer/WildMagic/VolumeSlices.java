package gov.nih.mipav.view.renderer.WildMagic;

import gov.nih.mipav.MipavCoordinateSystems;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;

public class VolumeSlices extends VolumeObject
{
    public VolumeSlices ( VolumeImage kImageA, Vector3f kTranslate, float fX, float fY, float fZ )
    {
        super(kImageA,kTranslate,fX,fY,fZ);

        m_akPlaneEffect = new VolumePlaneEffect[3];
        for ( int i = 0; i < 3; i++ )
        {
            m_akPlaneEffect[i] = new VolumePlaneEffect( m_kVolumeImageA, true );
        }
        m_kVertexColor3Shader = new VertexColor3Effect();

        CreatePlanes( );
        CreateBoundingBox();
        for ( int i = 0; i < 3; i++ )
        {
            SetBoundingBoxColor( i, m_akColors[i] );
        }
        SetCenter(new Vector3f( .5f, .5f, .5f ) );
        m_kScene.UpdateGS();
        m_kScene.UpdateRS();
    }

    public void PreRender( Renderer kRenderer, Culler kCuller )
    {
        if ( !m_bDisplay )
        {
            return;
        }
        for ( int i = 0; i < 3; i++ )
        {
            m_akPlanes[i].DetachAllEffects();
            m_akPlanes[i].AttachEffect( m_kVertexColor3Shader );
        }
        m_kScene.UpdateGS();
        kCuller.ComputeVisibleSet(m_kScene);
        kRenderer.DrawScene(kCuller.GetVisibleSet());
    }

    public void Render( Renderer kRenderer, Culler kCuller )
    {
        if ( !m_bDisplay )
        {
            return;
        }
        for ( int i = 0; i < 3; i++ )
        {
            m_akPlanes[i].DetachAllEffects();
            m_akPlanes[i].AttachEffect( m_akPlaneEffect[i] );
        }
        m_kScene.UpdateGS();
        kCuller.ComputeVisibleSet(m_kScene);
        kRenderer.DrawScene(kCuller.GetVisibleSet());
    }

    private void CreatePlanes ( )
    {
        m_kScene = new Node();

        m_kCull = new CullState();
        m_kCull.Enabled = false;
        m_kScene.AttachGlobalState(m_kCull);

        m_kAlpha = new AlphaState();
        m_kAlpha.BlendEnabled = true;
        m_kScene.AttachGlobalState(m_kAlpha);

        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetCChannels(0,3);
        kAttr.SetTChannels(0,3);

        StandardMesh kSM = new StandardMesh(kAttr);
        m_akPlanes = new TriMesh[3];
        for ( int i = 0; i < 3; i++ )
        {
            m_akPlanes[i] = kSM.Rectangle(2,2,1.0f,1.0f);
            m_akPlanes[i].Local.SetTranslate(m_kTranslate);
            m_kScene.AttachChild(m_akPlanes[i]);
        }
    }

    private void CreateBoundingBox ( )
    {
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetCChannels(0,3);

        float fX = m_fX * .5f;
        float fY = m_fY * .5f;
        float fZ = m_fZ * .5f;
        
        VertexBuffer[] akOutlineSquare = new VertexBuffer[3];
        for ( int i = 0; i < 3; i++ )
        {
            akOutlineSquare[i] = new VertexBuffer(kAttr, 4 );
            for ( int j = 0; j < 4; j++ )
            {
                akOutlineSquare[i].SetColor3( 0, j, m_akColors[i] );
            }
        }

        akOutlineSquare[0].SetPosition3( 0, fX, 0, 0 ) ;
        akOutlineSquare[0].SetPosition3( 1, fX, 0, m_fZ ) ;
        akOutlineSquare[0].SetPosition3( 2, fX, m_fY, m_fZ ) ;
        akOutlineSquare[0].SetPosition3( 3, fX, m_fY, 0 ) ;

        akOutlineSquare[1].SetPosition3( 0, m_fX, fY, m_fZ ) ;
        akOutlineSquare[1].SetPosition3( 1, 0, fY, m_fZ ) ;
        akOutlineSquare[1].SetPosition3( 2, 0, fY, 0 ) ;
        akOutlineSquare[1].SetPosition3( 3, m_fX, fY, 0 ) ;

        akOutlineSquare[2].SetPosition3( 0, m_fX, 0, fZ ) ;
        akOutlineSquare[2].SetPosition3( 1, 0, 0, fZ ) ;
        akOutlineSquare[2].SetPosition3( 2, 0, m_fY, fZ ) ;
        akOutlineSquare[2].SetPosition3( 3, m_fX, m_fY, fZ ) ;

        m_akBoundingBox = new Polyline[3];
        for ( int i = 0; i < 3; i++ )
        {
            m_akBoundingBox[i] = new Polyline( new VertexBuffer(akOutlineSquare[i]), true, true );
            m_akBoundingBox[i].AttachEffect( m_kVertexColor3Shader );
            m_akBoundingBox[i].Local.SetTranslate(m_kTranslate);
            m_kScene.AttachChild(m_akBoundingBox[i]);
        }
    }

    public void SetSliceOpacity( int i, float fAlpha )
    {
        int iIndex = MipavCoordinateSystems.fileToModel(i, m_kVolumeImageA.GetImage() );
        m_akPlaneEffect[iIndex].Blend( fAlpha );
    }

    public void SetBoundingBoxColor( int i, ColorRGB kColor )
    {
        int iIndex = MipavCoordinateSystems.fileToModel(i, m_kVolumeImageA.GetImage() );
        for ( int j = 0; j < 4; j++ )
        {
            m_akBoundingBox[iIndex].VBuffer.SetColor3(0, j, kColor );
        }
        m_akBoundingBox[iIndex].VBuffer.Release();
    }

    public void ShowBoundingBox( int i, boolean bShow )
    {
        int iIndex = MipavCoordinateSystems.fileToModel(i, m_kVolumeImageA.GetImage() );
        if ( m_abShowBoundingBox[iIndex] == bShow )
        {
            return;
        }
        m_abShowBoundingBox[iIndex] = bShow;
        if ( m_abShowBoundingBox[iIndex] )
        {
            m_kScene.AttachChild(m_akBoundingBox[iIndex]);
        }
        else
        {
            m_kScene.DetachChild(m_akBoundingBox[iIndex]);
        }
    }

    public void ShowSlice( int i, boolean bShow )
    {
        int iIndex = MipavCoordinateSystems.fileToModel(i, m_kVolumeImageA.GetImage() );
        if ( m_abShowPlanes[iIndex] == bShow )
        {
            return;
        }
        m_abShowPlanes[iIndex] = bShow;
        if ( m_abShowPlanes[iIndex] )
        {
            m_kScene.AttachChild(m_akPlanes[iIndex]);
        }
        else
        {
            m_kScene.DetachChild(m_akPlanes[iIndex]);
        }
    }


    public void SetCenter( Vector3f kCenter )
    {
        float fX = m_fX * kCenter.X();
        float fY = m_fY * kCenter.Y();
        float fZ = m_fZ * kCenter.Z();

        float fTCX = kCenter.X();
        float fTCY = kCenter.Y();
        float fTCZ = kCenter.Z();
        
        m_akPlanes[0].VBuffer.SetPosition3( 0, fX, 0, 0 ) ;
        m_akPlanes[0].VBuffer.SetPosition3( 1, fX, 0, m_fZ ) ;
        m_akPlanes[0].VBuffer.SetPosition3( 2, fX, m_fY, 0 ) ;
        m_akPlanes[0].VBuffer.SetPosition3( 3, fX, m_fY, m_fZ ) ;

        m_akPlanes[0].VBuffer.SetColor3( 0, 0, fTCX, 0, 0 ) ;
        m_akPlanes[0].VBuffer.SetColor3( 0, 1, fTCX, 0, 1.0f ) ;
        m_akPlanes[0].VBuffer.SetColor3( 0, 2, fTCX, 1.0f, 0 ) ;
        m_akPlanes[0].VBuffer.SetColor3( 0, 3, fTCX, 1.0f, 1.0f ) ;

        m_akPlanes[0].VBuffer.SetTCoord3( 0, 0, fTCX, 0, 0 ) ;
        m_akPlanes[0].VBuffer.SetTCoord3( 0, 1, fTCX, 0, 1.0f ) ;
        m_akPlanes[0].VBuffer.SetTCoord3( 0, 2, fTCX, 1.0f, 0 ) ;
        m_akPlanes[0].VBuffer.SetTCoord3( 0, 3, fTCX, 1.0f, 1.0f ) ;

        // Y-Slice:
        m_akPlanes[1].VBuffer.SetPosition3( 0, 0, fY, 0 ) ;
        m_akPlanes[1].VBuffer.SetPosition3( 1, 0, fY, m_fZ ) ;
        m_akPlanes[1].VBuffer.SetPosition3( 2, m_fX, fY, 0 ) ;
        m_akPlanes[1].VBuffer.SetPosition3( 3, m_fX, fY, m_fZ ) ;

        m_akPlanes[1].VBuffer.SetColor3( 0, 0, 0, fTCY, 0 ) ;
        m_akPlanes[1].VBuffer.SetColor3( 0, 1, 0, fTCY, 1.0f ) ;
        m_akPlanes[1].VBuffer.SetColor3( 0, 2, 1.0f, fTCY, 0 ) ;
        m_akPlanes[1].VBuffer.SetColor3( 0, 3, 1.0f, fTCY, 1.0f ) ;

        m_akPlanes[1].VBuffer.SetTCoord3( 0, 0, 0, fTCY, 0 ) ;
        m_akPlanes[1].VBuffer.SetTCoord3( 0, 1, 0, fTCY, 1.0f ) ;
        m_akPlanes[1].VBuffer.SetTCoord3( 0, 2, 1.0f, fTCY, 0 ) ;
        m_akPlanes[1].VBuffer.SetTCoord3( 0, 3, 1.0f, fTCY, 1.0f ) ;

        // Z-Slice:
        m_akPlanes[2].VBuffer.SetPosition3( 0, 0, 0, fZ ) ;
        m_akPlanes[2].VBuffer.SetPosition3( 1, m_fX, 0, fZ ) ;
        m_akPlanes[2].VBuffer.SetPosition3( 2, 0, m_fY, fZ ) ;
        m_akPlanes[2].VBuffer.SetPosition3( 3, m_fX, m_fY, fZ ) ;

        m_akPlanes[2].VBuffer.SetColor3( 0, 0, 0, 0, fTCZ ) ;
        m_akPlanes[2].VBuffer.SetColor3( 0, 1, 1.0f, 0, fTCZ ) ;
        m_akPlanes[2].VBuffer.SetColor3( 0, 2, 0, 1.0f, fTCZ ) ;
        m_akPlanes[2].VBuffer.SetColor3( 0, 3, 1.0f, 1.0f, fTCZ ) ;
            
        m_akPlanes[2].VBuffer.SetTCoord3( 0, 0, 0, 0, fTCZ ) ;
        m_akPlanes[2].VBuffer.SetTCoord3( 0, 1, 1.0f, 0, fTCZ ) ;
        m_akPlanes[2].VBuffer.SetTCoord3( 0, 2, 0, 1.0f, fTCZ ) ;
        m_akPlanes[2].VBuffer.SetTCoord3( 0, 3, 1.0f, 1.0f, fTCZ ) ;
        


        //Bounding Boxes:
        m_akBoundingBox[0].VBuffer.SetPosition3( 0, fX, 0, 0 ) ;
        m_akBoundingBox[0].VBuffer.SetPosition3( 1, fX, 0, m_fZ ) ;
        m_akBoundingBox[0].VBuffer.SetPosition3( 2, fX, m_fY, m_fZ ) ;
        m_akBoundingBox[0].VBuffer.SetPosition3( 3, fX, m_fY, 0 ) ;

        m_akBoundingBox[1].VBuffer.SetPosition3( 0, m_fX, fY, m_fZ ) ;
        m_akBoundingBox[1].VBuffer.SetPosition3( 1, 0, fY, m_fZ ) ;
        m_akBoundingBox[1].VBuffer.SetPosition3( 2, 0, fY, 0 ) ;
        m_akBoundingBox[1].VBuffer.SetPosition3( 3, m_fX, fY, 0 ) ;

        m_akBoundingBox[2].VBuffer.SetPosition3( 0, m_fX, 0, fZ ) ;
        m_akBoundingBox[2].VBuffer.SetPosition3( 1, 0, 0, fZ ) ;
        m_akBoundingBox[2].VBuffer.SetPosition3( 2, 0, m_fY, fZ ) ;
        m_akBoundingBox[2].VBuffer.SetPosition3( 3, m_fX, m_fY, fZ ) ;

        for ( int i = 0; i < 3; i++ )
        {
            m_akPlanes[i].VBuffer.Release();
            m_akBoundingBox[i].VBuffer.Release();
        }
    }


    private VertexColor3Effect m_kVertexColor3Shader;
    private VolumePlaneEffect[] m_akPlaneEffect;
    private TriMesh[] m_akPlanes;
    private boolean[] m_abShowPlanes = new boolean[]{true,true,true};
    private Polyline[] m_akBoundingBox;
    private boolean[] m_abShowBoundingBox = new boolean[]{true,true,true};

    private CullState m_kCull;
    private AlphaState m_kAlpha;

    /** Set of colors used to draw the X and Y Bars and the Z box:. */
    private ColorRGB[] m_akColors = { new ColorRGB(1, 0, 0), new ColorRGB(0, 1, 0), new ColorRGB(1, 1, 0) };


}
