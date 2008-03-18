package gov.nih.mipav.view.renderer.WildMagic;

import gov.nih.mipav.MipavCoordinateSystems;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;

/**
 * Displays the three orthogonal planes with the volume data.
 * @see GPUVolumeRender.java
 * @see VolumePlaneEffect.java
 * @see VolumeObject.java
 */
public class VolumeSlices extends VolumeObject
{
    /** Create a new VolumeObject with the VolumeImage parameter.
     * @param kImageA, the VolumeImage containing shared data and textures for
     * rendering.
     * @param kTranslate, translation in the scene-graph for this object.
     * @param fX, the size of the volume in the x-dimension (extent * resolutions)
     * @param fY, the size of the volume in the y-dimension (extent * resolutions)
     * @param fZ, the size of the volume in the z-dimension (extent * resolutions)
     */
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

    /**
     * PreRender the object, for embedding in the ray-cast volume.
     * @param kRenderer, the OpenGLRenderer object.
     * @param kCuller, the Culler object.
     */
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

    /**
     * Render the object.
     * @param kRenderer, the OpenGLRenderer object.
     * @param kCuller, the Culler object.
     */
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

    /** Creates the scene graph. */
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

    /** Creates the bounding frames for the planes. */
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

        System.err.println( "Bounding box " + m_fX + " " + m_fY + " " + m_fZ );
        
        m_akBoundingBox = new Polyline[3];
        for ( int i = 0; i < 3; i++ )
        {
            m_akBoundingBox[i] = new Polyline( new VertexBuffer(akOutlineSquare[i]), true, true );
            m_akBoundingBox[i].AttachEffect( m_kVertexColor3Shader );
            m_akBoundingBox[i].Local.SetTranslate(m_kTranslate);
            m_kScene.AttachChild(m_akBoundingBox[i]);
        }
    }

    /** Sets the opacity for the given plane.
     * @param i, the plane index (0-3) in file coordinates.
     * @param fAlpha, the opacity for the given plane.
     */
    public void SetSliceOpacity( int i, float fAlpha )
    {
        int iIndex = MipavCoordinateSystems.fileToModel(i, m_kVolumeImageA.GetImage() );
        m_akPlaneEffect[iIndex].Blend( fAlpha );
    }

    /** Sets the bounding box color for the given plane.
     * @param i, the plane index (0-3) in file coordinates.
     * @param kColor, the new color.
     */
    public void SetBoundingBoxColor( int i, ColorRGB kColor )
    {
        int iIndex = MipavCoordinateSystems.fileToModel(i, m_kVolumeImageA.GetImage() );
        for ( int j = 0; j < 4; j++ )
        {
            m_akBoundingBox[iIndex].VBuffer.SetColor3(0, j, kColor );
        }
        m_akBoundingBox[iIndex].VBuffer.Release();
    }

    /** Turns on/off displaying the bounding box for the given plane.
     * @param i, the plane index (0-3) in file coordinates.
     * @param bShow, when true, the bounding box is displayed.
     */
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

    /** Turns on/off displaying the given plane.
     * @param i, the plane index (0-3) in file coordinates.
     * @param bShow, when true, the plane is displayed.
     */
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

    /** Sets the positions of the three orthogonal planes. 
     * @param kCenter, the positions in file coordinates.
     */
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

    /** Delete local memory. */
    public void dispose()
    {
        if ( m_kVertexColor3Shader != null )
        {
            m_kVertexColor3Shader.dispose();
            m_kVertexColor3Shader = null;
        }
        for ( int i = 0; i < 3; i++ )
        {
            if ( m_akPlaneEffect != null )
            {
                if ( m_akPlaneEffect[i] != null )
                {
                    m_akPlaneEffect[i].dispose();
                    m_akPlaneEffect[i] = null;
                }
                m_akPlaneEffect = null;
            }
            if ( m_akPlanes != null )
            {
                if ( m_akPlanes[i] != null )
                {
                    m_akPlanes[i].dispose();
                    m_akPlanes[i] = null;
                }
                m_akPlanes = null;
            }
            if ( m_akBoundingBox != null )
            {
                if ( m_akBoundingBox[i] != null )
                {
                    m_akBoundingBox[i].dispose();
                    m_akBoundingBox[i] = null;
                }
                m_akBoundingBox = null;
            }
            if ( m_akColors != null )
            {
                if ( m_akColors[i] != null )
                {
                    m_akColors[i].dispose();
                    m_akColors[i] = null;
                }
                m_akColors = null;
            }
        }
        m_abShowPlanes = null;
        m_abShowBoundingBox = null;

    }

    /** ShaderEffect for the plane bounding-boxes. */
    private VertexColor3Effect m_kVertexColor3Shader;
    /** ShaderEffects for the planes. Each is unique so they can have different alpha values. */
    private VolumePlaneEffect[] m_akPlaneEffect;
    /** The three orthogonal plane TriMeshes. */
    private TriMesh[] m_akPlanes;
    /** Displaying each plane: */
    private boolean[] m_abShowPlanes = new boolean[]{true,true,true};
    /** The three plane bounding-box Polylines. */
    private Polyline[] m_akBoundingBox;
    /** Displaying each bounding-box: */
    private boolean[] m_abShowBoundingBox = new boolean[]{true,true,true};

    /** Set of colors used to draw the X and Y Bars and the Z box:. */
    private ColorRGB[] m_akColors = { new ColorRGB(1, 0, 0), new ColorRGB(0, 1, 0), new ColorRGB(1, 1, 0) };
}
