package gov.nih.mipav.view.renderer.WildMagic.Render;


import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.Rendering.Renderer;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Culler;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

/** Displays the BoundingBox frame around the volume data in the VolumeViewer.
 * @see VolumeObject.java
 * @see GPUVolumeRender.java
 */
public class VolumeBoundingBox extends VolumeObject
{
    /** The bounding box Polyline array. */
    private TriMesh[] m_akBoundingBox;

    /** The Pre - ShaderEffect for the bounding box. */
    private VertexColor3Effect m_kVertexColor3Shader;
    /** The ShaderEffect for the bounding box. */
    private BoundingBoxEffect[] m_akBoundingBoxEffect;

    /** Creates a new bounding box object.
     * @param kImageA the VolumeImage containing shared data and textures for
     * rendering.
     * @param kTranslate translation in the scene-graph for this object.
     * @param fX the size of the volume in the x-dimension (extent * resolutions)
     * @param fY the size of the volume in the y-dimension (extent * resolutions)
     * @param fZ the size of the volume in the z-dimension (extent * resolutions)
     */
    public VolumeBoundingBox ( VolumeImage kImageA, Vector3f kTranslate, float fX, float fY, float fZ )
    {
        super(kImageA,kTranslate,fX,fY,fZ);

        m_kVertexColor3Shader = new VertexColor3Effect();

        CreateBox();
        m_kScene.UpdateGS();
        m_kScene.UpdateRS();
    }

    /** Delete local memory. */
    public void dispose(Renderer kRenderer)
    {
        for ( int i = 0; i < 6; i++ )
        {
        	kRenderer.ReleaseVBuffer(m_akBoundingBox[i].VBuffer);
        	kRenderer.ReleaseIBuffer(m_akBoundingBox[i].IBuffer);
            m_akBoundingBox[i].dispose();
            m_akBoundingBox[i] = null;
        }
        m_akBoundingBox = null;
        if ( m_kVertexColor3Shader != null )
        {
        	kRenderer.ReleaseResources(m_kVertexColor3Shader);
            m_kVertexColor3Shader.dispose();
            m_kVertexColor3Shader = null;
        }
        super.dispose(kRenderer);
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeObject#Render(WildMagic.LibGraphics.Rendering.Renderer, WildMagic.LibGraphics.SceneGraph.Culler)
     */
    public void Render( Renderer kRenderer, Culler kCuller, boolean bPreRender, boolean bSolid )
    {
        if ( !m_bDisplay || !bSolid )
        {
            return;
        }
    	m_kScene.UpdateGS();
    	kCuller.ComputeVisibleSet(m_kScene);
        if ( bPreRender )
        {
            for ( int i = 0; i < 6; i++ )
            {
                m_akBoundingBox[i].DetachAllEffects( );
                m_akBoundingBox[i].AttachEffect( m_kVertexColor3Shader );
            }
            
            // Cull front-facing polygons:
            m_kCull.CullFace = CullState.CullMode.CT_FRONT;
            //kRenderer.DrawScene(kCuller.GetVisibleSet());
            // Undo culling:
            m_kCull.CullFace = CullState.CullMode.CT_BACK;
        }
        else
        {
            for ( int i = 0; i < 6; i++ )
            {
                m_akBoundingBox[i].DetachAllEffects( );
                m_akBoundingBox[i].AttachEffect( m_akBoundingBoxEffect[i] );
            }
            m_kCull.CullFace = CullState.CullMode.CT_FRONT;
        	kRenderer.DrawScene(kCuller.GetVisibleSet());
            m_kCull.CullFace = CullState.CullMode.CT_BACK;
        }
    }
    /**
     * Called from JPanelDisplay. Sets the bounding box color.
     * @param kColor bounding box color.
     */
    public void SetBoundingBoxColor( ColorRGB kColor )
    {
        for ( int i = 0; i < 6; i++ )
        {
            m_akBoundingBoxEffect[i].setColor( kColor );
        }
    }
    
    /**
     * Sets the background color.
     * @param kColor new background color.
     */
    public void SetBackgroundColor( ColorRGBA kColor )
    {
        for ( int i = 0; i < 6; i++ )
        {
            m_akBoundingBoxEffect[i].setBackground( kColor );
        }
    } 
    /*
	public void toggleMethod()
	{
        for ( int i = 0; i < 6; i++ )
        {
            m_akBoundingBoxEffect[i].toggleMethod( );
        }
	}*/
    
    /** Creates the bounding box Polylines. */
    private void CreateBox()
    {
        m_kScene = new Node();
        m_kCull = new CullState();
        m_kScene.AttachGlobalState(m_kCull);

        m_kAlpha = new AlphaState();
        m_kAlpha.BlendEnabled = true;
        m_kScene.AttachGlobalState(m_kAlpha);

        m_akBoundingBox = new TriMesh[6];
        IndexBuffer kIndexBuffer = new IndexBuffer(6);
        int[] aiIndexData = kIndexBuffer.GetData();
        aiIndexData[0] = 0;
        aiIndexData[1] = 1;
        aiIndexData[2] = 2;
        aiIndexData[3] = 0;
        aiIndexData[4] = 2;
        aiIndexData[5] = 3;

        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);
        kAttributes.SetTChannels(0,3);

        VertexBuffer[] akOutlineSquare = new VertexBuffer[6];
        for ( int i = 0; i < 6; i++ )
        {
            akOutlineSquare[i] = new VertexBuffer(kAttributes, 4 );
            for ( int j = 0; j < 4; j++ )
            {
                akOutlineSquare[i].SetColor3( 0, j, 1, 0, 0 ) ;
            }
        }
        m_akBoundingBoxEffect = new BoundingBoxEffect[6];
        
        //System.err.println( m_fX + " " + m_fY + " " + m_fZ );
        float[] afOrigin = m_kVolumeImageA.GetImage().getOrigin();
        Vector3f origin = new Vector3f( afOrigin[0], afOrigin[1], afOrigin[2] );
        int[] aiExtents = m_kVolumeImageA.GetImage().getExtents();
        int midZ = aiExtents.length > 2 ? aiExtents[2]/2 : 0;
        float[] afRes = m_kVolumeImageA.GetImage().getResolutions(midZ);
        Vector3f range = new Vector3f( aiExtents[0] * afRes[0],
        		aiExtents[1] * afRes[1],
        		aiExtents[2] * afRes[2]);
        //System.err.println( range );
        // neg x polyline:
        akOutlineSquare[0].SetPosition3( 0, 0, 0, 0 ) ;
        akOutlineSquare[0].SetPosition3( 1, 0, 0, m_fZ ) ;
        akOutlineSquare[0].SetPosition3( 2, 0, m_fY, m_fZ ) ;
        akOutlineSquare[0].SetPosition3( 3, 0, m_fY, 0 ) ;
        akOutlineSquare[0].SetTCoord3( 0, 0, 0, 0, 0 ) ;
        akOutlineSquare[0].SetTCoord3( 0, 1, 0, 0, 1 ) ;
        akOutlineSquare[0].SetTCoord3( 0, 2, 0, 1, 1 ) ;
        akOutlineSquare[0].SetTCoord3( 0, 3, 0, 1, 0 ) ;
        m_akBoundingBoxEffect[0] = new BoundingBoxEffect( origin, range,
        		new Vector3f( m_fX, m_fY, m_fZ ), new Vector3f( -1, 0, 0) );

        // pos x polyline:
        akOutlineSquare[1].SetPosition3( 0, m_fX, 0, m_fZ ) ;
        akOutlineSquare[1].SetPosition3( 1, m_fX, 0, 0 ) ;
        akOutlineSquare[1].SetPosition3( 2, m_fX, m_fY, 0 ) ;
        akOutlineSquare[1].SetPosition3( 3, m_fX, m_fY, m_fZ ) ;
        akOutlineSquare[1].SetTCoord3( 0, 0, 1, 0, 1 ) ;
        akOutlineSquare[1].SetTCoord3( 0, 1, 1, 0, 0 ) ;
        akOutlineSquare[1].SetTCoord3( 0, 2, 1, 1, 0 ) ;
        akOutlineSquare[1].SetTCoord3( 0, 3, 1, 1, 1 ) ;
        m_akBoundingBoxEffect[1] = new BoundingBoxEffect( origin, range,
        		new Vector3f( m_fX, m_fY, m_fZ ), new Vector3f( 1, 0, 0) );

        // neg y polyline:
        akOutlineSquare[2].SetPosition3( 0, m_fX, 0, m_fZ ) ;
        akOutlineSquare[2].SetPosition3( 1, 0, 0, m_fZ ) ;
        akOutlineSquare[2].SetPosition3( 2, 0, 0, 0 ) ;
        akOutlineSquare[2].SetPosition3( 3, m_fX, 0, 0 ) ;
        akOutlineSquare[2].SetTCoord3( 0, 0, 1, 0, 1 ) ;
        akOutlineSquare[2].SetTCoord3( 0, 1, 0, 0, 1 ) ;
        akOutlineSquare[2].SetTCoord3( 0, 2, 0, 0, 0 ) ;
        akOutlineSquare[2].SetTCoord3( 0, 3, 1, 0, 0 ) ;
        m_akBoundingBoxEffect[2] = new BoundingBoxEffect( origin, range,
        		new Vector3f( m_fX, m_fY, m_fZ ), new Vector3f( 0, -1, 0) );
        
        
        // pos y polyline:
        akOutlineSquare[3].SetPosition3( 0, m_fX, m_fY, 0 ) ;
        akOutlineSquare[3].SetPosition3( 1, 0, m_fY, 0 ) ;
        akOutlineSquare[3].SetPosition3( 2, 0, m_fY, m_fZ ) ;
        akOutlineSquare[3].SetPosition3( 3, m_fX, m_fY, m_fZ ) ;
        akOutlineSquare[3].SetTCoord3( 0, 0, 1, 1, 0 ) ;
        akOutlineSquare[3].SetTCoord3( 0, 1, 0, 1, 0 ) ;
        akOutlineSquare[3].SetTCoord3( 0, 2, 0, 1, 1 ) ;
        akOutlineSquare[3].SetTCoord3( 0, 3, 1, 1, 1 ) ;
        m_akBoundingBoxEffect[3] = new BoundingBoxEffect( origin, range,
        		new Vector3f( m_fX, m_fY, m_fZ ), new Vector3f( 0, 1, 0) );

        // neg z polyline:
        akOutlineSquare[4].SetPosition3( 0, m_fX, 0, 0 ) ;
        akOutlineSquare[4].SetPosition3( 1, 0, 0, 0 ) ;
        akOutlineSquare[4].SetPosition3( 2, 0, m_fY, 0 ) ;
        akOutlineSquare[4].SetPosition3( 3, m_fX, m_fY, 0 ) ;
        akOutlineSquare[4].SetTCoord3( 0, 0, 1, 0, 0 ) ;
        akOutlineSquare[4].SetTCoord3( 0, 1, 0, 0, 0 ) ;
        akOutlineSquare[4].SetTCoord3( 0, 2, 0, 1, 0 ) ;
        akOutlineSquare[4].SetTCoord3( 0, 3, 1, 1, 0 ) ;
        m_akBoundingBoxEffect[4] = new BoundingBoxEffect( origin, range,
        		new Vector3f( m_fX, m_fY, m_fZ ), new Vector3f( 0, 0, -1) );

        // pos z polyline:
        akOutlineSquare[5].SetPosition3( 0, 0, 0, m_fZ ) ;
        akOutlineSquare[5].SetPosition3( 1, m_fX, 0, m_fZ ) ;
        akOutlineSquare[5].SetPosition3( 2, m_fX, m_fY, m_fZ ) ;
        akOutlineSquare[5].SetPosition3( 3, 0, m_fY, m_fZ ) ;
        akOutlineSquare[5].SetTCoord3( 0, 0, 0, 0, 1 ) ;
        akOutlineSquare[5].SetTCoord3( 0, 1, 1, 0, 1 ) ;
        akOutlineSquare[5].SetTCoord3( 0, 2, 1, 1, 1 ) ;
        akOutlineSquare[5].SetTCoord3( 0, 3, 0, 1, 1 ) ;
        m_akBoundingBoxEffect[5] = new BoundingBoxEffect( origin, range,
        		new Vector3f( m_fX, m_fY, m_fZ ), new Vector3f( 0, 0, 1) );

        for ( int i = 0; i < 6; i++ )
        {
            m_akBoundingBox[i] = new TriMesh( akOutlineSquare[i], kIndexBuffer );
            m_akBoundingBox[i].AttachEffect( m_kVertexColor3Shader );
            m_akBoundingBox[i].Local.SetTranslate(m_kTranslate);
            m_kScene.AttachChild(m_akBoundingBox[i]);
        }
        
        //System.err.println( "BoundingBox" );
        //System.err.println( m_fX + " " + m_fY + " " + m_fZ );
        //System.err.println( m_kTranslate );
    }
}
