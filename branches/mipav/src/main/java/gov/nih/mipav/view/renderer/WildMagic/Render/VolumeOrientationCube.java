package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.model.file.FileInfoBase;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.TextureEffect;
import WildMagic.LibGraphics.Rendering.Renderer;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Culler;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

/** Displays the Orientation Cube in the VolumeViewer.
 * @see VolumeObject.java
 * @see GPUVolumeRender.java
 */
public class VolumeOrientationCube extends VolumeObject
{
    /** Orientation cube geometry: */
    private TriMesh[] m_akOrientationCube;

    /** Orientation cube texture names: */
    private String[] m_aakAxisFiles = new String[]{ "u", "u", "u", "u", "u", "u"};

    /** Orientation cube translation offset: */
    private Vector3f m_kCubeTranslate = Vector3f.ZERO;

    /** Creates a new VolumeOrientationCube object.
     * @param kImageA the VolumeImage containing shared data and textures for
     * rendering.
     * @param kTranslate translation in the scene-graph for this object.
     * @param fX the size of the volume in the x-dimension (extent * resolutions)
     * @param fY the size of the volume in the y-dimension (extent * resolutions)
     * @param fZ the size of the volume in the z-dimension (extent * resolutions)
     */
    public VolumeOrientationCube ( VolumeImage kImageA, Vector3f kTranslate, float fX, float fY, float fZ )
    {
        super(kImageA,kTranslate,fX,fY,fZ);

        CreateCube();
        m_kScene.UpdateGS();
        m_kScene.UpdateRS();
    }
    
    /** Delete local memory. */
    public void dispose(Renderer kRenderer)
    {
    	if (m_akOrientationCube != null)
    	{
    		for ( int i = 0; i < m_akOrientationCube.length; i++ )
    		{
            	kRenderer.ReleaseVAO( m_akOrientationCube[i] );
    			//kRenderer.ReleaseVBuffer(m_akOrientationCube[i].VBuffer);
    			//kRenderer.ReleaseIBuffer(m_akOrientationCube[i].IBuffer);
    			m_akOrientationCube[i].dispose();
    		}
    	}
        m_akOrientationCube = null;

        m_aakAxisFiles = null;
        m_kCubeTranslate = null;
        super.dispose(kRenderer);
    }
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeObject#Render(WildMagic.LibGraphics.Rendering.Renderer, WildMagic.LibGraphics.SceneGraph.Culler)
     */
    public void Render( Renderer kRenderer, Culler kCuller, boolean bPreRender, boolean bSolid )
    {
        if ( !m_bDisplay || bPreRender || !bSolid )
        {
            return;
        }
        for ( int i = 0; i < 6; i++ )
        {
            m_akOrientationCube[i].Local.SetRotateCopy(m_kScene.Local.GetRotate());
            m_akOrientationCube[i].UpdateGS();
            m_akOrientationCube[i].UpdateRS();
            kRenderer.Draw(m_akOrientationCube[i]);
        }
    }
    /** Creates the orientation cube. */
    private void CreateCube()
    {
        m_kScene = new Node();
        
        m_akOrientationCube = new TriMesh[6];
        IndexBuffer kIndexBuffer = new IndexBuffer(6);
        int[] aiIndexData = kIndexBuffer.GetData();
        aiIndexData[0] = 0;
        aiIndexData[1] = 1;
        aiIndexData[2] = 2;
        aiIndexData[3] = 0;
        aiIndexData[4] = 2;
        aiIndexData[5] = 3;

        InitCubicTextures();

        m_kCubeTranslate = new Vector3f( -1.5f, 1f, 1.5f );
        Vector3f kCubeScale = new Vector3f( .5f, .5f, .5f );

        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);
        kAttributes.SetTChannels(0,2);

        VertexBuffer[] akOutlineSquare = new VertexBuffer[6];
        for ( int i = 0; i < 6; i++ )
        {
            akOutlineSquare[i] = new VertexBuffer(kAttributes, 4 );
            for ( int j = 0; j < 4; j++ )
            {
                akOutlineSquare[i].SetColor3( 0, j, 1, 0, 0 ) ;
            }

            akOutlineSquare[i].SetTCoord2( 0, 0, 1, 1 ) ;
            akOutlineSquare[i].SetTCoord2( 0, 1, 0, 1 ) ;
            akOutlineSquare[i].SetTCoord2( 0, 2, 0, 0 ) ;
            akOutlineSquare[i].SetTCoord2( 0, 3, 1, 0 ) ;
        }
        // neg x clipping:
        akOutlineSquare[0].SetPosition3( 0, 0, 0, 0 ) ;
        akOutlineSquare[0].SetPosition3( 1, 0, 0, m_fZ ) ;
        akOutlineSquare[0].SetPosition3( 2, 0, m_fY, m_fZ ) ;
        akOutlineSquare[0].SetPosition3( 3, 0, m_fY, 0 ) ;

        // pos x clipping:
        akOutlineSquare[1].SetPosition3( 0, m_fX, 0, m_fZ ) ;
        akOutlineSquare[1].SetPosition3( 1, m_fX, 0, 0 ) ;
        akOutlineSquare[1].SetPosition3( 2, m_fX, m_fY, 0 ) ;
        akOutlineSquare[1].SetPosition3( 3, m_fX, m_fY, m_fZ ) ;

        // neg y clipping:
        akOutlineSquare[2].SetPosition3( 0, m_fX, 0, m_fZ ) ;
        akOutlineSquare[2].SetPosition3( 1, 0, 0, m_fZ ) ;
        akOutlineSquare[2].SetPosition3( 2, 0, 0, 0 ) ;
        akOutlineSquare[2].SetPosition3( 3, m_fX, 0, 0 ) ;
        // pos y clipping:
        akOutlineSquare[3].SetPosition3( 0, m_fX, m_fY, 0 ) ;
        akOutlineSquare[3].SetPosition3( 1, 0, m_fY, 0 ) ;
        akOutlineSquare[3].SetPosition3( 2, 0, m_fY, m_fZ ) ;
        akOutlineSquare[3].SetPosition3( 3, m_fX, m_fY, m_fZ ) ;

        // neg z clipping:
        akOutlineSquare[4].SetPosition3( 0, m_fX, 0, 0 ) ;
        akOutlineSquare[4].SetPosition3( 1, 0, 0, 0 ) ;
        akOutlineSquare[4].SetPosition3( 2, 0, m_fY, 0 ) ;
        akOutlineSquare[4].SetPosition3( 3, m_fX, m_fY, 0 ) ;

        // pos z clipping:
        akOutlineSquare[5].SetPosition3( 0, 0, 0, m_fZ ) ;
        akOutlineSquare[5].SetPosition3( 1, m_fX, 0, m_fZ ) ;
        akOutlineSquare[5].SetPosition3( 2, m_fX, m_fY, m_fZ ) ;
        akOutlineSquare[5].SetPosition3( 3, 0, m_fY, m_fZ ) ;

        Vector3f kHalf = new Vector3f( m_fX/2.0f, m_fY/2.0f, m_fZ/2.0f );
        Vector3f kPos = new Vector3f();
        for ( int i = 0; i < 6; i++ )
        {
            for ( int j = 0; j < 4; j++ )
            {
                akOutlineSquare[i].GetPosition3( j, kPos );
                kPos.sub(kHalf);
                akOutlineSquare[i].SetPosition3( j, kPos );
            }
            m_akOrientationCube[i] = new TriMesh( new VertexBuffer(akOutlineSquare[i]), kIndexBuffer );
            m_akOrientationCube[i].AttachEffect( new TextureEffect( m_aakAxisFiles[i] ) );
            m_akOrientationCube[i].Local.SetTranslate(m_kCubeTranslate);
            m_akOrientationCube[i].Local.SetScale(kCubeScale);
            m_akOrientationCube[i].UpdateGS();
            m_akOrientationCube[i].UpdateRS();
        }
        kPos = null;
    }
    /**
     * Create the rotation control cubic box. A cube representing the image
     * orientation, with labels painted on the cube faces showing which axis
     * corresponds to which axis in patient coordinates.
     */
    private void InitCubicTextures() {

        /* Read the axis strings from the FileInfo data structure: */
        String[] akAxisLabels = new String[3];
        int[] axisOrientation = MipavCoordinateSystems.getAxisOrientation(m_kVolumeImageA.GetImage()); 
        if ( axisOrientation != null )
        {
            for ( int i = 0; i < 3; i++ )
            {
                akAxisLabels[i] = FileInfoBase.getAxisOrientationStr( axisOrientation[i] ).toLowerCase();
                /* The file name correspond to the axis strings, read the file
                 * names from the axis strings: */
                m_aakAxisFiles[i*2 +0] = new String( String.valueOf( akAxisLabels[i].charAt(0) ) );
                m_aakAxisFiles[i*2 +1] = new String( String.valueOf( akAxisLabels[i].charAt( akAxisLabels[i].lastIndexOf( " " ) + 1 ) ) );
                //System.err.println(m_aakAxisFiles[i*2 +1]);
            }
        }
    }

}
