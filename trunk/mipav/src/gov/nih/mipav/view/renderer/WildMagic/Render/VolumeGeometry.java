package gov.nih.mipav.view.renderer.WildMagic.Render;


import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Effects.*;
import WildMagic.LibGraphics.Rendering.*;
import WildMagic.LibGraphics.SceneGraph.*;

public class VolumeGeometry extends VolumeObject
{
    /** 
     * @param kImageA, the VolumeImage containing shared data and textures for
     * rendering.
     * @param kTranslate, translation in the scene-graph for this object.
     * @param fX, the size of the volume in the x-dimension (extent * resolutions)
     * @param fY, the size of the volume in the y-dimension (extent * resolutions)
     * @param fZ, the size of the volume in the z-dimension (extent * resolutions)
     */
    public VolumeGeometry ( Geometry kGeometry, VolumeImage kImageA, Vector3f kTranslate, float fX, float fY, float fZ )
    {
        super(kImageA,kTranslate,fX,fY,fZ);
        m_kGeometry = kGeometry;
        m_kVertexColor3Shader = new VertexColor3Effect();

        m_kGeometry.AttachEffect( m_kVertexColor3Shader );
        //kLine.Local.SetTranslate(m_kTranslate);

        m_kScene = new Node();
        m_kScene.AttachChild(m_kGeometry);

        m_kScene.UpdateGS();
        m_kScene.UpdateRS();
    }
    

    public void Translate(Vector3f kTranslate)
    {
        super.Translate(kTranslate);
        m_kGeometry.Local.SetTranslate(kTranslate);
        m_kScene.UpdateGS();
    }

    /**
     * PreRender the object, for embedding in the ray-cast volume.
     * @param kRenderer, the OpenGLRenderer object.
     * @param kCuller, the Culler object.
     */
    public void PreRender( Renderer kRenderer, Culler kCuller )  { }

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
        m_kScene.UpdateGS();
        kCuller.ComputeVisibleSet(m_kScene);
        kRenderer.DrawScene(kCuller.GetVisibleSet());
    }
    
    
    public String GetName()
    {
        return m_kGeometry.GetName();
    }

    /** Delete local memory. */
    public void dispose()
    {
        if ( m_kVertexColor3Shader != null )
        {
            m_kVertexColor3Shader.dispose();
            m_kVertexColor3Shader = null;
        }
    }

    private VertexColor3Effect m_kVertexColor3Shader;
    private Geometry m_kGeometry = null;
}
