package gov.nih.mipav.view.renderer.WildMagic.Render;

import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Rendering.*;
import WildMagic.LibGraphics.SceneGraph.*;

/**
 * Displays the three orthogonal planes with the volume data.
 * @see GPUVolumeRender.java
 * @see VolumeObject.java
 */
public class VolumeNode extends VolumeObject
{
    /** Create a new VolumeObject with the VolumeImage parameter.
     * @param kImageA the VolumeImage containing shared data and textures for
     * rendering.
     * @param kTranslate translation in the scene-graph for this object.
     * @param fX the size of the volume in the x-dimension (extent * resolutions)
     * @param fY the size of the volume in the y-dimension (extent * resolutions)
     * @param fZ the size of the volume in the z-dimension (extent * resolutions)
     */
    public VolumeNode ( Renderer kRenderer, VolumeImage kImageA, Vector3f kTranslate,
                        float fX, float fY, float fZ, Node kNode )
    {
        super(kImageA,kTranslate,fX,fY,fZ);

        CreateScene();
        m_kNode = kNode;
        
        m_kScene.AttachChild(m_kNode);
        m_kScene.UpdateGS();
        m_kScene.UpdateRS();
        m_bDisplay = true;
    }

    /**
     * PreRender the object, for embedding in the ray-cast volume.
     * @param kRenderer the OpenGLRenderer object.
     * @param kCuller the Culler object.
     */
    public void PreRender( Renderer kRenderer, Culler kCuller ){}

    /**
     * Render the object.
     * @param kRenderer the OpenGLRenderer object.
     * @param kCuller the Culler object.
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
    
    public Node GetNode()
    {
        return m_kNode;
    }
    
    public String GetName()
    {
        return m_kNode.GetName();
    }

    /** Creates the scene graph. */
    private void CreateScene ( )
    {
        m_kScene = new Node();

        m_kCull = new CullState();
        m_kScene.AttachGlobalState(m_kCull);

        m_kAlpha = new AlphaState();
        m_kAlpha.BlendEnabled = true;
        m_kScene.AttachGlobalState(m_kAlpha);
    }

    public void Translate(Vector3f kTranslate)
    {
        super.Translate(kTranslate);
        m_kNode.Local.SetTranslate(kTranslate);
        m_kScene.UpdateGS();
    }


    /** Delete local memory. */
    public void dispose()
    {
        m_kNode = null;
    }
    private Node m_kNode = null;
}
