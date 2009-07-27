package gov.nih.mipav.view.renderer.WildMagic.Render;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.Rendering.Renderer;
import WildMagic.LibGraphics.SceneGraph.Culler;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.Spatial.CullingMode;

public class VolumeNode extends VolumeObject
{
    private Node m_kNode = null;

    /** Create a new VolumeNode with the VolumeImage parameter.
     * @param kImageA the VolumeImage containing shared data and textures for
     * rendering.
     * @param kTranslate translation in the scene-graph for this object.
     * @param fX the size of the volume in the x-dimension (extent * resolutions)
     * @param fY the size of the volume in the y-dimension (extent * resolutions)
     * @param fZ the size of the volume in the z-dimension (extent * resolutions)
     */
    public VolumeNode ( VolumeImage kImageA, Vector3f kTranslate,
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

    /** Delete local memory. */
    public void dispose()
    {
        m_kNode = null;
    }
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeObject#GetName()
     */
    public String GetName()
    {
        return m_kNode.GetName();
    }
    
    /**
     * Return the scene-graph Node.
     * @return scene-graph Node.
     */
    public Node GetNode()
    {
        return m_kNode;
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
        m_kScene.UpdateGS();
        kCuller.ComputeVisibleSet(m_kScene);
        kRenderer.DrawScene(kCuller.GetVisibleSet());
    }


    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeObject#Translate(WildMagic.LibFoundation.Mathematics.Vector3f)
     */
    public void Translate(Vector3f kTranslate)
    {
        super.Translate(kTranslate);
        m_kNode.Local.SetTranslate(kTranslate);
        m_kScene.UpdateGS();
    }
    
    /**
     * Return the translation vector.
     * @return translation vector.
     */
    public Vector3f GetTranslate()
    {
        return new Vector3f(m_kNode.Local.GetTranslate());
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
}
