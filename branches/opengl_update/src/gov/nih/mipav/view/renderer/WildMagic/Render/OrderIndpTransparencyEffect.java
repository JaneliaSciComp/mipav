package gov.nih.mipav.view.renderer.WildMagic.Render;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Shader;
import WildMagic.LibGraphics.Shaders.VertexShader;

/**
 * This class sets up the final pass in rendering multiple semi-transparent surfaces.
 * 
 * Rendering multiple translucent surfaces is accomplished with the following steps:
 *   1. A FrameBuffer Object (FBO) is created with three render-to-texture targets, and a depth buffer.
 *   2. The first render target is activated. Solid objects are rendered: writing color and depth
 *      information to the FBO target1. The color information displays the solid objects, the depth
 *      information enables the solid objects to properly occlude the translucent objects.
 *   3. The second and third render targets are activated. Translucent objects are rendered: writing
 *      accumulated color and alpha to the second target and the number of overlapping surfaces per
 *      pixel to the third target. In this pass the depth buffer is read-only.
 *   4. Rendering to the three texture targets is disabled.
 *   5. A screen-space polygon is rendered, it reads the three textures generated in the previous pass.
 *      The first texture contains the color from the solid objects, the next two textures contain the
 *      color information and weighted-average for rendering the translucent textures.
 */
public class OrderIndpTransparencyEffect extends ShaderEffect
{
    /**  */
    private static final long serialVersionUID = -1254697242675646161L;

    /**
     * Creates a new OrderIndpTransparencyEffect for the final rendering pass.
     * @param akTextures, set of three Frame Buffer Object (FBO) textures to render into/read from.
     * @param kBackgroundColor, background color.
     */
    public OrderIndpTransparencyEffect (Texture[] akTextures)
    {
        super(1);   

        PixelShader kPShader = new PixelShader( "OrderIndpTransparencyFinalP");
        kPShader.SetTextureQuantity(akTextures.length);
        for ( int i = 0; i < akTextures.length; i++ )
        {
            kPShader.SetTexture( i, akTextures[i], "ColorTex" + i );
            kPShader.SetImageName( i, akTextures[i].GetName(), "ColorTex" + i );
        }
        SetPShader(0, kPShader);
        SetVShader(0, new VertexShader( "TextureV", Shader.vertexShaderTexture2, true ));
    }
}
