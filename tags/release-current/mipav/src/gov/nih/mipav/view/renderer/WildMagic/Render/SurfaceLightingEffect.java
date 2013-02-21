package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.TransferFunction;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Rendering.GraphicsImage;
import WildMagic.LibGraphics.Rendering.Renderer;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.VertexShader;

/**
 * Surface lighting uses the lights defined in the Volume/Surface/Tri-Planar view in the vertex and pixel shaders.
 */
public class SurfaceLightingEffect extends VolumeClipEffect
{

    /**  */
    private static final long serialVersionUID = 5100923838093263591L;
    private VolumeImage m_kVolumeImage = null;

    private float m_fBlend = 1.0f;
    

    private int m_iReverseFace = 0;
    
    private VertexShader m_kVVertexLighting;
    
    private PixelShader m_kPVertexLighting;

    
    private VertexShader m_kVPixelLighting;
    
    private PixelShader m_kPPixelLighting;
    
    private boolean m_bPerPixelLighting = false;
    
    private Texture m_kVolumeTextureNew;
    
    private GraphicsImage m_kVolumeImageNew = null;
    
    private Texture m_kVolumeLUTNew;

    private GraphicsImage m_kColorMapNew = null;
    
    private boolean m_bUseNewLUT = false;
    
    private boolean m_bUseNewImage = false;
    private ModelImage m_kImageNew = null;
    private ModelLUT m_kLUTNew = null;
    /** Creates a LightingEffect
     * @param kImageA VolumeImage containing data and textures for the effect.
     */
    public SurfaceLightingEffect (VolumeImage kImageA, boolean bTransparent)
    {
        SetPassQuantity(1);
        m_kVVertexLighting = new VertexShader("MipavLighting", true);
        if ( !bTransparent )
        {
            m_kPVertexLighting = new PixelShader("PassThrough4");
        }
        else
        {
            m_kPVertexLighting = new PixelShader("PassThrough_Transparency4");
        }
        
        m_kVolumeTextureNew = new Texture();
        m_kVolumeLUTNew = new Texture();
        
        m_kVPixelLighting = new VertexShader("MipavLightingFragmentV", true);
        if ( !bTransparent )
        {
            m_kPPixelLighting = new PixelShader("MipavLightingFragmentP", true);
        }
        else
        {
            m_kPPixelLighting = new PixelShader("MipavLightingFragment_TransparencyP", true);
        }
        if ( kImageA != null )
        {
            m_kPPixelLighting.SetTextureQuantity(4);
            m_kPPixelLighting.SetImageName(0,"VolumeImageA");
            m_kPPixelLighting.SetTexture(0, kImageA.GetVolumeTarget() );
            m_kPPixelLighting.SetImageName(1, "ColorMapA");
            m_kPPixelLighting.SetTexture(1, kImageA.GetColorMapTarget() );
            m_kPPixelLighting.SetImageName(2,"VolumeImageNew");
            m_kPPixelLighting.SetTexture(2, m_kVolumeTextureNew );
            m_kPPixelLighting.SetImageName(3, "ColorMapNew");
            m_kPPixelLighting.SetTexture(3, m_kVolumeLUTNew );
        }
        m_kVShader.set(0, m_kVVertexLighting);
        m_kPShader.set(0, m_kPVertexLighting);
        
        m_kVolumeImage = kImageA;
    }
    /**
     * Sets surface blend/transparency value for alph-blending in the shader.
     * @param fValue surface blend/transparency value for alph-blending in the shader.
     */
    public void Blend( float fValue )
    {    
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram == null )
        {
            return;
        }
        if ( pkCProgram.GetUC("Blend") != null)
        {
            pkCProgram.GetUC("Blend").GetData()[0] = fValue;
        }
        m_fBlend = fValue;
    }
    /** Delete memory */
    public void dispose()
    {
        m_kVolumeImage = null;
        if ( m_kVVertexLighting != null )
        {
            m_kVVertexLighting.dispose();
            m_kVVertexLighting = null;
        }

        if ( m_kPVertexLighting != null )
        {
            m_kPVertexLighting.dispose();
            m_kPVertexLighting = null;
        }
        if ( m_kVPixelLighting != null )
        {
            m_kVPixelLighting.dispose();
            m_kVPixelLighting = null;
        }
        if ( m_kPPixelLighting != null )
        {
            m_kPPixelLighting.dispose();
            m_kPPixelLighting = null;
        }
        if ( m_kVolumeTextureNew != null )
        {
            m_kVolumeTextureNew.dispose();
            m_kVolumeTextureNew = null;
        }
        if ( m_kVolumeImageNew != null )
        {
            m_kVolumeImageNew.dispose();
            m_kVolumeImageNew = null;
        }
        if ( m_kVolumeLUTNew != null )
        {
            m_kVolumeLUTNew.dispose();
            m_kVolumeLUTNew = null;
        }
        if ( m_kColorMapNew != null )
        {
            m_kColorMapNew.dispose();
            m_kColorMapNew = null;
        }
        if ( m_kImageNew != null )
        {
            m_kImageNew.disposeLocal();
            m_kImageNew = null;
        }
        super.dispose();
    }
    /**
     * Reads the surface color from the texture map at the given location.
     * @param kTexCoord texture location.
     * @param rkDropperColor returned color.
     */
    public void Dropper( Vector3f kTexCoord, ColorRGBA rkDropperColor )
    {
        ModelImage kImage = m_kVolumeImage.GetImage();
        if ( m_bUseNewImage )
        {
            kImage = m_kImageNew;
        }
        int iX = (int)(kTexCoord.X * (kImage.getExtents()[0]-1));
        int iY = (int)(kTexCoord.Y * (kImage.getExtents()[1]-1));
        int iZ = (int)(kTexCoord.Z * (kImage.getExtents()[2]-1));
        if ( kImage.isColorImage() )
        {}
        else
        {
            float fValue = kImage.getFloat(iX,iY,iZ);           
            ModelLUT kLUT = m_kVolumeImage.GetLUT();
            if ( m_bUseNewLUT )
            {
                kLUT = m_kLUTNew;
            }
            float[][] RGB_LUT = kLUT.exportRGB_LUT(true);
            TransferFunction tf_imgA = kLUT.getTransferFunction();
            int index = (int)(tf_imgA.getRemappedValue(fValue, 256) + 0.5f);
            rkDropperColor.R = RGB_LUT[0][index]/255.0f;
            rkDropperColor.G = RGB_LUT[1][index]/255.0f;
            rkDropperColor.B = RGB_LUT[2][index]/255.0f;
        }
    }
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeClipEffect#OnLoadPrograms(int, WildMagic.LibGraphics.Shaders.Program, WildMagic.LibGraphics.Shaders.Program)
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram,
                                Program pkPProgram, Program pkCProgram)
    {
        SetColorImage(pkCProgram);
        Blend(m_fBlend);
        super.OnLoadPrograms ( iPass,  pkVProgram, pkPProgram, pkCProgram );
    }
    /**
     * Enables/disables surface clipping for the per-pixel shader.
     * @param bClip surface clipping on/off.
     */
    public void SetClipping( boolean bClip )
    {
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram == null )
        {
            return;
        }
        if ( pkCProgram.GetUC("ClipEnabled") != null)
        {
            if ( bClip )
            {
                pkCProgram.GetUC("ClipEnabled").GetData()[0] = 1;
            }
            else
            {
                pkCProgram.GetUC("ClipEnabled").GetData()[0] = 0;
            }
        }
    }
    /**
     * Sets alternate volume data for texture mapping.
     * @param kImage  alternate volume data for texture mapping.
     */
    public void SetImageNew( ModelImage kImage )
    {
        m_kImageNew = kImage;
        if ( m_kVolumeImageNew == null )
        {
            m_kVolumeImageNew = VolumeImage.UpdateData(kImage, 0, null, null, m_kVolumeTextureNew, new String(kImage.getImageName() + "New"), true, false );
            m_kVolumeTextureNew.Release();
            m_kVolumeTextureNew.SetImage(m_kVolumeImageNew);

            Program kCProgram = GetCProgram(0);
            if ( kCProgram.GetUC("IsColorNew") != null ) 
            {
                kCProgram.GetUC("IsColorNew").GetData()[0] = kImage.isColorImage() ? 1 : 0;
            } 
        }
        else
        {
            VolumeImage.UpdateData(kImage, 0, null, m_kVolumeImageNew, m_kVolumeTextureNew, new String(kImage.getImageName() + "New"), true, false );
        }
    }
    /**
     * Sets the light type for the given light.
     * @param kLightType the name of the light to set (Light0, Light1, etc.)
     * @param afType the type of light (Ambient = 0, Directional = 1, Point = 2, Spot = 3).
     */
    public void SetLight( String kLightType, float[] afType )
    {
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram == null )
        {
            return;
        }
        if ( pkCProgram.GetUC(kLightType) != null)
        {
            pkCProgram.GetUC(kLightType).SetDataSource(afType);
        }
        
    }
    /**
     * Sets the alternate LUT for surface texture mapping.
     * @param kLUT LUT for grayscale images.
     * @param kRGBT LUT for color images.
     */
    public void SetLUTNew( ModelLUT kLUT, ModelRGB kRGBT )
    {
        if ( m_kColorMapNew == null  )
        {
            m_kColorMapNew = VolumeImage.InitColorMap( kLUT, kRGBT, "New" );
            m_kVolumeLUTNew.Release();
            m_kVolumeLUTNew.SetImage(m_kColorMapNew);
        }
        else if ( kLUT != null )
        {
            VolumeImage.UpdateImages( m_kVolumeLUTNew, m_kColorMapNew, kLUT );
        }
        else if ( kRGBT != null )
        {
            VolumeImage.SetRGBT( m_kVolumeLUTNew, m_kColorMapNew, kRGBT );
        }
        m_kLUTNew = kLUT;
    }
    /**
     * Sets the lighting shader to be per-pixel or per-vertex.
     * @param kRenderer OpenGL renderer.
     * @param bOn turns per-pixel lighting on/off.
     */
    public void SetPerPixelLighting( Renderer kRenderer, boolean bOn )
    {
        if ( m_bPerPixelLighting != bOn)
        {
            m_bPerPixelLighting = bOn;
            if ( m_bPerPixelLighting )
            {
                m_kVShader.set(0, m_kVPixelLighting);
                m_kPShader.set(0, m_kPPixelLighting);
            }
            else
            {
                m_kVShader.set(0, m_kVVertexLighting);
                m_kPShader.set(0, m_kPVertexLighting);
            }
            LoadPrograms(kRenderer, 0,kRenderer.GetMaxColors(),
                    kRenderer.GetMaxTCoords(),
                    kRenderer.GetMaxVShaderImages(),
                    kRenderer.GetMaxPShaderImages());
            Blend(m_fBlend);
            SetReverseFace(m_iReverseFace);
            ResetClip();
        }
    }
    /**
     * Flag to reverse the direction of the triangle faces inside the vertex shader. Useful for rendering from inside a mesh.
     * @param iReverse 1 reverses the triangle face direction, 0 does nothing.
     */
    public void SetReverseFace( int iReverse )
    {    
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram.GetUC("ReverseFace") != null)
        {
            pkCProgram.GetUC("ReverseFace").GetData()[0] = iReverse;
        }
        m_iReverseFace = iReverse;
    }
    /**
     * Sets the surface texture on/off.
     * @param bTextureOn texture on/off
     * @param bUseNewImage indicates which volume to use as the texture.
     * @param bUseNewLUT indicates which LUT to use.
     */
    public void SetSurfaceTexture( boolean bTextureOn, boolean bUseNewImage, boolean bUseNewLUT )
    {
        m_bUseNewLUT = bUseNewLUT;
        m_bUseNewImage = bUseNewImage;
        Program pkCProgram = GetCProgram(0);  
        if ( pkCProgram == null )
        {
            return;
        }
        if ( pkCProgram.GetUC("UseTexture") != null)
        {
            if ( bTextureOn )
            {
                pkCProgram.GetUC("UseTexture").GetData()[0] = 1;
            }
            else
            {
                pkCProgram.GetUC("UseTexture").GetData()[0] = 0;
            }
        }
        if ( pkCProgram.GetUC("UseImageNew") != null)
        {
            if ( bUseNewImage )
            {
                pkCProgram.GetUC("UseImageNew").GetData()[0] = 1;
            }
            else
            {
                pkCProgram.GetUC("UseImageNew").GetData()[0] = 0;
            }
        }    
        if ( pkCProgram.GetUC("UseLUTNew") != null)
        {
            if ( bUseNewLUT )
            {
                pkCProgram.GetUC("UseLUTNew").GetData()[0] = 1;
            }
            else
            {
                pkCProgram.GetUC("UseLUTNew").GetData()[0] = 0;
            }
        }
    }
    /**
     * Sets the IsColor shader parameter values.
     */
    private void SetColorImage(Program pkCProgram)
    { 
        if ( (pkCProgram.GetUC("IsColor") != null) && ((m_kVolumeImage != null)) ) 
        {
            pkCProgram.GetUC("IsColor").GetData()[0] = m_kVolumeImage.IsColorImage() ? 1 : 0;
        } 
    }
}

