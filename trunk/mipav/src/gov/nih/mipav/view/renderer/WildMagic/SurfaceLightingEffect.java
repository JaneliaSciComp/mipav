package gov.nih.mipav.view.renderer.WildMagic;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Collision.PickRecord;
import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;

public class SurfaceLightingEffect extends VolumeClipEffect
{

    /** Creates a LightingEffect */
    public SurfaceLightingEffect (VolumeImage kImageA)
    {
        SetPassQuantity(1);
        m_kVVertexLighting = new VertexShader("MipavLighting");
        m_kPVertexLighting = new PixelShader("PassThrough4");
        
        m_kVolumeTextureNew = new Texture();
        m_kVolumeLUTNew = new Texture();
        
        m_kVPixelLighting = new VertexShader("MipavLightingFragment");
        m_kPPixelLighting = new PixelShader("MipavLightingFragment");
        m_kPPixelLighting.SetTextureQuantity(4);
        m_kPPixelLighting.SetImageName(0,"VolumeImageA");
        m_kPPixelLighting.SetTexture(0, kImageA.GetVolumeTarget() );
        m_kPPixelLighting.SetImageName(1, "ColorMapA");
        m_kPPixelLighting.SetTexture(1, kImageA.GetColorMapTarget() );
        m_kPPixelLighting.SetImageName(2,"VolumeImageNew");
        m_kPPixelLighting.SetTexture(2, m_kVolumeTextureNew );
        m_kPPixelLighting.SetImageName(3, "ColorMapNew");
        m_kPPixelLighting.SetTexture(3, m_kVolumeLUTNew );
        
        m_kVShader.set(0, m_kVVertexLighting);
        m_kPShader.set(0, m_kPVertexLighting);
        
        m_kVolumeImage = kImageA;
    }

    /** This function is called in LoadPrograms once the shader programs are
     * created.  It gives the ShaderEffect-derived classes a chance to do
     * any additional work to hook up the effect with the low-level objects.
     * @param iPass the ith rendering pass
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram,
                                Program pkPProgram)
    {
        Blend(m_fBlend);
    }
    
    /**
     * Sets the light type for the given light.
     * @param kLightType, the name of the light to set (Light0, Light1, etc.)
     * @param afType, the type of light (Ambient = 0, Directional = 1, Point = 2, Spot = 3).
     */
    public void SetLight( String kLightType, float[] afType )
    {
        Program pkProgram = null;
        if ( m_bPerPixelLighting )
        {
            pkProgram = GetPProgram(0);
        }
        else
        {
            pkProgram = GetVProgram(0);
        }
        if ( pkProgram == null )
        {
            return;
        }
        if ( pkProgram.GetUC(kLightType) != null)
        {
            pkProgram.GetUC(kLightType).SetDataSource(afType);
        }
        
    }
    
    public void Blend( float fValue )
    {    
        Program pkProgram = null;
        if ( m_bPerPixelLighting )
        {
            pkProgram = GetPProgram(0);
        }
        else
        {
            pkProgram = GetVProgram(0);
        }
        if ( pkProgram == null )
        {
            return;
        }
        if ( pkProgram.GetUC("Blend") != null)
        {
            pkProgram.GetUC("Blend").SetDataSource(new float[]{fValue,0,0,0});
        }
        m_fBlend = fValue;
    }

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
            ResetClip();
        }
    }
    
    public void SetClipping( boolean bClip )
    {
        Program pkProgram = null;
        if ( m_bPerPixelLighting )
        {
            pkProgram = GetPProgram(0);
        }
        else
        {
            pkProgram = GetVProgram(0);
        }
        if ( pkProgram == null )
        {
            return;
        }
        if ( pkProgram.GetUC("ClipEnabled") != null)
        {
            if ( bClip )
            {
                pkProgram.GetUC("ClipEnabled").SetDataSource(new float[]{1,0,0,0});
            }
            else
            {
                pkProgram.GetUC("ClipEnabled").SetDataSource(new float[]{0,0,0,0});
            }
        }
    }
    
    public void SetSurfaceTexture( boolean bTextureOn, boolean bUseNewImage, boolean bUseNewLUT )
    {
        m_bUseNewLUT = bUseNewLUT;
        m_bUseNewImage = bUseNewImage;
        Program pkProgram = GetPProgram(0);  
        if ( pkProgram == null )
        {
            return;
        }
        if ( pkProgram.GetUC("UseTexture") != null)
        {
            if ( bTextureOn )
            {
                pkProgram.GetUC("UseTexture").SetDataSource(new float[]{1,0,0,0});
            }
            else
            {
                pkProgram.GetUC("UseTexture").SetDataSource(new float[]{0,0,0,0});
            }
        }
        if ( pkProgram.GetUC("UseImageNew") != null)
        {
            if ( bUseNewImage )
            {
                pkProgram.GetUC("UseImageNew").SetDataSource(new float[]{1,0,0,0});
            }
            else
            {
                pkProgram.GetUC("UseImageNew").SetDataSource(new float[]{0,0,0,0});
            }
        }    
        if ( pkProgram.GetUC("UseLUTNew") != null)
        {
            if ( bUseNewLUT )
            {
                pkProgram.GetUC("UseLUTNew").SetDataSource(new float[]{1,0,0,0});
            }
            else
            {
                pkProgram.GetUC("UseLUTNew").SetDataSource(new float[]{0,0,0,0});
            }
        }
    }
    
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
    
    public void SetImageNew( ModelImage kImage )
    {
        m_kImageNew = kImage;
        if ( m_kVolumeImageNew == null )
        {
            m_kVolumeImageNew = VolumeImage.UpdateData(kImage, null, m_kVolumeTextureNew, true, new String("New") );
            m_kVolumeTextureNew.Release();
            m_kVolumeTextureNew.SetImage(m_kVolumeImageNew);
        }
        else
        {
            VolumeImage.UpdateData(kImage, m_kVolumeImageNew, m_kVolumeTextureNew, true, new String("New") );
        }
    }

    public void Dropper( Vector3f kTexCoord, ColorRGBA rkDropperColor )
    {
        ModelImage kImage = m_kVolumeImage.GetImage();
        if ( m_bUseNewImage )
        {
            kImage = m_kImageNew;
        }
        int iX = (int)(kTexCoord.X() * (kImage.getExtents()[0]-1));
        int iY = (int)(kTexCoord.Y() * (kImage.getExtents()[1]-1));
        int iZ = (int)(kTexCoord.Z() * (kImage.getExtents()[2]-1));
        int iIndex = iZ * kImage.getExtents()[1] * kImage.getExtents()[0] +
                iY * kImage.getExtents()[0] +
                iX;
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
            rkDropperColor.R(RGB_LUT[0][index]/255.0f);
            rkDropperColor.G(RGB_LUT[1][index]/255.0f);
            rkDropperColor.B(RGB_LUT[2][index]/255.0f);
        }
    }
    
    /** Delete memory */
    public void dispose()
    {
        m_kVolumeImage = null;
        m_kImageNew = null;
        super.dispose();
    }
    private VolumeImage m_kVolumeImage = null;
    private float m_fBlend = 1.0f;
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
}

