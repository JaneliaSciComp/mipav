
package gov.nih.mipav.view.renderer.volumeview;

import java.awt.GraphicsConfiguration;
import java.io.File;
import java.io.IOException;

import javax.vecmath.Vector3f;

import com.sun.j3d.utils.universe.SimpleUniverse;

import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Rendering.*;
import WildMagic.LibGraphics.Shaders.*;
import WildMagic.LibGraphics.ObjectSystem.*;
import WildMagic.LibGraphics.Effects.*;
import gov.nih.mipav.view.renderer.surfaceview.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.*;

/** 
 * VolumeShaderEffect is the workhorse of the GPU-based rendering in MIPAV. It
 * manages several Cg shaders for volume rendering. Each of the different
 * volume modes MIP, DDR, Composite, Surface, and Composite Surface
 * are implemented with different Cg shaders. The volume data and lookup table
 * information are stored and passed to the shaders as texture images;
 * VolumeShaderEffect manages these images. All UserConstant shader parameters
 * are also managed by the VolumeShaderEffect class.
 */
public class VolumeShaderEffect extends ShaderEffect
    implements StreamInterface
{
    /** View Mode Constants: */
    private final static int MIP = 0;
    private final static int DDR = 1;
    private final static int CMP = 2;
    private final static int SUR = 3;
    private final static int CMP_SUR = 4;
    /** Axis-aligned clip plane shader paramter names: */
    private final static String[] m_akClip =
        new String[]{ "clipXInv", "clipX", "clipYInv", "clipY", "clipZInv", "clipZ" };


    /** 
     * Creates a new VolumeShaderEffect object.
     * @param kImageA ModelImage A
     * @param kLUTa, LUT for ModelImage A
     * @param kRGBTa, RGB lookup table for ModelImage A
     * @param kImageB ModelImage B
     * @param kLUTb, LUT for ModelImage B
     * @param kRGBTb, RGB lookup table for ModelImage B
     * @param kSceneTarget, the SceneImage texture with the back-facing polygon texture coordinates.
     */
    public VolumeShaderEffect ( ModelImage kImageA, ModelLUT kLUTA, ModelRGB kRGBTA, 
                                ModelImage kImageB, ModelLUT kLUTB, ModelRGB kRGBTB, 
                                Texture kSceneTarget )
    {
        m_kImageA = kImageA;
        m_kImageB = kImageB;
        m_kSceneTarget = kSceneTarget;
        m_kColorMapA = InitColorMap(kLUTA, kRGBTA, new String("A"));
        m_kOpacityMapA = InitOpacityMap(m_kImageA, new String("A"));
        m_kOpacityMapA_GM = InitOpacityMap(m_kImageA, new String("A_GM"));
        m_kOpacityMapB_GM = InitOpacityMap(m_kImageA, new String("B_GM"));
        if ( kImageB == null )
        {
            CreateVolumeTexture();
        }
        else
        {
            m_kColorMapB = InitColorMap(kLUTB, kRGBTB, new String("B"));
            m_kOpacityMapB = InitOpacityMap(m_kImageB, new String("B"));
            CreateVolumeTextureAB();
        }
    }
    
    /**
     * memory cleanup.
     */
    public void dispose()
    {
        m_kImageA = null;
        m_kLUTA = null;
        m_kRGBA = null;
        m_kTransferA = null;
        m_kImageB = null;
        m_kLUTB = null;
        m_kRGBB = null;
        m_kTransferB = null;

        if ( m_kImageA_GM != null )
        {
            m_kImageA_GM.disposeLocal();
            m_kImageA_GM = null;
        }
        if ( m_kOpacityMapA_GM != null )
        {
            m_kOpacityMapA_GM.dispose();
            m_kOpacityMapA_GM = null;
        }
        if ( m_kOpacityMapTargetA_GM != null )
        {
            m_kOpacityMapTargetA_GM.dispose();
            m_kOpacityMapTargetA_GM = null;
        }
        if ( m_kVolumeA_GM != null )
        {
            m_kVolumeA_GM.dispose();
            m_kVolumeA_GM = null;
        }
        if ( m_kVolumeTargetA_GM != null )
        {
            m_kVolumeTargetA_GM.dispose();
            m_kVolumeTargetA_GM = null;
        }
        if ( m_kImageB_GM != null )
        {
            m_kImageB_GM.disposeLocal();
            m_kImageB_GM = null;
        }
        if ( m_kOpacityMapB_GM != null )
        {
            m_kOpacityMapB_GM.dispose();
            m_kOpacityMapB_GM = null;
        }
        if ( m_kOpacityMapTargetB_GM != null )
        {
            m_kOpacityMapTargetB_GM.dispose();
            m_kOpacityMapTargetB_GM = null;
        }
        if ( m_kVolumeB_GM != null )
        {
            m_kVolumeB_GM.dispose();
            m_kVolumeB_GM = null;
        }
        if ( m_kVolumeTargetB_GM != null )
        {
            m_kVolumeTargetB_GM.dispose();
            m_kVolumeTargetB_GM = null;
        }
        if ( m_kVolumeA != null )
        {
            m_kVolumeA.dispose();
            m_kVolumeA = null;
        }
        if ( m_kNormalA != null )
        {
            m_kNormalA.dispose();
            m_kNormalA = null;
        }
        if ( m_kColorMapA != null )
        {
            m_kColorMapA.dispose();
            m_kColorMapA = null;
        }
        if ( m_kOpacityMapA != null )
        {
            m_kOpacityMapA.dispose();
            m_kOpacityMapA = null;
        }
        if ( m_kVolumeTargetA != null )
        {
            m_kVolumeTargetA.dispose();
            m_kVolumeTargetA = null;
        }
        if ( m_kColorMapTargetA != null )
        {
            m_kColorMapTargetA.dispose();
            m_kColorMapTargetA = null;
        }
        if ( m_kOpacityMapTargetA != null )
        {
            m_kOpacityMapTargetA.dispose();
            m_kOpacityMapTargetA = null;
        }
        if ( m_kNormalMapTargetA != null )
        {
            m_kNormalMapTargetA.dispose();
            m_kNormalMapTargetA = null;
        }
        if ( m_kVolumeB != null )
        {
            m_kVolumeB.dispose();
            m_kVolumeB = null;
        }
        if ( m_kNormalB != null )
        {
            m_kNormalB.dispose();
            m_kNormalB = null;
        }
        if ( m_kColorMapB != null )
        {
            m_kColorMapB.dispose();
            m_kColorMapB = null;
        }
        if ( m_kOpacityMapB != null )
        {
            m_kOpacityMapB.dispose();
            m_kOpacityMapB = null;
        }
        if ( m_kVolumeTargetB != null )
        {
            m_kVolumeTargetB.dispose();
            m_kVolumeTargetB = null;
        }
        if ( m_kColorMapTargetB != null )
        {
            m_kColorMapTargetB.dispose();
            m_kColorMapTargetB = null;
        }
        if ( m_kOpacityMapTargetB != null )
        {
            m_kOpacityMapTargetB.dispose();
            m_kOpacityMapTargetB = null;
        }
        if ( m_kNormalMapTargetB != null )
        {
            m_kNormalMapTargetB.dispose();
            m_kNormalMapTargetB = null;
        }
        if ( m_kPShaderMIP != null )
        {
            m_kPShaderMIP.dispose();
            m_kPShaderMIP = null;
        }
        if ( m_kPShaderDDR != null )
        {
            m_kPShaderDDR.dispose();
            m_kPShaderDDR = null;
        }
        if ( m_kPShaderCMP != null )
        {
            m_kPShaderCMP.dispose();
            m_kPShaderCMP = null;
        }
        if ( m_kPShaderSUR != null )
        {
            m_kPShaderSUR.dispose();
            m_kPShaderSUR = null;
        }

        m_aafClipData = null;
        m_afClipEyeData = null;
        m_afClipEyeInvData = null;
        m_afClipArbData = null;
        m_kSceneTarget = null;
        m_afBlend = null;
        m_kBackgroundColor = null;

        m_afSelfShadow = null;
        m_afGradientMagnitude = null;

        super.dispose();
    }

    /**
     * The VolumeShaderEffect.CreateVolumeTexture() function constructs and
     * initializes the vertex and pixel shader programs for volume
     * rendering. The vertex shader is the same for each rendering type: MIP,
     * DDR, Composite, Surface, and Composite Surface. The pixel shaders are
     * different for each.
     */
    private void CreateVolumeTexture ()
    {
        /* Set single-pass rendering: */
        SetPassQuantity(1);

        /* Map the ModelImage volume data to a texture image, including for
         * the ModelImage gradient magnitude data: */
        m_kVolumeA = UpdateData(m_kImageA, null, m_kVolumeTargetA, new String("A") );
        m_kImageA_GM = CalcHistogramsGM( m_kImageA );
        m_kVolumeA_GM = UpdateData(m_kImageA_GM, null, m_kVolumeTargetA_GM, new String("A_GM") );

        /* Create the vertex shader program, shared by all rendering types. It
         * is implemented in the VolumeShaderVertex.cg file: */        
        VertexShader pkVShader = new VertexShader("VolumeShaderVertex");

        /* The following code block creates the MIP pixel shader, implemented
         * in VolumeShaderMIP.cg. It also sets up and stores references to the
         * texture images used by the pixel shader. The MIP pixel-shader uses
         * 6 textures. The SceneImage texture, storing the texture-coordinates
         * of the back-facing polygons. VolumeImageA texture stores the
         * ModelImage data. ColorMapA stores the color
         * lookup-table. OpacityMapA stores the opacity transfer
         * function. VolumeImageA_GM has the gradient magnitude data for the
         * ModelImage. OpacityMapA_GM has the gradient magnitude opacity
         * transfer function.  The references m_kVolumeTargetA,
         * m_kColorMapTargetA, m_kOpacityMapTargetA, m_kVolumeTargetA_GM, and
         * m_kOpacityTargetA_GM are stored so the images can be re-used by the
         * other pixel shaders. */
        m_kPShaderMIP = new PixelShader("VolumeShaderMIP");
        m_kPShaderMIP.SetTextureQuantity(6);
        m_kPShaderMIP.SetImageName(0,"SceneImage");
        m_kPShaderMIP.SetTexture(0,m_kSceneTarget);
        m_kPShaderMIP.SetImageName(1,"VolumeImageA");
        m_kPShaderMIP.GetTexture(1).SetFilterType(Texture.FilterType.LINEAR);
        m_kPShaderMIP.GetTexture(1).SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kPShaderMIP.GetTexture(1).SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kPShaderMIP.GetTexture(1).SetWrapType(2,Texture.WrapType.CLAMP_BORDER);
        m_kVolumeTargetA = m_kPShaderMIP.GetTexture(1);
        m_kPShaderMIP.SetImageName(2, "ColorMapA");
        m_kColorMapTargetA = m_kPShaderMIP.GetTexture(2);
        m_kPShaderMIP.SetImageName(3, "OpacityMapA");
        m_kOpacityMapTargetA = m_kPShaderMIP.GetTexture(3);
        m_kPShaderMIP.SetImageName(4,"VolumeImageA_GM");
        m_kPShaderMIP.GetTexture(4).SetFilterType(Texture.FilterType.LINEAR);
        m_kPShaderMIP.GetTexture(4).SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kPShaderMIP.GetTexture(4).SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kPShaderMIP.GetTexture(4).SetWrapType(2,Texture.WrapType.CLAMP_BORDER);
        m_kVolumeTargetA_GM = m_kPShaderMIP.GetTexture(4);
        m_kPShaderMIP.SetImageName(5, "OpacityMapA_GM");
        m_kOpacityMapTargetA_GM = m_kPShaderMIP.GetTexture(5);

        /* The following block creates the DDR volume shader, implemented in
         * VolumShaderDDR.cg. The DDR shader uses the same texture images and
         * data as the MIP shader. The textures are not duplicated in CPU or
         * GPU memory. */
        m_kPShaderDDR = new PixelShader("VolumeShaderDDR");
        m_kPShaderDDR.SetTextureQuantity(6);
        m_kPShaderDDR.SetImageName(0,"SceneImage");
        m_kPShaderDDR.SetTexture(0,m_kSceneTarget);
        m_kPShaderDDR.SetImageName(1,"VolumeImageA");
        m_kPShaderDDR.SetTexture(1,m_kVolumeTargetA);
        m_kPShaderDDR.SetImageName(2, "ColorMapA");
        m_kPShaderDDR.SetTexture(2,m_kColorMapTargetA);
        m_kPShaderDDR.SetImageName(3, "OpacityMapA");
        m_kPShaderDDR.SetTexture(3,m_kOpacityMapTargetA);
        m_kPShaderDDR.SetImageName(4,"VolumeImageA_GM");
        m_kPShaderDDR.SetTexture(4,m_kVolumeTargetA_GM);
        m_kPShaderDDR.SetImageName(5, "OpacityMapA_GM");
        m_kPShaderDDR.SetTexture(5, m_kOpacityMapTargetA_GM);

        /* The following block creates the Composite volume shader,
         * implemented in VolumShaderCMP.cg. The Composite shader uses the
         * same texture images and data as the MIP shader. The textures are
         * not duplicated in CPU or GPU memory. */
        m_kPShaderCMP = new PixelShader("VolumeShaderCMP");
        m_kPShaderCMP.SetTextureQuantity(6);
        m_kPShaderCMP.SetImageName(0,"SceneImage");
        m_kPShaderCMP.SetTexture(0,m_kSceneTarget);
        m_kPShaderCMP.SetImageName(1,"VolumeImageA");
        m_kPShaderCMP.SetTexture(1,m_kVolumeTargetA);
        m_kPShaderCMP.SetImageName(2, "ColorMapA");
        m_kPShaderCMP.SetTexture(2,m_kColorMapTargetA);
        m_kPShaderCMP.SetImageName(3, "OpacityMapA");
        m_kPShaderCMP.SetTexture(3,m_kOpacityMapTargetA);
        m_kPShaderCMP.SetImageName(4,"VolumeImageA_GM");
        m_kPShaderCMP.SetTexture(4,m_kVolumeTargetA_GM);
        m_kPShaderCMP.SetImageName(5, "OpacityMapA_GM");
        m_kPShaderCMP.SetTexture(5, m_kOpacityMapTargetA_GM);

        /* The following block creates the Surface and Composite Surface
         * volume shader, both are implemented in a single pixel program in
         * VolumShaderSUR.cg. The shader uses the same texture images and data
         * as the MIP shader, with the addition of another 3D volume texture
         * storing the normal data. */
        m_kPShaderSUR = new PixelShader("VolumeShaderSUR");
        m_kPShaderSUR.SetTextureQuantity(7);
        m_kPShaderSUR.SetImageName(0,"SceneImage");
        m_kPShaderSUR.SetTexture(0,m_kSceneTarget);
        m_kPShaderSUR.SetImageName(1,"VolumeImageA");
        m_kPShaderSUR.SetTexture(1,m_kVolumeTargetA);
        m_kPShaderSUR.SetImageName(2, "ColorMapA");
        m_kPShaderSUR.SetTexture(2,m_kColorMapTargetA);
        m_kPShaderSUR.SetImageName(3, "OpacityMapA");
        m_kPShaderSUR.SetTexture(3,m_kOpacityMapTargetA);
        m_kPShaderSUR.SetImageName(4,"VolumeImageA_GM");
        m_kPShaderSUR.SetTexture(4,m_kVolumeTargetA_GM);
        m_kPShaderSUR.SetImageName(5, "OpacityMapA_GM");
        m_kPShaderSUR.SetTexture(5, m_kOpacityMapTargetA_GM);

        int iXBound = m_kImageA.getExtents()[0];
        int iYBound = m_kImageA.getExtents()[1];
        int iZBound = m_kImageA.getExtents()[2];
        
        byte[] aucData = calcImageNormals( m_kImageA );
        
        m_kNormalA = new GraphicsImage(GraphicsImage.FormatMode.IT_RGB888,
                                       iXBound,iYBound,iZBound,aucData,
                                       "NormalMapA");
        m_kPShaderSUR.SetImageName(6, "NormalMapA" );
        m_kPShaderSUR.GetTexture(6).SetFilterType(Texture.FilterType.LINEAR);
        m_kPShaderSUR.GetTexture(6).SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kPShaderSUR.GetTexture(6).SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kPShaderSUR.GetTexture(6).SetWrapType(2,Texture.WrapType.CLAMP_BORDER);
        m_kNormalMapTargetA = m_kPShaderSUR.GetTexture(6);
         
        /* The vertex shader is set, it never changes. The first parameter is
         * the pass number used when applying different shaders during
         * multiple rendering passes, it is 0 because this effect is
         * implemented in single-pass rendering. */
        SetVShader(0,pkVShader);
        /* The pixel shader defaults to MIP: */
        SetPShader(0,m_kPShaderMIP);
    }

    /**
     * The VolumeShaderEffect.CreateVolumeTextureAM() function constructs and
     * initializes the vertex and pixel shader programs for volume rendering
     * of two images (imageA, imageB). The vertex shader is the same for each
     * rendering type: MIP, DDR, Composite, Surface, and Composite
     * Surface. The pixel shaders are different for each.
     */
    private void CreateVolumeTextureAB ()
    {
        SetPassQuantity(1);

        m_kVolumeA = UpdateData(m_kImageA, m_kVolumeA, m_kVolumeTargetA, new String("A") );
        m_kVolumeB = UpdateData(m_kImageB, m_kVolumeB, m_kVolumeTargetB, new String("B") );
        
        m_kImageA_GM = CalcHistogramsGM( m_kImageA );
        m_kVolumeA_GM = UpdateData(m_kImageA_GM, null, m_kVolumeTargetA_GM, new String("A_GM") );

        m_kImageB_GM = CalcHistogramsGM( m_kImageB );
        m_kVolumeB_GM = UpdateData(m_kImageB_GM, null, m_kVolumeTargetB_GM, new String("B_GM") );

        VertexShader pkVShader = new VertexShader("VolumeShaderVertex");

        // setup mip shader effect:
        m_kPShaderMIP = new PixelShader("VolumeShaderMIP_MIP");
        m_kPShaderMIP.SetTextureQuantity(7);
        m_kPShaderMIP.SetImageName(0,"SceneImage");
        m_kPShaderMIP.SetTexture(0,m_kSceneTarget);
        m_kPShaderMIP.SetImageName(1,"VolumeImageA");
        m_kPShaderMIP.GetTexture(1).SetFilterType(Texture.FilterType.LINEAR);
        m_kPShaderMIP.GetTexture(1).SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kPShaderMIP.GetTexture(1).SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kPShaderMIP.GetTexture(1).SetWrapType(2,Texture.WrapType.CLAMP_BORDER);
        m_kVolumeTargetA = m_kPShaderMIP.GetTexture(1);
        m_kPShaderMIP.SetImageName(2, "ColorMapA");
        m_kColorMapTargetA = m_kPShaderMIP.GetTexture(2);
        m_kPShaderMIP.SetImageName(3, "OpacityMapA");
        m_kOpacityMapTargetA = m_kPShaderMIP.GetTexture(3);
        m_kPShaderMIP.SetImageName(4,"VolumeImageB");
        m_kPShaderMIP.GetTexture(4).SetFilterType(Texture.FilterType.LINEAR);
        m_kPShaderMIP.GetTexture(4).SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kPShaderMIP.GetTexture(4).SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kPShaderMIP.GetTexture(4).SetWrapType(2,Texture.WrapType.CLAMP_BORDER);
        m_kVolumeTargetB = m_kPShaderMIP.GetTexture(4);
        m_kPShaderMIP.SetImageName(5, "ColorMapB");
        m_kColorMapTargetB = m_kPShaderMIP.GetTexture(5);
        m_kPShaderMIP.SetImageName(6, "OpacityMapB");
        m_kOpacityMapTargetB = m_kPShaderMIP.GetTexture(6);

        SetVShader(0,pkVShader);
        SetPShader(0,m_kPShaderMIP);

        m_kPShaderDDR = new PixelShader("VolumeShaderDDR_DDR");
        m_kPShaderDDR.SetTextureQuantity(7);
        m_kPShaderDDR.SetImageName(0,"SceneImage");
        m_kPShaderDDR.SetTexture(0,m_kSceneTarget);
        m_kPShaderDDR.SetImageName(1,"VolumeImageA");
        m_kPShaderDDR.SetTexture(1, m_kVolumeTargetA);
        m_kPShaderDDR.SetImageName(2, "ColorMapA");
        m_kPShaderDDR.SetTexture(2, m_kColorMapTargetA);
        m_kPShaderDDR.SetImageName(3, "OpacityMapA");
        m_kPShaderDDR.SetTexture(3, m_kOpacityMapTargetA);
        m_kPShaderDDR.SetImageName(4,"VolumeImageB");
        m_kPShaderDDR.SetTexture(4, m_kVolumeTargetB);
        m_kPShaderDDR.SetImageName(5, "ColorMapB");
        m_kPShaderDDR.SetTexture(5,m_kColorMapTargetB);
        m_kPShaderDDR.SetImageName(6, "OpacityMapB");
        m_kPShaderDDR.SetTexture(6,m_kOpacityMapTargetB);


        m_kPShaderCMP = new PixelShader("VolumeShaderCMP_CMP");
        m_kPShaderCMP.SetTextureQuantity(11);
        m_kPShaderCMP.SetImageName(0,"SceneImage");
        m_kPShaderCMP.SetTexture(0,m_kSceneTarget);
        m_kPShaderCMP.SetImageName(1,"VolumeImageA");
        m_kPShaderCMP.SetTexture(1, m_kVolumeTargetA);
        m_kPShaderCMP.SetImageName(2, "ColorMapA");
        m_kPShaderCMP.SetTexture(2, m_kColorMapTargetA);
        m_kPShaderCMP.SetImageName(3, "OpacityMapA");
        m_kPShaderCMP.SetTexture(3, m_kOpacityMapTargetA);
        m_kPShaderCMP.SetImageName(4,"VolumeImageA_GM");
        m_kPShaderCMP.GetTexture(4).SetFilterType(Texture.FilterType.LINEAR);
        m_kPShaderCMP.GetTexture(4).SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kPShaderCMP.GetTexture(4).SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kPShaderCMP.GetTexture(4).SetWrapType(2,Texture.WrapType.CLAMP_BORDER);
        m_kVolumeTargetA_GM = m_kPShaderCMP.GetTexture(4);
        m_kPShaderMIP.SetImageName(5, "OpacityMapA_GM");
        m_kOpacityMapTargetA_GM = m_kPShaderCMP.GetTexture(5);
        m_kPShaderCMP.SetImageName(6,"VolumeImageB");
        m_kPShaderCMP.SetTexture(6, m_kVolumeTargetB);
        m_kPShaderCMP.SetImageName(7, "ColorMapB");
        m_kPShaderCMP.SetTexture(7,m_kColorMapTargetB);
        m_kPShaderCMP.SetImageName(8, "OpacityMapB");
        m_kPShaderCMP.SetTexture(8,m_kOpacityMapTargetB);
        m_kPShaderCMP.SetImageName(9,"VolumeImageB_GM");
        m_kPShaderCMP.GetTexture(9).SetFilterType(Texture.FilterType.LINEAR);
        m_kPShaderCMP.GetTexture(9).SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kPShaderCMP.GetTexture(9).SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kPShaderCMP.GetTexture(9).SetWrapType(2,Texture.WrapType.CLAMP_BORDER);
        m_kVolumeTargetB_GM = m_kPShaderCMP.GetTexture(9);
        m_kPShaderMIP.SetImageName(10, "OpacityMapB_GM");
        m_kOpacityMapTargetB_GM = m_kPShaderCMP.GetTexture(10);


        m_kPShaderSUR = new PixelShader("VolumeShaderSUR_SUR");
        m_kPShaderSUR.SetTextureQuantity(9);
        m_kPShaderSUR.SetImageName(0,"SceneImage");
        m_kPShaderSUR.SetTexture(0,m_kSceneTarget);
        m_kPShaderSUR.SetImageName(1,"VolumeImageA");
        m_kPShaderSUR.SetTexture(1, m_kVolumeTargetA);
        m_kPShaderSUR.SetImageName(2, "ColorMapA");
        m_kPShaderSUR.SetTexture(2, m_kColorMapTargetA);
        m_kPShaderSUR.SetImageName(3, "OpacityMapA");
        m_kPShaderSUR.SetTexture(3, m_kOpacityMapTargetA);
        int iXBound = m_kImageA.getExtents()[0];
        int iYBound = m_kImageA.getExtents()[1];
        int iZBound = m_kImageA.getExtents()[2];
        byte[] aucData = calcImageNormals( m_kImageA );
        m_kNormalA = new GraphicsImage(GraphicsImage.FormatMode.IT_RGB888,
                                       iXBound,iYBound,iZBound,aucData,
                                       "NormalMapA");
        m_kPShaderSUR.SetImageName(4, "NormalMapA" );
        m_kPShaderSUR.GetTexture(4).SetFilterType(Texture.FilterType.LINEAR);
        m_kPShaderSUR.GetTexture(4).SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kPShaderSUR.GetTexture(4).SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kPShaderSUR.GetTexture(4).SetWrapType(2,Texture.WrapType.CLAMP_BORDER);
        m_kNormalMapTargetA = m_kPShaderSUR.GetTexture(4);


        m_kPShaderSUR.SetImageName(5,"VolumeImageB");
        m_kPShaderSUR.SetTexture(5, m_kVolumeTargetB);
        m_kPShaderSUR.SetImageName(6, "ColorMapB");
        m_kPShaderSUR.SetTexture(6,m_kColorMapTargetB);
        m_kPShaderSUR.SetImageName(7, "OpacityMapB");
        m_kPShaderSUR.SetTexture(7,m_kOpacityMapTargetB);
        iXBound = m_kImageB.getExtents()[0];
        iYBound = m_kImageB.getExtents()[1];
        iZBound = m_kImageB.getExtents()[2];
        aucData = calcImageNormals( m_kImageB );
        m_kNormalB = new GraphicsImage(GraphicsImage.FormatMode.IT_RGB888,
                                       iXBound,iYBound,iZBound,aucData,
                                       "NormalMapB");
        m_kPShaderSUR.SetImageName(8, "NormalMapB" );
        m_kPShaderSUR.GetTexture(8).SetFilterType(Texture.FilterType.LINEAR);
        m_kPShaderSUR.GetTexture(8).SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kPShaderSUR.GetTexture(8).SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kPShaderSUR.GetTexture(8).SetWrapType(2,Texture.WrapType.CLAMP_BORDER);
        m_kNormalMapTargetB = m_kPShaderSUR.GetTexture(8);

    }

    /**
     * Change to the MIP mode pixel shader program.
     * @param kRenderer, the Renderer displaying the scene-graph, to which the
     * new shader program is passed.
     */
    public void MIPMode(WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        if ( m_iWhichShader == MIP )
        {
            return;
        }
        m_iWhichShader = MIP;
        SetProgram( m_kPShaderMIP, kRenderer );
    }

    /**
     * Change to the input pixel shader program.
     * @param kShader, the new pixel shader program to use.
     * @param kRenderer, the Renderer displaying the scene-graph, to which the
     * new shader program is passed.
     */
    private void SetProgram(PixelShader kShader,
                            WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        SetPShader(0,kShader);
        LoadPrograms(kRenderer, 0,kRenderer.GetMaxColors(),
                     kRenderer.GetMaxTCoords(),
                     kRenderer.GetMaxVShaderImages(),
                     kRenderer.GetMaxPShaderImages());
        
        ResetClip();
        SetBackgroundColor( m_kBackgroundColor );
        Blend(m_afBlend[0]);
        SetColorImage();
        SetGradientMagnitude();
        SetSelfShadow();
    }

    /**
     * Change to the DDR mode pixel shader program.
     * @param kRenderer, the Renderer displaying the scene-graph, to which the
     * new shader program is passed.
     */
    public void DDRMode(WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        if ( m_iWhichShader == DDR )
        {
            return;
        }
        m_iWhichShader = DDR;
        SetProgram( m_kPShaderDDR, kRenderer );
    }

    /**
     * Change to the Composite mode pixel shader program.
     * @param kRenderer, the Renderer displaying the scene-graph, to which the
     * new shader program is passed.
     */
    public void CMPMode(WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        if ( m_iWhichShader == CMP )
        {
            return;
        }
        m_iWhichShader = CMP;
        SetProgram(m_kPShaderCMP, kRenderer);
    }

    /**
     * Change to the Composite Surface mode pixel shader program.
     * @param kRenderer, the Renderer displaying the scene-graph, to which the
     * new shader program is passed.
     */
    public void SURMode(WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {

        if ( m_iWhichShader == CMP_SUR )
        {
            return;
        }

        m_iWhichShader = CMP_SUR;
        SetProgram(m_kPShaderSUR, kRenderer);
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("Composite") != null )
        {
            pkProgram.GetUC("Composite").SetDataSource(new float[] {1,0,0,0});
        }
        if ( pkProgram.GetUC("CompositeA") != null )
        {
            pkProgram.GetUC("CompositeA").SetDataSource(new float[] {1,0,0,0});
        }
        if ( pkProgram.GetUC("CompositeB") != null )
        {
            pkProgram.GetUC("CompositeB").SetDataSource(new float[] {1,0,0,0});
        }
    }

    /**
     * Change to the Surface mode pixel shader program.
     * @param kRenderer, the Renderer displaying the scene-graph, to which the
     * new shader program is passed.
     */
    public void SURFASTMode(WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        if ( m_iWhichShader == SUR )
        {
            return;
        }
        m_iWhichShader = SUR;
        SetProgram(m_kPShaderSUR, kRenderer);
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("Composite") != null )
        {
            pkProgram.GetUC("Composite").SetDataSource(new float[] {0,0,0,0});
        }
        if ( pkProgram.GetUC("CompositeA") != null )
        {
            pkProgram.GetUC("CompositeA").SetDataSource(new float[] {0,0,0,0});
        }
        if ( pkProgram.GetUC("CompositeB") != null )
        {
            pkProgram.GetUC("CompositeB").SetDataSource(new float[] {0,0,0,0});
        }
    }

    /**
     * Sets the IsColor shader parameter values.
     */
    private void SetColorImage()
    {
        Program pkProgram = GetPProgram(0);
        if ( m_kImageA.isColorImage() )
        {
            if ( pkProgram.GetUC("IsColor") != null ) 
            {
                pkProgram.GetUC("IsColor").SetDataSource(new float[]{1,0,0,0});
            }
            if ( pkProgram.GetUC("IsColorA") != null ) 
            {
                pkProgram.GetUC("IsColorA").SetDataSource(new float[]{1,0,0,0});
            }
        }
        if ( (m_kImageB != null) && m_kImageB.isColorImage() )
        {
            if ( pkProgram.GetUC("IsColorB") != null ) 
            {
                pkProgram.GetUC("IsColorB").SetDataSource(new float[]{1,0,0,0});
            }
        }
    }
    


    private int get_index(int iX, int iY, int iZ, int iXBound, int iYBound)
    {
        return iX + iXBound * (iY + (iYBound * iZ));
    }
    
    /**
     * Calculates the image normals for the input ModelImage. Stores them in a byte[].
     * @param kImage, the ModelImage to calculate the normals for.
     * @return byte[] the image normals in byte format. 
     */
    private byte[] calcImageNormals( ModelImage kImage )
    {

        ModelSimpleImage kValueImageA;
        float[] afData;

        // Extract image slice.
        ModelSimpleImage kSimpleImageA = new ModelSimpleImage(kImage, 0);

        // Convert to intensity valued image.
        if (kImage.isColorImage()) {
            kValueImageA = kSimpleImageA.createIntensityImage();
            afData = kValueImageA.data;
        } else {
            afData = kSimpleImageA.data;
        }
        kSimpleImageA = null;
        System.gc();


        // Access intensity values as a linear array.
        int iXBound = kImage.getExtents()[0];;
        int iYBound = kImage.getExtents()[1];;
        int iZBound = kImage.getExtents()[2];;
        int iXYBound = iXBound * iYBound;

        // normals from gradient which are computed using central finite
        // differences everywhere except forward/backward finite differences
        // are used at the edges

        float fDX = 0;
        float fDY = 0;
        float fDZ = 0;

        int iOffX = 1;
        int iOffY = iXBound;
        int iOffZ = iXBound * iYBound;
        int iX, iY, iZ;

        float[] afDataN = new float[afData.length*3];

        for (iZ = 0; iZ < iZBound ; iZ++) {
           int iZplus1 = (iZBound - 1) == iZ ? iZ : iZ + 1;
           int iZminus1 = (0) == iZ ? iZ : iZ - 1;

            for (iY = 0; iY < iYBound ; iY++) {
               int iYplus1 = (iYBound - 1) == iY ? iY : iY + 1;
               int iYminus1 = (0) == iY ? iY : iY - 1;

                for (iX = 0; iX < iXBound; iX++) {
                   int iXplus1 = (iXBound - 1) == iX ? iX : iX + 1;
                   int iXminus1 = (0) == iX ? iX : iX - 1;

                    fDX = (((afData[get_index(iXminus1, iYminus1, iZ, iXBound, iYBound)] -
                             afData[get_index(iXplus1, iYminus1, iZ, iXBound, iYBound)]) * 0.71f) +
                           
                           (afData[get_index(iXminus1, iY, iZ, iXBound, iYBound)] -
                            afData[get_index(iXplus1, iY, iZ, iXBound, iYBound)]) +
                            
                           ((afData[get_index(iXminus1, iYplus1, iZ, iXBound, iYBound)] -
                             afData[get_index(iXplus1, iYplus1, iZ, iXBound, iYBound)]) * 0.71f) );

                    fDY = (((afData[get_index(iXminus1, iYminus1, iZ, iXBound, iYBound)] -
                             afData[get_index(iXminus1, iYplus1, iZ, iXBound, iYBound)]) * 0.71f) +
                           
                           (afData[get_index(iX, iYminus1, iZ, iXBound, iYBound)] -
                            afData[get_index(iX, iYplus1, iZ, iXBound, iYBound)]) +
                            
                           ((afData[get_index(iXplus1, iYminus1, iZ, iXBound, iYBound)] -
                             afData[get_index(iXplus1, iYplus1, iZ, iXBound, iYBound)]) * 0.71f) );
                       
                    fDZ = (((afData[get_index(iXminus1, iY, iZplus1, iXBound, iYBound)] -
                             afData[get_index(iXminus1, iY, iZminus1, iXBound, iYBound)]) * 0.71f) +
                           
                           (afData[get_index(iX, iY, iZplus1, iXBound, iYBound)] -
                            afData[get_index(iX, iY, iZminus1, iXBound, iYBound)]) +
                            
                           ((afData[get_index(iXplus1, iY, iZplus1, iXBound, iYBound)] -
                             afData[get_index(iXplus1, iY, iZminus1, iXBound, iYBound)]) * 0.71f) );
                       
                    if ((fDX != 0.0f) || (fDY != 0.0f) || (fDZ != 0.0f)) {
                        int i = get_index (iX, iY, iZ, iXBound, iYBound);
                        afDataN[i*3+0] = fDX;
                        afDataN[i*3+1] = fDY;
                        afDataN[i*3+2] = fDZ;
                    }
                }
            }
        }

        int[] aiNormalAverageIndex = new int[]{ 0, -1, +1, -iXBound, +iXBound, -iXYBound, +iXYBound };
        WildMagic.LibFoundation.Mathematics.Vector3f kNormal = new WildMagic.LibFoundation.Mathematics.Vector3f();
        WildMagic.LibFoundation.Mathematics.Vector3f kNormalTmp = new WildMagic.LibFoundation.Mathematics.Vector3f();
        byte[] acData = new byte[afData.length*3];
                   
        // Catch any zero-vector normals and replace them by an average of
        // neighboring normals.
        for (iZ = 1; iZ < (iZBound - 1); iZ++) {

            for (iY = 1; iY < (iYBound - 1); iY++) {
                int offset = iXBound * (iY + (iYBound * iZ));

                for (iX = 1; iX < (iXBound - 1); iX++) {
                    int i = iX + offset;

                    kNormal.Copy(WildMagic.LibFoundation.Mathematics.Vector3f.ZERO);
                    for ( int iN = 0; iN < aiNormalAverageIndex.length; iN++ )
                    {
                        int index = i + aiNormalAverageIndex[iN];
                        index *= 3;
                        kNormalTmp.X = afDataN[index + 0];
                        kNormalTmp.Y = afDataN[index + 1];
                        kNormalTmp.Z = afDataN[index + 2];

                        kNormal.Add( kNormalTmp );
                    }
                    kNormal.Normalize();
                    acData[i*3+0] = (byte)(kNormal.X*127 + 127);
                    acData[i*3+1] = (byte)(kNormal.Y*127 + 127);
                    acData[i*3+2] = (byte)(kNormal.Z*127 + 127);
                }
            }
        }

//         // Access intensity values as a linear array.
//         int iXBound = kImage.getExtents()[0];;
//         int iYBound = kImage.getExtents()[1];;
//         int iZBound = kImage.getExtents()[2];;
//         int iXYBound = iXBound * iYBound;

//         // normals from gradient which are computed using central finite
//         // differences everywhere except forward/backward finite differences
//         // are used at the edges

//         float fDX = 0;
//         float fDY = 0;
//         float fDZ = 0;

//         int iOffX = 1;
//         int iOffY = iXBound;
//         int iOffZ = iXBound * iYBound;
//         int iX, iY, iZ;

//         float[] afDataN = new float[afData.length*3];

//         for (iZ = 1; iZ < (iZBound - 1); iZ++) {
//             boolean bMinZ = 0 == iZ;
//             boolean bMaxZ = (iZBound - 1) == iZ;

//             for (iY = 1; iY < (iYBound - 1); iY++) {
//                 boolean bMinY = 0 == iY;
//                 boolean bMaxY = (iYBound - 1) == iY;
//                 int offset = iXBound * (iY + (iYBound * iZ));

//                 for (iX = 0; iX < iXBound; iX++) {
//                     boolean bMinX = 0 == iX;
//                     boolean bMaxX = (iXBound - 1) == iX;

//                     int i = iX + offset;

//                     fDX = (((bMinX ? afData[i] : afData[i - iOffX - iXBound]) -
//                             (bMaxX ? afData[i] : afData[i + iOffX - iXBound])) * 0.71f) +
                            
//                         (bMinX ? afData[i] : afData[i - iOffX]) - (bMaxX ? afData[i] : afData[i + iOffX]) +
//                         (
                             
//                          ((bMinX ? afData[i] : afData[i - iOffX + iXBound]) -
//                           (bMaxX ? afData[i] : afData[i + iOffX + iXBound])) * 0.71f);
                        
//                     fDY = (((bMinY ? afData[i] : afData[i - iOffY - 1]) - (bMaxY ? afData[i] : afData[i + iOffY - 1])) *
//                            0.71f) +
                            
//                         (bMinY ? afData[i] : afData[i - iOffY]) - (bMaxY ? afData[i] : afData[i + iOffY]) +
//                         (
                             
//                          ((bMinY ? afData[i] : afData[i - iOffY + 1]) - (bMaxY ? afData[i] : afData[i + iOffY + 1])) * 0.71f);
                        
//                     fDZ = (((bMinZ ? afData[i] : afData[i - iOffZ - 1]) - (bMaxZ ? afData[i] : afData[i + iOffZ - 1])) *
//                            0.71f) +
                            
//                         (bMinZ ? afData[i] : afData[i - iOffZ]) - (bMaxZ ? afData[i] : afData[i + iOffZ]) +
//                         (
                             
//                          ((bMinZ ? afData[i] : afData[i - iOffZ + 1]) - (bMaxZ ? afData[i] : afData[i + iOffZ + 1])) * 0.71f);
                        
                        
//                     if ((fDX != 0.0f) || (fDY != 0.0f) || (fDZ != 0.0f)) {
//                         afDataN[i*3+0] = fDX;
//                         afDataN[i*3+1] = fDY;
//                         afDataN[i*3+2] = fDZ;
//                     }
//                 }
//             }
//         }

//         int[] aiNormalAverageIndex = new int[]{ 0, -1, +1, -iXBound, +iXBound, -iXYBound, +iXYBound };
//         Vector3f kNormal = new Vector3f();
//         Vector3f kNormalTmp = new Vector3f();
//         byte[] acData = new byte[afData.length*3];
                    
//         // Catch any zero-vector normals and replace them by an average of
//         // neighboring normals.
//         for (iZ = 1; iZ < (iZBound - 1); iZ++) {

//             for (iY = 1; iY < (iYBound - 1); iY++) {
//                 int offset = iXBound * (iY + (iYBound * iZ));

//                 for (iX = 1; iX < (iXBound - 1); iX++) {
//                     int i = iX + offset;

//                     kNormal.copy(Vector3f.ZERO);
//                     for ( int iN = 0; iN < aiNormalAverageIndex.length; iN++ )
//                     {
//                         int index = i + aiNormalAverageIndex[iN];
//                         index *= 3;
//                         kNormalTmp.X(afDataN[index + 0]);
//                         kNormalTmp.Y(afDataN[index + 1]);
//                         kNormalTmp.Z(afDataN[index + 2]);

//                         kNormal.addEquals( kNormalTmp );
//                     }
//                     kNormal.Normalize();
//                     acData[i*3+0] = (byte)(kNormal.X()*127 + 127);
//                     acData[i*3+1] = (byte)(kNormal.Y()*127 + 127);
//                     acData[i*3+2] = (byte)(kNormal.Z()*127 + 127);
//                 }
//             }
//         }
        aiNormalAverageIndex = null;
        kNormal = null;
        kNormalTmp = null;
        afDataN = null;

        return acData;
    }

    /**
     * Update the image data.
     * @param kImage, the modified ModelImage
     * @param iImage, which image (imageA, imageB)
     */
    public void UpdateData( ModelImage kImage, int iImage )
    {
        if ( iImage == 0 )
        {
            m_kImageA = kImage;
            this.UpdateData( m_kImageA, m_kVolumeA, m_kVolumeTargetA, new String( "A" ) );
        }
    }

    /**
     * Update the image volume data on the GPU.
     * @param kImage, the new ModelImage
     * @param kVolumeImage, the volume data image.
     * @param kVolumeTexture, the volume data texture.
     * @param kPostFix, the postfix string for the image name.
     */
    private GraphicsImage UpdateData( ModelImage kImage, GraphicsImage kVolumeImage,
                                      Texture kVolumeTexture, String kPostFix )
    {
        int iXBound = kImage.getExtents()[0];
        int iYBound = kImage.getExtents()[1];
        int iZBound = kImage.getExtents()[2];
        kImage.calcMinMax();
        float fImageMax = (float)kImage.getMax();
        float fImageMin = (float)kImage.getMin();

        if ( kImage.isColorImage() )
        {
            byte[] aucData = new byte[4*iXBound*iYBound*iZBound];

            try {
                kImage.exportData( 0, kImage.getSize(), aucData );
                for ( int i = 0; i < kImage.getSize(); i += 4)
                {
                    byte tmp = aucData[i];
                    aucData[i] = aucData[i+1];
                    aucData[i+1] = aucData[i+2];
                    aucData[i+2] = aucData[i+3];
                    aucData[i+3] = tmp;
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
            if ( kVolumeImage == null )
            {
                kVolumeImage =
                    new GraphicsImage( GraphicsImage.FormatMode.IT_RGBA8888,
                                       iXBound,iYBound,iZBound,aucData,
                                       new String( "VolumeImage" + kPostFix));
            }
            if ( kVolumeTexture != null )
            {
                kVolumeTexture.GetImage().SetData( aucData, iXBound, iYBound, iZBound );
                kVolumeTexture.Release();
            }
        }
        else
        {
            float[] afData = new float[iXBound*iYBound*iZBound];

            int i = 0;
            for (int iZ = 0; iZ < iZBound; iZ++)
            {
                for (int iY = 0; iY < iYBound; iY++)
                {
                    for (int iX = 0; iX < iXBound; iX++)
                    {
                        float fValue = kImage.getFloat(iX,iY,iZ);
                        afData[i++] = (fValue - fImageMin)/(fImageMax - fImageMin);
                    }
                }
            }
            if ( kVolumeImage == null )
            {
                kVolumeImage =
                    new GraphicsImage( GraphicsImage.FormatMode.IT_L8,
                                       iXBound,iYBound,iZBound,afData,
                                       new String( "VolumeImage" + kPostFix));
            }

            if ( kVolumeTexture != null )
            {
                kVolumeTexture.GetImage().SetFloatData( afData, iXBound, iYBound, iZBound );
                kVolumeTexture.Release();
            }
        }

        return kVolumeImage;
    }

    /**
     * Update the transfer function for the image iImage.
     * @param kTransfer, the new opacity transfer function
     * @param iImage, the image to modify (0 = imageA, 1 = imageB, 2 = imageA_GM, 3 = imageB_GM)
     * @return boolean true when updated, false otherwise.
     */
    public boolean UpdateImages(TransferFunction kTransfer, int iImage)
    {
        if ( iImage == 0 )
        {
            return UpdateImages( m_kImageA, m_kOpacityMapTargetA, m_kOpacityMapA, kTransfer );
        }
        else if ( (iImage == 1) && m_kImageB != null )
        {
            return UpdateImages( m_kImageB, m_kOpacityMapTargetB, m_kOpacityMapB, kTransfer );
        }
        else if ( (iImage == 2) &&
                  (m_kImageA_GM != null) &&
                  (m_kOpacityMapTargetA_GM != null) &&
                  (m_kOpacityMapA_GM != null)  )
        {
            return UpdateImages( m_kImageA_GM, m_kOpacityMapTargetA_GM, m_kOpacityMapA_GM, kTransfer );
        }
        else if ( (iImage == 3) &&
                  (m_kImageB_GM != null) &&
                  (m_kOpacityMapTargetB_GM != null) &&
                  (m_kOpacityMapB_GM != null)  )
        {
            return UpdateImages( m_kImageB_GM, m_kOpacityMapTargetB_GM, m_kOpacityMapB_GM, kTransfer );
        }
        return false;
    }

    /**
     * Update the opacity transfer function.
     * @param kImage, the ModelImage the transfer function applies to.
     * @param kOpacityTexture, the opacity Texture passed to the GPU
     * @param kOpacityMap, the opacity data stored in the GraphicsImage
     * @param kTransfer, the new transfer function.
     */
    private boolean UpdateImages(ModelImage kImage, Texture kOpacityTexture,
                                 GraphicsImage kOpacityMap, TransferFunction kTransfer)
    {
         int iLutHeight = 256;
         float[] afData = kOpacityMap.GetFloatData();

         float fRange = (float)(kImage.getMax() - kImage.getMin());
         float fStep = fRange / (float)iLutHeight;
         float fDataValue = (float)kImage.getMin();
         for (int i = 0; i < iLutHeight; i++) {
             afData[i] = (kTransfer.getRemappedValue( fDataValue, iLutHeight )/255.0f);
             fDataValue += fStep;
         }
         kOpacityTexture.Reload(true);
         return true;
    }

    /**
     * Update the LUT for imageA and imageB.
     * @param kLUTa, new LUT for imageA.
     * @param kLUTb, new LUT for imageB.
     */
    public void UpdateImages(ModelLUT kLUTa, ModelLUT kLUTb)
    {
        this.UpdateImages( m_kColorMapTargetA, m_kColorMapA, kLUTa );
        if ( m_kImageB != null )
        {
            this.UpdateImages( m_kColorMapTargetB, m_kColorMapB, kLUTb );
        }
    }

    /**
     * Update the LUT texture sent to the GPU.
     * @param kColorTexture, the color-map Texture object.
     * @param kColorMap, the color-map GraphicsImage object (stores data).
     * @param kLUT, the new LUT.
     */
    private void UpdateImages(Texture kColorTexture, GraphicsImage kColorMap, ModelLUT kLUT)
    {
        if ( kLUT == null )
        {
            return;
        }
        byte[] oldData = kColorMap.GetData();
        byte[] aucData = ModelLUT.exportIndexedLUTMin( kLUT );

        kColorMap.SetData(aucData, aucData.length/4);
        if ( oldData.length != aucData.length )
        {
            kColorTexture.Release();
        }
        else
        {
            kColorTexture.Reload(true);
        }
    }

    /**
     * Initialize the textures for the color lookup table.
     * @param kLUT, the new LUT.
     * @param kRGBT, the new RGB table.
     * @param kPostfix, the string postfix to concatenate to the "ColorMap" image name.
     * @return GraphicsImage, the new GraphicsImage storing the colormap lookup table.
     */
    public GraphicsImage InitColorMap ( ModelLUT kLUT, ModelRGB kRGBT, String kPostFix )
    {
        byte[] aucData;
        if ( kLUT == null )
        {
            aucData = ModelLUT.exportIndexedLUTMin( kRGBT );
            
        }
        else
        {
            aucData = ModelLUT.exportIndexedLUTMin( kLUT );
        }
        return new GraphicsImage(
                                 GraphicsImage.FormatMode.IT_RGBA8888,aucData.length/4,aucData,
                                 new String( "ColorMap" + kPostFix ) );
    }

    /**
     * Initialize the textures for the opacity lookup table.
     * @param kImage, the ModelImage the opacity transfer function applies to.
     * @param kPostfix, the string postfix to concatenate to the "OpacityMap" image name.
     * @return GraphicsImage, the new GraphicsImage storing the colormap lookup table.
     */
    public GraphicsImage InitOpacityMap (ModelImage kImage, String kPostFix )
    {
        int iLutHeight = 256;
        float[] afData = new float[iLutHeight];
        float fRange = (float)(kImage.getMax() - kImage.getMin());
        float fStep = fRange / (float)iLutHeight;
        float fDataValue = (float)kImage.getMin();
        for (int i = 0; i < iLutHeight; i++) {
            afData[i] = (float)( iLutHeight * (kImage.getMax() - fDataValue) / fRange);
            fDataValue += fStep;
        }

        return new GraphicsImage(
                                 GraphicsImage.FormatMode.IT_L8,iLutHeight,afData,
                                 new String( "OpacityMap" + kPostFix ));
    }

    /**
     * Return the Texture containing the imageA volume data.
     * @return Texture containing the imageA volume data.
     */
    public Texture GetVolumeTargetA()
    {
        return m_kVolumeTargetA;
    }

    /**
     * Return the Texture containing the imageB volume data.
     * @return Texture containing the imageB volume data.
     */
    public Texture GetVolumeTargetB()
    {
        return m_kVolumeTargetB;
    }

    /**
     * Release the Textures containing the imageA (imageB) volume data. Once
     * Textures are released, they will be re-loaded onto the GPU during the
     * next frame.
     */
    public void ReleaseVolume()
    {
        m_kVolumeTargetA.Release();
        if ( m_kImageB != null )
        {
            m_kVolumeTargetB.Release();
        }
    }

    /**
     * Reload the current shader programs from disk, compile and parse and
     * send to the GPU.
     * @param kRenderer, the Renderer object displaying the scene-graph which
     * will apply the shader programs.
     */
    public void Reload( WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        Program kVProgram = GetVProgram(0);
        kVProgram.Release();
        VertexProgramCatalog.GetActive().Remove(kVProgram);

        Program kPProgram = GetPProgram(0);
        kPProgram.Release();
        PixelProgramCatalog.GetActive().Remove(kPProgram);

        VertexShader pkVShader = GetVShader(0);
        pkVShader.OnReleaseProgram();
        PixelShader pkPShader = GetPShader(0);
        pkPShader.OnReleaseProgram();

        LoadPrograms(kRenderer, 0,kRenderer.GetMaxColors(),
                     kRenderer.GetMaxTCoords(),
                     kRenderer.GetMaxVShaderImages(),
                     kRenderer.GetMaxPShaderImages());

        if ( m_iWhichShader == MIP )
        {
            m_iWhichShader = -1;
            MIPMode(kRenderer);
        }
        else if ( m_iWhichShader == DDR )
        {
            m_iWhichShader = -1;
            DDRMode(kRenderer);
        }
        else if ( m_iWhichShader == CMP )
        {
            m_iWhichShader = -1;
            CMPMode(kRenderer);
        }
        else if ( m_iWhichShader == SUR )
        {
            m_iWhichShader = -1;
            SURFASTMode(kRenderer);
        }
        else if ( m_iWhichShader == CMP_SUR )
        {
            m_iWhichShader = -1;
            SURMode(kRenderer);
        }
    }

    /**
     * Init the axis-aligned clip planes.
     */
    public void InitClip( float[] afData )
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("DoClip") != null ) 
        {
            pkProgram.GetUC("DoClip").SetDataSource(new float[]{0,0,0,0});
        }       

        for ( int i = 0; i < 6; i++ )
        {
            if ( pkProgram.GetUC(m_akClip[i]) != null ) 
            {
                pkProgram.GetUC(m_akClip[i]).SetDataSource(new float[]{afData[i],0,0,0});
                m_aafClipData[i][0] = afData[i];
            }       
        }
    }

    /**
     * Reset the axis-aligned clip planes, eye, inverse-eye and arbitrary clip
     * planes to neutral.
     */
    private void ResetClip()
    {
        for ( int i = 0; i < 6; i++ )
        {
            if ( m_aafClipData[i] != null )
            {
                SetClip( i, m_aafClipData[i] );
            }
        }
        if ( m_afClipEyeData != null )
        {
            SetClipEye(m_afClipEyeData);
        }
        if ( m_afClipEyeInvData != null )
        {
            SetClipEyeInv(m_afClipEyeInvData);
        }
        if ( m_afClipArbData != null )
        {
            SetClipArb(m_afClipArbData);
        }
    }

    /**
     * Enable and set the axis-aligned clip plane.
     * @param iWhich, one of 6 clip-planes to set.
     * @param data, the distance to the clip-plane.
     */
    public void SetClip(int iWhich, float[] data)
    {
        m_aafClipData[iWhich] = data;
        EnableClip();
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC(m_akClip[iWhich]) != null ) 
        {
            pkProgram.GetUC(m_akClip[iWhich]).SetDataSource(data);
        }
    }
    /**
     * Enable clipping.
     */
    private void EnableClip()
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("DoClip") != null ) 
        {
            pkProgram.GetUC("DoClip").SetDataSource(new float[]{1,0,0,0});
        }       
    }

    /**
     * Enable and set the eye clip plane.
     * @param afEquation, the clip-plane equation.
     */
    public void SetClipEye(float[] afEquation)
    {
        m_afClipEyeData = afEquation;
        EnableClip();
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("clipEye") != null ) 
        {
            pkProgram.GetUC("clipEye").SetDataSource(afEquation);
        }
    }

    /**
     * Enable and set the inverse-eye clip plane.
     * @param afEquation, the clip-plane equation.
     */
    public void SetClipEyeInv(float[] afEquation)
    {
        m_afClipEyeInvData = afEquation;
        EnableClip();
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("clipEyeInv") != null ) 
        {
            pkProgram.GetUC("clipEyeInv").SetDataSource(afEquation);
        }
    }

    /**
     * Enable and set the arbitrary clip plane.
     * @param afEquation, the clip-plane equation.
     */
    public void SetClipArb(float[] afEquation)
    {
        m_afClipArbData = afEquation;
        EnableClip();
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("clipArb") != null ) 
        {
            pkProgram.GetUC("clipArb").SetDataSource(afEquation);
        }
    }

    /**
     * Sets the light type for the given light.
     * @param kLightType, the name of the light to set (Light0, Light1, etc.)
     * @param afType, the type of light (Ambient = 0, Directional = 1, Point = 2, Spot = 3).
     */
    public void SetLight( String kLightType, float[] afType )
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC(kLightType) != null)
        {
            pkProgram.GetUC(kLightType).SetDataSource(afType);
        }
    }

    /**
     * Sets the ModelRGB for the iImage.
     * @param kRGBT, new ModelRGB
     * @param iImage, set imageA when iImage = 0, set imageB when iImage = 1.
     */
    public void SetRGBT(ModelRGB kRGBT, int iImage)
    {
        if ( iImage == 0 )
        {
            SetRGBT( m_kColorMapTargetA, m_kColorMapA, kRGBT );
        }
        else if ( m_kImageB != null )
        {
            SetRGBT( m_kColorMapTargetB, m_kColorMapB, kRGBT );
        }
    }

    /**
     * Sets the Texture object containing the color lookup table based on the ModelRGB.
     * @param kTexture, the Texture object containing the colormap GraphicsImage.
     * @param kImage, the GraphicsImage containing the colormap data.
     * @param kRGBT, the new ModelRGB.
     */
    private void SetRGBT( Texture kTexture, GraphicsImage kImage, ModelRGB kRGBT )
    {
        if ( kRGBT == null )
        {
            return;
        }
        byte[] oldData = kImage.GetData();
        byte[] aucData = ModelLUT.exportIndexedLUTMin( kRGBT );
        kImage.SetData(aucData, aucData.length/4);
        if ( oldData.length != aucData.length )
        {
            kTexture.Release();
        }
        else
        {
            kTexture.Reload(true);
        }
    }

    /**
     * Returns the current pixel program.
     * @return the current pixel program.
     */
    public Program GetPProgram()
    {
        return GetPProgram(0);
    }

    /**
     * Sets the blend factor shader parameter between imageA and imageB.
     * @param fBlend, blend factor (range = 0-1).
     */
    public void Blend(float fBlend)
    {
        m_afBlend[0] = fBlend;
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("blend") != null ) 
        {
            pkProgram.GetUC("blend").SetDataSource(m_afBlend);
        }
    }

    /**
     * Sets the steps size factor shader parameter.
     * @param stepsSize, blend factor (range = 0-1).
     */
    public void setSteps(float fsteps)
    {
        stepsSize[0] = fsteps;
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("steps") != null ) 
        {
            pkProgram.GetUC("steps").SetDataSource(stepsSize);
        }
    }

    
    /**
     * Sets the BackgroundColor shader parameter.
     * @param kColor, new BackgroundColor.
     */
    public void SetBackgroundColor( ColorRGBA kColor )
    {
        m_kBackgroundColor = kColor;
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("BackgroundColor") != null ) 
        {
            float[] afColor = new float[4];
            afColor[0] = kColor.R;
            afColor[1] = kColor.G;
            afColor[2] = kColor.B;
            afColor[3] = kColor.A;
            pkProgram.GetUC("BackgroundColor").SetDataSource(afColor);
        }
    }

    /**
     * Loads the gradient magnitude image instead of recalculating the image.
     *
     * @param   dName     String User specified directory name.
     * @param   fName     String GM image file name.
     * @return  ModelImage containing GM image, null when no image loaded.
     */
    private ModelImage loadGMImage(String dName, String fName)
    {
        FileIO fileIO = new FileIO();
        fileIO.setQuiet(true);

        if (new File(dName + File.separator + fName).exists()) {

            return fileIO.readImage(fName, dName, false, null, false);
        }
        return null;
    }

    /**
     * Calculates histogram for the gradient magnitude ModelImage
     * @param kImage, the image to calculate the gradient magnitude.
     * @return  ModelImage containing GM image, null when no image calculated.
     */
    private ModelImage CalcHistogramsGM( ModelImage kImage )
    {
        ModelImage kImage_GM = null;
        if (kImage != null) {
            kImage_GM = loadGMImage(ViewUserInterface.getReference().getDefaultDirectory(),
                                           kImage.getImageName() + "_gm_rescale" + ".xml");

            if ( kImage_GM == null )
            {
                kImage_GM = new ModelImage(kImage.getType(), kImage.getExtents(),
                                                  kImage.getImageName() + "_gm_rescale");

                float[] sigma = new float[] { 0.5f, 0.5f, 0.5f };
                AlgorithmGradientMagnitude gradMagAlgo_A =
                    new AlgorithmGradientMagnitude(kImage_GM, kImage, sigma,true, false);

                gradMagAlgo_A.setRunningInSeparateThread(false);
                gradMagAlgo_A.run();

                if (gradMagAlgo_A.isCompleted()) {
                    gradMagAlgo_A.finalize();
                    gradMagAlgo_A = null;
                }
                kImage_GM.calcMinMax();
                /** Scale the intensity range to 1024. */
                AlgorithmChangeType changeTypeAlgo_A =
                    new AlgorithmChangeType(kImage_GM, kImage_GM.getType(),
                                            kImage_GM.getMin(), kImage_GM.getMax(),
                                            0, 1023, false);

                changeTypeAlgo_A.setRunningInSeparateThread(false);
                changeTypeAlgo_A.run();
                kImage_GM.calcMinMax();

                if (changeTypeAlgo_A.isCompleted()) {
                    ModelImage.saveImage(kImage_GM);
                    changeTypeAlgo_A.finalize();
                    changeTypeAlgo_A = null;
                }
            }
        }
        return kImage_GM;
    }

    /** 
     * Enables/Disables self-shadowing for the Surface mode.
     * @param bShadow, self-shadowing on/off.
     */
    public void SelfShadow( boolean bShadow )
    {
        m_afSelfShadow[0] = 0;
        if ( bShadow )
        {
            m_afSelfShadow[0] = 1;
        }
        SetSelfShadow();
    }

    /**
     * Sets the SelfShadow shader parameter.
     */
    private void SetSelfShadow()
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("SelfShadow") != null ) 
        {
            pkProgram.GetUC("SelfShadow").SetDataSource(m_afSelfShadow);
        }
    }

    /** 
     * Enables/Disables gradient magnitude filter.
     * @param bShow, gradient magnitude filter on/off.
     */
    public void SetGradientMagnitude(boolean bShow)
    {
        m_afGradientMagnitude[0] = 0;
        if ( bShow )
        {
            m_afGradientMagnitude[0] = 1;
        }
        SetGradientMagnitude();
    }

    /**
     * Sets the GradientMagnitude shader parameter.
     */
    private void SetGradientMagnitude()
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("GradientMagnitude") != null ) 
        {
            pkProgram.GetUC("GradientMagnitude").SetDataSource(m_afGradientMagnitude);
        }
    }

    /**
     * Loads this object from the input parameter rkStream, using the input
     * Stream.Link to store the IDs of children objects of this object
     * for linking after all objects are loaded from the Stream.
     * @param rkStream, the Stream from which this object is being read.
     * @param pkLink, the Link class for storing the IDs of this object's
     * children objcts.
     */
    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);

        // native data
    }

    /**
     * Copies this objects children objects from the input Stream's HashTable,
     * based on the LinkID of the child stored in the pkLink paramter.
     * @param rkStream, the Stream where the child objects are stored.
     * @param pkLink, the Link class from which the child object IDs are read.
     */
    public void Link (Stream rkStream, Stream.Link pkLink)
    {
        super.Link(rkStream,pkLink);
    }

    /**
     * Registers this object with the input Stream parameter. All objects
     * streamed to disk are registered with the Stream so that a unique list
     * of objects is maintained.
     * @param rkStream, the Stream where the child objects are stored.
     * @return true if this object is registered, false if the object has
     * already been registered.
     */
    public boolean Register (Stream rkStream)
    {
        if (!super.Register(rkStream))
        {
            return false;
        }
        return true;
    }

    /**
     * Write this object and all it's children to the Stream.
     * @param rkStream, the Stream where the child objects are stored.
     */
    public void Save (Stream rkStream)
    {
        super.Save(rkStream);

        // native data
        //rkStream.Write(m_iPassQuantity);

        // link data
//         for (int iPass = 0; iPass < m_iPassQuantity; iPass++)
//         {
//             rkStream.Write(m_kVShader.get(iPass).GetID());
//             rkStream.Write(m_kPShader.get(iPass).GetID());
//         }
//         int iQuantity = m_kAlphaState.size();
//         rkStream.Write(iQuantity);
//         for (int i = 0; i < iQuantity; i++)
//         {
//             rkStream.Write(m_kAlphaState.get(i).GetID());
//         }

    }

    /**
     * Returns the size of this object and it's children on disk for the
     * current StreamVersion parameter.
     * @param rkVersion, the current version of the Stream file being created.
     * @return the size of this object on disk.
     */
    public int GetDiskUsed (StreamVersion rkVersion)
    {
        int iSize = super.GetDiskUsed(rkVersion);

        return iSize;
    }

    /**
     * Write this object into a StringTree for the scene-graph visualization.
     * @param acTitle, the header for this object in the StringTree.
     * @return StringTree containing a String-based representation of this
     * object and it's children.
     */
    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        // strings
        pkTree.Append(StringTree.Format("VolumeShaderEffect",GetName()));
        pkTree.Append(super.SaveStrings(null));

        return pkTree;
    }

    /** Reference to ModelImage imageA in ViewJFrameVolumeView */
    private ModelImage m_kImageA;
    /** ModelLUT applied to m_kImageA */
    private ModelLUT m_kLUTA;
    /** ModelRGB applied to m_kImageA */
    private ModelRGB m_kRGBA;
    /** Opacity transfer function applied to m_kImageA */
    private TransferFunction m_kTransferA;

    /** Reference to ModelImage imageB in ViewJFrameVolumeView */
    private ModelImage m_kImageB;
    /** ModelLUT applied to m_kImageB */
    private ModelLUT m_kLUTB;
    /** ModelRGB applied to m_kImageB */
    private ModelRGB m_kRGBB;
    /** Opacity transfer function applied to m_kImageB */
    private TransferFunction m_kTransferB;

    /** Gradient magnitude for m_kImageA */
    private ModelImage m_kImageA_GM = null;
    /** GraphicsImage contains GM opacity tranfer function data: */
    private GraphicsImage m_kOpacityMapA_GM = null;
    /** Texture contains texture filter modes and GraphicsImage for opacity
     * transfer function: */
    private Texture m_kOpacityMapTargetA_GM = null;
    /** GraphicsImage contains volume data for gradient magnitude */
    private GraphicsImage m_kVolumeA_GM = null;
    /** Texture contains the texture filter modes and GraphicsImage for
     * gradient magnitude */
    private Texture m_kVolumeTargetA_GM = null;
    
    /** Gradient magnitude for m_kImageB */
    private ModelImage m_kImageB_GM = null;
    /** GraphicsImage contains GM opacity tranfer function data: */
    private GraphicsImage m_kOpacityMapB_GM = null;
    /** Texture contains texture filter modes and GraphicsImage for opacity
     * transfer function: */
    private Texture m_kOpacityMapTargetB_GM = null;
    /** GraphicsImage contains volume data for gradient magnitude */
    private GraphicsImage m_kVolumeB_GM = null;
    /** Texture contains the texture filter modes and GraphicsImage for
     * gradient magnitude */
    private Texture m_kVolumeTargetB_GM = null;

    /** Data storage for imageA volume: */
    private GraphicsImage m_kVolumeA;
    /** Data storage for imageA normals: */
    private GraphicsImage m_kNormalA;
    /** Data storage for imageA color map: */
    private GraphicsImage m_kColorMapA;
    /** Data storage for imageA opacity map: */
    private GraphicsImage m_kOpacityMapA = null;
    /** Texture object for imageA data: */
    private Texture m_kVolumeTargetA;
    /** Texture object for imageA color map: */
    private Texture m_kColorMapTargetA;
    /** Texture object for imageA opacity map: */
    private Texture m_kOpacityMapTargetA;
    /** Texture object for imageA normal map: */
    private Texture m_kNormalMapTargetA;

    /** Data storage for imageB volume: */
    private GraphicsImage m_kVolumeB;
    /** Data storage for imageB normals: */
    private GraphicsImage m_kNormalB;
    /** Data storage for imageB color map: */
    private GraphicsImage m_kColorMapB;
    /** Data storage for imageB opacity map: */
    private GraphicsImage m_kOpacityMapB = null;
    /** Texture object for imageB data: */
    private Texture m_kVolumeTargetB;
    /** Texture object for imageB color map: */
    private Texture m_kColorMapTargetB;
    /** Texture object for imageB opacity map: */
    private Texture m_kOpacityMapTargetB;
    /** Texture object for imageB normal map: */
    private Texture m_kNormalMapTargetB;

    /** PixelShader program and data for MIP mode: */
    private PixelShader m_kPShaderMIP = null;
    /** PixelShader program and data for DDR mode: */
    private PixelShader m_kPShaderDDR = null;
    /** PixelShader program and data for Composite mode: */
    private PixelShader m_kPShaderCMP = null;
    /** PixelShader program and data for Composite Surface and Surface modes: */
    private PixelShader m_kPShaderSUR = null;

    /** Indicates which shader to use (MIP, DDR, CMP, SUR, CMP_SUR): */
    private int m_iWhichShader = -1;
    /** stores the axis-aligned clip plane information: */
    private float[][] m_aafClipData = new float[6][4];
    /** stores the eye clip plane information: */
    private float[] m_afClipEyeData = null;
    /** stores the inverse-eye clip plane information: */
    private float[] m_afClipEyeInvData = null;
    /** stores the arbitrary clip plane information: */
    private float[] m_afClipArbData = null;
    /** Reference to the SceneImage texture: */
    private Texture m_kSceneTarget;
    /** stores the blend function */
    private float[] m_afBlend = new float[]{.5f,0,0,0};
    /** stores the steps size */
    private float[] stepsSize = new float[]{450f,0,0,0};
    /** stores the background color */
    private ColorRGBA m_kBackgroundColor = ColorRGBA.BLACK;
    /** stores the self-shadow paramter on/off value: */
    private float[] m_afSelfShadow = new float[]{0,0,0,0};
    /** stores the gradient magnitude filter on/off value: */
    private float[] m_afGradientMagnitude = new float[]{0,0,0,0};

}
