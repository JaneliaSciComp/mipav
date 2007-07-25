// Geometric Tools, Inc.
// http://www.geometrictools.com
// Copyright (c) 1998-2006.  All Rights Reserved
//
// The Wild Magic Version 4 Restricted Libraries source code is supplied
// under the terms of the license agreement
//     http://www.geometrictools.com/License/Wm4RestrictedLicense.pdf
// and may not be copied or disclosed except in accordance with the terms
// of that agreement.
//
// Version: 4.0.0 (2006/06/28)

package gov.nih.mipav.view.renderer.volumeview;

import java.io.IOException;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;


import gov.nih.mipav.model.structures.*;


/** The shader effect is a manager of the vertex and pixel shaders.  It
 * reimplements the user-relevant interfaces for the managed objects as a
 * convenience to avoid long expressions involving pointer dereferencing.
 */
public class VolumeShaderEffect extends ShaderEffect
{

    /** 
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
    
    public void finalize()
    {

        m_kVolumeA.finalize();
        m_kVolumeA = null;
        m_kVolumeTargetA.finalize();
        m_kVolumeTargetA = null;
        super.finalize();
    }

    private void CreateVolumeTexture ()
    {
        SetPassQuantity(1);

        m_kVolumeA = UpdateData(m_kImageA, null, m_kVolumeTargetA, new String("A") );
        
        VertexShader pkVShader = new VertexShader("VolumeShaderVertex");

        // setup mip shader effect:
        m_kPShaderMIP = new PixelShader("VolumeShaderMIP");
        m_kPShaderMIP.SetTextureQuantity(4);
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

        m_kPShaderDDR = new PixelShader("VolumeShaderDDR");
        m_kPShaderDDR.SetTextureQuantity(4);
        m_kPShaderDDR.SetImageName(0,"SceneImage");
        m_kPShaderDDR.SetTexture(0,m_kSceneTarget);
        m_kPShaderDDR.SetImageName(1,"VolumeImageA");
        m_kPShaderDDR.SetTexture(1,m_kVolumeTargetA);
        m_kPShaderDDR.SetImageName(2, "ColorMapA");
        m_kPShaderDDR.SetTexture(2,m_kColorMapTargetA);
        m_kPShaderDDR.SetImageName(3, "OpacityMapA");
        m_kPShaderDDR.SetTexture(3,m_kOpacityMapTargetA);

        m_kPShaderCMP = new PixelShader("VolumeShaderCMP");
        m_kPShaderCMP.SetTextureQuantity(4);
        m_kPShaderCMP.SetImageName(0,"SceneImage");
        m_kPShaderCMP.SetTexture(0,m_kSceneTarget);
        m_kPShaderCMP.SetImageName(1,"VolumeImageA");
        m_kPShaderCMP.SetTexture(1,m_kVolumeTargetA);
        m_kPShaderCMP.SetImageName(2, "ColorMapA");
        m_kPShaderCMP.SetTexture(2,m_kColorMapTargetA);
        m_kPShaderCMP.SetImageName(3, "OpacityMapA");
        m_kPShaderCMP.SetTexture(3,m_kOpacityMapTargetA);


        m_kPShaderSUR = new PixelShader("VolumeShaderSUR");
        m_kPShaderSUR.SetTextureQuantity(5);
        m_kPShaderSUR.SetImageName(0,"SceneImage");
        m_kPShaderSUR.SetTexture(0,m_kSceneTarget);
        m_kPShaderSUR.SetImageName(1,"VolumeImageA");
        m_kPShaderSUR.SetTexture(1,m_kVolumeTargetA);
        m_kPShaderSUR.SetImageName(2, "ColorMapA");
        m_kPShaderSUR.SetTexture(2,m_kColorMapTargetA);
        m_kPShaderSUR.SetImageName(3, "OpacityMapA");
        m_kPShaderSUR.SetTexture(3,m_kOpacityMapTargetA);

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

        SetVShader(0,pkVShader);
        SetPShader(0,m_kPShaderMIP);
    }

    private void CreateVolumeTextureAB ()
    {
        SetPassQuantity(1);

        m_kVolumeA = UpdateData(m_kImageA, m_kVolumeA, m_kVolumeTargetA, new String("A") );
        m_kVolumeB = UpdateData(m_kImageB, m_kVolumeB, m_kVolumeTargetB, new String("B") );
        
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
        m_kPShaderCMP.SetTextureQuantity(7);
        m_kPShaderCMP.SetImageName(0,"SceneImage");
        m_kPShaderCMP.SetTexture(0,m_kSceneTarget);
        m_kPShaderCMP.SetImageName(1,"VolumeImageA");
        m_kPShaderCMP.SetTexture(1, m_kVolumeTargetA);
        m_kPShaderCMP.SetImageName(2, "ColorMapA");
        m_kPShaderCMP.SetTexture(2, m_kColorMapTargetA);
        m_kPShaderCMP.SetImageName(3, "OpacityMapA");
        m_kPShaderCMP.SetTexture(3, m_kOpacityMapTargetA);
        m_kPShaderCMP.SetImageName(4,"VolumeImageB");
        m_kPShaderCMP.SetTexture(4, m_kVolumeTargetB);
        m_kPShaderCMP.SetImageName(5, "ColorMapB");
        m_kPShaderCMP.SetTexture(5,m_kColorMapTargetB);
        m_kPShaderCMP.SetImageName(6, "OpacityMapB");
        m_kPShaderCMP.SetTexture(6,m_kOpacityMapTargetB);

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


    public void MIPMode(int iImage, gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        if ( m_iWhichShader == MIP )
        {
            return;
        }
        m_iWhichShader = MIP;
        SetProgram( m_kPShaderMIP, kRenderer );
    }

    private void SetProgram(PixelShader kShader,
                            gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        PixelProgram kPProgram = GetPProgram(0);
        kPProgram.Release();
        PixelProgramCatalog.GetActive().Remove(kPProgram);
        PixelShader pkPShader = GetPShader(0);
        pkPShader.OnReleaseProgram();
        SetPShader(0,kShader);
        
        LoadPrograms(0,kRenderer.GetMaxColors(),
                     kRenderer.GetMaxTCoords(),
                     kRenderer.GetMaxVShaderImages(),
                     kRenderer.GetMaxPShaderImages());

        //ResetClip();
        SetBackgroundColor( m_kBackgroundColor );
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("blend") != null ) 
        {
            pkProgram.GetUC("blend").SetDataSource(m_afBlend);
        }
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

    public void DDRMode(int iImage, gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        if ( m_iWhichShader == DDR )
        {
            return;
        }
        m_iWhichShader = DDR;
        SetProgram( m_kPShaderDDR, kRenderer );
    }

    public void CMPMode(int iImage, gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        if ( m_iWhichShader == CMP )
        {
            return;
        }
        m_iWhichShader = CMP;
        SetProgram(m_kPShaderCMP, kRenderer);
    }

    public void SURMode(int iImage, gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        if ( m_iWhichShader == SUR )
        {
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
            return;
        }

        m_iWhichShader = SUR;
        SetProgram(m_kPShaderSUR, kRenderer);
    }

    public void SURFASTMode(int iImage, gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        if ( m_iWhichShader == SUR )
        {
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
            return;
        }

        m_iWhichShader = SUR;
        SetProgram(m_kPShaderSUR, kRenderer);
    }

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

        for (iZ = 1; iZ < (iZBound - 1); iZ++) {
            boolean bMinZ = 0 == iZ;
            boolean bMaxZ = (iZBound - 1) == iZ;

            for (iY = 1; iY < (iYBound - 1); iY++) {
                boolean bMinY = 0 == iY;
                boolean bMaxY = (iYBound - 1) == iY;
                int offset = iXBound * (iY + (iYBound * iZ));

                for (iX = 0; iX < iXBound; iX++) {
                    boolean bMinX = 0 == iX;
                    boolean bMaxX = (iXBound - 1) == iX;

                    int i = iX + offset;

                    fDX = (((bMinX ? afData[i] : afData[i - iOffX - iXBound]) -
                            (bMaxX ? afData[i] : afData[i + iOffX - iXBound])) * 0.71f) +
                            
                        (bMinX ? afData[i] : afData[i - iOffX]) - (bMaxX ? afData[i] : afData[i + iOffX]) +
                        (
                             
                         ((bMinX ? afData[i] : afData[i - iOffX + iXBound]) -
                          (bMaxX ? afData[i] : afData[i + iOffX + iXBound])) * 0.71f);
                        
                    fDY = (((bMinY ? afData[i] : afData[i - iOffY - 1]) - (bMaxY ? afData[i] : afData[i + iOffY - 1])) *
                           0.71f) +
                            
                        (bMinY ? afData[i] : afData[i - iOffY]) - (bMaxY ? afData[i] : afData[i + iOffY]) +
                        (
                             
                         ((bMinY ? afData[i] : afData[i - iOffY + 1]) - (bMaxY ? afData[i] : afData[i + iOffY + 1])) * 0.71f);
                        
                    fDZ = (((bMinZ ? afData[i] : afData[i - iOffZ - 1]) - (bMaxZ ? afData[i] : afData[i + iOffZ - 1])) *
                           0.71f) +
                            
                        (bMinZ ? afData[i] : afData[i - iOffZ]) - (bMaxZ ? afData[i] : afData[i + iOffZ]) +
                        (
                             
                         ((bMinZ ? afData[i] : afData[i - iOffZ + 1]) - (bMaxZ ? afData[i] : afData[i + iOffZ + 1])) * 0.71f);
                        
                        
                    if ((fDX != 0.0f) || (fDY != 0.0f) || (fDZ != 0.0f)) {
                        afDataN[i*3+0] = fDX;
                        afDataN[i*3+1] = fDY;
                        afDataN[i*3+2] = fDZ;
                    }
                }
            }
        }

        int[] aiNormalAverageIndex = new int[]{ 0, -1, +1, -iXBound, +iXBound, -iXYBound, +iXYBound };
        Vector3f kNormal = new Vector3f();
        Vector3f kNormalTmp = new Vector3f();
        byte[] acData = new byte[afData.length*3];
                    
        // Catch any zero-vector normals and replace them by an average of
        // neighboring normals.
        for (iZ = 1; iZ < (iZBound - 1); iZ++) {

            for (iY = 1; iY < (iYBound - 1); iY++) {
                int offset = iXBound * (iY + (iYBound * iZ));

                for (iX = 1; iX < (iXBound - 1); iX++) {
                    int i = iX + offset;

                    kNormal.copy(Vector3f.ZERO);
                    for ( int iN = 0; iN < aiNormalAverageIndex.length; iN++ )
                    {
                        int index = i + aiNormalAverageIndex[iN];
                        index *= 3;
                        kNormalTmp.X(afDataN[index + 0]);
                        kNormalTmp.Y(afDataN[index + 1]);
                        kNormalTmp.Z(afDataN[index + 2]);

                        kNormal.addEquals( kNormalTmp );
                    }
                    kNormal.Normalize();
                    acData[i*3+0] = (byte)(kNormal.X()*127 + 127);
                    acData[i*3+1] = (byte)(kNormal.Y()*127 + 127);
                    acData[i*3+2] = (byte)(kNormal.Z()*127 + 127);
                }
            }
        }
        aiNormalAverageIndex = null;
        kNormal = null;
        kNormalTmp = null;
        afDataN = null;

        return acData;
    }

    public void UpdateData( ModelImage kImage, int iImage )
    {
        if ( iImage == 0 )
        {
            m_kImageA = kImage;
            this.UpdateData( m_kImageA, m_kVolumeA, m_kVolumeTargetA, new String( "A" ) );
        }
    }

    private GraphicsImage UpdateData( ModelImage kImage, GraphicsImage kVolumeImage, Texture kVolumeTexture, String kPostFix )
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
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            if ( kVolumeImage == null )
            {
                kVolumeImage = new GraphicsImage(
                                                 GraphicsImage.FormatMode.IT_RGBA8888,iXBound,iYBound,iZBound,aucData,
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
                kVolumeImage = new GraphicsImage(
                                                 GraphicsImage.FormatMode.IT_L8,iXBound,iYBound,iZBound,afData,
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

    public boolean UpdateImages(TransferFunction kTransfer, int iImage)
    {
        if ( iImage == 0 )
        {
            return UpdateImages( m_kImageA, m_kOpacityMapTargetA, m_kOpacityMapA, kTransfer );
        }
        else if ( m_kImageB != null )
        {
            return UpdateImages( m_kImageB, m_kOpacityMapTargetB, m_kOpacityMapB, kTransfer );
        }
        return false;
    }

    private boolean UpdateImages(ModelImage kImage, Texture kOpacityTexture, GraphicsImage kOpacityMap, TransferFunction kTransfer)
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


    public boolean UpdateImages(ModelLUT kLUTa, ModelLUT kLUTb)
    {
        this.UpdateImages( m_kColorMapTargetA, m_kColorMapA, kLUTa );
        if ( m_kImageB != null )
        {
            this.UpdateImages( m_kColorMapTargetB, m_kColorMapB, kLUTb );
        }
        return true;
    }

    /**
     * This methods calls corresponding render to update images with LUT changes.
     *
     * @param   LUTa        LUT used to update imageA
     * @param   LUTb        LUT used to update imageB
     * @param   forceShow   forces show to reimport image and calc. java image
     * @param   interpMode  image interpolation method (Nearest or Smooth)
     *
     * @return  boolean confirming successful update
     */
    private boolean UpdateImages(Texture kColorTexture, GraphicsImage kColorMap, ModelLUT kLUT)
    {
        if ( kLUT == null )
        {
            return false;
        }
        byte[] oldData = kColorMap.GetData();
        byte[] aucData = ModelLUT.exportIndexedLUTMin( kLUT.getTransferFunction(), kLUT.getExtents()[1],
                kLUT.getIndexedLUT());

        kColorMap.SetData(aucData, aucData.length/4);
        if ( oldData.length != aucData.length )
        {
            kColorTexture.Release();
        }
        else
        {
            kColorTexture.Reload(true);
        }
        return true;
    }

    public GraphicsImage InitColorMap ( ModelLUT kLUTa, ModelRGB kRGBT, String kPostFix )
    {
        byte[] aucData;
        if ( kLUTa == null )
        {
            aucData = ModelLUT.exportIndexedLUTMin( kRGBT.getRedFunction(), kRGBT.getGreenFunction(), kRGBT.getBlueFunction(),
                                                    kRGBT.getExtents()[1], kRGBT.exportIndexedRGB());
            
        }
        else
        {
            aucData = ModelLUT.exportIndexedLUTMin( kLUTa.getTransferFunction(), kLUTa.getExtents()[1],
                                                    kLUTa.getIndexedLUT());
        }
        return new GraphicsImage(
                                 GraphicsImage.FormatMode.IT_RGBA8888,aucData.length/4,aucData,
                                 new String( "ColorMap" + kPostFix ) );
    }

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

    public Texture GetVolumeTargetA()
    {
        return m_kVolumeTargetA;
    }

    public Texture GetVolumeTargetB()
    {
        return m_kVolumeTargetB;
    }

    public void ReleaseVolume()
    {
        m_kVolumeTargetA.Release();
        if ( m_kImageB != null )
        {
            m_kVolumeTargetB.Release();
        }
    }

    public void Reload( gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        VertexProgram kVProgram = GetVProgram(0);
        kVProgram.Release();
        VertexProgramCatalog.GetActive().Remove(kVProgram);

        PixelProgram kPProgram = GetPProgram(0);
        kPProgram.Release();
        PixelProgramCatalog.GetActive().Remove(kPProgram);

        VertexShader pkVShader = GetVShader(0);
        pkVShader.OnReleaseProgram();
        PixelShader pkPShader = GetPShader(0);
        pkPShader.OnReleaseProgram();

        LoadPrograms(0,kRenderer.GetMaxColors(),
                     kRenderer.GetMaxTCoords(),
                     kRenderer.GetMaxVShaderImages(),
                     kRenderer.GetMaxPShaderImages());
        //ResetClip();
    }

    public void ResetClip( float[] afData )
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
            }       
        }
    }

    public void SetClip(int iWhich, float[] data)
    {
        EnableClip();
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC(m_akClip[iWhich]) != null ) 
        {
            pkProgram.GetUC(m_akClip[iWhich]).SetDataSource(data);
        }       
    }
    private void EnableClip()
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("DoClip") != null ) 
        {
            pkProgram.GetUC("DoClip").SetDataSource(new float[]{1,0,0,0});
        }       
    }

    public void SetClipEye(float[] afEquation)
    {
        EnableClip();
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("clipEye") != null ) 
        {
            pkProgram.GetUC("clipEye").SetDataSource(afEquation);
        }
    }

    public void SetClipEyeInv(float[] afEquation)
    {
        EnableClip();
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("clipEyeInv") != null ) 
        {
            pkProgram.GetUC("clipEyeInv").SetDataSource(afEquation);
        }
    }

    public void SetClipArb(float[] afEquation)
    {
        EnableClip();
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("clipArb") != null ) 
        {
            pkProgram.GetUC("clipArb").SetDataSource(afEquation);
        }
    }

    public void SetLight( String kLightType, float[] afType )
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC(kLightType) != null)
        {
            pkProgram.GetUC(kLightType).SetDataSource(afType);
        }
    }

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

    private void SetRGBT( Texture kTexture, GraphicsImage kImage, ModelRGB kRGBT )
    {
        if ( kRGBT == null )
        {
            return;
        }
        byte[] oldData = kImage.GetData();
        byte[] aucData = ModelLUT.exportIndexedLUTMin( kRGBT.getRedFunction(),
                                                       kRGBT.getGreenFunction(),
                                                       kRGBT.getBlueFunction(),
                                                       kRGBT.getExtents()[1],
                                                       kRGBT.exportIndexedRGB());
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

    public Program GetPProgram()
    {
        return GetPProgram(0);
    }

    public void Blend(float fBlend)
    {
        m_afBlend[0] = fBlend;
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("blend") != null ) 
        {
            pkProgram.GetUC("blend").SetDataSource(m_afBlend);
        }
    }

    public void SetBackgroundColor( ColorRGBA kColor )
    {
        m_kBackgroundColor = kColor;
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("BackgroundColor") != null ) 
        {
            float[] afColor = new float[4];
            afColor[0] = kColor.R();
            afColor[1] = kColor.G();
            afColor[2] = kColor.B();
            afColor[3] = kColor.A();
            pkProgram.GetUC("BackgroundColor").SetDataSource(afColor);
        }
        
    }

    private ModelImage m_kImageA;
    private ModelLUT m_kLUTA;
    private ModelRGB m_kRGBA;
    private TransferFunction m_kTransferA;

    private ModelImage m_kImageB;
    private ModelLUT m_kLUTB;
    private ModelRGB m_kRGBB;
    private TransferFunction m_kTransferB;

    private GraphicsImage m_kVolumeA;
    private GraphicsImage m_kNormalA;
    private GraphicsImage m_kColorMapA;
    private GraphicsImage m_kOpacityMapA = null;
    private Texture m_kVolumeTargetA;
    private Texture m_kColorMapTargetA;
    private Texture m_kOpacityMapTargetA;
    private Texture m_kNormalMapTargetA;

    private GraphicsImage m_kVolumeB;
    private GraphicsImage m_kNormalB;
    private GraphicsImage m_kColorMapB;
    private GraphicsImage m_kOpacityMapB = null;
    private Texture m_kVolumeTargetB;
    private Texture m_kColorMapTargetB;
    private Texture m_kOpacityMapTargetB;
    private Texture m_kNormalMapTargetB;

    private PixelShader m_kPShaderMIP = null;
    private PixelShader m_kPShaderDDR = null;
    private PixelShader m_kPShaderCMP = null;
    private PixelShader m_kPShaderSUR = null;

    private static int MIP = 0;
    private static int DDR = 1;
    private static int CMP = 2;
    private static int SUR = 3;
    private int m_iWhichShader = -1;
    private String[] m_akClip = new String[]{ "clipXInv", "clipX", "clipYInv", "clipY", "clipZInv", "clipZ" };

    private Texture m_kSceneTarget;
    private float[] m_afBlend = new float[]{.5f,0,0,0};
    private ColorRGBA m_kBackgroundColor = ColorRGBA.BLACK;
}
