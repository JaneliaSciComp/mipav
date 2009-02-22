package gov.nih.mipav.view.renderer.WildMagic.Render;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransferFunction;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogGradientMagnitude;
import gov.nih.mipav.view.dialogs.JDialogLaplacian;

import java.awt.event.ActionEvent;
import java.io.IOException;
import java.nio.Buffer;

import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibGraphics.Rendering.GraphicsImage;
import WildMagic.LibGraphics.Rendering.Texture;


public class VolumeImage
{
    /** Reference to ModelImage image */
    private ModelImage m_kImage;

    /** GraphicsImage contains GM opacity transfer function data: */
    private GraphicsImage m_kOpacityMap_GM = null;
    /** Texture contains texture filter modes and GraphicsImage for opacity
     * transfer function: */
    private Texture m_kOpacityMapTarget_GM = null;

    /** Data storage for volume: */
    private GraphicsImage m_kVolume;
    /** Texture object for data: */
    private Texture m_kVolumeTarget;
    
    /** Data storage for normals: */
    private GraphicsImage m_kNormal;
    /** Texture object for normal map: */
    private Texture m_kNormalMapTarget;
    
    /** Data storage for color map: */
    private GraphicsImage m_kColorMap;
    /** Texture object for color map: */
    private Texture m_kColorMapTarget;
    
    /** Data storage for opacity map: */
    private GraphicsImage m_kOpacityMap = null;
    /** Texture object for opacity map: */
    private Texture m_kOpacityMapTarget;
    
    /** Data storage for volume gradient magnitude: */
    private GraphicsImage m_kVolumeGM;
    /** Texture object for data: */
    private Texture m_kVolumeGMTarget;
    
    /** Data storage for volume  second derivative: */
    private GraphicsImage m_kVolumeGMGM;
    /** Texture object for data: */
    private Texture m_kVolumeGMGMTarget;

    /** Data storage for surfaces: */
    private GraphicsImage m_kSurfaceImage;
    /** Texture object for surfaces: */
    private Texture m_kSurfaceTarget;

    private boolean m_bByte = true;
    private ModelLUT m_kLUT = null;

    private float m_fX = 1, m_fY = 1, m_fZ = 1;
    private String m_kPostfix = null;
    
    private GraphicsImage m_kHisto = null;
    private Vector2f[] m_akHistoTCoord = new Vector2f[4];
    private float m_fDRRNormalize = 255.0f;
    
    /**
     * Constructor. Stores the ModelImage Volume data.
     * Creates Textures for the Volume, Normals, Gradient Magnitude, Volume opacity transfer function.
     * Volume color map, GM opacity transfer function, and GM colormap.
     * @param kImage ModelImage volume data.
     * @param kLUT LUT.
     * @param kRGBT color LUT.
     * @param kPostfix String postfix for the Texture names.
     */
    public VolumeImage( ModelImage kImage, ModelLUT kLUT, ModelRGB kRGBT, String kPostfix )
    {
        m_kImage = kImage;
        m_fDRRNormalize = computeIntegralNormalizationFactor();
        m_kLUT = kLUT;
        m_kColorMap = InitColorMap(kLUT, kRGBT, kPostfix);
        m_kOpacityMap = InitOpacityMap(m_kImage, kPostfix);
        m_kOpacityMap_GM = InitOpacityMap(m_kImage, new String(kPostfix + "_GM"));

        /* Map the ModelImage volume data to a texture image, including for
         * the ModelImage gradient magnitude data: */
        m_kVolume = UpdateData(m_kImage, null, m_kVolumeTarget, kPostfix );
        //new ViewJFrameImage(CreateImageFromTexture(m_kVolume), null, new java.awt.Dimension(610, 200), false);
        GradientMagnitudeImage(m_kImage, kPostfix);
        GenerateHistogram( m_kVolume, m_kVolumeGM, kPostfix );
        
        m_kVolumeTarget = new Texture();
        m_kVolumeTarget.SetImage(m_kVolume);
        m_kVolumeTarget.SetShared(true);
        m_kVolumeTarget.SetFilterType(Texture.FilterType.LINEAR);
        m_kVolumeTarget.SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kVolumeTarget.SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kVolumeTarget.SetWrapType(2,Texture.WrapType.CLAMP_BORDER);
        
        m_kColorMapTarget = new Texture();
        m_kColorMapTarget.SetImage(m_kColorMap);
        m_kColorMapTarget.SetShared(true);

        m_kOpacityMapTarget = new Texture();
        m_kOpacityMapTarget.SetImage(m_kOpacityMap);
        m_kOpacityMapTarget.SetShared(true);

        int iXBound = m_kImage.getExtents()[0];
        int iYBound = m_kImage.getExtents()[1];
        int iZBound = m_kImage.getExtents()[2];
        m_kNormal = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA8888,
                                       iXBound,iYBound,iZBound,(byte[])null,
                                       new String("NormalMap"+kPostfix));
        m_kNormalMapTarget = new Texture();
        m_kNormalMapTarget.SetImage(m_kNormal);
        m_kNormalMapTarget.SetShared(true);
        m_kNormalMapTarget.SetFilterType(Texture.FilterType.LINEAR);
        m_kNormalMapTarget.SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kNormalMapTarget.SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kNormalMapTarget.SetWrapType(2,Texture.WrapType.CLAMP_BORDER);
        
        m_kOpacityMapTarget_GM = new Texture();
        m_kOpacityMapTarget_GM.SetImage(m_kOpacityMap_GM);
        m_kOpacityMapTarget_GM.SetShared(true);
        

        m_kSurfaceImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA8888,
                                       iXBound,iYBound,iZBound,(byte[])null,
                                       "SurfaceImage");
        m_kSurfaceTarget = new Texture();
        m_kSurfaceTarget.SetImage(m_kSurfaceImage);
        m_kSurfaceTarget.SetShared(true);
        m_kSurfaceTarget.SetFilterType(Texture.FilterType.LINEAR);
        m_kSurfaceTarget.SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kSurfaceTarget.SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kSurfaceTarget.SetWrapType(2,Texture.WrapType.CLAMP_BORDER);
        
        InitScale();
        
        m_kPostfix = new String(kPostfix);
        /*
        float[] afGaussX = new float[3*3*3];
        int[] aiExtents = new int[]{3,3,3};
        float[] afSigmas = new float[]{.5f, .5f, .5f};
        int[] aiOrder = new int[]{1,0,0};
        GenerateGaussian Gx = new GenerateGaussian(afGaussX, aiExtents, afSigmas, aiOrder);
        Gx.calc(false);

        float[] afGaussY = new float[3*3*3];
        aiOrder[0] = 0;
        aiOrder[1] = 1;
        GenerateGaussian Gy = new GenerateGaussian(afGaussY, aiExtents, afSigmas, aiOrder);
        Gy.calc(true);       

        float[] afGaussZ = new float[3*3*3];
        aiOrder[0] = 0;
        aiOrder[1] = 0;
        aiOrder[2] = 1;
        GenerateGaussian Gz = new GenerateGaussian(afGaussZ, aiExtents, afSigmas, aiOrder);
        Gz.calc(true);

        for ( int i = 0; i < aiExtents[0]; i++ )
        {
            for ( int j = 0; j < aiExtents[1]; j++ )
            {
                for ( int k = 0; k < aiExtents[2]; k++ )
                {
                    System.err.println( "gaussian[" + i + "][" + j + "][" + k + "].rgb = (" +
                            afGaussX[i*aiExtents[1]*aiExtents[2] + j*aiExtents[2] + k] + ", " + 
                            afGaussY[i*aiExtents[1]*aiExtents[2] + j*aiExtents[2] + k] + ", " + 
                            afGaussZ[i*aiExtents[1]*aiExtents[2] + j*aiExtents[2] + k] + ");" ); 
                    		
                   
                }
                System.err.println( );
            }
            System.err.println( );
            System.err.println( );
        }
        */
    }

    /**
     * Initialize the textures for the color lookup table.
     * @param kLUT the new LUT.
     * @param kRGBT the new RGB table.
     * @param kPostfix the string postfix to concatenate to the "ColorMap" image name.
     * @return GraphicsImage, the new GraphicsImage storing the colormap lookup table.
     */
    public static GraphicsImage InitColorMap ( ModelLUT kLUT, ModelRGB kRGBT, String kPostFix )
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
     * Sets the Texture object containing the color lookup table based on the ModelRGB.
     * @param kTexture the Texture object containing the colormap GraphicsImage.
     * @param kImage the GraphicsImage containing the colormap data.
     * @param kRGBT the new ModelRGB.
     */
    public static void SetRGBT( Texture kTexture, GraphicsImage kImage, ModelRGB kRGBT )
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
     * Update the image volume data on the GPU.
     * @param kImage the new ModelImage
     * @param kVolumeImage the volume data image.
     * @param kVolumeTexture the volume data texture.
     * @param kPostFix the postfix string for the image name.
     */
    public static GraphicsImage UpdateData( ModelImage kImage, GraphicsImage kVolumeImage,
            Texture kVolumeTexture, String kPostFix )
    {
        int iXBound = kImage.getExtents()[0];
        int iYBound = kImage.getExtents()[1];
        int iZBound = kImage.getExtents()[2];

        byte[] aucData = null;
        if ( kImage.isColorImage() )
        {
            aucData = new byte[4*iXBound*iYBound*iZBound];
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
        }
        else
        {
            aucData = new byte[iXBound*iYBound*iZBound];
            try {
                kImage.exportData( 0, kImage.getSize(), aucData );
                kVolumeImage =
                    new GraphicsImage( GraphicsImage.FormatMode.IT_L8, 
                            iXBound,iYBound,iZBound, aucData,
                            new String( "VolumeImage" + kPostFix));
            } catch (IOException e) {
                e.printStackTrace();
            }

        }

        if ( kVolumeTexture != null )
        {
            kVolumeTexture.GetImage().SetData( aucData, iXBound, iYBound, iZBound );
            kVolumeTexture.Release();
        }
        return kVolumeImage;
    }

    /**
     * Update the LUT texture sent to the GPU.
     * @param kColorTexture the color-map Texture object.
     * @param kColorMap the color-map GraphicsImage object (stores data).
     * @param kLUT the new LUT.
     */
    public static void UpdateImages(Texture kColorTexture, GraphicsImage kColorMap, ModelLUT kLUT)
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
     * Read the current Volume Texture from the GPU and return a new ModelImage of that data.
     * @return new ModelImage from Volume Texture on GPU.
     */
    public ModelImage CreateBinaryImageFromTexture( GraphicsImage kImage )
    {
        int iXBound = kImage.GetBound(0);
        int iYBound = kImage.GetBound(1);
        int iZBound = kImage.GetBound(2);
        int[] extents = new int[]{iXBound, iYBound, iZBound};
        
        ModelImage kResult = new ModelImage( ModelStorageBase.BOOLEAN, extents, JDialogBase.makeImageName(m_kImage.getImageName(), "_temp") );
        int i = 0;
        for (int iZ = 0; iZ < iZBound; iZ++)
        {
            for (int iY = 0; iY < iYBound; iY++)
            {
                for (int iX = 0; iX < iXBound; iX++)
                {
                    if ( kImage.GetData()[i++] > 0 )
                    {
                        kResult.set( iX, iY, iZ, true);
                    }
                }
            }
        }
        JDialogBase.updateFileInfo(m_kImage, kResult);
        return kResult;
    }

    /**
     * Read the current Volume Texture from the GPU and return a new ModelImage of that data.
     * @return new ModelImage from Volume Texture on GPU.
     */
    public ModelImage CreateImageFromTexture( GraphicsImage kImage )
    {
        int iXBound = kImage.GetBound(0);
        int iYBound = kImage.GetBound(1);
        int iZBound = kImage.GetBound(2);
        int[] extents = new int[]{iXBound, iYBound, iZBound};
        
        ModelImage kResult = null;
        if ( m_kImage.isColorImage() )
        {
            byte[] aucData = new byte[4*iXBound*iYBound*iZBound];
            for ( int i = 0; i < m_kImage.getSize(); i += 4)
            {
                aucData[i] = kImage.GetData()[i+3];
                aucData[i+1] = kImage.GetData()[i+1];
                aucData[i+2] = kImage.GetData()[i+2];
                aucData[i+3] = kImage.GetData()[i];
            }
            try {
                kResult = new ModelImage( m_kImage.getType(), extents, JDialogBase.makeImageName(m_kImage.getImageName(), "_Crop") );
                kResult.importData( 0, aucData, true );
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        else
        {
            byte[] aiImageData = kImage.GetData();
            try {
                kResult = new ModelImage( ModelStorageBase.UBYTE, extents, JDialogBase.makeImageName(m_kImage.getImageName(), "_Crop") );
                kResult.importData( 0, aiImageData, true );
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        kResult.copyFileTypeInfo(m_kImage);
        kResult.calcMinMax();
        return kResult;
    }

    /**
     * Memory cleanup.
     */
    public void dispose()
    {
        m_kImage = null;
        if ( m_kOpacityMap_GM != null )
        {
            m_kOpacityMap_GM.dispose();
            m_kOpacityMap_GM = null;
        }
        if ( m_kOpacityMapTarget_GM != null )
        {
            m_kOpacityMapTarget_GM.dispose();
            m_kOpacityMapTarget_GM = null;
        }
        if ( m_kVolume != null )
        {
            m_kVolume.dispose();
            m_kVolume = null;
        }
        if ( m_kNormal != null )
        {
            m_kNormal.dispose();
            m_kNormal = null;
        }
        if ( m_kColorMap != null )
        {
            m_kColorMap.dispose();
            m_kColorMap = null;
        }
        if ( m_kOpacityMap != null )
        {
            m_kOpacityMap.dispose();
            m_kOpacityMap = null;
        }
        if ( m_kVolumeTarget != null )
        {
            m_kVolumeTarget.dispose();
            m_kVolumeTarget = null;
        }
        if ( m_kColorMapTarget != null )
        {
            m_kColorMapTarget.dispose();
            m_kColorMapTarget = null;
        }
        if ( m_kOpacityMapTarget != null )
        {
            m_kOpacityMapTarget.dispose();
            m_kOpacityMapTarget = null;
        }
        if ( m_kNormalMapTarget != null )
        {
            m_kNormalMapTarget.dispose();
            m_kNormalMapTarget = null;
        }
    }

    public GraphicsImage GenerateGMImages( ModelImage kImageGM, String kPostFix )
    {
        int iXBound = kImageGM.getExtents()[0];
        int iYBound = kImageGM.getExtents()[1];
        int iZBound = kImageGM.getExtents()[2];
        kImageGM.calcMinMax();
        float fImageMaxGM = (float)kImageGM.getMax();
        float fImageMinGM = (float)kImageGM.getMin();

        byte[] abData = null;
        abData = new byte[iXBound*iYBound*iZBound];
        int i = 0;
        for (int iZ = 0; iZ < iZBound; iZ++)
        {
            for (int iY = 0; iY < iYBound; iY++)
            {
                for (int iX = 0; iX < iXBound; iX++)
                {
                    float fValue = kImageGM.getFloat(iX,iY,iZ);
                    abData[i++] = (byte)(255 * (fValue - fImageMinGM)/(fImageMaxGM - fImageMinGM));
                }
            }
        }
        return
            new GraphicsImage( GraphicsImage.FormatMode.IT_L8, 
                               iXBound,iYBound,iZBound, abData,
                               new String( "VolumeImage" + kPostFix));
    }

    public GraphicsImage GenerateImagesColor( ModelImage kImage, String kPostFix )
    {
        int iXBound = kImage.getExtents()[0];
        int iYBound = kImage.getExtents()[1];
        int iZBound = kImage.getExtents()[2];


        byte[] aucData = new byte[4*iXBound*iYBound*iZBound];
        byte[] aucGMData = new byte[4*iXBound*iYBound*iZBound];
        float temp;
        int iGM = 0;
        try {
            kImage.exportData( 0, kImage.getSize(), aucData );
            for ( int i = 0; i < kImage.getSize(); i += 4)
            {
                temp = (aucData[i+1] + aucData[i+2] + aucData[i+3])/3.0f;
                aucGMData[iGM++] = Float.valueOf(temp).byteValue();
                aucGMData[iGM++] = Float.valueOf(temp).byteValue();
                aucGMData[iGM++] = Float.valueOf(temp).byteValue();
                aucGMData[iGM++] = Float.valueOf(temp).byteValue();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return new GraphicsImage( GraphicsImage.FormatMode.IT_RGBA8888,
                iXBound,iYBound,iZBound,aucGMData,
                new String( "VolumeImage" + kPostFix));

    }
    

    /**
     * Return the Volume color map Texture.
     * @return Volume color map Texture.
     */
    public Texture GetColorMapTarget()
    {
        return m_kColorMapTarget;
    }
    
    /**
     * Return the Gradient Magnitude Texture.
     * @return Gradient Magnitude Texture.
     */
    public Texture GetGradientMapTarget()
    {
        return m_kVolumeGMTarget;
    }
    
    
    public GraphicsImage GetHisto()
    {
        return m_kHisto;
    }

    public Vector2f[] GetHistoTCoords()
    {
        return m_akHistoTCoord;
    }
    

    /**
     * Return the ModelImage volume data.
     * @return ModelImage volume data.
     */
    public ModelImage GetImage()
    {
        return m_kImage;
    }    

    /**
     * Return the Volume LUT.
     * @return Volume LUT.
     */
    public ModelLUT GetLUT()
    {
        return m_kLUT;
    }

    /**
     * Return the Volume normal Texture.
     * @return Volume normal Texture.
     */
    public Texture GetNormalMapTarget()
    {
        return m_kNormalMapTarget;
    }

    /**
     * Return the gradient magnitude opacity transfer function Texture.
     * @return gradient magnitude opacity transfer function Texture.
     */
    public Texture GetOpacityMapGMTarget()
    {
        return m_kOpacityMapTarget_GM;
    }

    /**
     * Return the Volume opacity transfer function Texture.
     * @return Volume opacity transfer function Texture.
     */
    public Texture GetOpacityMapTarget()
    {
        return m_kOpacityMapTarget;
    }

    /**
     * Return the postfix for this VolumeImage.
     * @return postfix for this VolumeImage.
     */
    public String GetPostfix()
    {
        return m_kPostfix;
    }

    /**
     * The ModelImage Volume x-scale factor.
     * @return Volume x-scale factor.
     */
    public float GetScaleX()
    {
        return m_fX;
    }
    
    /**
     * The ModelImage Volume y-scale factor.
     * @return Volume y-scale factor.
     */
    public float GetScaleY()
    {
        return m_fY;
    }


    /**
     * The ModelImage Volume z-scale factor.
     * @return Volume z-scale factor.
     */
    public float GetScaleZ()
    {
        return m_fZ;
    }


    /**
     * Return the 2nd derivative texture.
     * @return 2nd derivative texture.
     */
    public Texture GetSecondDerivativeMapTarget()
    {
        return m_kVolumeGMGMTarget;
    }

    /**
     * Return the surface mask Texture.
     * @return surface mask Texture.
     */
    public Texture GetSurfaceTarget()
    {
        return m_kSurfaceTarget;
    }
    
    /**
     * Return the Texture containing the volume data.
     * @return Texture containing the volume data.
     */
    public Texture GetVolumeTarget()
    {
        return m_kVolumeTarget;
    }
    
    /**
     * Return the Texture containing the volume data.
     * @return Texture containing the volume data.
     */
    public Buffer GetVolumeTargetBuffer()
    {
        return m_kVolumeTarget.GetImage().GetDataBuffer();
    }

    /**
     * Initialize the textures for the opacity lookup table.
     * @param kImage the ModelImage the opacity transfer function applies to.
     * @param kPostfix the string postfix to concatenate to the "OpacityMap" image name.
     * @return GraphicsImage, the new GraphicsImage storing the colormap lookup table.
     */
    public GraphicsImage InitOpacityMap (ModelImage kImage, String kPostFix )
    {
        int iLutHeight = 256;
        float[] afData = new float[iLutHeight];
        float fRange = (float)(kImage.getMax() - kImage.getMin());
        float fStep = fRange / iLutHeight;
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
     * Return true if the Volume image is a color image.
     * @return true if the Volume image is a color image.
     */
    public boolean IsColorImage()
    {
        return m_kImage.isColorImage();
    }

    /**
     * Release the Textures containing the volume data. Once
     * Textures are released, they will be re-loaded onto the GPU during the
     * next frame.
     */
    public void ReleaseVolume()
    {
        m_kVolumeTarget.Release();
    }
    
    /**
     * Sets the ModelRGB for the iImage.
     * @param kRGBT new ModelRGB
     */
    public void SetRGBT(ModelRGB kRGBT)
    {
        SetRGBT( m_kColorMapTarget, m_kColorMap, kRGBT );
    }   
    /**
     * Update the image data.
     * @param kImage the modified ModelImage
     * @param kPostfix which image (imageA, imageB)
     */
    public void UpdateData( ModelImage kImage, String kPostfix )
    {
        m_kImage = kImage;
        VolumeImage.UpdateData( m_kImage, m_kVolume, m_kVolumeTarget, kPostfix );
    }    
    /**
     * Update the LUT for the ModelImage.
     * @param kLUT new LUT for ModelImage.
     */
    public void UpdateImages(ModelLUT kLUT)
    {
        if ( kLUT != null )
        {
            this.UpdateImages( m_kColorMapTarget, m_kColorMap, kLUT );
        }
        m_kLUT = kLUT;
    }
    

    /**
     * Update the transfer function for the image iImage.
     * @param kTransfer the new opacity transfer function
     * @param iImage the image to modify (0 = volume image, 2 = gradient mag)
     * @return boolean true when updated, false otherwise.
     */
    public boolean UpdateImages(TransferFunction kTransfer, int iImage, ModelImage kImage)
    {
        if ( iImage == 0 )
        {
            return UpdateImages( m_kImage, m_kOpacityMapTarget, m_kOpacityMap, kTransfer );
        }
        else if ( (iImage == 2) &&
                   (kImage != null) &&
                   (m_kOpacityMapTarget_GM != null) &&
                   (m_kOpacityMap_GM != null)  )
         {
             return UpdateImages( kImage, m_kOpacityMapTarget_GM, m_kOpacityMap_GM, kTransfer );
         }
        return false;
    }


    private void GenerateHistogram( GraphicsImage kImage, GraphicsImage kImageGM, String kPostFix )
    {
        float[] afCount = new float[256*256];
        for(int i=0; i<256*256; ++i) {
            afCount[i] = 0;
        }

        short a1, a2;
        byte[] abHistoData = kImageGM.GetData();
        byte[] abData = kImage.GetData();
        /*
        if ( m_kImage.isColorImage() )
        {
            int iHisto = 0;
            for ( int i = 0; i < abData.length; i +=4 )
            {
                int iR = (abData[i]);
                int iG = (abData[i+1]);
                int iB = (abData[i+2]);
                a1 = (iR + iG + iB)/3;
                a1 = (short)(a1 & 0x00ff);
                a2 = (abHistoData[iHisto * 3 + 0]);
                a2 = (short)(a2 & 0x00ff);
                afCount[a1 +  a2 * 256] += 1;
                iHisto++;
            }
        }
        else
        */
        {
            int iHisto = 0;
            for ( int i = 0; i < abData.length; i++)
            {
                a1 = (abData[i]);
                a1 = (short)(a1 & 0x00ff);
                a2 = (abHistoData[iHisto]);
                a2 = (short)(a2 & 0x00ff);
                afCount[a1 +  a2 * 256] += 1;
                iHisto++;
            }
        }
        float max = 0;
        for(int i = 0; i< 256*256; ++i)
        {
            afCount[i] = (float)Math.log(afCount[i]);
            max = Math.max(afCount[i], max);
        }           

        byte[] abHisto = new byte[256*256];
        for(int i = 0; i< 256*256; ++i)
        {
            abHisto[i] = new Float(afCount[i]/max*255f).byteValue();
        }

        int iMinX = 255, iMaxX = 0;
        int iIndex = 0;
        for( int i=0; i< 256; i++ )
        {
            for( int j = 0; j < 256; j++ )
            {    
                iIndex = i*256 + j;
                if ( abHisto[iIndex] != 0 )
                {
                    if ( iMinX > j )
                    {
                        iMinX = j;
                    }
                    if ( j > iMaxX )
                    {
                        iMaxX = j;
                    }
                }
            }
        }

        int iMinY = 255, iMaxY = 0;
        for( int j = 0; j < 256; j++ )
        { 
            for( int i=0; i< 256; i++ )
            {
                iIndex = i*256 + j;
                if ( abHisto[iIndex] != 0 )
                {
                    if ( iMinY > i )
                    {
                        iMinY = i;
                    }
                    if ( i > iMaxY )
                    {
                        iMaxY = i;
                    }
                }
            }
        }

        //System.err.println( iMinX + ", " + iMinY + "    " + iMaxX + ", " + iMaxY );
        //iMinX = 0; iMaxX = 255;
        //iMinY = 0; iMaxY = 255;
        m_akHistoTCoord[0] = new Vector2f( iMinX/255.0f, iMinY/255.0f );
        m_akHistoTCoord[1] = new Vector2f( iMaxX/255.0f, iMinY/255.0f );
        m_akHistoTCoord[2] = new Vector2f( iMaxX/255.0f, iMaxY/255.0f );
        m_akHistoTCoord[3] = new Vector2f( iMinX/255.0f, iMaxY/255.0f );
        
        m_kHisto = 
            new GraphicsImage( GraphicsImage.FormatMode.IT_L8, 
                    256,256, null, new String("VolumeImageHisto" + kPostFix) );
        m_kHisto.SetData( abHisto, 256, 256 );
    }


    private void GradientMagnitudeImage(ModelImage kImage, String kPostfix)
    {
        JDialogGradientMagnitude kCalcMagnitude = new JDialogGradientMagnitude( null, kImage );
        kCalcMagnitude.setSeparateThread( false );
        kCalcMagnitude.actionPerformed( new ActionEvent(this, 0, "OK" ) );
        ModelImage kImageGM = kCalcMagnitude.getResultImage();
        
        m_kVolumeGM =  VolumeImage.UpdateData( kImageGM, null, m_kVolumeGMTarget, "GM"+kPostfix );
        m_kVolumeGMTarget = new Texture();
        m_kVolumeGMTarget.SetImage(m_kVolumeGM);
        m_kVolumeGMTarget.SetShared(true);
        m_kVolumeGMTarget.SetFilterType(Texture.FilterType.LINEAR);
        m_kVolumeGMTarget.SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kVolumeGMTarget.SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kVolumeGMTarget.SetWrapType(2,Texture.WrapType.CLAMP_BORDER);
        
        
        /*
        JDialogLaplacian kCalcLaplacian = new JDialogLaplacian( null, kImage );
        kCalcLaplacian.setSeparateThread( false );
        kCalcLaplacian.actionPerformed( new ActionEvent(this, 0, "OK" ) );
        ModelImage kImageGMGM = kCalcLaplacian.getResultImage();        
        AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(kImageGMGM, ModelStorageBase.UBYTE, 
                kImageGMGM.getMin(), kImageGMGM.getMax(), 
                0, 255, false);
        changeTypeAlgo.setRunningInSeparateThread(false);
        changeTypeAlgo.run();
        */
        m_kVolumeGMGM =  m_kVolumeGM;//VolumeImage.UpdateData( kImageGMGM, null, m_kVolumeGMGMTarget, "GMGM"+kPostfix );
        m_kVolumeGMGMTarget = new Texture();
        m_kVolumeGMGMTarget.SetImage(m_kVolumeGMGM);
        m_kVolumeGMGMTarget.SetShared(true);
        m_kVolumeGMGMTarget.SetFilterType(Texture.FilterType.LINEAR);
        m_kVolumeGMGMTarget.SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kVolumeGMGMTarget.SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kVolumeGMGMTarget.SetWrapType(2,Texture.WrapType.CLAMP_BORDER);
    }
    
    /**
     * Initialize the scale factors. Based on the ModelImage Volume.
     */
    private void InitScale()
    {
        float fMaxX = (m_kImage.getExtents()[0] - 1) * m_kImage.getFileInfo(0).getResolutions()[0];
        float fMaxY = (m_kImage.getExtents()[1] - 1) * m_kImage.getFileInfo(0).getResolutions()[1];
        float fMaxZ = (m_kImage.getExtents()[2] - 1) * m_kImage.getFileInfo(0).getResolutions()[2];

        float fMax = fMaxX;
        if (fMaxY > fMax) {
            fMax = fMaxY;
        }
        if (fMaxZ > fMax) {
            fMax = fMaxZ;
        }
        m_fX = fMaxX/fMax;
        m_fY = fMaxY/fMax;
        m_fZ = fMaxZ/fMax;
    }
    
    
    /**
     * Update the opacity transfer function.
     * @param kImage the ModelImage the transfer function applies to.
     * @param kOpacityTexture the opacity Texture passed to the GPU
     * @param kOpacityMap the opacity data stored in the GraphicsImage
     * @param kTransfer the new transfer function.
     */
    private boolean UpdateImages(ModelImage kImage, Texture kOpacityTexture,
                                 GraphicsImage kOpacityMap, TransferFunction kTransfer)
    {
         int iLutHeight = 256;
         float[] afData = kOpacityMap.GetFloatData();

         float fRange = (float)(kImage.getMax() - kImage.getMin());
         float fStep = fRange / iLutHeight;
         float fDataValue = (float)kImage.getMin();
         for (int i = 0; i < iLutHeight; i++) {
             afData[i] = (kTransfer.getRemappedValue( fDataValue, iLutHeight )/255.0f);
             fDataValue += fStep;
         }
         kOpacityTexture.Reload(true);
         return true;
    }
    
    
    /**
     * In order to map line integrals of image intensity to RGB colors where each color channel is 8 bits, it is
     * necessary to make sure that the integrals are in [0,255]. Producing a theoretical maximum value of a line
     * integral is not tractable in an application. This method constructs an approximate maximum by integrating along
     * each line of voxels in the image with line directions parallel to the coordinate axes. The 'processRay' call
     * adjusts the line integrals using the estimate, but still clamps the integrals to 255 since the estimate might not
     * be the true maximum.
     *
     * @return  float Integral normalization factor.
     */
    protected float computeIntegralNormalizationFactor() {       
        int iXBound = m_kImage.getExtents()[0];
        int iYBound = m_kImage.getExtents()[1];
        int iZBound = m_kImage.getExtents()[2];

        byte[] aucData = null;
        if ( m_kImage.isColorImage() )
        {
            aucData = new byte[4*iXBound*iYBound*iZBound];
        }
        else
        {
            aucData = new byte[iXBound*iYBound*iZBound];
        }

        try {
            m_kImage.exportData( 0, m_kImage.getSize(), aucData );
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        // compute image normalization factor
        int iX, iY, iZ, iBase, iSteps;
        float fMaxIntegral = 0.0f;
        float fTStep, fIntegral;

        // fix y and z, integrate over x
        for (iY = 0; iY < iYBound; iY++) {

            for (iZ = 0; iZ < iZBound; iZ++) {
                iBase = iXBound * (iY + (iYBound * iZ));
                iSteps = iXBound - 1;
                fIntegral = 0.5f * ((aucData[iBase] & 0x0ff) + (aucData[iBase + iSteps] & 0x0ff));
                fTStep = 1.0f / iSteps;

                for (iX = 1; iX < iSteps; iX++) {
                    fIntegral += (aucData[iBase + iX] & 0x0ff);
                }

                fIntegral *= fTStep;

                if (fIntegral > fMaxIntegral) {
                    fMaxIntegral = fIntegral;
                }
            }
        }
        int iXYProduct = iXBound * iYBound;
        // fix x and z, integrate over y
        for (iX = 0; iX < iXBound; iX++) {

            for (iZ = 0; iZ < iZBound; iZ++) {
                iBase = iX + (iXYProduct * iZ);
                iSteps = iYBound - 1;
                fIntegral = 0.5f * ((aucData[iBase] & 0x0ff) + (aucData[iBase + (iXBound * iSteps)] & 0x0ff));
                fTStep = 1.0f / iSteps;

                for (iY = 1; iY < iSteps; iY++) {
                    fIntegral += (aucData[iBase + (iXBound * iY)] & 0x0ff);
                }

                fIntegral *= fTStep;

                if (fIntegral > fMaxIntegral) {
                    fMaxIntegral = fIntegral;
                }
            }
        }

        // fix x and y, integrate over z
        for (iX = 0; iX < iXBound; iX++) {

            for (iY = 0; iY < iYBound; iY++) {
                iBase = iX + (iXBound * iY);
                iSteps = iZBound - 1;
                fIntegral = 0.5f * ((aucData[iBase] & 0x0ff) + (aucData[iBase + (iXYProduct * iSteps)] & 0x0ff));
                fTStep = 1.0f / iSteps;

                for (iZ = 1; iZ < iSteps; iZ++) {
                    fIntegral += (aucData[iBase + (iXYProduct * iZ)] & 0x0ff);
                }

                fIntegral *= fTStep;

                if (fIntegral > fMaxIntegral) {
                    fMaxIntegral = fIntegral;
                }
            }
        }

        return (fMaxIntegral > 0.0f) ? (1.0f / fMaxIntegral) : 0.00f;
    }
    
    public float getDRRNorm()
    {
        return m_fDRRNormalize;
    }

}
