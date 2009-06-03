package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.MipavMath;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.TransferFunction;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogGradientMagnitude;
import gov.nih.mipav.view.dialogs.JDialogLaplacian;
import gov.nih.mipav.view.dialogs.JDialogSubset;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.io.File;
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
    private GraphicsImage[] m_kVolume;
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
    private GraphicsImage[] m_kVolumeGM;
    /** Texture object for data: */
    private Texture m_kVolumeGMTarget;
    
    /** Data storage for volume  second derivative: */
    private GraphicsImage[] m_kVolumeGMGM;
    /** Texture object for data: */
    private Texture m_kVolumeGMGMTarget;

    /** Data storage for surfaces: */
    private GraphicsImage m_kSurfaceImage;
    /** Texture object for surfaces: */
    private Texture m_kSurfaceTarget;

    private boolean m_bByte = true;
    private ModelLUT m_kLUT = null;
    private ModelRGB m_kRGBT = null;

    private float m_fX = 1, m_fY = 1, m_fZ = 1;
    private String m_kPostfix = null;
    
    private GraphicsImage[] m_kHisto = null;
    /** Texture object for data: */
    private Texture m_kHistoTarget;
    private Vector2f[] m_akHistoTCoord = new Vector2f[4];
    private float m_fDRRNormalize = 255.0f;

    private int m_iTimeSlice = 0;
    private int m_iTimeSteps = 0;
    
    private ModelImage[] m_akImages;
    
    
    public VolumeImage( ModelImage kImage, String kPostfix, boolean bCompute, String kDir, int iFilterType, int[] aiExtents )
    {
        String kImageName = ModelImage.makeImageName( kImage.getFileInfo(0).getFileName(), kPostfix);
        File kFile = new File( kDir + kImageName + ".xml" );
        if ( !bCompute && kFile.exists() )
        {
            ReadFromDisk( kImageName, kDir, kPostfix );
        }
        else
        {
            bCompute = true;
            GenerateRenderFiles( kImage, kImageName, kDir, kPostfix, iFilterType, aiExtents );
        }
        m_kImage.calcMinMax();
        initLUT(kImage);
        initImages( bCompute, kPostfix, kDir );     
    }
    
    

    public void initImages( boolean bCreate, String kPostfix, String kDir )
    {
        m_fDRRNormalize = computeIntegralNormalizationFactor();
        m_kColorMap = InitColorMap(m_kLUT, m_kRGBT, kPostfix);
        m_kOpacityMap = InitOpacityMap(m_kImage, kPostfix);
        m_kOpacityMap_GM = InitOpacityMap(m_kImage, new String(kPostfix + "_GM"));

        /* Map the ModelImage volume data to a texture image, including for
         * the ModelImage gradient magnitude data: */
        int[] aiExtents = m_kImage.getExtents();
        int iNDims = aiExtents.length;
        if ( iNDims == 3 )
        {
            m_iTimeSteps = 1;
            m_kVolume = new GraphicsImage[1];
            m_kVolumeGM = new GraphicsImage[1];
            m_kVolumeGMGM = new GraphicsImage[1];
            m_kVolume[0] = UpdateData(m_kImage, m_iTimeSlice, null, null, m_kVolumeTarget, m_kImage.getImageName() );
        }
        else
        {
            m_iTimeSteps = aiExtents[3];
            int[] aiSubset = new int[]{aiExtents[0], aiExtents[1], aiExtents[2]};
            
            m_akImages = new ModelImage[ m_iTimeSteps ];            
            
            m_kVolume = new GraphicsImage[m_iTimeSteps];
            m_kVolumeGM = new GraphicsImage[m_iTimeSteps];
            m_kVolumeGMGM = new GraphicsImage[m_iTimeSteps];
            for ( int i = 0; i < m_kVolume.length; i++ )
            {
                m_akImages[i] = new ModelImage( m_kImage.getType(), aiSubset, JDialogBase.makeImageName(m_kImage.getImageName(), "_" + i) );
                m_kVolume[i] = UpdateData(m_kImage, i, m_akImages[i], null, m_kVolumeTarget, m_akImages[i].getImageName() );      
                
                m_akImages[i].copyFileTypeInfo(m_kImage);
                m_akImages[i].calcMinMax();
            }
        }
        GradientMagnitudeImage(m_kImage, kPostfix, kDir, bCreate );
        GenerateHistogram( m_kVolume, m_kVolumeGM, kPostfix, kDir, bCreate  );
        
        m_kVolumeTarget = new Texture();
        m_kVolumeTarget.SetImage(m_kVolume[0]);
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
        byte[] aucData = new byte[256*4];
        if ( kLUT == null )
        {
            ModelLUT.exportIndexedLUTMin( kRGBT, aucData );
            
        }
        else
        {
            ModelLUT.exportIndexedLUTMin( kLUT, aucData );
        }
        return new GraphicsImage(
                                 GraphicsImage.FormatMode.IT_RGBA8888,256,aucData,
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
        ModelLUT.exportIndexedLUTMin( kRGBT, kImage.GetData() );
        kTexture.Reload(true);
    }
    

    public void update4D( boolean bForward )
    {
        if (  m_iTimeSteps == 1 )
        {
            return;
        }
        if ( bForward )
        {
            m_iTimeSlice++;
        }
        else
        {
            m_iTimeSlice--;
        }
        if ( m_iTimeSlice >= m_iTimeSteps )
        {
            m_iTimeSlice = 0;
        }
        if ( m_iTimeSlice < 0 )
        {
            m_iTimeSlice = m_iTimeSteps-1;
        }
        
        update4D();
    }
    
    private void update4D()
    {
        m_kVolumeTarget.SetImage(m_kVolume[m_iTimeSlice]);
        m_kVolumeTarget.Release();
        m_kVolumeGMTarget.SetImage(m_kVolumeGM[m_iTimeSlice]);
        m_kVolumeGMTarget.Release();
        m_kVolumeGMGMTarget.SetImage(m_kVolumeGMGM[m_iTimeSlice]);
        m_kVolumeGMGMTarget.Release();
        
        m_kHistoTarget.SetImage( m_kHisto[m_iTimeSlice] );
        m_kHistoTarget.Release();
    }
    

    /**
     * Update the image volume data on the GPU.
     * @param kImage the new ModelImage
     * @param kVolumeImage the volume data image.
     * @param kVolumeTexture the volume data texture.
     * @param kPostFix the postfix string for the image name.
     */
    public static GraphicsImage UpdateData( ModelImage kImage, int iTimeSlice, ModelImage kNewImage, GraphicsImage kVolumeImage,
            Texture kVolumeTexture, String kImageName )
    {
        int iXBound = kImage.getExtents()[0];
        int iYBound = kImage.getExtents()[1];
        int iZBound = kImage.getExtents()[2];

        byte[] aucData = null;
        int iSize = iXBound*iYBound*iZBound;
        if ( kImage.isColorImage() )
        {
            iSize *= 4;
            aucData = new byte[iSize];
            try {
                kImage.exportData( iTimeSlice * iSize, iSize, aucData );
                for ( int i = 0; i < iSize; i += 4)
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
                            iXBound,iYBound,iZBound,aucData, kImageName);
            }
            else
            {
                kVolumeImage.SetData( aucData, iXBound, iYBound, iZBound );
            }
        }
        else
        {
            aucData = new byte[iSize];
            try {
                kImage.exportData( iTimeSlice * iSize, iSize, aucData );
                if ( kVolumeImage == null )
                {
                kVolumeImage =
                    new GraphicsImage( GraphicsImage.FormatMode.IT_L8, 
                            iXBound,iYBound,iZBound, aucData, kImageName);
                }
                else
                {
                    kVolumeImage.SetData( aucData, iXBound, iYBound, iZBound );
                }
            } catch (IOException e) {
                e.printStackTrace();
            }

        }
        if ( kNewImage != null )
        {
            try {
                kNewImage.importData( 0, aucData, true );
            } catch (IOException e) {}
        }
        if ( kVolumeTexture != null )
        {
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
        ModelLUT.exportIndexedLUTMin( kLUT, kColorMap.GetData() );
        kColorTexture.Reload(true);
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

        for ( int i = 0; i < m_kVolume.length; i++ )
        {
            m_kVolume[i].dispose();
        }
        m_kVolume = null;
        m_kVolumeTarget.dispose();
        m_kVolumeTarget = null;
    
        m_kNormal.dispose();
        m_kNormal = null;
        m_kNormalMapTarget.dispose();
        m_kNormalMapTarget = null;
    
        m_kColorMap.dispose();
        m_kColorMap = null;
        m_kColorMapTarget.dispose();
        m_kColorMapTarget = null;
    
        m_kOpacityMap.dispose();
        m_kOpacityMap = null;
        m_kOpacityMapTarget.dispose();
        m_kOpacityMapTarget = null;
    
        for ( int i = 0; i < m_kVolumeGM.length; i++ )
        {
            m_kVolumeGM[i].dispose();
        }
        m_kVolumeGM = null;
        m_kVolumeGMTarget.dispose();
        m_kVolumeGMTarget = null;
    
        for ( int i = 0; i < m_kVolumeGMGM.length; i++ )
        {
            m_kVolumeGMGM[i].dispose();
        }
        m_kVolumeGMGM = null;
        m_kVolumeGMGMTarget.dispose();
        m_kVolumeGMGMTarget = null;
        
        m_kOpacityMap_GM.dispose();
        m_kOpacityMap_GM = null;
        m_kOpacityMapTarget_GM.dispose();
        m_kOpacityMapTarget_GM = null;

        if ( m_kSurfaceImage != null )
        {
            m_kSurfaceImage.dispose();
            m_kSurfaceImage = null;
            m_kSurfaceTarget.dispose();
            m_kSurfaceTarget = null;
        }

        m_kLUT = null;
        m_kPostfix = null;
    
        for ( int i = 0; i < m_kHisto.length; i++ )
        {
            m_kHisto[i].dispose();
        }
        m_kHisto = null;
        m_akHistoTCoord = null;
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
    
    
    public String GetHistoName()
    {
        return m_kHisto[m_iTimeSlice].GetName();
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
     * Return the Volume RGBT.
     * @return Volume RGBT.
     */
    public ModelRGB GetRGB()
    {
        return m_kRGBT;
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
    
    public int GetTimeSlice()
    {
        return m_iTimeSlice;
    }
    
    public void SetTimeSlice(int iSlice)
    {
        m_iTimeSlice = iSlice;
        update4D();
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
        VolumeImage.UpdateData( m_kImage, m_iTimeSlice, m_akImages[m_iTimeSlice], m_kVolume[m_iTimeSlice], m_kVolumeTarget, m_kImage.getImageName() );
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
            UpdateImages2( m_kImage, m_kColorMapTarget, m_kColorMap, kTransfer );
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


    private void GenerateHistogram( GraphicsImage[] kImage, GraphicsImage[] kImageGM, String kPostFix, String kDir, boolean bCreate  )
    {
        int iTMinX = 255, iTMaxX = 0;
        int iTMinY = 255, iTMaxY = 0;
        m_kHisto = new GraphicsImage[m_iTimeSteps];
        for ( int t = 0; t < m_iTimeSteps; t ++ )
        {
            float[] afCount = new float[256*256];
            for(int i=0; i<256*256; ++i) {
                afCount[i] = 0;
            }

            short a1, a2;
            byte[] abHistoData = kImageGM[t].GetData();
            byte[] abData = kImage[t].GetData();
            if ( m_kImage.isColorImage() )
            {
                int iHisto = 0;
                for ( int i = 0; i < abData.length; i +=4 )
                {
                    int iR = (abData[i]);
                    int iG = (abData[i+1]);
                    int iB = (abData[i+2]);
                    a1 = (short)(iR * 0.299 + iG * 0.587 + iB * 0.114);
                    a1 = (short)(a1 & 0x00ff);

                    iR = (abHistoData[i]);
                    iG = (abHistoData[i+1]);
                    iB = (abHistoData[i+2]);
                    a2 = (short)(iR * 0.299 + iG * 0.587 + iB * 0.114);
                    a2 = (short)(a2 & 0x00ff);
                    afCount[a1 +  a2 * 256] += 1;
                    iHisto++;
                }
            }
            else
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
            afCount = null;

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
            if ( iTMinX > iMinX )
            {
                iTMinX = iMinX;
            }
            if ( iTMaxX < iMaxX )
            {
                iTMaxX = iMaxX;
            }
            
            if ( iTMinY > iMinY )
            {
                iTMinY = iMinY;
            }
            if ( iTMaxY < iMaxY )
            {
                iTMaxY = iMaxY;
            }
            
            System.err.println( iMinX + ", " + iMinY + "    " + iMaxX + ", " + iMaxY );
            //iMinX = 0; iMaxX = 255;
            //iMinY = 0; iMaxY = 255;

            m_kHisto[t] = 
                new GraphicsImage( GraphicsImage.FormatMode.IT_L8, 
                        256,256, null, new String("VolumeImageHisto" + kPostFix) );
            m_kHisto[t].SetData( abHisto, 256, 256 );
        }
        

        m_kHistoTarget = new Texture();
        m_kHistoTarget.SetImage(m_kHisto[0]);
        m_kHistoTarget.SetShared(true);
        m_kHistoTarget.SetFilterType(Texture.FilterType.LINEAR);
        m_kHistoTarget.SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kHistoTarget.SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kHistoTarget.SetWrapType(2,Texture.WrapType.CLAMP_BORDER);
        
        m_akHistoTCoord[0] = new Vector2f( iTMinX/255.0f, iTMinY/255.0f );
        m_akHistoTCoord[1] = new Vector2f( iTMaxX/255.0f, iTMinY/255.0f );
        m_akHistoTCoord[2] = new Vector2f( iTMaxX/255.0f, iTMaxY/255.0f );
        m_akHistoTCoord[3] = new Vector2f( iTMinX/255.0f, iTMaxY/255.0f );
        
        System.err.println( iTMinX + ", " + iTMinY + "    " + iTMaxX + ", " + iTMaxY );
    }

    private void GradientMagnitudeImage(ModelImage kImage, String kPostfix, String kDir, boolean bCreate )
    {
        for ( int i = 0; i < m_iTimeSteps; i++ )
        {
            String kImageName = ModelImage.makeImageName( kImage.getFileInfo(0).getFileName(), 
                    new String ( "_GM_" + kPostfix + "_" + i ) );

            ModelImage kImageGM = null;
            if ( !bCreate )
            {
                FileIO fileIO = new FileIO();
                kImageGM = fileIO.readImage( kImageName + ".xml", kDir );
                System.err.println( "Reading file " + kImageName );
            }
            if ( kImageGM == null )
            {
                if ( m_iTimeSteps == 1 )
                {
                    kImageGM = (ModelImage)kImage.clone();
                }
                else
                {
                    kImageGM = (ModelImage)m_akImages[i].clone();
                }
                JDialogGradientMagnitude kCalcMagnitude = new JDialogGradientMagnitude( null, kImageGM );
                kCalcMagnitude.setVisible(false);
                kCalcMagnitude.setOutputNewImage( false );
                kCalcMagnitude.setDisplayProgressBar( false );
                kCalcMagnitude.setSeparateThread( false );
                kCalcMagnitude.actionPerformed( new ActionEvent(this, 0, "OK" ) );
                kCalcMagnitude = null;     
                if ( kImageGM != null )
                {
                    System.err.println( "...writing file " + kImageName );
                    kImageGM.saveImage( kDir, kImageName, FileUtility.XML, false );
                }
            }
            if ( kImageGM == null )
            {
                System.err.println( "Gradient magnitude calculation returned null" );
                m_kVolumeGM[i] = VolumeImage.UpdateData( kImage, i, null, null, m_kVolumeGMTarget, kImageName );
            }
            else {
                m_kVolumeGM[i] = VolumeImage.UpdateData( kImageGM, 0, null, null, m_kVolumeGMTarget, kImageName );
            }
            ViewJFrameImage kImageFrame = ViewUserInterface.getReference().getFrameContainingImage(kImageGM);
            if ( kImageFrame != null )
            {
                kImageFrame.close();
            }
            else if ( kImageGM != null )
            {
                kImageGM.disposeLocal();
                kImageGM = null;
            }
        }
        m_kVolumeGMTarget = new Texture();
        m_kVolumeGMTarget.SetImage(m_kVolumeGM[0]);
        m_kVolumeGMTarget.SetShared(true);
        m_kVolumeGMTarget.SetFilterType(Texture.FilterType.LINEAR);
        m_kVolumeGMTarget.SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kVolumeGMTarget.SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kVolumeGMTarget.SetWrapType(2,Texture.WrapType.CLAMP_BORDER);

        for ( int i = 0; i < m_iTimeSteps; i++ )
        {
            String kImageName = ModelImage.makeImageName( kImage.getFileInfo(0).getFileName(), 
                    new String ( "_Laplacian_" + kPostfix + "_" + i ) );
            ModelImage kImageGMGM = null;            
            if ( !bCreate )
            {
                FileIO fileIO = new FileIO();
                kImageGMGM = fileIO.readImage( kImageName + ".xml", kDir );
                System.err.println( "Reading file " + kImageName );
            }
            if ( kImageGMGM == null )
            {
                if ( m_iTimeSteps == 1 )
                {
                    kImageGMGM = (ModelImage)kImage.clone();
                }
                else
                {
                    kImageGMGM = (ModelImage)m_akImages[i].clone();
                }    
                JDialogLaplacian kCalcLaplacian = new JDialogLaplacian( null, kImageGMGM );
                kCalcLaplacian.setVisible(false);
                kCalcLaplacian.setOutputNewImage( false );
                kCalcLaplacian.setDisplayProgressBar(false);
                kCalcLaplacian.setSeparateThread( false );
                kCalcLaplacian.actionPerformed( new ActionEvent(this, 0, "OK" ) );
                if ( kImageGMGM != null )
                {
                    kImageGMGM.calcMinMax();
                    AlgorithmChangeType changeTypeAlgo = null;          
                    if ( kImageGMGM.isColorImage() )
                    {
                        changeTypeAlgo = new AlgorithmChangeType(kImageGMGM, ModelStorageBase.ARGB, 
                                kImageGMGM.getMin(), kImageGMGM.getMax(), 
                                0, 255, false);
                    }
                    else
                    {
                        changeTypeAlgo = new AlgorithmChangeType(kImageGMGM, ModelStorageBase.UBYTE, 
                                kImageGMGM.getMin(), kImageGMGM.getMax(), 
                                0, 255, false);
                    }
                    changeTypeAlgo.setRunningInSeparateThread(false);
                    changeTypeAlgo.run();
                    kImageGMGM.saveImage( kDir, kImageName, FileUtility.XML, false );
                    System.err.println( "...writing file " + kImageName );
                }
            }
            if ( kImageGMGM != null )
            {            
                m_kVolumeGMGM[i] = VolumeImage.UpdateData( kImageGMGM, 0, null, null, m_kVolumeGMGMTarget, kImageName );
            }
            else
            {
                System.err.println( "Laplacian calculation returned null" );
                m_kVolumeGMGM[i] = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA8888,
                        kImage.getExtents()[0],kImage.getExtents()[1],kImage.getExtents()[2],(byte[])null,
                        new String("VolumeImageGMGM"+kPostfix+kPostfix));
            }
            ViewJFrameImage kImageFrame = ViewUserInterface.getReference().getFrameContainingImage(kImageGMGM);
            if ( kImageFrame != null )
            {
                kImageFrame.close();
            }
            else if ( kImageGMGM != null )
            {
                kImageGMGM.disposeLocal();
                kImageGMGM = null;
            }
        }
        m_kVolumeGMGMTarget = new Texture();
        m_kVolumeGMGMTarget.SetImage(m_kVolumeGMGM[0]);
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
     * Update the opacity transfer function.
     * @param kImage the ModelImage the transfer function applies to.
     * @param kOpacityTexture the opacity Texture passed to the GPU
     * @param kOpacityMap the opacity data stored in the GraphicsImage
     * @param kTransfer the new transfer function.
     */
    private boolean UpdateImages2(ModelImage kImage, Texture kOpacityTexture,
                                  GraphicsImage kOpacityMap, TransferFunction kTransfer)
    {
         int iLutHeight = kOpacityMap.GetBound(0);
         byte[] abData = kOpacityMap.GetData();

         float fRange = (float)(kImage.getMax() - kImage.getMin());
         float fStep = fRange / iLutHeight;
         float fDataValue = (float)kImage.getMin();
         float fVal;
         for (int i = 0; i < iLutHeight; i++) {
             fVal = (kTransfer.getRemappedValue( fDataValue, iLutHeight )/255.0f);
             abData[i*4 + 3] = (byte)(fVal*255);
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
        int iSize = iXBound*iYBound*iZBound;
        if ( m_kImage.isColorImage() )
        {
            iSize *= 4;
        }

        aucData = new byte[iSize];

        try {
            m_kImage.exportData( 0, iSize, aucData );
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
        aucData = null;
        return (fMaxIntegral > 0.0f) ? (1.0f / fMaxIntegral) : 0.00f;
    }
    
    public float getDRRNorm()
    {
        return m_fDRRNormalize;
    }
    

    private void GenerateRenderFiles( ModelImage kImage, String kImageName, String kDir, String kPostfix, int iFilterType, int[] aiExtents )
    {
        ReconfigureImage( kImage, kImageName, kDir, iFilterType, aiExtents );        
    }
    
    private void ReconfigureImage(ModelImage kImage, String kImageName, String kDir, int iFilterType, int[] aiExtents)
    {
        
        int[] extents = kImage.getExtents();
        float[] res = kImage.getFileInfo(0).getResolutions();
        
        float[] newRes = res.clone();
        int[] volExtents = extents.clone();
        
        
        // Checking to see if the image has all dimensions that are a power of 2.
        for (int i = 0; i < 3; i++) {
            if ( aiExtents != null )
            {
                volExtents[i] = aiExtents[i];
            }
            else
            {
                volExtents[i] = MipavMath.dimPowerOfTwo(extents[i]);
            }
            newRes[i] = (res[i] * extents[i]) / volExtents[i];
        }

        ModelImage kNewRes = kImage;
        if ( (extents[0] != volExtents[0]) || (extents[1] != volExtents[1]) || (extents[2] != volExtents[2]))
        { 
            AlgorithmTransform transformFunct = new AlgorithmTransform(kImage, new TransMatrix(4), iFilterType, newRes[0], newRes[1],
                                                    newRes[2], volExtents[0], volExtents[1], volExtents[2], false,
                                                    true, false);

            transformFunct.setRunningInSeparateThread(false);
            transformFunct.run();

            if (transformFunct.isCompleted() == false) {

                // What to do
                transformFunct.finalize();
                transformFunct = null;
            }

            kNewRes = transformFunct.getTransformedImage();
            kNewRes.calcMinMax();

            transformFunct.disposeLocal();
            transformFunct = null;
        }
        
        
        ModelImage kNewType = null;    
        if ( (kNewRes.getType() != ModelStorageBase.UBYTE) ||
                (kNewRes.isColorImage() && (kNewRes.getType() != ModelStorageBase.ARGB)) )
        {
            AlgorithmChangeType changeTypeAlgo = null;
            if ( kNewRes.isColorImage() )
            {
                kNewType = new ModelImage( ModelStorageBase.ARGB, kNewRes.getExtents(), kImageName );
                changeTypeAlgo = new AlgorithmChangeType( kNewType, kNewRes, 
                        kNewRes.getMin(), kNewRes.getMax(), 
                        0, 255, false);
            }
            else
            {
                kNewType = new ModelImage(  ModelStorageBase.UBYTE, kNewRes.getExtents(), kImageName );
                changeTypeAlgo = new AlgorithmChangeType( kNewType, kNewRes, 
                        kNewRes.getMin(), kNewRes.getMax(), 
                    0, 255, false);
            }
            changeTypeAlgo.setRunningInSeparateThread(false);
            changeTypeAlgo.run();
            changeTypeAlgo.finalize();
            changeTypeAlgo = null;
        }
        
        if ( (kNewType == null) && (kNewRes == kImage) )
        {
            m_kImage = (ModelImage)kImage.clone();
        }
        else
        {
            m_kImage = (kNewType == null) ? kNewRes : kNewType;
        }

        m_kImage.copyFileTypeInfo(kImage);
        FileInfoBase[] fileInfoBases = m_kImage.getFileInfo();
        for (int i=0;i<fileInfoBases.length;i++) {
            fileInfoBases[i].setResolutions(newRes);
        }
        m_kImage.calcMinMax();
        m_kImage.setImageName(  kImageName );
        m_kImage.saveImage( kDir, kImageName, FileUtility.XML, false );
    }

    private void ReadFromDisk( String kImageName, String kDir, String kPostfix )
    {
        FileIO fileIO = new FileIO();
        m_kImage = fileIO.readImage( kImageName + ".xml", kDir );
    }
    
    private void initLUT( ModelImage kImage )
    {

        if (m_kImage.isColorImage()) {    
            float[] x = new float[4];
            float[] y = new float[4];
            Dimension dim = new Dimension(256, 256);

            // Set ModelRGB min max values;
            x[0] = 0;
            y[0] = dim.height - 1;

            x[1] = 255 * 0.333f;
            y[1] = (dim.height - 1) - ((dim.height - 1) / 3.0f);

            x[2] = 255 * 0.667f;
            y[2] = (dim.height - 1) - ((dim.height - 1) * 0.67f);

            x[3] = 255;
            y[3] = 0;

            int[] RGBExtents = new int[2];
            RGBExtents[0] = 4;
            RGBExtents[1] = 256;
            m_kRGBT = new ModelRGB(RGBExtents);
            m_kRGBT.getRedFunction().importArrays(x, y, 4);
            m_kRGBT.getGreenFunction().importArrays(x, y, 4);
            m_kRGBT.getBlueFunction().importArrays(x, y, 4);
            m_kRGBT.makeRGB(-1);
        } else {
            int[] dimExtentsLUT = new int[2];

            dimExtentsLUT[0] = 4;
            dimExtentsLUT[1] = 256;

            m_kLUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);

            float min, max;

            if (m_kImage.getType() == ModelStorageBase.UBYTE) {
                min = 0;
                max = 255;
            } else if (m_kImage.getType() == ModelStorageBase.BYTE) {
                min = -128;
                max = 127;
            } else {
                min = (float) m_kImage.getMin();
                max = (float) m_kImage.getMax();
            }

            float imgMin = (float) m_kImage.getMin();
            float imgMax = (float) m_kImage.getMax();

            m_kLUT.resetTransferLine(min, imgMin, max, imgMax);
        }
    }

}
