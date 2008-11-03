package gov.nih.mipav.view.renderer.WildMagic.Render;

import java.io.File;
import java.io.IOException;
import java.nio.*;

import WildMagic.LibGraphics.Rendering.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogBase;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.*;


public class VolumeImage
{
    /** Reference to ModelImage image */
    private ModelImage m_kImage;

    /** Gradient magnitude for m_kImage */
    private ModelImage m_kImage_GM = null;
    /** GraphicsImage contains GM opacity transfer function data: */
    private GraphicsImage m_kOpacityMap_GM = null;
    /** Texture contains texture filter modes and GraphicsImage for opacity
     * transfer function: */
    private Texture m_kOpacityMapTarget_GM = null;
    /** GraphicsImage contains volume data for gradient magnitude */
    private GraphicsImage m_kVolume_GM = null;
    /** Texture contains the texture filter modes and GraphicsImage for
     * gradient magnitude */
    private Texture m_kVolumeTarget_GM = null;

    /** Data storage for volume: */
    private GraphicsImage m_kVolume;
    /** Data storage for normals: */
    private GraphicsImage m_kNormal;
    /** Data storage for color map: */
    private GraphicsImage m_kColorMap;
    /** Data storage for opacity map: */
    private GraphicsImage m_kOpacityMap = null;
    /** Texture object for data: */
    private Texture m_kVolumeTarget;
    /** Texture object for color map: */
    private Texture m_kColorMapTarget;
    /** Texture object for opacity map: */
    private Texture m_kOpacityMapTarget;
    /** Texture object for normal map: */
    private Texture m_kNormalMapTarget;

    /** Data storage for surfaces: */
    private GraphicsImage m_kSurfaceImage;
    /** Texture object for surfaces: */
    private Texture m_kSurfaceTarget;

    private boolean m_bByte = true;
    private ModelLUT m_kLUT = null;

    private float m_fX = 1, m_fY = 1, m_fZ = 1;
    private String m_kPostfix = null;
    
    public VolumeImage( ModelImage kImage, ModelLUT kLUT, ModelRGB kRGBT, String kPostfix )
    {
        m_kImage = kImage;
        m_kLUT = kLUT;
        m_kColorMap = InitColorMap(kLUT, kRGBT, kPostfix);
        m_kOpacityMap = InitOpacityMap(m_kImage, kPostfix);
        m_kOpacityMap_GM = InitOpacityMap(m_kImage, new String(kPostfix + "_GM"));

        /* Map the ModelImage volume data to a texture image, including for
         * the ModelImage gradient magnitude data: */
        m_kVolume = UpdateData(m_kImage, null, m_kVolumeTarget, m_bByte, kPostfix );

        m_kVolumeTarget = new Texture();
        m_kVolumeTarget.SetImage(m_kVolume);
        m_kVolumeTarget.SetShared(true);
        m_kVolumeTarget.SetFilterType(Texture.FilterType.LINEAR);
        m_kVolumeTarget.SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kVolumeTarget.SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kVolumeTarget.SetWrapType(2,Texture.WrapType.CLAMP_BORDER);
        
        
        m_kImage_GM = CalcHistogramsGM( m_kImage );
        m_kVolume_GM = UpdateData(m_kImage_GM, null, m_kVolumeTarget_GM, m_bByte, new String(kPostfix + "_GM") );

        m_kColorMapTarget = new Texture();
        m_kColorMapTarget.SetShared(true);

        m_kOpacityMapTarget = new Texture();
        m_kOpacityMapTarget.SetShared(true);

        int iXBound = m_kImage.getExtents()[0];
        int iYBound = m_kImage.getExtents()[1];
        int iZBound = m_kImage.getExtents()[2];
        m_kNormal = new GraphicsImage(GraphicsImage.FormatMode.IT_RGB888,
                                       iXBound,iYBound,iZBound,(byte[])null,
                                       new String("NormalMap"+kPostfix));
        m_kNormalMapTarget = new Texture();
        m_kNormalMapTarget.SetImage(m_kNormal);
        m_kNormalMapTarget.SetShared(true);
        m_kNormalMapTarget.SetFilterType(Texture.FilterType.LINEAR);
        m_kNormalMapTarget.SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kNormalMapTarget.SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kNormalMapTarget.SetWrapType(2,Texture.WrapType.CLAMP_BORDER);

        m_kVolumeTarget_GM = new Texture();
        m_kVolumeTarget_GM.SetShared(true);
        m_kVolumeTarget_GM.SetFilterType(Texture.FilterType.LINEAR);
        m_kVolumeTarget_GM.SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kVolumeTarget_GM.SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kVolumeTarget_GM.SetWrapType(2,Texture.WrapType.CLAMP_BORDER);

        m_kOpacityMapTarget_GM = new Texture();
        m_kOpacityMapTarget_GM.SetShared(true);
        

        m_kSurfaceImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGB888,
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
     * Update the image data.
     * @param kImage the modified ModelImage
     * @param kPostfix which image (imageA, imageB)
     */
    public void UpdateData( ModelImage kImage, String kPostfix )
    {
        m_kImage = kImage;
        VolumeImage.UpdateData( m_kImage, m_kVolume, m_kVolumeTarget, m_bByte, kPostfix );
    }

    /**
     * Update the image volume data on the GPU.
     * @param kImage the new ModelImage
     * @param kVolumeImage the volume data image.
     * @param kVolumeTexture the volume data texture.
     * @param kPostFix the postfix string for the image name.
     */
    public static GraphicsImage UpdateData( ModelImage kImage, GraphicsImage kVolumeImage,
                                      Texture kVolumeTexture, boolean bByte, String kPostFix )
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
            byte[] abData = null;
            float[] afData = null;

            if ( bByte )
            {
                abData = new byte[iXBound*iYBound*iZBound];
                
                int i = 0;
                for (int iZ = 0; iZ < iZBound; iZ++)
                {
                    for (int iY = 0; iY < iYBound; iY++)
                    {
                        for (int iX = 0; iX < iXBound; iX++)
                        {
                            float fValue = kImage.getFloat(iX,iY,iZ);
                            abData[i++] = (byte)(255 * (fValue - fImageMin)/(fImageMax - fImageMin));
                        }
                    }
                }
                kVolumeImage =
                    new GraphicsImage( GraphicsImage.FormatMode.IT_L8, 
                                       iXBound,iYBound,iZBound, abData,
                                       new String( "VolumeImage" + kPostFix));
            }
            else
            {
                afData = new float[iXBound*iYBound*iZBound];

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
                kVolumeImage =
                    new GraphicsImage( GraphicsImage.FormatMode.IT_L8, 
                                       iXBound,iYBound,iZBound, afData,
                                       new String( "VolumeImage" + kPostFix));
            }

            if ( kVolumeTexture != null )
            {
                if ( bByte )
                {
                    kVolumeTexture.GetImage().SetData( abData, iXBound, iYBound, iZBound );
                }
                else
                {
                    kVolumeTexture.GetImage().SetFloatData( afData, iXBound, iYBound, iZBound );
                }
                kVolumeTexture.Release();
            }
        }

        return kVolumeImage;
    }



    /**
     * Calculates histogram for the gradient magnitude ModelImage
     * @param kImage the image to calculate the gradient magnitude.
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

    public void dispose()
    {
        m_kImage = null;

        if ( m_kImage_GM != null )
        {
            m_kImage_GM.disposeLocal();
            m_kImage_GM = null;
        }
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
        if ( m_kVolume_GM != null )
        {
            m_kVolume_GM.dispose();
            m_kVolume_GM = null;
        }
        if ( m_kVolumeTarget_GM != null )
        {
            m_kVolumeTarget_GM.dispose();
            m_kVolumeTarget_GM = null;
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

    /**
     * Update the transfer function for the image iImage.
     * @param kTransfer the new opacity transfer function
     * @param iImage the image to modify (0 = volume image, 2 = gradient mag)
     * @return boolean true when updated, false otherwise.
     */
    public boolean UpdateImages(TransferFunction kTransfer, int iImage)
    {
        if ( iImage == 0 )
        {
            return UpdateImages( m_kImage, m_kOpacityMapTarget, m_kOpacityMap, kTransfer );
        }
       else if ( (iImage == 2) &&
                  (m_kImage_GM != null) &&
                  (m_kOpacityMapTarget_GM != null) &&
                  (m_kOpacityMap_GM != null)  )
        {
            return UpdateImages( m_kImage_GM, m_kOpacityMapTarget_GM, m_kOpacityMap_GM, kTransfer );
        }
        return false;
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

    public Texture GetVolumeGMTarget()
    {
        return m_kVolumeTarget_GM;
    }
    public Texture GetOpacityMapGMTarget()
    {
        return m_kOpacityMapTarget_GM;
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

    public Texture GetColorMapTarget()
    {
        return m_kColorMapTarget;
    }

    public Texture GetOpacityMapTarget()
    {
        return m_kOpacityMapTarget;
    }

    public Texture GetNormalMapTarget()
    {
        return m_kNormalMapTarget;
    }

    public Texture GetSurfaceTarget()
    {
        return m_kSurfaceTarget;
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

    public boolean IsColorImage()
    {
        return m_kImage.isColorImage();
    }
    
    public ModelImage GetImage()
    {
        return m_kImage;
    }
    
    public ModelLUT GetLUT()
    {
        return m_kLUT;
    }

    public ModelImage CreateImageFromTexture()
    {
        int iXBound = m_kImage.getExtents()[0];
        int iYBound = m_kImage.getExtents()[1];
        int iZBound = m_kImage.getExtents()[2];
        m_kImage.calcMinMax();
        float fImageMax = (float)m_kImage.getMax();
        float fImageMin = (float)m_kImage.getMin();
        
        ModelImage kResult = null;
        if ( m_kImage.isColorImage() )
        {
            byte[] aucData = new byte[4*iXBound*iYBound*iZBound];
            for ( int i = 0; i < m_kImage.getSize(); i += 4)
            {
                aucData[i] = m_kVolume.GetData()[i+3];
                aucData[i+1] = m_kVolume.GetData()[i+1];
                aucData[i+2] = m_kVolume.GetData()[i+2];
                aucData[i+3] = m_kVolume.GetData()[i];
            }
            try {
                kResult = new ModelImage( m_kImage.getType(), m_kImage.getExtents(), JDialogBase.makeImageName(m_kImage.getImageName(), "_Crop") );
                kResult.importData( 0, aucData, true );
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        else
        {
            float[] afData = new float[iXBound*iYBound*iZBound];

            if ( m_bByte )
            {
                int i = 0;
                for (int iZ = 0; iZ < iZBound; iZ++)
                {
                    for (int iY = 0; iY < iYBound; iY++)
                    {
                        for (int iX = 0; iX < iXBound; iX++)
                        {

                            byte bValue = m_kVolume.GetData()[i];
                            //afData[i++]  = (float)((fImageMax - fImageMin) * ( bValue / 255.0 )) + fImageMin;
                            afData[i++]  = (float)( bValue / 255.0 );
                        }
                    }
                }
            }
            else
            {
                int i = 0;
                for (int iZ = 0; iZ < iZBound; iZ++)
                {
                    for (int iY = 0; iY < iYBound; iY++)
                    {
                        for (int iX = 0; iX < iXBound; iX++)
                        {
                            float fValue = m_kVolume.GetFloatData()[i];
                            afData[i++] = (fValue*(fImageMax - fImageMin) + fImageMin);
                        }
                    }
                }
            }
            try {
                kResult = new ModelImage( ModelStorageBase.FLOAT, m_kImage.getExtents(), JDialogBase.makeImageName(m_kImage.getImageName(), "_Crop") );
                kResult.importData( 0, afData, true );
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        return kResult;
    }


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
    
    public float GetScaleX()
    {
        return m_fX;
    }    
    public float GetScaleY()
    {
        return m_fY;
    }    
    public float GetScaleZ()
    {
        return m_fZ;
    }
    
    public String GetPostfix()
    {
        return m_kPostfix;
    }
    
    
}
