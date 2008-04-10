package gov.nih.mipav.view.renderer.WildMagic.Render;

import java.io.File;
import java.io.IOException;
import java.nio.*;

import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Rendering.*;

import gov.nih.mipav.view.*;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.*;


public class VolumeImage
{
    /** Reference to ModelImage imageA in ViewJFrameVolumeView */
    private ModelImage m_kImageA;

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

    private boolean m_bByte = true;
    private ModelLUT m_kLUT = null;

    public VolumeImage( ModelImage kImage, ModelLUT kLUTA, ModelRGB kRGBTA )
    {
        m_kImageA = kImage;
        m_kLUT = kLUTA;
        m_kColorMapA = InitColorMap(kLUTA, kRGBTA, new String("A"));
        m_kOpacityMapA = InitOpacityMap(m_kImageA, new String("A"));
        m_kOpacityMapA_GM = InitOpacityMap(m_kImageA, new String("A_GM"));

        /* Map the ModelImage volume data to a texture image, including for
         * the ModelImage gradient magnitude data: */
        m_kVolumeA = UpdateData(m_kImageA, null, m_kVolumeTargetA, m_bByte, new String("A") );
        
        m_kImageA_GM = CalcHistogramsGM( m_kImageA );
        m_kVolumeA_GM = UpdateData(m_kImageA_GM, null, m_kVolumeTargetA_GM, m_bByte, new String("A_GM") );


        m_kVolumeTargetA = new Texture();
        m_kVolumeTargetA.SetShared(true);
        m_kVolumeTargetA.SetFilterType(Texture.FilterType.LINEAR);
        m_kVolumeTargetA.SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kVolumeTargetA.SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kVolumeTargetA.SetWrapType(2,Texture.WrapType.CLAMP_BORDER);

        m_kColorMapTargetA = new Texture();
        m_kColorMapTargetA.SetShared(true);

        m_kOpacityMapTargetA = new Texture();
        m_kOpacityMapTargetA.SetShared(true);

        int iXBound = m_kImageA.getExtents()[0];
        int iYBound = m_kImageA.getExtents()[1];
        int iZBound = m_kImageA.getExtents()[2];
        m_kNormalA = new GraphicsImage(GraphicsImage.FormatMode.IT_RGB888,
                                       iXBound,iYBound,iZBound,(byte[])null,
                                       "NormalMapA");
        m_kNormalMapTargetA = new Texture();
        m_kNormalMapTargetA.SetImage(m_kNormalA);
        m_kNormalMapTargetA.SetShared(true);
        m_kNormalMapTargetA.SetFilterType(Texture.FilterType.LINEAR);
        m_kNormalMapTargetA.SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kNormalMapTargetA.SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kNormalMapTargetA.SetWrapType(2,Texture.WrapType.CLAMP_BORDER);

        m_kVolumeTargetA_GM = new Texture();
        m_kVolumeTargetA_GM.SetShared(true);
        m_kVolumeTargetA_GM.SetFilterType(Texture.FilterType.LINEAR);
        m_kVolumeTargetA_GM.SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kVolumeTargetA_GM.SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kVolumeTargetA_GM.SetWrapType(2,Texture.WrapType.CLAMP_BORDER);

        m_kOpacityMapTargetA_GM = new Texture();
        m_kOpacityMapTargetA_GM.SetShared(true);
    }

    /**
     * Initialize the textures for the color lookup table.
     * @param kLUT, the new LUT.
     * @param kRGBT, the new RGB table.
     * @param kPostfix, the string postfix to concatenate to the "ColorMap" image name.
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
     * Update the image data.
     * @param kImage, the modified ModelImage
     * @param iImage, which image (imageA, imageB)
     */
    public void UpdateData( ModelImage kImage, int iImage )
    {
        if ( iImage == 0 )
        {
            m_kImageA = kImage;
            VolumeImage.UpdateData( m_kImageA, m_kVolumeA, m_kVolumeTargetA, m_bByte, new String( "A" ) );
        }
    }

    /**
     * Update the image volume data on the GPU.
     * @param kImage, the new ModelImage
     * @param kVolumeImage, the volume data image.
     * @param kVolumeTexture, the volume data texture.
     * @param kPostFix, the postfix string for the image name.
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
        m_kImageA = null;

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
        /*
        else if ( (iImage == 1) && m_kImageB != null )
        {
            return UpdateImages( m_kImageB, m_kOpacityMapTargetB, m_kOpacityMapB, kTransfer );
        }
        */
        else if ( (iImage == 2) &&
                  (m_kImageA_GM != null) &&
                  (m_kOpacityMapTargetA_GM != null) &&
                  (m_kOpacityMapA_GM != null)  )
        {
            return UpdateImages( m_kImageA_GM, m_kOpacityMapTargetA_GM, m_kOpacityMapA_GM, kTransfer );
        }
        /*
        else if ( (iImage == 3) &&
                  (m_kImageB_GM != null) &&
                  (m_kOpacityMapTargetB_GM != null) &&
                  (m_kOpacityMapB_GM != null)  )
        {
            return UpdateImages( m_kImageB_GM, m_kOpacityMapTargetB_GM, m_kOpacityMapB_GM, kTransfer );
        }
        */
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
        if ( kLUTa == null )
        {
            return;
        }
        this.UpdateImages( m_kColorMapTargetA, m_kColorMapA, kLUTa );
        /*
        if ( m_kImageB != null )
        {
            this.UpdateImages( m_kColorMapTargetB, m_kColorMapB, kLUTb );
        }
        */
        m_kLUT = kLUTa;
    }

    /**
     * Update the LUT texture sent to the GPU.
     * @param kColorTexture, the color-map Texture object.
     * @param kColorMap, the color-map GraphicsImage object (stores data).
     * @param kLUT, the new LUT.
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
        return m_kVolumeTargetA_GM;
    }
    public Texture GetOpacityMapGMTarget()
    {
        return m_kOpacityMapTargetA_GM;
    }
    

    /**
     * Return the Texture containing the imageA volume data.
     * @return Texture containing the imageA volume data.
     */
    public Texture GetVolumeTarget()
    {
        return m_kVolumeTargetA;
    }

    /**
     * Return the Texture containing the imageA volume data.
     * @return Texture containing the imageA volume data.
     */
    public Buffer GetVolumeTargetBuffer()
    {
        return m_kVolumeTargetA.GetImage().GetDataBuffer();
    }

    public Texture GetColorMapTarget()
    {
        return m_kColorMapTargetA;
    }

    public Texture GetOpacityMapTarget()
    {
        return m_kOpacityMapTargetA;
    }

    public Texture GetNormalMapTarget()
    {
        return m_kNormalMapTargetA;
    }

    /**
     * Release the Textures containing the imageA (imageB) volume data. Once
     * Textures are released, they will be re-loaded onto the GPU during the
     * next frame.
     */
    public void ReleaseVolume()
    {
        m_kVolumeTargetA.Release();
        /*
        if ( m_kImageB != null )
        {
            m_kVolumeTargetB.Release();
        }
        */
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
        /*
        else if ( m_kImageB != null )
        {
            SetRGBT( m_kColorMapTargetB, m_kColorMapB, kRGBT );
        }
        */
    }


    /**
     * Sets the Texture object containing the color lookup table based on the ModelRGB.
     * @param kTexture, the Texture object containing the colormap GraphicsImage.
     * @param kImage, the GraphicsImage containing the colormap data.
     * @param kRGBT, the new ModelRGB.
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
        return m_kImageA.isColorImage();
    }
    
    public ModelImage GetImage()
    {
        return m_kImageA;
    }
    
    public ModelLUT GetLUT()
    {
        return m_kLUT;
    }

}
