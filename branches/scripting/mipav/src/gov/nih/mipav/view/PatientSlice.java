package gov.nih.mipav.view;


import gov.nih.mipav.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.awt.image.*;
import java.io.*;
import javax.vecmath.*;

/**
 * PatientSlice provides oriented access to the ModelImage data for
 * rendering. The ModelImage data is written into float[] arrays that are then
 * used as textures for displaying the data as texture-mapped polygons in the
 * PlaneRender and ViewJComponentTriSliceImage classes or as Renders Slice
 * data in Patient Coordinates for the following classes:
 * ViewJComponentTriImage, ViewJComponentTriSliceImage, PlaneRender
 */
public class PatientSlice 
{
    private int timeSliceA = 0;
    private int timeSliceB = 0;
    private int slice = 0;
    private float[] imageBufferA;
    private float[] imageBufferB;
    private ModelImage imageActive;
    private ModelImage imageA;
    private ModelImage imageB;
    private ModelLUT LUTa = null;
    private ModelLUT LUTb = null;
    private ModelRGB RGBTA = null;
    private ModelRGB RGBTB = null;
    private int[] m_aiRGBIndexBufferA = null;
    private int[] m_aiRGBIndexBufferB = null;

    private float[] m_afNormColor = { 1, 1 };
    private Color3f[] m_akOffset = { new Color3f( 0.0f, 0.0f, 0.0f ), 
                                     new Color3f( 0.0f, 0.0f, 0.0f ) };

    private boolean useBlueThreshold = false;
    private boolean useGreenThreshold = false;
    private boolean useRedThreshold = false;
    private float threshold1;
    private float threshold2;
    private boolean hasThreshold1 = false;
    private boolean hasThreshold2 = false;
    private ModelImage imageColocalize;
    private float[] imageBufferColocalize;


    /** This indicates which of the 3 tri-image components we are currently
     * in; either AXIAL, SAGITTAL, or CORONAL. */
    private int orientation;

    /** image extents in the local (Patient) coordinate system: */
    private int[] localImageExtents = new int[3];

    private Point3Df m_kFilePoint = new Point3Df();
    private Point3Df m_kPatientPoint = new Point3Df();

    private Point3Df[] m_kFourCorners = new Point3Df[4];
    private boolean m_bShowDiagonal = false;

    private boolean m_bUpdateImage = true;

    private float[] m_afMask = null;
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     *
     * @param  _imageA                   Model of the image that will be displayed
     * @param  _LUTa                     LUT used to display imageA
     * @param  _imageB                   Model of the image that will be displayed
     * @param  _LUTb                     LUT used to display imageB
     * @param  _orientation  display orientation of the image
     */
    public PatientSlice( ModelImage _imageA, ModelLUT _LUTa,
                         ModelImage _imageB, ModelLUT _LUTb,
                         int _orientation )
    {
        imageActive = _imageA;
        imageA = _imageA;
        imageB = _imageB;
        LUTa = _LUTa;
        LUTb = _LUTb;
        
        orientation = _orientation;
        localImageExtents = imageA.getExtents( orientation );
        if ( localImageExtents.length < 3 )
        {
            int[] temp = new int[3];
            for ( int i = 0; i < localImageExtents.length; i++ )
            {
                temp[i] = localImageExtents[i];
            }
            for ( int i = localImageExtents.length; i < 3; i++ )
            {
                temp[i] = 0;
            }
            localImageExtents = new int[3];
            for ( int i = 0; i < 3; i++ )
            {
                localImageExtents[i] = temp[i];
            }
        }
        center();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------


    /**
     * Sets all variables to null, disposes, and garbage collects (sometimes).
     *
     * @param  flag  if true garbage collector should be called.
     */
    public void disposeLocal(boolean flag)
    {
        m_kPatientPoint = null;
        m_kFourCorners = null;
    }

    /**
     * @param i, FileCoordinates
     * @param j, FileCoordinates
     * @param k, FileCoordinates
     */
    public void setCenter( int i, int j, int k )
    {
        m_kFilePoint.x = i;
        m_kFilePoint.y = j;
        m_kFilePoint.z = k;

        MipavCoordinateSystems.FileToPatient( m_kFilePoint, m_kPatientPoint,
                                              imageA, orientation );
        if ( slice == (int)m_kPatientPoint.z )
        {
            return;
        }
        slice = (int)m_kPatientPoint.z;
        m_bUpdateImage = true;
    }

    /**
     * Calculate the volume center in patient coordinates and set the z-value
     * for this slice
     */
    private void center()
    {
        m_kPatientPoint.x = localImageExtents[0]/2;
        m_kPatientPoint.y = localImageExtents[1]/2;
        m_kPatientPoint.z = localImageExtents[2]/2;
        slice = (int)m_kPatientPoint.z;
        MipavCoordinateSystems.PatientToFile( m_kPatientPoint, m_kFilePoint,
                                              imageA, orientation );
    }

    /**
     * Return the current center of the volume in File Coordinates
     * @return volume center in FileCoordinates
     */
    public Point3Df getCenter()
    {
        MipavCoordinateSystems.PatientToFile( m_kPatientPoint, m_kFilePoint,
                                              imageA, orientation );
        return new Point3Df( m_kFilePoint.x,
                             m_kFilePoint.y,
                             m_kFilePoint.z );
    }



    /**
     *  updates the slice value when the wheel is moved or the page_up,
     *  page_down keys are pressed. Does bounds checking and comparison with
     *  the current slice value. Sets the new position and updates the
     *  triImageFrame.
     * @param newSlice the new slice value
     */
    public boolean updateSlice( int newSlice )
    {
        if ( newSlice >= localImageExtents[2] )
        {
            newSlice = localImageExtents[2] - 1;
        }
        if ( newSlice < 0 )
        {
            newSlice = 0;
        }
        if ( newSlice != m_kPatientPoint.z )
        {
            m_kPatientPoint.z = newSlice;
            slice = newSlice;
            MipavCoordinateSystems.PatientToFile( m_kPatientPoint, m_kFilePoint, imageA, orientation );
            m_bUpdateImage = true;
            return true;
        }
        return false;
    }


    /**
     * Sets the four corners of the bounding box for this slice in
     * FileCoordinates so that the volume data can be exported along a
     * diagonal slice, based on the positions of the input bounding box.
     * @param fourCorners, the bounding box of the diagonal slice in
     * FileCoordinates
     */
    public void setDiagonalVerts( Point3Df[] fourCorners )
    {
        if ( fourCorners == null )
        {
            for ( int i = 0; i < 4; i++ )
            {
                m_kFourCorners[i] = null;
            }
            m_bShowDiagonal = false;
        }
        else
        {
            for ( int i = 0; i < 4; i++ )
            {
                if ( fourCorners[i] == null )
                {
                    m_kFourCorners[i] = null;
                    m_bShowDiagonal = false;
                }
                else
                {
                    if ( m_kFourCorners[i] != null )
                    {
                        if ( !m_kFourCorners[i].equals( fourCorners[i] ) )
                        {
                            m_bUpdateImage = true;
                        }
                    }
                    m_kFourCorners[i] = new Point3Df( fourCorners[i].x,
                                                      fourCorners[i].y,
                                                      fourCorners[i].z  );
                    m_bShowDiagonal = true;
                }
            }
        }
    }

    /**
     * Sets the export for this Slice to be along a diagonal.
     * @param bDiagonal, if true, render this slice as a diagonal
     */
    public void setShowDiagonal( boolean bDiagonal )
    {
        m_bShowDiagonal = bDiagonal;
    }

    /**
     * Sets the booleans for using thresholds in showUsingOrientation.
     *
     * @param  useRedThreshold    whether to threshold the red paint buffer
     * @param  useGreenThreshold  whether to threshold the green paint buffer
     * @param  useBlueThreshold   whether to threshold the blue paint buffer
     */
    public void setThresholdColors(boolean useRedThreshold, boolean useGreenThreshold, boolean useBlueThreshold) {
        this.useRedThreshold = useRedThreshold;
        this.useGreenThreshold = useGreenThreshold;
        this.useBlueThreshold = useBlueThreshold;
    }

    /**
     * Sets the thresholds.
     *
     * @param  threshold1  the first threshold
     * @param  threshold2  the second threshold
     */
    public void setThresholds(float threshold1, float threshold2) {
        this.threshold1 = threshold1;
        this.threshold2 = threshold2;
    }

    /**
     * Sets the hasThreshold1 for setPaintBuffers.
     *
     * @param  hasThreshold1  whether the paint buffer has a threshold1
     */
    public void setHasThreshold1(boolean hasThreshold1) {
        this.hasThreshold1 = hasThreshold1;
    }

    /**
     * Sets the hasThreshold2 for setPaintBuffers.
     *
     * @param  hasThreshold2  whether the paint buffer has a threshold2
     */
    public void setHasThreshold2(boolean hasThreshold2) {
        this.hasThreshold2 = hasThreshold2;
    }

    public void setActiveImage( ModelImage kImage )
    {
        this.imageActive = kImage;
    }

    /**
     * Sets imageA.
     *
     * @param  image  imageA
     */
    public void setImageA(ModelImage image)
    {
        this.imageA = image;
    }

    /**
     * Sets imageB.
     *
     * @param  image  imageB
     */
    public void setImageB(ModelImage image)
    {
        this.imageB = image;
    }

    /**
     * Sets the colocalize image.
     *
     * @param  imageColocalize  the colocalization image
     */
    public void setImageColocalize(ModelImage imageColocalize)
    {
        this.imageColocalize = imageColocalize;
    }

    /**
     * Accessor that sets the LUT for image A.
     *
     * @param  LUT  The LUT.
     */
    public void setLUTa(ModelLUT LUT)
    {
        if ( LUT != null )
        {
            LUTa = LUT;
            m_bUpdateImage = true;
        }
    }

    /**
     * Accessor that sets the LUT for image B.
     *
     * @param  LUT  The LUT.
     */
    public void setLUTb(ModelLUT LUT)
    {
        if ( LUT != null )
        {
            LUTb = LUT;
            m_bUpdateImage = true;
        }
    }

    /**
     * Accessor that returns the LUT for image A.
     *
     * @return  LUTa
     */
    public ModelLUT getLUTa()
    {
        return LUTa;
    }

    /**
     * Accessor that returns the LUT for image B.
     *
     * @return  LUTb
     */
    public ModelLUT getLUTb()
    {
        return LUTb;
    }

    public ModelStorageBase getActiveLookupTable()
    {
        if ( imageActive == imageA )
        {
            if ( imageA.isColorImage() )
            {
                return RGBTA;
            }
            return LUTa;
        }
        else if ( (imageB != null) && (imageB.isColorImage()) )
        {
            return RGBTB;
        }
        return LUTb;
    }

    /**
     * Accessor that returns the RGBT for image A.
     *
     * @return  RGBTA
     */
    public ModelRGB getRGBTa()
    {
        return RGBTA;
    }

    /**
     * Accessor that returns the RGBT for image B.
     *
     * @return  RGBTB
     */
    public ModelRGB getRGBTb()
    {
        return RGBTB;
    }


    /**
     * Causes the data to be redrawn with new RGBTA values:
     *
     * @param  RGBT  DOCUMENT ME!
     */
    public void setRGBTA(ModelRGB RGBT)
    {
        RGBTA = RGBT;
        if (RGBTA != null)
        {
            m_aiRGBIndexBufferA = RGBTA.exportIndexedRGB();
        }
        if ( imageA != null )
        {
            calcMaxNormColors( imageA, 0 );
        }
        m_bUpdateImage = true;
    }

    /**
     * Causes the data to be redrawn with new RGBTB values:
     *
     * @param  RGBT  DOCUMENT ME!
     */
    public void setRGBTB(ModelRGB RGBT)
    {
        RGBTB = RGBT;
        if ( RGBTB != null )
        {
            m_aiRGBIndexBufferB = RGBTB.exportIndexedRGB();
        }
        if ( imageB != null )
        {
            calcMaxNormColors( imageB, 1 );
        }
        m_bUpdateImage = true;
    }

    private void calcMaxNormColors( ModelImage kImage, int index )
    {
        float fMaxColor = 255;
        if (kImage.getType() == ModelStorageBase.ARGB_USHORT) {
            fMaxColor = (float) kImage.getMaxR();
            fMaxColor = Math.max((float) kImage.getMaxG(), fMaxColor);
            fMaxColor = Math.max((float) kImage.getMaxB(), fMaxColor);
        } else if (kImage.getType() == ModelStorageBase.ARGB_FLOAT) {

            if (kImage.getMinR() < 0.0) {
                fMaxColor = (float) (kImage.getMaxR() - kImage.getMinR());
                m_akOffset[index].x = (float) (-kImage.getMinR());
            } else {
                fMaxColor = (float) kImage.getMaxR();
            }

            if (kImage.getMinG() < 0.0) {
                fMaxColor = Math.max((float) (kImage.getMaxG() - kImage.getMinG()), fMaxColor);
                m_akOffset[index].y= (float) (-kImage.getMinG());
            } else {
                fMaxColor = Math.max((float) kImage.getMaxG(), fMaxColor);
            }

            if (kImage.getMinB() < 0.0) {
                fMaxColor = Math.max((float) (kImage.getMaxB() - kImage.getMinB()), fMaxColor);
                m_akOffset[index].z = (float) (-kImage.getMinB());
            } else {
                fMaxColor = Math.max((float) kImage.getMaxB(), fMaxColor);
            }
        }

        m_afNormColor[index] = 255 / fMaxColor;

    }


    /**
     * The frame in which the image(s) is displayed, allocates the memory and
     * uses this method to pass the references to the buffers.
     *
     * @param  imgBufferA  storage buffer used to display image A
     * @param  imgBufferB  storage buffer used to display image B
     */
    public void setBuffers(float[] imgBufferA, float[] imgBufferB) {
        imageBufferA = imgBufferA;
        imageBufferB = imgBufferB;
    }


    /**
     * This indicates which of the 3 tri-image components we are currently in;
     * either AXIAL, SAGITTAL, or CORONAL.
     *
     * @return  The orientation, either AXIAL, SAGITTAL, or CORONAL.
     */
    public int getOrientation()
    {
        return orientation;
    }

    /**
     * For generating the display of 1 or 2 RGB images.
     *
     * @param   tSlice     t (time) slice to show
     *
     * @return  boolean to indicate if the show was successful
     */
    public boolean showUsingOrientation(int tSlice, int[] bufferA, int[] bufferB, boolean forceShow,
                                        boolean bBlend, float fAlpha, boolean bShowMask )
    {
        if ( !m_bUpdateImage && !forceShow || (imageA == null) )
        {
            return false;
        }
        //System.err.println( "PatientSlice: " + orientation + " " + slice + " " + m_bUpdateImage + " " + forceShow + " " + bBlend);

        timeSliceA = (imageA.getNDims() < 4) ? 0 : tSlice;
        if ( imageB != null )
        {
            timeSliceB = (imageB.getNDims() < 4) ? 0 : tSlice;
        }

        if (imageA.isColorImage() == true)
        {
            Color4f colorMappedA = new Color4f();
            Color4f colorMappedB = null;
            
            if (imageB != null)
            {
                colorMappedB = new Color4f();
            }
            
            fillImageBuffer(slice);
            
            for ( int j = 0; j < localImageExtents[1]; j++ )
            {
                for ( int i = 0; i < localImageExtents[0]; i++ )
                {
                    int ind4 = (j * localImageExtents[0]) + i;
                    int index = 4 * ind4;
                    int pixValue;                    
                    getColorMapped( RGBTA, m_aiRGBIndexBufferA, 0, imageBufferA, index, colorMappedA );
                    /* Get imageB color and blend if necessary: */
                    if ( imageB != null )
                    {
                        getColorMapped( RGBTB, m_aiRGBIndexBufferB, 1, imageBufferB, index, colorMappedB );
                        if ( bBlend )
                        {
                            colorMappedA.x = fAlpha * colorMappedA.x + (1.0f - fAlpha) * colorMappedB.x;
                            colorMappedA.y = fAlpha * colorMappedA.y + (1.0f - fAlpha) * colorMappedB.y;
                            colorMappedA.z = fAlpha * colorMappedA.z + (1.0f - fAlpha) * colorMappedB.z;
                        }
                        else
                        {
                            pixValue = 0xff000000 |
                                ((int) (colorMappedB.x) << 16) |
                                ((int) (colorMappedB.y) <<  8) |
                                ((int) (colorMappedB.z) );
                            bufferB[ind4] = pixValue;
                        }
                    }
                    /* Blend in the surface mask if necessary: */
                    if ( bShowMask && (m_afMask != null) )
                    {
                        if ( (m_afMask[index] != 0) &&
                             ((m_afMask[index + 1] != 0) ||
                              (m_afMask[index + 2] != 0) ||
                              (m_afMask[index + 3] != 0)) )
                        {
                            colorMappedA.x = m_afMask[index] * m_afMask[index + 1] + (1.0f - m_afMask[index]) * colorMappedA.x;
                            colorMappedA.y = m_afMask[index] * m_afMask[index + 2] + (1.0f - m_afMask[index]) * colorMappedA.y;
                            colorMappedA.z = m_afMask[index] * m_afMask[index + 3] + (1.0f - m_afMask[index]) * colorMappedA.z;
                        }
                    }
                    pixValue = 0xff000000 |
                        ((int) (colorMappedA.x) << 16) |
                        ((int) (colorMappedA.y) <<  8) |
                        ((int) (colorMappedA.z) );
                    bufferA[ind4] = pixValue;
                } 
            } 
        }
        else if ( LUTa != null )
        {
            float[][] RGB_LUTa = null;
            float[][] RGB_LUTb = null;
            int[] lutBufferRemapped = null;;
            
            TransferFunction tf_imgA = LUTa.getTransferFunction();
            TransferFunction tf_imgB = null;

            if (imageB != null)
            {
                RGB_LUTa = LUTa.exportRGB_LUT(true);
                RGB_LUTb = LUTb.exportRGB_LUT(true);
                tf_imgB = LUTb.getTransferFunction();
            }
            else
            {
                int lutHeightA = LUTa.getExtents()[1];
                lutBufferRemapped = new int[lutHeightA];
                LUTa.exportIndexedLUT(lutBufferRemapped);
            }
            
            fillImageBuffer(slice);

            float imageMinA = (float)Math.min( 0, imageA.getMin() );

            for ( int j = 0; j < localImageExtents[1]; j++ )
            {
                for ( int i = 0; i < localImageExtents[0]; i++ )
                {
                    int ind4 = (j * localImageExtents[0]) + i;
                    int index = 4 * ind4;

                    if ( hasThreshold1 )
                    {
                        if ((imageBufferA[i] < threshold1) ||
                            (imageBufferColocalize[i] < threshold2))
                        {
                            imageBufferA[i] = imageMinA;
                        }
                    }
                    else if (hasThreshold2)
                    {
                        if ((imageBufferColocalize[i] < threshold1) ||
                            (imageBufferA[i] < threshold2)) {
                            imageBufferA[i] = imageMinA;
                        }
                    }

                    if (imageB == null) {
                        int pixValueA = (int) (tf_imgA.getRemappedValue(imageBufferA[ind4], 256) + 0.5f);
                        bufferA[ind4] = lutBufferRemapped[pixValueA];
                    } 
                    else 
                    {
                        int indexA = (int) (tf_imgA.getRemappedValue(imageBufferA[ind4], 256) + 0.5f);
                        int indexB = (int) (tf_imgB.getRemappedValue(imageBufferB[ind4], 256) + 0.5f);
                        
                        float Ra = RGB_LUTa[0][indexA];
                        float Ga = RGB_LUTa[1][indexA];
                        float Ba = RGB_LUTa[2][indexA];
                        float Rb = RGB_LUTb[0][indexB];
                        float Gb = RGB_LUTb[1][indexB];
                        float Bb = RGB_LUTb[2][indexB];
                        
                        if ( bBlend )
                        {
                            Ra = fAlpha * Ra + (1.0f - fAlpha) * Rb;
                            Ga = fAlpha * Ga + (1.0f - fAlpha) * Gb;
                            Ba = fAlpha * Ba + (1.0f - fAlpha) * Bb;
                        }
                        else
                        {
                            int pixValueB = 0xff000000 | (((int)Rb) << 16) | (((int)Gb) << 8) | ((int)Bb);
                            bufferB[ind4] = pixValueB;
                        }
                        int pixValueA = 0xff000000 | (((int)Ra) << 16) | (((int)Ga) << 8) | ((int)Ba);
                        bufferA[ind4] = pixValueA;
                    }
            
                    /* Blend in mask: */
                    if ( bShowMask && (m_afMask != null) )
                    {
                        if ( (m_afMask[index] != 0) &&
                             ((m_afMask[index + 1] != 0) ||
                              (m_afMask[index + 2] != 0) ||
                              (m_afMask[index + 3] != 0)) )
                        {
                            float alpha = m_afMask[index];
                            int iMaskRa = (int)(alpha * m_afMask[index + 1]);
                            int iMaskGa = (int)(alpha * m_afMask[index + 2]);
                            int iMaskBa = (int)(alpha * m_afMask[index + 3]);
                            
                            int iRa = (int)((1 - alpha)*((bufferA[ind4] & 0x00ff0000) >> 16));
                            int iGa = (int)((1 - alpha)*((bufferA[ind4] & 0x0000ff00) >>  8));
                            int iBa = (int)((1 - alpha)*((bufferA[ind4] & 0x000000ff)));
                            
                            int pixValue = 0xff000000 | ((iMaskRa + iRa) << 16) | ((iMaskGa + iGa) << 8) | (iMaskBa + iBa);
                            bufferA[ind4] = pixValue;
                        }
                    }
                }
            }
        }
        else
        {
            return false;
        }
        m_bUpdateImage = false;
        return true;
    }


    /**
     * Clean up memory used by the component.
     *
     * @throws  Throwable  if there is a problem encountered during memory clean-up
     *
     * @see     #disposeLocal(boolean)
     */
    public void finalize() throws Throwable
    {
        disposeLocal(true);
    }

    /**
     * Returns the extents of the tri planar component (in the component's order, not the image volume's).
     *
     * @return  the extents of the tri image component
     */
    public int[] getExtents()
    {
        return localImageExtents;
    }



    /* MipavCoordinateSystems upgrade: TODO: */
    /**
     * Gets the image data based on the orientation
     *
     * @param  slice  data slize
     */
    private void fillImageBuffer(int slice)
    {
        try {
            int buffFactor = imageA.isColorImage() ? 4 : 1;
            if ( imageBufferA == null )
            {
                imageBufferA = new float[ localImageExtents[0] * localImageExtents[1] * buffFactor ];
            }
            if ( (imageB != null) && (imageBufferB == null) )
            {
                buffFactor = imageB.isColorImage() ? 4 : 1;
                imageBufferB = new float[ localImageExtents[0] * localImageExtents[1] * buffFactor ];
            }
            if ( (imageColocalize != null) && (imageBufferColocalize == null) )
            {
                buffFactor = imageColocalize.isColorImage() ? 4 : 1;
                imageBufferColocalize = new float[ localImageExtents[0] * localImageExtents[1] * buffFactor ];
            }

            if ( m_bShowDiagonal )
            {
                imageA.exportDiagonal( orientation, timeSliceA, slice, localImageExtents, m_kFourCorners, imageBufferA );
                if (imageB != null)
                {
                    imageB.exportDiagonal( orientation, timeSliceB, slice, localImageExtents, m_kFourCorners, imageBufferB );
                }
                if ( (imageColocalize != null) && (hasThreshold1 || hasThreshold2) )
                {
                    imageColocalize.exportDiagonal( orientation, timeSliceA, slice,
                                                    localImageExtents, m_kFourCorners,
                                                    imageBufferColocalize );
                }
            }
            else
            {
                m_afMask = imageA.export( orientation, timeSliceA, slice, imageBufferA );
                if (imageB != null)
                {
                    imageB.export( orientation, timeSliceB, slice, imageBufferB );
                }
                if ( (imageColocalize != null) && (hasThreshold1 || hasThreshold2) )
                {
                    imageColocalize.export( orientation, timeSliceA, slice,
                                            imageBufferColocalize );
                }
            }
        }
        catch (IOException error) {
            MipavUtil.displayError("" + error);
            error.printStackTrace();
            return;
        }
    }
    
    /** Get the color from the RGB lookup table: */
    private void getColorMapped( ModelRGB modelRGBTA, int[] RGBIndexBuffer, int imageIndex,
                                 float[] imageBuffer, int index, Color4f colorMapped )
    {

        colorMapped.x = 0;
        colorMapped.y = 0;
        colorMapped.z = 0;
        colorMapped.w = imageBuffer[index];
        if ( modelRGBTA != null )
        {
            if ( modelRGBTA.getROn() )
            {
                colorMapped.x = (RGBIndexBuffer[(int)((imageBuffer[index + 1] + m_akOffset[imageIndex].x) *
                                                m_afNormColor[imageIndex])] & 0x00ff0000) >> 16;
            }
            if ( modelRGBTA.getGOn() )
            {
                colorMapped.y = (RGBIndexBuffer[(int)((imageBuffer[index + 2] + m_akOffset[imageIndex].y) *
                                                m_afNormColor[imageIndex])] &  0x0000ff00) >> 8;
            }
            if ( modelRGBTA.getBOn() )
            {
                colorMapped.z = (RGBIndexBuffer[(int)((imageBuffer[index + 3] + m_akOffset[imageIndex].z) *
                                                m_afNormColor[imageIndex])] & 0x000000ff);
            }
        } 
        else
        {
            colorMapped.x = (imageBuffer[index + 1] + m_akOffset[imageIndex].x) * m_afNormColor[imageIndex];
            colorMapped.y = (imageBuffer[index + 2] + m_akOffset[imageIndex].y) * m_afNormColor[imageIndex];
            colorMapped.z = (imageBuffer[index + 3] + m_akOffset[imageIndex].z) * m_afNormColor[imageIndex];
        }
        /* Threshold colors: */
        if (useRedThreshold && useGreenThreshold)
        {
            if ((imageBuffer[index + 1] < threshold1) ||
                (imageBuffer[index + 2] < threshold2))
            {
                colorMapped.x = 0;
                colorMapped.y = 0;
                colorMapped.z = 0;
            }
        }
        else if (useRedThreshold && useBlueThreshold)
        {
            if ((imageBuffer[index + 1] < threshold1) ||
                (imageBuffer[index + 3] < threshold2))
            {
                colorMapped.x = 0;
                colorMapped.y = 0;
                colorMapped.z = 0;
            }
        }
        else if (useGreenThreshold && useBlueThreshold)
        {
            if ((imageBuffer[index + 2] < threshold1) ||
                (imageBuffer[index + 3] < threshold2)) 
            {
                colorMapped.x = 0;
                colorMapped.y = 0;
                colorMapped.z = 0;
            }
        }
    }

}
