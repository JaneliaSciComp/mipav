package gov.nih.mipav.view;


import gov.nih.mipav.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.awt.image.*;
import java.io.*;

/* MipavCoordinateSystems upgrade */
/**
 * Renders Slice data in Patient Coordinates for the following classes:
 * ViewJComponentTriImage, ViewJComponentTriSliceImage, PlaneRender
 */
public class PatientSlice 
{
    private int timeSliceA = 0;
    private int timeSliceB = 0;
    private int slice = 0;
    private float[] imageBufferA;
    private float[] imageBufferB;
    private ModelImage imageA;
    private ModelImage imageB;
    private ModelLUT LUTa;
    private ModelLUT LUTb;
    private ModelRGB RGBTA;
    private ModelRGB RGBTB;

    /** This indicates which of the 3 tri-image components we are currently
     * in; either AXIAL, SAGITTAL, or CORONAL. */
    private int orientation;

    /** image extents in the local (Patient) coordinate system: */
    private int[] localImageExtents = new int[3];

    private Point3Df m_kFilePoint = new Point3Df();
    private Point3Df m_kPatientPoint = new Point3Df();

    private Point3Df[] m_kFourCorners = new Point3Df[4];
    private boolean m_bShowDiagonal = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     *
     * @param  _imageA                   Model of the image that will be displayed
     * @param  _LUTa                     LUT used to display imageA
     * @param  imgBufferA                storage buffer used to display imageA
     * @param  _imageB                   Model of the image that will be displayed
     * @param  _LUTb                     LUT used to display imageB
     * @param  imgBufferB                storage buffer used to display imageB
     * @param  _orientation  display orientation of the image
     */
    public PatientSlice(ModelImage _imageA, ModelLUT _LUTa, float[] imgBufferA,
                 ModelImage _imageB, ModelLUT _LUTb, float[] imgBufferB, int _orientation )
    {
        imageA = _imageA;
        imageB = _imageB;
        LUTa = _LUTa;
        LUTb = _LUTb;

        orientation = _orientation;
        localImageExtents = imageA.getExtents( orientation );
        setBuffers( imgBufferA, imgBufferB );
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
        slice = (int)m_kPatientPoint.z;
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
        if ( newSlice < 0 )
        {
            newSlice = 0;
        }
        if ( newSlice >= localImageExtents[2] )
        {
            newSlice = localImageExtents[2] - 1;
        }
        if ( newSlice != m_kPatientPoint.z )
        {
            m_kPatientPoint.z = newSlice;
            MipavCoordinateSystems.PatientToFile( m_kPatientPoint, m_kFilePoint, imageA, orientation );
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
     * Accessor that sets the LUT for image A.
     *
     * @param  LUT  The LUT.
     */
    public void setLUTa(ModelLUT LUT)
    {
        if ( LUT != null )
        {
            LUTa = LUT;
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


    /**
     * Causes the data to be redrawn with new RGBTA values:
     *
     * @param  RGBT  DOCUMENT ME!
     */
    public void setRGBTA(ModelRGB RGBT)
    {
        RGBTA = RGBT;
    }

    /**
     * Causes the data to be redrawn with new RGBTA values:
     *
     * @param  RGBT  DOCUMENT ME!
     */
    public void setRGBTB(ModelRGB RGBT)
    {
        RGBTB = RGBT;
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
    public int getOrientation() {
        return orientation;
    }

    /**
     * For generating the display of 1 or 2 RGB images.
     *
     * @param   tSlice     t (time) slice to show
     * @return  boolean to indicate if the show was successful
     */
    private boolean showUsingOrientation( int[] bufferA, int[] bufferB )
    {
        // Note that alphaBlending is applied with 1 component taken as zero if both components are not present -for
        // example, if either imageA or imageB but not both has red, then the red component is alphaBlended with zero.


        float redMapped, greenMapped, blueMapped;
        int[] RGBIndexBufferA = null;
        int[] RGBIndexBufferB = null;

        if (RGBTA != null) {
            RGBIndexBufferA = RGBTA.exportIndexedRGB();
        }

        if ((imageB != null) && (RGBTB != null)) {
            RGBIndexBufferB = RGBTB.exportIndexedRGB();
        }

        if ( imageBufferA == null )
        {
            imageBufferA = new float[ localImageExtents[0] * localImageExtents[1] * 4 ];
        }
        if ( (imageB != null) && (imageBufferB == null) )
        {
            imageBufferB = new float[ localImageExtents[0] * localImageExtents[1] * 4 ];
        }
        fillImageBuffer(slice);
    
        if (imageB == null) {

            for ( int j = 0; j < localImageExtents[1]; j++ )
            {
                for ( int i = 0; i < localImageExtents[0]; i++ )
                {
                    int ind4 = (j * localImageExtents[0]) + i;
                    int index = 4 * ind4;

                    if (RGBTA != null)
                    {
                        if (RGBTA.getROn()) {
                            redMapped = (RGBIndexBufferA[(int) imageBufferA[index + 1]] & 0x00ff0000) >> 16;
                        } else {
                            redMapped = 0;
                        }
                        if (RGBTA.getGOn()) {
                            greenMapped = (RGBIndexBufferA[(int) imageBufferA[index + 2]] & 0x0000ff00) >> 8;
                        } else {
                            greenMapped = 0;
                        }
                        if (RGBTA.getBOn()) {
                            blueMapped = (RGBIndexBufferA[(int) imageBufferA[index + 3]] & 0x000000ff);
                        } else {
                            blueMapped = 0;
                        }
                    } 
                    else {
                        redMapped = imageBufferA[index + 1];
                        greenMapped = imageBufferA[index + 2];
                        blueMapped = imageBufferA[index + 3];
                    }

                    int pixValue = 0xff000000 |
                        (((int) (redMapped) << 16) | (((int) (greenMapped) << 8) | ((int) (blueMapped))));
                    bufferA[ind4] = pixValue;
                } 
            } 
        } 
        else
        {
            for ( int j = 0; j < localImageExtents[1]; j++ )
            {
                for ( int i = 0; i < localImageExtents[0]; i++ )
                {
                    int ind4 = (j * localImageExtents[0]) + i;
                    int index = 4 * ind4;

                    int Ra = 0;
                    int Rb = 0;
                    int Ga = 0;
                    int Gb = 0;
                    int Ba = 0;
                    int Bb = 0;

                    if ((RGBTA != null) && (RGBTB != null)) {

                        if (RGBTA.getROn()) {
                            Ra = (RGBIndexBufferA[(int) imageBufferA[index + 1]] & 0x00ff0000) >> 16;
                        } 

                        if (RGBTA.getGOn()) {
                            Rb = (RGBIndexBufferB[(int) imageBufferB[index + 1]] & 0x00ff0000) >> 16;
                        }

                        if (RGBTA.getBOn()) {
                            Ga = (RGBIndexBufferA[(int) imageBufferA[index + 2]] & 0x0000ff00) >> 8;
                        } 

                        if (RGBTB.getROn()) {
                            Gb = (RGBIndexBufferB[(int) imageBufferB[index + 2]] & 0x0000ff00) >> 8;
                        }

                        if (RGBTB.getGOn()) {
                            Ba = (RGBIndexBufferA[(int) imageBufferA[index + 3]] & 0x000000ff);
                        }

                        if (RGBTB.getBOn()) {
                            Bb = (RGBIndexBufferB[(int) imageBufferB[index + 3]] & 0x000000ff);
                        } 
                    }
                    else {
                        Ra = (int) imageBufferA[index + 1];
                        Rb = (int) imageBufferB[index + 1];
                        Ga = (int) imageBufferA[index + 2];
                        Gb = (int) imageBufferB[index + 2];
                        Ba = (int) imageBufferA[index + 3];
                        Bb = (int) imageBufferB[index + 3];
                    }

                    int pixValueA = 0xff000000 | (Ra << 16) | (Ga << 8) | Ba;
                    int pixValueB = 0xff000000 | (Rb << 16) | (Gb << 8) | Bb;
                    bufferA[ind4] = pixValueA;
                    bufferB[ind4] = pixValueB;
                } 
            } 
        } 
        return true;
    }


    /**
     * For generating the display of 1 or 2 RGB images.
     *
     * @param   tSlice     t (time) slice to show
     *
     * @return  boolean to indicate if the show was successful
     */
    public boolean showUsingOrientation(int tSlice, int[] bufferA, int[] bufferB )
    {

        int lutHeightA = 0;
        float[][] RGB_LUTa = null, RGB_LUTb = null;
        int[][] iRGB_LUTa = null, iRGB_LUTb = null;
        int[] lutBufferRemapped;

        if (imageA.getNDims() < 4) {
            timeSliceA = 0;
        } else {
            timeSliceA = tSlice;
        }
        
        if ((imageB != null) && (imageB.getNDims() < 4)) {
            timeSliceB = 0;
        } else {
            timeSliceB = tSlice;
        }

        if (imageA.isColorImage() == true) {
            // call the show method for displaying RGB images
            return (showUsingOrientation( bufferA, bufferB ));
        }
        
        if ( (imageA == null) || (LUTa == null) )
        {
            return false;
        }
        
        lutHeightA = LUTa.getExtents()[1];
        lutBufferRemapped = new int[lutHeightA];
        
        if (imageB != null) {
            RGB_LUTa = LUTa.exportRGB_LUT(true);
            RGB_LUTb = LUTb.exportRGB_LUT(true);
            iRGB_LUTa = new int[3][RGB_LUTa[0].length];
            iRGB_LUTb = new int[3][RGB_LUTb[0].length];
            
            for (int c = 0; c < RGB_LUTa[0].length; c++) {
                iRGB_LUTa[0][c] = (int) (RGB_LUTa[0][c] + 0.5f);
                iRGB_LUTb[0][c] = (int) (RGB_LUTb[0][c] + 0.5f);
                iRGB_LUTa[1][c] = (int) (RGB_LUTa[1][c] + 0.5f);
                iRGB_LUTb[1][c] = (int) (RGB_LUTb[1][c] + 0.5f);
                iRGB_LUTa[2][c] = (int) (RGB_LUTa[2][c] + 0.5f);
                iRGB_LUTb[2][c] = (int) (RGB_LUTb[2][c] + 0.5f);
            }
        } else {
            LUTa.exportIndexedLUT(lutBufferRemapped);
        }

        if ( imageBufferA == null )
        {
            imageBufferA = new float[ localImageExtents[0] * localImageExtents[1] ];
        }
        if ( (imageB != null) && (imageBufferB == null) )
        {
            imageBufferB = new float[ localImageExtents[0] * localImageExtents[1] ];
        }
        fillImageBuffer(slice);

        if (imageB == null) {
            TransferFunction tf_imgA = LUTa.getTransferFunction();
            for ( int j = 0; j < localImageExtents[1]; j++ )
            {
                for ( int i = 0; i < localImageExtents[0]; i++ )
                {
                    int index = (j * localImageExtents[0]) + i;
                    int pixValueA = (int) (tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5f);
                    bufferA[index] = lutBufferRemapped[pixValueA];
                } 
            } 
        } 
        else if ( imageB != null )
        {
            TransferFunction tf_imgA = LUTa.getTransferFunction();
            TransferFunction tf_imgB = LUTb.getTransferFunction();
            
            for ( int j = 0; j < localImageExtents[1]; j++ )
            {
                for ( int i = 0; i < localImageExtents[0]; i++ )
                {
                    int index = (j * localImageExtents[0]) + i;
                    
                    int indexA = (int) (tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5f);
                    int indexB = (int) (tf_imgB.getRemappedValue(imageBufferB[index], 256) + 0.5f);
                    
                    int Ra = iRGB_LUTa[0][indexA];
                    int Ga = iRGB_LUTa[1][indexA];
                    int Ba = iRGB_LUTa[2][indexA];
                    
                    int pixValueA = 0xff000000 | (Ra << 16) | (Ga << 8) | Ba;
                    int pixValueB = (0xff000000) | ((int) (RGB_LUTb[0][indexB]) << 16) |
                        ((int) (RGB_LUTb[1][indexB]) << 8) |
                        (int) (RGB_LUTb[2][indexB]);
                    
                    bufferA[index] = pixValueA;
                    bufferB[index] = pixValueB;
                }
            } 
        }
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
            if ( m_bShowDiagonal )
            {
                imageA.exportDiagonal( orientation, timeSliceA, slice, localImageExtents, m_kFourCorners, imageBufferA );
                if (imageB != null)
                {
                    imageB.exportDiagonal( orientation, timeSliceB, slice, localImageExtents, m_kFourCorners, imageBufferB );
                }
            }
            else
            {
                imageA.export( orientation, timeSliceA, slice, imageBufferA );
                if (imageB != null)
                {
                    imageB.export( orientation, timeSliceB, slice, imageBufferB );
                }
            }
        }
        catch (IOException error) {
            MipavUtil.displayError("" + error);
            error.printStackTrace();
            return;
        }
    }
}
