package gov.nih.mipav.view.renderer.J3D;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.*;

import java.awt.image.*;

import javax.vecmath.*;


/**
 * Image plane displayed in the surface renderer.  The image plane is
 * initialized with one of the following orientations: FileInfoBase.AXIAL,
 * FileInfoBase.CORONAL, FileInfoBase.SAGITTAL.
 *
 * ViewJComponentTriSliceImage contains oriented 2D slices of the ModelImage
 * data, reconfigured to the axial, sagittal, and coronal coordinates. It
 * passes the oriented 2D ModelImage slice buffer to the SurfaceRender object
 * where it is used as a Texture2D object on a texture-mapped polygon.
 *
 * @version  0.1 Nov 18, 1997
 * @author   Matthew J. McAuliffe, Ph.D. (primary)
 * @see      SurfaceRender
 * @see PatientSlice.java
 */
public class ViewJComponentTriSliceImage {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** AlphaBlending values for compositing two images. */
    private float alphaBlend = 0.5f;

    /** Frame where the component image is displayed. */
    private SurfaceRender frame;

    /** Model for image A. */
    private ModelImage imageA;

    /** Model for active image. */
    private ModelImage imageActive = null;

    /** Model for image A. */
    private ModelImage imageB;

    /** Extents of the 3D image. */
    private int[] localImageExtents;

    /** BufferedImage to hold the slice image. */
    private BufferedImage img = null;

    /** Orientation of this component image. */
    private int orientation = 0;

    /** PatientSlice contains all the Patient Coordinate System view-specific
     * data for rendering this component: */
    private PatientSlice m_kPatientSlice;


    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new component image plane with the appropriate arrays.
     *
     * @param  _frame        The SurfaceRender parent frame
     * @param  _imageA       Model of the image that will be displayed.
     * @param  _imageB       Model of the image that will be displayed.
     * @param  _orientation  Orientation of the image.
     */
    public ViewJComponentTriSliceImage( SurfaceRender _frame,
                                        ModelImage _imageA, ModelImage _imageB,
                                        int _orientation )
    {
        frame = _frame;
        imageA = _imageA;
        imageB = _imageB;
        imageActive = imageA;

        orientation = _orientation;
        localImageExtents = imageActive.getExtents( orientation );

        img = new BufferedImage( localImageExtents[0],
                                 localImageExtents[1],
                                 BufferedImage.TYPE_INT_ARGB);

        m_kPatientSlice = new PatientSlice( imageA, null,
                                            imageB, null,
                                            orientation );
        m_kPatientSlice.setShowDiagonal( true );

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Sets all variables to null, disposes, and garbage collects.
     */
    public void disposeLocal()
    {
        imageA = null;
        imageB = null;
        localImageExtents = null;
        m_kPatientSlice.disposeLocal();
        m_kPatientSlice = null;
    }

    /**
     * Accessor that returns the alphablend of the two image.
     *
     * @return  Opacity of paint.
     */
    public float getAlphaBlend() {
        return alphaBlend;
    }

    /**
     * Get the buffered image.
     *
     * @return  BufferedImage Buffered image.
     */
    public BufferedImage getImage() {
        return img;
    }

    /**
     * Accessor that returns the image A.
     *
     * @return  Image A.
     */
    public ModelImage getImageA() {
        return imageA;
    }

    /**
     * Accessor that returns the image B.
     *
     * @return  Image B.
     */
    public ModelImage getImageB() {
        return imageB;
    }

    /**
     * Accessor that returns the component's orientation either
     * FileInfoBase.AXIAL, FileInfoBase.CORONAL, FileInfoBase.SAGITTAL, or
     * FileInfoBase.UNKNOWN_ORIENT
     *
     * @return  Image orientation (AXIAL, CORONAL, SAGITTAL)
     */
    public int getOrientation() {
        return orientation;
    }


    /**
     * When set to true, the m_kPatientSlice.showDiagonal function does
     * tri-linear interpolation of the ModelImage data:
     *
     * @param  bSample  when true interpolate the ModelImage data
     */
    public void Interpolate(boolean bSample) {
        m_kPatientSlice.setInterpolate( bSample );
    }

    /**
     * Sets the alpha blending of parameter for two image displaying.
     *
     * @param  value  Amount [0,100] that is the percentage of Image A to be displayed.
     */
    public void setAlphaBlend(int value) {
        alphaBlend = value / 100.0f;
    }

    /**
     * Sets the buffers for the actual data, the displayable image, and the paint.
     *
     * @param  imgBufferA  Storage buffer used to display image A.
     * @param  imgBufferB  Storage buffer used to display image B.
     */
    public void setBuffers(float[] imgBufferA, float[] imgBufferB)
    {
        m_kPatientSlice.setBuffers( imgBufferA, imgBufferB );
    }

    /**
     * Sets component's Image A.
     *
     * @param  image  The component's image A.
     */
    public void setImageA(ModelImage image) {
        imageA = image;
    }

    /**
     * Sets component's Image B.
     *
     * @param  image  The component's image B.
     */
    public void setImageB(ModelImage image) {
        imageB = image;
    }

    /**
     * Sets component's Image B data buffer.
     *
     * @param  buffer  The component's image B data buffer.
     */
    public void setImageBufferB(float[] buffer) {
        //imageBufferB = buffer;
    }

    /**
     * Accessor that sets the model lut for the image A.
     *
     * @param  LUT  The model LUT for image A.
     */
    public void setLUTa(ModelLUT LUT) {
        m_kPatientSlice.setLUTa( LUT );
    }

    /**
     * Accessor that sets the model lut for the image B.
     *
     * @param  LUT  The model LUT for image B.
     */
    public void setLUTb(ModelLUT LUT) {
        m_kPatientSlice.setLUTb( LUT );
    }

    /**
     * Accessor that sets the orientation of the component either
     * FileInfoBase.AXIAL, FileInfoBase.CORONAL, FileInfoBase.SAGITTAL, or
     * FileInfoBase.UNKNOWN_ORIENT
     *
     * @param  _orientation  Orientaiton of image slice to be displayed.
     */
    public void setOrientation(int _orientation) {
        orientation = _orientation;
    }

    /**
     * Sets the RGB table for ARGB image A.
     *
     * @param  RGBT  RGB table.
     */
    public void setRGBTA(ModelRGB RGBT) {
        m_kPatientSlice.setRGBTA( RGBT );
    }

    /**
     * Sets the RGB table for ARGB image B.
     *
     * @param  RGBT  RGB table.
     */
    public void setRGBTB(ModelRGB RGBT) {
        m_kPatientSlice.setRGBTB( RGBT );
    }

    /**
     * Accessor that sets the slice of the image component.
     *
     * @param  _slice  Image slice to be displayed.
     */
    public void setSlice(int _slice) {
        m_kPatientSlice.updateSlice( _slice );
    }

    /**
     * setCenter sets the PatientSlice center:
     * @param i FileCoordinates
     * @param j FileCoordinates
     * @param k FileCoordinates
     */
    public void setCenter( int i, int j, int k )
    {
        m_kPatientSlice.setCenter( i, j, k );
    }

    /**
     * Shows the image.
     *
     * @param tSlice t (time) slice to show
     * @param zSlice z slice to show
     * @param _LUTa LUTa - to change to new LUT for imageA else null.
     * @param _LUTb LUTb - to change to new LUT for imageB else null.
     * @param forceShow Forces this method to import image and recalculate
     * java image.
     * @param akVertices four corners of the cut-plane bounding box for
     * diagonal slices
     *
     * @return Confirms if the show updated the image
     */
    public boolean show(int tSlice, int zSlice,
                        ModelLUT _LUTa, ModelLUT _LUTb,
                        boolean forceShow, Vector3f[] akVertices)
    {

        if ( imageA == null )
        {
            return false;
        }
        int buffFactor = imageA.isColorImage() ? 4 : 1;
        int[] imageBufferA =
            new int[ localImageExtents[0] * localImageExtents[1] * buffFactor ];

        WildMagic.LibFoundation.Mathematics.Vector3f[] akDiagonalVertices = new WildMagic.LibFoundation.Mathematics.Vector3f[akVertices.length];
        for ( int i = 0; i < akVertices.length; i++ )
        {
        	akDiagonalVertices[i] = new WildMagic.LibFoundation.Mathematics.Vector3f( akVertices[i].x,
        			akVertices[i].y, akVertices[i].z );
        }
        m_kPatientSlice.setDiagonalVerts( akDiagonalVertices );
        m_kPatientSlice.setLUTa( _LUTa );
        m_kPatientSlice.setLUTb( _LUTb );

        if (frame.getVolOpacityPanel() != null) {
            alphaBlend = (100.0f - (float) frame.getVolOpacityPanel().getAlphaBlendSliderValue()) / 100.0f;
        }

        if ( m_kPatientSlice.showUsingOrientation( tSlice, imageBufferA, null, forceShow, true, alphaBlend, false ) )
        {
            img.setRGB( 0, 0, localImageExtents[0], localImageExtents[1], imageBufferA, 0, localImageExtents[0] );
            return true;
        }
        return false;
    }


    /**
     * Calls garbage collector to release system resources.
     *
     * @throws  Throwable  if there is a problem encountered during memory clean-up
     */
    protected void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
    }

    /**
     * Returns whether the slice is axis-aligned or rotated.
     * @return true when the slice is axis-aligned, false otherwise.
     */
    public boolean getAxisAligned()
    {
        return m_kPatientSlice.getAxisAligned();
    }

}
