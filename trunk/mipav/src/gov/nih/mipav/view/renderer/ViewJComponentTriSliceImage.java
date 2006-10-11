package gov.nih.mipav.view.renderer;


import gov.nih.mipav.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.surfaceview.*;

import java.awt.*;
import java.awt.image.*;

import java.io.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * Image plane displayed in the surface renderer. There are three image planes, the XY plane, the XZ plane, and the ZX
 * plane. This image component represents one of those. This is where the image data, pizel data, and paint buffer are
 * stored.
 *
 * @version  0.1 Nov 18, 1997
 * @author   Matthew J. McAuliffe, Ph.D. (primary)
 * @see      SurfaceRender
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

    /**
     * OPACITY - opacity value used by the paint brush. value = 1.0 - opaque
     * value = 0.25 - default (mostly see through)
     */
    private float OPACITY = 0.25f;

    /** Orientation of this component image. */
    private int orientation = 0;

    /** PatientSlice contains all the Patient Coordinate System view-specific
     * data for rendering this component: */
    private PatientSlice m_kPatientSlice;


    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new component image plane with the appropriate arrays.
     *
     * @param  _imageA       Model of the image that will be displayed.
     * @param  _imageB       Model of the image that will be displayed.
     * @param  _orientation  Orientation of the image.
     */
    public ViewJComponentTriSliceImage( SurfaceRender _frame, ModelImage _imageA, ModelImage _imageB, int _orientation )
    {
        frame = _frame;
        imageA = _imageA;
        imageB = _imageB;
        imageActive = imageA;

        orientation = _orientation;
        localImageExtents = imageActive.getExtents( orientation );

        img = new BufferedImage( localImageExtents[0], localImageExtents[1], BufferedImage.TYPE_INT_ARGB);

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


    /************************************************************************/
    /**
     * Accessors.
     *
     * @return  DOCUMENT ME!
     */
    /************************************************************************/

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
     * Accessor that returns the opacity of the paint.
     *
     * @return  Opacity of paint.
     */
    public float getOPACITY() {
        return OPACITY;
    }

    /**
     * Accessor that returns the component's orientation (i.e., XY, ZY, XZ, or NA).
     *
     * @return  Image orientation (i.e., XY, ZY, XZ, or NA).
     */
    public int getOrientation() {
        return orientation;
    }


    /**
     * Sets the member variable m_bInterpolate. When set to true, the showDiagonal function does tri-linear
     * interpolation of the ModelImage data:
     *
     * @param  bSample  DOCUMENT ME!
     */
    public void Interpolate(boolean bSample) {
        //m_bInterpolate = bSample;
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
     * Accessor that sets the orientation of the component (i.e., XY, ZY, XZ, or NA).
     *
     * @param  _orientation  Orientaiton of image slice to be displayed.
     */
    public void setOrientation(int _orientation) {
        orientation = _orientation;
    }

    // The following 2 functions set the RGB tables for ARGB images A and B.
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
     * @param i, FileCoordinates
     * @param j, FileCoordinates
     * @param k, FileCoordinates
     */
    public void setCenter( int i, int j, int k )
    {
        m_kPatientSlice.setCenter( i, j, k );
    }

    /**
     * Shows the image.
     *
     * @param   tSlice      t (time) slice to show
     * @param   zSlice      z slice to show
     * @param   _LUTa       LUTa - to change to new LUT for imageA else null.
     * @param   _LUTb       LUTb - to change to new LUT for imageB else null.
     * @param   forceShow   Forces this method to import image and recalculate java image.
     * @param   kTransform  DOCUMENT ME!
     * @param   akVertices  DOCUMENT ME!
     *
     * @return  Confirms if the show was successful.
     */
    public boolean show(int tSlice, int zSlice, ModelLUT _LUTa, ModelLUT _LUTb, boolean forceShow, Point3Df[] akVertices) {

        if ( imageA == null )
        {
            return false;
        }
        int buffFactor = imageA.isColorImage() ? 4 : 1;
        int[] imageBufferA = new int[ localImageExtents[0] * localImageExtents[1] * buffFactor ];

        m_kPatientSlice.setDiagonalVerts( akVertices );
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
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
    }
}
