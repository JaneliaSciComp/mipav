package gov.nih.mipav.view.renderer;

import gov.nih.mipav.view.renderer.surfaceview.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import java.io.*;

import javax.media.j3d.*;
import javax.vecmath.*;
import java.awt.*;
import java.awt.image.*;

/**
 *		Image plane displayed in the surface renderer.  There are three image planes,
 *		the XY plane, the XZ plane, and the ZX plane.  This image component represents one
 *		of those.  This is where the image data, pizel data, and paint buffer are stored.
 *
 *		@version    0.1 Nov 18, 1997
 *		@author     Matthew J. McAuliffe, Ph.D. (primary)
 *		@see		SurfaceRender
 *
 */
public class ViewJComponentTriSliceImage {

    /**
     *   OPACITY - opacity value used by the paint brush.
     *             value = 1.0   - opaque
     *             value = 0.25  - default (mostly see through)
     */
    private float OPACITY = 0.25f;

    /** Frame where the component image is displayed. */
    private SurfaceRender frame;

    /** Model for image A. */
    private ModelImage imageA;

    /** Model for image A. */
    private ModelImage imageB;

    /** Lookup table for image A. */
    private ModelLUT LUTa;

    /** Lookup table for image A. */
    private ModelLUT LUTb;

    /** Buffer holding remapped LUT. */
    private int lutBufferRemapped[] = null;

    /** Model for active image. */
    private ModelImage imageActive = null;

    /** Buffer holding image data for image A. */
    private float imageBufferA[] = null;

    /** Buffer holding image data for image B. */
    private float imageBufferB[] = null;

    /** Buffer holding image data for active image. */
    private float imageBufferActive[] = null;

    /** Current slice being displayed. */
    private int slice = -99;

    /** Current time slice being displayed. */
    private int timeSlice = 0;

    /** Time slice for image A. */
    private int timeSliceA = 0;

    /** Time slice for image B. */
    private int timeSliceB = 0;

    /** AlphaBlending values for compositing two images. */
    private float alphaBlend = 0.5f;

    /** AlphaBlending values for compositing two images. */
    private float alphaPrime = 0.5f;

    /** RGB table for image A. */
    private ModelRGB RGBTA;

    /** RGB table for image A. */
    private ModelRGB RGBTB;

    /** Orientation of this component image. */
    private int orientation = NA;

    /** Extents of the 3D image. */
    private int imageExtents[];

    /** Resolutions of the image plane (2D). */
    private float res[] = new float[2];

    /** Real resolutions of the image. */
    private float resX, resY, resZ;

    /** Sampling for the showDiagonal function: nearest-neighbor or interpolate: */
    private boolean m_bInterpolate = false;

    /** BufferedImage to hold the slice image. */
    private BufferedImage  img = null;

    /** Image dimension, XY, XZ, ZY. */
    private   Dimension         imageDim = null;

    /** Orientation static values. */
    public static final int XY = 0;
    public static final int ZY = 1;
    public static final int XZ = 2;
    public static final int NA = 3;



    /**
     *   Constructs new component image plane with the appropriate arrays.
     *   @param frame        Frame where image(s) will be displayed.
     *   @param _imageA      Model of the image that will be displayed.
     *   @param _LUTa        LUT used to display imageA.
     *   @param imgBufferA   Storage buffer used to display image A.
     *   @param _imageB      Model of the image that will be displayed.
     *   @param _LUTb        LUT used to display imageB.
     *   @param imgBufferB   Storage buffer used to display image B.
     *   @param extents      Initial display dimensions of the image.
     *   @param _orientation  Orientation of the image.
     */
    public ViewJComponentTriSliceImage(SurfaceRender _frame,
                                      ModelImage _imageA, ModelLUT _LUTa, float imgBufferA[],
                                      ModelImage _imageB, ModelLUT _LUTb, float imgBufferB[],
                                      int extents[], int _orientation) {
        frame = _frame;
        imageA = _imageA;
        imageB = _imageB;
        imageActive = imageA;
        imageExtents = extents;

        orientation = _orientation;

        if (orientation == NA || orientation == XY) {
            imageDim = new Dimension(extents[0],extents[1]);
        }
        else if (orientation == XZ) {
            imageDim = new Dimension(extents[0],extents[2]);
        }
        else { // orientation == ZY
            imageDim = new Dimension(extents[2],extents[1]);
        }
        img = new BufferedImage(imageDim.width, imageDim.height, BufferedImage.TYPE_INT_ARGB);


        LUTa = _LUTa;
        LUTb = _LUTb;
        lutBufferRemapped = new int[1];

        imageBufferA = imgBufferA;
        imageBufferB = imgBufferB;

        imageBufferActive = imageBufferA;

        resX = imageA.getFileInfo(0).getResolutions()[0];
        resY = imageA.getFileInfo(0).getResolutions()[1];
        if (imageA.getNDims() >= 3) {
            resZ = imageA.getFileInfo(0).getResolutions()[2];
        }
        if ( (orientation == NA) || (orientation == XY)) {
            if ( (resX <= 0.0f) || (resY <= 0.0f)) {
                resX = 1.0f;
                resY = 1.0f;
            }
            res[0] = resX;
            res[1] = resY;
        } // end of if ((orientation == NA) || (orientation == XY))
        else if (orientation == XZ) {
            if ( (resX <= 0.0f) || (resZ <= 0.0f)) {
                resX = 1.0f;
                resZ = 1.0f;
            }
            res[0] = resX;
            res[1] = resZ;
        } // end of else if (orientation == XZ)
        else { // orientation == ZY
            if ( (resZ <= 0.0f) || (resY <= 0.0f)) {
                resZ = 1.0f;
                resY = 1.0f;
            }
            res[0] = resZ;
            res[1] = resY;
        } // end of else for orientation == ZY
    }

    /************************************************************************/
    /******************************* Accessors ******************************/
    /************************************************************************/

    /**
     * Get the buffered image
     * @return BufferedImage  Buffered image.
     */
    public BufferedImage getImage() {
        return img;
    }

    /**
     *  Accessor that returns the model lut for the image A.
     *  @return The model LUT for image A.
     */
    public ModelLUT getLUTa() {
        return LUTa;
    }

    /**
     *  Accessor that returns the model lut for the image B.
     *  @return The model LUT for image B.
     */
    public ModelLUT getLUTb() {
        return LUTb;
    }

    /**
     *  Accessor that sets the model lut for the image A.
     *  @param lut The model LUT for image A.
     */
    public void setLUTa(ModelLUT LUT) {
        LUTa = LUT;
    }

    /**
     *  Accessor that sets the model lut for the image B.
     *  @param lut The model LUT for image B.
     */
    public void setLUTb(ModelLUT LUT) {
        LUTb = LUT;
    }

    /**
     *  Accessor that returns the RGB table for image A.
     *  @return The RGB table.
     */
    public ModelRGB getRGBTA() {
        return RGBTA;
    }

    /**
     *  Accessor that returns the RGB table for image B.
     *  @return The RGB table.
     */
    public ModelRGB getRGBTB() {
        return RGBTB;
    }

    /**
     *  Accessor that returns the opacity of the paint.
     *  @return Opacity of paint.
     */
    public float getOPACITY() {
        return OPACITY;
    }

    /**
     *  Accessor that returns the alphablend of the two image.
     *  @return Opacity of paint.
     */
    public float getAlphaBlend() {
        return alphaBlend;
    }

    /**
     *  Accessor that returns the component's orientation (i.e., XY, ZY, XZ, or NA).
     *  @return Image orientation (i.e., XY, ZY, XZ, or NA).
     */
    public int getOrientation() {
        return orientation;
    }

    /**
     *  Accessor that sets the orientation of the component (i.e., XY, ZY, XZ, or NA).
     *  @param  _orientation  Orientaiton of image slice to be displayed.
     */
    public void setOrientation(int _orientation) {
        orientation = _orientation;
    }

    /**
     *  Accessor that returns the image A.
     *  @return Image A.
     */
    public ModelImage getImageA() {
        return imageA;
    }

    /**
     * Accessor that returns the image B.
     * @return Image B.
     */
    public ModelImage getImageB() {
        return imageB;
    }

    /**
     *  Accessor that returns the frame holding this component.
     *  @return The frame.
     */
    public SurfaceRender getFrame() {
        return frame;
    }

    /**
     *  Accessor that returns the active image.
     *  @return The active image.
     */
    public ModelImage getActiveImage() {
        return imageActive;
    }

    /**
     *  Accessor that returns the active image buffer.
     *  @return The active image buffer
     */
    public float[] getActiveImageBuffer() {
        return imageBufferActive;
    }

    /**
     *	Sets the buffers for the actual data, the displayable image, and the paint.
     *   @param imgBufferA   Storage buffer used to display image A.
     *   @param imgBufferB   Storage buffer used to display image B.
     *   @param paintBuff    Storage buffer used to display the combined paintBitmap and pixBuffer buffers.
     */
    public void setBuffers(float imgBufferA[], float imgBufferB[]) {

        imageBufferA = imgBufferA;
        imageBufferB = imgBufferB;
        imageBufferActive = imageBufferA;
    }

    /**
     *	Sets component's Image A.
     *	@param image The component's image A.
     */
    public void setImageA(ModelImage image) {
        imageA = image;
    }

    /**
     *  Sets component's Image B.
     *  @param image The component's image B.
     */
    public void setImageB(ModelImage image) {
        imageB = image;
    }

    /**
     *  Sets component's Image B data buffer.
     *  @param buffer The component's image B data buffer.
     */
    public void setImageBufferB(float buffer[]) {
        imageBufferB = buffer;
    }

    /**
     *   Sets the alpha blending of parameter for two image displaying.
     *   @param value   Amount [0,100] that is the percentage of Image A to be displayed.
     */
    public void setAlphaBlend(int value) {
        alphaBlend = value / 100.0f;
        alphaPrime = 1 - alphaBlend;
    }

    // The following 2 functions set the RGB tables for ARGB images A and B.
    /**
     *  Sets the RGB table for ARGB image A.
     *  @param RGBT  RGB table.
     */
    public void setRGBTA(ModelRGB RGBT) {
        RGBTA = RGBT;
    }

    /**
     *  Sets the RGB table for ARGB image B.
     *  @param RGBT  RGB table.
     */
    public void setRGBTB(ModelRGB RGBT) {
        RGBTB = RGBT;
    }

    /**
     *  Accessor that returns the slice of the image component.
     *  @return The slice.
     */
    public int getSlice() {
        return slice;
    }

    /**
     *  Accessor that sets the slice of the image component.
     *  @param _slice Image slice to be displayed.
     */
    public void setSlice(int _slice) {
        slice = _slice;
    }

    /**
     *   Calls garbage collector to release system resources.
     *
     */
    protected void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
    }

    /**
     *  Sets all variables to null, disposes, and garbage collects.
     */
    public void disposeLocal() {

        lutBufferRemapped = null;
        imageBufferA = null;
        imageBufferB = null;
        imageActive = null;
        imageBufferActive = null;
        frame = null;
        imageA = null;
        imageB = null;

        if (LUTa != null) {
            LUTa.disposeLocal();
        }
        LUTa = null;

        if (LUTb != null) {
            LUTb.disposeLocal();
        }
        LUTb = null;

        if (RGBTA != null) {
            RGBTA.disposeLocal();
        }
        RGBTA = null;

        if (RGBTB != null) {
            RGBTB.disposeLocal();
        }
        RGBTB = null;

        imageExtents = null;
        res = null;

        // super.disposeLocal();
    }

    /**
     * Sets the member variable m_bInterpolate. When set to true, the
     * showDiagonal function does tri-linear interpolation of the ModelImage
     * data: */
    public void Interpolate(boolean bSample) {
        m_bInterpolate = bSample;
    }

    /**
     *  Interpolates the ModelImage data for the non-interger data point
     * (x,y,z) through tri-linear interpolation. The iColor is used for
     * interpolating the color data RGB:
     * @param kImage
     * @param x
     * @param y
     * @param z
     * @param iXFactor
     * @param iYFactor
     * @param iZFactor
     * @param iColor
     * @param iSize
     * @return
     */
    private float interpolate(ModelImage kImage,
                              float x, float y, float z,
                              int iXFactor, int iYFactor, int iZFactor,
                              int iColor,
                              int iSize) {
        /* Round down and round up to the nearest int values for indexing into
         * the ModelImage: */
        float fXFloor = (float) Math.floor( (double) x);
        float fXCeil = (float) Math.ceil( (double) x);

        float fYFloor = (float) Math.floor( (double) y);
        float fYCeil = (float) Math.ceil( (double) y);

        float fZFloor = (float) Math.floor( (double) z);
        float fZCeil = (float) Math.ceil( (double) z);

        /* Calculate the ModelImage index values for the 8 points around the
         * input point (x,y,z): */
        int iX1L = iColor + (int) (fXFloor * iXFactor +
                                   fYFloor * iYFactor +
                                   fZFloor * iZFactor);

        int iX2L = iColor + (int) (fXFloor * iXFactor +
                                   fYFloor * iYFactor +
                                   fZCeil * iZFactor);

        int iX3L = iColor + (int) (fXFloor * iXFactor +
                                   fYCeil * iYFactor +
                                   fZCeil * iZFactor);

        int iX4L = iColor + (int) (fXFloor * iXFactor +
                                   fYCeil * iYFactor +
                                   fZFloor * iZFactor);

        int iX1R = iColor + (int) (fXCeil * iXFactor +
                                   fYFloor * iYFactor +
                                   fZFloor * iZFactor);

        int iX2R = iColor + (int) (fXCeil * iXFactor +
                                   fYFloor * iYFactor +
                                   fZCeil * iZFactor);

        int iX3R = iColor + (int) (fXCeil * iXFactor +
                                   fYCeil * iYFactor +
                                   fZCeil * iZFactor);

        int iX4R = iColor + (int) (fXCeil * iXFactor +
                                   fYCeil * iYFactor +
                                   fZFloor * iZFactor);

        /* Bounds checking: */
        if ( (iX1L < 0) || (iX1L >= iSize) ||
            (iX1R < 0) || (iX1R >= iSize) ||

            (iX2L < 0) || (iX2L >= iSize) ||
            (iX2R < 0) || (iX2R >= iSize) ||

            (iX3L < 0) || (iX3L >= iSize) ||
            (iX3R < 0) || (iX3R >= iSize) ||

            (iX4L < 0) || (iX4L >= iSize) ||
            (iX4R < 0) || (iX4R >= iSize)) {
            return 0;
        }

        /* Interpolate the eight points in the x-direction first, using the
         * difference between x and floor(x) as the interpolation factor, this
         * creates four intermediate points: */
        float fInterpX = (float) (x - fXFloor);
        float fXLowerFront = kImage.getFloat(iX1L) * (1 - fInterpX) +  kImage.getFloat(iX1R) * fInterpX;
        float fXLowerBack = kImage.getFloat(iX2L) * (1 - fInterpX) + kImage.getFloat(iX2R) * fInterpX;
        float fXUpperBack = kImage.getFloat(iX3L) * (1 - fInterpX) + kImage.getFloat(iX3R) * fInterpX;
        float fXUpperFront = kImage.getFloat(iX4L) * (1 - fInterpX) + kImage.getFloat(iX4R) * fInterpX;

        /* Interpolate the four intermediate points in the y-direction, using
         * the difference between y and floor(y) and the interpolation factor,
         * this creates two intermediate points: */
        float fInterpY = (float) (y - fYFloor);
        float fYFront = fXLowerFront * (1 - fInterpY) +  fXUpperFront * fInterpY;
        float fYBack = fXLowerBack * (1 - fInterpY) + fXUpperBack * fInterpY;

        /* Interpolate the two remaining points in the z-direction, using the
         * difference between z and floor(z) and the interpolation factor,
         * finding the final tri-linear interpolated point: */
        float fInterpZ = (float) (z - fZFloor);
        float fCenter = fYFront * (1 - fInterpZ) + fYBack * fInterpZ;

        /* Return the interpolated point: */
        return fCenter;
    }

    /**
     * showDiagonal samples the ModelImage data along a non-axis aligned
     * plane. The plane may intersect the ModelImage volume, defined in x,y,z
     * space, along a diagonal direction. This function has several steps,
     * similar to the show() functions below:
     *
     * 1. Get the HistoLUT or RGB lookup tables for grayscale or color images,
     * depending on the ModelImage data type. This first step initializes the
     * lookup tables for either type of image, and for both imageA and imageB
     * if both are defined:
     *
     * 2. The second step is to calculate the bounding box of the plane that
     * intersects the ModelImage data, and transforming the four corners of
     * the plane to the new rotated coordinates. The four transformed/rotated
     * coordinates are used for steping through and interpolating the
     * ModelImage data along the diagonal directions:
     *
     * 3. The third step is to step through the image we are writing the data
     * in, using the four transformed points to step through the ModelImage
     * along the diagonal directions, read the corresonding point in the
     * ModelImage and write the value into the image array. If m_bInterpolate
     * is set to true, the ModelImage data for non-interger vertices is
     * interpolated using tri-linear interpolation. Note: there is one loop
     * for steping though he data, no matter which type of plane this object
     * represents (XY, XZ, or ZY).
     * @param transform
     * @param akVerts
     */
    public boolean showDiagonal(Transform3D kTransform, Point3f[] akVerts) {
        int xDim, yDim, zDim;
        int bufferSize;
        int lutHeightA = 0, lutHeightB = 0;
        int index;
        float Ra, Ga, Ba, Rb, Gb, Bb;
        int pix;
        int value;

        int[] lutBufferRemappedB = null;

        float maxColorA = 255;
        float maxColorB = 255;
        float normColorB = 1;
        float normColorA = 1;
        float offsetAR = 0.0f;
        float offsetAG = 0.0f;
        float offsetAB = 0.0f;
        float offsetBR = 0.0f;
        float offsetBG = 0.0f;
        float offsetBB = 0.0f;

        /*
         * 1. Get the HistoLUT or RGB lookup tables for grayscale or color images,
         * depending on the ModelImage data type. This first step initializes the
         * lookup tables for either type of image, and for both imageA and imageB
         * if both are defined:
         */
        int RGBIndexBufferA[] = new int[256];
        int RGBIndexBufferB[] = new int[256];

        xDim = imageExtents[0];
        yDim = imageExtents[1];
        zDim = imageExtents[2];

        /* Setup the grayscale LUT for imageA: */
        if (LUTa != null) {
            lutHeightA = LUTa.getExtents()[1];
            if (lutHeightA != lutBufferRemapped.length) {
                try {
                    lutBufferRemapped = new int[lutHeightA];
                }
                catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: ComponentTriSliceImage.show");
                    return false;
                }
            }

            LUTa.exportIndexedLUT(lutBufferRemapped);
        }
        /* Setup the color LUT for imageA: */
        if (RGBTA != null) {
            if (imageA.getType() == ModelStorageBase.ARGB_USHORT) {
                maxColorA = (float) imageA.getMaxR();
                maxColorA = Math.max( (float) imageA.getMaxG(), maxColorA);
                maxColorA = Math.max( (float) imageA.getMaxB(), maxColorA);
            }
            else if (imageA.getType() == ModelStorageBase.ARGB_FLOAT) {
                if (imageA.getMinR() < 0.0) {
                    maxColorA = (float) (imageA.getMaxR() - imageA.getMinR());
                    offsetAR = (float) ( -imageA.getMinR());
                }
                else {
                    maxColorA = (float) imageA.getMaxR();
                }
                if (imageA.getMinG() < 0.0) {
                    maxColorA = Math.max( (float) (imageA.getMaxG() - imageA.getMinG()), maxColorA);
                    offsetAG = (float) ( -imageA.getMinG());
                }
                else {
                    maxColorA = Math.max( (float) imageA.getMaxG(), maxColorA);
                }
                if (imageA.getMinB() < 0.0) {
                    maxColorA = Math.max( (float) (imageA.getMaxB() - imageA.getMinB()), maxColorA);
                    offsetAB = (float) ( -imageA.getMinB());
                }
                else {
                    maxColorA = Math.max( (float) imageA.getMaxB(), maxColorA);
                }
            }
            normColorA = 255 / maxColorA;
            RGBIndexBufferA = RGBTA.exportIndexedRGB();
        }

        /* If imageB is defined: */
        if (imageB != null) {
            /* Setup the grayscale LUT for imageB: */
            if (LUTb != null) {
                lutHeightB = LUTb.getExtents()[1];
                lutBufferRemappedB = new int[lutHeightB];
                LUTb.exportIndexedLUT(lutBufferRemappedB);
            }
            /* Setup the color LUT for imageB: */
            if (RGBTB != null) {
                if (imageB.getType() == ModelStorageBase.ARGB_USHORT) {
                    maxColorB = (float) imageB.getMaxR();
                    maxColorB = Math.max( (float) imageB.getMaxG(), maxColorB);
                    maxColorB = Math.max( (float) imageB.getMaxB(), maxColorB);
                }
                if (imageB.getType() == ModelStorageBase.ARGB_FLOAT) {
                    if (imageB.getMinR() < 0.0) {
                        maxColorB = (float) (imageB.getMaxR() - imageB.getMinR());
                        offsetBR = (float) ( -imageB.getMinR());
                    }
                    else {
                        maxColorB = (float) imageB.getMaxR();
                    }
                    if (imageB.getMinG() < 0.0) {
                        maxColorB = Math.max( (float) (imageB.getMaxG() - imageB.getMinG()), maxColorB);
                        offsetBG = (float) ( -imageB.getMinG());
                    }
                    else {
                        maxColorB = Math.max( (float) imageB.getMaxG(), maxColorB);
                    }
                    if (imageB.getMinB() < 0.0) {
                        maxColorB = Math.max( (float) (imageB.getMaxB() - imageB.getMinB()), maxColorB);
                        offsetBB = (float) ( -imageB.getMinB());
                    }
                    else {
                        maxColorB = Math.max( (float) imageB.getMaxB(), maxColorB);
                    }
                }
                normColorB = 255 / maxColorB;
                RGBIndexBufferB = RGBTB.exportIndexedRGB();
            }
        }

        /*
         * 2. The second step is to calculate the bounding box of the plane that
         * intersects the ModelImage data, and transforming the four corners of
         * the plane to the new rotated coordinates. The four transformed/rotated
         * coordinates are used for steping through and interpolating the
         * ModelImage data along the diagonal directions:
         */
        float xBox = (xDim - 1) * Math.abs(imageA.getFileInfo()[0].getResolutions()[0]);
        float yBox = (yDim - 1) * Math.abs(imageA.getFileInfo()[0].getResolutions()[1]);
        float zBox = (zDim - 1) * Math.abs(imageA.getFileInfo()[0].getResolutions()[2]);
        float maxBox = xBox;
        if (yBox > maxBox) {
            maxBox = yBox;
        }
        if (zBox > maxBox) {
            maxBox = zBox;
        }

        /* Normalize the size
         * xBox range between 0 - 1. */
        xBox = xBox / maxBox;
        yBox = yBox / maxBox;
        zBox = zBox / maxBox;

        /* Transform the four vertices of the plane, so that the plane is
         * rotated, and convert the points to ModelImage space: */
        Point3f[] akVertices = new Point3f[4];
        for (int iVert = 0; iVert < 4; iVert++) {
            akVertices[iVert] = new Point3f(akVerts[iVert]);

            /* Rotate the points in the bounding box: */
            kTransform.transform(akVertices[iVert]);

            /* Convert the points to ModelImage space: */
            akVertices[iVert].x = ( (akVertices[iVert].x + xBox) / (  2.0f * xBox)) * (xDim - 1);
            akVertices[iVert].y = ( (akVertices[iVert].y - yBox) / ( -2.0f * yBox)) * (yDim - 1);
            akVertices[iVert].z = ( (akVertices[iVert].z - zBox) / ( -2.0f * zBox)) * (zDim - 1);
        }

        /* Calculate the slopes for traversing the data in x,y,z: */
        float xSlopeX = akVertices[1].x - akVertices[0].x;
        float ySlopeX = akVertices[1].y - akVertices[0].y;
        float zSlopeX = akVertices[1].z - akVertices[0].z;

        float xSlopeY = akVertices[3].x - akVertices[0].x;
        float ySlopeY = akVertices[3].y - akVertices[0].y;
        float zSlopeY = akVertices[3].z - akVertices[0].z;

        /* Set the data ranges and image orientations, based on the image
         * orientation of this object: */
        int iXRange = xDim;
        int iYRange = yDim;

        if ( (orientation == XY) || (orientation == NA)) {
            bufferSize = xDim * yDim;
        }
        else if (orientation == XZ) {
            bufferSize = xDim * zDim;
            iYRange = zDim;
        }
        else { // orientation == ZY
            bufferSize = zDim * yDim;
            iXRange = zDim;
        }

        /* If the image is a color image, set teh bufferFactor: */
        int bufferFactor = 1;
        if (imageA.isColorImage()) {
            bufferFactor = 4;
        }
        bufferSize *= bufferFactor;

        /* Calculate the factors for intexing into the ModelImage, and the
         * size of the ModelImage for bounds checking: */
        int iZFactor = xDim * yDim * bufferFactor;
        int iYFactor = xDim * bufferFactor;
        int iXFactor = 1 * bufferFactor;
        int iMISize = xDim * yDim * zDim * bufferFactor;
        int iMIIndex = 0;

        pix = 0;
        float x, y, z;
        float x0, y0, z0;
        x0 = akVertices[0].x;
        y0 = akVertices[0].y;
        z0 = akVertices[0].z;

        xSlopeX /= (float) (iXRange - 1);
        ySlopeX /= (float) (iXRange - 1);
        zSlopeX /= (float) (iXRange - 1);

        xSlopeY /= (float) (iYRange - 1);
        ySlopeY /= (float) (iYRange - 1);
        zSlopeY /= (float) (iYRange - 1);

        /*
         * 3. The third step is to step through the image we are writing the data
         * in, using the four transformed points to step through the ModelImage
         * along the diagonal directions, read the corresonding point in the
         * ModelImage and write the value into the image array. If m_bInterpolate
         * is set to true, the ModelImage data for non-interger vertices is
         * interpolated using tri-linear interpolation. Note: there is one loop
         * for steping though he data, no matter which type of plane this object
         * represents (XY, XZ, or ZY).
         */

        /* imageBufferA and imageBufferB have size iYRange * iXRange, step
         * through the imageBuffers while determinging which point along the
         * diagonal plane is shown at that point, accessing the ModelImage at
         * that diagonal point:*/
        for (int j = 0; j < iYRange; j++) {
            /* Initialize the first diagonal point(x,y,z): */
            x = x0;
            y = y0;
            z = z0;
            for (int i = 0; i < iXRange; i++) {
                /* Calculate the index into the imageBuffers: */
                index = i + j * iXRange;

                /* Bounds checking, if out of bounds, set to zero: */
                if ( ( (x < 0) || (x >= xDim)) ||
                    ( (y < 0) || (y >= yDim)) ||
                    ( (z < 0) || (z >= zDim))) {
                    for (int ib = 0; ib < bufferFactor; ib++) {
                        imageBufferA[index + ib] = 0;
                        if ( (imageB != null) && (imageBufferB != null)) {
                            imageBufferB[index + ib] = 0;
                        }
                    }
                }
                else {
                    /* Calculate the index into the ModelImage: */
                    iMIIndex =
                        ( ( (int) z) * iZFactor) +
                        ( ( (int) y) * iYFactor) +
                        ( ( (int) x) * iXFactor);
                    /* Bounds checking, if out of bounds, set to zero: */
                    if ( (iMIIndex < 0) || (iMIIndex >= iMISize)) {
                        for (int ib = 0; ib < bufferFactor; ib++) {
                            imageBufferA[index + ib] = 0;
                            if ( (imageB != null) && (imageBufferB != null)) {
                                imageBufferB[index + ib] = 0;
                            }
                        }
                    }
                    else {
                        /* Set the imageBuffer values for this ModelImage data point: */
                        for (int ib = 0; ib < bufferFactor; ib++) {
                            /* no tri-linear interpolation: */
                            if (!m_bInterpolate) {
                                imageBufferA[index + ib] = imageA.getFloat(iMIIndex + ib);
                                if ( (imageB != null) && (imageBufferB != null)) {
                                    imageBufferB[index + ib] =
                                        imageB.getFloat(iMIIndex + ib);
                                }
                            }
                            /* tri-linear interpolation: */
                            else {
                                imageBufferA[index + ib] =
                                    interpolate(imageA,
                                                x, y, z,
                                                iXFactor, iYFactor, iZFactor, ib,
                                                iMISize);
                                if ( (imageB != null) && (imageBufferB != null)) {
                                    imageBufferB[index + ib] =
                                        interpolate(imageB,
                                        x, y, z,
                                        iXFactor, iYFactor, iZFactor, ib,
                                        iMISize);
                                }
                            }
                        }
                    }
                }

                /* Calculate the lookup table values, grayscale or color for
                 * imageA and imageB (if defined): */
                Ra = 0;
                Ga = 0;
                Ba = 0;
                TransferFunction tf_imgA = LUTa.getTransferFunction();
                if (! (imageA.isColorImage())) {
                    pix = (int)(tf_imgA.getRemappedValue(imageBufferA[index], 256)+0.5f);
                    value = lutBufferRemapped[pix];
                    Ra = (value & 0x00ff0000) >> 16;
                    Ga = (value & 0x0000ff00) >> 8;
                    Ba = (value & 0x000000ff);
                }
                else if (RGBTA != null) {
                    if (RGBTA.getROn()) {
                        Ra =
                            (RGBIndexBufferA[ (int) ( (imageBufferA[index + 1] + offsetAR) *
                            normColorA)] & 0x00ff0000) >> 16;
                    }
                    else {
                        Ra = 0;
                    }
                    if (RGBTA.getGOn()) {
                        Ga =
                            (RGBIndexBufferA[ (int) ( (imageBufferA[index + 2] + offsetAG) *
                            normColorA)] & 0x0000ff00) >> 8;
                    }
                    else {
                        Ga = 0;
                    }
                    if (RGBTA.getBOn()) {
                        Gb = (RGBIndexBufferA[ (int) ( (imageBufferA[index + 3] + offsetAB) *
                            normColorA)] & 0x000000ff);
                    }
                    else {
                        Gb = 0;
                    }
                } // end of if (RGBTA != null)
                else {
                    Ra = (imageBufferA[index + 1] + offsetAR) * normColorA;
                    Ga = (imageBufferA[index + 2] + offsetAG) * normColorA;
                    Ba = (imageBufferA[index + 3] + offsetAB) * normColorA;
                }

                Rb = 0;
                Gb = 0;
                Bb = 0;
                if (imageB != null) {
                    TransferFunction tf_imgB = LUTb.getTransferFunction();
                    if (! (imageB.isColorImage())) {
                        pix = (int) (tf_imgB.getRemappedValue(imageBufferB[index], 256) + 0.5f);
                        value = lutBufferRemappedB[pix];
                        Rb = (value & 0x00ff0000) >> 16;
                        Gb = (value & 0x0000ff00) >> 8;
                        Bb = (value & 0x000000ff);

                    }
                    else if (RGBTB != null) {
                        if (RGBTB.getROn()) {
                            Rb =
                                (RGBIndexBufferB[ (int) ( (imageBufferB[index + 1] + offsetBR) *
                                normColorB)] & 0x00ff0000) >> 16;
                        }
                        else {
                            Rb = 0;
                        }
                        if (RGBTB.getGOn()) {
                            Gb =
                                (RGBIndexBufferB[ (int) ( (imageBufferB[index + 2] + offsetBG) *
                                normColorB)] & 0x0000ff00) >> 8;
                        }
                        else {
                            Gb = 0;
                        }
                        if (RGBTB.getBOn()) {
                            Bb =
                                (RGBIndexBufferB[ (int) ( (imageBufferB[index + 3] + offsetBB) *
                                normColorB)] & 0x000000ff);
                        }
                        else {
                            Bb = 0;
                        }
                    }
                    else {
                        Rb = (int) ( (imageBufferB[index + 1] + offsetBR) * normColorB);
                        Gb = (int) ( (imageBufferB[index + 2] + offsetBG) * normColorB);
                        Bb = (int) ( (imageBufferB[index + 3] + offsetBB) * normColorB);
                    }
                }

                if (Rb == 0 && Gb == 0 && Bb == 0) {
                    Ra = (int) (Ra);
                    Ga = (int) (Ga);
                    Ba = (int) (Ba);
                }
                else if (Ra == 0 && Ga == 0 && Ba == 0) {
                    Ra = (int) (Rb);
                    Ga = (int) (Gb);
                    Ba = (int) (Bb);
                }
                else {
                    Ra = (int) (Ra * alphaBlend + Rb * alphaPrime);
                    Ga = (int) (Ga * alphaBlend + Gb * alphaPrime);
                    Ba = (int) (Ba * alphaBlend + Bb * alphaPrime);
                }

                value = (0xFF000000) | ( (int) Ra << 16) | ( (int) Ga << 8) | (int) Ba;
                img.setRGB(i, j, value);

                /* Inner loop: Move to the next diagonal point along the
                 * x-direction of the plane, using the xSlopeX, ySlopeX and
                 * zSlopeX values: */
                x = x + xSlopeX;
                y = y + ySlopeX;
                z = z + zSlopeX;
            }
            /* Outer loop: Move to the next diagonal point along the
             * y-direction of the plane, using the xSlopeY, ySlopeY and
             * zSlopeY values: */
            x0 = x0 + xSlopeY;
            y0 = y0 + ySlopeY;
            z0 = z0 + zSlopeY;
        }

        return true;
    }

    /**
     *  Shows the image.
     *  @param tslice     t (time) slice to show
     *  @param zslice     z slice to show
     *  @param _LUTa      LUTa - to change to new LUT for imageA else null.
     *  @param _LUTb      LUTb - to change to new LUT for imageB else null.
     *  @param forceShow  Forces this method to import image and recalculate java image.
     *  @return           Confirms if the show was successful.
     */
    public boolean show(int tSlice, int zSlice, ModelLUT _LUTa, ModelLUT _LUTb, boolean forceShow,
                        Transform3D kTransform, Point3f[] akVertices) {

        int xDim, yDim, zDim;
        int bufferSize;
        int lutHeightA = 0;
        int index;
        float RGB_LUTa[][] = null, RGB_LUTb[][] = null;
        float Ra, Ga, Ba, Rb, Gb, Bb;
        int indexA, indexB;
        int pix;
        int i, j;

        if (imageA.isColorImage()) {
            // call the show method for displaying RGB images
            if ( (kTransform != null) && (akVertices != null)) {
                return (showDiagonal(kTransform, akVertices));
            }
            return (show(tSlice, zSlice, forceShow));
        }

        if (imageA == null) {
            return false;
        }
        if (LUTa == null && _LUTb == null) {
            return false;
        }
        if (_LUTa != null) {
            LUTa = _LUTa;
        }
        if (imageB != null && _LUTb != null) {
            LUTb = _LUTb;
        }

        if ( (kTransform != null) && (akVertices != null)) {
            return (showDiagonal(kTransform, akVertices));
        }

        lutHeightA = LUTa.getExtents()[1];

        xDim = imageExtents[0];
        yDim = imageExtents[1];
        zDim = 1;
        if (imageA.getNDims() >= 3) {
            zDim = imageExtents[2];
        }
        if (lutHeightA != lutBufferRemapped.length) {
            try {
                lutBufferRemapped = new int[lutHeightA];
            }
            catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: ComponentEditImage.show");
                return false;
            }
        }

        if (imageB == null) {
            LUTa.exportIndexedLUT(lutBufferRemapped);
        }

        if (orientation == XY || orientation == NA) {
            bufferSize = xDim * yDim;
        }
        else if (orientation == XZ) {
            bufferSize = xDim * zDim;
        }
        else { // orientation == ZY
            bufferSize = zDim * yDim;
        }

        if (imageB != null) {
            RGB_LUTa = LUTa.exportRGB_LUT(true);
            RGB_LUTb = LUTb.exportRGB_LUT(true);
        }

        if ( (orientation == XY) || (orientation == NA)) {
            if (slice != zSlice || timeSlice != tSlice || forceShow == true) {
                slice = zSlice;
                timeSlice = tSlice;
                if (imageA.getNDims() < 4) {
                    timeSliceA = 0;
                }
                else {
                    timeSliceA = timeSlice;
                }
                if ( (imageB != null) && (imageB.getNDims() < 4)) {
                    timeSliceB = 0;
                }
                else {
                    timeSliceB = timeSlice;
                }
                int zDimSlices = 0;
                if (imageA.getNDims() >= 3) {
                    zDimSlices = imageExtents[2];
                }

                try {
                    if (imageA.getType() == ModelStorageBase.COMPLEX) {
                        imageA.exportComplexSliceXY(timeSliceA * zDimSlices + slice, imageBufferA,
                            imageA.getLogMagDisplay());
                    }
                    else {
                        imageA.exportSliceXY(timeSliceA * zDimSlices + slice, imageBufferA);
                    }
                    if (imageB != null) {
                        if (imageB.getType() == ModelStorageBase.COMPLEX) {
                            imageB.exportComplexSliceXY(timeSliceB * zDimSlices + slice, imageBufferB,
                                imageB.getLogMagDisplay());
                        }
                        else {
                            imageB.exportSliceXY(timeSliceB * zDimSlices + slice, imageBufferB);
                        }
                    }
                }
                catch (IOException error) {
                    MipavUtil.displayError("" + error); // Need to fix this
                    return false;
                }
            } // end of if ( slice != zSlice || timeSlice != tSlice || forceShow == true)

            TransferFunction tf_imgA = LUTa.getTransferFunction();
            if (imageB == null) {
                pix = 0;
                for (index = 0; index < bufferSize; index++) {
                    pix = (int)(tf_imgA.getRemappedValue(imageBufferA[index], 256)+0.5f);
                    img.setRGB( index%xDim, index/xDim, lutBufferRemapped[pix]);
                } // end of for (index=0; index < bufferSize; index++)
            } // end of if (imageB == null)
            else { // imageB != null
                indexA = indexB = 0;
                TransferFunction tf_imgB = LUTb.getTransferFunction();
                for (index = 0; index < bufferSize; index++) {
                    indexA = (int)(tf_imgA.getRemappedValue(imageBufferA[index], 256)+0.5f);
                    indexB = (int)(tf_imgB.getRemappedValue(imageBufferB[index], 256)+0.5f);

                    if (frame.getVolOpacityPanel() != null) {
                        alphaBlend = (100.0f - (float) frame.getVolOpacityPanel().getAlphaBlendSliderValue()) / 100.0f;
                        alphaPrime = 1 - alphaBlend;
                    }

                    Ra = RGB_LUTa[0][indexA];
                    Rb = RGB_LUTb[0][indexB];
                    Ga = RGB_LUTa[1][indexA];
                    Gb = RGB_LUTb[1][indexB];
                    Ba = RGB_LUTa[2][indexA];
                    Bb = RGB_LUTb[2][indexB];

                    if (Rb == 0 && Gb == 0 && Bb == 0) {
                        Ra = (int) (Ra);
                        Ga = (int) (Ga);
                        Ba = (int) (Ba);
                    }
                    else if (Ra == 0 && Ga == 0 && Ba == 0) {
                        Ra = (int) (Rb);
                        Ga = (int) (Gb);
                        Ba = (int) (Bb);
                    }
                    else {
                        Ra = (int) (Ra * alphaBlend + Rb * alphaPrime);
                        Ga = (int) (Ga * alphaBlend + Gb * alphaPrime);
                        Ba = (int) (Ba * alphaBlend + Bb * alphaPrime);
                    }
                    img.setRGB( index%xDim, index/xDim, (0xFF000000) | ( (int) Ra << 16) | ( (int) Ga << 8) | (int) Ba);
                } // end of for (index=0; index < bufferSize; index++)
            } // end of else for imageB != null
        } // end of if ((orientation == XY) || (orientation == NA))
        else if (orientation == XZ) {
            if (slice != zSlice || timeSlice != tSlice || forceShow == true) {
                slice = zSlice;
                timeSlice = tSlice;
                if (imageA.getNDims() < 4) {
                    timeSliceA = 0;
                }
                else {
                    timeSliceA = timeSlice;
                }
                if ( (imageB != null) && (imageB.getNDims() < 4)) {
                    timeSliceB = 0;
                }
                else {
                    timeSliceB = timeSlice;
                }

                try {
                    if (imageA.getType() == ModelStorageBase.COMPLEX) {
                        imageA.exportComplexSliceXZ(timeSliceA, slice, imageBufferA, imageA.getLogMagDisplay());
                    }
                    else {
                        imageA.exportSliceXZ(timeSliceA, slice, imageBufferA);
                    }
                    if (imageB != null) {
                        if (imageB.getType() == ModelStorageBase.COMPLEX) {
                            imageB.exportComplexSliceXZ(timeSliceB, slice, imageBufferB, imageA.getLogMagDisplay());
                        }
                        else {
                            imageB.exportSliceXZ(timeSliceB, slice, imageBufferB);
                        }
                    }
                }
                catch (IOException error) {
                    MipavUtil.displayError("" + error); // Need to fix this
                    return false;
                }
            } // end of if ( slice != zSlice || timeSlice != tSlice || forceShow == true)
            if (imageB == null) {
                pix = 0;
                TransferFunction tf_imgA = LUTa.getTransferFunction();
                for (j = 0; j < zDim; j++) {
                    for (i = 0; i < xDim; i++) {
                        index = i + xDim * j;
                         pix = (int)(tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5f);
                         img.setRGB(i,j,lutBufferRemapped[pix]);
                    } // end of for (i = 0; i < xDim; i++)
                } // end of for (j = 0; j < zDim; j++)
            } // end of if (imageB == null)
            else { // imageB != null
                indexA = indexB = 0;
                TransferFunction tf_imgA = LUTa.getTransferFunction();
                TransferFunction tf_imgB = LUTb.getTransferFunction();
                for (j = 0; j < zDim; j++) {
                    for (i = 0; i < xDim; i++) {
                        index = i + xDim * j;
                        indexA = (int)(tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5f);
                        indexB = (int)(tf_imgB.getRemappedValue(imageBufferB[index], 256) + 0.5f);

                        Ra = RGB_LUTa[0][indexA];
                        Rb = RGB_LUTb[0][indexB];
                        Ga = RGB_LUTa[1][indexA];
                        Gb = RGB_LUTb[1][indexB];
                        Ba = RGB_LUTa[2][indexA];
                        Bb = RGB_LUTb[2][indexB];

                        if (frame.getVolOpacityPanel() != null) {
                            alphaBlend = (100.0f - (float) frame.getVolOpacityPanel().getAlphaBlendSliderValue()) /
                                100.0f;
                            alphaPrime = 1 - alphaBlend;
                        }

                        if (Rb == 0 && Gb == 0 && Bb == 0) {
                            Ra = (int) (Ra);
                            Ga = (int) (Ga);
                            Ba = (int) (Ba);
                        }
                        else if (Ra == 0 && Ga == 0 && Ba == 0) {
                            Ra = (int) (Rb);
                            Ga = (int) (Gb);
                            Ba = (int) (Bb);
                        }
                        else {
                            Ra = (int) (Ra * alphaBlend + Rb * alphaPrime);
                            Ga = (int) (Ga * alphaBlend + Gb * alphaPrime);
                            Ba = (int) (Ba * alphaBlend + Bb * alphaPrime);
                        }
                        img.setRGB(i,j,(0xFF000000) | ( (int) Ra << 16) | ( (int) Ga << 8) | (int) Ba);
                    } // end of for (i = 0; i < xDim; i++)
                } // end of for (j = 0; j < zDim; j++)
            } // end of else for imageB != null
        } // end of else if (orientation == XZ)
        else { // orientation == ZY
            if (slice != zSlice || timeSlice != tSlice || forceShow == true) {
                slice = zSlice;
                timeSlice = tSlice;
                if (imageA.getNDims() < 4) {
                    timeSliceA = 0;
                }
                else {
                    timeSliceA = timeSlice;
                }
                if ( (imageB != null) && (imageB.getNDims() < 4)) {
                    timeSliceB = 0;
                }
                else {
                    timeSliceB = timeSlice;
                }

                try {
                    if (imageA.getType() == ModelStorageBase.COMPLEX) {
                        imageA.exportComplexSliceZY(timeSliceA, slice, imageBufferA, imageA.getLogMagDisplay());
                    }
                    else {
                        imageA.exportSliceZY(timeSliceA, slice, imageBufferA);
                    }
                    if (imageB != null) {
                        if (imageB.getType() == ModelStorageBase.COMPLEX) {
                            imageB.exportComplexSliceZY(timeSliceB, slice, imageBufferB, imageA.getLogMagDisplay());
                        }
                        else {
                            imageB.exportSliceZY(timeSliceB, slice, imageBufferB);
                        }
                    }
                }
                catch (IOException error) {
                    MipavUtil.displayError("" + error); // Need to fix this
                    return false;
                }
            } // end of if ( slice != zSlice || timeSlice != tSlice || forceShow == true)
            if (imageB == null) {
                pix = 0;
                TransferFunction tf_imgA = LUTa.getTransferFunction();
                for (j = 0; j < yDim; j++) {
                    for (i = 0; i < zDim; i++) {
                        index = j * zDim + i;
                        pix = (int)(tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5f);
                        img.setRGB(i,j,lutBufferRemapped[pix]);
                    } // end of for (i = 0; i < zDim; i++)
                } // end of for (j = 0; j < yDim; j++)
            } // end of if (imageB == null)
            else { // imageB != null
                indexA = indexB = 0;
                TransferFunction tf_imgA = LUTa.getTransferFunction();
                TransferFunction tf_imgB = LUTb.getTransferFunction();
                for (j = 0; j < yDim; j++) {
                    for (i = 0; i < zDim; i++) {
                        index = j * zDim + i;
                        indexA = (int)(tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5f);
                        indexB = (int)(tf_imgB.getRemappedValue(imageBufferB[index], 256) + 0.5f);

                        Ra = RGB_LUTa[0][indexA];
                        Rb = RGB_LUTb[0][indexB];
                        Ga = RGB_LUTa[1][indexA];
                        Gb = RGB_LUTb[1][indexB];
                        Ba = RGB_LUTa[2][indexA];
                        Bb = RGB_LUTb[2][indexB];

                        if (frame.getVolOpacityPanel() != null) {
                            alphaBlend = (100.0f - (float) frame.getVolOpacityPanel().getAlphaBlendSliderValue()) /
                                100.0f;
                            alphaPrime = 1 - alphaBlend;
                        }

                        if (Rb == 0 && Gb == 0 && Bb == 0) {
                            Ra = (int) (Ra);
                            Ga = (int) (Ga);
                            Ba = (int) (Ba);
                        }
                        else if (Ra == 0 && Ga == 0 && Ba == 0) {
                            Ra = (int) (Rb);
                            Ga = (int) (Gb);
                            Ba = (int) (Bb);
                        }
                        else {
                            Ra = (int) (Ra * alphaBlend + Rb * alphaPrime);
                            Ga = (int) (Ga * alphaBlend + Gb * alphaPrime);
                            Ba = (int) (Ba * alphaBlend + Bb * alphaPrime);
                        }
                        img.setRGB(i,j,(0xFF000000) | ( (int) Ra << 16) | ( (int) Ga << 8) | (int) Ba);
                    } // end of for (i = 0; i < zDim; i++)
                } // end of for (j = 0; j < yDim; j++)
            } // end of else for imageB != null
        } // end of else for orientation == ZY

        return true;
    } // end of show(int tSlice, int zSlice, ModelLUT _LUTa, ModelLUT _LUTb, boolean forceShow)

    /**
     *  For generating the display of 1 or 2 RGB images.
     *  @param tslice     t (time) slice to show.
     *  @param zslice     z slice to show.
     *  @param forceShow  Forces this method to import image and recalculate java image.
     *  @return           Confirms if the show was successful.
     */
    public boolean show(int tSlice, int zSlice, boolean forceShow) {
        // Note that alphaBlending is applied with 1 component taken as zero if both components
        // are not present -for example, if either imageA or imageB but not both has red, then
        // the red component is alphaBlended with zero.
        int i, j;
        int bufferSize;
        int ind4, index;
        float Ra, Ga, Ba, Rb, Gb, Bb;
        int imageSize;
        float redMapped, greenMapped, blueMapped;
        int RGBIndexBufferA[] = new int[256];
        int RGBIndexBufferB[] = new int[256];
        int xDim, yDim, zDim;

        float maxColorA = 255;
        float maxColorB = 255;
        float normColorB = 1;
        float normColorA = 1;
        float offsetAR = 0.0f;
        float offsetAG = 0.0f;
        float offsetAB = 0.0f;
        float offsetBR = 0.0f;
        float offsetBG = 0.0f;
        float offsetBB = 0.0f;

        if (orientation == XY || orientation == NA) {
            bufferSize = imageExtents[0] * imageExtents[1] * 4;
            imageSize = imageExtents[0] * imageExtents[1];
        }
        else if (orientation == XZ) {
            bufferSize = imageExtents[0] * imageExtents[2] * 4;
            imageSize = imageExtents[0] * imageExtents[2];
        }
        else { // orientation == ZY
            bufferSize = imageExtents[2] * imageExtents[1] * 4;
            imageSize = imageExtents[2] * imageExtents[1];
        }

        xDim = imageExtents[0];
        yDim = imageExtents[1];
        zDim = 1;
        if (imageA.getNDims() >= 3) {
            zDim = imageExtents[2];
        }


        if (imageA.getType() == ModelStorageBase.ARGB_USHORT) {
            maxColorA = (float) imageA.getMaxR();
            maxColorA = Math.max( (float) imageA.getMaxG(), maxColorA);
            maxColorA = Math.max( (float) imageA.getMaxB(), maxColorA);
        }
        else if (imageA.getType() == ModelStorageBase.ARGB_FLOAT) {
            if (imageA.getMinR() < 0.0) {
                maxColorA = (float) (imageA.getMaxR() - imageA.getMinR());
                offsetAR = (float) ( -imageA.getMinR());
            }
            else {
                maxColorA = (float) imageA.getMaxR();
            }
            if (imageA.getMinG() < 0.0) {
                maxColorA = Math.max( (float) (imageA.getMaxG() - imageA.getMinG()), maxColorA);
                offsetAG = (float) ( -imageA.getMinG());
            }
            else {
                maxColorA = Math.max( (float) imageA.getMaxG(), maxColorA);
            }
            if (imageA.getMinB() < 0.0) {
                maxColorA = Math.max( (float) (imageA.getMaxB() - imageA.getMinB()), maxColorA);
                offsetAB = (float) ( -imageA.getMinB());
            }
            else {
                maxColorA = Math.max( (float) imageA.getMaxB(), maxColorA);
            }
        }
        normColorA = 255 / maxColorA;

        if ( (imageB != null) && (imageB.getType() == ModelStorageBase.ARGB_USHORT)) {
            maxColorB = (float) imageB.getMaxR();
            maxColorB = Math.max( (float) imageB.getMaxG(), maxColorB);
            maxColorB = Math.max( (float) imageB.getMaxB(), maxColorB);
        }
        if ( (imageB != null) && (imageB.getType() == ModelStorageBase.ARGB_FLOAT)) {
            if (imageB.getMinR() < 0.0) {
                maxColorB = (float) (imageB.getMaxR() - imageB.getMinR());
                offsetBR = (float) ( -imageB.getMinR());
            }
            else {
                maxColorB = (float) imageB.getMaxR();
            }
            if (imageB.getMinG() < 0.0) {
                maxColorB = Math.max( (float) (imageB.getMaxG() - imageB.getMinG()), maxColorB);
                offsetBG = (float) ( -imageB.getMinG());
            }
            else {
                maxColorB = Math.max( (float) imageB.getMaxG(), maxColorB);
            }
            if (imageB.getMinB() < 0.0) {
                maxColorB = Math.max( (float) (imageB.getMaxB() - imageB.getMinB()), maxColorB);
                offsetBB = (float) ( -imageB.getMinB());
            }
            else {
                maxColorB = Math.max( (float) imageB.getMaxB(), maxColorB);
            }
        }
        normColorB = 255 / maxColorB;

        if (RGBTA != null) {
            RGBIndexBufferA = RGBTA.exportIndexedRGB();
        }

        if ( (imageB != null) && (RGBTB != null)) {
            RGBIndexBufferB = RGBTB.exportIndexedRGB();
        }

        if ( (orientation == XY) || (orientation == NA)) {
            if (slice != zSlice || timeSlice != tSlice || forceShow == true) {
                slice = zSlice;
                timeSlice = tSlice;
                if (imageA.getNDims() < 4) {
                    timeSliceA = 0;
                }
                else {
                    timeSliceA = timeSlice;
                }
                if ( (imageB != null) && (imageB.getNDims() < 4)) {
                    timeSliceB = 0;
                }
                else {
                    timeSliceB = timeSlice;
                }
                int zDimSlices = 0;
                if (imageA.getNDims() >= 3) {
                    zDimSlices = imageExtents[2];
                }

                try {
                    imageA.exportData(timeSliceA * zDimSlices * bufferSize + slice * bufferSize, bufferSize,
                                      imageBufferA);
                    if (imageB != null) {
                        imageB.exportData(timeSliceB * zDimSlices * bufferSize + slice * bufferSize, bufferSize,
                                          imageBufferB);
                    }
                }
                catch (IOException error) {
                    MipavUtil.displayError("" + error);
                    return false;
                }
            } // end of if ( slice != zSlice || timeSlice != tSlice || forceShow == true)

            if (imageB == null) {
                for (index = 0, j = 0; j < imageSize; index += 4, j++) {
                    if (RGBTA != null) {
                        if (RGBTA.getROn()) {
                            redMapped = (RGBIndexBufferA[ (int) ( (imageBufferA[index + 1] + offsetAR) * normColorA)] &
                                         0x00ff0000) >> 16;
                        }
                        else {
                            redMapped = 0;
                        }
                        if (RGBTA.getGOn()) {
                            greenMapped = (RGBIndexBufferA[ (int) ( (imageBufferA[index + 2] + offsetAG) * normColorA)] &
                                           0x0000ff00) >> 8;
                        }
                        else {
                            greenMapped = 0;
                        }
                        if (RGBTA.getBOn()) {
                            blueMapped = (RGBIndexBufferA[ (int) ( (imageBufferA[index + 3] + offsetAB) * normColorA)] &
                                          0x000000ff);
                        }
                        else {
                            blueMapped = 0;
                        }
                    } // end of if (RGBTA != null)
                    else {
                        redMapped = (imageBufferA[index + 1] + offsetAR) * normColorA;
                        greenMapped = (imageBufferA[index + 2] + offsetAG) * normColorA;
                        blueMapped = (imageBufferA[index + 3] + offsetAB) * normColorA;
                    }

                    img.setRGB( j%xDim, j/xDim,
                       (0xFF000000) |(((int) (redMapped) << 16) |(((int) (greenMapped) << 8) |((int) (blueMapped)))));
                } // end of for (index=0, j=0; j < imageSize; index += 4, j++)
            } // end of if (imageB == null )
            else { // imageB != null
                for (index = 0, j = 0; j < imageSize; index += 4, j++) {
                    if ( (RGBTA != null) && (RGBTB != null)) {
                        if (RGBTA.getROn()) {
                            Ra = (RGBIndexBufferA[ (int) ( (imageBufferA[index + 1] + offsetAR) * normColorA)] &
                                  0x00ff0000) >> 16;
                        }
                        else {
                            Ra = 0;
                        }
                        if (RGBTA.getGOn()) {
                            Ga = (RGBIndexBufferA[ (int) ( (imageBufferA[index + 2] + offsetAG) * normColorA)] &
                                  0x0000ff00) >> 8;
                        }
                        else {
                            Ga = 0;
                        }
                        if (RGBTA.getBOn()) {
                            Ba = (RGBIndexBufferA[ (int) ( (imageBufferA[index + 3] + offsetAB) * normColorA)] &
                                  0x000000ff);
                        }
                        else {
                            Ba = 0;
                        }

                        if (RGBTB.getROn()) {
                            Rb = (RGBIndexBufferB[ (int) ( (imageBufferB[index + 1] + offsetBR) * normColorB)] &
                                  0x00ff0000) >> 16;
                        }
                        else {
                            Rb = 0;
                        }
                        if (RGBTB.getGOn()) {
                            Gb = (RGBIndexBufferB[ (int) ( (imageBufferB[index + 2] + offsetBG) * normColorB)] &
                                  0x0000ff00) >> 8;
                        }
                        else {
                            Gb = 0;
                        }
                        if (RGBTB.getBOn()) {
                            Bb = (RGBIndexBufferB[ (int) ( (imageBufferB[index + 3] + offsetBB) * normColorB)] &
                                  0x000000ff);
                        }
                        else {
                            Bb = 0;
                        }
                    }
                    else {
                        Ra = (int) ( (imageBufferA[index + 1] + offsetAR) * normColorA);
                        Ga = (int) ( (imageBufferA[index + 2] + offsetAG) * normColorA);
                        Ba = (int) ( (imageBufferA[index + 3] + offsetAB) * normColorA);

                        Rb = (int) ( (imageBufferB[index + 1] + offsetBR) * normColorB);
                        Gb = (int) ( (imageBufferB[index + 2] + offsetBG) * normColorB);
                        Bb = (int) ( (imageBufferB[index + 3] + offsetBB) * normColorB);
                    }

                    if (frame.getVolOpacityPanel() != null) {
                        alphaBlend = (100.0f - (float) frame.getVolOpacityPanel().getAlphaBlendSliderValue()) /
                            100.0f;
                        alphaPrime = 1 - alphaBlend;
                    }

                    if (Rb == 0 && Gb == 0 && Bb == 0) {
                        Ra = (int) (Ra);
                        Ga = (int) (Ga);
                        Ba = (int) (Ba);
                    }
                    else if (Ra == 0 && Ga == 0 && Ba == 0) {
                        Ra = (int) (Rb);
                        Ga = (int) (Gb);
                        Ba = (int) (Bb);
                    }
                    else {
                        Ra = (int) (Ra * alphaBlend + Rb * alphaPrime);
                        Ga = (int) (Ga * alphaBlend + Gb * alphaPrime);
                        Ba = (int) (Ba * alphaBlend + Bb * alphaPrime);
                    }
                    img.setRGB( j%xDim, j/xDim, (0xFF000000) | ( (int) Ra << 16) | ( (int) Ga << 8) | (int) Ba);
                } // end of for (index=0, j=0; j < imageSize; index += 4, j++)
            } // end of else for imageB != null
        } // end of if ((orientation == XY) || (orientation == NA))
        else if (orientation == XZ) {
            if (slice != zSlice || timeSlice != tSlice || forceShow == true) {
                slice = zSlice;
                timeSlice = tSlice;
                if (imageA.getNDims() < 4) {
                    timeSliceA = 0;
                }
                else {
                    timeSliceA = timeSlice;
                }
                if ( (imageB != null) && (imageB.getNDims() < 4)) {
                    timeSliceB = 0;
                }
                else {
                    timeSliceB = timeSlice;
                }

                try {
                    imageA.exportRGBSliceXZ(timeSliceA, slice, imageBufferA);
                    if (imageB != null) {
                        imageB.exportRGBSliceXZ(timeSliceB, slice, imageBufferB);
                    }
                }
                catch (IOException error) {
                    MipavUtil.displayError("" + error);
                    return false;
                }
            } // end of if ( slice != zSlice || timeSlice != tSlice || forceShow == true)

            if (imageB == null) {
                for (j = 0; j < zDim; j++) {
                    for (i = 0; i < xDim; i++) {
                        ind4 = i + xDim * j;
                        index = 4 * ind4;
                        if (RGBTA != null) {
                            if (RGBTA.getROn()) {
                                redMapped = (RGBIndexBufferA[ (int) ( (imageBufferA[index + 1] + offsetAR) * normColorA)] &
                                             0x00ff0000) >> 16;
                            }
                            else {
                                redMapped = 0;
                            }
                            if (RGBTA.getGOn()) {
                                greenMapped = (RGBIndexBufferA[ (int) ( (imageBufferA[index + 2] + offsetAG) *
                                    normColorA)] & 0x0000ff00) >> 8;
                            }
                            else {
                                greenMapped = 0;
                            }
                            if (RGBTA.getBOn()) {
                                blueMapped = (RGBIndexBufferA[ (int) ( (imageBufferA[index + 3] + offsetAB) *
                                    normColorA)] & 0x000000ff);
                            }
                            else {
                                blueMapped = 0;
                            }
                        } // end of if (RGBTA != null)
                        else {
                            redMapped = (imageBufferA[index + 1] + offsetAR) * normColorA;
                            greenMapped = (imageBufferA[index + 2] + offsetAG) * normColorA;
                            blueMapped = (imageBufferA[index + 3] + offsetAB) * normColorA;
                        }
                        img.setRGB( i, j, (0xFF000000) | (((int) (redMapped) << 16) |
                            (((int) (greenMapped) << 8) | ((int) (blueMapped)))));
                    } // end of for (i = 0; i < xDim; i++)
                } // end of for (j = 0; j < zDim; j++)
            } // end of if (imageB == null )
            else { // imageB != null
                for (j = 0; j < zDim; j++) {
                    for (i = 0; i < xDim; i++) {
                        ind4 = i + xDim * j;
                        index = 4 * ind4;
                        if ( (RGBTA != null) && (RGBTB != null)) {
                            if (RGBTA.getROn()) {
                                Ra = (RGBIndexBufferA[ (int) ( (imageBufferA[index + 1] + offsetAR) * normColorA)] &
                                      0x00ff0000) >> 16;
                            }
                            else {
                                Ra = 0;
                            }
                            if (RGBTA.getGOn()) {
                                Ga = (RGBIndexBufferA[ (int) ( (imageBufferA[index + 2] + offsetAG) * normColorA)] &
                                      0x0000ff00) >> 8;
                            }
                            else {
                                Ga = 0;
                            }
                            if (RGBTA.getBOn()) {
                                Ba = (RGBIndexBufferA[ (int) ( (imageBufferA[index + 3] + offsetAB) * normColorA)] &
                                      0x000000ff);
                            }
                            else {
                                Ba = 0;
                            }

                            if (RGBTB.getROn()) {
                                Rb = (RGBIndexBufferB[ (int) ( (imageBufferB[index + 1] + offsetBR) * normColorB)] &
                                      0x00ff0000) >> 16;
                            }
                            else {
                                Rb = 0;
                            }
                            if (RGBTB.getGOn()) {
                                Gb = (RGBIndexBufferB[ (int) ( (imageBufferB[index + 2] + offsetBG) * normColorB)] &
                                      0x0000ff00) >> 8;
                            }
                            else {
                                Gb = 0;
                            }
                            if (RGBTB.getBOn()) {
                                Bb = (RGBIndexBufferB[ (int) ( (imageBufferB[index + 3] + offsetBB) * normColorB)] &
                                      0x000000ff);
                            }
                            else {
                                Bb = 0;
                            }
                        }
                        else {
                            Ra = (int) ( (imageBufferA[index + 1] + offsetAR) * normColorA);
                            Ga = (int) ( (imageBufferA[index + 2] + offsetAG) * normColorA);
                            Ba = (int) ( (imageBufferA[index + 3] + offsetAB) * normColorA);

                            Rb = (int) ( (imageBufferB[index + 1] + offsetBR) * normColorB);
                            Gb = (int) ( (imageBufferB[index + 2] + offsetBG) * normColorB);
                            Bb = (int) ( (imageBufferB[index + 3] + offsetBB) * normColorB);
                        }

                        if (Rb == 0 && Gb == 0 && Bb == 0) {
                            Ra = (int) (Ra);
                            Ga = (int) (Ga);
                            Ba = (int) (Ba);
                        }
                        else if (Ra == 0 && Ga == 0 && Ba == 0) {
                            Ra = (int) (Rb);
                            Ga = (int) (Gb);
                            Ba = (int) (Bb);
                        }
                        else {
                            Ra = (int) (Ra * alphaBlend + Rb * alphaPrime);
                            Ga = (int) (Ga * alphaBlend + Gb * alphaPrime);
                            Ba = (int) (Ba * alphaBlend + Bb * alphaPrime);
                        }
                        img.setRGB( i, j, (0xFF000000) | ( (int) Ra << 16) | ( (int) Ga << 8) | (int) Ba);
                    } // end of for (i = 0; i < xDim; i++)
                } // end of for (j = 0; j < zDim; j++)
            } // end of else for imageB != null
        } // end of else if (orientation == XZ)
        else { // for orientation == ZY
            if (slice != zSlice || timeSlice != tSlice || forceShow == true) {
                slice = zSlice;
                timeSlice = tSlice;
                if (imageA.getNDims() < 4) {
                    timeSliceA = 0;
                }
                else {
                    timeSliceA = timeSlice;
                }
                if ( (imageB != null) && (imageB.getNDims() < 4)) {
                    timeSliceB = 0;
                }
                else {
                    timeSliceB = timeSlice;
                }

                try {
                    imageA.exportRGBSliceZY(timeSliceA, slice, imageBufferA);
                    if (imageB != null) {
                        imageB.exportRGBSliceZY(timeSliceB, slice, imageBufferB);
                    }
                }
                catch (IOException error) {
                    MipavUtil.displayError("" + error);
                    return false;
                }
            } // end of if ( slice != zSlice || timeSlice !=   || forceShow == true)

            if (imageB == null) {
                for (j = 0; j < yDim; j++) {
                    for (i = 0; i < zDim; i++) {
                        ind4 = j * zDim + i;
                        index = 4 * ind4;
                        if (RGBTA != null) {
                            if (RGBTA.getROn()) {
                                redMapped = (RGBIndexBufferA[ (int) ( (imageBufferA[index + 1] + offsetAR) * normColorA)] &
                                             0x00ff0000) >> 16;
                            }
                            else {
                                redMapped = 0;
                            }
                            if (RGBTA.getGOn()) {
                                greenMapped = (RGBIndexBufferA[ (int) ( (imageBufferA[index + 2] + offsetAG) *
                                    normColorA)] & 0x0000ff00) >> 8;
                            }
                            else {
                                greenMapped = 0;
                            }
                            if (RGBTA.getBOn()) {
                                blueMapped = (RGBIndexBufferA[ (int) ( (imageBufferA[index + 3] + offsetAB) *
                                    normColorA)] & 0x000000ff);
                            }
                            else {
                                blueMapped = 0;
                            }
                        } // end of if (RGBTA != null)
                        else {
                            redMapped = (imageBufferA[index + 1] + offsetAR) * normColorA;
                            greenMapped = (imageBufferA[index + 2] + offsetAG) * normColorA;
                            blueMapped = (imageBufferA[index + 3] + offsetAB) * normColorA;
                        }
                        img.setRGB( i, j, (0xFF000000) | (((int) (redMapped) << 16) |
                            (((int) (greenMapped) << 8) | ((int) (blueMapped)))));
                    } // end of for (i = 0; i < zDim; i++)
                } // end of for (j = 0; j < yDim; j++)
            } // end of if (imageB == null )
            else { // imageB != null
                for (j = 0; j < yDim; j++) {
                    for (i = 0; i < zDim; i++) {
                        ind4 = j * zDim + i;
                        index = 4 * ind4;
                        if ( (RGBTA != null) && (RGBTB != null)) {
                            if (RGBTA.getROn()) {
                                Ra = (RGBIndexBufferA[ (int) ( (imageBufferA[index + 1] + offsetAR) * normColorA)] &
                                      0x00ff0000) >> 16;
                            }
                            else {
                                Ra = 0;
                            }
                            if (RGBTA.getGOn()) {
                                Ga = (RGBIndexBufferA[ (int) ( (imageBufferA[index + 2] + offsetAG) * normColorA)] &
                                      0x0000ff00) >> 8;
                            }
                            else {
                                Ga = 0;
                            }
                            if (RGBTA.getBOn()) {
                                Ba = (RGBIndexBufferA[ (int) ( (imageBufferA[index + 3] + offsetAB) * normColorA)] &
                                      0x000000ff);
                            }
                            else {
                                Ba = 0;
                            }

                            if (RGBTB.getROn()) {
                                Rb = (RGBIndexBufferB[ (int) ( (imageBufferB[index + 1] + offsetBR) * normColorB)] &
                                      0x00ff0000) >> 16;
                            }
                            else {
                                Rb = 0;
                            }
                            if (RGBTB.getGOn()) {
                                Gb = (RGBIndexBufferB[ (int) ( (imageBufferB[index + 2] + offsetBG) * normColorB)] &
                                      0x0000ff00) >> 8;
                            }
                            else {
                                Gb = 0;
                            }
                            if (RGBTB.getBOn()) {
                                Bb = (RGBIndexBufferB[ (int) ( (imageBufferB[index + 3] + offsetBB) * normColorB)] &
                                      0x000000ff);
                            }
                            else {
                                Bb = 0;
                            }
                        }
                        else {
                            Ra = (int) ( (imageBufferA[index + 1] + offsetAR) * normColorA);
                            Ga = (int) ( (imageBufferA[index + 2] + offsetAG) * normColorA);
                            Ba = (int) ( (imageBufferA[index + 3] + offsetAB) * normColorA);

                            Rb = (int) ( (imageBufferB[index + 1] + offsetBR) * normColorB);
                            Gb = (int) ( (imageBufferB[index + 2] + offsetBG) * normColorB);
                            Bb = (int) ( (imageBufferB[index + 3] + offsetBB) * normColorB);
                        }

                        if (Rb == 0 && Gb == 0 && Bb == 0) {
                            Ra = (int) (Ra);
                            Ga = (int) (Ga);
                            Ba = (int) (Ba);
                        }
                        else if (Ra == 0 && Ga == 0 && Ba == 0) {
                            Ra = (int) (Rb);
                            Ga = (int) (Gb);
                            Ba = (int) (Bb);
                        }
                        else {
                            Ra = (int) (Ra * alphaBlend + Rb * alphaPrime);
                            Ga = (int) (Ga * alphaBlend + Gb * alphaPrime);
                            Ba = (int) (Ba * alphaBlend + Bb * alphaPrime);
                        }
                        img.setRGB( i, j,  (0xFF000000) | ( (int) Ra << 16) | ( (int) Ga << 8) | (int) Ba);
                    } // end of for (i = 0; i < zDim; i++)
                } // end of for (j = 0; j < yDim; j++)
            } // end of else for imageB != null
        } // end of else for orientation == ZY

        return true;
    }

}
