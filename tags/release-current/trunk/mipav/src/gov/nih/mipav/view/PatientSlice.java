package gov.nih.mipav.view;


import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.Preferences.ComplexDisplay;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * PatientSlice provides oriented or non-oriented interface to the ModelImage data for rendering, with LUT changes.
 *
 * <p>The PatientSlice class should be used any time a 2D slice of the ModelImage is needed for rendering purposes.</p>
 *
 * <p>The PatientSlice class can be used to extract any axial, coronal, sagittal slice from the ModelImage, or slices in
 * the native file coordinates.</p>
 *
 * <p>The PatientSlice class can also be used to extract diagonal slices through the ModelImage. This is done by setting
 * the four corners of the diagonal cut-plane, using file coordinates for the four points of the cut-plane bounding box.
 * </p>
 *
 * <p>The PatientSlice.showUsingOrientation() function extracts a 2D slice from the ModelImage into a 2D packed-int
 * array. This array can be passed to the BufferImage class for viewing 2D images, or it can be passed to the Texture2D
 * class to create texture-mapped polygons. Or the 2D array can be used for algorithms that operate on the 2D ModelImage
 * slices.</p>
 *
 * @see  FileInfoBase.java
 * @see  ModelStorageBase.java
 * @see  ViewJComponentEditImage.java
 * @see  ViewJComponentTriImage.java
 * @see  ViewJComponentTriSliceImage.java
 * @see  PlaneRender.java
 * @see  MipavCoordinateSystems.java
 */
public class PatientSlice {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Flag indicating whether to use the threshold1 value:. */
    private boolean hasThreshold1 = false;

    /** Flag indicating whether to use the threshold2 value:. */
    private boolean hasThreshold2 = false;

    /** imageA:. */
    private ModelImage imageA;

    /** The current active image:. */
    private ModelImage imageActive;

    /** imageB:. */
    private ModelImage imageB;

    /** The imageBuffer that stores the current ModelImage slice data for imageA:. */
    private float[] imageBufferA;

    /** The imageBuffer that stores the current ModelImage slice data for imageB:. */
    private float[] imageBufferB;

    /** colocalization imageBuffer:. */
    private float[] imageBufferColocalize;

    /** colocalization image:. */
    private ModelImage imageColocalize;

    /** image extents in the local coordinate system:. */
    private int[] localImageExtents = new int[3];

    /** ModelLUT lookup table for gray-scale ModelImages: imageA lut. */
    private ModelLUT LUTa = null;

    /** ModelLUT lookup table for gray-scale ModelImages: imageB lut. */
    private ModelLUT LUTb = null;

    /**
     * Mask data when a surface is loaded into the SurfaceRender, for blending with the PlaneRender image. Included here
     * for rendering performance
     */
    private float[] m_afMask = null;

    /** Color normalization factor:. */
    private float[] m_afNormColor = { 1, 1 };

    /** lookup table for mapping ModelImage data values to color for imageA:. */
    private int[] m_aiRGBIndexBufferA = null;

    /** lookup table for mapping ModelImage data values to color for imageB:. */
    private int[] m_aiRGBIndexBufferB = null;

    /** Color offset factor:. */
    private ColorRGB[] m_akOffset = { new ColorRGB(0.0f, 0.0f, 0.0f), new ColorRGB(0.0f, 0.0f, 0.0f) };

    /** Flag indicating whether the slice is axis-aligned or diagonal (for RFA probe rotataions). */
    private boolean m_bAxisAligned = false;

    /** Flag indicating whether to do TriLinear interpolation when exporting a diagonal slice:. */
    private boolean m_bInterpolate = false;

    /**
     * Flag indicating whether or not the slice represents an axis-aligned orientation or a diagonal orientation through
     * the ModelImage volume: true when the RFA probe is activated in the SurfaceRender:.
     */
    private boolean m_bShowDiagonal = false;

    /** Flag indicating whether the slice or LUT changed:. */
    private boolean m_bUpdateImage = true;

    /** current slice positions in FileCoordinates:. */
    private Vector3f m_kFilePoint = new Vector3f();

    /** The rotated non-axis aligned corners of the slice, from the RFA probe rotation in the SurfaceRender:. */
    private Vector3f[] m_kFourCorners = new Vector3f[4];

    /** current slice positions in local coordinates:. */
    private Vector3f m_kPatientPoint = new Vector3f();

    /**
     * This indicates which of the Patient coordinate systems the slice represents: either AXIAL, SAGITTAL, CORONAL, or
     * UNKNOWN.
     */
    private int orientation;

    /** ModelRGB lookup table for color ModelImages: imageA rgb table. */
    private ModelRGB RGBTA = null;

    /** ModelRGB lookup table for color ModelImages: imageB rgb table. */
    private ModelRGB RGBTB = null;

    /** The 2D slice to extract from the ModelImage:. */
    private int slice = 0;

    /** Threshold value:. */
    private float threshold1;

    /** Threshold value:. */
    private float threshold2;

    /** For 4D data, the timeSlice multiplier for imageA:. */
    private int timeSliceA = 0;

    /** For 4D data, the timeSlice multiplier for imageB:. */
    private int timeSliceB = 0;

    /** Flag indicating whether to threshold the blue color channel:. */
    private boolean useBlueThreshold = false;

    /** Flag indicating whether to threshold the green color channel:. */
    private boolean useGreenThreshold = false;

    /** Flag indicating whether to threshold the red color channel:. */
    private boolean useRedThreshold = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs a PatientSlice with the given ModelImages, LUTs, and orientation. The orientation parameter may take
     * on any of the following values: FileInfoBase.AXIAL, FileInfoBase.CORONAL, FileInfoBase.SAGITTAL, or
     * FileInfoBase.UNKNOWN_ORIENT
     *
     * @param  _imageA       Model of the image that will be displayed
     * @param  _LUTa         LUT used to display imageA
     * @param  _imageB       Model of the image that will be displayed
     * @param  _LUTb         LUT used to display imageB
     * @param  _orientation  display orientation of the image
     */
    public PatientSlice(ModelImage _imageA, ModelLUT _LUTa, ModelImage _imageB, ModelLUT _LUTb, int _orientation) {
        imageActive = _imageA;
        imageA = _imageA;
        imageB = _imageB;
        LUTa = _LUTa;
        LUTb = _LUTb;

        orientation = _orientation;
        localImageExtents = imageA.getExtents(orientation);

        if (localImageExtents.length < 3) {
            int[] temp = new int[3];

            for (int i = 0; i < localImageExtents.length; i++) {
                temp[i] = localImageExtents[i];
            }

            for (int i = localImageExtents.length; i < 3; i++) {
                temp[i] = 0;
            }

            localImageExtents = new int[3];

            for (int i = 0; i < 3; i++) {
                localImageExtents[i] = temp[i];
            }
        }

        center();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Default dispose.
     */
    public void disposeLocal() { }


    /**
     * Clean up memory used by the component.
     *
     * @throws  Throwable  if there is a problem encountered during memory clean-up
     *
     * @see     #disposeLocal(boolean)
     */
    public void finalize() throws Throwable {
        disposeLocal();
    }

    /**
     * Returns the ModelLUT or ModelRGB based on which image is currently active, either imageA or imageB and they type
     * of image (color or grayscale).
     *
     * @return  the active LUT/RGB table.
     */
    public ModelStorageBase getActiveLookupTable() {

        if (imageActive == imageA) {

            if (imageA.isColorImage()) {
                return RGBTA;
            }

            return LUTa;
        } else if ((imageB != null) && (imageB.isColorImage())) {
            return RGBTB;
        }

        return LUTb;
    }

    /**
     * Returns whether the current slice is axis-aligned or rotated.
     *
     * @return  true when axis-aligned, false otherwise.
     */
    public boolean getAxisAligned() {
        return m_bAxisAligned;
    }

    /**
     * Return the current center of the volume in FileCoordinates.
     *
     * @return  volume center in FileCoordinates
     */
    public Vector3f getCenter() {
        MipavCoordinateSystems.patientToFile(m_kPatientPoint, m_kFilePoint, imageA, orientation);

        return new Vector3f(m_kFilePoint.X, m_kFilePoint.Y, m_kFilePoint.Z);
    }

    /**
     * Returns the local image extents for the PatientSlice orientation.
     *
     * @return  image extents in local coordinates.
     */
    public int[] getExtents() {
        return localImageExtents;
    }

    /**
     * Accessor that returns the LUT for image A.
     *
     * @return  LUTa
     */
    public ModelLUT getLUTa() {
        return LUTa;
    }

    /**
     * Accessor that returns the LUT for image B.
     *
     * @return  LUTb
     */
    public ModelLUT getLUTb() {
        return LUTb;
    }


    /**
     * This indicates which of the 3 tri-image components we are currently in; either AXIAL, SAGITTAL, or CORONAL.
     *
     * @return  The orientation, either AXIAL, SAGITTAL, or CORONAL.
     */
    public int getOrientation() {
        return orientation;
    }

    /**
     * Accessor that returns the RGBT for image A.
     *
     * @return  RGBTA
     */
    public ModelRGB getRGBTa() {
        return RGBTA;
    }

    /**
     * Accessor that returns the RGBT for image B.
     *
     * @return  RGBTB
     */
    public ModelRGB getRGBTb() {
        return RGBTB;
    }

    /**
     * Sets which image is currently active.
     *
     * @param  kImage  the currently active image.
     */
    public void setActiveImage(ModelImage kImage) {
        this.imageActive = kImage;
    }


    /**
     * The frame in which the image(s) is displayed, allocates the memory and uses this method to pass the references to
     * the buffers.
     *
     * @param  imgBufferA  storage buffer used to display image A
     * @param  imgBufferB  storage buffer used to display image B
     */
    public void setBuffers(float[] imgBufferA, float[] imgBufferB) {
        imageBufferA = imgBufferA;
        imageBufferB = imgBufferB;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  i  FileCoordinates volume center i
     * @param  j  FileCoordinates volume center j
     * @param  k  FileCoordinates volume center k
     */
    public void setCenter(int i, int j, int k) {
        m_kFilePoint.X = i;
        m_kFilePoint.Y = j;
        m_kFilePoint.Z = k;

        MipavCoordinateSystems.fileToPatient(m_kFilePoint, m_kPatientPoint, imageA, orientation);

        if (slice == (int) m_kPatientPoint.Z) {
            return;
        }

        slice = (int) m_kPatientPoint.Z;
        m_bUpdateImage = true;
    }
    
    
    /*  Run when imageA extents are changed */
    public void setImageExtents() {
        localImageExtents = imageA.getExtents(orientation);

        if (localImageExtents.length < 3) {
            int[] temp = new int[3];

            for (int i = 0; i < localImageExtents.length; i++) {
                temp[i] = localImageExtents[i];
            }

            for (int i = localImageExtents.length; i < 3; i++) {
                temp[i] = 0;
            }

            localImageExtents = new int[3];

            for (int i = 0; i < 3; i++) {
                localImageExtents[i] = temp[i];
            }
        }

        center();
    }


    /**
     * Sets the four corners of the bounding box for this slice in FileCoordinates so that the volume data can be
     * exported along a diagonal slice, based on the positions of the input bounding box.
     *
     * @param  fourCorners  the bounding box of the diagonal slice in FileCoordinates
     */
    public void setDiagonalVerts(Vector3f[] fourCorners) {

        if (fourCorners == null) {

            for (int i = 0; i < 4; i++) {
                m_kFourCorners[i] = null;
            }

            m_bShowDiagonal = false;
        } else {

            for (int i = 0; i < 4; i++) {

                if (fourCorners[i] == null) {
                    m_kFourCorners[i] = null;
                    m_bShowDiagonal = false;
                } else {

                    if (m_kFourCorners[i] != null) {

                        if (!m_kFourCorners[i].equals(fourCorners[i])) {
                            m_bUpdateImage = true;
                        }
                    }

                    m_kFourCorners[i] = new Vector3f(fourCorners[i].X, fourCorners[i].Y, fourCorners[i].Z);
                    m_bShowDiagonal = true;
                }
            }
        }
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

    /**
     * Sets imageA.
     *
     * @param  image  imageA
     */
    public void setImageA(ModelImage image) {
        this.imageA = image;
    }

    /**
     * Sets imageB.
     *
     * @param  image  imageB
     */
    public void setImageB(ModelImage image) {
        this.imageB = image;
    }

    /**
     * Sets the colocalize image.
     *
     * @param  imageColocalize  the colocalization image
     */
    public void setImageColocalize(ModelImage imageColocalize) {
        this.imageColocalize = imageColocalize;
    }

    /**
     * Sets the export flag for displaying diagonal to use TriLinear interpolation of the volume data.
     *
     * @param  bInterpolate  when true use TriLinear interpolation of the volume data
     */
    public void setInterpolate(boolean bInterpolate) {
        m_bInterpolate = bInterpolate;
    }

    /**
     * Accessor that sets the LUT for image A.
     *
     * @param  LUT  New LUTa
     */
    public void setLUTa(ModelLUT LUT) {

        if (LUT != null) {
            LUTa = LUT;
            m_bUpdateImage = true;
        }
    }

    /**
     * Accessor that sets the LUT for image B.
     *
     * @param  LUT  New LUTb
     */
    public void setLUTb(ModelLUT LUT) {

        if (LUT != null) {
            LUTb = LUT;
            m_bUpdateImage = true;
        }
    }


    /**
     * Causes the data to be redrawn with new RGBTA values:
     *
     * @param  RGBT  the new RGBT for imageA
     */
    public void setRGBTA(ModelRGB RGBT) {
        RGBTA = RGBT;

        if (RGBTA != null) {
            m_aiRGBIndexBufferA = RGBTA.exportIndexedRGB();
        }

        if (imageA != null) {
            calcMaxNormColors(imageA, 0);
        }

        m_bUpdateImage = true;
    }

    /**
     * Causes the data to be redrawn with new RGBTB values:
     *
     * @param  RGBT  the new RGBT for imageB
     */
    public void setRGBTB(ModelRGB RGBT) {
        RGBTB = RGBT;

        if (RGBTB != null) {
            m_aiRGBIndexBufferB = RGBTB.exportIndexedRGB();
        }

        if (imageB != null) {
            calcMaxNormColors(imageB, 1);
        }

        m_bUpdateImage = true;
    }

    /**
     * Sets the export for this Slice to be along a diagonal.
     *
     * @param  bDiagonal  if true, render this slice as a diagonal
     */
    public void setShowDiagonal(boolean bDiagonal) {
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
     * For generating the display of one or two ModelImages (imageA and imageB). Images may be oriented or non-oriented,
     * axis-aligned or diagonal cut plans through the ModelImage volume.
     *
     * @param   tSlice     t (time) slice to show
     * @param   bufferA    the outbuffer for imageA
     * @param   bufferB    the outbuffer for imageB
     * @param   forceShow  when true the image is re-rendered regardless of whether the slice value displayed or LUT has
     *                     changed.
     * @param   bBlend     when true bufferA and bufferB are blended together
     * @param   fAlpha     the blend factor for blending bufferA and bufferB@
     * @param   bShowMask  when true the surface voxel mask (if any) is blended into the buffer.
     *
     * @return  boolean to indicate whether or not the show function updated
     */
    public boolean showUsingOrientation(int tSlice, int[] bufferA, int[] bufferB, boolean forceShow, boolean bBlend,
                                        float fAlpha, boolean bShowMask) {

        if ((!m_bUpdateImage && !forceShow) || (imageA == null)) {
            return false;
        }
        //System.err.println( "PatientSlice: " + orientation + " " + slice + " " + m_bUpdateImage + " " + forceShow + " " + bBlend);

        timeSliceA = (imageA.getNDims() < 4) ? 0 : tSlice;

        if (imageB != null) {
            timeSliceB = (imageB.getNDims() < 4) ? 0 : tSlice;
        }

        if (imageA.isColorImage() == true) {
            ColorRGBA colorMappedA = new ColorRGBA();
            ColorRGBA colorMappedB = null;

            if (imageB != null) {
                colorMappedB = new ColorRGBA();
            }

            fillImageBuffer(slice, bShowMask);

            for (int j = 0; j < localImageExtents[1]; j++) {

                for (int i = 0; i < localImageExtents[0]; i++) {
                    int ind4 = (j * localImageExtents[0]) + i;
                    int index = 4 * ind4;
                    int pixValue;
                    getColorMapped(RGBTA, m_aiRGBIndexBufferA, 0, imageBufferA, index, colorMappedA);


                    if (imageB != null) {
                        getColorMapped(RGBTB, m_aiRGBIndexBufferB, 1, imageBufferB, index, colorMappedB);

                        if (bBlend) {
                            colorMappedA.R = (fAlpha * colorMappedA.R) + ((1.0f - fAlpha) * colorMappedB.R);
                            colorMappedA.G = (fAlpha * colorMappedA.G) + ((1.0f - fAlpha) * colorMappedB.G);
                            colorMappedA.B = (fAlpha * colorMappedA.B) + ((1.0f - fAlpha) * colorMappedB.B);
                        } else {
                            pixValue = 0xff000000 | ((int) (colorMappedB.R) << 16) | ((int) (colorMappedB.G) << 8) |
                                           ((int) (colorMappedB.B));
                            bufferB[ind4] = pixValue;
                        }
                    }

   
                    if (bShowMask && (m_afMask != null)) {

                        if ((m_afMask[index] != 0) &&
                                ((m_afMask[index + 1] != 0) || (m_afMask[index + 2] != 0) ||
                                     (m_afMask[index + 3] != 0))) {
                            colorMappedA.R = (m_afMask[index] * m_afMask[index + 1]) +
                                             ((1.0f - m_afMask[index]) * colorMappedA.R);
                            colorMappedA.G = (m_afMask[index] * m_afMask[index + 2]) +
                                             ((1.0f - m_afMask[index]) * colorMappedA.G);
                            colorMappedA.B = (m_afMask[index] * m_afMask[index + 3]) +
                                             ((1.0f - m_afMask[index]) * colorMappedA.B);
                        }
                    }

                    pixValue = 0xff000000 | ((int) (colorMappedA.R) << 16) | ((int) (colorMappedA.G) << 8) |
                                   ((int) (colorMappedA.B));
                    bufferA[ind4] = pixValue;
                }
            }
        } else {

            if (LUTa == null) {

                try {
                    LUTa = ViewJFrameImage.initLUT(imageA);
                } catch (OutOfMemoryError e) {
                    return false;
                }
            }

            float[][] RGB_LUTa = null;
            float[][] RGB_LUTb = null;
            int[] lutBufferRemapped = null;
            ;

            TransferFunction tf_imgA = LUTa.getTransferFunction();
            TransferFunction tf_imgB = null;

            if (imageB != null) {
                RGB_LUTa = LUTa.exportRGB_LUT(true);

                if (LUTb == null) {

                    try {
                        LUTb = ViewJFrameImage.initLUT(imageB);
                    } catch (OutOfMemoryError e) {
                        return false;
                    }
                }

                RGB_LUTb = LUTb.exportRGB_LUT(true);
                tf_imgB = LUTb.getTransferFunction();
            } else {
                int lutHeightA = LUTa.getExtents()[1];
                lutBufferRemapped = new int[lutHeightA];
                LUTa.exportIndexedLUT(lutBufferRemapped);
            }

            fillImageBuffer(slice, bShowMask);

            float imageMinA = (float) Math.min(0, imageA.getMin());
            //set variables related to display of complex images
            int imageAsf = imageA.isComplexImage() && Preferences.getComplexDisplay() != ComplexDisplay.MAGNITUDE ? 2 : 1; //defines the mapping between image buffer locations and pixel buffer locations
            int imageBsf = 1;
            if(imageB != null) {
                imageBsf = imageB.isComplexImage() && Preferences.getComplexDisplay() != ComplexDisplay.MAGNITUDE ? 2 : 1;
            }
            boolean logMagDisplay = Preferences.is(Preferences.PREF_LOGMAG_DISPLAY);
            boolean imageAComplex = false, imageBComplex = false;
            imageAComplex  = imageA.isComplexImage() && Preferences.getComplexDisplay() != ComplexDisplay.MAGNITUDE;
            if(imageB != null) {
                imageBComplex = imageB.isComplexImage() && Preferences.getComplexDisplay() != ComplexDisplay.MAGNITUDE;
            }
            
            for (int j = 0; j < localImageExtents[1]; j++) {

                for (int i = 0; i < localImageExtents[0]; i++) {
                    int pixelBufferInd = (j * localImageExtents[0]) + i;
                    
                    int index = 4 * pixelBufferInd;

                    if (hasThreshold1) {

                        if ((imageBufferA[i] < threshold1) || (imageBufferColocalize[i] < threshold2)) {
                            imageBufferA[i] = imageMinA;
                        }
                    } else if (hasThreshold2) {

                        if ((imageBufferColocalize[i] < threshold1) || (imageBufferA[i] < threshold2)) {
                            imageBufferA[i] = imageMinA;
                        }
                    }
                    
                    if (imageB == null) {
                        int pixValueA = (int) (tf_imgA.getRemappedValue(getBufferValue(logMagDisplay, imageAComplex, imageBufferA, pixelBufferInd*imageAsf), 256) + 0.5f);
                        bufferA[pixelBufferInd] = lutBufferRemapped[pixValueA];
                    } else {
                        int indexA = (int) (tf_imgA.getRemappedValue(getBufferValue(logMagDisplay, imageAComplex, imageBufferA, pixelBufferInd*imageAsf), 256) + 0.5f);
                        int indexB = (int) (tf_imgB.getRemappedValue(getBufferValue(logMagDisplay, imageBComplex, imageBufferB, pixelBufferInd*imageBsf), 256) + 0.5f);

                        float Ra = RGB_LUTa[0][indexA];
                        float Ga = RGB_LUTa[1][indexA];
                        float Ba = RGB_LUTa[2][indexA];
                        float Rb = RGB_LUTb[0][indexB];
                        float Gb = RGB_LUTb[1][indexB];
                        float Bb = RGB_LUTb[2][indexB];

                        if (bBlend) {
                            Ra = (fAlpha * Ra) + ((1.0f - fAlpha) * Rb);
                            Ga = (fAlpha * Ga) + ((1.0f - fAlpha) * Gb);
                            Ba = (fAlpha * Ba) + ((1.0f - fAlpha) * Bb);
                        } else {
                            int pixValueB = 0xff000000 | (((int) Rb) << 16) | (((int) Gb) << 8) | ((int) Bb);
                            bufferB[pixelBufferInd] = pixValueB;
                        }

                        int pixValueA = 0xff000000 | (((int) Ra) << 16) | (((int) Ga) << 8) | ((int) Ba);
                        bufferA[pixelBufferInd] = pixValueA;
                    }

                   //Blend in mask: 
                    if (bShowMask && (m_afMask != null)) {

                        if ((m_afMask[index] != 0) &&
                                ((m_afMask[index + 1] != 0) || (m_afMask[index + 2] != 0) ||
                                     (m_afMask[index + 3] != 0))) {
                            float alpha = m_afMask[index];
                            int iMaskRa = (int) (alpha * m_afMask[index + 1]);
                            int iMaskGa = (int) (alpha * m_afMask[index + 2]);
                            int iMaskBa = (int) (alpha * m_afMask[index + 3]);

                            int iRa = (int) ((1 - alpha) * ((bufferA[pixelBufferInd] & 0x00ff0000) >> 16));
                            int iGa = (int) ((1 - alpha) * ((bufferA[pixelBufferInd] & 0x0000ff00) >> 8));
                            int iBa = (int) ((1 - alpha) * (bufferA[pixelBufferInd] & 0x000000ff));

                            int pixValue = 0xff000000 | ((iMaskRa + iRa) << 16) | ((iMaskGa + iGa) << 8) |
                                               (iMaskBa + iBa);
                            bufferA[pixelBufferInd] = pixValue;
                        }
                    }
                }
            }
        }

        m_bUpdateImage = false;

        return true;
    }
    
    
    
    /**
     * For generating the display of one or two ModelImages (imageA and imageB). Images may be oriented or non-oriented,
     * axis-aligned or diagonal cut plans through the ModelImage volume.
     *
     * @param   tSlice     t (time) slice to show
     * @param   bufferA    the outbuffer for imageA
     * @param   bufferB    the outbuffer for imageB
     * @param   forceShow  when true the image is re-rendered regardless of whether the slice value displayed or LUT has
     *                     changed.
     * @param   bShowMask  when true the surface voxel mask (if any) is blended into the buffer.
     *
     * @return  boolean to indicate whether or not the show function updated
     */
    public boolean showUsingOrientation(int tSlice, int[] bufferA, int[] bufferB, boolean forceShow, boolean bShowMask) {

        if ((!m_bUpdateImage && !forceShow) || (imageA == null)) {
            return false;
        }
        //System.err.println( "PatientSlice: " + orientation + " " + slice + " " + m_bUpdateImage + " " + forceShow + " " + bBlend);

        timeSliceA = (imageA.getNDims() < 4) ? 0 : tSlice;

        if (imageB != null) {
            timeSliceB = (imageB.getNDims() < 4) ? 0 : tSlice;
        }

        if (imageA.isColorImage() == true) {
            ColorRGBA colorMappedA = new ColorRGBA();
            ColorRGBA colorMappedB = null;

            if (imageB != null) {
                colorMappedB = new ColorRGBA();
            }

            fillImageBuffer(slice, bShowMask);

            for (int j = 0; j < localImageExtents[1]; j++) {

                for (int i = 0; i < localImageExtents[0]; i++) {
                    int ind4 = (j * localImageExtents[0]) + i;
                    int index = 4 * ind4;
                    int pixValue;
                    getColorMapped(RGBTA, m_aiRGBIndexBufferA, 0, imageBufferA, index, colorMappedA);

                    /* Get imageB color and blend if necessary: */
                    if (imageB != null) {
                        getColorMapped(RGBTB, m_aiRGBIndexBufferB, 1, imageBufferB, index, colorMappedB);

                       
                        pixValue = 0xff000000 | ((int) (colorMappedB.R) << 16) | ((int) (colorMappedB.G) << 8) |
                                           ((int) (colorMappedB.B));
                        bufferB[ind4] = pixValue;
                        
                    }

                    /* Blend in the surface mask if necessary: */
                    if (bShowMask && (m_afMask != null)) {

                        if ((m_afMask[index] != 0) &&
                                ((m_afMask[index + 1] != 0) || (m_afMask[index + 2] != 0) ||
                                     (m_afMask[index + 3] != 0))) {
                            colorMappedA.R = (m_afMask[index] * m_afMask[index + 1]) +
                                             ((1.0f - m_afMask[index]) * colorMappedA.R);
                            colorMappedA.G = (m_afMask[index] * m_afMask[index + 2]) +
                                             ((1.0f - m_afMask[index]) * colorMappedA.G);
                            colorMappedA.B = (m_afMask[index] * m_afMask[index + 3]) +
                                             ((1.0f - m_afMask[index]) * colorMappedA.B);
                        }
                    }

                    pixValue = 0xff000000 | ((int) (colorMappedA.R) << 16) | ((int) (colorMappedA.G) << 8) |
                                   ((int) (colorMappedA.B));
                    bufferA[ind4] = pixValue;
                }
            }
        } else {

            if (LUTa == null) {

                try {
                    LUTa = ViewJFrameImage.initLUT(imageA);
                } catch (OutOfMemoryError e) {
                    return false;
                }
            }

            float[][] RGB_LUTa = null;
            float[][] RGB_LUTb = null;
            int[] lutBufferRemapped = null;
            ;

            TransferFunction tf_imgA = LUTa.getTransferFunction();
            TransferFunction tf_imgB = null;

            if (imageB != null) {
                RGB_LUTa = LUTa.exportRGB_LUT(true);

                if (LUTb == null) {

                    try {
                        LUTb = ViewJFrameImage.initLUT(imageB);
                    } catch (OutOfMemoryError e) {
                        return false;
                    }
                }

                RGB_LUTb = LUTb.exportRGB_LUT(true);
                tf_imgB = LUTb.getTransferFunction();
            } else {
                int lutHeightA = LUTa.getExtents()[1];
                lutBufferRemapped = new int[lutHeightA];
                LUTa.exportIndexedLUT(lutBufferRemapped);
            }

            fillImageBuffer(slice, bShowMask);

            float imageMinA = (float) Math.min(0, imageA.getMin());
            //set variables related to display of complex images
            int imageAsf = imageA.isComplexImage() && Preferences.getComplexDisplay() != ComplexDisplay.MAGNITUDE ? 2 : 1; //defines the mapping between image buffer locations and pixel buffer locations
            int imageBsf = 1;
            if(imageB != null) {
                imageBsf = imageB.isComplexImage() && Preferences.getComplexDisplay() != ComplexDisplay.MAGNITUDE ? 2 : 1;
            }
            boolean logMagDisplay = Preferences.is(Preferences.PREF_LOGMAG_DISPLAY);
            boolean imageAComplex = false, imageBComplex = false;
            imageAComplex  = imageA.isComplexImage() && Preferences.getComplexDisplay() != ComplexDisplay.MAGNITUDE;
            if(imageB != null) {
                imageBComplex = imageB.isComplexImage() && Preferences.getComplexDisplay() != ComplexDisplay.MAGNITUDE;
            }
            
            for (int j = 0; j < localImageExtents[1]; j++) {

                for (int i = 0; i < localImageExtents[0]; i++) {
                    int pixelBufferInd = (j * localImageExtents[0]) + i;
                    
                    int index = 4 * pixelBufferInd;

                    if (hasThreshold1) {

                        if ((imageBufferA[i] < threshold1) || (imageBufferColocalize[i] < threshold2)) {
                            imageBufferA[i] = imageMinA;
                        }
                    } else if (hasThreshold2) {

                        if ((imageBufferColocalize[i] < threshold1) || (imageBufferA[i] < threshold2)) {
                            imageBufferA[i] = imageMinA;
                        }
                    }
                    
                    if (imageB == null) {
                        int pixValueA = (int) (tf_imgA.getRemappedValue(getBufferValue(logMagDisplay, imageAComplex, imageBufferA, pixelBufferInd*imageAsf), 256) + 0.5f);
                        bufferA[pixelBufferInd] = lutBufferRemapped[pixValueA];
                    } else {
                        int indexA = (int) (tf_imgA.getRemappedValue(getBufferValue(logMagDisplay, imageAComplex, imageBufferA, pixelBufferInd*imageAsf), 256) + 0.5f);
                        int indexB = (int) (tf_imgB.getRemappedValue(getBufferValue(logMagDisplay, imageBComplex, imageBufferB, pixelBufferInd*imageBsf), 256) + 0.5f);

                        float Ra = RGB_LUTa[0][indexA];
                        float Ga = RGB_LUTa[1][indexA];
                        float Ba = RGB_LUTa[2][indexA];
                        float Rb = RGB_LUTb[0][indexB];
                        float Gb = RGB_LUTb[1][indexB];
                        float Bb = RGB_LUTb[2][indexB];

                       
                        int pixValueB = 0xff000000 | (((int) Rb) << 16) | (((int) Gb) << 8) | ((int) Bb);
                        bufferB[pixelBufferInd] = pixValueB;
                        

                        int pixValueA = 0xff000000 | (((int) Ra) << 16) | (((int) Ga) << 8) | ((int) Ba);
                        bufferA[pixelBufferInd] = pixValueA;
                    }

                    /* Blend in mask: */
                    if (bShowMask && (m_afMask != null)) {

                        if ((m_afMask[index] != 0) &&
                                ((m_afMask[index + 1] != 0) || (m_afMask[index + 2] != 0) ||
                                     (m_afMask[index + 3] != 0))) {
                            float alpha = m_afMask[index];
                            int iMaskRa = (int) (alpha * m_afMask[index + 1]);
                            int iMaskGa = (int) (alpha * m_afMask[index + 2]);
                            int iMaskBa = (int) (alpha * m_afMask[index + 3]);

                            int iRa = (int) ((1 - alpha) * ((bufferA[pixelBufferInd] & 0x00ff0000) >> 16));
                            int iGa = (int) ((1 - alpha) * ((bufferA[pixelBufferInd] & 0x0000ff00) >> 8));
                            int iBa = (int) ((1 - alpha) * (bufferA[pixelBufferInd] & 0x000000ff));

                            int pixValue = 0xff000000 | ((iMaskRa + iRa) << 16) | ((iMaskGa + iGa) << 8) |
                                               (iMaskBa + iBa);
                            bufferA[pixelBufferInd] = pixValue;
                        }
                    }
                }
            }
        }

        m_bUpdateImage = false;

        return true;
    }   
    
    
    
    
    
    
    
    
    
    


    /**
     * Helper method for calculating complex values.  Performing isComplexBuffer and logMagDsplay checks elsewhere improves performance.
     */
    private float getBufferValue(boolean logMagDisplay, boolean isComplexBuffer, float[] imageBuffer, int ind4) {
        if(isComplexBuffer) { //complex images in MIPAV are always stored in a+bi form, but may not always be shown that way           
            double mag = Math.sqrt(imageBuffer[ind4]*imageBuffer[ind4] + imageBuffer[ind4+1]*imageBuffer[ind4+1]);
            
            if(logMagDisplay) {
                return (float) Math.log(mag);
            } else {
                return (float) mag;
            }
        } else {
            return imageBuffer[ind4];
        }
    }

    /**
     * updates the slice value when the wheel is moved or the page_up, page_down keys are pressed. Does bounds checking
     * and comparison with the current slice value. Sets the new position and updates the triImageFrame.
     *
     * @param   newSlice  the new slice value
     *
     * @return  true if the slice is updated and the image should be re-rendered, false otherwise
     */
    public boolean updateSlice(int newSlice) {

        if (newSlice >= localImageExtents[2]) {
            newSlice = localImageExtents[2] - 1;
        }

        if (newSlice < 0) {
            newSlice = 0;
        }

        if (newSlice != m_kPatientPoint.Z) {
            m_kPatientPoint.Z = newSlice;
            slice = newSlice;
            MipavCoordinateSystems.patientToFile(m_kPatientPoint, m_kFilePoint, imageA, orientation);
            m_bUpdateImage = true;

            return true;
        }

        return false;
    }

    /**
     * Determines if a slice is axis-aligned, based on the m_kFourCorners[] four corners of the slice in 3D Model space.
     *
     * @return  true when the slice is axis-aligned, false otherwise.
     */
    @SuppressWarnings("unused")
    private boolean axisAligned() {
        Vector3f kPatientCorner = new Vector3f();

        for (int i = 0; i < 4; i++) {

            if (m_kFourCorners[i] == null) {
                m_bAxisAligned = true;

                return m_bAxisAligned;
            }

            MipavCoordinateSystems.fileToPatient(m_kFourCorners[i], kPatientCorner, imageA, orientation);

            if (kPatientCorner.Z != m_kPatientPoint.Z) {
                m_bAxisAligned = false;

                return m_bAxisAligned;
            }

            if ((kPatientCorner.X != 0) && (kPatientCorner.X != (localImageExtents[0] - 1))) {
                m_bAxisAligned = false;

                return m_bAxisAligned;
            }

            if ((kPatientCorner.Y != 0) && (kPatientCorner.Y != (localImageExtents[1] - 1))) {
                m_bAxisAligned = false;

                return m_bAxisAligned;
            }

        }

        m_bAxisAligned = true;

        return m_bAxisAligned;
    }

    /**
     * calculates the color normalization factors.
     *
     * @param  kImage  the model image from which the normalization factors are calculated
     * @param  index   index for storing one of two colors normalization factors
     */
    private void calcMaxNormColors(ModelImage kImage, int index) {
        float fMaxColor = 255;

        if (kImage.getType() == ModelStorageBase.ARGB_USHORT) {
            fMaxColor = (float) kImage.getMaxR();
            fMaxColor = Math.max((float) kImage.getMaxG(), fMaxColor);
            fMaxColor = Math.max((float) kImage.getMaxB(), fMaxColor);
        } else if (kImage.getType() == ModelStorageBase.ARGB_FLOAT) {

            if (kImage.getMinR() < 0.0) {
                fMaxColor = (float) (kImage.getMaxR() - kImage.getMinR());
                m_akOffset[index].R = (float) (-kImage.getMinR());
            } else {
                fMaxColor = (float) kImage.getMaxR();
            }

            if (kImage.getMinG() < 0.0) {
                fMaxColor = Math.max((float) (kImage.getMaxG() - kImage.getMinG()), fMaxColor);
                m_akOffset[index].G = (float) (-kImage.getMinG());
            } else {
                fMaxColor = Math.max((float) kImage.getMaxG(), fMaxColor);
            }

            if (kImage.getMinB() < 0.0) {
                fMaxColor = Math.max((float) (kImage.getMaxB() - kImage.getMinB()), fMaxColor);
                m_akOffset[index].B = (float) (-kImage.getMinB());
            } else {
                fMaxColor = Math.max((float) kImage.getMaxB(), fMaxColor);
            }
        }

        m_afNormColor[index] = 255 / fMaxColor;
    }

    /**
     * Calculate the volume center in PatientCoordinates and set the z-value for this slice.
     */
    private void center() {
        m_kPatientPoint.X = localImageExtents[0] / 2;
        m_kPatientPoint.Y = localImageExtents[1] / 2;
        m_kPatientPoint.Z = localImageExtents[2] / 2;
        slice = (int) m_kPatientPoint.Z;
        MipavCoordinateSystems.patientToFile(m_kPatientPoint, m_kFilePoint, imageA, orientation);
    }

    /**
     * Gets the image data based on the orientation, either AXIAL, CORONAL, SAGITTAL, UNKNOWN_ORIENT, or a diagonal
     * slice if the plane is a diagonal cut-plane through the ModelImage.
     *
     * @param  slice  data slize
     */
    private void fillImageBuffer(int slice, boolean bShowMask) {

        try {
            int buffFactor = imageA.isColorImage() ? 4 : 1;
            
            if(!imageA.isColorImage()) {
                buffFactor = imageA.isComplexImage() ? Preferences.getComplexDisplay().getNumParts() : 1;
            }

            if (imageBufferA == null) {
                imageBufferA = new float[localImageExtents[0] * localImageExtents[1] * buffFactor];
            }

            if ((imageB != null) && (imageBufferB == null)) {
                buffFactor = imageB.isColorImage() ? 4 : 1;
                if(!imageB.isColorImage()) {
                    buffFactor = imageB.isComplexImage() ? Preferences.getComplexDisplay().getNumParts() : 1;
                }
                imageBufferB = new float[localImageExtents[0] * localImageExtents[1] * buffFactor];
            }

            if ((imageColocalize != null) && (imageBufferColocalize == null)) {
                buffFactor = imageColocalize.isColorImage() ? 4 : 1;
                if(!imageColocalize.isColorImage()) {
                    buffFactor = imageColocalize.isComplexImage() ? Preferences.getComplexDisplay().getNumParts() : 1;
                }
                imageBufferColocalize = new float[localImageExtents[0] * localImageExtents[1] * buffFactor];
            }

            if (m_bShowDiagonal) {
                imageA.exportDiagonal(timeSliceA, slice, localImageExtents, m_kFourCorners, imageBufferA,
                                      m_bInterpolate);

                if (imageB != null) {
                    imageB.exportDiagonal(timeSliceB, slice, localImageExtents, m_kFourCorners, imageBufferB,
                                          m_bInterpolate);
                }

                if ((imageColocalize != null) && (hasThreshold1 || hasThreshold2)) {
                    imageColocalize.exportDiagonal(timeSliceA, slice, localImageExtents, m_kFourCorners,
                                                   imageBufferColocalize, m_bInterpolate);
                }
            } else {
            	if ( bShowMask )
            	{
            		m_afMask = imageA.export(orientation, timeSliceA, slice, imageBufferA, bShowMask);
            	}
            	else
            	{
            		imageA.export(orientation, timeSliceA, slice, imageBufferA, bShowMask);
            	}
                if (imageB != null) {
                    imageB.export(orientation, timeSliceB, slice, imageBufferB, false);
                }

                if ((imageColocalize != null) && (hasThreshold1 || hasThreshold2)) {
                    imageColocalize.export(orientation, timeSliceA, slice, imageBufferColocalize, false);
                }
            }
        } catch (IOException error) {
            MipavUtil.displayError("" + error);
            error.printStackTrace();

            return;
        }
    }

    /**
     * Get the color from the RGB lookup table:
     *
     * @param  modelRGBT       the color lookup table
     * @param  RGBIndexBuffer  the color lookup table index buffer
     * @param  imageIndex      which color normalization factor to use
     * @param  imageBuffer     the imageBuffer the LUT is changing
     * @param  index           the current imageBuffer pixel
     * @param  colorMapped     the new color value
     */
    private void getColorMapped(ModelRGB modelRGBT, int[] RGBIndexBuffer, int imageIndex, float[] imageBuffer,
                                int index, ColorRGBA colorMapped) {

        colorMapped.R = 0;
        colorMapped.G = 0;
        colorMapped.B = 0;
        colorMapped.A = imageBuffer[index];

        if (modelRGBT != null) {

            if (modelRGBT.getROn()) {
                colorMapped.R = (RGBIndexBuffer[(int) ((imageBuffer[index + 1] + m_akOffset[imageIndex].R) *
                                                           m_afNormColor[imageIndex])] & 0x00ff0000) >> 16;
            }

            if (modelRGBT.getGOn()) {
                colorMapped.G = (RGBIndexBuffer[(int) ((imageBuffer[index + 2] + m_akOffset[imageIndex].G) *
                                                           m_afNormColor[imageIndex])] & 0x0000ff00) >> 8;
            }

            if (modelRGBT.getBOn()) {
                colorMapped.B = (RGBIndexBuffer[(int) ((imageBuffer[index + 3] + m_akOffset[imageIndex].B) *
                                                           m_afNormColor[imageIndex])] & 0x000000ff);
            }
        } else {
            colorMapped.R = (imageBuffer[index + 1] + m_akOffset[imageIndex].R) * m_afNormColor[imageIndex];
            colorMapped.G = (imageBuffer[index + 2] + m_akOffset[imageIndex].G) * m_afNormColor[imageIndex];
            colorMapped.B = (imageBuffer[index + 3] + m_akOffset[imageIndex].B) * m_afNormColor[imageIndex];
        }

        /* Threshold colors: */
        if (useRedThreshold && useGreenThreshold) {

            if ((imageBuffer[index + 1] < threshold1) || (imageBuffer[index + 2] < threshold2)) {
                colorMapped.R = 0;
                colorMapped.G = 0;
                colorMapped.B = 0;
            }
        } else if (useRedThreshold && useBlueThreshold) {

            if ((imageBuffer[index + 1] < threshold1) || (imageBuffer[index + 3] < threshold2)) {
                colorMapped.R = 0;
                colorMapped.G = 0;
                colorMapped.B = 0;
            }
        } else if (useGreenThreshold && useBlueThreshold) {

            if ((imageBuffer[index + 2] < threshold1) || (imageBuffer[index + 3] < threshold2)) {
                colorMapped.R = 0;
                colorMapped.G = 0;
                colorMapped.B = 0;
            }
        }
    }
}
