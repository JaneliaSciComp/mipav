package gov.nih.mipav.view.renderer;

import gov.nih.mipav.view.renderer.surfaceview.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.*;

import javax.vecmath.*;
import java.io.*;
import java.util.*;

/**
 *		Texture mapped image volume displayed in the surface renderer. .
 *
 *		@see		ViewJFrameSurfaceRenderer
 *
 */
public class ViewJComponentSurfaceVolume {

    /** Frame where the component image is displayed. */
    private RenderViewBase frame;

    /** Model for image A. */
    private ModelImage imageA;

    /** Model for image A. */
    private ModelImage imageB;

    /** Texture mapped image */
    private VolumeTexture texture;

    /** Lookup table for image A. */
    private ModelLUT LUTa;

    /** Lookup table for image A. */
    private ModelLUT LUTb;

    /** Model for active image. */
    private ModelImage imageActive = null;

    /** Buffer holding image data for image A. */
    private float[] imageBufferA = null;

    /** Buffer holding image data for image B. */
    private float[] imageBufferB = null;

    /** Buffer holding image data for imageA GM. */
    private float[] imageBufferA_GM = null;

    /** Buffer holding image data for imageB GM. */
    private float[] imageBufferB_GM = null;

    /** Buffer holding image data for active image. */
    private float[] imageBufferActive = null;

    /** Paint buffer. */
    private int[] paintBuffer = null;

    /** Current time slice being displayed. */
    private int timeSlice = -1;

    /** AlphaBlending values for compositing two images. */
    private float alphaBlend = 0.5f;

    /** AlphaBlending values for compositing two images. */
    private float alphaPrime = 0.5f;

    /** RGB table for image A. */
    private ModelRGB RGBTA;

    /** RGB table for image B. */
    private ModelRGB RGBTB;

    /** Extents of the 3D image. */
    private int[] imageExtents;

    /** Buffer for holding the most recently computed normal vectors of image A */
    private Vector3f[] m_akNormalsImageA = null;

    /** Buffer for holding volume texture composite values for image A */
    private int[] m_aiCompositeImageA = null;

    /**
     *   Constructs new component image plane with the appropriate arrays.
     *   @param frame        Frame where image(s) will be displayed.
     *   @param _imageA      Model of the image that will be displayed.
     *   @param _LUTa        LUT used to display imageA.
     *   @param imgBufferA   Storage buffer used to display image A.
     *   @param _imageB      Model of the image that will be displayed.
     *   @param _LUTb        LUT used to display imageB.
     *   @param imgBufferB   Storage buffer used to display image B.
     *   @param volTexture   Storage buffer used to build a displayable texture mapped image.
     *   @param extents      Initial display dimensions of the image.
     */
    public ViewJComponentSurfaceVolume(RenderViewBase _frame,
                                       ModelImage _imageA, ModelLUT _LUTa,
                                       float[] imgBufferA,
                                       ModelImage _imageB, ModelLUT _LUTb,
                                       float[] imgBufferB,
                                       VolumeTexture volTexture, int[] extents) {

        frame = _frame;
        imageA = _imageA;
        imageB = _imageB;
        imageActive = imageA;
        imageExtents = extents;
        texture = volTexture;

        LUTa = _LUTa;
        LUTb = _LUTb;

        imageBufferA = imgBufferA;
        imageBufferB = imgBufferB;
        imageBufferA_GM = new float[imgBufferA.length];
        if (imgBufferB != null) {
            imageBufferB_GM = new float[imgBufferB.length];
        }
        imageBufferActive = imageBufferA;


    }

    /************************************************************************/
    /******************************* Accessors ******************************/
    /************************************************************************/

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
     *  Accessor that returns the alphablend of the two image.
     *  @return Opacity of paint.
     */
    public float getAlphaBlend() {
        return alphaBlend;
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
    public RenderViewBase getFrame() {
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
     * Access to retrieve the normal vectors associated with image A.
     * If they have not been computed, then they are computed now.
     * @return Vector3f[] Array of references to normal vectors corresponding
     * to the samples in image A.
     */
    public Vector3f[] getNormalsImageA() {

        if (null == m_akNormalsImageA) {

            /**
             * Create array of normal vectors corresponding to the voxels in
             * the volume.  The normal vector is computed based on the
             * gradient of the volume intensity values.
             * @param afData float[] Input array of voxel values.
             * @param Vector3f[] Array of normal vectors corresponding to the
             * input voxel intensity values.
             */

            // Extract image slice.
            ModelSimpleImage kSimpleImageA = new ModelSimpleImage(imageA, timeSlice);

            // Convert to intensity valued image.
            ModelSimpleImage kValueImageA = kSimpleImageA.createIntensityImage();
            kSimpleImageA = null;

            // Access intensity values as a linear array.
            // Initially allocate all normal vectors as the zero vector.
            float[] afData = kValueImageA.data;
            Vector3f kZeroVector = new Vector3f(0.0f, 0.0f, 0.0f);
            m_akNormalsImageA = new Vector3f[afData.length];
            Arrays.fill(m_akNormalsImageA, kZeroVector);

            int iXBound = kValueImageA.extents[0];
            int iYBound = kValueImageA.extents[1];
            int iZBound = kValueImageA.extents[2];
            int iXYBound = iXBound * iYBound;

            // normals from gradient which are computed using central finite
            // differences everywhere except forward/backward finite differences
            // are used at the edges
            int iOffX = 1;
            int iOffY = iXBound;
            int iOffZ = iXBound * iYBound;
            int iX, iY, iZ;
            float fDX = 0;
            float fDY = 0;
            float fDZ = 0;
            float flength = 0;

            for (iZ = 1; iZ < iZBound-1; iZ++) {
                boolean bMinZ = 0 == iZ;
                boolean bMaxZ = (iZBound - 1) == iZ;
                for (iY = 1; iY < iYBound-1; iY++) {
                    boolean bMinY = 0 == iY;
                    boolean bMaxY = (iYBound - 1) == iY;
                    int offset = iXBound * (iY + iYBound * iZ);
                    for (iX = 0; iX < iXBound; iX++) {
                        boolean bMinX = 0 == iX;
                        boolean bMaxX = (iXBound - 1) == iX;

                        int i = iX + offset;

                        fDX =
                            ((bMinX ? afData[i] : afData[i - iOffX - iXBound]) -
                            (bMaxX ? afData[i] : afData[i + iOffX - iXBound]) ) * 0.71f  +

                            (bMinX ? afData[i] : afData[i - iOffX]) -
                            (bMaxX ? afData[i] : afData[i + iOffX]) +

                            ((bMinX ? afData[i] : afData[i - iOffX + iXBound]) -
                            (bMaxX ? afData[i] : afData[i + iOffX + iXBound])) * 0.71f;

                        fDY =
                            ((bMinY ? afData[i] : afData[i - iOffY - iYBound]) -
                            (bMaxY ? afData[i] : afData[i + iOffY - iYBound])) * 0.71f +

                            (bMinY ? afData[i] : afData[i - iOffY]) -
                            (bMaxY ? afData[i] : afData[i + iOffY]) +

                            ((bMinY ? afData[i] : afData[i - iOffY + iYBound]) -
                            (bMaxY ? afData[i] : afData[i + iOffY  + iYBound])) * 0.71f;

                        fDZ =
                            ((bMinZ ? afData[i] : afData[i - iOffZ - iZBound]) -
                            (bMaxZ ? afData[i] : afData[i + iOffZ - iZBound]) ) * 0.71f +

                            (bMinZ ? afData[i] : afData[i - iOffZ]) -
                            (bMaxZ ? afData[i] : afData[i + iOffZ]) +

                            ((bMinZ ? afData[i] : afData[i - iOffZ + iZBound]) -
                            (bMaxZ ? afData[i] : afData[i + iOffZ + iZBound])) * 0.71f;

                       flength = 1.0f / (float)Math.sqrt(fDX * fDX + fDY * fDY + fDZ * fDZ);
                       fDX *= flength;
                       fDY *= flength;
                       fDZ *= flength;

                        if (fDX != 0.0f || fDY != 0.0f || fDZ != 0.0f) {
                            m_akNormalsImageA[i] = new Vector3f(fDX, fDY, fDZ);
                            m_akNormalsImageA[i].normalize();
                        }
                    }
                }
            }

            // Catch any zero-vector normals and replace them by an average of
            // neighboring normals.
            for (iZ = 1; iZ < iZBound - 1; iZ++) {
                for (iY = 1; iY < iYBound - 1; iY++) {
                    int offset = iXBound * (iY + iYBound * iZ);
                    for (iX = 1; iX < iXBound - 1; iX++) {

                        int i = iX + offset;

                        if (afData[i] != 0.0f ){// && m_akNormalsImageA[i] == kZeroVector) {
                            m_akNormalsImageA[i] = new Vector3f(0.0f, 0.0f, 0.0f);
                            m_akNormalsImageA[i].add(m_akNormalsImageA[i - 1]);
                            m_akNormalsImageA[i].add(m_akNormalsImageA[i + 1]);
                            m_akNormalsImageA[i].add(m_akNormalsImageA[i - iXBound]);
                            m_akNormalsImageA[i].add(m_akNormalsImageA[i + iXBound]);
                            m_akNormalsImageA[i].add(m_akNormalsImageA[i - iXYBound]);
                            m_akNormalsImageA[i].add(m_akNormalsImageA[i + iXYBound]);
                            m_akNormalsImageA[i].normalize();
                        }
                    }
                }
            }
        }

        return m_akNormalsImageA;
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
     */
    public void setBuffers(float[] imgBufferA, float[] imgBufferB) {

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
    public void setImageBufferB(float[] buffer) {
        imageBufferB = buffer;
    }

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
     * Access to retrieve the array ARGB composite 3D texture values for
     * image A.  This is a copy of those values currently stored in
     * the 3D texture.
     * @return int[] Array of ARGB composite 3D texture values for image A.
     */
    public int[] getCompositeImageA() {
        if (null == m_aiCompositeImageA) {
            int iNumVoxels = imageExtents[0] * imageExtents[1] * imageExtents[2];
            m_aiCompositeImageA = new int[iNumVoxels];
            int iVoxel = 0;
            for (int z = 0; z < imageExtents[2]; z++) {
                int[] aiTextureSliceA = texture.getBufferedRaster(z);
                for (int i = 0; i < aiTextureSliceA.length; i++) {
                    m_aiCompositeImageA[iVoxel++] = aiTextureSliceA[i];
                }
            }
        }

        return m_aiCompositeImageA;
    }

    /**
     *   Calls garbage collector to release system resources.
     */
    protected void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
    }

    /**
     *  Sets all variables to null, disposes, and garbage collects.
     */
    public void disposeLocal() {
        Preferences.debug("ViewJCompoentSurfaceVolume.disposeLocal");

        frame = null;
        imageBufferA = null;
        imageBufferB = null;
        imageBufferA_GM = null;
        imageBufferB_GM = null;
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

        if (texture != null) {
            texture.disposeLocal();
            texture = null;
        }
        paintBuffer = null;
        imageExtents = null;
        m_aiCompositeImageA = null;
    }

    /**
     *  Shows the gray scale image(s).
     *  @param tslice     t (time) slice to show
     *  @param _LUTa      LUTa - to change to new LUT for imageA else null.
     *  @param _LUTb      LUTb - to change to new LUT for imageB else null.
     *  @param forceShow  Forces this method to import image and recalculate java image.
     *  @return           Confirms if the show was successful.
     */
    public boolean show(int tSlice, ModelLUT _LUTa, ModelLUT _LUTb, boolean forceShow) {

        int xDim, yDim, zDim;
        int bufferSize;
        int lutHeightA = 0;
        int index;
        float RGB_LUTa[][] = null, RGB_LUTb[][] = null;
        float Ra, Ga, Ba, Rb, Gb, Bb;
        int indexA, indexB;
        int pix;
        float pixGM = 0;
        int offset = 0;
        ModelImage gmImageA;
        ModelImage gmImageB;
        int opacityValA, opacityValB;
        int[] lutBufferRemapped = null;

        if (imageA.isColorImage()) {
            // call the show method for displaying RGB images
            return (show(tSlice, forceShow));
        }

        if (imageA == null || texture == null) {
                    return false;
        }

        if (LUTa == null && _LUTb == null) {
            return false;
        }

        JPanelVolOpacity volOpacityObj = ( (JPanelVolOpacity) ( (SurfaceRender) frame).getVolOpacityPanel());

        if (_LUTa != null) {
            LUTa = _LUTa;
        }

        if (imageB != null && _LUTb != null) {
            LUTb = _LUTb;
        }

        lutHeightA = LUTa.getExtents()[1];

        xDim = imageExtents[0];
        yDim = imageExtents[1];
        zDim = imageExtents[2];
        bufferSize = xDim * yDim;

        if (lutBufferRemapped == null || lutHeightA != lutBufferRemapped.length) {
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

        if (imageB != null) {
            RGB_LUTa = LUTa.exportRGB_LUT(true);
            RGB_LUTb = LUTb.exportRGB_LUT(true);
        }

        // Why - Matt fix
        if (timeSlice != tSlice || forceShow == true) {
            // Anytime the time slice changes, we need to destroy the previously
            // computed normal vectors for image A so that they can be recomputed
            // upon request.
            if (timeSlice != tSlice) {
                timeSlice = tSlice;
            }
        } // end of if ( timeSlice != tSlice || forceShow == true)

        // About to recompute the composite texture image for the volume.
        m_aiCompositeImageA = null;

        if (imageB == null) {
            // Loop through slices of the image.
            for (int sliceZ = 0; sliceZ < imageA.getExtents()[2]; sliceZ++) {
                paintBuffer = texture.getBufferedRaster(sliceZ);
                offset = timeSlice * xDim * yDim * zDim + sliceZ * bufferSize;
                try {
                    imageA.exportData(offset, bufferSize, imageBufferA);
                    if ( volOpacityObj.getGradMagA() != null) {
                        if (volOpacityObj.isGradientMagnitudeOpacityEnabled()) {
                            gmImageA = volOpacityObj.getGradMagA();
                            gmImageA.exportData(offset, bufferSize, imageBufferA_GM);
                        }
                    }
                }
                catch (IOException error) {
                    MipavUtil.displayError("" + error); // Need to fix this
                    return false;
                }

                pix = 0;
                opacityValA = 0;
                pixGM = 0;
                float val;

                TransferFunction tf_compA_GM = null;
                TransferFunction tf_compA = volOpacityObj.getCompA().getOpacityTransferFunction();

                if (volOpacityObj.isGradientMagnitudeOpacityEnabled() ) {
                    tf_compA_GM = volOpacityObj.getCompA_GM().getOpacityTransferFunction();
                }

                // Loop through image.
                TransferFunction tf_imgA = LUTa.getTransferFunction();
                for (index = 0; index < bufferSize; index++) {
                    // filter pipeline:
                    // original image -> 1. gradient magnitude filter ( GM tranfer function )
                    // -> 2. opacity filter (opacity tranfer function )

                    // Calculate the Gradient Magnitude value for the voxel through GM transfer function.
                    pixGM = 1;
                    if ( tf_compA_GM != null) {
                        pixGM = tf_compA_GM.getRemappedValue(imageBufferA_GM[index], 256) / 255.0f;
                    }

                    // Calculate the voxel value through the transfer function of the LUT.
                    pix = (int)(tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5f);

                    // Apply opacity filter after the gradient magnitude filter.
                    // Take the gradient magnitude result into the opacity transfer function.
                    val = tf_compA.getRemappedValue(imageBufferA[index], 256);
                    // Apply gradient magnitude to the image and normalize the value to the range [0:255]
                    opacityValA = (int) (val * pixGM + 0.5f);

                    try {
                        if (tf_compA != null) {
                            paintBuffer[index] = ( (opacityValA << 24) | 0x00ffffff) & lutBufferRemapped[pix];
                            // paintBuffer[index] = ( (opacityValA << 24) | 0x00ffffff) & lutBufferRemapped[200];
                        }
                        else {

                            paintBuffer[index] = lutBufferRemapped[pix];
                            // paintBuffer[index] = lutBufferRemapped[200];
                        }
                    }
                    catch (Exception e) {
                    }
                } // end of for (index=0; index < bufferSize; index++)
                texture.setImageComponent(sliceZ);
            } // end of if (imageB == null)
        }
        else { // imageB != null
            for (int sliceZ = 0; sliceZ < imageA.getExtents()[2]; sliceZ++) {
                paintBuffer = texture.getBufferedRaster(sliceZ);
                offset = timeSlice * xDim * yDim * zDim + sliceZ * bufferSize;

                try {
                    imageA.exportData(offset, bufferSize, imageBufferA);
                    if ( volOpacityObj.getGradMagA() != null) {
                        if (volOpacityObj.isGradientMagnitudeOpacityEnabled()) {
                            gmImageA = volOpacityObj.getGradMagA();
                            gmImageA.exportData(offset, bufferSize, imageBufferA_GM);
                        }
                    }
                    imageB.exportData(offset, bufferSize, imageBufferB);
                    if ( volOpacityObj.getGradMagB() != null) {
                        if (volOpacityObj.isGradientMagnitudeOpacityEnabled()) {
                            gmImageB = volOpacityObj.getGradMagB();
                            gmImageB.exportData(offset, bufferSize, imageBufferB_GM);
                        }
                    }
                }
                catch (IOException error) {
                    MipavUtil.displayError("" + error); // Need to fix this
                    return false;
                }
                indexA = indexB = 0;
                opacityValA = 0;
                opacityValB = 0;

                TransferFunction tf_compA_GM = null;
                TransferFunction tf_compA = volOpacityObj.getCompA().getOpacityTransferFunction();

                if (volOpacityObj.isGradientMagnitudeOpacityEnabled() ) {
                    tf_compA_GM = volOpacityObj.getCompA_GM().getOpacityTransferFunction();
                }

                TransferFunction tf_compB_GM = null;
                TransferFunction tf_compB = volOpacityObj.getCompB().getOpacityTransferFunction();

                if (volOpacityObj.isGradientMagnitudeOpacityEnabled() ) {
                    tf_compB_GM = volOpacityObj.getCompB_GM().getOpacityTransferFunction();
                }

                TransferFunction tf_imgA = LUTa.getTransferFunction();
                TransferFunction tf_imgB = LUTb.getTransferFunction();
                for (index = 0; index < bufferSize; index++) {
                    float pixGMa = 1;
                    if ( tf_compA_GM != null) {
                        pixGMa = tf_compA_GM.getRemappedValue(imageBufferA_GM[index], 256) / 255.0f;
                    }
                    float pixGMb = 1;
                    if ( tf_compB_GM != null) {
                        pixGMb = tf_compB_GM.getRemappedValue(imageBufferB_GM[index], 256) / 255.0f;
                    }

                    // Apply opacity filter after the gradient magnitude filter.
                    // Take the gradient magnitude result into the opacity transfer function.
                    float valA = tf_compA.getRemappedValue(imageBufferA[index], 256);
                    // Apply gradient magnitude to the image and normalize the value to the range [0:255]
                    opacityValA = (int) (valA * pixGMa  + 0.5f);

                    // Apply opacity filter after the gradient magnitude filter.
                    // Take the gradient magnitude result into the opacity transfer function.
                    float valB = tf_compB.getRemappedValue(imageBufferB[index], 256);
                    // Apply gradient magnitude to the image and normalize the value to the range [0:255]
                    opacityValB = (int) (valB * pixGMb  + 0.5f);

                    // Apply the imageA transfer function.
                    indexA = (int)(tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5f);

                    // Apply the image B transfer function.
                    indexB = (int)(tf_imgB.getRemappedValue(imageBufferB[index], 256) + 0.5f);

                    Ra = RGB_LUTa[0][indexA];
                    Rb = RGB_LUTb[0][indexB];
                    Ga = RGB_LUTa[1][indexA];
                    Gb = RGB_LUTb[1][indexB];
                    Ba = RGB_LUTa[2][indexA];
                    Bb = RGB_LUTb[2][indexB];

                    alphaBlend = (100.0f - (float) volOpacityObj.getAlphaBlendSliderValue()) / 100.0f;
                    alphaPrime = 1 - alphaBlend;

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

                    pix = ( ( (int) (opacityValA * alphaBlend + opacityValB * alphaPrime)) << 24)
                        | ( (int) Ra << 16) | ( (int) Ga << 8) | (int) Ba;
                    paintBuffer[index] = pix;
                } // end of for (index=0; index < bufferSize; index++)
                texture.setImageComponent(sliceZ);
            }
        } // end of else for imageB != null

        RGB_LUTa = null;
        RGB_LUTb = null;
        lutBufferRemapped = null;
        gmImageA = null;
        gmImageB = null;
        return true;
    } // end of show(int tSlice, int zSlice, ModelLUT _LUTa, ModelLUT _LUTb, boolean forceShow)

    /**
     *  For generating the display of 1 or 2 RGB images.
     *  @param tslice     t (time) slice to show.
     *  @param forceShow  Forces this method to import image and recalculate java image.
     *  @return           Confirms if the show was successful.
     */
    public boolean show(int tSlice, boolean forceShow) {
        // Note that alphaBlending is applied with 1 component taken as zero if both components
        // are not present -for example, if either imageA or imageB but not both has red, then
        // the red component is alphaBlended with zero.
        int j;
        int bufferSize = 0;
        int offset = 0;
        int index;
        float Ra, Ga, Ba, Rb, Gb, Bb;
        int imageSize = 0;
        float redMapped, greenMapped, blueMapped;
        int[] RGBIndexBufferA = null;
        int[] RGBIndexBufferB = null;

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

        ModelImage gmImageA;
        ModelImage gmImageB;

        float pixGM_R = 0, pixGM_G = 0, pixGM_B = 0;
        float imgBpixGM_R = 0, imgBpixGM_G = 0, imgBpixGM_B = 0;
        int opacityValA_Avg = 0, opacityValA_R = 0, opacityValA_G = 0, opacityValA_B = 0;
        int opacityValB_Avg = 0, opacityValB_R = 0, opacityValB_G = 0, opacityValB_B = 0;

        xDim = imageExtents[0];
        yDim = imageExtents[1];
        zDim = 1;
        if (imageA.getNDims() >= 3) {
            zDim = imageExtents[2];
        }

        JPanelVolOpacityRGB volOpacityObj = ( (JPanelVolOpacityRGB) ( (SurfaceRender) frame).getVolOpacityPanel());
        boolean gmMode = volOpacityObj.isGradientMagnitudeOpacityEnabled();


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

        if (RGBTB != null) {
            RGBIndexBufferB = RGBTB.exportIndexedRGB();
        }

        if (timeSlice != tSlice || forceShow == true) {
            // Anytime the time slice changes, we need to destroy the previously
            // computed normal vectors for image A so that they can be recomputed
            // upon request.
            if (timeSlice != tSlice) {
                timeSlice = tSlice;
            }
        } // end of if ( slice != zSlice || timeSlice != tSlice || forceShow == true)

        // About to recompute the composite texture image for the volume.
        m_aiCompositeImageA = null;

        imageSize = xDim * yDim;
        bufferSize = xDim * yDim * 4;
        if (imageB == null) {
            for (int sliceZ = 0; sliceZ < imageA.getExtents()[2]; sliceZ++) {
                paintBuffer = texture.getBufferedRaster(sliceZ);
                offset = timeSlice * xDim * yDim * zDim + sliceZ * bufferSize;
                try {
                    imageA.exportData(offset, bufferSize, imageBufferA);
                    if ( volOpacityObj.getGradMagA() != null) {

                        if (imageBufferA_GM == null) {
                            imageBufferA_GM = new float[bufferSize];
                        }

                        if (gmMode == true) {
                            gmImageA = volOpacityObj.getGradMagA();
                            gmImageA.exportData(offset, bufferSize, imageBufferA_GM);
                        }
                    }
                }
                catch (IOException error) {
                    MipavUtil.displayError("" + error); // Need to fix this
                    return false;
                }

                TransferFunction tfRed = volOpacityObj.getOpacityAfn(ViewJComponentHLUTBase.RED);
                TransferFunction tfGreen = volOpacityObj.getOpacityAfn(ViewJComponentHLUTBase.GREEN);
                TransferFunction tfBlue = volOpacityObj.getOpacityAfn(ViewJComponentHLUTBase.BLUE);

                TransferFunction tfRedGM = null;
                TransferFunction tfGreenGM = null;
                TransferFunction tfBlueGM = null;

                if (gmMode)
                {
                    tfRedGM = volOpacityObj.getOpacityGM_Afn(ViewJComponentHLUTBase.RED);
                    tfGreenGM = volOpacityObj.getOpacityGM_Afn(ViewJComponentHLUTBase.GREEN);
                    tfBlueGM = volOpacityObj.getOpacityGM_Afn(ViewJComponentHLUTBase.BLUE);
                }

                pixGM_R = 0;
                pixGM_G = 0;
                pixGM_B = 0;
                opacityValA_R = 0;
                opacityValA_G = 0;
                opacityValA_B = 0;
                for (index = 0, j = 0; j < imageSize; index += 4, j++) {
                    // Apply opacity filter after the gradient magnitude filter.
                    // Take the gradient magnitude result into the opacity transfer function.
                    // Commented out for testing.
                    if (tfRed != null && tfGreen != null && tfBlue != null ) {
                        opacityValA_R = MipavMath.round(tfRed.getRemappedValue(imageBufferA[index+1], 255));
                        opacityValA_G = MipavMath.round(tfGreen.getRemappedValue(imageBufferA[index+2], 255));
                        opacityValA_B = MipavMath.round(tfBlue.getRemappedValue(imageBufferA[index+3], 255));
                    }

                    if (gmMode) {
                        pixGM_R = tfRedGM.getRemappedValue(imageBufferA_GM[index+1], 256) / 255.0f;
                        pixGM_G = tfGreenGM.getRemappedValue(imageBufferA_GM[index+2], 256) / 255.0f;
                        pixGM_B = tfBlueGM.getRemappedValue(imageBufferA_GM[index+3], 256) / 255.0f;
                    }
                    else {
                        pixGM_R = 1;
                        pixGM_G = 1;
                        pixGM_B = 1;
                    }

                    opacityValA_Avg = (int) ( ( (opacityValA_R * pixGM_R) +
                                                (opacityValA_G * pixGM_G) +
                                                (opacityValA_B * pixGM_B)) * 0.333333f + 0.5f);

                    if (RGBTA != null) {
                        if (RGBTA.getROn()) {
                            redMapped = (RGBIndexBufferA[ (int) ( (imageBufferA[index + 1] +
                                offsetAR) * normColorA)] & 0x00ff0000) >> 16;
                        }
                        else {
                            redMapped = 0;
                        }
                        if (RGBTA.getGOn()) {
                            greenMapped = (RGBIndexBufferA[ (int) ( (imageBufferA[index + 2] +
                                offsetAG) * normColorA)] & 0x0000ff00) >> 8;
                        }
                        else {
                            greenMapped = 0;
                        }
                        if (RGBTA.getBOn()) {
                            blueMapped = (RGBIndexBufferA[ (int) ( (imageBufferA[index + 3] +
                                offsetAB) * normColorA)] & 0x000000ff);
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


                    paintBuffer[j] = ( (int) opacityValA_Avg << 24) |
                        ( (int) redMapped << 16) | ( (int) greenMapped << 8) | (int) blueMapped;
                } // end of for (index=0, j=0; j < imageSize; index += 4, j++)
                texture.setImageComponent(sliceZ);
            }

        } // end of if (imageB == null )
        else { // imageB != null
            for (int sliceZ = 0; sliceZ < imageA.getExtents()[2]; sliceZ++) {
                paintBuffer = texture.getBufferedRaster(sliceZ);
                offset = timeSlice * xDim * yDim * zDim + sliceZ * bufferSize;
                try {
                    if (imageA != null) {
                        imageA.exportData(offset, bufferSize, imageBufferA);
                        if ( volOpacityObj.getGradMagA() != null) {
                            if (imageBufferA_GM == null) {
                                imageBufferA_GM = new float[bufferSize];
                            }

                            if (gmMode == true) {
                                gmImageA = volOpacityObj.getGradMagA();
                                gmImageA.exportData(offset, bufferSize, imageBufferA_GM);
                            }
                        }
                    }

                    imageB.exportData(offset, bufferSize, imageBufferB);
                    if ( volOpacityObj.getGradMagB() != null) {
                        if (imageBufferB_GM == null) {
                            imageBufferB_GM = new float[bufferSize];
                        }

                        if (volOpacityObj.getGradMagB() != null) {
                            if (gmMode == true) {
                                gmImageB = volOpacityObj.getGradMagB();
                                gmImageB.exportData(offset, bufferSize, imageBufferB_GM);
                            }
                        }
                    }
                }
                catch (IOException error) {
                    MipavUtil.displayError("" + error); // Need to fix this
                    return false;
                }

                TransferFunction tfRedA = volOpacityObj.getOpacityAfn(ViewJComponentHLUTBase.RED);
                TransferFunction tfGreenA = volOpacityObj.getOpacityAfn(ViewJComponentHLUTBase.GREEN);
                TransferFunction tfBlueA = volOpacityObj.getOpacityAfn(ViewJComponentHLUTBase.BLUE);

                TransferFunction tfRedGMA = null;
                TransferFunction tfGreenGMA = null;
                TransferFunction tfBlueGMA = null;

                TransferFunction tfRedB = volOpacityObj.getOpacityBfn(ViewJComponentHLUTBase.RED);
                TransferFunction tfGreenB = volOpacityObj.getOpacityBfn(ViewJComponentHLUTBase.GREEN);
                TransferFunction tfBlueB = volOpacityObj.getOpacityBfn(ViewJComponentHLUTBase.BLUE);

                TransferFunction tfRedGMB = null;
                TransferFunction tfGreenGMB = null;
                TransferFunction tfBlueGMB = null;

                if (gmMode)
                {
                    tfRedGMA = volOpacityObj.getOpacityGM_Afn(ViewJComponentHLUTBase.RED);
                    tfGreenGMA= volOpacityObj.getOpacityGM_Afn(ViewJComponentHLUTBase.GREEN);
                    tfBlueGMA = volOpacityObj.getOpacityGM_Afn(ViewJComponentHLUTBase.BLUE);
                }

                pixGM_R = 0;
                pixGM_G = 0;
                pixGM_B = 0;
                opacityValA_R = 0;
                opacityValA_G = 0;
                opacityValA_B = 0;
                imgBpixGM_R = 0;
                imgBpixGM_G = 0;
                imgBpixGM_B = 0;
                opacityValB_R = 0;
                opacityValB_G = 0;
                opacityValB_B = 0;

                for (index = 0, j = 0; j < imageSize; index += 4, j++) {
                    opacityValA_R = MipavMath.round(tfRedA.getRemappedValue(imageBufferA[index+1], 255));
                    opacityValA_G = MipavMath.round(tfGreenA.getRemappedValue(imageBufferA[index+2], 255));
                    opacityValA_B = MipavMath.round(tfBlueA.getRemappedValue(imageBufferA[index+3], 255));

                    if (gmMode) {
                        pixGM_R = tfRedGMA.getRemappedValue(imageBufferA_GM[index+1], 256) / 255.0f;
                        pixGM_G = tfGreenGMA.getRemappedValue(imageBufferA_GM[index+2], 256) / 255.0f;
                        pixGM_B = tfBlueGMA.getRemappedValue(imageBufferA_GM[index+3], 256) / 255.0f;
                    }
                    else {
                        pixGM_R = 1;
                        pixGM_G = 1;
                        pixGM_B = 1;

                    }
                    opacityValA_Avg = (int) ( ( (opacityValA_R * pixGM_R) +
                                                (opacityValA_G * pixGM_G) +
                                                (opacityValA_B * pixGM_B)) * 0.333333f + 0.5f);

                    opacityValB_R = MipavMath.round(tfRedB.getRemappedValue(imageBufferB[index+1], 255));
                    opacityValB_G = MipavMath.round(tfGreenB.getRemappedValue(imageBufferB[index+2], 255));
                    opacityValB_B = MipavMath.round(tfBlueB.getRemappedValue(imageBufferB[index+3], 255));

                    if (gmMode) {
                        imgBpixGM_R = tfRedGMB.getRemappedValue(imageBufferB_GM[index+1], 256) / 255.0f;
                        imgBpixGM_G = tfGreenGMB.getRemappedValue(imageBufferB_GM[index+2], 256) / 255.0f;
                        imgBpixGM_B = tfBlueGMB.getRemappedValue(imageBufferB_GM[index+3], 256) / 255.0f;
                    }
                    else {
                        imgBpixGM_R = 1;
                        imgBpixGM_G = 1;
                        imgBpixGM_B = 1;
                    }

                    opacityValB_Avg = (int) ( ( (opacityValB_R * imgBpixGM_R) +
                                                (opacityValB_G * imgBpixGM_G) +
                                                (opacityValB_B * imgBpixGM_B))
                                             * 0.33333333f + 0.5f);

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

                    paintBuffer[j] = ( ( (int) (opacityValA_Avg * alphaBlend + opacityValB_Avg * alphaPrime)) << 24)
                        | ( (int) Ra << 16)
                        | ( (int) Ga << 8)
                        | (int) Ba;
                }
                texture.setImageComponent(sliceZ);
            } // end of for (index=0, j=0; j < imageSize; index += 4, j++)
        } // end of else for imageB != null

        RGBIndexBufferA = null;
        RGBIndexBufferB = null;
        gmImageA = null;
        gmImageB = null;
        return true;
    }

}
