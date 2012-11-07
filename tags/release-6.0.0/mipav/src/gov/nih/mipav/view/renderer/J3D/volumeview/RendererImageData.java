package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.JPanelVolOpacityRGB;
import gov.nih.mipav.view.renderer.J3D.*;

import java.io.IOException;
import java.util.Arrays;

import javax.vecmath.Vector3f;


/**
 * Storage of values for a 3D volume used by the volume renderers.
 */
public class RendererImageData {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final Vector3f m_kZeroVector = new Vector3f(0.0f, 0.0f, 0.0f);

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private byte[] m_acImageA = null;

    /** DOCUMENT ME! */
    private byte[] m_acImageB = null;

    /** DOCUMENT ME! */
    private byte[] m_acImageG = null;

    /** DOCUMENT ME! */
    private byte[] m_acImageR = null;

    /** DOCUMENT ME! */
    private float[] m_afData = null;

    /** Dimensions of the volume. */
    private final int m_iNumVoxels;

    /** DOCUMENT ME! */
    private final int m_iSizeX;

    /** DOCUMENT ME! */
    private final int m_iSizeY;

    /** DOCUMENT ME! */
    private final int m_iSizeZ;

    /** DOCUMENT ME! */
    private ModelImage m_kImage = null;

    /** DOCUMENT ME! */
    private ModelSimpleImage m_kImageGradMag = null;

    /** RGB intensity map for color images only. */
    private ModelRGB m_kIntensityRGB = null;

    /** RGB opacity map for color images only. */
    private ModelRGB m_kOpacityRGB = null;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Constructor to setup renderer image data associated with specified 3D volume.
     * 
     * @param kImage ModelImage Input volume.
     */
    public RendererImageData(final ModelImage kImage) {
        m_kImage = kImage;
        m_iSizeX = kImage.getExtents()[0];
        m_iSizeY = kImage.getExtents()[1];
        m_iSizeZ = kImage.getExtents()[2];
        m_iNumVoxels = m_iSizeX * m_iSizeY * m_iSizeZ;
        m_afData = new float[ (kImage.isColorImage() ? 4 : 1) * m_iNumVoxels];
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Clean up memory.
     */
    public void disposeLocal() {
        m_kImage = null;
        m_acImageR = null;
        m_acImageG = null;
        m_acImageB = null;
        m_acImageA = null;
        m_afData = null;
        m_kImageGradMag = null;
        m_kIntensityRGB = null;
        m_kOpacityRGB = null;
    }

    /**
     * Update the data for the intensity-based color mapped image.
     * 
     * @param kRayTracer Renderer Used to renderer the ray traced image.
     * @param kLUT ModelLUT Input color map definition.
     * @param transferFunction int[] Input intensity opacity map definition
     * @param kOpacityGradMag ViewJComponentVolOpacity Input gradient magnitude opacity map (optional by specifying a
     *            null reference).
     * @param iTimeSlice int Current time slice of data in the volume.
     * @param bUpdatedTimeSlice boolean Flag set if time slice is different.
     * @param bUpdatedLut boolean Flag set if color map definition is different.
     * @param bForceShow boolean Flag set if the color mapping must be performed even if nothing else changed.
     * 
     * @return boolean True if renderer is up-to-date; false if an error occurred.
     */
    public boolean updateRenderer(final Renderer kRayTracer, final ModelLUT kLUT,
            final TransferFunction transferFunction, final ViewJComponentVolOpacity kOpacityGradMag,
            final int iTimeSlice, final boolean bUpdatedTimeSlice, final boolean bUpdatedLut, final boolean bForceShow) {

        try {

            // Get the time slice of data from the volume.
            if (bUpdatedTimeSlice || !kRayTracer.hasInputData() || kRayTracer.reloadInputData()) {
                m_kImage.exportData(iTimeSlice * m_afData.length, m_afData.length, m_afData);
            }

            // Access gradient magnitude image if it is defined.
            if ( (null != kOpacityGradMag) && (null == m_kImageGradMag)) {
                m_kImageGradMag = new ModelSimpleImage(kOpacityGradMag.getImage());
            }

            // For color-based renderers
            if (kRayTracer instanceof RendererInterfaceColor) {
                final RendererInterfaceColor kRayTracerColor = (RendererInterfaceColor) kRayTracer;

                // Update the RGBA channels for the color-based renderers which
                // derive their data from color and opacity mapping of input
                // intensity values.
                if (bUpdatedTimeSlice || bUpdatedLut || bForceShow || !kRayTracer.hasInputData()) {

                    if (null == m_acImageR) {
                        m_acImageR = new byte[m_iNumVoxels];
                    }

                    if (null == m_acImageG) {
                        m_acImageG = new byte[m_iNumVoxels];
                    }

                    if (null == m_acImageB) {
                        m_acImageB = new byte[m_iNumVoxels];
                    }

                    if (null == m_acImageA) {
                        m_acImageA = new byte[m_iNumVoxels];
                    }

                    computeImageBytes(m_afData, transferFunction, kLUT);

                    if (null != m_kImageGradMag) {
                        modulateAlphaImageBytes(m_kImageGradMag.data, kOpacityGradMag.getOpacityTransferFunction());
                    }

                    kRayTracerColor.setInput(m_acImageR, m_acImageG, m_acImageB, m_acImageA);
                }

                // Compute normals from gradient of intensity.
                if (kRayTracer.usesNormals()) {

                    if (bUpdatedTimeSlice || !kRayTracer.hasNormals()) {
                        kRayTracer.setNormals(RenderViewBase.getNormals());
                        // kRayTracer.setNormals(createImageNormals(m_afData));
                    }
                }
            }

            // For intensity-based renderers
            else if (kRayTracer instanceof RendererInterfaceIntensity) {
                final RendererInterfaceIntensity kRayTracerIntensity = (RendererInterfaceIntensity) kRayTracer;

                // Specify the input color map for intensity-based renderers.
                if (bUpdatedLut || !kRayTracerIntensity.hasInputMap()) {
                    kRayTracerIntensity.setInputMap(new RendererMapIntensity(kLUT));
                }

                // Update input byte and alpha channels for intensity-based renderers.
                if (bUpdatedTimeSlice || bForceShow || !kRayTracer.hasInputData()) {

                    if (null == m_acImageB) {
                        m_acImageB = new byte[m_iNumVoxels];
                    }

                    if (null == m_acImageA) {
                        m_acImageA = new byte[m_iNumVoxels];
                    }

                    computeImageBytes(m_afData, transferFunction);

                    if (null != m_kImageGradMag) {
                        modulateAlphaImageBytes(m_kImageGradMag.data, kOpacityGradMag.getOpacityTransferFunction());
                    }

                    kRayTracerIntensity.setInput(m_acImageB, m_acImageA);
                }
            }
        } catch (final IOException error) {
            MipavUtil.displayError("" + error);

            return false;
        }

        return true;
    }

    /**
     * Update the data for the ARGB color image.
     * 
     * @param kRayTracer Renderer Used to renderer the ray traced image.
     * @param kIntensityRGB ModelRGB Input transfer function for each RGB channel.
     * @param volOpacityObj image opacity histogram reference.
     * @param iTimeSlice int Current time slice of data in the volume.
     * @param bUpdatedTimeSlice boolean Flag set if time slice is different.
     * @param bForceShow boolean Flag set if the color mapping must be performed even if nothing else changed.
     * 
     * @return boolean True if renderer is up-to-date; false if an error occurred.
     */
    public boolean updateRenderer(final Renderer kRayTracer, final ModelRGB kIntensityRGB,
            final JPanelVolOpacityRGB volOpacityObj, final int iTimeSlice, final boolean bUpdatedTimeSlice,
            final boolean bForceShow) {

        try {
            final RendererInterfaceColor kRayTracerColor = (RendererInterfaceColor) kRayTracer;

            // Get the time slice of data from the volume.
            if (bUpdatedTimeSlice || !kRayTracer.hasInputData() || kRayTracer.reloadInputData()) {
                m_kImage.exportData(iTimeSlice * m_afData.length, m_afData.length, m_afData);
            }

            // Compute a default map for colors to intensity.
            final RendererMapColor kMapColorToIntensity = new RendererMapColor();
            kMapColorToIntensity.setScales(1.0f, 1.0f, 1.0f);

            // Assign this map if not already done so.
            if ( !kRayTracerColor.hasInputMap()) {
                kRayTracerColor.setInputMap(kMapColorToIntensity);
            }

            // Compute normals from gradient of intensity.
            if (kRayTracer.usesNormals()) {

                if (bUpdatedTimeSlice || !kRayTracer.hasNormals()) {

                    float[] afIntensity = new float[m_iNumVoxels];

                    for (int i = 0; i < m_iNumVoxels; i++) {
                        afIntensity[i] = kMapColorToIntensity.mapValue(m_afData[ (4 * i) + 1], m_afData[ (4 * i) + 2],
                                m_afData[ (4 * i) + 3]);
                    }

                    kRayTracer.setNormals(RenderViewBase.getNormals());

                    // kRayTracer.setNormals(createImageNormals(afIntensity));
                    afIntensity = null;
                }
            }

            // Update the RGBA channels for the color-based renderers which
            // derive their data from opacity mapping of input colors.
            if (bUpdatedTimeSlice || bForceShow || !kRayTracer.hasInputData()) {

                if (null == m_acImageR) {
                    m_acImageR = new byte[m_iNumVoxels];
                }

                if (null == m_acImageG) {
                    m_acImageG = new byte[m_iNumVoxels];
                }

                if (null == m_acImageB) {
                    m_acImageB = new byte[m_iNumVoxels];
                }

                if (null == m_acImageA) {
                    m_acImageA = new byte[m_iNumVoxels];
                }

                // If the input intensity map for the color image is not
                // defined, then define one here. If it is defined and
                // changes, then remember the one specified.
                if ( (m_kIntensityRGB != kIntensityRGB) && (null != kIntensityRGB)) {
                    m_kIntensityRGB = kIntensityRGB;
                } else if (null == m_kIntensityRGB) {
                    m_kIntensityRGB = new ModelRGB(new int[] {4, 256});
                }

                computeImageBytes(m_afData, m_kIntensityRGB, kMapColorToIntensity, volOpacityObj);

                kRayTracerColor.setInput(m_acImageR, m_acImageG, m_acImageB, m_acImageA);
            }
        } catch (final IOException error) {
            MipavUtil.displayError("" + error);

            return false;
        }

        return true;
    }

    /**
     * Calls dispose.
     * 
     * @throws Throwable DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        this.disposeLocal();
        super.finalize();
    }

    /**
     * Given an intensity volume and opacity map, fill in the byte and alpha values for each voxel.
     * 
     * @param afData float[] Input array of voxel intensity values.
     * @param transferFunction int[] Input opacity map to apply to values.
     */
    private void computeImageBytes(final float[] afData, final TransferFunction transferFunction) {

        float fImageMax = Float.NEGATIVE_INFINITY;
        float fImageMin = Float.MAX_VALUE;

        if (m_kImage.getType() == ModelStorageBase.UBYTE) {
            fImageMin = 0;
            fImageMax = 255;
        } else if (m_kImage.getType() == ModelStorageBase.BYTE) {
            fImageMin = -128;
            fImageMax = 127;
        } else {

            if ( (fImageMin == Float.MAX_VALUE) || (fImageMax == Float.NEGATIVE_INFINITY)) {

                for (final float element : afData) {

                    if (element > fImageMax) {
                        fImageMax = element;
                    } else if (element < fImageMin) {
                        fImageMin = element;
                    }
                }
            }
        }

        float fRange = fImageMax - fImageMin;

        if (fRange == 0) {
            fRange = 1.0f;
        }

        final float fScale = 255.0f / fRange;

        for (int i = 0; i < afData.length; i++) {
            final int iValue = (int) ( (afData[i] - fImageMin) * fScale);
            m_acImageB[i] = (byte) iValue;
            m_acImageA[i] = (byte) transferFunction.getRemappedValue(afData[i], 256);
        }
    }

    /**
     * Given an intensity volume, opacity map, and color map, fill in the byte red, green, blue, and alpha values for
     * each voxel.
     * 
     * @param afData float[] Input array of voxel intensity values.
     * @param transferFunction int[] Input opacity map to apply to values.
     * @param kModelLut ModelLUT Input color map to apply to values.
     */
    private void computeImageBytes(final float[] afData, final TransferFunction transferFunction,
            final ModelLUT kModelLut) {

        // create a LUT we can easily map with
        final RendererMapIntensity kLut = new RendererMapIntensity(kModelLut);

        float fImageMax = Float.NEGATIVE_INFINITY;
        float fImageMin = Float.MAX_VALUE;

        if (m_kImage.getType() == ModelStorageBase.UBYTE) {
            fImageMin = 0;
            fImageMax = 255;
        } else if (m_kImage.getType() == ModelStorageBase.BYTE) {
            fImageMin = -128;
            fImageMax = 127;
        } else {

            if ( (fImageMin == Float.MAX_VALUE) || (fImageMax == Float.NEGATIVE_INFINITY)) {

                for (final float element : afData) {

                    if (element > fImageMax) {
                        fImageMax = element;
                    } else if (element < fImageMin) {
                        fImageMin = element;
                    }
                }
            }
        }

        float fRange = fImageMax - fImageMin;

        if (fRange == 0) {
            fRange = 1.0f;
        }

        final float fScale = 255.0f / fRange;

        for (int i = 0; i < afData.length; i++) {
            final int iValue = (int) ( (afData[i] - fImageMin) * fScale);
            final int iRGB = kLut.mapValue(iValue);
            m_acImageR[i] = (byte) ( (iRGB >> 16) & 0x0ff);
            m_acImageG[i] = (byte) ( (iRGB >> 8) & 0x0ff);
            m_acImageB[i] = (byte) ( (iRGB) & 0x0ff);
            m_acImageA[i] = (byte) transferFunction.getRemappedValue(afData[i], 256);
        }
    }

    /**
     * Given an RGB volume, a map from RGB to intensity, and an opacity map, fill in the RGB byte and alpha values for
     * each voxel.
     * 
     * @param afData float[] Input array of voxel ARGB channel values interleaved.
     * @param kIntensityRGB ModelRGB Input transfer function for each RGB channel.
     * @param kMapColorToIntensity RendererMapColor Map to convert RGB channel valeus to intensity.
     * @param volOpacityObj opacity map to apply to values.
     */
    private void computeImageBytes(final float[] afData, final ModelRGB kIntensityRGB,
            final RendererMapColor kMapColorToIntensity, final JPanelVolOpacityRGB volOpacityObj) {

        // Setup bits mask to enable which colors can be selected for rendering.
        final int iColorMask = 0xff000000 | (kIntensityRGB.getROn() ? 0x00ff0000 : 0)
                | (kIntensityRGB.getGOn() ? 0x0000ff00 : 0) | (kIntensityRGB.getBOn() ? 0x000000ff : 0);

        // Access the intensity map for the color imge channels.
        int[] aiIntensityRGB = kIntensityRGB.exportIndexedRGB();

        final int iNumVoxels = afData.length / 4;

        float redMapped, greenMapped, blueMapped;

        // Even through the ARGB image is stored in a float array,
        // each component value should be in the [0,255] range.
        int iIndex = 0;

        float pixGM_R = 0, pixGM_G = 0, pixGM_B = 0;
        int opacityValA_R = 0, opacityValA_G = 0, opacityValA_B = 0;

        float maxColorA = 255;
        float normColorA = 1;
        float offsetAR = 0.0f;
        float offsetAG = 0.0f;
        float offsetAB = 0.0f;

        final boolean gmMode = volOpacityObj.isGradientMagnitudeOpacityEnabled();

        if (m_kImage.getType() == ModelStorageBase.ARGB_USHORT) {
            maxColorA = (float) m_kImage.getMaxR();
            maxColorA = Math.max((float) m_kImage.getMaxG(), maxColorA);
            maxColorA = Math.max((float) m_kImage.getMaxB(), maxColorA);
        } else if (m_kImage.getType() == ModelStorageBase.ARGB_FLOAT) {

            if (m_kImage.getMinR() < 0.0) {
                maxColorA = (float) (m_kImage.getMaxR() - m_kImage.getMinR());
                offsetAR = (float) ( -m_kImage.getMinR());
            } else {
                maxColorA = (float) m_kImage.getMaxR();
            }

            if (m_kImage.getMinG() < 0.0) {
                maxColorA = Math.max((float) (m_kImage.getMaxG() - m_kImage.getMinG()), maxColorA);
                offsetAG = (float) ( -m_kImage.getMinG());
            } else {
                maxColorA = Math.max((float) m_kImage.getMaxG(), maxColorA);
            }

            if (m_kImage.getMinB() < 0.0) {
                maxColorA = Math.max((float) (m_kImage.getMaxB() - m_kImage.getMinB()), maxColorA);
                offsetAB = (float) ( -m_kImage.getMinB());
            } else {
                maxColorA = Math.max((float) m_kImage.getMaxB(), maxColorA);
            }
        }

        normColorA = 255 / maxColorA;

        final TransferFunction tfRed = volOpacityObj.getOpacityAfn(ViewJComponentHLUTBase.RED);
        final TransferFunction tfGreen = volOpacityObj.getOpacityAfn(ViewJComponentHLUTBase.GREEN);
        final TransferFunction tfBlue = volOpacityObj.getOpacityAfn(ViewJComponentHLUTBase.BLUE);

        TransferFunction tfRedGM = null;
        TransferFunction tfGreenGM = null;
        TransferFunction tfBlueGM = null;

        if (gmMode) {
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

        for (int i = 0; i < iNumVoxels; i++) {

            // Get the original sample values for each channel.
            int iA = (int) afData[iIndex++];
            int iR = (int) afData[iIndex++];
            int iG = (int) afData[iIndex++];
            int iB = (int) afData[iIndex++];

            if ( (tfRed != null) && (tfGreen != null) && (tfBlue != null)) {
                opacityValA_R = MipavMath.round(tfRed.getRemappedValue(iR, 255));
                opacityValA_G = MipavMath.round(tfGreen.getRemappedValue(iG, 255));
                opacityValA_B = MipavMath.round(tfBlue.getRemappedValue(iB, 255));
            }

            if (gmMode) {
                pixGM_R = tfRedGM.getRemappedValue(iR, 256) / 255.0f;
                pixGM_G = tfGreenGM.getRemappedValue(iG, 256) / 255.0f;
                pixGM_B = tfBlueGM.getRemappedValue(iB, 256) / 255.0f;
            } else {
                pixGM_R = 1;
                pixGM_G = 1;
                pixGM_B = 1;
            }

            // Clip RGB channels values to be in [0,255] range.
            if (iR < 0) {
                iR = 0;
            } else if (iR > 255) {
                iR = 255;
            }

            if (iG < 0) {
                iG = 0;
            } else if (iG > 255) {
                iG = 255;
            }

            if (iB < 0) {
                iB = 0;
            } else if (iB > 255) {
                iB = 255;
            }

            // Use RGB channel values to determine opacity.
            iA = (int) ( ( ( (opacityValA_R * pixGM_R) + (opacityValA_G * pixGM_G) + (opacityValA_B * pixGM_B)) * 0.333333f) + 0.5f);

            if (iA < 0) {
                iA = 0;
            } else if (iA > 255) {
                iA = 255;
            }

            redMapped = (aiIntensityRGB[(int) ( (iR + offsetAR) * normColorA)] & 0x00ff0000) >> 16;
            greenMapped = (aiIntensityRGB[(int) ( (iG + offsetAG) * normColorA)] & 0x0000ff00) >> 8;
            blueMapped = (aiIntensityRGB[(int) ( (iB + offsetAB) * normColorA)] & 0x000000ff);

            // Create a 32-bit ARGB value combining each channel, but
            // only after mapping the RGB channels through their intensity
            // transfer functions. Apply the mask to allow only those
            // bits enabled to be written.
            int iARGB = (iA << 24) | ((int) redMapped << 16) | ((int) greenMapped << 8) | (int) blueMapped;
            iARGB &= iColorMask;

            // Separate out each channel again into separate byte images.
            m_acImageA[i] = (byte) ( (iARGB & 0xff000000) >> 24);
            m_acImageR[i] = (byte) ( (iARGB & 0x00ff0000) >> 16);
            m_acImageG[i] = (byte) ( (iARGB & 0x0000ff00) >> 8);
            m_acImageB[i] = (byte) (iARGB & 0x000000ff);
        }

        aiIntensityRGB = null;
    }

    /**
     * Create array of normal vectors corresponding to the voxels in the volume. The normal vector is computed based on
     * the gradient of the volume intensity values.
     * 
     * @param afData float[] Input array of voxel values.
     * 
     * @return DOCUMENT ME!
     */
    private Vector3f[] createImageNormals(final float[] afData) {

        final Vector3f[] akNormal = new Vector3f[afData.length];
        Arrays.fill(akNormal, RendererImageData.m_kZeroVector);

        final int iXBound = m_iSizeX;
        final int iYBound = m_iSizeY;
        final int iZBound = m_iSizeZ;
        final int iXYBound = iXBound * iYBound;

        // normals from gradient which are computed using central finite
        // differences everywhere except forward/backward finite differences
        // are used at the edges
        final int iOffX = 1;
        final int iOffY = iXBound;
        final int iOffZ = iXBound * iYBound;
        int iX, iY, iZ;

        for (iZ = 0; iZ < iZBound; iZ++) {
            final boolean bMinZ = 0 == iZ;
            final boolean bMaxZ = (iZBound - 1) == iZ;

            for (iY = 0; iY < iYBound; iY++) {
                final boolean bMinY = 0 == iY;
                final boolean bMaxY = (iYBound - 1) == iY;
                final int offset = iXBound * (iY + (iYBound * iZ));

                for (iX = 0; iX < iXBound; iX++) {
                    final boolean bMinX = 0 == iX;
                    final boolean bMaxX = (iXBound - 1) == iX;

                    final int i = iX + offset;

                    final float fDX = (bMinX ? afData[i] : afData[i - iOffX]) - (bMaxX ? afData[i] : afData[i + iOffX]);
                    final float fDY = (bMinY ? afData[i] : afData[i - iOffY]) - (bMaxY ? afData[i] : afData[i + iOffY]);
                    final float fDZ = (bMinZ ? afData[i] : afData[i - iOffZ]) - (bMaxZ ? afData[i] : afData[i + iOffZ]);

                    if ( (fDX != 0.0f) || (fDY != 0.0f) || (fDZ != 0.0f)) {
                        akNormal[i] = new Vector3f(fDX, fDY, fDZ);
                        akNormal[i].normalize();
                    }
                }
            }
        }

        // Catch any zero-vector normals and replace them by an average of
        // neighboring normals.
        for (iZ = 1; iZ < (iZBound - 1); iZ++) {

            for (iY = 1; iY < (iYBound - 1); iY++) {
                final int offset = iXBound * (iY + (iYBound * iZ));

                for (iX = 1; iX < (iXBound - 1); iX++) {

                    final int i = iX + offset;

                    if ( (afData[i] != 0.0f) && (akNormal[i] == RendererImageData.m_kZeroVector)) {
                        akNormal[i] = new Vector3f(0.0f, 0.0f, 0.0f);
                        akNormal[i].add(akNormal[i - 1]);
                        akNormal[i].add(akNormal[i + 1]);
                        akNormal[i].add(akNormal[i - iXBound]);
                        akNormal[i].add(akNormal[i + iXBound]);
                        akNormal[i].add(akNormal[i - iXYBound]);
                        akNormal[i].add(akNormal[i + iXYBound]);
                        akNormal[i].normalize();
                    }
                }
            }
        }

        return akNormal;
    }

    /**
     * Given a single value volume and opacity map, modulate the alpha channel values for each value.
     * 
     * @param afData float[] Input array of voxel values.
     * @param transferFunction int[] Input opacity map to apply to values.
     */
    private void modulateAlphaImageBytes(final float[] afData, final TransferFunction transferFunction) {

        float fImageMax = Float.NEGATIVE_INFINITY;
        float fImageMin = Float.MAX_VALUE;

        if ( (fImageMin == Float.MAX_VALUE) || (fImageMax == Float.NEGATIVE_INFINITY)) {

            for (final float element : afData) {

                if (element > fImageMax) {
                    fImageMax = element;
                } else if (element < fImageMin) {
                    fImageMin = element;
                }
            }
        }

        float fRange = fImageMax - fImageMin;

        if (fRange == 0) {
            fRange = 1.0f;
        }

        final float fScale = 255.0f / fRange;

        for (int i = 0; i < afData.length; i++) {
            final int iValue = (int) ( (afData[i] - fImageMin) * fScale);
            m_acImageA[i] = (byte) ( (transferFunction.getRemappedValue(afData[i], 256)) * (m_acImageA[i] & 0xFF) / 255);
        }
    }

    /**
     * Given an RGB value volume and opacity map for each channel, modulate the alpha channel values for each value.
     * 
     * @param afData float[] Input array of voxel values.
     * @param kMapColorToIntensity RendererMapColor Map to convert RGB channel valeus to intensity.
     * @param kOpacityRGB ModelRGB Input opacity map to apply to values.
     */
    private void modulateAlphaImageBytes(final float[] afData, final RendererMapColor kMapColorToIntensity,
            final ModelRGB kOpacityRGB) {

        // Access the opacity map for the color image channels.
        final int[] aiOpacityRGB = kOpacityRGB.exportIndexedRGB();

        final int iNumVoxels = afData.length / 4;

        // Even through the ARGB image is stored in a float array,
        // each component value should be in the [0,255] range.
        int iIndex = 0;

        for (int i = 0; i < iNumVoxels; i++) {

            int iA = (int) afData[iIndex++];
            int iR = (int) afData[iIndex++];
            int iG = (int) afData[iIndex++];
            int iB = (int) afData[iIndex++];

            if (iR < 0) {
                iR = 0;
            } else if (iR > 255) {
                iR = 255;
            }

            if (iG < 0) {
                iG = 0;
            } else if (iG > 255) {
                iG = 255;
            }

            if (iB < 0) {
                iB = 0;
            } else if (iB > 255) {
                iB = 255;
            }

            iA = (int) kMapColorToIntensity.mapValue( (aiOpacityRGB[iR] & 0x00ff0000) >> 16,
                    (aiOpacityRGB[iG] & 0x0000ff00) >> 8, (aiOpacityRGB[iB] & 0x000000ff));

            if (iA < 0) {
                iA = 0;
            } else if (iA > 255) {
                iA = 255;
            }

            m_acImageA[i] = (byte) (iA * (m_acImageA[i] & 0xFF) / 255);
        }
    }
}
