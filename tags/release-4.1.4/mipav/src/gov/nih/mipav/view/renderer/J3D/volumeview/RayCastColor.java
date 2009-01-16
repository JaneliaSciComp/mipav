package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.model.structures.*;

import javax.vecmath.*;


/**
 * A ray tracer for 3D color images.
 */

public abstract class RayCastColor extends RayCastRenderer implements RendererInterfaceColor {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected byte[] m_acImageA = null;

    /** DOCUMENT ME! */
    protected byte[] m_acImageB = null;

    /** DOCUMENT ME! */
    protected byte[] m_acImageG = null;

    /** color channels of the image (R = red, G = green, B = blue, A = alpha). */
    protected byte[] m_acImageR = null;

    /** map to convert RGB channel values to intensity. */
    RendererMapColor m_kMap = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * The constructor for the ray tracer. Currently, the only client of this class is VolumeRenderer.
     *
     * @param  kImage    the 3D image
     * @param  iRBound   the dimension of the square 2D renderer image
     * @param  aiRImage  The rendered image data stored in row-major order. Each integer pixel represents an RGB color
     *                   in the format B | (G << 8) | (R << 16).
     */
    protected RayCastColor(ModelImage kImage, int iRBound, int[] aiRImage) {
        super(kImage, iRBound, aiRImage);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Clean memory.
     *
     * @param  flag  is true call the super.disposeLocal
     */
    public void disposeLocal(boolean flag) {
        System.out.println(" RayCastColor.disposeLocal");

        m_acImageR = null;
        m_acImageG = null;
        m_acImageB = null;
        m_acImageA = null;
        m_kMap = null;

        if (flag == true) {
            super.disposeLocal();
        }
    }

    /**
     * Return indication as to whether or not the input image data has been specified yet.
     *
     * @return  boolean True if the input image data has been specified.
     */
    public boolean hasInputData() {
        return null != m_acImageB;
    }

    /**
     * Return indication as to whether or not a map has been defined for mapping input colors to intensity.
     *
     * @return  boolean True if such a map has been defined.
     */
    public boolean hasInputMap() {
        return null != m_kMap;
    }

    /**
     * Specify the input volume to use for rendering. The image data stored in order of slice indices, each slice stored
     * in row-major order. That is, slice z=0 is stored first, slice z=1 is stored next, and so on. In slice z=0, the
     * y=0 row is stored first, the y=1 row is stored next, and so on.
     *
     * @param  acImageR  byte[] Array of byte red values for volume.
     * @param  acImageG  byte[] Array of byte green values for volume.
     * @param  acImageB  byte[] Array of byte blue values for volume.
     * @param  acImageA  byte[] Array of byte alpha values for volume.
     */
    public void setInput(byte[] acImageR, byte[] acImageG, byte[] acImageB, byte[] acImageA) {

        // Don't just keep pointers to the input arrays.  Actually make a
        // copy of the arrays in case we need to change their values.
        // Do we really need to clone ?? Tagged for CHECKING
        // m_acImageR = (byte[])acImageR.clone();
        // m_acImageG = (byte[])acImageG.clone();
        // m_acImageB = (byte[])acImageB.clone();
        // m_acImageA = (byte[])acImageA.clone();

        m_acImageR = acImageR;
        m_acImageG = acImageG;
        m_acImageB = acImageB;
        m_acImageA = acImageA;

    }

    /**
     * Specify the lookup table to use for mapping input RGB colors to intensity.
     *
     * @param  kMap  Look up table for mapping the color values to intensity.
     */
    public void setInputMap(RendererMapColor kMap) {
        m_kMap = kMap;
    }

    /**
     * Calls dispose.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal(false);
        super.finalize();
    }

    /**
     * Trilinear interpolation of the image to produce image values at points P that are not at the integer voxel
     * locations.
     *
     * @param   kP  the spatial point to be assigned an interpolated value
     *
     * @return  The interpolated color value for the point. If kP is not within the bounding box of the image, a value
     *          of -1 is returned. Each of the RGB colors is stored in 8 bits with red in bits 16-23, green in bits
     *          8-15, and blue in bits 0-7.
     */
    protected final int interpolateColor(Point3f kP) {
        int iX = (int) kP.x;

        if ((iX < 0) || (iX > m_iXBoundM2)) {
            return -1;
        }

        int iY = (int) kP.y;

        if ((iY < 0) || (iY > m_iYBoundM2)) {
            return -1;
        }

        int iZ = (int) kP.z;

        if ((iZ < 0) || (iZ > m_iZBoundM2)) {
            return -1;
        }

        int i000 = iX + (m_iXBound * iY) + (m_iXYProduct * iZ);
        int i100 = i000 + 1;
        int i010 = i000 + m_iXBound;
        int i110 = i100 + m_iXBound;
        int i001 = i000 + m_iXYProduct;
        int i101 = i100 + m_iXYProduct;
        int i011 = i010 + m_iXYProduct;
        int i111 = i110 + m_iXYProduct;

        // compute red channel
        float fF000 = (m_acImageR[i000] & 0x0ff);
        float fF100 = (m_acImageR[i100] & 0x0ff);
        float fF010 = (m_acImageR[i010] & 0x0ff);
        float fF110 = (m_acImageR[i110] & 0x0ff);
        float fF001 = (m_acImageR[i001] & 0x0ff);
        float fF101 = (m_acImageR[i101] & 0x0ff);
        float fF011 = (m_acImageR[i011] & 0x0ff);
        float fF111 = (m_acImageR[i111] & 0x0ff);
        float fDX = kP.x - iX;
        float fDY = kP.y - iY;
        float fDZ = kP.z - iZ;
        float fOmDX = 1.0f - fDX;
        float fOmDY = 1.0f - fDY;
        float fOmDZ = 1.0f - fDZ;
        float fInterp = (fOmDZ *
                             ((fOmDY * ((fOmDX * fF000) + (fDX * fF100))) + (fDY * ((fOmDX * fF010) + (fDX * fF110))))) +
                        (fDZ *
                             ((fOmDY * ((fOmDX * fF001) + (fDX * fF101))) + (fDY * ((fOmDX * fF011) + (fDX * fF111)))));
        int iRed = (int) (fInterp + 0.5f);

        if (iRed < 0) {
            iRed = 0;
        } else if (iRed > 255) {
            iRed = 255;
        }

        // compute green channel
        fF000 = (m_acImageG[i000] & 0x0ff);
        fF100 = (m_acImageG[i100] & 0x0ff);
        fF010 = (m_acImageG[i010] & 0x0ff);
        fF110 = (m_acImageG[i110] & 0x0ff);
        fF001 = (m_acImageG[i001] & 0x0ff);
        fF101 = (m_acImageG[i101] & 0x0ff);
        fF011 = (m_acImageG[i011] & 0x0ff);
        fF111 = (m_acImageG[i111] & 0x0ff);
        fDX = kP.x - iX;
        fDY = kP.y - iY;
        fDZ = kP.z - iZ;
        fOmDX = 1.0f - fDX;
        fOmDY = 1.0f - fDY;
        fOmDZ = 1.0f - fDZ;
        fInterp = (fOmDZ * ((fOmDY * ((fOmDX * fF000) + (fDX * fF100))) + (fDY * ((fOmDX * fF010) + (fDX * fF110))))) +
                  (fDZ * ((fOmDY * ((fOmDX * fF001) + (fDX * fF101))) + (fDY * ((fOmDX * fF011) + (fDX * fF111)))));

        int iGreen = (int) (fInterp + 0.5f);

        if (iGreen < 0) {
            iGreen = 0;
        } else if (iGreen > 255) {
            iGreen = 255;
        }

        // compute blue channel
        fF000 = (m_acImageB[i000] & 0x0ff);
        fF100 = (m_acImageB[i100] & 0x0ff);
        fF010 = (m_acImageB[i010] & 0x0ff);
        fF110 = (m_acImageB[i110] & 0x0ff);
        fF001 = (m_acImageB[i001] & 0x0ff);
        fF101 = (m_acImageB[i101] & 0x0ff);
        fF011 = (m_acImageB[i011] & 0x0ff);
        fF111 = (m_acImageB[i111] & 0x0ff);
        fDX = kP.x - iX;
        fDY = kP.y - iY;
        fDZ = kP.z - iZ;
        fOmDX = 1.0f - fDX;
        fOmDY = 1.0f - fDY;
        fOmDZ = 1.0f - fDZ;
        fInterp = (fOmDZ * ((fOmDY * ((fOmDX * fF000) + (fDX * fF100))) + (fDY * ((fOmDX * fF010) + (fDX * fF110))))) +
                  (fDZ * ((fOmDY * ((fOmDX * fF001) + (fDX * fF101))) + (fDY * ((fOmDX * fF011) + (fDX * fF111)))));

        int iBlue = (int) (fInterp + 0.5f);

        if (iBlue < 0) {
            iBlue = 0;
        } else if (iBlue > 255) {
            iBlue = 255;
        }

        return (iRed << 16) | (iGreen << 8) | iBlue;
    }
}
