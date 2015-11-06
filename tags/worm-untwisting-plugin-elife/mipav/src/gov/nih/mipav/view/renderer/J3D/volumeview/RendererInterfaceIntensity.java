package gov.nih.mipav.view.renderer.J3D.volumeview;


/**
 * An interface to be used to intensity-based Renderer implementations.
 */

public interface RendererInterfaceIntensity {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Return indication as to whether or not the input image data has been specified yet.
     *
     * @return  boolean True if the input image data has been specified.
     */
    boolean hasInputData();

    /**
     * Return indication as to whether or not a LUT to use for mapping input intensity values to colors has been
     * specified.
     *
     * @return  boolean True if such a LUT has been defined.
     */
    boolean hasInputMap();

    /**
     * Specify the input volume to use for rendering. The image data stored in order of slice indices, each slice stored
     * in row-major order. That is, slice z=0 is stored first, slice z=1 is stored next, and so on. In slice z=0, the
     * y=0 row is stored first, the y=1 row is stored next, and so on.
     *
     * @param  acImageB  byte[] Array of byte intensity values for volume.
     * @param  acImageA  byte[] Array of byte alpha values for volume.
     */
    void setInput(byte[] acImageB, byte[] acImageA);

    /**
     * Specify the lookup table to use for mapping input intensity values to colors.
     *
     * @param  kMap  Look up table for mapping the intensity values to colors.
     */
    void setInputMap(RendererMapIntensity kMap);
}
