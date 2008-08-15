package gov.nih.mipav.view.renderer.J3D.volumeview;


/**
 * An interface to be used to color-based Renderer implementations.
 */

public interface RendererInterfaceColor {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Return indication as to whether or not the input image data has been specified yet.
     *
     * @return  boolean True if the input image data has been specified.
     */
    boolean hasInputData();

    /**
     * Return indication as to whether or not a map has been defined for mapping input colors to intensity.
     *
     * @return  boolean True if such a map has been defined.
     */
    boolean hasInputMap();

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
    void setInput(byte[] acImageR, byte[] acImageG, byte[] acImageB, byte[] acImageA);

    /**
     * Specify the lookup table to use for mapping input RGB colors to intensity.
     *
     * @param  kMap  Look up table for mapping the color values to intensity.
     */
    void setInputMap(RendererMapColor kMap);
}
