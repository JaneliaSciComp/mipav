package gov.nih.mipav.view;


import java.awt.image.*;


/**
 * DOCUMENT ME!
 */
public class ViewJFilterAnimate extends RGBImageFilter {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int brightness;

    /** DOCUMENT ME! */
    private float contrast;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ViewJFilterAnimate object.
     *
     * @param   brightness  DOCUMENT ME!
     * @param   contrast    DOCUMENT ME!
     *
     * @throws  IllegalArgumentException  DOCUMENT ME!
     */
    public ViewJFilterAnimate(int brightness, float contrast) {

        if ((brightness < -255) || (brightness > 255)) {
            throw new IllegalArgumentException("Bad brightness argument");
        } else if ((contrast < 0.0f) || (contrast > 255.0f)) {
            throw new IllegalArgumentException("Bad contrast argument");
        }

        this.brightness = brightness;
        this.contrast = contrast;
        canFilterIndexColorModel = true;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int brightness() {
        return brightness;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  brightness  DOCUMENT ME!
     */
    public void brightness(int brightness) {
        this.brightness = brightness;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float contrast() {
        return contrast;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  contrast  DOCUMENT ME!
     */
    public void contrast(float contrast) {
        this.contrast = contrast;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   x    DOCUMENT ME!
     * @param   y    DOCUMENT ME!
     * @param   rgb  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int filterRGB(int x, int y, int rgb) {
        DirectColorModel cm = (DirectColorModel) ColorModel.getRGBdefault();

        int alpha = cm.getAlpha(rgb);
        int red = cm.getRed(rgb);
        int green = cm.getGreen(rgb);
        int blue = cm.getBlue(rgb);

        red = Math.max(0, Math.min((int) (brightness + (red * contrast)), 255));
        green = Math.max(0, Math.min((int) (brightness + (green * contrast)), 255));
        blue = Math.max(0, Math.min((int) (brightness + (blue * contrast)), 255));

        alpha = alpha << 24;
        red = red << 16;
        green = green << 8;

        return alpha | red | green | blue;
    }
}
