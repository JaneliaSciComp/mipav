package gov.nih.mipav.model.structures;

import WildMagic.LibFoundation.Mathematics.Vector2f;

import gov.nih.mipav.view.*;

import java.awt.*;


/**
 * Model of a RGB Table. 256 length red, green, and blue tables are constructed from the red, green, and blue transfer
 * functions. A 256 indexedRGB integer array is returned by exportIndexedRGB with alpha always equal to 255 in the most
 * significant byte, followed by red, green, and blue in the following bytes with the red, green , and blue values able
 * to vary from 0 to 255.
 *
 * @version  1.0
 * @author   Matthew J. McAuliffe, Ph.D.
 * @author   William Gandler
 */

public class ModelRGB extends ModelStorageBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -718098067552795698L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Function that maps the blue function of the LUT. */
    private TransferFunction blueLine = new TransferFunction();

    /** Flag indicating whether the blue channel should be displayed. */
    private boolean BOn = true;

    /** Flag indicating whether the green channel should be displayed. */
    private boolean GOn = true;

    /** Function that maps the green function of the LUT. */
    private TransferFunction greenLine = new TransferFunction();

    /** DOCUMENT ME! */
    private int[] indexedRGB = null; // Special int array where the RGB table

    /** Number of colors in the LUT. */
    private int nColors = 256;

    /** Function that maps the red function of the LUT. */
    private TransferFunction redLine = new TransferFunction();

    /** Flag indicating whether the red channel should be displayed. */
    private boolean ROn = true;

    // is packed with alpha in the most
    // significant byte of the int, followed
    // red, green, and blue.

    /** The X coordinates of the transfer functions. */
    private float[] x = new float[256]; // I don't expect transfer function to have > 256 points


    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * constructor.
     *
     * @param  dimExtents  array indicating RGB table extent in each dimension (e.g. 4x256)
     */
    public ModelRGB(int[] dimExtents) {
        super(ModelStorageBase.FLOAT, dimExtents);

        makeGrayTransferFunctions();
        makeRGB(256);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Creates the R, G, and B transfer functions to produce a gray scale LUT.
     */
    public void evenDistributionFunctions() {

        float[] x = null;
        float[] y = null;
        float[] z = null;

        int nPts = getRedFunction().size();

        try {
            x = new float[nPts];
            y = new float[nPts];
            z = new float[nPts];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelLUT:makeGrayTransferFunctions");

            return;
        }

        int height;

        height = getExtents()[1];

        x[0] = 0;
        y[0] = height - 1;
        z[0] = 0;

        for (int i = 1; i < (nPts - 1); i++) {
            x[i] = (float) (height - 1) * ((float) i / (float) (nPts - 1));
            y[i] = (height - 1) - (((height - 1) * (float) (i)) / (float) (nPts - 1));
            z[i] = 0;
        }

        x[nPts - 1] = height - 1;
        y[nPts - 1] = 0;
        z[nPts - 1] = 0;

        redLine.importArrays(x, y, nPts);
        greenLine.importArrays(x, y, nPts);
        blueLine.importArrays(x, y, nPts);

    }

    /**
     * This is a method to export a special int array where the alpha is stored in the most significant byte, then red,
     * green and blue.
     *
     * @return  indexedRGB location to which the exporting takes place
     */
    public final int[] exportIndexedRGB() {

        return (indexedRGB);
    }

    /**
     * Accessor that returns the blue transfer function.
     *
     * @return  the transfer function that describes how to map the blue values
     */
    public TransferFunction getBlueFunction() {
        return blueLine;
    }

    /**
     * Accessor that returns the blue transfer function.
     *
     * @return  the transfer function that describes how to map the blue values
     */
    public void setBlueFunction(TransferFunction fn) {
        blueLine = fn;
    }

    /**
     * Returns the boolean indicating if the blue channel should be displayed.
     *
     * @return  BOn = true then display the blue channel (default = true)
     */
    public final boolean getBOn() {
        return BOn;
    }

    /**
     * Gets a specific index of the RGB table.
     *
     * @param   index  index of the RGB table, normally 0-255
     *
     * @return  RGBcolor color at index
     */
    public Color getColor(int index) {
        int r, g, b;

        r = getInt(1, index); // get red   channel
        g = getInt(2, index); // get green channel
        b = getInt(3, index); // get blue  channel

        return (new Color(r, g, b));
    }

    /**
     * Returns the boolean indicating if the green channel should be displayed.
     *
     * @return  GOn = true then display the green channel (default = true)
     */
    public final boolean getGOn() {
        return GOn;
    }

    /**
     * Accessor that returns the green transfer function.
     *
     * @return  the transfer function that describes how to map the green values
     */
    public TransferFunction getGreenFunction() {
        return greenLine;
    }

    /**
     * Accessor that returns the green transfer function.
     *
     * @return  the transfer function that describes how to map the green values
     */
    public void setGreenFunction(TransferFunction fn) {
        greenLine = fn;
    }

    /**
     * Accessor that returns the red transfer function.
     *
     * @return  the transfer function that describes how to map the red values
     */
    public TransferFunction getRedFunction() {
        return redLine;
    }

    /**
     * Accessor that returns the red transfer function.
     *
     * @return  the transfer function that describes how to map the red values
     */
    public void setRedFunction(TransferFunction fn) {
        redLine = fn;
    }

    /**
     * Returns the boolean indicating if the red channel should be displayed.
     *
     * @return  RaOn = true then display the red channel (default = true)
     */
    public final boolean getROn() {
        return ROn;
    }

    /**
     * Creates the R, G, and B transfer functions to produce a gray scale LUT.
     */
    public void makeGrayTransferFunctions() {

        float[] x = null;
        float[] y = null;
        float[] z = null;

        try {
            x = new float[3];
            y = new float[3];
            z = new float[3];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelLUT:makeGrayTransferFunctions");

            return;
        }

        int height;

        height = getExtents()[1];

        x[0] = 0;
        y[0] = height - 1;
        z[0] = 0;

        x[1] = (height - 1) / 2.0f;
        y[1] = (height - 1) / 2.0f;
        z[1] = 0;

        x[2] = height - 1;
        y[2] = 0;
        z[2] = 0;

        redLine.importArrays(x, y, 3);
        greenLine.importArrays(x, y, 3);
        blueLine.importArrays(x, y, 3);

    }


    /**
     * Creates the R, G, and B transfer functions to produce a horizontal linear scale LUT.
     */
    public void makeHorizonTransferFunctions() {

        float[] x = null;
        float[] y = null;
        float[] z = null;

        try {
            x = new float[3];
            y = new float[3];
            z = new float[3];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelLUT:makeGrayTransferFunctions");

            return;
        }

        int height;

        height = getExtents()[1];

        x[0] = 0;
        y[0] = 0;
        z[0] = 0;

        x[1] = (height - 1) / 2.0f;
        y[1] = 0;
        z[1] = 0;

        x[2] = height - 1;
        y[2] = 0;
        z[2] = 0;

        redLine.importArrays(x, y, 3);
        greenLine.importArrays(x, y, 3);
        blueLine.importArrays(x, y, 3);
    }

    /**
     * Special RGB table to be used to display java image. Assumes RGB values that range between (0 and 255) are stored
     * in the RGB table;
     */
    public void makeIndexedRGB() {
        int index;
        int height;
        int r, g, b;

        height = getExtents()[1]; // Get the height of RGB array;

        if ((indexedRGB == null) || (indexedRGB.length != height)) {

            try {
                indexedRGB = new int[height]; // RGB table to be used to display image
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: ModelRGB: makeIndexedRGB");

                return;
            }
        }

        for (index = 0; index < height; index++) {
            r = (int) (getFloat(1, index) + 0.5);
            g = (int) (getFloat(2, index) + 0.5);
            b = (int) (getFloat(3, index) + 0.5);

            indexedRGB[index] = (255 << 24) | (r << 16) | (g << 8) | b; // set Java's alpha value always to 255
        }
    }

    /**
     * This method uses the R, G, B transfer functions to build the desired RGB tables.
     *
     * @param  _nColors  indicates the number of colors to used in the RGB table.
     */
    public void makeRGB(int _nColors) {

        float red, green, blue;
        int i, j;
        int height;
        float step;
        float[] r, g, b;

        if ((_nColors > 0) && (_nColors <= 256)) {
            nColors = _nColors;
        } else {
            nColors = 256;
        }

        height = getExtents()[1]; // number of entries in the LUT (i.e. 256)

        try {
            r = new float[height];
            g = new float[height];
            b = new float[height];

            indexedRGB = new int[height];

            // Calculate LUT values per band from its corresponding transfer function.
            calcRGBBand(redLine, r);
            calcRGBBand(greenLine, g);
            calcRGBBand(blueLine, b);
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelRGB: makeRGB");

            return;
        }

        // remap RGB table based on the number of colors
        step = (float) height / nColors;
        j = 0;
        red = r[0];
        green = g[0];
        blue = b[0];

        for (i = 0; i < height; i++) {

            if (i > (j * step)) {
                red = r[i];
                green = g[i];
                blue = b[i];
                j++;
            }

            set(1, i, red);
            set(2, i, green);
            set(3, i, blue);
        }

        // make special Java RGB table that is an int array where MSB is alpha, and then red, green
        // and blue follow;
        makeIndexedRGB();
    }

    /**
     * Sets if the blue channel to the boolean value.
     *
     * @param  blueOn  if true blue channel should be displayed.
     */
    public final void setBOn(boolean blueOn) {
        BOn = blueOn;
    }

    /**
     * Sets a specific index of the RGB table with the given color.
     *
     * @param  index     index of the RGB table, normally 0-255
     * @param  RGBcolor  color to be placed at the index
     */
    public void setColor(int index, Color RGBcolor) {

        set(1, index, RGBcolor.getRed()); // set red channel
        set(2, index, RGBcolor.getGreen()); // set green channel
        set(3, index, RGBcolor.getBlue()); // set blue channel
        indexedRGB[index] = (255 << 24) | (RGBcolor.getRed() << 16) | (RGBcolor.getGreen() << 8) | RGBcolor.getBlue();

    }

    /**
     * Sets if the green channel to the boolean value.
     *
     * @param  greenOn  if true green channel should be displayed.
     */
    public final void setGOn(boolean greenOn) {
        GOn = greenOn;
    }

    /**
     * Sets if the red channel to the boolean value.
     *
     * @param  redOn  if true red channel should be displayed.
     */
    public final void setROn(boolean redOn) {
        ROn = redOn;
    }

    /**
     * Calculates the color band (i.e. red, green, blue) for the LUT using the the corresponding transfer function
     *
     * @param  function  the band's transfer function
     * @param  band      storage location after conversion from transfer function to the band
     */
    private void calcRGBBand(TransferFunction function, float[] band) {

        int i, j;
        int height = getExtents()[1]; // number of entries in the LUT (i.e. 256)

        for (j = 0; j < function.size(); j++) {
            x[j] = ((Vector2f) (function.getPoint(j))).X;
        }

        for (i = 0; i < height; i++) {
            band[i] = (int) (function.getRemappedValue(i, 256) + 0.5f);
        }
    }

}
