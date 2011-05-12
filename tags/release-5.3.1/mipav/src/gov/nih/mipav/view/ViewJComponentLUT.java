package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;

import java.awt.*;


/**
 * Component LUT - the vertical or horizontal bar that gives a visual representation of the lookup table.
 *
 * @version  0.1 Oct 29, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      ViewJFrameHistoLUT
 */

public class ViewJComponentLUT extends ViewJComponentBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5027534629757769708L;

    /** Create horizontal LUT. */
    public static final int HORIZONTAL = 0;

    /** Create vertical LUT. */
    public static final int VERTICAL = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelLUT LUT;

    /** DOCUMENT ME! */
    private Dimension LUTDims;

    /** DOCUMENT ME! */
    private int[] LUTIndexBuffer = null;

    /** DOCUMENT ME! */
    private int orientation;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new component LUT.
     *
     * @param  _LUT          Model that the image will be filter through to produce a viewable image
     * @param  _LUTDims      The LUT dimensions
     * @param  _orientation  Flag for the orientation, HORIZONTAL or VERTICAL.
     */
    public ViewJComponentLUT(ModelLUT _LUT, Dimension _LUTDims, int _orientation) {
        super(_LUTDims);

        this.LUT = _LUT;
        LUTDims = _LUTDims;
        LUTIndexBuffer = new int[1];

        setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
        orientation = _orientation;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Sets the variables to null and gets rid of the frame.
     */
    public void dispose() {

        LUTIndexBuffer = null;

        if (LUT != null) {
            LUT = null;
        }

    }

    /**
     * Returns the model LUT.
     *
     * @return  The LUT
     */
    public ModelLUT getModelLUT() {
        return LUT;
    }

    /**
     * Filters the model LUT through the LUT and produces an int array (aRGB) that can be convert to a java image for
     * display purposes.
     *
     * @param  lut  LUT to filter with
     */
    public void show(ModelLUT lut) {

        int x, y;
        int xDim, yDim;
        int bufferSize;
        int[] pixBuffer;

        if ((lut == null) && (LUT == null)) {
            return;
        }

        if (lut != null) {
            LUT = lut;
        }

        try {

            if (orientation == HORIZONTAL) {
                xDim = LUT.getExtents()[1];
                yDim = LUTDims.width;

                if (xDim != LUTIndexBuffer.length) {
                    LUTIndexBuffer = new int[xDim];
                }

                LUT.exportIndexedLUT(LUTIndexBuffer);

                bufferSize = xDim * yDim;
                pixBuffer = new int[bufferSize];

                for (y = 0; y < yDim; y++) {

                    for (x = 0; x < xDim; x++) {
                        pixBuffer[(y * xDim) + x] = LUTIndexBuffer[x];
                    }
                }
            } else {
                xDim = LUTDims.width;
                yDim = LUT.getExtents()[1];

                if (yDim != LUTIndexBuffer.length) {
                    LUTIndexBuffer = new int[yDim];
                }

                LUT.exportIndexedLUT(LUTIndexBuffer);

                bufferSize = xDim * yDim;
                pixBuffer = new int[bufferSize];

                for (y = 0; y < yDim; y++) {

                    for (x = 0; x < xDim; x++) {
                        pixBuffer[(y * xDim) + x] = LUTIndexBuffer[yDim - 1 - y];
                    }
                }

            }

            setSliceString(null);
            importImage(pixBuffer); // Method in parent class to import the image
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("ComponentLUT.show: Out of memory");
        }

        paintComponent(getGraphics());
    }

    /**
     * Calls paintComponent - reduces flicker.
     *
     * @param  g  Graphics
     */
    public void update(Graphics g) {
        this.paintComponent(g);
    }
}
