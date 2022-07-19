package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;

import java.awt.*;


/**
 * Component LUT - the vertical or horizontal bar that gives a visual representation of the lookup table.
 *
 * @version  0.1 Oct 29, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      JFrameHistogram
 */

public class ViewJComponentLUTTable extends ViewJComponentBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4862175635665317557L;

    /** Create horizontal LUT. */
    public static final int HORIZONTAL = 0;

    /** Create vertical LUT. */
    public static final int VERTICAL = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Offset to draw the tick mark and value label. */
    protected int offsetX = 60;

    /** DOCUMENT ME! */
    protected int offsetY = 0;

    /** control points coordinate of LUT transfer function . */
    protected float[] x = new float[100];

    /** DOCUMENT ME! */
    protected int[] xN = new int[100];

    /** DOCUMENT ME! */
    protected float[] y = new float[100];

    /** DOCUMENT ME! */
    protected int[] yN = new int[100];

    /** DOCUMENT ME! */
    protected float[] z = new float[100];

    /** DOCUMENT ME! */
    protected int[] zN = new int[100];

    /** DOCUMENT ME! */
    private ModelImage image;

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
     * @param  _image        DOCUMENT ME!
     * @param  _LUT          Model that the image will be filter through to produce a viewable image
     * @param  _LUTDims      The LUT dimensions
     * @param  _orientation  Flag for the orientation, HORIZONTAL or VERTICAL.
     */
    public ViewJComponentLUTTable(ModelImage _image, ModelLUT _LUT, Dimension _LUTDims, int _orientation) {
        super(_LUTDims);
        image = _image;
        LUT = _LUT;
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
     * Paints the image and border.
     *
     * @param  g  Graphics handle
     */
    public void paintComponent(Graphics g) {

        int nPts;
        int value, startValue, endValue;
        int x0, y0;
        float min, max;

        if (image.getType() == ModelStorageBase.UBYTE) {
            min = 0;
            max = 255;
        } else if (image.getType() == ModelStorageBase.BYTE) {
            min = -128;
            max = 127;
        } else {
            min = (float) image.getMin();
            max = (float) image.getMax();
        }

        try {

            if (g == null) {
                return;
            }

            if (img != null) {
                g.setClip(getVisibleRect());
               
                              
                if (getInterpMode() == SMOOTH) {
                    g.drawImage(img, 0, 0, null);
                } else {
                    g.drawImage(img, 20, 0, Math.round(zoomX * img.getWidth(this) * resolutionX) - 70,
                                Math.round(zoomY * img.getHeight(this) * resolutionY), 0, 0, img.getWidth(this) - 70,
                                img.getHeight(this), null);
                }

                nPts = LUT.getTransferFunction().size();
                LUT.getTransferFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    float range = (float) (max - min);

                    if (range == 0) {
                        range = 1;
                    }

                    // remap data into display coordinates
                    xN[i] = (int) (((x[i] - min) / range * 255) + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                endValue = 255 + offsetY - yN[nPts - 1]; // 255
                startValue = 255 + offsetY - yN[0]; // 0

                //g.setColor(Color.white);
                //g.fillRect(offsetX, offsetY, 256, 256);

                for (int i = 0; i < nPts; i++) { // draw tick marks and values
                    x0 = offsetX;
                    y0 = yN[i];
                    drawLine(g, x0, y0);
                    value = 255 + offsetY - yN[i];

                    if (i == (nPts - 1)) {
                        x0 = offsetX;
                        y0 = yN[i] + 10;

                        if (value >= endValue) {
                            y0 = yN[i] + 15;
                        }

                        drawString(g, MipavUtil.makeFloatString(x[i], 2), x0, y0);
                    } else if (i == (nPts - 2)) {
                        x0 = offsetX;
                        y0 = yN[i] + 10;

                        if (value >= endValue) {
                            y0 = yN[i] + 15;
                           
                            drawString(g, MipavUtil.makeFloatString(x[i], 2), x0, y0);
                            drawString(g, " to ", x0, y0 + 14);
                            drawString(g, MipavUtil.makeFloatString(x[i + 1], 2), x0, y0 + 28);

                            break;
                        } else {
                            drawString(g, MipavUtil.makeFloatString(x[i], 2), x0, y0);
                        }
                    } else if (i == 1) {
                        x0 = offsetX;
                        y0 = yN[i] + 10;

                        if (value <= startValue) {
                            y0 = yN[i] + 3;
                            drawString(g, MipavUtil.makeFloatString(x[i - 1], 2), x0, y0);
                            drawString(g, " to ", x0, y0 - 14);
                            drawString(g, MipavUtil.makeFloatString(x[i], 2), x0, y0 - 28);

                        } else {
                            drawString(g, MipavUtil.makeFloatString(x[i], 2), x0, y0);
                        }
                    } else if (i == 0) {
                        x0 = offsetX;
                        y0 = yN[i];

                        if (value <= startValue) {
                            y0 = yN[i] + 3;
                        }

                        drawString(g, MipavUtil.makeFloatString(x[i], 2), x0, y0);

                    } else {
                        x0 = offsetX;
                        y0 = yN[i] + 10;

                        if (value <= startValue) {
                            y0 = yN[i] + 3;
                        }

                        drawString(g, MipavUtil.makeFloatString(x[i], 2), x0, y0);
                    }
                }

            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentBase.paintComponent.");
        }
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

                        /*
                         * if ( x > 90 ) { pixBuffer[y * xDim + x] = 255; } else { pixBuffer[y * xDim + x] =
                         * LUTIndexBuffer[yDim - 1 - y]; }
                         */
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

    /**
     * DOCUMENT ME!
     *
     * @param  g  DOCUMENT ME!
     * @param  x  DOCUMENT ME!
     * @param  y  DOCUMENT ME!
     */
    private void drawLine(Graphics g, int x, int y) {
        g.setColor(Color.black);

        /*
         * g.drawLine(offsetX - 30, y0 + 1, offsetX - 33, y0 + 1); g.drawLine(offsetX - 30, y0 - 1, offsetX - 33, y0 -
         * 1); g.drawLine(offsetX - 31, y0, offsetX - 34, y0); g.drawLine(offsetX - 29, y0, offsetX - 32, y0);
         * g.setColor(Color.white); */
        g.drawLine(x + 3, y, x, y);

    }

    /**
     * Draw the string with the given position.
     *
     * @param  g    Graphics
     * @param  str  String
     * @param  x    int
     * @param  y    int
     */
    private void drawString(Graphics g, String str, int x, int y) {
        g.setColor(Color.black);

        /*
         * g.drawString(str, 5 + x, y - 5); g.drawString(str, 5 + x, y - 6); g.drawString(str, 5 + x, y - 4);
         * g.drawString(str, 6 + x, y - 5); g.drawString(str, 4 + x, y - 5);
         */
        // g.setColor(Color.white);
        g.drawString(str, 5 + x, y - 5);

    }

    /**
     *   Paints the image and border  @param g   Graphics handle
     */

    /*
     * public void paintComponent(Graphics g) {
     *
     * try { if (g == null) {return;} //setDebugGraphicsOptions(DebugGraphics.LOG_OPTION);
     * //setDebugGraphicsOptions(DebugGraphics.FLASH_OPTION); //setDebugGraphicsOptions(DebugGraphics.BUFFERED_OPTION);
     * if (img != null) { g.setClip(getVisibleRect()); if (getInterpMode() == SMOOTH) { g.drawImage(img, 0, 0, null); }
     * else { g.drawImage(img, 0, 0, Math.round(zoomX*img.getWidth(this)*resolutionX)-80,
     * Math.round(zoomY*img.getHeight(this)*resolutionY), 0, 0, img.getWidth(this)-80, img.getHeight(this), null); }
     *
     * g.setFont(MipavUtil.font12); string = "ruida"; if ((int)(zoomX*imageDim.width+0.5)-40 > 0 && string != null){
     * g.setColor(Color.black); g.drawString(string, 5+offsetX, (int)(zoomY*resolutionY*imageDim.height+0.5)-5);
     * g.drawString(string, 5+offsetX, (int)(zoomY*resolutionY*imageDim.height+0.5)-6); g.drawString(string, 5+offsetX,
     * (int)(zoomY*resolutionY*imageDim.height+0.5)-4); g.drawString(string, 6+offsetX,
     * (int)(zoomY*resolutionY*imageDim.height+0.5)-5); g.drawString(string, 4+offsetX,
     * (int)(zoomY*resolutionY*imageDim.height+0.5)-5); g.setColor(Color.white); g.drawString(string, 5+offsetX,
     * (int)(zoomY*resolutionY*imageDim.height+0.5)-5); }
     *
     * } } catch (OutOfMemoryError error ) { System.gc(); MipavUtil.displayError("Out of memory:
     * ComponentBase.paintComponent."); } }
     */
}
