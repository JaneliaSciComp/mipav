package gov.nih.mipav.view;

import WildMagic.LibFoundation.Mathematics.Vector2f;

import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.structures.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;


/**
 * This class extends the ViewJComponentHLUTBase class and used to coordinate how a histogram and LUT for an image are
 * to be displayed to the screen. For display purposes, this component has a LUT Model. Note y inversion in transfer
 * segment because graphical origin is in upper left corner.
 *
 * <pre>
             255     ^                __________
                     |               /
                     |              /
                     |             /  <------- Transfer function
                     |            /
              L      |           /
              U      |          /
              T      |         /
                     |        /
                     |       /
                     |______/
              0      |________________________________>

                      min                         max

                              Image intensity
 * </pre>
 *
 * <p>The transfer function ( member of the LUT class) dictates how the image intensity values are converted into the
 * LUT.</p>
 *
 * @version  0.1 Aug 1, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class ViewJComponentHistoLUT extends ViewJComponentHLUTBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1320755809299024115L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    ModelLUT lut;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new component histogram LUT.
     *
     * @param  _histoFrame  Frame where histogram is to be displayed
     * @param  _histo       histogram model
     * @param  _lut         lut used to display histogram
     * @param  _image       image of the displayed histogram and associated LUT
     */
    public ViewJComponentHistoLUT(HistoLUTParent _histoFrame, ModelHistogram _histo, ModelLUT _lut, ModelImage _image) {
        super(_histoFrame, _histo, _image, new Dimension(377, 336));
        lut = _lut;
        // set the default color LUT transfer function to linear. 
        // otherwise, the init transfer function setup messed up. 
        //linearMode(); --- commented back out so that LUTs do not reset.
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void addFunctionPoint(float x, float y) {
        float mx = x - offsetX; // see ViewJComponentHLUTBase
        float my = y - offsetY;
        if(my < 0) {
        	my = 0;
        } else if(my > 255) {
        	my = 255;
        }
        
        if(mx < 0) {
        	mx = 0;
        } else if(mx > 255) {
        	mx = 255;
        }

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

        if ((mode == DUAL_THRESHOLD) || (mode == CT) || (mode == DUAL_THRESHOLD_INV)) {
            return;
        } // Can't add points in these modes

        if (mode == RED) { // Add or remove points to red transfer function

            if ((addPointFlag == true) && (index != -99)) {
                lut.getRedFunction().insertPoint(new Vector2f(mx, my), index + 1);
            }
        } else if (mode == GREEN) {

            if ((addPointFlag == true) && (index != -99)) {
                lut.getGreenFunction().insertPoint(new Vector2f(mx, my), index + 1);
            }
        } else if (mode == BLUE) {

            if ((addPointFlag == true) && (index != -99)) {
                lut.getBlueFunction().insertPoint(new Vector2f(mx, my), index + 1);
            }
        } else if (mode == ALPHA) {

            if ((addPointFlag == true) && (index != -99)) {
                lut.getAlphaFunction().insertPoint(new Vector2f(mx, my), index + 1);
            }
        } else if (mode == LINEAR) {

            // add or remove point to transfer function but first put mouse point into image intensity range
            mx = (float) (min + (mx / 255.0f * (max - min)));

            if ((addPointFlag == true) && (index != -99)) {
                lut.getTransferFunction().insertPoint(new Vector2f(mx, my), index + 1);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    public void checkProximityToTransferFunction(float px, float py, boolean remove) {
        int nPts = 0;
        float mx = px - offsetX;
        float my = py - offsetY;
        float xN, yN, x1N, y1N;
        float min = 0, max = 255;

        if ((mx >= dim.width) || (mx < 0) || (my >= dim.height) || (my < 1)) {
            return;
        }

        if ((mode == LINEAR) || (mode == DUAL_THRESHOLD) || (mode == CT) || (mode == DUAL_THRESHOLD_INV)) {
            lut.getTransferFunction().exportArrays(x, y);
            nPts = lut.getTransferFunction().size();

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

            // min = (float)(image.getMin());
            // max = (float)(image.getMax());
        } else if (mode == RED) {
            lut.getRedFunction().exportArrays(x, y);
            nPts = lut.getRedFunction().size();
            min = 0;
            max = 255.0f;
        } else if (mode == GREEN) {
            lut.getGreenFunction().exportArrays(x, y);
            nPts = lut.getGreenFunction().size();
            min = 0;
            max = 255.0f;
        } else if (mode == BLUE) {
            lut.getBlueFunction().exportArrays(x, y);
            nPts = lut.getBlueFunction().size();
            min = 0;
            max = 255.0f;
        } else if (mode == ALPHA) {
            lut.getAlphaFunction().exportArrays(x, y);
            nPts = lut.getAlphaFunction().size();
            min = 0;
            max = 255.0f;
        }

        for (int i = 0; i < nPts; i++) {
            xN = (int) (((x[i] - min) / (max - min) * 255) + 0.5);
            yN = (int) (y[i] + 0.5);

            if (MipavMath.distance(mx, xN, my, yN) < 5) { // Are we close to a point ??

                if (remove) { // are we in going to remove this point

                    if ((mode == DUAL_THRESHOLD) || (mode == DUAL_THRESHOLD_INV)) {
                        return;
                    }

                    // setCursor(new Cursor(Cursor.E_RESIZE_CURSOR));
                    setCursor(resizeCursor);
                    addPointFlag = false;
                    index = i; // save index to point

                    return;
                } else {
                    setCursor(moveCursor);
                    addPointFlag = false;
                    index = i; // save index to point

                    return;
                }
            }
        }

        for (int i = 0; i < (nPts - 1); i++) {
            xN = (int) (((x[i] - min) / (max - min) * 255) + 0.5);
            yN = (int) (y[i] + 0.5);
            x1N = (int) (((x[i + 1] - min) / (max - min) * 255) + 0.5);
            y1N = (int) (y[i + 1] + 0.5);

            if ((MipavMath.distance(mx, xN, my, yN) + MipavMath.distance(mx, x1N, my, y1N) -
                     MipavMath.distance(xN, x1N, yN, y1N)) < 0.5) { // Are we close to a line.

                if ((MipavMath.distance(mx, xN, my, yN) > 5) && (MipavMath.distance(mx, x1N, my, y1N) > 5)) {
                    setCursor(handCursor);
                    addPointFlag = true;
                    index = i; // save index to point after which to insert a point

                    return;
                }
            } else {
                addPointFlag = false;
                index = -99;
            }
        }

        setCursor(crosshairCursor);
        addPointFlag = false;
        index = -99;
    }

    /**
     * Sets mode to CT and sets range to CT presets.
     *
     * @param  preset1  first CT preset
     * @param  preset2  second CT preset
     */
    public void ctMode(int preset1, int preset2) {

        mode = CT;

        float min, max;
        float yVal, m, b;

        min = (float) image.getMin();
        max = (float) image.getMax();

        x[0] = min; // -1024;
        y[0] = dim.height - 1;
        z[0] = 0;

        if (preset2 < max) {
            x[2] = preset2;
        } else {
            x[2] = max;
        }

        y[2] = 0;
        z[2] = 0;

        if (preset1 < min) {

            // y = m * x + b, line equation
            // Assume: pt1 ( preset1, 255 ),  pt2 ( x[2], y[2])
            // find: pt3 ( -1024, yVal);
            m = (255 - y[2]) / (preset1 - x[2]);
            b = 255 - (m * preset1);
            yVal = (m * (-1024)) + b;
            x[1] = -1024;
            y[1] = yVal;
            z[1] = 0;
            //System.out.println("yVal = " + yVal);
        } else {
            x[1] = preset1;
            y[1] = dim.height - 1;
            z[1] = 0;
        }

        if (y[1] > 255) {
            y[1] = 255;
        }

        x[3] = max; // 3071;
        y[3] = 0;
        z[3] = 0;

        lut.getTransferFunction().importArrays(x, y, 4);
        histogramParent.updateFrames(false);
        showHistogram();
    }

    /**
     * Sets variables to null and gets rid of frame.
     */
    public void dispose() {

        // System.out.println( "ViewJComponentHistoLUT disposeLocal" );
        lut = null;
        super.dispose();
    }

    /**
     * Drag a point on the Histo LUT to another value.
     *
     * @param  mouseEvent  the mouse dragged event (passed on from the containing panel)
     */
    public void dragPoint(MouseEvent mouseEvent) {
        int nPts;
        float mx = mouseEvent.getX() - offsetX;
        float my = mouseEvent.getY() - offsetY;
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

        if (mx < 0) {
            mx = 0;
        }

        if (mx >= dim.width) {
            mx = dim.width - 1;
        }

        if (my < 0) {
            my = 0;
        }

        if (my >= dim.height) {
            my = dim.height - 1;
        }

        if (addPointFlag == true) {
            return;
        }

        if (index == -99) {
            return;
        }

        if (mode == RED) { // Drag point of red transfer function
            lut.getRedFunction().exportArrays(x, y);
            nPts = lut.getRedFunction().size();

            setLinearIndex(mx, my);

            lut.getRedFunction().importArrays(x, y, nPts);
            histogramParent.setRangeText(mx, my, index);
            lut.makeLUT(-1); // make LUT and use nColors already set for LUT
            histogramParent.setLUT(lut);

            if (histogramParent.isImageUpdate() == true) {
                histogramParent.updateFrames(false);
            }

            showHistogram();
        }

        if (mode == GREEN) {
            lut.getGreenFunction().exportArrays(x, y);
            nPts = lut.getGreenFunction().size();

            setLinearIndex(mx, my);

            lut.getGreenFunction().importArrays(x, y, nPts);
            histogramParent.setRangeText(mx, my, index);
            lut.makeLUT(-1);
            histogramParent.setLUT(lut);

            if (histogramParent.isImageUpdate() == true) {
                histogramParent.updateFrames(false);
            }

            showHistogram();
        }

        if (mode == BLUE) {
            lut.getBlueFunction().exportArrays(x, y);
            nPts = lut.getBlueFunction().size();

            setLinearIndex(mx, my);

            lut.getBlueFunction().importArrays(x, y, nPts);
            histogramParent.setRangeText(mx, my, index);
            lut.makeLUT(-1);
            histogramParent.setLUT(lut);

            if (histogramParent.isImageUpdate() == true) {
                histogramParent.updateFrames(false);
            }

            showHistogram();
        }

        if (mode == ALPHA) {
            lut.getAlphaFunction().exportArrays(x, y);
            nPts = lut.getAlphaFunction().size();

            setLinearIndex(mx, my);

            lut.getAlphaFunction().importArrays(x, y, nPts);
            histogramParent.setRangeText(mx, my, index);
            lut.makeLUT(-1);
            histogramParent.setLUT(lut);

            if (histogramParent.isImageUpdate() == true) {
                histogramParent.updateFrames(false);
            }

            showHistogram();
        } else if ((mode == LINEAR) || (mode == CT)) {

            mx = (float) (min + (mx / 255.0f * (max - min)));
            lut.getTransferFunction().exportArrays(x, y);
            nPts = lut.getTransferFunction().size();

            setLinearIndex(mx, my);
	            
            lut.getTransferFunction().importArrays(x, y, nPts);
            histogramParent.setRangeText(mx, my, index);

            if (histogramParent.isImageUpdate() == true) {
                histogramParent.updateFrames(false);
            }

            showHistogram();

            return;
        } else if ((mode == DUAL_THRESHOLD) || (mode == DUAL_THRESHOLD_INV)) {
            mx = (float) (min + (mx / 255.0f * (max - min)));
            lut.getTransferFunction().exportArrays(x, y);
            nPts = lut.getTransferFunction().size();

            if (index == 0) {

                if (mode == DUAL_THRESHOLD) {
                    y[0] = my;
                }
            } else if ((index == 1) && (mx < x[3])) {
                x[index] = mx;

                if (mode == DUAL_THRESHOLD) {
                    y[index] = my;
                    y[4] = my;
                }

                /*
                 * else { y[index] = my; y[3] = my;
                 *
                 * //System.err.println("my IS (index 1): " + my);
                 *
                 * //adjust LUT lut.makeGrayTransferFunctions(); // System.err.println("INDEX FOR LUT IS : " + (256 -
                 * ((int)my + 1))); lut.makeThresholdLUT(256 - ((int)my + 1)); for (int i = 0; i < (my + 1); i++) {
                 * lut.setColor((255-i), new Color(200,0,0)); }
                 *
                 * }
                 */
                x[index + 1] = mx;
            } else if ((index == 2) && (mx < x[3])) {
                x[index] = mx;
                x[index - 1] = mx;

                if (mode == DUAL_THRESHOLD_INV) {
                    y[index] = my;
                }
            } else if ((index == 3) && (mx > x[2])) {
                x[3] = mx;
                x[4] = mx;

                if (mode == DUAL_THRESHOLD_INV) {
                    y[3] = my;
                }
            } else if ((index == 4) && (mx > x[2])) {
                x[4] = mx;
                x[3] = mx;

                if (mode == DUAL_THRESHOLD) {
                    y[4] = my;
                    y[1] = my;
                }

            } else if (index == 5) {

                if (mode == DUAL_THRESHOLD) {
                    y[5] = my;
                }
            }

            lut.getTransferFunction().importArrays(x, y, nPts);

            if ((mode == DUAL_THRESHOLD_INV) && ((index == 1) || (index == 3))) {
                histogramParent.updateComponentLUT();
            }

            histogramParent.setRangeText(mx, my, index);

            // update the threshold text fields in the frame with the lower & upper thresholds
            histogramParent.updateThresholdFields(x[1], x[3]);

            if (histogramParent.isImageUpdate() == true) {
                histogramParent.updateFrames(false);
            }

            showHistogram();
        }
    }
    
    /**
     * Checks boundaries for new dragged point in LUT.
     * 
     * @param mx new x value for point based on mouse movement
     * @param my new y value for point based on mouse movement
     */
    private void setLinearIndex(float mx, float my) {
    	if ((index != (lut.getTransferFunction().size() - 1)) && (mx > x[index + 1])) {
            x[index] = x[index + 1];
        } else if ((index != 0) && (mx < x[index - 1])) {
        	x[index] = x[index - 1];
        } else if ((index == (lut.getTransferFunction().size() - 1)) || (index == 0)) {
        	//do not set x[index] point since it should already be a boundary value
        } else {
        	x[index] = mx;
        }
        
        y[index] = my;
    }

    /**
     * {@inheritDoc}
     */
    public void dualThresholdMode(int newMode) {
        float min, max;

        boolean wasSwitched = (thresholdMode != NO_THRESHOLD) && (newMode != thresholdMode);

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

        mode = newMode;
        thresholdMode = newMode;

        if (thresholdMode == DUAL_THRESHOLD_INV) {
            x[0] = min;
            y[0] = 0;
            z[0] = 0;

            if (!wasSwitched) {
                x[1] = (int) (min + ((max - min) / 3.0f));
            }

            y[1] = 0;
            z[1] = 0;

            x[2] = x[1];
            y[2] = dim.height - 1;
            z[2] = 0;

            if (!wasSwitched) {
                x[3] = (min + ((max - min) * 2.0f / 3.0f));
            }

            y[3] = (dim.height - 1) * 2.0f / 3.0f;
            z[3] = 0;

            x[4] = x[3];
            y[4] = 0;
            z[4] = 0;

            x[5] = max - ((max - min) / 255.0f);
            y[5] = 0;
            z[5] = 0;
        } else if (thresholdMode == DUAL_THRESHOLD) {
            x[0] = min;
            y[0] = dim.height - 1;
            z[0] = 0;

            if (!wasSwitched) {
                x[1] = (int) (min + ((max - min) / 3.0f));
            }

            y[1] = (dim.height - 1) * 2.0f / 3.0f;
            z[1] = 0;

            if (!wasSwitched) {
                x[2] = (int) (min + ((max - min) / 3.0f));
            }

            y[2] = 0;
            z[2] = 0;

            if (!wasSwitched) {
                x[3] = (min + ((max - min) * 2.0f / 3.0f));
            }

            y[3] = 0;
            z[3] = 0;

            x[4] = x[3];

            // y[4] = (dim.height-1)*1.0f/3.0f;
            y[4] = y[1];
            z[4] = 0;

            x[5] = max - ((max - min) / 255.0f);
            y[5] = 0;
            z[5] = 0;
        }

        lut.getTransferFunction().importArrays(x, y, 6);

        histogramParent.updateFrames(false);
        showHistogram();
    }

    /**
     * Evenly distributed the controls points of linear transfer function.
     */
    public void evenDistribution() {
        int nPts;

        mode = LINEAR;

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

        nPts = lut.getTransferFunction().size();
        x[0] = min;
        y[0] = dim.height - 1;
        z[0] = 0;

        for (int i = 1; i < (nPts - 1); i++) {
            x[i] = (min + ((max - min) * (float) i / (float) (nPts - 1)));
            y[i] = (dim.height - 1) - (((dim.height - 1) * (float) (i)) / (float) (nPts - 1));
            z[i] = 0;
        }

        x[nPts - 1] = max; // - (max-min)/255.0f;
        y[nPts - 1] = 0;
        z[nPts - 1] = 0;

        lut.getTransferFunction().importArrays(x, y, nPts);

        histogramParent.updateFrames(false);
        showHistogram();

    }

    /**
     * Get the lookup table associated with the image and histogram.
     *
     * @return  returns the LUT used to display the image
     */
    public ModelLUT getLUT() {
        return lut;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getThresholdFill() {
        return (dim.height - y[1] - 1);
    }

    /**
     * {@inheritDoc}
     */
    public TransferFunction getTransferFunction() {
        return lut.getTransferFunction();
    }

    /**
     * Sets mode to linear and shows component.
     */
    public void linearMode() {

        mode = LINEAR;

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

        float xRange = ( ( max - min ) < 255 ? 255 : ( max - min ) );
        
        x[0] = min;
        y[0] = dim.height - 1;
        z[0] = 0;

        x[1] = (min + ( xRange / 3.0f));
        y[1] = (dim.height - 1) - ((dim.height - 1) / 3.0f);
        z[1] = 0;

        x[2] = (min + ( xRange * 2.0f / 3.0f));
        y[2] = (dim.height - 1) - (((dim.height - 1) * 2.0f) / 3.0f);
        z[2] = 0;

        x[3] = max; // - (max-min)/255.0f;
        y[3] = 0;
        z[3] = 0;

        lut.getTransferFunction().importArrays(x, y, 4);
      
        histogramParent.updateFrames(false);
        showHistogram();
    }

    /**
     * Paints the component.
     *
     * @param  g  graphics to paint in
     */
    public void paintComponent(Graphics g) {
        int nPts;
        int yLog;
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

        if (g != null) {
            g.setColor(backgroundColor);
            g.fillRect(0, 0, offsetX - 29, 290 + offsetY);
            g.fillRect(offsetX - 3, 0, 2, 290 + offsetY);
            g.fillRect(0, 277, offsetX + 310, 40);

            super.paintComponent(g);

            g.setFont(MipavUtil.font12B);
            g.setColor(Color.black);
            g.drawRect(offsetX - 1, offsetY - 1, 256 + 1, 256 + 1); // put border around histogram
            g.drawRect(offsetX - 2, offsetY - 2, 256 + 3, 256 + 3); // put border around histogram
            g.setColor(Color.blue.darker());
            g.drawString(String.valueOf("Image Intensities"), offsetX + 80, 307 + offsetY);
            g.drawString("C", offsetX + 310, offsetY + 100);
            g.drawString("o", offsetX + 310, offsetY + 114);
            g.drawString("u", offsetX + 310, offsetY + 128);
            g.drawString("n", offsetX + 310, offsetY + 142);
            g.drawString("t", offsetX + 310, offsetY + 156);

            g.setFont(MipavUtil.font12);

            if (logFlag == true) { // add ticks and values on right side of histogram component (log)
                g.setColor(Color.black);

                double number = 0.5;

                for (int i = 0; i < 4; i++) {

                    // yLog = (int)((Math.exp(histogramMaxLog*number)/histogramMax) * 255 + offsetY);
                    yLog = (int) (255 - (int) ((number * 255) - 0.5) + offsetY);
                    g.drawLine(offsetX + 254, yLog, offsetX + 260, yLog);
                    g.drawString(String.valueOf((int) (Math.exp(histogramMaxLog * number) + 0.5)), offsetX + 265,
                                 yLog + 6);
                    number = number * 0.5;
                }
            } else { // add ticks and values on right side of histogram component - linear scale
                g.setColor(Color.black);
                g.drawLine(offsetX + 254, offsetY + 63, offsetX + 260, offsetY + 63);
                g.drawString(String.valueOf((int) ((histogramMax * 0.75) + 0.5)), offsetX + 265, offsetY + 69);
                g.drawLine(offsetX + 254, offsetY + 127, offsetX + 260, offsetY + 127);
                g.drawString(String.valueOf((int) ((histogramMax * 0.5) + 0.5)), offsetX + 265, offsetY + 133);
                g.drawLine(offsetX + 254, offsetY + 191, offsetX + 260, offsetY + 191);
                g.drawString(String.valueOf((int) ((histogramMax * 0.25) + 0.5)), offsetX + 265, offsetY + 198);
            }

            g.drawString(String.valueOf((int) histogramMax), offsetX + 260, offsetY + 6); // top label

            if ((mode == LINEAR) || (mode == CT)) {
                // g.setColor(new Color(255,255,0));
                g.setColor(Color.yellow);
                nPts = lut.getTransferFunction().size();
                lut.getTransferFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    float range = (float) (max - min);

                    if (range == 0) {
                        range = 1;
                    }

                    // remap data into display coordinates
                    xN[i] = (int) (((x[i] - min) / range * 255) + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts); // draw transfer function
                g.setColor(Color.black);

                for (int i = 0; i < nPts; i++) {
                    g.setColor(Color.black);
                    g.fillRect(xN[i] - 2, yN[i] - 2, 5, 5); // draw points
                    g.setColor(Color.white);
                    g.drawRect(xN[i] - 2, yN[i] - 2, 5, 5);
                }

                g.setColor(Color.black);

                g.drawString(MipavUtil.makeFloatString(min, 2), offsetX - 10, 275 + offsetY);

                if ((nPts % 2) != 0) {
                    g.drawString(MipavUtil.makeFloatString(max, 2), offsetX + 256 - 15, 275 + offsetY);
                } else {
                    g.drawString(MipavUtil.makeFloatString(max, 2), offsetX + 256 - 15, 285 + offsetY);
                }

                // g.setColor(new Color(0,0,200));
                for (int i = 1; i < (nPts - 1); i++) { // draw tick marks and values
                    g.drawLine(offsetX + 3, yN[i], offsetX - 3, yN[i]);
                    g.drawLine(offsetX - 30, yN[i], offsetX - 33, yN[i]);
                    g.drawString(String.valueOf(255 + offsetY - yN[i]), offsetX - 55, yN[i] + 6);

                    g.drawLine(xN[i], 250 + offsetY, xN[i], 260 + offsetY);

                    if ((i % 2) == 0) {
                        g.drawString(MipavUtil.makeFloatString(x[i], 2), xN[i] - 10, 273 + offsetY);
                    } else {
                        g.drawString(MipavUtil.makeFloatString(x[i], 2), xN[i] - 10, 287 + offsetY);
                    }
                }

            } else if ((mode == DUAL_THRESHOLD) || (mode == DUAL_THRESHOLD_INV)) {
                g.setColor(Color.yellow);
                nPts = lut.getTransferFunction().size();

                lut.getTransferFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (((x[i] - min) / (max - min) * 255) + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);
                g.setColor(Color.black);

                for (int i = 0; i < nPts; i++) {
                    g.setColor(Color.black);

                    g.fillRect(xN[i] - 2, yN[i] - 2, 5, 5);
                    g.setColor(Color.white);
                    g.drawRect(xN[i] - 2, yN[i] - 2, 5, 5);
                }

                g.setColor(Color.black);

                g.drawString(MipavUtil.makeFloatString(min, 2), offsetX - 10, 275 + offsetY);

                if ((nPts % 2) != 0) {
                    g.drawString(MipavUtil.makeFloatString(max, 2), offsetX + 256 - 15, 275 + offsetY);
                } else {
                    g.drawString(MipavUtil.makeFloatString(max, 2), offsetX + 256 - 15, 285 + offsetY);
                }

                for (int i = 1; i < 5; i += 3) {
                    g.drawLine(offsetX + 3, yN[i], offsetX - 3, yN[i]);
                    g.drawLine(offsetX - 30, yN[i], offsetX - 33, yN[i]);
                    g.drawString(String.valueOf(255 + offsetY - yN[i]), offsetX - 55, yN[i] + 6);

                    g.drawLine(xN[i], 250 + offsetY, xN[i], 260 + offsetY);

                    if ((i % 2) == 0) {

                        if (!((mode == DUAL_THRESHOLD_INV) && (i == 4))) {
                            g.drawString(MipavUtil.makeFloatString(x[i], 2), xN[i] - 10, 273 + offsetY);
                        }
                    } else {
                        g.drawString(MipavUtil.makeFloatString(x[i], 2), xN[i] - 10, 287 + offsetY);
                    }
                }
            } else if (mode == RED) {
                g.setColor(Color.cyan);
                nPts = lut.getAlphaFunction().size();
                lut.getAlphaFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (x[i] + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);

                g.setColor(Color.green);
                nPts = lut.getGreenFunction().size();
                lut.getGreenFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (x[i] + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);

                g.setColor(Color.blue);
                nPts = lut.getBlueFunction().size();
                lut.getBlueFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (x[i] + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);

                g.setColor(Color.red);
                nPts = lut.getRedFunction().size();
                lut.getRedFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (x[i] + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);

                g.setColor(Color.black);

                for (int i = 0; i < nPts; i++) {
                    g.setColor(Color.black);
                    g.fillRect(xN[i] - 2, yN[i] - 2, 5, 5);
                    g.setColor(Color.white);
                    g.drawRect(xN[i] - 2, yN[i] - 2, 5, 5);
                }

                g.setColor(Color.black);
                g.drawString("0", offsetX - 10, 275 + offsetY);
                g.drawString("255", offsetX + 256 - 15, 275 + offsetY);

                for (int i = 1; i < (nPts - 1); i++) { // draw tick marks and values
                    g.drawLine(offsetX + 3, yN[i], offsetX - 3, yN[i]);
                    g.drawLine(offsetX - 30, yN[i], offsetX - 33, yN[i]);
                    g.drawString(String.valueOf(255 + offsetY - yN[i]), offsetX - 55, yN[i] + 6);
                    g.drawLine(xN[i], 250 + offsetY, xN[i], 260 + offsetY);

                    if ((i % 2) == 0) {
                        g.drawString(String.valueOf(x[i]), xN[i] - 10, 273 + offsetY);
                    } else {
                        g.drawString(String.valueOf(x[i]), xN[i] - 10, 287 + offsetY);
                    }
                }
            } else if (mode == GREEN) {
                g.setColor(Color.cyan);
                nPts = lut.getAlphaFunction().size();
                lut.getAlphaFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (x[i] + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);

                g.setColor(Color.blue);
                nPts = lut.getBlueFunction().size();
                lut.getBlueFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (x[i] + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);

                g.setColor(Color.red);
                nPts = lut.getRedFunction().size();
                lut.getRedFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (x[i] + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);

                g.setColor(Color.green);
                nPts = lut.getGreenFunction().size();
                lut.getGreenFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (x[i] + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);
                g.setColor(Color.black);

                for (int i = 0; i < nPts; i++) {
                    g.setColor(Color.black);
                    g.fillRect(xN[i] - 2, yN[i] - 2, 5, 5);
                    g.setColor(Color.white);
                    g.drawRect(xN[i] - 2, yN[i] - 2, 5, 5);
                }

                g.setColor(Color.black);
                g.drawString("0", offsetX - 10, 275 + offsetY);
                g.drawString("255", offsetX + 256 - 15, 275 + offsetY);

                for (int i = 1; i < (nPts - 1); i++) { // draw tick marks and values
                    g.drawLine(offsetX + 3, yN[i], offsetX - 3, yN[i]);
                    g.drawLine(offsetX - 30, yN[i], offsetX - 33, yN[i]);
                    g.drawString(String.valueOf(255 + offsetY - yN[i]), offsetX - 55, yN[i] + 6);
                    g.drawLine(xN[i], 250 + offsetY, xN[i], 260 + offsetY);

                    if ((i % 2) == 0) {
                        g.drawString(String.valueOf(x[i]), xN[i] - 10, 273 + offsetY);
                    } else {
                        g.drawString(String.valueOf(x[i]), xN[i] - 10, 287 + offsetY);
                    }
                }
            } else if (mode == BLUE) {
                g.setColor(Color.cyan);
                nPts = lut.getAlphaFunction().size();
                lut.getAlphaFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (x[i] + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);

                g.setColor(Color.red);
                nPts = lut.getRedFunction().size();
                lut.getRedFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (x[i] + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);

                g.setColor(Color.green);
                nPts = lut.getGreenFunction().size();
                lut.getGreenFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (x[i] + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);

                g.setColor(Color.blue);
                nPts = lut.getBlueFunction().size();
                lut.getBlueFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (x[i] + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);

                g.setColor(Color.black);

                for (int i = 0; i < nPts; i++) {
                    g.setColor(Color.black);
                    g.fillRect(xN[i] - 2, yN[i] - 2, 5, 5);
                    g.setColor(Color.white);
                    g.drawRect(xN[i] - 2, yN[i] - 2, 5, 5);

                }

                g.setColor(Color.black);
                g.drawString("0", offsetX - 10, 275 + offsetY);
                g.drawString("255", offsetX + 256 - 15, 275 + offsetY);

                for (int i = 1; i < (nPts - 1); i++) { // draw tick marks and values
                    g.drawLine(offsetX + 3, yN[i], offsetX - 3, yN[i]);
                    g.drawLine(offsetX - 30, yN[i], offsetX - 33, yN[i]);
                    g.drawString(String.valueOf(255 + offsetY - yN[i]), offsetX - 55, yN[i] + 6);
                    g.drawLine(xN[i], 250 + offsetY, xN[i], 260 + offsetY);

                    if ((i % 2) == 0) {
                        g.drawString(String.valueOf(x[i]), xN[i] - 10, 273 + offsetY);
                    } else {
                        g.drawString(String.valueOf(x[i]), xN[i] - 10, 287 + offsetY);
                    }
                }

            } else if (mode == ALPHA) {
                g.setColor(Color.blue);
                nPts = lut.getBlueFunction().size();
                lut.getBlueFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (x[i] + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);

                g.setColor(Color.green);
                nPts = lut.getGreenFunction().size();
                lut.getGreenFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (x[i] + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);

                g.setColor(Color.red);
                nPts = lut.getRedFunction().size();
                lut.getRedFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (x[i] + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);

                g.setColor(Color.cyan);
                nPts = lut.getAlphaFunction().size();
                lut.getAlphaFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (x[i] + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);

                g.setColor(Color.black);

                for (int i = 0; i < nPts; i++) {
                    g.setColor(Color.black);
                    g.fillRect(xN[i] - 2, yN[i] - 2, 5, 5);
                    g.setColor(Color.white);
                    g.drawRect(xN[i] - 2, yN[i] - 2, 5, 5);
                }

                g.setColor(Color.black);
                g.drawString("0", offsetX - 10, 275 + offsetY);
                g.drawString("255", offsetX + 256 - 15, 275 + offsetY);

                if ((index >= 0) && (index < nPts)) {
                    g.drawLine(offsetX + 3, yN[index], offsetX - 3, yN[index]);
                    g.drawLine(offsetX - 30, yN[index], offsetX - 33, yN[index]);
                    g.drawString(String.valueOf(255 + offsetY - yN[index]), offsetX - 55, yN[index] + 6);
                    g.drawLine(xN[index], 250 + offsetY, xN[index], 260 + offsetY);
                    g.drawString(String.valueOf(x[index]), xN[index] - 10, 287 + offsetY);
                }
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    public void removeClickedFunctionPoint() {

        if ((mode == DUAL_THRESHOLD) || (mode == CT) || (mode == DUAL_THRESHOLD_INV)) {
            return;
        } // Can't add points in these modes

        if (mode == RED) { // Add or remove points to red transfer function

            if ((addPointFlag == false) && (index != -99)) {
                lut.getRedFunction().removePoint(index);
            }
        } else if (mode == GREEN) {

            if ((addPointFlag == false) && (index != -99)) {
                lut.getGreenFunction().removePoint(index);
            }
        } else if (mode == BLUE) {

            if ((addPointFlag == false) && (index != -99)) {
                lut.getBlueFunction().removePoint(index);
            }
        } else if (mode == ALPHA) {

            if ((addPointFlag == false) && (index != -99)) {
                lut.getAlphaFunction().removePoint(index);
            }
        } else if (mode == LINEAR) {

            // add or remove point to transfer function but first put mouse point into image intensity range
            if ((addPointFlag == false) && (index != -99)) {
                lut.getTransferFunction().removePoint(index);
            }
        }
    }

    /**
     * Change the lookup table associated with the image and histogram.
     *
     * @param  newLUT  the new lut
     */
    public void setLUT(ModelLUT newLUT) {
        lut = newLUT;
    }

    /**
     * {@inheritDoc}
     */
    public void showHistogram(ModelLUT LUT) {

        int i, j, k;
        double sum;
        int end, aRGB;
        int lutHeight;
        int index;
        float ptX1, ptX2, ptY1, ptY2;
        float iNew;
        int offset;
        int success;
        float min, max;

        if (histogram == null) {
            return;
        }

        if ((LUT == null) && (lut == null)) {
            return;
        }

        if (LUT != null) {
            lut = LUT;
        }

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

        lutHeight = lut.getExtents()[1];

        if (lutHeight != lutIndexBuffer.length) {

            try {
                lutIndexBuffer = new int[lutHeight];
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: ComponentHistoLUT.show");
            }
        }

        try { // get histogram
            histogram.exportData(0, histogramBuffer.length, histogramBuffer);
        } catch (IOException error) {
            MipavUtil.displayError("IOException: ComponentHistoLUT.show");

            return;
        }
        // get LUT in special format

        success = lut.exportIndexedLUT(lutIndexBuffer);

        if (success == 0) {
            MipavUtil.displayError("lut.exportIndexedLUT failure: ComponentHistoLUT.show");
        }

        int samples = Math.round(histogramBuffer.length / (float) dim.width);

        // float samples = histogramBuffer.length / (float) dim.width;
        if (samples == 0) {
            samples = 1;
        }

        // find histogram max normalized to the width of the display component
        histogramMax = -9999;

        for (i = 0; i < dim.width; i++) {

            for (sum = 0, k = i * samples; k < ((i + 1) * samples); k++) {

                if (k < histogramBuffer.length) {
                    sum += histogramBuffer[k];
                }
            }

            if (sum > histogramMax) {
                histogramMax = sum;
            }
        }

        if (logFlag == true) {
            histogramMaxLog = Math.log(histogramMax);
        }

        // Builds special buffer by remapping the data in the LUT range
        int lightBlue = 0xFFAFAFF0;
        Arrays.fill(pixBuffer, lightBlue);

        int nPts = lut.getTransferFunction().size();

        for (i = 0; i < dim.width; i++) {

            // if histogram width and component width are not the same then correct
            for (sum = 0, k = i * samples; k < ((i + 1) * samples); k++) {

                if (k < histogramBuffer.length) {
                    sum += histogramBuffer[k];
                }
            }

            if (logFlag == true) {

                if (sum >= 1.0) {
                    end = MipavMath.round((dim.height - 1) * (Math.log(sum) / histogramMaxLog));
                } else {
                    end = 0;
                }
            } else {
                end = MipavMath.round((dim.height - 1) * (sum / histogramMax));
            }

            if ((end == 0) && (sum > 0)) {
                end = 1;
            }


            // put i into the image intensity range -> iNew
            iNew = (float) (min + ((float) i / (dim.width - 1) * (max - min)));
            index = i;

            for (j = 0; j < (nPts - 1); j++) {
                ptX1 = ((Vector2f) (lut.getTransferFunction().getPoint(j))).X;
                ptX2 = ((Vector2f) (lut.getTransferFunction().getPoint(j + 1))).X;
                ptY1 = (dim.height - 1) - ((Vector2f) (lut.getTransferFunction().getPoint(j))).Y;
                ptY2 = (dim.height - 1) - ((Vector2f) (lut.getTransferFunction().getPoint(j + 1))).Y;

                if ((iNew >= ptX1) && (iNew < ptX2)) {

                    // index is a "pointer" into the LUT range - its the color of the histogram bar
                    index = MipavMath.round(ptY1 + (((iNew - ptX1) / (ptX2 - ptX1)) * (ptY2 - ptY1)));
                }
            }

            aRGB = lutIndexBuffer[index];

            // build histogram image
            offset = (dim.width * (dim.height - 1)) + i;

            for (j = 0; j < end; j++) {
                pixBuffer[offset - (j * dim.width)] = aRGB;
            }
        }

        importImage(pixBuffer); // Method in parent class to import the image
        paintComponent(getGraphics());
        repaint();
    }

    /**
     * Update the cursor position when mouse slider is moved.
     *
     * @param  _mx     cursor x postion.
     * @param  _my     cursor y position.
     * @param  _index  cursor index
     */
    public void updateCursor(float _mx, float _my, int _index) {
        int nPts = 0;
        float mx = _mx;
        float my = _my;

        //System.out.println("updateCursor: x = " + _mx + " y = " + _my);

        index = _index;

        /*
         * lut.getTransferFunction().exportArrays( x, y, z ); nPts = lut.getTransferFunction().size();
         *
         * if ( index == lut.getTransferFunction().size() - 1 || index == 0 ) { y[index] = my; } else { x[index] = mx;
         * y[index] = my; }
         */
        if (mode == RED) { // Drag point of red transfer function
            nPts = lut.getRedFunction().size();

            if ((index != (lut.getTransferFunction().size() - 1)) && (mx > x[index + 1])) {
                return;
            }

            if ((index != 0) && (mx < x[index - 1])) {
                return;
            }

            if ((index == (lut.getTransferFunction().size() - 1)) || (index == 0)) {
                y[index] = my;
            } else {
                x[index] = mx;
                y[index] = my;
            }

            lut.getRedFunction().importArrays(x, y, nPts);
            lut.makeLUT(-1); // make LUT and use nColors already set for LUT
            histogramParent.setLUT(lut);

            if (histogramParent.isImageUpdate() == true) {
                histogramParent.updateFrames(false);
            }

            showHistogram();
        }

        if (mode == GREEN) {
            nPts = lut.getGreenFunction().size();

            if ((index != (lut.getTransferFunction().size() - 1)) && (mx > x[index + 1])) {
                return;
            }

            if ((index != 0) && (mx < x[index - 1])) {
                return;
            }

            if ((index == (lut.getTransferFunction().size() - 1)) || (index == 0)) {
                y[index] = my;
            } else {
                x[index] = mx;
                y[index] = my;
            }

            lut.getGreenFunction().importArrays(x, y, nPts);
            lut.makeLUT(-1);
            histogramParent.setLUT(lut);

            if (histogramParent.isImageUpdate() == true) {
                histogramParent.updateFrames(false);
            }

            showHistogram();
        }

        if (mode == BLUE) {
            nPts = lut.getBlueFunction().size();

            if ((index != (lut.getTransferFunction().size() - 1)) && (mx > x[index + 1])) {
                return;
            }

            if ((index != 0) && (mx < x[index - 1])) {
                return;
            }

            if ((index == (lut.getTransferFunction().size() - 1)) || (index == 0)) {
                y[index] = my;
            } else {
                x[index] = mx;
                y[index] = my;
            }

            lut.getBlueFunction().importArrays(x, y, nPts);
            lut.makeLUT(-1);
            histogramParent.setLUT(lut);

            if (histogramParent.isImageUpdate() == true) {
                histogramParent.updateFrames(false);
            }

            showHistogram();
        }

        if (mode == ALPHA) {
            nPts = lut.getAlphaFunction().size();

            if ((index != (lut.getTransferFunction().size() - 1)) && (mx > x[index + 1])) {
                return;
            }

            if ((index != 0) && (mx < x[index - 1])) {
                return;
            }

            if ((index == (lut.getTransferFunction().size() - 1)) || (index == 0)) {
                y[index] = my;
            } else {
                x[index] = mx;
                y[index] = my;
            }

            lut.getAlphaFunction().importArrays(x, y, nPts);
            lut.makeLUT(-1);
            histogramParent.setLUT(lut);

            if (histogramParent.isImageUpdate() == true) {
                histogramParent.updateFrames(false);
            }

            showHistogram();
        } else if ((mode == LINEAR) || (mode == CT)) {
            nPts = lut.getTransferFunction().size();

            // if (index == lut.getTransferFunction().size()-1 || mx > x[index+1]) return;
            // if (index == 0 || mx < x[index-1]) return;
            if ((index != (lut.getTransferFunction().size() - 1)) && (mx > x[index + 1])) {
                return;
            }

            if ((index != 0) && (mx < x[index - 1])) {
                return;
            }

            if ((index == (lut.getTransferFunction().size() - 1)) || (index == 0)) {
                y[index] = my;
            } else {
                x[index] = mx;
                y[index] = my;
            }

            lut.getTransferFunction().importArrays(x, y, nPts);

            if (histogramParent.isImageUpdate() == true) {
                histogramParent.updateFrames(false);
            }

            showHistogram();

            return;
        } else if (mode == DUAL_THRESHOLD) {
            nPts = lut.getTransferFunction().size();

            if ((index == 1) && (mx < x[3])) {
                x[index] = mx;
                y[index] = my;
                x[index + 1] = mx;
                y[4] = my;
            } else if ((index == 2) && (mx < x[3])) {
                x[index] = mx;
                x[index - 1] = mx;
            } else if ((index == 3) && (mx > x[2])) {
                x[index] = mx;
                x[index + 1] = mx;
            } else if ((index == 4) && (mx > x[2])) {
                x[index] = mx;
                y[index] = my;
                x[index - 1] = mx;
                y[1] = my;
            }

            lut.getTransferFunction().importArrays(x, y, nPts);

            float low = ((Vector2f) lut.getTransferFunction().getPoint(1)).X;
            float high = ((Vector2f) lut.getTransferFunction().getPoint(4)).X;

            histogramParent.updateThresholdFields(low, high);

            if (histogramParent.isImageUpdate() == true) {
                histogramParent.updateFrames(false);
            }

            showHistogram();
        }

    }

    /**
     * Update the cursor position when mouse slider is moved.
     *
     * @param  _mx     cursor x postion.
     * @param  _my     cursor y position.
     * @param  _index  cursor index
     */
    public void updateCursorXPos(float _mx, float _my, int _index) {
        int nPts = 0;
        float mx = _mx;

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

        // System.out.println( "updateCursorXPos: x = " + _mx + " y = " + _my );

        if (mode == RED) { // Drag point of red transfer function
            nPts = lut.getRedFunction().size();

            if ((_index == (lut.getTransferFunction().size() - 1)) || (_index == 0)) { }
            else {

                if (_index != 0) {
                    x[_index] = mx;
                }
            }

            lut.getRedFunction().importArrays(x, y, nPts);
            lut.makeLUT(-1); // make LUT and use nColors already set for LUT
            histogramParent.setLUT(lut);

            if (histogramParent.isImageUpdate() == true) {
                histogramParent.updateFrames(false);
            }

            showHistogram();
        }

        if (mode == GREEN) {
            nPts = lut.getGreenFunction().size();

            if ((_index == (lut.getTransferFunction().size() - 1)) || (_index == 0)) { }
            else {

                if (_index != 0) {
                    x[_index] = mx;
                }
            }

            lut.getGreenFunction().importArrays(x, y, nPts);
            lut.makeLUT(-1);
            histogramParent.setLUT(lut);

            if (histogramParent.isImageUpdate() == true) {
                histogramParent.updateFrames(false);
            }

            showHistogram();
        }

        if (mode == BLUE) {
            nPts = lut.getBlueFunction().size();

            if ((_index == (lut.getTransferFunction().size() - 1)) || (_index == 0)) { }
            else {

                if (_index != 0) {
                    x[_index] = mx;
                }
            }

            lut.getBlueFunction().importArrays(x, y, nPts);
            lut.makeLUT(-1);
            histogramParent.setLUT(lut);

            if (histogramParent.isImageUpdate() == true) {
                histogramParent.updateFrames(false);
            }

            showHistogram();
        }

        if (mode == ALPHA) {
            nPts = lut.getAlphaFunction().size();

            if ((_index == (lut.getTransferFunction().size() - 1)) || (_index == 0)) { }
            else {

                if (_index != 0) {
                    x[_index] = mx;
                }
            }

            lut.getAlphaFunction().importArrays(x, y, nPts);
            lut.makeLUT(-1);
            histogramParent.setLUT(lut);

            if (histogramParent.isImageUpdate() == true) {
                histogramParent.updateFrames(false);
            }

            showHistogram();
        } else if ((mode == LINEAR) || (mode == CT)) {
            nPts = lut.getTransferFunction().size();

            if ((_index == (lut.getTransferFunction().size() - 1)) || (_index == 0)) { }
            else {

                if (_index != 0) {
                    x[_index] = mx;
                }
            }

            lut.getTransferFunction().importArrays(x, y, nPts);

            if (histogramParent.isImageUpdate() == true) {
                histogramParent.updateFrames(false);
            }

            showHistogram();

            return;
        } else if (mode == DUAL_THRESHOLD) {
            mx = (float) (min + (mx / 255.0f * (max - min)));
            lut.getTransferFunction().exportArrays(x, y);
            nPts = lut.getTransferFunction().size();

            if ((_index == 1) && (mx < x[3])) {
                x[_index] = mx;

                // y[index] = my;
                x[_index + 1] = mx;
                // y[4] = my;
            } else if ((_index == 2) && (mx < x[3])) {
                x[_index] = mx;
                x[_index - 1] = mx;
            } else if ((_index == 3) && (mx > x[2])) {
                x[_index] = mx;
                x[_index + 1] = mx;
            } else if ((_index == 4) && (mx > x[2])) {
                x[_index] = mx;

                // y[index] = my;
                x[_index - 1] = mx;
                // y[1] = my;
            }

            lut.getTransferFunction().importArrays(x, y, nPts);

            float low = ((Vector2f) lut.getTransferFunction().getPoint(1)).X;
            float high = ((Vector2f) lut.getTransferFunction().getPoint(4)).X;

            histogramParent.updateThresholdFields(low, high);

            if (histogramParent.isImageUpdate() == true) {
                histogramParent.updateFrames(false);
            }

            showHistogram();
        }
    }

    /**
     * Clean up some resources!
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        dispose();
        super.finalize();
    }
}
