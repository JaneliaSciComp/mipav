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
 * to be displayed to the screen. For display purposes, this component has a LUT Model.
 *
 * @version  0.1 Aug 1, 1997
 * @author   Matthew J. McAuliffe, Ph.D. (primary)
 * @author   Harman Singh
 */
public class ViewJComponentHistoRGB extends ViewJComponentHLUTBase implements MouseMotionListener, MouseListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 288314366500212340L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelRGB RGBT;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a Histogram RGB component.
     *
     * @param  _histoFrame  Frame where histogram is to be displayed
     * @param  _histo       histogram model
     * @param  RGBTable     DOCUMENT ME!
     * @param  _image       image of the displayed histogram
     */
    public ViewJComponentHistoRGB(HistoLUTParent _histoFrame, ModelHistogram _histo, ModelRGB RGBTable,
                                  ModelImage _image) {
        super(_histoFrame, _histo, _image, new Dimension(377, 336));

        RGBT = RGBTable;

        addMouseMotionListener(this);
        addMouseListener(this);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * /
     *
     * @param  x  DOCUMENT ME!
     * @param  y  DOCUMENT ME!
     */
    public void addFunctionPoint(float x, float y) { }

    /**
     * DOCUMENT ME!
     *
     * @param  x       DOCUMENT ME!
     * @param  y       DOCUMENT ME!
     * @param  remove  DOCUMENT ME!
     */
    public void checkProximityToTransferFunction(float x, float y, boolean remove) { }

    /**
     * DOCUMENT ME!
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void dragPoint(MouseEvent mouseEvent) { }

    /**
     * {@inheritDoc}
     */
    public void dualThresholdMode(int newMode) {
        float min, max;

        // tells whether the mode was previously the other threshold
        boolean wasSwitched = ((thresholdMode != NO_THRESHOLD) && (newMode != thresholdMode));

        thresholdMode = newMode;

        if (image.getType() != ModelStorageBase.ARGB_USHORT) {
            min = 0;
            max = 255;
        } else {
            min = (float) image.getMin();
            max = (float) image.getMax();
        }

        // System.err.println("MIN IS: " + min + " MAX IS: " + max);


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

        if ((mode == RED) || (mode == ALL)) {
            RGBT.getRedFunction().importArrays(x, y, 6);
        }

        if ((mode == GREEN) || (mode == ALL)) {
            RGBT.getGreenFunction().importArrays(x, y, 6);
        }

        if ((mode == BLUE) || (mode == ALL)) {
            RGBT.getBlueFunction().importArrays(x, y, 6);
        }

        histogramParent.updateThresholdFields(x[1], x[3]);

        RGBT.setColor(255, Color.black);
        RGBT.makeRGB(-1);

        histogramParent.updateFrames(false);
        showHistogram();
    }

    /**
     * Evenly distributed the controls points of linear transfer function.
     */
    public void evenDistribution() {
        RGBT.evenDistributionFunctions();
        RGBT.makeRGB(-1);
        histogramParent.updateFrames(false);
        showHistogram();
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getLowerThreshold() {
        return x[1];
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getThresholdMode() {
        return this.thresholdMode;
    }

    /**
     * {@inheritDoc}
     */
    public TransferFunction getTransferFunction() {

        if ((mode == RED) || (mode == ALL)) {
            return RGBT.getRedFunction();
        } else if ((mode == GREEN) || (mode == ALL)) {
            return RGBT.getGreenFunction();
        } else if ((mode == BLUE) || (mode == ALL)) {
            return RGBT.getBlueFunction();
        }

        // default to red for no particular reason
        return RGBT.getRedFunction();
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getUpperThreshold() {
        return x[3];
    }

    /**
     * Resets mode to linear and shows component.
     */
    public void linearMode() {
        RGBT.makeGrayTransferFunctions();
        RGBT.makeRGB(-1);
        histogramParent.updateFrames(false);
        showHistogram();
    }

    // ************************************************************************
    // ***************************** Mouse Events *****************************
    // ************************************************************************

    /**
     * Unchanged.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseClicked(MouseEvent mouseEvent) { }

    // ************************************************************************
    // ************************** Mouse Motion Events *************************
    // ************************************************************************

    /**
     * Continually updates the image depending on where the mouse is.
     *
     * @param  mouseEvent  event that triggered this function
     */
    public void mouseDragged(MouseEvent mouseEvent) {
        int nPts;
        float mx = mouseEvent.getX() - offsetX;
        float my = mouseEvent.getY() - offsetY;

        // if ( (mx >= dim.width) || (mx < 0) ||
        // (my >= dim.height)|| (my < 0)) {
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
        // return;
        // }

        if (addPointFlag == true) {
            return;
        }

        if (index == -99) {
            return;
        }

        if (thresholdMode == NO_THRESHOLD) {

            if ((mode == RED) || (mode == ALL)) { // Drag point of red transfer function
                RGBT.getRedFunction().exportArrays(x, y);
                nPts = RGBT.getRedFunction().size();

                if ((index == (RGBT.getRedFunction().size() - 1)) || (mx > x[index + 1])) {
                    return;
                }

                if ((index == 0) || (mx < x[index - 1])) {
                    return;
                }

                x[index] = mx;
                y[index] = my;

                RGBT.getRedFunction().importArrays(x, y, nPts);
            }

            if ((mode == GREEN) || (mode == ALL)) {
                RGBT.getGreenFunction().exportArrays(x, y);
                nPts = RGBT.getGreenFunction().size();

                if ((index == (RGBT.getGreenFunction().size() - 1)) || (mx > x[index + 1])) {
                    return;
                }

                if ((index == 0) || (mx < x[index - 1])) {
                    return;
                }

                x[index] = mx;
                y[index] = my;

                RGBT.getGreenFunction().importArrays(x, y, nPts);
            }

            if ((mode == BLUE) || (mode == ALL)) {
                RGBT.getBlueFunction().exportArrays(x, y);
                nPts = RGBT.getBlueFunction().size();

                if ((index == (RGBT.getBlueFunction().size() - 1)) || (mx > x[index + 1])) {
                    return;
                }

                if ((index == 0) || (mx < x[index - 1])) {
                    return;
                }

                x[index] = mx;
                y[index] = my;

                RGBT.getBlueFunction().importArrays(x, y, nPts);
            }
        } // Thresholding
        else {

            if ((mode == RED) || (mode == ALL)) {
                RGBT.getRedFunction().exportArrays(x, y);
            } else if (mode == GREEN) {
                RGBT.getGreenFunction().exportArrays(x, y);
            } else if (mode == BLUE) {
                RGBT.getBlueFunction().exportArrays(x, y);
            }

            // allow dragging based on index of point dragged
            if (index == 0) {

                if (thresholdMode == DUAL_THRESHOLD) {
                    y[0] = my;
                }
            } else if ((index == 1) && (mx < x[3])) {
                x[index] = mx;

                if (thresholdMode == DUAL_THRESHOLD) {
                    y[index] = my;
                    y[4] = my;
                }

                x[index + 1] = mx;
            } else if ((index == 2) && (mx < x[3])) {
                x[index] = mx;
                x[index - 1] = mx;

                if (thresholdMode == DUAL_THRESHOLD_INV) {
                    y[index] = my;
                }

            } else if ((index == 3) && (mx > x[2])) {
                x[3] = mx;
                x[4] = mx;

                if (thresholdMode == DUAL_THRESHOLD_INV) {
                    y[3] = my;
                }
            } else if ((index == 4) && (mx > x[2])) {
                x[4] = mx;
                x[3] = mx;

                if (thresholdMode == DUAL_THRESHOLD) {
                    y[4] = my;
                    y[1] = my;
                }

            } else if (index == 5) {

                if (thresholdMode == DUAL_THRESHOLD) {
                    y[5] = my;
                }
            }

            if ((mode == RED) || (mode == ALL)) {
                RGBT.getRedFunction().importArrays(x, y, 6);
            }

            if ((mode == GREEN) || (mode == ALL)) {
                RGBT.getGreenFunction().importArrays(x, y, 6);
            }

            if ((mode == BLUE) || (mode == ALL)) {
                RGBT.getBlueFunction().importArrays(x, y, 6);
            }

            histogramParent.updateThresholdFields(x[1], x[3]);

        }

        RGBT.makeRGB(-1); // make LUT and use nColors already set for LUT

        if (histogramParent.isImageUpdate() == true) {
            histogramParent.updateFrames(false);
        }

        showHistogram();

        if (mode != ALL) {
            histogramParent.setAllOff();
        }
    }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseEntered(MouseEvent mouseEvent) { }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseExited(MouseEvent mouseEvent) { }

    /**
     * Changes the cursor so that function can add points depending on how near the line it is.
     *
     * @param  mouseEvent  event that triggered this function
     */
    public void mouseMoved(MouseEvent mouseEvent) {
        int nPts = 0;
        float mx = mouseEvent.getX() - offsetX;
        float my = mouseEvent.getY() - offsetY;
        float xN, yN, x1N, y1N;
        float min = 0, max = 255;

        if ((mx >= dim.width) || (mx < 0) || (my >= dim.height) || (my < 1)) {
            return;
        }

        if ((mode == RED) || (mode == ALL)) {
            RGBT.getRedFunction().exportArrays(x, y);
            nPts = RGBT.getRedFunction().size();
            min = 0;
            max = 255.0f;
        }

        if (mode == GREEN) {
            RGBT.getGreenFunction().exportArrays(x, y);
            nPts = RGBT.getGreenFunction().size();
            min = 0;
            max = 255.0f;
        }

        if (mode == BLUE) {
            RGBT.getBlueFunction().exportArrays(x, y);
            nPts = RGBT.getBlueFunction().size();
            min = 0;
            max = 255.0f;
        }

        int theStart = 0, theEnd = nPts;

        if (mode == NO_THRESHOLD) {
            theStart = 1;
            theEnd--;
        }

        for (int i = theStart; i < theEnd; i++) {
            xN = (int) (((x[i] - min) / (max - min) * 255) + 0.5);
            yN = (int) (y[i] + 0.5);

            if (MipavMath.distance(mx, xN, my, yN) < 5) { // Are we close to a point ??

                if (mouseEvent.isShiftDown()) { // are we in going to remove this point
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

        if (thresholdMode == NO_THRESHOLD) {

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
        }

        setCursor(crosshairCursor);
        addPointFlag = false;
        index = -99;
    }

    /**
     * Checks for making new points or not.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mousePressed(MouseEvent mouseEvent) {
        float mx = mouseEvent.getX() - offsetX; // see ViewJComponentHLUTBase
        float my = mouseEvent.getY() - offsetY;

        if (mode == RED) { // Add or remove points to red transfer function

            if ((addPointFlag == true) && (index != -99) && (mouseEvent.isShiftDown() == false)) {
                RGBT.getRedFunction().insertPoint(new Vector2f(mx, my), index + 1);
            }

            if ((addPointFlag == false) && (index != -99) && mouseEvent.isShiftDown()) {
                RGBT.getRedFunction().removePoint(index);
            }
        } else if (mode == GREEN) {

            if ((addPointFlag == true) && (index != -99) && (mouseEvent.isShiftDown() == false)) {
                RGBT.getGreenFunction().insertPoint(new Vector2f(mx, my), index + 1);
            }

            if ((addPointFlag == false) && (index != -99) && mouseEvent.isShiftDown()) {
                RGBT.getGreenFunction().removePoint(index);
            }
        } else if (mode == BLUE) {

            if ((addPointFlag == true) && (index != -99) && (mouseEvent.isShiftDown() == false)) {
                RGBT.getBlueFunction().insertPoint(new Vector2f(mx, my), index + 1);
            }

            if ((addPointFlag == false) && (index != -99) && mouseEvent.isShiftDown()) {
                RGBT.getBlueFunction().removePoint(index);
            }
        } else if (mode == ALL) {

            if ((addPointFlag == true) && (index != -99) && (mouseEvent.isShiftDown() == false)) {
                RGBT.getRedFunction().insertPoint(new Vector2f(mx, my), index + 1);
                RGBT.getGreenFunction().insertPoint(new Vector2f(mx, my), index + 1);
                RGBT.getBlueFunction().insertPoint(new Vector2f(mx, my), index + 1);
            }

            if ((addPointFlag == false) && (index != -99) && mouseEvent.isShiftDown()) {
                RGBT.getRedFunction().removePoint(index);
                RGBT.getGreenFunction().removePoint(index);
                RGBT.getBlueFunction().removePoint(index);
            }
        }

    }

    /**
     * Updates image and shows it.
     *
     * @param  mouseEvent  event that triggered this function
     */
    public void mouseReleased(MouseEvent mouseEvent) {
        histogramParent.updateFrames(false);
        showHistogram();
        // System.gc();
    }

    /**
     * DOCUMENT ME!
     */
    public void noThreshold() {
        this.thresholdMode = NO_THRESHOLD;
    }

    /**
     * Paints the component.
     *
     * @param  g  graphics to paint in
     */
    public void paintComponent(Graphics g) {
        int nPts;
        int yLog;
        float minR, maxR, minG, maxG, minB, maxB, max;
        float range;
        float scale;

        if (image.getType() == ModelImage.ARGB) {
            minR = 0.0f;
            maxR = 255.0f;
            minG = 0.0f;
            maxG = 255.0f;
            minB = 0.0f;
            maxB = 255.0f;
        } else {
            max = (float) Math.max(image.getMaxR(), image.getMaxG());
            max = (float) Math.max(image.getMaxB(), max);
            minR = (float) image.getMinR();
            maxR = (float) image.getMaxR();
            minG = (float) image.getMinG();
            maxG = (float) image.getMaxG();
            minB = (float) image.getMinB();
            maxB = (float) image.getMaxB();
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
            g.drawString("C", offsetX + 305, offsetY + 100);
            g.drawString("o", offsetX + 305, offsetY + 114);
            g.drawString("u", offsetX + 305, offsetY + 128);
            g.drawString("n", offsetX + 305, offsetY + 142);
            g.drawString("t", offsetX + 305, offsetY + 156);

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

            if ((mode == RED) || (mode == ALL)) {

                g.setColor(Color.green);
                nPts = RGBT.getGreenFunction().size();
                RGBT.getGreenFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (x[i] + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);

                g.setColor(Color.blue);
                nPts = RGBT.getBlueFunction().size();
                RGBT.getBlueFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (x[i] + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);

                g.setColor(Color.white);
                nPts = RGBT.getRedFunction().size();
                RGBT.getRedFunction().exportArrays(x, y);

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
                g.drawString(MipavUtil.makeFloatString(minR, 2), offsetX - 10, 275 + offsetY);
                g.drawString(MipavUtil.makeFloatString(maxR, 2), offsetX + 256 - 15, 275 + offsetY);

                range = maxR - minR;
                scale = range / 255;

                for (int i = 1; i < (nPts - 1); i++) { // draw tick marks and values
                    g.drawLine(offsetX + 3, yN[i], offsetX - 3, yN[i]);
                    g.drawLine(offsetX - 30, yN[i], offsetX - 33, yN[i]);
                    g.drawString(String.valueOf(255 + offsetY - yN[i]), offsetX - 55, yN[i] + 6);
                    g.drawLine(xN[i], 250 + offsetY, xN[i], 260 + offsetY);

                    if ((i % 2) == 0) {
                        g.drawString(MipavUtil.makeFloatString(((x[i] * scale) + minR), 2), xN[i] - 10, 273 + offsetY);
                    } else {
                        g.drawString(MipavUtil.makeFloatString(((x[i] * scale) + minR), 2), xN[i] - 10, 287 + offsetY);
                    }
                }

            } else if (mode == GREEN) {

                g.setColor(Color.blue);
                nPts = RGBT.getBlueFunction().size();
                RGBT.getBlueFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (x[i] + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);

                g.setColor(Color.red);
                nPts = RGBT.getRedFunction().size();
                RGBT.getRedFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (x[i] + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);

                g.setColor(Color.white);
                nPts = RGBT.getGreenFunction().size();
                RGBT.getGreenFunction().exportArrays(x, y);

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
                g.drawString(MipavUtil.makeFloatString(minG, 2), offsetX - 10, 275 + offsetY);
                g.drawString(MipavUtil.makeFloatString(maxG, 2), offsetX + 256 - 15, 275 + offsetY);

                range = maxG - minG;
                scale = range / 255;

                for (int i = 1; i < (nPts - 1); i++) { // draw tick marks and values
                    g.drawLine(offsetX + 3, yN[i], offsetX - 3, yN[i]);
                    g.drawLine(offsetX - 30, yN[i], offsetX - 33, yN[i]);
                    g.drawString(String.valueOf(255 + offsetY - yN[i]), offsetX - 55, yN[i] + 6);
                    g.drawLine(xN[i], 250 + offsetY, xN[i], 260 + offsetY);

                    if ((i % 2) == 0) {
                        g.drawString(MipavUtil.makeFloatString(((x[i] * scale) + minG), 2), xN[i] - 10, 273 + offsetY);
                    } else {
                        g.drawString(MipavUtil.makeFloatString(((x[i] * scale) + minG), 2), xN[i] - 10, 287 + offsetY);
                    }
                }

            } else if (mode == BLUE) {

                g.setColor(Color.red);
                nPts = RGBT.getRedFunction().size();
                RGBT.getRedFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (x[i] + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);

                g.setColor(Color.green);
                nPts = RGBT.getGreenFunction().size();
                RGBT.getGreenFunction().exportArrays(x, y);

                for (int i = 0; i < nPts; i++) {
                    xN[i] = (int) (x[i] + offsetX + 0.5);
                    yN[i] = (int) (y[i] + offsetY + 0.5);
                }

                g.drawPolyline(xN, yN, nPts);

                g.setColor(Color.white);
                nPts = RGBT.getBlueFunction().size();
                RGBT.getBlueFunction().exportArrays(x, y);

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
                g.drawString(MipavUtil.makeFloatString(minB, 2), offsetX - 10, 275 + offsetY);
                g.drawString(MipavUtil.makeFloatString(maxB, 2), offsetX + 256 - 15, 275 + offsetY);

                range = maxB - minB;
                scale = range / 255;

                for (int i = 1; i < (nPts - 1); i++) { // draw tick marks and values
                    g.drawLine(offsetX + 3, yN[i], offsetX - 3, yN[i]);
                    g.drawLine(offsetX - 30, yN[i], offsetX - 33, yN[i]);
                    g.drawString(String.valueOf(255 + offsetY - yN[i]), offsetX - 55, yN[i] + 6);
                    g.drawLine(xN[i], 250 + offsetY, xN[i], 260 + offsetY);

                    if ((i % 2) == 0) {
                        g.drawString(MipavUtil.makeFloatString(((x[i] * scale) + minB), 2), xN[i] - 10, 273 + offsetY);
                    } else {
                        g.drawString(MipavUtil.makeFloatString(((x[i] * scale) + minB), 2), xN[i] - 10, 287 + offsetY);
                    }
                }

            }

        }
    }

    /**
     * DOCUMENT ME!
     */
    public void removeClickedFunctionPoint() { }

    /**
     * {@inheritDoc}
     */
    public void showHistogram(ModelLUT lut) {
        int lutHeight = 256;

        if (histogram == null) {
            return;
        }

        if (RGBT != null) {
            lutHeight = RGBT.getExtents()[1];
        }

        if (lutHeight != lutIndexBuffer.length) {

            try {
                lutIndexBuffer = new int[lutHeight];
            } catch (OutOfMemoryError error) {
                System.gc();
                Preferences.debug("Out of memory: ComponentHistoRGB.show");
            }
        }

        try { // get histogram
            histogram.exportData(0, histogramBuffer.length, histogramBuffer);
        } catch (IOException error) {
            Preferences.debug("IOException: ComponentHistoRGB.show");
            error.printStackTrace();
            Preferences.debug(error.getMessage());

            return;
        }

        showRGB(lutIndexBuffer, mode);
        repaint();
    }

    /**
     * Clean up some resources!
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        RGBT = null;
        dispose();
        super.finalize();
    }

    /**
     * DOCUMENT ME!
     *
     * @param  lutIndexBuffer  LUT index buffer for desired mode (Red, Green, Blue or All) 
     * @param  mode            Indicates the channel for which the histogram is displayed
     */
    private void showRGB(int[] lutIndexBuffer, int mode) {
        int i, j, k;
        double sum;
        int end, aRGB;
        int index;
        float ptX1 = 0, ptX2 = 0, ptY1 = 0, ptY2 = 0;
        float iNew;
        int offset;

        int nPts = 0;

        // create a red LUT for the red mode, a green LUT for the green mode, and
        // a blue LUT for the blue mode
        if (mode == RED) {

            for (i = 0; i < 256; i++) {
                lutIndexBuffer[i] = (255 << 24) | (i << 16);
            }
        } else if (mode == GREEN) {

            for (i = 0; i < 256; i++) {
                lutIndexBuffer[i] = (255 << 24) | (i << 8);
            }
        } else if (mode == BLUE) {

            for (i = 0; i < 256; i++) {
                lutIndexBuffer[i] = (255 << 24) | i;
            }
        } //else if (mode == ALL) {                   // RGB mode does not work with linear scale if this code is present.
           // paintComponent(getGraphics());
            
            // return;
       // }

        int samples = Math.round(histogramBuffer.length / (float) dim.width);

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
            histogramMaxLog = Math.log((double) (histogramMax));
        }
        //System.out.println("logFlag = " + logFlag);

        // Builds special buffer by remapping the data in the LUT range
        int lightGray = 0xFFAFAFAF;
        Arrays.fill(pixBuffer, lightGray);

        if (mode == RED) {
            nPts = RGBT.getRedFunction().size();
        } else if (mode == GREEN) {
            nPts = RGBT.getGreenFunction().size();
        } else if (mode == BLUE) {
            nPts = RGBT.getBlueFunction().size();
        }

        for (i = 0; i < dim.width; i++) {

            // if histogram width and component width are not the same then correct
            for (sum = 0, k = i * samples; k < ((i + 1) * samples); k++) {

                if (k < histogramBuffer.length) {
                    sum += histogramBuffer[k];
                }
            }

            if (logFlag == true) {

                if (sum >= 1.0) {
                    end = MipavMath.round((dim.height - 1) * Math.log((sum)) / histogramMaxLog);
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
            iNew = (float) i; // Assumes that the histogram length is 256.
            index = i;

            for (j = 0; j < (nPts - 1); j++) {

                if (mode == RED) {
                    ptX1 = ((Vector2f) (RGBT.getRedFunction().getPoint(j))).X;
                    ptX2 = ((Vector2f) (RGBT.getRedFunction().getPoint(j + 1))).X;
                    ptY1 = (dim.height - 1) - ((Vector2f) (RGBT.getRedFunction().getPoint(j))).Y;
                    ptY2 = (dim.height - 1) - ((Vector2f) (RGBT.getRedFunction().getPoint(j + 1))).Y;
                } else if (mode == GREEN) {
                    ptX1 = ((Vector2f) (RGBT.getGreenFunction().getPoint(j))).X;
                    ptX2 = ((Vector2f) (RGBT.getGreenFunction().getPoint(j + 1))).X;
                    ptY1 = (dim.height - 1) - ((Vector2f) (RGBT.getGreenFunction().getPoint(j))).Y;
                    ptY2 = (dim.height - 1) - ((Vector2f) (RGBT.getGreenFunction().getPoint(j + 1))).Y;
                } else if (mode == BLUE) {
                    ptX1 = ((Vector2f) (RGBT.getBlueFunction().getPoint(j))).X;
                    ptX2 = ((Vector2f) (RGBT.getBlueFunction().getPoint(j + 1))).X;
                    ptY1 = (dim.height - 1) - ((Vector2f) (RGBT.getBlueFunction().getPoint(j))).Y;
                    ptY2 = (dim.height - 1) - ((Vector2f) (RGBT.getBlueFunction().getPoint(j + 1))).Y;
                }

                if ((iNew >= ptX1) && (iNew < ptX2)) {

                    // index is a "pointer" into the LUT range - its the color of the histogram bar
                    index = (int) (ptY1 + (((iNew - ptX1) / (ptX2 - ptX1)) * (ptY2 - ptY1)) + 0.5);
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
        
    }
}
