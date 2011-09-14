package gov.nih.mipav.view.renderer;


import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;

import javax.swing.*;


/**
 * Abstract class used for generating custom components, especially within images.
 *
 * @version  1 - Sept 6, 2005
 * @author   Lee M Orsino, Ph.D.
 */
public abstract class ViewJComponentVolOpacityBase extends JComponent implements MouseListener, MouseMotionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 826985329858268537L;

    /** The red channel transfer function. */
    public static final int RED = 7;

    /** The green channel transfer function. */
    public static final int GREEN = 8;

    /** The blue channel transfer function. */
    public static final int BLUE = 9;

    /** Move all the channel transfer functions. */
    public static final int ALL = 11;

    /** DOCUMENT ME! */
    public static final int INACTIVE = -99;

    /** The default background color of JPanels (look and feel dependent). */
    protected static final Color backgroundColor = new JPanel().getBackground();

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected boolean addPointFlag = false;

    /** The outer dimension of the histogram component we will be drawing. */
    protected Dimension componentDim;

    /** The histogram image dimension. */
    protected Dimension dim = new Dimension(256, 256);

    /** The image histogram. */
    protected ModelHistogram histogram;

    /** DOCUMENT ME! */
    protected int[] histogramBuffer;

    /** DOCUMENT ME! */
    protected double histogramMax;

    /** DOCUMENT ME! */
    protected double histogramMaxLog = 1;

    /** The image whose histo lut we are displaying. */
    protected ModelImage image;

    /** DOCUMENT ME! */
    protected Image img;

    /** Whether to transform the histogram data by log10. */
    protected boolean logFlag = true;

    /** DOCUMENT ME! */
    protected int[] lutIndexBuffer = null;

    /** DOCUMENT ME! */
    protected float min, max;

    /** The current mode of the histogram component. */
    protected int mode;

    /** DOCUMENT ME! */
    protected int newPointIndex = INACTIVE;

    /** DOCUMENT ME! */
    protected int offsetX = 75;

    /** DOCUMENT ME! */
    protected int offsetY = 30;

    /** The container of this histogram component. */
    protected JPanelVolOpacityBase parentPanel;

    /** DOCUMENT ME! */
    protected int[] pixBuffer;

    /** DOCUMENT ME! */
    protected float range;

    /** DOCUMENT ME! */
    protected int stRange;

    /** DOCUMENT ME! */
    protected int tfActiveIndex = INACTIVE;

    /** DOCUMENT ME! */
    protected TransferFunction transferFunction;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates object of size defined by width &amp; height.
     *
     * @param  parent   DOCUMENT ME!
     * @param  histo    DOCUMENT ME!
     * @param  image    DOCUMENT ME!
     * @param  compDim  width and height of component
     */
    public ViewJComponentVolOpacityBase(JPanelVolOpacityBase parent, ModelHistogram histo, ModelImage image,
                                        Dimension compDim) {

        componentDim = compDim;
        setSize(componentDim.width, componentDim.height);
        dim = new Dimension(256, 256);
        transferFunction = new TransferFunction();


        parentPanel = parent;
        setHistogramInfo(image, histo);

        parentPanel.setAdjustersEnabled(false);

        addMouseMotionListener(this);
        addMouseListener(this);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Filters the histogram through the LUT and produces an int array (aRGB) that can be converted to a java image of
     * the histogram for display purposes.
     *
     * @param  LUT  LUT to filter with
     */
    public abstract void showHistogram(ModelLUT LUT);

    /**
     * Abstract method to force extended classes to implement a disposal method to clean up memory.
     */
    public void dispose() {
        histogramBuffer = null;
        pixBuffer = null;
        lutIndexBuffer = null;

        if (histogram != null) {
            histogram.disposeLocal();
            histogram = null;
        }

        image = null;

        if (parentPanel != null) {
            parentPanel = null;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getActiveIndex() {
        return tfActiveIndex;
    }

    /**
     * Access the associated image.
     *
     * @return  ModelImage Reference to image instance.
     */
    public ModelImage getImage() {
        return image;
    }

    /**
     * Gets minimum size equal to object size.
     *
     * @return  Dimension with the size
     */
    public Dimension getMinimumSize() {
        return componentDim;
    }

    /**
     * Get the histogram mode (ie - RED, GREEN, BLUE ).
     *
     * @return  the histogram mode
     */
    public int getMode() {
        return mode;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public TransferFunction getOpacityTransferFunction() {
        return transferFunction;
    }

    /**
     * Gets preferred size to set object size to.
     *
     * @return  Dimension with the size
     */
    public Dimension getPreferredSize() {
        return componentDim;
    }

    /**
     * Get the current transfer function we are working with.
     *
     * @return  the LUT tranfer function
     */
    public VOIBase getTransferFunction() {
        return null;
    }

    /**
     * Resets mode to linear horizontal and shows component.
     */
    public void horizonMode() {

        transferFunction.removeAll();
        transferFunction.addPoint(min, 0);
        transferFunction.addPoint((min + ((max - min) / 3.0f)), 0);
        transferFunction.addPoint((min + ((max - min) * 2.0f / 3.0f)), 0);
        transferFunction.addPoint(max, 0);

        tfActiveIndex = INACTIVE;
        parentPanel.setAdjustersEnabled(false);

        showHistogram();
    }

    /**
     * Creates a Image object from an array of ints that have been formatted (packed) properly (ie, aRGB).
     *
     * @param  data  Data (image) to be displayed.
     */
    public void importImage(int[] data) {
        MemoryImageSource memImg = null;

        if (data != null) {

            if (img != null) {
                img.flush();
                img = null;
            }

            try {
                memImg = new MemoryImageSource(dim.width, dim.height, data, 0, 256);
                memImg.newPixels();
                data = null;
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: LUTComponentBase.importImage.");
            }

            img = createImage(memImg);
        }
    }

    /**
     * Sets mode to linear and shows component.
     */
    public void linearBackSlashMode() {
        transferFunction.removeAll();
        transferFunction.addPoint(min, 0);
        transferFunction.addPoint((min + ((max - min) / 3.0f)), 85);
        transferFunction.addPoint((min + ((max - min) * 2.0f / 3.0f)), 170);
        transferFunction.addPoint(max, 255);

        showHistogram();
    }

    /**
     * Resets mode to linear and shows component.
     */
    public void linearMode() {
        transferFunction.removeAll();
        transferFunction.addPoint(min, 255);
        transferFunction.addPoint((min + ((max - min) / 3.0f)), 255 * 0.67f);
        transferFunction.addPoint((min + ((max - min) * 2.0f / 3.0f)), 255 * 0.333f);
        transferFunction.addPoint(max, 0);

        tfActiveIndex = INACTIVE;
        parentPanel.setAdjustersEnabled(false);

        showHistogram();
    }

    /**
     * DOCUMENT ME!
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseClicked(MouseEvent mouseEvent) { }

    /**
     * Continually updates the image depending on where the mouse is.
     *
     * @param  mouseEvent  event that triggered this function
     */
    public void mouseDragged(MouseEvent mouseEvent) {

        if (tfActiveIndex == INACTIVE) {
            return;
        }

        int offsetMouseX = mouseEvent.getX() - offsetX;
        int offsetMouseY = mouseEvent.getY() - offsetY;

        if (offsetMouseX < 0) {
            offsetMouseX = 0;
        }

        if (offsetMouseY < 0) {
            offsetMouseY = 0;
        }

        if (offsetMouseX >= dim.width) {
            offsetMouseX = dim.width - 1;
        }

        if (offsetMouseY >= dim.height) {
            offsetMouseY = dim.height - 1;
        }


        if (transferFunction.isEndpoint(tfActiveIndex) == false) {

            // do not allow transfer function to fail vertical line test
            if (offsetMouseX <= convertFnPtToScreenSpaceX(transferFunction.getPoint(tfActiveIndex - 1).X)) {
                offsetMouseX = (int) convertFnPtToScreenSpaceX(transferFunction.getPoint(tfActiveIndex - 1).X) + 1;
            } else if (offsetMouseX >= convertFnPtToScreenSpaceX(transferFunction.getPoint(tfActiveIndex + 1).X)) {
                offsetMouseX = (int) convertFnPtToScreenSpaceX(transferFunction.getPoint(tfActiveIndex + 1).X) - 1;
            }

            parentPanel.setAdjustersEnabled(true); // user is adjusting a non-endpoint, so enable adjustment components
        } else {

            // do not allow transfer function endpoints to move off histogram bounds
            if (tfActiveIndex == 0) {
                offsetMouseX = 0;
            } else {
                offsetMouseX = dim.width - 1;
            }

            parentPanel.setAdjustersEnabled(false); // user is adjusting an endpoint, so disable adjustment components
        }

        transferFunction.replacePoint(convertScreenSpaceXToFnPt(offsetMouseX), offsetMouseY, tfActiveIndex);

        if (parentPanel instanceof JPanelVolOpacity) {
            ((JPanelVolOpacity) parentPanel).updateSlider(this);
        } else if (parentPanel instanceof JPanelVolOpacityRGB) {
            ((JPanelVolOpacityRGB) parentPanel).updateSlider(this);
        }

        showHistogram();
        
        parentPanel.update();

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseEntered(MouseEvent mouseEvent) { }

    /**
     * DOCUMENT ME!
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
        int offsetMouseX = mouseEvent.getX() - offsetX;
        int offsetMouseY = mouseEvent.getY() - offsetY;

        // bail if mouse cursor out of bounds
        if ((offsetMouseX >= dim.width) || (offsetMouseX < 0) || (offsetMouseY >= dim.height) || (offsetMouseY < 1)) {
            return;
        }

        // test to see if the cursor is near a point
        for (int i = 0; i < transferFunction.size(); i++) {
            int x_point = (int) convertFnPtToScreenSpaceX(transferFunction.getPoint(i).X);
            int y_point = (int) transferFunction.getPoint(i).Y;

            if (MipavMath.distance(offsetMouseX, x_point, offsetMouseY, y_point) < 5) {

                if (mouseEvent.isShiftDown() == false) {
                    setCursor(MipavUtil.moveCursor);
                } else {
                    setCursor(MipavUtil.resizeCursor);
                }

                addPointFlag = false;

                return;
            }
        }

        // test to see if the cursor is near a line
        if (isNearLine(offsetMouseX, offsetMouseY)) {

            for (int i = 0; i < (transferFunction.size() - 1); i++) {
                int x_point1 = (int) convertFnPtToScreenSpaceX(transferFunction.getPoint(i).X);
                int x_point2 = (int) convertFnPtToScreenSpaceX(transferFunction.getPoint(i + 1).X);

                if ((offsetMouseX >= x_point1) && (offsetMouseX <= x_point2)) {
                    newPointIndex = i;

                    break;
                }
            }

            addPointFlag = true;
            setCursor(MipavUtil.handCursor);

            return;
        }

        setCursor(MipavUtil.crosshairCursor);
        addPointFlag = false;

    }

    /**
     * ************************************************************************ ***************************** Mouse
     * Events *****************************.************************************************************************
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mousePressed(MouseEvent mouseEvent) {
        int offsetMouseX = mouseEvent.getX() - offsetX;
        int offsetMouseY = mouseEvent.getY() - offsetY;

        if ((offsetMouseX > dim.width) || (offsetMouseX < 0) || (offsetMouseY > dim.height) || (offsetMouseY < 0)) {
            tfActiveIndex = INACTIVE;

            return;
        }

        // test to see if the cursor is near a point
        for (int i = 0; i < transferFunction.size(); i++) {
            int x_point = (int) convertFnPtToScreenSpaceX(transferFunction.getPoint(i).X);
            int y_point = (int) transferFunction.getPoint(i).Y;

            if (MipavMath.distance(offsetMouseX, x_point, offsetMouseY, y_point) < 5) {
                parentPanel.setAdjustersEnabled(!transferFunction.isEndpoint(i)); // user is adjusting a non-endpoint,
                                                                                  // so enable adjustment components
                addPointFlag = false; // user has clicked on a point, and is therefore not adding one (might be
                                      // preparing to drag the point)
                tfActiveIndex = i;

                break;
            }
        }

        float convertedFunctionPoint = convertScreenSpaceXToFnPt(offsetMouseX);

        if (addPointFlag == true) {
            transferFunction.insertPoint(convertedFunctionPoint, offsetMouseY, newPointIndex + 1);

            parentPanel.setAdjustersEnabled(!transferFunction.isEndpoint(newPointIndex + 1));
            tfActiveIndex = newPointIndex + 1;
        } else if (mouseEvent.isShiftDown() == true) {
            transferFunction.removePoint(tfActiveIndex);

            parentPanel.setAdjustersEnabled(false);
            tfActiveIndex = INACTIVE;
        }

        if (parentPanel instanceof JPanelVolOpacity) {
            ((JPanelVolOpacity) parentPanel).updateSlider(this);
        } else if (parentPanel instanceof JPanelVolOpacityRGB) {
            ((JPanelVolOpacityRGB) parentPanel).updateSlider(this);
        }

        showHistogram();
    }

    /**
     * DOCUMENT ME!
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseReleased(MouseEvent mouseEvent) {
        showHistogram();
    }

    /**
     * Paints the component.
     *
     * @param  graphics  graphics to paint in
     */
    public void paintComponent(Graphics graphics) {

        if (graphics == null) {
            return;
        }

        Graphics2D graphics2d = (Graphics2D) graphics;
        graphics2d.translate(offsetX, offsetY);

        if (img != null) {
            graphics2d.drawImage(img, 0, 0, null);
        }

        drawAxisAccoutrements(graphics2d);
        drawTransferFunctionLine(graphics2d, transferFunction, Color.white);
        drawTransferFunctionHandles(graphics2d, transferFunction);
        drawTransferFunctionHandleTicks(graphics2d, transferFunction);
    }

    /**
     * Set up the image, LUT and histogram information.
     *
     * @param  _image  image
     * @param  _histo  histogram of the image
     */
    public void setHistogramInfo(ModelImage _image, ModelHistogram _histo) {
        histogram = _histo;
        image = _image;

        pixBuffer = new int[dim.width * dim.height];

        stRange = 0;
        /*
                if (image.getType() == ModelStorageBase.ARGB) {
                    range = 256;
                }
                else if (image.getType() == ModelStorageBase.ARGB_USHORT) {
                    switch (getMode()) {
                        case RED:
                            range = (int) Math.round(image.getMaxR() - image.getMinR()) + 1;
                            break;
                        case GREEN:
                            range = (int) Math.round(image.getMaxG() - image.getMinG()) + 1;
                            break;
                        case BLUE:
                            range = (int) Math.round(image.getMaxB() - image.getMinB()) + 1;
                            break;
                        default:
                            range = (int) Math.round(image.getMaxR() - image.getMinR()) + 1;
                    } // switch (getMode() )
                    if (range < 256) {
                        range = 256;
                    }
                }
                else {
                    range = (int) Math.round(image.getMax() - image.getMin()) + 1;
                    if (range < 256) {
                        range = 256;
                    }
                }
        */
        histogramBuffer = new int[histogram.getExtents()[0]];
        lutIndexBuffer = new int[1];
    }

    /**
     * This flag indicates if the histogram should be displayed using a log10 scale.
     *
     * @param  value  <code>true</code> use the Log scale and <code>false</code> paint bars in linear scale
     */
    public void setLogFlag(boolean value) {
        logFlag = value;
    }

    /**
     * Accessor that sets the mode.
     *
     * @param  _mode  the mode of the histogram
     */
    public void setMode(int _mode) {
        mode = _mode;

        if (mode != ALL) {

            // parentPanel.updateFrames(false);
            showHistogram();
        }
    }

    /**
     * Show the histogram without filtering it through a LUT.
     *
     * @see  this#showHistogram(ModelLUT)
     */
    public void showHistogram() {
        showHistogram(null);
    }

    /**
     * Update the cursor position when mouse slider is moved.
     *
     * @param  _mx  cursor x postion.
     */
    public void updateCursorXPos(float _mx) {

        if ((tfActiveIndex == INACTIVE) || (tfActiveIndex < 0) || (tfActiveIndex > (transferFunction.size() - 1))) {
            return;
        }

        transferFunction.replacePoint(_mx, transferFunction.getPoint(tfActiveIndex).Y, tfActiveIndex);

        showHistogram();

        return;
    }

    /**
     * Load preset transfer function for the image.
     *
     * @param  transFunc  transfer function
     */
    public void updateTransFunc(TransferFunction transFunc) {
        transferFunction.removeAll();

        for (int i = 0; i < transFunc.size(); i++) {
            transferFunction.addPoint(transFunc.getPoint(i));
        }

        tfActiveIndex = INACTIVE;
        parentPanel.setAdjustersEnabled(false);
        showHistogram();
    }

    /**
     * This method converts a transfer function point value into screen space so that it can be displayed on the
     * histogram. This is necessary because the histogram that is painted on the screen is typically has a different
     * range than the transfer function.
     *
     * @param   point_x  float - the point on the transfer function
     *
     * @return  float
     */
    protected float convertFnPtToScreenSpaceX(float point_x) {
        return (point_x - min) / range * dim.width;
    }

    /**
     * This method converts a point in screen space into a transfer function point value. This is necessary because the
     * histogram that is painted on the screen is typically has a different range than the transfer function.
     *
     * @param   pixel  int - the pixel value to translate into a transfer function point
     *
     * @return  float
     */
    protected float convertScreenSpaceXToFnPt(int pixel) {
        return (float) ((pixel / (float) dim.width) * range) + min;
    }

    /**
     * Draw the histogram border and the stationary marker ticks (function point ticks are drawn in
     * drawTransferFunctionHandleTicks(Graphics2D).
     *
     * @param  graphics  Graphics2D - the graphics context to draw in
     */
    protected void drawAxisAccoutrements(Graphics2D graphics) {
        graphics.setFont(MipavUtil.font12B);

        graphics.setColor(Color.black);
        graphics.drawRect(0, 0, dim.width, dim.height); // put border around histogram
        graphics.drawRect(1, 1, dim.width - 2, dim.height - 2); // put border around histogram

        graphics.setColor(Color.blue.darker());
        graphics.drawString(String.valueOf("Image Intensities"), 80, 307);

        drawRotatedVerticalAxisLabel(graphics);

        graphics.setFont(MipavUtil.font12);
        graphics.setColor(Color.black);

        if (logFlag == true) { // add ticks and values on right side of histogram component (log)

            double number = 0.5;

            for (int i = 0; i < 4; i++) {
                int yLog = (int) (255 - (int) ((number * 255) - 0.5));
                graphics.drawLine(254, yLog, 260, yLog);
                graphics.drawString(String.valueOf((int) (Math.exp(histogramMaxLog * number) + 0.5)),
                                    270 - String.valueOf((int) (Math.exp(histogramMaxLog * number) + 0.5)).length(),
                                    yLog + 5);
                number = number * 0.5;

            }
        } else { // add ticks and values on right side of histogram component - linear scale
            graphics.drawLine(254, 63, 260, 63);

            graphics.drawString(String.valueOf((int) ((histogramMax * 0.75) + 0.5)),
                                215 - String.valueOf((int) ((histogramMax * 0.75) + 0.5)).length(), 69);
            graphics.drawLine(254, 127, 260, 127);
            graphics.drawString(String.valueOf((int) ((histogramMax * 0.5) + 0.5)),
                                215 - String.valueOf((int) ((histogramMax * 0.5) + 0.5)).length(), 133);
            graphics.drawLine(254, 191, 260, 191);
            graphics.drawString(String.valueOf((int) ((histogramMax * 0.25) + 0.5)),
                                215 - String.valueOf((int) ((histogramMax * 0.25) + 0.5)).length(), 198);
        }

        graphics.drawString(String.valueOf((int) histogramMax), 265, 5); // top label
    }

    /**
     * DOCUMENT ME!
     *
     * @param  graphics  DOCUMENT ME!
     */
    protected void drawRotatedVerticalAxisLabel(Graphics2D graphics) {
        FontMetrics fontMetrics = graphics.getFontMetrics();
        graphics.translate(dim.width + 60, (dim.height / 2) + (fontMetrics.stringWidth("Count") / 2));
        graphics.rotate(-Math.PI / 2);
        graphics.drawString("Count", 0, 0);
        graphics.rotate(Math.PI / 2);
        graphics.translate(-(dim.width + 60), -((dim.height / 2) + (fontMetrics.stringWidth("Count") / 2)));
    }

    /**
     * Draw the transfer function handles (the little squares used to drag points).
     *
     * @param  graphics  Graphics2D - the graphics context to draw in
     * @param  tf        TransferFunction - the TransferFunction object to draw
     */
    protected void drawTransferFunctionHandles(Graphics2D graphics, TransferFunction tf) {

        for (int i = 0; i < tf.size(); i++) {
            graphics.setColor(Color.black);
            graphics.fillRect((int) (convertFnPtToScreenSpaceX(tf.getPoint(i).X) - 2), (int) (tf.getPoint(i).Y) - 2, 5,
                              5);

            if (tfActiveIndex == i) {
                graphics.setColor(Color.yellow);
            } else {
                graphics.setColor(Color.white);
            }

            graphics.drawRect((int) (convertFnPtToScreenSpaceX(tf.getPoint(i).X) - 2), (int) (tf.getPoint(i).Y) - 2, 5,
                              5);
        }
    }

    /**
     * This method draws the transfer function line.
     *
     * @param  graphics  Graphics2D - the graphics context to draw in
     * @param  tf        TransferFunction - the TransferFunction object to draw
     * @param  color     Color - the color used to draw the function line
     */
    protected void drawTransferFunctionLine(Graphics2D graphics, TransferFunction tf, Color color) {
        graphics.setColor(color);

        int[] x_points = new int[tf.size()];
        int[] y_points = new int[tf.size()];

        for (int i = 0; i < tf.size(); i++) {
            x_points[i] = (int) (convertFnPtToScreenSpaceX(tf.getPoint(i).X) + 0.5);
            y_points[i] = (int) (tf.getPoint(i).Y);
        }

        graphics.drawPolyline(x_points, y_points, tf.size());
    }

    /**
     * Clean up some resources.
     *
     * @throws  Throwable  if there is a problem during cleanup
     */
    protected void finalize() throws Throwable {

        if (img != null) {
            img.flush();
            img = null;
        }

        super.finalize();
    }

    /**
     * Determines whether the mouse cursor is near the active transfer function line.
     *
     * @param   offsetMouseX  int
     * @param   offsetMouseY  int
     *
     * @return  boolean true if cursor is near the active transfer function line, false otherwise
     */
    protected boolean isNearLine(int offsetMouseX, int offsetMouseY) {

        for (int i = 0; i < (transferFunction.size() - 1); i++) {
            int x1 = (int) (convertFnPtToScreenSpaceX(transferFunction.getPoint(i).X) + 0.5);
            int y1 = (int) (transferFunction.getPoint(i).Y + 0.5);
            int x2 = (int) (convertFnPtToScreenSpaceX(transferFunction.getPoint(i + 1).X) + 0.5);
            int y2 = (int) (transferFunction.getPoint(i + 1).Y + 0.5);

            if ((MipavMath.distance(offsetMouseX, x1, offsetMouseY, y1) +
                     MipavMath.distance(offsetMouseX, x2, offsetMouseY, y2) - MipavMath.distance(x1, x2, y1, y2)) <
                    0.5) { // is the mouse cursor close to a line?

                if ((MipavMath.distance(offsetMouseX, x1, offsetMouseY, y1) > 5) &&
                        (MipavMath.distance(offsetMouseX, x2, offsetMouseY, y2) > 5)) {
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * This method draws the transfer function handle marks on the axes of the histogram.
     *
     * @param  graphics  Graphics2D - the graphics context to draw in
     * @param  tf        TransferFunction - the TransferFunction object to draw
     */
    private void drawTransferFunctionHandleTicks(Graphics2D graphics, TransferFunction tf) {
        graphics.setColor(Color.black);

        for (int i = 0; i < tf.size(); i++) {

            if (tfActiveIndex == i) {
                graphics.setFont(MipavUtil.font12B);
            } else {
                graphics.setFont(MipavUtil.font12);
            }

            // draw the vertical axis ticks and numbers
            graphics.drawLine(-3, (int) tf.getPoint(i).Y, 3, (int) tf.getPoint(i).Y);

            String valueString = MipavUtil.makeFloatString((dim.height - 1) - tf.getPoint(i).Y, 1);
            FontMetrics fontMetrics = graphics.getFontMetrics();
            graphics.drawString(valueString, -5 - (fontMetrics.stringWidth(valueString)),
                                (int) tf.getPoint(i).Y + (fontMetrics.getAscent() / 2));


            if ((i % 2) == 0) // stagger every other tick to prevent overlap along the horizontal axis
            {

                // draw the horizontal axis ticks and numbers
                graphics.drawLine((int) (convertFnPtToScreenSpaceX(tf.getPoint(i).X)), dim.height - 3,
                                  (int) (convertFnPtToScreenSpaceX(tf.getPoint(i).X)), dim.height + 3);
                valueString = MipavUtil.makeFloatString(tf.getPoint(i).X, 1);
                graphics.drawString(valueString,
                                    (int) (convertFnPtToScreenSpaceX(tf.getPoint(i).X)) -
                                    (fontMetrics.stringWidth(valueString) / 2),
                                    dim.height + fontMetrics.getAscent() + 5);
            } else {
                graphics.drawLine((int) (convertFnPtToScreenSpaceX(tf.getPoint(i).X)), dim.height - 3,
                                  (int) (convertFnPtToScreenSpaceX(tf.getPoint(i).X)), dim.height + 3);
                valueString = MipavUtil.makeFloatString(tf.getPoint(i).X, 1);
                graphics.drawString(valueString,
                                    (int) (convertFnPtToScreenSpaceX(tf.getPoint(i).X)) -
                                    (fontMetrics.stringWidth(valueString) / 2),
                                    dim.height + fontMetrics.getAscent() + 17);
            }
        }
    }
}
