package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;

import java.awt.*;
import java.awt.image.*;

import javax.swing.*;


/**
 * Abstract class used for generating custom components, especially within images.
 *
 * @version  0.1 Aug 1, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public abstract class ViewJComponentHLUTBase extends JComponent {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3244043088343763762L;

    /** Linear tranfer function mode (grayscale). */
    public static final int LINEAR = 0;

    /** Thresholded transfer function mode (grayscale). */
    public static final int THRESHOLD = 1;

    /** Not using a tranfer function (grayscale). */
    public static final int NO_THRESHOLD = 2;

    /** Dual threshold function. */
    public static final int DUAL_THRESHOLD = 3;

    /** Inverse dual threshold function. */
    public static final int DUAL_THRESHOLD_INV = 4;

    /** Staircase transfer function. */
    public static final int STAIRCASE = 5;

    /** CT predefined tranfer function. */
    public static final int CT = 6;

    /** The red channel transfer function. */
    public static final int RED = 7;

    /** The green channel transfer function. */
    public static final int GREEN = 8;

    /** The blue channel transfer function. */
    public static final int BLUE = 9;

    /** The alpha channel transfer function. */
    public static final int ALPHA = 10;

    /** Move all the channel transfer functions. */
    public static final int ALL = 11;

    /** DOCUMENT ME! */
    protected static final int INACTIVE = -99;

    /** The default background color of JPanels (look and feel dependent). */
    protected static final Color backgroundColor = new JPanel().getBackground();

    /** Crosshair mouse cursor. Should move to MipavUtil eventually. */
    protected static final Cursor crosshairCursor = new Cursor(Cursor.CROSSHAIR_CURSOR);

    /** Movement mouse cursor. Should move to MipavUtil eventually. */
    protected static final Cursor moveCursor = new Cursor(Cursor.MOVE_CURSOR);

    /** Component resize mouse cursor. Should move to MipavUtil eventually. */
    protected static final Cursor resizeCursor = new Cursor(Cursor.E_RESIZE_CURSOR);

    /** Hand-shaped mouse cursor. Should move to MipavUtil eventually. */
    protected static final Cursor handCursor = new Cursor(Cursor.HAND_CURSOR);

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

    /** The container of this histogram component. */
    protected HistoLUTParent histogramParent;

    /** The image whose histo lut we are displaying. */
    protected ModelImage image;

    /** DOCUMENT ME! */
    protected Image img;

    /** DOCUMENT ME! */
    protected int index = INACTIVE;

    /** Whether to transform the histogram data by log10. */
    protected boolean logFlag = true;

    /** DOCUMENT ME! */
    protected int[] lutIndexBuffer = null;

    /** The current mode of the histogram component. */
    protected int mode;

    /** DOCUMENT ME! */
    protected int offsetX = 60;

    /** DOCUMENT ME! */
    protected int offsetY = 20;

    /** DOCUMENT ME! */
    protected int[] pixBuffer;

    /** DOCUMENT ME! */
    protected int range;

    /** DOCUMENT ME! */
    protected int stRange;

    /** The current threshold mode (dual or dual inverse) or NO_THRESHOLD if not in theshold mode. */
    protected int thresholdMode;

    /** DOCUMENT ME! */
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

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates object of size defined by width &amp; height.
     *
     * @param  parent   DOCUMENT ME!
     * @param  histo    DOCUMENT ME!
     * @param  image    DOCUMENT ME!
     * @param  compDim  width and height of component
     */
    public ViewJComponentHLUTBase(HistoLUTParent parent, ModelHistogram histo, ModelImage image, Dimension compDim) {
        img = null;

        componentDim = compDim;
        setSize(componentDim.width, componentDim.height);

        histogramParent = parent;
        setHistogramInfo(image, histo);
        thresholdMode = NO_THRESHOLD;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Add a new function point to the current LUT transfer function.
     *
     * @param  x  the x position within the component to add the new point
     * @param  y  the y position within the component to add the new point
     */
    public abstract void addFunctionPoint(float x, float y);

    /**
     * Check to see if the position of the mouse is close to one of the points in the LUT transfer function or the line
     * drawn in between the points.
     *
     * @param  x       the x position within the component
     * @param  y       the y position within the component
     * @param  remove  whether we want to remove a point (assuming that we are near one)
     */
    public abstract void checkProximityToTransferFunction(float x, float y, boolean remove);

    /**
     * Switch the histogram component to (inverse or regular) dual threshold mode.
     *
     * @param  thresholdMode  either DUAL_THRESHOLD, DUAL_THRESHOLD_INV, or NO_THRESHOLD
     */
    public abstract void dualThresholdMode(int thresholdMode);

    /**
     * Switch the histogram component to even distribution linear transfer mode.
     */
    public abstract void evenDistribution();

    /**
     * Switch the histogram component to linear line mode.
     */
    public abstract void linearMode();

    /**
     * Remove the point of the LUT transfer function which the mouse is hovering over (determined by <code>
     * mouseMotion</code>).
     */
    public abstract void removeClickedFunctionPoint();

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
        x = y = z = null;
        xN = yN = zN = null;

        if (histogram != null) {
            histogram.disposeLocal();
            histogram = null;
        }

        image = null;

        if (histogramParent != null) {
            histogramParent = null;
        }
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
    public TransferFunction getTransferFunction() {
        return null;
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
        // paintComponent(getGraphics());
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean isVolOpacityChanged() {
        return false;
    }

    /**
     * Paints the image and border.
     *
     * @param  g  Graphics handle
     */
    public void paintComponent(Graphics g) {
        setDoubleBuffered(false);

        if ((img != null) && (g != null)) {
            g.drawImage(img, offsetX, offsetY, 256, 256, this);
        }
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

        if (image.getType() == ModelStorageBase.ARGB) {
            range = 256;
        } else if (image.getType() == ModelStorageBase.ARGB_USHORT) {

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
        } else {
            range = (int) Math.round(image.getMax() - image.getMin()) + 1;

            if (range < 256) {
                range = 256;
            }
        }

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
            histogramParent.updateFrames(false);
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
     * Updates the corresponding points for the threshold (transfer line).
     *
     * @param  lower  the lower threshold value
     * @param  upper  the upper threshold value
     */
    public void updateDualThreshold(float lower, float upper) {

        if (x != null) {
            x[1] = lower;
            x[2] = lower;

            x[3] = upper;
            x[4] = upper;

            if (getTransferFunction() != null) {
                getTransferFunction().importArrays(x, y, 6);
            }

            histogramParent.updateFrames(false);
            showHistogram();
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  transFunc  VOIContour
     */
    public void updateTransFunc(VOIContour transFunc) { }

    /**
     * Clean up some resources.
     *
     * @throws  Throwable  if there is a problem during cleanup
     */
    protected void finalize() throws Throwable {
        // System.err.println( "finalizing ViewJComponentHLUTBase" );

        if (img != null) {
            img.flush();
            img = null;
        }

        super.finalize();
    }
}
