package gov.nih.mipav.view.renderer;


import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.util.*;


/**
 * This class extends the ViewJComponentVolOpacityBase class and used to coordinate how a histogram and LUT for an image
 * are to be displayed to the screen. For display purposes, this component has a LUT Model.
 *
 * @version  0.1 Aug 1, 1997
 * @author   Matthew J. McAuliffe, Ph.D. (primary)
 */
public class ViewJComponentVolOpacityRGB extends ViewJComponentVolOpacityBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1208349901510241151L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int[] lutIndexBuffer;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new component histogram LUT.
     *
     * @param  parent  Frame where histogram is to be displayed -- must implement ViewJComponentVolOpacityListener
     * @param  _histo  histogram model
     * @param  _image  image of the displayed histogram and associated LUT
     */
    public ViewJComponentVolOpacityRGB(ViewJComponentVolOpacityListener parent, ModelHistogram _histo, ModelImage _image) {
        super(parent, _histo, _image, new Dimension(450, 375));

        setupMinMax();

        setDoubleBuffered(false);

        linearMode();
    }
    /**
     * Creates a Histogram RGB component.
     *
     * @param  opacityPanel  Frame where histogram is to be displayed
     * @param  _histo        DOCUMENT ME!
     * @param  _image        image of the displayed histogram
     */
    public ViewJComponentVolOpacityRGB(JPanelVolOpacityRGB opacityPanel, ModelHistogram _histo, ModelImage _image) {
        super(opacityPanel, _histo, _image, new Dimension(450, 375));

        setupMinMax();

        setDoubleBuffered(false);

        linearMode();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Sets variables to null and gets rid of frame.
     */
    public void dispose() {
        super.dispose();
    }

    /**
     * Get the transfer function.
     *
     * @return  TransferFunction trans function
     */
    public TransferFunction getActiveTransferFunction() {
        return transferFunction;
    }

    /**
     * Get the transfer function.
     *
     * @param   channel  int channel flag.
     *
     * @return  TransferFunction trans function
     */
    public TransferFunction getTransferFunction(int channel) {
        return transferFunction;
    }

    /**
     * {@inheritDoc}
     */
    public void showHistogram(ModelLUT lut) {

        if (histogram == null) {
            return;
        }

        try { // get histogram
            histogram.exportData(stRange, histogramBuffer.length, histogramBuffer);
        } catch (IOException error) {
            Preferences.debug("IOException: ComponentHistoRGB.show");
            error.printStackTrace();

            return;
        }

        showRGB();
        repaint();
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

    /**
     * Set the image min, max values.
     */
    private void setupMinMax() {

        if (image.getType() == ModelStorageBase.ARGB) {
            min = 0;
            max = 255;
        } else {
            min = (float) image.getMinR();
            max = (float) image.getMaxR();
        }

        range = max - min;
    }

    /**
     * Show RGB histogram.
     */
    private void showRGB() {
        int j, k;
        double sum;
        int end;
        int index;
        float iNew;
        int offset;

        if (lutIndexBuffer == null) {

            try {
                lutIndexBuffer = new int[256]; // will always be 256
            } catch (OutOfMemoryError error) {
                System.gc();
                Preferences.debug("Out of memory: ComponentHistoRGB.show");
            }
        }

        // create a red LUT for the red mode, a green LUT for the green mode, and
        // a blue LUT for the blue mode
        if (mode == RED) {

            for (int i = 0; i < 256; i++) {
                lutIndexBuffer[i] = (255 << 24) | (i << 16);
            }
        } else if (mode == GREEN) {

            for (int i = 0; i < 256; i++) {
                lutIndexBuffer[i] = (255 << 24) | (i << 8);
            }
        } else if (mode == BLUE) {

            for (int i = 0; i < 256; i++) {
                lutIndexBuffer[i] = (255 << 24) | i;
            }
        } else {

            for (int i = 0; i < 256; i++) {
                lutIndexBuffer[i] = (255 << 24) | (i << 16) | (i << 8) | i;
            }
        }

        int samples = MipavMath.round(histogramBuffer.length / dim.width);

        if (samples == 0) {
            samples = 1;
        }

        // find histogram max normalized to the width of the display component
        histogramMax = -9999;

        for (int i = 0; i < dim.width; i++) {

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
        int lightGray = 0xFFAFAFAF;
        Arrays.fill(pixBuffer, lightGray);

        for (int i = 0; i < dim.width; i++) {

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
            iNew = (float) i;
            index = MipavMath.round(transferFunction.getRemappedValue(iNew, 256));

            // build histogram image
            offset = (dim.width * (dim.height - 1)) + i;

            for (j = 0; j < end; j++) {
                pixBuffer[offset - (j * dim.width)] = lutIndexBuffer[index];
            }
        }

        importImage(pixBuffer); // Method in parent class to import the image
        paintComponent(getGraphics());
    }

}
