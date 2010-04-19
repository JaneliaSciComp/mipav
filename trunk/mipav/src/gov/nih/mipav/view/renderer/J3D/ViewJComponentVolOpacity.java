package gov.nih.mipav.view.renderer.J3D;


import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.*;
import gov.nih.mipav.view.renderer.WildMagic.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.media.j3d.*;


/**
 * This class is used to coordinate how a histogram and LUT for an image are to be displayed to the screen. Note y
 * inversion in transfer segment because graphical origin is in upper left corner.
 *
 * <pre>
             255     ^                __________
                     |               /
                     |              /
              O      |             /  <------- Transfer function
              P      |            /
              A      |           /
              C      |          /
              I      |         /
              T      |        /
              Y      |       /
                     |______/
              0      |________________________________>

                      min                         max

                              Image intensity
 * </pre>
 *
 * @version  0.1 Aug 1, 1997
 */
public class ViewJComponentVolOpacity extends ViewJComponentVolOpacityBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5441344478555285964L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private RenderViewBase myParent;
    private VolumeTriPlanarInterface m_kVolumeViewer;

    /** Opacity slider event count. */
    private int opacityCount;

    /** Opacity slider changed events, which is used by the mouse recorder. */
    private MouseEventVector opacityEvents;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new component histogram LUT.
     *
     * @param  renderOpacityPanel  Frame where histogram is to be displayed
     * @param  _histo              histogram model
     * @param  _image              image of the displayed histogram and associated LUT
     */
    public ViewJComponentVolOpacity(JPanelVolOpacity renderOpacityPanel, ModelHistogram _histo, ModelImage _image) {
        super(renderOpacityPanel, _histo, _image, new Dimension(450, 375));

        setupMinMax();

        myParent = renderOpacityPanel.getParentFrame();
        m_kVolumeViewer = renderOpacityPanel.getParentVolumeViewer();

        ModelLUT lut = new ModelLUT(ModelLUT.GRAY, 256, new int[] { 4, 256 });

        lutIndexBuffer = new int[256];
        lut.exportIndexedLUT(lutIndexBuffer);
        lut.disposeLocal();

        linearMode();
    }


    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Sets variables to null and gets rid of frame.
     */
    public void disposeLocal() {

        if (histogram != null) {
            histogram.disposeLocal();
            histogram = null;
        }

        myParent = null;
        histogramBuffer = null;
        pixBuffer = null;
        lutIndexBuffer = null;
        img = null;
        image = null;
    }

    /**
     * Checks for making new points or not.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mousePressed(MouseEvent mouseEvent) {
        super.mousePressed(mouseEvent);

        if (myParent instanceof SurfaceRender) {
            JPanelMouse myMouseDialog = ((SurfaceRender) myParent).getMouseDialog();

            if (myMouseDialog.mode == JPanelMouse.RECORD_MODE) {
                Transform3D t3D = new Transform3D();
                myParent.getSceneRootTG().getTransform(t3D);
                double[] mat = new double[16];
                t3D.get(mat);
                opacityEvents = new MouseEventVector("VolOpacity" + opacityCount, mat, myMouseDialog.first,
                                                     ((SurfaceRender) myParent).getSceneState(),
                                                     ((SurfaceRender) myParent).getMouseMode());
                myMouseDialog.listModel.addElement("VolOpacity" + opacityCount);
            }
        }

    }

    /**
     * Updates image and shows it.
     *
     * @param  mouseEvent  event that triggered this function
     */
    public void mouseReleased(MouseEvent mouseEvent) {
        showHistogram();

        if (myParent instanceof SurfaceRender) {

            if (((SurfaceRender) myParent).getMouseDialog().isRecording()) {
                opacityEvents.add(mouseEvent, myParent.getSceneState());
                ((SurfaceRender) myParent).getMouseDialog().events.add(opacityEvents);
                opacityCount++;
            }
        }
    }

    /**
     * Accessor that sets the mode.
     *
     * @param  _mode  the mode of the histogram
     */
    public void setMode(int _mode) {
        mode = _mode;
        showHistogram();
    }

    /**
     * Filters the histogram through the LUT and produces an int array (aRGB) that can be converted to a java image of
     * the histogram for display purposes.
     */
    public void showHistogram() {
        showHistogram(true);
    }

    /**
     * Filters the histogram through the LUT and produces an int array (aRGB) that can be converted to a java image of
     * the histogram for display purposes.
     *
     * @param  repaint  whether to repaint the histogram graph
     */
    public void showHistogram(boolean repaint) {
        int i, j, k;
        double sum;
        int end, aRGB;
        float iNew;
        int offset;

        if (histogram == null) {
            return;
        }

        try { // get histogram
            histogram.exportData(0, histogramBuffer.length, histogramBuffer);
        } catch (IOException error) {
            MipavUtil.displayError("IOException: ComponentVolOpacity.showHistogram");

            return;
        }

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
            histogramMaxLog = Math.log(histogramMax);
        }

        // Builds special buffer by remapping the data in the LUT range
        int lightBlue = 0xFFAFAFF0;

        Arrays.fill(pixBuffer, lightBlue);

        for (i = 0; i < (dim.width - 1); i++) {

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

            int index = MipavMath.round(transferFunction.getRemappedValue(iNew, dim.height));

            if (index < 0) {
                index = 0;
            }

            if (index > (lutIndexBuffer.length - 1)) {
                index = lutIndexBuffer.length - 1;
            }

            aRGB = lutIndexBuffer[index];

            // build histogram image
            offset = (dim.width * (dim.height - 1)) + i;

            for (j = 0; j < end; j++) {
                pixBuffer[offset - (j * dim.width)] = aRGB;
            }
        }

        importImage(pixBuffer); // Method in parent class to import the image

        if (repaint) {
            repaint();
        }
    }

    /**
     * Placeholder. Will go away.
     *
     * @param  newLUT  histogram filter lut
     */
    public void showHistogram(ModelLUT newLUT) { }

    /**
     * Clean up some resources!
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
    }

    /**
     * Set the min max values for the opacity histogram.
     */
    private void setupMinMax() {

        if (image.getType() == ModelStorageBase.UBYTE) {
            min = 0;
            max = 255;
        } else if (image.getType() == ModelStorageBase.BYTE) {
            min = -128;
            max = 127;
        } else if (image.isColorImage()) {
            min = 0;
            max = 255;
        } else {
            min = (float) image.getMin();
            max = (float) image.getMax();
        }

        range = (int) (max - min);
    }
}
