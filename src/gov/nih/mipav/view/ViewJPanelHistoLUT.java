package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.border.*;


/**
 * Panel containing the histogram component (the graph) and the lut component (the gradient vertical bar).
 *
 * @author   Evan McCreedy
 * @version  1.0
 */
public class ViewJPanelHistoLUT extends JPanel implements HistoLUTParent, MouseListener, MouseMotionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3873204112932340987L;

    /** Indicates a grayscale LUT. */
    public static final int GRAY_LUT = 0;

    /** Indicates a RGB LUT. */
    public static final int RGB_LUT = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The LUT color chooser. */
    private ViewJColorChooser colorChooser;

    /** The histogram LUT of the image. The square one. */
    private ViewJComponentHLUTBase componentHistogram;

    /** Lookup table of image. The narrow rectangular one. */
    private ViewJComponentLUT componentLUT;

    /** Model histogram. */
    private ModelHistogram histogram = null;

    /** The type of histogram component to show. */
    private int histoLUTType;

    /** The image. */
    private ModelImage image;

    /** The grayscale lookup table for the image. */
    private ModelLUT lut;

    /** The LUT index used when chaning the LUT color by clicking on the ViewJComponentLUT. */
    private int LUTIndex;

    /** LUT recorder. */
    private JDialogRecordLUT lutRecorder;

    /** The component containing this panel. */
    private HistoLUTParent parent;

    /** The RGB lookup table. */
    private ModelRGB RGBT;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create this panel.
     *
     * @param  parent     component which will hold this panel
     * @param  image      the image
     * @param  lut        the image lut
     * @param  histogram  the image histogram
     */
    public ViewJPanelHistoLUT(HistoLUTParent parent, ModelImage image, ModelStorageBase lut, ModelHistogram histogram) {

        this.parent = parent;
        this.image = image;
        this.histogram = histogram;

        this.lut = (lut instanceof ModelLUT) ? (ModelLUT)lut : null;
        this.RGBT = (lut instanceof ModelRGB) ? (ModelRGB)lut : null;
        if ( this.lut != null )
        {
        	this.histoLUTType = GRAY_LUT;
        }
        else
        {
        	this.histoLUTType = RGB_LUT;
        }

        initGUI();
    }
    
    /**
     * Create this panel.
     *
     * @param  parent     component which will hold this panel
     * @param  image      the image
     * @param  lut        the image lut
     * @param  histogram  the image histogram
     * @deprecated
     */
    public ViewJPanelHistoLUT(HistoLUTParent parent, ModelImage image, ModelLUT lut, ModelHistogram histogram) {
        this.histoLUTType = GRAY_LUT;

        this.parent = parent;
        this.image = image;
        this.lut = lut;
        this.histogram = histogram;

        initGUI();
    }

    /**
     * Creates a new ViewJPanelHistoLUT object.
     *
     * @param  parent     HistoLUTParent
     * @param  image      ModelImage
     * @param  RGBT       ModelRGB
     * @param  histogram  ModelHistogram
     * @deprecated
     */
    public ViewJPanelHistoLUT(HistoLUTParent parent, ModelImage image, ModelRGB RGBT, ModelHistogram histogram) {
        this.histoLUTType = RGB_LUT;

        this.parent = parent;
        this.image = image;
        this.RGBT = RGBT;
        this.histogram = histogram;

        initGUI();
    }

    /**
     * Deletes local memory.
     */
    public void disposeLocal()
    {
        parent = null;
        image = null;
        lut = null;
        RGBT = null;
        histogram = null;

        if ( componentLUT != null )
        {
        	componentLUT.dispose();
        	componentLUT = null;
        }
        if ( componentHistogram != null )
        {
        	componentHistogram.dispose();
        	componentHistogram = null;
        }
    }
    
    
    
    /**
     * {@inheritDoc}
     */
    public void dragPoint(MouseEvent mouseEvent) {
        parent.dragPoint(mouseEvent);
    }

    /**
     * Clean up the panel memory.
     */
    public void finalize() {

        if (componentHistogram != null) {
            componentHistogram.dispose();
            componentHistogram = null;
        }

        if (componentLUT != null) {
            componentLUT.dispose(false);
            componentLUT = null;
        }

        if (lutRecorder != null) {
            lutRecorder.setVisible(false);
            lutRecorder.dispose();
            lutRecorder = null;
        }
    }

    /**
     * Get the histogram LUT component.
     *
     * @return  the histogram component
     */
    public ViewJComponentHLUTBase getHistoLUTComponent() {
        return componentHistogram;
    }

    /**
     * Get the LUT component.
     *
     * @return  the lut component
     */
    public ViewJComponentLUT getLUTComponent() {
        return componentLUT;
    }

    /**
     * Get the LUT recorder reference.
     *
     * @return  JDialogRecordLUT recorder dialog reference
     */
    public JDialogRecordLUT getLUTRecorder() {
        return lutRecorder;
    }

    /**
     * Construct the panel.
     */
    public void initGUI() {
        int borderSize = 3;

        Dimension prefSize = null;

        JPanel LUTPanel = new JPanel();

        LUTPanel.setBorder(new EtchedBorder());

        if (histoLUTType == GRAY_LUT) {

            // Make a displayable version of the LUT
            componentLUT = new ViewJComponentLUT(lut, new Dimension(20, lut.getExtents()[1]),
                                                 ViewJComponentLUT.VERTICAL);
            componentLUT.show(null);
            componentLUT.setLocation(borderSize, borderSize);
            componentLUT.addMouseMotionListener(this);
            componentLUT.addMouseListener(this);
            componentLUT.setToolTipText("Lookup Table (LUT)");

            LUTPanel.setLayout(null);
            LUTPanel.add(componentLUT);
            LUTPanel.setBounds(34, 20, componentLUT.getSize().width + (2 * borderSize),
                               componentLUT.getSize().height + (2 * borderSize));

            // Make a display version of the histogram
            componentHistogram = new ViewJComponentHistoLUT(this, histogram, lut, image);
            componentHistogram.addMouseMotionListener(this);
            componentHistogram.addMouseListener(this);
            componentHistogram.setLocation(borderSize, borderSize);
            componentHistogram.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
            componentHistogram.setBackground(new Color(190, 208, 230));
            componentHistogram.showHistogram();

            prefSize = new Dimension(componentHistogram.getSize().width + componentLUT.getSize().width +
                                     (2 * borderSize), componentHistogram.getSize().height + (2 * borderSize));

            this.setLayout(null);
            this.add(LUTPanel);
        } else if (histoLUTType == RGB_LUT) {

            // Make a display version of the histogram
            componentHistogram = new ViewJComponentHistoRGB(this, histogram, RGBT, image);
            componentHistogram.setLocation(borderSize, borderSize);
            componentHistogram.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
            componentHistogram.setBackground(new Color(190, 208, 230));
            componentHistogram.setMode(ViewJComponentHLUTBase.RED);

            prefSize = new Dimension(componentHistogram.getSize().width + (2 * borderSize),
                                     componentHistogram.getSize().height + (2 * borderSize));
        }

        this.add(componentHistogram);
        this.setPreferredSize(prefSize);
        this.setBorder(new EtchedBorder());

        if (!image.isColorImage()) {
            lutRecorder = new JDialogRecordLUT(image, lut);
        }
    }

    /**
     * {@inheritDoc}
     */
    public boolean isImageUpdate() {
        return parent.isImageUpdate();
    }

    /**
     * HistoLUTParent end.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */

    /**
     * MouseListener.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */

    /**
     * Calls color chooser.
     *
     * @param  mouseEvent  Event that triggered function
     */
    public void mouseClicked(MouseEvent mouseEvent) {

        if (mouseEvent.getSource() instanceof ViewJComponentLUT) {

            try {
                Frame frame = null;

                if (parent instanceof Frame) {
                    frame = (Frame) parent;
                } else {
                    frame = ViewUserInterface.getReference().getActiveImageFrame();
                }

                colorChooser = new ViewJColorChooser(frame, "Pick LUT index color", new OkColorListener(),
                                                     new CancelColorListener());
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: ViewJFrameHistoLUT.mouseClicked");
            }
        }
    }

    /**
     * end MouseListener.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */

    /**
     * MouseMotionListener.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */

    /**
     * Continually updates the image depending on where the mouse is.
     *
     * @param  mouseEvent  event that triggered this function
     */
    public void mouseDragged(MouseEvent mouseEvent) {


        if (mouseEvent.getSource() instanceof ViewJComponentHLUTBase) {
            ((ViewJComponentHistoLUT) componentHistogram).dragPoint(mouseEvent);
            
            // updateFrames(false);
            //componentHistogram.showHistogram();

            if (lutRecorder != null) {
                lutRecorder.dragPoint(((ViewJComponentHistoLUT) componentHistogram).getLUT());
                lutRecorder.update(lutRecorder.getGraphics());
            }
        }
    }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  event
     */
    public void mouseEntered(MouseEvent mouseEvent) { }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  event
     */
    public void mouseExited(MouseEvent mouseEvent) { }

    /**
     * Changes the LUT text field display based on the Y value of the mouse when inside ViewJComponentLUT.
     *
     * @param  mouseEvent  Event that triggered this function
     */
    public void mouseMoved(MouseEvent mouseEvent) {
        Object source = mouseEvent.getSource();

        if (source instanceof ViewJComponentLUT) {

            if ((colorChooser != null) && (colorChooser.isVisible() == true)) {
                return;
            }

            LUTIndex = 255 - mouseEvent.getY();

            updateLUTPositionString(String.valueOf(LUTIndex) + ": (" +
                                    MipavUtil.makeFloatString(lut.getFloat(0, LUTIndex), 2) + ", " +
                                    Math.round(lut.getFloat(1, LUTIndex)) + ", " +
                                    Math.round(lut.getFloat(2, LUTIndex)) + ", " +
                                    Math.round(lut.getFloat(3, LUTIndex)) + ")");
        } else if (source instanceof ViewJComponentHLUTBase) {
            float x = mouseEvent.getX();
            float y = mouseEvent.getY();

            componentHistogram.checkProximityToTransferFunction(x, y, mouseEvent.isShiftDown());
        }
    }

    /**
     * Checks whether the user is making new points in the lut function or not.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mousePressed(MouseEvent mouseEvent) {

        if (mouseEvent.getSource() instanceof ViewJComponentHLUTBase) {
            float x = mouseEvent.getX();
            float y = mouseEvent.getY();

            if (!mouseEvent.isShiftDown()) {
                componentHistogram.addFunctionPoint(x, y);
            } else {
                componentHistogram.removeClickedFunctionPoint();
            }

            if (lutRecorder != null) {
                lutRecorder.dragPoint(((ViewJComponentHistoLUT) componentHistogram).getLUT());
            }
        }
    }

    /**
     * Updates the image and shows it in case a new point has been added to the function (or removed).
     *
     * @param  mouseEvent  the event that triggered this function
     */
    public void mouseReleased(MouseEvent mouseEvent) {

        if (mouseEvent.getSource() instanceof ViewJComponentHLUTBase) {
            updateFrames(false);
            componentHistogram.showHistogram();
        }
    }

    /**
     * {@inheritDoc}
     */
    public void setAllOff() {
        parent.setAllOff();
    }

    /**
     * HistoLUTParent interface (pass along gui-related things which aren't in our panel).
     *
     * @param  newLUT  DOCUMENT ME!
     */

    /**
     * {@inheritDoc}
     */
    public void setLUT(ModelLUT newLUT) {
        parent.setLUT(newLUT);
    }

    /**
     * {@inheritDoc}
     */
    public void setRangeText(float x, float y, int _index) {
        parent.setRangeText(x, y, _index);
    }

    /**
     * Show the LUT recorder dialog.
     */
    public void showLUTRecorder() {

        if (!image.isColorImage()) {

            if (lutRecorder == null) {
                lutRecorder = new JDialogRecordLUT(image, lut);
            } else {
                lutRecorder.setVisible(true);
            }
        }

    }

    /**
     * {@inheritDoc}
     */
    public void updateComponentLUT() {
        componentLUT.show(lut);
    }

    /**
     * {@inheritDoc}
     */
    public void updateFrames(boolean flag) {
        parent.updateFrames(flag);
    }

    /**
     * {@inheritDoc}
     */
    public void updateLUTPositionString(String str) {
        parent.updateLUTPositionString(str);
    }

    /**
     * Update the LUT recorder table transfer function.
     */
    public void updateLUTRecorder() {

        if (lutRecorder != null) {
            lutRecorder.dragPoint(((ViewJComponentHistoLUT) componentHistogram).getLUT());
        }
    }

    /**
     * Update the LUT table of LUTRecorder.
     *
     * @param  _lut  ModelLUT lut table
     */
    public void updateLUTRecorder(ModelLUT _lut) {

        if (lutRecorder != null) {
            lutRecorder.updateLUT(_lut);
        }
    }

    /**
     * {@inheritDoc}
     */
    public void updateThresholdFields(float lower, float upper) {
        parent.updateThresholdFields(lower, upper);
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Notice when the user cancels the color chooser dialog.
     */
    private class CancelColorListener implements ActionListener {

        /**
         * DOCUMENT ME!
         *
         * @param  e  DOCUMENT ME!
         */
        public void actionPerformed(ActionEvent e) {
            colorChooser.setVisible(false);
        }
    }

    /**
     * Pick up the selected color and change the image border color.
     */
    private class OkColorListener implements ActionListener {

        /**
         * DOCUMENT ME!
         *
         * @param  e  DOCUMENT ME!
         */
        public void actionPerformed(ActionEvent e) {
            Color color = colorChooser.getColor();

            colorChooser.setVisible(false);

            if (componentHistogram != null) {
                lut.setColor(LUTIndex, color);
                setLUT(lut);
            }

            updateFrames(false);
        }
    }

    /************** end MouseMotionListener **********/

}
