package gov.nih.mipav.view;


import gov.nih.mipav.model.scripting.ScriptRecorder;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;


/**
 * This class builds the control panel used in ViewJFrameImage frames. Toolbars are created and added to the panel and
 * the listener for the toolbars is the image frame. If the image frame is to display two images then alpha blending
 * tools are added and displayed.
 *
 * @version  0.1 Dec 15, 1998 update 9 august 2001
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class ViewControlsImage extends JPanel implements ChangeListener, ActionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3317301000984234468L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected JSlider alphaSlider;

    /** DOCUMENT ME! */
    protected TitledBorder borderActiveImage;

    /** DOCUMENT ME! */
    protected TitledBorder borderImageSlider;

    /** DOCUMENT ME! */
    protected ViewJFrameBase frame; // image(s) frame

    /** DOCUMENT ME! */
    protected GridBagConstraints gbc;

    /** DOCUMENT ME! */
    protected JPanel generalPanel;

    /** DOCUMENT ME! */
    protected ButtonGroup group1;

    /** DOCUMENT ME! */
    protected int imageSize;

    /** JSlider.majorSpacing/minorSpacing. */
    protected int majorSpacing = 100; // 25;

    /** DOCUMENT ME! */
    protected int minorSpacing = 20; // 5;

    /** Paint toolbar. */
    protected JToolBar paintToolBar;

    /** DOCUMENT ME! */
    protected JPanel panelActiveImage;

    /** DOCUMENT ME! */
    protected JPanel panelImageSlider;

    /** DOCUMENT ME! */
    protected JPanel panelOptionToolbars;

    /** DOCUMENT ME! */
    protected JPanel panelToolbars;

    /** DOCUMENT ME! */
    protected JRadioButton radioImageA;

    /** DOCUMENT ME! */
    protected JRadioButton radioImageB;

    /** Registration toolbar. */
    protected JToolBar registrationToolBar;

    /** DOCUMENT ME! */
    protected JToolBar scriptToolBar;

    /**
     * the slice slider is a JSlider defining an integer number of steps. That number of steps is given by
     * sliderResolution. Although the major&minor tick marks will not necessarily align with any particular slice, there
     * is built-in ability to have the tick-marks correspond to a particular percentage of an image set. ie, distance
     * between each major tick will represent 1/10 of image-set, and the minor ticks will be 1/5 of that 1/10. Should
     * the number of slices the slider must refer to be larger than the resolution the slider will not always refer to
     * the correct slice when moved.
     */
    protected int sliderResolutionInt = 1000;

    /** DOCUMENT ME! */
    protected float sliderResolutionFloat = (float) sliderResolutionInt;

    /** DOCUMENT ME! */
    protected JSlider tImageSlider;

    /** DOCUMENT ME! */
    protected Hashtable tImageSliderDictionary = new Hashtable();

    /** Main toolbar . */
    protected JToolBar toolBar;

    /** Object builds and controls the toolbars. */
    protected ViewToolBarBuilder toolBarObj;

    /** DOCUMENT ME! */
    protected ViewToolBarBuilder.JVOIToolBar VOIToolBar;

    /** DOCUMENT ME! */
    protected JSlider zImageSlider;

    /** DOCUMENT ME! */
    protected Hashtable zImageSliderDictionary = new Hashtable();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new control panel.
     *
     * @param  _frame  Parent frame.
     */
    public ViewControlsImage(ViewJFrameBase _frame) {

        setLayout(new BorderLayout());
        frame = _frame;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // **************************************************************************
    // *************************** Action listener ******************************
    // **************************************************************************

    /**
     * Action event handler. Switches active image between image A and image B.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        // Set the active image to image A or to image B
        if ((source == radioImageA) || (source == radioImageB)) {
            frame.setPaintBitmapSwitch(true);

            if (radioImageA.isSelected()) {
                frame.setActiveImage(ViewJFrameImage.IMAGE_A); // Image A
                frame.updateImages();
                // Sets the spinner (on paint toolbar) values used in paint based on image type
                toolBarObj.setSpinnerValues(frame.getImageA().getType());
            } else if (radioImageB.isSelected()) {
                frame.setActiveImage(ViewJFrameImage.IMAGE_B); // Image B
                frame.updateImages();
                toolBarObj.setSpinnerValues(frame.getImageB().getType());
            }
        }
    }

    /**
     * Adds a component to the toolbar panel using the given gridbag constraints.
     *
     * @param  c    Component to add.
     * @param  gbc  Grid bag constraints for component.
     * @param  x    <code>GridBagConstraints.gridx</code>
     * @param  y    <code>GridBagConstraints.gridy</code>
     * @param  w    <code>GridBagConstraints.gridwidth</code>
     * @param  h    <code>GridBagConstraints.gridheight</code>
     *
     * @see    GridBagConstraints
     */
    public void add(Component c, GridBagConstraints gbc, int x, int y, int w, int h) {
        gbc.gridx = x;
        gbc.gridy = y;
        gbc.gridwidth = w;
        gbc.gridheight = h;
        panelToolbars.add(c, gbc);
    }

    /**
     * Displays the panel which controls which image is the "active image" displayed when imageB is available.
     */
    public void addActiveImageControl() {

        if (frame.getImageB() != null) {
            add(panelActiveImage, "Center");
            toolBarObj.setRegButtonEnabled(true); // This is a hack but it works - window region in imageB
            toolBarObj.setCheckboardButtonEnabled(true); // checker board
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  showImage  Indicates if the image toolbar is shown.
     * @param  showVOI    Indicates if the VOI toolbar is shown.
     * @param  showPaint  Indicates if the paint toolbar is shown.
     * @param  voiIndex   Indicates the index of the currently (or previously) selected VOI
     */
    public void buildToolbar(boolean showImage, boolean showVOI, boolean showPaint, int voiIndex) {
        buildToolbar(showImage, showVOI, showPaint, false, voiIndex);
    }

    /**
     * Builds the toolbars.
     *
     * @param  showImage      Indicates if the image toolbar is shown.
     * @param  showVOI        Indicates if the VOI toolbar is shown.
     * @param  showPaint      Indicates if the paint toolbar is shown.
     * @param  showScripting  Indicates if the scripting toolbar is shown.
     * @param  voiIndex       Indicates the index of the currently (or previously) selected VOI
     */
    public void buildToolbar(boolean showImage, boolean showVOI, boolean showPaint, boolean showScripting,
                             int voiIndex) {

        int zDim, tDim;
        int numberOfDimensions;
        Font font12 = MipavUtil.font12;

        toolBar = null;
        VOIToolBar = null;
        panelToolbars = new JPanel();
        panelOptionToolbars = new JPanel();
        generalPanel = new JPanel();
        toolBarObj = new ViewToolBarBuilder(frame);

        panelToolbars.setLayout(new BorderLayout());
        panelOptionToolbars.setLayout(new BorderLayout());
        generalPanel.setLayout(new BorderLayout());

        numberOfDimensions = frame.getImageA().getNDims();

        if (frame.getImageB() != null) {
            numberOfDimensions = Math.max(numberOfDimensions, frame.getImageB().getNDims());
        }

        if (showVOI) {
            VOIToolBar = toolBarObj.buildVOIToolBar(numberOfDimensions, voiIndex);
            panelToolbars.add(VOIToolBar, "North");
        }

        if (showPaint) {
            paintToolBar = toolBarObj.buildPaintToolBar(frame.getImageA().getType(), numberOfDimensions);
            panelOptionToolbars.add(paintToolBar, "North");
        }

        if (showScripting) {
            scriptToolBar = toolBarObj.buildScriptToolBar(ScriptRecorder.getReference().getRecorderStatus() == ScriptRecorder.RECORDING);
            panelOptionToolbars.add(scriptToolBar, "West");
        }

        panelToolbars.add(panelOptionToolbars, "Center");

        if (showImage) {
            toolBar = toolBarObj.buildGeneralToolBar(numberOfDimensions, frame.getImageA().getType());
            panelToolbars.add(toolBar, "South");
        }

        if (panelImageSlider != null) {
            generalPanel.remove(panelImageSlider);
            panelImageSlider = null;
        }

        if (zImageSlider != null) {
            zImageSlider.removeChangeListener(this);
            zImageSlider = null;
        }

        if (tImageSlider != null) {
            tImageSlider.removeChangeListener(this);
            tImageSlider = null;
        }

        System.gc();

        if (numberOfDimensions == 4) {
        	zDim = frame.getImageA().getExtents()[2];
            panelImageSlider = new JPanel();
            panelImageSlider.setLayout(new GridLayout(2, 1));
            panelImageSlider.setForeground(Color.black);
            borderImageSlider = new TitledBorder(" Image slice index  [total number slices=" + zDim + "] ");
            borderImageSlider.setTitleColor(Color.black);
            borderImageSlider.setTitleFont(MipavUtil.font12B);
            borderImageSlider.setBorder(new EtchedBorder());
            panelImageSlider.setBorder(borderImageSlider);

            if (frame.getImageA().getNDims() == 4) {
                tDim = frame.getImageA().getExtents()[3];
            } else {
                tDim = frame.getImageB().getExtents()[3];
            }

            buildTImageSliderLabels(0, tDim);
            tImageSlider = new JSlider(JSlider.HORIZONTAL, 0, sliderResolutionInt, sliderResolutionInt / 2);
            tImageSlider.setMajorTickSpacing(majorSpacing);
            tImageSlider.setMinorTickSpacing(minorSpacing);
            tImageSlider.setPaintTicks(true);
            tImageSlider.setPaintLabels(true);
            tImageSlider.setLabelTable(tImageSliderDictionary);
            tImageSlider.setValue(0);
            tImageSlider.setVisible(true);
            tImageSlider.addChangeListener(this);

            
            buildZImageSliderLabels(0, zDim);
            zImageSlider = new JSlider(JSlider.HORIZONTAL, 0, sliderResolutionInt, sliderResolutionInt / 2);
            zImageSlider.setMajorTickSpacing(majorSpacing);
            zImageSlider.setMinorTickSpacing(minorSpacing);
            zImageSlider.setPaintTicks(true);
            zImageSlider.setPaintLabels(true);
            zImageSlider.setLabelTable(zImageSliderDictionary);

            // could have used local meth setZslider(extents/2),
            // but there are too many other commands that could
            // just as well be used here:
            zImageSlider.setValue(sliderResolutionInt / 2); // set midpoint

            panelImageSlider.add(zImageSlider);
            panelImageSlider.add(tImageSlider);

            generalPanel.add(panelToolbars, "North");
            generalPanel.add(panelImageSlider, "South");
            zImageSlider.setVisible(true);
            zImageSlider.addChangeListener(this);
        } else if (numberOfDimensions == 3) {
        	zDim = frame.getImageA().getExtents()[2];
            panelImageSlider = new JPanel();
            panelImageSlider.setLayout(new GridLayout(1, 1));

            // panelImageSlider.setLayout(new BorderLayout());
            panelImageSlider.setForeground(Color.black);
            borderImageSlider = new TitledBorder(" Image slice index  [total number slices=" + zDim + "] ");
            borderImageSlider.setTitleColor(Color.black);
            borderImageSlider.setTitleFont(MipavUtil.font12B);
            borderImageSlider.setBorder(new EtchedBorder());
            panelImageSlider.setBorder(borderImageSlider);

            
            buildZImageSliderLabels(0, zDim);

            // System.out.println("resol = " + sliderResolutionInt);
            zImageSlider = new JSlider(JSlider.HORIZONTAL, 0, sliderResolutionInt, sliderResolutionInt / 2);
            zImageSlider.setMajorTickSpacing(majorSpacing);
            zImageSlider.setMinorTickSpacing(minorSpacing);
            zImageSlider.setPaintTicks(true);
            zImageSlider.setPaintLabels(true);
            zImageSlider.setLabelTable(zImageSliderDictionary);

            // could have used local meth setZslider(extents/2),
            // but there are too many other commands that could
            // just as well be used here:
            // zImageSlider.setValue(sliderResolutionInt/2);   // set midpoint
            // updateZImageSlider(zDim/2);
            zImageSlider.setVisible(true);
            zImageSlider.addChangeListener(this);
            panelImageSlider.add(zImageSlider);
            generalPanel.add(panelToolbars, "North");
            generalPanel.add(panelImageSlider, "South");
        } else {
            generalPanel.add(panelToolbars, "Center");
        }

        // generalPanel.validate();
        add(generalPanel, "North");

        panelActiveImage = new JPanel();
        panelActiveImage.setLayout(new BorderLayout());
        borderActiveImage = new TitledBorder("Active Image and Alpha Blending");
        borderActiveImage.setTitleColor(Color.black);
        borderActiveImage.setTitleFont(MipavUtil.font12B);
        borderActiveImage.setBorder(new EtchedBorder());
        panelActiveImage.setBorder(borderActiveImage);

        // Only add controls if two images are in the frame
        if (frame.getImageB() != null) {
            add(panelActiveImage, "Center");
        }

        group1 = new ButtonGroup();
        radioImageA = new JRadioButton("Image A      ", true);
        radioImageA.setFont(font12);
        group1.add(radioImageA);
        radioImageA.addActionListener(this);
        panelActiveImage.add(radioImageA, BorderLayout.WEST);

        radioImageB = new JRadioButton("Image B       ", false);
        radioImageB.setFont(font12);
        group1.add(radioImageB);
        radioImageB.addActionListener(this);
        panelActiveImage.add(radioImageB, BorderLayout.EAST);
        buildAlphaSlider();

        // validate();
    }

    /**
     * Clean up memory.
     */
    public void finalize() {
        frame = null; // image(s) frame
        toolBar = null;
        VOIToolBar = null;
        paintToolBar = null;
        registrationToolBar = null;
        scriptToolBar = null;
        panelToolbars = null;
        panelOptionToolbars = null;
        generalPanel = null;
        toolBarObj = null;

        panelActiveImage = null;
        borderActiveImage = null;
        group1 = null;
        radioImageA = null;
        radioImageB = null;

        panelImageSlider = null;
        borderImageSlider = null;
        zImageSlider = null;
        zImageSliderDictionary = null;
        tImageSlider = null;
        tImageSliderDictionary = null;
        alphaSlider = null;
        gbc = null;
    }

    /**
     * Identifies which image is active.
     *
     * @return  the identifier for the active image
     */
    public int getActiveImage() {

        if (radioImageA.isSelected()) {
            return ViewJFrameImage.IMAGE_A;
        } else if (radioImageB.isSelected()) {
            return ViewJFrameImage.IMAGE_B;
        }

        return -1;
    }

    /**
     * Accessor for the ViewJFrameBase used.
     *
     * @return  the frame used
     */
    public ViewJFrameBase getFrame() {
        return frame;
    }

    /**
     * Accessor for the tool bar held by these controls.
     *
     * @return  The tool bar held by this controls.
     */
    public ViewToolBarBuilder getTools() {
        return toolBarObj;
    }

    /**
     * Removes the controls to the display of which image is "active" in the frame when imageB is not available.
     */
    public void removeActiveImageControl() {

        if (frame.getImageB() == null) {
            remove(panelActiveImage);
            toolBarObj.setRegButtonEnabled(false); // This is a hack but it works - window region in imageB
            toolBarObj.setCheckboardButtonEnabled(false); // checker board
        }
    }

    /**
     * Invokes the toolbar builder to run the currently selected script.
     */
    public void runCurrentScript() {

        if (scriptToolBar == null) {
            return;
        }

        toolBarObj.runCurrentScript();

    }

    /**
     * Sets the active image to either A or B and updates the paint spinner values.
     *
     * @param  image  Indicates which image is active.
     */
    public void setActiveImage(int image) {

        if (image == ViewJFrameImage.IMAGE_A) {
            radioImageA.setSelected(true);
            radioImageB.setSelected(false);

            // Sets the spinner (on paint toolbar) values used in paint based on image type
            toolBarObj.setSpinnerValues(frame.getImageA().getType());
        }

        if (image == ViewJFrameImage.IMAGE_B) {
            radioImageA.setSelected(false);
            radioImageB.setSelected(true);
            toolBarObj.setSpinnerValues(frame.getImageB().getType());
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  enabled  DOCUMENT ME!
     */
    public void setAlphaSliderEnabled(boolean enabled) {
        alphaSlider.setEnabled(enabled);
    }

    /**
     * Used to set the state for the script recorder.
     *
     * @param  isRecording  boolean
     */
    public void setRecording(boolean isRecording) {

        if (scriptToolBar != null) {
            ((ViewToolBarBuilder.JScriptToolBar) scriptToolBar).setRecording(isRecording);
        }
    }

    
    /**
     * Directs the image to display a particular time slice.
     *
     * @param  slice  The time slice to display.
     *
     * @see    ModelImage#setTimeSlice(int)
     */
    public void setTimeSl(int slice) {

        /* called by ViewJFrameTriImage */
        frame.getImageA().setTimeSlice(slice);
    }

    /**
     * Places the pointer on the (time) slider bar to a location corresponding to the given slice value (within the
     * set).
     *
     * @param  tSlice  The slice in the frame that is displayed.
     *
     * @see    JSlider
     */
    public void setTSlider(int tSlice) {
        int newValue;

        if (tImageSlider == null) {
            return;
        }

        if (frame.getImageA().getNDims() == 4) {
            newValue = Math.round((sliderResolutionFloat * tSlice / (frame.getImageA().getExtents()[3] - 1)) - 0.01f);
        } else {
            newValue = Math.round((sliderResolutionFloat * tSlice / (frame.getImageB().getExtents()[3] - 1)) - 0.01f);
        }

        tImageSlider.removeChangeListener(this);
        tImageSlider.setValue(newValue);
        tImageSlider.addChangeListener(this);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  voiUID  DOCUMENT ME!
     */
    public void setVOIColor(int voiUID) {

        if (VOIToolBar != null) {
            VOIToolBar.setVOIColor(voiUID);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  newColor  DOCUMENT ME!
     */
    public void setVOIColor(Color newColor) {

        if (VOIToolBar != null) {
            VOIToolBar.setVOIColor(newColor);
        }
    }

    /**
     * Places the pointer on the (slice) slider bar to a location corresponding to the given slice value (within the
     * set) without notifying the listeners.
     *
     * @param  zSlice  the slice in the frame that is displayed
     *
     * @see    JSlider
     * @see    ViewControlsImage#updateZImageSlider(int)
     */
    public void setZSlider(int zSlice) {
        int newValue;

        if (zImageSlider == null) {
            return;
        }

        if (frame.getImageA().getNDims() >= 3) {
            newValue = Math.round((sliderResolutionFloat * zSlice / (frame.getImageA().getExtents()[2] - 1)) - 0.01f);
            // newValue = zSlice;
        } else {
            newValue = Math.round((sliderResolutionFloat * zSlice / (frame.getImageB().getExtents()[2] - 1)) - 0.01f);
            // newValue = zSlice;
        }

        zImageSlider.removeChangeListener(this);
        zImageSlider.setValue(newValue);
        zImageSlider.addChangeListener(this);
    }

    // **************************************************************************
    // *************************** Change listener ******************************
    // **************************************************************************

    /**
     * Sets values based on knob along slider.
     *
     * @param  e  Event that triggered this function.
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();
        int newValue = 1;

        if (source == zImageSlider) {
            newValue = Math.round((zImageSlider.getValue() / sliderResolutionFloat *
                                       (frame.getImageA().getExtents()[2] - 1)) - 0.01f);

            if (frame.getImageA().getLightBoxFrame() != null) {
                frame.getImageA().getLightBoxFrame().setSlice(newValue);
            }

            frame.getImageA().setSlice(newValue);
        } else if (source == tImageSlider) {

            if (frame.getImageA().getNDims() == 4) {
                newValue = Math.round((tImageSlider.getValue() / sliderResolutionFloat *
                                           (frame.getImageA().getExtents()[3] - 1)) - 0.01f);
            } else {
                newValue = Math.round((tImageSlider.getValue() / sliderResolutionFloat *
                                           (frame.getImageB().getExtents()[3] - 1)) - 0.01f);
            }

            frame.getImageA().setTimeSlice(newValue);
        } else if (source == alphaSlider) {

            if ((alphaSlider.getValueIsAdjusting() == true) && (imageSize > (1024 * 1024))) {
                return;
            }

            updateFrames(true, 100 - alphaSlider.getValue());
        }
    }

    /**
     * Calls ModelImage's updateFrames method.
     *
     * @see    ModelImage#notifyImageDisplayListners
     *
     * @param  flag        this boolean indicates if the frame that displays the image should re-export the image and
     *                     apply new LUT
     * @param  alphaBlend  the amount of alpha blending to do
     */
    public void updateFrames(boolean flag, int alphaBlend) {

        if (frame.getImageA() != null) {
            frame.getImageA().notifyImageDisplayListeners(null, flag, alphaBlend, -1);
        }
    }

    /**
     * Invokes the toolbar builder to update the scripts combo box based on the new script directory.
     *
     * @param  dir  - name of directory for script files.
     */
    public void updateScripts(String dir) {

        if (scriptToolBar == null) {
            return;
        }

        toolBarObj.updateScripts(dir);

    } // end updateScripts()

    /**
     * Places the pointer on the (slice) slider bar to a location corresponding to the given slice value (within the
     * set) allowing notification of the listeners.
     *
     * @param  value  The slice in the frame that is displayed
     *
     * @see    JSlider
     * @see    ViewControlsImage#setZSlider(int)
     */
    public void updateZImageSlider(int value) {
        int newValue;

        newValue = Math.round((float) value / (frame.getImageA().getExtents()[2] - 1) * sliderResolutionInt);

        if (zImageSlider != null) {
            zImageSlider.setValue(newValue);
        }
    }

    /**
     * Builds the slider used to control the alpha blending.
     */
    protected void buildAlphaSlider() {

        Font font12 = MipavUtil.font12;

        // Make labels to be used in display in the alpha blending slider
        Hashtable dictionary = new Hashtable();
        JLabel label1 = new JLabel("Image A");

        label1.setForeground(Color.black);
        label1.setFont(font12);
        dictionary.put(new Integer(0), label1);

        JLabel label2 = new JLabel("0.75A");

        label2.setForeground(Color.black);
        label2.setFont(font12);
        dictionary.put(new Integer(25), label2);

        JLabel label3 = new JLabel("0.5A/B");

        label3.setForeground(Color.black);
        label3.setFont(font12);
        dictionary.put(new Integer(50), label3);

        JLabel label4 = new JLabel("0.75B");

        label4.setForeground(Color.black);
        label4.setFont(font12);
        dictionary.put(new Integer(75), label4);

        JLabel label5 = new JLabel("Image B  ");

        label5.setForeground(Color.black);
        label5.setFont(font12);
        dictionary.put(new Integer(100), label5);

        alphaSlider = new JSlider(JSlider.HORIZONTAL, 0, 100, 50);

        imageSize = frame.getImageA().getSliceSize();
        alphaSlider.setMajorTickSpacing(25);
        alphaSlider.setPaintTicks(true);
        alphaSlider.setPaintLabels(true);
        alphaSlider.setLabelTable(dictionary); // loads the labels made above
        alphaSlider.setValue(50);

        panelActiveImage.add(alphaSlider, BorderLayout.SOUTH);
        alphaSlider.addChangeListener(this);
    }

    /**
     * Places JLabels under the (time) slider bar indicating the minimum, the maximum and the midpoint of the set.
     *
     * @param  min  The minimum for the slider.
     * @param  max  The maximum for the slider.
     *
     * @see    JLabel
     */
    protected void buildTImageSliderLabels(int min, int max) {

        Font font12 = MipavUtil.font12;
        float rangeF = (max) / 4.0f;

        JLabel label1 = new JLabel("0");

        label1.setForeground(Color.black);
        label1.setFont(font12);
        tImageSliderDictionary.put(new Integer(0), label1);

        if ((max - min) > 3) {
            JLabel label2 = new JLabel(Integer.toString(Math.round(rangeF * 2)-1));

            label2.setForeground(Color.black);
            label2.setFont(font12);
            tImageSliderDictionary.put(new Integer(sliderResolutionInt / 2), label2);
        }

        JLabel label5 = new JLabel(Integer.toString(max-1));

        label5.setForeground(Color.black);
        label5.setFont(font12);
        tImageSliderDictionary.put(new Integer(sliderResolutionInt), label5);
    }

    /**
     * Places JLabels under the (slice) slider bar indicating the minimum, the maximum and the midpoint of the set.
     *
     * @param  min  Minimum value for slider.
     * @param  max  Maximum value for slider.
     *
     * @see    JLabel
     */
    protected void buildZImageSliderLabels(int min, int max) {

        Font font12 = MipavUtil.font12;
        float rangeF = (max) / 4.0f;

        JLabel label1 = new JLabel("0");

        label1.setForeground(Color.black);
        label1.setFont(font12);
        zImageSliderDictionary.put(new Integer(0), label1);

        if ((max - min) > 3) {
            JLabel label2 = new JLabel(Integer.toString(Math.round(rangeF * 2)-1));

            label2.setForeground(Color.black);
            label2.setFont(font12);
            zImageSliderDictionary.put(new Integer(sliderResolutionInt / 2), label2);
        }

        JLabel label5 = new JLabel(Integer.toString(max-1));

        label5.setForeground(Color.black);
        label5.setFont(font12);
        zImageSliderDictionary.put(new Integer(sliderResolutionInt), label5);
    }
    
    
    
    /**
     * This method toggles between the intensities
     * between Image A and Image B
     * 
     * 
     */
    public void toggleSlider() {
    	if(alphaSlider != null) {
    		int value = alphaSlider.getValue();
    		if(value != 100) {
    			alphaSlider.setValue(100);
    		}
    		else {
    			alphaSlider.setValue(0);
    		}
    	}
    }
}
