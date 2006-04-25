package gov.nih.mipav.view.components;


import gov.nih.mipav.model.structures.*;

import javax.swing.*;


/**
 * A panel containing checkboxes which allow the user to indicate which channels of a color image should be processed.
 *
 * @author  mccreedy
 */
public class JPanelColorChannels extends JPanel {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3679634471868989877L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JCheckBox blueCheckbox;

    /** DOCUMENT ME! */
    private JCheckBox greenCheckbox;

    /** DOCUMENT ME! */
    private JCheckBox redCheckbox;

    /** DOCUMENT ME! */
    private ModelImage srcImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Construct the panel, with all of the channels marked to be processed by default.
     *
     * @param  img  the algorithm's input image
     */
    public JPanelColorChannels(ModelImage img) {
        this(img, true, true, true);
    }

    /**
     * Construct the panel.
     *
     * @param  img           the algorithm's input image
     * @param  processRed    whether to enable processing of the red channel by default
     * @param  processGreen  whether to enable processing of the green channel by default
     * @param  processBlue   whether to enable processing of the blue channel by default
     */
    public JPanelColorChannels(ModelImage img, boolean processRed, boolean processGreen, boolean processBlue) {
        srcImage = img;

        initGUI(processRed, processGreen, processBlue);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Enables or disables the color channel check boxes.
     *
     * @param  enableCheckboxes  if true, enables the check boxes; disables them if false
     */
    public void enableChannelSelection(boolean enableCheckboxes) {
        redCheckbox.setEnabled(enableCheckboxes);
        greenCheckbox.setEnabled(enableCheckboxes);
        blueCheckbox.setEnabled(enableCheckboxes);
    }

    /**
     * Returns whether the algorithm should process the input image's blue channel.
     *
     * @return  true if the blue channel should be processed (and the image is color)
     */
    public boolean isBlueProcessingRequested() {
        return blueCheckbox.isSelected() && blueCheckbox.isEnabled();
    }

    /**
     * Returns whether the algorithm should process the input image's green channel.
     *
     * @return  true if the green channel should be processed (and the image is color)
     */
    public boolean isGreenProcessingRequested() {
        return greenCheckbox.isSelected() && greenCheckbox.isEnabled();
    }

    /**
     * Returns whether the algorithm should process the input image's red channel.
     *
     * @return  true if the red channel should be processed (and the image is color)
     */
    public boolean isRedProcessingRequested() {
        return redCheckbox.isSelected() && redCheckbox.isEnabled();
    }

    /**
     * Changes whether the blue channel of the input image should be processed.
     *
     * @param  processChannel  whether to process the blue channel
     */
    public void setBlueProcessingRequested(boolean processChannel) {
        blueCheckbox.setSelected(processChannel);
    }

    /**
     * Changes whether the green channel of the input image should be processed.
     *
     * @param  processChannel  whether to process the green channel
     */
    public void setGreenProcessingRequested(boolean processChannel) {
        greenCheckbox.setSelected(processChannel);
    }

    /**
     * Changes whether the red channel of the input image should be processed.
     *
     * @param  processChannel  whether to process the red channel
     */
    public void setRedProcessingRequested(boolean processChannel) {
        redCheckbox.setSelected(processChannel);
    }

    /**
     * Initializes the panel's GUI.
     *
     * @param  processRed    whether to enable processing of the red channel by default
     * @param  processGreen  whether to enable processing of the green channel by default
     * @param  processBlue   whether to enable processing of the blue channel by default
     */
    private void initGUI(boolean processRed, boolean processGreen, boolean processBlue) {
        PanelManager mainPanelManager = new PanelManager(this);
        this.setBorder(WidgetFactory.buildTitledBorder("Color channel selection"));

        redCheckbox = WidgetFactory.buildCheckBox("Process red channel", processRed);
        greenCheckbox = WidgetFactory.buildCheckBox("Process green channel", processGreen);
        blueCheckbox = WidgetFactory.buildCheckBox("Process blue channel", processBlue);

        enableChannelSelection(srcImage.isColorImage());

        mainPanelManager.add(redCheckbox);
        mainPanelManager.addOnNextLine(greenCheckbox);
        mainPanelManager.addOnNextLine(blueCheckbox);
    }
}
