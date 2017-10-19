package gov.nih.mipav.view.components;


import gov.nih.mipav.model.structures.*;

import javax.swing.*;


/**
 * This panel encapsulates radio buttons which allow the user to indicate whether a new image should be generated by the
 * algorithm dialog parent of the panel and whether the whole image or a VOI region should be processed.
 *
 * @author  mccreedy
 */
public class JPanelAlgorithmOutputOptions extends JPanel {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5964491506018618684L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JRadioButton newImageRadio;

    /** DOCUMENT ME! */
    private JRadioButton replaceImageRadio;

    /** DOCUMENT ME! */
    private ModelImage srcImage;

    /** DOCUMENT ME! */
    private JRadioButton voiRegionsRadio;

    /** DOCUMENT ME! */
    private JRadioButton wholeImageRadio;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create the algorithm output options panel.
     *
     * @param  img  the input image which will be processed by the algorithm
     */
    public JPanelAlgorithmOutputOptions(ModelImage img) {
        srcImage = img;
        initGUI();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns whether a new image should be produced by the dialog this panel is a part of.
     *
     * @return  true if a new image should be created, false if the input image should be replaced
     */
    public boolean isOutputNewImageSet() {
        return newImageRadio.isSelected();
    }

    /**
     * Returns whether the whole image should be processed (as opposed to just VOI regions).
     *
     * @return  true if the whole image should be processed, false if regions inside VOIs should be processed
     */
    public boolean isProcessWholeImageSet() {
        return wholeImageRadio.isSelected();
    }

    /**
     * Changes whether a new image should be generated by the algorithm.
     *
     * @param  flag  true if a new image should be made, false otherwise
     */
    public void setOutputNewImage(boolean flag) {
        newImageRadio.setSelected(flag);
        replaceImageRadio.setSelected(!flag);
    }

    /**
     * Changes whether a whole image should be processed (as opposed to just VOI regions).
     *
     * @param  flag  true if the whole image should be processed, false if just VOI regions
     */
    public void setProcessWholeImage(boolean flag) {
        wholeImageRadio.setSelected(flag);
        voiRegionsRadio.setSelected(!flag);
    }

    /**
     * Construct the panel's GUI.
     */
    private void initGUI() {
        PanelManager mainPanelManager = new PanelManager(this);
        PanelManager destPanelManager = new PanelManager("Destination");
        PanelManager regionPanelManager = new PanelManager("Process");

        ButtonGroup destinationGroup = new ButtonGroup();
        ButtonGroup imageVOIGroup = new ButtonGroup();

        newImageRadio = WidgetFactory.buildRadioButton("New image", true, destinationGroup);
        destPanelManager.add(newImageRadio);
        replaceImageRadio = WidgetFactory.buildRadioButton("Replace image", false, destinationGroup);
        destPanelManager.addOnNextLine(replaceImageRadio);

        // Only if the image is unlocked can it be replaced.
        if (srcImage.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImageRadio.setEnabled(true);
        } else {
            replaceImageRadio.setEnabled(false);
        }

        wholeImageRadio = WidgetFactory.buildRadioButton("Whole image", true, imageVOIGroup);
        regionPanelManager.add(wholeImageRadio);
        voiRegionsRadio = WidgetFactory.buildRadioButton("VOI region(s)", false, imageVOIGroup);
        regionPanelManager.addOnNextLine(voiRegionsRadio);

        mainPanelManager.add(destPanelManager.getPanel());
        mainPanelManager.add(regionPanelManager.getPanel());
    }
}