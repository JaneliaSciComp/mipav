package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input two MRI image SNR Algorithms are executed in their own thread.
 */
public class JDialogTwoMRIImagesSNR extends JDialogBase implements AlgorithmInterface, ItemListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2992719068907961685L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    // private static final long serialVersionUID;

    /** DOCUMENT ME! */
    private JComboBox comboBoxCostFunct;

    /** DOCUMENT ME! */
    private ViewJComponentEditImage componentImage;

    /** DOCUMENT ME! */
    private ViewJComponentEditImage componentImage2;

    /** DOCUMENT ME! */
    private int cost;

    /** DOCUMENT ME! */
    private JCheckBox createRegCheckBox;

    /** DOCUMENT ME! */
    private boolean createRegImage = false;

    /** DOCUMENT ME! */
    private ModelImage image; // image1

    /** DOCUMENT ME! */
    private ModelImage image2;

    /** DOCUMENT ME! */
    private JComboBox imageComboBox;

    /** DOCUMENT ME! */
    private JLabel labelCost;

    /** DOCUMENT ME! */
    private int nBoundingVOIs;

    /** DOCUMENT ME! */
    private JCheckBox regCheckBox;

    /** DOCUMENT ME! */
    private boolean register;

    /** DOCUMENT ME! */
    private JRadioButton signal2Button;

    /** DOCUMENT ME! */
    private int signal2Image = 1;

    /** DOCUMENT ME! */
    private int signal2Index = -1;

    /** DOCUMENT ME! */
    private JRadioButton signalButton;

    /** DOCUMENT ME! */
    private int signalImage = 1;

    /** DOCUMENT ME! */
    private int signalIndex = -1;


    /** DOCUMENT ME! */
    private AlgorithmTwoMRIImagesSNR snrAlgo = null;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private ButtonGroup VOIGroup;

    /** DOCUMENT ME! */
    private ViewVOIVector VOIs; // from image

    /** DOCUMENT ME! */
    private ViewVOIVector VOIs2; // from image 2

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogFRET object.
     *
     * @param  image  DOCUMENT ME!
     */
    public JDialogTwoMRIImagesSNR(ModelImage image) {
        super();
        this.UI = ViewUserInterface.getReference();
        this.image = image;
        parentFrame = image.getParentFrame();
        componentImage = ((ViewJFrameImage) parentFrame).getComponentImage();
    }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogTwoMRIImagesSNR(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        componentImage = ((ViewJFrameImage) theParentFrame).getComponentImage();
        UI = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Help")) {
            // MipavUtil.showHelp("");
        } else if (command.equals("Cancel")) {
            componentImage2.getVOIHandler().setPresetHue(-1.0f);
            componentImage.getVOIHandler().setPresetHue(-1.0f);
            dispose();
        } else if ((source == signalButton) || (source == signal2Button)) {

            if (signalButton.isSelected()) {
                componentImage2.getVOIHandler().newVOI(0.0f); // red
                //componentImage2.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                //componentImage2.getVOIHandler().setPresetHue(0.0f); // red
                componentImage.getVOIHandler().newVOI(0.0f); // red
                //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                //componentImage.getVOIHandler().setPresetHue(0.0f); // red
            } else if (signal2Button.isSelected()) {
                componentImage2.getVOIHandler().newVOI(1.0f / 3.0f); // green
                //componentImage2.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                //componentImage2.getVOIHandler().setPresetHue(1.0f / 3.0f); // green
                componentImage.getVOIHandler().newVOI(1.0f / 3.0f); // green
                //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                //componentImage.getVOIHandler().setPresetHue(1.0f / 3.0f); // green
            }
        } else {
            super.actionPerformed(event);
        }
    }


    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {


        if (snrAlgo.isCompleted() == true) { }

        dispose();
    }

    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * itemStateChanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (source == regCheckBox) {

            if (regCheckBox.isSelected()) {
                labelCost.setEnabled(true);
                comboBoxCostFunct.setEnabled(true);
                createRegCheckBox.setEnabled(true);
                createRegCheckBox.setSelected(true);
            } else {
                labelCost.setEnabled(false);
                comboBoxCostFunct.setEnabled(false);
                createRegCheckBox.setEnabled(false);
                createRegCheckBox.setSelected(false);
            }
        } else if (source == imageComboBox) {
            String secondName = (String) imageComboBox.getSelectedItem();

            if (secondName == null) {
                MipavUtil.displayError("No second image available");

                return;
            }

            image2 = UI.getRegisteredImageByName(secondName);
            componentImage2 = image2.getParentFrame().getComponentImage();
        }


    }

    /**
     * DOCUMENT ME!
     *
     * @param  cost  DOCUMENT ME!
     */
    public void setCost(int cost) {
        this.cost = cost;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  image2  DOCUMENT ME!
     */
    public void setImage2(ModelImage image2) {
        this.image2 = image2;
    }


    /**
     * DOCUMENT ME!
     *
     * @param  register  DOCUMENT ME!
     */
    public void setRegister(boolean register) {
        this.register = register;
    }


    /**
     * Disposes of error dialog, then frame. Sets cancelled to <code>true</code>.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowClosing(WindowEvent event) {

        componentImage2.getVOIHandler().setPresetHue(-1.0f);
        componentImage.getVOIHandler().setPresetHue(-1.0f);

        cancelFlag = true;
        dispose();
    }

    /**
     * Builds a list of images. Returns combobox. List must be all color or all black and white.
     *
     * @param   image  DOCUMENT ME!
     *
     * @return  Newly created combo box.
     */
    private JComboBox buildComboBox(ModelImage image) {
        ViewUserInterface UI;
        ModelImage nextImage;
        boolean doAdd;
        int i;

        JComboBox comboBox = new JComboBox();
        comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);

        UI = ViewUserInterface.getReference();

        Enumeration<String> names = UI.getRegisteredImageNames();

        while (names.hasMoreElements()) {
            String name = names.nextElement();

            if (!name.equals(image.getImageName())) {
                nextImage = UI.getRegisteredImageByName(name);

                if (UI.getFrameContainingImage(nextImage) != null) {

                    if ((!nextImage.isColorImage()) && (image.getNDims() == nextImage.getNDims())) {
                        doAdd = true;

                        for (i = 0; i < image.getNDims(); i++) {

                            if (image.getExtents()[i] != nextImage.getExtents()[i]) {
                                doAdd = false;
                            }
                        }

                        if (doAdd) {
                            comboBox.addItem(name);
                        }
                    }
                }
            }
        }

        return comboBox;
    }

    /**
     * DOCUMENT ME!
     */
    private void callAlgorithm() {

        try {

            componentImage2.getVOIHandler().setPresetHue(-1.0f);
            componentImage.getVOIHandler().setPresetHue(-1.0f);
            // Make algorithm

            snrAlgo = new AlgorithmTwoMRIImagesSNR(image, image2, signalIndex, signalImage, signal2Index, register,
                                                   cost, createRegImage);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            snrAlgo.addListener(this);

            createProgressBar(image.getImageName(), snrAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (snrAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                snrAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog Two MRI Images SNR: unable to allocate enough memory");

            return;
        }
    }


    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        JPanel imagePanel;
        JPanel imageSelectionPanel;
        JPanel VOIPanel;
        JLabel labelImage;
        JLabel labelImage2;
        String secondName;

        setForeground(Color.black);
        setTitle("Two MRI images SNR");

        imageSelectionPanel = new JPanel(new GridBagLayout());

        imageSelectionPanel.setBorder(buildTitledBorder("Select second image"));

        GridBagConstraints gbc5 = new GridBagConstraints();
        gbc5.gridwidth = 1;
        gbc5.gridheight = 1;
        gbc5.anchor = GridBagConstraints.WEST;
        gbc5.weightx = 1;
        gbc5.insets = new Insets(3, 3, 3, 3);
        gbc5.fill = GridBagConstraints.HORIZONTAL;
        gbc5.gridx = 0;
        gbc5.gridy = 0;

        labelImage = new JLabel("image1: " + image.getImageName());
        labelImage.setForeground(Color.black);
        labelImage.setFont(serif12);
        imageSelectionPanel.add(labelImage, gbc5);

        labelImage2 = new JLabel("image2: ");
        labelImage2.setForeground(Color.black);
        labelImage2.setFont(serif12);
        gbc5.gridy = 1;
        imageSelectionPanel.add(labelImage2, gbc5);
        imageComboBox = buildComboBox(image);
        imageComboBox.addItemListener(this);
        gbc5.gridx = 1;
        imageSelectionPanel.add(imageComboBox, gbc5);

        secondName = (String) imageComboBox.getSelectedItem();

        if (secondName == null) {
            MipavUtil.displayError("No second image available");

            return;
        }

        image2 = UI.getRegisteredImageByName(secondName);
        componentImage2 = image2.getParentFrame().getComponentImage();


        VOIPanel = new JPanel(new GridBagLayout());
        VOIPanel.setBorder(buildTitledBorder("Select signal VOIs"));

        GridBagConstraints gbc4 = new GridBagConstraints();
        gbc4.gridwidth = 1;
        gbc4.gridheight = 1;
        gbc4.anchor = GridBagConstraints.WEST;
        gbc4.weightx = 1;
        gbc4.insets = new Insets(3, 3, 3, 3);
        gbc4.fill = GridBagConstraints.HORIZONTAL;
        gbc4.gridx = 0;
        gbc4.gridy = 0;

        VOIGroup = new ButtonGroup();

        signalButton = new JRadioButton("Add required signal VOI", true);
        signalButton.setForeground(Color.red);
        signalButton.setFont(serif12);
        signalButton.addActionListener(this);
        VOIGroup.add(signalButton);
        VOIPanel.add(signalButton, gbc4);

        componentImage2.getVOIHandler().newVOI(0.0f); // red
        //componentImage2.setCursorMode(ViewJComponentEditImage.NEW_VOI);
        //componentImage2.getVOIHandler().setPresetHue(0.0f); // red
        componentImage.getVOIHandler().newVOI(0.0f); // red
        //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
        //componentImage.getVOIHandler().setPresetHue(0.0f); // red


        signal2Button = new JRadioButton("Add an optional second signal VOI", false);
        signal2Button.setForeground(Color.green.darker());
        signal2Button.setFont(serif12);
        signal2Button.addActionListener(this);
        VOIGroup.add(signal2Button);
        gbc4.gridy = 2;
        VOIPanel.add(signal2Button, gbc4);


        imagePanel = new JPanel(new GridBagLayout());

        GridBagConstraints gbc3 = new GridBagConstraints();
        gbc3.gridwidth = 1;
        gbc3.gridheight = 1;
        gbc3.anchor = GridBagConstraints.WEST;
        gbc3.weightx = 1;
        gbc3.insets = new Insets(3, 3, 3, 3);
        gbc3.fill = GridBagConstraints.HORIZONTAL;
        gbc3.gridx = 0;
        gbc3.gridy = 0;
        imagePanel.add(imageSelectionPanel, gbc3);
        gbc3.gridy = 1;
        imagePanel.add(VOIPanel, gbc3);


        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        JPanel registrationPanel = new JPanel(new GridBagLayout());
        registrationPanel.setForeground(Color.black);
        registrationPanel.setBorder(buildTitledBorder("Registration"));

        regCheckBox = new JCheckBox("Registration before SNR");
        regCheckBox.setFont(serif12);
        regCheckBox.setForeground(Color.black);
        regCheckBox.setSelected(false);
        regCheckBox.addItemListener(this);
        registrationPanel.add(regCheckBox, gbc);

        labelCost = new JLabel("Cost function:");
        labelCost.setForeground(Color.black);
        labelCost.setFont(serif12);
        labelCost.setAlignmentX(Component.LEFT_ALIGNMENT);
        labelCost.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy = 1;
        registrationPanel.add(labelCost, gbc);

        comboBoxCostFunct = new JComboBox();
        comboBoxCostFunct.setFont(MipavUtil.font12);
        comboBoxCostFunct.setBackground(Color.white);
        comboBoxCostFunct.setToolTipText("Cost function");
        comboBoxCostFunct.addItem("Correlation ratio");
        comboBoxCostFunct.addItem("Least squares");
        comboBoxCostFunct.addItem("Normalized cross correlation");
        comboBoxCostFunct.addItem("Normalized mutual information");
        comboBoxCostFunct.setSelectedIndex(0);
        comboBoxCostFunct.setEnabled(false);
        gbc.gridx = 1;
        gbc.gridy = 1;
        registrationPanel.add(comboBoxCostFunct, gbc);

        createRegCheckBox = new JCheckBox("Create registration image");
        createRegCheckBox.setFont(serif12);
        createRegCheckBox.setForeground(Color.black);
        createRegCheckBox.setSelected(false);
        createRegCheckBox.setEnabled(false);
        createRegCheckBox.addItemListener(this);
        gbc.gridx = 0;
        gbc.gridy = 2;
        registrationPanel.add(createRegCheckBox, gbc);

        getContentPane().add(imagePanel, BorderLayout.NORTH);
        getContentPane().add(registrationPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        int i;
        int nVOIs;
        int nVOIs2;
        float[] hsb;
        float hue;
        nBoundingVOIs = 0;

        boolean signalCurveFound;

        String selectedName = (String) imageComboBox.getSelectedItem();
        image2 = UI.getRegisteredImageByName(selectedName);

        if (image2 == null) {
            return false;
        }

        VOIs = image.getVOIs();
        VOIs2 = image2.getVOIs();


        nVOIs = VOIs.size();
        nVOIs2 = VOIs2.size();

        for (i = 0; i < nVOIs; i++) {

            if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                nBoundingVOIs++;
                hsb = Color.RGBtoHSB(VOIs.VOIAt(i).getColor().getRed(), VOIs.VOIAt(i).getColor().getGreen(),
                                     VOIs.VOIAt(i).getColor().getBlue(), null);
                hue = hsb[0];

                if ((Math.abs(hue - 0.0f)) < 0.0001f) {

                    if (signalIndex == -1) {
                        signalIndex = i;
                        signalImage = 1;
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 mandatory signal VOI");

                        return false;
                    }
                } else if ((Math.abs(hue - (1.0f / 3.0f))) < 0.0001f) {

                    if (signal2Index == -1) {
                        signal2Index = i;
                        signal2Image = 1;
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 second signal VOI");

                        return false;
                    }
                } else {
                    MipavUtil.displayError("Contour VOI with illegal hue = " + hue + " is present");

                    return false;
                }
            }
        } // for (i = 0; i < nVOIs; i++)

        for (i = 0; i < nVOIs2; i++) {

            if ((VOIs2.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs2.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                nBoundingVOIs++;
                hsb = Color.RGBtoHSB(VOIs2.VOIAt(i).getColor().getRed(), VOIs2.VOIAt(i).getColor().getGreen(),
                                     VOIs2.VOIAt(i).getColor().getBlue(), null);
                hue = hsb[0];

                if ((Math.abs(hue - 0.0f)) < 0.0001f) {

                    if (signalIndex == -1) {
                        signalIndex = i;
                        signalImage = 2;
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 mandatory signal VOI");

                        return false;
                    }
                } else if ((Math.abs(hue - (1.0f / 3.0f))) < 0.0001f) {

                    if (signal2Index == -1) {
                        signal2Index = i;
                        signal2Image = 2;
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 second signal VOI");

                        return false;
                    }
                } else {
                    MipavUtil.displayError("Contour VOI with illegal hue = " + hue + " is present");

                    return false;
                }
            }
        } // for (i = 0; i < nVOIs2; i++)

        if (signalIndex == -1) {
            MipavUtil.displayError("Must specify a signal VOI");

            return false;
        }

        if (signalImage != signal2Image) {
            MipavUtil.displayError("Must take both VOIs from the same image");

            return false;
        }

        signalCurveFound = false;

        if (signalImage == 1) {

            if (VOIs.VOIAt(signalIndex).getCurves().size() > 0) {
                signalCurveFound = true;
            }
        } else {

            if (VOIs2.VOIAt(signalIndex).getCurves().size() > 0) {
                signalCurveFound = true;
            }
        }

        if (!signalCurveFound) {
            MipavUtil.displayError("Signal VOI has no curves");

            return false;
        } // if (!signalCurveFound)

        register = regCheckBox.isSelected();

        switch (comboBoxCostFunct.getSelectedIndex()) {

            case 0:
                cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
                break;

            case 1:
                cost = AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED;
                break;
                // case 2:  cost = AlgorithmCostFunctions.MUTUAL_INFORMATION_SMOOTHED;             break;

            case 2:
                cost = AlgorithmCostFunctions.NORMALIZED_XCORRELATION_SMOOTHED;
                break;

            case 3:
                cost = AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED;
                break;

            default:
                cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
                break;
        }

        createRegImage = createRegCheckBox.isSelected();


        return true;
    }
}
