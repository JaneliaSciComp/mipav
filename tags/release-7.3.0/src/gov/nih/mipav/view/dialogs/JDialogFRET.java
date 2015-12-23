package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input Fluorescence resonance energy transfer Algorithms are executed in their own thread.
 */
public class JDialogFRET extends JDialogBase implements AlgorithmInterface, ItemListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1026032715678640392L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JRadioButton backgroundButton;

    /** DOCUMENT ME! */
    private int backgroundIndex = -1;

    /** DOCUMENT ME! */
    private JRadioButton blueButton;

    /** DOCUMENT ME! */
    private ButtonGroup colorGroup;

    /** DOCUMENT ME! */
    private int colorsPresent = 0;

    /** DOCUMENT ME! */
    private JComboBox comboBoxCostFunct;

    /** DOCUMENT ME! */
    private ViewJComponentEditImage componentImage;

    /** DOCUMENT ME! */
    private ViewJComponentEditImage componentPostImage;

    /** DOCUMENT ME! */
    private int cost;

    /** DOCUMENT ME! */
    private JCheckBox createRegCheckBox;

    /** DOCUMENT ME! */
    private boolean createRegImage = false;

    /** DOCUMENT ME! */
    private JRadioButton donorButton;

    /** DOCUMENT ME! */
    private int donorIndex = -1;


    /** DOCUMENT ME! */
    private AlgorithmFRETAcceptorPhotobleach fretAlgo = null;

    /** DOCUMENT ME! */
    private JRadioButton greenButton;

    /** DOCUMENT ME! */
    private ModelImage image; // prebleached image

    /** DOCUMENT ME! */
    private JComboBox imageComboBox;

    /** DOCUMENT ME! */
    private JLabel labelCost;

    /** DOCUMENT ME! */
    private double maxR, maxG, maxB;

    /** DOCUMENT ME! */
    private double minR, minG, minB;

    /** DOCUMENT ME! */
    private int nBoundingVOIs;

    /** DOCUMENT ME! */
    private ModelImage postImage; // postbleached image

    /** DOCUMENT ME! */
    private JRadioButton redButton;

    /** DOCUMENT ME! */
    private JCheckBox regCheckBox;

    /** DOCUMENT ME! */
    private boolean register;

    /** DOCUMENT ME! */
    private JRadioButton signalButton;

    /** DOCUMENT ME! */
    private int signalIndex = -1;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private boolean useBlue = false;

    /** DOCUMENT ME! */
    private boolean useGreen = false;

    /** DOCUMENT ME! */
    private boolean useRed = false;

    /** DOCUMENT ME! */
    private ButtonGroup VOIGroup;

    /** DOCUMENT ME! */
    private ViewVOIVector VOIs;

    /** DOCUMENT ME! */
    private int yPos;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogFRET object.
     *
     * @param  image  DOCUMENT ME!
     */
    public JDialogFRET(ModelImage image) {
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
    public JDialogFRET(Frame theParentFrame, ModelImage im) {
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
            //MipavUtil.showHelp("10095");
            MipavUtil.showWebHelp("Microscopy:_Fluorescent_Resonance_Energy_Transfer_(FRET)_Bleed_Through_and_Efficiency#FRET_Bleed_Through_dialog_box");
        } else if (command.equals("Cancel")) {

            if (image.getNDims() == 2) {
                componentPostImage.getVOIHandler().setPresetHue(-1.0f);
            } else {
                componentImage.getVOIHandler().setPresetHue(-1.0f);
            }

            dispose();
        } else if ((source == donorButton) || (source == backgroundButton) || (source == signalButton)) {

            if (donorButton.isSelected()) {

                if (image.getNDims() == 2) {
                    componentPostImage.getVOIHandler().newVOI(0.0f); // red
                    //componentPostImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                    //componentPostImage.getVOIHandler().setPresetHue(0.0f); // red
                } else {
                    componentImage.getVOIHandler().newVOI(0.0f); // red
                    //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                    //componentImage.getVOIHandler().setPresetHue(0.0f); // red
                }
            } else if (backgroundButton.isSelected()) {

                if (image.getNDims() == 2) {
                    componentPostImage.getVOIHandler().newVOI(2.0f / 3.0f); // blue
                    //componentPostImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                    //componentPostImage.getVOIHandler().setPresetHue(2.0f / 3.0f); // blue
                } else {
                    componentImage.getVOIHandler().newVOI(2.0f / 3.0f); // blue
                    //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                    //componentImage.getVOIHandler().setPresetHue(2.0f / 3.0f); // blue
                }
            } else if (signalButton.isSelected()) {

                if (image.getNDims() == 2) {
                    componentPostImage.getVOIHandler().newVOI(1.0f / 3.0f); // green
                    //componentPostImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                    //componentPostImage.getVOIHandler().setPresetHue(1.0f / 3.0f); // green
                } else {
                    componentImage.getVOIHandler().newVOI(1.0f / 3.0f); // green
                    //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                    //componentImage.getVOIHandler().setPresetHue(1.0f / 3.0f); // green
                }

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


        if (fretAlgo.isCompleted() == true) { }

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
                MipavUtil.displayError("No postbleached image available");

                return;
            }

            postImage = UI.getRegisteredImageByName(secondName);
            componentPostImage = postImage.getParentFrame().getComponentImage();
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
     * @param  postImage  DOCUMENT ME!
     */
    public void setPostImage(ModelImage postImage) {
        this.postImage = postImage;
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
     * DOCUMENT ME!
     *
     * @param  useBlue  DOCUMENT ME!
     */
    public void setUseBlue(boolean useBlue) {
        this.useBlue = useBlue;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  useGreen  DOCUMENT ME!
     */
    public void setUseGreen(boolean useGreen) {
        this.useGreen = useGreen;
    }


    /**
     * DOCUMENT ME!
     *
     * @param  useRed  DOCUMENT ME!
     */
    public void setUseRed(boolean useRed) {
        this.useRed = useRed;
    }

    /**
     * Disposes of error dialog, then frame. Sets cancelled to <code>true</code>.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowClosing(WindowEvent event) {

        if (image.getNDims() == 2) {
            componentPostImage.getVOIHandler().setPresetHue(-1.0f);
        } else {
            componentImage.getVOIHandler().setPresetHue(-1.0f);
        }

        cancelFlag = true;
        dispose();
    }

    /**
     * DOCUMENT ME!
     */
    protected void callAlgorithm() {

        try {

            if (image.getNDims() == 2) {
                componentPostImage.getVOIHandler().setPresetHue(-1.0f);
            } else {
                componentImage.getVOIHandler().setPresetHue(-1.0f);
            }
            // Make algorithm

            fretAlgo = new AlgorithmFRETAcceptorPhotobleach(image, postImage, useRed, useGreen, useBlue, donorIndex,
                                                            backgroundIndex, signalIndex, register, cost,
                                                            createRegImage);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            fretAlgo.addListener(this);

            createProgressBar(image.getImageName(), fretAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (fretAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                fretAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog FRET: unable to allocate enough memory");

            return;
        }
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

                    if ((image.isColorImage() == nextImage.isColorImage()) && (nextImage.getNDims() == 2)) {
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
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        boolean haveRed = false;
        boolean haveGreen = false;
        boolean haveBlue = false;
        int yButton = 1;
        boolean buttonSet = true;
        JPanel imagePanel;
        JPanel imageSelectionPanel;
        JPanel VOIPanel;
        JPanel colorPanel;
        JLabel labelImage;
        JLabel labelImage2;
        String secondName;

        setForeground(Color.black);
        setTitle("Fluorescence Resonance Energy Transfer");

        imageSelectionPanel = new JPanel(new GridBagLayout());

        if (image.getNDims() == 2) {
            imageSelectionPanel.setBorder(buildTitledBorder("Select postbleached image"));

            GridBagConstraints gbc5 = new GridBagConstraints();
            gbc5.gridwidth = 1;
            gbc5.gridheight = 1;
            gbc5.anchor = GridBagConstraints.WEST;
            gbc5.weightx = 1;
            gbc5.insets = new Insets(3, 3, 3, 3);
            gbc5.fill = GridBagConstraints.HORIZONTAL;
            gbc5.gridx = 0;
            gbc5.gridy = 0;

            labelImage = new JLabel("Prebleached image: " + image.getImageName());
            labelImage.setForeground(Color.black);
            labelImage.setFont(serif12);
            imageSelectionPanel.add(labelImage, gbc5);

            labelImage2 = new JLabel("Postbleached image: ");
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
                MipavUtil.displayError("No postbleached image available");

                return;
            }

            postImage = UI.getRegisteredImageByName(secondName);
            componentPostImage = postImage.getParentFrame().getComponentImage();
        } // if (image.getNDims() == 2)
        else { // 3D image
            imageSelectionPanel.setBorder(buildTitledBorder("Image with pre and post bleaching"));

            GridBagConstraints gbc5 = new GridBagConstraints();
            gbc5.gridwidth = 1;
            gbc5.gridheight = 1;
            gbc5.anchor = GridBagConstraints.WEST;
            gbc5.weightx = 1;
            gbc5.insets = new Insets(3, 3, 3, 3);
            gbc5.fill = GridBagConstraints.HORIZONTAL;
            gbc5.gridx = 0;
            gbc5.gridy = 0;

            labelImage = new JLabel("Image: " + image.getImageName());
            labelImage.setForeground(Color.black);
            labelImage.setFont(serif12);
            imageSelectionPanel.add(labelImage, gbc5);
        } // else 3D image

        VOIPanel = new JPanel(new GridBagLayout());
        VOIPanel.setBorder(buildTitledBorder("Select postbleach VOIs"));

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

        donorButton = new JRadioButton("Add required donor fluorescence VOI", true);
        donorButton.setForeground(Color.red);
        donorButton.setFont(serif12);
        donorButton.addActionListener(this);
        VOIGroup.add(donorButton);
        VOIPanel.add(donorButton, gbc4);

        if (image.getNDims() == 2) {
            componentPostImage.getVOIHandler().newVOI(0.0f); // red
            //componentPostImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
            //componentPostImage.getVOIHandler().setPresetHue(0.0f); // red
        } // if (image.getNDims() == 2)
        else {
            componentImage.getVOIHandler().newVOI(0.0f); // red
            //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
            //componentImage.getVOIHandler().setPresetHue(0.0f); // red
        }

        backgroundButton = new JRadioButton("Add optional background VOI", false);
        backgroundButton.setForeground(Color.blue);
        backgroundButton.setFont(serif12);
        backgroundButton.addActionListener(this);
        VOIGroup.add(backgroundButton);
        gbc4.gridy = 1;
        VOIPanel.add(backgroundButton, gbc4);

        signalButton = new JRadioButton("Add optional signal normalization VOI", false);
        signalButton.setForeground(Color.green.darker());
        signalButton.setFont(serif12);
        signalButton.addActionListener(this);
        VOIGroup.add(signalButton);
        gbc4.gridy = 2;
        VOIPanel.add(signalButton, gbc4);

        colorPanel = new JPanel(new GridBagLayout());

        if (image.isColorImage()) {
            minR = image.getMinR();
            maxR = image.getMaxR();

            if (minR != maxR) {
                haveRed = true;
            }

            minG = image.getMinG();
            maxG = image.getMaxG();

            if (minG != maxG) {
                haveGreen = true;
            }

            minB = image.getMinB();
            maxB = image.getMaxB();

            if (minB != maxB) {
                haveBlue = true;
            }

            colorsPresent = 0;

            if (haveRed) {
                colorsPresent++;
            }

            if (haveGreen) {
                colorsPresent++;
            }

            if (haveBlue) {
                colorsPresent++;
            }

            if (colorsPresent == 0) {
                MipavUtil.displayError("All channels in this color image are single valued");

                return;
            } else if (colorsPresent == 1) {

                if (haveRed) {
                    useRed = true;
                    labelImage = new JLabel("Color red used");
                } else if (haveGreen) {
                    useGreen = true;
                    labelImage = new JLabel("Color green used");
                } else {
                    useBlue = true;
                    labelImage = new JLabel("Color blue used");
                }

                labelImage.setForeground(Color.black);
                labelImage.setFont(serif12);

                colorPanel.setBorder(buildTitledBorder("Color used"));
                colorPanel.add(labelImage);
            } // else if (colorsPresent == 1)
            else if (colorsPresent >= 2) {
                labelImage = new JLabel("Select 1 of the colors");
                labelImage.setForeground(Color.black);
                labelImage.setFont(serif12);

                GridBagConstraints gbc2 = new GridBagConstraints();
                gbc2.gridwidth = 1;
                gbc2.gridheight = 1;
                gbc2.anchor = GridBagConstraints.WEST;
                gbc2.weightx = 1;
                gbc2.insets = new Insets(3, 3, 3, 3);
                gbc2.fill = GridBagConstraints.HORIZONTAL;
                gbc2.gridx = 0;
                gbc2.gridy = 0;

                colorPanel.setBorder(buildTitledBorder("Color selection"));
                colorPanel.add(labelImage, gbc2);
                colorGroup = new ButtonGroup();

                if (haveRed) {
                    gbc2.gridy = yButton++;
                    redButton = new JRadioButton("Red", buttonSet);
                    buttonSet = false;
                    redButton.setFont(serif12);
                    redButton.setForeground(Color.black);
                    colorGroup.add(redButton);
                    colorPanel.add(redButton, gbc2);
                } // if (haveRed)

                if (haveGreen) {
                    gbc2.gridy = yButton++;
                    greenButton = new JRadioButton("Green", buttonSet);
                    buttonSet = false;
                    greenButton.setFont(serif12);
                    greenButton.setForeground(Color.black);
                    colorGroup.add(greenButton);
                    colorPanel.add(greenButton, gbc2);
                } // if (haveGreen)

                if (haveBlue) {
                    gbc2.gridy = yButton++;
                    blueButton = new JRadioButton("Blue", buttonSet);
                    blueButton.setFont(serif12);
                    blueButton.setForeground(Color.black);
                    colorGroup.add(blueButton);
                    colorPanel.add(blueButton, gbc2);
                } // if (haveBlue)
            } // else if (colorsPresent >= 2)

        } // if (image.isColorImage())

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

        if (image.isColorImage()) {
            gbc3.gridy = 2;
            imagePanel.add(colorPanel, gbc3);
        }

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        yPos = 0;
        gbc.gridx = 0;
        gbc.gridy = 0;


        gbc = new GridBagConstraints();
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

        regCheckBox = new JCheckBox("Registration before FRET");
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


        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        yPos = 0;
        gbc.gridy = yPos++;
        mainPanel.add(registrationPanel, gbc);

        getContentPane().add(imagePanel, BorderLayout.NORTH);
        getContentPane().add(mainPanel, BorderLayout.CENTER);
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
        float[] hsb;
        float hue;
        nBoundingVOIs = 0;

        if (image.getNDims() == 2) {
            String selectedName = (String) imageComboBox.getSelectedItem();
            postImage = UI.getRegisteredImageByName(selectedName);

            if (postImage == null) {
                return false;
            }

            VOIs = postImage.getVOIs();
        } // if (image.getNDims() == 2)
        else {
            VOIs = image.getVOIs();
        }

        nVOIs = VOIs.size();

        for (i = 0; i < nVOIs; i++) {

            if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                nBoundingVOIs++;
                hsb = Color.RGBtoHSB(VOIs.VOIAt(i).getColor().getRed(), VOIs.VOIAt(i).getColor().getGreen(),
                                     VOIs.VOIAt(i).getColor().getBlue(), null);
                hue = hsb[0];

                if ((Math.abs(hue - 0.0f)) < 0.0001f) {

                    if (donorIndex == -1) {
                        donorIndex = i;
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 donor VOI");

                        return false;
                    }
                } else if ((Math.abs(hue - (2.0f / 3.0f))) < 0.0001f) {

                    if (backgroundIndex == -1) {
                        backgroundIndex = i;
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 background VOI");

                        return false;
                    }
                } else if ((Math.abs(hue - (1.0f / 3.0f))) < 0.0001f) {

                    if (signalIndex == -1) {
                        signalIndex = i;
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 signal VOI");

                        return false;
                    }
                } else {
                    MipavUtil.displayError("Contour VOI with illegal hue = " + hue + " is present");

                    return false;
                }
            }
        } // for (i = 0; i < nVOIs; i++)

        if (donorIndex == -1) {
            MipavUtil.displayError("Must specify a donor VOI");

            return false;
        }

        register = regCheckBox.isSelected();

        if (image.getNDims() == 2) {

            if (VOIs.VOIAt(donorIndex).getCurves().size() == 0) {
                MipavUtil.displayError("Must have donor VOI present in postbleached " + postImage.getImageName());

                return false;
            }

            if (backgroundIndex >= 0) {

                if (VOIs.VOIAt(backgroundIndex).getCurves().size() == 0) {
                    MipavUtil.displayError("Must have background VOI present in postbleached " +
                                           postImage.getImageName());

                    return false;
                }
            } // if (backgroundIndex >= 0)

            if (signalIndex >= 0) {

                if (VOIs.VOIAt(signalIndex).getCurves().size() == 0) {
                    MipavUtil.displayError("Must have signal VOI present in postbleached " + postImage.getImageName());

                    return false;
                }

                if (backgroundIndex < 0) {
                    MipavUtil.displayError("Cannot use signal VOI without a background VOI");

                    return false;
                }
            } // if (signalIndex >= 0)
        } // if (image.getNDims() == 2)
        else { // 3D image

            if (VOIs.VOIAt(donorIndex).getCurves().size() == 1) {
                MipavUtil.displayError("Must have donor VOI present in postbleached slice of " + image.getImageName());

                return false;
            }

            if (backgroundIndex >= 0) {

                if (VOIs.VOIAt(backgroundIndex).getCurves().size() == 1) {
                    MipavUtil.displayError("Must have background VOI present in postbleached slice of " +
                                           image.getImageName());

                    return false;
                }
            } // if (backgroundIndex >= 0)

            if (signalIndex >= 0) {

                if (VOIs.VOIAt(signalIndex).getCurves().size() == 1) {
                    MipavUtil.displayError("Must have signal VOI present in postbleached slice of " +
                                           image.getImageName());

                    return false;
                }

                if (backgroundIndex < 0) {
                    MipavUtil.displayError("Cannot use signal VOI without a background VOI");

                    return false;
                }
            } // if (signalIndex >= 0)

        }

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

        if (image.isColorImage()) {

            if (colorsPresent >= 2) {

                if ((redButton != null) && (redButton.isSelected())) {
                    useRed = true;
                } else if ((greenButton != null) && (greenButton.isSelected())) {
                    useGreen = true;
                } else {
                    useBlue = true;
                }
            }


        } // if (image.isColorImage())


        return true;
    }

}
