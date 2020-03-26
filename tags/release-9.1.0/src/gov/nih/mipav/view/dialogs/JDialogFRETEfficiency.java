package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * Dialog to get user input, then call the algorithm.
 */
public class JDialogFRETEfficiency extends JDialogScriptableBase implements AlgorithmInterface, ListSelectionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 156910291527442369L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage acceptorImage = null;

    /** DOCUMENT ME! */
    private JRadioButton activeButton;

    /** DOCUMENT ME! */
    private int activeIndex;

    /** DOCUMENT ME! */
    private float AFPintoDFP;

    /** DOCUMENT ME! */
    private float AFPintoFRET;

    /** DOCUMENT ME! */
    private JRadioButton backgroundButton;

    /** DOCUMENT ME! */
    private int backgroundIndex;

    /** DOCUMENT ME! */
    private JRadioButton blueButton;

    /** DOCUMENT ME! */
    private JButton chooserButton;

    /** DOCUMENT ME! */
    private JButton chooserButton2;

    /** DOCUMENT ME! */
    private ButtonGroup colorGroup;

    /** DOCUMENT ME! */
    private JPanel colorPanel;

    /** DOCUMENT ME! */
    private ViewJComponentEditImage componentImage;

    /** DOCUMENT ME! */
    private boolean createEfficiencyImage = false;

    /** DOCUMENT ME! */
    private boolean createFDivAImage = false;

    /** DOCUMENT ME! */
    private boolean createFDivDImage = false;

    /** DOCUMENT ME! */
    private float DFPintoAFP;

    /** DOCUMENT ME! */
    private float DFPintoFRET;

    /** DOCUMENT ME! */
    private boolean doColor;

    /** DOCUMENT ME! */
    private ModelImage efficiencyImage = null;

    /** DOCUMENT ME! */
    private JCheckBox effImageCheckBox;

    /** DOCUMENT ME! */
    private JCheckBox fDivACheckBox;

    /** DOCUMENT ME! */
    private ModelImage fDivAImage = null;

    /** DOCUMENT ME! */
    private JCheckBox fDivDCheckBox;

    /** DOCUMENT ME! */
    private ModelImage fDivDImage = null;

    /** DOCUMENT ME! */
    private AlgorithmFRETEfficiency feffAlgo;

    /** DOCUMENT ME! */
    private ModelImage FRETImage = null;

    /** DOCUMENT ME! */
    private JRadioButton greenButton;

    /** DOCUMENT ME! */
    private JList imageList;

    /** DOCUMENT ME! */
    private JList imageList2;

    /** DOCUMENT ME! */
    private JPanel imagePanel;

    /** DOCUMENT ME! */
    private JPanel imagePanel2;

    /** DOCUMENT ME! */
    private DefaultListModel model;

    /** DOCUMENT ME! */
    private DefaultListModel model2;

    /** DOCUMENT ME! */
    private int nBoundingVOIs;

    /** DOCUMENT ME! */
    private JPanel paramPanel;

    /** DOCUMENT ME! */
    private JRadioButton redButton;

    /** DOCUMENT ME! */
    private JButton removeButton;

    /** DOCUMENT ME! */
    private JButton removeButton2;

    /** The source image must be the image taken with the donor fluorescent peak filter. */
    private ModelImage srcImage;

    /** DOCUMENT ME! */
    private JTextField textAFPintoDFP;

    /** DOCUMENT ME! */
    private JTextField textAFPintoFRET;

    /** DOCUMENT ME! */
    private JTextField textDFPintoAFP;

    /** DOCUMENT ME! */
    private JTextField textDFPintoFRET;

    /** DOCUMENT ME! */
    private boolean useBlue = false;

    /** DOCUMENT ME! */
    private boolean useGreen = false;

    /** DOCUMENT ME! */
    private boolean useRed = false;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private ButtonGroup VOIGroup;

    /** DOCUMENT ME! */
    private ViewVOIVector VOIs;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogFRETEfficiency() { }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  im  Source image.
     */
    public JDialogFRETEfficiency(ModelImage im) {
        super();
        userInterface = ViewUserInterface.getReference();
        srcImage = im;
        parentFrame = srcImage.getParentFrame();
        componentImage = ((ViewJFrameImage) parentFrame).getComponentImage();

        if (im.isColorImage()) {
            doColor = true;
            useRed = true;
            useBlue = false;
            useGreen = false;
        } else {
            doColor = false;
            useRed = false;
            useBlue = false;
            useGreen = false;
        }
    }


    /**
     * Creates a new JDialogFRETEfficiency object.
     *
     * @param  theParentFrame  parent frame
     * @param  im              source image
     */
    public JDialogFRETEfficiency(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        srcImage = im;
        componentImage = ((ViewJFrameImage) theParentFrame).getComponentImage();
        userInterface = ViewUserInterface.getReference();

        if (im.isColorImage()) {
            doColor = true;
            useRed = true;
            useBlue = false;
            useGreen = false;
        } else {
            doColor = false;
            useRed = false;
            useBlue = false;
            useGreen = false;
        }

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

        if (command.equals("Choose")) {
            FRETImage = open();

            if (FRETImage == null) {
                return;
            }

            if (!checkImage(FRETImage)) {
                FRETImage.disposeLocal();
                FRETImage = null;

                return;
            }

            model.addElement(FRETImage.getImageName());
            removeButton.setEnabled(true);
            chooserButton.setEnabled(false);
        } // if (command.equals("Choose"))
        else if (command.equals("Remove")) {
            model.removeElement(FRETImage.getImageName());
            FRETImage.disposeLocal();
            FRETImage = null;
            chooserButton.setEnabled(true);
            removeButton.setEnabled(false);
        } // else if (command.equals("Remove"))
        else if (command.equals("Choose2")) {
            acceptorImage = open();

            if (acceptorImage == null) {
                return;
            }

            if (!checkImage(acceptorImage)) {
                acceptorImage.disposeLocal();
                acceptorImage = null;

                return;
            }

            model2.addElement(acceptorImage.getImageName());
            removeButton2.setEnabled(true);
            chooserButton2.setEnabled(false);
        } // if ((command.equals("Choose2"))
        else if (command.equals("Remove2")) {
            model2.removeElement(acceptorImage.getImageName());
            acceptorImage.disposeLocal();
            acceptorImage = null;
            chooserButton2.setEnabled(true);
            removeButton2.setEnabled(false);
        } // else if (command.equals("Remove2"))
        else if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if ((source == activeButton) || (source == backgroundButton)) {

            if (activeButton.isSelected()) {
                componentImage.getVOIHandler().newVOI(-1.0f); // no preset color
                //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                //componentImage.getVOIHandler().setPresetHue(-1.0f); // no preset color
            } else if (backgroundButton.isSelected()) {
                componentImage.getVOIHandler().newVOI(2.0f / 3.0f); // blue
                //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                //componentImage.getVOIHandler().setPresetHue(2.0f / 3.0f); // blue
            }
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("10097");
            MipavUtil.showWebHelp("Microscopy:_Fluorescence_Resonance_Energy_Transfer_(FRET)-Acceptor#Applying_the_FRET_algorithm");
        } else {
            super.actionPerformed(event);
        }
    }


    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmFRETEfficiency) {
            int yInc = 0;

            if (fDivDImage != null) {

                try {
                    new ViewJFrameImage(fDivDImage, null, new Dimension(610, 200));
                    yInc += 20;
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } // if (fDivDImage != null)

            if (fDivAImage != null) {

                try {
                    new ViewJFrameImage(fDivAImage, null, new Dimension(610, 200 + yInc));
                    yInc += 20;
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } // if (fDivAImage != null)

            if (efficiencyImage != null) {

                try {
                    new ViewJFrameImage(efficiencyImage, null, new Dimension(610, 200 + yInc));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } // if (efficiencyImage != null)

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }

            dispose();
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  AFPintoDFP  DOCUMENT ME!
     */
    public void setAFPintoDFP(float AFPintoDFP) {
        this.AFPintoDFP = AFPintoDFP;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  AFPintoFRET  DOCUMENT ME!
     */
    public void setAFPintoFRET(float AFPintoFRET) {
        this.AFPintoFRET = AFPintoFRET;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  useBlue  DOCUMENT ME!
     */
    public void setBlue(boolean useBlue) {
        this.useBlue = useBlue;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  createEfficiencyImage  DOCUMENT ME!
     */
    public void setCreateEfficiencyImage(boolean createEfficiencyImage) {
        this.createEfficiencyImage = createEfficiencyImage;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  createFDivAImage  DOCUMENT ME!
     */
    public void setCreateFDivAImage(boolean createFDivAImage) {
        this.createFDivAImage = createFDivAImage;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  createFDivDImage  DOCUMENT ME!
     */
    public void setCreateFDivDImage(boolean createFDivDImage) {
        this.createFDivDImage = createFDivDImage;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  DFPintoAFP  DOCUMENT ME!
     */
    public void setDFPintoAFP(float DFPintoAFP) {
        this.DFPintoAFP = DFPintoAFP;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  DFPintoFRET  DOCUMENT ME!
     */
    public void setDFPintoFRET(float DFPintoFRET) {
        this.DFPintoFRET = DFPintoFRET;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  useGreen  DOCUMENT ME!
     */
    public void setGreen(boolean useGreen) {
        this.useGreen = useGreen;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  useRed  DOCUMENT ME!
     */
    public void setRed(boolean useRed) {
        this.useRed = useRed;
    }


    /**
     * Accessor that sets the source image.
     *
     * @param  image  new source image.
     */
    public void setSourceImage(ModelImage image) {
        srcImage = image;
    }

    /**
     * Sets the remove index based on the selected index in the list.
     *
     * @param  evt  Event that caused this method to fire.
     */
    public void valueChanged(ListSelectionEvent evt) { }

    /**
     * Once all the necessary variables are set, call AlgorithmFRETEfficiency.
     */
    protected void callAlgorithm() {
        System.gc();

        try {

            // Make algorithm
            if (createFDivDImage) {
                fDivDImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(),
                                            makeImageName(srcImage.getImageName(), "_f/d"));
            }

            if (createFDivAImage) {
                fDivAImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(),
                                            makeImageName(srcImage.getImageName(), "_f/a"));
            }

            if (createEfficiencyImage) {
                efficiencyImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(),
                                                 makeImageName(srcImage.getImageName(), "_eff"));
            }

            feffAlgo = new AlgorithmFRETEfficiency(srcImage, FRETImage, acceptorImage, useRed, useGreen, useBlue,
                                                   DFPintoFRET, AFPintoFRET, DFPintoAFP, AFPintoDFP, fDivDImage,
                                                   fDivAImage, efficiencyImage);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            feffAlgo.addListener(this);

            createProgressBar(srcImage.getImageName(), feffAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (feffAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                feffAlgo.run();
            }
        } catch (OutOfMemoryError x) {


            System.gc();
            MipavUtil.displayError("Dialog FRET Efficiency: unable to allocate enough memory");

            return;
        }

    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        srcImage = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = srcImage.getParentFrame();

        FRETImage = scriptParameters.retrieveImage("FRET_image");
        acceptorImage = scriptParameters.retrieveImage("acceptor_image");

        if (srcImage.isColorImage()) {
            doColor = true;
            useRed = true;
            useBlue = false;
            useGreen = false;
        } else {
            doColor = false;
            useRed = false;
            useBlue = false;
            useGreen = false;
        }

        boolean[] rgb = scriptParameters.getParams().getList(AlgorithmParameters.DO_PROCESS_RGB).getAsBooleanArray();
        setRed(rgb[0]);
        setGreen(rgb[1]);
        setBlue(rgb[2]);

        setDFPintoFRET(scriptParameters.getParams().getFloat("DFP_into_FRET"));
        setAFPintoFRET(scriptParameters.getParams().getFloat("AFP_into_FRET"));
        setDFPintoAFP(scriptParameters.getParams().getFloat("DFP_into_AFP"));
        setAFPintoDFP(scriptParameters.getParams().getFloat("AFP_into_DFP"));
        setCreateFDivDImage(scriptParameters.getParams().getBoolean("do_create_FRET_donor_image"));
        setCreateFDivAImage(scriptParameters.getParams().getBoolean("do_create_FRET_acceptor_image"));
        setCreateEfficiencyImage(scriptParameters.getParams().getBoolean("do_create_efficiency_image"));

        if (!checkImage(FRETImage)) {
            return;
        }

        if (!checkImage(acceptorImage)) {
            return;
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(srcImage);
        scriptParameters.storeImage(FRETImage, "FRET_image");
        scriptParameters.storeImage(acceptorImage, "acceptor_image");

        scriptParameters.storeColorOptions(useRed, useGreen, useBlue);
        scriptParameters.getParams().put(ParameterFactory.newParameter("DFP_into_FRET", DFPintoFRET));
        scriptParameters.getParams().put(ParameterFactory.newParameter("AFP_into_FRET", AFPintoFRET));
        scriptParameters.getParams().put(ParameterFactory.newParameter("DFP_into_AFP", DFPintoAFP));
        scriptParameters.getParams().put(ParameterFactory.newParameter("AFP_into_DFP", AFPintoDFP));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_create_FRET_donor_image", createFDivDImage));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_create_FRET_acceptor_image",
                                                                       createFDivAImage));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_create_efficiency_image",
                                                                       createEfficiencyImage));
    }

    /**
     * Checks the color and dimensionality of the new image vs. the original source image. The new image cannot be color
     * unless the source image is color. However, then new image may be black and white when the source image is color.
     * All new images should have the same dimensions as the source.
     *
     * @param   testImage  DOCUMENT ME!
     *
     * @return  Flag indicating if the image checks out.
     */
    private boolean checkImage(ModelImage testImage) {

        if (testImage == null) {
            return false;
        }

        if (testImage.getImageName() == srcImage.getImageName()) {
            MipavUtil.displayError("Cannot add original donor filter image");

            return false;
        }

        if ((acceptorImage != null) && (FRETImage != null) &&
                (FRETImage.getImageName() == acceptorImage.getImageName())) {
            MipavUtil.displayError("Cannot use same image for FRET and acceptor filters");

            return false;
        }

        if ((srcImage.isColorImage() == false) && (testImage.isColorImage() == true)) {
            MipavUtil.displayError("Cannot load a color (" + testImage.getImageName() +
                                   ") unless the original file is color.");

            return false;
        }

        if (srcImage.getNDims() != testImage.getNDims()) {
            MipavUtil.displayError("Error! " + srcImage.getImageName() + " is " + srcImage.getNDims() + "D, while " +
                                   testImage.getImageName() + " is " + testImage.getNDims() + "D");

            return false;
        }

        for (int i = 0; i < srcImage.getNDims(); i++) {

            if ((testImage != null) && (srcImage.getExtents()[i] != testImage.getExtents()[i])) {
                MipavUtil.displayError("Error! For dimension = " + i + " " + srcImage.getImageName() +
                                       " has length = " + srcImage.getExtents()[i] + " while " +
                                       testImage.getImageName() + " has length = " + testImage.getExtents()[i]);

                return false;
            }
        }

        return true;

    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        int yPos = 0;
        JPanel VOIPanel;
        JPanel namePanel;
        JPanel bleedThruPanel;
        setForeground(Color.black);

        JLabel labelDFPintoFRET;
        JLabel labelAFPintoFRET;
        JLabel labelDFPintoAFP;
        JLabel labelAFPintoDFP;
        int i;
        RandomAccessFile raFile;
        String StringDFPintoFRET = null;
        String StringDFPintoAFP = null;
        String StringAFPintoFRET = null;
        String StringAFPintoDFP = null;

        setTitle("FRET Efficiency");

        GridBagConstraints gbc4 = new GridBagConstraints();
        gbc4.gridwidth = 1;
        gbc4.gridheight = 1;
        gbc4.anchor = GridBagConstraints.WEST;
        gbc4.weightx = 1;
        gbc4.insets = new Insets(3, 3, 3, 3);
        gbc4.fill = GridBagConstraints.HORIZONTAL;
        gbc4.gridx = 0;
        gbc4.gridy = 0;


        namePanel = new JPanel(new GridBagLayout());

        JLabel nameLabel = new JLabel("DFP/AFP image taken with DFP filter: " + srcImage.getImageName());
        nameLabel.setFont(serif12);
        nameLabel.setForeground(Color.black);
        namePanel.add(nameLabel, gbc4);

        VOIPanel = new JPanel(new GridBagLayout());
        VOIPanel.setBorder(buildTitledBorder("Select VOIs"));


        VOIGroup = new ButtonGroup();

        backgroundButton = new JRadioButton("Add background VOI", true);
        backgroundButton.setForeground(Color.blue);
        backgroundButton.setFont(serif12);
        backgroundButton.addActionListener(this);
        VOIGroup.add(backgroundButton);
        gbc4.gridy = 0;
        VOIPanel.add(backgroundButton, gbc4);
        componentImage.getVOIHandler().newVOI(2.0f / 3.0f); // blue
        //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
        //componentImage.getVOIHandler().setPresetHue(2.0f / 3.0f); // blue

        activeButton = new JRadioButton("Add active VOI", false);
        activeButton.setForeground(Color.red);
        activeButton.setFont(serif12);
        activeButton.addActionListener(this);
        VOIGroup.add(activeButton);
        gbc4.gridy = 1;
        VOIPanel.add(activeButton, gbc4);


        paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;


        gbc.gridx = 0;
        gbc.gridy = yPos++;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1;
        gbc.gridwidth = 1;
        paramPanel.add(namePanel, gbc);
        gbc.gridy = yPos++;
        paramPanel.add(VOIPanel, gbc);

        bleedThruPanel = new JPanel(new GridBagLayout());
        bleedThruPanel.setBorder(buildTitledBorder("Enter bleed thru values"));
        gbc.gridy = yPos++;
        paramPanel.add(bleedThruPanel, gbc);

        File currentFileDir = new File(srcImage.getFileInfo(0).getFileDirectory());

        if (currentFileDir.exists() && currentFileDir.isDirectory()) {
            File[] files = currentFileDir.listFiles();

            for (i = 0; i < files.length; i++) {

                if (files[i].getName().startsWith("acceptor_")) {

                    if (files[i].exists()) {

                        try {
                            raFile = new RandomAccessFile(files[i], "r");
                            StringAFPintoFRET = raFile.readLine();
                            StringAFPintoDFP = raFile.readLine();
                            raFile.close();
                        } catch (IOException e) {
                            MipavUtil.displayError("IOException from " + files[i].getName());
                        }
                    }
                }

                if (files[i].getName().startsWith("donor_")) {

                    if (files[i].exists()) {

                        try {
                            raFile = new RandomAccessFile(files[i], "r");
                            StringDFPintoFRET = raFile.readLine();
                            StringDFPintoAFP = raFile.readLine();
                            raFile.close();
                        } catch (IOException e) {
                            MipavUtil.displayError("IOException from " + files[i].getName());
                        }
                    }
                }
            } // for (i = 0; i < files.length; i++)
        } // if (currentFileDir.exists() && currentFileDir.isDirectory())

        labelDFPintoFRET = new JLabel("Bleed thru from DFP into FRET (0.0 - 1.0) ");
        labelDFPintoFRET.setForeground(Color.black);
        labelDFPintoFRET.setFont(serif12);
        gbc4.gridx = 0;
        gbc4.gridy = 0;
        bleedThruPanel.add(labelDFPintoFRET, gbc4);
        textDFPintoFRET = new JTextField();

        if (StringDFPintoFRET != null) {
            textDFPintoFRET.setText(StringDFPintoFRET);
        } else {
            textDFPintoFRET.setText("0.0");
        }

        textDFPintoFRET.setFont(serif12);
        gbc4.gridx = 1;
        bleedThruPanel.add(textDFPintoFRET, gbc4);

        labelAFPintoFRET = new JLabel("Bleed thru from AFP into FRET (0.0 - 1.0) ");
        labelAFPintoFRET.setForeground(Color.black);
        labelAFPintoFRET.setFont(serif12);
        gbc4.gridx = 0;
        gbc4.gridy = 1;
        bleedThruPanel.add(labelAFPintoFRET, gbc4);
        textAFPintoFRET = new JTextField();

        if (StringAFPintoFRET != null) {
            textAFPintoFRET.setText(StringAFPintoFRET);
        } else {
            textAFPintoFRET.setText("0.0");
        }

        textAFPintoFRET.setFont(serif12);
        gbc4.gridx = 1;
        bleedThruPanel.add(textAFPintoFRET, gbc4);

        labelDFPintoAFP = new JLabel("Bleed thru from DFP into AFP (0.0 - 1.0) ");
        labelDFPintoAFP.setForeground(Color.black);
        labelDFPintoAFP.setFont(serif12);
        gbc4.gridx = 0;
        gbc4.gridy = 2;
        bleedThruPanel.add(labelDFPintoAFP, gbc4);
        textDFPintoAFP = new JTextField();

        if (StringDFPintoAFP != null) {
            textDFPintoAFP.setText(StringDFPintoAFP);
        } else {
            textDFPintoAFP.setText("0.0");
        }

        textDFPintoAFP.setFont(serif12);
        gbc4.gridx = 1;
        bleedThruPanel.add(textDFPintoAFP, gbc4);

        labelAFPintoDFP = new JLabel("Bleed thru from AFP into DFP (0.0 - 1.0) ");
        labelAFPintoDFP.setForeground(Color.black);
        labelAFPintoDFP.setFont(serif12);
        gbc4.gridx = 0;
        gbc4.gridy = 3;
        bleedThruPanel.add(labelAFPintoDFP, gbc4);
        textAFPintoDFP = new JTextField();

        if (StringAFPintoDFP != null) {
            textAFPintoDFP.setText(StringAFPintoDFP);
        } else {
            textAFPintoDFP.setText("0.0");
        }

        textAFPintoDFP.setFont(serif12);
        gbc4.gridx = 1;
        bleedThruPanel.add(textAFPintoDFP, gbc4);

        if (doColor) {
            colorPanel = new JPanel(new GridBagLayout());
            colorPanel.setBorder(buildTitledBorder("Channels"));
            colorGroup = new ButtonGroup();
            redButton = new JRadioButton("Use red color", true);
            redButton.setForeground(Color.red);
            redButton.setFont(serif12);
            colorGroup.add(redButton);


            greenButton = new JRadioButton("Use green color", false);
            greenButton.setForeground(Color.green.darker());
            greenButton.setFont(serif12);
            colorGroup.add(greenButton);

            blueButton = new JRadioButton("Use blue color", false);
            blueButton.setForeground(Color.blue);
            blueButton.setFont(serif12);
            colorGroup.add(blueButton);

            gbc.gridx = 0;
            gbc.gridy = 0;
            colorPanel.add(redButton, gbc);
            gbc.gridy = 1;
            colorPanel.add(greenButton, gbc);
            gbc.gridy = 2;
            colorPanel.add(blueButton, gbc);
            gbc.gridx = 1;
            colorPanel.add(Box.createHorizontalStrut(5), gbc);

            gbc.gridx = 0;
            gbc.gridy = yPos++;
            gbc.gridwidth = 1;
            paramPanel.add(colorPanel, gbc);

        } // if (doColor)

        imagePanel = new JPanel(new BorderLayout());
        imagePanel.setBorder(buildTitledBorder("Load DFP/AFP image taken with FRET filter"));

        model = new DefaultListModel();
        imageList = new JList(model);
        imageList.setVisibleRowCount(2);
        imageList.setPreferredSize(new Dimension(300, 60));
        imageList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        imageList.addListSelectionListener(this);
        imagePanel.add(imageList);

        JPanel chooserPanel = new JPanel();
        chooserButton = new JButton("Load");
        chooserButton.setPreferredSize(MipavUtil.defaultButtonSize);
        chooserButton.setFont(serif12B);
        chooserPanel.add(chooserButton);
        chooserButton.addActionListener(this);
        chooserButton.setActionCommand("Choose");

        removeButton = new JButton("Remove");
        removeButton.setPreferredSize(MipavUtil.defaultButtonSize);
        removeButton.setFont(serif12B);
        removeButton.setEnabled(false);
        chooserPanel.add(removeButton);
        removeButton.addActionListener(this);
        removeButton.setActionCommand("Remove");

        imagePanel.add(chooserPanel, BorderLayout.SOUTH);

        imagePanel2 = new JPanel(new BorderLayout());
        imagePanel2.setBorder(buildTitledBorder("Load DFP/AFP image taken with AFP filter"));

        model2 = new DefaultListModel();
        imageList2 = new JList(model2);
        imageList2.setVisibleRowCount(2);
        imageList2.setPreferredSize(new Dimension(300, 60));
        imageList2.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        imageList2.addListSelectionListener(this);
        imagePanel2.add(imageList2);

        JPanel chooserPanel2 = new JPanel();
        chooserButton2 = new JButton("Load");
        chooserButton2.setPreferredSize(MipavUtil.defaultButtonSize);
        chooserButton2.setFont(serif12B);
        chooserPanel2.add(chooserButton2);
        chooserButton2.addActionListener(this);
        chooserButton2.setActionCommand("Choose2");

        removeButton2 = new JButton("Remove");
        removeButton2.setPreferredSize(MipavUtil.defaultButtonSize);
        removeButton2.setFont(serif12B);
        removeButton2.setEnabled(false);
        chooserPanel2.add(removeButton2);
        removeButton2.addActionListener(this);
        removeButton2.setActionCommand("Remove2");

        imagePanel2.add(chooserPanel2, BorderLayout.SOUTH);

        JPanel outputPanel = new JPanel(new GridBagLayout());
        outputPanel.setBorder(buildTitledBorder("Create output images"));

        gbc.gridx = 0;
        gbc.gridy = 0;
        fDivDCheckBox = new JCheckBox("Create fret/donor image");
        fDivDCheckBox.setSelected(false);
        fDivDCheckBox.setFont(serif12);
        outputPanel.add(fDivDCheckBox, gbc);

        gbc.gridy = 1;
        fDivACheckBox = new JCheckBox("Create fret/acceptor image");
        fDivACheckBox.setSelected(false);
        fDivACheckBox.setFont(serif12);
        outputPanel.add(fDivACheckBox, gbc);

        gbc.gridy = 2;
        effImageCheckBox = new JCheckBox("Create efficiency image");
        effImageCheckBox.setSelected(false);
        effImageCheckBox.setFont(serif12);
        outputPanel.add(effImageCheckBox, gbc);

        gbc.gridx = 0;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.gridy = yPos++;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        paramPanel.add(imagePanel, gbc);
        gbc.gridy = yPos++;
        paramPanel.add(imagePanel2, gbc);
        gbc.gridy = yPos++;
        paramPanel.add(outputPanel, gbc);
        gbc.gridy = yPos++;

        getContentPane().add(paramPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * Open an image based on the suffix of the file.
     *
     * @return  The image.
     */
    private ModelImage open() {
        JFileChooser chooser = null;
        FileIO fileIO = null;
        boolean multiFile = false;
        String fileName;
        String directory;

        try {

            chooser = new JFileChooser();

            if (userInterface.getDefaultDirectory() != null) {
                File file = new File(userInterface.getDefaultDirectory());

                if (file != null) {
                    chooser.setCurrentDirectory(file);
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }

            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));

            chooser.setDialogTitle("Open Image");

            int returnValue = chooser.showOpenDialog(userInterface.getMainFrame());

            if (returnValue == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                userInterface.setDefaultDirectory(directory);
            } else {
                return null;
            }
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return null;
        }

        try {
            fileIO = new FileIO();

            return fileIO.readImage(fileName, directory, multiFile, null);
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return null;
        }
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        int i;
        int nVOIs;
        float[] hsb;
        float hue;
        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();
        nBoundingVOIs = 0;
        activeIndex = -1;
        backgroundIndex = -1;


        for (i = 0; i < nVOIs; i++) {

            if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                nBoundingVOIs++;
                hsb = Color.RGBtoHSB(VOIs.VOIAt(i).getColor().getRed(), VOIs.VOIAt(i).getColor().getGreen(),
                                     VOIs.VOIAt(i).getColor().getBlue(), null);
                hue = hsb[0];

                if ((Math.abs(hue - (2.0f / 3.0f))) < 0.0001f) {

                    if (backgroundIndex == -1) {
                        backgroundIndex = i;
                        VOIs.VOIAt(i).setName("Background");
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 background VOI");

                        return false;
                    }
                } else {
                    activeIndex = i;
                    VOIs.VOIAt(i).setName("Active" + i);
                }
            }
        } // for (i = 0; i < nVOIs; i++)

        if (activeIndex == -1) {
            MipavUtil.displayError("Must specify an active VOI");

            return false;
        }

        if (backgroundIndex == -1) {
            MipavUtil.displayError("Must specify a background VOI");

            return false;
        }

        if (doColor) {
            useRed = redButton.isSelected();
            useGreen = greenButton.isSelected();
            useBlue = blueButton.isSelected();
        } // if (doColor)

        tmpStr = textDFPintoFRET.getText();

        if (testParameter(tmpStr, 0.0, 1.0)) {
            DFPintoFRET = Float.valueOf(tmpStr).floatValue();
        } else {
            textDFPintoFRET.requestFocus();
            textDFPintoFRET.selectAll();

            return false;
        }

        tmpStr = textAFPintoFRET.getText();

        if (testParameter(tmpStr, 0.0, 1.0)) {
            AFPintoFRET = Float.valueOf(tmpStr).floatValue();
        } else {
            textAFPintoFRET.requestFocus();
            textAFPintoFRET.selectAll();

            return false;
        }

        tmpStr = textDFPintoAFP.getText();

        if (testParameter(tmpStr, 0.0, 1.0)) {
            DFPintoAFP = Float.valueOf(tmpStr).floatValue();
        } else {
            textDFPintoAFP.requestFocus();
            textDFPintoAFP.selectAll();

            return false;
        }

        tmpStr = textAFPintoDFP.getText();

        if (testParameter(tmpStr, 0.0, 1.0)) {
            AFPintoDFP = Float.valueOf(tmpStr).floatValue();
        } else {
            textAFPintoDFP.requestFocus();
            textAFPintoDFP.selectAll();

            return false;
        }

        createFDivDImage = fDivDCheckBox.isSelected();

        createFDivAImage = fDivACheckBox.isSelected();

        createEfficiencyImage = effImageCheckBox.isSelected();

        return true;
    }

}
