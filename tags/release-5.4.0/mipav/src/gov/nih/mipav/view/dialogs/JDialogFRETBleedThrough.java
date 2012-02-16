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
public class JDialogFRETBleedThrough extends JDialogScriptableBase
        implements AlgorithmInterface, ListSelectionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 8726771970079234501L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JRadioButton acceptorButton;

    /** DOCUMENT ME! */
    private boolean acceptorRun = true;

    /** DOCUMENT ME! */
    private JRadioButton activeButton;

    /** DOCUMENT ME! */
    private int activeIndex;

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
    private boolean doColor;

    /** DOCUMENT ME! */
    private JRadioButton donorButton;

    /** DOCUMENT ME! */
    private ButtonGroup dyeGroup;

    /** DOCUMENT ME! */
    private AlgorithmFRETBleedThrough fbtAlgo;

    /** DOCUMENT ME! */
    private ModelImage FP2Image = null;

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

    /**
     * The source image must be either an image with donor dye only taken with a donor fluorescent peak filter or an
     * image with acceptor dye only taken with a acceptor fluorescent peak filter.
     */
    private ModelImage srcImage;

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
    public JDialogFRETBleedThrough() { }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  im  Source image.
     */
    public JDialogFRETBleedThrough(ModelImage im) {
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
     * Creates a new JDialogFRETBleedThrough object.
     *
     * @param  theParentFrame  parent frame
     * @param  im              source image
     */
    public JDialogFRETBleedThrough(Frame theParentFrame, ModelImage im) {
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
            FP2Image = open();

            if (FP2Image == null) {
                return;
            }

            if (!checkImage(FP2Image)) {
                FP2Image.disposeLocal();
                FP2Image = null;

                return;
            }

            model2.addElement(FP2Image.getImageName());
            removeButton2.setEnabled(true);
            chooserButton2.setEnabled(false);
        } // if (command.equals("Choose2"))
        else if (command.equals("Remove2")) {
            model2.removeElement(FP2Image.getImageName());
            FP2Image.disposeLocal();
            FP2Image = null;
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
            MipavUtil.showHelp("10096");
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

        if (algorithm instanceof AlgorithmFRETBleedThrough) {

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }

            dispose();
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  acceptorRun  DOCUMENT ME!
     */
    public void setAcceptorRun(boolean acceptorRun) {
        this.acceptorRun = acceptorRun;
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
     * Once all the necessary variables are set, call AlgorithmFRETBleedThrough.
     */
    protected void callAlgorithm() {
        System.gc();

        try {

            // Make algorithm
            fbtAlgo = new AlgorithmFRETBleedThrough(srcImage, FRETImage, FP2Image, useRed, useGreen, useBlue,
                                                    acceptorRun);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            fbtAlgo.addListener(this);

            createProgressBar(srcImage.getImageName(), fbtAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (fbtAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                fbtAlgo.run();
            }
        } catch (OutOfMemoryError x) {


            System.gc();
            MipavUtil.displayError("Dialog FRET Bleed Through: unable to allocate enough memory");

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
        FP2Image = scriptParameters.retrieveImage("FP2_image");

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

        setAcceptorRun(scriptParameters.getParams().getBoolean("is_acceptor_dye_only"));

        if (!checkImage(FRETImage)) {
            return;
        }

        if (!checkImage(FP2Image)) {
            return;
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(srcImage);
        scriptParameters.storeImage(FRETImage, "FRET_image");
        scriptParameters.storeImage(FP2Image, "FP2_image");

        scriptParameters.storeColorOptions(useRed, useGreen, useBlue);
        scriptParameters.getParams().put(ParameterFactory.newParameter("is_acceptor_dye_only", acceptorRun));
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
            MipavUtil.displayError("Cannot add original FP1 filter image");

            return false;
        }

        if ((FP2Image != null) && (FRETImage != null) && (FRETImage.getImageName() == FP2Image.getImageName())) {
            MipavUtil.displayError("Cannot use same image for FRET and FP2 bleed throughs");

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

        JPanel namePanel;

        JPanel VOIPanel;
        JPanel dyePanel;
        setForeground(Color.black);

        setTitle("FRET Bleed Through");

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

        JLabel nameLabel = new JLabel("1FP image with 1FP filter: " + srcImage.getImageName());
        nameLabel.setFont(serif12);
        nameLabel.setForeground(Color.black);
        namePanel.add(nameLabel, gbc4);

        dyePanel = new JPanel(new GridBagLayout());
        dyePanel.setBorder(buildTitledBorder("Select dye type used"));
        dyeGroup = new ButtonGroup();
        acceptorButton = new JRadioButton("Acceptor dye only", true);
        acceptorButton.setForeground(Color.black);
        acceptorButton.setFont(serif12);
        dyeGroup.add(acceptorButton);
        gbc4.gridy = 0;
        dyePanel.add(acceptorButton, gbc4);

        donorButton = new JRadioButton("Donor dye only", false);
        donorButton.setForeground(Color.black);
        donorButton.setFont(serif12);
        dyeGroup.add(donorButton);
        gbc4.gridy = 1;
        dyePanel.add(donorButton, gbc4);

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
        paramPanel.add(dyePanel, gbc);
        gbc.gridy = yPos++;
        paramPanel.add(VOIPanel, gbc);

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
        imagePanel.setBorder(buildTitledBorder("Load 1FP image taken with FRET filter"));

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
        imagePanel2.setBorder(buildTitledBorder("Load 1FP image taken with 2FP filter"));

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


        gbc.gridx = 0;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.gridy = yPos++;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        paramPanel.add(imagePanel, gbc);
        gbc.gridy = yPos++;
        paramPanel.add(imagePanel2, gbc);

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

        acceptorRun = acceptorButton.isSelected();

        return true;
    }

}
