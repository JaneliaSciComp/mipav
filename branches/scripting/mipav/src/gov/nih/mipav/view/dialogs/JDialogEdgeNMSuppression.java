package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user is able to control the degree of blurring in all
 * dimensions. User can indicate whether to have algorithm applied to whole image or to the VOI regions. Algorithms are
 * executed in their own thread.
 *
 * @see  AlgorithmGaussianBlur
 */
public class JDialogEdgeNMSuppression extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7682705151759726551L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** false = apply algorithm only to VOI regions. */
    private boolean image25D = false; // Flag for applying to every slice

    /** DOCUMENT ME! */
    private JCheckBox image25DCheckbox;

    /** DOCUMENT ME! */
    private ButtonGroup imageVOIGroup;

    /** DOCUMENT ME! */
    private JPanel imageVOIPanel;

    /** DOCUMENT ME! */
    private JLabel labelCorrected;

    /** DOCUMENT ME! */
    private JLabel labelGaussX;

    /** DOCUMENT ME! */
    private JLabel labelGaussY;

    /** DOCUMENT ME! */
    private JLabel labelGaussZ;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private AlgorithmEdgeNMSuppression nmSupAlgo;

    /** DOCUMENT ME! */
    private float normFactor = 1; // normalization factor to adjust for resolution

    /** or if the source image is to be replaced. */
    private boolean regionFlag; // true = apply algorithm to the whole image

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** difference between x,y resolutions (in plane) and z resolution (between planes). */
    private JCheckBox resolutionCheckbox;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JPanel scalePanel;

    /** DOCUMENT ME! */
    private float scaleX;

    /** DOCUMENT ME! */
    private float scaleY;

    /** DOCUMENT ME! */
    private float scaleZ;

    /** DOCUMENT ME! */
    private JTextField textGaussX;

    /** DOCUMENT ME! */
    private JTextField textGaussY;

    /** DOCUMENT ME! */
    private JTextField textGaussZ;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JRadioButton VOIRegions;

    /** DOCUMENT ME! */
    private JRadioButton wholeImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogEdgeNMSuppression() { }

    /**
     * Creates new dialog and displays it.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogEdgeNMSuppression(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, true);
        image = im;
        userInterface = ((ViewJFrameBase) (parentFrame)).getUserInterface();
        init();
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  UI  The user interface, needed to create the image frame.
     * @param  im  Source image.
     */
    public JDialogEdgeNMSuppression(ViewUserInterface UI, ModelImage im) {
        super();
        userInterface = UI;
        image = im;
        parentFrame = image.getParentFrame();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
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
        ModelImage edgeImage;
        ViewJFrameImage imageFrame = null;

        if (algorithm instanceof AlgorithmEdgeNMSuppression) {
            image.clearMask();

            if ((nmSupAlgo.isCompleted() == true) && (resultImage != null)) {

                // updateFileInfo(image, resultImage);
                // resultImage.clearMask();
                // The algorithm has completed and produced a new image to be displayed.
                try {
                    // resultImage.setImageName("EdgeNMSup");
                    // imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610,200), userInterface);

                    edgeImage = nmSupAlgo.getZeroXMask();

                    // edgeImage.setImageName("Edge");
                    imageFrame = new ViewJFrameImage(edgeImage, null, new Dimension(610, 200));

                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        }

        insertScriptLine(algorithm);

        nmSupAlgo.finalize();
        nmSupAlgo = null;
        dispose();
    }

    /**
     * focusLost - when the user clicks the mouse out of a text field, resets the neccessary variables.
     *
     * @param  event  event that triggers this function
     */
    public void focusLost(FocusEvent event) {
        Object source = event.getSource();
        JTextField field;
        String text;
        float tempNum;

        if (source == textGaussZ) {
            field = (JTextField) source;
            text = field.getText();

            if (resolutionCheckbox.isSelected()) {
                tempNum = normFactor * Float.valueOf(textGaussZ.getText()).floatValue();
                labelCorrected.setText("      Corrected scale = " + makeString(tempNum, 3));
            } else {
                labelCorrected.setText(" ");
            }
        }
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     *
     * @param  algo  the algorithm to make an entry for
     */
    public void insertScriptLine(AlgorithmBase algo) {

        if (algo.isCompleted()) {

            if (userInterface.isScriptRecording()) {

                // check to see if the match image is already in the ImgTable
                if (userInterface.getScriptDialog().getImgTableVar(image.getImageName()) == null) {

                    if (userInterface.getScriptDialog().getActiveImgTableVar(image.getImageName()) == null) {
                        userInterface.getScriptDialog().putActiveVar(image.getImageName());
                    }
                }

                userInterface.getScriptDialog().append("EdgeNMSuppression " +
                                                       userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                       " ");

                if (displayLoc == NEW) {
                    userInterface.getScriptDialog().putVar(resultImage.getImageName());
                    userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(resultImage.getImageName()) +
                                                           " " + regionFlag + " " + image25D + " " + scaleX + " " +
                                                           scaleY + " " + scaleZ);
                } else {
                    userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                           " " + regionFlag + " " + image25D + " " + scaleX + " " +
                                                           scaleY + " " + scaleZ);
                }

                userInterface.getScriptDialog().append("\n");
            }
        }
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * itemStateChanged - method to handle item events.
     *
     * @param  event  event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();
        float tempNum;

        if (source == resolutionCheckbox) {

            if (resolutionCheckbox.isSelected()) {
                tempNum = normFactor * Float.valueOf(textGaussZ.getText()).floatValue();
                labelCorrected.setText("      Corrected scale = " + makeString(tempNum, 3));
            } else {
                labelCorrected.setText(" ");
            }
        } else if (source == image25DCheckbox) {

            if (image25DCheckbox.isSelected()) {
                resolutionCheckbox.setEnabled(false); // Image is only 2D or 2.5D, thus this checkbox
                labelGaussZ.setEnabled(false); // is not relevent
                textGaussZ.setEnabled(false);
                labelCorrected.setEnabled(false);
            } else {
                resolutionCheckbox.setEnabled(true);
                labelGaussZ.setEnabled(true);
                textGaussZ.setEnabled(true);
                labelCorrected.setEnabled(true);
            }
        }
    }

    /**
     * Run this algorithm from a script.
     *
     * @param   parser  the script parser we get the state from
     *
     * @throws  IllegalArgumentException  if there is something wrong with the arguments in the script
     */
    public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException {
        String srcImageKey = null;
        String destImageKey = null;

        try {
            srcImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        ModelImage im = parser.getImage(srcImageKey);

        image = im;
        userInterface = image.getUserInterface();
        parentFrame = image.getParentFrame();

        // the result image
        try {
            destImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        if (srcImageKey.equals(destImageKey)) {
            this.setDisplayLocReplace();
        } else {
            this.setDisplayLocNew();
        }

        try {
            setRegionFlag(parser.getNextBoolean());
            setImage25D(parser.getNextBoolean());
            setScaleX(parser.getNextFloat());
            setScaleY(parser.getNextFloat());
            setScaleZ(parser.getNextFloat());
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        setSeparateThread(false);
        callAlgorithm();

        if (!srcImageKey.equals(destImageKey)) {
            parser.putVariable(destImageKey, getResultImage().getImageName());
        }
    }

    /**
     * Accessor that sets the display loc variable to new, so that a new image is created once the algorithm completes.
     */
    public void setDisplayLocNew() {
        displayLoc = NEW;
    }

    /**
     * Accessor that sets the display loc variable to replace, so the current image is replaced once the algorithm
     * completes.
     */
    public void setDisplayLocReplace() {
        displayLoc = REPLACE;
    }

    /**
     * Accessor that sets the slicing flag.
     *
     * @param  flag  <code>true</code> indicates slices should be blurred independently.
     */
    public void setImage25D(boolean flag) {
        image25D = flag;
    }

    /**
     * Accessor that sets the region flag.
     *
     * @param  flag  <code>true</code> indicates the whole image is blurred, <code>false</code> indicates a region.
     */
    public void setRegionFlag(boolean flag) {
        regionFlag = flag;
    }

    /**
     * Accessor that sets the x scale.
     *
     * @param  scale  Value to set x scale to (should be between 0.5 and 5.0).
     */
    public void setScaleX(float scale) {
        scaleX = scale;
    }

    /**
     * Accessor that sets the y scale.
     *
     * @param  scale  Value to set y scale to (should be between 0.5 and 5.0).
     */
    public void setScaleY(float scale) {
        scaleY = scale;
    }

    /**
     * Accessor that sets the z scale.
     *
     * @param  scale  Value to set z scale to (should be between 0.5 and 5.0).
     */
    public void setScaleZ(float scale) {
        scaleZ = scale;
    }

    /**
     * Once all the necessary variables are set, call the algorithm based on what type of image this is and whether or
     * not there is a separate destination image.
     */
    protected void callAlgorithm() {
        System.gc();

        String name = makeImageName(image.getImageName(), "_edgeNM");

        if (image.getNDims() == 2) { // source image is 2D

            int[] destExtents = new int[2];

            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            float[] sigmas = new float[2];

            sigmas[0] = scaleX; // set standard deviations (sigma) in X and Y
            sigmas[1] = scaleY;

            // if (displayLoc == NEW) {
            try {

                // Make result image of float type
                resultImage = new ModelImage(ModelImage.FLOAT, destExtents, " EdgeNMSup", userInterface);
                // resultImage = (ModelImage)image.clone();

                resultImage.setImageName(name);

                if ((resultImage.getFileInfo()[0]).getFileFormat() == FileBase.DICOM) {
                    ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0002,0002", "1.2.840.10008.5.1.4.1.1.7 ",
                                                                            26); // Secondary Capture SOP UID
                    ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0008,0016", "1.2.840.10008.5.1.4.1.1.7 ",
                                                                            26);
                    ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0002,0012", "1.2.840.34379.17", 16); // bogus Implementation UID made up by Matt
                    ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0002,0013", "MIPAV--NIH", 10); //
                }

                // Make algorithm
                nmSupAlgo = new AlgorithmEdgeNMSuppression(resultImage, image, sigmas, regionFlag, image25D);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                nmSupAlgo.addListener(this);

                // Hide dialog
                setVisible(false);

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (nmSupAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog EdgeNMSup: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }

                return;
            }
            // }
            /*else {
             * try{ // No need to make new image space because the user has choosen to replace the source image // Make
             * the algorithm class nmSupAlgo = new AlgorithmEdgeNMSuppression(image, sigmas, regionFlag); // This is
             * very important. Adding this object as a listener allows the algorithm to // notify this object when it
             * has completed of failed. See algorithm performed event. // This is made possible by implementing
             * AlgorithmedPerformed interface nmSupAlgo.addListener(this); // Hide the dialog since the algorithm is
             * about to run. setVisible(false); // These next lines set the titles in all frames where the source image
             * is displayed to // "locked - " image name so as to indicate that the image is now read/write locked! //
             * The image frames are disabled and then unregisted from the userinterface until the // algorithm has
             * completed. Vector imageFrames = image.getImageFrameVector(); titles = new String[imageFrames.size()]; for
             * (int i = 0; i < imageFrames.size(); i++) { titles[i] =
             * ((ViewJFrameBase)(imageFrames.elementAt(i))).getTitle();
             * ((ViewJFrameBase)(imageFrames.elementAt(i))).setTitle("Locked: " + titles[i] );
             * ((ViewJFrameBase)(imageFrames.elementAt(i))).setEnabled(false);
             * userInterface.unregisterFrame((Frame)(imageFrames.elementAt(i))); } // Start the thread as a low priority
             * because we wish to still have user interface. if (nmSupAlgo.startMethod(Thread.MIN_PRIORITY) == false){
             * MipavUtil.displayError("A thread is already running on this object"); } } catch (OutOfMemoryError x){
             * MipavUtil.displayError("Dialog EdgeNMSup: unable to allocate enough memory"); return; } }*/
        } else if (image.getNDims() == 3) {
            int[] destExtents = new int[3];

            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];

            float[] sigmas = new float[3];

            sigmas[0] = scaleX;
            sigmas[1] = scaleY;
            sigmas[2] = scaleZ;

            // if (displayLoc == NEW) {
            try {

                // Make result image of float type
                resultImage = new ModelImage(ModelImage.FLOAT, destExtents, "EdgeNMSup", userInterface);

                // resultImage = (ModelImage)image.clone();
                resultImage.setImageName(name);
                resultImage.setImageName(name);

                if ((resultImage.getFileInfo()[0]).getFileFormat() == FileBase.DICOM) {

                    for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                        ((FileInfoDicom) (resultImage.getFileInfo(i))).setValue("0002,0002",
                                                                                "1.2.840.10008.5.1.4.1.1.7 ", 26); // Secondary Capture SOP UID
                        ((FileInfoDicom) (resultImage.getFileInfo(i))).setValue("0008,0016",
                                                                                "1.2.840.10008.5.1.4.1.1.7 ", 26);
                        ((FileInfoDicom) (resultImage.getFileInfo(i))).setValue("0002,0012", "1.2.840.34379.17", 16); // bogus Implementation UID made up by Matt
                        ((FileInfoDicom) (resultImage.getFileInfo(i))).setValue("0002,0013", "MIPAV--NIH", 10); //
                    }
                }

                // Make algorithm
                nmSupAlgo = new AlgorithmEdgeNMSuppression(resultImage, image, sigmas, regionFlag, image25D);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                nmSupAlgo.addListener(this);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (nmSupAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    if (!userInterface.isAppFrameVisible()) {
                        nmSupAlgo.setProgressBarVisible(false);
                    }

                    nmSupAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog EdgeNMSup: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up image memory
                    resultImage = null;
                }

                return;
            }
            // }
            /*else {
             * try{ // Make algorithm nmSupAlgo = new AlgorithmEdgeNMSuppression(image, sigmas, regionFlag); // This is
             * very important. Adding this object as a listener allows the algorithm to // notify this object when it
             * has completed of failed. See algorithm performed event. // This is made possible by implementing
             * AlgorithmedPerformed interface nmSupAlgo.addListener(this); // Hide dialog setVisible(false); // These
             * next lines set the titles in all frames where the source image is displayed to // "locked - " image name
             * so as to indicate that the image is now read/write locked! // The image frames are disabled and then
             * unregisted from the userinterface until the // algorithm has completed. Vector imageFrames =
             * image.getImageFrameVector(); titles = new String[imageFrames.size()]; for (int i = 0; i <
             * imageFrames.size(); i++) { titles[i] = ((ViewJFrameBase)(imageFrames.elementAt(i))).getTitle();
             * ((ViewJFrameBase)(imageFrames.elementAt(i))).setTitle("Locked: " + titles[i] );
             * ((ViewJFrameBase)(imageFrames.elementAt(i))).setEnabled(false);
             * userInterface.unregisterFrame((Frame)(imageFrames.elementAt(i))); } // Start the thread as a low priority
             * because we wish to still have user interface work fast if (nmSupAlgo.startMethod(Thread.MIN_PRIORITY) ==
             * false){ MipavUtil.displayError("A thread is already running on this object"); } } catch (OutOfMemoryError
             * x){ MipavUtil.displayError("Dialog EdgeNMSup: unable to allocate enough memory"); return; } }*/
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        getContentPane().setLayout(new BorderLayout());
        setTitle("EdgeNMSuppression");

        JPanel mainPanel;

        mainPanel = new JPanel();
        mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        mainPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = gbc.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        scalePanel = new JPanel(new GridLayout(3, 2));
        scalePanel.setForeground(Color.black);
        scalePanel.setBorder(buildTitledBorder("Scale of the Gaussian"));
        mainPanel.add(scalePanel, gbc);

        labelGaussX = new JLabel("X Dimension (0.5 - 5.0) ");
        labelGaussX.setForeground(Color.black);
        labelGaussX.setFont(serif12);
        scalePanel.add(labelGaussX);

        textGaussX = new JTextField();
        textGaussX.setText("1.0");
        textGaussX.setFont(serif12);
        scalePanel.add(textGaussX);

        labelGaussY = new JLabel("Y Dimension (0.5 - 5.0) ");
        labelGaussY.setForeground(Color.black);
        labelGaussY.setFont(serif12);
        scalePanel.add(labelGaussY);

        textGaussY = new JTextField();
        textGaussY.setText("1.0");

        // textGaussY.setBounds(10, 55, 44, 30);
        textGaussY.setFont(serif12);
        scalePanel.add(textGaussY);

        labelGaussZ = new JLabel("Z Dimension (0.5 - 5.0) ");
        labelGaussZ.setForeground(Color.black);
        labelGaussZ.setFont(serif12);
        scalePanel.add(labelGaussZ);

        textGaussZ = new JTextField();
        textGaussZ.setText("1.0");
        textGaussZ.setFont(serif12);
        scalePanel.add(textGaussZ);

        JPanel resPanel = new JPanel(new BorderLayout());

        resPanel.setBorder(buildTitledBorder("Resolution options"));
        resolutionCheckbox = new JCheckBox("Use image resolutions to normalize Z scale.");
        resolutionCheckbox.setFont(serif12);
        resPanel.add(resolutionCheckbox, BorderLayout.NORTH);
        resolutionCheckbox.setSelected(true);

        image25DCheckbox = new JCheckBox("Process each slice independently (2.5D).");
        image25DCheckbox.setFont(serif12);
        resPanel.add(image25DCheckbox, BorderLayout.SOUTH);
        image25DCheckbox.setSelected(false);
        image25DCheckbox.addItemListener(this);

        if (image.getNDims() == 3) { // if the source image is 3D then allow
            resolutionCheckbox.setEnabled(true); // the user to indicate if it wishes to
            resolutionCheckbox.addItemListener(this); // use the correction factor
            textGaussZ.addFocusListener(this);
            textGaussZ.setEnabled(true);
        } else {
            resolutionCheckbox.setEnabled(false); // Image is only 2D, thus this checkbox
            labelGaussZ.setEnabled(false); // is not relevent
            textGaussZ.setEnabled(false);
            image25DCheckbox.setEnabled(false);
        }

        if (image.getNDims() == 3) { // Source image is 3D, thus show correction factor

            int index = image.getExtents()[2] / 2;
            float xRes = image.getFileInfo(index).getResolutions()[0];
            float zRes = image.getFileInfo(index).getResolutions()[2];

            normFactor = xRes / zRes; // Calculate correction factor
            labelCorrected = new JLabel("      Corrected scale = " +
                                        String.valueOf(normFactor * Float.valueOf(textGaussZ.getText()).floatValue()));
            labelCorrected.setForeground(Color.black);
            labelCorrected.setFont(serif12);
            resPanel.add(labelCorrected, BorderLayout.CENTER);
        }

        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(resPanel, gbc);

        JPanel outputOptPanel = new JPanel(new GridLayout(1, 1));

        /*       destinationPanel = new JPanel(new BorderLayout());
         * destinationPanel.setForeground(Color.black); destinationPanel.setBorder(buildTitledBorder("Destination"));
         * outputOptPanel.add(destinationPanel); destinationGroup = new ButtonGroup(); newImage = new JRadioButton("New
         * image", true); newImage.setFont(serif12); destinationGroup.add(newImage); destinationPanel.add(newImage,
         * BorderLayout.NORTH); replaceImage = new JRadioButton("Replace image",false); replaceImage.setFont(serif12);
         * destinationGroup.add(replaceImage); destinationPanel.add(replaceImage, BorderLayout.CENTER); // Only if the
         * image is unlocked can it be replaced. if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
         * replaceImage.setEnabled(true); } else { replaceImage.setEnabled(false); }
         */
        imageVOIPanel = new JPanel();
        imageVOIPanel.setLayout(new BorderLayout());
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Process"));
        outputOptPanel.add(imageVOIPanel);

        imageVOIGroup = new ButtonGroup();
        wholeImage = new JRadioButton("Whole image", true);
        wholeImage.setFont(serif12);
        imageVOIGroup.add(wholeImage);
        imageVOIPanel.add(wholeImage, BorderLayout.NORTH);

        VOIRegions = new JRadioButton("VOI region(s)", false);
        VOIRegions.setFont(serif12);
        imageVOIGroup.add(VOIRegions);
        imageVOIPanel.add(VOIRegions, BorderLayout.CENTER);
        gbc.gridx = 0;
        gbc.gridy = 2;
        mainPanel.add(outputOptPanel, gbc);

        JPanel buttonPanel = new JPanel();

        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setResizable(true);
        setVisible(true);

        System.gc();
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        // if (replaceImage.isSelected())    displayLoc = REPLACE;
        // else if (newImage.isSelected())   displayLoc = NEW;

        if (wholeImage.isSelected()) {
            regionFlag = true;
        } else if (VOIRegions.isSelected()) {
            regionFlag = false;
        }

        if (image25DCheckbox.isSelected()) {
            image25D = true;
        }

        tmpStr = textGaussX.getText();

        if (testParameter(tmpStr, 0.5, 5.0)) {
            scaleX = Float.valueOf(tmpStr).floatValue();
        } else {
            textGaussX.requestFocus();
            textGaussX.selectAll();

            return false;
        }

        tmpStr = textGaussY.getText();

        if (testParameter(tmpStr, 0.5, 5.0)) {
            scaleY = Float.valueOf(tmpStr).floatValue();
        } else {
            textGaussY.requestFocus();
            textGaussY.selectAll();

            return false;
        }

        tmpStr = textGaussZ.getText();

        if (testParameter(tmpStr, 0.5, 5.0)) {
            scaleZ = Float.valueOf(tmpStr).floatValue();
        } else {
            textGaussZ.requestFocus();
            textGaussZ.selectAll();

            return false;
        }

        if (resolutionCheckbox.isSelected()) {
            scaleZ = scaleZ * normFactor;
        }

        return true;
    }

}
