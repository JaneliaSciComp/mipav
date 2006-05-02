package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image. It should be noted that the algorithms are executed in their own thread.
 *
 * @version  0.1 September 12, 2005
 * @author   William Gandler
 * @see      AlgorithmNonparametricSegmentation
 */
public class JDialogNonparametricSegmentation extends JDialogBase
        implements AlgorithmInterface, ScriptableInterface, DialogDefaultsInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4783902509527192384L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private ModelImage featureImage = null;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private JPanel paramPanel;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private AlgorithmNonparametricSegmentation segAlgo;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogNonparametricSegmentation() { }

    // or if the source image is to be replaced

    /**
     * Creates a new JDialogNonparametricSegmentation object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogNonparametricSegmentation(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ((ViewJFrameBase) (parentFrame)).getUserInterface();
        init();
        loadDefaults();
        setVisible(true);
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  UI  The user interface, needed to create the image frame.
     * @param  im  Source image.
     */
    public JDialogNonparametricSegmentation(ViewUserInterface UI, ModelImage im) {
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
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            // MipavUtil.showHelp( "" );
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

        ViewJFrameImage imageFrame = null;

        if (algorithm instanceof AlgorithmNonparametricSegmentation) {
            image.clearMask();

            if ((segAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);

                try {

                    // resultImage.setImageName("Unsharp mask");
                    imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }

                // featureImage  = ((AlgorithmNonparametricSegmentation)algorithm).getFeatureImage();
                // new ViewJFrameImage(featureImage,null, new Dimension(610, 220));
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                image.notifyImageDisplayListeners(null, true);
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        }
        // Update frame
        // ((ViewJFrameBase)parentFrame).updateImages(true);

        insertScriptLine(algorithm);

        segAlgo.finalize();
        segAlgo = null;
        dispose();
    }

    /**
     * Construct a delimited string that contains the parameters to this algorithm.
     *
     * @param   delim  the parameter delimiter (defaults to " " if empty)
     *
     * @return  the parameter string
     */
    public String getParameterString(String delim) {

        if (delim.equals("")) {
            delim = " ";
        }

        String str = new String();

        return str;
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

                userInterface.getScriptDialog().append("NonparametricSegmentation " +
                                                       userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                       " ");

                if (displayLoc == NEW) {
                    userInterface.getScriptDialog().putVar(resultImage.getImageName());
                    userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(resultImage.getImageName()) +
                                                           "\n");
                } else {
                    userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                           "\n");

                }
            }
        }
    }


    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if (defaultsString != null) {

            try {
                System.err.println(defaultsString);

                StringTokenizer st = new StringTokenizer(defaultsString, ",");

                if (MipavUtil.getBoolean(st)) {
                    newImage.setSelected(true);
                } else {
                    replaceImage.setSelected(true);
                }
            } catch (Exception ex) {

                // since there was a problem parsing the defaults string, start over with the original defaults
                System.out.println("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
            }
        }
    }


    /**
     * Saves the default settings into the Preferences file.
     */
    public void saveDefaults() {
        String defaultsString = new String(getParameterString(",") + "," + newImage.isSelected());

        // System.err.println(defaultsString);
        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }

    /**
     * Run this algorithm from a script.
     *
     * @param   parser  the script parser we get the state from
     *
     * @throws  IllegalArgumentException  if there is something wrong with the arguments in the script
     */
    public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException {
        setScriptRunning(true);

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

        try { }
        catch (Exception e) {
            throw new IllegalArgumentException();
        }

        setActiveImage(parser.isActiveImage());
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
     * Once all the necessary variables are set, call the Entropy Minimization algorithm based on what type of image
     * this is and whether or not there is a separate destination image.
     */
    private void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_nonParametricSeg");

        if (displayLoc == NEW) {

            try {

                // Make result image of float type
                // resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name, userInterface);
                if (image.isColorImage()) {
                    resultImage = new ModelImage(ModelImage.SHORT, image.getExtents(), name, userInterface);
                } else {
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);
                    resultImage.resetVOIs();
                }

                /*if ((resultImage.getFileInfo()[0]).getFileFormat() == FileBase.DICOM){
                 *  ((FileInfoDicom)(resultImage.getFileInfo(0))).setValue("0002,0002", "1.2.840.10008.5.1.4.1.1.7 ",
                 * 26); // Secondary Capture SOP UID ((FileInfoDicom)(resultImage.getFileInfo(0))).setValue("0008,0016",
                 * "1.2.840.10008.5.1.4.1.1.7 ", 26);
                 * ((FileInfoDicom)(resultImage.getFileInfo(0))).setValue("0002,0012", "1.2.840.34379.17", 16); // bogus
                 * Implementation UID made up by Matt
                 * ((FileInfoDicom)(resultImage.getFileInfo(0))).setValue("0002,0013", "MIPAV--NIH", 10); //
                 *  } */
                // Make algorithm
                segAlgo = new AlgorithmNonparametricSegmentation(resultImage, image);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                segAlgo.addListener(this);

                // Hide dialog
                setVisible(false);

                if (runInSeparateThread) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (segAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    segAlgo.setActiveImage(isActiveImage);

                    if (!userInterface.isAppFrameVisible()) {
                        segAlgo.setProgressBarVisible(false);
                    }

                    segAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Nonparametric Segmentation: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }

                return;
            }
        } else {

            try {

                // No need to make new image space because the user has choosen to replace the source image
                // Make the algorithm class
                segAlgo = new AlgorithmNonparametricSegmentation(image);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                segAlgo.addListener(this);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector imageFrames = image.getImageFrameVector();
                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                    ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }

                if (runInSeparateThread) {

                    // Start the thread as a low priority because we wish to still have user interface.
                    if (segAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    segAlgo.setActiveImage(isActiveImage);

                    if (!userInterface.isAppFrameVisible()) {
                        segAlgo.setProgressBarVisible(false);
                    }

                    segAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Nonparametric Segemntation: unable to allocate enough memory");

                return;
            }
        }

    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Non-parametricSegmentation");
        getContentPane().setLayout(new BorderLayout());

        JPanel mainPanel;

        mainPanel = new JPanel();
        mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        mainPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridwidth = 6;
        gbc.gridheight = 1;
        gbc.anchor = gbc.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        JLabel labelPoint = new JLabel("Place one point over each segmented class ");
        labelPoint.setForeground(Color.black);
        labelPoint.setFont(serif12);
        paramPanel.add(labelPoint, gbc);

        destinationPanel = new JPanel(new BorderLayout());
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage, BorderLayout.NORTH);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage, BorderLayout.CENTER);

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add(paramPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(destinationPanel, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);

        // setVisible( true );

        System.gc();
    }


    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************


    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        return true;
    }


}
