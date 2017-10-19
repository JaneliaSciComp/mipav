package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call AlgorithmEvaluateSegmentation. Selected image is test image, the image that is
 * compared to a gold standard true image. Algorithms are executed in their own thread.
 */
public class JDialogEvaluateSegmentation extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6446476110592386369L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Combo box with image names for choosing test image. */
    private JComboBox comboBoxImage;

    /** Algorithm to run from this dialog. */
    private AlgorithmEvaluateSegmentation evalSeg = null;

    /** DOCUMENT ME! */
    private int imagesFound = 0;

    /** DOCUMENT ME! */
    private int nVOIs;

    /** DOCUMENT ME! */
    private ModelImage testImage;

    /** DOCUMENT ME! */
    private ViewVOIVector testVOIs;

    /** Used to lock and unlock images. */
    private String[] titles;

    /** DOCUMENT ME! */
    private int trueBoundingVOIs = 0;

    /** DOCUMENT ME! */
    private int[] trueID = new int[20];

    /** DOCUMENT ME! */
    private ModelImage trueImage;

    /** DOCUMENT ME! */
    private ViewVOIVector trueVOIs;

    /** Pointer to GUI. */
    private ViewUserInterface UI;


    /** Reference to userface. */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogEvaluateSegmentation() { }

    /**
     * Creates new evaluate segmentation dialog to get test image name.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogEvaluateSegmentation(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        trueImage = im;
        userInterface = ((ViewJFrameBase) (parentFrame)).getUserInterface();
        trueVOIs = trueImage.getVOIs();
        nVOIs = trueVOIs.size();

        for (int i = 0; i < nVOIs; i++) {

            if ((trueVOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) ||
                    (trueVOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                trueID[trueBoundingVOIs++] = trueVOIs.VOIAt(i).getID();
            }
        }

        if (trueBoundingVOIs == 0) {
            MipavUtil.displayError("True image must have at least 1 contour or polyline VOI");

            return;
        }

        comboBoxImage = buildImageEvalComboBox(trueImage);

        if (imagesFound == 0) {
            MipavUtil.displayError("Must have a matching test image with equal dimensions and VOI number");

            return;
        }

        init();
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  _UI  The user interface, needed to create the image frame.
     * @param  im   Source image.
     */
    public JDialogEvaluateSegmentation(ViewUserInterface _UI, ModelImage im) {
        super();
        userInterface = _UI;
        trueImage = im;
        parentFrame = im.getParentFrame();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, set variables, and calls the algorithm.
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
        } else if (command.equals("Help")) {
            // MipavUtil.showHelp("");
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        ViewJFrameImage imageFrame = null;

        if (algorithm instanceof AlgorithmEvaluateSegmentation) {

            // These next lines set the titles in all frames where the source image is displayed to
            // image name so as to indicate that the image is now unlocked!
            // The image frames are enabled and then registed to the userinterface.
            Vector imageFrames = trueImage.getImageFrameVector();

            for (int i = 0; i < imageFrames.size(); i++) {
                ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                    userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                }
            }

            insertScriptLine(algorithm);

            if (parentFrame != null) {
                userInterface.registerFrame(parentFrame);
            }

            dispose();
        }
    }

    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     *
     * @param  algo  the algorithm to make an entry for
     */
    public void insertScriptLine(AlgorithmBase algo) {

        if (algo.isCompleted()) {

            if (userInterface.isScriptRecording()) {

                // check to see if the true image is already in the ImgTable
                if (userInterface.getScriptDialog().getImgTableVar(trueImage.getImageName()) == null) {

                    if (userInterface.getScriptDialog().getActiveImgTableVar(trueImage.getImageName()) == null) {
                        userInterface.getScriptDialog().putActiveVar(trueImage.getImageName());
                    }
                }

                // check to see if the test image is already in the ImgTable
                if (userInterface.getScriptDialog().getImgTableVar(testImage.getImageName()) == null) {

                    if (userInterface.getScriptDialog().getActiveImgTableVar(testImage.getImageName()) == null) {
                        userInterface.getScriptDialog().putActiveVar(testImage.getImageName());
                    }
                }

                userInterface.getScriptDialog().append("EvaluateSegmentation " +
                                                       userInterface.getScriptDialog().getVar(trueImage.getImageName()) +
                                                       " " +
                                                       userInterface.getScriptDialog().getVar(testImage.getImageName()) +
                                                       "\n");
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
        String trueImageKey = null;
        String testImageKey = null;

        try {
            trueImageKey = parser.getNextString();
            testImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        ModelImage trueIm = parser.getImage(trueImageKey);
        ModelImage testIm = parser.getImage(testImageKey);

        trueImage = trueIm;
        setTestImage(testIm);
        userInterface = trueImage.getUserInterface();
        parentFrame = trueImage.getParentFrame();

        setSeparateThread(false);
        callAlgorithm();
    }

    /**
     * Accessor to set the reference image.
     *
     * @param  im  Reference image.
     */
    public void setTestImage(ModelImage im) {
        testImage = im;
    }

    /**
     * Constructs a string indicating if the algorithm completed sucessfully.
     */
    protected void closingLog() {
        String logString;

        if (evalSeg.isCompleted() == true) {
            logString = new String("EvaluateSegmentation " + testImage.getImageName() + " to " +
                                   trueImage.getImageName() + " Completed successfully!" + "\n");
        } else {
            logString = new String("EvaluateSegmentation " + testImage.getImageName() + " to " +
                                   trueImage.getImageName() + " Algorithm failed!" + "\n");
        }
        //        Preferences.log(trueImage.getUserInterface(), logString);
    }

    /**
     * Builds a list of images. Returns combobox.
     *
     * @param   image  DOCUMENT ME!
     *
     * @return  Newly created combo box.
     */
    private JComboBox buildImageEvalComboBox(ModelImage image) {
        ViewUserInterface UI;
        ModelImage img;
        boolean dimMatch;
        int voiIDsMatch;
        int i, j;
        int[] testID = new int[20];
        int testBoundingVOIs = 0;

        JComboBox comboBox = new JComboBox();
        comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);

        UI = image.getUserInterface();

        Enumeration names = UI.getRegisteredImageNames();

        while (names.hasMoreElements()) {
            String name = (String) names.nextElement();

            try {
                img = UI.getRegisteredImageByName(name);

                if (UI.getFrameContainingImage(img) != null) {

                    if (!name.equals(image.getImageName())) {

                        if (img.getNDims() == image.getNDims()) {
                            dimMatch = true;

                            for (i = 0; i < image.getNDims(); i++) {

                                if (image.getExtents()[i] != img.getExtents()[i]) {
                                    dimMatch = false;
                                }
                            }

                            if (dimMatch) {
                                testVOIs = null;
                                testVOIs = img.getVOIs();
                                nVOIs = testVOIs.size();

                                for (i = 0; i < nVOIs; i++) {

                                    if ((testVOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) ||
                                            (testVOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                                        testID[testBoundingVOIs++] = testVOIs.VOIAt(i).getID();
                                    }
                                }

                                if (trueBoundingVOIs == testBoundingVOIs) {
                                    voiIDsMatch = 0;

                                    for (i = 0; i < trueBoundingVOIs; i++) {

                                        for (j = 0; j < trueBoundingVOIs; j++) {

                                            if (trueID[i] == testID[j]) {
                                                voiIDsMatch++;
                                            }
                                        }
                                    }

                                    if (trueBoundingVOIs == voiIDsMatch) {
                                        comboBox.addItem(name);
                                        imagesFound++;
                                    }
                                }
                            }
                        }
                    }
                }
            } catch (IllegalArgumentException iae) {

                // MipavUtil.displayError("There was a problem with the supplied name.\n" );
                Preferences.debug("Illegal Argument Exception in " +
                                  "JDialogEvaluateSegmentation.buildImageEvalComboBox(). " +
                                  "Somehow the Image list sent an incorrect name to " + "the image image hashtable. " +
                                  "\n", 1);
                System.out.println("Bad argument.");
            }
        }

        return comboBox;
    }

    /**
     * Sets arrays appropriately and calls registration algorithm, running it in it's own thread.
     */
    private void callAlgorithm() {

        try {
            evalSeg = new AlgorithmEvaluateSegmentation(trueImage, testImage);

        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Evaluate Segmentation: unable to allocate enough memory");

            return;
        }

        // This is very important. Adding this object as a listener allows
        // the algorithm to notify this object when it has completed of failed.
        // See algorithm performed event. This is made possible by implementing
        evalSeg.addListener(this);

        // These next lines set the titles in all frames where the source image
        // is displayed to "locked - " image name so as to indicate that the image
        // is now read/write locked!  The image frames are disabled and then
        // unregisted from the userinterface until the algorithm has completed.
        Vector imageFrames = trueImage.getImageFrameVector();
        titles = new String[imageFrames.size()];

        for (int i = 0; i < imageFrames.size(); i++) {
            titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
            ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
            ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
            userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
        }

        // Start the thread as a low priority because we wish to still have
        // user interface work fast
        // if (evalSeg.startMethod(Thread.MIN_PRIORITY) == false){
        // MipavUtil.displayError("A thread is already running on this object", "Error");
        if (evalSeg.startMethod(Thread.MIN_PRIORITY) == false) {
            MipavUtil.displayError("A thread is already running on this object");
        }
    }

    /**
     * Constructs a string of the construction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
        String logString = new String("EvaluateSegmentation " + testImage.getImageName() + " to " +
                                      trueImage.getImageName() + "\n");
        // Preferences.log(trueImage.getUserInterface(), logString);
    }

    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Evaluate segmentation");

        String trueName = trueImage.getImageName();

        JLabel labelTrue = new JLabel("True image: " + trueName + " ");
        labelTrue.setForeground(Color.black);
        labelTrue.setFont(serif12);

        JLabel labelTest = new JLabel("Test image: ");
        labelTest.setForeground(Color.black);
        labelTest.setFont(serif12);

        JPanel imagePanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = gbc.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        imagePanel.add(labelTrue, gbc);
        gbc.gridy = 1;
        imagePanel.add(labelTest, gbc);
        gbc.gridx = 1;
        imagePanel.add(comboBoxImage, gbc);

        getContentPane().add(imagePanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * Sets the variables needed for calling the algorithm.
     *
     * @return  <code>true</code> if successful in setting variables.
     */
    private boolean setVariables() {

        // assign testImage to image selected in comboBox
        ViewUserInterface UI = trueImage.getUserInterface();
        String selectedName = (String) comboBoxImage.getSelectedItem();

        testImage = UI.getRegisteredImageByName(selectedName);


        constructLog();

        return true;
    }

}