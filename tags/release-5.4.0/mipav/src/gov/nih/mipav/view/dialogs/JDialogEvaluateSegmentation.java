package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterException;
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
public class JDialogEvaluateSegmentation extends JDialogScriptableBase implements AlgorithmInterface {

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
    private ModelImage testImage;

    /** Used to lock and unlock images. */
    private String[] titles;

    /** DOCUMENT ME! */
    private int trueBoundingVOIs = 0;

    /** DOCUMENT ME! */
    private int[] trueID = new int[20];

    /** DOCUMENT ME! */
    private ModelImage trueImage;

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
        userInterface = ViewUserInterface.getReference();
        
        ViewVOIVector trueVOIs = trueImage.getVOIs();
        int nVOIs = trueVOIs.size();
        for (int i = 0; i < nVOIs; i++) {

            if ((trueVOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (trueVOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
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

    //~ Methods --------------------------------------------------------------------------------------------------------
    
    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(trueImage);
        scriptParameters.storeImage(trueImage, "test_image");
    }
    
    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        trueImage = scriptParameters.retrieveInputImage();
        
        userInterface = ViewUserInterface.getReference();
        
        testImage = scriptParameters.retrieveImage("test_image");

        ViewVOIVector trueVOIs = trueImage.getVOIs();
        int nVOIs = trueVOIs.size();
        for (int i = 0; i < nVOIs; i++) {

            if ((trueVOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (trueVOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                trueID[trueBoundingVOIs++] = trueVOIs.VOIAt(i).getID();
            }
        }

        if (trueBoundingVOIs == 0) {
            throw new ParameterException("test_image", "True image must have at least 1 contour or polyline VOI");
        }
        
        // just make sure the image dims match, not the voi testing done in a run from the dialog.  TODO: the full testing may be added later
        if (trueImage.getNDims() != testImage.getNDims()) {
            throw new ParameterException("test_image", "The true image and test image have different dimensionalities.");
        } else {
            for (int i = 0; i < trueImage.getNDims(); i++) {
                if (trueImage.getExtents()[i] != testImage.getExtents()[i]) {
                    throw new ParameterException("test_image", "The true image and test image extents do not match.");
                }
            }
        }
    }
    
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
            MipavUtil.showHelp("Mor008ES");
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
        if (algorithm instanceof AlgorithmEvaluateSegmentation) {

            // These next lines set the titles in all frames where the source image is displayed to
            // image name so as to indicate that the image is now unlocked!
            // The image frames are enabled and then registed to the userinterface.
            Vector<ViewImageUpdateInterface> imageFrames = trueImage.getImageFrameVector();

            for (int i = 0; i < imageFrames.size(); i++) {
                ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                    userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                }
            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }

            if (parentFrame != null) {
                userInterface.registerFrame(parentFrame);
            }

            dispose();
        }
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

        UI = ViewUserInterface.getReference();

        Enumeration<String> names = UI.getRegisteredImageNames();

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
                                ViewVOIVector testVOIs = img.getVOIs();
                                int nVOIs = testVOIs.size();
                                for (i = 0; i < nVOIs; i++) {
                                    if ((testVOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (testVOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
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
    protected void callAlgorithm() {

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

        createProgressBar(testImage.getImageName(), evalSeg);
        
        // These next lines set the titles in all frames where the source image
        // is displayed to "locked - " image name so as to indicate that the image
        // is now read/write locked!  The image frames are disabled and then
        // unregisted from the userinterface until the algorithm has completed.
        Vector<ViewImageUpdateInterface> imageFrames = trueImage.getImageFrameVector();
        titles = new String[imageFrames.size()];

        for (int i = 0; i < imageFrames.size(); i++) {
            titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
            ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
            ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
            userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
        }

        if(isRunInSeparateThread()) {
	        // Start the thread as a low priority because we wish to still have user interface work fast
	        if (evalSeg.startMethod(Thread.MIN_PRIORITY) == false) {
	            MipavUtil.displayError("A thread is already running on this object");
	        }
        } else {
        	evalSeg.run();
        }
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
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
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
        String selectedName = (String) comboBoxImage.getSelectedItem();

        testImage = userInterface.getRegisteredImageByName(selectedName);

        return true;
    }
}
