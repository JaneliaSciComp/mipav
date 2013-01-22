package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user is able to control the degree of blurring in all
 * dimensions and indicate if a correction factor be applied to the z-dimension to account for differing resolutions
 * between the xy resolutions (intra-plane) and the z resolution (inter-plane). The user has the option to generate a
 * new image or replace the source image. In addition the user can indicate if you wishes to have the algorithm applied
 * to whole image or to the VOI regions. In should be noted, that the algorithms are executed in their own thread.
 *
 * @version  0.1 Nov 17, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      AlgorithmMorphology2D
 */
public class JDialogIDObjects extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6478138973143988561L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmMorphology2D idObjectsAlgo2D = null;

    /** DOCUMENT ME! */
    private AlgorithmMorphology3D idObjectsAlgo3D = null;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JLabel labelMax;

    /** DOCUMENT ME! */
    private JLabel labelMin;

    /** DOCUMENT ME! */
    private int max, min;

    /** DOCUMENT ME! */
    private JPanelAlgorithmOutputOptions outputPanel;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JTextField textMax;

    /** DOCUMENT ME! */
    private JTextField textMin;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogIDObjects() { }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogIDObjects(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        if ((im.getType() != ModelImage.BOOLEAN) && (im.getType() != ModelImage.UBYTE) &&
                (im.getType() != ModelImage.USHORT)) {
            MipavUtil.displayError("Source Image must be BOOLEAN, UNSIGNED BYTE or UNSIGNED SHORT");
            dispose();

            return;
        }

        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
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
        	//MipavUtil.showHelp("Mor011IDO");
        	MipavUtil.showWebHelp("Morphology#ID_objects");
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

        if (algorithm instanceof AlgorithmMorphology2D) {
            image.clearMask();

            if ((idObjectsAlgo2D.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName("ID image");
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (userInterface != null) {
                    userInterface.registerFrame(parentFrame);
                }

                image.notifyImageDisplayListeners(null, true);
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        } else if (algorithm instanceof AlgorithmMorphology3D) {
            image.clearMask();

            if ((idObjectsAlgo3D.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName("ID image");
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);

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

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        if (idObjectsAlgo2D != null) {
            idObjectsAlgo2D.finalize();
            idObjectsAlgo2D = null;
        }

        if (idObjectsAlgo3D != null) {
            idObjectsAlgo3D.finalize();
            idObjectsAlgo3D = null;
        }

        dispose();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  the result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Once all the necessary variables are set, call the IDObjects algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        int kernel = 0;
        String name = makeImageName(image.getImageName(), "_IDObjects");

        // identify objects
        if (image.getNDims() == 2) { // source image is 2D

            int[] destExtents = new int[2];
            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            if (outputPanel.isOutputNewImageSet()) {

                try {

                    // Make result image of float type
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    // Make algorithm
                    idObjectsAlgo2D = new AlgorithmMorphology2D(resultImage, kernel, 0,
                                                                AlgorithmMorphology2D.ID_OBJECTS, 0, 0, 0, 0,
                                                                outputPanel.isProcessWholeImageSet());
                    idObjectsAlgo2D.setMinMax(min, max);

                    if (!outputPanel.isProcessWholeImageSet()) {
                        idObjectsAlgo2D.setMask(image.generateVOIMask());
                    }

                    idObjectsAlgo2D.addListener(this);

                    createProgressBar(image.getImageName(), idObjectsAlgo2D);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (idObjectsAlgo2D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        idObjectsAlgo2D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog ID objects: unable to allocate enough memory");

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
                    idObjectsAlgo2D = new AlgorithmMorphology2D(image, kernel, 0, AlgorithmMorphology2D.ID_OBJECTS, 0,
                                                                0, 0, 0, outputPanel.isProcessWholeImageSet());
                    idObjectsAlgo2D.setMinMax(min, max);

                    if (!outputPanel.isProcessWholeImageSet()) {
                        idObjectsAlgo2D.setMask(image.generateVOIMask());
                    }

                    idObjectsAlgo2D.addListener(this);

                    createProgressBar(image.getImageName(), idObjectsAlgo2D);

                    // Hide the dialog since the algorithm is about to run.
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((ViewJFrameBase) (imageFrames.elementAt(i))).getTitle();
                        ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface.
                        if (idObjectsAlgo2D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        idObjectsAlgo2D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog ID objects: unable to allocate enough memory");

                    return;
                }
            }
        } else if (image.getNDims() == 3) {
            int[] destExtents = new int[3];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];

            if (outputPanel.isOutputNewImageSet()) {

                try {

                    // Make result image of float type
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    // Make algorithm
                    idObjectsAlgo3D = new AlgorithmMorphology3D(resultImage, kernel, 0,
                                                                AlgorithmMorphology3D.ID_OBJECTS, 0, 0, 0, 0,
                                                                outputPanel.isProcessWholeImageSet());
                    idObjectsAlgo3D.setMinMax(min, max);

                    if (!outputPanel.isProcessWholeImageSet()) {
                        idObjectsAlgo3D.setMask(image.generateVOIMask());
                        // This is very important. Adding this object as a listener allows the algorithm to notify this
                        // object when it has completed of failed. See algorithm performed event. This is made possible
                        // by implementing AlgorithmedPerformed interface
                    }

                    idObjectsAlgo3D.addListener(this);

                    createProgressBar(image.getImageName(), idObjectsAlgo3D);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (idObjectsAlgo3D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        idObjectsAlgo3D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog ID objects: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {

                    // Make algorithm
                    idObjectsAlgo3D = new AlgorithmMorphology3D(image, kernel, 0, AlgorithmMorphology3D.ID_OBJECTS, 0,
                                                                0, 0, 0, outputPanel.isProcessWholeImageSet());
                    idObjectsAlgo3D.setMinMax(min, max);

                    if (!outputPanel.isProcessWholeImageSet()) {
                        idObjectsAlgo3D.setMask(image.generateVOIMask());
                        // This is very important. Adding this object as a listener allows the algorithm to notify this
                        // object when it has completed of failed. See algorithm performed event. This is made possible
                        // by implementing AlgorithmedPerformed interface
                    }

                    idObjectsAlgo3D.addListener(this);

                    createProgressBar(image.getImageName(), idObjectsAlgo3D);

                    // Hide dialog
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((ViewJFrameBase) (imageFrames.elementAt(i))).getTitle();
                        ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (idObjectsAlgo3D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        idObjectsAlgo3D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog ID objects: unable to allocate enough memory");

                    return;
                }
            }
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        if (outputPanel.isOutputNewImageSet()) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        if ((image.getType() != ModelImage.BOOLEAN) && (image.getType() != ModelImage.UBYTE) &&
                (image.getType() != ModelImage.USHORT)) {
            MipavUtil.displayError("Source Image must be Boolean or UByte or UShort");
            dispose();

            return;
        }

        outputPanel = new JPanelAlgorithmOutputOptions(image);
        scriptParameters.setOutputOptionsGUI(outputPanel);

        int[] objectRange = scriptParameters.getParams().getList("object_size_range").getAsIntArray();
        min = objectRange[0];
        max = objectRange[1];
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), outputPanel.isOutputNewImageSet());
        scriptParameters.storeProcessWholeImage(outputPanel.isProcessWholeImageSet());
        scriptParameters.getParams().put(ParameterFactory.newParameter("object_size_range", new int[] { min, max }));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Identify objects");

        outputPanel = new JPanelAlgorithmOutputOptions(image);

        JPanel mainPanel = new JPanel(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.insets = new Insets(5, 5, 5, 5);
        mainPanel.add(outputPanel, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        JPanel maskPanel = new JPanel(new GridBagLayout());
        maskPanel.setForeground(Color.black);
        maskPanel.setBorder(buildTitledBorder("Delete particles outside range"));

        labelMax = new JLabel("Maximum size");
        labelMax.setForeground(Color.black);
        labelMax.setFont(serif12);

        textMax = new JTextField(5);
        textMax.setText("5000");
        textMax.setFont(serif12);

        labelMin = new JLabel("Minimum size");
        labelMin.setForeground(Color.black);
        labelMin.setFont(serif12);

        textMin = new JTextField(5);
        textMin.setText("1");
        textMin.setFont(serif12);

        gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        maskPanel.add(labelMax, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        maskPanel.add(textMax, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        maskPanel.add(labelMin, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        maskPanel.add(textMin, gbc);

        JPanel controlPanel = new JPanel();
        controlPanel.setLayout(new BoxLayout(controlPanel, BoxLayout.Y_AXIS));
        controlPanel.add(mainPanel);
        controlPanel.add(maskPanel);
        controlPanel.add(buttonPanel);
        getContentPane().add(controlPanel);
        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        System.gc();

        String tmpStr;

        tmpStr = textMax.getText();

        if (testParameter(tmpStr, 1, 1000000000)) {
            max = Integer.valueOf(tmpStr).intValue();
        } else {
            textMax.requestFocus();
            textMax.selectAll();

            return false;
        }

        tmpStr = textMin.getText();

        if (testParameter(tmpStr, 1, 1000000000)) {
            min = Integer.valueOf(tmpStr).intValue();
        } else {
            textMin.requestFocus();
            textMin.selectAll();

            return false;
        }

        return true;
    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms.Morphology");
            }

            public String getDescription() {
                return new String("Performs ID Objects algorithm");
            }

            public String getDescriptionLong() {
                return new String("Performs ID Objects algorithm");
            }

            public String getShortLabel() {
                return new String("IDObjects");
            }

            public String getLabel() {
                return new String("IDObjects");
            }

            public String getName() {
                return new String("IDObjects");
            }
        };
    }

    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
    public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();
        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));

			table.put(new ParameterFloat("min", 1.0f));
			table.put(new ParameterList("object_size_range", Parameter.PARAM_INT));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }
        
        return table;
    }

    /**
     * Returns a table listing the output parameters of this algorithm (usually just labels used to obtain output image
     * names later).
     * 
     * @return A parameter table listing the outputs of this algorithm.
     */
    public ParameterTable createOutputParameters() {
    	final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    /**
     * Returns the name of an image output by this algorithm, the image returned depends on the parameter label given
     * (which can be used to retrieve the image object from the image registry).
     * 
     * @param imageParamName The output image parameter label for which to get the image name.
     * @return The image name of the requested output image parameter label.
     */
    public String getOutputImageName(final String imageParamName) {
        if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE)) {
            return resultImage.getImageName();
        }

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;
    }

    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
    }

}
