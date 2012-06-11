package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to call AlgorithmAutoCorrelation.
 */
public class JDialogAutoCorrelation extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface  {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5909303016264063087L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmAutoCorrelation algoAutoCorrelation = null;

    /** DOCUMENT ME! */
    private boolean haveBlue = false;

    /** DOCUMENT ME! */
    private boolean haveGreen = false;

    /** DOCUMENT ME! */
    private boolean haveRed = false;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private double maxR, maxG, maxB;

    /** DOCUMENT ME! */
    private double minR, minG, minB;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private ModelImage resultImageB = null;

    /** DOCUMENT ME! */
    private ModelImage resultImageG = null;

    /** DOCUMENT ME! */
    private ModelImage resultImageR = null;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogAutoCorrelation() { }

    /**
     * Constructs new transform dialog and sets up GUI components.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogAutoCorrelation(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        UI = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, sets the variables, and calls the algorithm.
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
            MipavUtil.showHelp("Autocor001");
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
        int verticalPosition = 200;

        if (algorithm instanceof AlgorithmAutoCorrelation) {
            image.clearMask();

            if (image.isColorImage()) {

                if ((algoAutoCorrelation.isCompleted() == true) &&
                        ((resultImageR != null) || (resultImageG != null) || (resultImageB != null))) {
                    // The algorithm has completed and produced a new image to be displayed.

                    if (resultImageR != null) {
                        updateFileInfo(image, resultImageR);
                        resultImageR.clearMask();
                    }

                    if (resultImageG != null) {
                        updateFileInfo(image, resultImageG);
                        resultImageG.clearMask();
                    }

                    if (resultImageB != null) {
                        updateFileInfo(image, resultImageB);
                        resultImageB.clearMask();
                    }

                    if (resultImageR != null) {

                        try {
                            new ViewJFrameImage(resultImageR, null, new Dimension(610, verticalPosition));
                            verticalPosition += 20;
                        } catch (OutOfMemoryError error) {
                            System.gc();
                            MipavUtil.displayError("Out of memory: unable to open imageFrameR");
                        }

                    } // if (resultImageR != null)

                    if (resultImageG != null) {

                        try {
                            new ViewJFrameImage(resultImageG, null, new Dimension(610, verticalPosition));
                            verticalPosition += 20;
                        } catch (OutOfMemoryError error) {
                            System.gc();
                            MipavUtil.displayError("Out of memory: unable to open imageFrameG");
                        }
                    } // if (resultImageG != null)

                    if (resultImageB != null) {

                        try {
                            new ViewJFrameImage(resultImageB, null, new Dimension(610, verticalPosition));
                        } catch (OutOfMemoryError error) {
                            System.gc();
                            MipavUtil.displayError("Out of memory: unable to open imageFrameB");
                        }
                    } // if (resultImageB != null)
                } else if ((resultImageR == null) && (resultImageG == null) && (resultImageB == null)) {

                    // These next lines set the titles in all frames where the source image is displayed to
                    // image name so as to indicate that the image is now unlocked!
                    // The image frames are enabled and then registered to the userinterface.
                    Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                    for (int i = 0; i < imageFrames.size(); i++) {
                        ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
                        ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);

                        if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                            UI.registerFrame((Frame) (imageFrames.elementAt(i)));

                        }
                    }

                    if (parentFrame != null) {
                        UI.registerFrame(parentFrame);
                    }

                    image.notifyImageDisplayListeners(null, true);
                } else if ((resultImageR != null) || (resultImageG != null) || (resultImageB != null)) {

                    // algorithm failed but result image still has garbage
                    if (resultImageR != null) {
                        resultImageR.disposeLocal(); // clean up memory
                        resultImageR = null;
                    }

                    if (resultImageG != null) {
                        resultImageG.disposeLocal(); // clean up memory
                        resultImageG = null;
                    }

                    if (resultImageB != null) {
                        resultImageB.disposeLocal(); // clean up memory
                        resultImageB = null;
                    }

                    System.gc();

                }
            } // if (image.isColorImage())
            else { // image black and white

                if ((algoAutoCorrelation.isCompleted() == true) && (resultImage != null)) {
                    // The algorithm has completed and produced a new image to be displayed.

                    updateFileInfo(image, resultImage);
                    resultImage.clearMask();

                    try {
                        new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        MipavUtil.displayError("Out of memory: unable to open new frame");
                    }
                } else if (resultImage == null) {

                    // These next lines set the titles in all frames where the source image is displayed to
                    // image name so as to indicate that the image is now unlocked!
                    // The image frames are enabled and then registered to the userinterface.
                    Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                    for (int i = 0; i < imageFrames.size(); i++) {
                        ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
                        ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);

                        if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                            UI.registerFrame((Frame) (imageFrames.elementAt(i)));

                        }
                    }

                    if (parentFrame != null) {
                        UI.registerFrame(parentFrame);
                    }

                    image.notifyImageDisplayListeners(null, true);
                } else if (resultImage != null) {

                    // algorithm failed but result image still has garbage
                    resultImage.disposeLocal(); // clean up memory
                    resultImage = null;
                    System.gc();

                }
            } // else image black and white
        } // if ( algorithm instanceof AlgorithmAutoCorrelation)

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
     // save the completion status for later
        setComplete(algoAutoCorrelation.isCompleted());

        algoAutoCorrelation.finalize();
        algoAutoCorrelation = null;
        dispose();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Accessor that returns the blue image.
     *
     * @return  The result blue image
     */
    public ModelImage getResultImageB() {
        return resultImageB;
    }

    /**
     * Accessor that returns the green image.
     *
     * @return  The result green image
     */
    public ModelImage getResultImageG() {
        return resultImageG;
    }

    /**
     * Accessor that returns the red image.
     *
     * @return  The result red image
     */
    public ModelImage getResultImageR() {
        return resultImageR;
    }

    /**
     * Calls the algorithm with the set variables.
     */
    protected void callAlgorithm() {
        // Can correlate from 0 to extents - 1 in every dimension.
        // Therefore, make the source image and result image
        // extents identical.

        try {

            if (image.isColorImage()) {

                if (haveRed) {
                    String nameR = makeImageName(image.getImageName(), "_autocorrelationR");
                    resultImageR = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), nameR);
                }

                if (haveGreen) {
                    String nameG = makeImageName(image.getImageName(), "_autocorrelationG");
                    resultImageG = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), nameG);
                }

                if (haveBlue) {
                    String nameB = makeImageName(image.getImageName(), "_autocorrelationB");
                    resultImageB = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), nameB);
                }

                algoAutoCorrelation = new AlgorithmAutoCorrelation(resultImageR, resultImageG, resultImageB, image);
            } else {
                String name = makeImageName(image.getImageName(), "_autocorrelation");
                resultImage = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), name);

                algoAutoCorrelation = new AlgorithmAutoCorrelation(resultImage, image);
            }

            // This is very important. Adding this object as a listener allows
            // the algorithm to notify this object when it has completed of failed.
            // See algorithm performed event. This is made possible by implementing
            algoAutoCorrelation.addListener(this);

            createProgressBar(image.getImageName(), algoAutoCorrelation);

            // Start the thread as a low priority because we wish to still have
            // user interface work fast

            if (isRunInSeparateThread()) {

                if (algoAutoCorrelation.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                algoAutoCorrelation.run();
            }
        } catch (OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            if (resultImageR != null) {
                resultImageR.disposeLocal(); // Clean up memory of result image
                resultImageR = null;
            }

            if (resultImageG != null) {
                resultImageG.disposeLocal(); // Clean up memory of result image
                resultImageG = null;
            }

            if (resultImageB != null) {
                resultImageB.disposeLocal(); // Clean up memory of result image
                resultImageB = null;
            }

            System.gc();
            MipavUtil.displayError("Dialog Autocorrelation: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Register the result image(s) in the script runner.
     */
    protected void doPostAlgorithmActions() {

        if (image.isColorImage()) {
            AlgorithmParameters.storeImageInRunner(getResultImageB());
            AlgorithmParameters.storeImageInRunner(getResultImageG());
            AlgorithmParameters.storeImageInRunner(getResultImageR());
        } else {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        UI = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

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
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        if (image.isColorImage()) {
            scriptParameters.storeImageInRecorder(getResultImageR());
            scriptParameters.storeImageInRecorder(getResultImageG());
            scriptParameters.storeImageInRecorder(getResultImageB());
        } else {
            scriptParameters.storeImageInRecorder(getResultImage());
        }
    }

    /**
     * Initializes the dialog box to a certain size and adds the components.
     */
    private void init() {
        int colorsPresent;
        JPanel mainPanel = null;
        JLabel limitLabel = null;
        GridBagConstraints gbc;
        setForeground(Color.black);
        setTitle("AutoCorrelation coefficients");

        JPanel buttonPanel = new JPanel();

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
                MipavUtil.displayError("Red, green, and blue are all single valued");

                return;
            } else if (colorsPresent == 1) {
                mainPanel = new JPanel(new GridBagLayout());
                gbc = new GridBagConstraints();
                gbc.gridx = 0;
                gbc.gridy = 0;
                gbc.weightx = 1;
                gbc.anchor = GridBagConstraints.WEST;
                gbc.gridwidth = 1;
                gbc.fill = GridBagConstraints.BOTH;
                gbc.insets = new Insets(5, 5, 5, 5);

                if (haveRed) {
                    limitLabel = new JLabel("Only the red channel has more than 1 value");
                } else if (haveGreen) {
                    limitLabel = new JLabel("Only the green channel has more than 1 value");
                } else {
                    limitLabel = new JLabel("Only the blue channel has more than 1 value");
                }

                limitLabel.setForeground(Color.black);
                limitLabel.setFont(serif12);
                mainPanel.add(limitLabel, gbc);
            } // else if (colorsPresent == 1)
            else if (colorsPresent == 2) {
                mainPanel = new JPanel(new GridBagLayout());
                gbc = new GridBagConstraints();
                gbc.gridx = 0;
                gbc.gridy = 0;
                gbc.weightx = 1;
                gbc.anchor = GridBagConstraints.WEST;
                gbc.gridwidth = 1;
                gbc.fill = GridBagConstraints.BOTH;
                gbc.insets = new Insets(5, 5, 5, 5);

                if (haveRed && haveGreen) {
                    limitLabel = new JLabel("Only the red and green channels have more than 1 value");
                } else if (haveRed && haveBlue) {
                    limitLabel = new JLabel("Only the red and blue channels have more than 1 value");
                } else {
                    limitLabel = new JLabel("Only the green and blue channels have more than 1 value");
                }

                limitLabel.setForeground(Color.black);
                limitLabel.setFont(serif12);
                mainPanel.add(limitLabel, gbc);
            } // else if (colorsPresent == 2)
        } // if (image.isColorImage())

        buildOKButton();
        buildCancelButton();
        buildHelpButton();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);
        buttonPanel.add(helpButton);

        if (mainPanel != null) {
            mainDialogPanel.add(mainPanel);
            mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);
        } else {
            mainDialogPanel.add(buttonPanel);
        }

        getContentPane().add(mainDialogPanel);

        pack();
        setVisible(true);
    }

    /**
     * Sets the variables needed to run the algorithm.
     *
     * @return  Flag indicating successful set of the variables.
     */
    private boolean setVariables() {

        // Hide dialog
        setVisible(false);

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
                return new String("Algorithms");
            }

            public String getDescription() {
                return new String("Calculates AutoCorrelation Coefficients.");
            }

            public String getDescriptionLong() {
                return new String("Calculates AutoCorrelation Coefficients.");
            }

            public String getShortLabel() {
                return new String("AutoCorrelation");
            }

            public String getLabel() {
                return new String("AutoCorrelation Coefficients");
            }

            public String getName() {
                return new String("AutoCorrelation Coefficients");
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
            if (getResultImage() != null) {
                // algo produced a new result image
                return getResultImage().getImageName();
            } else {
                // algo was done in place
                return image.getImageName();
            }
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
