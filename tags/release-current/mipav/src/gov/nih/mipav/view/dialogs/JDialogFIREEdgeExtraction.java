package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.IOException;

import java.util.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class JDialogFIREEdgeExtraction extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image
    
    private JTextField textGaussianStdDev;
    
    private double gaussianStdDev;

    /** DOCUMENT ME! */
    private JTextField textWhiteStart;

    /** DOCUMENT ME! */
    private double whiteStart;
    
    private JTextField textBlackEnd;
    
    private double blackEnd;

    /** DOCUMENT ME! */
    private String[] titles;


    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private AlgorithmFIREEdgeExtraction feeAlgo;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogFIREEdgeExtraction() { }

    /**
     * Creates new dialog for entering parameters for FIRE Edge Extraction.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogFIREEdgeExtraction(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        } else if (command.equals("Cancel")) {
            dispose();
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

        if (algorithm instanceof AlgorithmFIREEdgeExtraction) {
            System.err.println("FIRE Edge Extraction Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((feeAlgo.isCompleted() == true) && (resultImage != null)) {
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
                System.gc();
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
     // save the completion status for later
        setComplete(algorithm.isCompleted());

        feeAlgo.finalize();
        feeAlgo = null;
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

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Method to handle item events.
     *
     * @param  event  event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) {
        // Object source = event.getSource();
        // float tempNum;

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
    
    public void setGaussianStdDev(double gaussianStdDev) {
    	this.gaussianStdDev = gaussianStdDev;
    }

    /**
     * Accessor that sets the white start.
     *
     * @param  whiteStart  DOCUMENT ME!
     */
    public void setWhiteStart(double whiteStart) {
        this.whiteStart = whiteStart;
    }
    
    public void setBlackEnd(double blackEnd) {
    	this.blackEnd = blackEnd;
    }
    
    /**
     * Once all the necessary variables are set, call the rule based contrast enhancement algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_contrastEnhancement");
        int zDim;
        

            if (displayLoc == NEW) {

                try {
                	

                    resultImage     = new ModelImage(image.getType(), image.getExtents(), name);
                    if (resultImage.getNDims() >= 3) {
                    	zDim = resultImage.getExtents()[2];
                    }
                    else {
                    	zDim = 1;
                    }

                    if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                    	for (int i = 0; i < zDim; i++) {
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                        }
                    }

                    // Make algorithm
                    feeAlgo = new AlgorithmFIREEdgeExtraction(resultImage, image, gaussianStdDev, whiteStart, blackEnd);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    feeAlgo.addListener(this);

                    createProgressBar(image.getImageName(), feeAlgo);

                    setVisible(false); // Hide dialog

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (feeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        feeAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog FIRE Edge Extraction: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }

                    return;
                }
            } else { // displayLoc == REPLACE

                try {

                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    feeAlgo = new AlgorithmFIREEdgeExtraction(image, gaussianStdDev, whiteStart, blackEnd);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    feeAlgo.addListener(this);
                    createProgressBar(image.getImageName(), feeAlgo);

                    // Hide the dialog since the algorithm is about to run.
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (feeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        feeAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog FIRE Edge Extraction: unable to allocate enough memory");

                    return;
                }
            }
        
    }

    

    /**
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {

        if (displayLoc == NEW) {
            AlgorithmParameters.storeImageInRunner(resultImage);
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();

        if (scriptParameters.doOutputNewImage()) {
            this.setDisplayLocNew();
        } else {
            this.setDisplayLocReplace();
        }

        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        gaussianStdDev = scriptParameters.getParams().getDouble("gaussian_standard_deviation");
        whiteStart = scriptParameters.getParams().getDouble("white_start");
        blackEnd = scriptParameters.getParams().getDouble("black_end");

    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("gaussian_standard_deviation", gaussianStdDev));
        scriptParameters.getParams().put(ParameterFactory.newParameter("white_start", whiteStart));
        scriptParameters.getParams().put(ParameterFactory.newParameter("black_end", blackEnd));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Rule Based Contrast Enhancement");
        double imageRange = image.getMax() - image.getMin();

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(buildTitledBorder("Parameters"));
        
        JLabel labelGaussianStdDev = new JLabel("Zero membership intensity difference standard deviation");
        labelGaussianStdDev.setForeground(Color.black);
        labelGaussianStdDev.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        paramPanel.add(labelGaussianStdDev, gbc);

        textGaussianStdDev = new JTextField(10);
        textGaussianStdDev.setText(String.valueOf(imageRange/25.0));
        textGaussianStdDev.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textGaussianStdDev, gbc);

        JLabel labelWhiteStart = new JLabel("Gray level at which white membership becomes positive");
        labelWhiteStart.setForeground(Color.black);
        labelWhiteStart.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 1;
        paramPanel.add(labelWhiteStart, gbc);

        textWhiteStart = new JTextField(10);
        textWhiteStart.setText(String.valueOf((image.getMin() + imageRange/3.0)));
        textWhiteStart.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textWhiteStart, gbc);
        
        JLabel labelBlackEnd = new JLabel("Gray level at which black membership becomes zero");
        labelBlackEnd.setForeground(Color.black);
        labelBlackEnd.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 2;
        paramPanel.add(labelBlackEnd, gbc);

        textBlackEnd = new JTextField(10);
        textBlackEnd.setText(String.valueOf((image.getMin() + (2.0*imageRange)/3.0)));
        textBlackEnd.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textBlackEnd, gbc);

        JPanel destinationPanel = new JPanel(new GridLayout(2, 1));
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        ButtonGroup destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage);

        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        mainPanel.add(paramPanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(destinationPanel, gbc);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        //setResizable(false);
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

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }
        
        tmpStr = textGaussianStdDev.getText();

        if (testParameter(tmpStr, 0.0, image.getMax()-image.getMin())) {
            gaussianStdDev = Double.valueOf(tmpStr).doubleValue();
        } else {
            textGaussianStdDev.requestFocus();
            textGaussianStdDev.selectAll();

            return false;
        }

        tmpStr = textWhiteStart.getText();

        if (testParameter(tmpStr, image.getMin(), image.getMax())) {
            whiteStart = Double.valueOf(tmpStr).doubleValue();
            if ((whiteStart == image.getMin()) || (whiteStart == image.getMax())) {
            	textWhiteStart.requestFocus();
                textWhiteStart.selectAll();

                return false;	
            }
        } else {
            textWhiteStart.requestFocus();
            textWhiteStart.selectAll();

            return false;
        }
        
        tmpStr = textBlackEnd.getText();

        if (testParameter(tmpStr, image.getMin(), image.getMax())) {
            blackEnd = Double.valueOf(tmpStr).doubleValue();
        } else {
            textBlackEnd.requestFocus();
            textBlackEnd.selectAll();

            return false;
        }
        
        if (whiteStart >= blackEnd) {
        	MipavUtil.displayError("Must have whiteStart < blackEnd" );
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
                return new String("Algorithms.Filters (Edge Extraction by FIRE Operators)");
            }

            public String getDescription() {
                return new String("Applies Edge Extraction by FIRE Operators.");
            }

            public String getDescriptionLong() {
                return new String("Applies Edge Extraction by FIRE Operators.");
            }

            public String getShortLabel() {
                return new String("EdgeExtractionFIREOperators");
            }

            public String getLabel() {
                return new String("Edge Extraction by FIRE Operators");
            }

            public String getName() {
                return new String("Edge Extraction by FIRE Operators");
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
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
            table.put(new ParameterDouble("gaussian_standard_deviation", 5.0));
            table.put(new ParameterDouble("white_start", 80.0));
            table.put(new ParameterDouble("black_end", 160.0));
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
