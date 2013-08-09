package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class JDialogCannyEdgeDetection extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image
    
    private JTextField textHighThreshold;
    
    private double highThreshold;

    /** DOCUMENT ME! */
    private JTextField textLowThreshold;

    /** DOCUMENT ME! */
    private double lowThreshold;
    
    private JTextField textSigma;
    
    private float sigma;

    /** DOCUMENT ME! */
    private AlgorithmCannyEdgeDetection cannyAlgo;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogCannyEdgeDetection() { }

    /**
     * Creates new dialog for entering parameters for Canny edge detection.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogCannyEdgeDetection(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
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

        if (algorithm instanceof AlgorithmCannyEdgeDetection) {
            System.err.println("Canny Edge Detection Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((cannyAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                try {

                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
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

        cannyAlgo.finalize();
        cannyAlgo = null;
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
     * 
     * @param highThreshold
     */
    public void setHighThreshold(double highThreshold) {
        this.highThreshold = highThreshold;
    }

    /**
     * Accessor that sets the lowThreshold.
     *
     * @param  lowThreshold  DOCUMENT ME!
     */
    public void setLowThreshold(double lowThreshold) {
        this.lowThreshold = lowThreshold;
    }
    
    /**
     * 
     * @param sigma
     */
    public void setSigma(float sigma) {
        this.sigma = sigma;
    }
    
    /**
     * Once all the necessary variables are set, call the rule based contrast enhancement algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_CannyEdgeDetection");

        try {
        	
            resultImage     = new ModelImage(ModelStorageBase.BYTE, image.getExtents(), name);
            if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags();
            }

            // Make algorithm
            cannyAlgo = new AlgorithmCannyEdgeDetection(resultImage, image, highThreshold, lowThreshold, sigma);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            cannyAlgo.addListener(this);

            createProgressBar(image.getImageName(), cannyAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (cannyAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                cannyAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Canny Edge Detection: unable to allocate enough memory");

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            return;
        }
           
    }

    

    /**
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {

        AlgorithmParameters.storeImageInRunner(resultImage);
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        highThreshold = scriptParameters.getParams().getDouble("high_threshold");
        lowThreshold = scriptParameters.getParams().getDouble("low_threshold");
        sigma = scriptParameters.getParams().getFloat("sig");

    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, true);
        scriptParameters.getParams().put(ParameterFactory.newParameter("high_threshold", highThreshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("low_threshold", lowThreshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("sig", sigma));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Canny Edge Detection");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(buildTitledBorder("Parameters"));
        
        JLabel labelHighThreshold = new JLabel("High Threshold ( <= 1.0)");
        labelHighThreshold.setForeground(Color.black);
        labelHighThreshold.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        paramPanel.add(labelHighThreshold, gbc);

        textHighThreshold = new JTextField(10);
        textHighThreshold.setText("0.12");
        textHighThreshold.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textHighThreshold, gbc);

        JLabel labelLowThreshold = new JLabel("Low Threshold (usually 1/3-1/2 High Threshold)");
        labelLowThreshold.setForeground(Color.black);
        labelLowThreshold.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 1;
        paramPanel.add(labelLowThreshold, gbc);

        textLowThreshold = new JTextField(10);
        textLowThreshold.setText("0.05");
        textLowThreshold.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textLowThreshold, gbc);
        
        JLabel labelSigma = new JLabel("Gaussian smoothing sigma (1.0-10.0)");
        labelSigma.setForeground(Color.black);
        labelSigma.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 2;
        paramPanel.add(labelSigma, gbc);

        textSigma = new JTextField(10);
        textSigma.setText("1.4");
        textSigma.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textSigma, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        getContentPane().add(paramPanel);
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
        
        tmpStr = textHighThreshold.getText();

        if (testParameter(tmpStr, 0.001, 1.0)) {
            highThreshold = Double.valueOf(tmpStr).doubleValue();
        } else {
            textHighThreshold.requestFocus();
            textHighThreshold.selectAll();

            return false;
        }

        tmpStr = textLowThreshold.getText();

        if (testParameter(tmpStr, 0.0001, 0.9999 * highThreshold)) {
            lowThreshold = Double.valueOf(tmpStr).doubleValue();
        } else {
            textLowThreshold.requestFocus();
            textLowThreshold.selectAll();

            return false;
        }
        
        tmpStr = textSigma.getText();

        if (testParameter(tmpStr, 1.0, 10.0)) {
            sigma = Float.valueOf(tmpStr).floatValue();
        } else {
            textSigma.requestFocus();
            textSigma.selectAll();

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
                return new String("Algorithms.EdgeDetection (Canny)");
            }

            public String getDescription() {
                return new String("Applies Canny Edge Detection.");
            }

            public String getDescriptionLong() {
                return new String("Applies Canny Edge Detection.");
            }

            public String getShortLabel() {
                return new String("CannyEdgeDetection");
            }

            public String getLabel() {
                return new String("Canny Edge Detection");
            }

            public String getName() {
                return new String("Canny Edge Detection");
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
            table.put(new ParameterDouble("high_threshold", 0.12));
            table.put(new ParameterDouble("low_threshold", 0.05));
            table.put(new ParameterFloat("sig", 1.4f));
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
