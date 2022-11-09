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
public class JDialogHarrisCornerDetector extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image
    
    private JTextField textRadius;

    /** DOCUMENT ME! */
    private JTextField textPointThreshold;
    
    private JTextField textSigma;
    
    private float sigma;

    /** DOCUMENT ME! */
    private AlgorithmHarrisCornerDetector HarrisAlgo;
    
    private int radius = 1;   // Radius of the region considered in non-maximal suppression.
                              // Typical values to use might be 1-3 pixels.
    //private double k = 0.1;
    
    // No idea what this value should be
    private double pointThreshold = 1.0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogHarrisCornerDetector() { }

    /**
     * Creates new dialog for entering parameters for Harris corner detection.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogHarrisCornerDetector(Frame theParentFrame, ModelImage im) {
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

        if (algorithm instanceof AlgorithmHarrisCornerDetector) {
            System.err.println("Harris corner detector Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((HarrisAlgo.isCompleted() == true) && (resultImage != null)) {
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

        HarrisAlgo.finalize();
        HarrisAlgo = null;
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
     * @param radius
     */
    public void setRadius(int radius) {
        this.radius = radius;
    }

    /**
     * Accessor that sets the pointThreshold.
     *
     * @param  pointThreshold  DOCUMENT ME!
     */
    public void setPointThreshold(double pointThreshold) {
        this.pointThreshold = pointThreshold;
    }
    
    /**
     * 
     * @param sigma
     */
    public void setSigma(float sigma) {
        this.sigma = sigma;
    }
    
    /**
     * Once all the necessary variables are set, call the Harris corner detector algorithm.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_HarrisCornerDetector");

        try {
        	
            resultImage     = new ModelImage(ModelStorageBase.BYTE, image.getExtents(), name);
            if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags(resultImage.getNDims() > 2,
                		resultImage.getFileInfo()[0].getDataType());
            }

            // Make algorithm
            HarrisAlgo = new AlgorithmHarrisCornerDetector(resultImage, image, sigma, radius, pointThreshold);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            HarrisAlgo.addListener(this);

            createProgressBar(image.getImageName(), HarrisAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (HarrisAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                HarrisAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Harris corner detector: unable to allocate enough memory");

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
        sigma = scriptParameters.getParams().getFloat("sig");
        radius = scriptParameters.getParams().getInt("rad");
        pointThreshold = scriptParameters.getParams().getDouble("point_threshold");

    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, true);
        scriptParameters.getParams().put(ParameterFactory.newParameter("sig", sigma));
        scriptParameters.getParams().put(ParameterFactory.newParameter("rad", radius));
        scriptParameters.getParams().put(ParameterFactory.newParameter("point_threshold", pointThreshold));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Harris Corner Detector");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(buildTitledBorder("Parameters"));
        
        JLabel labelSigma = new JLabel("Gaussian smoothing sigma (1.0-10.0)");
        labelSigma.setForeground(Color.black);
        labelSigma.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        paramPanel.add(labelSigma, gbc);

        textSigma = new JTextField(10);
        textSigma.setText("1.4");
        textSigma.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textSigma, gbc);
        
        JLabel labelRadius = new JLabel("Non-maximal suppression radius");
        labelRadius.setForeground(Color.black);
        labelRadius.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 1;
        paramPanel.add(labelRadius, gbc);

        textRadius = new JTextField(5);
        textRadius.setText("1");
        textRadius.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textRadius, gbc);

        JLabel labelPointThreshold = new JLabel("Point Threshold ");
        labelPointThreshold.setForeground(Color.black);
        labelPointThreshold.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 2;
        paramPanel.add(labelPointThreshold, gbc);

        textPointThreshold = new JTextField(10);
        textPointThreshold.setText("1.0");
        textPointThreshold.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textPointThreshold, gbc);

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
        
        tmpStr = textSigma.getText();

        if (testParameter(tmpStr, 1.0, 10.0)) {
            sigma = Float.valueOf(tmpStr).floatValue();
        } else {
            textSigma.requestFocus();
            textSigma.selectAll();

            return false;
        }
        
        tmpStr = textRadius.getText();

        if (testParameter(tmpStr, 1, 7)) {
            radius = Integer.valueOf(tmpStr).intValue();
        } else {
            textRadius.requestFocus();
            textRadius.selectAll();

            return false;
        }

        tmpStr = textPointThreshold.getText();

        if (testParameter(tmpStr, Double.MIN_VALUE, Double.MAX_VALUE)) {
            pointThreshold = Double.valueOf(tmpStr).doubleValue();
        } else {
            textPointThreshold.requestFocus();
            textPointThreshold.selectAll();

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
                return new String("Algorithms.CornerDetection (Harris)");
            }

            public String getDescription() {
                return new String("Applies Harris Corner Detector.");
            }

            public String getDescriptionLong() {
                return new String("Applies Harris Corner Detector.");
            }

            public String getShortLabel() {
                return new String("HarisCornerDetector");
            }

            public String getLabel() {
                return new String("Harris Corner Detector");
            }

            public String getName() {
                return new String("Harris Corner Detector");
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
            table.put(new ParameterFloat("sig", 1.4f));
            table.put(new ParameterDouble("rad", 1));
            table.put(new ParameterDouble("point_threshold", 1));
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
