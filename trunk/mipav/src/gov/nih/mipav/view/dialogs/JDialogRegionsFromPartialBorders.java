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
public class JDialogRegionsFromPartialBorders extends JDialogScriptableBase implements AlgorithmInterface, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image
    
    private JTextField textThreshold;
    
    private double threshold;

    /** DOCUMENT ME! */
    private JTextField textMaximumDistance;
    
    private ButtonGroup objectGroup;
    
    private JRadioButton darkObjectButton;
    
    private JRadioButton whiteObjectButton;
	
	private boolean darkObjectOnWhiteBackground;
	
	private double maximumDistance;

    /** DOCUMENT ME! */
    private AlgorithmRegionsFromPartialBorders  regionsAlgo;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRegionsFromPartialBorders() { }

    /**
     * Creates new dialog for entering parameters for regions from partial borders.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogRegionsFromPartialBorders(Frame theParentFrame, ModelImage im) {
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

        if (algorithm instanceof AlgorithmRegionsFromPartialBorders) {
            System.err.println("Regions From Partial Borders elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((regionsAlgo.isCompleted() == true) && (resultImage != null)) {
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

        regionsAlgo.finalize();
        regionsAlgo = null;
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
     * 
     * @param threshold
     */
    public void setthreshold(double threshold) {
        this.threshold = threshold;
    }

    /**
     * Accessor that sets the maximumDistance.
     *
     * @param  maximumDistance  DOCUMENT ME!
     */
    public void setmaximumDistance(double maximumDistance) {
        this.maximumDistance = maximumDistance;
    }
    
    /**
     * 
     * @param darkObjectOnWhiteBackground
     */
    public void setDarkObjectOnWhiteBackground(boolean darkObjectOnWhiteBackground) {
        this.darkObjectOnWhiteBackground = darkObjectOnWhiteBackground;
    }
    
    /**
     * Once all the necessary variables are set, call the regions from partial borders
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_RegionsFromPartialBorders");

        try {
        	
            resultImage     = new ModelImage(ModelStorageBase.BYTE, image.getExtents(), name);
            if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags(resultImage.getNDims() > 2,
                		resultImage.getFileInfo()[0].getDataType());
            }

            // Make algorithm
            regionsAlgo = new AlgorithmRegionsFromPartialBorders(resultImage, image, threshold, darkObjectOnWhiteBackground,
            		maximumDistance);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            regionsAlgo.addListener(this);

            createProgressBar(image.getImageName(), regionsAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (regionsAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                regionsAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Regions From Partial Borders: unable to allocate enough memory");

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
        threshold = scriptParameters.getParams().getDouble("thresh");
        maximumDistance = scriptParameters.getParams().getDouble("max_dist");
        darkObjectOnWhiteBackground = scriptParameters.getParams().getBoolean("obj");

    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, true);
        scriptParameters.getParams().put(ParameterFactory.newParameter("thresh", threshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_dist", maximumDistance));
        scriptParameters.getParams().put(ParameterFactory.newParameter("obj", darkObjectOnWhiteBackground));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Regions From Partial Borders");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(buildTitledBorder("Parameters"));
        
        JLabel labelthreshold = new JLabel("Threshold > 0.0");
        labelthreshold.setForeground(Color.black);
        labelthreshold.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        paramPanel.add(labelthreshold, gbc);

        textThreshold = new JTextField(10);
        textThreshold.setText("100.0");
        textThreshold.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textThreshold, gbc);

        JLabel labelmaximumDistance = new JLabel("Maximum distance >= 1.0");
        labelmaximumDistance.setForeground(Color.black);
        labelmaximumDistance.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 1;
        paramPanel.add(labelmaximumDistance, gbc);

        textMaximumDistance = new JTextField(10);
        textMaximumDistance.setText("10.0");
        textMaximumDistance.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textMaximumDistance, gbc);
        
        objectGroup = new ButtonGroup();
        darkObjectButton = new JRadioButton("Dark object on white background", true);
        darkObjectButton.setFont(serif12);
        darkObjectButton.setForeground(Color.black);
        objectGroup.add(darkObjectButton);
        gbc.gridx = 0;
        gbc.gridy = 2;
        paramPanel.add(darkObjectButton, gbc);

        whiteObjectButton = new JRadioButton("White object on dark background", false);
        whiteObjectButton.setFont(serif12);
        whiteObjectButton.setForeground(Color.black);
        objectGroup.add(whiteObjectButton);
        gbc.gridx = 0;
        gbc.gridy = 3;
        paramPanel.add(whiteObjectButton, gbc);

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
        
        tmpStr = textThreshold.getText();

        if (testParameter(tmpStr, Double.MIN_VALUE, Double.MAX_VALUE)) {
            threshold = Double.valueOf(tmpStr).doubleValue();
        } else {
            textThreshold.requestFocus();
            textThreshold.selectAll();

            return false;
        }

        tmpStr = textMaximumDistance.getText();

        if (testParameter(tmpStr, 1.0, Double.MAX_VALUE)) {
            maximumDistance = Double.valueOf(tmpStr).doubleValue();
        } else {
            textMaximumDistance.requestFocus();
            textMaximumDistance.selectAll();

            return false;
        }
        
        
       darkObjectOnWhiteBackground = darkObjectButton.isSelected();

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
                return new String("Algorithms.RegionsFromPartialBorders");
            }

            public String getDescription() {
                return new String("Regions From Partial Borders.");
            }

            public String getDescriptionLong() {
                return new String("Regions From Partial Borders.");
            }

            public String getShortLabel() {
                return new String("RegionsFromPartialBorders");
            }

            public String getLabel() {
                return new String("Regions From Partial Borders");
            }

            public String getName() {
                return new String("Regions From Partial Borders");
            }
        };
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
