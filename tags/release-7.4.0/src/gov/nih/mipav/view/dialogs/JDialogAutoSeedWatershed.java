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
public class JDialogAutoSeedWatershed extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private AlgorithmAutoSeedWatershed wsAlgo;
    
    private JTextField textGaussX;
    
    private JTextField textGaussY;
    
    private float scaleX;
    
    private float scaleY;
    
    private JCheckBox mergeCheckBox;
    
    private boolean mergeSimilar;
    
    private JLabel distanceLabel;
    
    private JTextField distanceText;
    
    private double maxDistance;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogAutoSeedWatershed() { }

    /**
     * Creates new dialog for entering parameters for Auto Seed Watershed.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogAutoSeedWatershed(Frame theParentFrame, ModelImage im) {
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
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (source.equals(mergeCheckBox)) {
           distanceLabel.setEnabled(mergeCheckBox.isSelected());
           distanceText.setEnabled(mergeCheckBox.isSelected());
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

        if (algorithm instanceof AlgorithmAutoSeedWatershed) {
            System.err.println("Auto Seed Watershed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((wsAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                //updateFileInfo(image, resultImage);
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

        wsAlgo.finalize();
        wsAlgo = null;
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
     * Once all the necessary variables are set, call the rule based contrast enhancement algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_AutoSeedWatershed");

        try {
        	
            resultImage     = new ModelImage(ModelStorageBase.SHORT, image.getExtents(), name);
            if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags(false, resultImage.getFileInfo(0).getDataType());
            }

            // Make algorithm
            wsAlgo = new AlgorithmAutoSeedWatershed(resultImage, image, scaleX, scaleY, mergeSimilar, maxDistance);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            wsAlgo.addListener(this);

            createProgressBar(image.getImageName(), wsAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (wsAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                wsAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Auto Seed Watershed: unable to allocate enough memory");

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
        scaleX = scriptParameters.getParams().getFloat("scaleX");
        scaleY = scriptParameters.getParams().getFloat("scaleY");
        if (image.isColorImage()) {
            mergeSimilar = scriptParameters.getParams().getBoolean("merge_similar");
            maxDistance = scriptParameters.getParams().getDouble("max_distance");
        }

    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, true);
        scriptParameters.getParams().put(ParameterFactory.newParameter("scaleX", scaleX));
        scriptParameters.getParams().put(ParameterFactory.newParameter("scaleY", scaleY));
        if (image.isColorImage()) {
        	scriptParameters.getParams().put(ParameterFactory.newParameter("merge_similar", mergeSimilar));
        	scriptParameters.getParams().put(ParameterFactory.newParameter("max_distance", maxDistance));
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Auto Seed Watershed");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(buildTitledBorder("Parameters"));
        
        JPanel scalePanel = new JPanel(new GridBagLayout());
        scalePanel.setBorder(buildTitledBorder("Scale of the Gaussian"));
        GridBagConstraints gbc2 = new GridBagConstraints();
        JLabel labelGaussX = new JLabel("X Dimension (0.5 - 5.0)");
        labelGaussX.setForeground(Color.black);
        labelGaussX.setFont(serif12);
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        gbc2.weightx = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        scalePanel.add(labelGaussX, gbc2);
        textGaussX = new JTextField(10);
        textGaussX.setText("1.0");
        textGaussX.setFont(serif12);
        gbc2.gridx = 1;
        gbc2.gridy = 0;
        gbc2.weightx = 1;
        gbc2.gridheight = 1;
        gbc2.gridwidth = 1;
        gbc2.fill = GridBagConstraints.REMAINDER;
        gbc2.insets = new Insets(0, 5, 0, 5);
        scalePanel.add(textGaussX, gbc2);
        JLabel labelGaussY = new JLabel("Y Dimension (0.5 - 5.0)");
        labelGaussY.setForeground(Color.black);
        labelGaussY.setFont(serif12);
        gbc2.gridx = 0;
        gbc2.gridy = 1;
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        gbc2.weightx = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        scalePanel.add(labelGaussY, gbc2);
        textGaussY = new JTextField(10);
        textGaussY.setText("1.0");
        textGaussY.setFont(serif12);
        gbc2.gridx = 1;
        gbc2.gridy = 1;
        gbc2.weightx = 1;
        gbc2.gridheight = 1;
        gbc2.gridwidth = 1;
        gbc2.fill = GridBagConstraints.REMAINDER;
        gbc2.insets = new Insets(0, 5, 0, 5);
        scalePanel.add(textGaussY, gbc2);
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        scalePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        gbc.gridwidth = 2;
        paramPanel.add(scalePanel, gbc);
        if (image.isColorImage()) {
            gbc.gridy = 1;
            mergeCheckBox = new JCheckBox("Merge similar color regions together");
            mergeCheckBox.setSelected(false);
            mergeCheckBox.setForeground(Color.black);
            mergeCheckBox.setFont(serif12);
            mergeCheckBox.addActionListener(this);
            paramPanel.add(mergeCheckBox, gbc);
            
            gbc.gridy = 2;
            gbc.gridwidth = 1;
            distanceLabel = new JLabel("Maximum Bhattachryya distance");
            distanceLabel.setForeground(Color.black);
            distanceLabel.setFont(serif12);
            distanceLabel.setEnabled(false);
            paramPanel.add(distanceLabel, gbc);
            
            distanceText = new JTextField(10);
            distanceText.setText("0.1");
            distanceText.setFont(serif12);
            gbc.gridx = 1;
            distanceText.setEnabled(false);
            paramPanel.add(distanceText, gbc);
        }

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
        
        tmpStr = textGaussX.getText();

        if (testParameter(tmpStr, 0.01, 5.0)) {
            scaleX = Float.valueOf(tmpStr).floatValue();
        } else {
            textGaussX.requestFocus();
            textGaussX.selectAll();

            return false;
        }

        tmpStr = textGaussY.getText();

        if (testParameter(tmpStr, 0.01, 5.0)) {
            scaleY = Float.valueOf(tmpStr).floatValue();
        } else {
            textGaussY.requestFocus();
            textGaussY.selectAll();

            return false;
        }
        
        if (image.isColorImage()) {
        	mergeSimilar = mergeCheckBox.isSelected();
        	if (mergeSimilar) {
        		tmpStr = distanceText.getText();
        		if (testParameter(tmpStr, 0.0, 100.0)) {
        		    maxDistance = Double.valueOf(tmpStr).doubleValue();
        		}
        		else {
        			distanceText.requestFocus();
        			distanceText.selectAll();
        			return false;
        		}
        	}
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
                return new String("Algorithms.Segmentation (Auto Seed Watershed)");
            }

            public String getDescription() {
                return new String("Applies Auto Seed Watershed.");
            }

            public String getDescriptionLong() {
                return new String("Applies Auto Seed Watershed.");
            }

            public String getShortLabel() {
                return new String("Auto Seed Watershed");
            }

            public String getLabel() {
                return new String("Auto Seed Watershed");
            }

            public String getName() {
                return new String("Auto Seed Watershed");
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
            table.put(new ParameterFloat("scaleX", 1.0f));
            table.put(new ParameterFloat("scaleY", 1.0f));
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
