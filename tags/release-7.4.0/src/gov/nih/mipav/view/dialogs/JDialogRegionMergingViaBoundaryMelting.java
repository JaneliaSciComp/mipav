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
public class JDialogRegionMergingViaBoundaryMelting extends JDialogScriptableBase implements AlgorithmInterface, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image
    
    private JTextField textThreshold1;
    
    private double threshold1;
    
    private JTextField textThreshold2;
    
    private double threshold2;
    
    private JTextField textThreshold3;
    
    private double threshold3;

    /** DOCUMENT ME! */
    private AlgorithmRegionMergingViaBoundaryMelting  regionAlgo;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRegionMergingViaBoundaryMelting() { }

    /**
     * Creates new dialog for entering parameters for region merging via boundary melting.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogRegionMergingViaBoundaryMelting(Frame theParentFrame, ModelImage im) {
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

        if (algorithm instanceof AlgorithmRegionMergingViaBoundaryMelting) {
            System.err.println("Regions Merging Via Boundary Melting elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((regionAlgo.isCompleted() == true) && (resultImage != null)) {
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

        regionAlgo.finalize();
        regionAlgo = null;
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
     * @param threshold1
     */
    public void setthreshold1(double threshold1) {
        this.threshold1 = threshold1;
    }
    
    /**
     * 
     * @param threshold2
     */
    public void setthreshold2(double threshold2) {
        this.threshold2 = threshold2;
    }
    
    /**
     * 
     * @param threshold3
     */
    public void setthreshold3(double threshold3) {
        this.threshold3 = threshold3;
    }
    
    /**
     * Once all the necessary variables are set, call the region merging via boundary melting
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_RegionMergingViaBoundaryMelting");

        try {
        	
            resultImage     = new ModelImage(ModelStorageBase.INTEGER, image.getExtents(), name);
            if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags(resultImage.getNDims() > 2,
                		resultImage.getFileInfo()[0].getDataType());
            }

            // Make algorithm
            regionAlgo = new AlgorithmRegionMergingViaBoundaryMelting(resultImage, image, threshold1, threshold2,
            		threshold3);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            regionAlgo.addListener(this);

            createProgressBar(image.getImageName(), regionAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (regionAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                regionAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Region Merging Via Boundary Melting: unable to allocate enough memory");

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
        threshold1 = scriptParameters.getParams().getDouble("thresh1");
        threshold2 = scriptParameters.getParams().getDouble("thresh2");
        threshold3 = scriptParameters.getParams().getDouble("thresh3");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, true);
        scriptParameters.getParams().put(ParameterFactory.newParameter("thresh1", threshold1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("thresh2", threshold2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("thresh3", threshold3));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Region Merging Via Boundary Melting");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(buildTitledBorder("Parameters"));
        
        JLabel labelthreshold1 = new JLabel("Threshold1 > 0.0");
        labelthreshold1.setForeground(Color.black);
        labelthreshold1.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        paramPanel.add(labelthreshold1, gbc);

        textThreshold1 = new JTextField(10);
        textThreshold1.setText("10.0");
        textThreshold1.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textThreshold1, gbc);

        JLabel labelthreshold2 = new JLabel("1.0 > Threshold2 > 0.0");
        labelthreshold2.setForeground(Color.black);
        labelthreshold2.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 1;
        paramPanel.add(labelthreshold2, gbc);

        textThreshold2 = new JTextField(10);
        textThreshold2.setText("0.1");
        textThreshold2.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textThreshold2, gbc);
        
        JLabel labelthreshold3 = new JLabel("1.0 > Threshold3 > 0.0");
        labelthreshold3.setForeground(Color.black);
        labelthreshold3.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 2;
        paramPanel.add(labelthreshold3, gbc);

        textThreshold3 = new JTextField(10);
        textThreshold3.setText("0.2");
        textThreshold3.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textThreshold3, gbc);
        
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
        
        tmpStr = textThreshold1.getText();

        if (testParameter(tmpStr, Double.MIN_VALUE, Double.MAX_VALUE)) {
            threshold1 = Double.valueOf(tmpStr).doubleValue();
        } else {
            textThreshold1.requestFocus();
            textThreshold1.selectAll();

            return false;
        }

        tmpStr = textThreshold2.getText();

        if (testParameter(tmpStr, Double.MIN_VALUE, 0.9999)) {
            threshold2 = Double.valueOf(tmpStr).doubleValue();
        } else {
            textThreshold2.requestFocus();
            textThreshold2.selectAll();

            return false;
        }
        
        tmpStr = textThreshold3.getText();

        if (testParameter(tmpStr, Double.MIN_VALUE, 0.9999)) {
            threshold3 = Double.valueOf(tmpStr).doubleValue();
        } else {
            textThreshold3.requestFocus();
            textThreshold3.selectAll();

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
                return new String("Algorithms.RegionMergingViaBoundaryMelting");
            }

            public String getDescription() {
                return new String("Region Merging Via Boundary Melting.");
            }

            public String getDescriptionLong() {
                return new String("Region Merging Via Boundary Melting.");
            }

            public String getShortLabel() {
                return new String("RegionMergingViaBoundaryMelting");
            }

            public String getLabel() {
                return new String("Regions Merging Via Boundary Melting");
            }

            public String getName() {
                return new String("Regions Merging Via Boundary Melting");
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
