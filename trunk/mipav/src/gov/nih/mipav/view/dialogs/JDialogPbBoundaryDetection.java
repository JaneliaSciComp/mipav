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
public class JDialogPbBoundaryDetection extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image
    
    private static final int BGTG = 1;
    
    private static final int CGTG = 2;
	
	private int gradientType = BGTG;
	
	private static final int GRAY_PRESENTATION = 1;
	
	private static final int COLOR_PRESENTATION = 2;
	
	private int presentation = GRAY_PRESENTATION;
	
	private double lowRadius = 0.01;
	
	private double highRadius = 0.02;
	
	private int numOrientations = 8;
	
	private JTextField textOrientations;

    /** DOCUMENT ME! */
    private AlgorithmPbBoundaryDetection pbAlgo;
    
    private ButtonGroup gradientGroup;
    
    private JRadioButton bgtgButton;
    
    private JRadioButton cgtgButton;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogPbBoundaryDetection() { }

    /**
     * Creates new dialog for entering parameters for Pb boundary detection.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogPbBoundaryDetection(Frame theParentFrame, ModelImage im) {
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

        if (algorithm instanceof AlgorithmPbBoundaryDetection) {
            System.err.println("Pb Boundary Detection Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((pbAlgo.isCompleted() == true) && (resultImage != null)) {
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

        pbAlgo.finalize();
        pbAlgo = null;
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
     * @param sigma
     */
    public void setNumOrientations(int numOrientatons) {
        this.numOrientations = numOrientations;
    }
    
    /**
     * Once all the necessary variables are set, call the rule based contrast enhancement algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_Detection");

        try {
        	
            resultImage     = new ModelImage(ModelStorageBase.DOUBLE, image.getExtents(), name);
            if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags();
            }

            // Make algorithm
            pbAlgo = new AlgorithmPbBoundaryDetection(resultImage, image, gradientType, presentation, lowRadius, highRadius,
            		numOrientations);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            pbAlgo.addListener(this);

            createProgressBar(image.getImageName(), pbAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (pbAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                pbAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Pb boundary Detection: unable to allocate enough memory");

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
        numOrientations = scriptParameters.getParams().getInt("num_orientations");
        gradientType = scriptParameters.getParams().getInt("grad_type");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, true);
        scriptParameters.getParams().put(ParameterFactory.newParameter("num_orientations", numOrientations));
        scriptParameters.getParams().put(ParameterFactory.newParameter("grad_type", gradientType));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Pb boundary Detection");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(buildTitledBorder("Parameters"));
        
        JLabel labelOrientations = new JLabel("Number of orientations");
        labelOrientations.setForeground(Color.black);
        labelOrientations.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        paramPanel.add(labelOrientations, gbc);

        textOrientations = new JTextField(10);
        textOrientations.setText("8");
        textOrientations.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textOrientations, gbc);
        
        gradientGroup = new ButtonGroup();
        if (image.isColorImage()) {
            bgtgButton = new JRadioButton("Brightness gradient texture gradient", false);
        }
        else {
        	bgtgButton = new JRadioButton("Brightness gradient texture gradient", true);	
        }
        bgtgButton.setFont(serif12);
        bgtgButton.setForeground(Color.black);
        gradientGroup.add(bgtgButton);
        gbc.gridy++;
        gbc.gridx = 0;
        paramPanel.add(bgtgButton, gbc);
        
        if (image.isColorImage()) {
            cgtgButton = new JRadioButton("Color gradient texture gradient", true);
        }
        else {
        	cgtgButton = new JRadioButton("Color gradient texture gradient", false);
        	cgtgButton.setEnabled(false);
        }
        cgtgButton.setFont(serif12);
        cgtgButton.setForeground(Color.black);
        gradientGroup.add(cgtgButton);
        gbc.gridy++;
        gbc.gridx = 0;
        paramPanel.add(cgtgButton, gbc);

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
        
        tmpStr = textOrientations.getText();

        if (testParameter(tmpStr, 1.0, 100.0)) {
            numOrientations = Integer.valueOf(tmpStr).intValue();
        } else {
            textOrientations.requestFocus();
            textOrientations.selectAll();

            return false;
        }
        
        if (bgtgButton.isSelected()) {
        	gradientType = BGTG;
        }
        else {
        	gradientType = CGTG;
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
                return new String("Algorithms.EdgeDetection (Pb Boundary)");
            }

            public String getDescription() {
                return new String("Applies Pb Boundary Detection.");
            }

            public String getDescriptionLong() {
                return new String("Applies Pb Boundary Detection.");
            }

            public String getShortLabel() {
                return new String("PbBoundaryDetection");
            }

            public String getLabel() {
                return new String("Pb Boundary Detection");
            }

            public String getName() {
                return new String("Pb Boundary Detection");
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
            table.put(new ParameterInt("num_orientations", 8));
            table.put(new ParameterInt("grad_type", BGTG));
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
