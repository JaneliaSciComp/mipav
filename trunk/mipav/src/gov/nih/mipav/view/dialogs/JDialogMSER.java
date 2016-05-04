package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.AlgorithmBRISK;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmMSER;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.ScriptableActionInterface;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.util.Enumeration;
import java.util.Vector;

import javax.swing.*;


// import javax.swing.*;

/**
 * DOCUMENT ME!
 */
@SuppressWarnings("serial")
public class JDialogMSER extends JDialogScriptableBase implements AlgorithmInterface, ScriptableActionInterface {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    // private static final long serialVersionUID;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    private final static int POINTS_ONLY = 1;
	private final static int ELLIPSES_ONLY = 2;
	private final static int POINTS_AND_ELLIPSES = 3;
	
	// Must be a non-negative number
	private double delta = 5.0;
	
	// Maximum region (relative) area.  Must be in the [0,1] range.
	private double max_area = 0.75;
	
	// Minimum region (relative) area.  Must be in the [0,1] range.
	// Default is 3.0/sliceSize;
	private double min_area;
	
	// Maximum absolute region stability.  Must be a non-negative number.
	private double max_variation = 0.25;
	
	// Must be in the [0,1] range.
	private double min_diversity = 0.2;
	
	// Enable or disable bright_on_dark regions.  Default enabled
	private boolean bright_on_dark = true;
	
	// Enable or disable dark_on_bright regions.  Default enabled.
	private boolean dark_on_bright = true;
	
	private int outputVOIType;

    /** DOCUMENT ME! */
    private AlgorithmMSER mserAlgo;

    

    private JPanel paramPanel;

    private JPanel imageVOIPanel;

    private ButtonGroup imageVOIGroup;

    private JRadioButton wholeImage;

    private JRadioButton VOIRegions;

    private JLabel labelDelta;

    private JTextField textDelta;

    private JLabel labelMaxArea;

    private JTextField textMaxArea;

    private JLabel labelMinArea;

    private JTextField textMinArea;
    
    private JLabel labelMaxVariation;
    
    private JTextField textMaxVariation;

    private JCheckBox rotationCheckBox;

    private JCheckBox scaleCheckBox;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogMSER() {}

    /**
     * Creates new dialog for entering parameters for BRISK.
     * 
     * @param theParentFrame Parent frame
     * @param im Source image
     */
    public JDialogMSER(final Frame theParentFrame, final ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     * 
     * @param event event that triggers function
     */
    @Override
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Help")) {
            // MipavUtil.showHelp("");
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
     * @param algorithm Algorithm that caused the event.
     */
    @Override
    public void algorithmPerformed(final AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmMSER) {
            System.err.println("MSER Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
        // save the completion status for later
        setComplete(algorithm.isCompleted());

        mserAlgo.finalize();
        mserAlgo = null;
        // dispose();
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Method to handle item events.
     * 
     * @param event event that cause the method to fire
     */
    @Override
    public void itemStateChanged(final ItemEvent event) {
        // Object source = event.getSource();
        // float tempNum;

    }

    /**
     * Once all the necessary variables are set, call the rule based contrast enhancement algorithm based on what type
     * of image this is and whether or not there is a separate destination image.
     */
    @Override
    protected void callAlgorithm() {

        try {

            // Make algorithm
            mserAlgo = new AlgorithmMSER(image, delta, max_area, min_area, max_variation, 
        			min_diversity, bright_on_dark, dark_on_bright, outputVOIType);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            mserAlgo.addListener(this);

            createProgressBar(image.getImageName(), mserAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (mserAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                mserAlgo.run();
            }
        } catch (final OutOfMemoryError x) {
            MipavUtil.displayError("Dialog BRISK: unable to allocate enough memory");

            return;
        }

    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void doPostAlgorithmActions() {

    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
    	double defaultMinArea;
        setForeground(Color.black);

        setTitle("Maximally Stable Extremal Regions");

        labelDelta = new JLabel("Delta (> 0)");
        labelDelta.setForeground(Color.black);
        labelDelta.setFont(serif12);

        textDelta = new JTextField(10);
        textDelta.setText("5.0");
        textDelta.setFont(serif12);

        labelMaxArea = new JLabel("Maximum region relative area (0-1)");
        labelMaxArea.setForeground(Color.black);
        labelMaxArea.setFont(serif12);

        textMaxArea = new JTextField(10);
        textMaxArea.setText("0.75");
        textMaxArea.setFont(serif12);

        labelMinArea = new JLabel("Minimum region relative area (0-1)");
        labelMinArea.setForeground(Color.black);
        labelMinArea.setFont(serif12);

        textMinArea = new JTextField(10);
        defaultMinArea = 3.0/(image.getExtents()[0] * image.getExtents()[1]);
        textMinArea.setText(String.valueOf(defaultMinArea));
        textMinArea.setFont(serif12);
        
        labelMaxVariation = new JLabel("Maximum absolute region stability (> 0)");
        labelMaxVariation.setForeground(Color.black);
        labelMaxVariation.setFont(serif12);

        textMaxVariation = new JTextField(10);
        textMaxVariation.setText("0.25");
        textMaxVariation.setFont(serif12);


        rotationCheckBox = new JCheckBox("Rotation invariance");
        rotationCheckBox.setFont(serif12);
        rotationCheckBox.setSelected(true);

        scaleCheckBox = new JCheckBox("Scale invariance");
        scaleCheckBox.setFont(serif12);
        scaleCheckBox.setSelected(true);

        final JPanel upperPanel = new JPanel(new GridBagLayout());

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);

       
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelDelta, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textDelta, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelMaxArea, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textMaxArea, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelMinArea, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textMinArea, gbc);
        gbc.gridx = 0;
        gbc.gridy = 5;
        gbc.gridwidth = 2;
        upperPanel.add(rotationCheckBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 6;
        gbc.gridwidth = 2;
        upperPanel.add(scaleCheckBox, gbc);

        imageVOIPanel = new JPanel(new GridBagLayout());
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Image region"));

        imageVOIGroup = new ButtonGroup();
        wholeImage = new JRadioButton("Whole image", true);
        wholeImage.setFont(serif12);
        imageVOIGroup.add(wholeImage);

        VOIRegions = new JRadioButton("VOI region(s)", false);
        VOIRegions.setFont(serif12);
        imageVOIGroup.add(VOIRegions);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.gridwidth = 1;
        gbc.insets = new Insets(0, 0, 0, 0);
        imageVOIPanel.add(wholeImage, gbc);
        gbc.gridy = 1;
        imageVOIPanel.add(VOIRegions, gbc);

        paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        gbc.gridx = 0;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        int ypos = 0;
        gbc.gridy = ypos++;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1;
        paramPanel.add(upperPanel, gbc);
        gbc.gridy = ypos++;
        gbc.gridwidth = 1;
        paramPanel.add(imageVOIPanel, gbc);

        getContentPane().add(paramPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     * 
     * @return <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        
        tmpStr = textDelta.getText();

        if (testParameter(tmpStr, Double.MIN_VALUE, Double.MAX_VALUE)) {
            delta = Double.valueOf(tmpStr).doubleValue();
        } else {
            textDelta.requestFocus();
            textDelta.selectAll();

            return false;
        }
        
        tmpStr = textMaxArea.getText();

        if (testParameter(tmpStr, 0.0, 1.0)) {
            max_area = Double.valueOf(tmpStr).doubleValue();
        } else {
            textMaxArea.requestFocus();
            textMaxArea.selectAll();

            return false;
        }
        
        tmpStr = textMinArea.getText();

        if (testParameter(tmpStr, 0.0, 1.0)) {
            min_area = Double.valueOf(tmpStr).doubleValue();
        } else {
            textMinArea.requestFocus();
            textMinArea.selectAll();

            return false;
        }
        
        return true;

    }

   

}
