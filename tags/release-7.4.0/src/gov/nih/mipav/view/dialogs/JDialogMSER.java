package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmMSER;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.ScriptableActionInterface;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.text.DecimalFormat;

import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;


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

    private JRadioButton pointsOnly;

    private JRadioButton ellipsesOnly;
    
    private JRadioButton pointsAndEllipses;

    private JLabel labelDelta;

    private JTextField textDelta;

    private JLabel labelMaxArea;

    private JTextField textMaxArea;

    private JLabel labelMinArea;

    private JTextField textMinArea;
    
    private JLabel labelMaxVariation;
    
    private JTextField textMaxVariation;
    
    private JLabel labelMinDiversity;
    
    private JTextField textMinDiversity;

    private JCheckBox brightOnDarkCheckBox;

    private JCheckBox darkOnBrightCheckBox;

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
        delta = scriptParameters.getParams().getDouble("del");
        max_area = scriptParameters.getParams().getDouble("maxArea");
        min_area = scriptParameters.getParams().getDouble("minArea");
        max_variation = scriptParameters.getParams().getDouble("maxVariation");
        min_diversity = scriptParameters.getParams().getDouble("minDiversity");
        bright_on_dark = scriptParameters.getParams().getBoolean("brightOnDark");
        dark_on_bright = scriptParameters.getParams().getBoolean("darkOnBright");
        outputVOIType = scriptParameters.getParams().getInt("output_VOI_type");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.getParams().put(ParameterFactory.newParameter("del", delta));
        scriptParameters.getParams().put(ParameterFactory.newParameter("maxArea", max_area));
        scriptParameters.getParams().put(ParameterFactory.newParameter("minArea", min_area));
        scriptParameters.getParams().put(ParameterFactory.newParameter("maxVariation", max_variation));
        scriptParameters.getParams().put(ParameterFactory.newParameter("minDiversity", min_diversity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("brightOnDark", bright_on_dark));
        scriptParameters.getParams().put(ParameterFactory.newParameter("darkOnBright", dark_on_bright));
        scriptParameters.getParams().put(ParameterFactory.newParameter("output_VOI_type", outputVOIType));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
    	double defaultMinArea;
    	DecimalFormat nfe = new DecimalFormat("0.000E0");
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
        textMinArea.setText(nfe.format(defaultMinArea));
        textMinArea.setFont(serif12);
        
        labelMaxVariation = new JLabel("Maximum absolute region stability (> 0)");
        labelMaxVariation.setForeground(Color.black);
        labelMaxVariation.setFont(serif12);

        textMaxVariation = new JTextField(10);
        textMaxVariation.setText("0.25");
        textMaxVariation.setFont(serif12);
        
        labelMinDiversity = new JLabel("Minimum diversity (0-1)");
        labelMinDiversity.setForeground(Color.black);
        labelMinDiversity.setFont(serif12);

        textMinDiversity = new JTextField(10);
        textMinDiversity.setText("0.2");
        textMinDiversity.setFont(serif12);

        brightOnDarkCheckBox = new JCheckBox("Bright on dark regions");
        brightOnDarkCheckBox.setFont(serif12);
        brightOnDarkCheckBox.setSelected(true);

        darkOnBrightCheckBox = new JCheckBox("Dark on bright regions");
        darkOnBrightCheckBox.setFont(serif12);
        darkOnBrightCheckBox.setSelected(true);

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
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelMaxVariation, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textMaxVariation, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelMinDiversity, gbc);
        gbc.gridx = 1;
        gbc.gridy = 4;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textMinDiversity, gbc);
        gbc.gridx = 0;
        gbc.gridy = 5;
        gbc.gridwidth = 2;
        upperPanel.add(brightOnDarkCheckBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 6;
        gbc.gridwidth = 2;
        upperPanel.add(darkOnBrightCheckBox, gbc);

        imageVOIPanel = new JPanel(new GridBagLayout());
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Output VOI Type"));

        imageVOIGroup = new ButtonGroup();
        pointsOnly = new JRadioButton("Points only", false);
        pointsOnly.setFont(serif12);
        imageVOIGroup.add(pointsOnly);

        ellipsesOnly = new JRadioButton("Ellipses only", true);
        ellipsesOnly.setFont(serif12);
        imageVOIGroup.add(ellipsesOnly);
        
        pointsAndEllipses = new JRadioButton("Points and ellipses", false);
        pointsAndEllipses.setFont(serif12);
        imageVOIGroup.add(pointsAndEllipses);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.gridwidth = 1;
        gbc.insets = new Insets(0, 0, 0, 0);
        imageVOIPanel.add(pointsOnly, gbc);
        gbc.gridy = 1;
        imageVOIPanel.add(ellipsesOnly, gbc);
        gbc.gridy = 2;
        imageVOIPanel.add(pointsAndEllipses, gbc);

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
        
        tmpStr = textMaxVariation.getText();

        if (testParameter(tmpStr, Double.MIN_VALUE, Double.MAX_VALUE)) {
            max_variation = Double.valueOf(tmpStr).doubleValue();
        } else {
            textMaxVariation.requestFocus();
            textMaxVariation.selectAll();

            return false;
        }
        
        tmpStr = textMinDiversity.getText();

        if (testParameter(tmpStr, 0.0, 1.0)) {
            min_diversity = Double.valueOf(tmpStr).doubleValue();
        } else {
            textMinDiversity.requestFocus();
            textMinDiversity.selectAll();

            return false;
        }
        
        bright_on_dark = brightOnDarkCheckBox.isSelected();
        
        dark_on_bright = darkOnBrightCheckBox.isSelected();
        
        if (pointsOnly.isSelected()) {
        	outputVOIType = POINTS_ONLY;
        }
        else if (ellipsesOnly.isSelected()) {
        	outputVOIType = ELLIPSES_ONLY;
        }
        else {
        	outputVOIType = POINTS_AND_ELLIPSES;
        }
        
        return true;

    }

   

}
