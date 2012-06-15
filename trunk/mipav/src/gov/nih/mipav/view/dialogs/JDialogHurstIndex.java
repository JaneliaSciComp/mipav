package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm.
 *
 * @version  0.1 June 14, 2012
 * @author   William Gandler
 * @see      AlgorithmHurstIndex
 */
public class JDialogHurstIndex extends JDialogScriptableBase
        implements AlgorithmInterface, DialogDefaultsInterface, ScriptableActionInterface
     {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;
    
    /** Red channel. */
    private static final int RED_OFFSET = 1;

    /** Green channel. */
    private static final int GREEN_OFFSET = 2;

    /** Blue channel. */
    private static final int BLUE_OFFSET = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    /** DOCUMENT ME! */
    private JPanel colorPanel;
    
    /** DOCUMENT ME! */
    private ButtonGroup colorGroup;
    
    /** DOCUMENT ME! */
    private JRadioButton redButton;
    
    /** DOCUMENT ME! */
    private JRadioButton greenButton;
    
    /** DOCUMENT ME! */
    private JRadioButton blueButton;
    
    /** DOCUMENT ME! */
    private int RGBOffset = RED_OFFSET;
    
    // Values of 4 and 64 for minDistance and maxDistance obtained from article 
    // "Sonographic Texture Characterization of Salivary Gland Tumors by Fractal Analysis"
    private double minDistance = 4;
    
    private double maxDistance = 64;
    
    /** If true, take integer part of Euclidean distance as distance
     *  If false, take Euclidean distance as distance
     */
    private boolean integerDistancePart = true;
    
    private JLabel labelVOI;
    
    private JTextField textMinDistance;
    
    private JLabel labelMinDistance;
    
    private JTextField textMaxDistance;
    
    private JLabel labelMaxDistance;
    
    private JCheckBox integerDistancePartCheckBox;
    
    /** DOCUMENT ME! */
    private JPanel distancePanel;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private AlgorithmHurstIndex hurstAlgo;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogHurstIndex() { }


    /**
     * Creates a new JDialogHurstIndex object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogHurstIndex(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
        loadDefaults();
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
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
            //MipavUtil.showHelp("");
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

        if (algorithm instanceof AlgorithmHurstIndex) {
            image.clearMask();

            if (hurstAlgo.isCompleted() == true) {

                
            	// save the completion status for later
            	setComplete(hurstAlgo.isCompleted()); 
                
            } 

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }

        dispose();
        }

    }

    /**
     * Construct a delimited string that contains the parameters to this algorithm.
     *
     * @param   delim  the parameter delimiter (defaults to " " if empty)
     *
     * @return  the parameter string
     */
    public String getParameterString(String delim) {

        if (delim.equals("")) {
            delim = " ";
        }

        String str = new String();
        if (image.isColorImage()) {
            str += RGBOffset + delim;
        }
        str += minDistance + delim;
        str += maxDistance + delim;
        str += integerDistancePart;

        return str;
    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if (defaultsString != null) {

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");
                textMinDistance.setText("" + MipavUtil.getDouble(st));
                textMaxDistance.setText("" + MipavUtil.getDouble(st));
                integerDistancePartCheckBox.setSelected(MipavUtil.getBoolean(st));
            } catch (Exception ex) {

                // since there was a problem parsing the defaults string, start over with the original defaults
                Preferences.debug("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
            }
        }
    }

    /**
     * Saves the default settings into the Preferences file.
     */
    public void saveDefaults() {
        String defaultsString = new String(getParameterString(","));
        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }
    
    /**
     * 
     * @param minDistance
     */
    public void setMinDistance(double minDistance) {
        this.minDistance = minDistance;
    }
    
    /**
     * 
     * @param maxDistance
     */
    public void setMaxDistance(double maxDistance) {
        this.maxDistance = maxDistance;
    }
    
    /**
     * If true, the integer part of the Euclidean distance is taken as the distance
     * If false, the Euclidean distance is taken as the distance
     * @param integerDistancePart
     */
    public void setIntegerDistancePart(boolean integerDistancePart) {
        this.integerDistancePart = integerDistancePart;
    }

    
    /**
     * Accessor that sets the RGBOffset.
     *
     * @param  RGBoffset  DOCUMENT ME!
     */
    public void setRGBOffset(int RGBoffset) {
        this.RGBOffset = RGBoffset;
    }

    /**
     * Once all the necessary variables are set, call the Hurst Index algorithm.
     */
    protected void callAlgorithm() {

        try {
           
            if (image.isColorImage()) {
                hurstAlgo = new AlgorithmHurstIndex(image, RGBOffset, minDistance, maxDistance,
                                                    integerDistancePart);    
            }
            else {
                hurstAlgo = new AlgorithmHurstIndex(image, minDistance, maxDistance, integerDistancePart);
            }

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            hurstAlgo.addListener(this);
            createProgressBar(image.getImageName(), hurstAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (hurstAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                hurstAlgo.run();
            }
            

        } catch (OutOfMemoryError x) {
            
         // save the completion status for later
            setComplete(hurstAlgo.isCompleted());

            System.gc();
            MipavUtil.displayError("Dialog Hurst Index: unable to allocate enough memory");

            return;
        }

    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();

        if (image.isColorImage() && scriptParameters.getParams().containsParameter("RGB_offset")) {
            RGBOffset = scriptParameters.getParams().getInt("RGB_offset");
        } else if (image.isColorImage()) {
            throw new ParameterException("RGB_offset",
                                         "This parameter (RGB_offset) is required for the processing of color images.  Please re-record this script using a color image.");
        }
        setMinDistance(scriptParameters.getParams().getDouble("min_distance"));
        setMaxDistance(scriptParameters.getParams().getInt("max_distance"));
        setIntegerDistancePart(scriptParameters.getParams().getBoolean("integer_distance_part"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        if (image.isColorImage()) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("RGB_offset", RGBOffset));
        }
        scriptParameters.getParams().put(ParameterFactory.newParameter("min_distance", minDistance));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_distance", maxDistance));
        scriptParameters.getParams().put(ParameterFactory.newParameter("integer_distance_part", integerDistancePart));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        int ypos = 0;
        setForeground(Color.black);

        setTitle("Hurst Index");
        getContentPane().setLayout(new BorderLayout());

        JPanel mainPanel;

        mainPanel = new JPanel();
        mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        mainPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.BOTH;
        
        if (image.isColorImage()) {
            colorPanel = new JPanel(new GridLayout(3, 1));
            colorPanel.setForeground(Color.black);

            colorGroup = new ButtonGroup();
            redButton = new JRadioButton("Red", true);
            redButton.setFont(serif12);
            redButton.setForeground(Color.black);
            redButton.addActionListener(this);
            colorGroup.add(redButton);
            colorPanel.add(redButton);

            greenButton = new JRadioButton("Green", false);
            greenButton.setFont(serif12);
            greenButton.setForeground(Color.black);
            greenButton.addActionListener(this);
            colorGroup.add(greenButton);
            colorPanel.add(greenButton);

            blueButton = new JRadioButton("Blue", false);
            blueButton.setFont(serif12);
            blueButton.setForeground(Color.black);
            blueButton.addActionListener(this);
            colorGroup.add(blueButton);
            colorPanel.add(blueButton);
            gbc.gridx = 0;
            gbc.gridy = ypos++;
            gbc.weighty = .1;
            JScrollPane colorScroll = new JScrollPane(colorPanel);
            colorScroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
            colorScroll.setBorder(buildTitledBorder("Colors"));
            mainPanel.add(colorScroll, gbc);   
        } // if (image.isColorImage())

        distancePanel = new JPanel(new GridBagLayout()); //4 rows x 2 columns
        distancePanel.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = ypos++;
        gbc.weighty = .1;
        JScrollPane scaleScroll = new JScrollPane(distancePanel);
        scaleScroll.setBorder(buildTitledBorder("Distances"));
        scaleScroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        mainPanel.add(scaleScroll, gbc);

        GridBagConstraints gbcScale = new GridBagConstraints();
        //first row
        labelVOI = new JLabel("Finds distances for Hurst index within each VOI");
        labelVOI.setForeground(Color.black);
        labelVOI.setFont(serif12);
        gbcScale.gridx = 0;
        gbcScale.gridy = 0;
        gbcScale.gridwidth = 2;
        gbcScale.gridwidth = 1;
        gbcScale.fill = GridBagConstraints.HORIZONTAL;
        gbcScale.anchor = GridBagConstraints.WEST;
        gbcScale.insets = new Insets(0, 0, 2, 0);
        distancePanel.add(labelVOI, gbcScale);
        
        // Second row
        labelMinDistance = new JLabel("Minimum pixel distance (>= 1.0): ");
        labelMinDistance.setForeground(Color.black);
        labelMinDistance.setFont(serif12);
        gbcScale.gridx = 0;
        gbcScale.gridy++;
        gbcScale.gridwidth = 1;
        gbcScale.fill = GridBagConstraints.HORIZONTAL;
        gbcScale.anchor = GridBagConstraints.WEST;
        gbcScale.insets = new Insets(0, 0, 2, 0);
        distancePanel.add(labelMinDistance, gbcScale);
        textMinDistance = new JTextField(10);
        textMinDistance.setText("4.0");
        textMinDistance.setFont(serif12);
        textMinDistance.setForeground(Color.black);
        gbcScale.gridx++;
        gbcScale.insets = new Insets(0, 4, 2, 0);
        distancePanel.add(textMinDistance, gbcScale);

        //third row
        labelMaxDistance = new JLabel("Maximum pixel distance: ");
        labelMaxDistance.setForeground(Color.black);
        labelMaxDistance.setFont(serif12);
        gbcScale.gridx = 0;
        gbcScale.gridy++;
        gbcScale.insets = new Insets(0, 0, 2, 0);
        distancePanel.add(labelMaxDistance, gbcScale);
        textMaxDistance = new JTextField(10);
        textMaxDistance.setText("64.0");
        textMaxDistance.setFont(serif12);
        textMaxDistance.setForeground(Color.black);
        gbcScale.gridx++;
        gbcScale.insets = new Insets(0, 4, 2, 0);
        distancePanel.add(textMaxDistance, gbcScale);
        
        //fourth row
        integerDistancePartCheckBox = new JCheckBox("Take integer part of Euclidean distance as distance", true);
        integerDistancePartCheckBox.setForeground(Color.black);
        integerDistancePartCheckBox.setFont(serif12);
        gbcScale.gridwidth = 2;
        gbcScale.gridx = 0;
        gbcScale.gridy++;
        gbcScale.insets = new Insets(0, 0, 2, 0);
        distancePanel.add(integerDistancePartCheckBox, gbcScale);
        
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);

        // setVisible( true );

        System.gc();
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************


    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        int i;
        
        ViewVOIVector VOIs = image.getVOIs();
        int nVOIs = VOIs.size();
        int nBoundingVOIs = 0;
        
        for (i = 0; i < nVOIs; i++) {

            if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                nBoundingVOIs++;
            }
        }
        if (nBoundingVOIs == 0) {
            MipavUtil.displayError("Must have at least 1 contour of polyline VOI");
            return false;
        }
        
        if (image.isColorImage()) {

            if (redButton.isSelected()) {
                RGBOffset = RED_OFFSET;
            } else if (greenButton.isSelected()) {
                RGBOffset = GREEN_OFFSET;
            } else {
                RGBOffset = BLUE_OFFSET;
            }
        } // if (image.isColorImage())
        
        double maxXDistance = image.getExtents()[0] - 1;
        double maxYDistance = image.getExtents()[1] - 1;
        double maxPossibleDistance = Math.sqrt(maxXDistance*maxXDistance + maxYDistance*maxYDistance);

        tmpStr = textMinDistance.getText();
        if (testParameter(tmpStr, 1.0, maxPossibleDistance)) {
            minDistance = Double.valueOf(tmpStr).doubleValue();
        } else {
            textMinDistance.requestFocus();
            textMinDistance.selectAll();

            return false;
        }
        
        tmpStr = textMaxDistance.getText();
        if (testParameter(tmpStr, minDistance + Double.MIN_VALUE, maxPossibleDistance)) {
            maxDistance = Double.valueOf(tmpStr).doubleValue();
        } else {
            textMaxDistance.requestFocus();
            textMaxDistance.selectAll();

            return false;
        }
        
        integerDistancePart = integerDistancePartCheckBox.isSelected();
        
        
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
                return new String("Algorithms.Filters (spatial)");
            }

            public String getDescription() {
                return new String("Applies a Hurst index to the image.");
            }

            public String getDescriptionLong() {
                return new String("Applies a Hurst index to the image.");
            }

            public String getShortLabel() {
                return new String("HurstIndex");
            }

            public String getLabel() {
                return new String("Hurst Index");
            }

            public String getName() {
                return new String("Hurst Index");
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
            table.put(new ParameterInt("RGB_offset",1));
            table.put(new ParameterDouble("min_distance", 4.0));
            table.put(new ParameterDouble("max_distance", 64.0));
            table.put(new ParameterBoolean("integer_distance_part", true));
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
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
    }

}
