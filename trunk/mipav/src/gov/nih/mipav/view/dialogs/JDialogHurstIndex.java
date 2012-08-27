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
 * @version  0.1 June 25, 2012
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
    
    private double minDistance = 1;
    
    private double maxDistance = 7;
    
    /** If true, take rounding of Euclidean distance as distance
     *  If false, take Euclidean distance as distance
     */
    private boolean integerDistanceRound = true;
    
    private JLabel labelDimensionality;
    
    private JTextField textMinDistance;
    
    private JLabel labelMinDistance;
    
    private JTextField textMaxDistance;
    
    private JLabel labelMaxDistance;
    
    private JCheckBox integerDistanceRoundCheckBox;
    
    /** DOCUMENT ME! */
    private JPanel distancePanel;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private AlgorithmHurstIndex hurstAlgo;
    
    private ButtonGroup pixelOrVOIGroup;
    
    private JRadioButton pixelButton;
    
    private JRadioButton voiButton;
    
    private JRadioButton sliceButton;
    
    private static final int PIXEL_GROUPING = 1;
    
    private static final int VOI_GROUPING = 2;
    
    private static final int SLICE_GROUPING = 3;
    
    private int grouping = PIXEL_GROUPING;
    
    private ModelImage resultImage = null;

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

        if (algorithm instanceof AlgorithmHurstIndex) {
            image.clearMask();
            
            if ((hurstAlgo.isCompleted()) && (resultImage != null) && (grouping == PIXEL_GROUPING)) {

                // The algorithm has completed and produced a new image to be displayed.
                // Take resultImage out of array form or null pointer errors can
                // result in one of the resultImages after another of the resultImages
                // has been deleted.
                
                
                // save the completion status for later
                setComplete(hurstAlgo.isCompleted());

                updateFileInfo(image, resultImage);
                resultImage.clearMask();
                
                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    JOptionPane.showMessageDialog(null, "Out of memory: unable to open new resultImage frame",
                                                  "Error", JOptionPane.ERROR_MESSAGE);
                }

            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
                System.gc();
            }
            else if ((hurstAlgo.isCompleted()) && (grouping != PIXEL_GROUPING)) {

                
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
        str += integerDistanceRound + delim;
        str += grouping;

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
                integerDistanceRoundCheckBox.setSelected(MipavUtil.getBoolean(st));
                grouping = (MipavUtil.getInt(st));
                pixelButton.setSelected(grouping == PIXEL_GROUPING);
                voiButton.setSelected(grouping == VOI_GROUPING);
                sliceButton.setSelected(grouping == SLICE_GROUPING);
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
     * If true, the rounding of the Euclidean distance is taken as the distance
     * If false, the Euclidean distance is taken as the distance
     * @param integerDistanceRound
     */
    public void setIntegerDistanceRound(boolean integerDistanceRound) {
        this.integerDistanceRound = integerDistanceRound;
    }

    
    /**
     * Accessor that sets the RGBOffset.
     *
     * @param  RGBoffset  DOCUMENT ME!
     */
    public void setRGBOffset(int RGBoffset) {
        this.RGBOffset = RGBoffset;
    }
    
    public void setGrouping(int grouping) {
        this.grouping = grouping;
    }
    
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Once all the necessary variables are set, call the Hurst Index algorithm.
     */
    protected void callAlgorithm() {

        try {
           
            if (grouping == PIXEL_GROUPING) {
                String name =  makeImageName(image.getImageName(), "_Hurst");
                resultImage = new ModelImage(ModelStorageBase.DOUBLE, image.getExtents(), name);
                if (image.isColorImage()) {
                    hurstAlgo = new AlgorithmHurstIndex(resultImage, image, RGBOffset, minDistance, maxDistance,
                                                        integerDistanceRound);    
                }
                else {
                    hurstAlgo = new AlgorithmHurstIndex(resultImage, image, minDistance, maxDistance, integerDistanceRound);
                }    
            }
            else {
                if (image.isColorImage()) {
                    hurstAlgo = new AlgorithmHurstIndex(image, RGBOffset, minDistance, maxDistance,
                                                        integerDistanceRound, grouping);    
                }
                else {
                    hurstAlgo = new AlgorithmHurstIndex(image, minDistance, maxDistance, integerDistanceRound, grouping);
                }
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
            
            if (resultImage != null) {
                resultImage.disposeLocal();
                resultImage = null;
            }
            
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
        AlgorithmParameters.storeImageInRunner(getResultImage());
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
        setIntegerDistanceRound(scriptParameters.getParams().getBoolean("integer_distance_round"));
        setGrouping(scriptParameters.getParams().getInt("group"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        if (grouping == PIXEL_GROUPING) {
            scriptParameters.storeImageInRecorder(getResultImage());
        }

        if (image.isColorImage()) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("RGB_offset", RGBOffset));
        }
        scriptParameters.getParams().put(ParameterFactory.newParameter("min_distance", minDistance));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_distance", maxDistance));
        scriptParameters.getParams().put(ParameterFactory.newParameter("integer_distance_round", integerDistanceRound));
        scriptParameters.getParams().put(ParameterFactory.newParameter("group", grouping));
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

        distancePanel = new JPanel(new GridBagLayout()); //6 rows x 2 columns
        distancePanel.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = ypos++;
        gbc.weighty = .1;
        JScrollPane scaleScroll = new JScrollPane(distancePanel);
        scaleScroll.setBorder(buildTitledBorder("Distances"));
        scaleScroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        mainPanel.add(scaleScroll, gbc);

        GridBagConstraints gbcScale = new GridBagConstraints();
        
        // First row
        labelDimensionality = new JLabel("Dimensionality = 3.0 - Hurst index ");
        labelDimensionality.setForeground(Color.black);
        labelDimensionality.setFont(serif12);
        gbcScale.gridx = 0;
        gbcScale.gridy = 0;
        gbcScale.gridwidth = 2;
        gbcScale.fill = GridBagConstraints.HORIZONTAL;
        gbcScale.anchor = GridBagConstraints.WEST;
        gbcScale.insets = new Insets(0, 0, 2, 0);
        distancePanel.add(labelDimensionality, gbcScale);
        
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
        textMinDistance.setText(String.valueOf(minDistance));
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
        textMaxDistance.setText(String.valueOf(maxDistance));
        textMaxDistance.setFont(serif12);
        textMaxDistance.setForeground(Color.black);
        gbcScale.gridx++;
        gbcScale.insets = new Insets(0, 4, 2, 0);
        distancePanel.add(textMaxDistance, gbcScale);
        
        //Fourth row
        integerDistanceRoundCheckBox = new JCheckBox("Take rounding of Euclidean distance as distance", true);
        integerDistanceRoundCheckBox.setForeground(Color.black);
        integerDistanceRoundCheckBox.setFont(serif12);
        gbcScale.gridwidth = 2;
        gbcScale.gridx = 0;
        gbcScale.gridy++;
        gbcScale.insets = new Insets(0, 0, 2, 0);
        distancePanel.add(integerDistanceRoundCheckBox, gbcScale);
        
        pixelOrVOIGroup = new ButtonGroup();
        // Fifth row
        pixelButton = new JRadioButton("Calculate Hurst index for every pixel", true);
        pixelButton.setForeground(Color.black);
        pixelButton.setFont(serif12);
        pixelOrVOIGroup.add(pixelButton);
        gbcScale.gridx = 0;
        gbcScale.gridy++;
        gbcScale.insets = new Insets(0, 0, 2, 0);
        distancePanel.add(pixelButton, gbcScale);
        
        // Sixth row
        voiButton = new JRadioButton("Cacluate Hurst index for every voi", false);
        voiButton.setForeground(Color.black);
        voiButton.setFont(serif12);
        pixelOrVOIGroup.add(voiButton);
        gbcScale.gridx = 0;
        gbcScale.gridy++;
        gbcScale.insets = new Insets(0, 0, 2, 0);
        distancePanel.add(voiButton, gbcScale);
        
        sliceButton = new JRadioButton("Cacluate Hurst index for every slice", false);
        sliceButton.setForeground(Color.black);
        sliceButton.setFont(serif12);
        pixelOrVOIGroup.add(sliceButton);
        gbcScale.gridx = 0;
        gbcScale.gridy++;
        gbcScale.insets = new Insets(0, 0, 2, 0);
        distancePanel.add(sliceButton, gbcScale);
        
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
        int nBoundingVOIs = 0;
        
        if (pixelButton.isSelected()) {
            grouping = PIXEL_GROUPING;
        }
        else if (voiButton.isSelected()) {
            grouping = VOI_GROUPING;
        }
        else {
            grouping = SLICE_GROUPING;
        }
        
        if (grouping == VOI_GROUPING) {
            ViewVOIVector VOIs = image.getVOIs();
            if (VOIs != null) {
                int nVOIs = VOIs.size();
                
                for (i = 0; i < nVOIs; i++) {
        
                    if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                        nBoundingVOIs++;
                    }
                }
            } // if VOIs != null)
            if (nBoundingVOIs == 0) {
                MipavUtil.displayError("Must have at least 1 contour or polyline VOI");
                return false;
            }
        } // if (grouping == VOI_GROUPING)
        
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
        
        integerDistanceRound = integerDistanceRoundCheckBox.isSelected();
        
        
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
            table.put(new ParameterDouble("min_distance", 1.0));
            table.put(new ParameterDouble("max_distance", 7.0));
            table.put(new ParameterBoolean("integer_distance_round", true));
            table.put(new ParameterInt("group", PIXEL_GROUPING));
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
