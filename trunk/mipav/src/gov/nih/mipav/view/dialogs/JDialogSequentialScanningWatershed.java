package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
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
 * @version  0.1 September 14, 2016
 * @author   William Gandler
 * @see      AlgorithmSequentialScanningWatershed
 */
public class JDialogSequentialScanningWatershed extends JDialogScriptableBase
        implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface
     {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------ 

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    
    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JPanel neighborPanel;

    /** DOCUMENT ME! */
    private AlgorithmSequentialScanningWatershed watershedAlgo;
    
    private boolean neighbor8;
    
    private ButtonGroup neighborGroup;
    
    private JRadioButton fourButton;
    
    private JRadioButton eightButton;
    
    private boolean limitBins;
    
    private JLabel labelBins;
    
    private JTextField textBins;
    
    private JCheckBox binCheckBox;
    
    private int binNumber;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogSequentialScanningWatershed() { }


    /**
     * Creates a new JDialogSequentialScanningWatershed object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogSequentialScanningWatershed(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
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
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (source.equals(binCheckBox)) {
        	labelBins.setEnabled(binCheckBox.isSelected());
        	textBins.setEnabled(binCheckBox.isSelected());
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showWebHelp("Segmentation:_Sequential_Scanning_Watershed");
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

        if (algorithm instanceof AlgorithmSequentialScanningWatershed) {
            System.err.println("Sequential Scanning Watershed elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((watershedAlgo.isCompleted() == true) && (resultImage != null)) {
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

        watershedAlgo.finalize();
        watershedAlgo = null;
        dispose();
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
        
        str += neighbor8;
        

        return str;
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void legacyLoadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if (defaultsString != null) {

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");
                neighbor8 =  MipavUtil.getBoolean(st);
                if (!neighbor8) {
                	fourButton.setSelected(true);
                	eightButton.setSelected(false);
                }
                else {
                	fourButton.setSelected(false);
                	eightButton.setSelected(true);		
                }
                limitBins = MipavUtil.getBoolean(st);
                if (limitBins) {
                	binCheckBox.setSelected(true);
                	labelBins.setEnabled(true);
                	textBins.setEnabled(true);
                }
                else {
                	binCheckBox.setSelected(false);
                	labelBins.setEnabled(false);
                	textBins.setEnabled(false);
                }
                binNumber = MipavUtil.getInt(st);
                textBins.setText(String.valueOf(binNumber));
            } catch (Exception ex) {

                // since there was a problem parsing the defaults string, start over with the original defaults
                Preferences.debug("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
            }
        }
    }
    
    /**
     * Accessor that sets neighbor8.
     *
     * @param  neighbor8
     */
    public void setNeighbor8(boolean neighbor8) {
        this.neighbor8 = neighbor8;
    }
    
   
    /**
     * 
     * @param limitBins
     */
    public void setLimitBins(boolean limitBins) {
    	this.limitBins = limitBins;
    }
    
    /**
     * 
     * @param binNumber
     */
    public void setBinNumber(int binNumber) {
    	this.binNumber = binNumber;
    }
    
    

    /**
     * Once all the necessary variables are set, call the SequentialScanning Watershed algorithm.
     */
    protected void callAlgorithm() {
        try {
            
               // Image name set in program
               resultImage = new ModelImage(ModelStorageBase.INTEGER, image.getExtents(), 
                        		image.getImageName() + "_watershed");
           
           watershedAlgo = new AlgorithmSequentialScanningWatershed(resultImage, image, neighbor8, limitBins, binNumber);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            watershedAlgo.addListener(this);
            createProgressBar(image.getImageName(), watershedAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (watershedAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                watershedAlgo.run();
            }
            

        } catch (OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal();
                resultImage = null;
            }
            
         // save the completion status for later
            setComplete(watershedAlgo.isCompleted());

            System.gc();
            MipavUtil.displayError("Dialog SequentialScanning Watershed: unable to allocate enough memory");

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

        setNeighbor8(scriptParameters.getParams().getBoolean("neighbor_8"));
        setLimitBins(scriptParameters.getParams().getBoolean("limit_bins"));
        setBinNumber(scriptParameters.getParams().getInt("bin_number"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeImageInRecorder(getResultImage());

        scriptParameters.getParams().put(ParameterFactory.newParameter("neighbor_8", neighbor8));
        scriptParameters.getParams().put(ParameterFactory.newParameter("limit_bins", limitBins));
        scriptParameters.getParams().put(ParameterFactory.newParameter("bin_number", binNumber));
    }

    

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        int ypos = 0;
        setForeground(Color.black);

        setTitle("Sequential Scanning Watershed");
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

        neighborPanel = new JPanel(new GridBagLayout()); //2 rows x 2 columns
        neighborPanel.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = ypos++;
        gbc.weighty = .1;
        JScrollPane scaleScroll = new JScrollPane(neighborPanel);
        scaleScroll.setBorder(buildTitledBorder("Parameters"));
        scaleScroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        mainPanel.add(scaleScroll, gbc);

        GridBagConstraints gbcScale = new GridBagConstraints();
        gbcScale.gridx = 0;
        gbcScale.gridy = 0;
        gbcScale.fill = GridBagConstraints.HORIZONTAL;
        gbcScale.anchor = GridBagConstraints.WEST;
        gbcScale.insets = new Insets(0, 0, 2, 0);
        neighborGroup = new ButtonGroup();
        fourButton = new JRadioButton("4 nearest neighbors", true);
        fourButton.setFont(serif12);
        fourButton.setForeground(Color.black);
        neighborGroup.add(fourButton);
        gbcScale.gridy++;
        neighborPanel.add(fourButton, gbcScale); 
        eightButton = new JRadioButton("8 nearest neighbors", false);
        eightButton.setFont(serif12);
        eightButton.setForeground(Color.black);
        neighborGroup.add(eightButton);
        gbcScale.gridy++;
        neighborPanel.add(eightButton, gbcScale); 
        binCheckBox = new JCheckBox("Limit bins per frame");
        binCheckBox.setFont(serif12);
        binCheckBox.setForeground(Color.black);
        binCheckBox.setSelected(false);
        binCheckBox.addActionListener(this);
        gbcScale.gridy++;
        neighborPanel.add(binCheckBox, gbcScale);
        labelBins = new JLabel("Bin number per frame");
        labelBins.setFont(serif12);
        labelBins.setForeground(Color.black);
        labelBins.setEnabled(false);
        gbcScale.gridy++;
        neighborPanel.add(labelBins, gbcScale);
        textBins = new JTextField(10);
        textBins.setText("4");
        textBins.setFont(serif12);
        textBins.setForeground(Color.black);
        textBins.setEnabled(false);
        gbcScale.gridx = 1;
        neighborPanel.add(textBins, gbcScale);
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
        
        if (fourButton.isSelected()) {
        	neighbor8 = false;
        }
        else {
        	neighbor8 = true;
        }
        
        limitBins = binCheckBox.isSelected();
        if (limitBins) {
        	tmpStr = textBins.getText();
            binNumber = Integer.parseInt(tmpStr);
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
                return new String("Algorithms.Segmentation");
            }

            public String getDescription() {
                return new String("Applies Sequential Scanning watershed segmentation to the image.");
            }

            public String getDescriptionLong() {
                return new String("Applies Sequential Scanning watershed segmentation to the image.");
            }

            public String getShortLabel() {
                return new String("SequentialScanningWatershedSegmentation");
            }

            public String getLabel() {
                return new String("Sequential Scanning Watershed Segmentation");
            }

            public String getName() {
                return new String("Sequential Scanning Watershed Segmentation");
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
            table.put(new ParameterBoolean("neighbor_8", false));
            table.put(new ParameterBoolean("limit_bins", false));
            table.put(new ParameterInt("bin_number", 4));
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
    		//System.out.println(resultImage.length);
                return resultImage.getImageName();
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
