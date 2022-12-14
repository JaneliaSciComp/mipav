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
 * @version  0.1 September 12, 2016
 * @author   William Gandler
 * @see      AlgorithmLowerCompletion
 */
public class JDialogLowerCompletion extends JDialogScriptableBase
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
    private AlgorithmLowerCompletion lcAlgo;
    
    private int numNeighbor;
    
    private ButtonGroup neighborGroup;
    
    private JRadioButton fourButton;
    
    private JRadioButton eightButton;
    
    private JRadioButton sixButton;
    
    private JRadioButton eighteenButton;
    
    private JRadioButton twentySixButton;
    
    private boolean limitBins;
    
    private JLabel labelBins;
    
    private JTextField textBins;
    
    private JCheckBox binCheckBox;
    
    private int binNumber;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogLowerCompletion() { }


    /**
     * Creates a new JDialogLowerCompletion object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogLowerCompletion(Frame theParentFrame, ModelImage im) {
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
            //MipavUtil.showWebHelp("Segmentation:_Lower_Completion");
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

        if (algorithm instanceof AlgorithmLowerCompletion) {
            System.err.println("Lower Completion elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((lcAlgo.isCompleted() == true) && (resultImage != null)) {
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

        lcAlgo.finalize();
        lcAlgo = null;
        dispose();
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
     * Accessor that sets numNeighbor.
     *
     * @param  numNeighbor
     */
    public void setNumNeighbor(int numNeighbor) {
        this.numNeighbor = numNeighbor;
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
     * Once all the necessary variables are set, call the Lower Completion algorithm.
     */
    protected void callAlgorithm() {
        try {
            
               // Image name set in program
               resultImage = new ModelImage(ModelStorageBase.INTEGER, image.getExtents(), 
                        		image.getImageName() + "_lowerCompletion");
           
           lcAlgo = new AlgorithmLowerCompletion(resultImage, image, numNeighbor, limitBins, binNumber);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            lcAlgo.addListener(this);
            createProgressBar(image.getImageName(), lcAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (lcAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                lcAlgo.run();
            }
            

        } catch (OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal();
                resultImage = null;
            }
            
         // save the completion status for later
            setComplete(lcAlgo.isCompleted());

            System.gc();
            MipavUtil.displayError("Dialog Lower Completion: unable to allocate enough memory");

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

        setNumNeighbor(scriptParameters.getParams().getInt("num_neighbor"));
        setLimitBins(scriptParameters.getParams().getBoolean("limit_bins"));
        setBinNumber(scriptParameters.getParams().getInt("bin_number"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeImageInRecorder(getResultImage());

        scriptParameters.getParams().put(ParameterFactory.newParameter("num_neighbor", numNeighbor));
        scriptParameters.getParams().put(ParameterFactory.newParameter("limit_bins", limitBins));
        scriptParameters.getParams().put(ParameterFactory.newParameter("bin_number", binNumber));
    }

    

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        int ypos = 0;
        setForeground(Color.black);

        setTitle("Lower Completion");
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
        if (image.getNDims() >= 3) {
        	JLabel label3D = new JLabel("3D segmentation neighbors:");
            label3D.setFont(serif12);
            label3D.setForeground(Color.black);
            gbcScale.gridy++;
            neighborPanel.add(label3D, gbcScale);
            sixButton = new JRadioButton("6 nearest neighbors", false);
            sixButton.setFont(serif12);
            sixButton.setForeground(Color.black);
            neighborGroup.add(sixButton);
            gbcScale.gridy++;
            neighborPanel.add(sixButton, gbcScale);
            eighteenButton = new JRadioButton("18 nearest neighbors", false);
            eighteenButton.setFont(serif12);
            eighteenButton.setForeground(Color.black);
            neighborGroup.add(eighteenButton);
            gbcScale.gridy++;
            neighborPanel.add(eighteenButton, gbcScale);
            twentySixButton = new JRadioButton("26 nearest neighbors", false);
            twentySixButton.setFont(serif12);
            twentySixButton.setForeground(Color.black);
            neighborGroup.add(twentySixButton);
            gbcScale.gridy++;
            neighborPanel.add(twentySixButton, gbcScale);
        } // if (image.getNDims() >= 3)
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
        	numNeighbor = 4;
        }
        else if (eightButton.isSelected()) {
        	numNeighbor = 8;
        }
        else if ((sixButton != null) && (sixButton.isSelected())) {
        	numNeighbor = 6;
        }
        else if ((eighteenButton != null) && (eighteenButton.isSelected())) {
        	numNeighbor = 18;
        }
        else {
        	numNeighbor = 26;
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
                return new String("Applies Lower Completion to the image.");
            }

            public String getDescriptionLong() {
                return new String("Applies Lower Completion to the image.");
            }

            public String getShortLabel() {
                return new String("LowerCompletion");
            }

            public String getLabel() {
                return new String("Lower Completion");
            }

            public String getName() {
                return new String("Lower Completion");
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
            table.put(new ParameterInt("num_neighbor", 4));
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
