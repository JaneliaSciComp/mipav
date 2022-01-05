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
 * @version  0.1 July 10, 2014
 * @author   William Gandler
 * @see      AlgorithmLawsTexture
 */
public class JDialogLawsTexture extends JDialogScriptableBase
        implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface
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

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JLabel labelWindowSize;

    
    /** DOCUMENT ME! */
    private ModelImage[] resultImage = null; // result image

    /** DOCUMENT ME! */
    private int resultNumber;

    /** DOCUMENT ME! */
    private JPanel scalePanel;

    /** DOCUMENT ME! */
    private AlgorithmLawsTexture textureAlgo;

    /** DOCUMENT ME! */
    private int windowSize;
    
    private ButtonGroup sizeGroup;
    
    private JRadioButton threeButton;
    
    private JRadioButton fiveButton;
    
    private JRadioButton sevenButton;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogLawsTexture() { }


    /**
     * Creates a new JDialogLawsTexture object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogLawsTexture(Frame theParentFrame, ModelImage im) {
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

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showWebHelp("Filters_(Spatial):_Laws_Texture");
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
        int i;
        ViewJFrameImage[] imageFrame = new ViewJFrameImage[resultNumber];

        if (algorithm instanceof AlgorithmLawsTexture) {
            image.clearMask();

            if ((textureAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                // Take resultImage out of array form or null pointer errors can
                // result in one of the resultImages after another of the resultImages
                // has been deleted.
            	
            	
            	// save the completion status for later
            	setComplete(textureAlgo.isCompleted());

                for (i = 0; i < resultNumber; i++) {
                    updateFileInfo(image, resultImage[i]);
                    resultImage[i].clearMask();
                    
                    try {
                        imageFrame[i] = new ViewJFrameImage(resultImage[i], null, new Dimension(610, 200));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        JOptionPane.showMessageDialog(null, "Out of memory: unable to open new resultImage frame",
                                                      "Error", JOptionPane.ERROR_MESSAGE);
                    }

                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                for (i = 0; i < resultNumber; i++) {

                    if (resultImage[i] != null) {
                        resultImage[i].disposeLocal(); // Clean up memory of result image
                        resultImage[i] = null;
                    }
                }

                resultImage = null;
                System.gc();
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        dispose();

    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage[] getResultImage() {
        return resultImage;
    }

    /**
     * Accessor that sets the window size.
     *
     * @param  windowSize  int
     */
    public void setWindowSize(int windowSize) {
        this.windowSize = windowSize;
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
     * Once all the necessary variables are set, call the Laws feature algorithm.
     */
    protected void callAlgorithm() {
        int i, index;
        
        if (windowSize == 3) {
        	resultNumber = 6;
        }
        else if (windowSize == 5) {
        	resultNumber = 15;
        }
        else {
        	resultNumber = 21;
        }

        try {
            resultImage = new ModelImage[resultNumber];
            
           for (index = 0; index < resultNumber; index++) {
               // Image name set in program
               resultImage[index] = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), 
                        		image.getImageName() + "_" + index);
           }
           
           textureAlgo = new AlgorithmLawsTexture(resultImage, image, windowSize, RGBOffset);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            textureAlgo.addListener(this);
            createProgressBar(image.getImageName(), textureAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (textureAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                textureAlgo.run();
            }
            

        } catch (OutOfMemoryError x) {

            if (resultImage != null) {

                for (i = 0; i < resultNumber; i++) {

                    if (resultImage[i] != null) {
                        resultImage[i].disposeLocal(); // Clean up memory of result image
                        resultImage[i] = null;
                    }
                }

                resultImage = null;
            }
            
         // save the completion status for later
            setComplete(textureAlgo.isCompleted());

            System.gc();
            MipavUtil.displayError("Dialog Laws Texture: unable to allocate enough memory");

            return;
        }

    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        for (int i = 0; i < resultNumber; i++) {
            AlgorithmParameters.storeImageInRunner(getResultImage()[i]);
        }
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
        setWindowSize(scriptParameters.getParams().getInt("window_size"));
        
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        for (int i = 0; i < resultNumber; i++) {
            scriptParameters.storeImageInRecorder(getResultImage()[i]);
        }

        if (image.isColorImage()) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("RGB_offset", RGBOffset));
        }
        scriptParameters.getParams().put(ParameterFactory.newParameter("window_size", windowSize));
    }

    

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        int ypos = 0;
        setForeground(Color.black);

        setTitle("Laws Texture");
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

        scalePanel = new JPanel(new GridBagLayout()); //3 rows x 2 columns
        scalePanel.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = ypos++;
        gbc.weighty = .1;
        JScrollPane scaleScroll = new JScrollPane(scalePanel);
        scaleScroll.setBorder(buildTitledBorder("Sizes"));
        scaleScroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        mainPanel.add(scaleScroll, gbc);

        GridBagConstraints gbcScale = new GridBagConstraints();
        labelWindowSize = new JLabel("Window size ");
        labelWindowSize.setForeground(Color.black);
        labelWindowSize.setFont(serif12);
        gbcScale.gridx = 0;
        gbcScale.gridy = 0;
        gbcScale.fill = GridBagConstraints.HORIZONTAL;
        gbcScale.anchor = GridBagConstraints.WEST;
        gbcScale.insets = new Insets(0, 0, 2, 0);
        scalePanel.add(labelWindowSize, gbcScale);
        sizeGroup = new ButtonGroup();
        threeButton = new JRadioButton("3 X 3", false);
        threeButton.setFont(serif12);
        threeButton.setForeground(Color.black);
        sizeGroup.add(threeButton);
        gbcScale.gridy++;
        scalePanel.add(threeButton, gbcScale); 
        fiveButton = new JRadioButton("5 X 5", true);
        fiveButton.setFont(serif12);
        fiveButton.setForeground(Color.black);
        sizeGroup.add(fiveButton);
        gbcScale.gridy++;
        scalePanel.add(fiveButton, gbcScale);
        sevenButton = new JRadioButton("7 X 7", false);
        sevenButton.setFont(serif12);
        sevenButton.setForeground(Color.black);
        sizeGroup.add(sevenButton);
        gbcScale.gridy++;
        scalePanel.add(sevenButton, gbcScale); 
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
        
        if (image.isColorImage()) {

            if (redButton.isSelected()) {
                RGBOffset = RED_OFFSET;
            } else if (greenButton.isSelected()) {
                RGBOffset = GREEN_OFFSET;
            } else {
                RGBOffset = BLUE_OFFSET;
            }
        } // if (image.isColorImage())
        
        if (threeButton.isSelected()) {
        	windowSize = 3;
        }
        else if (fiveButton.isSelected()) {
        	windowSize = 5;
        }
        else {
        	windowSize = 7;
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
                return new String("Algorithms.Filters (spatial)");
            }

            public String getDescription() {
                return new String("Applies a Laws texture to the image.");
            }

            public String getDescriptionLong() {
                return new String("Applies a Laws texture to the image.");
            }

            public String getShortLabel() {
                return new String("LawsTexture");
            }

            public String getLabel() {
                return new String("Laws Texture");
            }

            public String getName() {
                return new String("Laws Texture");
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
            table.put(new ParameterInt("window_size", 7));
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
                return resultImage[0].getImageName();
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
