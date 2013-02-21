package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmSCDSegmentation;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.ScriptableActionInterface;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageUpdateInterface;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.PanelManager;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.JCheckBox;


/**
 * Dialog to get user input, then call the algorithm. 
 * The user has the option to generate a new image or replace the source image. It should be noted that the
 * algorithms are executed in their own thread.
 *
 * @version  0.1 January 18, 2013
 * @author   William Gandler
 * @see      AlgorithmSCDSegmentaiton
 */
public class JDialogSCDSegmentation extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmSCDSegmentation scdAlgo;

    /** Source image. */
    private ModelImage image;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;
    
    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated
                            // or if the source image is to be replaced
    
    private ButtonGroup destinationGroup;
    
    private JRadioButton newImage;
    
    private JRadioButton replaceImage;

    /** Result image. */
    private ModelImage resultImage = null;
    
    private JPanel parameterPanel;
    
    private JLabel lengthLabel;
    private JTextField lengthText;
    
    private int initialSideLength;
    
    private JLabel classLabel;
    
    private JTextField classText;
    
    private int numClasses;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;
    
    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogSCDSegmentation() { }

    /**
     * Construct the SCD segmentation dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogSCDSegmentation(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
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

        if (algorithm instanceof AlgorithmSCDSegmentation) {
            Preferences.debug("SCD Segmentation Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((scdAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                if (resultImage.isColorImage()) {
                    updateFileInfo(image, resultImage);
                }

                resultImage.clearMask();

                try {
                	openNewFrame(resultImage);
                 //   openNewFrame(resultImage);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registered to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                image.notifyImageDisplayListeners(null, true);
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();

            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        } // if (algorithm instanceof AlgorithmSCDSegmentation)

        

        if (scdAlgo != null) {
        	// save the completion status for later
        	setComplete(algorithm.isCompleted());

            scdAlgo.finalize();
            scdAlgo = null;
        }

        dispose();
    }

    /**
	 * Accessor that returns the image.
	 * 
	 * @return The result image.
	 */
    public ModelImage getResultImage() {
        return resultImage;
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Changes labels based on whether or not check box is checked.
     *
     * @param  event  event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) {

        
    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void legacyLoadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if ((defaultsString != null) && destinationPanel != null) {

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");
                displayLoc = MipavUtil.getInt(st);
                if (displayLoc == REPLACE) {
                    replaceImage.setSelected(true);
                    newImage.setSelected(false);
                }
                else {
                    replaceImage.setSelected(false);
                    newImage.setSelected(true);
                }

                initialSideLength = MipavUtil.getInt(st);
                lengthText.setText(String.valueOf(initialSideLength));
                
                numClasses = MipavUtil.getInt(st);
                classText.setText(String.valueOf(numClasses));
                
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
    public void legacySaveDefaults() {
        String delim = ",";
        String defaultsString = displayLoc + delim;
        defaultsString += initialSideLength + delim;
        defaultsString += numClasses;

        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }

    

    /**
     * Once all the necessary variables are set, call the Bilateral Filter algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_SCD_Seg");
        displayInNewFrame = (displayLoc == NEW);

        if (displayInNewFrame) {

            try {

                // Make result image
                resultImage = new ModelImage(image.getType(), image.getExtents(), name);

                // Make algorithm
                scdAlgo = new AlgorithmSCDSegmentation(resultImage, image, initialSideLength, numClasses);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                scdAlgo.addListener(this);

                createProgressBar(image.getImageName(), scdAlgo);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (scdAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    scdAlgo.run();
                }
            } catch (OutOfMemoryError x) {

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }

                System.gc();
                MipavUtil.displayError("Dialog SCD Segmentation: unable to allocate enough memory");

                return;
            }
        } else {

            try {

                // No need to make new image space because the user has choosen to replace the source image
                // Make the algorithm class
                scdAlgo = new AlgorithmSCDSegmentation(null, image, initialSideLength, numClasses);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                scdAlgo.addListener(this);

                createProgressBar(image.getImageName(), scdAlgo);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);


                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                    ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (scdAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    scdAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                System.gc();
                MipavUtil.displayError("Dialog SCD Segmentation: unable to allocate enough memory");

                return;
            }
        }
       
    }

    /**
     * Perform any actions required after the running of the algorithm is complete.
     */
    protected void doPostAlgorithmActions() {

        if (displayLoc == NEW) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    /**
     * Set up the dialog GUI based on the parameters before running the algorithm as part of a script.
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        displayLoc = scriptParameters.getParams().getInt("display_loc");
        initialSideLength = scriptParameters.getParams().getInt("initial_side_length");
        numClasses = scriptParameters.getParams().getInt("num_classes");
    }

    /**
     * Store the parameters from the dialog to record the execution of this algorithm.
     *
     * @throws  ParserException  If there is a problem creating one of the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.getParams().put(ParameterFactory.newParameter("display_loc", displayLoc));

        scriptParameters.getParams().put(ParameterFactory.newParameter("initial_side_length", initialSideLength));
        scriptParameters.getParams().put(ParameterFactory.newParameter("num_classes", numClasses));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("SCD Segmentation");
        getContentPane().setLayout(new BorderLayout());
        
        parameterPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        parameterPanel.setForeground(Color.black);
        parameterPanel.setBorder(buildTitledBorder("Parameters"));
        lengthLabel = new JLabel("Initial side length (must be odd)");
        lengthLabel.setForeground(Color.black);
        lengthLabel.setFont(serif12);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.gridx = 0;
        gbc.gridy = 0;
        parameterPanel.add(lengthLabel, gbc);
        lengthText = new JTextField("5");
        lengthText.setFont(serif12);
        lengthText.setColumns(5);
        gbc.anchor = GridBagConstraints.EAST;
        gbc.gridx = 1;
        parameterPanel.add(lengthText, gbc);
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridx = 0;
        gbc.gridy = 1;
        classLabel = new JLabel("Number of classes");
        classLabel.setForeground(Color.black);
        classLabel.setFont(serif12);
        parameterPanel.add(classLabel, gbc);
        classText = new JTextField("4");
        classText.setFont(serif12);
        classText.setColumns(5);
        gbc.anchor = GridBagConstraints.EAST;
        gbc.gridx = 1;
        parameterPanel.add(classText, gbc);
        
        destinationPanel = new JPanel(new BorderLayout());
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage, BorderLayout.NORTH);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage, BorderLayout.CENTER);

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        PanelManager paramPanelManager = new PanelManager();
        paramPanelManager.addOnNextLine(parameterPanel);
        paramPanelManager.addOnNextLine(destinationPanel);

        getContentPane().add(paramPanelManager.getPanel(), BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);

        System.gc();
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }
        
        tmpStr = lengthText.getText();
        initialSideLength = Integer.parseInt(tmpStr);

        if (initialSideLength <= 0) {
            MipavUtil.displayError("initial side length must be greater than 0");
            lengthText.requestFocus();
            lengthText.selectAll();

            return false;
        }
        
        if ((initialSideLength % 2) == 0) {
            MipavUtil.displayError("initial side length must be odd");
            lengthText.requestFocus();
            lengthText.selectAll();

            return false;    
        }
        
        tmpStr = classText.getText();
        numClasses = Integer.parseInt(tmpStr);
        
        if (numClasses <= 0) {
            MipavUtil.displayError("Number of classes must be greater than 0");
            classText.requestFocus();
            classText.selectAll();
            
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
                return new String("Algorithms.segmentation (spatial color compactness degree)");
            }

            public String getDescription() {
                return new String("Applies spatial color compactness degree segmentation.");
            }

            public String getDescriptionLong() {
                return new String("Applies spatial color compactness degree segmentation.");
            }

            public String getShortLabel() {
                return new String("SCD Segmentation");
            }

            public String getLabel() {
                return new String("SCD Segmentation");
            }

            public String getName() {
                return new String("SCD Segmentation");
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
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
            table.put(new ParameterInt("initial_side_length", 5));
            table.put(new ParameterInt("num_classes", 4));
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
