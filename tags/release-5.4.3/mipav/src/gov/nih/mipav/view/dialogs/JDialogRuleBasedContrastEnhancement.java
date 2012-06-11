package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.IOException;

import java.util.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class JDialogRuleBasedContrastEnhancement extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image
    
    private JTextField textgmin;
    
    private double gmin;

    /** DOCUMENT ME! */
    private JTextField textgmid;

    /** DOCUMENT ME! */
    private double gmid;
    
    private JTextField textgmax;
    
    private double gmax;

    /** DOCUMENT ME! */
    private String[] titles;


    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private AlgorithmRuleBasedContrastEnhancement rbceAlgo;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRuleBasedContrastEnhancement() { }

    /**
     * Creates new dialog for entering parameters for wavelet threshold.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogRuleBasedContrastEnhancement(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
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

        if (algorithm instanceof AlgorithmRuleBasedContrastEnhancement) {
            System.err.println("Rule Base Contrast Enhancement Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((rbceAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                try {

                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
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
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
     // save the completion status for later
        setComplete(algorithm.isCompleted());

        rbceAlgo.finalize();
        rbceAlgo = null;
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
     * Accessor that sets the display loc variable to new, so that a new image is created once the algorithm completes.
     */
    public void setDisplayLocNew() {
        displayLoc = NEW;
    }

    /**
     * Accessor that sets the display loc variable to replace, so the current image is replaced once the algorithm
     * completes.
     */
    public void setDisplayLocReplace() {
        displayLoc = REPLACE;
    }

    /**
     * Accessor that sets the gmid.
     *
     * @param  gmid  DOCUMENT ME!
     */
    public void setgmid(double gmid) {
        this.gmid = gmid;
    }
    
    /**
     * Once all the necessary variables are set, call the rule based contrast enhancement algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_contrastEnhancement");
        int zDim;
        int newType;
        

            if (displayLoc == NEW) {

                try {
                	newType = image.getType();
                    if (newType == ModelStorageBase.DOUBLE) {
                    	
                    }
                    else if (newType == ModelStorageBase.FLOAT) {
                    	if ((gmin < -Float.MAX_VALUE) || (gmax > Float.MAX_VALUE)) {
                    		newType = ModelStorageBase.DOUBLE;
                    	}
                    }
                    else if ((gmin < Long.MIN_VALUE) || (gmax > Long.MAX_VALUE)) {
                    	newType = ModelStorageBase.DOUBLE;
                    }
                    else if (newType == ModelStorageBase.LONG) {
                    	
                    }
                    else if (newType == ModelStorageBase.UINTEGER) {
                    	if ((gmin < 0) || (gmax > 4294967295L)) {
                    	    newType = ModelStorageBase.LONG;
                    	}
                    }
                    else if ((gmin < Integer.MIN_VALUE) || (gmax > Integer.MAX_VALUE)) {
                    	newType = ModelStorageBase.LONG;
                    }
                    else if (newType == ModelStorageBase.INTEGER) {
                    	
                    }
                    else if (newType == ModelStorageBase.USHORT) {
                    	if ((gmin < 0) || (gmax > 65535)) {
                    		newType = ModelStorageBase.INTEGER;
                    	}
                    }
                    else if ((gmin < -32768) || (gmax > 32767)) {
                    	newType = ModelStorageBase.INTEGER;
                    }
                    else if (newType == ModelStorageBase.SHORT) {
                    	
                    }
                    else if (newType == ModelStorageBase.UBYTE) {
                    	if ((gmin < 0) || (gmax > 255)) {
                    		newType = ModelStorageBase.SHORT;
                    	}
                    }
                    else if (newType == ModelStorageBase.BYTE) {
                    	if ((gmin < -128) || (gmax > 127)) {
                    		newType = ModelStorageBase.SHORT;
                    	}
                    }

                    resultImage     = new ModelImage(newType, image.getExtents(), name);
                    if (resultImage.getNDims() >= 3) {
                    	zDim = resultImage.getExtents()[2];
                    }
                    else {
                    	zDim = 1;
                    }

                    if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                    	for (int i = 0; i < zDim; i++) {
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                        }
                    }

                    // Make algorithm
                    rbceAlgo = new AlgorithmRuleBasedContrastEnhancement(resultImage, image, gmin, gmid, gmax);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    rbceAlgo.addListener(this);

                    createProgressBar(image.getImageName(), rbceAlgo);

                    setVisible(false); // Hide dialog

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (rbceAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        rbceAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Rule Based Contrast Enhancement: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }

                    return;
                }
            } else { // displayLoc == REPLACE

                try {

                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    rbceAlgo = new AlgorithmRuleBasedContrastEnhancement(image, gmin, gmid, gmax);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    rbceAlgo.addListener(this);
                    createProgressBar(image.getImageName(), rbceAlgo);

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
                        if (rbceAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        rbceAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Rule Based Contrast Enhancement: unable to allocate enough memory");

                    return;
                }
            }
        
    }

    

    /**
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {

        if (displayLoc == NEW) {
            AlgorithmParameters.storeImageInRunner(resultImage);
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();

        if (scriptParameters.doOutputNewImage()) {
            this.setDisplayLocNew();
        } else {
            this.setDisplayLocReplace();
        }

        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        gmin = scriptParameters.getParams().getDouble("gmin");
        gmid = scriptParameters.getParams().getDouble("gmid");
        gmax = scriptParameters.getParams().getDouble("gmax");

    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("gmin", gmin));
        scriptParameters.getParams().put(ParameterFactory.newParameter("gmid", gmid));
        scriptParameters.getParams().put(ParameterFactory.newParameter("gmax", gmax));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Rule Based Contrast Enhancement");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(buildTitledBorder("Parameters"));
        
        JLabel labelgmin = new JLabel("New image min ( <= " + image.getMin() + ")");
        labelgmin.setForeground(Color.black);
        labelgmin.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        paramPanel.add(labelgmin, gbc);

        textgmin = new JTextField(10);
        textgmin.setText("");
        textgmin.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textgmin, gbc);

        JLabel labelgmid = new JLabel("gray level at which ugray = 1.0 (> " + image.getMin() + " to < " + image.getMax() + ")");
        labelgmid.setForeground(Color.black);
        labelgmid.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 1;
        paramPanel.add(labelgmid, gbc);

        textgmid = new JTextField(10);
        textgmid.setText(String.valueOf((image.getMin() + image.getMax())/2.0));
        textgmid.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textgmid, gbc);
        
        JLabel labelgmax = new JLabel("New image max ( >= " + image.getMax() + ")");
        labelgmax.setForeground(Color.black);
        labelgmax.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 2;
        paramPanel.add(labelgmax, gbc);

        textgmax = new JTextField(10);
        textgmax.setText("");
        textgmax.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textgmax, gbc);

        JPanel destinationPanel = new JPanel(new GridLayout(2, 1));
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        ButtonGroup destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage);

        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        mainPanel.add(paramPanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(destinationPanel, gbc);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        getContentPane().add(mainPanel);
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

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }
        
        tmpStr = textgmin.getText();

        if (testParameter(tmpStr, -Double.MAX_VALUE, image.getMin())) {
            gmin = Double.valueOf(tmpStr).doubleValue();
        } else {
            textgmin.requestFocus();
            textgmin.selectAll();

            return false;
        }

        tmpStr = textgmid.getText();

        if (testParameter(tmpStr, image.getMin(), image.getMax())) {
            gmid = Double.valueOf(tmpStr).doubleValue();
            if ((gmid == image.getMin()) || (gmid == image.getMax())) {
            	textgmid.requestFocus();
                textgmid.selectAll();

                return false;	
            }
        } else {
            textgmid.requestFocus();
            textgmid.selectAll();

            return false;
        }
        
        tmpStr = textgmax.getText();

        if (testParameter(tmpStr, image.getMax(), Double.MAX_VALUE)) {
            gmax = Double.valueOf(tmpStr).doubleValue();
        } else {
            textgmax.requestFocus();
            textgmax.selectAll();

            return false;
        }
        
        if ((gmin == image.getMin())  && (gmax == image.getMax())) {
        	MipavUtil.displayError("Must have either gmin < " + image.getMin() + " or gmax > " + image.getMax());
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
                return new String("Algorithms.Filters (rule based contrast enhancement)");
            }

            public String getDescription() {
                return new String("Applies a rule based contrast enhancement.");
            }

            public String getDescriptionLong() {
                return new String("Applies a rule based contrast enhancement.");
            }

            public String getShortLabel() {
                return new String("RuleBasedContrastEnhancement");
            }

            public String getLabel() {
                return new String("Rule Based Contrast Enhancement");
            }

            public String getName() {
                return new String("Rule Based Contrast Enahncement");
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
            table.put(new ParameterDouble("gmin", -Double.MAX_VALUE));
            table.put(new ParameterDouble("gmid", 0.0));
            table.put(new ParameterDouble("gmax", Double.MAX_VALUE));
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
