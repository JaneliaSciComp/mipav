package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterBoolean;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Simple dialog to invert an image - substitute light intensities for dark intensities and dark intensities for light
 * intensities.
 *
 * @version  1.0 May 24, 2005
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class JDialogInvert extends JDialogScriptableBase implements AlgorithmInterface, ItemListener, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 6770504320836953902L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmChangeType changeTypeAlgo;
    
    private AlgorithmImageMath imageMathAlgo;

    /** DOCUMENT ME! */
    private int dataType;

    /** DOCUMENT ME! */
    private int displayLoc;
    // Flag indicating if a new image is to be generated
    // or if the source image is to be replaced

    /** DOCUMENT ME! */
    private boolean endianess;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private double inTempMin, inTempMax;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private double outTempMin, outTempMax;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogInvert() { }

    /**
     * Creates new dialog for converting type of image.
     *
     * @param  theParentFrame  Parent frame.
     * @param  _image          Source image.
     */
    public JDialogInvert(Frame theParentFrame, ModelImage _image) {
        super(theParentFrame, false);
        image = _image;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and sets the variables.
     *
     * @param  event  Event that triggers this function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            cancelFlag = true;
            dispose();
        } else if (command.equals("Help")) {
        	MipavUtil.showHelp("U4035");
        }

    } // end actionPerformed()

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmChangeType) {

            if ((changeTypeAlgo.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);

                // The algorithm has completed and produced a new image to be displayed.
                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
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
            }
        }
        
        if (algorithm instanceof AlgorithmImageMath) {

            if ((imageMathAlgo.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);

                // The algorithm has completed and produced a new image to be displayed.
                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
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
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        // Update frame
        // ((ViewJFrameBase)parentFrame).updateImages(true);
     // save the completion status for later
        setComplete(algorithm.isCompleted());

        if (changeTypeAlgo != null) {
            changeTypeAlgo.finalize();
            changeTypeAlgo = null;
        }
        if (imageMathAlgo != null) {
        	imageMathAlgo.finalize();
        	imageMathAlgo = null;
        }
        dispose();
        System.gc();
    }

    /**
     * Accessor that returns the data type.
     *
     * @return  the data type
     */
    public int getDataType() {
        return dataType;
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
     * Sets the flags for the checkboxes and resets labels.
     *
     * @param  event  Event that triggered this function.
     */
    public synchronized void itemStateChanged(ItemEvent event) { }

    /**
     * Accessor that sets the data type for what the converted image is to be.
     *
     * @param  type  New data type.
     */
    public void setDataType(int type) {
        dataType = type;
    }

    /**
     * Sets the default values for the input and output range.
     */
    public void setDefaultRanges() {
        inTempMin = (float) image.getMin();
        inTempMax = (float) image.getMax();
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
     * Accessor that sets the endianess.
     *
     * @param  endns  Endianess.
     */
    public void setEndianess(boolean endns) {
        endianess = endns;
    }

    /**
     * Accessor that sets the maximum input range to the parameter.
     *
     * @param  max  Maximum input range.
     */
    public void setInputRangeMax(double max) {
        inTempMax = max;
    }

    /**
     * Accessor that sets the minimum input range to the parameter.
     *
     * @param  min  Minimum input range.
     */
    public void setInputRangeMin(double min) {
        inTempMin = min;
    }

    /**
     * Accessor that sets the maximum output range to the parameter.
     *
     * @param  max  Maximum output range.
     */
    public void setOutputRangeMax(double max) {
        outTempMax = max;
    }

    /**
     * Accessor that sets the minimum output range to the parameter.
     *
     * @param  min  Minimum output range.
     */
    public void setOutputRangeMin(double min) {
        outTempMin = min;
    }

    /**
     * Once all the necessary variables are set, call the Change Type algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        if (image.getNDims() == 2) { // source image is 2D

            int[] destExtents = new int[2];

            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            image.getFileInfo(0).setEndianess(endianess);

            if (displayLoc == NEW) {

                try {

                    // Make result image of the new data type
                    resultImage = new ModelImage(dataType, destExtents, makeImageName(image.getImageName(), "_invert"));
                    
                    if (resultImage.isComplexImage()) {
                        imageMathAlgo = new AlgorithmImageMath(resultImage, image, AlgorithmImageMath.INVERSE, 0.0,
                        		0.0, 0.0, AlgorithmImageMath.CLIP, true);
                        // This is very important. Adding this object as a listener allows the algorithm to
	                    // notify this object when it has completed of failed. See algorithm performed event.
	                    // This is made possible by implementing AlgorithmedPerformed interface
	                    imageMathAlgo.addListener(this);
	
	                    createProgressBar(image.getImageName(), imageMathAlgo);
	
	                    // Hide dialog
	                    setVisible(false);
	
	                    if (isRunInSeparateThread()) {
	
	                        // Start the thread as a low priority because we wish to still have user interface work fast.
	                        if (imageMathAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
	                            MipavUtil.displayError("A thread is already running on this object");
	                        }
	                    } else {
	                        imageMathAlgo.run();
	                    }
                    }
                    else { // not complexImage
	                    // Make algorithm
	                    changeTypeAlgo = new AlgorithmChangeType(resultImage, image, inTempMin, inTempMax, outTempMin,
	                                                             outTempMax, false);
	
	                    // This is very important. Adding this object as a listener allows the algorithm to
	                    // notify this object when it has completed of failed. See algorithm performed event.
	                    // This is made possible by implementing AlgorithmedPerformed interface
	                    changeTypeAlgo.addListener(this);
	
	                    createProgressBar(image.getImageName(), changeTypeAlgo);
	
	                    // Hide dialog
	                    setVisible(false);
	
	                    if (isRunInSeparateThread()) {
	
	                        // Start the thread as a low priority because we wish to still have user interface work fast.
	                        if (changeTypeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
	                            MipavUtil.displayError("A thread is already running on this object");
	                        }
	                    } else {
	                        changeTypeAlgo.run();
	                    }
                    } // else not complexImage
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog invert: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {

                    if (image.isComplexImage()) {
                    	imageMathAlgo = new AlgorithmImageMath(image, AlgorithmImageMath.INVERSE, 0.0,
                        		0.0, 0.0, AlgorithmImageMath.CLIP, true);
                    	
                    	imageMathAlgo.addListener(this);
                    }
                    else {
	                	// No need to make new image space because the user has choosen to replace the source image
	                    // Make the algorithm class
	                    changeTypeAlgo = new AlgorithmChangeType(image, dataType, inTempMin, inTempMax, outTempMin,
	                                                             outTempMax, false);
	
	                    // This is very important. Adding this object as a listener allows the algorithm to
	                    // notify this object when it has completed of failed. See algorithm performed event.
	                    // This is made possible by implementing AlgorithmedPerformed interface
	                    changeTypeAlgo.addListener(this);
                    }

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

                    if (image.isComplexImage()) {
                    	if (isRunInSeparateThread()) {
                    		
	                        // Start the thread as a low priority because we wish to still have user interface.
	                        if (imageMathAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
	                            MipavUtil.displayError("A thread is already running on this object");
	                        }
	                    } else {
	                        imageMathAlgo.run();
	                    }	
                    }
                    else {
	                    if (isRunInSeparateThread()) {
	
	                        // Start the thread as a low priority because we wish to still have user interface.
	                        if (changeTypeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
	                            MipavUtil.displayError("A thread is already running on this object");
	                        }
	                    } else {
	                        changeTypeAlgo.run();
	                    }
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog invert: unable to allocate enough memory");

                    return;
                }
            }
        } else if ((image.getNDims() == 3) || (image.getNDims() == 4)) {
            int[] destExtents;

            if (image.getNDims() == 3) {
                destExtents = new int[3];
                destExtents[0] = image.getExtents()[0];
                destExtents[1] = image.getExtents()[1];
                destExtents[2] = image.getExtents()[2];

                for (int n = 0; n < image.getExtents()[2]; n++) {
                    image.getFileInfo(n).setEndianess(endianess);
                }
            } else {
                destExtents = new int[4];
                destExtents[0] = image.getExtents()[0];
                destExtents[1] = image.getExtents()[1];
                destExtents[2] = image.getExtents()[2];
                destExtents[3] = image.getExtents()[3];

                for (int n = 0; n < (image.getExtents()[2] * image.getExtents()[3]); n++) {
                    image.getFileInfo(n).setEndianess(endianess);
                }
            }

            if (displayLoc == NEW) {

                try {

                    // Make result image of the new data type
                    resultImage = new ModelImage(dataType, destExtents, makeImageName(image.getImageName(), "_invert"));
                    
                    if (resultImage.isComplexImage()) {
                    	imageMathAlgo = new AlgorithmImageMath(resultImage, image, AlgorithmImageMath.INVERSE, 0.0,
                        		0.0, 0.0, AlgorithmImageMath.CLIP, true);
                        // This is very important. Adding this object as a listener allows the algorithm to
	                    // notify this object when it has completed of failed. See algorithm performed event.
	                    // This is made possible by implementing AlgorithmedPerformed interface
	                    imageMathAlgo.addListener(this);
	
	                    createProgressBar(image.getImageName(), imageMathAlgo);
	
	                    // Hide dialog
	                    setVisible(false);
	
	                    if (isRunInSeparateThread()) {
	
	                        // Start the thread as a low priority because we wish to still have user interface work fast.
	                        if (imageMathAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
	                            MipavUtil.displayError("A thread is already running on this object");
	                        }
	                    } else {
	                        imageMathAlgo.run();
	                    }	
                    }
                    else {
	                    // Make algorithm
	                    //System.out.println(inTempMin + " " + inTempMax + " " + outTempMin + " " + outTempMax);
	                    changeTypeAlgo = new AlgorithmChangeType(resultImage, image, inTempMin, inTempMax, outTempMin,
	                                                             outTempMax, false);
	
	                    // This is very important. Adding this object as a listener allows the algorithm to
	                    // notify this object when it has completed or failed. See algorithm performed event.
	                    // This is made possible by implementing AlgorithmedPerformed interface
	                    changeTypeAlgo.addListener(this);
	
	                    // Hide dialog
	                    setVisible(false);
	
	                    if (isRunInSeparateThread()) {
	
	                        // Start the thread as a low priority because we wish to still have user interface work fast
	                        if (changeTypeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
	                            MipavUtil.displayError("A thread is already running on this object");
	                        }
	                    } else {
	                        changeTypeAlgo.run();
	                    }
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog invert: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {
                	
                	if (image.isComplexImage()) {
                		imageMathAlgo = new AlgorithmImageMath(image, AlgorithmImageMath.INVERSE, 0.0,
                        		0.0, 0.0, AlgorithmImageMath.CLIP, true);
                    	
                    	imageMathAlgo.addListener(this);	
                	}
                	else {
	                    // Make algorithm
	                    //System.out.println(inTempMin + " " + inTempMax + " " + outTempMin + " " + outTempMax);
	                    changeTypeAlgo = new AlgorithmChangeType(image, dataType, inTempMin, inTempMax, outTempMin,
	                                                             outTempMax, false);
	
	                    // This is very important. Adding this object as a listener allows the algorithm to
	                    // notify this object when it has completed or failed. See algorithm performed event.
	                    // This is made possible by implementing AlgorithmedPerformed interface
	                    changeTypeAlgo.addListener(this);
                	}

                    // Hide dialog
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
                    
                    if (image.isComplexImage()) {
                    	if (isRunInSeparateThread()) {
                    		
	                        // Start the thread as a low priority because we wish to still have user interface work fast
	                        if (imageMathAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
	                            MipavUtil.displayError("A thread is already running on this object");
	                        }
	                    } else {
	                        imageMathAlgo.run();
	                    }	
                    }
                    else {
	                    if (isRunInSeparateThread()) {
	
	                        // Start the thread as a low priority because we wish to still have user interface work fast
	                        if (changeTypeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
	                            MipavUtil.displayError("A thread is already running on this object");
	                        }
	                    } else {
	                        changeTypeAlgo.run();
	                    }
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog invert: unable to allocate enough memory");

                    return;
                }
            }
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        if (displayLoc == NEW) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        if (scriptParameters.doOutputNewImage()) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }

        setDataType(image.getFileInfo(0).getDataType());
        setEndianess(image.getFileInfo(0).getEndianess());
        setDefaultRanges();
        setOutputRangeMin(image.getMax());
        setOutputRangeMax(image.getMin());
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Invert image");

        GridBagConstraints gbc = new GridBagConstraints();

        JPanel destinationPanel = new JPanel(new GridBagLayout());

        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        ButtonGroup destinationGroup = new ButtonGroup();

        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        destinationPanel.add(newImage, gbc);
        gbc.gridy = 1;
        destinationPanel.add(replaceImage, gbc);

        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) { // Only if the image is unlocked can it be replaced.
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        JPanel buttonPanel = new JPanel();

        buttonPanel.add(buildButtons());

        JPanel mainPanel = new JPanel();

        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
        mainPanel.add(destinationPanel);


        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        System.gc();

        dataType = image.getType();

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        endianess = image.getFileInfo(0).getEndianess();

        this.setDefaultRanges();

        outTempMin = image.getMax();
        outTempMax = image.getMin();

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
                return new String("Utilities");
            }

            public String getDescription() {
                return new String("Inverts an image");
            }

            public String getDescriptionLong() {
                return new String("Inverts an image");
            }

            public String getShortLabel() {
                return new String("Invert");
            }

            public String getLabel() {
                return new String("Invert");
            }

            public String getName() {
                return new String("Invert");
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
