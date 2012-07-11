package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterBoolean;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm for BG distance map.
 *
 * @version  0.1 Nov 17, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      AlgorithmMorphology2D
 * @see      AlgorithmMorphology3D
 */
public class JDialogBGDistanceMap extends JDialogScriptableBase 
	implements AlgorithmInterface, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 8286579547782268746L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmMorphology2D distanceMapAlgo2D = null;

    /** DOCUMENT ME! */
    private AlgorithmMorphology3D distanceMapAlgo3D = null;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;
    
    private JPanelAlgorithmOutputOptions outputPanel;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogBGDistanceMap() { }

    /**
     * Creates new dialog for finding bg distance map.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogBGDistanceMap(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        if ((im.getType() != ModelImage.BOOLEAN) && (im.getType() != ModelImage.UBYTE) &&
                (im.getType() != ModelImage.USHORT)) {
            MipavUtil.displayError("Source Image must be BOOLEAN, UNSIGNED BYTE or UNSIGNED SHORT");
            dispose();

            return;
        }

        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
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
        	MipavUtil.showHelp("Mor001");
        }
    }

    /**
     * Record the parameters just used to run this algorithm in a script.
     * 
     * @throws  ParserException  If there is a problem creating/recording the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException{
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, outputPanel.isOutputNewImageSet());
       
        scriptParameters.storeProcessWholeImage(outputPanel.isProcessWholeImageSet());
     }
    
    /**
     * Set the dialog GUI using the script parameters while running this algorithm as part of a script.
     */
    protected void setGUIFromParams(){        
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
        
        outputPanel = new JPanelAlgorithmOutputOptions(image);
        scriptParameters.setOutputOptionsGUI(outputPanel);
    }
    
    /**
     * Used to perform actions after the execution of the algorithm is completed (e.g., put the result image in the image table).
     * Defaults to no action, override to actually have it do something.
     */
    protected void doPostAlgorithmActions() {
        if (outputPanel.isOutputNewImageSet()) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }
    
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
        if (algorithm instanceof AlgorithmMorphology2D) {
            image.clearMask();

            if ((distanceMapAlgo2D.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {
                    resultImage.setImageName("Bg. distance map image");
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
        } else if (algorithm instanceof AlgorithmMorphology3D) {
            image.clearMask();

            if ((distanceMapAlgo3D.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {
                    resultImage.setImageName("Bg. distance map");
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
        if (distanceMapAlgo2D != null) {
            distanceMapAlgo2D.finalize();
            distanceMapAlgo2D = null;
        }

        if (distanceMapAlgo3D != null) {
            distanceMapAlgo3D.finalize();
            distanceMapAlgo3D = null;
        }

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

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        int kernel = 0;
        String name = makeImageName(image.getImageName(), "_BGDistance");

        if (image.getNDims() == 2) { // source image is 2D

            int[] destExtents = new int[2];
            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            if (outputPanel.isOutputNewImageSet()) {

                try {

                    // Make result image of float type
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    // Make algorithm
                    distanceMapAlgo2D = new AlgorithmMorphology2D(resultImage, kernel, 0,
                                                                  AlgorithmMorphology2D.BG_DISTANCE_MAP, 0, 0, 0, 0,
                                                                  outputPanel.isProcessWholeImageSet());

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    distanceMapAlgo2D.addListener(this);

                    createProgressBar(image.getImageName(), distanceMapAlgo2D);
                    
                    if (outputPanel.isProcessWholeImageSet() == false) {
                        distanceMapAlgo2D.setMask(image.generateVOIMask());
                    }

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {
	                    // Start the thread as a low priority because we wish to still have user interface work fast.
	                    if (distanceMapAlgo2D.startMethod(Thread.MIN_PRIORITY) == false) {
	                        MipavUtil.displayError("A thread is already running on this object");
	                    }
                    } else {
                    	//to maintain proper script execution, directly calling run
                    	distanceMapAlgo2D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog distance map: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {

                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    distanceMapAlgo2D = new AlgorithmMorphology2D(image, kernel, 0,
                                                                  AlgorithmMorphology2D.BG_DISTANCE_MAP, 0, 0, 0, 0,
                                                                  outputPanel.isProcessWholeImageSet());

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    distanceMapAlgo2D.addListener(this);
                    
                    createProgressBar(image.getImageName(), distanceMapAlgo2D);
                    

                    if (outputPanel.isProcessWholeImageSet() == false) {
                        distanceMapAlgo2D.setMask(image.generateVOIMask());
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

                    if (isRunInSeparateThread()) {
	                    // Start the thread as a low priority because we wish to still have user interface work fast.
	                    if (distanceMapAlgo2D.startMethod(Thread.MIN_PRIORITY) == false) {
	                        MipavUtil.displayError("A thread is already running on this object");
	                    }
                    } else {
                    	//to maintain proper script execution, directly calling run
                    	distanceMapAlgo2D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog distance map: unable to allocate enough memory");

                    return;
                }
            }
        } else if (image.getNDims() == 3) {
            int[] destExtents = new int[3];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];

            if (outputPanel.isOutputNewImageSet()) {

                try {

                    // Make result image of float type
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    // Make algorithm
                    distanceMapAlgo3D = new AlgorithmMorphology3D(resultImage, kernel, 0,
                                                                  AlgorithmMorphology3D.BG_DISTANCE_MAP, 0, 0, 0, 0,
                                                                  outputPanel.isProcessWholeImageSet());

                    if (outputPanel.isProcessWholeImageSet() == false) {
                        distanceMapAlgo3D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    distanceMapAlgo3D.addListener(this);

                    createProgressBar(image.getImageName(), distanceMapAlgo3D);
                    
                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {
	                    // Start the thread as a low priority because we wish to still have user interface work fast.
	                    if (distanceMapAlgo3D.startMethod(Thread.MIN_PRIORITY) == false) {
	                        MipavUtil.displayError("A thread is already running on this object");
	                    }
                    } else {
                    	//to maintain proper script execution, directly calling run
                    	distanceMapAlgo3D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog distance map: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {

                    // Make algorithm
                    distanceMapAlgo3D = new AlgorithmMorphology3D(image, kernel, 0,
                                                                  AlgorithmMorphology3D.BG_DISTANCE_MAP, 0, 0, 0, 0,
                                                                  outputPanel.isProcessWholeImageSet());

                    if (outputPanel.isProcessWholeImageSet() == false) {
                        distanceMapAlgo3D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    distanceMapAlgo3D.addListener(this);

                    createProgressBar(image.getImageName(), distanceMapAlgo3D);
                    
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

                    if (isRunInSeparateThread()) {
	                    // Start the thread as a low priority because we wish to still have user interface work fast.
	                    if (distanceMapAlgo3D.startMethod(Thread.MIN_PRIORITY) == false) {
	                        MipavUtil.displayError("A thread is already running on this object");
	                    }
                    } else {
                    	//to maintain proper script execution, directly calling run
                    	distanceMapAlgo3D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog distance map: unable to allocate enough memory");

                    return;
                }
            }
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Background Distance map");
        
        outputPanel = new JPanelAlgorithmOutputOptions(image);
        
        JPanel mainPanel = new JPanel(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.insets = new Insets(5, 5, 5, 5);
        mainPanel.add(outputPanel, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);
        
        

        mainDialogPanel.add(mainPanel);
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

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
                return new String("Algorithms.Morphological");
            }

            public String getDescription() {
                return new String("Creates a background distance map for the image.");
            }

            public String getDescriptionLong() {
                return new String("Creates a background distance map for the image.");
            }

            public String getShortLabel() {
                return new String("BGDistanceMap");
            }

            public String getLabel() {
                return new String("BG Distance Map");
            }

            public String getName() {
                return new String("BG Distance Map");
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
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE, true));
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
