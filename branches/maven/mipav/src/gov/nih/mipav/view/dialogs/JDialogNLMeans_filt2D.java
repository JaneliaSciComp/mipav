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
 * Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image. It should be noted that the algorithms are executed in their own threads.
 *
 * @version  September 3, 2021
 * @see      NLMeans_filt2D
 */
public class JDialogNLMeans_filt2D extends JDialogScriptableBase implements AlgorithmInterface, ItemListener, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    
    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private JPanel filterPanel;

    /** DOCUMENT ME! */
    private double sigmaX;

    /** DOCUMENT ME! */
    private NLMeans_filt2D NLMeansAlgo = null;
    

    /** DOCUMENT ME! */
    private double sigmaY;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JLabel labelSigmaX;

    /** DOCUMENT ME! */
    private JLabel labelSigmaY;

    /** DOCUMENT ME! */
    private JLabel labelKSize;

    /** DOCUMENT ME! */
    private JLabel labelSSize;

    /** DOCUMENT ME! */
    private JLabel labelNoise;

    /** DOCUMENT ME! */
    private JPanel mainPanel;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** Neighbor window size */
    private int ksize;

    /** DOCUMENT ME! */
    private int ssize;

    /** DOCUMENT ME! */
    private JTextField textSigmaX;

    /** DOCUMENT ME! */
    private JTextField textSigmaY;

    /** DOCUMENT ME! */
    private JTextField textKSize;

    /** DOCUMENT ME! */
    private JTextField textSSize;

    /** DOCUMENT ME! */
    private JTextField textNoise;

    /** DOCUMENT ME! */
    private double noise_std;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogNLMeans_filt2D() { }

    // or if the source image is to be replaced
    /**
     * Creates a new JDialogNLMeans_filt2D object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogNLMeans_filt2D(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
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
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (source == helpButton) {
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

        if (algorithm instanceof NLMeans_filt2D) {
            image.clearMask();

            if ((NLMeansAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                try {

                    // resultImage.setImageName("Mean: "+image.getImageName());
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

        NLMeansAlgo.finalize();
        NLMeansAlgo = null;
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
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("sigma_x", sigmaX));
        scriptParameters.getParams().put(ParameterFactory.newParameter("sigma_y", sigmaY));
        scriptParameters.getParams().put(ParameterFactory.newParameter("k_size", ksize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("s_size", ssize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("noise_std_dev", noise_std));
    }
    
    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
        
        if (scriptParameters.getParams().getBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE)) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }
        
        setSigmaX(scriptParameters.getParams().getDouble("sigma_x"));
        setSigmaY(scriptParameters.getParams().getDouble("sigma_y"));
        setKSize(scriptParameters.getParams().getInt("k_size"));
        setSSize(scriptParameters.getParams().getInt("s_size"));
        setNoise_std(scriptParameters.getParams().getFloat("noise_std_dev"));
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
     * Accessor that sets X dimension Gaussian scale.
     *
     * @param  sigmaX  Value to set X dimension Gaussian scale to.
     */
    public void setSigmaX(double sigmaX) {
        this.sigmaX = sigmaX;
    }
    
    /**
     * Accessor that sets Y dimension Gaussian scale.
     *
     * @param  sigmaY  Value to set Y dimension Gaussian scale to.
     */
    public void setSigmaY(double sigmaY) {
        this.sigmaY = sigmaY;
    }

    /**
     * Accessor that sets the Neighbor window size.
     *
     * @param  ksize  Value to set neighbor window size to.
     */
    public void setKSize(int ksize) {
        this.ksize = ksize;
    }
    
    /**
     * Accessor that sets the search window size.
     *
     * @param  ssize  Value to set search window size to.
     */
    public void setSSize(int ssize) {
        this.ssize = ssize;
    }

    /**
     * Accessor that sets the noise standard deviation.
     *
     * @param  noise_std  Value to set noise standard deviation to.
     */
    public void setNoise_std(double noise_std) {
        this.noise_std = noise_std;
    }

    /**
     * Once all the necessary variables are set, call the Frequency Filter algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_NLMeans");

        if (displayLoc == NEW) {

            try {
                resultImage = (ModelImage) image.clone();
                resultImage.setImageName(name);
                resultImage.resetVOIs();

                // Make algorithm
                NLMeansAlgo = new NLMeans_filt2D(resultImage, image, sigmaX, sigmaY, ksize, ssize, noise_std);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                NLMeansAlgo.addListener(this);
                
                createProgressBar(image.getImageName(), NLMeansAlgo);

                // Hide dialog since the algorithm is about to run
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (NLMeansAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    NLMeansAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog NLMeans_filt2D: unable to allocate enough memory");

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
                NLMeansAlgo = new NLMeans_filt2D(null, image, sigmaX, sigmaY, ksize, ssize, noise_std);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                NLMeansAlgo.addListener(this);

                createProgressBar(image.getImageName(), NLMeansAlgo);
                
                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((ViewJFrameBase) (imageFrames.elementAt(i))).getTitle();
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (NLMeansAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    NLMeansAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog NLMeans_filt2D: unable to allocate enough memory");

                return;
            }
        }
    }
    
    
    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Non Local-means Filter");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 3;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;

        filterPanel = new JPanel(new GridBagLayout());
        filterPanel.setBorder(buildTitledBorder("Non Local-means filter specifications"));

        labelSigmaX = new JLabel("Gaussian scale X (0.5-50.0) ");
        labelSigmaX.setForeground(Color.black);
        labelSigmaX.setFont(serif12);
        labelSigmaX.setEnabled(true);

        textSigmaX = new JTextField(10);
        textSigmaX.setText("5.0");
        textSigmaX.setFont(serif12);
        textSigmaX.setEnabled(true);

        labelSigmaY = new JLabel("Gaussian scale Y (0.5-50.0) ");
        labelSigmaY.setForeground(Color.black);
        labelSigmaY.setFont(serif12);
        labelSigmaY.setEnabled(true);

        textSigmaY = new JTextField(10);
        textSigmaY.setText("5.0");
        textSigmaY.setFont(serif12);
        textSigmaY.setEnabled(true);

        labelKSize = new JLabel("Neighbor window size odd(3-21)");
        labelKSize.setForeground(Color.black);
        labelKSize.setFont(serif12);
        labelKSize.setEnabled(true);

        textKSize = new JTextField(10);
        textKSize.setText("7");
        textKSize.setFont(serif12);
        textKSize.setEnabled(true);

        labelSSize = new JLabel("Search window size odd(7-99)");
        labelSSize.setForeground(Color.black);
        labelSSize.setFont(serif12);
        labelSSize.setEnabled(true);

        textSSize = new JTextField(10);
        textSSize.setText("21");
        textSSize.setFont(serif12);
        textSSize.setEnabled(true);

        labelNoise = new JLabel("Noise standard deviation ");
        labelNoise.setForeground(Color.black);
        labelNoise.setFont(serif12);
        labelNoise.setEnabled(true);

        textNoise = new JTextField(10);
        textNoise.setText("20.0");
        textNoise.setFont(serif12);
        textNoise.setEnabled(true);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.weightx = .5;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        filterPanel.add(labelSigmaX, gbc);
        gbc.gridx = 1;
        filterPanel.add(textSigmaX, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        filterPanel.add(labelSigmaY, gbc);
        gbc.gridx = 1;
        filterPanel.add(textSigmaY, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        filterPanel.add(labelKSize, gbc);
        gbc.gridx = 1;
        filterPanel.add(textKSize, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        filterPanel.add(labelSSize, gbc);
        gbc.gridx = 1;
        filterPanel.add(textSSize, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        filterPanel.add(labelNoise, gbc);
        gbc.gridx = 1;
        filterPanel.add(textNoise, gbc);
        

        destinationPanel = new JPanel(new GridLayout(2, 1));
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        newImage.setForeground(Color.black);
        if (image.isColorImage()) {
        	newImage.setEnabled(false);
        }
        destinationGroup.add(newImage);
        destinationPanel.add(newImage);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        replaceImage.setForeground(Color.black);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage);

        // Only if the image is unlocked can it be replaced.
        if ((image.getLockStatus() == ModelStorageBase.UNLOCKED) && (!image.isColorImage())) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(filterPanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(destinationPanel, gbc);

        getContentPane().add(mainPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setVisible(true);
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

        tmpStr = textSigmaX.getText();

        if (testParameter(tmpStr, 0.5, 50.0)) {
            sigmaX = Double.valueOf(tmpStr).doubleValue();
        } else {
            MipavUtil.displayError("Gaussian scale X must be between 0.5 and 50.0");
            textSigmaX.requestFocus();
            textSigmaX.selectAll();

            return false;
        }

        tmpStr = textSigmaY.getText();

        if (testParameter(tmpStr, 0.5, 50.0)) {
            sigmaY = Double.valueOf(tmpStr).doubleValue();
        } else {
            MipavUtil.displayError("Gaussian scale Y must be between 0.5 and 50.0");
            textSigmaY.requestFocus();
            textSigmaY.selectAll();

            return false;
        }

        tmpStr = textKSize.getText();

        if (testParameter(tmpStr, 3, 21)) {
            ksize = Integer.valueOf(tmpStr).intValue();
        } else {
            MipavUtil.displayError("Neighbor window size must be between 3 and 21");
            textKSize.requestFocus();
            textKSize.selectAll();

            return false;
        }
        
        if ((ksize % 2) == 0) {
        	MipavUtil.displayError("Neighbor window size must be an odd number");
            textKSize.requestFocus();
            textKSize.selectAll();

            return false;
        }

        tmpStr = textSSize.getText();

        if (testParameter(tmpStr, 7, 99)) {
            ssize = Integer.valueOf(tmpStr).intValue();
        } else {
            MipavUtil.displayError("Search window size must be between 7 and 99");
            textSSize.requestFocus();
            textSSize.selectAll();

            return false;
        }
        
        if ((ssize % 2) == 0) {
        	MipavUtil.displayError("Search window size must be an odd number");
            textSSize.requestFocus();
            textSSize.selectAll();

            return false;
        }

        if (testParameter(tmpStr, 0.0, Double.MAX_VALUE)) {
            noise_std = Double.valueOf(tmpStr).doubleValue();
        } else {
            MipavUtil.displayError("Noise standard deviation must be >= 0.0");
            textNoise.requestFocus();
            textNoise.selectAll();

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
                return new String("Algorithms");
            }

            public String getDescription() {
                return new String("Applies a Non Local-means Filter.");
            }

            public String getDescriptionLong() {
                return new String("Applies a Non Local-means Filter.");
            }

            public String getShortLabel() {
                return new String("NL-means Filter");
            }

            public String getLabel() {
                return new String("Non Local-means Filter");
            }

            public String getName() {
                return new String("Non Local-means Filter");
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
            table.put(new ParameterDouble("sigma_x", 5.0));
            table.put(new ParameterDouble("sigma_y", 5.0));
            table.put(new ParameterInt("k_size", 7));
            table.put(new ParameterInt("s_size", 21));
            table.put(new ParameterDouble("noise_std_dev", 20.0));
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
            table.put(new ParameterImage("NLMeans_image"));
            
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
