package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.text.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image. It should be noted that the algorithms are executed in their own threads.
 *
 * @version  1.0 February 25, 2005
 * @author   William Gandler
 * @see      AlgorithmFrequencyFilter
 */
public class JDialogHomomorphicFilter extends JDialogScriptableBase implements AlgorithmInterface, ItemListener, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1061706659841470806L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int butterworthOrder;

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private JPanel filterPanel;

    /** DOCUMENT ME! */
    private float freq1 = 0.4f;

    /** DOCUMENT ME! */
    private AlgorithmFrequencyFilter FrequencyFilterAlgo = null;

    /** DOCUMENT ME! */
    private float highGain = 2.0f;

    /** DOCUMENT ME! */
    private float highTruncated = 0.05f;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private boolean image25D = false;

    /** DOCUMENT ME! */
    private JCheckBox image25DCheckbox;

    /** DOCUMENT ME! */
    private JLabel labelF1;

    /** DOCUMENT ME! */
    private JLabel labelHighGain;

    /** DOCUMENT ME! */
    private JLabel labelHighTruncated;

    /** DOCUMENT ME! */
    private JLabel labelLowGain;

    /** DOCUMENT ME! */
    private JLabel labelLowTruncated;

    /** DOCUMENT ME! */
    private JLabel labelOrder;

    /** DOCUMENT ME! */
    private float lowGain = 0.5f;

    /** DOCUMENT ME! */
    private float lowTruncated = 0.01f;

    /** DOCUMENT ME! */
    private JPanel mainPanel;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private DecimalFormat nf = new DecimalFormat("##0.0##");

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JTextField textF1;

    /** DOCUMENT ME! */
    private JTextField textHighGain;

    /** DOCUMENT ME! */
    private JTextField textHighTruncated;

    /** DOCUMENT ME! */
    private JTextField textLowGain;

    /** DOCUMENT ME! */
    private JTextField textLowTruncated;

    /** DOCUMENT ME! */
    private JTextField textOrder;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogHomomorphicFilter() { }

    // or if the source image is to be replaced
    /**
     * Creates a new JDialogHomomorphicFilter object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogHomomorphicFilter(Frame theParentFrame, ModelImage im) {
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
            // MipavUtil.showHelp("");
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

        if (algorithm instanceof AlgorithmFrequencyFilter) {

            if ((algorithm.isCompleted() == true) && (resultImage != null)) {

                updateFileTypeInfo(image, resultImage, ModelStorageBase.FLOAT);

                // resultImage is the same or smaller than image.
                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName("Frequency Filtered image");
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        ((ViewJFrameBase) parentFrame).getUserInterface().registerFrame((Frame)
                                                                                        (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    ((ViewJFrameBase) parentFrame).getUserInterface().registerFrame(parentFrame);
                    ((ViewJFrameImage) parentFrame).getComponentImage().setLogMagDisplay(true);
                }

                image.notifyImageDisplayListeners(null, true);
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        }
     // save the completion status for later
        setComplete(algorithm.isCompleted());

        FrequencyFilterAlgo.finalize();
        FrequencyFilterAlgo = null;
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
     * Accessor that sets the butterworth order.
     *
     * @param  order  Value to set the butterworth order to.
     */
    public void setButterworthOrder(int order) {
        butterworthOrder = order;
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
     * Accessor that sets the frequency 1 variable.
     *
     * @param  scale  Value to set frequency 1 to.
     */
    public void setFreq1(float scale) {
        freq1 = scale;
    }

    /**
     * Accessor that sets highGain variable.
     *
     * @param  highGain  float
     */
    public void setHighGain(float highGain) {
        this.highGain = highGain;
    }

    /**
     * Acceessor that sets highTruncated variable.
     *
     * @param  highTruncated  float
     */
    public void setHighTruncated(float highTruncated) {
        this.highTruncated = highTruncated;
    }

    /**
     * Accessor that sets image25D for single slice processing.
     *
     * @param  image25D  DOCUMENT ME!
     */
    public void setImage25D(boolean image25D) {
        this.image25D = image25D;
    }

    /**
     * Accessor that sets lowGain variable.
     *
     * @param  lowGain  float
     */
    public void setLowGain(float lowGain) {
        this.lowGain = lowGain;
    }

    /**
     * Accessor that sets lowTruncated variable.
     *
     * @param  lowTruncated  float
     */
    public void setLowTruncated(float lowTruncated) {
        this.lowTruncated = lowTruncated;
    }

    /**
     * Once all the necessary variables are set, call the Frequency Filter algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_homomorphic");

        if (displayLoc == NEW) {

            try {
                resultImage = (ModelImage) image.clone();
                resultImage.setImageName(name);
                resultImage.resetVOIs();

                // Make algorithm
                FrequencyFilterAlgo = new AlgorithmFrequencyFilter(resultImage, image, image25D, freq1,
                                                                   butterworthOrder, lowGain, highGain, lowTruncated,
                                                                   highTruncated);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                FrequencyFilterAlgo.addListener(this);

                createProgressBar(image.getImageName(), FrequencyFilterAlgo);

                // Hide dialog since the algorithm is about to run
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (FrequencyFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    FrequencyFilterAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog HomomorphicFilter: unable to allocate enough memory");

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
                FrequencyFilterAlgo = new AlgorithmFrequencyFilter(image, image25D, freq1, butterworthOrder, lowGain,
                                                                   highGain, lowTruncated, highTruncated);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                FrequencyFilterAlgo.addListener(this);

                createProgressBar(image.getImageName(), FrequencyFilterAlgo);

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
                    if (FrequencyFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    FrequencyFilterAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog HomomorphicFilter: unable to allocate enough memory");

                return;
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

        setImage25D(scriptParameters.doProcess3DAs25D());
        setFreq1(scriptParameters.getParams().getFloat("cutoff_freq"));
        setButterworthOrder(scriptParameters.getParams().getInt("butterworth_order"));
        setLowGain(scriptParameters.getParams().getFloat("low_freq_gain"));
        setHighGain(scriptParameters.getParams().getFloat("high_freq_gain"));
        setLowTruncated(scriptParameters.getParams().getFloat("low_percentage_truncated"));
        setHighTruncated(scriptParameters.getParams().getFloat("high_percentage_truncated"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));

        scriptParameters.storeProcess3DAs25D(image25D);
        scriptParameters.getParams().put(ParameterFactory.newParameter("cutoff_freq", freq1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("butterworth_order", butterworthOrder));
        scriptParameters.getParams().put(ParameterFactory.newParameter("low_freq_gain", lowGain));
        scriptParameters.getParams().put(ParameterFactory.newParameter("high_freq_gain", highGain));
        scriptParameters.getParams().put(ParameterFactory.newParameter("low_percentage_truncated", lowTruncated));
        scriptParameters.getParams().put(ParameterFactory.newParameter("high_percentage_truncated", highTruncated));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Homomorphic Filter");

        JPanel optionsPanel = new JPanel(new GridLayout(1, 1));
        optionsPanel.setBorder(buildTitledBorder("Options"));

        image25DCheckbox = new JCheckBox("Process each slice independently (2.5D)");
        image25DCheckbox.setFont(serif12);
        image25DCheckbox.setForeground(Color.black);

        if (image.getNDims() == 3) {
            image25DCheckbox.setEnabled(true);
            image25DCheckbox.setSelected(false);
        } else {
            image25DCheckbox.setEnabled(false);
            image25DCheckbox.setSelected(false);
        }

        optionsPanel.add(image25DCheckbox);

        destinationPanel = new JPanel(new GridLayout(2, 1));
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        newImage.setForeground(Color.black);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        replaceImage.setForeground(Color.black);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage);

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        filterPanel = new JPanel(new GridBagLayout());
        filterPanel.setBorder(buildTitledBorder("Filter specifications"));

        textLowGain = new JTextField(10);
        textLowGain.setText(String.valueOf(lowGain));
        textLowGain.setFont(serif12);
        textLowGain.setEnabled(true);

        labelLowGain = new JLabel("Low frequency gain (<1.0)");
        labelLowGain.setForeground(Color.black);
        labelLowGain.setFont(serif12);
        labelLowGain.setEnabled(true);

        textHighGain = new JTextField(10);
        textHighGain.setText(String.valueOf(highGain));
        textHighGain.setFont(serif12);
        textHighGain.setEnabled(true);

        labelHighGain = new JLabel("High frequency gain (>1.0)");
        labelHighGain.setForeground(Color.black);
        labelHighGain.setFont(serif12);
        labelHighGain.setEnabled(true);

        textF1 = new JTextField(10);
        textF1.setText(String.valueOf(freq1));
        textF1.setFont(serif12);
        textF1.setEnabled(true);

        labelF1 = new JLabel("Cutoff frequency >0.0 to 1.0 ");
        labelF1.setForeground(Color.black);
        labelF1.setFont(serif12);
        labelF1.setEnabled(true);

        textOrder = new JTextField(10);
        textOrder.setText("1");
        textOrder.setFont(serif12);
        textOrder.setForeground(Color.black);
        textOrder.setEnabled(true);

        labelOrder = new JLabel("Butterworth filter order");
        labelOrder.setForeground(Color.black);
        labelOrder.setFont(serif12);
        labelOrder.setEnabled(true);

        textLowTruncated = new JTextField(10);
        textLowTruncated.setText(String.valueOf(nf.format(100.0 * lowTruncated)));
        textLowTruncated.setFont(serif12);
        textLowTruncated.setForeground(Color.black);
        textLowTruncated.setEnabled(true);

        labelLowTruncated = new JLabel("% of low histogram end truncated");
        labelLowTruncated.setForeground(Color.black);
        labelLowTruncated.setFont(serif12);
        labelLowTruncated.setEnabled(true);

        textHighTruncated = new JTextField(10);
        textHighTruncated.setText(String.valueOf(nf.format(100.0 * highTruncated)));
        textHighTruncated.setFont(serif12);
        textHighTruncated.setForeground(Color.black);
        textHighTruncated.setEnabled(true);

        labelHighTruncated = new JLabel("% of high histogram end truncated");
        labelHighTruncated.setForeground(Color.black);
        labelHighTruncated.setFont(serif12);
        labelHighTruncated.setEnabled(true);


        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        filterPanel.add(labelLowGain, gbc);
        gbc.gridx = 1;
        filterPanel.add(textLowGain, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        filterPanel.add(labelHighGain, gbc);
        gbc.gridx = 1;
        filterPanel.add(textHighGain, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        filterPanel.add(labelF1, gbc);
        gbc.gridx = 1;
        filterPanel.add(textF1, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        filterPanel.add(labelOrder, gbc);
        gbc.gridx = 1;
        filterPanel.add(textOrder, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        filterPanel.add(labelLowTruncated, gbc);
        gbc.gridx = 1;
        filterPanel.add(textLowTruncated, gbc);
        gbc.gridx = 0;
        gbc.gridy = 5;
        filterPanel.add(labelHighTruncated, gbc);
        gbc.gridx = 1;
        filterPanel.add(textHighTruncated, gbc);

        mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(optionsPanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(destinationPanel, gbc);
        gbc.gridy = 2;
        mainPanel.add(filterPanel, gbc);

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

        if (image25DCheckbox.isSelected()) {
            image25D = true;
        } else {
            image25D = false;
        }

        tmpStr = textLowGain.getText();
        lowGain = Float.valueOf(tmpStr).floatValue();

        if (lowGain <= 0.0) {
            MipavUtil.displayError("Low gain must be greater than 0.0");
            textLowGain.requestFocus();
            textLowGain.selectAll();

            return false;
        } else if (lowGain >= 1.0) {
            MipavUtil.displayError("Low gain must be less than 1.0");
            textLowGain.requestFocus();
            textLowGain.selectAll();

            return false;
        }

        tmpStr = textHighGain.getText();
        highGain = Float.valueOf(tmpStr).floatValue();

        if (highGain <= 1.0) {
            MipavUtil.displayError("High gain must be greater than 1.0");
            textHighGain.requestFocus();
            textHighGain.selectAll();

            return false;
        }

        tmpStr = textF1.getText();
        freq1 = Float.valueOf(tmpStr).floatValue();

        if (freq1 <= 0.0) {
            MipavUtil.displayError("F1 must exceed 0.0");
            textF1.requestFocus();
            textF1.selectAll();

            return false;
        } else if (freq1 >= 1.0) {
            MipavUtil.displayError("F1 must be less than 1.0");
            textF1.requestFocus();
            textF1.selectAll();

            return false;
        }

        tmpStr = textOrder.getText();
        butterworthOrder = Integer.parseInt(tmpStr);

        if (butterworthOrder < 1) {
            MipavUtil.displayError("Butterworth order must be at least 1");
            textOrder.requestFocus();
            textOrder.selectAll();

            return false;
        }

        tmpStr = textLowTruncated.getText();
        lowTruncated = Float.valueOf(tmpStr).floatValue() / 100.0f;

        if (lowTruncated < 0.0) {
            MipavUtil.displayError("% of low end truncated cannot be negative");
            textLowTruncated.requestFocus();
            textLowTruncated.selectAll();

            return false;
        } else if (lowTruncated >= 1.0) {
            MipavUtil.displayError("% of low end truncated must be less than 100.0");
            textLowTruncated.requestFocus();
            textLowTruncated.selectAll();

            return false;
        }

        tmpStr = textHighTruncated.getText();
        highTruncated = Float.valueOf(tmpStr).floatValue() / 100.0f;

        if (highTruncated < 0.0) {
            MipavUtil.displayError("% of high end truncated cannot be negative");
            textHighTruncated.requestFocus();
            textHighTruncated.selectAll();

            return false;
        } else if (highTruncated >= 1.0) {
            MipavUtil.displayError("% of high end truncated must be less than 100");
            textHighTruncated.requestFocus();
            textHighTruncated.selectAll();

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
                return new String("Applies a homomorphic filter.");
            }

            public String getDescriptionLong() {
                return new String("Applies a homomorphic filter.");
            }

            public String getShortLabel() {
                return new String("HomomorphicFilter");
            }

            public String getLabel() {
                return new String("Homomorphic Filter");
            }

            public String getName() {
                return new String("Homomorphic Filter");
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
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D, false));
            table.put(new ParameterFloat("cutoff_freq",(float) .4));
            table.put(new ParameterInt("butterworth_order",1));
            table.put(new ParameterFloat("low_freq_gain",(float) .5));
            table.put(new ParameterFloat("high_freq_gain",2));
            table.put(new ParameterFloat("low_percentage_truncated",.01f));
            table.put(new ParameterFloat("high_percentage_truncated",.05f));
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
