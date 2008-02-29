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
 * source image. It should be noted that the algorithms are executed in their own thread.
 *
 * @version  0.1 September 30, 2005
 * @author   William Gandler
 * @see      AlgorithmGraphBasedSegmentation
 */
public class JDialogGraphBasedSegmentation extends JDialogScriptableBase
        implements AlgorithmInterface, DialogDefaultsInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5153750542245396807L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    // private ModelImage featureImage = null;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private int minComponentSize;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private JPanel paramPanel;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private AlgorithmGraphBasedSegmentation segAlgo;

    /** DOCUMENT ME! */
    private float sigma;

    /** DOCUMENT ME! */
    private JTextField textGauss;

    /** DOCUMENT ME! */
    private JTextField textMinComponentSize;

    /** DOCUMENT ME! */
    private JTextField textThreshold;

    /** DOCUMENT ME! */
    private float threshold;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogGraphBasedSegmentation() { }

    // or if the source image is to be replaced

    /**
     * Creates a new JDialogGraphBasedSegmentation object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogGraphBasedSegmentation(Frame theParentFrame, ModelImage im) {
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
            // MipavUtil.showHelp( "" );
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

        if (algorithm instanceof AlgorithmGraphBasedSegmentation) {
            image.clearMask();

            if ((segAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);

                try {

                    // resultImage.setImageName("Graph Based Segmentation");
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }

                // featureImage  = ((AlgorithmGraphBasedSegmentation)algorithm).getFeatureImage();
                // new ViewJFrameImage(featureImage,null, new Dimension(610, 220));
            } else if (resultImage == null) {
                image = segAlgo.getImage();

                try {
                    new ViewJFrameImage(image, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }

            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        }
        // Update frame
        // ((ViewJFrameBase)parentFrame).updateImages(true);

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        segAlgo.finalize();
        segAlgo = null;
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
        str += sigma + delim;
        str += threshold + delim;
        str += minComponentSize;

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
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if (defaultsString != null) {

            try {
                System.err.println(defaultsString);

                StringTokenizer st = new StringTokenizer(defaultsString, ",");

                textGauss.setText("" + MipavUtil.getFloat(st));
                textThreshold.setText("" + MipavUtil.getFloat(st));
                textMinComponentSize.setText("" + MipavUtil.getInt(st));

                if (MipavUtil.getBoolean(st)) {
                    newImage.setSelected(true);
                } else {
                    replaceImage.setSelected(true);
                }
            } catch (Exception ex) {

                // since there was a problem parsing the defaults string, start over with the original defaults
                System.out.println("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
            }
        }
    }

    /**
     * Saves the default settings into the Preferences file.
     */
    public void saveDefaults() {
        String defaultsString = new String(getParameterString(",") + "," + newImage.isSelected());

        // System.err.println(defaultsString);
        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
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
     * Accessor that sets the minimum component size.
     *
     * @param  minComponentSize  Value to set minComponentSize to (should be between 1 and 100000).
     */
    public void setMinComponentSize(int minComponentSize) {
        this.minComponentSize = minComponentSize;
    }

    /**
     * Accessor that sets the sigma.
     *
     * @param  sigma  Value to set sigma to (should be between 0.0 and 10.0).
     */
    public void setSigma(float sigma) {
        this.sigma = sigma;
    }

    /**
     * Accessor that sets the threshold.
     *
     * @param  threshold  Value to set threshold to (should be between 0.0 and 50000.0).
     */
    public void setThreshold(float threshold) {
        this.threshold = threshold;
    }

    /**
     * Once all the necessary variables are set, call the Graph Based Segmentation algorithm based on whether or not
     * there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_graphBasedSeg");

        if (displayLoc == NEW) {

            try {
                resultImage = new ModelImage(ModelImage.ARGB, image.getExtents(), name);

                /*if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM){
                 *  ((FileInfoDicom)(resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                 *  } */
                // Make algorithm
                segAlgo = new AlgorithmGraphBasedSegmentation(resultImage, image, sigma, threshold, minComponentSize);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                segAlgo.addListener(this);

                createProgressBar(image.getImageName(), segAlgo);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (segAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    segAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Graph Based Segmentation: unable to allocate enough memory");

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
                segAlgo = new AlgorithmGraphBasedSegmentation(image, sigma, threshold, minComponentSize);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                segAlgo.addListener(this);

                createProgressBar(image.getImageName(), segAlgo);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface.
                    if (segAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    segAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Graph Based Segemntation: unable to allocate enough memory");

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
        parentFrame = image.getParentFrame();

        if (scriptParameters.doOutputNewImage()) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }

        setSigma(scriptParameters.getParams().getFloat("sigma"));
        setThreshold(scriptParameters.getParams().getFloat("threshold"));
        setMinComponentSize(scriptParameters.getParams().getInt("min_component_size"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("sigma", sigma));
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold", threshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("min_component_size", minComponentSize));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Graph Based Segmentation");
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
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        JLabel labelGauss = new JLabel("Gaussian smoothing before segmentation (0.0 - 10.0) ");
        labelGauss.setForeground(Color.black);
        labelGauss.setFont(serif12);
        paramPanel.add(labelGauss, gbc);

        gbc.gridx = 1;
        textGauss = new JTextField(10);
        textGauss.setText("0.5");
        textGauss.setFont(serif12);
        paramPanel.add(textGauss, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;

        JLabel labelThreshold = new JLabel("Threshold function ");
        labelThreshold.setForeground(Color.black);
        labelThreshold.setFont(serif12);
        paramPanel.add(labelThreshold, gbc);

        gbc.gridx = 1;
        textThreshold = new JTextField(10);
        textThreshold.setText("500.0");
        textThreshold.setFont(serif12);
        paramPanel.add(textThreshold, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;

        JLabel labelMinComponentSize = new JLabel("Minimum component size ");
        labelMinComponentSize.setForeground(Color.black);
        labelMinComponentSize.setFont(serif12);
        paramPanel.add(labelMinComponentSize, gbc);

        gbc.gridx = 1;
        textMinComponentSize = new JTextField(10);
        textMinComponentSize.setText("20");
        textMinComponentSize.setFont(serif12);
        paramPanel.add(textMinComponentSize, gbc);

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

        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add(paramPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(destinationPanel, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);
        setVisible(true);

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

        tmpStr = textGauss.getText();

        if (testParameter(tmpStr, 0.0, 10.0)) {
            sigma = Float.valueOf(tmpStr).floatValue();
        } else {
            textGauss.requestFocus();
            textGauss.selectAll();

            return false;
        }

        tmpStr = textThreshold.getText();

        if (testParameter(tmpStr, 0.0, 50000.0)) {
            threshold = Float.valueOf(tmpStr).floatValue();
        } else {
            textThreshold.requestFocus();
            textThreshold.selectAll();

            return false;
        }

        tmpStr = textMinComponentSize.getText();

        if (testParameter(tmpStr, 1.0, 100000.0)) {
            minComponentSize = Integer.valueOf(tmpStr).intValue();
        } else {
            textMinComponentSize.requestFocus();
            textMinComponentSize.selectAll();

            return false;
        }

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        return true;
    }


}
