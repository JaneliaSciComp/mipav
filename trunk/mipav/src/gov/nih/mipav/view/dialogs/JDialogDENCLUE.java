package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. It should be noted that the algorithms are executed in their own
 * thread.
 */
public class JDialogDENCLUE extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -8761669177340930135L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JRadioButton arbitraryButton;

    /** DOCUMENT ME! */
    private JRadioButton centerButton;

    /** DOCUMENT ME! */
    private ButtonGroup clusterGroup;

    /** DOCUMENT ME! */
    private AlgorithmDENCLUE denAlgo = null;

    /** DOCUMENT ME! */
    private float distance = 1.0f;

    /** DOCUMENT ME! */
    private JLabel distanceLabel;

    /** DOCUMENT ME! */
    private JTextField distanceText;

    /** DOCUMENT ME! */
    private JRadioButton gaussianButton;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ButtonGroup influenceGroup;

    /** DOCUMENT ME! */
    private boolean isArbitrary;

    /** DOCUMENT ME! */
    private boolean isGaussian;


    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JRadioButton squareButton;

    /** DOCUMENT ME! */
    private float threshold = 1.0f;

    /** DOCUMENT ME! */
    private JLabel thresholdLabel;

    /** DOCUMENT ME! */
    private JTextField thresholdText;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogDENCLUE() { }

    /**
     * Creates a new JDialogDENCLUE object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogDENCLUE(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        image = im;
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
        } else if (command.equals("Help")) {
            // MipavUtil.showHelp("");
        } else if (command.equals("Cancel")) {
            dispose();
        } else if ((source == gaussianButton) || (source == squareButton)) {

            if (gaussianButton.isSelected()) {
                distanceLabel.setText("Standard deviation");
            } else {
                distanceLabel.setText("Influence distance");
            }
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

        if (algorithm instanceof AlgorithmDENCLUE) {
            image.clearMask();

            if ((denAlgo.isCompleted() == true) && (resultImage != null)) {
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

        denAlgo.finalize();
        denAlgo = null;
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
     * Accessor that sets the distance.
     *
     * @param  distance  DOCUMENT ME!
     */
    public void setDistance(float distance) {
        this.distance = distance;
    }

    /**
     * Accessor that sets the wheteher the cluster shape is arbitrary or center defined.
     *
     * @param  isArbitrary  DOCUMENT ME!
     */
    public void setIsArbitrary(boolean isArbitrary) {
        this.isArbitrary = isArbitrary;
    }

    /**
     * Accessor that sets whether the influence function is gaussian or square wave.
     *
     * @param  isGaussian  DOCUMENT ME!
     */
    public void setIsGaussian(boolean isGaussian) {
        this.isGaussian = isGaussian;
    }

    /**
     * Accessor that sets the threshold.
     *
     * @param  threshold  DOCUMENT ME!
     */
    public void setThreshold(float threshold) {
        this.threshold = threshold;
    }


    /**
     * Once all the necessary variables are set, call the mean algorithm based on what type of image this is and whether
     * or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_DENCLUE");

        try {

            // Make result image of float type
            resultImage = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), name);

            // Make algorithm
            denAlgo = new AlgorithmDENCLUE(resultImage, image, isGaussian, distance, threshold, isArbitrary);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            denAlgo.addListener(this);

            createProgressBar(image.getImageName(), denAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (denAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                denAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog DENCLUE: unable to allocate enough memory");

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

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

        distance = scriptParameters.getParams().getFloat("gaussian_std_dev_or_influence_distance");
        threshold = scriptParameters.getParams().getFloat("threshold");
        isGaussian = scriptParameters.getParams().getBoolean("do_use_gaussian_function");
        isArbitrary = scriptParameters.getParams().getBoolean("do_use_arbitrary_shape_cluster");

        if (distance <= 0) {
            throw new ParameterException("gaussian_std_dev_or_influence_distance",
                                         "Distance/standard deviation must be greater than 0 (found " + distance +
                                         ").");
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeImageInRecorder(getResultImage());

        scriptParameters.getParams().put(ParameterFactory.newParameter("gaussian_std_dev_or_influence_distance",
                                                                       distance));
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold", threshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_use_gaussian_function", isGaussian));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_use_arbitrary_shape_cluster", isArbitrary));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Density based clustering");

        // panel gets a grid layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(2, 0, 2, 0); // component width = minwidth + (2ipadx)


        JPanel paramPanel = new JPanel();
        paramPanel.setLayout(gbl);
        gbc.anchor = GridBagConstraints.WEST;
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Clustering parameters")); // set the border ... "Colour channel
                                                                          // Selection"

        influenceGroup = new ButtonGroup();
        gaussianButton = new JRadioButton("Gaussian influence function", true);
        gaussianButton.setFont(serif12);
        gaussianButton.setForeground(Color.black);
        gaussianButton.addActionListener(this);
        influenceGroup.add(gaussianButton);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        paramPanel.add(gaussianButton, gbc);

        squareButton = new JRadioButton("Square wave influence function", false);
        squareButton.setFont(serif12);
        squareButton.setForeground(Color.black);
        squareButton.addActionListener(this);
        influenceGroup.add(squareButton);
        gbc.gridy = 1;
        paramPanel.add(squareButton, gbc);

        distanceLabel = new JLabel("Standard deviation");
        distanceLabel.setForeground(Color.black);
        distanceLabel.setFont(serif12);
        gbc.gridy = 2;
        paramPanel.add(distanceLabel, gbc);

        distanceText = new JTextField(10);
        distanceText.setText("1.0");
        distanceText.setForeground(Color.BLACK);
        distanceText.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(distanceText, gbc);

        thresholdLabel = new JLabel("Threshold");
        thresholdLabel.setForeground(Color.black);
        thresholdLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 3;
        paramPanel.add(thresholdLabel, gbc);

        thresholdText = new JTextField(10);
        thresholdText.setText("1.0");
        thresholdText.setForeground(Color.BLACK);
        thresholdText.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(thresholdText, gbc);

        clusterGroup = new ButtonGroup();
        arbitraryButton = new JRadioButton("Arbitrary shape cluster", true);
        arbitraryButton.setFont(serif12);
        arbitraryButton.setForeground(Color.black);
        clusterGroup.add(arbitraryButton);
        gbc.gridx = 0;
        gbc.gridy = 4;
        paramPanel.add(arbitraryButton, gbc);

        centerButton = new JRadioButton("Center defined cluster", false);
        centerButton.setFont(serif12);
        centerButton.setForeground(Color.black);
        clusterGroup.add(centerButton);
        gbc.gridy = 5;
        paramPanel.add(centerButton, gbc);


        getContentPane().add(paramPanel, BorderLayout.CENTER); // put the setupBox into the dialog

        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
        //setResizable(false);
        System.gc();

    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        isGaussian = gaussianButton.isSelected();

        tmpStr = distanceText.getText();
        distance = Float.parseFloat(tmpStr);

        if (distance <= 0) {
            MipavUtil.displayError("Distance must be greater than zero");
            distanceText.requestFocus();
            distanceText.selectAll();

            return false;
        }

        tmpStr = thresholdText.getText();
        threshold = Float.parseFloat(tmpStr);

        isArbitrary = arbitraryButton.isSelected();

        return true;
    }
}
