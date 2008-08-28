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
 * Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image. It should be noted that the algorithms are executed in their own threads.
 *
 * @version  November 21, 2006
 * @see      AlgorithmDenoisingBLS_GSM
 */
public class JDialogDenoisingBLS_GSM extends JDialogScriptableBase implements AlgorithmInterface, ItemListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -8519961604071482971L;

    /** DOCUMENT ME! */
    private static final int ORTHOGONAL_WAVELET = 1;

    /** DOCUMENT ME! */
    private static final int UNDECIMATED_ORTHOGONAL_WAVELET = 2;

    /** DOCUMENT ME! */
    private static final int STEERABLE_PYRAMID = 3;

    /** DOCUMENT ME! */
    private static final int FULL_STEERABLE_PYRAMID = 4;

    /** Possible repres2 values. */
    private static final int NONE = 0;

    /** DOCUMENT ME! */
    private static final int HAAR = 1; // Haar wavelet

    /** Symmetric quadrature mirror filters. */
    private static final int QMF5 = 2;

    /** DOCUMENT ME! */
    private static final int QMF8 = 3;

    /** DOCUMENT ME! */
    private static final int QMF9 = 4;

    /** DOCUMENT ME! */
    private static final int QMF12 = 5;

    /** DOCUMENT ME! */
    private static final int QMF13 = 6;

    /** DOCUMENT ME! */
    private static final int QMF16 = 7;

    /** Daubechies wavelet. */
    private static final int DAUB1 = 8;

    /** DOCUMENT ME! */
    private static final int DAUB2 = 9;

    /** DOCUMENT ME! */
    private static final int DAUB3 = 10;

    /** DOCUMENT ME! */
    private static final int DAUB4 = 11;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int blockSizeX = 3;

    /** DOCUMENT ME! */
    private int blockSizeY = 3;

    /** DOCUMENT ME! */
    private JCheckBox boundaryCheckBox;

    /** DOCUMENT ME! */
    private JComboBox comboBoxFilter;

    /** DOCUMENT ME! */
    private JCheckBox covarianceCheckBox;

    /** DOCUMENT ME! */
    private AlgorithmDenoisingBLS_GSM deAlgo = null;

    /** Use serialVersionUID for interoperability. */
    // private static final long serialVersionUID = -7693130193527088045L;
    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private int filter = QMF9;

    /** DOCUMENT ME! */
    private JPanel filterPanel;

    /** DOCUMENT ME! */
    private JRadioButton fullButton;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private boolean includeCovar = true;

    /** DOCUMENT ME! */
    private JLabel localXLabel;

    /** DOCUMENT ME! */
    private JTextField localXText;

    /** DOCUMENT ME! */
    private JLabel localYLabel;

    /** DOCUMENT ME! */
    private JTextField localYText;

    /** DOCUMENT ME! */
    private JPanel mainPanel;

    /** DOCUMENT ME! */
    private int method = ORTHOGONAL_WAVELET;

    /** DOCUMENT ME! */
    private ButtonGroup methodGroup;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private ButtonGroup noiseGroup;

    /** DOCUMENT ME! */
    private JLabel noiseLabel;

    /** DOCUMENT ME! */
    private JTextField noiseText;

    /** DOCUMENT ME! */
    private int nOrientations = 3;

    /** DOCUMENT ME! */
    private int nScales = 4;

    /** DOCUMENT ME! */
    private boolean optimize = true;

    /** DOCUMENT ME! */
    private JCheckBox optimizeCheckBox;

    /** DOCUMENT ME! */
    private JLabel orientLabel;

    /** DOCUMENT ME! */
    private JTextField orientText;

    /** DOCUMENT ME! */
    private JRadioButton orthogonalButton;

    /** DOCUMENT ME! */
    private JCheckBox parentCheckBox;

    /** DOCUMENT ME! */
    private JRadioButton psdButton;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JLabel scaleLabel;

    /** DOCUMENT ME! */
    private JTextField scaleText;

    /** DOCUMENT ME! */
    private float sig = 1.0f;

    /** DOCUMENT ME! */
    private JRadioButton steerableButton;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private JRadioButton undecimatedButton;

    /** DOCUMENT ME! */
    private boolean useBoundary = true;

    /** DOCUMENT ME! */
    private boolean useParent = true;

    /** DOCUMENT ME! */
    private boolean usePSD = false;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JRadioButton whiteButton;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogDenoisingBLS_GSM() { }

    // or if the source image is to be replaced
    /**
     * Creates a new JDialogGaborFilter object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogDenoisingBLS_GSM(Frame theParentFrame, ModelImage im) {
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
        int i;

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (source == helpButton) {
            MipavUtil.showHelp("WBLSGSM01");
        } else if ((source == orthogonalButton) || (source == undecimatedButton) || (source == steerableButton) ||
                       (source == fullButton)) {

            if (orthogonalButton.isSelected()) {

                if (comboBoxFilter.getItemCount() != 10) {
                    comboBoxFilter.insertItemAt("Haar", 3);
                    comboBoxFilter.insertItemAt("Quadrature Mirror Filter 5", 4);
                    comboBoxFilter.insertItemAt("Quadrature Mirror Filter 8", 5);
                    comboBoxFilter.insertItemAt("Quadrature Mirror Filter 9", 6);
                    comboBoxFilter.insertItemAt("Quadrature Mirror Filter 12", 7);
                    comboBoxFilter.insertItemAt("Quadrature Mirror Filter 13", 8);
                    comboBoxFilter.insertItemAt("Quadrature Mirror Filter 16", 9);
                    comboBoxFilter.setSelectedIndex(6);
                } // if (comboBoxFilter.getItemCount() != 10)

                comboBoxFilter.setEnabled(true);
                orientText.setText("3");
                orientLabel.setEnabled(false);
                orientText.setEnabled(false);
            } // if (orthogonalButton.isSelected())
            else if (undecimatedButton.isSelected()) {
                if (comboBoxFilter.getItemCount() != 3) {
                    for (i = 9; i >= 3; i--) {
                        comboBoxFilter.removeItemAt(i);
                    }
                } // if (comboBoxFilter.getItemCount() != 3)

                comboBoxFilter.setSelectedIndex(0);
                comboBoxFilter.setEnabled(true);
                orientText.setText("3");
                orientLabel.setEnabled(false);
                orientText.setEnabled(false);
            } // else if (undecimatedButton.isSelected()
            else {
                comboBoxFilter.setEnabled(false);
                orientText.setText("8");
                orientLabel.setEnabled(true);
                orientText.setEnabled(true);
            }
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

        if (algorithm instanceof AlgorithmDenoisingBLS_GSM) {

            if ((algorithm.isCompleted() == true) && (resultImage != null)) {

                // resultImage is the same or smaller than image.
                // The algorithm has completed and produced a new image to be displayed.
                try {

                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector imageFrames = image.getImageFrameVector();

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

        deAlgo.finalize();
        deAlgo = null;
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
     * DOCUMENT ME!
     *
     * @param  blockSizeX  DOCUMENT ME!
     */
    public void setBlockSizeX(int blockSizeX) {
        this.blockSizeX = blockSizeX;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  blockSizeY  DOCUMENT ME!
     */
    public void setBlockSizeY(int blockSizeY) {
        this.blockSizeY = blockSizeY;
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
     * DOCUMENT ME!
     *
     * @param  filter  DOCUMENT ME!
     */
    public void setFilter(int filter) {
        this.filter = filter;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  includeCovar  DOCUMENT ME!
     */
    public void setIncludeCovar(boolean includeCovar) {
        this.includeCovar = includeCovar;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  method  DOCUMENT ME!
     */
    public void setMethod(int method) {
        this.method = method;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  nOrientations  DOCUMENT ME!
     */
    public void setNOrientations(int nOrientations) {
        this.nOrientations = nOrientations;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  nScales  DOCUMENT ME!
     */
    public void setNScales(int nScales) {
        this.nScales = nScales;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  optimize  DOCUMENT ME!
     */
    public void setOptimize(boolean optimize) {
        this.optimize = optimize;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  sig  DOCUMENT ME!
     */
    public void setSig(float sig) {
        this.sig = sig;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  useBoundary  DOCUMENT ME!
     */
    public void setUseBoundary(boolean useBoundary) {
        this.useBoundary = useBoundary;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  useParent  DOCUMENT ME!
     */
    public void setUseParent(boolean useParent) {
        this.useParent = useParent;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  usePSD  DOCUMENT ME!
     */
    public void setUsePSD(boolean usePSD) {
        this.usePSD = usePSD;
    }


    /**
     * Once all the necessary variables are set, call the Frequency Filter algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_denoisingBLS_GSM");

        if (displayLoc == NEW) {

            try {
                resultImage = (ModelImage) image.clone();
                resultImage.setImageName(name);
                resultImage.resetVOIs();

                // Make algorithm
                deAlgo = new AlgorithmDenoisingBLS_GSM(resultImage, image, sig, usePSD, blockSizeX, blockSizeY,
                                                       useParent, useBoundary, nScales, nOrientations, includeCovar,
                                                       optimize, method, filter);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                deAlgo.addListener(this);

                createProgressBar(image.getImageName(), deAlgo);

                // Hide dialog since the algorithm is about to run
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (deAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    deAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog GaborFilter: unable to allocate enough memory");

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
                deAlgo = new AlgorithmDenoisingBLS_GSM(resultImage, image, sig, usePSD, blockSizeX, blockSizeY,
                                                       useParent, useBoundary, nScales, nOrientations, includeCovar,
                                                       optimize, method, filter);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                deAlgo.addListener(this);

                createProgressBar(image.getImageName(), deAlgo);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector imageFrames = image.getImageFrameVector();
                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((ViewJFrameBase) (imageFrames.elementAt(i))).getTitle();
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (deAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    deAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog DenoisingBLS_GSM: unable to allocate enough memory");

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

        if (scriptParameters.getParams().getBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE)) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }

        setSig(scriptParameters.getParams().getFloat("noise_std_dev"));
        setUsePSD(scriptParameters.getParams().getBoolean("use_power_spectral_density"));
        setBlockSizeX(scriptParameters.getParams().getInt("local_neighborhood_x"));
        setBlockSizeY(scriptParameters.getParams().getInt("local_neighborhood_y"));
        setUseParent(scriptParameters.getParams().getBoolean("use_parent"));
        setUseBoundary(scriptParameters.getParams().getBoolean("use_boundary"));
        setNScales(scriptParameters.getParams().getInt("number_of_scales"));
        setNOrientations(scriptParameters.getParams().getInt("number_of_orientations"));
        setIncludeCovar(scriptParameters.getParams().getBoolean("include_covariance"));
        setOptimize(scriptParameters.getParams().getBoolean("do_optimize"));
        setMethod(scriptParameters.getParams().getInt("wavelet_method"));
        setFilter(scriptParameters.getParams().getInt("wavelet_filter"));

    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("noise_std_dev", sig));
        scriptParameters.getParams().put(ParameterFactory.newParameter("use_power_spectral_density", usePSD));
        scriptParameters.getParams().put(ParameterFactory.newParameter("local_neighborhood_x", blockSizeX));
        scriptParameters.getParams().put(ParameterFactory.newParameter("local_neighborhood_y", blockSizeY));
        scriptParameters.getParams().put(ParameterFactory.newParameter("use_parent", useParent));
        scriptParameters.getParams().put(ParameterFactory.newParameter("use_boundary", useBoundary));
        scriptParameters.getParams().put(ParameterFactory.newParameter("number_of_scales", nScales));
        scriptParameters.getParams().put(ParameterFactory.newParameter("number_of_orientations", nOrientations));
        scriptParameters.getParams().put(ParameterFactory.newParameter("include_covariance", includeCovar));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_optimize", optimize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("wavelet_method", method));
        scriptParameters.getParams().put(ParameterFactory.newParameter("wavelet_filter", filter));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Denoising BLS_GSM");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 3;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;

        filterPanel = new JPanel(new GridBagLayout());
        filterPanel.setBorder(buildTitledBorder("Denoising Bayesian Least Squares Gray Scale Mixture"));

        methodGroup = new ButtonGroup();
        orthogonalButton = new JRadioButton("Orthogonal wavelet", true);
        orthogonalButton.setFont(serif12);
        orthogonalButton.setForeground(Color.black);
        orthogonalButton.addActionListener(this);
        methodGroup.add(orthogonalButton);

        undecimatedButton = new JRadioButton("Undecimated orthgonal wavelet", false);
        undecimatedButton.setFont(serif12);
        undecimatedButton.setForeground(Color.black);
        undecimatedButton.addActionListener(this);
        methodGroup.add(undecimatedButton);

        steerableButton = new JRadioButton("Steerable pyramid", false);
        steerableButton.setFont(serif12);
        steerableButton.setForeground(Color.black);
        steerableButton.addActionListener(this);
        methodGroup.add(steerableButton);

        fullButton = new JRadioButton("Full steerable pyramid", false);
        fullButton.setFont(serif12);
        fullButton.setForeground(Color.black);
        fullButton.addActionListener(this);
        methodGroup.add(fullButton);

        comboBoxFilter = new JComboBox();
        comboBoxFilter.setFont(serif12);
        comboBoxFilter.setBackground(Color.white);
        comboBoxFilter.setAlignmentX(Component.LEFT_ALIGNMENT);
        comboBoxFilter.insertItemAt("Daubechies 2", 0);
        comboBoxFilter.insertItemAt("Daubechies 3", 1);
        comboBoxFilter.insertItemAt("Daubechies 4", 2);
        comboBoxFilter.insertItemAt("Haar", 3);
        comboBoxFilter.insertItemAt("Quadrature Mirror Filter 5", 4);
        comboBoxFilter.insertItemAt("Quadrature Mirror Filter 8", 5);
        comboBoxFilter.insertItemAt("Quadrature Mirror Filter 9", 6);
        comboBoxFilter.insertItemAt("Quadrature Mirror Filter 12", 7);
        comboBoxFilter.insertItemAt("Quadrature Mirror Filter 13", 8);
        comboBoxFilter.insertItemAt("Quadrature Mirror Filter 16", 9);
        comboBoxFilter.setSelectedIndex(6);

        orientLabel = new JLabel("Number of orientations");
        orientLabel.setForeground(Color.black);
        orientLabel.setFont(serif12);
        orientLabel.setEnabled(false);

        orientText = new JTextField(10);
        orientText.setText("3");
        orientText.setFont(serif12);
        orientText.setEnabled(false);

        scaleLabel = new JLabel("Number of scales");
        scaleLabel.setForeground(Color.black);
        scaleLabel.setFont(serif12);
        scaleLabel.setEnabled(true);

        scaleText = new JTextField(10);
        scaleText.setText("4");
        scaleText.setFont(serif12);
        scaleText.setEnabled(true);

        noiseLabel = new JLabel("Noise standard deviation");
        noiseLabel.setForeground(Color.black);
        noiseLabel.setFont(serif12);
        noiseLabel.setEnabled(true);

        noiseText = new JTextField(10);
        noiseText.setText("1.0");
        noiseText.setFont(serif12);
        noiseText.setEnabled(true);

        noiseGroup = new ButtonGroup();
        whiteButton = new JRadioButton("Use white noise", true);
        whiteButton.setFont(serif12);
        whiteButton.setForeground(Color.black);
        noiseGroup.add(whiteButton);

        psdButton = new JRadioButton("Use power spectral density", false);
        psdButton.setFont(serif12);
        psdButton.setForeground(Color.black);
        noiseGroup.add(psdButton);

        localXLabel = new JLabel("Local neighborhood X (odd number)");
        localXLabel.setForeground(Color.black);
        localXLabel.setFont(serif12);
        localXLabel.setEnabled(true);

        localXText = new JTextField(10);
        localXText.setText("3");
        localXText.setFont(serif12);
        localXText.setEnabled(true);

        localYLabel = new JLabel("Local neighborhood Y (odd number)");
        localYLabel.setForeground(Color.black);
        localYLabel.setFont(serif12);
        localYLabel.setEnabled(true);

        localYText = new JTextField(10);
        localYText.setText("3");
        localYText.setFont(serif12);
        localYText.setEnabled(true);

        parentCheckBox = new JCheckBox("Use parent");
        parentCheckBox.setFont(serif12);
        parentCheckBox.setForeground(Color.black);
        parentCheckBox.setSelected(true);
        parentCheckBox.setEnabled(true);

        boundaryCheckBox = new JCheckBox("Use boundary");
        boundaryCheckBox.setFont(serif12);
        boundaryCheckBox.setForeground(Color.black);
        boundaryCheckBox.setSelected(true);
        boundaryCheckBox.setEnabled(true);

        covarianceCheckBox = new JCheckBox("Include covariance in GSM model");
        covarianceCheckBox.setFont(serif12);
        covarianceCheckBox.setForeground(Color.black);
        covarianceCheckBox.setSelected(true);
        covarianceCheckBox.setEnabled(true);

        optimizeCheckBox = new JCheckBox("Optimize");
        optimizeCheckBox.setFont(serif12);
        optimizeCheckBox.setForeground(Color.black);
        optimizeCheckBox.setSelected(true);
        optimizeCheckBox.setEnabled(true);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.weightx = .5;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        filterPanel.add(orthogonalButton, gbc);
        gbc.gridy = 1;
        filterPanel.add(undecimatedButton, gbc);
        gbc.gridy = 2;
        filterPanel.add(steerableButton, gbc);
        gbc.gridy = 3;
        filterPanel.add(fullButton, gbc);
        gbc.gridy = 4;
        filterPanel.add(comboBoxFilter, gbc);
        gbc.gridy = 5;
        filterPanel.add(orientLabel, gbc);
        gbc.gridx = 1;
        filterPanel.add(orientText, gbc);
        gbc.gridx = 0;
        gbc.gridy = 6;
        filterPanel.add(scaleLabel, gbc);
        gbc.gridx = 1;
        filterPanel.add(scaleText, gbc);
        gbc.gridx = 0;
        gbc.gridy = 7;
        filterPanel.add(noiseLabel, gbc);
        gbc.gridx = 1;
        filterPanel.add(noiseText, gbc);
        gbc.gridx = 0;
        gbc.gridy = 8;
        filterPanel.add(whiteButton, gbc);
        gbc.gridy = 9;
        filterPanel.add(psdButton, gbc);
        gbc.gridy = 10;
        filterPanel.add(localXLabel, gbc);
        gbc.gridx = 1;
        filterPanel.add(localXText, gbc);
        gbc.gridx = 0;
        gbc.gridy = 11;
        filterPanel.add(localYLabel, gbc);
        gbc.gridx = 1;
        filterPanel.add(localYText, gbc);
        gbc.gridx = 0;
        gbc.gridy = 12;
        filterPanel.add(parentCheckBox, gbc);
        gbc.gridy = 13;
        filterPanel.add(boundaryCheckBox, gbc);
        gbc.gridy = 14;
        filterPanel.add(covarianceCheckBox, gbc);
        gbc.gridy = 15;
        filterPanel.add(optimizeCheckBox, gbc);

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

        if (orthogonalButton.isSelected()) {
            method = ORTHOGONAL_WAVELET;
        } else if (undecimatedButton.isSelected()) {
            method = UNDECIMATED_ORTHOGONAL_WAVELET;
        } else if (steerableButton.isSelected()) {
            method = STEERABLE_PYRAMID;
        } else if (fullButton.isSelected()) {
            method = FULL_STEERABLE_PYRAMID;
        }

        tmpStr = (String) comboBoxFilter.getSelectedItem();

       if (tmpStr.equals("Daubechies 2")) {
            filter = DAUB2;
        } else if (tmpStr.equals("Daubechies 3")) {
            filter = DAUB3;
        } else if (tmpStr.equals("Daubechies 4")) {
            filter = DAUB4;
        } else if (tmpStr.equals("Haar")) {
            filter = HAAR;
        } else if (tmpStr.equals("Quadrature Mirror Filter 5")) {
            filter = QMF5;
        } else if (tmpStr.equals("Quadrature Mirror Filter 8")) {
            filter = QMF8;
        } else if (tmpStr.equals("Quadrature Mirror Filter 9")) {
            filter = QMF9;
        } else if (tmpStr.equals("Quadrature Mirror Filter 12")) {
            filter = QMF12;
        } else if (tmpStr.equals("Quadrature Mirror Filter 13")) {
            filter = QMF13;
        } else if (tmpStr.equals("Quadrature Mirror Filter 16")) {
            filter = QMF16;
        }

        tmpStr = orientText.getText();

        if (testParameter(tmpStr, 1, 100)) {
            nOrientations = Integer.valueOf(tmpStr).intValue();
        } else {
            MipavUtil.displayError("Number of orientations must be between 1 and 100");
            orientText.requestFocus();
            orientText.selectAll();

            return false;
        }

        tmpStr = scaleText.getText();

        if (testParameter(tmpStr, 1, 20)) {
            nScales = Integer.valueOf(tmpStr).intValue();
        } else {
            MipavUtil.displayError("Number of scales must be between 1 and 20");
            scaleText.requestFocus();
            scaleText.selectAll();

            return false;
        }

        tmpStr = noiseText.getText();

        if (testParameter(tmpStr, 0.0, 1.0E8)) {
            sig = Float.valueOf(tmpStr).floatValue();
        } else {
            MipavUtil.displayError("Noise standard deviation must be between 0.0 and 1.0E8");
            noiseText.requestFocus();
            noiseText.selectAll();

            return false;
        }

        tmpStr = localXText.getText();

        if (testParameter(tmpStr, 1, image.getExtents()[0])) {
            blockSizeX = Integer.valueOf(tmpStr).intValue();
        } else {
            MipavUtil.displayError("Local neighborhood X must be between 1 and " + image.getExtents()[0]);
            localXText.requestFocus();
            localXText.selectAll();

            return false;
        }

        if ((blockSizeX % 2) == 0) {
            MipavUtil.displayError("Local neighborhood X must be an odd number");
            localXText.requestFocus();
            localXText.selectAll();

            return false;
        }

        tmpStr = localYText.getText();

        if (testParameter(tmpStr, 1, image.getExtents()[1])) {
            blockSizeY = Integer.valueOf(tmpStr).intValue();
        } else {
            MipavUtil.displayError("Local neighborhood Y must be between 1 and " + image.getExtents()[1]);
            localYText.requestFocus();
            localYText.selectAll();

            return false;
        }

        if ((blockSizeY % 2) == 0) {
            MipavUtil.displayError("Local neighborhood Y must be an odd number");
            localYText.requestFocus();
            localYText.selectAll();

            return false;
        }

        usePSD = psdButton.isSelected();

        useParent = parentCheckBox.isSelected();

        useBoundary = boundaryCheckBox.isSelected();

        includeCovar = covarianceCheckBox.isSelected();

        optimize = optimizeCheckBox.isSelected();

        return true;
    }
}
