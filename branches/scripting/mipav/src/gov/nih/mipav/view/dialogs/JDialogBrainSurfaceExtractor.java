package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * This dialog collects parameters for the BSE algorithm and then starts it up.
 *
 * @version  1.0 June 3, 2004
 * @author   Evan McCreedy
 * @see      AlgorithmBrainSurfaceExtractor
 */
public class JDialogBrainSurfaceExtractor extends JDialogBase
        implements AlgorithmInterface, ScriptableInterface, DialogDefaultsInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4594568089644651497L;

    /** The nuber of pixels to include in the initial closing kernel diameter. */
    private static final int closeKernelPixels = 6;

    /** Number of closing operations to perform. */
    private static final int closeIterations = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Size of the kernel used in the closing operations. */
    private float closeKernelSize;

    /** Text field used to enter the number of closings to perform. */
    private JTextField closeKernelSizeTF;

    /** Edge detection kernel size (.6 in Shattuck paper). */
    private float edgeKernelSize = .62f;

    /** Text field used to enter the edge detection kernel size. */
    private JTextField edgeKernelSizeTF;

    /** Whether to process the slices of the 3D image separately while performing the erosion / dilation. */
    private boolean erosion25D = false;

    /** Check box for indicating whether erosion / dilation should process slices independently. */
    private JCheckBox erosion25DCB;

    /** Number of erosion / dilation operations to perform (2 in Shattuck paper). */
    private int erosionIterations = 1;

    /** Text field used to enter the number of erosions / dilations. */
    private JTextField erosionIterationsTF;

    /** Reference to the algorithm we will be running. */
    private AlgorithmBrainSurfaceExtractor extractBrainAlgo;

    /** Whether to extract the brain to paint instead of removing image data. */
    private boolean extractPaint = false;

    /** Checkbox to extract the brain to paint instead of removing image data. */
    private JCheckBox extractPaintCheckBox;

    /** Whether to fill in all internal holes in the extracted brain. */
    private boolean fillHoles = false;

    /** Checkbox for indicating whether to close all of the interior holes in the extracted brain. */
    private JCheckBox fillHolesCB;

    /** Standard deviation of the filter's gaussian kernel. */
    private float filterGaussianStdDev = 0.5f;

    /** Text field used to enter the standard deviation of the filter's gaussian kernel. */
    private JTextField filterGaussianStdDevTF;

    /** Number of filter iterations. */
    private int filterIterations = 3;

    /** Text field used to enter the number of filter iterations. */
    private JTextField filterIterationsTF;

    /** The source image. */
    private ModelImage image = null;

    /** A copy of the source image to run the algorithm on. */
    private ModelImage imageCopy = null;

    /** The original image name. */
    private String imgName;

    /** The result image. */
    private ModelImage resultImage = null;

    /** Whether to show images from intermediate steps of the BSE algorithm. */
    private boolean showIntermediateImages = false;

    /** Checkbox to show images which can help in paramater tweaking. */
    private JCheckBox showIntermediateImagesCB;

    /** Reference to the main user interface. */
    private ViewUserInterface userInterface;

    /** Whether to use a separable convolver during edge detection. */
    private boolean useSeparable = true;

    /** Checkbox to elect to use the separable convolver in the edge detection algorithm. */
    private JCheckBox useSeparableCB;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogBrainSurfaceExtractor() { }

    /**
     * Sets the appropriate variables. Does not actually create a dialog that is visible because no user input is
     * necessary at present.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogBrainSurfaceExtractor(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        parentFrame = theParentFrame;

        setForeground(Color.black);
        imgName = im.getImageName();
        image = im;
        userInterface = ((ViewJFrameBase) parentFrame).getUserInterface();

        // make the kernel encompas about 6 pixels total (diameter) (+1 to make sure that we get the last pixel inside
        // the kernel)
        closeKernelSize = (Math.max(im.getFileInfo(0).getResolutions()[0], im.getFileInfo(0).getResolutions()[1]) *
                               closeKernelPixels) + 1;

        init();
        loadDefaults();

        fillHolesCB.setSelected(true); // set to true by default, always
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Handles events generated by the user interface, and takes appropriate action.
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
            MipavUtil.showHelp("10090");
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && (this.getOwner() != null) && !isScriptRunning()) {
            saveDefaults();
        }

        if (algorithm instanceof AlgorithmBrainSurfaceExtractor) {


            if (extractBrainAlgo.isCompleted() == true) {
                userInterface.setDataText(image.getImageName() + " volume = \t" +
                                          (extractBrainAlgo.getBrainVolume() / 1000.0f) + " cc\n");
                resultImage = extractBrainAlgo.getResultImage();

                insertScriptLine(algorithm);

                if (extractPaint) {
                    BitSet paintMask = extractBrainAlgo.getComputedPaintMask();
                    ((ViewJFrameImage) parentFrame).getImageA().setMask(paintMask);
                    ((ViewJFrameImage) parentFrame).setActiveImage(ViewJFrameImage.IMAGE_A);

                    // clean up result image
                    resultImage.disposeLocal(true);
                    resultImage = null;
                } else {
                    resultImage.setImageName(imgName + "_brain");
                    resultImage.calcMinMax();

                    new ViewJFrameImage(resultImage, null, new Dimension(600, 600));
                }
            }

            extractBrainAlgo.finalize();

            if (imageCopy != null) {
                imageCopy.disposeLocal();
            }

            extractBrainAlgo = null;
        }
    }

    /**
     * Calls the algorithm.
     */
    public void callAlgorithm() {

        try {
            System.gc();

            if (!extractPaint) {

                // no paint is being used, brain extracted into separate frame, thus we need an image clone
                imageCopy = (ModelImage) image.clone(imgName + "_src");
            }

            extractBrainAlgo = new AlgorithmBrainSurfaceExtractor(image, filterIterations, filterGaussianStdDev,
                                                                  edgeKernelSize, erosion25D, erosionIterations,
                                                                  closeKernelSize, closeIterations,
                                                                  showIntermediateImages, fillHoles, useSeparable,
                                                                  extractPaint);


            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            extractBrainAlgo.addListener(this);

            /**
             * Creates the progress bar and make it listen to the FileBase.
             */
            ViewJProgressBar progressBar = new ViewJProgressBar(image.getImageName(), "Extracting brain...", 0, 100, true);
            progressBar.setSeparateThread(runInSeparateThread);
            extractBrainAlgo.addProgressChangeListener(progressBar);
            progressBar.setVisible(true);
           
            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (extractBrainAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                if (!userInterface.isAppFrameVisible()) {
                    extractBrainAlgo.setProgressBarVisible(false);
                }

                extractBrainAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog Extract Brain : unable to allocate enough memory");

            return;
        }
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
        str += filterIterations + delim;
        str += filterGaussianStdDev + delim;
        str += edgeKernelSize + delim;
        str += erosionIterations + delim;
        str += closeKernelSize + delim;
        str += closeIterations + delim;
        str += showIntermediateImages + delim;
        str += useSeparable + delim;
        str += 1 + delim;
        str += erosion25D + delim;
        str += fillHoles + delim;
        str += extractPaint;

        return str;
    }

    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     *
     * @param  algo  the algorithm to make an entry for
     */
    public void insertScriptLine(AlgorithmBase algo) {

        if (algo.isCompleted()) {

            if (userInterface.isScriptRecording()) {

                // check to see if the match image is already in the ImgTable
                if (userInterface.getScriptDialog().getImgTableVar(image.getImageName()) == null) {

                    if (userInterface.getScriptDialog().getActiveImgTableVar(image.getImageName()) == null) {
                        userInterface.getScriptDialog().putActiveVar(image.getImageName());
                    }
                }

                String line = "BrainSurfaceExtractor " + userInterface.getScriptDialog().getVar(image.getImageName()) +
                              " ";

                // if extracting brain to paint, there is no result image
                if (!extractPaint) {

                    // put the result image into the image variable table
                    userInterface.getScriptDialog().putVar(resultImage.getImageName());
                    line += userInterface.getScriptDialog().getVar(resultImage.getImageName()) + " ";
                } else {
                    line += userInterface.getScriptDialog().getVar(image.getImageName()) + " ";
                }

                line += getParameterString(" ") + "\n";
                userInterface.getScriptDialog().append(line);
            }
        }
    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if (defaultsString != null) {

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");
                filterIterationsTF.setText("" + MipavUtil.getInt(st));
                filterGaussianStdDevTF.setText("" + MipavUtil.getFloat(st));
                edgeKernelSizeTF.setText("" + MipavUtil.getFloat(st));
                erosionIterationsTF.setText("" + MipavUtil.getInt(st));
                closeKernelSizeTF.setText("" + MipavUtil.getFloat(st));
                st.nextToken(); // placeholder for close iterations
                showIntermediateImagesCB.setSelected(MipavUtil.getBoolean(st));
                useSeparableCB.setSelected(MipavUtil.getBoolean(st));
                st.nextToken(); // placeholder for erosion kernel size
                erosion25DCB.setSelected(MipavUtil.getBoolean(st));
                fillHolesCB.setSelected(MipavUtil.getBoolean(st));
                extractPaintCheckBox.setSelected(MipavUtil.getBoolean(st));
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
    public void saveDefaults() {
        String defaultsString = new String(getParameterString(","));

        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }

    /**
     * Run this algorithm from a script.
     *
     * @param   parser  the script parser we get the state from
     *
     * @throws  IllegalArgumentException  if there is something wrong with the arguments in the script
     */
    public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException {
        setScriptRunning(true);

        String srcImageKey = null;
        String destImageKey = null;

        try {
            srcImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        ModelImage im = parser.getImage(srcImageKey);
        imgName = im.getImageName();
        image = im;
        userInterface = image.getUserInterface();
        parentFrame = image.getParentFrame();

        try {
            destImageKey = parser.getNextString();
            setFilterIterations(parser.getNextInteger());
            setFilterGaussianStdDev(parser.getNextFloat());
            setEdgeKernelSize(parser.getNextFloat());
            setErosionIterations(parser.getNextInteger());
            closeKernelSize = parser.getNextFloat();
            parser.getNextInteger(); // placeholder for close iterations
            setShowIntermediateImages(parser.getNextBoolean());
            setUseSeparable(parser.getNextBoolean());
            parser.getNextFloat(); // placeholder for erosion kernel size
            erosion25D = parser.getNextBoolean();
            fillHoles = parser.getNextBoolean();
            extractPaint = parser.getNextBoolean();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        setSeparateThread(false);
        callAlgorithm();

        if (!srcImageKey.equals(destImageKey)) {
            parser.putVariable(destImageKey, resultImage.getImageName());
        }
    }

    /**
     * Accessor to set the edge detection kernel size.
     *
     * @param  s  the kernel size for edge detection
     */
    public void setEdgeKernelSize(float s) {
        edgeKernelSize = s;
    }

    /**
     * Accessor to set the number of erosions / dialations.
     *
     * @param  iter  the number of erosions / dialations to do
     */
    public void setErosionIterations(int iter) {
        erosionIterations = iter;
    }

    /**
     * Accessor to set the filter's gaussian standard deviation.
     *
     * @param  s  the standard deviation
     */
    public void setFilterGaussianStdDev(float s) {
        filterGaussianStdDev = s;
    }

    /**
     * Accessor to set the number of filter iterations.
     *
     * @param  iter  the number of filtering passes to make
     */
    public void setFilterIterations(int iter) {
        filterIterations = iter;
    }

    /**
     * Accessor to set whether intermediate images will be produced.
     *
     * @param  show  whether to keep intermediate images made
     */
    public void setShowIntermediateImages(boolean show) {
        showIntermediateImages = show;
    }

    /**
     * Accessor to set whether to use the separable convolver for edge detection.
     *
     * @param  use  whether use the separable convolver
     */
    public void setUseSeparable(boolean use) {
        useSeparable = use;
    }

    /**
     * Makes the GUI elements of the dialog.
     */
    private void init() {
        setTitle("Extract Brain (BSE)");

        mainDialogPanel.setLayout(new GridBagLayout());

        JPanel filterPanel = new JPanel(new GridBagLayout());

        filterPanel.setForeground(Color.black);
        filterPanel.setBorder(buildTitledBorder("Filtering"));

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JLabel filterIterationsLabel = new JLabel("Iterations (0 - 5) ");

        filterIterationsLabel.setFont(serif12);
        filterPanel.add(filterIterationsLabel, gbc);

        gbc.gridx = 1;
        filterIterationsTF = new JTextField();
        filterIterationsTF.setText("" + filterIterations);
        filterIterationsTF.setFont(serif12);
        filterPanel.add(filterIterationsTF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;

        JLabel filterGaussianStdDevLabel = new JLabel("Gaussian standard deviation (0.1 - 5.0) ");

        filterGaussianStdDevLabel.setFont(serif12);
        filterPanel.add(filterGaussianStdDevLabel, gbc);

        gbc.gridx = 1;
        filterGaussianStdDevTF = new JTextField();
        filterGaussianStdDevTF.setText("" + filterGaussianStdDev);
        filterGaussianStdDevTF.setFont(serif12);
        filterPanel.add(filterGaussianStdDevTF, gbc);

        JPanel edgePanel = new JPanel(new GridBagLayout());

        edgePanel.setForeground(Color.black);
        edgePanel.setBorder(buildTitledBorder("Edge Detection"));
        gbc.gridx = 0;
        gbc.gridy = 0;

        JLabel edgeKernelSizeLabel = new JLabel("Kernel size (0.1 - 5.0) ");

        edgeKernelSizeLabel.setFont(serif12);
        edgePanel.add(edgeKernelSizeLabel, gbc);

        gbc.gridx = 1;
        edgeKernelSizeTF = new JTextField();
        edgeKernelSizeTF.setText("" + edgeKernelSize);
        edgeKernelSizeTF.setFont(serif12);
        edgePanel.add(edgeKernelSizeTF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 2;
        useSeparableCB = new JCheckBox("Perform fast convolutions (requires more memory)", useSeparable);
        useSeparableCB.setFont(serif12);
        useSeparableCB.addItemListener(this);
        edgePanel.add(useSeparableCB, gbc);

        JPanel erosionPanel = new JPanel(new GridBagLayout());

        erosionPanel.setForeground(Color.black);
        erosionPanel.setBorder(buildTitledBorder("Erosion / Dilation"));

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;

        JLabel erosionIterationsLabel = new JLabel("Iterations (1 - 10) ");
        erosionIterationsLabel.setFont(serif12);
        erosionPanel.add(erosionIterationsLabel, gbc);

        gbc.gridx = 1;
        erosionIterationsTF = new JTextField();
        erosionIterationsTF.setText("" + erosionIterations);
        erosionIterationsTF.setFont(serif12);
        erosionPanel.add(erosionIterationsTF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 2;
        erosion25DCB = new JCheckBox("Process slices independently", this.erosion25D);
        erosion25DCB.setFont(serif12);
        erosion25DCB.addItemListener(this);
        erosionPanel.add(erosion25DCB, gbc);

        JPanel closePanel = new JPanel(new GridBagLayout());

        closePanel.setForeground(Color.black);
        closePanel.setBorder(buildTitledBorder("Closing"));
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;

        JLabel closeKernelSizeLabel = new JLabel("Kernel diameter (mm) ");
        closeKernelSizeLabel.setFont(serif12);
        closePanel.add(closeKernelSizeLabel, gbc);

        gbc.gridx = 1;
        closeKernelSizeTF = new JTextField();
        closeKernelSizeTF.setText("" + closeKernelSize);
        closeKernelSizeTF.setFont(serif12);
        closePanel.add(closeKernelSizeTF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 2;
        fillHolesCB = new JCheckBox("Fill all interior holes", fillHoles);
        fillHolesCB.setFont(serif12);
        fillHolesCB.addItemListener(this);
        closePanel.add(fillHolesCB, gbc);

        JPanel optionPanel = new JPanel(new GridLayout());

        optionPanel.setForeground(Color.black);
        optionPanel.setBorder(buildTitledBorder("Options"));

        showIntermediateImagesCB = new JCheckBox("Show intermediate images", showIntermediateImages);
        showIntermediateImagesCB.setFont(serif12);
        showIntermediateImagesCB.addItemListener(this);
        optionPanel.add(showIntermediateImagesCB);

        extractPaintCheckBox = new JCheckBox("Extract brain to paint", extractPaint);
        extractPaintCheckBox.setFont(serif12);
        optionPanel.add(extractPaintCheckBox);

        gbc.gridx = 0;
        gbc.gridy = 0;
        mainDialogPanel.add(filterPanel, gbc);
        gbc.gridy = 1;
        mainDialogPanel.add(edgePanel, gbc);
        gbc.gridy = 2;
        mainDialogPanel.add(erosionPanel, gbc);
        gbc.gridy = 3;
        mainDialogPanel.add(closePanel, gbc);
        gbc.gridy = 4;
        mainDialogPanel.add(optionPanel, gbc);
        gbc.gridy = 5;
        mainDialogPanel.add(buildButtons(), gbc);

        getContentPane().add(mainDialogPanel);

        pack();
        setResizable(false);
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        tmpStr = filterIterationsTF.getText();

        if (testParameter(tmpStr, 0, 5)) {
            filterIterations = Integer.valueOf(tmpStr).intValue();
        } else {
            filterIterationsTF.requestFocus();
            filterIterationsTF.selectAll();

            return false;
        }

        tmpStr = filterGaussianStdDevTF.getText();

        if (testParameter(tmpStr, .1f, 5.0f)) {
            filterGaussianStdDev = Float.valueOf(tmpStr).floatValue();
        } else {
            filterGaussianStdDevTF.requestFocus();
            filterGaussianStdDevTF.selectAll();

            return false;
        }

        tmpStr = edgeKernelSizeTF.getText();

        if (testParameter(tmpStr, 0.1f, 5.0f)) {
            edgeKernelSize = Float.valueOf(tmpStr).floatValue();
        } else {
            edgeKernelSizeTF.requestFocus();
            edgeKernelSizeTF.selectAll();

            return false;
        }

        useSeparable = useSeparableCB.isSelected();

        erosion25D = erosion25DCB.isSelected();

        tmpStr = erosionIterationsTF.getText();

        if (testParameter(tmpStr, 1, 10)) {
            erosionIterations = Integer.valueOf(tmpStr).intValue();
        } else {
            erosionIterationsTF.requestFocus();
            erosionIterationsTF.selectAll();

            return false;
        }

        tmpStr = closeKernelSizeTF.getText();

        if (testParameter(tmpStr, 1, 20)) {
            closeKernelSize = Float.valueOf(tmpStr).floatValue();
        } else {
            closeKernelSizeTF.requestFocus();
            closeKernelSizeTF.selectAll();

            return false;
        }

        fillHoles = fillHolesCB.isSelected();

        showIntermediateImages = showIntermediateImagesCB.isSelected();

        extractPaint = extractPaintCheckBox.isSelected();

        return true;
    }
}
