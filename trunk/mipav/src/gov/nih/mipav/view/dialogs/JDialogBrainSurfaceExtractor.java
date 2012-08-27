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
 * This dialog collects parameters for the BSE algorithm and then starts it up.
 *
 * @version  1.0 June 3, 2004
 * @author   Evan McCreedy
 * @see      AlgorithmBrainSurfaceExtractor
 */
public class JDialogBrainSurfaceExtractor extends JDialogScriptableBase
        implements AlgorithmInterface, ActionDiscovery, DialogDefaultsInterface {

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
     * Sets the appropriate variables. 
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
        
        Object eventSource = event.getSource();
        
        if (eventSource == OKButton){
            if (setVariables()) {
                callAlgorithm();
            }
        } else if (eventSource == cancelButton){
            dispose();
        } else if (eventSource == helpButton) {
            //MipavUtil.showHelp("10090");
            MipavUtil.showWebHelp("Extract_Brain:_Extract_Brain_Surface_(BSE)");
        } else {
            super.actionPerformed(event);
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

            if (extractBrainAlgo.isCompleted()) {
                insertScriptLine();
            }

            extractBrainAlgo.finalize();

            if (imageCopy != null) {
                imageCopy.disposeLocal();
            }
            //          necessary for paint to appear when using 'extract to paint' option
            if (parentFrame != null) {
                ((ViewJFrameImage) parentFrame).getComponentImage().setPaintMask(image.getMask());
            }

            image.notifyImageDisplayListeners(null, true);

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

            createProgressBar(image.getImageName(), extractBrainAlgo);
            progressBar.setMessage("Extracting brain ...");

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (extractBrainAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

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
     * Accessor to set the edge detection kernel size.
     *
     * @param  s  the kernel size for edge detection
     */
    public void setEdgeKernelSize(float s) {
        edgeKernelSize = s;
    }
    
    /**
     * Accessor to set the fill holes parameter.
     *
     * @param  b  when true holes are filled.
     */
    public void setFillHoles(boolean b) {
        fillHoles = b;
    }

    /**
     * Accessor to set the extract to paint parameter.
     *
     * @param  b  when true extract to paint.
     */
    public void setExtractPaint(boolean b) {
        extractPaint = b;
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
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        if (!extractPaint) {
            AlgorithmParameters.storeImageInRunner(resultImage);
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        
        
        if(scriptParameters.getParams().containsParameter("close_kernel_size")) {
            closeKernelSize = scriptParameters.getParams().getFloat("close_kernel_size");
        } else {
            closeKernelSize = (Math.max(image.getFileInfo(0).getResolutions()[0],
                    image.getFileInfo(0).getResolutions()[1]) * closeKernelPixels) + 1;
        }

        filterIterations = scriptParameters.getParams().getInt("filter_iterations");
        erosionIterations = scriptParameters.getParams().getInt("erosion_iterations");
        edgeKernelSize = scriptParameters.getParams().getFloat("edge_kernel_size");
        filterGaussianStdDev = scriptParameters.getParams().getFloat("filter_gaussian_std_dev");
        showIntermediateImages = scriptParameters.getParams().getBoolean("do_show_intermediate_images");
        useSeparable = scriptParameters.getParams().getBoolean("edge_do_separable_convolution");
        erosion25D = scriptParameters.getParams().getBoolean("do_erosion_2.5d");
        fillHoles = scriptParameters.getParams().getBoolean("do_fill_interior_holes");
        extractPaint = scriptParameters.getParams().getBoolean("do_extract_paint");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        if (!extractPaint) {
            scriptParameters.storeImageInRecorder(resultImage);
        }

        scriptParameters.getParams().put(ParameterFactory.newParameter("filter_iterations", filterIterations));
        scriptParameters.getParams().put(ParameterFactory.newParameter("edge_kernel_size", edgeKernelSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("erosion_iterations", erosionIterations));
        scriptParameters.getParams().put(ParameterFactory.newParameter("filter_gaussian_std_dev",
                                                                       filterGaussianStdDev));
        scriptParameters.getParams().put(ParameterFactory.newParameter("close_kernel_size", closeKernelSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_show_intermediate_images",
                                                                       showIntermediateImages));
        scriptParameters.getParams().put(ParameterFactory.newParameter("edge_do_separable_convolution", useSeparable));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_erosion_2.5d", erosion25D));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_fill_interior_holes", fillHoles));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_extract_paint", extractPaint));
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

        JLabel filterIterationsLabel = new JLabel("Iterations (1 - 5) ");

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

        JLabel closeKernelSizeLabel = new JLabel("Kernel diameter (0.1 - 50.0 mm) ");
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
        //setResizable(false);
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
    
    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms.Brain tools");
            }

            public String getDescription() {
                return new String("Segmentation of the brain from a 3D MRI.");
            }

            public String getDescriptionLong() {
                return new String("Segmentation of the brain from a 3D MRI.");
            }

            public String getShortLabel() {
                return new String("BSE");
            }

            public String getLabel() {
                return new String("Extract Brain Surface (BSE)");
            }

            public String getName() {
                return new String("Extract Brain Surface (BSE)");
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
            table.put(new ParameterInt("filter_iterations", 3));
            table.put(new ParameterFloat("filter_gaussian_std_dev", 0.5f));
            table.put(new ParameterFloat("edge_kernel_size", 0.62f));
            table.put(new ParameterFloat("close_kernel_size", 1.1f));
            table.put(new ParameterBoolean("edge_do_separable_convolution", true));
            table.put(new ParameterInt("erosion_iterations", 1));
            table.put(new ParameterBoolean("do_erosion_2.5d", false));
            table.put(new ParameterBoolean("do_fill_interior_holes", true));
            table.put(new ParameterBoolean("do_show_intermediate_images", false));
            table.put(new ParameterBoolean("do_extract_paint", false));    
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
                return resultImage.getImageName();
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
