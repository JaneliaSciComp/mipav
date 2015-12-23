package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call AlgorithmMode. The user has the option to generate a new image or replace the
 * source image. In addition the user can select having the algorithm applied to whole image or to the VOI regions. It
 * should be noted that the algorithms are executed in their own thread.
 */

public class JDialogMode extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4183545541629455949L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JRadioButton bySlice; // allows selection of processing each slice of a 3D image-set individually

    /** DOCUMENT ME! */
    private JComboBox comboBoxKernelShape;

    /** DOCUMENT ME! */
    private JComboBox comboBoxKernelSize;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private boolean image25D = false; // flag indicating if slices should be processed independently

    /** DOCUMENT ME! */
    private int kernelShape;

    /** DOCUMENT ME! */
    private int kernelSize;

    /** DOCUMENT ME! */
    private AlgorithmMode modeAlgo = null;

    /** DOCUMENT ME! */
    private JPanelAlgorithmOutputOptions outputPanel;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JRadioButton wholeVolume;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogMode() { }

    /**
     * Creates a new JDialogMode object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogMode(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        if ((im.getType() != ModelImage.BYTE) && (im.getType() != ModelImage.UBYTE) &&
                (im.getType() != ModelImage.SHORT) && (im.getType() != ModelImage.USHORT) &&
                (im.getType() != ModelImage.INTEGER) && (im.getType() != ModelImage.UINTEGER)) {
            MipavUtil.displayError("Source Image must be an BYTE, SHORT, or INTEGER");
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
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Volume")) {

            // kernel Size
            int indx = comboBoxKernelSize.getSelectedIndex(); // get the current combo-box selection
            comboBoxKernelSize.removeAllItems();
            buildKernelSizeComboBox(false);
            comboBoxKernelSize.setSelectedIndex(indx); // set the new combo-box to the old selection

            // kernel Shape
            indx = comboBoxKernelShape.getSelectedIndex();
            comboBoxKernelShape.removeAllItems();
            buildKernelShapeComboBox(false);
            comboBoxKernelShape.setSelectedIndex(indx); // set the new combo-box to the old selection
            pack();
        } else if (command.equals("Slice")) {

            // kernel size
            int indx = comboBoxKernelSize.getSelectedIndex(); // get the current combo-box selection
            comboBoxKernelSize.removeAllItems();
            buildKernelSizeComboBox(true);
            comboBoxKernelSize.setSelectedIndex(indx); // set the new combo-box to the old selection

            // kernel shape
            // indx = comboBoxKernelShape.getSelectedIndex();
            indx = 0;
            comboBoxKernelShape.removeAllItems();
            buildKernelShapeComboBox(true);
            comboBoxKernelShape.setSelectedIndex(indx); // set the new combo-box to the old selection
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("10093");
            MipavUtil.showWebHelp("Filters_(Spatial):_Mode#Applying_the_Mode_algorithm");
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

        if (algorithm instanceof AlgorithmMode) {
            System.err.println("Mode Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((modeAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                try {

                    // resultImage.setImageName("Mode: "+image.getImageName());
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

        modeAlgo.finalize();
        modeAlgo = null;
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
     * Accessor that sets the slicing flag.
     *
     * @param  flag  <code>true</code> indicates slices should be processed independently.
     */
    public void setImage25D(boolean flag) {
        image25D = flag;
    }

    /**
     * Accessor that sets the kernel shape.
     *
     * @param  shape  Value to set size to (0 == square, 1 == cross).
     */
    public void setKernelShape(int shape) {
        kernelShape = shape;
    }

    /**
     * Accessor that sets the kernel size.
     *
     * @param  size  Value to set size to (3 == 3x3, 5 == 5x5, etc.)
     */
    public void setKernelSize(int size) {
        kernelSize = size;
    }

    /**
     * Once all the necessary variables are set, call the median algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_mode");

        // stuff to do when working on 2-D images.
        if (image.getNDims() == 2) { // source image is 2D

            if (outputPanel.isOutputNewImageSet()) { // (2D)

                try {

                    // Make result image of float type
                    // resultImage     = new ModelImage(image.getType(), destExtents, name, userInterface);
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                        ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                    }

                    // Make algorithm
                    modeAlgo = new AlgorithmMode(resultImage, image, kernelSize, kernelShape,
                                                 outputPanel.isProcessWholeImageSet());

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    modeAlgo.addListener(this);

                    createProgressBar(image.getImageName(), modeAlgo);

                    setVisible(false); // Hide dialog

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (modeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        modeAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog median: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }

                    return;
                }
            } else { // displayLoc == REPLACE        (2D)

                try {

                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    modeAlgo = new AlgorithmMode(image, kernelSize, kernelShape, outputPanel.isProcessWholeImageSet());

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    modeAlgo.addListener(this);

                    createProgressBar(image.getImageName(), modeAlgo);

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
                        if (modeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        modeAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog median: unable to allocate enough memory");

                    return;
                }
            }
        } else if (image.getNDims() == 3) {

            if (outputPanel.isOutputNewImageSet()) { // (3D)

                try {

                    // Make result image of float type
                    // resultImage     = new ModelImage(image.getType(), destExtents, name, userInterface);
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {

                        for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                        }
                    }

                    // Make algorithm
                    modeAlgo = new AlgorithmMode(resultImage, image, kernelSize, kernelShape, image25D,
                                                 outputPanel.isProcessWholeImageSet());

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    modeAlgo.addListener(this);

                    createProgressBar(image.getImageName(), modeAlgo);

                    setVisible(false); // Hide dialog

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (modeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        modeAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog median: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    return;
                }
            } else { // displayLoc == REPLACE         (3D)

                try {

                    // Make algorithm
                    modeAlgo = new AlgorithmMode(image, kernelSize, kernelShape, image25D,
                                                 outputPanel.isProcessWholeImageSet());

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    modeAlgo.addListener(this);

                    createProgressBar(image.getImageName(), modeAlgo);

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
                        if (modeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        modeAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog median: unable to allocate enough memory");

                    return;
                }
            }
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        if (outputPanel.isOutputNewImageSet()) {
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

        if ((image.getType() != ModelImage.BYTE) && (image.getType() != ModelImage.UBYTE) &&
                (image.getType() != ModelImage.SHORT) && (image.getType() != ModelImage.USHORT) &&
                (image.getType() != ModelImage.INTEGER) && (image.getType() != ModelImage.UINTEGER)) {
            MipavUtil.displayError("Source Image must be an BYTE, SHORT, or INTEGER");
            dispose();

            return;
        }

        outputPanel = new JPanelAlgorithmOutputOptions(image);
        scriptParameters.setOutputOptionsGUI(outputPanel);

        setImage25D(scriptParameters.getParams().getBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D));
        setKernelSize(scriptParameters.getParams().getInt("kernel_size"));
        setKernelShape(scriptParameters.getParams().getInt("kernel_shape"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        scriptParameters.storeOutputImageParams(getResultImage(), outputPanel.isOutputNewImageSet());
        scriptParameters.storeProcessingOptions(outputPanel.isProcessWholeImageSet(), image25D);

        scriptParameters.getParams().put(ParameterFactory.newParameter("kernel_size", kernelSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("kernel_shape", kernelShape));
    }

    /**
     * Creates the combo-box that allows user to select the shape of the kernel (mask).
     *
     * @param  singleSlices  DOCUMENT ME!
     */
    private void buildKernelShapeComboBox(boolean singleSlices) {

        if (singleSlices) {
            comboBoxKernelShape.addItem("Square");
            comboBoxKernelShape.addItem("Cross (+)");
            comboBoxKernelShape.addItem("Corner-to-corner (x)");
            comboBoxKernelShape.addItem("Horizontal");
            comboBoxKernelShape.addItem("Vertical");
        } else {
            comboBoxKernelShape.addItem("Cube");
            comboBoxKernelShape.addItem("Axis");
            comboBoxKernelShape.addItem("Corner-to-corner");
        }
    }

    /**
     * Creates the combo-box that allows user to select the size of the kernel (mask).
     *
     * @param  singleSlices  DOCUMENT ME!
     */
    private void buildKernelSizeComboBox(boolean singleSlices) {

        if (singleSlices) {
            comboBoxKernelSize.addItem("3x3");
            comboBoxKernelSize.addItem("5x5");
            comboBoxKernelSize.addItem("7x7");
            comboBoxKernelSize.addItem("9x9");
            comboBoxKernelSize.addItem("11x11");
        } else {
            comboBoxKernelSize.addItem("3x3x3");
            comboBoxKernelSize.addItem("5x5x5");
            comboBoxKernelSize.addItem("7x7x7");
            comboBoxKernelSize.addItem("9x9x9");
            comboBoxKernelSize.addItem("11x11x11");
        }
    }

    /**
     * Associate one side of the kernel size with selectBox choice.
     */
    private void determineKernelSize() {

        if (comboBoxKernelSize.getSelectedIndex() == 0) {
            kernelSize = 3;
        } else if (comboBoxKernelSize.getSelectedIndex() == 1) {
            kernelSize = 5;
        } else if (comboBoxKernelSize.getSelectedIndex() == 2) {
            kernelSize = 7;
        } else if (comboBoxKernelSize.getSelectedIndex() == 3) {
            kernelSize = 9;
        } else if (comboBoxKernelSize.getSelectedIndex() == 4) {
            kernelSize = 11;
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Mode Filter");

        // place everything setting up the kernel & filtering into the setupBox
        Box setupBox = new Box(BoxLayout.Y_AXIS);

        // maskPanel holds all the "Parameters"
        JPanel maskPanel = new JPanel();

        // panel gets a grid layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(2, 0, 2, 0); // component width = minwidth + (2ipadx)
        maskPanel.setLayout(gbl);

        maskPanel.setForeground(Color.black);
        maskPanel.setBorder(buildTitledBorder("Parameters")); // set the border ... "Parameters"

        JLabel labelKernelSize = createLabel("Kernel size:"); // make & set a label
        labelKernelSize.setForeground(Color.black);
        labelKernelSize.setFont(serif12);
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelKernelSize, gbc);
        maskPanel.add(labelKernelSize); // add kernel label

        comboBoxKernelSize = new JComboBox();
        comboBoxKernelSize.setFont(serif12);
        comboBoxKernelSize.setBackground(Color.white);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(comboBoxKernelSize, gbc);

        if (image.getNDims() == 2) {
            this.buildKernelSizeComboBox(true); // 2D images MUST be slice filtered
        } else {
            this.buildKernelSizeComboBox(true); // default setting for 3D+ images is slice filtering
        }

        maskPanel.add(comboBoxKernelSize); // add the comboboxt to the panel

        JLabel labelKernelShape = new JLabel("Kernel shape:"); // make & set a label
        labelKernelShape.setForeground(Color.black);
        labelKernelShape.setFont(serif12);
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelKernelShape, gbc);
        maskPanel.add(labelKernelShape); // add kernel label

        comboBoxKernelShape = new JComboBox();
        comboBoxKernelShape.setFont(serif12);
        comboBoxKernelShape.setBackground(Color.white);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(comboBoxKernelShape, gbc);

        if (image.getNDims() == 2) {
            this.buildKernelShapeComboBox(true); // 2D MUST be singleSlice filtered
        } else {
            this.buildKernelShapeComboBox(true); // default for 3D+ filtering is slice filtering
        }

        maskPanel.add(comboBoxKernelShape); // add the combo box to the panel

        setupBox.add(maskPanel); // the parameters-panel is at the top of the box

        outputPanel = new JPanelAlgorithmOutputOptions(image);
        setupBox.add(outputPanel);

        if (image.getNDims() > 2) { // only perform if the image has more than 1 slice

            JPanel kernelThicknessPanel = new JPanel();
            kernelThicknessPanel.setBorder(buildTitledBorder("Kernel Thickness")); // give the panel a border

            Box kernelThicknessBox = new Box(BoxLayout.Y_AXIS);

            ButtonGroup kernelThicknessGroup = new ButtonGroup();
            bySlice = new JRadioButton("Apply slice kernel", true);
            bySlice.setFont(serif12);
            bySlice.addActionListener(this);
            bySlice.setActionCommand("Slice");
            kernelThicknessGroup.add(bySlice); // add the button to the grouping
            kernelThicknessBox.add(bySlice); // add the button to the component

            wholeVolume = new JRadioButton("Apply volume kernel", false);
            wholeVolume.setFont(serif12);
            wholeVolume.addActionListener(this);
            wholeVolume.setActionCommand("Volume");
            kernelThicknessGroup.add(wholeVolume); // add the button to the grouping
            kernelThicknessBox.add(wholeVolume); // add the button to the component

            kernelThicknessPanel.add(kernelThicknessBox);
            setupBox.add(kernelThicknessPanel);
        }

        getContentPane().add(setupBox, BorderLayout.CENTER); // put the setupBox into the dialog

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

        // associate kernel size with selectBox choice.
        this.determineKernelSize();

        // get the index of the combo-box for kernelShape
        kernelShape = comboBoxKernelShape.getSelectedIndex();

        if (bySlice != null) {
            image25D = bySlice.isSelected();
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
                return new String("Algorithms.Filters (spatial)");
            }

            public String getDescription() {
                return new String("Applies a mode filter.");
            }

            public String getDescriptionLong() {
                return new String("Applies a mode filter.");
            }

            public String getShortLabel() {
                return new String("Mode");
            }

            public String getLabel() {
                return new String("Mode");
            }

            public String getName() {
                return new String("Mode");
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
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D, false));
            table.put(new ParameterInt("kernel_size", 3));
            table.put(new ParameterInt("kernel_shape", 0));
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


} // end class JDialogMode
