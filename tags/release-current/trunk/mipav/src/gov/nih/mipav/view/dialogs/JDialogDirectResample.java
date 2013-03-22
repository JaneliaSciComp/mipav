package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.ActionEvent;

import javax.swing.*;


/**
 * Dialog to ask user to resample the images or not.
 * 
 * @author Ruida Cheng
 */
public class JDialogDirectResample extends JDialogScriptableBase implements AlgorithmInterface {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2885627426314170051L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** The algorithm. */
    AlgorithmTransform algoTransform;

    /** Number of available dimension. */
    int dim;

    /** Boolean flag to enable volume render button. */
    boolean enableVolRender = true;

    /** Original dimensions extents value array. */
    int[] extents;

    /** Original X, Y, Z dimension extents values. */
    JTextField extXInput, extYInput, extZInput;

    /** Power of 2 X, Y, Z dimension extents values. */
    JTextField extXOutput, extYOutput, extZOutput;

    /** Boolean flag to do resample images. */
    boolean forceResample = false;

    /** Model images A and B. */
    ModelImage image, imageB;

    /** Left panel and right panels corresponding to original and expected extents. */
    JPanel leftPanel, rightPanel;

    /** Resample resolutioin corresponding to Power of 2. */
    float[] newRes = new float[3];

    /** Boolean flag to indicate the original image is in Power of 2. */
    boolean originalVolPowerOfTwo = true;

    /** Original resolutioin arrray. */
    float[] res;

    /** DOCUMENT ME! */
    ViewJFrameImage resampledImageFrame = null;

    /** Temp Model image. */
    ModelImage resultImage = null;

    /** DOCUMENT ME! */
    ModelImage resultImageB = null;

    /** Parent ui. */
    ViewUserInterface userInterface;

    /** Resampled dimension value in Power of 2. */
    int[] volExtents = new int[3];

    /** Volume size X*Y*Z. */
    int volSize = 1;

    /** DOCUMENT ME! */
    private JComboBox comboBoxInterp;

    /** DOCUMENT ME! */
    private int interp = 0;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Empty Contructor for script running.
     */
    public JDialogDirectResample() {}

    /**
     * Creates the dialog, using the input parameters to place it on the screen.
     * 
     * @param _imageA Model image A.
     * @param _imageB Model image B.
     */
    public JDialogDirectResample(final ModelImage _imageA, final ModelImage _imageB) {
        super(ViewUserInterface.getReference().getMainFrame(), false);
        this.image = _imageA;
        image.makeUnitsOfMeasureIdentical();
        this.imageB = _imageB;
        if (imageB != null) {
            imageB.makeUnitsOfMeasureIdentical();
        }
        this.userInterface = ViewUserInterface.getReference();
        extents = image.getExtents();
        res = image.getFileInfo(0).getResolutions();
        this.dim = extents.length;

        for (int i = 0; i < Math.min(3, extents.length); i++) {
            volExtents[i] = MipavMath.dimPowerOfTwo(extents[i]);
            volSize *= volExtents[i];

            if (volExtents[i] != extents[i]) {
                originalVolPowerOfTwo = false;
            }

            newRes[i] = (res[i] * (extents[i])) / (volExtents[i]);
        }

        init();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * On "OK", sets the name variable to the text entered. On "Cancel" disposes of this dialog and sets cancel flag.
     * 
     * @param event Event that triggered this method.
     */
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equals("OK")) {
            setInterp();

            if (dim >= 3) {

                if (extXOutput.getText().trim().equals("") || extYOutput.getText().trim().equals("")
                        || extZOutput.getText().trim().equals("")) {
                    return;
                }
            } else {

                if (extXOutput.getText().trim().equals("") || extYOutput.getText().trim().equals("")) {
                    return;
                }
            }

            volExtents[0] = MipavMath.dimPowerOfTwo(Integer.parseInt(extXOutput.getText()));
            newRes[0] = ( (extents[0]) * res[0]) / (volExtents[0]);
            volExtents[1] = MipavMath.dimPowerOfTwo(Integer.parseInt(extYOutput.getText()));
            newRes[1] = ( (extents[1]) * res[1]) / (volExtents[1]);

            if (dim >= 3) {
                volExtents[2] = MipavMath.dimPowerOfTwo(Integer.parseInt(extZOutput.getText()));
                newRes[2] = ( (extents[2]) * res[2]) / (volExtents[2]);
            }

            if (dim >= 3) {

                if ( (extents[0] == volExtents[0]) && (extents[1] == volExtents[1]) && (extents[2] == volExtents[2])) {
                    forceResample = false;
                } else {
                    forceResample = true;
                }
            } else {

                if ( (extents[0] == volExtents[0]) && (extents[1] == volExtents[1])) {
                    forceResample = false;
                } else {
                    forceResample = true;
                }
            }

            callAlgorithm();
            dispose();
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("xChanged")) {
            int x = Integer.parseInt(extXOutput.getText());
            x = MipavMath.dimPowerOfTwo(x);
            extXOutput.setText(Integer.toString(x));
        } else if (command.equals("yChanged")) {
            int y = Integer.parseInt(extYOutput.getText());
            y = MipavMath.dimPowerOfTwo(y);
            extYOutput.setText(Integer.toString(y));
        } else if (command.equals("zChanged")) {
            int z = Integer.parseInt(extZOutput.getText());
            z = MipavMath.dimPowerOfTwo(z);
            extZOutput.setText(Integer.toString(z));
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("19040");
            MipavUtil.showWebHelp("Transform_to_power_of_2#Resample_dialog_box");
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * Algorithm notifies dialog of status.
     * 
     * @param algo DOCUMENT ME!
     */
    public void algorithmPerformed(final AlgorithmBase algo) {

        if (algo instanceof AlgorithmTransform) {

            if (algoTransform.isCompleted()) {

                if (resultImage == null) {
                    resultImage = algoTransform.getTransformedImage();
                    resultImage.calcMinMax();

                    algoTransform.disposeLocal();
                    algoTransform = null;

                    if (imageB != null) {
                        callAlgorithm();

                        return;
                    } else {
                        resampledImageFrame = new ViewJFrameImage(resultImage, null, new Dimension(200, 200));
                        insertScriptLine();
                    }

                    return;
                } else if (imageB != null) {
                    resultImageB = algoTransform.getTransformedImage();
                    resultImageB.calcMinMax();

                    algoTransform.disposeLocal();
                    algoTransform = null;

                    resampledImageFrame = new ViewJFrameImage(resultImage, null, new Dimension(200, 200));
                    resampledImageFrame.setImageB(resultImageB);
                    resampledImageFrame.setControls();
                    resampledImageFrame.setTitle();
                    insertScriptLine();
                }
            }
        }
    }

    /**
     * Resample images to power of 2.
     */
    public void callAlgorithm() {

        if (forceResample && (resultImage == null)) {

            // resample image
            if (dim >= 3) {
                algoTransform = new AlgorithmTransform(image, new TransMatrix(4), interp, newRes[0], newRes[1],
                        newRes[2], volExtents[0], volExtents[1], volExtents[2], false, true, false);
            } else {
                algoTransform = new AlgorithmTransform(image, new TransMatrix(4), interp, newRes[0], newRes[1],
                        volExtents[0], volExtents[1], false, true, false);
            }

            algoTransform.addListener(this);

            createProgressBar(image.getImageName(), algoTransform);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (algoTransform.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                algoTransform.run();
            }

            return;
        }

        // resample imageB
        if ( (imageB != null) && forceResample) {

            // Resample image into volume that is a power of two !
            Preferences.debug("ViewJFrameSurfaceRenderer.buildTexture: Volume resampled.");

            if (dim >= 3) {
                algoTransform = new AlgorithmTransform(imageB, new TransMatrix(4), AlgorithmTransform.TRILINEAR,
                        newRes[0], newRes[1], newRes[2], volExtents[0], volExtents[1], volExtents[2], false, true,
                        false);
            } else {
                algoTransform = new AlgorithmTransform(imageB, new TransMatrix(4),

                        // AlgorithmTransform.CUBIC_LAGRANGIAN,
                        AlgorithmTransform.BILINEAR, newRes[0], newRes[1], volExtents[0], volExtents[1], false, true,
                        false);

            }

            algoTransform.addListener(this);

            createProgressBar(image.getImageName(), algoTransform);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (algoTransform.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                algoTransform.run();
            }
        }
    }

    /**
     * Dispose memory.
     * 
     * @param flag DOCUMENT ME!
     */
    public void dispose(final boolean flag) {

        /*
         * if (componentImage != null) { componentImage.setBuffers(null, null, null, null, null);
         * componentImage.setImageA(null); componentImage.setImageB(null); componentImage.dispose(false);
         * componentImage.disposeLocal(); componentImage = null; }
         * 
         * if (resampledImageFrame != null) { resampledImageFrame.dispose(); resampledImageFrame = null; }
         * 
         * if (resImageA != null) { resImageA.disposeLocal(); resImageA = null; }
         * 
         * if (resImageB != null) { resImageB.disposeLocal(); resImageA = null; }
         * 
         * if (resampledImage != null) { resampledImage.disposeLocal(); resampledImage = null; }
         */
        extents = null;
        res = null;
        volExtents = null;
        newRes = null;

        super.dispose();
    }

    /**
     * Build the resample dialog.
     */
    public void init() {
        setTitle("Resample Dialog");

        final Box mainBox = new Box(BoxLayout.Y_AXIS);

        /*
         * JPanel msgPanel = new JPanel(); msgPanel.setLayout(new BorderLayout());
         * msgPanel.setBorder(buildTitledBorder("")); msgPanel.add(new JLabel("Do you want to resample the images?"),
         * BorderLayout.NORTH); msgPanel.add(new JLabel("You can modify the extents to Power of 2."),
         * BorderLayout.CENTER);
         */
        final JPanel endPanel = new JPanel();
        endPanel.setLayout(new BorderLayout());
        // endPanel.add(new JLabel("Selecting Resample will resample the extents to Power of 2."), BorderLayout.NORTH);
        // endPanel.add(new JLabel("Cancel will exit the resample dialog."), BorderLayout.CENTER);

        // msgPanel.add(endPanel, BorderLayout.SOUTH);

        // mainBox.add(msgPanel);
        mainBox.add(endPanel);

        final Box contentBox = new Box(BoxLayout.X_AXIS);
        final JPanel leftPanel = new JPanel();
        final JPanel rightPanel = new JPanel();

        // make border
        leftPanel.setBorder(buildTitledBorder("Original Extents"));
        contentBox.add(leftPanel);

        // set layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        leftPanel.setLayout(gbl);

        // extent X
        leftPanel.add(Box.createHorizontalStrut(10));

        final JLabel extXLabel = new JLabel("extent X:");
        extXLabel.setFont(serif12);
        extXLabel.setForeground(Color.black);
        extXLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(extXLabel, gbc);
        leftPanel.add(extXLabel);
        leftPanel.add(Box.createHorizontalStrut(10));

        extXInput = new JTextField(Integer.toString(extents[0]), 3);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(extXInput, gbc);
        extXInput.setEnabled(false);
        leftPanel.add(extXInput);

        // extent Y
        leftPanel.add(Box.createHorizontalStrut(10));

        final JLabel extYLabel = new JLabel("extent Y:");
        extYLabel.setFont(serif12);
        extYLabel.setForeground(Color.black);
        extYLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(extYLabel, gbc);
        leftPanel.add(extYLabel);
        leftPanel.add(Box.createHorizontalStrut(10));

        extYInput = new JTextField(Integer.toString(extents[1]), 3);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(extYInput, gbc);
        extYInput.setEnabled(false);
        leftPanel.add(extYInput);

        if (dim >= 3) {

            // extent Z
            leftPanel.add(Box.createHorizontalStrut(10));

            final JLabel extZLabel = new JLabel("extent Z:");
            extZLabel.setFont(serif12);
            extZLabel.setForeground(Color.black);
            extZLabel.setRequestFocusEnabled(false);
            gbc.gridwidth = 2;
            gbl.setConstraints(extZLabel, gbc);
            leftPanel.add(extZLabel);
            leftPanel.add(Box.createHorizontalStrut(10));

            extZInput = new JTextField(Integer.toString(extents[2]), 3);
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(extZInput, gbc);
            extZInput.setEnabled(false);
            leftPanel.add(extZInput);
        }
        // make border

        rightPanel.setBorder(buildTitledBorder("Expected Extents"));
        contentBox.add(rightPanel);

        // set layout
        gbl = new GridBagLayout();
        gbc = new GridBagConstraints();
        rightPanel.setLayout(gbl);

        // extent X expected
        rightPanel.add(Box.createHorizontalStrut(10));

        final JLabel extXNewLabel = new JLabel("extent X:");
        extXNewLabel.setFont(serif12);
        extXNewLabel.setForeground(Color.black);
        extXNewLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(extXNewLabel, gbc);
        rightPanel.add(extXNewLabel);
        rightPanel.add(Box.createHorizontalStrut(10));

        extXOutput = new JTextField(Integer.toString(volExtents[0]), 3);
        extXOutput.addActionListener(this);
        extXOutput.setActionCommand("xChanged");
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(extXOutput, gbc);
        MipavUtil.makeNumericsOnly(extXOutput, false);
        rightPanel.add(extXOutput);

        // extent Y expected
        rightPanel.add(Box.createHorizontalStrut(10));

        final JLabel extYNewLabel = new JLabel("extent Y:");
        extYNewLabel.setFont(serif12);
        extYNewLabel.setForeground(Color.black);
        extYNewLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(extYNewLabel, gbc);
        rightPanel.add(extYNewLabel);
        rightPanel.add(Box.createHorizontalStrut(10));

        extYOutput = new JTextField(Integer.toString(volExtents[1]), 3);
        extYOutput.addActionListener(this);
        extYOutput.setActionCommand("yChanged");
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(extYOutput, gbc);
        MipavUtil.makeNumericsOnly(extYOutput, false);
        rightPanel.add(extYOutput);

        if (dim >= 3) {

            // extent Z expected
            rightPanel.add(Box.createHorizontalStrut(10));

            final JLabel extZNewLabel = new JLabel("extent Z:");
            extZNewLabel.setFont(serif12);
            extZNewLabel.setForeground(Color.black);
            extZNewLabel.setRequestFocusEnabled(false);
            gbc.gridwidth = 2;
            gbl.setConstraints(extZNewLabel, gbc);
            rightPanel.add(extZNewLabel);
            rightPanel.add(Box.createHorizontalStrut(10));

            extZOutput = new JTextField(Integer.toString(volExtents[2]), 3);
            extZOutput.addActionListener(this);
            extZOutput.setActionCommand("zChanged");
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(extZOutput, gbc);
            MipavUtil.makeNumericsOnly(extZOutput, false);
            rightPanel.add(extZOutput);
        }

        mainBox.add(contentBox);

        // *******INTERPOLATION****************
        final JPanel optionPanel = new JPanel();
        optionPanel.setForeground(Color.black);
        optionPanel.setBorder(buildTitledBorder("Options"));

        final JLabel labelInterp = new JLabel("Interpolation:");
        labelInterp.setForeground(Color.black);
        labelInterp.setFont(serif12);
        labelInterp.setAlignmentX(Component.LEFT_ALIGNMENT);
        comboBoxInterp = new JComboBox();
        comboBoxInterp.setFont(serif12);
        comboBoxInterp.setBackground(Color.white);
        comboBoxInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp.addItem("Nearest Neighbor");

        if (image.getNDims() == 2) {
            comboBoxInterp.addItem("Bilinear");
        } else {
            comboBoxInterp.addItem("Trilinear");
        }

        comboBoxInterp.addItem("Bspline 3rd order");
        comboBoxInterp.addItem("Bspline 4th order");
        comboBoxInterp.addItem("Cubic Lagrangian");
        comboBoxInterp.addItem("Quintic Lagrangian");
        comboBoxInterp.addItem("Heptic Lagrangian");
        comboBoxInterp.addItem("Windowed sinc");
        comboBoxInterp.setSelectedIndex(1);
        optionPanel.add(labelInterp);
        optionPanel.add(comboBoxInterp);

        mainBox.add(optionPanel);
        mainBox.add(buildButtons());

        getContentPane().add(mainBox);

        pack();
        setVisible(true);

    }

    /**
     * Perform any actions required after the running of the algorithm is complete.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(resultImage);

        if (resultImageB != null) {
            AlgorithmParameters.storeImageInRunner(resultImageB);
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @throws Throwable DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        dispose(true);
        super.finalize();
    }

    /**
     * Set up the dialog GUI based on the parameters before running the algorithm as part of a script.
     */
    protected void setGUIFromParams() {

        image = scriptParameters.retrieveInputImage();
        image.makeUnitsOfMeasureIdentical();

        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        imageB = ((ViewJFrameImage) parentFrame).getImageB(); // can be null
        if (imageB != null) {
            imageB.makeUnitsOfMeasureIdentical();
        }

        extents = image.getExtents();
        res = image.getFileInfo(0).getResolutions();
        dim = extents.length;

        for (int i = 0; i < extents.length; i++) {
            volExtents[i] = MipavMath.dimPowerOfTwo(extents[i]);
            volSize *= volExtents[i];

            newRes[i] = (res[i] * (extents[i])) / (volExtents[i]);
        }

        this.originalVolPowerOfTwo = false; // force this to act regardless (script expects a result image)
        forceResample = true;
    }

    /**
     * Store the parameters from the dialog to record the execution of this algorithm.
     * 
     * @throws ParserException If there is a problem creating one of the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        scriptParameters.storeImageInRecorder(resultImage);

        if (resultImageB != null) {
            scriptParameters.storeImageInRecorder(resultImageB);
        }

    }

    /**
     * Document ME.
     */
    private void setInterp() {
        final int boxIndex = comboBoxInterp.getSelectedIndex();

        if (boxIndex == 0) {
            interp = AlgorithmTransform.NEAREST_NEIGHBOR;
        } else if (boxIndex == 1) {

            if (image.getNDims() == 2) {
                interp = AlgorithmTransform.BILINEAR;
            } else {
                interp = AlgorithmTransform.TRILINEAR;
            }
        } // else if (boxIndex == 1)
        else if (boxIndex == 2) {
            interp = AlgorithmTransform.BSPLINE3;
        } else if (boxIndex == 3) {
            interp = AlgorithmTransform.BSPLINE4;
        } else if (boxIndex == 4) {
            interp = AlgorithmTransform.CUBIC_LAGRANGIAN;
        } else if (boxIndex == 5) {
            interp = AlgorithmTransform.QUINTIC_LAGRANGIAN;
        } else if (boxIndex == 6) {
            interp = AlgorithmTransform.HEPTIC_LAGRANGIAN;
        } else if (boxIndex == 7) {
            interp = AlgorithmTransform.WSINC;
        }
    }

}
