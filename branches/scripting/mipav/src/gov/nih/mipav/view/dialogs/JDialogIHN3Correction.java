package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm.
 */
public class JDialogIHN3Correction extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 9210373092056571003L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean autoThreshold;

    /** DOCUMENT ME! */
    private JCheckBox autoThresholdCheckbox;

    /** DOCUMENT ME! */
    private float convertTomm = 1.0f; // default for resXUnit in millimeters

    /** DOCUMENT ME! */
    private boolean createField;

    /** DOCUMENT ME! */
    private JCheckBox createFieldCheckbox;

    /** DOCUMENT ME! */
    private int[] destExtents;

    /** DOCUMENT ME! */
    private float endTol;

    /** DOCUMENT ME! */
    private float fieldDistance;

    /** DOCUMENT ME! */
    private ModelImage fieldImage = null;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private float kernelfwhm;

    /** DOCUMENT ME! */
    private JLabel labelDistance;

    /** DOCUMENT ME! */
    private JLabel labelEndTol;

    /** DOCUMENT ME! */
    private JLabel labelKernel;

    /** DOCUMENT ME! */
    private JLabel labelMaxIter;

    /** DOCUMENT ME! */
    private JLabel labelNoise;

    /** DOCUMENT ME! */
    private JLabel labelShrink;

    /** DOCUMENT ME! */
    private JLabel labelSignal;

    /** DOCUMENT ME! */
    private int maxIter;

    /** DOCUMENT ME! */
    private AlgorithmIHN3Correction N3Algo;

    /** DOCUMENT ME! */
    private float noise;

    /** DOCUMENT ME! */
    private float[] orgResol; // distance per pixel

    /** DOCUMENT ME! */
    private boolean regionFlag; // true = apply algorithm to the whole image

    // false = apply algorithm only to VOI regions

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private int resXUnit, resYUnit, resZUnit; // type of measurement unit

    /** DOCUMENT ME! */
    private float shrink;

    /** DOCUMENT ME! */
    private JTextField textDistance;

    /** DOCUMENT ME! */
    private JTextField textEndTol;

    /** DOCUMENT ME! */
    private JTextField textKernel;

    /** DOCUMENT ME! */
    private JTextField textMaxIter;

    /** DOCUMENT ME! */
    private JTextField textNoise;

    /** DOCUMENT ME! */
    private JTextField textShrink;

    /** DOCUMENT ME! */
    private JTextField textSignal;

    /** DOCUMENT ME! */
    private float threshold;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private boolean useScript = false;

    /** DOCUMENT ME! */
    private JRadioButton VOIRegions;

    /** DOCUMENT ME! */
    private JRadioButton wholeImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogIHN3Correction() { }

    /**
     * Create new dialog to set parameters for IHN3 algorithm.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogIHN3Correction(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, true);
        image = im;

        if ((image.getNDims() == 3) && (image.getExtents()[2] <= 3)) {
            MipavUtil.displayError("3D images must have at least 4 slices to use IHN3Correction");

            return;
        }

        userInterface = ViewUserInterface.getReference();
        initVars();
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
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Auto")) {

            if (autoThresholdCheckbox.isSelected()) {
                labelSignal.setEnabled(false);
                textSignal.setEnabled(false);
                wholeImage.setSelected(true);
                wholeImage.setEnabled(false);
                VOIRegions.setSelected(false);
                VOIRegions.setEnabled(false);
            } else {
                labelSignal.setEnabled(true);
                textSignal.setEnabled(true);
                wholeImage.setEnabled(true);
                VOIRegions.setEnabled(true);
            }
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("10101");
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
        if (algorithm instanceof AlgorithmIHN3Correction) {
            image.clearMask();

            if ((N3Algo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    JOptionPane.showMessageDialog(null, "Out of memory: unable to open new frame", "Error",
                                                  JOptionPane.ERROR_MESSAGE);
                }

                if (createField) {
                    updateFileInfo(image, fieldImage);
                    fieldImage.clearMask();

                    try {
                        new ViewJFrameImage(fieldImage, null, new Dimension(610, 220));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        JOptionPane.showMessageDialog(null, "Out of memory: unable to open new field frame", "Error",
                                                      JOptionPane.ERROR_MESSAGE);
                    }
                } // if (createField)
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;

                if (fieldImage != null) {
                    fieldImage.disposeLocal();
                    fieldImage = null;
                }

                System.gc();
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
        
        N3Algo.finalize();
        N3Algo = null;
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
        scriptParameters.storeOutputImageParams(getResultImage(), true);
        
        scriptParameters.storeProcessWholeImage(regionFlag);
        scriptParameters.getParams().put(ParameterFactory.newParameter("signal_threshold", threshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("end_tolerance", endTol));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_iterations", maxIter));
        scriptParameters.getParams().put(ParameterFactory.newParameter("field_distance_mm", fieldDistance));
        scriptParameters.getParams().put(ParameterFactory.newParameter("subsampling_factor", shrink));
        scriptParameters.getParams().put(ParameterFactory.newParameter("kernel_fwhm", kernelfwhm));
        scriptParameters.getParams().put(ParameterFactory.newParameter("wiener_noise_filter", noise));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_auto_histo_thresholding", autoThreshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_create_field_image", createField));
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
        
        setRegionFlag(scriptParameters.getParams().getBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE));
        setThreshold(scriptParameters.getParams().getFloat("signal_threshold"));
        setEndTol(scriptParameters.getParams().getFloat("end_tolerance"));
        setMaxIter(scriptParameters.getParams().getInt("max_iterations"));
        setFieldDistance(scriptParameters.getParams().getFloat("field_distance_mm"));
        setShrink(scriptParameters.getParams().getFloat("subsampling_factor"));
        setKernel(scriptParameters.getParams().getFloat("kernel_fwhm"));
        setNoise(scriptParameters.getParams().getFloat("wiener_noise_filter"));
        setAutoFlag(scriptParameters.getParams().getBoolean("do_auto_histo_thresholding"));
        setCreateField(scriptParameters.getParams().getBoolean("do_create_field_image"));
    }
    
    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * Accessor that sets the auto threshold flag.
     *
     * @param  flag  <code>true</code> indicates auto threshold, <code>false</code> otherwise.
     */
    public void setAutoFlag(boolean flag) {
        autoThreshold = flag;
    }

    /**
     * Accessor that sets the create field flag.
     *
     * @param  flag  <code>true</code> indicates create field image, <code>false</code> otherwise.
     */
    public void setCreateField(boolean flag) {
        createField = flag;
    }

    /**
     * Accessor that sets the end tol.
     *
     * @param  scale  Value to set end tol to.
     */
    public void setEndTol(float scale) {
        endTol = scale;
    }

    /**
     * Accessor that sets the field distance.
     *
     * @param  scale  Value to set field distance to.
     */
    public void setFieldDistance(float scale) {
        fieldDistance = scale;
    }

    /**
     * Accessor that sets the kernel.
     *
     * @param  scale  Value to set kernel to.
     */
    public void setKernel(float scale) {
        kernelfwhm = scale;
    }

    /**
     * Accessor that sets the max iterations.
     *
     * @param  max  The max iterations
     */
    public void setMaxIter(int max) {
        maxIter = max;
    }

    /**
     * Accessor that sets the noise.
     *
     * @param  scale  Value to set noise to.
     */
    public void setNoise(float scale) {
        noise = scale;
    }

    /**
     * Accessor that sets the region flag.
     *
     * @param  flag  <code>true</code> indicates the whole image is blurred, <code>false</code> indicates a region.
     */
    public void setRegionFlag(boolean flag) {
        regionFlag = flag;
    }

    /**
     * Accessor that sets the shrink.
     *
     * @param  scale  Value to set shrink to.
     */
    public void setShrink(float scale) {
        shrink = scale;
    }

    /**
     * Accessor that sets the threshold.
     *
     * @param  scale  Value to set the threshold to.
     */
    public void setThreshold(float scale) {
        threshold = scale;
    }

    /**
     * Once all the necessary variables are set, call the IHN3 Correction algorithm.
     */
    protected void callAlgorithm() {

        String newName = makeImageName(image.getImageName(), "_N3Corrected");

        if (image.getNDims() == 2) { // source image is 2D
            destExtents = new int[2];
            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim
        } else { // image.getNDims)() == 3
            destExtents = new int[3];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];
        }

        try {

            if ((image.getType() == ModelStorageBase.BYTE) || (image.getType() == ModelStorageBase.UBYTE)) {
                resultImage = new ModelImage(ModelStorageBase.SHORT, destExtents, newName, userInterface);
            } else {
                resultImage = new ModelImage(image.getType(), destExtents, newName, userInterface);
            }

            if (createField) {
                fieldImage = new ModelImage(ModelStorageBase.FLOAT, destExtents,
                                            makeImageName(image.getImageName(), "_Field"), userInterface);
            } // if (createField)

            // Make algorithm
            N3Algo = new AlgorithmIHN3Correction(resultImage, fieldImage, image, threshold, maxIter, endTol,
                                                 fieldDistance, shrink, kernelfwhm, noise, regionFlag, autoThreshold,
                                                 useScript);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            N3Algo.addListener(this);

            createProgressBar(image.getImageName(), N3Algo);
            
            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (N3Algo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                if (!userInterface.isAppFrameVisible()) {
                    N3Algo.setProgressBarVisible(false);
                }

                N3Algo.run();
            }
        } catch (OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            if (fieldImage != null) {
                fieldImage.disposeLocal();
                fieldImage = null;
            }

            System.gc();
            MipavUtil.displayError("Dialog IHN3Correction: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("N3 Inhomogeneity Correction");

        labelSignal = new JLabel("Signal threshold (default:min+1)");
        labelSignal.setForeground(Color.black);
        labelSignal.setFont(serif12);

        textSignal = new JTextField(5);
        textSignal.setText(String.valueOf(image.getMin() + 1.0f));
        textSignal.setFont(serif12);

        labelMaxIter = new JLabel("Maximum number of iterations.");
        labelMaxIter.setForeground(Color.black);
        labelMaxIter.setFont(serif12);

        textMaxIter = new JTextField(5);
        textMaxIter.setText("50");
        textMaxIter.setFont(serif12);

        labelEndTol = new JLabel("End tolerance (0.01-0.00001)");
        labelEndTol.setForeground(Color.black);
        labelEndTol.setFont(serif12);

        textEndTol = new JTextField(5);
        textEndTol.setText("0.001");
        textEndTol.setFont(serif12);

        if (resXUnit == FileInfoBase.MILLIMETERS) {
            labelDistance = new JLabel("Field distance (mm.)");
        } else if (resXUnit == FileInfoBase.INCHES) {
            labelDistance = new JLabel("Field distnace (in.)");
        } else if (resXUnit == FileInfoBase.CENTIMETERS) {
            labelDistance = new JLabel("Field distance (cm.)");
        } else if (resXUnit == FileInfoBase.ANGSTROMS) {
            labelDistance = new JLabel("Field distance (A.)");
        } else if (resXUnit == FileInfoBase.NANOMETERS) {
            labelDistance = new JLabel("Field distance (nm.)");
        } else if (resXUnit == FileInfoBase.MICROMETERS) {
            labelDistance = new JLabel("Field distance (um.)");
        } else if (resXUnit == FileInfoBase.METERS) {
            labelDistance = new JLabel("Field distance (m.)");
        } else if (resXUnit == FileInfoBase.KILOMETERS) {
            labelDistance = new JLabel("Field distance (km.)");
        } else if (resXUnit == FileInfoBase.MILES) {
            labelDistance = new JLabel("Field distance (mi.)");
        }

        labelDistance.setForeground(Color.black);
        labelDistance.setFont(serif12);

        textDistance = new JTextField(5);
        textDistance.setText(makeString(fieldDistance, 3));
        textDistance.setFont(serif12);

        labelShrink = new JLabel("Subsampling factor (1.0-32.0)");
        labelShrink.setForeground(Color.black);
        labelShrink.setFont(serif12);

        textShrink = new JTextField(5);
        textShrink.setText("4.0");
        textShrink.setFont(serif12);

        labelKernel = new JLabel("Kernel fwhm (0.05-0.50)");
        labelKernel.setForeground(Color.black);
        labelKernel.setFont(serif12);

        textKernel = new JTextField(5);
        textKernel.setText("0.15");
        textKernel.setFont(serif12);

        labelNoise = new JLabel("Wiener filter noise (0.0-0.1)");
        labelNoise.setForeground(Color.black);
        labelNoise.setFont(serif12);

        textNoise = new JTextField(5);
        textNoise.setText("0.01");
        textNoise.setFont(serif12);

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        paramPanel.add(labelSignal, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        paramPanel.add(textSignal, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        paramPanel.add(labelMaxIter, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        paramPanel.add(textMaxIter, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        paramPanel.add(labelEndTol, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        paramPanel.add(textEndTol, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        paramPanel.add(labelDistance, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        paramPanel.add(textDistance, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        paramPanel.add(labelShrink, gbc);
        gbc.gridx = 1;
        gbc.gridy = 4;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        paramPanel.add(textShrink, gbc);
        gbc.gridx = 0;
        gbc.gridy = 5;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        paramPanel.add(labelKernel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 5;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        paramPanel.add(textKernel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 6;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        paramPanel.add(labelNoise, gbc);
        gbc.gridx = 1;
        gbc.gridy = 6;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        paramPanel.add(textNoise, gbc);

        ButtonGroup imageVOIGroup = new ButtonGroup();
        wholeImage = new JRadioButton("Whole image", true);
        wholeImage.setFont(serif12);
        imageVOIGroup.add(wholeImage);

        VOIRegions = new JRadioButton("VOI region(s)", false);
        VOIRegions.setFont(serif12);
        imageVOIGroup.add(VOIRegions);

        JPanel imageVOIPanel = new JPanel(new GridBagLayout());
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Process"));

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        imageVOIPanel.add(wholeImage, gbc);
        gbc.gridy = 1;
        imageVOIPanel.add(VOIRegions, gbc);

        autoThresholdCheckbox = new JCheckBox("Automatic histogram thresholding");
        autoThresholdCheckbox.setFont(serif12);
        autoThresholdCheckbox.setSelected(false);
        autoThresholdCheckbox.setEnabled(true);
        autoThresholdCheckbox.addActionListener(this);
        autoThresholdCheckbox.setActionCommand("Auto");

        createFieldCheckbox = new JCheckBox("Create field image");
        createFieldCheckbox.setFont(serif12);
        createFieldCheckbox.setSelected(false);
        createFieldCheckbox.setEnabled(true);
        createFieldCheckbox.addActionListener(this);

        JPanel otherPanel = new JPanel(new GridBagLayout());
        otherPanel.setBorder(buildTitledBorder("Options"));

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        otherPanel.add(autoThresholdCheckbox, gbc);
        gbc.gridy = 1;
        otherPanel.add(createFieldCheckbox, gbc);

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.gridwidth = 2;
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(paramPanel, gbc);
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        mainPanel.add(imageVOIPanel, gbc);
        gbc.gridx = 1;
        mainPanel.add(otherPanel, gbc);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        getContentPane().add(mainPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setVisible(true);

    }

    /**
     * Initializes the variables (resolutions) needed for the algorithm.
     */
    private void initVars() {
        orgResol = new float[image.getNDims()];
        resXUnit = image.getFileInfo(0).getUnitsOfMeasure(0);

        if ((resXUnit == FileInfoBase.INCHES) || (resXUnit == FileInfoBase.CENTIMETERS) ||
                (resXUnit == FileInfoBase.ANGSTROMS) || (resXUnit == FileInfoBase.NANOMETERS) ||
                (resXUnit == FileInfoBase.MICROMETERS) || (resXUnit == FileInfoBase.MILLIMETERS) ||
                (resXUnit == FileInfoBase.METERS) || (resXUnit == FileInfoBase.KILOMETERS) ||
                (resXUnit == FileInfoBase.MILES)) {
            orgResol[0] = image.getFileInfo(0).getResolutions()[0];

            if (orgResol[0] <= 0.0f) {
                MipavUtil.displayError("X resolution was recorded as " + orgResol[0] + " It is being changed to 1.0");
                orgResol[0] = 1.0f;
            }

            // Be ready for conversions between different units.
            if (resXUnit == FileInfoBase.INCHES) {
                convertTomm = 25.4f;
            } else if (resXUnit == FileInfoBase.CENTIMETERS) {
                convertTomm = 10.0f;
            } else if (resXUnit == FileInfoBase.ANGSTROMS) {
                convertTomm = 1.0e-7f;
            } else if (resXUnit == FileInfoBase.NANOMETERS) {
                convertTomm = 1.0e-6f;
            } else if (resXUnit == FileInfoBase.MICROMETERS) {
                convertTomm = 1.0e-3f;
            } else if (resXUnit == FileInfoBase.METERS) {
                convertTomm = 1.0e3f;
            } else if (resXUnit == FileInfoBase.KILOMETERS) {
                convertTomm = 1.0e6f;
            } else if (resXUnit == FileInfoBase.MILES) {
                convertTomm = 1.6093e6f;
            }
        } else {
            MipavUtil.displayError("JDialogIHN3Correction - x dimension is not a distance measure");

            return;
        }

        resYUnit = image.getFileInfo(0).getUnitsOfMeasure(1);

        if ((resYUnit == FileInfoBase.INCHES) || (resYUnit == FileInfoBase.CENTIMETERS) ||
                (resYUnit == FileInfoBase.ANGSTROMS) || (resYUnit == FileInfoBase.NANOMETERS) ||
                (resYUnit == FileInfoBase.MICROMETERS) || (resYUnit == FileInfoBase.MILLIMETERS) ||
                (resYUnit == FileInfoBase.METERS) || (resYUnit == FileInfoBase.KILOMETERS) ||
                (resYUnit == FileInfoBase.MILES)) {
            orgResol[1] = image.getFileInfo(0).getResolutions()[1];

            if (orgResol[1] <= 0.0f) {
                MipavUtil.displayError("Y resolution was recorded as " + orgResol[1] + " It is being changed to 1.0");
                orgResol[1] = 1.0f;
            }

            if (resYUnit != resXUnit) {

                if (resYUnit == FileInfoBase.MILLIMETERS) {
                    orgResol[1] = orgResol[1] / convertTomm;
                } else if (resYUnit == FileInfoBase.INCHES) {
                    orgResol[1] = 25.4f * orgResol[1] / convertTomm;
                } else if (resYUnit == FileInfoBase.CENTIMETERS) {
                    orgResol[1] = 10.0f * orgResol[1] / convertTomm;
                } else if (resYUnit == FileInfoBase.ANGSTROMS) {
                    orgResol[1] = 1.0e-7f * orgResol[1] / convertTomm;
                } else if (resYUnit == FileInfoBase.NANOMETERS) {
                    orgResol[1] = 1.0e-6f * orgResol[1] / convertTomm;
                } else if (resYUnit == FileInfoBase.MICROMETERS) {
                    orgResol[1] = 1.0e-3f * orgResol[1] / convertTomm;
                } else if (resYUnit == FileInfoBase.METERS) {
                    orgResol[1] = 1.0e3f * orgResol[1] / convertTomm;
                } else if (resYUnit == FileInfoBase.KILOMETERS) {
                    orgResol[1] = 1.0e6f * orgResol[1] / convertTomm;
                } else if (resYUnit == FileInfoBase.MILES) {
                    orgResol[1] = 1.6093e6f * orgResol[1] / convertTomm;
                }
            } // if (resYUnit != resXUnit)
        } else {
            MipavUtil.displayError("JDialogIHN3Correction - y dimension is not a distance measure");

            return;
        }

        if (image.getNDims() == 3) {
            resZUnit = image.getFileInfo(0).getUnitsOfMeasure(2);

            if ((resZUnit == FileInfoBase.INCHES) || (resZUnit == FileInfoBase.CENTIMETERS) ||
                    (resZUnit == FileInfoBase.ANGSTROMS) || (resZUnit == FileInfoBase.NANOMETERS) ||
                    (resZUnit == FileInfoBase.MICROMETERS) || (resZUnit == FileInfoBase.MILLIMETERS) ||
                    (resZUnit == FileInfoBase.METERS) || (resZUnit == FileInfoBase.KILOMETERS) ||
                    (resZUnit == FileInfoBase.MILES)) {
                orgResol[2] = image.getFileInfo(0).getResolutions()[2];

                if (orgResol[2] <= 0.0f) {
                    MipavUtil.displayError("Z resolution was recorded as " + orgResol[2] +
                                           " It is being changed to 1.0");
                    orgResol[2] = 1.0f;
                }

                if (resZUnit != resXUnit) {

                    if (resZUnit == FileInfoBase.MILLIMETERS) {
                        orgResol[2] = orgResol[2] / convertTomm;
                    }

                    if (resZUnit == FileInfoBase.INCHES) {
                        orgResol[2] = 25.4f * orgResol[2] / convertTomm;
                    } else if (resZUnit == FileInfoBase.CENTIMETERS) {
                        orgResol[2] = 10.0f * orgResol[2] / convertTomm;
                    } else if (resZUnit == FileInfoBase.ANGSTROMS) {
                        orgResol[2] = 1.0e-7f * orgResol[2] / convertTomm;
                    } else if (resZUnit == FileInfoBase.NANOMETERS) {
                        orgResol[2] = 1.0e-6f * orgResol[2] / convertTomm;
                    } else if (resZUnit == FileInfoBase.MICROMETERS) {
                        orgResol[2] = 1.0e-3f * orgResol[2] / convertTomm;
                    } else if (resZUnit == FileInfoBase.METERS) {
                        orgResol[2] = 1.0e3f * orgResol[2] / convertTomm;
                    } else if (resZUnit == FileInfoBase.KILOMETERS) {
                        orgResol[2] = 1.0e6f * orgResol[2] / convertTomm;
                    } else if (resZUnit == FileInfoBase.MILES) {
                        orgResol[2] = 1.6093e6f * orgResol[2] / convertTomm;
                    }
                } // if (resZUnit != resXUnit)
            } else {
                MipavUtil.displayError("JDialogIHN3Correction - z dimension is not a distance measure");

                return;
            }
        } // if (image.getNDims() == 3)

        fieldDistance = orgResol[0] * image.getExtents()[0];

        for (int i = 1; i < image.getNDims(); i++) {

            if ((orgResol[i] * image.getExtents()[i]) < fieldDistance) {
                fieldDistance = orgResol[i] * image.getExtents()[i];
            }
        }

        fieldDistance /= 3.0f;
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        tmpStr = textSignal.getText();

        if (testParameter(tmpStr, -Float.MAX_VALUE, Float.MAX_VALUE)) {
            threshold = Float.valueOf(tmpStr).floatValue();
        } else {
            textSignal.requestFocus();
            textSignal.selectAll();

            return false;
        }

        tmpStr = textMaxIter.getText();

        if (testParameter(tmpStr, 1.0, 10000.0)) {
            maxIter = Integer.valueOf(tmpStr).intValue();
        } else {
            textMaxIter.requestFocus();
            textMaxIter.selectAll();

            return false;
        }

        tmpStr = textEndTol.getText();

        if (testParameter(tmpStr, 0.00001, 0.01)) {
            endTol = Float.valueOf(tmpStr).floatValue();
        } else {
            textEndTol.requestFocus();
            textEndTol.selectAll();

            return false;
        }

        tmpStr = textDistance.getText();

        if (testParameter(tmpStr, 0.00001, 200000.0)) {
            fieldDistance = Float.valueOf(tmpStr).floatValue();
        } else {
            textDistance.requestFocus();
            textDistance.selectAll();

            return false;
        }

        tmpStr = textShrink.getText();

        if (testParameter(tmpStr, 1.0, 32.0)) {
            shrink = Float.valueOf(tmpStr).floatValue();
        } else {
            textShrink.requestFocus();
            textShrink.selectAll();

            return false;
        }

        tmpStr = textKernel.getText();

        if (testParameter(tmpStr, 0.05, 2.0)) {
            kernelfwhm = Float.valueOf(tmpStr).floatValue();
        } else {
            textKernel.requestFocus();
            textKernel.selectAll();

            return false;
        }

        tmpStr = textNoise.getText();

        if (testParameter(tmpStr, 0.0, 0.10)) {
            noise = Float.valueOf(tmpStr).floatValue();
        } else {
            textNoise.requestFocus();
            textNoise.selectAll();

            return false;
        }

        if (wholeImage.isSelected()) {
            regionFlag = true;
        } else if (VOIRegions.isSelected()) {
            regionFlag = false;
        }

        if (autoThresholdCheckbox.isSelected()) {
            autoThreshold = true;
        } else {
            autoThreshold = false;
        }

        if (createFieldCheckbox.isSelected()) {
            createField = true;
        } else {
            createField = false;
        }

        return true;
    }
}
