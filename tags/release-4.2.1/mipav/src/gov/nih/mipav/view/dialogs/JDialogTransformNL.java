package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * Dialog to get user input, then call algorithmTransform. User may select resample or transform. User may input matrix
 * or use image's associated transformation matrix. User may input desired resolutions and dims. User may select
 * interpolation method. Creates new volume.
 */

public class JDialogTransformNL extends JDialogScriptableBase implements AlgorithmInterface, ChangeListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 6309170791275620891L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    int interp = 0;

    /** DOCUMENT ME! */
    int oXdim, oYdim, oZdim;

    /** DOCUMENT ME! */
    float oXres, oYres, oZres;

    /** DOCUMENT ME! */
    boolean transformVOI, doClip;

    /** or if the source image is to be replaced. */
    private AlgorithmRegNonlinear algoTrans = null;

    /** DOCUMENT ME! */
    private JCheckBox clipCheckbox, voiCheckbox;

    /** DOCUMENT ME! */
    private int coeffp;

    /** DOCUMENT ME! */
    private JComboBox comboBoxInterp;

    /** DOCUMENT ME! */
    private int coords;

    /** DOCUMENT ME! */
    private boolean endianess = true;

    /** DOCUMENT ME! */
    private double[][] es;

    /** DOCUMENT ME! */
    private JButton fileButton;

    /** DOCUMENT ME! */
    private int fmodel;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private int order;

    /** DOCUMENT ME! */
    private String parametersFile;

    /** DOCUMENT ME! */
    private JTextField parametersFName;

    /** DOCUMENT ME! */
    private RandomAccessFile raFile;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private float[] resultResolutions = new float[3];

    /** DOCUMENT ME! */
    private int[] targetUnits = new int[3];

    /** DOCUMENT ME! */
    private int targetXDim, targetYDim, targetZDim;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogTransformNL() { }

    /**
     * Constructs new transform dialog and sets up GUI components.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogTransformNL(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, sets the variables, and calls the algorithm.
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
        } else if (command.equals("fileChoose")) {
            parametersFile = parametersFileMenu();
        }
        else if (command.equals("Help")) {
            MipavUtil.showHelp("TransNon10");
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

        if (algorithm instanceof AlgorithmRegNonlinear) {
            resultImage = algoTrans.getTransformedImage();

            if ((algoTrans.isCompleted() == true) && (resultImage != null)) {
                resultImage.calcMinMax();

                // The algorithm has completed and produced a new image to be displayed.
                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        }

        System.gc();

        // Update frames
        image.notifyImageDisplayListeners(null, true);

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        if (algoTrans != null) {
            algoTrans.disposeLocal();
            algoTrans = null;
        }

        dispose();
    }

    /**
     * Reads eight unsigned bytes from file.
     *
     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the double read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final double getDouble(boolean endianess) throws IOException {
        long b1, b2, b3, b4, b5, b6, b7, b8;
        b1 = raFile.readUnsignedByte();
        b2 = raFile.readUnsignedByte();
        b3 = raFile.readUnsignedByte();
        b4 = raFile.readUnsignedByte();
        b5 = raFile.readUnsignedByte();
        b6 = raFile.readUnsignedByte();
        b7 = raFile.readUnsignedByte();
        b8 = raFile.readUnsignedByte();

        long tmpLong;

        if (endianess == true) {
            tmpLong = (((long) b1 << 56) | ((long) b2 << 48) | ((long) b3 << 40) | ((long) b4 << 32) |
                           ((long) b5 << 24) | ((long) b6 << 16) | ((long) b7 << 8) | b8);

            return (Double.longBitsToDouble(tmpLong));
        } else {
            tmpLong = (((long) b8 << 56) | ((long) b7 << 48) | ((long) b6 << 40) | ((long) b5 << 32) |
                           ((long) b4 << 24) | ((long) b3 << 16) | ((long) b2 << 8) | b1);

            return (Double.longBitsToDouble(tmpLong));
        }
    }

    /**
     * Reads four unsigned bytes from file.
     *
     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the float read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final float getFloat(boolean endianess) throws IOException {
        int b1, b2, b3, b4;
        b1 = raFile.readUnsignedByte();
        b2 = raFile.readUnsignedByte();
        b3 = raFile.readUnsignedByte();
        b4 = raFile.readUnsignedByte();

        int tmpInt;

        if (endianess == true) {
            tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

            return (Float.intBitsToFloat(tmpInt));
        } else {
            tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);

            return (Float.intBitsToFloat(tmpInt));
        }
    }

    /**
     * Reads four unsigned bytes from file.
     *
     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the integer read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final int getInt(boolean endianess) throws IOException {
        int b1, b2, b3, b4;
        b1 = raFile.readUnsignedByte();
        b2 = raFile.readUnsignedByte();
        b3 = raFile.readUnsignedByte();
        b4 = raFile.readUnsignedByte();

        if (endianess == true) {
            return ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
        } else {
            return ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
        }
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Method to handle item events.
     *
     * @param  event  event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (source == comboBoxInterp) {

            if (comboBoxInterp.getSelectedIndex() != 1) {
                clipCheckbox.setSelected(true);
                clipCheckbox.setEnabled(false);
            } else {
                clipCheckbox.setEnabled(true);
            }
        }

    }

    /**
     * Allows the user to select parameters file.
     *
     * @return  fileName
     */
    public String parametersFileMenu() {
        String fileName, directory;
        JFileChooser chooser;
        ViewUserInterface UI = ViewUserInterface.getReference();
        fileName = null;

        // bring up file dialog
        try {
            chooser = new JFileChooser();

            if (UI.getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(UI.getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MATRIX));

            int returnVal = chooser.showOpenDialog(UI.getMainFrame());

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                UI.setDefaultDirectory(directory);
                parametersFName.setText(fileName);
            } else {
                return null;
            }
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: JDialogTransformNL.parametersFileMenu");

            return null;
        }

        readTransformParameters(fileName);

        return fileName;
    }

    /**
     * Reads parameters from a file.
     *
     * @param  fileName  name of the matrix file.
     */
    public void readTransformParameters(String fileName) {
        int i, j;

        if (fileName == null) {
            MipavUtil.displayError("filename = null");
        }

        try {
            File file = new File(userInterface.getDefaultDirectory() + fileName);
            raFile = new RandomAccessFile(file, "r");
            targetXDim = getInt(endianess);
            targetYDim = getInt(endianess);
            targetZDim = getInt(endianess);
            resultResolutions[0] = getFloat(endianess);
            resultResolutions[1] = getFloat(endianess);
            resultResolutions[2] = getFloat(endianess);
            targetUnits[0] = getInt(endianess);
            targetUnits[1] = getInt(endianess);
            targetUnits[2] = getInt(endianess);
            fmodel = getInt(endianess);

            if (fmodel > 20) { // 2D processing
                coords = 2;
                order = fmodel - 20;
                coeffp = (order + 1) * (order + 2) / 2;
            } else {
                coords = 3;
                order = fmodel;
                coeffp = (order + 1) * (order + 2) * (order + 3) / 6;
            }

            es = new double[coords][coeffp];

            for (i = 0; i < coords; i++) {

                for (j = 0; j < coeffp; j++) {
                    es[i][j] = getDouble(endianess);
                }
            }

            raFile.close();

        } catch (IOException error) {
            MipavUtil.displayError("Parameters read error");
        }
    }

    /**
     * Accessor that sets the clip flag.
     *
     * @param  flag  <code>true</code> indicates clip image, <code>false</code> otherwise.
     */
    public void setClipFlag(boolean flag) {
        doClip = flag;
    }

    /**
     * Accessor to set the nonlinear transformation parameters.
     *
     * @param  es  DOCUMENT ME!
     */
    public void setEs(double[][] es) {
        this.es = es;
    }

    /**
     * Accessor to set the nonlinear model of the transformed image.
     *
     * @param  fmodel  DOCUMENT ME!
     */
    public void setFmodel(int fmodel) {
        this.fmodel = fmodel;
    }

    /**
     * Accessor to set the interpolation model.
     *
     * @param  interp  DOCUMENT ME!
     */
    public void setInterp(int interp) {
        this.interp = interp;
    }

    /**
     * Accessor to set the resolutions of the transformed image.
     *
     * @param  resultResolutions  DOCUMENT ME!
     */
    public void setResultResolutions(float[] resultResolutions) {
        this.resultResolutions = resultResolutions;
    }

    /**
     * Accessor to set the units of measurement of the transformed image.
     *
     * @param  targetUnits  DOCUMENT ME!
     */
    public void setTargetUnits(int[] targetUnits) {
        this.targetUnits = targetUnits;
    }

    /**
     * Accessor to set the x dimension of the target image.
     *
     * @param  targetXDim  DOCUMENT ME!
     */
    public void setTargetXDim(int targetXDim) {
        this.targetXDim = targetXDim;
    }

    /**
     * Accessor to set the y dimension of the target image.
     *
     * @param  targetYDim  DOCUMENT ME!
     */
    public void setTargetYDim(int targetYDim) {
        this.targetYDim = targetYDim;
    }

    /**
     * Accessor to set the z dimension of the target image.
     *
     * @param  targetZDim  DOCUMENT ME!
     */
    public void setTargetZDim(int targetZDim) {
        this.targetZDim = targetZDim;
    }

    /**
     * Accessor that sets the voi flag.
     *
     * @param  flag  <code>true</code> indicates transform VOI, <code>false</code> otherwise.
     */
    public void setVOIFlag(boolean flag) {
        transformVOI = flag;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void stateChanged(ChangeEvent e) {
        // Object source = e.getSource();
    }

    /**
     * Calls the algorithm with the set variables.
     */
    protected void callAlgorithm() {

        algoTrans = new AlgorithmRegNonlinear(image, targetXDim, targetYDim, targetZDim, resultResolutions, targetUnits,
                                              fmodel, es, interp, doClip, transformVOI);

        // This is very important. Adding this object as a listener allows
        // the algorithm to notify this object when it has completed of failed.
        // See algorithm performed event. This is made possible by implementing
        algoTrans.addListener(this);

        createProgressBar(image.getImageName(), algoTrans);

        // Start the thread as a low priority because we wish to still have
        // user interface work fast

        if (isRunInSeparateThread()) {

            if (algoTrans.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            algoTrans.run();
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(resultImage);
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        int[] outputDim = scriptParameters.getParams().getList("output_dim").getAsIntArray();
        targetXDim = outputDim[0];
        targetYDim = outputDim[1];
        targetZDim = outputDim[2];

        resultResolutions = scriptParameters.getParams().getList("output_res").getAsFloatArray();
        targetUnits = scriptParameters.getParams().getList("output_units").getAsIntArray();

        fmodel = scriptParameters.getParams().getInt("nonlinear_model");
        coords = scriptParameters.getParams().getInt("coords");

        es = new double[fmodel][coords];

        for (int i = 0; i < coords; i++) {
            es[i] = scriptParameters.getParams().getList("transform_parameters_" + i).getAsDoubleArray();
        }

        interp = scriptParameters.getParams().getInt("interpolation_type");
        doClip = scriptParameters.getParams().getBoolean("do_clip_output");
        transformVOI = scriptParameters.getParams().getBoolean("do_transform_VOIs");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeImageInRecorder(resultImage);

        scriptParameters.getParams().put(ParameterFactory.newParameter("output_dim",
                                                                       new int[] { targetXDim, targetYDim, targetZDim }));
        scriptParameters.getParams().put(ParameterFactory.newParameter("output_res", resultResolutions));
        scriptParameters.getParams().put(ParameterFactory.newParameter("output_units", targetUnits));
        scriptParameters.getParams().put(ParameterFactory.newParameter("nonlinear_model", fmodel));
        scriptParameters.getParams().put(ParameterFactory.newParameter("coords", coords));
        scriptParameters.getParams().put(ParameterFactory.newParameter("interpolation_type", interp));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_clip_output", doClip));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_transform_VOIs", transformVOI));


        for (int i = 0; i < coords; i++) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("transform_parameters_" + i, es[i]));
        }
    }

    /**
     * Builds the OptionPanel.
     *
     * @return  DOCUMENT ME!
     */
    private JPanel buildOptionPanel() {

        JPanel optionPanel = new JPanel();
        optionPanel.setForeground(Color.black);
        optionPanel.setBorder(buildTitledBorder("Options"));

        // *******INTERPOLATION****************
        JLabel labelInterp = new JLabel("Interpolation:");
        labelInterp.setForeground(Color.black);
        labelInterp.setFont(serif12);
        labelInterp.setAlignmentX(Component.LEFT_ALIGNMENT);
        comboBoxInterp = new JComboBox();
        comboBoxInterp.setFont(serif12);
        comboBoxInterp.setBackground(Color.white);
        comboBoxInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp.addItem("Linear");
        comboBoxInterp.addItem("Windowed sinc");
        comboBoxInterp.addItem("Nearest Neighbor");
        comboBoxInterp.addItemListener(this);

        clipCheckbox = new JCheckBox("Clip output values to input range");
        clipCheckbox.setFont(serif12);
        optionPanel.add(clipCheckbox);
        clipCheckbox.setSelected(true);
        clipCheckbox.setEnabled(false);
        clipCheckbox.addItemListener(this);
        clipCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);

        voiCheckbox = new JCheckBox("Transform VOIs");
        voiCheckbox.setFont(serif12);
        optionPanel.add(voiCheckbox);
        voiCheckbox.setSelected(false);
        voiCheckbox.addItemListener(this);
        voiCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);

        // Should be transform VOI also
        if ((image.getVOIs() == null) || (image.getVOIs().isEmpty() == true)) {
            voiCheckbox.setEnabled(false);
        }

        optionPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(2, 0, 2, 0);

        gbc.gridx = 0;
        gbc.gridy = 0;
        optionPanel.add(labelInterp, gbc);
        gbc.gridx = 1;
        optionPanel.add(Box.createHorizontalStrut(10), gbc);
        gbc.gridx = 2;
        gbc.gridwidth = 2;
        optionPanel.add(comboBoxInterp, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.weightx = 1;
        optionPanel.add(clipCheckbox, gbc);
        gbc.gridy = 3;
        optionPanel.add(voiCheckbox, gbc);

        return optionPanel;
    }

    /**
     * Builds the matrixPanel.
     *
     * @return  The matrix panel.
     */
    private JPanel buildParametersPanel() {

        JPanel parametersPanel = new JPanel();
        parametersPanel.setLayout(new BoxLayout(parametersPanel, BoxLayout.Y_AXIS));
        parametersPanel.setForeground(Color.black);
        parametersPanel.setBorder(buildTitledBorder("Transformation parameters"));

        JPanel filePanel = new JPanel(new BorderLayout());
        filePanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        fileButton = new JButton("Read parameters from file");
        fileButton.setFont(serif12);
        fileButton.setAlignmentX(Component.LEFT_ALIGNMENT);
        filePanel.add(fileButton, BorderLayout.WEST);
        fileButton.addActionListener(this);
        fileButton.setMargin(new Insets(0, 0, 0, 0));
        fileButton.setActionCommand("fileChoose");

        parametersFName = new JTextField(20);
        parametersFName.setFont(serif12);
        parametersFName.setAlignmentX(Component.LEFT_ALIGNMENT);
        parametersFName.setEnabled(false);
        filePanel.add(parametersFName);

        parametersPanel.add(filePanel);

        return parametersPanel;
    }

    /**
     * Initializes the dialog box to a certain size and adds the components.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Image Nonlinear Transformation");

        JPanel parametersPanel = buildParametersPanel();
        JPanel optionPanel = buildOptionPanel();

        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());
        mainPanel.add(parametersPanel);
        mainPanel.add(optionPanel, BorderLayout.SOUTH);

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(buildButtons());

        //buildOKButton();
        //buildCancelButton();
        //buttonPanel.add(OKButton);
       // buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        comboBoxInterp.setSelectedIndex(0);
        pack();
        setVisible(true);

    }

    /**
     * Sets the variables needed to run the algorithm.
     *
     * @return  Flag indicating successful set of the variables.
     */
    private boolean setVariables() {

        transformVOI = voiCheckbox.isSelected();
        doClip = clipCheckbox.isSelected();

        // Hide dialog
        setVisible(false);

        int boxIndex = comboBoxInterp.getSelectedIndex();

        if (boxIndex == 0) {

            if (fmodel > 20) {
                interp = AlgorithmTransform.BILINEAR;
            } else {
                interp = AlgorithmTransform.TRILINEAR;
            }
        } else if (boxIndex == 1) {
            interp = AlgorithmTransform.WSINC;
        } else if (boxIndex == 2) {
            interp = AlgorithmTransform.NEAREST_NEIGHBOR;
        }

        return true;
    }
}
