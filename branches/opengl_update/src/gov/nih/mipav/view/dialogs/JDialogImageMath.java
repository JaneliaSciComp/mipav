package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageMath.Operator;
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
 * Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image. In addition the user can indicate if you wishes to have the algorithm applied to whole image or to the
 * VOI regions. In should be noted, that the algorithms are executed in their own thread.
 *
 * @version  0.1 Dec 21, 1999
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class JDialogImageMath extends JDialogScriptableBase implements AlgorithmInterface, DialogDefaultsInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2720105824431405427L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int clipMode = AlgorithmImageMath.CLIP;

    /** DOCUMENT ME! */
    private JComboBox comboBoxOperator;

    /** DOCUMENT ME! */
    private ButtonGroup group;

    /** source image. */
    private ModelImage image;

    /** Used as both imaginary value and blue value */
    private double imaginaryValue = 0.0;
    
    private double blueValue = 0.0;

    /** DOCUMENT ME! */
    private JPanel inputPanel;

    /** DOCUMENT ME! */
    private JLabel labelOperator;

    /** Used for single value, real part of complex, and red part of color */
    private JLabel labelValue;

    /** Used for imaginary part of complex and green part of color */
    private JLabel labelValueI;
    
    /** Used for blue part of color */
    private JLabel labelValueB;

    /** DOCUMENT ME! */
    private AlgorithmImageMath mathAlgo;

    /** DOCUMENT ME! */
    private Operator opType;

    /** DOCUMENT ME! */
    private JPanelAlgorithmOutputOptions outputPanel;

    /** DOCUMENT ME! */
    private JRadioButton radioClip;

    /** DOCUMENT ME! */
    private JRadioButton radioPromote;
    
    /** DOCUMENT ME! */
    private JRadioButton radioFloat;
    

    /** DOCUMENT ME! */
    private double realValue;

    /** result image. */
    private ModelImage resultImage = null;

    /** Used for single value, real part of complex, and red part of color */
    private JTextField textValue;

    /** Use for imaginary part of complex and green part of color */
    private JTextField textValueI;
    
    /** Used for blue part of color */
    private JTextField textValueB;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private boolean useComplex = false;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogImageMath() { }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogImageMath(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;

        if (image.isComplexImage()) {
            useComplex = true;
        }

        userInterface = ViewUserInterface.getReference();
        init();
        loadDefaults();
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
        } if (command.equals("Help")) {
            MipavUtil.showHelp("U4033");
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

        if (Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && (this.getOwner() != null) && !isScriptRunning()) {
            saveDefaults();
        }

        if (algorithm instanceof AlgorithmImageMath) {
            image.clearMask();

            if ((mathAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                try {
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

                if (image.isColorImage()) {
                    image.notifyImageDisplayListeners(true, 0, null);    
                }
                else {
                    image.notifyImageDisplayListeners(null, true);
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

        // Update frame
        if (parentFrame != null) {
            ((ViewJFrameBase) parentFrame).updateImages(true);
        }

        mathAlgo.finalize();
        mathAlgo = null;
        dispose();
    }

    /**
     * focusLost - when the user clicks the mouse out of a text field, resets the neccessary variables.
     *
     * @param  event  event that triggers this function
     */
    public void focusLost(FocusEvent event) { }

    /**
     * Return the correct extension for the new image based on the given opType.
     *
     * @param   op  - integer defining the math operation. These are defined in AlgorithmImageMath.
     *
     * @return  string - the proper extension to append to the image name. For instance, for operator =
     *          AlgorithmImageMath.ADD the extension returned would be "_add".
     */
    public String getOpName(int opType) {
        Operator op = Operator.getOperatorFromLegacyNum(opType);
        
        String name = new String("_"+op.getShortOp().toLowerCase());
        
        return name;
    } // end getOpName

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
        str += outputPanel.isProcessWholeImageSet() + delim;
        str += realValue + delim;
        str += imaginaryValue + delim;
        str += blueValue + delim;
        str += opType + delim;
        str += clipMode;

        return str;
    }

    /**
     * Accessor that returns the image.
     *
     * @return  the result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * itemStateChanged - unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        textValue.setEnabled(true);
        if (useComplex || image.isColorImage()) {
            textValueI.setEnabled(true);
        }
        if (image.isColorImage()) {
            textValueB.setEnabled(true);
        }
        outputPanel.setOutputImageOptionsEnabled(true);
        radioClip.setEnabled(true);
        radioPromote.setEnabled(true);
        if (image.getType() == ModelStorageBase.FLOAT || image.getType() == ModelStorageBase.DOUBLE ||
            image.getType() == ModelStorageBase.ARGB_FLOAT) {
        	radioFloat.setEnabled(false);
        } else {
        	radioFloat.setEnabled(true);
        }
        

        String selectedText;

        if (source == comboBoxOperator) {

            selectedText = (String)comboBoxOperator.getSelectedItem();

            if (selectedText.equals("Add")) {
                opType = Operator.ADD;
            } else if (selectedText.equals("Subtract")) {
                opType = Operator.SUBTRACT;
            } else if (selectedText.equals("Multiply")) {
                opType = Operator.MULTIPLY;
            } else if (selectedText.equals("Divide")) {
                opType = Operator.DIVIDE;
            } else if (selectedText.equals("Square")) {
                textValue.setEnabled(false);
                if (useComplex || image.isColorImage()) {
                    textValueI.setEnabled(false);
                }
                if (image.isColorImage()) {
                    textValueB.setEnabled(false);
                }
                opType = Operator.SQUARE;
            } else if (selectedText.equals("Square Root")) {
                textValue.setEnabled(false);
                if (useComplex || image.isColorImage()) {
                    textValueI.setEnabled(false);
                }
                if (image.isColorImage()) {
                    textValueB.setEnabled(false);
                }
                opType = Operator.SQUARE_ROOT;
                radioClip.setEnabled(false);
                radioPromote.setEnabled(false);
                radioFloat.setEnabled(false);
                radioFloat.setSelected(true);
            } else if (selectedText.equals("Log")) {
                textValue.setEnabled(false);
                if (useComplex || image.isColorImage()) {
                    textValueI.setEnabled(false);
                }
                if (image.isColorImage()) {
                    textValueB.setEnabled(false);
                }
                opType = Operator.LOG;
                radioClip.setEnabled(false);
                radioPromote.setEnabled(false);
                radioFloat.setEnabled(false);
                radioFloat.setSelected(true);
            } else if (selectedText.equals("Constant")) {
                opType = Operator.CONSTANT;
            } else if (selectedText.equals("Absolute Value")) {
                textValue.setEnabled(false);
                if (useComplex || image.isColorImage()) {
                    textValueI.setEnabled(false);
                }
                if (image.isColorImage()) {
                    textValueB.setEnabled(false);
                }
                opType = Operator.ABSOLUTE_VALUE;
            } else if (selectedText.equals("Average")) {
                textValue.setEnabled(false);
                if (useComplex || image.isColorImage()) {
                    textValueI.setEnabled(false);
                }
                if (image.isColorImage()) {
                    textValueB.setEnabled(false);
                }
                outputPanel.setOutputNewImage(true);
                outputPanel.setOutputImageOptionsEnabled(false);
                opType = Operator.AVERAGE;
            } else if (selectedText.equals("Sum")) {
                textValue.setEnabled(false);
                if (useComplex || image.isColorImage()) {
                    textValueI.setEnabled(false);
                }
                if (image.isColorImage()) {
                    textValueB.setEnabled(false);
                }
                outputPanel.setOutputNewImage(true);
                outputPanel.setOutputImageOptionsEnabled(false);
                opType = Operator.SUM;
            }else if (selectedText.equals("Inverse (1 / intensity)")) {
                opType = Operator.INVERSE;
                radioClip.setEnabled(false);
                radioPromote.setEnabled(false);
                radioFloat.setEnabled(false);
                radioFloat.setSelected(true);
            } 
        }
    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if ((defaultsString != null) && (comboBoxOperator != null)) {

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");
                // System.err.println("defaultsSTring is: "+ defaultsString);

                outputPanel.setProcessWholeImage(MipavUtil.getBoolean(st));

                textValue.setText(st.nextToken());

                String iString = st.nextToken();

                if (useComplex || image.isColorImage()) {
                    textValueI.setText(iString);
                }
                
                String blueString = st.nextToken();
                if (image.isColorImage()) {
                    textValueB.setText(blueString);
                }

                int selection = MipavUtil.getInt(st);

                if (comboBoxOperator.getItemCount() > selection) {
                    comboBoxOperator.setSelectedIndex(selection);
                }

                int mode = MipavUtil.getInt(st);

                if (mode == AlgorithmImageMath.CLIP) {
                    radioClip.setSelected(true);
                } else if (mode == AlgorithmImageMath.CONVERT_FLOAT) {
                	radioFloat.setSelected(true);
                } else {
                	radioPromote.setSelected(true);
                }

                outputPanel.setOutputNewImage(MipavUtil.getBoolean(st));
            } catch (Exception ex) {

                // since there was a problem parsing the defaults string, start over with the original defaults
                Preferences.debug("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
                ex.printStackTrace();
            }

        }
    }

    /**
     * Saves the default settings into the Preferences file.
     */
    public void saveDefaults() {
        String defaultsString = new String(getParameterString(",") + "," + outputPanel.isOutputNewImageSet());

        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }

    /**
     * Accessor that sets the clip mode.
     *
     * @param  n  the clip mode to be used when performing the math algorithm
     */
    public void setClipMode(int n) {
        clipMode = n;
    }

    /**
     * Accessor that sets the imaginaryValue or green value to be used when performing the algorithm.
     *
     * @param  v  realValue
     */
    public void setImaginaryValue(double v) {
        imaginaryValue = v;
    }
    
    public void setBlueValue(double blueValue) {
        this.blueValue = blueValue;
    }

    /**
     * Accessor that sets the operator type.
     *
     * @param  n  operator type
     */
    public void setOperator(int n) {
        opType = Operator.getOperatorFromLegacyNum(n);
    }

    /**
     * Accessor that sets the realValue to be used when performing the algorithm.
     *
     * @param  v  realValue
     */
    public void setRealValue(double v) {
        realValue = v;
    }

    public void setImage(ModelImage image) {
        this.image = image;
    }

    /**
     * Once all the necessary variables are set, call the Image Math algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        int i;

        if (image.getNDims() <= 5) {

            if (outputPanel.isOutputNewImageSet()) {

                try {

                    // make the new image name
                    String math = getOpName(opType.getLegacyNum());
                    String name = makeImageName(image.getImageName(), math);

                    // Make result image of source type

                    if ((opType != Operator.AVERAGE) && (opType != Operator.SUM)) {
                        resultImage = new ModelImage(image.getType(), image.getExtents(), name);
                    } else {
                        int[] extents = new int[Math.max(2,image.getNDims()-1)];
                        for (i = 0; i < Math.max(2, image.getNDims() - 1); i++) {
                            extents[i] = image.getExtents()[i];
                        }

                        resultImage = new ModelImage(image.getType(), extents, name);
                       
                    }

                    // Make algorithm
                    mathAlgo = new AlgorithmImageMath(resultImage, image, opType.getLegacyNum(), realValue, imaginaryValue, 
                                                      blueValue, clipMode,
                                                      outputPanel.isProcessWholeImageSet());

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    mathAlgo.addListener(this);

                    createProgressBar(image.getImageName(), mathAlgo);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (mathAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        mathAlgo.run();
                    }
                } catch (OutOfMemoryError x) {

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }

                    System.gc();
                    MipavUtil.displayError("Dialog Image math: unable to allocate enough memory");

                    return;
                }
            } else {

                try {

                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    mathAlgo = new AlgorithmImageMath(image, opType.getLegacyNum(), realValue, imaginaryValue, 
                                                      blueValue, clipMode,
                                                      outputPanel.isProcessWholeImageSet());

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    mathAlgo.addListener(this);

                    createProgressBar(image.getImageName(), mathAlgo);

                    // Hide the dialog since the algorithm is about to run.
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                    titles = new String[imageFrames.size()];

                    for (i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface.
                        if (mathAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        mathAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    System.gc();
                    MipavUtil.displayError("Dialog Image Math: unable to allocate enough memory");

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

        outputPanel = new JPanelAlgorithmOutputOptions(image);
        scriptParameters.setOutputOptionsGUI(outputPanel);

        setRealValue(scriptParameters.getParams().getDouble("real_value"));
        setImaginaryValue(scriptParameters.getParams().getDouble("imaginary_value"));
        setBlueValue(scriptParameters.getParams().getDouble("blue_value"));
        setOperator(scriptParameters.getParams().getInt("operator_type"));
        setClipMode(scriptParameters.getParams().getInt("data_type_clip_mode"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), outputPanel.isOutputNewImageSet());

        scriptParameters.storeProcessWholeImage(outputPanel.isProcessWholeImageSet());
        scriptParameters.getParams().put(ParameterFactory.newParameter("real_value", realValue));
        scriptParameters.getParams().put(ParameterFactory.newParameter("imaginary_value", imaginaryValue));
        scriptParameters.getParams().put(ParameterFactory.newParameter("blue_value", blueValue));
        scriptParameters.getParams().put(ParameterFactory.newParameter("operator_type", opType));
        scriptParameters.getParams().put(ParameterFactory.newParameter("data_type_clip_mode", clipMode));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Image Math");

        inputPanel = new JPanel(new GridBagLayout());
        inputPanel.setForeground(Color.black);
        inputPanel.setBorder(buildTitledBorder("Input parameters"));

        if (useComplex) {
            labelValue = new JLabel("Real value");
        } else if (image.isColorImage()) {
            labelValue = new JLabel("Red value");
        } else {
            labelValue = new JLabel("Value");
        }

        labelValue.setForeground(Color.black);
        labelValue.setFont(serif12);

        textValue = new JTextField(5);
        textValue.setText("1.0");
        textValue.setFont(serif12);

        if (useComplex) {
            labelValueI = new JLabel("Imaginary value");
            labelValueI.setForeground(Color.black);
            labelValueI.setFont(serif12);

            textValueI = new JTextField(5);
            textValueI.setText("0.0");
            textValueI.setFont(serif12);
        } // if (useComplex)
        else if (image.isColorImage()) {
            labelValueI = new JLabel("Green value");
            labelValueI.setForeground(Color.black);
            labelValueI.setFont(serif12);

            textValueI = new JTextField(5);
            textValueI.setText("1.0");
            textValueI.setFont(serif12); 
            
            labelValueB = new JLabel("Blue value");
            labelValueB.setForeground(Color.black);
            labelValueB.setFont(serif12);

            textValueB = new JTextField(5);
            textValueB.setText("1.0");
            textValueB.setFont(serif12);   
        }

        labelOperator = new JLabel("Operator");
        labelOperator.setForeground(Color.black);
        labelOperator.setFont(serif12);

        comboBoxOperator = new JComboBox();
        comboBoxOperator.setFont(serif12);
        comboBoxOperator.setBackground(Color.white);

        comboBoxOperator.addItem("Absolute Value");
        comboBoxOperator.addItem("Add");
        comboBoxOperator.addItem("Average");
        comboBoxOperator.addItem("Constant");
        comboBoxOperator.addItem("Divide");
        comboBoxOperator.addItem("Inverse (1 / intensity)");
        comboBoxOperator.addItem("Log");
        comboBoxOperator.addItem("Multiply");
        comboBoxOperator.addItem("Square");
        comboBoxOperator.addItem("Square Root");
        comboBoxOperator.addItem("Subtract");
        comboBoxOperator.addItem("Sum");

        comboBoxOperator.addItemListener(this);
        comboBoxOperator.setSelectedIndex(0);

        group = new ButtonGroup();
        radioClip = new JRadioButton("Clip", true);
        radioClip.setFont(serif12);
        group.add(radioClip);

        radioPromote = new JRadioButton("Promote image type", false);
        radioPromote.setFont(serif12);
        group.add(radioPromote);
        
        radioFloat = new JRadioButton("Convert to float", false);
        radioFloat.setFont(serif12);
        group.add(radioFloat);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);

        int yPos = 0;
        gbc.gridx = 0;
        gbc.gridy = yPos;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        inputPanel.add(labelValue, gbc);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        inputPanel.add(textValue, gbc);

        if (useComplex || image.isColorImage()) {
            gbc.gridx = 0;
            gbc.gridy = yPos;
            gbc.weightx = 0;
            gbc.fill = GridBagConstraints.NONE;
            inputPanel.add(labelValueI, gbc);
            gbc.gridx = 1;
            gbc.gridy = yPos++;
            gbc.weightx = 1;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            inputPanel.add(textValueI, gbc);
        } // if (useComplex || image.isColorImage())
        
        if (image.isColorImage()) {
            gbc.gridx = 0;
            gbc.gridy = yPos;
            gbc.weightx = 0;
            gbc.fill = GridBagConstraints.NONE;
            inputPanel.add(labelValueB, gbc);
            gbc.gridx = 1;
            gbc.gridy = yPos++;
            gbc.weightx = 1;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            inputPanel.add(textValueB, gbc);    
        } // if (image.isColorImage())

        gbc.gridx = 0;
        gbc.gridy = yPos;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        inputPanel.add(labelOperator, gbc);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        inputPanel.add(comboBoxOperator, gbc);
        gbc.gridx = 0;
        gbc.gridy = yPos++;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridwidth = 2;
        gbc.insets = new Insets(0, 0, 0, 0);
        inputPanel.add(radioClip, gbc);
        gbc.gridy = yPos++;
        inputPanel.add(radioPromote, gbc);
        gbc.gridy = yPos++;
        inputPanel.add(radioFloat, gbc);

        outputPanel = new JPanelAlgorithmOutputOptions(image);

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(inputPanel, gbc);
        gbc.gridy = 1;
        gbc.gridwidth = 2;
        gbc.fill = GridBagConstraints.BOTH;
        mainPanel.add(outputPanel, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        buttonPanel.add(buildHelpButton());
        
        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        // set this for Absolute Value
        textValue.setEnabled(false);
        if (useComplex || image.isColorImage()) {
            textValueI.setEnabled(false);
        }
        if (image.isColorImage()) {
            textValueB.setEnabled(false);
        }
        radioClip.setSelected(true);
        if (image.getType() == ModelStorageBase.FLOAT || image.getType() == ModelStorageBase.DOUBLE ||
            image.getType() == ModelStorageBase.ARGB_FLOAT) {
            radioFloat.setEnabled(false);
        } else {
            radioFloat.setEnabled(true);
        }

        pack();
        // setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        tmpStr = textValue.getText();

        if (testParameter(tmpStr, -Double.MAX_VALUE, Double.MAX_VALUE)) {
            realValue = Double.valueOf(tmpStr).doubleValue();
        } else {
            textValue.requestFocus();
            textValue.selectAll();

            return false;
        }

        if (useComplex || image.isColorImage()) {
            tmpStr = textValueI.getText();

            if (testParameter(tmpStr, -Double.MAX_VALUE, Double.MAX_VALUE)) {
                imaginaryValue = Double.valueOf(tmpStr).doubleValue();
            } else {
                textValueI.requestFocus();
                textValueI.selectAll();

                return false;
            }
        } // if (useComplex)
        
        if (image.isColorImage()) {
            tmpStr = textValueB.getText();

            if (testParameter(tmpStr, -Double.MAX_VALUE, Double.MAX_VALUE)) {
                blueValue = Double.valueOf(tmpStr).doubleValue();
            } else {
                textValueB.requestFocus();
                textValueB.selectAll();

                return false;
            }    
        } // if (image.isColorImage())

        if (radioClip.isSelected()) {
            clipMode = AlgorithmImageMath.CLIP;
        } else if (radioPromote.isSelected()) {
            clipMode = AlgorithmImageMath.PROMOTE;
        } else if (radioFloat.isSelected()) {
        	clipMode = AlgorithmImageMath.CONVERT_FLOAT;
        }

        return true;
    }

}
