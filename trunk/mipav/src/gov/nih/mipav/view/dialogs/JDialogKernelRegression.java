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
 * source image. Algorithms are executed in their own thread.
 *
 * @see  AlgorithmKernelRegression
 */
public class JDialogKernelRegression extends JDialogScriptableBase
        implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface, DialogDefaultsInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmKernelRegression kernelRegressionAlgo;

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    // or if the source image is to be replaced

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private boolean image25D = true;

    /** DOCUMENT ME! */
    private JCheckBox image25DCheckBox;
    
    private int method = AlgorithmKernelRegression.ITERATIVE_STEERING_KERNEL_SECOND_ORDER;
    
    private JLabel labelInitialGlobal;
    
    private JTextField textInitialGlobal;
    
    /** 0.8 for STEERING_KERNEL_SECOND_ORDER_L1_NORM */
    private float initialGlobalSmoothing = 0.5f;
    
    private JLabel labelIterativeGlobal;
    
    private JTextField textIterativeGlobal;
    
    /** 1.5 for STEERING_KERNEL_SECOND_ORDER_L1_NORM */
    private float iterativeGlobalSmoothing = 2.4f;
    
    private JLabel labelIterativeGlobal2;
    
    private JTextField textIterativeGlobal2;
    
    private float iterativeGlobalSmoothing2 = 1.75f;
    
    private JLabel labelUpscale;
    
    private JTextField textUpscale;
    
    /** Upscaling factor */
    private int upscale = 1;
    
    private JLabel labelInitialKernel;
    
    private JTextField textInitialKernel;
    
    private int initialKernelSize = 5;
    
    private JLabel labelIterativeKernel;
    
    private JTextField textIterativeKernel;
    
    private int iterativeKernelSize = 21;
    
    private JLabel labelIterations;
    
    private JTextField textIterations;
    
    /** iterations = 750 in ckr2L1_regular for steepest descent method in L1 classic kernel regression in
     *  STEERING_KERNEL_SECOND_ORDER_L1_NORM.
     */
    /** Total number of iterations */
    private int iterations = 4;
    
    private JLabel labelIterations2;
     
    private JTextField textIterations2;
    
    /** Iterations in skr2L1_regular for steepest descent method in L1 steering kernel regression in
     *  STEERING_KERNEL_SECOND_ORDER_L1_NORM. */
    private int iterations2 = 300;
    
    private JLabel labelWindowSize;
    
    private JTextField textWindowSize;
    
    /** Size of local orientation analysis window */
    private int windowSize = 11;
    
    private JLabel labelLambda;
    
    private JTextField textLambda;
    
    /** Regularization for the elongation parameter */
    private float lambda = 1.0f;
    
    private JLabel labelAlpha;
    
    private JTextField textAlpha;
    
    /** Structure sensitive parameter */
    private float alpha = 0.5f;
    
    private ButtonGroup methodGroup;
    
    private JRadioButton iterSteering2;
    
    private JRadioButton iterIrregular;
    
    private JRadioButton regSampled2Classic;
    
    private JRadioButton steering2L1Norm;
    
    private JLabel labelClassicStep;
    
    private JTextField textClassicStep;
    
    private float classicStepSize = 0.1f;
    
    private JLabel labelSteeringStep;
    
    private JTextField textSteeringStep;
    
    private float steeringStepSize = 0.1f;
    
    private JCheckBox NaNCheckBox;
    
    private boolean hasNaN = false;
    
    private JCheckBox infinityCheckBox;
    
    private boolean hasInfinity = false;
    
    private JCheckBox greaterEqualCheckBox;
    
    private boolean hasGreaterEqual = false;
    
    private JTextField textGreaterEqual;
    
    private double greaterEqualValue = 0.0;
    
    private JCheckBox equalCheckBox;
    
    private boolean hasEqual = false;
    
    private JTextField textEqual;
    
    private double equalValue = 0.0;
    
    private JCheckBox lesserEqualCheckBox;
    
    private boolean hasLesserEqual = false;
    
    private JTextField textLesserEqual;
    
    private double lesserEqualValue = 0.0;
    

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private JPanel paramPanel;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogKernelRegression() { }

    /**
     * Creates a new JDialogKernelRegression object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogKernelRegression(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
        loadDefaults();
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, sets variables and calls algorithm.
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
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        } else if ((source.equals(iterSteering2)) || (source.equals(iterIrregular)) || 
                   (source.equals(regSampled2Classic)) || (source.equals(steering2L1Norm))) {
            if (iterSteering2.isSelected()) {
                labelIterativeGlobal.setEnabled(true);
                labelIterativeGlobal.setText("Iterative global smoothing ");
                textIterativeGlobal.setEnabled(true);
                textIterativeGlobal.setText("2.4");
                labelIterativeGlobal2.setEnabled(false);
                textIterativeGlobal2.setEnabled(false);
                labelUpscale.setEnabled(true);
                textUpscale.setEnabled(true);
                labelIterativeKernel.setEnabled(true);
                textIterativeKernel.setEnabled(true);
                labelIterations.setEnabled(true);
                labelIterations.setText("Iterations ");
                textIterations.setEnabled(true);
                if (image.isColorImage()) {
                    textIterations.setText("2");
                }
                else {
                    textIterations.setText("4");
                }
                labelIterations2.setEnabled(false);
                textIterations2.setEnabled(false);
                labelWindowSize.setEnabled(true);
                textWindowSize.setEnabled(true);
                labelLambda.setEnabled(true);
                textLambda.setEnabled(true);
                labelAlpha.setEnabled(true);
                textAlpha.setEnabled(true); 
                labelClassicStep.setEnabled(false);
                textClassicStep.setEnabled(false);
                labelSteeringStep.setEnabled(false);
                textSteeringStep.setEnabled(false);
                NaNCheckBox.setEnabled(false);
                infinityCheckBox.setEnabled(false);
                greaterEqualCheckBox.setEnabled(false);
                textGreaterEqual.setEnabled(false);
                equalCheckBox.setEnabled(false);
                textEqual.setEnabled(false);
                lesserEqualCheckBox.setEnabled(false);
                textLesserEqual.setEnabled(false);
            }
            else if (iterIrregular.isSelected()) {
                labelIterativeGlobal.setEnabled(true);
                labelIterativeGlobal.setText("Iterative global smoothing ");
                textIterativeGlobal.setEnabled(true);
                textIterativeGlobal.setText("2.4");
                labelIterativeGlobal2.setEnabled(false);
                textIterativeGlobal2.setEnabled(false);
                labelUpscale.setEnabled(false);
                textUpscale.setEnabled(false);
                textUpscale.setText("1");
                labelIterativeKernel.setEnabled(true);
                textIterativeKernel.setEnabled(true);
                labelIterations.setEnabled(true);
                labelIterations.setText("Iterations ");
                textIterations.setEnabled(true);
                if (image.isColorImage()) {
                    textIterations.setText("2");
                }
                else {
                    textIterations.setText("4");
                }
                labelIterations2.setEnabled(false);
                textIterations2.setEnabled(false);
                labelWindowSize.setEnabled(true);
                textWindowSize.setEnabled(true);
                labelLambda.setEnabled(true);
                textLambda.setEnabled(true);
                labelAlpha.setEnabled(true);
                textAlpha.setEnabled(true); 
                labelClassicStep.setEnabled(false);
                textClassicStep.setEnabled(false);
                labelSteeringStep.setEnabled(false);
                textSteeringStep.setEnabled(false); 
                if ((image.getType() == ModelStorageBase.FLOAT) || (image.getType() == ModelStorageBase.DOUBLE) ||
                    (image.getType() == ModelStorageBase.ARGB_FLOAT)) {
                    NaNCheckBox.setEnabled(true);
                    infinityCheckBox.setEnabled(true);    
                }
                else {
                    NaNCheckBox.setEnabled(false);
                    infinityCheckBox.setEnabled(false);
                }
                greaterEqualCheckBox.setEnabled(true);
                if (greaterEqualCheckBox.isSelected()) {
                    textGreaterEqual.setEnabled(true);
                }
                else {
                    textGreaterEqual.setEnabled(false);
                }
                equalCheckBox.setEnabled(true);
                if (equalCheckBox.isSelected()) {
                    textEqual.setEnabled(true);
                }
                else {
                    textEqual.setEnabled(false);
                }
                lesserEqualCheckBox.setEnabled(true);
                if (lesserEqualCheckBox.isSelected()) {
                    textLesserEqual.setEnabled(true);
                }
                else {
                    textLesserEqual.setEnabled(false);
                }
            }
            else if (regSampled2Classic.isSelected()) {
                labelIterativeGlobal.setEnabled(false);
                labelIterativeGlobal.setText("Iterative global smoothing ");
                textIterativeGlobal.setEnabled(false);
                labelIterativeGlobal2.setEnabled(false);
                textIterativeGlobal2.setEnabled(false);
                labelUpscale.setEnabled(true);
                textUpscale.setEnabled(true);
                labelIterativeKernel.setEnabled(false);
                textIterativeKernel.setEnabled(false);
                labelIterations.setText("Iterations ");
                labelIterations.setEnabled(false);
                textIterations.setEnabled(false);
                labelIterations2.setEnabled(false);
                textIterations2.setEnabled(false);
                labelWindowSize.setEnabled(false);
                textWindowSize.setEnabled(false);
                labelLambda.setEnabled(false);
                textLambda.setEnabled(false);
                labelAlpha.setEnabled(false);
                textAlpha.setEnabled(false);
                labelClassicStep.setEnabled(false);
                textClassicStep.setEnabled(false);
                labelSteeringStep.setEnabled(false);
                textSteeringStep.setEnabled(false);
                NaNCheckBox.setEnabled(false);
                infinityCheckBox.setEnabled(false);
                greaterEqualCheckBox.setEnabled(false);
                textGreaterEqual.setEnabled(false);
                equalCheckBox.setEnabled(false);
                textEqual.setEnabled(false);
                lesserEqualCheckBox.setEnabled(false);
                textLesserEqual.setEnabled(false);
            }
            else if (steering2L1Norm.isSelected()) {
                labelIterativeGlobal.setEnabled(true);
                labelIterativeGlobal.setText("Global smoothing for L1 classic kernel regression ");
                textIterativeGlobal.setEnabled(true);
                textIterativeGlobal.setText("1.5");
                labelIterativeGlobal2.setEnabled(true);
                textIterativeGlobal2.setEnabled(true);
                labelUpscale.setEnabled(true);
                textUpscale.setEnabled(true);
                labelIterativeKernel.setEnabled(true);
                textIterativeKernel.setEnabled(true);
                labelIterations.setText("Iterations for L1 classic kernel regression ");
                labelIterations.setEnabled(true);
                textIterations.setEnabled(true);
                textIterations.setText("750");
                labelIterations2.setEnabled(true);
                textIterations2.setEnabled(true);
                labelWindowSize.setEnabled(true);
                textWindowSize.setEnabled(true);
                labelLambda.setEnabled(true);
                textLambda.setEnabled(true);
                labelAlpha.setEnabled(true);
                textAlpha.setEnabled(true);
                labelClassicStep.setEnabled(true);
                textClassicStep.setEnabled(true);
                labelSteeringStep.setEnabled(true);
                textSteeringStep.setEnabled(true);
                NaNCheckBox.setEnabled(false);
                infinityCheckBox.setEnabled(false);
                greaterEqualCheckBox.setEnabled(false);
                textGreaterEqual.setEnabled(false);
                equalCheckBox.setEnabled(false);
                textEqual.setEnabled(false);
                lesserEqualCheckBox.setEnabled(false);
                textLesserEqual.setEnabled(false);
            }
        }
        else if (source.equals(greaterEqualCheckBox)) {
            if (greaterEqualCheckBox.isSelected()) {
                textGreaterEqual.setEnabled(true);
            }
            else {
                textGreaterEqual.setEnabled(false);
            }
        }
        else if (source.equals(equalCheckBox)) {
            if (equalCheckBox.isSelected()) {
                textEqual.setEnabled(true);
            }
            else {
                textEqual.setEnabled(false);
            }
        }
        else if (source.equals(lesserEqualCheckBox)) {
            if (lesserEqualCheckBox.isSelected()) {
                textLesserEqual.setEnabled(true);
            }
            else {
                textLesserEqual.setEnabled(false);
            }
        } else {
            super.actionPerformed(event);
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

        if (algorithm instanceof AlgorithmKernelRegression) {
            image.clearMask();

            if ((kernelRegressionAlgo.isCompleted() == true) && (resultImage != null)) {

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
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
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        kernelRegressionAlgo.finalize();
        kernelRegressionAlgo = null;
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
        str += method + delim;
        str += hasNaN + delim;
        str += hasInfinity + delim;
        str += hasGreaterEqual + delim;
        str += greaterEqualValue + delim;
        str += hasEqual + delim;
        str += equalValue + delim;
        str += hasLesserEqual + delim;
        str += lesserEqualValue + delim;
        str += initialGlobalSmoothing + delim;
        str += iterativeGlobalSmoothing + delim;
        str += iterativeGlobalSmoothing2 + delim;
        str += upscale + delim;
        str += initialKernelSize + delim;
        str += iterativeKernelSize + delim;
        str += iterations + delim;
        str += iterations2 + delim;
        str += windowSize + delim;
        str += lambda + delim;
        str += alpha + delim;
        str += classicStepSize + delim;
        str += steeringStepSize + delim;
        str += image25D;

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

        if ((defaultsString != null) && (newImage != null)) {

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");

                method = MipavUtil.getInt(st);
                if (method == AlgorithmKernelRegression.ITERATIVE_STEERING_KERNEL_SECOND_ORDER) {
                    iterSteering2.setSelected(true);
                    iterIrregular.setSelected(false);
                    regSampled2Classic.setSelected(false);
                    steering2L1Norm.setSelected(false);
                }
                else if (method == AlgorithmKernelRegression.ITERATIVE_STEERING_KERNEL_SECOND_ORDER_IRREGULAR) {
                    iterSteering2.setSelected(false);
                    iterIrregular.setSelected(true);
                    regSampled2Classic.setSelected(false);
                    steering2L1Norm.setSelected(false);    
                }
                else if (method == AlgorithmKernelRegression.REGULARLY_SAMPLED_SECOND_ORDER_CLASSIC) {
                    iterSteering2.setSelected(false);
                    iterIrregular.setSelected(false);
                    regSampled2Classic.setSelected(true);
                    steering2L1Norm.setSelected(false);
                }
                else if (method == AlgorithmKernelRegression.STEERING_KERNEL_SECOND_ORDER_L1_NORM) {
                    iterSteering2.setSelected(false);
                    iterIrregular.setSelected(false);
                    regSampled2Classic.setSelected(false);
                    steering2L1Norm.setSelected(true);    
                }
                NaNCheckBox.setSelected(MipavUtil.getBoolean(st));
                infinityCheckBox.setSelected(MipavUtil.getBoolean(st));
                hasGreaterEqual = MipavUtil.getBoolean(st);
                greaterEqualCheckBox.setSelected(hasGreaterEqual);
                textGreaterEqual.setText("" + MipavUtil.getDouble(st));
                hasEqual = MipavUtil.getBoolean(st);
                equalCheckBox.setSelected(hasEqual);
                textEqual.setText("" + MipavUtil.getDouble(st));
                hasLesserEqual = MipavUtil.getBoolean(st);
                lesserEqualCheckBox.setSelected(hasLesserEqual);
                textLesserEqual.setText("" + MipavUtil.getDouble(st));
                textInitialGlobal.setText("" + MipavUtil.getFloat(st));
                textIterativeGlobal.setText("" + MipavUtil.getFloat(st));
                textIterativeGlobal2.setText("" + MipavUtil.getFloat(st));
                textUpscale.setText("" + MipavUtil.getInt(st));
                textInitialKernel.setText("" +  MipavUtil.getInt(st));
                textIterativeKernel.setText("" + MipavUtil.getInt(st));
                textIterations.setText("" + MipavUtil.getInt(st));
                textIterations2.setText("" + MipavUtil.getInt(st));
                textWindowSize.setText("" + MipavUtil.getInt(st));
                textLambda.setText("" + MipavUtil.getFloat(st));
                textAlpha.setText("" + MipavUtil.getFloat(st));
                textClassicStep.setText("" + MipavUtil.getFloat(st));
                textSteeringStep.setText("" + MipavUtil.getFloat(st));
                if (method == AlgorithmKernelRegression.ITERATIVE_STEERING_KERNEL_SECOND_ORDER){
                    labelIterativeGlobal.setText("Iterative global smoothing ");
                    labelIterativeGlobal.setEnabled(true);
                    textIterativeGlobal.setEnabled(true);
                    labelIterativeGlobal2.setEnabled(false);
                    textIterativeGlobal2.setEnabled(false);
                    labelUpscale.setEnabled(true);
                    textUpscale.setEnabled(true);
                    labelIterativeKernel.setEnabled(true);
                    textIterativeKernel.setEnabled(true);
                    labelIterations.setText("Iterations ");
                    labelIterations.setEnabled(true);
                    textIterations.setEnabled(true);
                    labelIterations2.setEnabled(false);
                    textIterations2.setEnabled(false);
                    labelWindowSize.setEnabled(true);
                    textWindowSize.setEnabled(true);
                    labelLambda.setEnabled(true);
                    textLambda.setEnabled(true);
                    labelAlpha.setEnabled(true);
                    textAlpha.setEnabled(true);
                    labelClassicStep.setEnabled(false);
                    textClassicStep.setEnabled(false);
                    labelSteeringStep.setEnabled(false);
                    textSteeringStep.setEnabled(false);
                    NaNCheckBox.setEnabled(false);
                    infinityCheckBox.setEnabled(false);
                    greaterEqualCheckBox.setEnabled(false);
                    textGreaterEqual.setEnabled(false);
                    equalCheckBox.setEnabled(false);
                    textEqual.setEnabled(false);
                    lesserEqualCheckBox.setEnabled(false);
                    textLesserEqual.setEnabled(false);
                }
                else if (method == AlgorithmKernelRegression.ITERATIVE_STEERING_KERNEL_SECOND_ORDER_IRREGULAR) {
                    labelIterativeGlobal.setText("Iterative global smoothing ");
                    labelIterativeGlobal.setEnabled(true);
                    textIterativeGlobal.setEnabled(true);
                    labelIterativeGlobal2.setEnabled(false);
                    textIterativeGlobal2.setEnabled(false);
                    labelUpscale.setEnabled(false);
                    textUpscale.setEnabled(false);
                    textUpscale.setText("1");
                    labelIterativeKernel.setEnabled(true);
                    textIterativeKernel.setEnabled(true);
                    labelIterations.setText("Iterations ");
                    labelIterations.setEnabled(true);
                    textIterations.setEnabled(true);
                    labelIterations2.setEnabled(false);
                    textIterations2.setEnabled(false);
                    labelWindowSize.setEnabled(true);
                    textWindowSize.setEnabled(true);
                    labelLambda.setEnabled(true);
                    textLambda.setEnabled(true);
                    labelAlpha.setEnabled(true);
                    textAlpha.setEnabled(true);
                    labelClassicStep.setEnabled(false);
                    textClassicStep.setEnabled(false);
                    labelSteeringStep.setEnabled(false);
                    textSteeringStep.setEnabled(false);
                    if ((image.getType() == ModelStorageBase.FLOAT) || (image.getType() == ModelStorageBase.DOUBLE) ||
                        (image.getType() == ModelStorageBase.ARGB_FLOAT)) {
                        NaNCheckBox.setEnabled(true);
                        infinityCheckBox.setEnabled(true);    
                    }
                    else {
                        NaNCheckBox.setEnabled(false);
                        infinityCheckBox.setEnabled(false);
                    }
                    greaterEqualCheckBox.setEnabled(true);
                    textGreaterEqual.setEnabled(hasGreaterEqual);
                    equalCheckBox.setEnabled(true);
                    textEqual.setEnabled(hasEqual);
                    lesserEqualCheckBox.setEnabled(true);
                    textLesserEqual.setEnabled(hasLesserEqual);
                }
                else if (method == AlgorithmKernelRegression.REGULARLY_SAMPLED_SECOND_ORDER_CLASSIC) {
                    labelIterativeGlobal.setText("Iterative global smoothing ");
                    labelIterativeGlobal.setEnabled(false);
                    textIterativeGlobal.setEnabled(false);
                    labelIterativeGlobal2.setEnabled(false);
                    textIterativeGlobal2.setEnabled(false);
                    labelUpscale.setEnabled(true);
                    textUpscale.setEnabled(true);
                    labelIterativeKernel.setEnabled(false);
                    textIterativeKernel.setEnabled(false);
                    labelIterations.setText("Iterations ");
                    textIterations.setEnabled(false);
                    labelIterations2.setEnabled(false);
                    textIterations2.setEnabled(false);
                    labelWindowSize.setEnabled(false);
                    textWindowSize.setEnabled(false);
                    labelLambda.setEnabled(false);
                    textLambda.setEnabled(false);
                    labelAlpha.setEnabled(false);
                    textAlpha.setEnabled(false);
                    labelClassicStep.setEnabled(false);
                    textClassicStep.setEnabled(false);
                    labelSteeringStep.setEnabled(false);
                    textSteeringStep.setEnabled(false);
                    NaNCheckBox.setEnabled(false);
                    infinityCheckBox.setEnabled(false);
                    greaterEqualCheckBox.setEnabled(false);
                    textGreaterEqual.setEnabled(false);
                    equalCheckBox.setEnabled(false);
                    textEqual.setEnabled(false);
                    lesserEqualCheckBox.setEnabled(false);
                    textLesserEqual.setEnabled(false);
                }
                else if (method == AlgorithmKernelRegression.STEERING_KERNEL_SECOND_ORDER_L1_NORM) {
                    labelIterativeGlobal.setText("Global smoothing for L1 classic kernel regression ");
                    labelIterativeGlobal.setEnabled(true);
                    textIterativeGlobal.setEnabled(true);
                    labelIterativeGlobal2.setEnabled(true);
                    textIterativeGlobal2.setEnabled(true);
                    labelUpscale.setEnabled(true);
                    textUpscale.setEnabled(true);
                    labelIterativeKernel.setEnabled(true);
                    textIterativeKernel.setEnabled(true);
                    labelIterations.setText("Iterations for L1 classic kernel regression ");
                    labelIterations.setEnabled(true);
                    textIterations.setEnabled(true);
                    labelIterations2.setEnabled(true);
                    textIterations2.setEnabled(true);
                    labelWindowSize.setEnabled(true);
                    textWindowSize.setEnabled(true);
                    labelLambda.setEnabled(true);
                    textLambda.setEnabled(true);
                    labelAlpha.setEnabled(true);
                    textAlpha.setEnabled(true); 
                    labelClassicStep.setEnabled(true);
                    textClassicStep.setEnabled(true);
                    labelSteeringStep.setEnabled(true);
                    textSteeringStep.setEnabled(true);
                    NaNCheckBox.setEnabled(false);
                    infinityCheckBox.setEnabled(false);
                    greaterEqualCheckBox.setEnabled(false);
                    textGreaterEqual.setEnabled(false);
                    equalCheckBox.setEnabled(false);
                    textEqual.setEnabled(false);
                    lesserEqualCheckBox.setEnabled(false);
                    textLesserEqual.setEnabled(false);
                }
                image25DCheckBox.setSelected(MipavUtil.getBoolean(st));

                if (MipavUtil.getBoolean(st)) {
                    newImage.setSelected(true);
                } else {
                    replaceImage.setSelected(true);
                }

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
        String defaultsString = new String(getParameterString(",") + "," + newImage.isSelected());

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
     * Accessor that sets whether 3D images are 3D or 2.5D filtered.
     *
     * @param  image25D  true for 2.5D filtering
     */
    public void setImage25D(boolean image25D) {
        this.image25D = image25D;
    }
    
    /**
     * 
     * @param method
     */
    public void setMethod(int method) {
        this.method = method;
    }
    
    /**
     * 
     * @param initialGlobalSmoothing
     */
    public void setInitialGlobalSmoothing(float initialGlobalSmoothing) {
        this.initialGlobalSmoothing = initialGlobalSmoothing;
    }
    
    /**
     * 
     * @param iterativeGlobalSmoothing
     */
    public void setIterativeGlobalSmoothing(float iterativeGlobalSmoothing) {
        this.iterativeGlobalSmoothing = iterativeGlobalSmoothing;
    }
    
    /**
     * 
     * @param iterativeGlobalSmoothing2
     */
    public void setIterativeGlobalSmoothing2(float iterativeGlobalSmoothing2) {
        this.iterativeGlobalSmoothing2 = iterativeGlobalSmoothing2;
    }
    
    /**
     * 
     * @param upscale
     */
    public void setUpscale(int upscale) {
        this.upscale = upscale;
    }
    
    /**
     * 
     * @param initialKernelSize
     */
    public void setInitialKernelSize(int initialKernelSize) {
        this.initialKernelSize = initialKernelSize;
    }
    
    /**
     * 
     * @param iterativeKernelSize
     */
    public void setIterativeKernelSize(int iterativeKernelSize) {
        this.iterativeKernelSize = iterativeKernelSize;
    }
    
    /**
     * 
     * @param iterations
     */
    public void setIterations(int iterations) {
        this.iterations = iterations;
    }
    
    /**
     * 
     * @param iterations2
     */
    public void setIterations2(int iterations2) {
        this.iterations2 = iterations2;
    }
    
    /**
     * 
     * @param windowSize
     */
    public void setWindowSize(int windowSize) {
        this.windowSize = windowSize;
    }
    
    /**
     * 
     * @param lambda
     */
    public void setLambda(float lambda) {
        this.lambda = lambda;
    }
    
    /**
     * 
     * @param alpha
     */
    public void setAlpha(float alpha) {
        this.alpha = alpha;
    }
    
    /**
     * 
     * @param classicStepSize
     */
    public void setClassicStepSize(float classicStepSize) {
        this.classicStepSize = classicStepSize;
    }
    
    /**
     * 
     * @param steeringStepSize
     */
    public void setSteeringStepSize(float steeringStepSize) {
        this.steeringStepSize = steeringStepSize;
    }
    
    /**
     * 
     * @param hasNaN
     */
    public void setHasNaN(boolean hasNaN) {
        this.hasNaN = hasNaN;
    }
    
    /**
     * 
     * @param hasInfinity
     */
    public void setHasInfinity(boolean hasInfinity) {
        this.hasInfinity = hasInfinity;
    }
    
    /**
     * 
     * @param hasGreaterEqual
     */
    public void setHasGreaterEqual(boolean hasGreaterEqual) {
        this.hasGreaterEqual = hasGreaterEqual;
    }
    
    /**
     * 
     * @param greaterEqualValue
     */
    public void setGreaterEqualValue(double greaterEqualValue) {
        this.greaterEqualValue = greaterEqualValue;
    }
    
    /**
     * 
     * @param hasEqual
     */
    public void setHasEqual(boolean hasEqual) {
        this.hasEqual = hasEqual;
    }
    
    /**
     * 
     * @param equalValue
     */
    public void setEqualValue(double equalValue) {
        this.equalValue = equalValue;
    }
    
    /**
     * 
     * @param hasLesserEqual
     */
    public void setHasLesserEqual(boolean hasLesserEqual) {
        this.hasLesserEqual = hasLesserEqual;
    }
    
    /**
     * 
     * @param lesserEqualValue
     */
    public void setLesserEqualValue(double lesserEqualValue) {
        this.lesserEqualValue = lesserEqualValue;
    }

    
    /**
     * Once all the necessary variables are set, call the Kernel Regression algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_KernelRegression");
        int[] destExtents;

        if (image.getNDims() == 2) { // source image is 2D
            destExtents = new int[2];
            destExtents[0] = image.getExtents()[0] * upscale; // X dim
            destExtents[1] = image.getExtents()[1] * upscale; // Y dim
        } else {
            destExtents = new int[3];
            destExtents[0] = image.getExtents()[0] * upscale;
            destExtents[1] = image.getExtents()[1] * upscale;
            destExtents[2] = image.getExtents()[2];
        }

        if (displayLoc == NEW) {

            try {

                // Make result image of float type
                if (image.isColorImage()) {
                    resultImage = new ModelImage(image.getType(), destExtents, name);
                } else {
                    resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name);
                }

                // resultImage = (ModelImage)image.clone();
                // resultImage.setImageName(name);
                // Make algorithm
                kernelRegressionAlgo = new AlgorithmKernelRegression(resultImage, image, method,
                        hasNaN, hasInfinity, hasGreaterEqual, greaterEqualValue, hasEqual,
                        equalValue, hasLesserEqual, lesserEqualValue,
                        initialGlobalSmoothing, iterativeGlobalSmoothing, iterativeGlobalSmoothing2, 
                        upscale, initialKernelSize, iterativeKernelSize, iterations, iterations2, 
                        windowSize, lambda, alpha, classicStepSize, steeringStepSize, image25D);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                kernelRegressionAlgo.addListener(this);
                createProgressBar(image.getImageName(), kernelRegressionAlgo);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (kernelRegressionAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    kernelRegressionAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Kernel Regression: unable to allocate enough memory");

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
                kernelRegressionAlgo = new AlgorithmKernelRegression(null, image, method,
                        hasNaN, hasInfinity, hasGreaterEqual, greaterEqualValue, hasEqual,
                        equalValue, hasLesserEqual, lesserEqualValue,
                        initialGlobalSmoothing, iterativeGlobalSmoothing, iterativeGlobalSmoothing2, 
                        upscale, initialKernelSize, iterativeKernelSize, iterations, iterations2, 
                        windowSize, lambda, alpha, classicStepSize, steeringStepSize, image25D);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                kernelRegressionAlgo.addListener(this);
                createProgressBar(image.getImageName(), kernelRegressionAlgo);

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

                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (kernelRegressionAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    kernelRegressionAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Kernel Regression: unable to allocate enough memory");

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

        if (scriptParameters.doOutputNewImage()) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }

        method = scriptParameters.getParams().getInt("_method");
        hasNaN = scriptParameters.getParams().getBoolean("has_NaN");
        hasInfinity = scriptParameters.getParams().getBoolean("has_infinity");
        hasGreaterEqual = scriptParameters.getParams().getBoolean("has_greater_equal");
        greaterEqualValue = scriptParameters.getParams().getDouble("greater_equal_value");
        hasEqual = scriptParameters.getParams().getBoolean("has_equal");
        equalValue = scriptParameters.getParams().getDouble("equal_value");
        hasLesserEqual = scriptParameters.getParams().getBoolean("has_lesser_equal");
        lesserEqualValue = scriptParameters.getParams().getDouble("lesser_equal_value");
        initialGlobalSmoothing = scriptParameters.getParams().getFloat("initial_global_smoothing");
        iterativeGlobalSmoothing = scriptParameters.getParams().getFloat("iterative_global_smoothing");
        iterativeGlobalSmoothing2 = scriptParameters.getParams().getFloat("iterative_global_smoothing2");
        upscale = scriptParameters.getParams().getInt("_upscale");
        initialKernelSize = scriptParameters.getParams().getInt("initial_kernel_size");
        iterativeKernelSize = scriptParameters.getParams().getInt("iterative_kernel_size");
        iterations = scriptParameters.getParams().getInt("_iterations");
        iterations2 = scriptParameters.getParams().getInt("_iterations2");
        windowSize = scriptParameters.getParams().getInt("window_size");
        lambda = scriptParameters.getParams().getFloat("_lambda");
        alpha = scriptParameters.getParams().getFloat("_alpha");
        classicStepSize = scriptParameters.getParams().getFloat("classic_step_size");
        steeringStepSize = scriptParameters.getParams().getFloat("steering_step_size");
        image25D = scriptParameters.doProcess3DAs25D();
    }


    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("_method", method));
        scriptParameters.getParams().put(ParameterFactory.newParameter("has_NaN", hasNaN));
        scriptParameters.getParams().put(ParameterFactory.newParameter("has_infinity", hasInfinity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("has_greater_equal", hasGreaterEqual));
        scriptParameters.getParams().put(ParameterFactory.newParameter("greater_equal_value", greaterEqualValue));
        scriptParameters.getParams().put(ParameterFactory.newParameter("has_equal", hasEqual));
        scriptParameters.getParams().put(ParameterFactory.newParameter("equal_value", equalValue));
        scriptParameters.getParams().put(ParameterFactory.newParameter("has_lesser_equal", hasLesserEqual));
        scriptParameters.getParams().put(ParameterFactory.newParameter("lesser_equal_value", lesserEqualValue));
        scriptParameters.getParams().put(ParameterFactory.newParameter("initial_global_smoothing", initialGlobalSmoothing));
        scriptParameters.getParams().put(ParameterFactory.newParameter("iterative_global_smoothing", iterativeGlobalSmoothing));
        scriptParameters.getParams().put(ParameterFactory.newParameter("iterative_global_smoothing2", iterativeGlobalSmoothing2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("_upscale", upscale));
        scriptParameters.getParams().put(ParameterFactory.newParameter("initial_kernel_scale", initialKernelSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("iterative_kernel_size", iterativeKernelSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("_iterations", iterations));
        scriptParameters.getParams().put(ParameterFactory.newParameter("_iterations2", iterations2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("window_size", windowSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("_lambda", lambda));
        scriptParameters.getParams().put(ParameterFactory.newParameter("_alpha", alpha));
        scriptParameters.getParams().put(ParameterFactory.newParameter("classic_step_size", classicStepSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("steering_step_size", steeringStepSize));
        scriptParameters.storeProcess3DAs25D(image25D);
    }

    /**
     * Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Kernel Regression");

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
        mainPanel.add(paramPanel, gbc);

        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.weightx = 1;
        gbc2.insets = new Insets(3, 3, 3, 3);
        gbc2.gridx = 0;
        int yPos = 0;
        gbc2.gridy = yPos++;
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        
        methodGroup = new ButtonGroup();
        iterSteering2 = new JRadioButton("Iterative Steering Kernel Second Order", true);
        iterSteering2.setFont(serif12);
        iterSteering2.setForeground(Color.black);
        iterSteering2.addActionListener(this);
        methodGroup.add(iterSteering2);
        paramPanel.add(iterSteering2, gbc2);
        
        gbc2.gridy = yPos++;
        iterIrregular = new JRadioButton("Iterative Steering for Irregular Sampling", false);
        iterIrregular.setFont(serif12);
        iterIrregular.setForeground(Color.black);
        iterIrregular.addActionListener(this);
        methodGroup.add(iterIrregular);
        paramPanel.add(iterIrregular, gbc2);
        
        gbc2.gridy = yPos++;
        NaNCheckBox = new JCheckBox("NaN ", false);
        NaNCheckBox.setEnabled(false);
        NaNCheckBox.setFont(serif12);
        NaNCheckBox.setForeground(Color.black);
        paramPanel.add(NaNCheckBox, gbc2);
        
        gbc2.gridy = yPos++;
        infinityCheckBox = new JCheckBox("Infinity ", false);
        infinityCheckBox.setEnabled(false);
        infinityCheckBox.setFont(serif12);
        infinityCheckBox.setForeground(Color.black);
        paramPanel.add(infinityCheckBox, gbc2);
        
        gbc2.gridy = yPos++;
        greaterEqualCheckBox = new JCheckBox("Value >= ", false);
        greaterEqualCheckBox.setEnabled(false);
        greaterEqualCheckBox.setFont(serif12);
        greaterEqualCheckBox.setForeground(Color.black);
        greaterEqualCheckBox.addActionListener(this);
        paramPanel.add(greaterEqualCheckBox, gbc2);
        
        gbc2.gridx = 1;
        textGreaterEqual = createTextField("0");
        textGreaterEqual.setColumns(10);
        textGreaterEqual.setEnabled(false);
        paramPanel.add(textGreaterEqual, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy = yPos++;
        equalCheckBox = new JCheckBox("Value = ", false);
        equalCheckBox.setEnabled(false);
        equalCheckBox.setFont(serif12);
        equalCheckBox.setForeground(Color.black);
        equalCheckBox.addActionListener(this);
        paramPanel.add(equalCheckBox, gbc2);
        
        gbc2.gridx = 1;
        textEqual = createTextField("0");
        textEqual.setColumns(10);
        textEqual.setEnabled(false);
        paramPanel.add(textEqual, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy = yPos++;
        lesserEqualCheckBox = new JCheckBox("Value <= ", false);
        lesserEqualCheckBox.setEnabled(false);
        lesserEqualCheckBox.setFont(serif12);
        lesserEqualCheckBox.setForeground(Color.black);
        lesserEqualCheckBox.addActionListener(this);
        paramPanel.add(lesserEqualCheckBox, gbc2);
        
        gbc2.gridx = 1;
        textLesserEqual = createTextField("0");
        textLesserEqual.setColumns(10);
        textLesserEqual.setEnabled(false);
        paramPanel.add(textLesserEqual, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy = yPos++;
        regSampled2Classic = new JRadioButton("Regularly Sampled Second Order Classic", false);
        regSampled2Classic.setFont(serif12);
        regSampled2Classic.setForeground(Color.black);
        regSampled2Classic.addActionListener(this);
        methodGroup.add(regSampled2Classic);
        paramPanel.add(regSampled2Classic, gbc2);
        
        gbc2.gridy = yPos++;
        steering2L1Norm = new JRadioButton("Steering Kernel Second Order with L1 Norm", false);
        steering2L1Norm.setFont(serif12);
        steering2L1Norm.setForeground(Color.black);
        steering2L1Norm.addActionListener(this);
        methodGroup.add(steering2L1Norm);
        paramPanel.add(steering2L1Norm, gbc2);

        gbc2.gridy = yPos++;
        labelInitialGlobal = createLabel("Initial global smoothing");
        paramPanel.add(labelInitialGlobal, gbc2);

        gbc2.gridx = 1;
        textInitialGlobal = createTextField("0.5");
        paramPanel.add(textInitialGlobal, gbc2);

        gbc2.gridx = 0;
        gbc2.gridy = yPos++;
        labelIterativeGlobal = createLabel("Iterative global smoothing ");
        paramPanel.add(labelIterativeGlobal, gbc2);

        gbc2.gridx = 1;
        textIterativeGlobal = createTextField("2.4");
        paramPanel.add(textIterativeGlobal, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy = yPos++;
        labelIterativeGlobal2 = createLabel("Global smoothing for L1 steering kernel regression");
        paramPanel.add(labelIterativeGlobal2, gbc2);
        labelIterativeGlobal2.setEnabled(false);
        
        gbc2.gridx = 1;
        textIterativeGlobal2 = createTextField("1.75");
        paramPanel.add(textIterativeGlobal2, gbc2);
        textIterativeGlobal2.setEnabled(false);

        gbc2.gridx = 0;
        gbc2.gridy = yPos++;
        labelUpscale = createLabel("Upscaling factor ");
        paramPanel.add(labelUpscale, gbc2);

        gbc2.gridx = 1;
        textUpscale = createTextField("1");
        paramPanel.add(textUpscale, gbc2);

        gbc2.gridx = 0;
        gbc2.gridy = yPos++;
        labelInitialKernel = createLabel("Initial kernel size ");
        paramPanel.add(labelInitialKernel, gbc2);
        
        gbc2.gridx = 1;
        textInitialKernel = createTextField("5");
        paramPanel.add(textInitialKernel, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy = yPos++;
        labelIterativeKernel = createLabel("Iterative kernel size ");
        paramPanel.add(labelIterativeKernel, gbc2);
        
        gbc2.gridx = 1;
        textIterativeKernel = createTextField("21");
        paramPanel.add(textIterativeKernel, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy = yPos++;
        labelIterations = createLabel("Iterations ");
        paramPanel.add(labelIterations, gbc2);
        
        gbc2.gridx = 1;
        if (image.isColorImage()) {
            textIterations = createTextField("2");    
        }
        else {
            textIterations = createTextField("4");
        }
        paramPanel.add(textIterations, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy = yPos++;
        labelIterations2 = createLabel("Iterations for L1 steering kernel regression ");
        labelIterations2.setEnabled(false);
        paramPanel.add(labelIterations2, gbc2);
        
        gbc2.gridx = 1;
        textIterations2 = createTextField("300");
        textIterations2.setEnabled(false);
        paramPanel.add(textIterations2, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy = yPos++;
        labelWindowSize = createLabel("Size of local orientation analyis window");
        paramPanel.add(labelWindowSize, gbc2);
        
        gbc2.gridx = 1;
        textWindowSize = createTextField("11");
        paramPanel.add(textWindowSize, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy = yPos++;
        labelLambda = createLabel("Regularization for the elongation parameter");
        paramPanel.add(labelLambda, gbc2);
        
        gbc2.gridx = 1;
        textLambda = createTextField("1.0");
        paramPanel.add(textLambda, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy = yPos++;
        labelAlpha = createLabel("Structure sensitive parameter");
        paramPanel.add(labelAlpha, gbc2);
        
        gbc2.gridx = 1;
        textAlpha = createTextField("0.5");
        paramPanel.add(textAlpha, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy = yPos++;
        labelClassicStep = createLabel("Step size for L1 classic kernel regression ");
        labelClassicStep.setEnabled(false);
        paramPanel.add(labelClassicStep, gbc2);
        
        gbc2.gridx = 1;
        textClassicStep = createTextField("0.1");
        textClassicStep.setEnabled(false);
        paramPanel.add(textClassicStep, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy = yPos++;
        labelSteeringStep = createLabel("Step size for L1 steering kernel regression ");
        labelSteeringStep.setEnabled(false);
        paramPanel.add(labelSteeringStep, gbc2);
        
        gbc2.gridx = 1;
        textSteeringStep = createTextField("0.1");
        textSteeringStep.setEnabled(false);
        paramPanel.add(textSteeringStep, gbc2);
        
        if (image.getNDims() > 2) {
            gbc2.gridx = 0;
            gbc2.gridy = yPos++;
            gbc2.gridwidth = 2;

            image25DCheckBox = new JCheckBox("Process each slice independently (2.5D)");
            image25DCheckBox.setFont(serif12);
            paramPanel.add(image25DCheckBox, gbc2);
            image25DCheckBox.setSelected(true);
            image25DCheckBox.setEnabled(false);
        } // if (image.getNDims > 2)

        JPanel outputOptPanel = new JPanel(new GridLayout(1, 2));
        destinationPanel = new JPanel(new BorderLayout());
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));
        outputOptPanel.add(destinationPanel);

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setBounds(10, 16, 120, 25);
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
        gbc.gridy = 1;
        mainPanel.add(outputOptPanel, gbc);

        mainDialogPanel.add(mainPanel, BorderLayout.CENTER);
        mainDialogPanel.add(buildButtons(), BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
        setResizable(true);
        // setVisible(true);

        System.gc();
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        System.gc();

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }
        
        if (iterSteering2.isSelected()) {
            method = AlgorithmKernelRegression.ITERATIVE_STEERING_KERNEL_SECOND_ORDER;
        }
        else if (iterIrregular.isSelected()) {
            method = AlgorithmKernelRegression.ITERATIVE_STEERING_KERNEL_SECOND_ORDER_IRREGULAR;
        }
        else if (regSampled2Classic.isSelected()) {
            method = AlgorithmKernelRegression.REGULARLY_SAMPLED_SECOND_ORDER_CLASSIC;
        }
        else if (steering2L1Norm.isSelected()) {
            method = AlgorithmKernelRegression.STEERING_KERNEL_SECOND_ORDER_L1_NORM;
            if (image.isColorImage()) {
                MipavUtil.displayError("Cannot select Steering Kernel Second Order with L1 Norm for color image");
                return false;
            }
        }

        tmpStr = textInitialGlobal.getText();

        if (testParameter(tmpStr, 0.01, 100.0)) {
            initialGlobalSmoothing = Float.valueOf(tmpStr).floatValue();
        } else {
            MipavUtil.displayError("Initial global smoothing must be between 0.01 and 100.0");
            textInitialGlobal.requestFocus();
            textInitialGlobal.selectAll();

            return false;
        }
        
        tmpStr = textUpscale.getText();
        
        if (testParameter(tmpStr, 1, 100)) {
            upscale = Integer.valueOf(tmpStr).intValue();
        } else {
            MipavUtil.displayError("upscale must be between 1 and 100");
            textUpscale.requestFocus();
            textUpscale.selectAll();
            return false;
        }

        tmpStr = textInitialKernel.getText();
        
        if (testParameter(tmpStr, 1, 99)) {
            initialKernelSize = Integer.valueOf(tmpStr).intValue();
        }
        else {
            MipavUtil.displayError("Initial kernel size must be between 1 and 99");
            textInitialKernel.requestFocus();
            textInitialKernel.selectAll();
            return false;
        }
        
        if ((initialKernelSize % 2) == 0) {
            MipavUtil.displayError("Initial kernel size must be an odd number");
            textInitialKernel.requestFocus();
            textInitialKernel.selectAll();
            return false;
        }
        
        if ((method == AlgorithmKernelRegression.ITERATIVE_STEERING_KERNEL_SECOND_ORDER) ||
            (method == AlgorithmKernelRegression.STEERING_KERNEL_SECOND_ORDER_L1_NORM)) {
            tmpStr = textIterations.getText();
            
            if (testParameter(tmpStr, 1, 10000)) {
                iterations = Integer.valueOf(tmpStr).intValue();
            }
            else {
                MipavUtil.displayError("Iterations must be between 1 and 10000");
                textIterations.requestFocus();
                textIterations.selectAll();
                return false;
            }
            
            tmpStr = textWindowSize.getText();
            if (testParameter(tmpStr, 1, 99)) {
                windowSize = Integer.valueOf(tmpStr).intValue();
            }
            else {
                MipavUtil.displayError("Size of local orientation analysis window must be between 1 and 99");
                textWindowSize.requestFocus();
                textWindowSize.selectAll();
                return false;
            }
            
            if ((windowSize % 2) == 0) {
                MipavUtil.displayError("Size of local orientation analysis window must be odd");
                textWindowSize.requestFocus();
                textWindowSize.selectAll();
                return false;
            }
            
            tmpStr = textLambda.getText();
            if (testParameter(tmpStr, 0.1, 10.0)) {
                lambda = Float.valueOf(tmpStr).floatValue();
            }
            else {
                MipavUtil.displayError("Regularization for the elongation parameter must be between 0.1 and 10.0");
                textLambda.requestFocus();
                textLambda.selectAll();
                return false;
            }
            
            tmpStr = textAlpha.getText();
            if (testParameter(tmpStr, 0.05, 5.0)) {
                alpha = Float.valueOf(tmpStr).floatValue();
            }
            else {
                MipavUtil.displayError("Structure sensitive parameter must be between 0.05 and 5.0");
                textAlpha.requestFocus();
                textAlpha.selectAll();
                return false;
            }
            
            tmpStr = textIterativeGlobal.getText();
            if (testParameter(tmpStr, 0.01, 100.0)) {
                iterativeGlobalSmoothing = Float.valueOf(tmpStr).floatValue();
            }
            else {
                MipavUtil.displayError("Iterative global smoothing must be between 0.01 and 100.0");
                textIterativeGlobal.requestFocus();
                textIterativeGlobal.selectAll();
                return false;
            }
            
            tmpStr = textIterativeKernel.getText();
            
            if (testParameter(tmpStr, 1, 99)) {
                iterativeKernelSize = Integer.valueOf(tmpStr).intValue();
            }
            else {
                MipavUtil.displayError("Iterative kernel size must be between 1 and 99");
                textIterativeKernel.requestFocus();
                textIterativeKernel.selectAll();
                return false;
            }
            
            if ((iterativeKernelSize % 2) == 0) {
                MipavUtil.displayError("Iterative kernel size must be an odd number");
                textIterativeKernel.requestFocus();
                textIterativeKernel.selectAll();
                return false;
            }
        } // if ((method == AlgorithmKernelRegression.ITERATIVE_STEERING_KERNEL_SECOND_ORDER) ||
        
        if (method == AlgorithmKernelRegression.ITERATIVE_STEERING_KERNEL_SECOND_ORDER_IRREGULAR) {
            hasNaN = NaNCheckBox.isSelected();
            
            hasInfinity = infinityCheckBox.isSelected();
            
            hasGreaterEqual = greaterEqualCheckBox.isSelected();
            if (hasGreaterEqual) {
                tmpStr = textGreaterEqual.getText();
                if (testParameter(tmpStr, -Double.MAX_VALUE, Double.MAX_VALUE)) {
                    greaterEqualValue = Double.valueOf(tmpStr).doubleValue();
                }
                else {
                    MipavUtil.displayError(">= field does not have a valid value");
                    textGreaterEqual.requestFocus();
                    textGreaterEqual.selectAll();
                    return false;
                }
            } // if (hasGreaterEqual)
            
            hasEqual = equalCheckBox.isSelected();
            if (hasEqual) {
                tmpStr = textEqual.getText();
                if (testParameter(tmpStr, -Double.MAX_VALUE, Double.MAX_VALUE)) {
                    equalValue = Double.valueOf(tmpStr).doubleValue();
                }
                else {
                    MipavUtil.displayError("= field does not have a valid value");
                    textEqual.requestFocus();
                    textEqual.selectAll();
                    return false;
                }
            } // if (hasEqual)
            
            hasLesserEqual = lesserEqualCheckBox.isSelected();
            if (hasLesserEqual) {
                tmpStr = textLesserEqual.getText();
                if (testParameter(tmpStr, -Double.MAX_VALUE, Double.MAX_VALUE)) {
                    lesserEqualValue = Double.valueOf(tmpStr).doubleValue();
                }
                else {
                    MipavUtil.displayError("<= field does not have a valid value");
                    textLesserEqual.requestFocus();
                    textLesserEqual.selectAll();
                    return false;
                }
            } // if (hasLesserEqual)
        } // if (method == AlgorithmKernelRegression.ITERATIVE_STEERING_KERNEL_SECOND_ORDER_IRREGULAR)
        
        if (method == AlgorithmKernelRegression.STEERING_KERNEL_SECOND_ORDER_L1_NORM) {
            tmpStr = textIterations2.getText();
            
            if (testParameter(tmpStr, 1, 10000)) {
                iterations2 = Integer.valueOf(tmpStr).intValue();
            }
            else {
                MipavUtil.displayError("Iterations2 must be between 1 and 10000");
                textIterations2.requestFocus();
                textIterations2.selectAll();
                return false;
            }
            
            tmpStr = textIterativeGlobal2.getText();
            if (testParameter(tmpStr, 0.01, 100.0)) {
                iterativeGlobalSmoothing2 = Float.valueOf(tmpStr).floatValue();
            }
            else {
                MipavUtil.displayError("Iterative global smoothing 2 must be between 0.01 and 100.0");
                textIterativeGlobal2.requestFocus();
                textIterativeGlobal2.selectAll();
                return false;
            }   

            tmpStr = textClassicStep.getText();
            if (testParameter(tmpStr, 1.0E-8, 1.0)) {
                classicStepSize = Float.valueOf(tmpStr).floatValue();
            }
            else {
                MipavUtil.displayError("Classic step size must be between 1.0E-8 and 1.0");
                textClassicStep.requestFocus();
                textClassicStep.selectAll();
                return false;
            }
            
            tmpStr = textSteeringStep.getText();
            if (testParameter(tmpStr, 1.0E-8, 1.0)) {
                steeringStepSize = Float.valueOf(tmpStr).floatValue();
            }
            else {
                MipavUtil.displayError("Steering step size must be between 1.0E-8 and 1.0");
                textSteeringStep.requestFocus();
                textSteeringStep.selectAll();
                return false;
            }
        } // if (method == AlgorithmKernelRegression.STEERING_KERNEL_SECOND_ORDER_L1_NORM)

        if (image.getNDims() > 2) {
            image25D = image25DCheckBox.isSelected();
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
                return new String("Applies a kernel regression filter.");
            }

            public String getDescriptionLong() {
                return new String("Applies a kernel regression filter.");
            }

            public String getShortLabel() {
                return new String("KernelRegression");
            }

            public String getLabel() {
                return new String("Kernel Regression");
            }

            public String getName() {
                return new String("Kernel Regression");
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

        /*method = scriptParameters.getParams().getInt("_method");*/
        
        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            
            //Parameters
            table.put(new ParameterInt("_method", 1));
            
            final Parameter method = table.getParameter("_method");
            
            Parameter p = new ParameterBoolean("has_NaN", false);
            p.setParentCondition(method, Integer.toString(AlgorithmKernelRegression.ITERATIVE_STEERING_KERNEL_SECOND_ORDER_IRREGULAR));
            table.put(p);
            
            p = (new ParameterBoolean("has_infinity", false));
            p.setParentCondition(method, Integer.toString(AlgorithmKernelRegression.ITERATIVE_STEERING_KERNEL_SECOND_ORDER_IRREGULAR));
            table.put(p);
            
            p = (new ParameterBoolean("has_greater_equal", false));
            p.setParentCondition(method, Integer.toString(AlgorithmKernelRegression.ITERATIVE_STEERING_KERNEL_SECOND_ORDER_IRREGULAR));
            table.put(p);
            
            p = (new ParameterDouble("greater_equal_value", 0));
            p.setParentCondition(method, Integer.toString(AlgorithmKernelRegression.ITERATIVE_STEERING_KERNEL_SECOND_ORDER_IRREGULAR));
            table.put(p);
            
            table.put(new ParameterBoolean("has_equal", false));
            p.setParentCondition(method, Integer.toString(AlgorithmKernelRegression.ITERATIVE_STEERING_KERNEL_SECOND_ORDER_IRREGULAR));
            table.put(p);
            
            p = (new ParameterDouble("equal_value", 0));
            p.setParentCondition(method, Integer.toString(AlgorithmKernelRegression.ITERATIVE_STEERING_KERNEL_SECOND_ORDER_IRREGULAR));
            table.put(p);
            
            p = (new ParameterBoolean("has_lesser_equal", false));
            p.setParentCondition(method, Integer.toString(AlgorithmKernelRegression.ITERATIVE_STEERING_KERNEL_SECOND_ORDER_IRREGULAR));
            table.put(p);
            
            p = (new ParameterDouble("lesser_equal_value", 0));
            p.setParentCondition(method, Integer.toString(AlgorithmKernelRegression.ITERATIVE_STEERING_KERNEL_SECOND_ORDER_IRREGULAR));
            table.put(p);
            
            table.put(new ParameterFloat("initial_global_smoothing", 0.5f));
            table.put(new ParameterFloat("iterative_global_smoothing", 2.4f));
            
            p = (new ParameterFloat("iterative_global_smoothing2", 1.75f));
            p.setParentCondition(method, Integer.toString(AlgorithmKernelRegression.STEERING_KERNEL_SECOND_ORDER_L1_NORM));
            table.put(p);
            
            table.put(new ParameterInt("_upscale", 1));           
            table.put(new ParameterInt("initial_kernel_size", 5));
            table.put(new ParameterInt("iterative_kernel_size", 21));
            table.put(new ParameterInt("_iterations", 4));
            
            p = (new ParameterInt("_iterations2", 300));
            p.setParentCondition(method, Integer.toString(AlgorithmKernelRegression.STEERING_KERNEL_SECOND_ORDER_L1_NORM));
            table.put(p);
            
            table.put(new ParameterInt("window_size", 11));
            table.put(new ParameterFloat("_lambda", 1.0f));
            table.put(new ParameterFloat("_alpha", 0.5f));
            
            p = (new ParameterFloat("classic_step_size", 0.1f));
            p.setParentCondition(method, Integer.toString(AlgorithmKernelRegression.STEERING_KERNEL_SECOND_ORDER_L1_NORM));
            table.put(p);
            
            p = (new ParameterFloat("steering_step_size", 0.1f));
            p.setParentCondition(method, Integer.toString(AlgorithmKernelRegression.STEERING_KERNEL_SECOND_ORDER_L1_NORM));
            table.put(p);
            
            //Output options
            p = (new ParameterBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D, true));
            //This is kind of hacky. Prevents the check box from ever being enabled.
            p.setParentCondition(method, "5");
            table.put(p);
            
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
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
                return getResultImage().getImageName();						///DOING IT WRONG??
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

}
