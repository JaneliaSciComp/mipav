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
        implements AlgorithmInterface, ScriptableActionInterface, DialogDefaultsInterface {

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
    
    private float initialGlobalSmoothing = 0.5f;
    
    private JLabel labelIterativeGlobal;
    
    private JTextField textIterativeGlobal;
    
    private float iterativeGlobalSmoothing = 2.4f;
    
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
    
    /** Total number of iterations */
    private int iterations = 4;
    
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
    
    private JRadioButton regSampled2Classic;

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
        } else if ((source.equals(iterSteering2)) || (source.equals(regSampled2Classic))) {
            if (iterSteering2.isSelected()) {
                textIterativeGlobal.setEnabled(true);
                textIterativeKernel.setEnabled(true);
                textIterations.setEnabled(true);
                textWindowSize.setEnabled(true);
                textLambda.setEnabled(true);
                textAlpha.setEnabled(true);    
            }
            else if (regSampled2Classic.isSelected()) {
                textIterativeGlobal.setEnabled(false);
                textIterativeKernel.setEnabled(false);
                textIterations.setEnabled(false);
                textWindowSize.setEnabled(false);
                textLambda.setEnabled(false);
                textAlpha.setEnabled(false);        
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
                Vector imageFrames = image.getImageFrameVector();

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
        str += initialGlobalSmoothing + delim;
        str += iterativeGlobalSmoothing + delim;
        str += upscale + delim;
        str += initialKernelSize + delim;
        str += iterativeKernelSize + delim;
        str += iterations + delim;
        str += windowSize + delim;
        str += lambda + delim;
        str += alpha + delim;
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
                    regSampled2Classic.setSelected(false);
                }
                else if (method == AlgorithmKernelRegression.REGULARLY_SAMPLED_SECOND_ORDER_CLASSIC) {
                    iterSteering2.setSelected(false);
                    regSampled2Classic.setSelected(true);
                }
                textInitialGlobal.setText("" + MipavUtil.getFloat(st));
                textIterativeGlobal.setText("" + MipavUtil.getFloat(st));
                textUpscale.setText("" + MipavUtil.getInt(st));
                textInitialKernel.setText("" +  MipavUtil.getInt(st));
                textIterativeKernel.setText("" + MipavUtil.getInt(st));
                textIterations.setText("" + MipavUtil.getInt(st));
                textWindowSize.setText("" + MipavUtil.getInt(st));
                textLambda.setText("" + MipavUtil.getFloat(st));
                textAlpha.setText("" + MipavUtil.getFloat(st));
                if (method == AlgorithmKernelRegression.ITERATIVE_STEERING_KERNEL_SECOND_ORDER) {
                    textIterativeGlobal.setEnabled(true);
                    textIterativeKernel.setEnabled(true);
                    textIterations.setEnabled(true);
                    textWindowSize.setEnabled(true);
                    textLambda.setEnabled(true);
                    textAlpha.setEnabled(true);
                }
                else if (method == AlgorithmKernelRegression.REGULARLY_SAMPLED_SECOND_ORDER_CLASSIC) {
                    textIterativeGlobal.setEnabled(false);
                    textIterativeKernel.setEnabled(false);
                    textIterations.setEnabled(false);
                    textWindowSize.setEnabled(false);
                    textLambda.setEnabled(false);
                    textAlpha.setEnabled(false);    
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
                        initialGlobalSmoothing, iterativeGlobalSmoothing,
                        upscale, initialKernelSize, iterativeKernelSize, iterations,
                        windowSize, lambda, alpha, image25D);

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
                        initialGlobalSmoothing, iterativeGlobalSmoothing,
                        upscale, initialKernelSize, iterativeKernelSize, iterations,
                        windowSize, lambda, alpha, image25D);

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
                Vector imageFrames = image.getImageFrameVector();
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
        initialGlobalSmoothing = scriptParameters.getParams().getFloat("initial_global_smoothing");
        iterativeGlobalSmoothing = scriptParameters.getParams().getFloat("iterative_global_smoothing");
        upscale = scriptParameters.getParams().getInt("_upscale");
        initialKernelSize = scriptParameters.getParams().getInt("initial_kernel_size");
        iterativeKernelSize = scriptParameters.getParams().getInt("iterative_kernel_size");
        iterations = scriptParameters.getParams().getInt("_iterations");
        windowSize = scriptParameters.getParams().getInt("window_size");
        lambda = scriptParameters.getParams().getFloat("_lambda");
        alpha = scriptParameters.getParams().getFloat("_alpha");
        image25D = scriptParameters.doProcess3DAs25D();
    }


    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("_method", method));
        scriptParameters.getParams().put(ParameterFactory.newParameter("initial_global_smoothing", initialGlobalSmoothing));
        scriptParameters.getParams().put(ParameterFactory.newParameter("iterative_global_smoothing", iterativeGlobalSmoothing));
        scriptParameters.getParams().put(ParameterFactory.newParameter("_upscale", upscale));
        scriptParameters.getParams().put(ParameterFactory.newParameter("initial_kernel_scale", initialKernelSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("iterative_kernel_size", iterativeKernelSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("_iterations", iterations));
        scriptParameters.getParams().put(ParameterFactory.newParameter("window_size", windowSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("_lambda", lambda));
        scriptParameters.getParams().put(ParameterFactory.newParameter("_alpha", alpha));
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
        regSampled2Classic = new JRadioButton("Regularly Sampled Second Order Classic", false);
        regSampled2Classic.setFont(serif12);
        regSampled2Classic.setForeground(Color.black);
        regSampled2Classic.addActionListener(this);
        methodGroup.add(regSampled2Classic);
        paramPanel.add(regSampled2Classic, gbc2);

        gbc.gridy = yPos++;
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
        textIterations = createTextField("4");
        paramPanel.add(textIterations, gbc2);
        
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
        else if (regSampled2Classic.isSelected()) {
            method = AlgorithmKernelRegression.REGULARLY_SAMPLED_SECOND_ORDER_CLASSIC;
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
        
        if (method == AlgorithmKernelRegression.ITERATIVE_STEERING_KERNEL_SECOND_ORDER) {
            tmpStr = textIterations.getText();
            
            if (testParameter(tmpStr, 1, 100)) {
                iterations = Integer.valueOf(tmpStr).intValue();
            }
            else {
                MipavUtil.displayError("Iterations must be between 1 and 100");
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
        } // if (method == AlgorithmKernelRegression.ITERATIVE_STEERING_KERNEL_SECOND_ORDER)

        if (image.getNDims() > 2) {
            image25D = image25DCheckBox.isSelected();
        }

        return true;
    }

}
