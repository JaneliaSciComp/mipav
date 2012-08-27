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
 * Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image. In addition the user can select having the algorithm applied to whole image or to the VOI regions. It
 * should be noted that the algorithms are executed in their own thread.
 *
 * @version  1.0; 17 February 2000
 */
public class JDialogFuzzyMinimization extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    

    //~ Instance fields ------------------------------------------------------------------------------------------------

    

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private int iters;

    /** DOCUMENT ME! */
    private JLabel labelExpFuzzifier;

    
    private AlgorithmFuzzyMinimization fuzzyMinAlgo = null;

    /** DOCUMENT ME! */
    private JPanelAlgorithmOutputOptions outputPanel;

    
    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private double expFuzzifier;

    /** DOCUMENT ME! */
    private JTextField textNIter;

    /** DOCUMENT ME! */
    private JTextField textExpFuzzifier; // textfield to hold exponential fuzzifier value (1.0-2.0).

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;
    
    private double minVal;
    
    private double maxVal;
    
    private JLabel labelCross;
    
    private JTextField textCross;
    
    private double crossVal;
    
    private JLabel labelgmin;
    
    private JTextField textgmin;
    
    private double gmin;
    
    private JLabel labelgmax;
    
    private JTextField textgmax;
    
    private double gmax;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogFuzzyMinimization() { }

    /**
     * Creates a new JDialogFuzzyMinimization object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogFuzzyMinimization(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        if (im.getType() == ModelImage.BOOLEAN) {
            MipavUtil.displayError("Source Image must NOT be Boolean");
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
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
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

        if (algorithm instanceof AlgorithmFuzzyMinimization) {
            System.err.println("Fuzzy Minimization Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((fuzzyMinAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                try {

                    // resultImage.setImageName("Fuzzy Minimization: "+image.getImageName());
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

        fuzzyMinAlgo.finalize();
        fuzzyMinAlgo = null;
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
     * Accessor that sets the number of iterations.
     *
     * @param  num  Value to set iterations to (should be between 1 and 20).
     */
    public void setIters(int num) {
        iters = num;
    }

    /**
     * Accessor that sets the kernel shape.
     *
     * @param  shape  Value to set size to (0 == square, 1 == cross).
     */
    
    /**
     * Accessor that sets the crossover value at which the membership function = 0.5
     * 
     * @param crossVal
     */
    public void setCrossoverValue(double crossVal) {
    	this.crossVal = crossVal;
    }
    
    /**
     * 
     * @param gmin
     */
    public void setgmin(double gmin) {
    	this.gmin = gmin;
    }
    
    /**
     * 
     * @param gmax
     */
    public void setgmax(double gmax) {
    	this.gmax = gmax;
    }
    

    /**
     * Accessor that sets the exponential fuzzifier.
     *
     * @param  dev  Value to set the exponential fuzzifier to (should be between 1.0 and 2.0).
     */
    public void setExpFuzzifier(double expFuzzifier) {
        this.expFuzzifier = expFuzzifier;
    }

    /**
     * Once all the necessary variables are set, call the median algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_fuzzyMin");
        int zDim;
        int newType;

            

            if (outputPanel.isOutputNewImageSet()) {

                try {
                	newType = image.getType();
                    if (newType == ModelStorageBase.DOUBLE) {
                    	
                    }
                    else if (newType == ModelStorageBase.FLOAT) {
                    	if ((gmin < -Float.MAX_VALUE) || (gmax > Float.MAX_VALUE)) {
                    		newType = ModelStorageBase.DOUBLE;
                    	}
                    }
                    else if ((gmin < Long.MIN_VALUE) || (gmax > Long.MAX_VALUE)) {
                    	newType = ModelStorageBase.DOUBLE;
                    }
                    else if (newType == ModelStorageBase.LONG) {
                    	
                    }
                    else if (newType == ModelStorageBase.UINTEGER) {
                    	if ((gmin < 0) || (gmax > 4294967295L)) {
                    	    newType = ModelStorageBase.LONG;
                    	}
                    }
                    else if ((gmin < Integer.MIN_VALUE) || (gmax > Integer.MAX_VALUE)) {
                    	newType = ModelStorageBase.LONG;
                    }
                    else if (newType == ModelStorageBase.INTEGER) {
                    	
                    }
                    else if (newType == ModelStorageBase.USHORT) {
                    	if ((gmin < 0) || (gmax > 65535)) {
                    		newType = ModelStorageBase.INTEGER;
                    	}
                    }
                    else if ((gmin < -32768) || (gmax > 32767)) {
                    	newType = ModelStorageBase.INTEGER;
                    }
                    else if (newType == ModelStorageBase.SHORT) {
                    	
                    }
                    else if (newType == ModelStorageBase.UBYTE) {
                    	if ((gmin < 0) || (gmax > 255)) {
                    		newType = ModelStorageBase.SHORT;
                    	}
                    }
                    else if (newType == ModelStorageBase.BYTE) {
                    	if ((gmin < -128) || (gmax > 127)) {
                    		newType = ModelStorageBase.SHORT;
                    	}
                    }

                    resultImage     = new ModelImage(newType, image.getExtents(), name);
                    if (resultImage.getNDims() >= 3) {
                    	zDim = resultImage.getExtents()[2];
                    }
                    else {
                    	zDim = 1;
                    }

                    if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                    	for (int i = 0; i < zDim; i++) {
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                        }
                    }

                    // Make algorithm
                   fuzzyMinAlgo = new AlgorithmFuzzyMinimization(resultImage, image, iters, crossVal, expFuzzifier,
                                                     gmin, gmax, outputPanel.isProcessWholeImageSet());

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    fuzzyMinAlgo.addListener(this);

                    createProgressBar(image.getImageName(), fuzzyMinAlgo);

                    setVisible(false); // Hide dialog

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (fuzzyMinAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        fuzzyMinAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Fuzzy Minimization: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }

                    return;
                }
            } else { // displayLoc == REPLACE

                try {

                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    fuzzyMinAlgo = new AlgorithmFuzzyMinimization(image, iters, crossVal, expFuzzifier,
                                                     gmin, gmax, outputPanel.isProcessWholeImageSet());

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    fuzzyMinAlgo.addListener(this);
                    createProgressBar(image.getImageName(), fuzzyMinAlgo);

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
                        if (fuzzyMinAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        fuzzyMinAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Fuzzy Minimization: unable to allocate enough memory");

                    return;
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

        if (image.getType() == ModelImage.BOOLEAN) {
            MipavUtil.displayError("Source Image must NOT be Boolean");
            dispose();

            return;
        }

        outputPanel = new JPanelAlgorithmOutputOptions(image);
        scriptParameters.setOutputOptionsGUI(outputPanel);

        setIters(scriptParameters.getNumIterations());
        setCrossoverValue(scriptParameters.getParams().getDouble("crossover_value"));
        setExpFuzzifier(scriptParameters.getParams().getDouble("exponential_fuzzifier"));
        setgmin(scriptParameters.getParams().getDouble("gmin"));
        setgmax(scriptParameters.getParams().getDouble("gmax"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        scriptParameters.storeOutputImageParams(getResultImage(), outputPanel.isOutputNewImageSet());

        scriptParameters.storeNumIterations(iters);
        scriptParameters.getParams().put(ParameterFactory.newParameter("crossover_value", crossVal));
        scriptParameters.getParams().put(ParameterFactory.newParameter("exponential_fuzzifier", expFuzzifier));
        scriptParameters.getParams().put(ParameterFactory.newParameter("gmin", gmin));
        scriptParameters.getParams().put(ParameterFactory.newParameter("gmax", gmax));
    }

    
    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
    	image.calcMinMax();
    	minVal = image.getMin();
    	maxVal = image.getMax();
    	crossVal = (minVal + maxVal)/2.0;
        setForeground(Color.black);
        setTitle("Fuzzy Minimization");
        Box setupBox = new Box(BoxLayout.Y_AXIS);

        // paramPanel holds all the "Parameters"
        JPanel paramPanel = new JPanel();

        // panel gets a grid layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(2, 0, 2, 0); // component width = minwidth + (2ipadx)
        paramPanel.setLayout(gbl);

        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Parameters")); // set the border ... "Parameters"

        // iterations
        JLabel labelNIter = createLabel("Number of iterations (1-5):"); // make and set the iteration instruction
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelNIter, gbc);
        paramPanel.add(labelNIter);

        textNIter = createTextField("1"); // make and set the input field
        textNIter.setColumns(2);
        textNIter.setMaximumSize(textNIter.getPreferredSize()); // don't let it get any bigger than what it prefers
        textNIter.setHorizontalAlignment(JTextField.CENTER);
        textNIter.setFont(serif12);
        MipavUtil.makeNumericsOnly(textNIter, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(textNIter, gbc);
        paramPanel.add(textNIter);
        
        labelCross = createLabel("Crossover value (> " + minVal + " to < " + maxVal + "):");
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelCross, gbc);
        paramPanel.add(labelCross); // add the instructions


        textCross = createTextField(String.valueOf(crossVal)); // make & set input
        textCross.setColumns(10);
        textCross.setMaximumSize(textCross.getPreferredSize()); // don't let it get any bigger

        // than what it prefers
        textCross.setHorizontalAlignment(JTextField.CENTER);
        textCross.setFont(serif12);
        MipavUtil.makeNumericsOnly(textCross, true);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(textCross, gbc);
        paramPanel.add(textCross); // add inpu

        labelExpFuzzifier = createLabel("Exponential fuzzifier (1.0 - 2.0):");
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelExpFuzzifier, gbc);
        paramPanel.add(labelExpFuzzifier); // add the instructions


        textExpFuzzifier = createTextField("1.0"); // make & set input
        textExpFuzzifier.setColumns(5);
        textExpFuzzifier.setMaximumSize(textExpFuzzifier.getPreferredSize()); // don't let it get any bigger

        // than what it prefers
        textExpFuzzifier.setHorizontalAlignment(JTextField.CENTER);
        textExpFuzzifier.setFont(serif12);
        MipavUtil.makeNumericsOnly(textExpFuzzifier, true);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(textExpFuzzifier, gbc);
        paramPanel.add(textExpFuzzifier); // add input
        
        labelgmin = createLabel("New minimum value (<= " + minVal + "):");
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelgmin, gbc);
        paramPanel.add(labelgmin); // add the instructions


        textgmin = createTextField(String.valueOf(minVal)); // make & set input
        textgmin.setColumns(10);
        textgmin.setMaximumSize(textgmin.getPreferredSize()); // don't let it get any bigger

        // than what it prefers
        textgmin.setHorizontalAlignment(JTextField.CENTER);
        textgmin.setFont(serif12);
        MipavUtil.makeNumericsOnly(textgmin, true);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(textgmin, gbc);
        paramPanel.add(textgmin); // add input
        
        labelgmax = createLabel("New maximum value (>= " + maxVal + "):");
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelgmax, gbc);
        paramPanel.add(labelgmax); // add the instructions


        textgmax = createTextField(String.valueOf(maxVal)); // make & set input
        textgmax.setColumns(10);
        textgmax.setMaximumSize(textgmax.getPreferredSize()); // don't let it get any bigger

        // than what it prefers
        textgmax.setHorizontalAlignment(JTextField.CENTER);
        textgmax.setFont(serif12);
        MipavUtil.makeNumericsOnly(textgmax, true);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(textgmax, gbc);
        paramPanel.add(textgmax); // add input
        setupBox.add(paramPanel);
        
        outputPanel = new JPanelAlgorithmOutputOptions(image);
        setupBox.add(outputPanel);

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
        String tmpStr;
        image.calcMinMax();
    	minVal = image.getMin();
    	maxVal = image.getMax();

        // verify iteration is within bounds
        tmpStr = textNIter.getText();

        if (testParameter(tmpStr, 1, 5)) {
            iters = Integer.valueOf(tmpStr).intValue();
        } else {
            textNIter.requestFocus();
            textNIter.selectAll();

            return false;
        }
        
        // verify crossover value if within bounds
        tmpStr = textCross.getText();

        if (testParameter(tmpStr, minVal, maxVal)) {
            crossVal = Double.valueOf(tmpStr).doubleValue();
            if ((crossVal == minVal) || (crossVal == maxVal)) {
            	textCross.requestFocus();
                textCross.selectAll();

                return false;	
            }
        } else {
            textCross.requestFocus();
            textCross.selectAll();

            return false;
        }

        // verify exponential fuzzifier is within bounds
        tmpStr = textExpFuzzifier.getText();

        if (testParameter(tmpStr, 1.0, 2.0)) {
            expFuzzifier = Double.valueOf(tmpStr).doubleValue();
        } else {
            textExpFuzzifier.requestFocus();
            textExpFuzzifier.selectAll();

            return false;
        }
        
        tmpStr = textgmin.getText();

        if (testParameter(tmpStr, -Double.MAX_VALUE, minVal)) {
            gmin = Double.valueOf(tmpStr).doubleValue();
        } else {
            textgmin.requestFocus();
            textgmin.selectAll();

            return false;
        }
        
        tmpStr = textgmax.getText();

        if (testParameter(tmpStr, maxVal, Double.MAX_VALUE)) {
            gmax = Double.valueOf(tmpStr).doubleValue();
        } else {
            textgmax.requestFocus();
            textgmax.selectAll();

            return false;
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
                return new String("Algorithms.Fuzzy");
            }

            public String getDescription() {
                return new String("Applies fuzzy minimization.");
            }

            public String getDescriptionLong() {
                return new String("Applies fuzzy minimization.");
            }

            public String getShortLabel() {
                return new String("Fuzzy minimization");
            }

            public String getLabel() {
                return new String("Fuzzy minimization");
            }

            public String getName() {
                return new String("Fuzzy Minimization");
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
            table.put(new ParameterInt(AlgorithmParameters.NUM_ITERATIONS, 1));
            table.put(new ParameterDouble("crossover_value", 0.0));
            table.put(new ParameterDouble("exponential_fuzzifier", 1.0));
            table.put(new ParameterDouble("gmin",0.0));
            table.put(new ParameterDouble("gmax",255.0));
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

}
