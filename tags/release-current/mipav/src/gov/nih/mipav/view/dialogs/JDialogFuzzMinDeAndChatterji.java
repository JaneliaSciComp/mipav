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
public class JDialogFuzzMinDeAndChatterji extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    

    //~ Instance fields ------------------------------------------------------------------------------------------------

    

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    
    private AlgorithmFuzzMinDeAndChatterji fuzzMinAlgo = null;

    /** DOCUMENT ME! */
    private JPanelAlgorithmOutputOptions outputPanel;

    
    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;
    
    private double minVal;
    
    private double maxVal;
    
    private double crossVal;
    
    private JLabel labelgmin;
    
    private JTextField textgmin;
    
    private double gmin;
    
    private JLabel labelgmax;
    
    private JTextField textgmax;
    
    private double gmax;
    
    private JLabel labelSrcThreshold;
    
    private JTextField textSrcThreshold;
    
    /** Original image threshold */
    private double srcThreshold;
    
    private JCheckBox autoCheckBox;
    
    /** If true, ignore srcThreshold input and calculate best value */
    private boolean autoThreshold;
    
    private JLabel labelEnhancedThreshold;
    
    private JTextField textEnhancedThreshold;
    
    /** Enhanced image threshold */
    private double enhancedThreshold;
    
    private JLabel labelTheta1;
    
    private JTextField textTheta1;
    
    /** Angular equivalence of black band, 0 <= theta1 <= PI/2 */
    private double theta1;
    
    private JLabel labelTheta2;
    
    private JTextField textTheta2;
    
    /** Angular equivalence of white band, 0 <= theta2 <= PI/2 */
    private double theta2;
    
    private JLabel labelP1;
    
    private JTextField textP1;
    
    /** Black region exponent, 0 <= p1 <= 1.0 */
    private double p1;
    
    private JLabel labelP2;
    
    private JTextField textP2;
    
    /** White region exponent, 0 <= p2 <= 1.0 */
    private double p2;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogFuzzMinDeAndChatterji() { }

    /**
     * Creates a new JDialogFuzzyMinimization object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogFuzzMinDeAndChatterji(Frame theParentFrame, ModelImage im) {
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
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        } else if (source == autoCheckBox) {
        	if (autoCheckBox.isSelected()) {
        		labelSrcThreshold.setEnabled(false);
        		textSrcThreshold.setEnabled(false);
        	}
        	else {
        		labelSrcThreshold.setEnabled(true);
        		textSrcThreshold.setEnabled(true);	
        	}
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

        if (algorithm instanceof AlgorithmFuzzMinDeAndChatterji) {
            System.err.println("FuzzMinDeAndChatterji Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((fuzzMinAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                try {

                    // resultImage.setImageName("FuzzMinDeAndChatterji: "+image.getImageName());
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

        fuzzMinAlgo.finalize();
        fuzzMinAlgo = null;
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
     * Accessor that sets the srcThreshold value
     * 
     * @param srcThreshold
     */
    public void setSrcThreshold(double srcThreshold) {
    	this.srcThreshold = srcThreshold;
    }
    
    /**
     * Accessor that sets the autoThreshold value
     * 
     * @param autoThreshold
     */
    public void setAutoThreshold(boolean autoThreshold) {
    	this.autoThreshold = autoThreshold;
    }
    
    /**
     * Accessor that sets the enahncedThreshold value
     * 
     * @param enchancedThreshold
     */
    public void setEnhancedThreshold(double enhancedThreshold) {
    	this.enhancedThreshold = enhancedThreshold;
    }
    
    /**
     * Accessor that sets the theta1 value
     * 
     * @param theta1
     */
    public void setTheta1(double theta1) {
    	this.theta1 = theta1;
    }
    
    /**
     * Accessor that sets the theta2 value
     * 
     * @param theta2
     */
    public void setTheta2(double theta2) {
    	this.theta2 = theta2;
    }
    
    /**
     * Accessor that sets the p1 value
     * 
     * @param p1
     */
    public void setP1(double p1) {
    	this.p1 = p1;
    }
    
    /**
     * Accessor that sets the p2 value
     * 
     * @param p2
     */
    public void setP2(double p2) {
    	this.p2 = p2;
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
     * Once all the necessary variables are set, call the median algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_fuzzyMin");
        int zDim;
        int newType;
        theta1 = (Math.PI/180.0)*theta1;
        theta2 = (Math.PI/180.0)*theta2;
            

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
                   fuzzMinAlgo = new AlgorithmFuzzMinDeAndChatterji(resultImage, image, srcThreshold, autoThreshold, enhancedThreshold,
                		                             theta1, theta2, p1, p2,
                                                     gmin, gmax, outputPanel.isProcessWholeImageSet());

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    fuzzMinAlgo.addListener(this);

                    createProgressBar(image.getImageName(), fuzzMinAlgo);

                    setVisible(false); // Hide dialog

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (fuzzMinAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        fuzzMinAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog median: unable to allocate enough memory");

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
                    fuzzMinAlgo = new AlgorithmFuzzMinDeAndChatterji(image, srcThreshold, autoThreshold, enhancedThreshold,
                    		                         theta1, theta2, p1, p2,
                                                     gmin, gmax, outputPanel.isProcessWholeImageSet());

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    fuzzMinAlgo.addListener(this);
                    createProgressBar(image.getImageName(), fuzzMinAlgo);

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
                        if (fuzzMinAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        fuzzMinAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog FuzzMinDeAndChatterji: unable to allocate enough memory");

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

        setSrcThreshold(scriptParameters.getParams().getDouble("source_threshold"));
        setAutoThreshold(scriptParameters.getParams().getBoolean("auto_threshold"));
        setEnhancedThreshold(scriptParameters.getParams().getDouble("enhanced_threshold"));
        setTheta1(scriptParameters.getParams().getDouble("theta1"));
        setTheta2(scriptParameters.getParams().getDouble("theta2"));
        setP1(scriptParameters.getParams().getDouble("p1"));
        setP2(scriptParameters.getParams().getDouble("p2"));
        setgmin(scriptParameters.getParams().getDouble("gmin"));
        setgmax(scriptParameters.getParams().getDouble("gmax"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        scriptParameters.storeOutputImageParams(getResultImage(), outputPanel.isOutputNewImageSet());

        scriptParameters.getParams().put(ParameterFactory.newParameter("source_threshold", srcThreshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("auto_threshold", autoThreshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("enhanced_threshold", enhancedThreshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("theta1", theta1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("theta2", theta2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("p1", p1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("p2", p2));
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
        setTitle("Fuzzy Minimization De and Chatterji");
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
        
        labelSrcThreshold = createLabel("Source threshold (> " + minVal + " to < " + maxVal + "):");
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelSrcThreshold, gbc);
        paramPanel.add(labelSrcThreshold); // add the instructions


        textSrcThreshold = createTextField(String.valueOf(crossVal)); // make & set input
        textSrcThreshold.setColumns(10);
        textSrcThreshold.setMaximumSize(textSrcThreshold.getPreferredSize()); // don't let it get any bigger
        
     // than what it prefers
        textSrcThreshold.setHorizontalAlignment(JTextField.CENTER);
        textSrcThreshold.setFont(serif12);
        MipavUtil.makeNumericsOnly(textSrcThreshold, true);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(textSrcThreshold, gbc);
        paramPanel.add(textSrcThreshold); // add inpu
        
        autoCheckBox = new JCheckBox("Auto determination of source threshold", false);
        autoCheckBox.setFont(serif12);
        autoCheckBox.addActionListener(this);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(autoCheckBox, gbc);
        paramPanel.add(autoCheckBox); // add the instructions
        
        labelEnhancedThreshold = createLabel("Enhanced threshold (> new minimum value to < new maximum value ):");
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelEnhancedThreshold, gbc);
        paramPanel.add(labelEnhancedThreshold); // add the instructions


        textEnhancedThreshold = createTextField(String.valueOf(crossVal)); // make & set input
        textEnhancedThreshold.setColumns(10);
        textEnhancedThreshold.setMaximumSize(textEnhancedThreshold.getPreferredSize()); // don't let it get any bigger

        // than what it prefers
        textEnhancedThreshold.setHorizontalAlignment(JTextField.CENTER);
        textEnhancedThreshold.setFont(serif12);
        MipavUtil.makeNumericsOnly(textEnhancedThreshold, true);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(textEnhancedThreshold, gbc);
        paramPanel.add(textEnhancedThreshold); // add inpu

        labelTheta1 = createLabel("Black region angular equivalence (0.0 - 90.0):");
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelTheta1, gbc);
        paramPanel.add(labelTheta1); // add the instructions


        textTheta1 = createTextField("45.0"); // make & set input
        textTheta1.setColumns(10);
        textTheta1.setMaximumSize(textTheta1.getPreferredSize()); // don't let it get any bigger

        // than what it prefers
        textTheta1.setHorizontalAlignment(JTextField.CENTER);
        textTheta1.setFont(serif12);
        MipavUtil.makeNumericsOnly(textTheta1, true);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(textTheta1, gbc);
        paramPanel.add(textTheta1); // add input
        
        labelTheta2 = createLabel("White region angular equivalence (0.0 - 90.0):");
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelTheta2, gbc);
        paramPanel.add(labelTheta2); // add the instructions


        textTheta2 = createTextField("45.0"); // make & set input
        textTheta2.setColumns(10);
        textTheta2.setMaximumSize(textTheta2.getPreferredSize()); // don't let it get any bigger

        // than what it prefers
        textTheta2.setHorizontalAlignment(JTextField.CENTER);
        textTheta2.setFont(serif12);
        MipavUtil.makeNumericsOnly(textTheta2, true);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(textTheta2, gbc);
        paramPanel.add(textTheta2); // add input
        
        labelP1 = createLabel("Black region exponential power (0.0 - 1.0):");
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelP1, gbc);
        paramPanel.add(labelP1); // add the instructions


        textP1 = createTextField("0.5"); // make & set input
        textP1.setColumns(10);
        textP1.setMaximumSize(textP1.getPreferredSize()); // don't let it get any bigger

        // than what it prefers
        textP1.setHorizontalAlignment(JTextField.CENTER);
        textP1.setFont(serif12);
        MipavUtil.makeNumericsOnly(textP1, true);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(textP1, gbc);
        paramPanel.add(textP1); // add input
        
        labelP2 = createLabel("White region exponential power (0.0 - 1.0):");
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelP2, gbc);
        paramPanel.add(labelP2); // add the instructions


        textP2 = createTextField("0.5"); // make & set input
        textP2.setColumns(10);
        textP2.setMaximumSize(textP2.getPreferredSize()); // don't let it get any bigger

        // than what it prefers
        textP2.setHorizontalAlignment(JTextField.CENTER);
        textP2.setFont(serif12);
        MipavUtil.makeNumericsOnly(textP2, true);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(textP2, gbc);
        paramPanel.add(textP2); // add input
        
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
        
        autoThreshold = autoCheckBox.isSelected();
        
        if (!autoThreshold) {
	        // verify crossover value if within bounds
	        tmpStr = textSrcThreshold.getText();
	
	        if (testParameter(tmpStr, minVal, maxVal)) {
	            srcThreshold = Double.valueOf(tmpStr).doubleValue();
	            if ((srcThreshold == minVal) || (srcThreshold == maxVal)) {
	            	textSrcThreshold.requestFocus();
	                textSrcThreshold.selectAll();
	
	                return false;	
	            }
	        } else {
	            textSrcThreshold.requestFocus();
	            textSrcThreshold.selectAll();
	
	            return false;
	        }
        } // if (!autoThreshold)
        
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


        tmpStr = textEnhancedThreshold.getText();

        if (testParameter(tmpStr, gmin, gmax)) {
            enhancedThreshold = Double.valueOf(tmpStr).doubleValue();
            if ((enhancedThreshold == gmin) || (enhancedThreshold == gmax)) {
            	textEnhancedThreshold.requestFocus();
                textEnhancedThreshold.selectAll();

                return false;	
            }
        } else {
            textEnhancedThreshold.requestFocus();
            textEnhancedThreshold.selectAll();

            return false;
        }
        
        tmpStr = textTheta1.getText();
        if (testParameter(tmpStr,0.0,90.0)) {
        	theta1 = Double.valueOf(tmpStr).doubleValue();
        }
        else {
        	textTheta1.requestFocus();
        	textTheta1.selectAll();
        	return false;
        }
        
        tmpStr = textTheta2.getText();
        if (testParameter(tmpStr,0.0,90.0)) {
        	theta2 = Double.valueOf(tmpStr).doubleValue();
        }
        else {
        	textTheta2.requestFocus();
        	textTheta2.selectAll();
        	return false;
        }
        
        tmpStr = textP1.getText();
        if (testParameter(tmpStr,0.0,1.0)) {
        	p1 = Double.valueOf(tmpStr).doubleValue();
        }
        else {
        	textP1.requestFocus();
        	textP1.selectAll();
        	return false;
        }
        
        tmpStr = textP2.getText();
        if (testParameter(tmpStr,0.0,1.0)) {
        	p2 = Double.valueOf(tmpStr).doubleValue();
        }
        else {
        	textP2.requestFocus();
        	textP2.selectAll();
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
                return new String("Applies fuzzy minimization De and Chatterji.");
            }

            public String getDescriptionLong() {
                return new String("Applies fuzzy minimization De and Chatterji.");
            }

            public String getShortLabel() {
                return new String("Fuzzy minimization De and Chatterji");
            }

            public String getLabel() {
                return new String("Fuzzy minimization De and Chatterji");
            }

            public String getName() {
                return new String("Fuzzy Minimization De and Chatterji");
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
            table.put(new ParameterDouble("source_threshold", 0.0));
            table.put(new ParameterBoolean("auto_threshold",false));
            table.put(new ParameterDouble("enhanced_threshold", 1.0));
            table.put(new ParameterDouble("theta1", 45.0));
            table.put(new ParameterDouble("theta2", 45.0));
            table.put(new ParameterDouble("p1", 0.5));
            table.put(new ParameterDouble("p2", 0.5));
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
