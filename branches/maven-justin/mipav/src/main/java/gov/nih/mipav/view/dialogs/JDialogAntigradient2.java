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
 * Dialog to get user input, then call the algorithm. The user can select having the algorithm applied to whole image
 * or to the VOI regions. It should be noted that the algorithms are executed in their own thread.
 *
 * @version  1.0; April 24, 2013
 */
public class JDialogAntigradient2 extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private int iters;

    /** DOCUMENT ME! */
    private JLabel labelDestinationMean;

    /** DOCUMENT ME! */
    private AlgorithmAntigradient2 antigradient2Algo = null;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private double mean;

    /** DOCUMENT ME! */
    private JTextField textNIter;

    /** DOCUMENT ME! */
    private JTextField textDestinationMean; // textfield to hold destination image mean value.
    
    private boolean entireImage = true;
    
    private JRadioButton radioEntireImage;
    
    private JRadioButton radioVOIRegion;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogAntigradient2() { }

    /**
     * Creates a new JDialogAntigradient2 object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogAntigradient2(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        if (im.getType() == ModelImage.BOOLEAN) {
            MipavUtil.displayError("Source Image must NOT be Boolean");
            dispose();

            return;
        }
        if ((im.getNDims() == 3) && (im.getExtents()[2] != 2)) {
            MipavUtil.displayError("3D source image must have im.getExtents()[2] == 2");
            dispose();
            return;
        }
        if ((im.getNDims() == 4) && (im.getExtents()[3] != 3)) {
            MipavUtil.displayError("4D source image must have im.getExtents()[3] == 3");
            dispose();
            return;
        }

        image = im;
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
            //MipavUtil.showWebHelp("");
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

        if (algorithm instanceof AlgorithmAntigradient2) {
            System.err.println("Antigradient2 Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((antigradient2Algo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                try {

                    // resultImage.setImageName("Median: "+image.getImageName());
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
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
        // save the completion status for later
        setComplete(algorithm.isCompleted());

        antigradient2Algo.finalize();
        antigradient2Algo = null;
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
     * Accessor that sets destination image mean.
     *
     * @param  mean  Value to set the destination image unrecoverable mean
     */
    public void setMean(double mean) {
        this.mean = mean;
    }

    /**
     * Once all the necessary variables are set, call the median algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_inverseGradient");
        
        int destExtents[] = null;
        if (image.getNDims() == 3) {
            destExtents = new int[2];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
        }
        else if (image.getNDims() == 4) {
            destExtents = new int[3];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];
        }
        resultImage = new ModelImage(ModelStorageBase.DOUBLE, destExtents, name);
        try {
            antigradient2Algo = new AlgorithmAntigradient2(resultImage, image, entireImage, mean, iters);
            
            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            antigradient2Algo.addListener(this);

            createProgressBar(image.getImageName(), antigradient2Algo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (antigradient2Algo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                antigradient2Algo.run();
            }
        }
        catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Antigradient2: unable to allocate enough memory");

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            return;
        }
           
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();

        if (image.getType() == ModelImage.BOOLEAN) {
            MipavUtil.displayError("Source Image must NOT be Boolean");
            dispose();

            return;
        }
        
        if ((image.getNDims() == 3) && (image.getExtents()[2] != 2)) {
            MipavUtil.displayError("3D source image must have image.getExtents()[2] == 2");
            dispose();
            return;
        }
        if ((image.getNDims() == 4) && (image.getExtents()[3] != 3)) {
            MipavUtil.displayError("4D source image must have image.getExtents()[3] == 3");
            dispose();
            return;
        }

        entireImage = scriptParameters.doProcessWholeImage();
        setIters(scriptParameters.getNumIterations());
        setMean(scriptParameters.getParams().getDouble("destination_mean"));
        
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        scriptParameters.storeOutputImageParams(getResultImage(), true);

        
        scriptParameters.storeProcessWholeImage(entireImage);
        scriptParameters.storeNumIterations(iters);
        scriptParameters.getParams().put(ParameterFactory.newParameter("destination_mean", mean));
        
    }

   

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Inverse Gradient");

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

        // iterations
        JLabel labelNIter = createLabel("Number of iterations (1-100):"); // make and set the iteration instruction
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelNIter, gbc);
        maskPanel.add(labelNIter);

        textNIter = createTextField("15"); // make and set the input field
        textNIter.setColumns(3);
        textNIter.setMaximumSize(textNIter.getPreferredSize()); // don't let it get any bigger than what it prefers
        textNIter.setHorizontalAlignment(JTextField.CENTER);
        textNIter.setFont(serif12);
        MipavUtil.makeNumericsOnly(textNIter, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(textNIter, gbc);
        maskPanel.add(textNIter);

        // Unrecoverable destination image mean
        labelDestinationMean = createLabel("Destination image mean:");
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelDestinationMean, gbc);
        maskPanel.add(labelDestinationMean); // add the instructions

        textDestinationMean = createTextField("0.0"); // make & set input
        textDestinationMean.setColumns(8);
        textDestinationMean.setMaximumSize(textDestinationMean.getPreferredSize()); // don't let it get any bigger

        // than what it prefers
        textDestinationMean.setHorizontalAlignment(JTextField.CENTER);
        textDestinationMean.setFont(serif12);
        MipavUtil.makeNumericsOnly(textDestinationMean, true);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(textDestinationMean, gbc);
        maskPanel.add(textDestinationMean); // add input

        setupBox.add(maskPanel); // the parameters-panel is at the top of the box

        JPanel outputPanel = new JPanel();
        // panel gets a grid layout
        gbl = new GridBagLayout();
        gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(2, 0, 2, 0); // component width = minwidth + (2ipadx)
        outputPanel.setLayout(gbl);

        outputPanel.setForeground(Color.black);
        outputPanel.setBorder(buildTitledBorder("Process")); // set the border ... "Parameters"
        ButtonGroup regionGroup = new ButtonGroup();

        radioEntireImage = new JRadioButton("Entire image", entireImage);
        radioEntireImage.setFont(MipavUtil.font12);
        regionGroup.add(radioEntireImage);
        gbc.gridx = 0;
        gbc.gridy = 0;
        outputPanel.add(radioEntireImage, gbc);

        radioVOIRegion = new JRadioButton("VOI regions", !entireImage);
        radioVOIRegion.setFont(MipavUtil.font12);
        regionGroup.add(radioVOIRegion);
        gbc.gridy = 1;
        outputPanel.add(radioVOIRegion, gbc);
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

        

        // verify iteration is within bounds
        tmpStr = textNIter.getText();

        if (testParameter(tmpStr, 1, 100)) {
            iters = Integer.valueOf(tmpStr).intValue();
        } else {
            textNIter.requestFocus();
            textNIter.selectAll();

            return false;
        }

        // verify Standard deviation is within bounds
        tmpStr = textDestinationMean.getText();

        if (testParameter(tmpStr, -Double.MAX_VALUE, Double.MAX_VALUE)) {
            mean = Float.valueOf(tmpStr).floatValue();
        } else {
            textDestinationMean.requestFocus();
            textDestinationMean.selectAll();

            return false;
        }
        
        entireImage = radioEntireImage.isSelected();

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
                return new String("Algorithms Inverse gradient");
            }

            public String getDescription() {
                return new String("Finds inverse gradient.");
            }

            public String getDescriptionLong() {
                return new String("Finds inverse gradient.");
            }

            public String getShortLabel() {
                return new String("Inverse gradient");
            }

            public String getLabel() {
                return new String("Inverse gradient");
            }

            public String getName() {
                return new String("Inveerse gradient");
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
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE, true));
            table.put(new ParameterInt(AlgorithmParameters.NUM_ITERATIONS, 15));
            table.put(new ParameterDouble("destination_mean", 0.0));
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
