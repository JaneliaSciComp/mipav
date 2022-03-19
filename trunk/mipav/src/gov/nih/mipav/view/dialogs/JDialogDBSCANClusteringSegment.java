package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm.
 * The program does 2D superepixel DBSCAN clutering segmentation for color image segmentation.
 *Algorithms are executed in their own thread.
 *
 * @see  DBSCANClusteringSegment
 */
public class JDialogDBSCANClusteringSegment extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------
	// SLIC input parameters:
    private final int MEAN_CENTER = 1;
    private final int MEDIAN_CENTER = 2;
	// Number of desired superpixels. Note that this is nominal
	// the actual number of superpixels generated will generally
	// be a bit larger, especially if parameter m is small.
	private int k;
	// Weighting factor between colour and spatial
	// differences. Values from about 5 to 40 are useful.  Use a
	// large value to enforce superpixels with more regular and
	// smoother shapes. Try a value of 10 to start with.
	private double m = 10.0;
	// mRegions morphologically smaller than this are merged with
	// adjacent regions. Try a value of 1 or 1.5.  Use 0 to
	// disable.
	private double seRadius = 1.0;
	// String "mean" or "median" indicating how the cluster
	// colour centre should be computed. Defaults to "mean"
	private int center = MEAN_CENTER;
	// Optional median filtering window size.  Image compression
	// can result in noticeable artifacts in the a*b* components
	// of the image.  Median filtering can reduce this. mw can be
	// a single value in which case the same median filtering is
	// applied to each L* a* and b* components.  Alternatively it
	// can be a 2-vector where mw(1) specifies the median
	// filtering window to be applied to L* and mw(2) is the
	// median filtering window to be applied to a* and b*.
	private int mw1 = 0;
	private int mw2 = 0;
	private int nItr = 10;
	
	// SPDBSCAN input parameter:
	// Matching tolerance value/distance threshold that controls which
	// superpixels are clustered together.  This value is in L*a*b*
	// colour units.  Try a value in the range 5-10 to start with.
	// Changing the value of Ec by just 1 unit can give a significant
	// difference. 
	private double Ec = 7.5;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JLabel labelm;
    
    private JLabel labelse;

    /** DOCUMENT ME! */
    private JLabel labelk;

    /** DOCUMENT ME! */
    private DBSCANClusteringSegment dbAlgo;

    /** DOCUMENT ME! */
    private JPanel optionsPanel;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image
    
    private JTextField textm;
    
    private JTextField textse;

    /** DOCUMENT ME! */
    private JTextField textk;
    
    private ButtonGroup centerGroup;
    
    private JRadioButton meanButton;
    
    private JRadioButton medianButton;
    
    private JLabel labelmw1;
    
    private JTextField textmw1;
    
    private JLabel labelmw2;
    
    private JTextField textmw2;
    
    private JLabel labelIter;
    
    private JTextField textIter;
    
    private JLabel labelEc;
    
    private JTextField textEc;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogDBSCANClusteringSegment() { }

    /**
     * Creates a new JDialogDBSCANClusteringSegment object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogDBSCANClusteringSegment(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        setDefaults();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, sets variables and calls algorithm.
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
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof DBSCANClusteringSegment) {
            image.clearMask();

            if ((dbAlgo.isCompleted() == true) && (resultImage != null)) {

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

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

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
     // save the completion status for later
        setComplete(algorithm.isCompleted());

        dbAlgo.finalize();
        dbAlgo = null;
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
     * Once all the necessary variables are set, call the Markov Segment algorithm which will create
     * a new segmented image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_DBSCANSegment");

                try {

                    // Make result image of float BYTE
                    resultImage= new ModelImage(ModelImage.INTEGER, image.getExtents(), name);

                    /* if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM){
                     *   ((FileInfoDicom)(resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                     * }*/
                    // Make algorithm
                    dbAlgo = new DBSCANClusteringSegment(resultImage, image, k, m, seRadius, center,
                    		mw1, mw2, nItr, Ec);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    dbAlgo.addListener(this);

                    createProgressBar(image.getImageName(), dbAlgo);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (dbAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        dbAlgo.run();
                    }

                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog DBSCANClusteringSegment: unable to allocate enough memory");

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

        setDefaults();

        k = scriptParameters.getParams().getInt("k_num");
        m = scriptParameters.getParams().getDouble("m_num");
        seRadius = scriptParameters.getParams().getDouble("se");
        center = scriptParameters.getParams().getInt("cen");
        mw1 = scriptParameters.getParams().getInt("m1");
        mw2 = scriptParameters.getParams().getInt("m2");
        nItr = scriptParameters.getParams().getInt("iter");
        Ec = scriptParameters.getParams().getDouble("Ec_num");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), true);

        scriptParameters.getParams().put(ParameterFactory.newParameter("k_num", k));
        scriptParameters.getParams().put(ParameterFactory.newParameter("m_num", m));
        scriptParameters.getParams().put(ParameterFactory.newParameter("se", seRadius));
        scriptParameters.getParams().put(ParameterFactory.newParameter("cen", center));
        scriptParameters.getParams().put(ParameterFactory.newParameter("m1", mw1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("m2", mw2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("iter", nItr));
        scriptParameters.getParams().put(ParameterFactory.newParameter("Ec_num", Ec));
    }

    /**
     * Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
     */
    private void init() {

        setForeground(Color.black);

        getContentPane().setLayout(new BorderLayout());
        setTitle("SuperPixel DBSCAN clustering");

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
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        optionsPanel = new JPanel(new GridBagLayout());
        optionsPanel.setForeground(Color.black);
        optionsPanel.setBorder(buildTitledBorder("Parameters"));
        getContentPane().add(optionsPanel);

        labelk = createLabel("Number of desired superpixels");

        optionsPanel.add(labelk, gbc);

        gbc.gridx = 1;
        gbc.gridy = 0;
        textk = new JTextField();
        textk.setText("1000");
        textk.setFont(serif12);
        optionsPanel.add(textk, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        labelm = createLabel("Color and spatial differences weighting");
        optionsPanel.add(labelm, gbc);

        gbc.gridx = 1;
        gbc.gridy = 1;
        textm = new JTextField();
        textm.setText("10.0");
        textm.setFont(serif12);
        optionsPanel.add(textm, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        labelse = createLabel("Structuring element radius (>= 1.0)");
        optionsPanel.add(labelse, gbc);

        gbc.gridx = 1;
        gbc.gridy = 2;
        textse = new JTextField();
        textse.setText("1.0");
        textse.setFont(serif12);
        optionsPanel.add(textse, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 3;
        centerGroup = new ButtonGroup();
        meanButton = new JRadioButton("Mean center", true);
        meanButton.setFont(serif12);
        meanButton.setForeground(Color.black);
        centerGroup.add(meanButton);
        optionsPanel.add(meanButton, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 4;
        medianButton = new JRadioButton("Median center", false);
        medianButton.setFont(serif12);
        medianButton.setForeground(Color.black);
        centerGroup.add(medianButton);
        optionsPanel.add(medianButton, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 5;
        labelmw1 = createLabel("L median filter size");
        optionsPanel.add(labelmw1, gbc);
        
        gbc.gridx = 1;
        gbc.gridy = 5;
        textmw1 = new JTextField();
        textmw1.setText("0");
        textmw1.setFont(serif12);
        optionsPanel.add(textmw1, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 6;
        labelmw2 = createLabel("a and b median filter size");
        optionsPanel.add(labelmw2, gbc);
        
        gbc.gridx = 1;
        gbc.gridy = 6;
        textmw2 = new JTextField();
        textmw2.setText("0");
        textmw2.setFont(serif12);
        optionsPanel.add(textmw2, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 7;
        labelIter = createLabel("Number of iterations");
        optionsPanel.add(labelIter, gbc);
        
        gbc.gridx = 1;
        gbc.gridy = 7;
        textIter = new JTextField();
        textIter.setText("10");
        textIter.setFont(serif12);
        optionsPanel.add(textIter, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 8;
        labelEc = createLabel("Tolerance value/distance threshold");
        optionsPanel.add(labelEc, gbc);
        
        gbc.gridx = 1;
        gbc.gridy = 8;
        textEc = new JTextField();
        textEc.setText("7.5");
        textEc.setFont(serif12);
        optionsPanel.add(textEc, gbc);

        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add(optionsPanel, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);
        setVisible(true);

        System.gc();
    }

    /**
     * Set the default values for the parameters.
     */
    private void setDefaults() {

        k = 1000;
    	m = 10.0;
        seRadius = 1.0;
        center = MEAN_CENTER;
        mw1 = 0;
        mw2 = 0;
        nItr = 10;
        Ec = 7.5;
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        System.gc();

        tmpStr = textk.getText();

        if (testParameter(tmpStr, 10, 10000)) {
            k = Integer.valueOf(tmpStr).intValue();
        } else {
            textk.requestFocus();
            textk.selectAll();

            return false;
        }

        tmpStr = textm.getText();

        if (testParameter(tmpStr, 5.0, 40.0)) {
            m = Double.valueOf(tmpStr).doubleValue();
        } else {
            textm.requestFocus();
            textm.selectAll();

            return false;
        }
        
        tmpStr = textse.getText();

        if (testParameter(tmpStr, 1.0, 5.0)) {
            seRadius = Double.valueOf(tmpStr).doubleValue();
        } else {
            textse.requestFocus();
            textse.selectAll();

            return false;
        }
        
        if (meanButton.isSelected()) {
        	center = MEAN_CENTER;
        }
        else {
        	center = MEDIAN_CENTER;
        }
        
        tmpStr = textmw1.getText();

        if (testParameter(tmpStr, 0, 21)) {
            mw1 = Integer.valueOf(tmpStr).intValue();
        } else {
            textmw1.requestFocus();
            textmw1.selectAll();

            return false;
        }
        
        tmpStr = textmw2.getText();

        if (testParameter(tmpStr, 0, 21)) {
            mw2 = Integer.valueOf(tmpStr).intValue();
        } else {
            textmw2.requestFocus();
            textmw2.selectAll();

            return false;
        }
        
        tmpStr = textIter.getText();

        if (testParameter(tmpStr, 1, 100)) {
            nItr = Integer.valueOf(tmpStr).intValue();
        } else {
            textIter.requestFocus();
            textIter.selectAll();

            return false;
        }
        
        tmpStr = textEc.getText();

        if (testParameter(tmpStr, 1.0, 50.0)) {
            Ec = Double.valueOf(tmpStr).doubleValue();
        } else {
            textEc.requestFocus();
            textEc.selectAll();

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
                return new String("Algorithms");
            }

            public String getDescription() {
                return new String("Superpixel DBSCAN clusteing for image segmentation");
            }

            public String getDescriptionLong() {
                return new String("Superpixel DBSCAN clustering for image segmentation");
            }

            public String getShortLabel() {
                return new String("DSCAN Segment");
            }

            public String getLabel() {
                return new String("Superpixel DBSCAN clustering for image segmentation");
            }

            public String getName() {
                return new String("DBSCAN Segment");
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
            table.put(new ParameterInt("k_num", 1000));
            table.put(new ParameterDouble("m_num", 10.0));
            table.put(new ParameterDouble("se",1.0));
            table.put(new ParameterInt("cen", MEAN_CENTER));
            table.put(new ParameterInt("m1", 0));
            table.put(new ParameterInt("m2", 0));
            table.put(new ParameterInt("iter", 10));
            table.put(new ParameterDouble("Ec_num", 7.5));
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
