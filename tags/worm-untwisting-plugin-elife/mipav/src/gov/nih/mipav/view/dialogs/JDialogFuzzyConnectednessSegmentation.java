package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmFuzzyConnectednessSegmentation;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.io.IOException;
import java.util.*;

import javax.swing.*;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * DOCUMENT ME!
 */
public class JDialogFuzzyConnectednessSegmentation extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------
	/** DOCUMENT ME! */
    public static final int BOTH_FUZZY_HARD = 0;

    /** DOCUMENT ME! */
    public static final int FUZZY_ONLY = 1;

    /** DOCUMENT ME! */
    public static final int HARD_ONLY = 2;

    /** Use serialVersionUID for interoperability. */
    // private static final long serialVersionUID;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    private int algo;

    // The index in the image array of the seed
    private final Vector<Integer> index_seeds = new Vector<Integer>();

    // Values from 1 to n with n <= 255 for segmentation in n labels
    private final Vector<Short> index_labels = new Vector<Short>();


    private ButtonGroup algoGroup;

    private JRadioButton hardOnlyButton;

    private JRadioButton fuzzyOnlyButton;

    private JRadioButton hardFuzzyButton;

    private JButton pointsButton;

    private JButton maskButton;

    private short pointNum = 1;

    private boolean haveOne = false;

    private boolean haveTwo = false;

    private int presentvoiIndex = 0;

    /** DOCUMENT ME! */
    private AlgorithmFuzzyConnectednessSegmentation fcsAlgo;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage[] resultImage = null; // result image

    /** DOCUMENT ME! */
    private int resultNumber;

    private ViewJComponentEditImage componentImage;

    @SuppressWarnings("rawtypes")
	private JComboBox imageComboBox;

    private String maskName;

    private ModelImage maskImage;

    private final ViewUserInterface UI = ViewUserInterface.getReference();

    private ArrayList<labelIndexItem> pointList = null;
    
    private JTextField L1DistanceText;
    
    private int L1Distance;
    
    private JTextField distanceDeclineText;
    
    private double distanceDecline;
    
    private JTextField gradientWeightText;
    
    private double gradientWeight;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogFuzzyConnectednessSegmentation() {}

    // or if the source image is to be replaced

    /**
     * Creates new dialog for entering parameters for Power watershed.
     * 
     * @param theParentFrame Parent frame
     * @param im Source image
     */
    public JDialogFuzzyConnectednessSegmentation(final Frame theParentFrame, final ModelImage im) {
        super(theParentFrame, false);
        image = im;
        componentImage = ((ViewJFrameImage) theParentFrame).getComponentImage();
        componentImage.getVOIHandler().newVOI(0.0f);
        init();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    @Override
    protected void storeParamsFromGUI() throws ParserException {
    	scriptParameters.storeInputImage(image);

        scriptParameters.getParams().put(ParameterFactory.newParameter("number_of_result_images", resultNumber));

        for (int i = 0; i < resultNumber; i++) {
            scriptParameters.storeImageInRecorder(getResultImage()[i]);
        }

        scriptParameters.getParams().put(ParameterFactory.newParameter("algorithm", algo));
        scriptParameters.getParams().put(ParameterFactory.newParameter("L1_distance", L1Distance));
        scriptParameters.getParams().put(ParameterFactory.newParameter("distance_decline", distanceDecline));
        scriptParameters.getParams().put(ParameterFactory.newParameter("gradient_weight", gradientWeight));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();
        resultNumber = scriptParameters.getParams().getInt("number_of_result_images");

        algo = scriptParameters.getParams().getInt("algorithm");
        L1Distance = scriptParameters.getParams().getInt("L1_distance");
        distanceDecline = scriptParameters.getParams().getDouble("distance_decline");
        gradientWeight = scriptParameters.getParams().getDouble("gradient_weight");
    }
    
    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage[] getResultImage() {
        return resultImage;
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    @Override
    protected void doPostAlgorithmActions() {
    	for (int i = 0; i < resultNumber; i++) {
            AlgorithmParameters.storeImageInRunner(getResultImage()[i]);
        }
    }

    /**
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     * 
     * @param event event that triggers function
     */
    @Override
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        }
        if (command.equals("Add")) {
            int i, j;
            int x, y, z;
            VOIVector voiVector;
            VOI presentVOI;
            Vector3f[] tmppt = null;
            int index;
            final int xDim = image.getExtents()[0];
            final int yDim = image.getExtents()[1];
            final int sliceSize = xDim * yDim;
            voiVector = image.getVOIs();
            if (pointList == null) {
                pointList = new ArrayList<labelIndexItem>();
            }
            for (i = presentvoiIndex; i < voiVector.size(); i++) {
                presentVOI = image.getVOIs().VOIAt(i);
                if (presentVOI.getCurveType() == VOI.POINT) {
                    tmppt = presentVOI.exportAllPoints();
                    for (j = 0; j < tmppt.length; j++) {
                        x = Math.round(tmppt[j].X);
                        y = Math.round(tmppt[j].Y);
                        z = Math.round(tmppt[j].Z);
                        index = x + y * xDim + z * sliceSize;
                        pointList.add(new labelIndexItem(pointNum, index));
                        if (pointNum == 1) {
                            haveOne = true;
                        } else if (pointNum == 2) {
                            haveTwo = true;
                        }
                    }
                }
            }
            presentvoiIndex = voiVector.size();
            if (pointNum < 255) {
                pointNum++;
                pointsButton.setText("Finished adding " + String.valueOf(pointNum) + " points");
                if (pointNum == 2) {
                    componentImage.getVOIHandler().newVOI(1.0f / 3.0f);
                } else if (pointNum == 3) {
                    componentImage.getVOIHandler().newVOI(2.0f / 3.0f);
                } else if (pointNum == 4) {
                    componentImage.getVOIHandler().newVOI(1.0f / 6.0f);
                } else if (pointNum == 5) {
                    componentImage.getVOIHandler().newVOI(1.0f / 2.0f);
                } else if (pointNum >= 6) {
                    componentImage.getVOIHandler().newVOI(5.0f / 6.0f);
                }
            } else {
                pointsButton.setText("Cannot add any more points");
                pointsButton.setEnabled(false);
            }
        } else if (command.equals("Mask")) {
            if (maskImage == null) {
                MipavUtil.displayError("No mask image has been found");
                return;
            }
            int i;
            int firstLabel = -1;
            // maskImage is same size as image
            final int xDim = maskImage.getExtents()[0];
            final int yDim = maskImage.getExtents()[1];
            int length = xDim * yDim;
            int zDim = 1;
            if (maskImage.getNDims() > 2) {
                zDim = maskImage.getExtents()[2];
                length = length * zDim;
            }
            final short buffer[] = new short[length];
            try {
                maskImage.exportData(0, length, buffer);
            } catch (final IOException e) {
                MipavUtil.displayError("IOException " + e + " on maskImage.exportData(0, length, buffer)");
                return;
            }
            
            for (i = 0; i < length; i++) {
                if (buffer[i] != 0) {
                    index_seeds.add(i);
                    index_labels.add(buffer[i]);
                    if ( !haveOne) {
                        haveOne = true;
                        firstLabel = buffer[i];
                    } else if (haveOne && ( !haveTwo) && (buffer[i] != firstLabel)) {
                        haveTwo = true;
                    }
                }
            }
            maskButton.setEnabled(false);
        } else {
            super.actionPerformed(event);
        }

    }

    /**
     * itemStateChanged.
     * 
     * @param event DOCUMENT ME!
     */
    @Override
    public void itemStateChanged(final ItemEvent event) {
        final Object source = event.getSource();

        if (source == imageComboBox) {
            maskName = (String) imageComboBox.getSelectedItem();

            if (maskName != null) {
                maskImage = UI.getRegisteredImageByName(maskName);
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
        int i;
        ViewJFrameImage[] imageFrame = new ViewJFrameImage[resultNumber];

        if (algorithm instanceof AlgorithmFuzzyConnectednessSegmentation) {
            image.clearMask();

            if ((fcsAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                for (i = 0; i < resultNumber; i++) {
                    updateFileInfo(image, resultImage[i]);
                    resultImage[i].clearMask();

                    try {
                        imageFrame[i] = new ViewJFrameImage(resultImage[i], null, new Dimension(610, 200 + (i * 20)));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        JOptionPane.showMessageDialog(null, "Out of memory: unable to open new frame", "Error",
                                                      JOptionPane.ERROR_MESSAGE);
                    }
                    
                	// Copy original source NIFTI matrices to result images
                    resultImage[i].getMatrixHolder().replaceMatrices(image.getMatrixHolder().getMatrices());
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                for (i = 0; i < resultNumber; i++) {

                    if (resultImage[i] != null) {
                        resultImage[i].disposeLocal(); // Clean up memory of result image
                        resultImage[i] = null;
                    }
                }

                resultImage = null;
                System.gc();
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
     // save the completion status for later
        setComplete(algorithm.isCompleted());

        fcsAlgo.finalize();
        fcsAlgo = null;
        dispose();
    }

   
    /**
     * 
     * @param algo
     */
    public void setAlgo(final int algo) {
        this.algo = algo;
    }

    /**
     * 
     * @param L1Distance
     */
    public void setL1Distance(final int L1Distance) {
        this.L1Distance = L1Distance;
    }
    
    public void setDistanceDecline(final double distanceDecline) {
    	this.distanceDecline = distanceDecline;
    }
    
    public void setGradientWeight(final double gradientWeight) {
    	this.gradientWeight = gradientWeight;
    }
    
    /**
     * Once all the necessary variables are set, call the Fuzzy C Means algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        int i;
        int presentNumber = 0;

        // Calculate the number of result images.
        resultNumber = 1;

        if (algo == BOTH_FUZZY_HARD) {
            resultNumber++; // segmented image
        } // if ((segmentation == HARD_ONLY) || (segmentation == BOTH_FUZZY_HARD))

        try {
        	if (resultImage == null){
        		resultImage = new ModelImage[resultNumber];
        	}
            presentNumber = 0;

            if ((algo == FUZZY_ONLY) || (algo == BOTH_FUZZY_HARD)) {

                
                    resultImage[presentNumber++] = new ModelImage(ModelStorageBase.DOUBLE, image.getExtents(),
                                                    makeImageName(image.getImageName(),
                                                    "_fuzzy"));
                
            }

            if ((algo == HARD_ONLY) || (algo == BOTH_FUZZY_HARD)) {
                resultImage[presentNumber++] = new ModelImage(ModelStorageBase.UBYTE, image.getExtents(),
                                                          makeImageName(image.getImageName(), "_seg"));
            }

            // Make algorithm
            fcsAlgo = new AlgorithmFuzzyConnectednessSegmentation(resultImage, image, algo, L1Distance, distanceDecline,
            		        gradientWeight, index_seeds, index_labels);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            fcsAlgo.addListener(this);

            createProgressBar(image.getImageName(), fcsAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (fcsAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                fcsAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            if (resultImage != null) {

                for (i = 0; i < resultNumber; i++) {

                    if (resultImage[i] != null) {
                        resultImage[i].disposeLocal(); // Clean up memory of result image
                        resultImage[i] = null;
                    }
                }

                resultImage = null;
            }

            System.gc();
            MipavUtil.displayError("Dialog Fuzzy Connectedness Segmentation: unable to allocate enough memory");

            return;
        }

    }


    
    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Fuzzy Connectedness Segmentation");

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;

        int yPos = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        final JPanel algoPanel = new JPanel(new GridBagLayout());
        algoPanel.setBorder(buildTitledBorder("Algorithms"));

        algoGroup = new ButtonGroup();
        hardOnlyButton = new JRadioButton("Hard only", false);
        hardOnlyButton.setFont(serif12);
        algoGroup.add(hardOnlyButton);
        gbc.gridy = yPos++;
        algoPanel.add(hardOnlyButton, gbc);

        fuzzyOnlyButton = new JRadioButton("Fuzzy only", false);
        fuzzyOnlyButton.setFont(serif12);
        algoGroup.add(fuzzyOnlyButton);
        gbc.gridy = yPos++;
        algoPanel.add(fuzzyOnlyButton, gbc);

        hardFuzzyButton = new JRadioButton("Hard and fuzzy both", true);
        hardFuzzyButton.setFont(serif12);
        algoGroup.add(hardFuzzyButton);
        gbc.gridy = yPos++;
        algoPanel.add(hardFuzzyButton, gbc);

        final GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.weightx = 1;

        int yPos2 = 0;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.fill = GridBagConstraints.HORIZONTAL;

        final JPanel labelsPanel = new JPanel(new GridBagLayout());
        labelsPanel.setForeground(Color.black);
        labelsPanel.setBorder(buildTitledBorder("Labels"));

        final JLabel headLabel = new JLabel("Add points or load mask image");
        headLabel.setForeground(Color.black);
        headLabel.setFont(serif12);
        gbc2.gridy = yPos2++;
        labelsPanel.add(headLabel, gbc2);

        final JLabel valuesLabel = new JLabel("Values from 1 to n");
        valuesLabel.setForeground(Color.black);
        valuesLabel.setFont(serif12);
        gbc2.gridy = yPos2++;
        labelsPanel.add(valuesLabel, gbc2);

        pointsButton = new JButton("Finished adding 1 points");
        pointsButton.setPreferredSize(MipavUtil.defaultButtonSize);
        pointsButton.setFont(serif12B);
        gbc2.gridy = yPos2++;
        labelsPanel.add(pointsButton, gbc2);
        pointsButton.addActionListener(this);
        pointsButton.setActionCommand("Add");

        final JLabel labelImage2 = new JLabel("Mask image: ");
        labelImage2.setForeground(Color.black);
        labelImage2.setFont(serif12);
        gbc2.gridy = yPos2++;
        labelsPanel.add(labelImage2, gbc2);
        imageComboBox = buildComboBox(image);
        imageComboBox.addItemListener(this);
        gbc2.gridx = 1;
        labelsPanel.add(imageComboBox, gbc2);

        maskName = (String) imageComboBox.getSelectedItem();
        if (maskName != null) {
            maskImage = UI.getRegisteredImageByName(maskName);
        }

        maskButton = new JButton("Use mask image to generate seeds");
        maskButton.setFont(serif12B);
        gbc2.gridx = 0;
        gbc2.gridy = yPos2++;
        labelsPanel.add(maskButton, gbc2);
        maskButton.addActionListener(this);
        maskButton.setActionCommand("Mask");

        JLabel L1DistanceLabel = new JLabel("L1 Distance of neighborhood ");
        L1DistanceLabel.setFont(serif12);
        L1DistanceLabel.setForeground(Color.black);
        
        L1DistanceText = new JTextField(10);
        L1DistanceText.setText("1");
        L1DistanceText.setFont(serif12);
        L1DistanceText.setForeground(Color.black);

        final JPanel mainPanel = new JPanel(new GridBagLayout());
        
        JLabel distanceDeclineLabel = new JLabel("Distance decline factor ");
        distanceDeclineLabel.setFont(serif12);
        distanceDeclineLabel.setForeground(Color.black);
        
        distanceDeclineText = new JTextField(10);
        distanceDeclineText.setText("0.1");
        distanceDeclineText.setFont(serif12);
        distanceDeclineText.setForeground(Color.black);
        
        JLabel gradientWeightLabel = new JLabel("Gradient weight ");
        gradientWeightLabel.setFont(serif12);
        gradientWeightLabel.setForeground(Color.black);
        
        gradientWeightText = new JTextField(10);
        gradientWeightText.setText("2.0");
        gradientWeightText.setFont(serif12);
        gradientWeightText.setForeground(Color.black);
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 2;
        mainPanel.add(algoPanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(labelsPanel, gbc);
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        mainPanel.add(L1DistanceLabel, gbc);
        gbc.gridx = 1;
        mainPanel.add(L1DistanceText, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        mainPanel.add(distanceDeclineLabel, gbc);
        gbc.gridx = 1;
        mainPanel.add(distanceDeclineText, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        mainPanel.add(gradientWeightLabel, gbc);
        gbc.gridx = 1;
        mainPanel.add(gradientWeightText, gbc);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        final JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        setResizable(false);
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     * 
     * @return <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        int i;
        String tmpStr;

        if (hardOnlyButton.isSelected()) {
            algo = HARD_ONLY;
        } else if (fuzzyOnlyButton.isSelected()) {
            algo = FUZZY_ONLY;
        } else if (hardFuzzyButton.isSelected()) {
            algo = BOTH_FUZZY_HARD;
        }

        tmpStr = L1DistanceText.getText();

        if (testParameter(tmpStr, 1.0, 20.0)) {
            L1Distance = Integer.valueOf(tmpStr).intValue();
        } else {
            L1DistanceText.requestFocus();
            L1DistanceText.selectAll();

            return false;
        }
        
        tmpStr = distanceDeclineText.getText();

        if (testParameter(tmpStr, 1.0E-5, 1.0E5)) {
            distanceDecline = Double.valueOf(tmpStr).doubleValue();
        } else {
            distanceDeclineText.requestFocus();
            distanceDeclineText.selectAll();

            return false;
        }
        
        tmpStr = gradientWeightText.getText();

        if (testParameter(tmpStr, 1.0E-5, 1.0E5)) {
            gradientWeight = Double.valueOf(tmpStr).doubleValue();
        } else {
            gradientWeightText.requestFocus();
            gradientWeightText.selectAll();

            return false;
        }

        if ( !haveOne) {
            MipavUtil.displayError("No 1 point is present");
            return false;
        } else if ( !haveTwo) {
            MipavUtil.displayError("No 2 point is present");
            return false;
        }

        if (pointList != null) {
            Collections.sort(pointList, new labelIndexComparator());
            for (i = 0; i < pointList.size(); i++) {
                index_labels.add(pointList.get(i).getLabel());
                index_seeds.add(pointList.get(i).getIndex());
            }
            pointList.clear();
        } // if (pointList != null)

        return true;
    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    @Override
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            @Override
            public String getCategory() {
                return new String("Algorithms.Segmentation");
            }

            @Override
            public String getDescription() {
            	 return new String("Fuzzy Connectedness Segmentation");    
            }

            @Override
            public String getDescriptionLong() {
            	return new String("Fuzzy Connectedness Segmentation");   
            }

            @Override
            public String getShortLabel() {
                return new String("FuzzyConnectednessSegmentation");
            }

            @Override
            public String getLabel() {
                return new String("Fuzzy Connectedness Segmentation");
            }

            @Override
            public String getName() {
                return new String("Fuzzy Connectedness Segmentation");
            }
        };
    }

    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
    @Override
    public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();
        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterInt("number_of_result_images", 2));
            table.put(new ParameterInt("algorithm", BOTH_FUZZY_HARD));
            table.put(new ParameterInt("L1_distance", 1));
            table.put(new ParameterDouble("distance_decline",0.1));
            table.put(new ParameterDouble("gradient_weight", 2.0));
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
    @Override
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
        if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE+"1")) {
        	return resultImage[0].getImageName();
        }
        else if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE+"2")) {
        	return resultImage[1].getImageName();
        }

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;
    }

    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    @Override
    public boolean isActionComplete() {
        return isComplete();
    }

    /**
     * Builds a list of images. Returns combobox. List must be all color or all black and white.
     * 
     * @param image DOCUMENT ME!
     * 
     * @return Newly created combo box.
     */
    @SuppressWarnings({ "rawtypes", "unchecked" })
	private JComboBox buildComboBox(final ModelImage image) {
        ViewUserInterface UI;
        ModelImage nextImage;
        boolean doAdd;
        int i;

        final JComboBox comboBox = new JComboBox();
        comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);

        UI = ViewUserInterface.getReference();

        final Enumeration<String> names = UI.getRegisteredImageNames();

        while (names.hasMoreElements()) {
            final String name = names.nextElement();

            if ( !name.equals(image.getImageName())) {
                nextImage = UI.getRegisteredImageByName(name);

                if (UI.getFrameContainingImage(nextImage) != null) {

                    if ( ( !nextImage.isColorImage()) && (nextImage.getNDims() == image.getNDims())) {
                        doAdd = true;

                        for (i = 0; i < image.getNDims(); i++) {

                            if (image.getExtents()[i] != nextImage.getExtents()[i]) {
                                doAdd = false;
                            }
                        }

                        if (doAdd) {
                            comboBox.addItem(name);
                        }
                    }
                }
            }
        }

        return comboBox;
    }

    private class labelIndexItem {

        /** DOCUMENT ME! */
        private final short label;

        /** DOCUMENT ME! */
        private final int index;

        /**
         * Creates a new labelIndexItem object.
         * 
         * @param label
         * @param index
         */
        public labelIndexItem(final short label, final int index) {
            this.label = label;
            this.index = index;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public short getLabel() {
            return label;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int getIndex() {
            return index;
        }
    }

    private class labelIndexComparator implements Comparator<labelIndexItem> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        @Override
        public int compare(final labelIndexItem o1, final labelIndexItem o2) {
            final int a = o1.getIndex();
            final int b = o2.getIndex();

            if (a < b) {
                return -1;
            } else if (a > b) {
                return 1;
            } else {
                return 0;
            }
        }

    }

}
