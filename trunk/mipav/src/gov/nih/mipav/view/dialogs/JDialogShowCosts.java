package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterBoolean;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.scripting.parameters.ParameterInt;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Calculate costs for various voxel similarity cost functions that are used in registration and output them to the data
 * window. Algorithm is executed in its own thread.
 */
public class JDialogShowCosts extends JDialogScriptableBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7737847738523057492L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Is either a AlgorithmCostFunction or a AlgorithmCostFunctions2D depending on images */
    private AlgorithmOptimizeFunctionBase algoCost = null;

    /** Number of bins for each image */
    private int bin1;

    /** User input of bins */
    private JTextField bin1Text;

    /** The current cost function */
    private String currentCostFunct;

    /** Active image when algorithm is called. */
    private ModelImage firstImage;

    /** Image list for user to pick registered image */
    private JComboBox imageComboBox;

    /** The registered image */
    private JLabel labelImage;
    
    /** Whether the available cost functions should be performed */
    private JCheckBox doCorrelationRatioSmoothed, doMutualInfoSmoothed,
    					doNormMutualInfoSmoothed, doNormXCorr;
    
    private boolean correlationRatioSmoothed = true;
    
    private boolean mutualInfoSmoothed = true;
    
    private boolean normMutualInfoSmoothed = true;
    
    private boolean normXCorr = true;

    /** Initial guesses for bin values */
    private double possibleIntValues1;

    /** The registered image as specified by user through gui */
    private ModelImage secondImage = null;

    /** Holds extents, other info about firstImage and SecondImage */
    private ModelSimpleImage simpleImg1, simpleImg2;

    /** Optional smooth parameter for both cost functions */
    private float smoothSize = 1;

    /** Identity matrix for testing cost */
    private TransMatrix tMatrix;

    /** The MIPAV user interface */
    private ViewUserInterface UI;

    //~ Constructors ---------------------------------------------------------------------------------------------------
    
    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogShowCosts() { }


    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogShowCosts(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        firstImage = im;
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
                dispose();
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        }
    }
    
    
    
    /**
     * Calls the algorithm with the set-up parameters.
     */
    protected void callAlgorithm() {
    	ViewJProgressBar progressBar;
    	progressBar = new ViewJProgressBar("Calculating costs", " ", 0, 100, false, this, this);

        int xScreen = Toolkit.getDefaultToolkit().getScreenSize().width;
        int yScreen = 100; // Toolkit.getDefaultToolkit().getScreenSize().height;
        progressBar.setLocation(xScreen / 2, yScreen / 2);
        progressBar.setVisible(true);

        if(correlationRatioSmoothed) {
        	progressBar.setMessage("Calculating correlation ratio");
            currentCostFunct = "Correlation Ratio Smoothed";
            if (firstImage.getNDims() > 2) {
                callAlgorithm(AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED);
            } else {
                callAlgorithm(AlgorithmCostFunctions2D.CORRELATION_RATIO_SMOOTHED);
            }
        }

        progressBar.updateValueImmed(25);

        if(mutualInfoSmoothed) {
        	progressBar.setMessage("Calculating mutual information");
        	currentCostFunct = "Mutual Information Smoothed";
            if (firstImage.getNDims() > 2) {
                callAlgorithm(AlgorithmCostFunctions.MUTUAL_INFORMATION_SMOOTHED);
            } else {
                callAlgorithm(AlgorithmCostFunctions2D.MUTUAL_INFORMATION_SMOOTHED);
            }
        }

        progressBar.updateValueImmed(50);

        if(normMutualInfoSmoothed) {
        	progressBar.setMessage("Calculating normalized mutual information");
        	currentCostFunct = "Normalized Mutual Information Smoothed";
            if (firstImage.getNDims() > 2) {
                callAlgorithm(AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED);
            } else {
                callAlgorithm(AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED);
            }
        }

        progressBar.setMessage("Calculating normalized cross correlation");
        progressBar.updateValueImmed(75);
        currentCostFunct = "Normalized Cross Correlation Smoothed";

        if(normXCorr) {
            if (firstImage.getNDims() > 2) {
                callAlgorithm(AlgorithmCostFunctions.NORMALIZED_XCORRELATION_SMOOTHED);
            } else {
                callAlgorithm(AlgorithmCostFunctions2D.NORMALIZED_XCORRELATION_SMOOTHED);
            }
        }
        insertScriptLine();

        progressBar.updateValue(100, true);
        if (!isScriptRunning()) {
            MipavUtil.displayInfo("Calculations are complete.  " +
                                  "Select the data tab in the output window to see results.");
        }
        progressBar.dispose();	
    }
    
    


    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * itemStateChanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (source == imageComboBox) {
            UI = ViewUserInterface.getReference();

            String selectedName = (String) imageComboBox.getSelectedItem();
            secondImage = UI.getRegisteredImageByName(selectedName);

        } // if ( source == imageComboBox)
       
    }

    /**
     * Accessor that sets bin1.
     *
     * @param  bin1  DOCUMENT ME!
     */
    public void setBin1(int bin1) {
        this.bin1 = bin1;
    }
   
    /**
     * Accessor to set the secondImage.
     *
     * @param  secondImage  DOCUMENT ME!
     */
    public void setSecondImage(ModelImage secondImage) {
        this.secondImage = secondImage;
    }

    /**
     * Builds a list of images. Returns combobox. List must be all color or all black and white.
     *
     * @param   image  DOCUMENT ME!
     *
     * @return  Newly created combo box.
     */
    private JComboBox buildComboBox(ModelImage image) {
        ViewUserInterface UI;
        ModelImage nextImage;
        boolean doAdd;
        int i;

        JComboBox comboBox = new JComboBox();
        comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);

        UI = ViewUserInterface.getReference();

        Enumeration<String> names = UI.getRegisteredImageNames();

        while (names.hasMoreElements()) {
            String name = names.nextElement();

            if (!name.equals(image.getImageName())) {
                nextImage = UI.getRegisteredImageByName(name);

                if (UI.getFrameContainingImage(nextImage) != null) {

                    if (image.getNDims() == nextImage.getNDims()) {
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

    /**
     * DOCUMENT ME!
     *
     * @param  costChoice  DOCUMENT ME!
     */
    private void callAlgorithm(int costChoice) {

        try {

            // Make algorithm
            if (firstImage.getNDims() > 2) {
                algoCost = new AlgorithmCostFunctions(simpleImg1, simpleImg2, costChoice, bin1, smoothSize);
            } else {
                algoCost = new AlgorithmCostFunctions2D(simpleImg1, simpleImg2, costChoice, bin1, smoothSize);
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog Show Costs: unable to allocate enough memory");

            return;
        }

        double cost;

        cost = algoCost.cost(tMatrix);
        
        UI.setDataText(currentCostFunct + ":\t" + cost + "\n");

        if(algoCost != null) {
        	if(algoCost instanceof AlgorithmCostFunctions) {
        		((AlgorithmCostFunctions) algoCost).disposeLocal();
        	} else if(algoCost instanceof AlgorithmCostFunctions2D) {
        		((AlgorithmCostFunctions2D) algoCost).disposeLocal();
        	}
        	algoCost = null;
        }
    }

    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        JPanel imagePanel;

        setForeground(Color.black);
        setTitle("Show pixel similarity cost function values");

        if (firstImage.getNDims() > 2) {
            tMatrix = new TransMatrix(4);
        } else {
            tMatrix = new TransMatrix(3);

        }

        /* Set up image panel */
        String matchName = firstImage.getImageName();
        labelImage = new JLabel("Pixel similarity costs between [" + matchName + "] and:");
        labelImage.setForeground(Color.black);
        labelImage.setFont(serif12);

        imageComboBox = buildComboBox(firstImage);
        imageComboBox.addItemListener(this);
        UI = ViewUserInterface.getReference();

        String selectedName = (String) imageComboBox.getSelectedItem();

        if (selectedName == null) {
            MipavUtil.displayError("Comparison image must have the same dimensions and extents " +
                                   "as the active image.  No comparison image found.");

            return;
        }

        secondImage = UI.getRegisteredImageByName(selectedName);

        imagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        imagePanel.add(labelImage);
        imagePanel.add(imageComboBox);
        
        /**set up available costs panel*/
        JPanel availableCostsPanel = new JPanel();
        availableCostsPanel.setLayout(new BoxLayout(availableCostsPanel, BoxLayout.Y_AXIS));
        availableCostsPanel.setBorder(MipavUtil.buildTitledBorder("Available cost functions"));
        
        doCorrelationRatioSmoothed = new JCheckBox("Calculate correlation ratio");
        doCorrelationRatioSmoothed.setSelected(true);
        doCorrelationRatioSmoothed.setFont(serif12);
        doMutualInfoSmoothed = new JCheckBox("Calculate mutual information");
        doMutualInfoSmoothed.setSelected(true);
        doMutualInfoSmoothed.setFont(serif12);
        doNormMutualInfoSmoothed = new JCheckBox("Calculate normalized mutual information");
        doNormMutualInfoSmoothed.setSelected(true);
        doNormMutualInfoSmoothed.setFont(serif12);
        doNormXCorr = new JCheckBox("Calculate normalized cross correlation");
        doNormXCorr.setSelected(true);
        doNormXCorr.setFont(serif12);
        
        availableCostsPanel.add(doCorrelationRatioSmoothed);
        availableCostsPanel.add(doMutualInfoSmoothed);
        availableCostsPanel.add(doNormMutualInfoSmoothed);
        availableCostsPanel.add(doNormXCorr);

        /* Set up the grid bag */
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;

        /* Set up rescale panel */
        JPanel rescalePanel = new JPanel(new GridBagLayout());
        rescalePanel.setForeground(Color.black);
        rescalePanel.setBorder(buildTitledBorder("Bin panel"));

        gbc.gridwidth = 1;
        gbc.gridx = 0;
        gbc.gridy = 0;

        /* Initialize the number of bins */
        bin1 = 256;
        possibleIntValues1 = firstImage.getMax() - firstImage.getMin() + 1;

        if (((firstImage.getType() == ModelStorageBase.BYTE) || (firstImage.getType() == ModelStorageBase.UBYTE) ||
                 (firstImage.getType() == ModelStorageBase.SHORT) ||
                 (firstImage.getType() == ModelStorageBase.USHORT) ||
                 (firstImage.getType() == ModelStorageBase.INTEGER) ||
                 (firstImage.getType() == ModelStorageBase.UINTEGER) ||
                 (firstImage.getType() == ModelStorageBase.LONG)) && (possibleIntValues1 < 256)) {
            bin1 = (int) Math.round(possibleIntValues1);
        }

        /* Set up interface for changing number of bins - bin labels and text */
        JLabel bin1Label = new JLabel("Image 1 bin number ");
        bin1Label.setForeground(Color.black);
        bin1Label.setFont(serif12);
        rescalePanel.add(bin1Label, gbc);

        gbc.gridx = 1;
        gbc.gridy = 0;
        bin1Text = new JTextField();
        bin1Text.setText(String.valueOf(bin1));
        bin1Text.setFont(serif12);
        rescalePanel.add(bin1Text, gbc);

        /* Set up button panel */
        buildOKButton();
        buildCancelButton();

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);
        
        gbc = new GridBagConstraints();
        getContentPane().setLayout(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.WEST;
        
        getContentPane().add(imagePanel, gbc);
        gbc.gridy = 1;
        getContentPane().add(availableCostsPanel, gbc);
        gbc.gridy = 2;
        getContentPane().add(rescalePanel, gbc);
        gbc.gridy = 3;
        getContentPane().add(buttonPanel, gbc);
        
        pack();
        validate();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        /* Images */
        UI = ViewUserInterface.getReference();

        String selectedName = (String) imageComboBox.getSelectedItem();
        secondImage = UI.getRegisteredImageByName(selectedName);

        if (secondImage == null) {
            return false;
        }

        simpleImg1 = new ModelSimpleImage(firstImage.getExtents(), firstImage.getFileInfo(0).getResolutions(),
                                          firstImage);

        simpleImg2 = new ModelSimpleImage(secondImage.getExtents(), secondImage.getFileInfo(0).getResolutions(),
                                          secondImage);

        /* Bins */
        String tmpStr;
        tmpStr = bin1Text.getText();
        bin1 = Integer.parseInt(tmpStr);

        if (bin1 < 1) {
            MipavUtil.displayError("Image 1 must have at least 1 bin");
            bin1Text.requestFocus();
            bin1Text.selectAll();

            return false;
        } else if ((bin1 > Math.round(possibleIntValues1)) &&
                       ((firstImage.getType() == ModelStorageBase.BYTE) ||
                            (firstImage.getType() == ModelStorageBase.UBYTE) ||
                            (firstImage.getType() == ModelStorageBase.SHORT) ||
                            (firstImage.getType() == ModelStorageBase.USHORT) ||
                            (firstImage.getType() == ModelStorageBase.INTEGER) ||
                            (firstImage.getType() == ModelStorageBase.UINTEGER) ||
                            (firstImage.getType() == ModelStorageBase.LONG))) {
            MipavUtil.displayError("Image 1 must not have more than " + Math.round(possibleIntValues1) + " bins");
            bin1Text.requestFocus();
            bin1Text.selectAll();

            return false;
        }
        
        correlationRatioSmoothed = doCorrelationRatioSmoothed.isSelected();
        mutualInfoSmoothed = doMutualInfoSmoothed.isSelected();
        normMutualInfoSmoothed = doNormMutualInfoSmoothed.isSelected();
        normXCorr = doNormXCorr.isSelected();

        return true;
    }
    
    public void setCorrelationRatioSmoothed(boolean correlationRatioSmoothed) {
    	this.correlationRatioSmoothed = correlationRatioSmoothed;
    }
    
    public void setMutualInfoSmoothed(boolean mutualInfoSmoothed) {
    	this.mutualInfoSmoothed = mutualInfoSmoothed;
    }
    public void setNormMutualInfoSmoothed(boolean normMutualInfoSmoothed) {
    	this.normMutualInfoSmoothed = normMutualInfoSmoothed;
    }
    
    public void setNormXCorr(boolean normXCorr) {
    	this.normXCorr = normXCorr;
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
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(2)));
            
            table.put(new ParameterBoolean("correlation_ratio_smoohted", true));
            table.put(new ParameterBoolean("mutual_info_smoothed", true));
            table.put(new ParameterBoolean("norm_mutual_info_smoothed", true));
            table.put(new ParameterBoolean("norm_x_corr", true));
            table.put(new ParameterInt("bin_1",256));

        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }
    
    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        firstImage = scriptParameters.retrieveInputImage();
        setSecondImage(scriptParameters.retrieveInputImage(2));
        simpleImg1 = new ModelSimpleImage(firstImage.getExtents(), firstImage.getFileInfo(0).getResolutions(),
                firstImage);

        simpleImg2 = new ModelSimpleImage(secondImage.getExtents(), secondImage.getFileInfo(0).getResolutions(),
                secondImage);
        if (firstImage.getNDims() > 2) {
            tMatrix = new TransMatrix(4);
        } else {
            tMatrix = new TransMatrix(3);

        }
        UI = ViewUserInterface.getReference();
        
        setCorrelationRatioSmoothed(scriptParameters.getParams().getBoolean("correlation_ratio_smoothed"));
        setMutualInfoSmoothed(scriptParameters.getParams().getBoolean("mututal_info_smoothed"));
        setNormMutualInfoSmoothed(scriptParameters.getParams().getBoolean("norm_mutual_info_smoothed"));
        setNormXCorr(scriptParameters.getParams().getBoolean("norm_x_corr"));
        setBin1(scriptParameters.getParams().getInt("bin_1"));
    }
    
    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(firstImage);
        scriptParameters.storeInputImage(secondImage);
    
        scriptParameters.getParams().put(ParameterFactory.newParameter("correlation_ratio_smoothed", 
        		correlationRatioSmoothed));
        scriptParameters.getParams().put(ParameterFactory.newParameter("mututal_info_smoothed", 
        		mutualInfoSmoothed));
        scriptParameters.getParams().put(ParameterFactory.newParameter("norm_mutual_info_smoothed", 
        		normMutualInfoSmoothed));
        scriptParameters.getParams().put(ParameterFactory.newParameter("norm_x_corr", normXCorr));
        scriptParameters.getParams().put(ParameterFactory.newParameter("bin_1", bin1));
    }
}
