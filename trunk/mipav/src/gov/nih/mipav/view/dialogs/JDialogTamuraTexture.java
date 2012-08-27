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
 * Dialog to get user input, then call the algorithm.
 *
 * @version  0.1 July 6, 2012
 * @author   William Gandler
 * @see      AlgorithmTamuraTexture
 */
public class JDialogTamuraTexture extends JDialogScriptableBase
        implements AlgorithmInterface, DialogDefaultsInterface, ActionDiscovery, ScriptableActionInterface
     {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;
    
    /** Red channel. */
    private static final int RED_OFFSET = 1;

    /** Green channel. */
    private static final int GREEN_OFFSET = 2;

    /** Blue channel. */
    private static final int BLUE_OFFSET = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    /** DOCUMENT ME! */
    private JPanel colorPanel;
    
    /** DOCUMENT ME! */
    private ButtonGroup colorGroup;
    
    /** DOCUMENT ME! */
    private JRadioButton redButton;
    
    /** DOCUMENT ME! */
    private JRadioButton greenButton;
    
    /** DOCUMENT ME! */
    private JRadioButton blueButton;
    
    /** DOCUMENT ME! */
    private int RGBOffset = RED_OFFSET;
    
    /** coarsenessThreshold must be <= 1.0.
    If coarsenessThreshold < 1.0, select the largest k for which Ek >= coarsenessThreshold * Emax.
    If coarsenessThreshold = 1.0, select the largest k which gives Emax.
    */
    private double coarsenessThreshold = 1.0;
    
    private JTextField textCoarsenessThreshold;
    
    private JLabel labelCoarsenessThreshold;
    
    private boolean doCoarseness;
    
    private JCheckBox coarsenessCheckBox;
    
    private boolean doContrast;
    
    private JCheckBox contrastCheckBox;
    
    // Contrast neighborhood size for individual pixel calculations, must be an odd integer
    private int cSize = 13;
    
    private JTextField textCSize;
    
    private JLabel labelCSize;
    
    private boolean doDirectionality;
    
    private JCheckBox directionalityCheckBox;
    
    private int histogramBins = 16;
    
    private JTextField textHistogramBins;
    
    private JLabel labelHistogramBins;
    
    // Threshold for gradient magnitude for gradient angle to be included in histogram.
    private double histogramThreshold = 12.0;
    
    private JTextField textHistogramThreshold;
    
    private JLabel labelHistogramThreshold;

    /** DOCUMENT ME! */
    private ModelImage image; // source image
   
    /** DOCUMENT ME! */
    private int numOperators = 0;

    /** DOCUMENT ME! */
    private ModelImage[] resultImage = null; // result image

    /** DOCUMENT ME! */
    private AlgorithmTamuraTexture textureAlgo;

    

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogTamuraTexture() { }


    /**
     * Creates a new JDialogTamuraTexture object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogTamuraTexture(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
        loadDefaults();
        setVisible(true);
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
        } else if (source == coarsenessCheckBox) {
            if (coarsenessCheckBox.isSelected()) {
                labelCoarsenessThreshold.setEnabled(true);
                textCoarsenessThreshold.setEnabled(true);
            }
            else {
                labelCoarsenessThreshold.setEnabled(false);
                textCoarsenessThreshold.setEnabled(false);    
            }
        } else if (source == contrastCheckBox) {
            if (contrastCheckBox.isSelected()) {
                labelCSize.setEnabled(true);
                textCSize.setEnabled(true);
            }
            else {
                labelCSize.setEnabled(false);
                textCSize.setEnabled(false);    
            }
        } else if (source == directionalityCheckBox) {
            if (directionalityCheckBox.isSelected()) {
                labelHistogramBins.setEnabled(true);
                textHistogramBins.setEnabled(true);
                labelHistogramThreshold.setEnabled(true);
                textHistogramThreshold.setEnabled(true);
            }
            else {
                labelHistogramBins.setEnabled(false);
                textHistogramBins.setEnabled(false);
                labelHistogramThreshold.setEnabled(false);
                textHistogramThreshold.setEnabled(false);    
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
        int i;
        ViewJFrameImage[] imageFrame = new ViewJFrameImage[numOperators];

        if (algorithm instanceof AlgorithmTamuraTexture) {
            image.clearMask();

            if ((textureAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                // Take resultImage out of array form or null pointer errors can
                // result in one of the resultImages after another of the resultImages
                // has been deleted.
            	
            	
            	// save the completion status for later
            	setComplete(textureAlgo.isCompleted());

                for (i = 0; i < numOperators; i++) {
                    updateFileInfo(image, resultImage[i]);
                    resultImage[i].clearMask();
                    
                    try {
                        imageFrame[i] = new ViewJFrameImage(resultImage[i], null, new Dimension(610, 200));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        JOptionPane.showMessageDialog(null, "Out of memory: unable to open new resultImage frame",
                                                      "Error", JOptionPane.ERROR_MESSAGE);
                    }

                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                for (i = 0; i < numOperators; i++) {

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
        if (image.isColorImage()) {
            str += RGBOffset + delim;
        }
        str += doCoarseness + delim;
        str += coarsenessThreshold + delim;
        str += doContrast + delim;
        str += cSize + delim;
        str += doDirectionality + delim;
        str += histogramBins + delim;
        str += histogramThreshold;

        return str;
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
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if (defaultsString != null) {

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");
                coarsenessCheckBox.setSelected(MipavUtil.getBoolean(st));
                textCoarsenessThreshold.setText("" + MipavUtil.getDouble(st));
                contrastCheckBox.setSelected(MipavUtil.getBoolean(st));
                textCSize.setText("" + MipavUtil.getInt(st));
                directionalityCheckBox.setSelected(MipavUtil.getBoolean(st));
                textHistogramBins.setText("" + MipavUtil.getInt(st));
                textHistogramThreshold.setText("" + MipavUtil.getDouble(st));
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
        String defaultsString = new String(getParameterString(","));
        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }
    
   
    /**
     * Accessor that sets if coarseness is calculated
     * @param doCoarseness
     */
    public void setDoCoarseness(boolean doCoarseness) {
        this.doCoarseness = doCoarseness;
    }
    
    /**
     * Accessor that sets coarseness threshold
     * @param coarsenessThreshold
     */
    public void setCoarsenessThreshold(double coarsenessThreshold) {
        this.coarsenessThreshold = coarsenessThreshold;
    }
    
    /**
     * Accessor that sets if contrast is calculated
     * @param doContrast
     */
    public void setDoContrast(boolean doContrast) {
        this.doContrast = doContrast;
    }
    
    /**
     * Accessor that sets size of neighborhood window for pixel contrast calculations
     * @param cSize
     */
    public void setCSize(int cSize) {
        this.cSize = cSize;
    }
    
    /**
     * Accessor that sets if directionality is calculated
     * @param doDirectionality
     */
    public void setDoDirectionality(boolean doDirectionality) {
        this.doDirectionality = doDirectionality;
    }
    
    /**
     * Accessor that sets number of histogram bins used in slice directionality calculation
     * @param histogramBins
     */
    public void setHistogramBins(int histogramBins) {
        this.histogramBins = histogramBins;
    }
    
    /**
     * Accessor that sets the gradient magnitude threshold reuqired for putting a gradient direction 
     *                    into a histogram bin in the slice directionality calculation
     * @param histogramThreshold
     */
    public void setHistogramThreshold(double histogramThreshold) {
        this.histogramThreshold = histogramThreshold;
    }

    /**
     * Accessor that sets the RGBOffset.
     *
     * @param  RGBoffset  DOCUMENT ME!
     */
    public void setRGBOffset(int RGBoffset) {
        this.RGBOffset = RGBoffset;
    }

    /**
     * Once all the necessary variables are set, call the Tamura Texture algorithm.
     */
    protected void callAlgorithm() {
        int i, index;
        String[] name = null;

        try {
            resultImage = new ModelImage[numOperators];
            name = new String[numOperators];
            
            index = 0;
            if (doCoarseness) {
                name[index] = makeImageName(image.getImageName(), "_coarseness");
                resultImage[index] = new ModelImage(ModelStorageBase.BYTE, image.getExtents(), name[index]); 
                index++;
            }
            if (doContrast) {
                name[index] = makeImageName(image.getImageName(), "_contrast");
                resultImage[index] = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), name[index]); 
                index++;    
            }
            if (doDirectionality) {
                name[index] = makeImageName(image.getImageName(), "_directionality");
                resultImage[index] = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), name[index]); 
                index++;        
            }
            
            

            if (image.isColorImage()) {
                textureAlgo = new AlgorithmTamuraTexture(resultImage, image, RGBOffset, doCoarseness, coarsenessThreshold,
                                                         doContrast, cSize, doDirectionality, histogramBins, histogramThreshold);    
            }
            else {
                textureAlgo = new AlgorithmTamuraTexture(resultImage, image, doCoarseness, coarsenessThreshold,
                        doContrast, cSize, doDirectionality, histogramBins, histogramThreshold);
            }

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            textureAlgo.addListener(this);
            createProgressBar(image.getImageName(), textureAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (textureAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                textureAlgo.run();
            }
            

        } catch (OutOfMemoryError x) {

            if (resultImage != null) {

                for (i = 0; i < numOperators; i++) {

                    if (resultImage[i] != null) {
                        resultImage[i].disposeLocal(); // Clean up memory of result image
                        resultImage[i] = null;
                    }
                }

                resultImage = null;
            }
            
         // save the completion status for later
            setComplete(textureAlgo.isCompleted());

            System.gc();
            MipavUtil.displayError("Dialog Tamura Texture: unable to allocate enough memory");

            return;
        }

    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        for (int i = 0; i < numOperators; i++) {
            AlgorithmParameters.storeImageInRunner(getResultImage()[i]);
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();

        if (image.isColorImage() && scriptParameters.getParams().containsParameter("RGB_offset")) {
            RGBOffset = scriptParameters.getParams().getInt("RGB_offset");
        } else if (image.isColorImage()) {
            throw new ParameterException("RGB_offset",
                                         "This parameter (RGB_offset) is required for the processing of color images.  Please re-record this script using a color image.");
        }
        setDoCoarseness(scriptParameters.getParams().getBoolean("do_coarseness"));
        setCoarsenessThreshold(scriptParameters.getParams().getDouble("coarseness_threshold"));
        setDoContrast(scriptParameters.getParams().getBoolean("do_contrast"));
        setCSize(scriptParameters.getParams().getInt("c_size"));
        setDoDirectionality(scriptParameters.getParams().getBoolean("do_directionality"));
        setHistogramBins(scriptParameters.getParams().getInt("histogram_bins"));
        setHistogramThreshold(scriptParameters.getParams().getDouble("histogram_threshold"));

        if ((cSize % 2) == 0) {
            throw new ParameterException("c_size", "Contrast neighborhoold size must not be even");
        }

        if (cSize > (image.getExtents()[0])) {
            throw new ParameterException("c_size",
                                         "Contrast neighborhood size must not excced image width of " + image.getExtents()[0]);
        }

        if (cSize > image.getExtents()[1]) {
            throw new ParameterException("c_size",
                                         "Contrast neighborhood size must not excced image height of " + image.getExtents()[1]);
        }

        numOperators = getNumOperators();
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        for (int i = 0; i < numOperators; i++) {
            scriptParameters.storeImageInRecorder(getResultImage()[i]);
        }

        if (image.isColorImage()) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("RGB_offset", RGBOffset));
        }
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_coarseness", doCoarseness));
        scriptParameters.getParams().put(ParameterFactory.newParameter("coarseness_threshold", coarsenessThreshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_contrast", doContrast));
        scriptParameters.getParams().put(ParameterFactory.newParameter("c_size", cSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_directionality", doDirectionality));
        scriptParameters.getParams().put(ParameterFactory.newParameter("histogram_bins", histogramBins));
        scriptParameters.getParams().put(ParameterFactory.newParameter("histogram_threshold", histogramThreshold));
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int getNumOperators() {
        int numOps = 0;

        if (doCoarseness) {
            numOps++;
        }

        if (doContrast) {
            numOps++;
        }

        if (doDirectionality) {
            numOps++;
        }

        return numOps;
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        int ypos = 0;
        setForeground(Color.black);

        setTitle("Tamura Texture");
        getContentPane().setLayout(new BorderLayout());

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
        gbc.fill = GridBagConstraints.BOTH;
        
        if (image.isColorImage()) {
            colorPanel = new JPanel(new GridLayout(3, 1));
            colorPanel.setForeground(Color.black);

            colorGroup = new ButtonGroup();
            redButton = new JRadioButton("Red", true);
            redButton.setFont(serif12);
            redButton.setForeground(Color.black);
            redButton.addActionListener(this);
            colorGroup.add(redButton);
            colorPanel.add(redButton);

            greenButton = new JRadioButton("Green", false);
            greenButton.setFont(serif12);
            greenButton.setForeground(Color.black);
            greenButton.addActionListener(this);
            colorGroup.add(greenButton);
            colorPanel.add(greenButton);

            blueButton = new JRadioButton("Blue", false);
            blueButton.setFont(serif12);
            blueButton.setForeground(Color.black);
            blueButton.addActionListener(this);
            colorGroup.add(blueButton);
            colorPanel.add(blueButton);
            gbc.gridx = 0;
            gbc.gridy = ypos++;
            gbc.weighty = .1;
            JScrollPane colorScroll = new JScrollPane(colorPanel);
            colorScroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
            colorScroll.setBorder(buildTitledBorder("Colors"));
            mainPanel.add(colorScroll, gbc);   
        } // if (image.isColorImage())

        JPanel operatorPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc3 = new GridBagConstraints();
        gbc3.gridwidth = 1;
        gbc3.gridheight = 1;
        gbc3.anchor = GridBagConstraints.WEST;
        gbc3.weightx = 1;
        gbc3.insets = new Insets(2, 3, 2, 3);
        gbc3.fill = GridBagConstraints.HORIZONTAL;

        coarsenessCheckBox = new JCheckBox("Coarseness");
        coarsenessCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 0;
        operatorPanel.add(coarsenessCheckBox, gbc3);
        coarsenessCheckBox.setSelected(true);
        coarsenessCheckBox.addActionListener(this);
        
        labelCoarsenessThreshold = new JLabel("Coarseness threshold (0.8-1.0)");
        labelCoarsenessThreshold.setFont(serif12);
        labelCoarsenessThreshold.setForeground(Color.black);
        gbc3.gridx = 0;
        gbc3.gridy = 1;
        operatorPanel.add(labelCoarsenessThreshold, gbc3);
        
        textCoarsenessThreshold = new JTextField(10);
        textCoarsenessThreshold.setText(String.valueOf(coarsenessThreshold));
        textCoarsenessThreshold.setFont(serif12);
        textCoarsenessThreshold.setForeground(Color.black);
        gbc3.gridx = 1;
        gbc3.gridy = 1;
        operatorPanel.add(textCoarsenessThreshold, gbc3);

        contrastCheckBox = new JCheckBox("Contrast");
        contrastCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 2;
        operatorPanel.add(contrastCheckBox, gbc3);
        contrastCheckBox.setSelected(true);
        contrastCheckBox.addActionListener(this);
        
        labelCSize = new JLabel("Contrast neighborhood size (must be odd)");
        labelCSize.setFont(serif12);
        labelCSize.setForeground(Color.black);
        gbc3.gridx = 0;
        gbc3.gridy = 3;
        operatorPanel.add(labelCSize, gbc3);
        
        textCSize = new JTextField(10);
        textCSize.setText(String.valueOf(cSize));
        textCSize.setFont(serif12);
        textCSize.setForeground(Color.black);
        gbc3.gridx = 1;
        gbc3.gridy = 3;
        operatorPanel.add(textCSize, gbc3);

        directionalityCheckBox = new JCheckBox("Directionality");
        directionalityCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 4;
        operatorPanel.add(directionalityCheckBox, gbc3);
        directionalityCheckBox.setSelected(true);
        directionalityCheckBox.addActionListener(this);
        
        labelHistogramBins = new JLabel("Number of histogram bins");
        labelHistogramBins.setFont(serif12);
        labelHistogramBins.setForeground(Color.black);
        gbc3.gridx = 0;
        gbc3.gridy = 5;
        operatorPanel.add(labelHistogramBins, gbc3);
        
        textHistogramBins = new JTextField(10);
        textHistogramBins.setText(String.valueOf(histogramBins));
        textHistogramBins.setFont(serif12);
        textHistogramBins.setForeground(Color.black);
        gbc3.gridx = 1;
        gbc3.gridy = 5;
        operatorPanel.add(textHistogramBins, gbc3);
        
        labelHistogramThreshold = new JLabel("Minimum gradient magnitude for histogram");
        labelHistogramThreshold.setFont(serif12);
        labelHistogramThreshold.setForeground(Color.black);
        gbc3.gridx = 0;
        gbc3.gridy = 6;
        operatorPanel.add(labelHistogramThreshold, gbc3);
        
        textHistogramThreshold = new JTextField(10);
        textHistogramThreshold.setText(String.valueOf(histogramThreshold));
        textHistogramThreshold.setFont(serif12);
        textHistogramThreshold.setForeground(Color.black);
        gbc3.gridx = 1;
        gbc3.gridy = 6;
        operatorPanel.add(textHistogramThreshold, gbc3);

        gbc.gridx = 0;
        gbc.gridy = ypos++;
        gbc.weighty = .55;  
        JScrollPane operatorScroll = new JScrollPane(operatorPanel);
        operatorScroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        operatorScroll.setBorder(buildTitledBorder("Texture operators"));
        mainPanel.add(operatorScroll, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);

        // setVisible( true );

        System.gc();
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************


    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        numOperators = 0;
        
        if (image.isColorImage()) {

            if (redButton.isSelected()) {
                RGBOffset = RED_OFFSET;
            } else if (greenButton.isSelected()) {
                RGBOffset = GREEN_OFFSET;
            } else {
                RGBOffset = BLUE_OFFSET;
            }
        } // if (image.isColorImage())

        doCoarseness = coarsenessCheckBox.isSelected();
        
        if (doCoarseness) {
            tmpStr = textCoarsenessThreshold.getText();
            
            if (testParameter(tmpStr, 0.8, 1.0)) {
                coarsenessThreshold = Double.valueOf(tmpStr).doubleValue();
            } else {
                textCoarsenessThreshold.requestFocus();
                textCoarsenessThreshold.selectAll();
                
                return false;
            }
        } // if (doCoarseness)
        
        doContrast = contrastCheckBox.isSelected();
        
        if (doContrast) {
            tmpStr = textCSize.getText();
            
            if (testParameter(tmpStr,3, Math.min(image.getExtents()[0],image.getExtents()[1]))) {
                cSize = Integer.valueOf(tmpStr).intValue();
            } else {
                textCSize.requestFocus();
                textCSize.selectAll();
                
                return false;
            }
            
            if ((cSize % 2) == 0) {
                MipavUtil.displayError("Contrast neighborhood size must not be even");
                textCSize.requestFocus();
                textCSize.selectAll();
            }
        }

        doDirectionality = directionalityCheckBox.isSelected();
        
        if (doDirectionality) {
            tmpStr = textHistogramBins.getText();
            
            if (testParameter(tmpStr, 3, 256)) {
                histogramBins = Integer.valueOf(tmpStr).intValue();
            } else {
                textHistogramBins.requestFocus();
                textHistogramBins.selectAll();
                
                return false;
            }
            
            tmpStr = textHistogramThreshold.getText();
            
            if (testParameter(tmpStr, Double.MIN_VALUE, Double.MAX_VALUE)) {
                histogramThreshold = Double.valueOf(tmpStr).doubleValue();
            } else {
                textHistogramThreshold.requestFocus();
                textHistogramThreshold.selectAll();
                
                return false;
            }
            
        } // if (doDirectionality)

        numOperators = getNumOperators();

        if (numOperators == 0) {
            MipavUtil.displayError("At least 1 texture operator must be selected");

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
                return new String("Algorithms.Filters (spatial)");
            }

            public String getDescription() {
                return new String("Applies a Tamura texture to the image.");
            }

            public String getDescriptionLong() {
                return new String("Applies a Tamura texture to the image.");
            }

            public String getShortLabel() {
                return new String("TamuraTexture");
            }

            public String getLabel() {
                return new String("Tamura Texture");
            }

            public String getName() {
                return new String("Tamura Texture");
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
            table.put(new ParameterInt("RGB_offset",1));
            table.put(new ParameterBoolean("do_coarseness", true));
            table.put(new ParameterDouble("coarsenessThreshold", 1.0));
            table.put(new ParameterBoolean("do_contrast", true));
            table.put(new ParameterInt("c_size", 13));
            table.put(new ParameterBoolean("do_directionality", true));
            table.put(new ParameterInt("histogram_bins", 16));
            table.put(new ParameterDouble("histogram_threshold", 12.0));
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
    		//System.out.println(resultImage.length);
                return resultImage[0].getImageName();
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
