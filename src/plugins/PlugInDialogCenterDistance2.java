import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;
import java.text.DecimalFormat;

import javax.swing.*;


/**
 * @version  July 14, 2008
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInDialogCenterDistance.java $ $Revision: 21 $ $Date: 1/25/06 4:59p $
 *           </p>
 */
public class PlugInDialogCenterDistance2 extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private JLabel greenMergingLabel;
    
    /** Green merging radius around peak green spot */
    private JTextField greenMergingText;
    
    /** Minimum number of pixels in a green VOI */
    private int greenMin = 10;
    
    private JLabel greenMinLabel;
    
    private JTextField greenMinText;
    
    /** DOCUMENT ME! */
    private float greenFraction = 0.01f;

    /** DOCUMENT ME! */
    private JLabel greenFractionLabel;

    /** DOCUMENT ME! */
    private JTextField greenFractionText;
    
    private int redMin = 50;
    
    private JLabel redMinLabel;
    
    private JTextField redMinText;
    
    /** DOCUMENT ME! */
    private float redFraction = 0.15f;

    /** DOCUMENT ME! */
    private JLabel redFractionLabel;

    /** DOCUMENT ME! */
    private JTextField redFractionText;
    
    private JLabel blueMinLabel;
    
    private JTextField blueMinText;
    
    private JLabel greenRegionsLabel;
    
    private ButtonGroup greenGroup;
    
    private JRadioButton oneButton;
    
    private JRadioButton twoButton;
    
    private JRadioButton threeButton;
    
    private JRadioButton fourButton;
    
    private float mergingDistance;
    
    private int blueMin = 1000;
    
    // Number of green regions per cell
    // 1, 2, 3, or 4 will be applied to all cells
    private int greenRegionNumber;
    
    private JCheckBox twoBox;
    
    private boolean twoGreenLevels = true;
    
    
    /** DOCUMENT ME! */
    private ModelImage image; // source image   

    /** DOCUMENT ME! */
    private PlugInAlgorithmCenterDistance2 centerDistanceAlgo = null;
    
    private JCheckBox blueSmoothBox;
    
    private boolean blueSmooth;
    
    private JLabel interpolationLabel;
    
    private JTextField interpolationText;
    
    private float interpolationDivisor;
    
    private JLabel blueValueLabel;
    
    private JTextField blueValueText;
    
    /** Nucleus boundary requires a minimum blue value >=
     *  image blue min + blueBoundaryFraction * (image blue max - image blue min)
     */
    private float blueBoundaryFraction = 0.15f;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogCenterDistance2() { }

    /**
     * Creates new dialog for distances within a cell from the geometric center using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogCenterDistance2(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        if (!im.isColorImage()) {
            MipavUtil.displayError("Source Image must be Color");
            dispose();

            return;
        }

        image = im;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************

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
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (source == blueSmoothBox) {
            if (blueSmoothBox.isSelected()) {
                interpolationLabel.setEnabled(true);
                interpolationText.setEnabled(true);
            }
            else {
                interpolationLabel.setEnabled(false);
                interpolationText.setEnabled(false);
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

        if (algorithm instanceof PlugInAlgorithmCenterDistance2) {
            image.clearMask();

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }

            if (algorithm != null) {
                algorithm.finalize();
                algorithm = null;
            }

            dispose();
        }

    } // end AlgorithmPerformed()

    /**
     * Accessor that sets the mergingDistance variable, for green merging radius around green peak spot.
     *
     * @param  mergingDistance float
     */
    public void setMergingDistance(float mergingDistance) {
        this.mergingDistance = mergingDistance;
    }
    
    /**
     * Accessor that set the greenFraction variable, for portion of green pixels considered by fuzzy c means
     * segmentation.
     *
     * @param  greenFraction  float
     */
    public void setGreenFraction(float greenFraction) {
        this.greenFraction = greenFraction;
    }
    
    /**
     * Accessor that sets the greenMin variable, for minimum pixel number in a green voi.
     *
     * @param  greenMin  minimum number of pixels in a green voi
     */
    public void setGreenMin(int greenMin) {
        this.greenMin = greenMin;
    }
    
    /**
     * Accessor that set the redFraction variable, for portion of red pixels considered by fuzzy c means
     * segmentation.
     *
     * @param  redFraction  float
     */
    public void setRedFraction(float redFraction) {
        this.redFraction = redFraction;
    }
    
    /**
     * Accessor that sets the redMin variable, for minimum pixel number in a red voi.
     *
     * @param  redMin  minimum number of pixels in a red voi
     */
    public void setRedMin(int redMin) {
        this.redMin = redMin;
    }

    /**
     * Accessor that set the blueMin variable, minimum number of blue pixels per cell
     *
     * @param  blueMin int
     */
    public void setBlueMin(int blueMin) {
        this.blueMin = blueMin;
    }

    /**
     * Accessor that sets the greenRegionNumber variable, for number of green regions per cell
     * 1, 2, 3, or 4 will be applied to all cells
     *
     * @param  greenRegionNumber
     */
    public void setGreenRegionNumber(int greenRegionNumber) {
        this.greenRegionNumber = greenRegionNumber;
    }
    
    public void setTwoGreenLevels(boolean twoGreenLevels) {
        this.twoGreenLevels = twoGreenLevels;
    }
    
    public void setBlueBoundaryFraction(float blueBoundaryFraction) {
        this.blueBoundaryFraction = blueBoundaryFraction;    
    }
    
    public void setBlueSmooth(boolean blueSmooth) {
        this.blueSmooth = blueSmooth;
    }
    
    public void setInterpolationDivisor(float interpolationDivisor) {
        this.interpolationDivisor = interpolationDivisor;
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        try {

            centerDistanceAlgo = new PlugInAlgorithmCenterDistance2(image, blueMin, redMin, redFraction, mergingDistance,
                                                                   greenMin, greenFraction,
                                                                   greenRegionNumber, twoGreenLevels,
                                                                   blueBoundaryFraction, 
                                                                   blueSmooth, interpolationDivisor);

            // This is very important. Adding this object as a listener allows
            // the algorithm to
            // notify this object when it has completed or failed. See algorithm
            // performed event.
            // This is made possible by implementing AlgorithmedPerformed
            // interface
            centerDistanceAlgo.addListener(this);
            createProgressBar(image.getImageName(), " ...", centerDistanceAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (centerDistanceAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                centerDistanceAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Center Distance: unable to allocate enough memory");

            return;
        }

    } // end callAlgorithm()

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() { }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();

        if (!image.isColorImage()) {
            throw new ParameterException(AlgorithmParameters.getInputImageLabel(1), "Source Image must be Color");
        }

        setBlueMin(scriptParameters.getParams().getInt("blue_min"));
        setRedMin(scriptParameters.getParams().getInt("red_min"));
        setRedFraction(scriptParameters.getParams().getFloat("red_fraction"));
        setMergingDistance(scriptParameters.getParams().getFloat("merging_distance"));
        setGreenMin(scriptParameters.getParams().getInt("green_min"));
        setGreenFraction(scriptParameters.getParams().getFloat("green_fraction"));
        setGreenRegionNumber(scriptParameters.getParams().getInt("green_region_number"));
        setTwoGreenLevels(scriptParameters.getParams().getBoolean("two_green_levels"));
        setBlueBoundaryFraction(scriptParameters.getParams().getFloat("blue_boundary_fraction"));
        setBlueSmooth(scriptParameters.getParams().getBoolean("blue_smooth"));
        setInterpolationDivisor(scriptParameters.getParams().getFloat("interpolation_divisor"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.getParams().put(ParameterFactory.newParameter("blue_min", blueMin));
        scriptParameters.getParams().put(ParameterFactory.newParameter("red_min", redMin));
        scriptParameters.getParams().put(ParameterFactory.newParameter("red_fraction", redFraction));
        scriptParameters.getParams().put(ParameterFactory.newParameter("merging_distance", mergingDistance));
        scriptParameters.getParams().put(ParameterFactory.newParameter("green_min", greenMin));
        scriptParameters.getParams().put(ParameterFactory.newParameter("green_fraction", greenFraction));
        scriptParameters.getParams().put(ParameterFactory.newParameter("green_region_number", greenRegionNumber));
        scriptParameters.getParams().put(ParameterFactory.newParameter("two_green_levels", twoGreenLevels));
        scriptParameters.getParams().put(ParameterFactory.newParameter("blue_boundary_fraction", blueBoundaryFraction));
        scriptParameters.getParams().put(ParameterFactory.newParameter("blue_smooth", blueSmooth));
        scriptParameters.getParams().put(ParameterFactory.newParameter("interpolation_divisor", interpolationDivisor));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        DecimalFormat df;
        int xUnits;
        String unitStr;
        String distStr;
        setForeground(Color.black);
        setTitle("Center Distances version 2  07/14/08");
        
        df = new DecimalFormat("0.000E0");

        GridBagConstraints gbc = new GridBagConstraints();
        int yPos = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = yPos++;

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(buildTitledBorder("Input parameters"));
        
        blueMinLabel = new JLabel("Minimum number of blue pixels per nucleus");
        blueMinLabel.setForeground(Color.black);
        blueMinLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(blueMinLabel, gbc);

        blueMinText = new JTextField(5);
        if (image.getNDims() == 2) {
            blueMinText.setText("1000");
        }
        else {
            blueMinText.setText("20000");
        }
        blueMinText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(blueMinText, gbc);
        
        redMinLabel = new JLabel("Minimum red pixel count");
        redMinLabel.setForeground(Color.black);
        redMinLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(redMinLabel, gbc);

        redMinText = new JTextField(5);
        redMinText.setText("50");
        redMinText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(redMinText, gbc);
        
        redFractionLabel = new JLabel("Fraction of red pixels to consider");
        redFractionLabel.setForeground(Color.black);
        redFractionLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(redFractionLabel, gbc);

        redFractionText = new JTextField(5);
        redFractionText.setText("0.15");
        redFractionText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(redFractionText, gbc);
        
        xUnits = image.getFileInfo(0).getUnitsOfMeasure()[0];
        if (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum()) {
            unitStr = (Unit.getUnitFromLegacyNum(xUnits)).toString();
            greenMergingLabel = new JLabel("Green merging radius around peak (" + unitStr + ")");
        }
        else {
            greenMergingLabel = new JLabel("Green merging radius around peak");    
        }
        greenMergingLabel.setForeground(Color.black);
        greenMergingLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(greenMergingLabel, gbc);

        if (image.getNDims() == 2) {
            //mergingDistance = 8.0f * image.getFileInfo(0).getResolutions()[0]; 
            mergingDistance = 0.0f;
        }
        else {
            //mergingDistance = 4.0f * image.getFileInfo(0).getResolutions()[0];
            mergingDistance = 0.0f;
        }
        distStr = df.format(mergingDistance);
        greenMergingText = new JTextField(10);
        greenMergingText.setText(distStr);
        greenMergingText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(greenMergingText, gbc);
        
        greenMinLabel = new JLabel("Minimum green pixel count");
        greenMinLabel.setForeground(Color.black);
        greenMinLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(greenMinLabel, gbc);

        greenMinText = new JTextField(5);
        greenMinText.setText("10");
        greenMinText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(greenMinText, gbc);
        
        greenFractionLabel = new JLabel("Fraction of green pixels to consider");
        greenFractionLabel.setForeground(Color.black);
        greenFractionLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(greenFractionLabel, gbc);

        greenFractionText = new JTextField(5);
        greenFractionText.setText("0.01");
        greenFractionText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(greenFractionText, gbc);

        greenRegionsLabel = new JLabel("Green regions per cell");
        greenRegionsLabel.setForeground(Color.black);
        greenRegionsLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(greenRegionsLabel, gbc);
        
        JPanel buttonPanel = new JPanel(new GridBagLayout());
        
        greenGroup = new ButtonGroup();
        
        oneButton = new JRadioButton("1", false);
        oneButton.setForeground(Color.black);
        oneButton.setFont(serif12);
        greenGroup.add(oneButton);
        gbc.gridx = 0;
        gbc.gridy = 0;
        buttonPanel.add(oneButton, gbc);
        
        twoButton = new JRadioButton("2", true);
        twoButton.setForeground(Color.black);
        twoButton.setFont(serif12);
        greenGroup.add(twoButton);
        gbc.gridx = 1;
        gbc.gridy = 0;
        buttonPanel.add(twoButton, gbc);
        
        threeButton = new JRadioButton("3", false);
        threeButton.setForeground(Color.black);
        threeButton.setFont(serif12);
        greenGroup.add(threeButton);
        gbc.gridx = 2;
        gbc.gridy = 0;
        buttonPanel.add(threeButton, gbc);
        
        fourButton = new JRadioButton("4", false);
        fourButton.setForeground(Color.black);
        fourButton.setFont(serif12);
        greenGroup.add(fourButton);
        gbc.gridx = 3;
        gbc.gridy = 0;
        buttonPanel.add(fourButton, gbc);
        
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(buttonPanel, gbc);
        
        twoBox = new JCheckBox("Use 2 top gray levels in green segmentation", true);
        twoBox.setForeground(Color.black);
        twoBox.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos++;
        mainPanel.add(twoBox, gbc);
        
        blueValueLabel = new JLabel("Fraction of blue transition from image min to max at nucleus boundary");
        blueValueLabel.setForeground(Color.black);
        blueValueLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(blueValueLabel, gbc);
        
        blueValueText = new JTextField(5);
        blueValueText.setText("0.15");
        blueValueText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(blueValueText, gbc);
        
        blueSmoothBox = new JCheckBox("Smooth blue VOI contours with AlgorithmBSmooth", true);
        blueSmoothBox.setForeground(Color.black);
        blueSmoothBox.setFont(serif12);
        blueSmoothBox.addActionListener(this);
        gbc.gridx = 0;
        gbc.gridy = yPos++;
        mainPanel.add(blueSmoothBox, gbc);
        
        interpolationLabel = new JLabel("Number of interpolation points determined by divisor (> 1.0)");
        interpolationLabel.setForeground(Color.black);
        interpolationLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(interpolationLabel, gbc);
        
        interpolationText = new JTextField(5);
        interpolationText.setText("24.0");
        interpolationText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(interpolationText, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();

    } // end init()

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        int i;
        
        int totLength = image.getExtents()[0];

        for (i = 1; i < image.getNDims(); i++) {
            totLength *= image.getExtents()[i];
        }
        
        tmpStr = greenMergingText.getText();
        mergingDistance = Float.parseFloat(tmpStr);
        if (mergingDistance < 0.0f) {
            MipavUtil.displayError("Merging distance cannot be less than 0");
            greenMergingText.requestFocus();
            greenMergingText.selectAll();
            return false;
        }
        
        tmpStr = redMinText.getText();
        redMin = Integer.parseInt(tmpStr);

        if (redMin < 1) {
            MipavUtil.displayError("red minimum must be at least 1");
            redMinText.requestFocus();
            redMinText.selectAll();

            return false;
        } else if (redMin > totLength) {
            MipavUtil.displayError("red minimum must not exceed " + totLength);
            redMinText.requestFocus();
            redMinText.selectAll();

            return false;
        }
        
        tmpStr = redFractionText.getText();
        redFraction = Float.parseFloat(tmpStr);

        if (redFraction <= 0.0f) {
            MipavUtil.displayError("red fraction must be greater than zero");
            redFractionText.requestFocus();
            redFractionText.selectAll();

            return false;
        } else if (redFraction > 1.0f) {
            MipavUtil.displayError("red fraction must not exceed one");
            redFractionText.requestFocus();
            redFractionText.selectAll();

            return false;
        }
        
        tmpStr = greenMinText.getText();
        greenMin = Integer.parseInt(tmpStr);

        if (greenMin < 1) {
            MipavUtil.displayError("green minimum must be at least 1");
            greenMinText.requestFocus();
            greenMinText.selectAll();

            return false;
        } else if (greenMin > totLength) {
            MipavUtil.displayError("green minimum must not exceed " + totLength);
            greenMinText.requestFocus();
            greenMinText.selectAll();

            return false;
        }
        
        tmpStr = greenFractionText.getText();
        greenFraction = Float.parseFloat(tmpStr);

        if (greenFraction <= 0.0f) {
            MipavUtil.displayError("green fraction must be greater than zero");
            greenFractionText.requestFocus();
            greenFractionText.selectAll();

            return false;
        } else if (greenFraction > 1.0f) {
            MipavUtil.displayError("green fraction must not exceed one");
            greenFractionText.requestFocus();
            greenFractionText.selectAll();

            return false;
        }
        
        tmpStr = blueMinText.getText();
        blueMin = Integer.parseInt(tmpStr);
        if (blueMin <= 0) {
            MipavUtil.displayError("Number of blue pixels must be greater than 0");
            blueMinText.requestFocus();
            blueMinText.selectAll();
            return false;    
        } else if (blueMin > totLength) {
            MipavUtil.displayError("blue minimum must not exceed " + totLength);
            blueMinText.requestFocus();
            blueMinText.selectAll();

            return false;
        }
        
        if (oneButton.isSelected()) {
            greenRegionNumber = 1;
        }
        else if (twoButton.isSelected()){
            greenRegionNumber = 2;
        }
        else if (threeButton.isSelected()) {
            greenRegionNumber = 3;
        }
        else {
            greenRegionNumber = 4;
        }
        
        twoGreenLevels = twoBox.isSelected();
        
        tmpStr = blueValueText.getText();
        blueBoundaryFraction = Float.parseFloat(tmpStr);
        if (blueBoundaryFraction < 0.0f) {
            MipavUtil.displayError("Blue boundary fraction cannot be less than 0.0");
            blueValueText.requestFocus();
            blueValueText.selectAll();
            return false;
        }
        else if (blueBoundaryFraction > 1.0f) {
            MipavUtil.displayError("Blue boundary value cannot be greater than 1.0");
            blueValueText.requestFocus();
            blueValueText.selectAll();
            return false;    
        }
        
        blueSmooth = blueSmoothBox.isSelected();
        
        tmpStr = interpolationText.getText();
        interpolationDivisor = Float.parseFloat(tmpStr);
        if (interpolationDivisor <= 1.0f) {
            MipavUtil.displayError("Interpolation divisor must be greater than 1");
            interpolationText.requestFocus();
            interpolationText.selectAll();
            return false;
        }
        
        return true;
    } // end setVariables()

}
