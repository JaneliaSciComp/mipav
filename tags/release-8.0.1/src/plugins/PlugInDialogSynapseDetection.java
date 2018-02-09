import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;
import java.io.IOException;

import javax.swing.*;


/**
 * @version  March 5, 2009
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInDialogSynapseDetection.java $ $Revision: 21 $ $Date: 1/25/06 4:59p $
 *           </p>
 */
public class PlugInDialogSynapseDetection extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    /** Minimum number of red pixels in a transition */
    private int redMin = 1;
    
    private JLabel redMinLabel;
    
    private JTextField redMinText;
    
    private int redMax = 20;
    
    private JLabel redMaxLabel;
    
    private JTextField redMaxText;
    
    private int greenMin = 1;
    
    private JLabel greenMinLabel;
    
    private JTextField greenMinText;
    
    private int greenMax = 200;
    
    private JLabel greenMaxLabel;
    
    private JTextField greenMaxText;
    
    private int blueMinXY = 1;
    
    private JLabel blueMinXYLabel;
    
    private JTextField blueMinXYText;
    
    private int blueMaxXY = 20;
    
    private JLabel blueMaxXYLabel;
    
    private JTextField blueMaxXYText;
    
    private int blueMinZ = 1;
    
    private JLabel blueMinZLabel;
    
    private JTextField blueMinZText;
    
    private int blueMaxZ = 20;
    
    private JLabel blueMaxZLabel;
    
    private JTextField blueMaxZText;
    
    private int redIntensity = 5;
    
    private JLabel redIntensityLabel;
    
    private JTextField redIntensityText;
    
    private int redBrightIntensity = 55;
    
    private JLabel redBrightIntensityLabel;
    
    private JTextField redBrightIntensityText;
    
    private int greenIntensity = 5;
    
    private JLabel greenIntensityLabel;
    
    private JTextField greenIntensityText;
    
    private int greenBrightIntensity = 85;
    
    private JLabel greenBrightIntensityLabel;
    
    private JTextField greenBrightIntensityText;
    
    private int blueIntensity = 15;
    
    private JLabel blueIntensityLabel;
    
    private JTextField blueIntensityText;
    
    private int blueBrightIntensity = 80;
    
    private JLabel blueBrightIntensityLabel;
    
    private JTextField blueBrightIntensityText;
    
    private JCheckBox histoInfoCheckBox;
    
    /** If true, provide histograms of red, green, and blue values along detection lines */
    private boolean histoInfo = false;
    
    /** DOCUMENT ME! */
    private ModelImage image; // source image   

    /** DOCUMENT ME! */
    private PlugInAlgorithmSynapseDetection synapseDetectionAlgo = null;
    
    // true if blueFraction >= 0.05
    private boolean bigBlueFraction = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogSynapseDetection() { }

    /**
     * Creates new dialog for distances within a cell from the geometric center using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogSynapseDetection(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        if (!im.isColorImage()) {
            MipavUtil.displayError("Source Image must be Color");
            dispose();

            return;
        }

        image = im;
        preprocess();
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

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            preprocess();
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
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

        if (algorithm instanceof PlugInAlgorithmFociAndStrands) {
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
        str += redMin + delim;
        str += redMax + delim;
        str += greenMin + delim;
        str += greenMax + delim;
        str += blueMinXY + delim;
        str += blueMaxXY + delim;
        str += blueMinZ + delim;
        str += blueMaxZ + delim;
        str += redIntensity + delim;
        str += redBrightIntensity + delim;
        str += greenIntensity + delim;
        str += greenBrightIntensity + delim;
        str += blueIntensity + delim;
        str += blueBrightIntensity + delim;
        str += histoInfo;
        
        return str;
    }
    
    /**
     * Accessor that sets the redMin variable, for minimum red pixel width.
     *
     * @param  redMin  minimum number of red pixels
     */
    public void setRedMin(int redMin) {
        this.redMin = redMin;
    }
    
    /**
     * Accessor that sets the redMax variable, for maximum red pixel width.
     *
     * @param  redMax  maximum number of red pixels
     */
    public void setRedMax(int redMax) {
        this.redMax = redMax;
    }
    
    /**
     * Accessor that sets the greenMin variable, for minimum green pixel width.
     *
     * @param  greenMin  minimum number of green pixels
     */
    public void setGreenMin(int greenMin) {
        this.greenMin = greenMin;
    }
    
    /**
     * Accessor that sets the greenMax variable, for maximum green pixel width.
     *
     * @param  greenMax  maximum number of green pixels
     */
    public void setGreenMax(int greenMax) {
        this.greenMax = greenMax;
    }
    
    /**
     * Accessor that sets the blueMinXY variable, for minimum blue pixel width within a slice.
     *
     * @param  blueMinXY  minimum number of blue pixels within one slice
     */
    public void setBlueMinXY(int blueMinXY) {
        this.blueMinXY = blueMinXY;
    }
    
    /**
     * Accessor that sets the blueMaxXY variable, for maximum blue pixel width within a slice.
     *
     * @param  blueMaxXY  maximum number of blue pixels within one slice
     */
    public void setBlueMaxXY(int blueMaxXY) {
        this.blueMaxXY = blueMaxXY;
    }
    
    /**
     * Accessor that sets the blueMinZ variable, for minimum blue pixel width between slices.
     *
     * @param  blueMinZ  minimum number of blue pixels between slices
     */
    public void setBlueMinZ(int blueMinZ) {
        this.blueMinZ = blueMinZ;
    }
    
    /**
     * Accessor that sets the blueMaxZ variable, for maximum blue pixel width between slices.
     *
     * @param  blueMaxZ  maximum number of blue pixels between slices
     */
    public void setBlueMaxZ(int blueMaxZ) {
        this.blueMaxZ = blueMaxZ;
    }
    
    /**
     * Accessor that sets the redIntensity variable, for minimum red intensity over green, blue.
     *
     * @param  redIntensity  minimum red intensity over green, blue
     */
    public void setRedIntensity(int redIntensity) {
        this.redIntensity = redIntensity;
    }
    
    /**
     * Accessor that sets the redBrightIntensity variable, for minimum red BrightIntensity over green, blue.
     *
     * @param  redBrightIntensity  minimum red BrightIntensity over green, blue
     */
    public void setRedBrightIntensity(int redBrightIntensity) {
        this.redBrightIntensity = redBrightIntensity;
    }
    
    /**
     * Accessor that sets the greenIntensity variable, for minimum green intensity over red, blue.
     *
     * @param  green intensity  minimum green intensity over red, blue
     */
    public void setGreenIntensity(int greenIntensity) {
        this.greenIntensity = greenIntensity;
    }
    
    /**
     * Accessor that sets the greenBrightIntensity variable, for minimum green BrightIntensity over red, blue.
     *
     * @param  greenBrightIntensity  minimum green BrightIntensity over red, blue
     */
    public void setGreenBrightIntensity(int greenBrightIntensity) {
        this.greenBrightIntensity = greenBrightIntensity;
    }
    
    /**
     * Accessor that sets the blueIntensity variable, for minimum blue intensity over red, green.
     *
     * @param  blueIntensity  minimum blue intensity over red, green
     */
    public void setBlueIntensity(int blueIntensity) {
        this.blueIntensity = blueIntensity;
    }
    
    /**
     * Accessor that sets the blueBrightIntensity variable, for minimum blue BrightIntensity over red, green.
     *
     * @param  blueBrightIntensity  minimum blue BrightIntensity over red, green
     */
    public void setBlueBrightIntensity(int blueBrightIntensity) {
        this.blueBrightIntensity = blueBrightIntensity;
    }
    
    public void setHistoInfo(boolean histoInfo) {
        this.histoInfo = histoInfo;    
    }
    
    private void preprocess() {
        int xDim = image.getExtents()[0];
        int yDim = image.getExtents()[1];
        int zDim = image.getExtents()[2];
        int length = xDim * yDim * zDim;
        byte buffer[] = new byte[length];
        byte greenBuffer[] = new byte[length];
        byte blueBuffer[] = new byte[length];
        int pos;
        int red;
        int green;
        int blue;
        int redCount = 0;
        int greenCount = 0;
        int blueCount = 0;
        double redFraction;
        double greenFraction;
        double blueFraction;
        
        try {
            image.exportRGBData(1, 0, length, buffer); // export red data
        } catch (IOException error) {
            buffer = null;
            greenBuffer = null;
            blueBuffer = null;
            MipavUtil.displayError("Plugin SynapseDetection reports: source image locked");

            return;
        }
        
        try {
            image.exportRGBData(2, 0, length, greenBuffer); // export green data
        } catch (IOException error) {
            buffer = null;
            greenBuffer = null;
            blueBuffer = null;
            MipavUtil.displayError("Plugin SynapseDetection reports: source image locked");

            return;
        }
        
        try {
            image.exportRGBData(3, 0, length, blueBuffer); // export blue data
        } catch (IOException error) {
            buffer = null;
            greenBuffer = null;
            blueBuffer = null;
            MipavUtil.displayError("Plugin SynapseDetection reports: source image locked");

            return;
        }
        
        
        for (pos = 0; pos < length; pos++) {
            red = buffer[pos] & 0xff;
            green = greenBuffer[pos] & 0xff;
            blue = blueBuffer[pos] & 0xff;
            if ((red > green) && (red > blue)) {
                redCount++;
            }
            else if ((green > red) && (green > blue)) {
                greenCount++;
            }
            else if ((blue > red) && (blue > green)) {
                blueCount++;
            }
        } // for (pos = 0; pos < length; pos++)
        buffer = null;
        greenBuffer = null;
        blueBuffer = null;
        redFraction = (double)redCount/pos;
        greenFraction = (double)greenCount/pos;
        blueFraction = (double)blueCount/pos;
        Preferences.debug("redFraction = " + redFraction + "\n");
        Preferences.debug("greenFraction = " + greenFraction + "\n");
        Preferences.debug("blueFraction = " + blueFraction + "\n");
        if (blueFraction >= 0.05) {
            bigBlueFraction = true;
        }     
    }
    
    /**
     * Once all the necessary variables are set, call the Synapse Detection algorithm.
     */
    protected void callAlgorithm() {

        try {

            synapseDetectionAlgo = new PlugInAlgorithmSynapseDetection(image, redMin, redMax, greenMin, greenMax,
                                                                   blueMinXY, blueMaxXY, blueMinZ, blueMaxZ,
                                                                   bigBlueFraction, redIntensity, redBrightIntensity,
                                                                   greenIntensity, greenBrightIntensity,
                                                                   blueIntensity, blueBrightIntensity, histoInfo);

            // This is very important. Adding this object as a listener allows
            // the algorithm to
            // notify this object when it has completed or failed. See algorithm
            // performed event.
            // This is made possible by implementing AlgorithmedPerformed
            // interface
            synapseDetectionAlgo.addListener(this);
            createProgressBar(image.getImageName(), " ...", synapseDetectionAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (synapseDetectionAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                synapseDetectionAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Synapse Detection: unable to allocate enough memory");

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
        setRedMin(scriptParameters.getParams().getInt("red_min"));
        setRedMax(scriptParameters.getParams().getInt("red_max"));
        setGreenMin(scriptParameters.getParams().getInt("green_min"));
        setGreenMax(scriptParameters.getParams().getInt("green_max"));
        setBlueMinXY(scriptParameters.getParams().getInt("blue_minxy"));
        setBlueMaxXY(scriptParameters.getParams().getInt("blue_maxxy"));
        setBlueMinZ(scriptParameters.getParams().getInt("blue_minz"));
        setBlueMaxZ(scriptParameters.getParams().getInt("blue_maxz"));
        setRedIntensity(scriptParameters.getParams().getInt("red_intensity"));
        setRedBrightIntensity(scriptParameters.getParams().getInt("red_bright_intensity"));
        setGreenIntensity(scriptParameters.getParams().getInt("green_intensity"));
        setGreenBrightIntensity(scriptParameters.getParams().getInt("green_bright_intensity"));
        setBlueIntensity(scriptParameters.getParams().getInt("blue_intensity"));
        setBlueBrightIntensity(scriptParameters.getParams().getInt("blue_bright_intensity"));
        setHistoInfo(scriptParameters.getParams().getBoolean("histo_info"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.getParams().put(ParameterFactory.newParameter("red_min", redMin));
        scriptParameters.getParams().put(ParameterFactory.newParameter("red_max", redMax));
        scriptParameters.getParams().put(ParameterFactory.newParameter("green_min", greenMin));
        scriptParameters.getParams().put(ParameterFactory.newParameter("green_max", greenMax));
        scriptParameters.getParams().put(ParameterFactory.newParameter("blue_minxy", blueMinXY));
        scriptParameters.getParams().put(ParameterFactory.newParameter("blue_maxxy", blueMaxXY));
        scriptParameters.getParams().put(ParameterFactory.newParameter("blue_minz", blueMinZ));
        scriptParameters.getParams().put(ParameterFactory.newParameter("blue_maxz", blueMaxZ));
        scriptParameters.getParams().put(ParameterFactory.newParameter("red_intensity", redIntensity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("red_bright_intensity", redBrightIntensity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("green_intensity", greenIntensity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("green_bright_intensity", greenBrightIntensity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("blue_intensity", blueIntensity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("blue_bright_intensity", blueBrightIntensity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("histo_info", histoInfo));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Synapse Detection  03/05/09");
     
        GridBagConstraints gbc = new GridBagConstraints();
        int yPos = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = yPos;

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(buildTitledBorder("Input parameters"));
        
        redMinLabel = new JLabel("Minimum red pixel width");
        redMinLabel.setForeground(Color.black);
        redMinLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(redMinLabel, gbc);

        redMinText = new JTextField(10);
        redMinText.setText("1");
        redMinText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(redMinText, gbc);
        
        redMaxLabel = new JLabel("Maximum red pixel width");
        redMaxLabel.setForeground(Color.black);
        redMaxLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(redMaxLabel, gbc);

        redMaxText = new JTextField(10);
        redMaxText.setText("20");
        redMaxText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(redMaxText, gbc);
        
        greenMinLabel = new JLabel("Minimum green pixel width");
        greenMinLabel.setForeground(Color.black);
        greenMinLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(greenMinLabel, gbc);

        greenMinText = new JTextField(10);
        greenMinText.setText("1");
        greenMinText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(greenMinText, gbc);
        
        greenMaxLabel = new JLabel("Maximum green pixel width");
        greenMaxLabel.setForeground(Color.black);
        greenMaxLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(greenMaxLabel, gbc);

        greenMaxText = new JTextField(10);
        greenMaxText.setText("200");
        greenMaxText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(greenMaxText, gbc);
        
        blueMinXYLabel = new JLabel("Minimum blue pixel width within a slice");
        blueMinXYLabel.setForeground(Color.black);
        blueMinXYLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(blueMinXYLabel, gbc);

        blueMinXYText = new JTextField(10);
        blueMinXYText.setText("1");
        blueMinXYText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(blueMinXYText, gbc);
        
        blueMaxXYLabel = new JLabel("Maximum blue pixel width within a slice");
        blueMaxXYLabel.setForeground(Color.black);
        blueMaxXYLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(blueMaxXYLabel, gbc);
        
        blueMaxXYText = new JTextField(10);
        if (bigBlueFraction) {
            blueMaxXYText.setText("200");
        }
        else {
            blueMaxXYText.setText("20");
        }
        blueMaxXYText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(blueMaxXYText, gbc);
        
        blueMinZLabel = new JLabel("Minimum blue pixel width between slices");
        blueMinZLabel.setForeground(Color.black);
        blueMinZLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(blueMinZLabel, gbc);

        blueMinZText = new JTextField(10);
        blueMinZText.setText("1");
        blueMinZText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(blueMinZText, gbc);
        
        blueMaxZLabel = new JLabel("Maximum blue pixel width  between slices");
        blueMaxZLabel.setForeground(Color.black);
        blueMaxZLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(blueMaxZLabel, gbc);
        
        blueMaxZText = new JTextField(10);
        if (bigBlueFraction) {
            blueMaxZText.setText("200");
        }
        else {
            blueMaxZText.setText("20");
        }
        blueMaxZText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(blueMaxZText, gbc);
        
        redIntensityLabel = new JLabel("Minimum red intensity");
        redIntensityLabel.setForeground(Color.black);
        redIntensityLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(redIntensityLabel, gbc);

        redIntensityText = new JTextField(10);
        redIntensityText.setText("5");
        redIntensityText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(redIntensityText, gbc);
        
        redBrightIntensityLabel = new JLabel("Minimum bright red intensity");
        redBrightIntensityLabel.setForeground(Color.black);
        redBrightIntensityLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(redBrightIntensityLabel, gbc);

        redBrightIntensityText = new JTextField(10);
        redBrightIntensityText.setText("55");
        redBrightIntensityText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(redBrightIntensityText, gbc);
        
        greenIntensityLabel = new JLabel("Minimum green intensity");
        greenIntensityLabel.setForeground(Color.black);
        greenIntensityLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(greenIntensityLabel, gbc);

        greenIntensityText = new JTextField(10);
        greenIntensityText.setText("5");
        greenIntensityText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(greenIntensityText, gbc);
        
        greenBrightIntensityLabel = new JLabel("Minimum bright green intensity");
        greenBrightIntensityLabel.setForeground(Color.black);
        greenBrightIntensityLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(greenBrightIntensityLabel, gbc);

        greenBrightIntensityText = new JTextField(10);
        greenBrightIntensityText.setText("85");
        greenBrightIntensityText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(greenBrightIntensityText, gbc);
        
        blueIntensityLabel = new JLabel("Minimum blue intensity");
        blueIntensityLabel.setForeground(Color.black);
        blueIntensityLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(blueIntensityLabel, gbc);

        blueIntensityText = new JTextField(10);
        blueIntensityText.setText("15");
        blueIntensityText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(blueIntensityText, gbc);
        
        blueBrightIntensityLabel = new JLabel("Minimum bright blue intensity");
        blueBrightIntensityLabel.setForeground(Color.black);
        blueBrightIntensityLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(blueBrightIntensityLabel, gbc);

        blueBrightIntensityText = new JTextField(10);
        blueBrightIntensityText.setText("80");
        blueBrightIntensityText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(blueBrightIntensityText, gbc);
        
        histoInfoCheckBox = new JCheckBox("Obtain histograms of detected line colors");
        histoInfoCheckBox.setFont(serif12);
        histoInfoCheckBox.setForeground(Color.black);
        histoInfoCheckBox.setSelected(false);
        gbc.gridx = 0;
        gbc.gridy = yPos++;
        mainPanel.add(histoInfoCheckBox, gbc);

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
        
        tmpStr = redMinText.getText();
        redMin = Integer.parseInt(tmpStr);

        if (redMin < 1) {
            MipavUtil.displayError("red minimum must be at least 1");
            redMinText.requestFocus();
            redMinText.selectAll();

            return false;
        } else if (redMin > 200) {
            MipavUtil.displayError("red minimum must not exceed 200");
            redMinText.requestFocus();
            redMinText.selectAll();

            return false;
        }
        
        tmpStr = redMaxText.getText();
        redMax = Integer.parseInt(tmpStr);

        if (redMax < redMin) {
            MipavUtil.displayError("red maximum must be at least red minimum");
            redMaxText.requestFocus();
            redMaxText.selectAll();

            return false;
        } else if (redMax > 500) {
            MipavUtil.displayError("red maximum must not exceed 500");
            redMaxText.requestFocus();
            redMaxText.selectAll();

            return false;
        }
        
        tmpStr = greenMinText.getText();
        greenMin = Integer.parseInt(tmpStr);

        if (greenMin < 1) {
            MipavUtil.displayError("green minimum must be at least 1");
            greenMinText.requestFocus();
            greenMinText.selectAll();

            return false;
        } else if (greenMin > 200) {
            MipavUtil.displayError("green minimum must not exceed 200");
            greenMinText.requestFocus();
            greenMinText.selectAll();

            return false;
        }
        
        tmpStr = greenMaxText.getText();
        greenMax = Integer.parseInt(tmpStr);

        if (greenMax < greenMin) {
            MipavUtil.displayError("green maximum must be at least green minimum");
            greenMaxText.requestFocus();
            greenMaxText.selectAll();

            return false;
        } else if (greenMax > 500) {
            MipavUtil.displayError("green maximum must not exceed 500");
            greenMaxText.requestFocus();
            greenMaxText.selectAll();

            return false;
        }
        
        tmpStr = blueMinXYText.getText();
        blueMinXY = Integer.parseInt(tmpStr);

        if (blueMinXY < 1) {
            MipavUtil.displayError("blue minimum xy must be at least 1");
            blueMinXYText.requestFocus();
            blueMinXYText.selectAll();

            return false;
        } else if (blueMinXY > 200) {
            MipavUtil.displayError("blue minimum xy must not exceed 200");
            blueMinXYText.requestFocus();
            blueMinXYText.selectAll();

            return false;
        }
        
        tmpStr = blueMaxXYText.getText();
        blueMaxXY = Integer.parseInt(tmpStr);

        if (blueMaxXY < blueMinXY) {
            MipavUtil.displayError("blue maximum xy must be at least blue minimum");
            blueMaxXYText.requestFocus();
            blueMaxXYText.selectAll();

            return false;
        } else if (blueMaxXY > 500) {
            MipavUtil.displayError("blue maximum xy must not exceed 500");
            blueMaxXYText.requestFocus();
            blueMaxXYText.selectAll();

            return false;
        }
        
        tmpStr = blueMinZText.getText();
        blueMinZ = Integer.parseInt(tmpStr);

        
        if (blueMinZ < 1) {
            MipavUtil.displayError("blue minimum z must be at least 1");
            blueMinZText.requestFocus();
            blueMinZText.selectAll();

            return false;
        } else if (blueMinZ > 200) {
            MipavUtil.displayError("blue minimum z must not exceed 200");
            blueMinZText.requestFocus();
            blueMinZText.selectAll();

            return false;
        }
        
        tmpStr = blueMaxZText.getText();
        blueMaxZ = Integer.parseInt(tmpStr);

        if (blueMaxZ < blueMinZ) {
            MipavUtil.displayError("blue maximum z must be at least blue minimum");
            blueMaxZText.requestFocus();
            blueMaxZText.selectAll();

            return false;
        } else if (blueMaxZ > 500) {
            MipavUtil.displayError("blue maximum z must not exceed 500");
            blueMaxZText.requestFocus();
            blueMaxZText.selectAll();

            return false;
        }
        
        tmpStr = redIntensityText.getText();
        redIntensity = Integer.parseInt(tmpStr);

        if (redIntensity < 1) {
            MipavUtil.displayError("red intensity must be at least 1");
            redIntensityText.requestFocus();
            redIntensityText.selectAll();

            return false;
        } else if (redIntensity > 255) {
            MipavUtil.displayError("red intensity must not exceed 255");
            redIntensityText.requestFocus();
            redIntensityText.selectAll();

            return false;
        }
        
        tmpStr = redBrightIntensityText.getText();
        redBrightIntensity = Integer.parseInt(tmpStr);

        if (redBrightIntensity < redIntensity) {
            MipavUtil.displayError("red bright intensity must be at least red intensity");
            redBrightIntensityText.requestFocus();
            redBrightIntensityText.selectAll();

            return false;
        } else if (redBrightIntensity > 255) {
            MipavUtil.displayError("red bright intensity must not exceed 255");
            redBrightIntensityText.requestFocus();
            redBrightIntensityText.selectAll();

            return false;
        }
        
        tmpStr = greenIntensityText.getText();
        greenIntensity = Integer.parseInt(tmpStr);

        if (greenIntensity < 1) {
            MipavUtil.displayError("green intensity must be at least 1");
            greenIntensityText.requestFocus();
            greenIntensityText.selectAll();

            return false;
        } else if (greenIntensity > 255) {
            MipavUtil.displayError("green intensity must not exceed 255");
            greenIntensityText.requestFocus();
            greenIntensityText.selectAll();

            return false;
        }
        
        tmpStr = greenBrightIntensityText.getText();
        greenBrightIntensity = Integer.parseInt(tmpStr);

        if (greenBrightIntensity < greenIntensity) {
            MipavUtil.displayError("green bright intensity must be at least green intensity");
            greenBrightIntensityText.requestFocus();
            greenBrightIntensityText.selectAll();

            return false;
        } else if (greenBrightIntensity > 255) {
            MipavUtil.displayError("green bright intensity must not exceed 255");
            greenBrightIntensityText.requestFocus();
            greenBrightIntensityText.selectAll();

            return false;
        }
        
        tmpStr = blueIntensityText.getText();
        blueIntensity = Integer.parseInt(tmpStr);

        if (blueIntensity < 1) {
            MipavUtil.displayError("blue intensity must be at least 1");
            blueIntensityText.requestFocus();
            blueIntensityText.selectAll();

            return false;
        } else if (blueIntensity > 255) {
            MipavUtil.displayError("blue intensity must not exceed 255");
            blueIntensityText.requestFocus();
            blueIntensityText.selectAll();

            return false;
        }
        
        tmpStr = blueBrightIntensityText.getText();
        blueBrightIntensity = Integer.parseInt(tmpStr);

        if (blueBrightIntensity < blueIntensity) {
            MipavUtil.displayError("blue bright intensity must be at least blue intensity");
            blueBrightIntensityText.requestFocus();
            blueBrightIntensityText.selectAll();

            return false;
        } else if (blueBrightIntensity > 255) {
            MipavUtil.displayError("blue bright intensity must not exceed 255");
            blueBrightIntensityText.requestFocus();
            blueBrightIntensityText.selectAll();

            return false;
        }
        
        histoInfo = histoInfoCheckBox.isSelected();
        
        return true;
    } // end setVariables()

}
