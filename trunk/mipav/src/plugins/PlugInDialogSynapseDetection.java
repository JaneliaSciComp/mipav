import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;
import java.text.DecimalFormat;

import javax.swing.*;


/**
 * @version  February 9, 2009
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
    
    private JLabel contourLabel;
    
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
    
    private int blueMin = 1;
    
    private JLabel blueMinLabel;
    
    private JTextField blueMinText;
    
    private int blueMax = 20;
    
    private JLabel blueMaxLabel;
    
    private JTextField blueMaxText;
    
    /* Margin by which red exceeds green and blue */
    private int redMargin = 1;
    
    private JLabel redMarginLabel;
    
    private JTextField redMarginText;
    
    private int greenMargin = 1;
    
    private JLabel greenMarginLabel;
    
    private JTextField greenMarginText;
    
    private int blueMargin = 1;
    
    private JLabel blueMarginLabel;
    
    private JTextField blueMarginText;
    
    /** DOCUMENT ME! */
    private ModelImage image; // source image   

    /** DOCUMENT ME! */
    private PlugInAlgorithmSynapseDetection synapseDetectionAlgo = null;

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
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
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
        str += blueMin + delim;
        str += blueMax + delim;
        str += redMargin + delim;
        str += greenMargin + delim;
        str += blueMargin;
        
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
     * Accessor that sets the blueMin variable, for minimum blue pixel width.
     *
     * @param  blueMin  minimum number of blue pixels
     */
    public void setBlueMin(int blueMin) {
        this.blueMin = blueMin;
    }
    
    /**
     * Accessor that sets the blueMax variable, for maximum blue pixel width.
     *
     * @param  blueMax  maximum number of blue pixels
     */
    public void setBlueMax(int blueMax) {
        this.blueMax = blueMax;
    }
    
    /**
     * Accessor that sets the redMargin variable, for minimum red margin over green, blue.
     *
     * @param  redMargin  minimum red margin over green, blue
     */
    public void setRedMargin(int redMargin) {
        this.redMargin = redMargin;
    }
    
    /**
     * Accessor that sets the greenMargin variable, for minimum green margin over red, blue.
     *
     * @param  greenMargin  minimum green margin over red, blue
     */
    public void setGreenMargin(int greenMargin) {
        this.greenMargin = greenMargin;
    }
    
    /**
     * Accessor that sets the blueMargin variable, for minimum blue margin over red, green.
     *
     * @param  blueMargin  minimum blue margin over red, green
     */
    public void setBlueMargin(int blueMargin) {
        this.blueMargin = blueMargin;
    }
    
    /**
     * Once all the necessary variables are set, call the Synapse Detection algorithm.
     */
    protected void callAlgorithm() {

        try {

            synapseDetectionAlgo = new PlugInAlgorithmSynapseDetection(image, redMin, redMax, greenMin, greenMax,
                                                                   blueMin, blueMax, redMargin, greenMargin,
                                                                   blueMargin);

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
        setBlueMin(scriptParameters.getParams().getInt("blue_min"));
        setBlueMax(scriptParameters.getParams().getInt("blue_max"));
        setRedMargin(scriptParameters.getParams().getInt("red_margin"));
        setGreenMargin(scriptParameters.getParams().getInt("green_margin"));
        setBlueMargin(scriptParameters.getParams().getInt("blue_margin"));
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
        scriptParameters.getParams().put(ParameterFactory.newParameter("blue_min", blueMin));
        scriptParameters.getParams().put(ParameterFactory.newParameter("blue_max", blueMax));
        scriptParameters.getParams().put(ParameterFactory.newParameter("red_margin", redMargin));
        scriptParameters.getParams().put(ParameterFactory.newParameter("green_margin", greenMargin));
        scriptParameters.getParams().put(ParameterFactory.newParameter("blue_margin", blueMargin));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Synapse Detection  02/09/09");

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
        
        blueMinLabel = new JLabel("Minimum blue pixel width");
        blueMinLabel.setForeground(Color.black);
        blueMinLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(blueMinLabel, gbc);

        blueMinText = new JTextField(10);
        blueMinText.setText("1");
        blueMinText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(blueMinText, gbc);
        
        blueMaxLabel = new JLabel("Maximum blue pixel width");
        blueMaxLabel.setForeground(Color.black);
        blueMaxLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(blueMaxLabel, gbc);
        
        blueMaxText = new JTextField(10);
        blueMaxText.setText("20");
        blueMaxText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(blueMaxText, gbc);
        
        redMarginLabel = new JLabel("Minimum red margin over green, blue");
        redMarginLabel.setForeground(Color.black);
        redMarginLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(redMarginLabel, gbc);

        redMarginText = new JTextField(10);
        redMarginText.setText("1");
        redMarginText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(redMarginText, gbc);
        
        greenMarginLabel = new JLabel("Minimum green margin over red, blue");
        greenMarginLabel.setForeground(Color.black);
        greenMarginLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(greenMarginLabel, gbc);

        greenMarginText = new JTextField(10);
        greenMarginText.setText("1");
        greenMarginText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(greenMarginText, gbc);
        
        blueMarginLabel = new JLabel("Minimum blue margin over red, green");
        blueMarginLabel.setForeground(Color.black);
        blueMarginLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(blueMarginLabel, gbc);

        blueMarginText = new JTextField(10);
        blueMarginText.setText("1");
        blueMarginText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(blueMarginText, gbc);

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
        
        tmpStr = blueMinText.getText();
        blueMin = Integer.parseInt(tmpStr);

        if (blueMin < 1) {
            MipavUtil.displayError("blue minimum must be at least 1");
            blueMinText.requestFocus();
            blueMinText.selectAll();

            return false;
        } else if (blueMin > 200) {
            MipavUtil.displayError("blue minimum must not exceed 200");
            blueMinText.requestFocus();
            blueMinText.selectAll();

            return false;
        }
        
        tmpStr = blueMaxText.getText();
        blueMax = Integer.parseInt(tmpStr);

        if (blueMax < blueMin) {
            MipavUtil.displayError("blue maximum must be at least blue minimum");
            blueMaxText.requestFocus();
            blueMaxText.selectAll();

            return false;
        } else if (blueMax > 500) {
            MipavUtil.displayError("blue maximum must not exceed 500");
            blueMaxText.requestFocus();
            blueMaxText.selectAll();

            return false;
        }
        
        tmpStr = redMarginText.getText();
        redMargin = Integer.parseInt(tmpStr);

        if (redMargin < 1) {
            MipavUtil.displayError("red margin must be at least 1");
            redMarginText.requestFocus();
            redMarginText.selectAll();

            return false;
        } else if (redMargin > 255) {
            MipavUtil.displayError("red margin must not exceed 255");
            redMarginText.requestFocus();
            redMarginText.selectAll();

            return false;
        }
        
        tmpStr = greenMarginText.getText();
        greenMargin = Integer.parseInt(tmpStr);

        if (greenMargin < 1) {
            MipavUtil.displayError("green margin must be at least 1");
            greenMarginText.requestFocus();
            greenMarginText.selectAll();

            return false;
        } else if (greenMargin > 255) {
            MipavUtil.displayError("green margin must not exceed 255");
            greenMarginText.requestFocus();
            greenMarginText.selectAll();

            return false;
        }
        
        tmpStr = blueMarginText.getText();
        blueMargin = Integer.parseInt(tmpStr);

        if (blueMargin < 1) {
            MipavUtil.displayError("blue margin must be at least 1");
            blueMarginText.requestFocus();
            blueMarginText.selectAll();

            return false;
        } else if (blueMargin > 255) {
            MipavUtil.displayError("blue margin must not exceed 255");
            blueMarginText.requestFocus();
            blueMarginText.selectAll();

            return false;
        }
        
        return true;
    } // end setVariables()

}
