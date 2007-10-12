import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * @version  March 29, 2004
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInDialogRegionDistance.java $ $Revision: 21 $ $Date: 1/25/06 4:59p $
 *           </p>
 */
public class PlugInDialogRegionDistance extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2063809955712228256L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JLabel blueLabel;

    /** DOCUMENT ME! */
    private int blueMin;

    /** DOCUMENT ME! */
    private JTextField blueText;

    /** DOCUMENT ME! */
    private float greenFraction = 0.15f;

    /** DOCUMENT ME! */
    private JLabel greenFractionLabel;

    /** DOCUMENT ME! */
    private JTextField greenFractionText;

    /** DOCUMENT ME! */
    private JLabel greenLabel;

    /** DOCUMENT ME! */
    private int greenMin;

    /** DOCUMENT ME! */
    private int greenNumber;

    /** DOCUMENT ME! */
    private JLabel greenNumberLabel;

    /** DOCUMENT ME! */
    private JTextField greenNumberText;

    /** DOCUMENT ME! */
    private JTextField greenText;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private int iters;

    /** DOCUMENT ME! */
    private JLabel itersLabel;

    /** DOCUMENT ME! */
    private JTextField itersText;

    /** DOCUMENT ME! */
    private float redFraction = 0.25f;

    /** DOCUMENT ME! */
    private JLabel redFractionLabel;

    /** DOCUMENT ME! */
    private JTextField redFractionText;

    /** DOCUMENT ME! */
    private JLabel redLabel;

    /** DOCUMENT ME! */
    private int redMin;

    /** DOCUMENT ME! */
    private int redNumber;

    /** DOCUMENT ME! */
    private JLabel redNumberLabel;

    /** DOCUMENT ME! */
    private JTextField redNumberText;

    /** DOCUMENT ME! */
    private JTextField redText;

    /** DOCUMENT ME! */
    private PlugInAlgorithmRegionDistance regionDistAlgo = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogRegionDistance() { }

    /**
     * Creates new dialog for region distances within a cell using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogRegionDistance(Frame theParentFrame, ModelImage im) {
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

        if (algorithm instanceof PlugInAlgorithmRegionDistance) {
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
        str += redFraction + delim;
        str += redNumber + delim;
        str += greenMin + delim;
        str += greenFraction + delim;
        str += greenNumber + delim;
        str += blueMin + delim;
        str += iters;

        return str;
    }

    /**
     * Accessor that sets the blueMin variable, for minimum pixel number in a nucleus.
     *
     * @param  blueMin  the minimum number of pixels in a nucleus
     */
    public void setBlueMin(int blueMin) {
        this.blueMin = blueMin;
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
     * Accessor that sets the greenNumber variable, for number of green vois in a nucleus.
     *
     * @param  greenNumber  the number of green vois in a nucleus
     */
    public void setGreenNumber(int greenNumber) {
        this.greenNumber = greenNumber;
    }

    /**
     * Accessor that sets the number of erosion and dilation iterations.
     *
     * @param  iters  int
     */
    public void setIters(int iters) {
        this.iters = iters;
    }

    /**
     * Accessor that set the redFraction variable, for portion of red pixels considered by fuzzy c means segmentation.
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
     * Accessor that sets the redNumber variable, for number of red vois in a nucleus.
     *
     * @param  redNumber  the number of red vois in a nucleus
     */
    public void setRedNumber(int redNumber) {
        this.redNumber = redNumber;
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        try {

            regionDistAlgo = new PlugInAlgorithmRegionDistance(image, redMin, redFraction, redNumber, greenMin,
                                                               greenFraction, greenNumber, blueMin, iters);

            // This is very important. Adding this object as a listener allows
            // the algorithm to
            // notify this object when it has completed or failed. See algorithm
            // performed event.
            // This is made possible by implementing AlgorithmedPerformed
            // interface
            regionDistAlgo.addListener(this);
            createProgressBar(image.getImageName(), " ...", regionDistAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (regionDistAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                regionDistAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Region Distance: unable to allocate enough memory");

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
        setRedFraction(scriptParameters.getParams().getFloat("red_fraction"));
        setRedNumber(scriptParameters.getParams().getInt("red_vois_per_nucleus"));
        setGreenMin(scriptParameters.getParams().getInt("green_min"));
        setGreenFraction(scriptParameters.getParams().getFloat("green_fraction"));
        setGreenNumber(scriptParameters.getParams().getInt("green_vois_per_nucleus"));
        setBlueMin(scriptParameters.getParams().getInt("blue_min"));
        setIters(scriptParameters.getNumIterations());
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        scriptParameters.getParams().put(ParameterFactory.newParameter("red_min", redMin));
        scriptParameters.getParams().put(ParameterFactory.newParameter("red_fraction", redFraction));
        scriptParameters.getParams().put(ParameterFactory.newParameter("red_vois_per_nucleus", redNumber));
        scriptParameters.getParams().put(ParameterFactory.newParameter("green_min", greenMin));
        scriptParameters.getParams().put(ParameterFactory.newParameter("green_fraction", greenFraction));
        scriptParameters.getParams().put(ParameterFactory.newParameter("green_vois_per_nucleus", greenNumber));
        scriptParameters.getParams().put(ParameterFactory.newParameter("blue_min", blueMin));
        scriptParameters.storeNumIterations(iters);
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Region Distances 12/22/05");

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

        JLabel labelVOI = new JLabel("Select a new VOI for each region");
        labelVOI.setForeground(Color.black);
        labelVOI.setFont(serif12);
        mainPanel.add(labelVOI, gbc);

        if (image.getNDims() > 2) {
            JLabel labelSlices = new JLabel("Propagate VOIs to desired slices");
            labelSlices.setForeground(Color.black);
            labelSlices.setFont(serif12);
            gbc.gridy = yPos++;
            mainPanel.add(labelSlices, gbc);
        } // if (image.getNDims() > 2)

        JLabel labelVOI2 = new JLabel("Program automatically generates VOIs if none present");
        labelVOI2.setForeground(Color.black);
        labelVOI2.setFont(serif12);
        gbc.gridy = yPos++;
        mainPanel.add(labelVOI2, gbc);

        redLabel = new JLabel("Minimum red pixel count");
        redLabel.setForeground(Color.black);
        redLabel.setFont(serif12);
        gbc.gridy = yPos;
        mainPanel.add(redLabel, gbc);

        redText = new JTextField(5);
        redText.setText("100");
        redText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(redText, gbc);

        redFractionLabel = new JLabel("Fraction of red pixels to consider");
        redFractionLabel.setForeground(Color.black);
        redFractionLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(redFractionLabel, gbc);

        redFractionText = new JTextField(5);
        redFractionText.setText("0.25");
        redFractionText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(redFractionText, gbc);

        redNumberLabel = new JLabel("Red vois per nucleus");
        redNumberLabel.setForeground(Color.black);
        redNumberLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(redNumberLabel, gbc);

        redNumberText = new JTextField(5);
        redNumberText.setText("2");
        redNumberText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(redNumberText, gbc);

        greenLabel = new JLabel("Minimum green pixel count");
        greenLabel.setForeground(Color.black);
        greenLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(greenLabel, gbc);

        greenText = new JTextField(5);
        greenText.setText("100");
        greenText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(greenText, gbc);

        greenFractionLabel = new JLabel("Fraction of green pixels to consider");
        greenFractionLabel.setForeground(Color.black);
        greenFractionLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(greenFractionLabel, gbc);

        greenFractionText = new JTextField(5);
        greenFractionText.setText("0.15");
        greenFractionText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(greenFractionText, gbc);

        greenNumberLabel = new JLabel("Green vois per nucleus");
        greenNumberLabel.setForeground(Color.black);
        greenNumberLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(greenNumberLabel, gbc);

        greenNumberText = new JTextField(5);
        greenNumberText.setText("2");
        greenNumberText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(greenNumberText, gbc);

        blueLabel = new JLabel("Minimum blue pixel count");
        blueLabel.setForeground(Color.black);
        blueLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(blueLabel, gbc);

        blueText = new JTextField(5);
        blueText.setText("1000");
        blueText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(blueText, gbc);

        itersLabel = new JLabel("Erosion and 1/6 dilation iterations");
        itersLabel.setForeground(Color.black);
        itersLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(itersLabel, gbc);

        itersText = new JTextField(5);

        if (image.getNDims() > 2) {
            itersText.setText("6");
        } else {
            itersText.setText("8");
        }

        itersText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(itersText, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);

        // Build the Panel that holds the OK and CANCEL Buttons
        JPanel OKCancelPanel = new JPanel();

        // size and place the OK button
        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        // size and place the CANCEL button
        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

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

        tmpStr = redText.getText();
        redMin = Integer.parseInt(tmpStr);

        if (redMin < 1) {
            MipavUtil.displayError("red minimum must be at least 1");
            redText.requestFocus();
            redText.selectAll();

            return false;
        } else if (redMin > totLength) {
            MipavUtil.displayError("red minimum must not exceed " + totLength);
            redText.requestFocus();
            redText.selectAll();

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

        tmpStr = redNumberText.getText();
        redNumber = Integer.parseInt(tmpStr);

        if (redNumber < 0) {
            MipavUtil.displayError("red vois per nucleus must be at least 0");
            redNumberText.requestFocus();
            redNumberText.selectAll();

            return false;
        } else if (redNumber > 10) {
            MipavUtil.displayError("red vois per nucleus must not exceed 10");
            redNumberText.requestFocus();
            redNumberText.selectAll();

            return false;
        }

        tmpStr = greenText.getText();
        greenMin = Integer.parseInt(tmpStr);

        if (greenMin < 1) {
            MipavUtil.displayError("green minimum must be at least 1");
            greenText.requestFocus();
            greenText.selectAll();

            return false;
        } else if (greenMin > totLength) {
            MipavUtil.displayError("green minimum must not exceed " + totLength);
            greenText.requestFocus();
            greenText.selectAll();

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

        tmpStr = greenNumberText.getText();
        greenNumber = Integer.parseInt(tmpStr);

        if (greenNumber < 0) {
            MipavUtil.displayError("green vois per nucleus must be at least 0");
            greenNumberText.requestFocus();
            greenNumberText.selectAll();

            return false;
        } else if (greenNumber > 10) {
            MipavUtil.displayError("green vois per nucleus must not exceed 10");
            greenNumberText.requestFocus();
            greenNumberText.selectAll();

            return false;
        }

        tmpStr = blueText.getText();
        blueMin = Integer.parseInt(tmpStr);

        if (blueMin < 1) {
            MipavUtil.displayError("blue minimum must be at least 1");
            blueText.requestFocus();
            blueText.selectAll();

            return false;
        } else if (blueMin > totLength) {
            MipavUtil.displayError("blue minimum must not exceed " + totLength);
            blueText.requestFocus();
            blueText.selectAll();

            return false;
        }

        tmpStr = itersText.getText();
        iters = Integer.parseInt(tmpStr);

        if (iters < 0) {
            MipavUtil.displayError("iters must be at least 0");
            itersText.requestFocus();
            itersText.selectAll();

            return false;
        } else if (iters > 100) {
            MipavUtil.displayError("iters must not exceed 100");
            itersText.requestFocus();
            itersText.selectAll();

            return false;
        }

        return true;
    } // end setVariables()

}
