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
 * @version  April 11, 2008
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInDialogCenterDistance.java $ $Revision: 21 $ $Date: 1/25/06 4:59p $
 *           </p>
 */
public class PlugInDialogCenterDistance extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private JLabel greenMergingLabel;
    
    /** Green merging radius around peak green spot */
    private JTextField greenMergingText;
    
    /** Minimum nuber of pixels in a green VOI */
    private int greenMin;
    
    private JLabel greenMinLabel;
    
    private JTextField greenMinText;
    
    /** DOCUMENT ME! */
    private float greenFraction = 0.15f;

    /** DOCUMENT ME! */
    private JLabel greenFractionLabel;

    /** DOCUMENT ME! */
    private JTextField greenFractionText;
    
    private JLabel cellNumberLabel;
    
    private JTextField cellNumberText;
    
    private JLabel greenRegionsLabel;
    
    private ButtonGroup greenGroup;
    
    private JRadioButton oneButton;
    
    private JRadioButton twoButton;
    
    private JRadioButton variesButton;
    
    private float mergingDistance = 1.0f;
    
    private int cellNumber;
    
    // Number of green regions per cell
    // Either 1 for 1 for all cells, 2 for 2 for all cells, or 0 for 1 or 2 for all cells
    private int greenRegionNumber;
    
    
    /** DOCUMENT ME! */
    private ModelImage image; // source image

    

    /** DOCUMENT ME! */
    private PlugInAlgorithmCenterDistance centerDistanceAlgo = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogCenterDistance() { }

    /**
     * Creates new dialog for distances within a cell from the geometric center using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogCenterDistance(Frame theParentFrame, ModelImage im) {
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

        if (algorithm instanceof PlugInAlgorithmCenterDistance) {
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
        str += mergingDistance + delim;
        str += greenMin + delim;
        str += greenFraction + delim;
        str += cellNumber + delim;
        str += greenRegionNumber + delim;

        return str;
    }

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
     * Accessor that set the cellNumber variable
     *
     * @param  cellNumber float
     */
    public void setCellNumber(int cellNumber) {
        this.cellNumber = cellNumber;
    }

    /**
     * Accessor that sets the greenRegionNumber variable, for number of green regions per cell
     * 1 for 1 for all cells, 2 for 2 for all cells, or 0 for 1 or 2 for all cells
     *
     * @param  greenRegionNumber
     */
    public void setGreenRegionNumber(int greenRegionNumber) {
        this.greenRegionNumber = greenRegionNumber;
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        try {

            centerDistanceAlgo = new PlugInAlgorithmCenterDistance(image, mergingDistance, greenMin, greenFraction,
                                                                   cellNumber, greenRegionNumber);

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

        setMergingDistance(scriptParameters.getParams().getFloat("merging_distance"));
        setGreenMin(scriptParameters.getParams().getInt("green_min"));
        setGreenFraction(scriptParameters.getParams().getFloat("green_fraction"));
        setCellNumber(scriptParameters.getParams().getInt("cell_number"));
        setGreenRegionNumber(scriptParameters.getParams().getInt("green_region_number"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        scriptParameters.getParams().put(ParameterFactory.newParameter("merging_distance", mergingDistance));
        scriptParameters.getParams().put(ParameterFactory.newParameter("green_min", greenMin));
        scriptParameters.getParams().put(ParameterFactory.newParameter("green_fraction", greenFraction));
        scriptParameters.getParams().put(ParameterFactory.newParameter("cell_number", cellNumber));
        scriptParameters.getParams().put(ParameterFactory.newParameter("green_region_number", greenRegionNumber));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Center Distances 04/11/08");

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

        JLabel labelVOI = new JLabel("Optionally draw a new VOI around each cell");
        labelVOI.setForeground(Color.black);
        labelVOI.setFont(serif12);
        mainPanel.add(labelVOI, gbc);

        JLabel labelVOI2 = new JLabel("Program automatically generates VOIs if none present");
        labelVOI2.setForeground(Color.black);
        labelVOI2.setFont(serif12);
        gbc.gridy = yPos++;
        mainPanel.add(labelVOI2, gbc);
        
        greenMergingLabel = new JLabel("Green merging radius around peak (microns)");
        greenMergingLabel.setForeground(Color.black);
        greenMergingLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(greenMergingLabel, gbc);

        greenMergingText = new JTextField(5);
        greenMergingText.setText("1.0");
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
        greenFractionText.setText("0.15");
        greenFractionText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(greenFractionText, gbc);

        cellNumberLabel = new JLabel("Number of cells");
        cellNumberLabel.setForeground(Color.black);
        cellNumberLabel.setFont(serif12);
        gbc.gridy = yPos;
        mainPanel.add(cellNumberLabel, gbc);

        cellNumberText = new JTextField(5);
        cellNumberText.setText("1");
        cellNumberText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(cellNumberText, gbc);

        greenRegionsLabel = new JLabel("Green regions per cell");
        greenRegionsLabel.setForeground(Color.black);
        greenRegionsLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos++;
        mainPanel.add(greenRegionsLabel, gbc);
        
        greenGroup = new ButtonGroup();
        
        oneButton = new JRadioButton("1", true);
        oneButton.setForeground(Color.black);
        oneButton.setFont(serif12);
        greenGroup.add(oneButton);
        gbc.gridx = 0;
        gbc.gridy = yPos++;
        mainPanel.add(oneButton, gbc);
        
        twoButton = new JRadioButton("2", true);
        twoButton.setForeground(Color.black);
        twoButton.setFont(serif12);
        greenGroup.add(twoButton);
        gbc.gridx = 0;
        gbc.gridy = yPos++;
        mainPanel.add(twoButton, gbc);
        
        variesButton = new JRadioButton("Varies with cell", true);
        variesButton.setForeground(Color.black);
        variesButton.setFont(serif12);
        greenGroup.add(variesButton);
        gbc.gridx = 0;
        gbc.gridy = yPos++;
        mainPanel.add(variesButton, gbc);

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
        ViewVOIVector VOIs;
        int nVOIs;
        int i;
        
        int totLength = image.getExtents()[0];

        for (i = 1; i < image.getNDims(); i++) {
            totLength *= image.getExtents()[i];
        }
        
        VOIs = image.getVOIs();
        nVOIs = VOIs.size();
        
        for (i = nVOIs - 1; i >=0; i--) {

            if (VOIs.VOIAt(i).getCurveType() != VOI.CONTOUR) {
                VOIs.remove(i);
            }
        }
        
        nVOIs = VOIs.size();
        
        tmpStr = greenMergingText.getText();
        mergingDistance = Float.parseFloat(tmpStr);
        if (mergingDistance <= 0.0f) {
            MipavUtil.displayError("Merging distance must be greater than 0");
            greenMergingText.requestFocus();
            greenMergingText.selectAll();
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
        
        
        if (nVOIs > 0) {
            cellNumber = nVOIs;
        }
        else { // nVOIs = 0
            tmpStr = cellNumberText.getText();
            cellNumber = Integer.parseInt(tmpStr);
            if (cellNumber <= 0) {
                MipavUtil.displayError("Number of cells must be greater than 0");
                cellNumberText.requestFocus();
                cellNumberText.selectAll();
                return false;    
            }
        } // else nVOIs = 0
        
        if (oneButton.isSelected()) {
            greenRegionNumber = 1;
        }
        else if (twoButton.isSelected()) {
            greenRegionNumber = 2;
        }
        else {
            greenRegionNumber = 0;
        }
        
        return true;
    } // end setVariables()

}
