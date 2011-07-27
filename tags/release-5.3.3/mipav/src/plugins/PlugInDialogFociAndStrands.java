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
 * @version  September 16, 2008
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInDialogFociAndStrands.java $ $Revision: 21 $ $Date: 1/25/06 4:59p $
 *           </p>
 */
public class PlugInDialogFociAndStrands extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    private JLabel contourLabel;
    
    /** Minimum number of pixels in a focus */
    private int redMin = 1;
    
    private JLabel redMinLabel;
    
    private JTextField redMinText;
    
    private JLabel redFractionLabel;
    
    private JTextField redFractionText;
    
    /** Focus requires a minimum red value >=
     *  image red min + redFraction * (image red max - image red min)
     */
    private float redFraction = 0.30f;
    
    /** Minimum number of pixels in a green strand */
    private int greenMin = 50;
    
    private JLabel greenFractionLabel;
    
    private JTextField greenFractionText;
    
    /** Strand requires a minimum green value >=
     *  image green min + greenFraction * (image green max - image green min)
     */
    private float greenFraction = 0.10f;
    
    private JLabel greenMinLabel;
    
    private JTextField greenMinText;
    
    private JLabel radiusLabel;
    
    private JTextField radiusText;
    
    /** Radius of circles drawn around colocalized foci */
    private float radius;
    
    private int minLength;
    
    /** DOCUMENT ME! */
    private ModelImage image; // source image   

    /** DOCUMENT ME! */
    private PlugInAlgorithmFociAndStrands fociAndStrandsAlgo = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogFociAndStrands() { }

    /**
     * Creates new dialog for distances within a cell from the geometric center using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogFociAndStrands(Frame theParentFrame, ModelImage im) {
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
        str += redFraction + delim;
        str += greenMin + delim;
        str += greenFraction + delim;
        str += radius + delim;

        return str;
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
     * Accessor that sets the redFraction variable, where red threshold =
     * image red min + redFraction*(image red max - image red min)
     * @param redFraction
     */
    public void setRedFraction(float redFraction) {
        this.redFraction = redFraction;
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
     * Accessor that sets the greenFraction variable, where green threshold =
     * image green min + greenFraction*(image green max - image green min)
     * @param greenFraction
     */
    public void setGreenFraction(float greenFraction) {
        this.greenFraction = greenFraction;
    }
    
    /**
     * Accesor that sets the radius variable, the radius of circle
     * drawn around colocalized foci
     * @param radius
     */
    public void setRadius(float radius) {
        this.radius = radius;
    }

    
    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        try {

            fociAndStrandsAlgo = new PlugInAlgorithmFociAndStrands(image, redMin, redFraction, greenMin, greenFraction,
                                                                   radius);

            // This is very important. Adding this object as a listener allows
            // the algorithm to
            // notify this object when it has completed or failed. See algorithm
            // performed event.
            // This is made possible by implementing AlgorithmedPerformed
            // interface
            fociAndStrandsAlgo.addListener(this);
            createProgressBar(image.getImageName(), " ...", fociAndStrandsAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (fociAndStrandsAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                fociAndStrandsAlgo.run();
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
        setRedMin(scriptParameters.getParams().getInt("red_min"));
        setRedFraction(scriptParameters.getParams().getFloat("red_fraction"));
        setGreenMin(scriptParameters.getParams().getInt("green_min"));
        setGreenFraction(scriptParameters.getParams().getFloat("green_fraction"));
        setRadius(scriptParameters.getParams().getFloat("circle_radius"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.getParams().put(ParameterFactory.newParameter("red_min", redMin));
        scriptParameters.getParams().put(ParameterFactory.newParameter("red_fraction", redFraction));
        scriptParameters.getParams().put(ParameterFactory.newParameter("green_min", greenMin));
        scriptParameters.getParams().put(ParameterFactory.newParameter("green_fraction", greenFraction));
        scriptParameters.getParams().put(ParameterFactory.newParameter("circle_radius", radius));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        DecimalFormat df = new DecimalFormat("0.00");
        setForeground(Color.black);
        setTitle("Foci and Strands  09/16/08");

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
        
        contourLabel = new JLabel("Draw a contour VOI to limit the region analyzed");
        contourLabel.setForeground(Color.black);
        contourLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos++;
        mainPanel.add(contourLabel, gbc);
        
        redMinLabel = new JLabel("Minimum red pixel count");
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
        
        redFractionLabel = new JLabel("Fraction of red transition from image min to max at threshold");
        redFractionLabel.setForeground(Color.black);
        redFractionLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(redFractionLabel, gbc);
        
        redFractionText = new JTextField(10);
        redFractionText.setText("0.30");
        redFractionText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(redFractionText, gbc);
        
        greenMinLabel = new JLabel("Minimum green pixel count");
        greenMinLabel.setForeground(Color.black);
        greenMinLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(greenMinLabel, gbc);

        greenMinText = new JTextField(10);
        greenMinText.setText("50");
        greenMinText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(greenMinText, gbc);
        
        greenFractionLabel = new JLabel("Fraction of green transition from image min to max at threshold");
        greenFractionLabel.setForeground(Color.black);
        greenFractionLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(greenFractionLabel, gbc);
        
        greenFractionText = new JTextField(10);
        greenFractionText.setText("0.10");
        greenFractionText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(greenFractionText, gbc);
        
        radiusLabel = new JLabel("Radius of circles drawn around colocalized foci");
        radiusLabel.setForeground(Color.black);
        radiusLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(radiusLabel, gbc);
        
        minLength = Math.min(image.getExtents()[0],image.getExtents()[1]);        
        radiusText = new JTextField(10);
        if (minLength/4.0 > 4.0) {
            radius = 4.0f;
        }
        else {
            radius = minLength/4.0f;
        }
        radiusText.setText(df.format(radius));
        radiusText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(radiusText, gbc);

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
        int totLength;
        
        
        totLength = image.getExtents()[0];
        for (i = 1; i < image.getNDims(); i++) {
            totLength *= image.getExtents()[i];
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
        
        tmpStr = radiusText.getText();
        radius = Float.parseFloat(tmpStr);
        if (radius <= 0.0) {
            MipavUtil.displayError("radius must be greater than 0");
            radiusText.requestFocus();
            radiusText.selectAll();
        }
        else if (radius > minLength/2.0f) {
            MipavUtil.displayError("radius must be <= " + (minLength/2.0f));
            radiusText.requestFocus();
            radiusText.selectAll();
        }
        
        return true;
    } // end setVariables()

}
