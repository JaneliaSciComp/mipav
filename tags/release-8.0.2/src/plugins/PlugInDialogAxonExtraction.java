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
 * @version  March 26, 2009
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInDialogAxonExtraction.java $ $Revision: 21 $ $Date: 1/25/06 4:59p $
 *           </p>
 */
public class PlugInDialogAxonExtraction extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    private int redRadius = 3;
    
    private JLabel redRadiusLabel;
    
    private JTextField redRadiusText;
    
    private int greenRadius = 3;
    
    private JLabel greenRadiusLabel;
    
    private JTextField greenRadiusText;
    
   
    /** DOCUMENT ME! */
    private ModelImage image; // source image   

    /** DOCUMENT ME! */
    private PlugInAlgorithmAxonExtraction axonExtractionAlgo = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogAxonExtraction() { }

    /**
     * Creates new dialog for distances within a cell from the geometric center using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogAxonExtraction(Frame theParentFrame, ModelImage im) {
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
        }  else {
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
        str += redRadius + delim;
        str += greenRadius;
        
        return str;
    }
    
    /**
     * Accessor that sets the redRadius variable, for preprocessing morphology.
     *
     * @param  redRadius
     */
    public void setRedRadius(int redRadius) {
        this.redRadius = redRadius;
    }
    
    /**
     * Accessor that sets the greenRadius variable, for preprocessing morphology.
     *
     * @param  greenRadius
     */
    public void setGreenRadius(int greenRadius) {
        this.greenRadius = greenRadius;
    }
    
    /**
     * Once all the necessary variables are set, call the Axon extraction algorithm.
     */
    protected void callAlgorithm() {

        try {

            axonExtractionAlgo = new PlugInAlgorithmAxonExtraction(image, redRadius, greenRadius);

            // This is very important. Adding this object as a listener allows
            // the algorithm to
            // notify this object when it has completed or failed. See algorithm
            // performed event.
            // This is made possible by implementing AlgorithmedPerformed
            // interface
            axonExtractionAlgo.addListener(this);
            createProgressBar(image.getImageName(), " ...",axonExtractionAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (axonExtractionAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                axonExtractionAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Axon extraction dialog: unable to allocate enough memory");

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
        setRedRadius(scriptParameters.getParams().getInt("red_radius"));
        setGreenRadius(scriptParameters.getParams().getInt("green_radius"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.getParams().put(ParameterFactory.newParameter("red_radius", redRadius));
        scriptParameters.getParams().put(ParameterFactory.newParameter("green_radius", greenRadius));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Axon Extraction  03/26/09");
     
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
        
        JLabel infoLabel = new JLabel("Radius = 0 for no preprocessing");
        infoLabel.setForeground(Color.black);
        infoLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos++;
        mainPanel.add(infoLabel, gbc);
        
        redRadiusLabel = new JLabel("Preprocessing red strucure element radius");
        redRadiusLabel.setForeground(Color.black);
        redRadiusLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(redRadiusLabel, gbc);

        redRadiusText = new JTextField(10);
        redRadiusText.setText("3");
        redRadiusText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(redRadiusText, gbc);
        
        greenRadiusLabel = new JLabel("Preprocessing green strucure element radius");
        greenRadiusLabel.setForeground(Color.black);
        greenRadiusLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(greenRadiusLabel, gbc);

        greenRadiusText = new JTextField(10);
        greenRadiusText.setText("3");
        greenRadiusText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(greenRadiusText, gbc);

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
        
        tmpStr = redRadiusText.getText();
        redRadius = Integer.parseInt(tmpStr);

        if (redRadius > 200) {
            MipavUtil.displayError("red radius must not exceed 200");
            redRadiusText.requestFocus();
            redRadiusText.selectAll();

            return false;
        }
        
        tmpStr = greenRadiusText.getText();
        greenRadius = Integer.parseInt(tmpStr);

        if (greenRadius > 200) {
            MipavUtil.displayError("green radius must not exceed 200");
            greenRadiusText.requestFocus();
            greenRadiusText.selectAll();

            return false;
        }
        
        return true;
    } // end setVariables()

}
