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
 * @version  May 9, 2013
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 *           <p>$Logfile:
 *           </p>
 */
public class PlugInDialogNucleiDeformation extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private JTextField minSizeText;
    
    private int minSize = 100;
    
    private JTextField maxSizeText;
    
    private int maxSize = 1000000;
    
    private ModelImage image;

    private PlugInAlgorithmNucleiDeformation nucleiDeformAlgo = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogNucleiDeformation() { }

    /**
     * Creates new dialog for distances within a cell from the geometric center using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogNucleiDeformation(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        if (im.isColorImage()) {
            MipavUtil.displayError("Source Image must be black and white");
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

        if (algorithm instanceof PlugInAlgorithmNucleiDeformation) {
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
        str += minSize + delim;
        str += maxSize + delim;

        return str;
    }
    
    /**
     * 
     * @param minSize
     */
    public void setMinSize(int minSize) {
        this.minSize = minSize;
    }
    
    /**
     * 
     * @param maxSize
     */
    public void setMaxSize(int maxSize) {
        this.maxSize = maxSize;
    }

   
    /**
     * Once all the necessary variables are set, call PluginAlgorithmNucleiDeformation
     */
    protected void callAlgorithm() {

        try {

            nucleiDeformAlgo = new PlugInAlgorithmNucleiDeformation(image, minSize, maxSize);

            // This is very important. Adding this object as a listener allows
            // the algorithm to
            // notify this object when it has completed or failed. See algorithm
            // performed event.
            // This is made possible by implementing AlgorithmedPerformed
            // interface
            nucleiDeformAlgo.addListener(this);
            createProgressBar(image.getImageName(), " ...", nucleiDeformAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (nucleiDeformAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                nucleiDeformAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Nuclei Deformation: unable to allocate enough memory");

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

        if (image.isColorImage()) {
            throw new ParameterException(AlgorithmParameters.getInputImageLabel(1), "Source Image must be black and white");
        }

        setMinSize(scriptParameters.getParams().getInt("min_size"));
        setMaxSize(scriptParameters.getParams().getInt("max_size"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.getParams().put(ParameterFactory.newParameter("min_size", minSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_size", maxSize));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        JLabel minSizeLabel;
        JLabel maxSizeLabel;
        setForeground(Color.black);
        setTitle("Nuclei Deformation 05/09/2013");
        int length = image.getExtents()[0] * image.getExtents()[1];

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
        
        minSizeLabel = new JLabel("Minimum nucleus pixel count");
        minSizeLabel.setForeground(Color.black);
        minSizeLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(minSizeLabel, gbc);

        minSizeText = new JTextField(8);
        minSizeText.setText(String.valueOf(minSize));
        minSizeText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(minSizeText, gbc);
        
        maxSizeLabel = new JLabel("Maximum nucleus pixel count");
        maxSizeLabel.setForeground(Color.black);
        maxSizeLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(maxSizeLabel, gbc);

        maxSizeText = new JTextField(8);
        maxSizeText.setText(String.valueOf(length));
        maxSizeText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(maxSizeText, gbc);
        
        

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
        
        int length = image.getExtents()[0] * image.getExtents()[1];
        
        tmpStr = minSizeText.getText();
        minSize = Integer.parseInt(tmpStr);

        if (minSize < 1) {
            MipavUtil.displayError("Nucleus minimum pixel size must be at least 1");
            minSizeText.requestFocus();
            minSizeText.selectAll();

            return false;
        } else if (minSize > length) {
            MipavUtil.displayError("Nucleus minimum pixel size must not exceed " + length);
            minSizeText.requestFocus();
            minSizeText.selectAll();

            return false;
        }
        
        tmpStr = maxSizeText.getText();
        maxSize = Integer.parseInt(tmpStr);

        if (maxSize < minSize) {
            MipavUtil.displayError("Nucleus maximum pixel size must be at least " + minSize);
            maxSizeText.requestFocus();
            maxSizeText.selectAll();

            return false;
        } else if (maxSize > length) {
            MipavUtil.displayError("Nucleus maximum pixel size must not exceed " + length);
            maxSizeText.requestFocus();
            maxSizeText.selectAll();

            return false;
        }
        
        return true;
    } // end setVariables()

}
