package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog for adjusting color saturation
 */
public class JDialogColorSaturation extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    // private static final long serialVersionUID =;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmColorSaturation csAlgo;

    /** Source image. */
    private ModelImage image;

    /** Result image. */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private JPanel paramsPanel;
    
    private float a = 0.0f;
    
    private JLabel mainLabel;
    
    private JLabel mainLabel2;
    
    private JLabel mainLabel3;
    
    private JLabel mainLabel4;
    
    private JLabel mainLabel5;
    
    private JLabel aLabel;
    
    private JTextField aText;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogColorSaturation() { }

    /**
     * Construct the saturation adjustment dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogColorSaturation(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
        //loadDefaults();
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

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
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

        if (Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && (this.getOwner() != null) && !isScriptRunning()) {
            saveDefaults();
        }

        if (algorithm instanceof AlgorithmColorSaturation) {
            Preferences.debug("Color saturation: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((csAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                updateFileInfo(image, resultImage);

                resultImage.clearMask();

                try {
                    openNewFrame(resultImage);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();

            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        } // if (algorithm instanceof AlgorithmColorSaturation)

        

        if (csAlgo != null) {
            csAlgo.finalize();
            csAlgo = null;
        }

        dispose();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Saves the default settings into the Preferences file.
     */
    public void saveDefaults() {

        String defaultsString = String.valueOf(a);
        

        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }

    /**
     * Accessor that sets a.
     *
     * @param  a.
     */
    public void setA(float a) {
        this.a = a;
    }

    /**
     * Once all the necessary variables are set, call the Color Saturation
     * Adjustment algorithm based on what type of image this is
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_saturation");
        
        try {

            // Make result image
            if (image.getType() == ModelImage.ARGB) {
                resultImage = new ModelImage(ModelImage.ARGB, image.getExtents(), name);
            } else if (image.getType() == ModelImage.ARGB_USHORT) {
                resultImage = new ModelImage(ModelImage.ARGB_USHORT, image.getExtents(), name);
            } else {
                resultImage = new ModelImage(ModelImage.ARGB_FLOAT, image.getExtents(), name);
            } 

            // Make algorithm
            csAlgo = new AlgorithmColorSaturation(resultImage, image, a);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            csAlgo.addListener(this);

            createProgressBar(image.getImageName(), csAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (csAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                csAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            System.gc();
            MipavUtil.displayError("Dialog Color saturation: unable to allocate enough memory");

            return;
        }
            
    }

    /**
     * Perform any actions required after the running of the algorithm is complete.
     */
    protected void doPostAlgorithmActions() {

        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * Set up the dialog GUI based on the parameters before running the algorithm as part of a script.
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();
        
        a = scriptParameters.getParams().getFloat("a");
    }

    /**
     * Store the parameters from the dialog to record the execution of this algorithm.
     *
     * @throws  ParserException  If there is a problem creating one of the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, true);

        scriptParameters.getParams().put(ParameterFactory.newParameter("a", a));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Color saturation adjustment");
        getContentPane().setLayout(new BorderLayout());

        paramsPanel = new JPanel(new GridBagLayout());
        paramsPanel.setBorder(buildTitledBorder("Adjustment parameter"));
        
        mainLabel = new JLabel("a < 0 to decrease saturation or move towards r = g = b line");
        mainLabel.setForeground(Color.black);
        mainLabel.setFont(serif12);
        mainLabel.setEnabled(true);
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;

        gbc.gridx = 0;
        gbc.gridy = 0;
        paramsPanel.add(mainLabel, gbc);
        
        mainLabel2 = new JLabel("a = -1 sets r = g = b");
        mainLabel2.setForeground(Color.black);
        mainLabel2.setFont(serif12);
        mainLabel2.setEnabled(true);

        gbc.gridx = 0;
        gbc.gridy = 1;
        paramsPanel.add(mainLabel2, gbc);
        
        mainLabel3 = new JLabel("a = -2 complements the image in an opposing color sense");
        mainLabel3.setForeground(Color.black);
        mainLabel3.setFont(serif12);
        mainLabel3.setEnabled(true);

        gbc.gridx = 0;
        gbc.gridy = 2;
        paramsPanel.add(mainLabel3, gbc);
        
        mainLabel4 = new JLabel("a > 0 to increase saturation or move away from r = g = b line");
        mainLabel4.setForeground(Color.black);
        mainLabel4.setFont(serif12);
        mainLabel4.setEnabled(true);

        gbc.gridx = 0;
        gbc.gridy = 3;
        paramsPanel.add(mainLabel4, gbc);
        
        mainLabel5 = new JLabel("Hue and intensity remain constant");
        mainLabel5.setForeground(Color.black);
        mainLabel5.setFont(serif12);
        mainLabel5.setEnabled(true);

        gbc.gridx = 0;
        gbc.gridy = 4;
        paramsPanel.add(mainLabel5, gbc);
        
        aLabel = new JLabel("a  ");
        aLabel.setForeground(Color.black);
        aLabel.setFont(serif12);
        aLabel.setEnabled(true);
        
        gbc.gridx = 0;
        gbc.gridy = 5;
        paramsPanel.add(aLabel, gbc);
        
        aText = new JTextField(10);
        aText.setText("0.0");
        aText.setFont(serif12);
        aText.setForeground(Color.black);
        aText.setEnabled(true);
        
        gbc.gridx = 1;
        gbc.gridy = 5;
        paramsPanel.add(aText, gbc);

        getContentPane().add(paramsPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);

        System.gc();
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        tmpStr = aText.getText();
        if (testParameter(tmpStr, -100.0, 100.0)) {
            a = Float.valueOf(tmpStr).floatValue();
        } else {
            MipavUtil.displayError("a must be between -100.0 and 100.0");
            aText.requestFocus();
            aText.selectAll();

            return false;
        }
       
        return true;
    }
}
