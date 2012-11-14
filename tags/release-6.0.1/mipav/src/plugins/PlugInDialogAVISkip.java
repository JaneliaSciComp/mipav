import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import javax.swing.*;




/**
 * @version  April 2, 2008
 * @see      PlugInAVISkip
 * @see      AlgorithmInterface
 *
 *           
 */
public class PlugInDialogAVISkip extends JDialogScriptableBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    public static final String BROWSE = "BROWSE";
    
    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    
    /** Text field for input avi file. */
    private JTextField inText;
    
    /** Button to browse for avi file. */
    private JButton browseButton;
    
    /** Text field for output avi file. */
    private JTextField outText;
    
    /** Text field for capture time */
    private JTextField captureText;
    
    /** Text field for skip time */
    private JTextField skipText;
    
    /** Whether the dialog exited successfully. */
    private boolean successfulExit = false;
    
    /** The AVI skip plugin. */
    private PlugInAVISkip aviSkipPlugin;
    
    private String inputFileName = null;
    
    private String directory = null;
    
    private String outputFileName = null;
    
    private float captureTime;
    
    private float skipTime;
    

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogAVISkip() { }

    /**
     * Creates new dialog selecting an avi file on which only some frames will be read.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogAVISkip(boolean modal, PlugInAVISkip aviSkipPlugin) {
        super(modal);
        this.aviSkipPlugin = aviSkipPlugin;
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
        if (command.equals(BROWSE)) {

            try {
                
                JFileChooser chooser = new JFileChooser();
                chooser.setDialogTitle("Select AVI file");
                chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                chooser.setCurrentDirectory(new File(Preferences.getImageDirectory()));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.AVI));
                chooser.setMultiSelectionEnabled(false);
                
                int returnValue = chooser.showOpenDialog(this);

                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    inText.setText(chooser.getSelectedFile().getAbsolutePath());
                    inText.setToolTipText(null);
                    inputFileName = chooser.getSelectedFile().getName();
                    directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;

                    Preferences.setImageDirectory(chooser.getCurrentDirectory());
                } else {
                    return;
                }
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory");

                return;
            }

        }
        else if (command.equals("OK")) {

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


    
    
    public String getInputFileName() {
        return inputFileName;
    }
    
    public String getDirectory() {
        return directory;
    }
    
    public String getOutputFileName() {
        return outputFileName;
    }
    
    public float getCaptureTime() {
        return captureTime;
    }
    
    public float getSkipTime() {
        return skipTime;
    }

    /**
     * Accessor that indicates successful completion of dialog
     */
    public boolean isSuccessfulExit() {
        return successfulExit;
    }
    

    
    /**
     * Once all the necessary variables are set, call the cheshire to voi plugin
     */
    protected void callAlgorithm() {

        try {
        
            setVisible(false); // Hide dialog
            successfulExit = true;
            aviSkipPlugin.runPlugin();    //continues execution of plugin with successful exit
        
        } catch (OutOfMemoryError x) {
            
            MipavUtil.displayError("PlugInDialogAVISkip: unable to allocate enough memory");
            successfulExit = false;
        }

    } // end callAlgorithm()
    

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {

        //setFileName(scriptParameters.getParams().getString("file_name"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {


        //scriptParameters.getParams().put(ParameterFactory.newParameter("file_name", fileName));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        addNotify();
        setTitle("AVI skipped frame file");

        JPanel inputPanel = new JPanel(new GridBagLayout());
        inputPanel.setForeground(Color.black);
        inputPanel.setBorder(buildTitledBorder("Select AVI input file"));

        inText = new JTextField(30);
        inText.setText("Input AVI file name");
        inText.setFont(serif12);
        inText.setEnabled(false);

        browseButton = new JButton("Browse");
        browseButton.setPreferredSize(MipavUtil.defaultButtonSize);
        browseButton.setFont(serif12B);
        browseButton.setActionCommand(BROWSE);
        browseButton.addActionListener(this);
        
        
        Insets insets = new Insets(0, 2, 0, 2);
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.insets = insets;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;

        inputPanel.add(browseButton, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        inputPanel.add(inText, gbc);
        
        GridBagConstraints gbc2 = new GridBagConstraints();
        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.gridwidth = 2;
        gbc2.gridheight = 1;
        gbc2.weightx = 0;
        gbc2.fill = GridBagConstraints.NONE;
        gbc2.anchor = GridBagConstraints.WEST;
        mainPanel.add(inputPanel, gbc2);
        
        JLabel outLabel = new JLabel("AVI ouput file name");
        outLabel.setForeground(Color.black);
        outLabel.setFont(serif12);
        gbc2.gridx = 0;
        gbc2.gridy = 1;
        gbc2.gridwidth = 1;
        mainPanel.add(outLabel, gbc2);
        
        outText = new JTextField(30);
        outText.setFont(serif12);
        gbc2.gridx = 1;
        mainPanel.add(outText, gbc2);

        JLabel captureLabel = new JLabel("Capture time (sec)");
        captureLabel.setForeground(Color.black);
        captureLabel.setFont(serif12);
        gbc2.gridx = 0;
        gbc2.gridy = 2;
        mainPanel.add(captureLabel, gbc2);
        
        captureText = new JTextField(10);
        captureText.setFont(serif12);
        gbc2.gridx = 1;
        mainPanel.add(captureText, gbc2);
        
        JLabel skipLabel = new JLabel("Skip time (sec)");
        skipLabel.setForeground(Color.black);
        skipLabel.setFont(serif12);
        gbc2.gridx = 0;
        gbc2.gridy = 3;
        mainPanel.add(skipLabel, gbc2);
        
        skipText = new JTextField(10);
        skipText.setFont(serif12);
        gbc2.gridx = 1;
        mainPanel.add(skipText, gbc2);
        
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
        int index;
        String tmpStr;
        if (inputFileName == null) {
            return false;
        }
        
        if (directory == null) {
            return false;
        }
        
        outputFileName = outText.getText();
        if (outputFileName == null) {
            return false;
        }
        index = outputFileName.lastIndexOf(".");
        if (index == -1) {
            outputFileName = outputFileName + ".avi";
        }
        
        tmpStr = captureText.getText();
        if (tmpStr == null) {
            return false;
        }
        if (testParameter(tmpStr, 0.01, 100000.0)) {
            captureTime = Float.valueOf(tmpStr).floatValue();
        } else {
            MipavUtil.displayError("capture time must be between 0.01 and 100000.0");
            captureText.requestFocus();
            captureText.selectAll();
            return false;
        }
        
        tmpStr = skipText.getText();
        if (tmpStr == null) {
            return false;
        }
        if (testParameter(tmpStr, 0.00, 100000.0)) {
            skipTime = Float.valueOf(tmpStr).floatValue();
        } else {
            MipavUtil.displayError("skip time must be between 0.00 and 100000.0");
            skipText.requestFocus();
            skipText.selectAll();
            return false;
        }

        return true;
    } // end setVariables()

}
