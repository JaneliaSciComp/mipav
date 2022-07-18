import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;
import gov.nih.mipav.util.ThreadUtil;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 
 */
public class PlugInDialogScoutRemoval extends JDialogStandaloneScriptablePlugin implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------
	
	private PlugInAlgorithmScoutRemoval scoutRemovalAlgo = null;
	
	private JTextField fileDirectoryText;

	private String fileDirectory;
	
	
	/**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogScoutRemoval() {}

    /**
     * 
     */
    public PlugInDialogScoutRemoval(final boolean modal) {
        super(modal);

        init();
    }
    
 // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     * 
     * @param event Event that triggers function.
     */
    @Override
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

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
        // System.out.print(this.getSize());
    } // end actionPerformed()
	
	/**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Scout Removal");
        
        final GuiBuilder gui = new GuiBuilder(this);

        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        JPanel inputPanel = new JPanel(new GridBagLayout());
        inputPanel.setForeground(Color.black);
        inputPanel.setBorder(buildTitledBorder("Input parameters"));
        
        fileDirectoryText = gui.buildFileField("Directory containing patient directories: ", " ", false, JFileChooser.DIRECTORIES_ONLY);
        inputPanel.add(fileDirectoryText.getParent(), gbc);
        
        getContentPane().add(inputPanel, BorderLayout.NORTH);

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
    }

	/**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     * 
     * @param algorithm Algorithm that caused the event.
     */
    @Override
    public void algorithmPerformed(final AlgorithmBase algorithm) {
        if (algorithm instanceof PlugInAlgorithmScoutRemoval) {
            Preferences.debug("Elapsed: " + algorithm.getElapsedTime());

            if ( (scoutRemovalAlgo.isCompleted() == true)) {
                insertScriptLine();
            }

            if (scoutRemovalAlgo != null) {
                scoutRemovalAlgo.finalize();
                scoutRemovalAlgo = null;
            }

            // dispose();
            if (super.isExitRequired()) {
                ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
            }
        }
    } // end algorithmPerformed()
    
    /**
     * Once all the necessary variables are set, call the kidney segmentation algorithm
     */
    @Override
    protected void callAlgorithm() {

        try {

            scoutRemovalAlgo = new PlugInAlgorithmScoutRemoval(fileDirectory);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            scoutRemovalAlgo.addListener(this);
            // createProgressBar("Creating plugin", " ...", generateFusionAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (scoutRemovalAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                scoutRemovalAlgo.run();
            }
        } catch (final OutOfMemoryError x) {
            
            MipavUtil.displayError("Generic algorithm: unable to allocate enough memory");

            return;
        }

    } // end callAlgorithm()
    
    /**
     * Used in turning your plugin into a script
     */
    @Override
    protected void setGUIFromParams() {
        fileDirectory = scriptParameters.getParams().getString("fileDirectory");
    }
    
    /**
     * Used in turning your plugin into a script
     */
    @Override
    protected void storeParamsFromGUI() throws ParserException {
    	scriptParameters.getParams().put(ParameterFactory.newParameter("fileDirectory", fileDirectory));
    }
    
    private boolean setVariables() {
    	fileDirectory = fileDirectoryText.getText();
    	if (fileDirectory == null) {
    		return false;
    	}
    	
    	return true;
    }

}
