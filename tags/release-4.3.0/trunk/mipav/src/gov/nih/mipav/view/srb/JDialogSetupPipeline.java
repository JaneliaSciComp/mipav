package gov.nih.mipav.view.srb;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import edu.sdsc.grid.io.srb.*;

import java.awt.*;
import java.awt.event.*;
import java.io.IOException;
import java.io.File;

import javax.swing.*;


/**
 * This dialog is used to set up the parameters of NDARPipeline.
 */
public class JDialogSetupPipeline extends JDialog implements ActionListener {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** the authentication schema that the srb server uses. */
    private JLabel scriptFileNameLabel;

    private JTextField scriptFileNameField;
    
    private JButton scriptFileNameBrowseButton;

    /** DOCUMENT ME! */
    private JTextField targetSRBDirField;

    /** the target srb directory */
    private JLabel targetSRBDirLabel;

    private JButton targetSRBDirBrowseButton;
    
    /** DOCUMENT ME! */
    private JButton okButton;

    /** DOCUMENT ME! */
    private JButton cancelButton,helpButton;

    /** DOCUMENT ME! */
    private final int COLUMN_COUNT = 30;

    private boolean cancelled = true;
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogLoginSRB object.
     *
     * @param  dialogTitle  DOCUMENT ME!
     */
    public JDialogSetupPipeline(String dialogTitle) {
        super(ViewUserInterface.getReference().getMainFrame(), dialogTitle, false);
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------


    /**
     * Action event listener.
     *
     * @param  e  the action event.
     */
    public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();

        if (command.equals("OK")) {
            approve();
        } else if (command.equals("Cancel")) {
            cancel();
        } else if(command.equals("scriptFileNameBrowse")){
            /**
             * Uses the JFileChooser to retrieve the file that the user wants to open.
             */
            JFileChooser chooser = null;

            try {
                chooser = new JFileChooser(Preferences.getScriptsDirectory());
            } catch (OutOfMemoryError ex) {
                ex.printStackTrace(System.err);
                MipavUtil.displayError("Out of memory!");

                return;
            } 
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            chooser.setMultiSelectionEnabled(false);

            int returnValue = chooser.showDialog(ViewUserInterface.getReference().getMainFrame(), "Open");

            if (returnValue == JFileChooser.APPROVE_OPTION) {

                /**
                 * According to the files selected by user, tries to create the file list.
                 */
                File file = chooser.getSelectedFile();
                scriptFileNameField.setText(file.getAbsolutePath());
            }
        } else if(command.equals("targetSRBDirBrowse")){
            if (!JDialogLoginSRB.hasValidSRBFileSystem()) {
                new JDialogLoginSRB("Connect to");
                if (!JDialogLoginSRB.hasValidSRBFileSystem()) {
                    return;
                }
            }


            /**
             * Uses the JargonFileChooser to retrieve the file that the user wants to open.
             */
            JargonFileChooser chooser = null;

            try {
                chooser = new JargonFileChooser(JDialogLoginSRB.srbFileSystem);
            } catch (OutOfMemoryError ex) {
                ex.printStackTrace(System.err);
                MipavUtil.displayError("Out of memory!");

                return;
            } catch (IOException ex) {
                ex.printStackTrace(System.err);
                MipavUtil.displayError(ex.getMessage());

                return;
            }

            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            chooser.setMultiSelectionEnabled(false);

            int returnValue = chooser.showDialog(ViewUserInterface.getReference().getMainFrame(), "Open");

            if (returnValue == JargonFileChooser.APPROVE_OPTION) {

                /**
                 * According to the files selected by user, tries to create the srb file list.
                 */
                SRBFile file = chooser.getSelectedFile();
                targetSRBDirField.setText(file.getAbsolutePath());
            }
        }else if (command.equals("Help")) {
            MipavUtil.showHelp("20020");
        }
    }


    /**
     * Initializes the GUI of this pipeline.
     */
    private void init() {
        PanelManager manager = new PanelManager();
        manager.getConstraints().insets = new Insets(5, 5, 5, 5);

        // Sets up the script file label.
        scriptFileNameLabel = WidgetFactory.buildLabel("Choose Script File");
        scriptFileNameLabel.setHorizontalAlignment(SwingConstants.RIGHT);
        manager.add(scriptFileNameLabel);

        // Sets up the script file name field.
        String scriptFile = Preferences.getScriptFile();
        if(scriptFile == null){
            scriptFile = "";
        }
        scriptFileNameField = WidgetFactory.buildTextField(scriptFile);
        scriptFileNameField.setColumns(COLUMN_COUNT);
        manager.add(scriptFileNameField);

        scriptFileNameBrowseButton = WidgetFactory.buildTextButton("Browse", "Choose the script file", "scriptFileNameBrowse", this);
        scriptFileNameBrowseButton.setPreferredSize(new Dimension(90, 30));
        manager.add(scriptFileNameBrowseButton);

        // Sets up the srb target directory label.
        targetSRBDirLabel = WidgetFactory.buildLabel("Choose target SRB directory");
        targetSRBDirLabel.setHorizontalAlignment(SwingConstants.RIGHT);
        manager.addOnNextLine(targetSRBDirLabel);

        // Sets up the script file name field.
        targetSRBDirField = WidgetFactory.buildTextField("");
        targetSRBDirField.setColumns(COLUMN_COUNT);
        manager.add(targetSRBDirField);

        targetSRBDirBrowseButton = WidgetFactory.buildTextButton("Browse", "Choose the target srb directory", "targetSRBDirBrowse", this);
        targetSRBDirBrowseButton.setPreferredSize(new Dimension(90, 30));
        manager.add(targetSRBDirBrowseButton);

        this.getContentPane().setLayout(new BorderLayout());
        this.getContentPane().add(manager.getPanel(), BorderLayout.CENTER);

        JPanel bottomPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        okButton = WidgetFactory.buildTextButton("  OK  ", "Set up the parameters for the pipeline", "OK", this);
        okButton.setPreferredSize(new Dimension(90, 30));
        bottomPanel.add(okButton);

        cancelButton = WidgetFactory.buildTextButton("Cancel", "Cancel the parameters for the pipeline", "Cancel", this);
        cancelButton.setPreferredSize(new Dimension(90, 30));
        helpButton = new JButton("Help");
        helpButton.addActionListener(this);
        helpButton.setPreferredSize(new Dimension(90, 30));
        bottomPanel.add(cancelButton);
        bottomPanel.add(helpButton);
        this.getContentPane().add(bottomPanel, BorderLayout.SOUTH);
        
        this.pack();

        // Added the responses to the key board.
        okButton.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0, false), "Approve");
        okButton.getActionMap().put("Approve", new AbstractAction("Approve"){
            public void actionPerformed(ActionEvent e){
                approve();
            }
        });
        cancelButton.getInputMap(JComponent.WHEN_FOCUSED).put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0, false), "Cancel");
        
        cancelButton.getActionMap().put("Cancel", new AbstractAction("Cancel"){
            public void actionPerformed(ActionEvent e){
                cancel();
            }
        });
        okButton.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, false), "Cancel");
        okButton.getActionMap().put("Cancel", new AbstractAction("Cancel"){
            public void actionPerformed(ActionEvent e){
                cancel();
            }
        });
        /**
         * You have to bring these two statement before the setVisible(true), otherwise doesn't work.
         */
        MipavUtil.centerOnScreen(this);
        this.setVisible(true);
    }
    
    /**
     * Returns the script file name.
     * @return the script file name.
     */
    public String getScriptFileName(){
        return scriptFileNameField.getText();
    }
    
    /**
     * Returns the destination directory on the srb server.
     * @return the destination directory on the srb server.
     */
    public String getTargetSRBDir(){
        return targetSRBDirField.getText();
    }
    
    /**
     * Return true if this dialog was cancelled.
     * @return true if this dialog was cancelled.
     */
    public boolean isCancelled(){
        return cancelled;
    }
    
    /**
     * The action need be performed when this dialog was approved.
     */
    private void approve(){

        /**
         * First check every text field, make sure it has right input.
         */
        if (scriptFileNameField.getText().length() == 0) {
            scriptFileNameField.requestFocus();

            return;
        }

        if (targetSRBDirField.getText().length() == 0) {
            targetSRBDirField.requestFocus();

            return;
        }
        String scriptFileName = scriptFileNameField.getText();
        Preferences.setScriptFile(scriptFileName);
        int index = scriptFileName.lastIndexOf(File.separator);
        Preferences.setScriptsDirectory(scriptFileName.substring(0, index));
        this.dispose();
        cancelled = false;
    }
    
    /**
     * The action need be performed when this dialog was cancelled.
     *
     */
    private void cancel(){
        this.dispose();
        cancelled = true;
    }
}

