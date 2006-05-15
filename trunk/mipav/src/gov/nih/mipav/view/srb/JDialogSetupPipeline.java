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
 * DOCUMENT ME!
 */
public class JDialogSetupPipeline extends JDialog implements ActionListener, KeyListener {

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
    private JButton cancelButton;

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
        super(ViewUserInterface.getReference().getMainFrame(), dialogTitle, true);
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------


    /**
     * Action event listener.
     *
     * @param  e  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();

        if (command.equals("OK")) {

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
            this.dispose();
            cancelled = false;
        } else if (command.equals("Cancel")) {
            this.dispose();
            cancelled = true;
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
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyPressed(KeyEvent e) { }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyReleased(KeyEvent e) { }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyTyped(KeyEvent e) {
        int keyChar = e.getKeyChar();

        if (keyChar == KeyEvent.VK_ENTER) {
            actionPerformed(new ActionEvent(this, 10, "OK"));
        } else if (keyChar == KeyEvent.VK_ESCAPE) {
            actionPerformed(new ActionEvent(this, 11, "Cancel"));
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void init() {
        PanelManager manager = new PanelManager();
        manager.getConstraints().insets = new Insets(5, 5, 5, 5);

        // Sets up the script file label.
        scriptFileNameLabel = WidgetFactory.buildLabel("Choose Script File");
        scriptFileNameLabel.setHorizontalAlignment(SwingConstants.RIGHT);
        manager.add(scriptFileNameLabel);

        // Sets up the script file name field.
        scriptFileNameField = WidgetFactory.buildTextField("");
        scriptFileNameField.setColumns(COLUMN_COUNT);
        scriptFileNameField.addKeyListener(this);
        manager.add(scriptFileNameField);

        scriptFileNameBrowseButton = WidgetFactory.buildTextButton("Browse", "Choose the script file", "scriptFileNameBrowse", this);
        scriptFileNameBrowseButton.setPreferredSize(new Dimension(90, 30));
        scriptFileNameBrowseButton.addKeyListener(this);
        manager.add(scriptFileNameBrowseButton);

        // Sets up the srb target directory label.
        targetSRBDirLabel = WidgetFactory.buildLabel("Choose target SRB directory");
        targetSRBDirLabel.setHorizontalAlignment(SwingConstants.RIGHT);
        manager.addOnNextLine(targetSRBDirLabel);

        // Sets up the script file name field.
        targetSRBDirField = WidgetFactory.buildTextField("");
        targetSRBDirField.setColumns(COLUMN_COUNT);
        targetSRBDirField.addKeyListener(this);
        manager.add(targetSRBDirField);

        targetSRBDirBrowseButton = WidgetFactory.buildTextButton("Browse", "Choose the target srb directory", "targetSRBDirBrowse", this);
        targetSRBDirBrowseButton.setPreferredSize(new Dimension(90, 30));
        targetSRBDirBrowseButton.addKeyListener(this);
        manager.add(targetSRBDirBrowseButton);

        this.getContentPane().setLayout(new BorderLayout());
        this.getContentPane().add(manager.getPanel(), BorderLayout.CENTER);

        JPanel bottomPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        okButton = WidgetFactory.buildTextButton("  OK  ", "Set up the parameters for the pipeline", "OK", this);
        okButton.setPreferredSize(new Dimension(90, 30));
        okButton.addKeyListener(this);
        bottomPanel.add(okButton);

        cancelButton = WidgetFactory.buildTextButton("Cancel", "Cancel the parameters for the pipeline", "Cancel", this);
        cancelButton.setPreferredSize(new Dimension(90, 30));
        bottomPanel.add(cancelButton);
        this.getContentPane().add(bottomPanel, BorderLayout.SOUTH);
        this.pack();

        /**
         * You have to bring these two statement before the setVisible(true), otherwise doesn't work.
         */
        MipavUtil.centerOnScreen(this);
        this.setVisible(true);
    }
    
    public String getScriptFileName(){
        return scriptFileNameField.getText();
    }
    
    public String getTargetSRBDir(){
        return targetSRBDirField.getText();
    }
    
    public boolean isCancelled(){
        return cancelled;
    }
}
