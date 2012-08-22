package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.actions.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.swing.*;


/**
 * This is the "record script" dialog. While it's running, when scriptable commands happen they show up in the text area.
 * The text area can be changed to be editable (which pauses the recording) or uneditable (default uneditable). Script 
 * recording can be paused and resumed; when paused, commands will not show up in the script. There is a file menu that
 * allows the user to open a script file (this dumps the contents of the script file to the text area), save a script 
 * file, and exit. Right now open and save don't request specific extensions, but this could change in the future.
 * 
 * @author   Neva Cherniavsky
 * @see      ScriptRecorder
 * @see      ScriptRecordingListener
 * @see      ScriptableActionInterface
 * @see      Parser
 */
public class JDialogScriptRecorder extends JDialogBase implements ScriptRecordingListener, ActionListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -8409175669783444792L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JButton editButton;

    /** DOCUMENT ME! */
    private JLabel instructionLabel;

    /** DOCUMENT ME! */
    private String message = new String("The script is now recording.  Your actions will appear below.");

    /** DOCUMENT ME! */
    private JButton pauseButton, pauseButton2;

    /** DOCUMENT ME! */
    private JTextArea scriptTextArea;
    
    /** A reference to the global MIPAV script recorder.  This dialog listens to changes in its status and script. */
    private ScriptRecorder scriptRecorder = ScriptRecorder.getReference();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs a new script recorder dialog and turns recording on.
     */
    public JDialogScriptRecorder() {
        super(ViewUserInterface.getReference().getMainFrame(), false);
        setResizable(true);
        
        initGUI();
        
        scriptRecorder.addScriptRecordingListener(this);
        scriptRecorder.startRecording();
        
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Reacts to the following actions:<br>
     * Open - asks the user to choose a script file and dumps the contents to the text area<br>
     * Save - saves the script to a file<br>
     * Exit - calls <code>windowClosing</code><br>
     * Pause - pauses or resumes the script recording by setting isRecording appropriately<br>
     * Edit - makes the text area editable or uneditable.<br>
     *
     * @param  event  Event that triggers this function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Exit")) {
            windowClosing(null);
        } else if (command.equals("Edit")) {
            // allow editing if the text field isn't already editable, and vice-versa
            changeScriptEditingAbility(!scriptTextArea.isEditable());
        } else if (command.equals("InsertExit")) {
            scriptRecorder.addLine(new ActionExit());
        } else if (command.equals("InsertGC")) {
            scriptRecorder.addLine(new ActionCollectGarbage());
        } else if (command.equals("InsertComment")) {
            if (scriptRecorder.getRecorderStatus() == ScriptRecorder.RECORDING) {
                String commentText = JOptionPane.showInputDialog("Enter comment");

                if (commentText != null) {
                    scriptRecorder.addCommentLine(commentText);
                }
            }
        // TODO: somehow allow the user to specify prefixes/suffixes used in SaveImage/SaveImageAs commands (prefixes no longer a separate command)
        //} else if (command.equals("InsertPrefix")) {
        //    String inputValue = JOptionPane.showInputDialog("Enter prefix");
        //
        //    if (inputValue != null) {
        //        scriptText.append("Prefix " + inputValue + "\n");
        //    }
        } else if (command.equals("Pause")) {
            if (scriptRecorder.getRecorderStatus() == ScriptRecorder.RECORDING) {
                scriptRecorder.pauseRecording();
            } else {
                scriptRecorder.startRecording();
                changeScriptEditingAbility(false);
            }
        } else if (command.equals("Save")) {
            try {
                save();
            } catch (IOException e) {
                MipavUtil.displayError("Error saving script: " + e);
            }
        } else if (command.equals("Open")) {
            try {
                open();
            } catch (IOException e) {
                MipavUtil.displayError("Error opening script: " + e);
            }
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("10703");
            MipavUtil.showWebHelp("Using_Scripts_(Macros)_in_MIPAV");
        } else if (command.equals("Clear")) {
            scriptRecorder.resetScript();
        }
    }

    /**
     * Do nothing.
     *
     * @param  event  Window event.
     */
    public void windowActivated(WindowEvent event) { }

    /**
     * Do nothing.
     *
     * @param  event  Window event.
     */
    public void windowClosed(WindowEvent event) { }

    /**
     * Closes the dialog and stop the script recorder.
     *
     * @param  event  Window event (null if we are closing the window programmatically).
     */
    public void windowClosing(WindowEvent event) {
        scriptRecorder.stopRecording();
        scriptRecorder.removeScriptRecordingListener(this);
        dispose();
    }

    /**
     * Do nothing.
     *
     * @param  event  Window event.
     */
    public void windowDeactivated(WindowEvent event) { }

    /**
     * Do nothing.
     *
     * @param  event  Window event.
     */
    public void windowDeiconified(WindowEvent event) { }

    /**
     * Do nothing.
     *
     * @param  event  Window event.
     */
    public void windowIconified(WindowEvent event) { }
    
    /**
     * Do nothing.
     *
     * @param  event  Window event.
     */
    public void windowOpened(WindowEvent event) { }

    /**
     * Initializes the dialog box and adds the components.
     */
    private void initGUI() {
        setTitle("Record new script");

        JPanel instructionPanel = new JPanel();
        JPanel scrollPanel = new JPanel();
        JPanel buttonPanel = new JPanel();
        
        instructionLabel = WidgetFactory.buildLabel(message);
        instructionLabel.setBorder(BorderFactory.createEmptyBorder(10, 5, 0, 5));
        instructionPanel.add(instructionLabel);

        scriptTextArea = WidgetFactory.buildTextArea("", false);
        scriptTextArea.setRows(10);
        scriptTextArea.setColumns(30);
        scriptTextArea.setBackground(Color.lightGray);
        
        JScrollPane scrollPane = WidgetFactory.buildScrollPane(scriptTextArea);
        scrollPanel.setLayout(new BorderLayout(25, 25));
        scrollPanel.setBorder(BorderFactory.createEmptyBorder(15, 15, 15, 15));
        scrollPanel.add(scrollPane);

        JMenu fileMenu = new JMenu("File");

        fileMenu.setFont(MipavUtil.font12B);

        JMenuItem itemOpen = new JMenuItem("Open");
        itemOpen.addActionListener(this);
        itemOpen.setActionCommand("Open");
        itemOpen.setFont(MipavUtil.font12B);
        fileMenu.add(itemOpen);

        JMenuItem itemSave = new JMenuItem("Save");
        itemSave.addActionListener(this);
        itemSave.setActionCommand("Save");
        itemSave.setFont(MipavUtil.font12B);
        fileMenu.add(itemSave);

        fileMenu.addSeparator();

        JMenuItem itemExit = new JMenuItem("Exit");
        itemExit.addActionListener(this);
        itemExit.setActionCommand("Exit");
        itemExit.setFont(MipavUtil.font12B);
        fileMenu.add(itemExit);

        JMenuBar menuBar = new JMenuBar();
        menuBar.add(fileMenu);
        setJMenuBar(menuBar);

        JToolBar tBar = WidgetFactory.initToolbar();

        ViewToolBarBuilder toolbarBuilder = new ViewToolBarBuilder(this);
        tBar.add(toolbarBuilder.buildButton("Open", "Open script", "open"));
        tBar.add(toolbarBuilder.buildButton("Save", "Save script", "save"));
        tBar.add(toolbarBuilder.buildButton("Clear", "Clear the script recorder", "clear"));
        pauseButton2 = toolbarBuilder.buildButton("Pause", "Pause script recording", "pause");
        tBar.add(pauseButton2);
        tBar.add(ViewToolBarBuilder.makeSeparator());
        tBar.add(toolbarBuilder.buildButton("InsertComment", "Insert comment (will be ignored by parser)",
                                            "insertcomment"));
        tBar.add(toolbarBuilder.buildButton("InsertGC", "Insert command to collect garbage (which frees up memory)",
                                            "insertgc"));
        tBar.add(toolbarBuilder.buildButton("InsertExit", "Insert command to exit MIPAV", "insertexit"));
        
        //tBar.add(toolbarBuilder.buildButton("InsertPrefix", "Insert prefix for image file name", "insertprefix"));

        // tBar.add(toolbarBuilder.buildButton("InsertLoop", "Insert loop", "insertloop"));
        tBar.add(ViewToolBarBuilder.makeSeparator());

        pauseButton = new JButton("Pause");
        editButton = new JButton("Enable Edit");
        pauseButton.setActionCommand("Pause");
        editButton.setActionCommand("Edit");
        pauseButton.addActionListener(this);
        editButton.addActionListener(this);
        pauseButton.setFont(serif12B);
        editButton.setFont(serif12B);
        pauseButton.setPreferredSize(MipavUtil.defaultButtonSize);
        editButton.setPreferredSize(new Dimension(100, 30));
        buttonPanel.add(pauseButton);
        buttonPanel.add(editButton);
        buttonPanel.add(buildHelpButton());
        buttonPanel.setBorder(BorderFactory.createEmptyBorder(0, 5, 10, 5));

        getContentPane().setLayout(new BorderLayout(25, 25));
        getContentPane().add(tBar, BorderLayout.NORTH);

        JPanel centerPanel = new JPanel(new BorderLayout());
        centerPanel.add(instructionLabel, BorderLayout.NORTH);
        centerPanel.add(scrollPanel, BorderLayout.CENTER);

        // getContentPane().add( instructionLabel, BorderLayout.NORTH );
        // getContentPane().add( scrollPanel, BorderLayout.CENTER );

        getContentPane().add(centerPanel, BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
    }

    /**
     * Opens the script as a text file and dumps it to the text area.
     *
     * @exception  IOException  If the open fails for some reason
     */
    private void open() throws IOException {
        FileReader istream = null;

        JFileChooser chooser = new JFileChooser();

        // if (userInterface.getDefaultDirectory()!=null)
        chooser.setCurrentDirectory(new File(Preferences.getScriptsDirectory()));
        // else chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));

        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.SCRIPT));

        int returnVal = chooser.showOpenDialog(this);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            istream = new FileReader(chooser.getCurrentDirectory() + "" + File.separatorChar + "" +
                                     chooser.getSelectedFile().getName());
        } else {
            return;
        }

        Preferences.setProperty(Preferences.PREF_SCRIPT_DIR, String.valueOf(chooser.getCurrentDirectory()));
        // userInterface.setDefaultDirectory(String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar);


        String newText = "";
        int ch;

        ch = istream.read();

        while (ch != -1) {
            newText += (char) ch;
            ch = istream.read();
        }

        //scriptTextArea.setText(newText);
        scriptRecorder.setScript(newText);
        
        istream.close();
    }

    /**
     * Saves the script as a text file; calls the appropriate save dialogs to do so.
     *
     * @exception  IOException  If the save fails for some reason.
     */
    private void save() throws IOException {
        FileWriter ostream = null;
        String fileName;

        JFileChooser chooser = new JFileChooser();
        ViewImageFileFilter filter = new ViewImageFileFilter(ViewImageFileFilter.SCRIPT);

        chooser.setFileFilter(filter);

        // if (userInterface.getDefaultDirectory()!=null)
        chooser.setCurrentDirectory(new File(Preferences.getScriptsDirectory()));

        // else
        // chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        int returnVal = chooser.showSaveDialog(this);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();

            if (fileName.lastIndexOf('.') == -1) {
                fileName = fileName + ".sct";
            }

            ostream = new FileWriter(chooser.getCurrentDirectory() + "" + File.separatorChar + "" + fileName);
        } else {
            return;
        }

        Preferences.setProperty(Preferences.PREF_SCRIPT_DIR, String.valueOf(chooser.getCurrentDirectory()));
        // userInterface.setDefaultDirectory(String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar);

        //ostream.write(scriptTextArea.getText());
        
        // if hand-editing is being done, the contents of the script text area are the most current
        if (scriptTextArea.isEditable()) {
            ostream.write(scriptTextArea.getText());
        } else {
            ostream.write(scriptRecorder.getScript());
        }
        
        ostream.close();
    }

    /**
     * Sets the instruction label to the message. Used mostly for Pause/Resume.
     *
     * @param  message  Text to set the label to.
     */
    private void setUserInstructions(String message) {
        this.message = message;
        instructionLabel.setText(message);
    }
    
    /**
     * Changes whether the current script can be edited by hand (which pauses the script recording while the editing is being done.
     * @param  doAllowEditing  Whether the ability to edit the script should be enabled.
     */
    private void changeScriptEditingAbility(boolean doAllowEditing) {
        if (doAllowEditing) {
            if (scriptRecorder.getRecorderStatus() == ScriptRecorder.RECORDING) {
                scriptRecorder.pauseRecording();
            }
            
            scriptTextArea.setEditable(true);
            scriptTextArea.setBackground(Color.white);
            editButton.setText("Disable editing");
        } else {
            scriptTextArea.setEditable(false);
            scriptTextArea.setBackground(Color.lightGray);
            editButton.setText("Enable editing");
            
            scriptRecorder.setScript(scriptTextArea.getText());
            if (scriptRecorder.getRecorderStatus() == ScriptRecorder.PAUSED) {
                scriptRecorder.startRecording();
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    public void updateScript(String newScriptText) {
        scriptTextArea.setText(newScriptText);
    }
    
    /**
     * {@inheritDoc}
     */
    public void changeRecordingStatus(int recorderStatus) {
        if (recorderStatus == ScriptRecorder.RECORDING) {
            pauseButton.setText("Pause");
            pauseButton2.setIcon(MipavUtil.getIcon("pause.gif"));
            pauseButton2.setRolloverIcon(MipavUtil.getIcon("pauseroll.gif"));
            pauseButton2.setToolTipText("Pause scripting");
            setUserInstructions("The script is now recording.  Your actions will appear below.");
        } else {
            pauseButton.setText("Resume");
            pauseButton2.setIcon(MipavUtil.getIcon("record.gif"));
            pauseButton2.setRolloverIcon(MipavUtil.getIcon("recordroll.gif"));
            pauseButton2.setToolTipText("Resume scripting");
            setUserInstructions("The script is now paused.  Press Resume to resume recording.");
        }
    }
}
