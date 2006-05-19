package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.text.*;


/**
 * This is the "record script" dialog. While it's running, when commands happen they show up in the text area. The text
 * area can be changed to be editable or uneditable (default uneditable). Script recording can be paused and resumed;
 * when paused, commands will not show up in the script. There is a file menu that allows the user to open a script file
 * (this dumps the contents of the script file to the text area), save a script file, and exit. Right now open and save
 * don't request specific extensions, but this could change in the future.
 *
 * @version  1.0 June 1, 2001
 * @author   Neva Cherniavsky
 * @see      gov.nih.mipav.model.algorithms.AlgorithmScriptParser
 */
public class JDialogScript extends JDialogBase implements ScriptRecorderInterface, ActionListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -8409175669783444792L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean activeImageFlag = false;

    /** DOCUMENT ME! */
    private Hashtable activeImagesTable;

    /** DOCUMENT ME! */
    private JPanel buttonPanel;

    /** DOCUMENT ME! */
    private JButton editButton;

    /** DOCUMENT ME! */
    private JMenu fileMenu;

    /** DOCUMENT ME! */
    private int imageNum = 1, voiNum = 1, activeImageNum = 1;

    /** DOCUMENT ME! */
    private Hashtable imagesTable;

    /** DOCUMENT ME! */
    private JLabel instructionLabel;

    /** DOCUMENT ME! */
    private JPanel instructionPanel;

    /** DOCUMENT ME! */
    private boolean isRecording = false;

    /** DOCUMENT ME! */
    private JMenuItem itemExit;

    /** DOCUMENT ME! */
    private JMenuItem itemOpen;

    /** DOCUMENT ME! */
    private JMenuItem itemSave;

    /** DOCUMENT ME! */
    private JMenuBar menuBar;

    /** DOCUMENT ME! */
    private String message = new String("The script is now paused.  Press Resume to resume recording.");

    /** DOCUMENT ME! */
    private JButton pauseButton, pauseButton2;

    /** DOCUMENT ME! */
    private JTextArea scriptText;

    /** DOCUMENT ME! */
    private JScrollPane scrollPane;

    /** DOCUMENT ME! */
    private JPanel scrollPanel;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private Hashtable voisTable;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    // private Vector potentialActives = null;
    // private     int    numUsedActive = 0;

    /**
     * Constructs a new script dialog, displays it, and turns recording on.
     *
     * @param  title  Title of dialog frame
     * @param  ui     user interface (dialog uses main frame from UI as parent)
     */
    public JDialogScript(String title, ViewUserInterface ui) {
        super(ui.getMainFrame(), false);
        setResizable(true);
        userInterface = ui;
        imagesTable = new Hashtable();
        activeImagesTable = new Hashtable();
        voisTable = new Hashtable();
        init(title);
        ui.setScriptDialog(this);
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

            if (scriptText.isEditable()) {
                scriptText.setEditable(false);
                scriptText.setBackground(Color.lightGray);
                editButton.setText("Enable Edit");
            } else {
                scriptText.setEditable(true);
                scriptText.setBackground(Color.white);
                editButton.setText("Disable Edit");
            }
        } else if (command.equals("InsertExit")) {
            scriptText.append("Exit\n");
        } else if (command.equals("InsertGC")) {
            scriptText.append("CollectGarbage\n");
        } else if (command.equals("InsertComment")) {
            String inputValue = JOptionPane.showInputDialog("Enter comment");

            if (inputValue != null) {
                scriptText.append("Comment " + inputValue + "\n");
            }
        } else if (command.equals("InsertPrefix")) {
            String inputValue = JOptionPane.showInputDialog("Enter prefix");

            if (inputValue != null) {
                scriptText.append("Prefix " + inputValue + "\n");
            }
        } else if (command.equals("Pause")) {

            if (isRecording) {
                isRecording = false;
                pauseButton.setText("Resume");
                pauseButton2.setIcon(MipavUtil.getIcon("record.gif"));
                pauseButton2.setRolloverIcon(MipavUtil.getIcon("recordroll.gif"));
                pauseButton2.setToolTipText("Resume scripting");
                setMessage("The script is now paused.  Press Resume to resume recording.");
            } else {
                isRecording = true;
                pauseButton.setText("Pause");
                pauseButton2.setIcon(MipavUtil.getIcon("pause.gif"));
                pauseButton2.setRolloverIcon(MipavUtil.getIcon("pauseroll.gif"));
                pauseButton2.setToolTipText("Pause scripting");
                setMessage("The script is now recording.  Your actions will appear below.");
            }
        } else if (command.equals("Save")) {

            try {
                save();
            } catch (IOException e) {
                MipavUtil.displayError("Error: " + e);
            }
        } else if (command.equals("Open")) {

            try {
                open();
            } catch (IOException e) {
                MipavUtil.displayError("Error: " + e);
            }
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("10703");
        } else if (command.equals("Clear")) {
            scriptText.setText("");
        }
    }

    /**
     * Appends the script text area with the message. Used by other classes to record commands from the GUI (such as
     * <code>OpenImage</code>).
     *
     * @param  appMessage  The command or information to record
     */
    public void append(String appMessage) {
        scriptText.append(appMessage);
    }

    /**
     * Changes the active image name for an existing entry (for ChangeName).
     *
     * @param  oldKey  String old image name
     * @param  newKey  String new image name
     */
    public void changeActiveVar(String oldKey, String newKey) {

        if (activeImagesTable.get(oldKey) != null) {
            String varName = (String) activeImagesTable.get(oldKey);
            activeImagesTable.remove(oldKey);
            activeImagesTable.put(newKey, varName);
        }
    }

    /**
     * Changes the image name for an existing entry (for ChangeName).
     *
     * @param  oldKey  String old image name
     * @param  newKey  String new image name
     */
    public void changeVar(String oldKey, String newKey) {

        if (imagesTable.get(oldKey) != null) {
            String varName = (String) imagesTable.get(oldKey);
            imagesTable.remove(oldKey);
            imagesTable.put(newKey, varName);
        }
    }

    /**
     * Get active image flag.
     *
     * @return  <code>true</code> if , <code>false</code> if not
     */
    public boolean getActiveImageFlag() {
        return activeImageFlag;
    }

    /**
     * Get image number from the active image table.
     *
     * @param   key  active image name
     *
     * @return  active image number corresponding to the given image name
     */
    public Object getActiveImgTableVar(String key) {
        return activeImagesTable.get(key);
    }

    /**
     * Get image number from the image table.
     *
     * @param   key  image name
     *
     * @return  image number corresponding to the given image name
     */
    public Object getImgTableVar(String key) {
        return imagesTable.get(key);
    }

    /**
     * Returns the variable name associated with an image name. For example, if the user opens an image "test.img", the
     * name "test.img" is stored as a key to the variable name $image1. From then on, whenever the user refers to the
     * image "test.img", the variable name $image1 is recorded instead. This is so that when the user runs the script on
     * multiple images, the action takes place on a variable instead of an absolute (like "test.img").
     *
     * @param   key  Image name (if null, $active is used)
     *
     * @return  The variable name, such as $image1
     */
    public Object getVar(String key) {

        if ((key == null) || key.equals(" ")) {
            return new String("$active");
        } else {

            if (imagesTable.get(key) == null) {

                if (activeImagesTable.get(key) == null) {
                    return new String("$active");
                } else {
                    return activeImagesTable.get(key);
                }
            } else {
                return imagesTable.get(key);
            }
        }

    }

    /**
     * Returns the variable name associated with a voi name. Like getVar, only for voi's.
     *
     * @param   key  DOCUMENT ME!
     *
     * @return  The variable name, such as $voi1
     */
    public Object getVoiVar(String key) {

        if ((key == null) || key.equals(" ")) {
            return new String("$activeV");
        } else {

            if (voisTable.get(key) == null) {
                return new String("$activeV");
            } else {
                return voisTable.get(key);
            }
        }
    }

    /**
     * Determines if the dialog is currently recording GUI actions.
     *
     * @return  <code>true</code> if currently recording, <code>false</code> if not
     */
    public boolean isRecording() {
        return isRecording;
    }


    /**
     * Register the active image name in the active image table.
     *
     * @param  key  active image name
     */
    public void putActiveVar(String key) {

        if (activeImagesTable.get(key) == null) {
            String varName = "$active" + activeImageNum;

            activeImageNum++;
            activeImagesTable.put(key, varName);
        }
    }

    /**
     * Stores an image name as a key to an image variable. The image variable is just $image plus a number (e.g.,
     * $image1, $image2, etc.). This must be called before <code>getVar</code> or else <code>getVar</code> will return a
     * null. Any time a new image is created in the GUI, this method ought to be called.
     *
     * @param  key  The image name to be associated with a new variable.
     */
    public void putVar(String key) {

        if (imagesTable.get(key) == null) {
            String varName = "$image" + imageNum;

            imageNum++;
            imagesTable.put(key, varName);
        }
    }

    /**
     * Stores a voi name as a key to a voi variable. Like putVar, only for voi's.
     *
     * @param  key  The voi name to be associated with a new variable.
     */
    public void putVoiVar(String key) {
        String varName = "$voi" + voiNum;

        voiNum++;
        voisTable.put(key, varName);
    }

    /**
     * Return the currently potential active images.
     */
    /*public Vector getPotentialActives() {
     *  if ( potentialActives == null ) {     return new Vector(); } return potentialActives;}*/

    // public int getUsedActiveImageNum() {
    // return numUsedActive;
    // }

    /**
     * Removes the last appended line from the text area.
     */
    public void removeLine() {

        try {
            int line = scriptText.getLineCount();
            int start = scriptText.getLineStartOffset(line - 1);

            // int end = scriptText.getLineEndOffset(line-1);
            String text = scriptText.getText();

            text = text.substring(0, start);
            scriptText.setText(text);
        } catch (BadLocationException error) { }
    }

    /**
     * Set active image flag to true when script start recording.
     *
     * @param  flag  DOCUMENT ME!
     */
    public void setActiveImageFlag(boolean flag) {
        activeImageFlag = flag;

        /* if (activeImageFlag) {
         * this.potentialActives = new Vector(); Enumeration e = userInterface.getRegisteredImages(); while
         * (e.hasMoreElements()) { ModelImage image = (ModelImage) e.nextElement(); ViewJFrameImage frame =
         * userInterface.getFrameContainingImage(image); if (frame != null) {
         * potentialActives.add(image.getImageName()); System.err.println("Added:  " + image.getImageName()); } } }
         */
    }

    /**
     * Set the script recording flag.
     *
     * @param  flag  <code>true</code> if currently recording, <code>false</code> if not
     */
    public void setRecording(boolean flag) {
        isRecording = flag;
    }

    /**
     * unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowActivated(WindowEvent event) { }

    /**
     * unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowClosed(WindowEvent event) { }

    /**
     * Closes the dialog. Sets <code>isRecording</code> and <code>isOpen</code> to <code>false</code>.
     *
     * @param  event  Event that triggered this function.
     */
    public void windowClosing(WindowEvent event) {
        isRecording = false;
        userInterface.setScriptDialog(null);
        dispose();
    }

    /**
     * unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowDeactivated(WindowEvent event) { }

    /**
     * unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowDeiconified(WindowEvent event) { }

    /**
     * unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowIconified(WindowEvent event) { }

    // ************************************************************************
    // **************************** Window Events *****************************
    // ************************************************************************

    /**
     * unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowOpened(WindowEvent event) { }

    /**
     * Initializes the dialog box and adds the components.
     *
     * @param  title  Title of the dialog box.
     */
    private void init(String title) {
        setTitle(title);
        isRecording = true;

        BorderLayout layout = new BorderLayout(25, 25);

        instructionPanel = new JPanel();
        scrollPanel = new JPanel();
        buttonPanel = new JPanel();

        instructionLabel = new JLabel(message);
        instructionLabel.setFont(serif12);
        instructionLabel.setForeground(Color.black);
        instructionLabel.setBorder(BorderFactory.createEmptyBorder(10, 5, 0, 5));
        instructionPanel.add(instructionLabel);

        scriptText = new JTextArea(10, 30);
        scriptText.setEditable(false);
        scriptText.setFont(serif12);
        scriptText.setBackground(Color.lightGray);
        scrollPane = new JScrollPane(scriptText, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                     JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPanel.setLayout(new BorderLayout(25, 25));
        scrollPanel.setBorder(BorderFactory.createEmptyBorder(15, 15, 15, 15));
        scrollPanel.add(scrollPane);

        JMenu fileMenu = new JMenu("File");

        fileMenu.setFont(MipavUtil.font12B);

        itemOpen = new JMenuItem("Open");
        itemOpen.addActionListener(this);
        itemOpen.setActionCommand("Open");
        itemOpen.setFont(MipavUtil.font12B);
        fileMenu.add(itemOpen);

        itemSave = new JMenuItem("Save");
        itemSave.addActionListener(this);
        itemSave.setActionCommand("Save");
        itemSave.setFont(MipavUtil.font12B);
        fileMenu.add(itemSave);

        fileMenu.addSeparator();

        itemExit = new JMenuItem("Exit");
        itemExit.addActionListener(this);
        itemExit.setActionCommand("Exit");
        itemExit.setFont(MipavUtil.font12B);
        fileMenu.add(itemExit);

        menuBar = new JMenuBar();
        menuBar.add(fileMenu);
        setJMenuBar(menuBar);

        JToolBar tBar = WidgetFactory.initToolbar();

        ViewToolBarBuilder toolbarBuilder = new ViewToolBarBuilder(this);
        tBar.add(toolbarBuilder.buildButton("Open", "Open script", "open"));
        tBar.add(toolbarBuilder.buildButton("Save", "Save script", "save"));
        tBar.add(toolbarBuilder.buildButton("Clear", "Clear the script recorder", "clear"));
        pauseButton2 = toolbarBuilder.buildButton("Pause", "Pause script recording", "pause");
        tBar.add(pauseButton2);
        tBar.add(makeSeparator());
        tBar.add(toolbarBuilder.buildButton("InsertComment", "Insert comment (will be ignored by parser)",
                                            "insertcomment"));
        tBar.add(toolbarBuilder.buildButton("InsertGC", "Insert command to collect garbage (which frees up memory)",
                                            "insertgc"));
        tBar.add(toolbarBuilder.buildButton("InsertExit", "Insert command to exit MIPAV", "insertexit"));
        tBar.add(toolbarBuilder.buildButton("InsertPrefix", "Insert prefix for image file name", "insertprefix"));

        // tBar.add(toolbarBuilder.buildButton("InsertLoop", "Insert loop", "insertloop"));
        tBar.add(makeSeparator());

        pauseButton = new JButton("Resume");
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

        getContentPane().setLayout(layout);
        getContentPane().add(tBar, BorderLayout.NORTH);

        JPanel centerPanel = new JPanel(new BorderLayout());
        centerPanel.add(instructionLabel, BorderLayout.NORTH);
        centerPanel.add(scrollPanel, BorderLayout.CENTER);

        // getContentPane().add( instructionLabel, BorderLayout.NORTH );
        // getContentPane().add( scrollPanel, BorderLayout.CENTER );

        getContentPane().add(centerPanel, BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        pauseButton.setText("Pause");
        instructionLabel.setText("The script is now recording.  Your actions will appear below.");
    }

    /**
     * Makes a separator for the use in the toolbars.
     *
     * @return  Separator button.
     */
    private JButton makeSeparator() {
        JButton separator = new JButton(MipavUtil.getIcon("separator.gif"));

        separator.setBorderPainted(false);
        separator.setFocusPainted(false);

        return (separator);
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

        Preferences.setProperty("ScriptsDir", String.valueOf(chooser.getCurrentDirectory()));
        // userInterface.setDefaultDirectory(String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar);


        String newText = "";
        int ch;

        ch = istream.read();

        while (ch != -1) {
            newText += (char) ch;
            ch = istream.read();
        }

        scriptText.setText(newText);
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

        Preferences.setProperty("ScriptsDir", String.valueOf(chooser.getCurrentDirectory()));
        // userInterface.setDefaultDirectory(String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar);

        ostream.write(scriptText.getText());
        ostream.close();
    }

    /**
     * Sets the instruction label to the message. Used mostly for Pause/Resume.
     *
     * @param  message  Text to set the label to.
     */
    private void setMessage(String message) {
        this.message = message;
        instructionLabel.setText(message);
    }

}
