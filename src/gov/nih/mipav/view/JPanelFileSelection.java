package gov.nih.mipav.view;


import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.swing.*;
import javax.swing.border.*;


/**
 * provides a nice plug-in swing panel to simplify the file-selection process. Contains a textfield on the left and a
 * browse button on the right to bring a JFileChooser front, either of which may be used to indicate the file. This
 * object should work just fine for both "open" applications as well as "save-as" applications, and indeed,
 * "folder-select" applications. The Panel may choose between permitting selection of folders and both folders and
 * directories in its file chooser.
 *
 * <p>This panel has an etched, titled border.</p>
 *
 * <p>Handles its own button clicks and text, and when the file it points to is requested. The preferred file is chosen
 * based on the text in the textfield, as there is no way to ensure that a mouseExit event or focusExit event will occur
 * before some kind of OKAY button ActionEvent which requests the file. SO.. the file is translated from the text when
 * the file is requested. If it can't find the chosen file from the text (ie., the path does not exist, or the text was
 * left blank), then the file comes from the button choice, which should be otherwise no different from the textfield.
 * Be aware that if a file does not exist in the textfield, a file of that name can still be returned -- this Panel is
 * designed to be perfectly fine with save-as type of applications.</p>
 *
 * <p>The file that comes back is unadulterated and not made. Possible future expansion may be for specialising this for
 * File input, or output, ensuring that the file exists, or finding a way to set up this panel to fire notification when
 * a file is chosen (using either the text or the file chooser).</p>
 */
public class JPanelFileSelection extends JPanelEdit implements ActionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5350976001384182834L;

    /**
     * Specifies the default directory for the panel editor, which is the &quot;user.home&quot; directory, with a
     * separator character appended at the end.
     */
    public static String defaultDirectory = System.getProperties().getProperty("user.home") + File.separator;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JFileChooser chooser;

    /** DOCUMENT ME! */
    private File chosenFile = null;

    /** DOCUMENT ME! */
    private JButton fileBrowse;

    /** DOCUMENT ME! */
    private JTextField fileTextField;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * basic constructor, creates a panel with textfield and browser. defaults to pointing at the user's default
     * directory, There is no title for either the file chooser or the panel itself, and the file chooser will search
     * for both files and directories.
     */
    public JPanelFileSelection() {
        this(null, "");
    }


    /**
     * custom constructor -- selects the preference for the file chooser to permit choice of selection of directories
     * only or both files and directories. Uses the default user - directory location in the text field, and will have
     * no title on the border.
     *
     * @param  browseFiles  DOCUMENT ME!
     */
    public JPanelFileSelection(boolean browseFiles) {
        this(new File(defaultDirectory), "", browseFiles);
    }

    /**
     * custom constructor -- Selection of the starting directory, and the title on the border. Chooser selects both
     * files and directories.
     *
     * @param  prechosenFile  DOCUMENT ME!
     * @param  title          DOCUMENT ME!
     */
    public JPanelFileSelection(File prechosenFile, String title) {
        this(prechosenFile, title, true);
    }

    /**
     * custom constructor -- Selection of the starting directory, and the title on the border. Chooser selects based on
     * argument.
     *
     * @param  prechosenFile  DOCUMENT ME!
     * @param  title          DOCUMENT ME!
     * @param  browseFiles    DOCUMENT ME!
     */
    public JPanelFileSelection(File prechosenFile, String title, boolean browseFiles) {
        super();
        setLayout(new BorderLayout());

        setPropertyName("File Selector");

        // check on the parents file because although the file in question
        // (prechosen) doesn't necesarily need to exist (say we want to
        // create prechosen), the location we want to put it in ought to exist.

        try {
            chosenFile = prechosenFile;
            /*
             * if (prechosenFile.isDirectory()) {  chosenFile = prechosenFile; } else {  chosenFile = new
             * File(defaultDirectory); } */
        } catch (NullPointerException npe) {
            chosenFile = new File(defaultDirectory);
        }

        fileTextField = new JTextField();
        fileTextField.setColumns(30);
        fileTextField.setFont(MipavUtil.font12);
        fileTextField.addActionListener(this);
        fileTextField.addKeyListener(new KeyAdapter() {
                public void keyTyped(KeyEvent typed) {
                    String newText = fileTextField.getText().substring(0, fileTextField.getCaretPosition()) +
                                     typed.getKeyChar() +
                                     fileTextField.getText().substring(fileTextField.getCaretPosition());
                    firePropertyChange(getPropertyName(), fileTextField.getText(), newText);
                }
            });

        /*
         * if (chosenFile.isDirectory() || !chosenFile.exists()) { fileTextField.setText(""); } else {
         * fileTextField.setText(chosenFile.getAbsolutePath());} */
        fileTextField.setText(chosenFile.getAbsolutePath());
        add(fileTextField, BorderLayout.CENTER);

        // handle file-browser button
        fileBrowse = new JButton("Browse");
        fileBrowse.setActionCommand("File Browse");
        fileBrowse.setPreferredSize(new Dimension(90, 30));
        fileBrowse.setFont(MipavUtil.font12B);
        fileBrowse.addActionListener(this);
        add(fileBrowse, BorderLayout.EAST);

        // handle file-browser
        chooser = new JFileChooser(chosenFile); // MUST use swing.

        /*
         * if (chosenFile.isDirectory()) { chooser.setCurrentDirectory(chosenFile); }
         */
        if (browseFiles) {
            chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
        } else {
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        }

        chooser.setDialogTitle("Select File"); // use the title is available


        // handle the title for border
        TitledBorder border = BorderFactory.createTitledBorder("");

        if (title != null) {
            border.setTitle(title);
            chooser.setDialogTitle(title); // use the title if available on browser
        }

        border.setBorder(BorderFactory.createEtchedBorder());
        border.setTitleColor(Color.black);
        border.setTitleFont(MipavUtil.font12B);
        setBorder(border);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Responds to ActionEvents, such as the &quot;Browse&quot; button being clicked. When the Browse button is
     * selected, if it returns a file, it will provide the path of the chosen file to the text field. It then fires a
     * property change for all interested listeners.
     *
     * @param  action  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent action) {

        if (action.getSource() instanceof JButton) {

            if (action.getSource() == fileBrowse) {
                int returnVal = chooser.showDialog(this, "Select");

                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    chosenFile = chooser.getSelectedFile();

                    String oldPath = fileTextField.getText();
                    fileTextField.setText(chosenFile.getAbsolutePath());
                    firePropertyChange(getPropertyName(), oldPath, fileTextField.getText());
                }
            }
        } else if (action.getSource() instanceof JTextField) {
           // System.out.println("JTEXTFIELD");
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean checkFields() {
        return true;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Object getCodedValue() {
        return new String("useless");
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getPanelValue() {
        return fileTextField.getText();
    }

    /**
     * we are enforcing existance at the calling/owning object level this panel doens't care if the file exists, nor if
     * there are read rights or write rights there. returns the File pointed to by the text field, unless it is empty;
     * at that point, we assume the file chooser was used to point to a file and the chosen file is stored there.
     *
     * @return  DOCUMENT ME!
     */
    public File getSelectedFile() {

        // assume that the text-field is correct, unless null.
        if (!fileTextField.getText().equals("")) {
            chosenFile = new File(fileTextField.getText());
        } else {
            fileTextField.setText(chosenFile.getAbsolutePath());
        }

        return chosenFile;
    }

    /**
     * gets the focus on the textfield and selects all text there. Recommended for error highlighting.
     *
     * @throws  NullPointerException  DOCUMENT ME!
     */
    public void highlight() throws NullPointerException {

        try {
            fileTextField.requestFocus();
            fileTextField.selectAll();
        } catch (NullPointerException npe) {
            throw new NullPointerException("Unable to complete.\n" + npe.getMessage());
        }
    }

    /**
     * determines how the file chooser will search. <code>true</code> will set the file chooser to permit viewing both
     * File and directories, where-as <code>false</code> will only let the user select directories.
     *
     * @param  browseFiles  DOCUMENT ME!
     */
    public void setBrowseForFiles(boolean browseFiles) {

        if (browseFiles) {
            chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
        } else {
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        }
    }

    /**
     * sets the file filter for the file chooser.
     *
     * @param  filter  DOCUMENT ME!
     */
    public void setBrowserFileFilter(javax.swing.filechooser.FileFilter filter) {
        chooser.setFileFilter(filter);
    }

    /**
     * Sets the file chooser title independantly of the panel title.
     *
     * @param  title  DOCUMENT ME!
     */
    public void setBrowserTitle(String title) {
        chooser.setDialogTitle(title);
    }

    /**
     * sets the file chooser and the text field to point to a different file. See JFileChooser.setCurrentDirectory for
     * the resetting process if an invalid File is provided.
     *
     * @param  current  DOCUMENT ME!
     *
     * @See    JFileChooser#setCurrentDirectory()
     */
    public void setCurrentDirectory(File current) {
        chooser.setCurrentDirectory(current);
        fileTextField.setText(chooser.getCurrentDirectory().getAbsolutePath());
    }

    /**
     * Sets the panel title independant of the file chooser title.
     *
     * @see  #setBrowserTitle()
     */
    public void setPanelTitle(String title) {
        ((TitledBorder) getBorder()).setTitle(title);
    }

}
