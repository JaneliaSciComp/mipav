package gov.nih.mipav.view;


import java.awt.*;

import java.io.*;

import javax.swing.*;


/**
 * THIS CLASS IS MOST LIKELY TO BE DESTROYED!!!!!!!!!!
 * 
 * Displays a chooser for selecting a directory.
 *
 * @version  0.1 Dec. 6, 2001
 * @author   Lynne Pusanik
 */
public class ViewDirectoryChooser {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private String directory;

    /** DOCUMENT ME! */
    private Component parentFrame;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a dialog for choosing a directory.
     */
    public ViewDirectoryChooser() {
        UI = ViewUserInterface.getReference();
        parentFrame = UI.getMainFrame();
    }

    /**
     * Creates a dialog for choosing a directory with a parent frame.
     *
     * @param  parent  Parent frame.
     */
    public ViewDirectoryChooser(Component parent) {
        UI = ViewUserInterface.getReference();
        parentFrame = parent;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Shows the file dialogs with the given directory as the default.
     *
     * @param   initialDirectory  Initial directory.
     *
     * @return  Directory chosen by user.
     */
    public String chooseDirectory(String initialDirectory) {
        JFileChooser chooser = null;
        String fileName;
        String dir;
        File file;
        File tmpFile;

        directory = null;

        try {

            // first find the file to use as a default
            file = new File(initialDirectory);

            if (file == null) {

                // if no script directory is set, try the image directory
                tmpFile = new File(UI.getDefaultDirectory());

                if (tmpFile == null) {

                    // if all else fails, try the user directory
                    file = new File(System.getProperty("user.dir"));
                } else {
                    file = tmpFile;
                }
            }

            chooser = new JFileChooser();
            chooser.setLocation(Toolkit.getDefaultToolkit().getScreenSize().width / 2,
                                Toolkit.getDefaultToolkit().getScreenSize().height / 2);

            // set the starting directory
            if (file.getParent() != null) {
                chooser.setCurrentDirectory(new File(file.getParent()));
                chooser.setSelectedFile(file);
            } else {
                chooser.setCurrentDirectory(file);
            }

            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            chooser.setDialogTitle("Choose Directory");

            int returnValue = chooser.showOpenDialog(parentFrame);

            if (returnValue == JFileChooser.APPROVE_OPTION) {

                fileName = chooser.getSelectedFile().getName();
                dir = String.valueOf(chooser.getCurrentDirectory());

                // if fileName is null, then assume the parent
                // directory is the chosen directory
                if (fileName == null) {
                    directory = dir;
                }

                // otherwise, directory is dir /fileName
                else {
                    directory = new String(dir + File.separatorChar + fileName);
                }
            }
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return (String) null;
        }

        return directory;
    }

    /**
     * Displays a dialog to select a directory, with the script directory as the default.
     *
     * @return  Directory chosen by user.
     */
    public String getDirectory() {
        return chooseDirectory(UI.getDefaultScriptDirectory());
    }

    /**
     * Displays a dialog to select a directory, with the image directory as the default.
     *
     * @return  Directory chosen by user.
     */
    public String getImageDirectory() {
        return chooseDirectory(UI.getDefaultDirectory());
    }
} // end class ViewDirectoryChooser
