package gov.nih.mipav.view;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

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
     * Creates a new ViewDirectoryChooser object.
     *
     * @deprecated  Creates a dialog for choosing a directory.
     *
     * @param       ui  Main user interface.
     */
    public ViewDirectoryChooser(ViewUserInterface ui) {
        this();
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

    /**
     * Creates a new ViewDirectoryChooser object.
     *
     * @deprecated  Creates a dialog for choosing a directory with a parent frame.
     *
     * @param       ui      Main user interface.
     * @param       parent  Parent frame.
     */
    public ViewDirectoryChooser(ViewUserInterface ui, Component parent) {
        this(parent);
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

    /**
     * Open an image based on the suffix of the file.
     *
     * @param   fileName   Full pathname of file to open.
     * @param   multiFile  Flag to indicate if image is an array of 2D image files (<code>true</code>) or if the image
     *                     is stored in a single file (<code>false</code>).
     * @param   fileInfo   File info, can be null. In the case of RAW images read by the script, will not be null.
     *
     * @return  The image name of the image that was read in.
     */
    public String open(String fileName, boolean multiFile, FileInfoBase fileInfo) {
        int index;
        ModelImage image;
        ModelLUT LUT;
        ViewJFrameImage imageFrame;
        FileIO fileIO = null;
        String directory;

        // separate fileName and directory
        index = fileName.lastIndexOf(File.separatorChar);

        if (index <= 0) {
            return null;
        }

        directory = fileName.substring(0, index + 1); // ends with File.separator
        Preferences.debug(directory);
        fileName = fileName.substring(index + 1, fileName.length());
        Preferences.debug(fileName);

        try {
            fileIO = new FileIO();
            image = fileIO.readImage(fileName, directory, multiFile, fileInfo);
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return null;
        }

        if (image == null) {
            return null;
        }

        // Not sure if this is really needed or wanted -- Matt 6/2004. Tagged for removal.
        // image.getMatrix().identity();
        LUT = fileIO.getModelLUT(); // LUT is not null if TIFF image has a LUT else it is null
                                    // and create in ViewFrameImage

        try {
            imageFrame = new ViewJFrameImage(image, LUT);
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return null;
        }

        UI.getMainFrame().pack();

        if (UI.isScriptRecording()) {

            if (image.getFileInfo(0).getFileFormat() != FileBase.RAW) { // RAW files need special info appended so it's
                                                                        // done in that function.
                UI.getScriptDialog().putVar(image.getImageName());

                if (multiFile) {
                    UI.getScriptDialog().append("OpenMultiFile " + UI.getScriptDialog().getVar(image.getImageName()) +
                                                "\n");
                } else {
                    UI.getScriptDialog().append("OpenImage " + UI.getScriptDialog().getVar(image.getImageName()) +
                                                "\n");
                }
            }
        }

        return image.getImageName();

    }

} // end class ViewDirectoryChooser
