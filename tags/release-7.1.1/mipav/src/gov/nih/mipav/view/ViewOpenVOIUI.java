package gov.nih.mipav.view;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;

import javax.swing.*;


/**
 * User interface to open a VOI.
 *
 * @version  0.1 Feb 24, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */

public class ViewOpenVOIUI {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The main user interface. */
    private ViewUserInterface UI;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor to allow user to open a file.
     */
    public ViewOpenVOIUI() {
        UI = ViewUserInterface.getReference();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Open a VOI array based on the suffix of the file.
     *
     * @param   image    Image to open the VOI on.
     * @param   doLabel  DOCUMENT ME!
     *
     * @return  the opened VOIs
     */
    public VOI[] open(ModelImage image, boolean doLabel) {
        VOI[] voi;
        FileCheshireVOI fileCheshireVOI;
        FileVOI fileVOI;
        String extension = " ";
        String fileName;
        String directory;
        int i;

        JFileChooser chooser = new JFileChooser();

        chooser.setDialogTitle("Open VOI");

        if (UI.getDefaultDirectory() != null) {
            File file = new File(UI.getDefaultDirectory());

            if (file != null) {
                chooser.setCurrentDirectory(file);
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }
        } else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }

        if (doLabel) {
            chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { ".lbl" }));
        } else {
            chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { ".xml", ".voi" }));
        }

        int returnValue = chooser.showOpenDialog(UI.getMainFrame());

        if (returnValue == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            UI.setDefaultDirectory(directory);
        } else {
            return null;
        }

        try {
            i = fileName.lastIndexOf('.');

            if ((i > 0) && (i < (fileName.length() - 1))) {
                extension = fileName.substring(i + 1).toLowerCase();
            }

            if (extension.equals("oly")) {
                fileCheshireVOI = new FileCheshireVOI(fileName, directory, image);
                voi = fileCheshireVOI.readVOI();
            } else {
                fileVOI = new FileVOI(fileName, directory, image);

                if (doLabel || extension.equals("lbl")) {
                    voi = fileVOI.readVOI(true);
                } else {
                    voi = fileVOI.readVOI(false);
                }
            }
        } catch (IOException error) {
            MipavUtil.displayError(error.getMessage());

            return null;
        }

        if (voi == null) {
            MipavUtil.displayError("VOI is null");

            return null;
        }

        // UI.getController().registerModel(voi);
        // System.err.println("registering VOIS");
        for (i = 0; i < voi.length; i++) {
            image.registerVOI(voi[i]);
        }

        image.notifyImageDisplayListeners();

        return (voi);
    }
    
    /**
     * Open a VOI array based on the suffix of the file.
     *
     * @param   image    Image to open the VOI on.
     *
     * @return  the opened VOIs
     */
    public VOI[] openOtherOrientation(ModelImage image) {
        VOI[] voi;
        FileCheshireVOI fileCheshireVOI;
        FileVOI fileVOI;
        String extension = " ";
        String fileName;
        String directory;
        int i;

        JFileChooser chooser = new JFileChooser();

        chooser.setDialogTitle("Open VOI");

        if (UI.getDefaultDirectory() != null) {
            File file = new File(UI.getDefaultDirectory());

            if (file != null) {
                chooser.setCurrentDirectory(file);
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }
        } else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }

        
        chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { ".xml", ".voi" }));

        int returnValue = chooser.showOpenDialog(UI.getMainFrame());

        if (returnValue == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            UI.setDefaultDirectory(directory);
        } else {
            return null;
        }

        try {
            i = fileName.lastIndexOf('.');

            if ((i > 0) && (i < (fileName.length() - 1))) {
                extension = fileName.substring(i + 1).toLowerCase();
            }

            if (extension.equals("oly")) {
                fileCheshireVOI = new FileCheshireVOI(fileName, directory, image);
                voi = fileCheshireVOI.readVOI();
            } else {
                fileVOI = new FileVOI(fileName, directory, image);
                voi = fileVOI.readOtherOrientationVOI();
            }
        } catch (IOException error) {
            MipavUtil.displayError(error.getMessage());

            return null;
        }

        if (voi == null) {
            MipavUtil.displayError("VOI is null");

            return null;
        }

        // UI.getController().registerModel(voi);
        // System.err.println("registering VOIS");
        for (i = 0; i < voi.length; i++) {
            image.registerVOI(voi[i]);
        }

        image.notifyImageDisplayListeners();

        return (voi);
    }
}
