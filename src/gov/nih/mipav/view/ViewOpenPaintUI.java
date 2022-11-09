package gov.nih.mipav.view;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;
import java.util.BitSet;

import javax.swing.*;


public class ViewOpenPaintUI {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The main user interface. */
    private ViewUserInterface UI;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor to allow user to open a file.
     */
    public ViewOpenPaintUI() {
        UI = ViewUserInterface.getReference();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Open a VOI array based on the suffix of the file.
     *
     * @param   image    Image to open the paint bitmap on.
     *
     * @return  the paint bitmap
     */
    public BitSet open(ModelImage image) {
        BitSet paintBitmap = null;
        FilePaintBitmap filePaint;
        String fileName;
        String directory;

        JFileChooser chooser = new JFileChooser();

        chooser.setDialogTitle("Open Paint");

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

        
        chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { ".pbm", }));

        int returnValue = chooser.showOpenDialog(UI.getMainFrame());

        if (returnValue == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            UI.setDefaultDirectory(directory);
        } else {
            return null;
        }

        try {
            
           filePaint = new FilePaintBitmap(fileName, directory, image);

           paintBitmap = filePaint.readPaintBitmap();
        } catch (IOException error) {
            MipavUtil.displayError(error.getMessage());

            return null;
        }

        if (paintBitmap == null) {
            MipavUtil.displayError("paint bitmap is null");

            return null;
        }

        // UI.getController().registerModel(voi);
        // System.err.println("registering VOIS");
        ViewJComponentEditImage componentImage = image.getParentFrame().getComponentImage();
        componentImage.setPaintMask(paintBitmap);

        image.notifyImageDisplayListeners();

        return paintBitmap;
    }
}
