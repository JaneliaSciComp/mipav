package gov.nih.mipav.view;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;

import java.util.*;

import javax.swing.*;


/**
 * Opens an image file by calling FileIO.readImage and puts it into an image frame.
 *
 * @version  0.1 Sept 4, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      FileIO
 */
public class ViewOpenFileUI extends ViewFileChooserBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Indicate what the current filter should be on opening the the dialog. The values for this are the static ints in
     * ViewImageFileFilter.
     */
    private int filterType = -1;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private ModelImage image2;

    /** DOCUMENT ME! */
    private ModelLUT LUT = null;

    /** DOCUMENT ME! */
    private ModelLUT LUT2 = null;

    /** DOCUMENT ME! */
    private ModelRGB modelRGB = null;

    /** DOCUMENT ME! */
    private ModelRGB modelRGB2 = null;

    /** DOCUMENT ME! */
    private boolean putInFrame = true; // default is to put the image into a frame

    /** DOCUMENT ME! */
    private boolean setLastImageFlag = true; // default is to put image path into recently opened image list

    /** DOCUMENT ME! */
    private boolean xmlLinked = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates the way to open and verify files before they are loaded by the FileIO.
     *
     * @param  ui          Main user interface.
     * @param  openDialog  a boolean that selects whether this UI should be built and displayed. <code>true</code>
     *                     indicates the UI will be displayed, and <code>false</code> is that it will not be displayed.
     */
    public ViewOpenFileUI(ViewUserInterface ui, boolean openDialog) {
        super(ui, openDialog, false);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param   chooser     DOCUMENT ME!
     * @param   filterType  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static final javax.swing.filechooser.FileFilter findFilter(JFileChooser chooser, int filterType) {

        // get the list of file filters held in chooser
        javax.swing.filechooser.FileFilter[] filters = chooser.getChoosableFileFilters();

        for (int i = 0; i < filters.length; i++) {

            if (ViewImageFileFilter.matches(filters[i], filterType)) {
                return filters[i];
            }
        }

        return null;

    } // end findFilter()

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage getImage() {
        return image;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage getImage2() {
        return image2;
    }

    /**
     * Returns the path to the last opened image.
     *
     * @return  imagePath
     */
    public String getImagePath() {
        return this.directory + this.fileName;
    }

    /**
     * Returns the model LUT from the file.
     *
     * @return  ModelLUT -- returns LUT.
     */
    public ModelLUT getLUT() {

        return this.LUT;
    }

    /**
     * Returns the model LUT2 from the file.
     *
     * @return  ModelLUT -- returns LUT2.
     */
    public ModelLUT getLUT2() {

        return this.LUT2;
    }

    /**
     * Indicates whether there is an xml linked image.
     *
     * @return  xmllinked
     */
    public boolean hasXMLLinked() {
        return this.xmlLinked;
    }

    /**
     * Returns whether or not the new image will be put into a frame.
     *
     * @return  boolean -- if true, then image will be put into a frame.
     */
    public boolean isPutInFrame() {

        return this.putInFrame;
    }


    /**
     * Open an image based on the suffix of the file.
     *
     * @param   multiFile  Flag to indicate if image is an array of 2D image files (<code>true</code>) or if the image
     *                     is stored in a single file (<code>false</code>).
     * @param   imageset   DOCUMENT ME!
     *
     * @return  Vector Vector of the names of the images that were opened.
     */
    public Vector open(boolean multiFile, boolean imageset) {
        ViewJFrameImage imageFrame;
        FileIO fileIO = new FileIO();
        FileIO fileIO2 = null;
        Vector images = new Vector();
        ModelImage linkedImage = null;

        int secondImage = 0; // 0 if not present

        // address of TIFF header of second image in file if present
        // for LSM510 image files

        // set the filter type to the preferences saved filter
        int filter = 0;

        try {
            filter = Integer.parseInt(Preferences.getProperty("FilenameFilter"));
        } catch (NumberFormatException nfe) {

            // an invalid value was set in preferences -- so don't use it!
            filter = -1;
        }

        if (!useAWT) {

            try {
                setMulti(multiFile);

                // chooser = new JFileChooser();
                if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                    File file = new File(UI.getDefaultDirectory());

                    if (file != null) {
                        chooser.setCurrentDirectory(file);
                    } else {
                        chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                    }
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }

                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

                if (filter != -1) {
                    // it seems that the set command adds the filter again...
                    // chooser.addChoosableFileFilter(new ViewImageFileFilter(filter));

                    // if filter is something we already added, then remove it before
                    // setting it..... (kludgy, kludgy....)
                    javax.swing.filechooser.FileFilter found = findFilter(chooser, filter);

                    if (found != null) {
                        chooser.removeChoosableFileFilter(found);
                    }

                    // initially set to the preferences
                    chooser.setFileFilter(new ViewImageFileFilter(filter));
                }

                // but if the filterType was set, then use that instead
                // set the current filter to filterType
                if (filterType != -1) { // filterType has been set

                    // don't add this filter twice --- if it's there, then remove it
                    javax.swing.filechooser.FileFilter found2 = findFilter(chooser, filterType);

                    if (found2 != null) {
                        chooser.removeChoosableFileFilter(found2);
                    }

                    // set the current file filter
                    chooser.setFileFilter(new ViewImageFileFilter(filterType));
                }

                chooser.setDialogTitle("Open Image");

                int returnValue = chooser.showOpenDialog(UI.getMainFrame());

                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    fileIO = new FileIO();
                    fileName = chooser.getSelectedFile().getName();
                    openedFile = chooser.getSelectedFile();
                    directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                    UI.setDefaultDirectory(directory);
                    multiFile = isMulti();
                    UI.setLastStackFlag(multiFile);
                } else {
                    return null;
                }
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory!");

                return null;
            }
        }

        try {
            image = fileIO.readImage(fileName, directory, multiFile, null);

            if (image == null) {
                System.err.println("ViewOpenFileUI: image = null");

                return null;
            }

            LUT = fileIO.getModelLUT();
            modelRGB = fileIO.getModelRGB();

            linkedImage = readLinkedImage();

            if (linkedImage != null) {
                xmlLinked = true;

                LUT2 = fileIO.getModelLUT();
                modelRGB2 = fileIO.getModelRGB();

                // in the case of iaso running, put in frame will be false
                // this must be changed to true if the modalities are already set up
                // in the image and linkedImage
                if ((((image.getFileInfo(0).getModality() == FileInfoBase.RED_FREE) &&
                          (linkedImage.getFileInfo(0).getModality() == FileInfoBase.ICG)) ||
                         ((image.getFileInfo(0).getModality() == FileInfoBase.ICG) &&
                              (linkedImage.getFileInfo(0).getModality() == FileInfoBase.RED_FREE))) && !imageset) {

                    this.putInFrame = true;
                }
            }
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return null;
        }

        try {

            if (putInFrame == true) {
                imageFrame = new ViewJFrameImage(image, LUT, ViewUserInterface.getReference().getNewFrameLocation());

                if (modelRGB != null) {
                    imageFrame.setRGBTA(modelRGB);
                }

                if (xmlLinked && (linkedImage != null) && !imageset) {
                    imageFrame.setAndLoad(linkedImage);

                    if (LUT2 != null) {

                        if (modelRGB2 != null) {
                            imageFrame.setRGBTB(modelRGB2);
                        }

                        imageFrame.setLUTb(LUT2);
                        imageFrame.updateImages(LUT, LUT2, true, 0);
                    }
                }
            }

            images.addElement(image.getImageName());

            if (xmlLinked) {
                images.addElement(linkedImage.getImageName());
            }
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return null;
        }

        secondImage = fileIO.getSecondImage();

        if (UI.isScriptRecording()) {

            if ((image.getFileInfo(0).getFileFormat() != FileBase.RAW) &&
                    (image.getFileInfo(0).getFileFormat() != FileBase.RAW_MULTIFILE)) {

                // RAW files need special info appended so it's done in that function.
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

        if (secondImage != 0) {

            try {
                fileIO2 = new FileIO();
                image2 = fileIO2.readImage(fileName, directory, multiFile, null, secondImage);
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory!");
                image2 = null;
            }

            if (image2 != null) {
                LUT2 = fileIO2.getModelLUT(); // LUT is not null if TIFF image has a LUT

                // or compressed AVI image has a LUT
                // else it is null
                // and create in ViewFrameImage
                try {

                    if (putInFrame == true) {
                        new ViewJFrameImage(image2, LUT2, ViewUserInterface.getReference().getNewFrameLocation());
                    }

                    images.addElement(image2.getImageName());
                } catch (OutOfMemoryError e) {
                    MipavUtil.displayError("Out of memory!");

                    return images; // we did successfully open the first image
                }

                if (UI.isScriptRecording()) {

                    if ((image2.getFileInfo(0).getFileFormat() != FileBase.RAW) &&
                            (image2.getFileInfo(0).getFileFormat() != FileBase.RAW_MULTIFILE)) {

                        // RAW files need special info appended so it's done in that function.
                        UI.getScriptDialog().putVar(image2.getImageName());

                        if (multiFile) {
                            UI.getScriptDialog().append("OpenMultiFile " +
                                                        UI.getScriptDialog().getVar(image2.getImageName()) + "\n");
                        } else {
                            UI.getScriptDialog().append("OpenImage " +
                                                        UI.getScriptDialog().getVar(image2.getImageName()) + "\n");
                        }
                    }
                }
            }
        } // if (secondImage != 0)

        // System.err.println("Getting to set last image flag");
        if (setLastImageFlag) {
            Preferences.setLastImage(directory + fileName, image.getFileInfo()[0].getMultiFile(), image.getNDims());
        }

        // updates menubar for each image
        Vector imageFrames = UI.getImageFrameVector();

        if (imageFrames.size() < 1) {
            UI.buildMenu();
            UI.setControls();
        } else {
            UI.buildMenu();

            for (int i = 0; i < imageFrames.size(); i++) {

                if (imageFrames.elementAt(i) instanceof ViewJFrameImage) {
                    ((ViewJFrameImage) (imageFrames.elementAt(i))).updateMenubar();
                }
            }

            UI.getActiveImageFrame().setControls();
        }

        return images;
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
        ViewJFrameImage imageFrame;
        FileIO fileIO = null;
        ViewJFrameImage imageFrame2;
        FileIO fileIO2 = null;
        String directory;
        int secondImage = 0; // 0 if not present

        // address of TIFF header of second image in file if present
        // for LSM510 image files

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
            secondImage = fileIO.getSecondImage();
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return null;
        }

        if (image == null) {
            return null;
        }

        // image.getMatrix().identity();
        LUT = fileIO.getModelLUT(); // LUT is not null if TIFF image has a LUT else it is null

        modelRGB = fileIO.getModelRGB();

        ModelImage linkedImage = readLinkedImage();

        try {

            if (putInFrame == true) {
                imageFrame = new ViewJFrameImage(image, LUT, ViewUserInterface.getReference().getNewFrameLocation());

                if (modelRGB != null) {
                    imageFrame.setRGBTA(modelRGB);
                }

                if (linkedImage != null) {
                    imageFrame.setAndLoad(linkedImage);

                    if (LUT2 != null) {

                        if (modelRGB2 != null) {
                            imageFrame.setRGBTB(modelRGB2);
                        }

                        imageFrame.setLUTb(LUT2);
                        imageFrame.updateImages(LUT, LUT2, true, 0);
                    }
                }
            }
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return null;
        }

        if (UI.isScriptRecording()) {

            if ((image.getFileInfo(0).getFileFormat() != FileBase.RAW) &&
                    (image.getFileInfo(0).getFileFormat() != FileBase.RAW_MULTIFILE)) {

                // RAW files need special info appended so it's done in that function.
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

        if (secondImage != 0) {

            try {
                fileIO2 = new FileIO();
                image2 = fileIO2.readImage(fileName, directory, multiFile, fileInfo, secondImage);
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory!");

                return null;
            }

            if (image2 == null) {
                return null;
            }

            // image2.getMatrix().identity();
            LUT2 = fileIO2.getModelLUT(); // LUT is not null if TIFF image has a LUT else it is null
            modelRGB2 = fileIO2.getModelRGB();

            // and create in ViewFrameImage
            try {

                if (putInFrame == true) {
                    imageFrame2 = new ViewJFrameImage(image2, LUT2,
                                                      ViewUserInterface.getReference().getNewFrameLocation());

                    if (modelRGB2 != null) {
                        imageFrame2.setRGBTA(modelRGB2);
                    }
                }
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory!");

                return null;
            }

            // UI.getMainFrame().pack();
            if (UI.isScriptRecording()) {

                if ((image2.getFileInfo(0).getFileFormat() != FileBase.RAW) &&
                        (image2.getFileInfo(0).getFileFormat() != FileBase.RAW_MULTIFILE)) {

                    // RAW files need special info appended so it's done in that function.
                    UI.getScriptDialog().putVar(image2.getImageName());

                    if (multiFile) {
                        UI.getScriptDialog().append("OpenMultiFile " +
                                                    UI.getScriptDialog().getVar(image2.getImageName()) + "\n");
                    } else {
                        UI.getScriptDialog().append("OpenImage " + UI.getScriptDialog().getVar(image2.getImageName()) +
                                                    "\n");
                    }
                }
            }

        } // if (secondImage != 0)


        if (setLastImageFlag) {
            Preferences.setLastImage(directory + fileName, image.getFileInfo()[0].getMultiFile(), image.getNDims());
        }

        // updates menubar for each image
        Vector imageFrames = UI.getImageFrameVector();

        if (imageFrames.size() < 1) {
            UI.buildMenu();
            UI.setControls();
        } else {
            UI.buildMenu();

            for (int i = 0; i < imageFrames.size(); i++) {

                if (imageFrames.elementAt(i) instanceof ViewJFrameImage) {
                    ((ViewJFrameImage) (imageFrames.elementAt(i))).updateMenubar();
                }
            }
        }

        return image.getImageName();

    }

    /**
     * Set the current filter type to the given parameter.
     *
     * @param  fType  Indicate what the current filter should be on opening the the dialog. The values for this are the
     *                static ints in ViewImageFileFilter.
     */
    public void setFilterType(int fType) {
        this.filterType = fType;
    }

    /**
     * Indicates whether or not to put the image file path into the list of recently opened images.
     *
     * @param  flag  if true, then image path is put into recently used image list
     */
    public void setLastImage(boolean flag) {
        this.setLastImageFlag = flag;
    }

    /**
     * Indicates whether or not the new image should be put into a frame.
     *
     * @param  putIn  -- if true, then image is put into a frame.
     */
    public void setPutInFrame(boolean putIn) {

        this.putInFrame = putIn;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private ModelImage readLinkedImage() {

        if (image.getFileInfo(0) instanceof FileInfoImageXML) {
            FileInfoImageXML fileInfoXML = (FileInfoImageXML) image.getFileInfo(0);
            String linkedFilename = fileInfoXML.getLinkedImagePath();

            if ((linkedFilename != null) && !linkedFilename.equals("")) {
                FileIO fileIO = new FileIO();

                return fileIO.readImage(linkedFilename);
            }

        }

        return null;
    }
}
