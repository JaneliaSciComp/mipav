import gov.nih.mipav.plugins.PlugInGeneric;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.*;

import java.io.File;
import java.util.*;

import javax.swing.JFileChooser;


public class PlugInDicom2Conversion implements PlugInGeneric {
    public PlugInDicom2Conversion() {}

    public void run() {
        final ArrayList<Vector<String>> openImagesArrayList = openImage();

        if (openImagesArrayList != null) {
            for (int i = 0; i < openImagesArrayList.size(); i++) {
                final Vector<String> openImageNames = openImagesArrayList.get(i);

                // if open failed, then imageNames will be null
                if (openImageNames == null) {
                    return;
                }

                for (final String imgName : openImageNames) {
                    final ModelImage img = ViewUserInterface.getReference().getRegisteredImageByName(imgName);

                    final FileInfoBase[] fInfos = img.getFileInfo();
                    for (final FileInfoBase fInfo : fInfos) {
                        if (fInfo instanceof FileInfoDicom) {
                            ((FileInfoDicom) fInfo).containsDICM = true;
                        }
                    }

                    saveImage(img);
                }
            }
        }
    }

    private ArrayList<Vector<String>> openImage() {
        final ViewOpenFileUI openFile = new ViewOpenFileUI(true);

        final boolean stackFlag = true;

        // set the filter type to the preferences saved filter
        int filter = ViewImageFileFilter.TECH;

        try {
            filter = Integer.parseInt(Preferences.getProperty(Preferences.PREF_FILENAME_FILTER));
        } catch (final NumberFormatException nfe) {

            // an invalid value was set in preferences -- so fix it!
            filter = ViewImageFileFilter.TECH;
            Preferences.setProperty(Preferences.PREF_FILENAME_FILTER, Integer.toString(filter));
        }

        openFile.setFilterType(filter);

        return openFile.open(stackFlag);
    }

    private void saveImage(final ModelImage img) {
        final FileWriteOptions options = new FileWriteOptions(true);
        int filterType = -1;

        String fileName = null;
        String extension = null;
        String directory = null;
        String suffix = null;
        int fileType = FileUtility.UNDEFINED;
        ViewImageFileFilter vFilter = null;
        int i;

        // save into its own subdirectory when on SaveAs.
        // (preferrably used in multi-file formats., ie DICOM)
        options.setSaveInSubdirectory(true);

        if (options.isSet()) {
            fileName = options.getFileName();
            directory = options.getFileDirectory();
        } else {

            try {
                final ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, true);

                try {

                    // try to prefill the "save as" text area
                    if (img.getFileInfo(0).getFileDirectory() != null) {
                        fileChooser.getFileChooser().setSelectedFile(
                                new File(img.getFileInfo(0).getFileDirectory() + img.getImageFileName()));
                    } else {
                        fileChooser.getFileChooser().setSelectedFile(new File(img.getImageFileName()));
                    }
                } catch (final Throwable t) {
                    // if prefill fails, do nothing
                }

                final JFileChooser chooser = fileChooser.getFileChooser();

                // chooser.setName("Save image as");
                if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                    chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
                }

                if (filterType >= 0) {
                    chooser.addChoosableFileFilter(new ViewImageFileFilter(filterType));
                } else {
                    chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
                    chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
                }

                final int returnVal = chooser.showSaveDialog(img.getParentFrame());

                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    fileName = chooser.getSelectedFile().getName();

                    if (filterType >= 0) {
                        i = fileName.lastIndexOf('.');

                        if ( (i > 0) && (i < (fileName.length() - 1))) {
                            extension = fileName.substring(i + 1).toLowerCase();
                            vFilter = new ViewImageFileFilter(filterType);

                            if ( !vFilter.accept(extension)) {
                                MipavUtil.displayError("Extension does not match filter type");

                                return;
                            }
                        } // if ( i > 0 && i < fileName.length() - 1 )
                        else if (i < 0) {

                            switch (filterType) {

                                case ViewImageFileFilter.AVI:
                                    fileName = fileName + ".avi";
                                    break;

                                case ViewImageFileFilter.VOI:
                                    fileName = fileName + ".voi";
                                    break;

                                case ViewImageFileFilter.FUNCT:
                                    fileName = fileName + ".fun";
                                    break;

                                case ViewImageFileFilter.LUT:
                                    fileName = fileName + ".lut";
                                    break;

                                case ViewImageFileFilter.PLOT:
                                    fileName = fileName + ".plt";
                                    break;

                                case ViewImageFileFilter.CLASS:
                                    fileName = fileName + ".class";
                                    break;

                                case ViewImageFileFilter.SCRIPT:
                                    fileName = fileName + ".sct";
                                    break;

                                case ViewImageFileFilter.SURFACE:
                                    fileName = fileName + ".sur";
                                    break;

                                case ViewImageFileFilter.FREESURFER:
                                    fileName = fileName + ".asc";
                                    break;
                            }
                        } // else if (i < 0)
                    } // if (filterType >= 0)

                    directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                    ViewUserInterface.getReference().setDefaultDirectory(directory);
                } else {
                    return;
                }

            } catch (final OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ViewJFrameBase.save");
                Preferences.debug("Out of memory: ViewJFrameBase.save\n", Preferences.DEBUG_COMMS);

                return;
            }
        }

        /*
         * I'm not sure why this wasn't done before.... if we do a save-as we should also update the name of the file
         */
        // if (options.isSaveAs()) {
        // img.setImageName(fileName.substring(0, fileName.length()-4));
        // }
        options.setFileName(fileName);
        options.setFileDirectory(directory);

        if (fileName != null) {
            final FileIO fileIO = new FileIO();

            fileIO.writeImage(img, options);
        }

        // set the new fileName and directory in the fileInfo for the img -- so that it's
        // updated correctly in memory as well -- don't move this before the saveAllOnSave loop --
        // that needs to look at the former settings!
        final FileInfoBase[] fileInfo = img.getFileInfo();

        if (suffix == null) {
            suffix = FileUtility.getExtension(fileName);

            if (suffix.equals("")) {
                fileName = options.getFileName();
                suffix = FileUtility.getExtension(fileName);
            }

            boolean zerofunused[] = new boolean[1];
            fileType = FileUtility.getFileType(fileName, directory, false, false, zerofunused);
        }

        // now, get rid of any numbers at the end of the name (these
        // are part of the dicom file name, but we only want the 'base'
        // part of the name
        String baseName = new String(fileName);

        if (fileType == FileUtility.DICOM) {
            final int index = fileName.lastIndexOf(".");

            if (index > 0) {
                baseName = fileName.substring(0, index);
            }

            int newIndex = baseName.length();

            for (i = baseName.length() - 1; i >= 0; i--) {
                final char myChar = baseName.charAt(i);

                if (Character.isDigit(myChar)) {
                    newIndex = i;
                } else {
                    break;
                } // as soon as something is NOT a digit, leave loop
            }

            if (newIndex > 0) {
                baseName = baseName.substring(0, newIndex);
            }

            fileName = new String(baseName + ".dcm");

            if ( !directory.endsWith(baseName)) {
                directory = new String(directory + baseName + File.separator);
            }
        }

        for (i = 0; i < fileInfo.length; i++) {
            fileInfo[i].setFileDirectory(directory);

            if (fileType == FileUtility.DICOM) {
                fileInfo[i].setFileName(baseName + (i + 1) + ".dcm");
            } else {
                fileInfo[i].setFileName(fileName);
            }

            fileInfo[i].setFileSuffix(suffix);
            // fileInfo[i].setFileFormat (fileType);
        }
    }
}
