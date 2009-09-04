package gov.nih.mipav.model.srb;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import edu.sdsc.grid.io.*;
import edu.sdsc.grid.io.local.*;
import edu.sdsc.grid.io.srb.*;

import java.io.*;

import java.util.*;


public class SRBUtility {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Converts the comma separated file names string into the file name list.
     *
     * @param   fileNames  the comma separated file names string.
     *
     * @return  the list of the file names.
     */
    public static List<String> converToFileNameList(String fileNames) {

        if ((fileNames == null) || (fileNames.length() == 0)) {
            return null;
        }

        Vector<String> newFileNameList = null;

        if (fileNames.indexOf(",") >= 0) {
            String[] fns = fileNames.split(",");

            if ((fns == null) || (fns.length == 0)) {
                return null;
            }

            newFileNameList = new Vector<String>(fns.length);

            for (int i = 0; i < fns.length; i++) {
                newFileNameList.add(fns[i]);
            }

        } else {
            newFileNameList = new Vector<String>(1);
            newFileNameList.add(fileNames);
        }

        return newFileNameList;
    }

    /**
     * Parses the string to extract the file informations.
     *
     * @param   fileSystem  the file system that the file reside.
     * @param   fileName    the file names string separated by comma.
     *
     * @return  the GeneralFile object array.
     */
    public static GeneralFile[] converToFiles(GeneralFileSystem fileSystem, String fileName) {
        GeneralFile[] selectedFiles = null;

        if (fileName.indexOf(",") >= 0) {
            String[] fileNames = fileName.split(",");

            if ((fileNames == null) || (fileNames.length == 0)) {
                return null;
            }

            selectedFiles = new GeneralFile[fileNames.length];

            for (int i = 0; i < fileNames.length; i++) {
                GeneralFile newFile = FileFactory.newFile(fileSystem, fileNames[i]);

                if (newFile == null) {
                    return null;
                }

                selectedFiles[i] = newFile;
            }
        } else {
            selectedFiles = new GeneralFile[1];

            GeneralFile newFile = FileFactory.newFile(fileSystem, fileName);

            if (newFile == null) {
                return null;
            }

            selectedFiles[0] = newFile;
        }

        return selectedFiles;
    }

    /**
     * Converts the list of GeneralFile into the string separated by comma.
     *
     * @param   files  a file array.
     *
     * @return  the string representation of the file array.
     */
    public static String convertToString(File[] files) {

        if ((files == null) || (files.length == 0)) {
            return null;
        }

        StringBuffer sb = new StringBuffer();

        for (int i = 0; i < files.length; i++) {

            if (sb.length() == 0) {
                sb.append(files[i].getPath());
            } else {
                sb.append("," + files[i].getPath());
            }
        }

        return sb.toString();
    }

    /**
     * Converts the list of GeneralFile into the string separated by comma.
     *
     * @param   files  a file array
     *
     * @return  the string representation of the file array.
     */
    public static String convertToString(GeneralFile[] files) {

        if ((files == null) || (files.length == 0)) {
            return null;
        }

        StringBuffer sb = new StringBuffer();

        for (int i = 0; i < files.length; i++) {

            if (sb.length() == 0) {
                sb.append(files[i].getPath());
            } else {
                sb.append("," + files[i].getPath());
            }
        }

        return sb.toString();
    }

    /**
     * Creates the complete file list based on the partial file list.
     *
     * @param   sourceFiles  the partial file list.
     *
     * @return  the complete file list.
     */
    public static GeneralFile[] createCompleteFileList(GeneralFile[] sourceFiles) {

        if (sourceFiles == null) {
            return null;
        }

        Vector<GeneralFile> completeFileList = new Vector<GeneralFile>(sourceFiles.length);

        for (int i = 0; i < sourceFiles.length; i++) {
            completeFileList.add(sourceFiles[i]);

            GeneralFile partnerFile = getPartnerFile(sourceFiles[i]);

            if (partnerFile != null) {
                completeFileList.add(partnerFile);
            }
        }

        GeneralFile[] completeFiles = new GeneralFile[completeFileList.size()];

        for (int i = 0; i < completeFiles.length; i++) {
            completeFiles[i] = (GeneralFile) completeFileList.get(i);
        }

        return completeFiles;
    }

    /**
     * A helper function which is used to create GeneralFile array based on the file system and file name list.
     *
     * @param   fileSystem    the file system
     * @param   fileNameList  the file name list.
     *
     * @return  an array of the GeneralFile.
     */
    public static GeneralFile[] createFiles(GeneralFileSystem fileSystem, Vector<String> fileNameList) {

        if ((fileSystem == null) || (fileNameList == null)) {
            return null;
        }

        int n = fileNameList.size();
        GeneralFile[] newFiles = new GeneralFile[n];

        for (int i = 0; i < n; i++) {
            newFiles[i] = FileFactory.newFile(fileSystem, fileNameList.get(i));
        }

        return newFiles;
    }

    /**
     * A helper function to create a LocalFile object based on the file name.
     *
     * @param   fileName  a file name
     *
     * @return  a LocalFile object.
     */
    public static LocalFile createLocalFile(String fileName) {

        if ((fileName == null) || (fileName.length() == 0)) {
            return null;
        }

        File file = new File(fileName);

        if (!file.exists()) {
            file.mkdirs();
        }

        return new LocalFile(file);
    }

    /**
     * A helper function to create a random subdirectory under the parent directoy localDirBase.
     *
     * @param   localDirBase  the local parent directory.
     *
     * @return  the created random diretory.
     */
    public static File createRandomLocalDir(File localDirBase) {

        if (localDirBase == null) {
            return null;
        }

        long currentTime = System.currentTimeMillis();
        File newLocalDir = new File(localDirBase, Long.toString(currentTime));

        if (!newLocalDir.exists()) {
            newLocalDir.mkdirs();
        }

        return newLocalDir;
    }

    /**
     * A helper function to create the target file based on the target directory, the source base directory and the
     * source file.
     *
     * @param   targetDir       the target directory.
     * @param   baseSourceFile  the source base directory.
     * @param   sourceFile      the source file.
     *
     * @return  the target file.
     */
    public static GeneralFile createTargetFile(GeneralFile targetDir, GeneralFile baseSourceFile,
                                               GeneralFile sourceFile) {

        if ((targetDir == null) || (baseSourceFile == null) || (sourceFile == null)) {
            return null;
        }

        String baseSourceFileName = baseSourceFile.getAbsolutePath();
        String sourceFileName = sourceFile.getAbsolutePath();

        if (sourceFileName.indexOf(baseSourceFileName) == 0) {
            String sourcePathSeperator = (baseSourceFile instanceof SRBFile) ? SRBFile.PATH_SEPARATOR : File.separator;
            String targetPathSeperator = (targetDir instanceof SRBFile) ? SRBFile.PATH_SEPARATOR : File.pathSeparator;
            String targetFileName = sourceFileName.substring(baseSourceFileName.length() +
                                                             sourcePathSeperator.length());

            if (!sourcePathSeperator.equals(targetPathSeperator)) {
                targetFileName = SRBUtility.replacePathSeparator(targetFileName, sourcePathSeperator,
                                                                 targetPathSeperator);
            }

            return FileFactory.newFile(targetDir, targetFileName);
        }

        return null;
    }

    /**
     * A helper function to create the target file list based on the given source file list, the target directory, and
     * the source base directory.
     *
     * @param   targetDir       the target directory.
     * @param   baseSourceFile  the source base directory.
     * @param   sourceFiles     the source file list.
     *
     * @return  the target file list.
     */
    public static GeneralFile[] createTargetFiles(GeneralFile targetDir, GeneralFile baseSourceFile,
                                                  GeneralFile[] sourceFiles) {

        if ((targetDir == null) || (baseSourceFile == null) || (sourceFiles == null)) {
            return null;
        }

        GeneralFile[] targetFiles = new GeneralFile[sourceFiles.length];

        for (int i = 0; i < sourceFiles.length; i++) {
            targetFiles[i] = createTargetFile(targetDir, baseSourceFile, sourceFiles[i]);
        }

        return targetFiles;
    }

    /**
     * Returns the extension of the file name which does not include path information.
     *
     * @param   fileName  a file name
     *
     * @return  the extension of the file name/
     */
    public static String getExtension(String fileName) {

        if ((fileName == null) || (fileName.length() == 0)) {
            return null;
        }

        int index = fileName.lastIndexOf(".");

        if (index == -1) {
            return new String("");
        }

        return fileName.substring(index + 1);
    }

    /**
     * Saves the memory image to the local temporary files and returns the LocalFile object list.
     *
     * @param   image    the memory image.
     * @param   tempDir  the local directory that the memory image will saved to
     *
     * @return  the saved LocalFile object list of this memory image.
     */
    public static GeneralFile[] getFileList(ModelImage image, String tempDir) {

        if (image == null) {
            return null;
        }

        if ((tempDir == null) || (tempDir.length() == 0)) {
            return null;
        }

        LocalFile localTempDir = createLocalFile(tempDir);

        if (localTempDir == null) {
            return null;
        }

        /**
         * Gets the file informations for the current opened images.
         */
        FileInfoBase[] currentFileInfoList = image.getFileInfo();

        if (currentFileInfoList == null) {
            return null;
        }

        /**
         * Saves the current directory for recovery at the end of function.
         *
         * The idea is try to use the save function save the files to different directory.
         */
        String savedDir = currentFileInfoList[0].getFileDirectory();

        /**
         * Sets the new directory which we want the files saved to.
         */
        for (int i = 0; i < currentFileInfoList.length; i++) {
            currentFileInfoList[i].setFileDirectory(localTempDir.getPath() + File.separator);
        }

        /**
         * Creates the local temporary file list.
         */
        List<String> fileNameList = getFileNameList(image);

        Vector<GeneralFile> sourceFileList = new Vector<GeneralFile>();

        for (int i = 0; i < fileNameList.size(); i++) {
            sourceFileList.add(FileFactory.newFile(localTempDir, FileUtility.getFileName(fileNameList.get(i))));
        }

        /**
         * Constructs the FileWriteOptions to prepare the file name for save.
         */
        FileWriteOptions opts = new FileWriteOptions(((LocalFile) sourceFileList.get(0)).getName(),
                                                     localTempDir.getPath() + "//", false);

        //set flag to not put these files into the quicklist
        opts.doPutInQuicklist(false);
        
        if (image.getNDims() == 3) {
            opts.setBeginSlice(0);
            opts.setEndSlice(image.getExtents()[2] - 1);
        }

        opts.setOptionsSet(true);

        /**
         * Saves the opened images to the local temporary directory.
         */
        image.getParentFrame().saveSRB(opts, -1);

        /**
         * Recovers the original directory which these files belong to.
         */
        for (int i = 0; i < currentFileInfoList.length; i++) {
            currentFileInfoList[i].setFileDirectory(savedDir);
        }

        GeneralFile[] sourceFiles = new GeneralFile[sourceFileList.size()];

        for (int i = 0; i < sourceFiles.length; i++) {
            sourceFiles[i] = (GeneralFile) sourceFileList.get(i);
        }

        return sourceFiles;
    }

    /**
     * Gets the file name list from which this ModelImage is opened.
     *
     * @param   image  the ModelImage object.
     *
     * @return  the actual file name list.
     */
    public static List<String> getFileNameList(ModelImage image) {

        if (image == null) {
            return null;
        }

        FileInfoBase[] fileInfoList = image.getFileInfo();

        if ((fileInfoList == null) || (fileInfoList.length == 0)) {
            return null;
        }

        FileInfoBase fileInfo = fileInfoList[0];
        int fileFormat = fileInfo.getFileFormat();
        Vector<String> fileNameList = null;

        switch (fileFormat) {

            /* Ill defined file type.              */
            case FileUtility.ERROR:
                return null;

            /* Undefined file type.                */
            case FileUtility.UNDEFINED:
                return null;

            /* Not presently implemented.          */
            case FileUtility.MIPAV:
                return null;

            /* RAW image data, no header.          */
            case FileUtility.RAW:
        	fileNameList = new Vector<String>();
                fileNameList.add(fileInfo.getFileDirectory() + File.separator + fileInfo.getFileName());
                return fileNameList;

            /* TIFF file; tagged header            */
            case FileUtility.TIFF:
        	fileNameList = new Vector<String>();
                for (int i = 0; i < fileInfoList.length; i++) {
                    fileNameList.add(fileInfo.getFileDirectory() + File.separator + fileInfoList[i].getFileName());
                }

                return fileNameList;

            /* VOI file, used to read VOIs.        */
            case FileUtility.VOI_FILE:
                return null;

            /* Analyze format (Mayo).              */
            case FileUtility.ANALYZE:

            /* NIFTI format */
            case FileUtility.NIFTI:
                fileNameList = new Vector<String>();

                if (fileInfo.getFileName().toLowerCase().endsWith(".nii")) {
                    fileNameList.add(fileInfo.getFileDirectory() + File.separator + fileInfo.getFileName());
                } else {
                    String imgFileName = fileInfo.getFileName();
                    String hdrFileName = imgFileName.replaceFirst(".img", ".hdr");
                    fileNameList.add(fileInfo.getFileDirectory() + File.separator + hdrFileName);
                    fileNameList.add(fileInfo.getFileDirectory() + File.separator + imgFileName);
                }

                return fileNameList;

            /* Digital Imaging and COmmunications in Medicine file type.
             * Fully implemented versions 2 & 3.   */
            case FileUtility.DICOM:
                fileNameList = new Vector<String>();
                for (int i = 0; i < fileInfoList.length; i++) {
                    fileNameList.add(fileInfo.getFileDirectory() + File.separator + fileInfoList[i].getFileName());
                }

                return fileNameList;

            /* Medvision file type.                */
            case FileUtility.MEDIVISION:
                return null;

            /* Benes Trus special file type.       */
            case FileUtility.MAP:
                return null;

            /* Java Image Manangement Interface file type. */
            case FileUtility.JIMI:
                return null;

            /* Multiple files of TIFF images.      */
            case FileUtility.TIFF_MULTIFILE:
        	fileNameList = new Vector<String>();
                for (int i = 0; i < fileInfoList.length; i++) {
                    fileNameList.add(fileInfo.getFileDirectory() + File.separator + fileInfoList[i].getFileName());
                }

                return fileNameList;

            /* MINC file type.  MINC is a medical imaging oriented extension
             * of the NetCDF file format. NetCDF stands for `Network Common Data Form'.  */
            case FileUtility.MINC:
                fileNameList = new Vector<String>();
                fileNameList.add(fileInfo.getFileDirectory() + File.separator + fileInfo.getFileName());

                return fileNameList;

            /* AVI file type.  Windows Media.*/
            case FileUtility.AVI:
                return null;

            /* Multiple files of type analyze.     */
            case FileUtility.ANALYZE_MULTIFILE:
        	
            /* NIFTI multi-file format */
            case FileUtility.NIFTI_MULTIFILE:
        	fileNameList = new Vector<String>();
        	
        	if (fileInfoList[0].getFileName().toLowerCase().endsWith(".nii")) {
        	    for (int i = 0; i < fileInfoList.length; i++) {
        		fileNameList.add(fileInfo.getFileDirectory() + File.separator + fileInfoList[i].getFileName());
        	    }
                } else {
                    for (int i = 0; i < fileInfoList.length; i++) {
                	String imgFileName = fileInfoList[i].getFileName();
                	String hdrFileName = imgFileName.replaceFirst(".img", ".hdr");
                	
                	fileNameList.add(fileInfo.getFileDirectory() + File.separator + hdrFileName);
                	fileNameList.add(fileInfo.getFileDirectory() + File.separator + imgFileName);
                    }
                }

                return fileNameList;

            /* Quicktime file type.            */
            case FileUtility.QT:
                return null;

            /* Cheshire file type (a kind of Analyze).*/
            case FileUtility.CHESHIRE:
                return null;

            /* Cheshire overlay file type.  Contains VOIs. */
            case FileUtility.CHESHIRE_OVERLAY:
                return null;

            /* AFNI file type. */
            case FileUtility.AFNI:
                fileNameList = new Vector<String>();

                String headFileName = fileInfo.getFileName();
                String brikFileName = headFileName.replaceFirst(".HEAD", ".BRIK");
                fileNameList.add(fileInfo.getFileDirectory() + File.separator + headFileName);
                fileNameList.add(fileInfo.getFileDirectory() + File.separator + brikFileName);

                return fileNameList;

            /* FITS file type. */
            case FileUtility.FITS:
                return null;

            /* MetaMorph Stack (STK) file type. */
            case FileUtility.STK:
                return null;

            /* Siemens MAGNETOM VISION */
            case FileUtility.MAGNETOM_VISION:
        	fileNameList = new Vector<String>();
                for (int i = 0; i < fileInfoList.length; i++) {
                    fileNameList.add(fileInfo.getFileDirectory() + File.separator + fileInfoList[i].getFileName());
                }

                return fileNameList;

            /* GE Genesis 5X and LX */
            case FileUtility.GE_GENESIS:
        	fileNameList = new Vector<String>();
                for (int i = 0; i < fileInfoList.length; i++) {
                    fileNameList.add(fileInfo.getFileDirectory() + File.separator + fileInfoList[i].getFileName());
                }

                return fileNameList;

            /* MRC file format used by IMOD */
            case FileUtility.MRC:
                return null;

            /* Interfile file format used in Nuclear Medicine */
            case FileUtility.INTERFILE:
                return null;

            /* Micro CT format for small animal imaging */
            case FileUtility.MICRO_CAT:
                return null;

            /* RAW MULTIFLE image data, no header. */
            case FileUtility.RAW_MULTIFILE:
        	fileNameList = new Vector<String>();
                for (int i = 0; i < fileInfoList.length; i++) {
                    fileNameList.add(fileInfo.getFileDirectory() + File.separator + fileInfoList[i].getFileName());
                }

                return fileNameList;

            /* Used by the Zeiss LSM 510 Dataserver */
            case FileUtility.LSM:
                return null;

            /* Used by the Zeiss LSM 510 Dataserver */
            case FileUtility.LSM_MULTIFILE:
                return null;

            /* Used by the Bio-Rad Pic format */
            case FileUtility.BIORAD:
                return null;

            /* Used by FreeSurfer software */
            case FileUtility.COR:
                return null;

            /* Bruker file format */
            case FileUtility.BRUKER:
                return null;

            /* MIPAV XML file format */
            case FileUtility.XML:
                fileNameList = new Vector<String>();

                String xmlFileName = fileInfo.getFileName();
                String rawFileName = ((FileInfoXML) fileInfo).getImageDataFileName();
                fileNameList.add(fileInfo.getFileDirectory() + File.separator + xmlFileName);
                fileNameList.add(fileInfo.getFileDirectory() + File.separator + rawFileName);

                return fileNameList;

            /* MIPAV XML file format */
            case FileUtility.XML_MULTIFILE:
        	fileNameList = new Vector<String>();
        	
        	for (int i = 0; i < fileInfoList.length; i++) {
        	    xmlFileName = fileInfoList[i].getFileName();
        	    rawFileName = ((FileInfoXML) fileInfoList[i]).getImageDataFileName();
        	    
                    fileNameList.add(fileInfo.getFileDirectory() + File.separator + xmlFileName);
                    fileNameList.add(fileInfo.getFileDirectory() + File.separator + rawFileName);
                }

                return fileNameList;

            /* SPM file format */
            case FileUtility.SPM:
                return null;

            /* MIPAV project format */
            case FileUtility.PROJECT:
                return null;

            /* Image Cytometry Standard */
            case FileUtility.ICS:
                return null;

            /* Optical coherence tomography */
            case FileUtility.TMG:
                return null;

            /* Washington University OSM dataset structure */
            case FileUtility.OSM:
                return null;

            /* MIPAV Surface XML file format */
            case FileUtility.SURFACE_XML:
                return null;

            /* Gatan's Digital Micrograph version 3 file format */
            case FileUtility.DM3: /* Image modality unknown. */
                return null;
        }

        return null;
    }

    /**
     * A helper function to obtain the parent directory based on the given file name and the path separator string.
     *
     * @param   fileName       the file name.
     * @param   pathSeparator  the path separator string.
     *
     * @return  the parent directory string of this file name.
     */
    public static String getParentDirectory(String fileName, String pathSeparator) {

        if ((fileName == null) || (fileName.length() == 0) || (pathSeparator == null)) {
            return null;
        }

        if (fileName.endsWith(pathSeparator) && !fileName.equals(pathSeparator)) {
            fileName = fileName.substring(0, fileName.length() - pathSeparator.length());
        }

        if (fileName.indexOf(pathSeparator) >= 0) {
            int index = fileName.lastIndexOf(pathSeparator);

            if (index == 0) {
                return fileName.substring(0, pathSeparator.length());
            } else {
                return fileName.substring(0, index);
            }
        }

        return fileName;
    }

    /**
     * Returns the partner file of the specified file. Because some format image file can contains several files,
     * otherwise it can not be opened.
     *
     * @param   sourceFile  the specified file
     *
     * @return  the partner file of the specified file.
     */
    public static GeneralFile getPartnerFile(GeneralFile sourceFile) {

        if (sourceFile == null) {
            return null;
        }

        /*
         * Gets the extension of the selected file.
         */
        String extension = getExtension(sourceFile.getName());

        /*
         * According to the file type, determines what other files should be added into the file list.
         */
        GeneralFile partnerFile = null;

        if (extension.equals("xml")) {
            partnerFile = FileFactory.newFile(sourceFile.getParentFile(),
                                              sourceFile.getName().replaceFirst("xml", "raw"));

            if (!partnerFile.exists()) {
                MipavUtil.displayError("The file " + partnerFile.getAbsolutePath() + " doesn't exist!");
                partnerFile = null;
            }
        } else if (extension.equals("XML")) {
            partnerFile = FileFactory.newFile(sourceFile.getParentFile(),
                                              sourceFile.getName().replaceFirst("XML", "RAW"));

            if (!partnerFile.exists()) {
                MipavUtil.displayError("The file " + partnerFile.getAbsolutePath() + " doesn't exist!");
                partnerFile = null;
            }
        } else if (extension.equals("img")) {
            partnerFile = FileFactory.newFile(sourceFile.getParentFile(),
                                              sourceFile.getName().replaceFirst("img", "hdr"));

            if (!partnerFile.exists()) {
                MipavUtil.displayError("The file " + partnerFile.getAbsolutePath() + " doesn't exist!");
                partnerFile = null;
            }
        } else if (extension.equals("IMG")) {
            partnerFile = FileFactory.newFile(sourceFile.getParentFile(),
                                              sourceFile.getName().replaceFirst("IMG", "HDR"));

            if (!partnerFile.exists()) {
                MipavUtil.displayError("The file " + partnerFile.getAbsolutePath() + " doesn't exist!");
                partnerFile = null;
            }
        } else if (extension.equals("head")) {
            partnerFile = FileFactory.newFile(sourceFile.getParentFile(),
                                              sourceFile.getName().replaceFirst("head", "brik"));

            if (!partnerFile.exists()) {
                MipavUtil.displayError("The file " + partnerFile.getAbsolutePath() + " doesn't exist!");
                partnerFile = null;
            }
        } else if (extension.equals("HEAD")) {
            partnerFile = FileFactory.newFile(sourceFile.getParentFile(),
                                              sourceFile.getName().replaceFirst("HEAD", "BRIK"));

            if (!partnerFile.exists()) {
                MipavUtil.displayError("The file " + partnerFile.getAbsolutePath() + " doesn't exist!");
                partnerFile = null;
            }
        }

        return partnerFile;
    }

    /**
     * A helper function to obtain the path separator based on the different file system.
     *
     * @param   fs  the file system
     *
     * @return  the path separator used by the file system.
     */
    public static String getPathSeparator(GeneralFileSystem fs) {

        if (fs == null) {
            return null;
        }

        if (fs instanceof LocalFileSystem) {
            return File.separator;
        } else {
            return SRBFile.PATH_SEPARATOR;
        }
    }

    /**
     * Returns the primary file which was recognized by the mipav.
     *
     * @param   sourceFiles  the file list which contains the whole file list of some format image.
     *
     * @return  the primary file list.
     */
    public static GeneralFile[] getPrimaryFiles(GeneralFile[] sourceFiles) {

        if (sourceFiles == null) {
            return null;
        }

        Vector<GeneralFile> primaryFileList = new Vector<GeneralFile>();
        boolean dicomAdded = false;

        for (int i = 0; i < sourceFiles.length; i++) {
            String extension = getExtension(sourceFiles[i].getName());

            if (extension.equalsIgnoreCase("xml") || extension.equalsIgnoreCase("img") ||
                    extension.equalsIgnoreCase("head") || extension.equalsIgnoreCase("mnc")) {
                primaryFileList.add(sourceFiles[i]);
            } else if (extension.equals("dcm") && !dicomAdded) {
                primaryFileList.add(sourceFiles[i]);
                dicomAdded = true;
            } else {
                primaryFileList.add(sourceFiles[i]);
            }
        }

        GeneralFile[] primaryFiles = new GeneralFile[primaryFileList.size()];

        for (int i = 0; i < primaryFiles.length; i++) {
            primaryFiles[i] = primaryFileList.get(i);
        }

        return primaryFiles;
    }

    /**
     * A helper function to convert the GeneralFile list ot the GeneralFile array.
     *
     * @param   fileList  the GeneralFile list
     *
     * @return  the GeneralFile array.
     */
    public static GeneralFile[] list2Array(List<GeneralFile> fileList) {

        if (fileList == null) {
            return null;
        }

        GeneralFile[] files = new GeneralFile[fileList.size()];

        for (int i = 0; i < fileList.size(); i++) {
            files[i] = fileList.get(i);
        }

        return files;
    }

    /**
     * Deletes the file or directory denoted by this abstract pathname recurively.
     *
     * @param  file  the file or directory denoted by this abstract pathname.
     */
    public static void recursivelyDeleteDir(GeneralFile file) {

        if (file == null) {
            return;
        }

        if (!file.delete()) {
            GeneralFile[] children = file.listFiles();

            if (children == null) {
                return;
            }

            for (int i = 0; i < children.length; i++) {
                recursivelyDeleteDir(children[i]);
            }

            file.delete();
        }
    }

    /**
     * A helper function to create directory recursively instead of using mkdirs(). Notes: The function makeDirs() of
     * File works fine, but SRBFile's doesn't work very well, that's why i write this functions.
     *
     * @param  newDir  the new directory need to be created.
     */
    public static void recursivelyMakeDir(GeneralFile newDir) {

        if (newDir == null) {
            return;
        }

        if (!newDir.exists()) {
            GeneralFile parentDir = newDir.getParentFile();

            if (!parentDir.exists()) {
                recursivelyMakeDir(parentDir);
            }

            newDir.mkdir();
        }
    }

    /**
     * Converts the source path seperator to the target path seperator. If the first character is path separator, then
     * just remove this path separator.
     *
     * @param   s                the string which includes source path seperator need to be replaced.
     * @param   sourceSeparator  the source path seperator.
     * @param   targetSeparator  the target path seperator.
     *
     * @return  the new path.
     */
    public static String replacePathSeparator(String s, String sourceSeparator, String targetSeparator) {

        if ((s == null) || (s.length() == 0) || (sourceSeparator == null) || (targetSeparator == null)) {
            return s;
        }

        if (sourceSeparator.equals(targetSeparator)) {
            return s;
        }

        int index = s.indexOf(sourceSeparator);

        while (index >= 0) {

            if (index == 0) {
                s = s.substring(index + sourceSeparator.length());
            } else {
                s = s.substring(0, index) + targetSeparator + s.substring(index + sourceSeparator.length());
            }

            index = s.indexOf(sourceSeparator);
        }

        return s;
    }

    /**
     * A helper function to retrieve all files contained in this directory and subdirectory recursively and put into the
     * <code>fileList</code>.
     *
     * @param  file      the given file or directory.
     * @param  fileList  the file list which stores all files included in the <code>file</code> and its subdirectory.
     */
    public static void retrieveFileList(GeneralFile file, List<GeneralFile> fileList) {

        if ((file == null) || (fileList == null)) {
            return;
        }

        if (file.isFile()) {
            fileList.add(file);

            return;
        }

        GeneralFile[] children = file.listFiles();

        if (children == null) {
            return;
        }

        for (int i = 0; i < children.length; i++) {
            retrieveFileList(children[i], fileList);
        }
    }

}
