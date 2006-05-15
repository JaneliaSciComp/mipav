package gov.nih.mipav.model.srb;

import java.util.Vector;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JTextField;


import edu.sdsc.grid.io.GeneralFile;
import edu.sdsc.grid.io.GeneralFileSystem;
import edu.sdsc.grid.io.GeneralRandomAccessFile;
import edu.sdsc.grid.io.local.LocalFile;
import edu.sdsc.grid.io.local.LocalFileSystem;
import edu.sdsc.grid.io.local.LocalRandomAccessFile;
import edu.sdsc.grid.io.srb.SRBFile;
import edu.sdsc.grid.io.srb.SRBFileSystem;
import edu.sdsc.grid.io.srb.SRBRandomAccessFile;
import gov.nih.mipav.model.file.FileBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.PanelManager;
import gov.nih.mipav.view.components.WidgetFactory;
import gov.nih.mipav.view.srb.FileTransferSRB;
import gov.nih.mipav.view.srb.JDialogLoginSRB;
import gov.nih.mipav.view.srb.JargonFileChooser;

public class SRBFileTransferer implements FileTransferable, Runnable, ActionListener, WindowListener {
    public static final String[] SCHEMAS = { "file", "srb" };
    public static final String TRANSFER_MODE_SEQUENTIAL = "sequential";
    public static final String TRANSFER_MODE_PARELLEL = "parellel";
    
    private static final String DEFAULT_TEMP_DIR = "c:\\temp\\srb-temp";
    /**
     * The progress bar used to indicate the progress of reading or writing.
     */
    private ViewJProgressBar progressBar;

    /**
     * The location of the progress bar.
     */
    private Point progressBarLocation;

    /**
     * The flag to indicate the visibility of the progress bar.
     */
    private boolean progressBarVisible = true;
    
    /**
     * The list of the source files.
     */
    private GeneralFile[] sourceFiles;
    
    /**
     * The list of the target files.
     */
    private GeneralFile[] targetFiles;
    
    /**
     * The target directory the files will be transferred to.
     */
    private GeneralFile targetDir;
    
    /**
     * Used to control the transferring mode.
     */
    private String transferMode;
    
    private boolean threadSeperated;
    
    private File tempDir;
    public SRBFileTransferer(){
        this(DEFAULT_TEMP_DIR);
    }
    
    public SRBFileTransferer(String localTempDir){
        if(localTempDir == null || localTempDir.length() == 0){
            localTempDir = DEFAULT_TEMP_DIR;
        }
        tempDir = new File(localTempDir);
        if(!tempDir.exists()){
            tempDir.mkdirs();
        }

        transferMode = TRANSFER_MODE_PARELLEL;
        threadSeperated = true;
    }
    public boolean isProgressBarVisible(){
        return progressBarVisible;
    }
    
    public void setProgressBarVisible(boolean visible){
        this.progressBarVisible = visible;
    }
    
    public void setProgressBarLocation(Point location){
        this.progressBarLocation = location;
    }
    
    public boolean isThreadSeperated(){
        return threadSeperated;
    }
    
    public void setThreadSeperated(boolean threadSeperated){
        this.threadSeperated = threadSeperated;
    }
    
    public LocalFile getLocalTempDir(){
        if(tempDir != null){
            return new LocalFile(tempDir);
        }
        return null;
    }
    
    public GeneralFile[] getSourceFiles() {
        return sourceFiles;
    }

    public void setSourceFiles(GeneralFile[] sourceFiles){
        this.sourceFiles = sourceFiles;
    }
    
    public GeneralFile[] selectSourceFiles(String schema){
        if(schema == null ||schema.length() == 0){
            return null;
        }
        if(schema.equals(SCHEMAS[0])){

            /**
             * Uses the JFileChooser to retrieve the file that the user wants to open.
             */
            JFileChooser chooser = new JFileChooser();

            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            chooser.setMultiSelectionEnabled(true);

            int returnValue = chooser.showDialog(ViewUserInterface.getReference().getMainFrame(), "Open");

            if (returnValue == JargonFileChooser.APPROVE_OPTION) {
                File[] files = chooser.getSelectedFiles();
                if(files != null){
                    LocalFile[] localFiles = new LocalFile[files.length];
                    for(int i = 0; i < files.length; i++){
                        localFiles[i] = new LocalFile(files[i]);
                    }
                    return localFiles;
                }
            }            
            return null;
        }else if(schema.equals(SCHEMAS[1])){
            /**
             * If the srbFileSystem was not set up, then uses the JDialogLoginSRB dialog to retrieve information to
             * construct SRBFileSystem instance.
             */
            if (!JDialogLoginSRB.hasValidSRBFileSystem()) {
                new JDialogLoginSRB("Connect to");
            }

            if (!JDialogLoginSRB.hasValidSRBFileSystem()) {
                return null;
            }

            /**
             * Uses the JargonFileChooser to retrieve the file that the user wants to open.
             */
            JargonFileChooser chooser = null;

            try {
                chooser = new JargonFileChooser(JDialogLoginSRB.srbFileSystem);
            } catch (OutOfMemoryError e) {
                e.printStackTrace(System.err);
                MipavUtil.displayError("Out of memory!");

                return null;
            } catch (IOException e) {
                e.printStackTrace(System.err);
                MipavUtil.displayError(e.getMessage());

                return null;
            }

            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            chooser.setMultiSelectionEnabled(true);

            int returnValue = chooser.showDialog(ViewUserInterface.getReference().getMainFrame(), "Open");

            if (returnValue == JargonFileChooser.APPROVE_OPTION) {
                return chooser.getSelectedFiles();
            }            
        }
        return null;
    }
    
    public GeneralFile selectTargetDirectory(String schema) {
        if(schema == null ||schema.length() == 0){
            return null;
        }
        if(schema.equals(SCHEMAS[0])){

            /**
             * Uses the JargonFileChooser to retrieve the file that the user wants to open.
             */
            JFileChooser chooser = new JFileChooser();

            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            chooser.setMultiSelectionEnabled(false);

            int returnValue = chooser.showDialog(ViewUserInterface.getReference().getMainFrame(), "Open");

            if (returnValue == JargonFileChooser.APPROVE_OPTION) {
                return new LocalFile(chooser.getSelectedFile());
            }            
        }else if(schema.equals(SCHEMAS[1])){
            /**
             * If the srbFileSystem was not set up, then uses the JDialogLoginSRB dialog to retrieve information to
             * construct SRBFileSystem instance.
             */
            if (!JDialogLoginSRB.hasValidSRBFileSystem()) {
                new JDialogLoginSRB("Connect to");
            }

            if (!JDialogLoginSRB.hasValidSRBFileSystem()) {
                return null;
            }

            /**
             * Uses the JargonFileChooser to retrieve the file that the user wants to open.
             */
            JargonFileChooser chooser = null;

            try {
                chooser = new JargonFileChooser(JDialogLoginSRB.srbFileSystem);
            } catch (OutOfMemoryError e) {
                e.printStackTrace(System.err);
                MipavUtil.displayError("Out of memory!");

                return null;
            } catch (IOException e) {
                e.printStackTrace(System.err);
                MipavUtil.displayError(e.getMessage());

                return null;
            }

            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            chooser.setMultiSelectionEnabled(false);

            int returnValue = chooser.showDialog(ViewUserInterface.getReference().getMainFrame(), "Open");

            if (returnValue == JargonFileChooser.APPROVE_OPTION) {
                return chooser.getSelectedFile();
            }            
        }
        return null;
    }

    public void setTargetDir(GeneralFile targetDir){
        this.targetDir = targetDir;
    }
    
    public static GeneralFile createTargetFile(GeneralFile targetDir,
            GeneralFile baseSourceFile, GeneralFile sourceFile) {
        
        if(targetDir == null || baseSourceFile == null || sourceFile == null){
            return null;
        }
        
        String baseSourceFileName = baseSourceFile.getAbsolutePath().toLowerCase();
        String sourceFileName = sourceFile.getAbsolutePath().toLowerCase();
        if(sourceFileName.indexOf(baseSourceFileName) == 0){
            String targetFileName = sourceFileName.substring(baseSourceFileName.length());
            String sourcePathSeperator = (baseSourceFile instanceof SRBFile)?SRBFile.PATH_SEPARATOR:File.separator;
            String targetPathSeperator = (targetDir instanceof SRBFile)?SRBFile.PATH_SEPARATOR:File.pathSeparator;
            if(!sourcePathSeperator.equals(targetPathSeperator)){
                replacePathSeparator(targetFileName, sourcePathSeperator, targetPathSeperator);
            }
            if(targetDir instanceof SRBFile){
                return new SRBFile((SRBFile)targetDir, targetFileName);
            }else{
                return new LocalFile((LocalFile)targetDir, targetFileName);
            }
        }
        return null;
    }

    public static GeneralFile[] createTargetFiles(GeneralFile targetDir,
            GeneralFile baseSourceFile, GeneralFile[] sourceFiles) {
        if(targetDir == null || baseSourceFile == null || sourceFiles == null){
            return null;
        }
        GeneralFile[] targetFiles = new GeneralFile[sourceFiles.length];
        for(int i = 0; i < sourceFiles.length; i++){
            targetFiles[i] = createTargetFile(targetDir, baseSourceFile, sourceFiles[i]);
        }
        return targetFiles;
    }

    public GeneralFile[] getTargetFiles(){
        return targetFiles;
    }
    
    public void setTargetFiles(GeneralFile[] targetFiles){
        this.targetFiles = targetFiles;
    }
    
    /**
     * 
     * @param sourceFile
     * @param targetFile
     * @return
     */
    public boolean transfer(GeneralFile sourceFile, GeneralFile targetFile) {
        if(sourceFile == null || targetFile == null){
            return false;
        }
        GeneralFile targetDir = targetFile.getParentFile();
        if(!targetDir.exists()){
            recursivelyMakeDir(targetDir);
        }
        if(transferMode.equals(TRANSFER_MODE_PARELLEL)){
            try{
                sourceFile.copyTo(targetFile);
                return true;
            }catch(IOException e){
                e.printStackTrace(System.err);
                MipavUtil.displayError("File I/O error: " + e.getMessage());
                return false;
            }
        }else{
            GeneralRandomAccessFile sourceRandomAccessFile;
            GeneralRandomAccessFile destinationRandomAccessFile;

            try {

                if (sourceFile instanceof LocalFile) {
                    sourceRandomAccessFile = new LocalRandomAccessFile((LocalFile) sourceFile, "r");
                } else {
                    sourceRandomAccessFile = new SRBRandomAccessFile((SRBFile) sourceFile, "r");
                }

                if (targetFile instanceof LocalFile) {
                    destinationRandomAccessFile = new LocalRandomAccessFile((LocalFile) targetFile, "rw");
                } else {
                    destinationRandomAccessFile = new SRBRandomAccessFile((SRBFile) targetFile, "rw");
                }

                long length = sourceRandomAccessFile.length();
                byte[] buffer = new byte[(int) length];
                sourceRandomAccessFile.read(buffer, 0, (int) length);
                destinationRandomAccessFile.write(buffer, 0, (int) length);

                sourceRandomAccessFile.close();
                destinationRandomAccessFile.close();

                return true;
            } catch (IOException e) {
                e.printStackTrace(System.err);
                MipavUtil.displayError("File I/O error: " + e.getMessage());

                return false;
            }
        }
    }

    /**
     * A helper function to create directory recursively instead of using mkdirs().
     * @param newDir the new directory need to be created.
     */
    public static void recursivelyMakeDir(GeneralFile newDir){
        if(newDir == null){
            return;
        }
        if(!newDir.exists()){
            GeneralFile parentDir = newDir.getParentFile();
            if(!parentDir.exists()){
                recursivelyMakeDir(parentDir);
            }
            newDir.mkdir();
        }
    }
    public boolean transfer(GeneralFile[] sourceFiles, GeneralFile[] targetFiles) {
        if(sourceFiles == null || targetFiles == null){
            return false;
        }
        
        if(sourceFiles.length != targetFiles.length){
            return false;
        }
        /**
         * Builds the progress bar for the file transferring.
         */
        if(isProgressBarVisible()){
            buildProgressBar("Transfering " + sourceFiles[0].getName(), "Transfering files ...", 0, 100);
        }
        int mod = sourceFiles.length / 100;
        if(mod < 1){
            mod = 1;
        }
        
        initProgressBar();
        
        boolean ret = true;
        for(int i = 0; i < sourceFiles.length; i++){
            if ((((i) % mod) == 0) && isProgressBarVisible()) {
                progressBar.setTitle("Transfering " + sourceFiles[i].getName());
                progressBar.updateValue(Math.round(((float) i / sourceFiles.length) * 100), isThreadSeperated());
            }
            ret |= transfer(sourceFiles[i], targetFiles[i]);
        }

        progressBar.updateValue(100, isThreadSeperated());

        try {
            Thread.sleep(100);
        } catch (InterruptedException e) { }

        disposeProgressBar();

        return ret;
    }

    public void transferFiles(){
        new JDialogPickFiles(null);
    }
    public void run() {
        transfer(sourceFiles, targetFiles);
    }

    /**
     * Constructs progress bar.
     *
     * @param  imageName  title of the toolbar
     * @param  message    message to be displayed in the frame
     * @param  start      start (typical = 0)
     * @param  end        end (typical = 100)
     */
    protected void buildProgressBar(String imageName, String message, int start, int end) {

        if (progressBarVisible == true) {
            progressBar = new ViewJProgressBar(imageName, message, start, end, false, this, this);
        }
    }

    /**
     * Disposes of progress bar.
     */
    protected void disposeProgressBar() {

        if (progressBar != null) {
            progressBar.dispose();
        }
    }

    /**
     * Initializes progress bar.
     */
    protected void initProgressBar() {

        if ((progressBarVisible == true) && (progressBar != null)) {

            if (progressBarLocation == null) {
                MipavUtil.centerOnScreen(progressBar);
            } else {
                progressBar.setLocation(progressBarLocation);
            }

            progressBar.setVisible(true);
        }
    }
    
    /**
     * Parses the string to extract the file informations.
     *
     * @param   fileSystem  DOCUMENT ME!
     * @param   fileName    DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static GeneralFile[] converToFiles(GeneralFileSystem fileSystem, String fileName) {
        GeneralFile[] selectedFiles = null;

        if (fileName.indexOf(",") >= 0) {
            String[] fileNames = fileName.split(",");

            if ((fileNames == null) || (fileNames.length == 0)) {
                return null;
            }

            selectedFiles = new SRBFile[fileNames.length];

            for (int i = 0; i < fileNames.length; i++) {
                GeneralFile newFile = createFile(fileSystem, fileNames[i]);

                if (newFile == null) {
                    return null;
                }

                selectedFiles[i] = newFile;
            }
        } else {
            selectedFiles = new GeneralFile[1];

            GeneralFile newFile = createFile(fileSystem, fileName);

            if (newFile == null) {
                return null;
            }

            selectedFiles[0] = newFile;
        }

        return selectedFiles;
    }

    /**
     * Converts the source path seperator to the target path seperator.
     *
     * @param   s                the string which includes source path seperator need to be replaced.
     * @param   sourceSeparator  the source path seperator.
     * @param   targetSeparator  the target path seperator.
     *
     * @return  DOCUMENT ME!
     */
    public static String replacePathSeparator(String s, String sourceSeparator, String targetSeparator) {

        if ((s == null) || (sourceSeparator == null) || (targetSeparator == null)) {
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
     * Converts the list of GeneralFile into the string.
     *
     * @param   files  a file array
     *
     * @return  the string representation of the file array.
     */
    public static String convertToString(GeneralFile[] files) {

        if ((files == null) || (files.length == 0)) {
            return "";
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
     * Converts the list of GeneralFile into the string.
     *
     * @param   files  a file array.
     *
     * @return  the string representation of the file array.
     */
    public static String convertToString(File[] files) {

        if ((files == null) || (files.length == 0)) {
            return "";
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

    public static GeneralFile[] convertFromVectorToArray(Vector fileList){
        if(fileList == null){
            return null;
        }
        
        GeneralFile[] files = new GeneralFile[fileList.size()];
        for(int i = 0; i < fileList.size(); i++){
            files[i] = (GeneralFile)fileList.get(i);
        }
        return files;
    }
    
    /**
     * A helper function which is used to create GeneralFile array based
     * on the file system and file name list.
     * 
     * @param fileSystem      the file system
     * @param fileNameList    the file name list.
     * @return                an array of the GeneralFile.
     */
    public static GeneralFile[] createFiles(GeneralFileSystem fileSystem, Vector fileNameList){
        if(fileSystem == null || fileNameList == null){
            return null;
        }
        int n = fileNameList.size();
        GeneralFile[] newFiles = new GeneralFile[n];
        for(int i = 0; i < n; i++){
            newFiles[i] = createFile(fileSystem, (String)fileNameList.get(i));
        }
        
        return newFiles;
    }
    /**
     * A helper function which create a GeneralFile according to the <code>targetDir</code> and gived file name.
     *
     * @param   targetDir  the parent directory.
     * @param   fileName   the file name which could include the path.
     *
     * @return  the created GeneralFile object.
     */
    public static GeneralFile createFile(GeneralFile targetDir, String fileName) {

        if ((targetDir == null) || (fileName == null) || (fileName.length() == 0)) {
            return null;
        }

        if (targetDir instanceof SRBFile) {
            fileName = replacePathSeparator(fileName, File.separator, SRBFile.PATH_SEPARATOR);

            return new SRBFile((SRBFile) targetDir, fileName);
        } else if (targetDir instanceof LocalFile) {
            fileName = replacePathSeparator(fileName, SRBFile.PATH_SEPARATOR, File.separator);

            return new LocalFile((LocalFile) targetDir, fileName);
        } else {
            return null;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   fileSystem  DOCUMENT ME!
     * @param   fileName    DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static GeneralFile createFile(GeneralFileSystem fileSystem, String fileName) {

        if ((fileName == null) || (fileName.length() == 0)) {
            return null;
        }

        if ((fileSystem == null) || (fileSystem instanceof LocalFileSystem)) {
            fileName = replacePathSeparator(fileName, SRBFile.PATH_SEPARATOR, File.separator);

            return new LocalFile(new File(fileName));
        } else if (fileSystem instanceof SRBFileSystem) {
            fileName = replacePathSeparator(fileName, File.separator, SRBFile.PATH_SEPARATOR);

            return new SRBFile((SRBFileSystem) fileSystem, fileName);
        } else {
            return null;
        }
    }

    /**
     * Recursively creates the directories that are necessary for creating current directory.
     *
     * @param  currentDir  the current directory.
     */
    public static void recursivelyCreateDirectory(GeneralFile currentDir) {

        if (currentDir.exists()) {
            return;
        } else {
            GeneralFile parentFile = currentDir.getParentFile();

            if (!parentFile.exists()) {
                recursivelyCreateDirectory(parentFile);
            }

            currentDir.mkdir();
        }
    }

    /**
     * Recursively creates the file list according to the source file and target directory information.
     *
     * @param  sourceFile      the source file.
     * @param  targetDir       the target directory.
     * @param  sourceRootDir   the source directory equivalent to the target directory which is selected by user.
     * @param  sourceFileList  the source file list which need to be transferred.
     * @param  targetFileList  the target file list which will be created.
     */
    public static void recursivelyCreateFileList(GeneralFile sourceFile, GeneralFile targetDir,
                                                 GeneralFile sourceRootDir, Vector sourceFileList,
                                                 Vector targetFileList) {

        if ((sourceFile == null) || (targetDir == null) || !targetDir.isDirectory()) {
            return;
        }

        if (sourceFile.isDirectory()) {
            GeneralFile[] childrenFiles = sourceFile.listFiles();

            for (int i = 0; i < childrenFiles.length; i++) {

                if (childrenFiles[i].isDirectory()) {
                    recursivelyCreateFileList(childrenFiles[i], targetDir, sourceRootDir, sourceFileList,
                                              targetFileList);
                } else {
                    sourceFileList.add(childrenFiles[i]);

                    GeneralFile newTargetFile = createFile(targetDir,
                                                           childrenFiles[i].getPath().substring(sourceRootDir.getPath().length()));
                    recursivelyCreateDirectory(newTargetFile.getParentFile());
                    targetFileList.add(newTargetFile);
                }
            }
        } else {
            sourceFileList.add(sourceFile);

            GeneralFile newTargetFile = createFile(targetDir,
                                                   sourceFile.getPath().substring(sourceRootDir.getPath().length()));
            recursivelyCreateDirectory(newTargetFile.getParentFile());
            targetFileList.add(newTargetFile);
        }
    }

    public static GeneralFile[] getPrimaryFiles(GeneralFile[] sourceFiles){
        if(sourceFiles == null){
            return null;
        }
        
        Vector primaryFileList = new Vector();
        boolean dicomAdded = false;
        for(int i = 0; i < sourceFiles.length; i++){
            String extension = getExtension(sourceFiles[i].getName());
            if (extension.equalsIgnoreCase("xml")
                    || extension.equalsIgnoreCase("img")
                    || extension.equalsIgnoreCase("head")
                    || extension.equalsIgnoreCase("mnc")) {
                primaryFileList.add(sourceFiles[i]);
            }else if(extension.equals("dcm") && !dicomAdded){
                primaryFileList.add(sourceFiles[i]);
                dicomAdded = true;
            }
        }
        GeneralFile[] primaryFiles = new GeneralFile[primaryFileList.size()];
        for(int i = 0; i < primaryFiles.length; i++){
            primaryFiles[i] = (GeneralFile)primaryFileList.get(i);
        }
        return primaryFiles;
    }
    
    public static GeneralFile getPartnerFile(GeneralFile sourceFile){
        if(sourceFile == null){
            return null;
        }
        
        /**
         * Gets the extension of the selected file.
         */
        String extension = getExtension(sourceFile.getName());

        /**
         * According to the file type, determines what other files should be added into the file list.
         */
        if (sourceFile instanceof SRBFile) {
            SRBFile partnerFile = null;
            if (extension.equalsIgnoreCase("xml")) {
                partnerFile = new SRBFile((SRBFile) sourceFile.getParentFile(),
                        sourceFile.getName().toLowerCase().replaceFirst("xml",  "raw"));
                if(!partnerFile.exists()){
                    partnerFile = new SRBFile((SRBFile) sourceFile.getParentFile(),
                            sourceFile.getName().toLowerCase().replaceFirst("xml",  "RAW"));
                    if(!partnerFile.exists()){
                        MipavUtil.displayError("The file " + partnerFile.getAbsolutePath() + " doesn't exist!");
                        partnerFile = null;
                    }
                }
            } else if (extension.equalsIgnoreCase("img")) {
                partnerFile = new SRBFile((SRBFile) sourceFile.getParentFile(),
                        sourceFile.getName().toLowerCase().replaceFirst("img", "hdr"));
                if(!partnerFile.exists()){
                    partnerFile = new SRBFile((SRBFile) sourceFile.getParentFile(),
                            sourceFile.getName().toLowerCase().replaceFirst("img", "HDR"));
                    if(!partnerFile.exists()){
                        MipavUtil.displayError("The file " + partnerFile.getAbsolutePath() + " doesn't exist!");
                        partnerFile = null;
                    }
                }
            } else if (extension.equalsIgnoreCase("head")) {
                partnerFile = new SRBFile((SRBFile) sourceFile.getParentFile(),
                        sourceFile.getName().toLowerCase().replaceFirst("head", "brik"));
                if(!partnerFile.exists()){
                    partnerFile = new SRBFile((SRBFile) sourceFile.getParentFile(),
                            sourceFile.getName().toLowerCase().replaceFirst("head", "BRIK"));
                    if(!partnerFile.exists()){
                        MipavUtil.displayError("The file " + partnerFile.getAbsolutePath() + " doesn't exist!");
                        partnerFile = null;
                    }
                }
            }
            return partnerFile;
        }else{
            LocalFile partnerFile = null;
            if (extension.equalsIgnoreCase("xml")) {
                partnerFile = new LocalFile((LocalFile) sourceFile.getParentFile(),
                        sourceFile.getName().toLowerCase().replaceFirst("xml",  "raw"));
                if(!partnerFile.exists()){
                    partnerFile = new LocalFile((LocalFile) sourceFile.getParentFile(),
                            sourceFile.getName().toLowerCase().replaceFirst("xml",  "RAW"));
                    if(!partnerFile.exists()){
                        MipavUtil.displayError("The file " + partnerFile.getAbsolutePath() + " doesn't exist!");
                        partnerFile = null;
                    }
                }
            } else if (extension.equalsIgnoreCase("img")) {
                partnerFile = new LocalFile((LocalFile) sourceFile.getParentFile(),
                        sourceFile.getName().toLowerCase().replaceFirst("img", "hdr"));
                if(!partnerFile.exists()){
                    partnerFile = new LocalFile((LocalFile) sourceFile.getParentFile(),
                            sourceFile.getName().toLowerCase().replaceFirst("img", "HDR"));
                    if(!partnerFile.exists()){
                        MipavUtil.displayError("The file " + partnerFile.getAbsolutePath() + " doesn't exist!");
                        partnerFile = null;
                    }
                }
            } else if (extension.equalsIgnoreCase("head")) {
                partnerFile = new LocalFile((LocalFile) sourceFile.getParentFile(),
                        sourceFile.getName().toLowerCase().replaceFirst("head", "brik"));
                if(!partnerFile.exists()){
                    partnerFile = new LocalFile((LocalFile) sourceFile.getParentFile(),
                            sourceFile.getName().toLowerCase().replaceFirst("head", "BRIK"));
                    if(!partnerFile.exists()){
                        MipavUtil.displayError("The file " + partnerFile.getAbsolutePath() + " doesn't exist!");
                        partnerFile = null;
                    }
                }
            }
            return partnerFile;
        }
    }
    
    
    public static GeneralFile[] createCompleteFileList(GeneralFile[] sourceFiles){
        if(sourceFiles == null){
            return null;
        }
        
        Vector completeFileList = new Vector(sourceFiles.length);
        
        for(int i = 0; i < sourceFiles.length; i++){
            completeFileList.add(sourceFiles[i]);
            GeneralFile partnerFile = getPartnerFile(sourceFiles[i]);
            if(partnerFile != null){
                completeFileList.add(partnerFile);
            }
        }
        GeneralFile[] completeFiles = new GeneralFile[completeFileList.size()];
        for(int i = 0; i < completeFiles.length; i++){
            completeFiles[i] = (GeneralFile)completeFileList.get(i);
        }
        return completeFiles;
    }

    /**
     * Returns the extension of the file name.
     *
     * @param   fileName  a file name
     *
     * @return  the extension of the file name/
     */
    private static String getExtension(String fileName) {
        int index = fileName.lastIndexOf(".");

        if (index == -1) {
            return new String("");
        }

        return fileName.substring(index+1);
    }
    
    /**
     * Gets the file name list that the current image contains according to the file info.
     *
     * @param   fileInfoList  the file information list which was contained in the ModelImage.
     *
     * @return  the actual file name list.
     */
    public static Vector getFileNameList(FileInfoBase[] fileInfoList) {

        if ((fileInfoList == null) || (fileInfoList.length == 0)) {
            return null;
        }

        FileInfoBase fileInfo = fileInfoList[0];
        int fileFormat = fileInfo.getFileFormat();
        Vector fileNameList = null;

        switch (fileFormat) {

            /** Ill defined file type.              */
            case FileBase.ERROR:
                return null;

            /** Undefined file type.                */
            case FileBase.UNDEFINED:
                return null;

            /** Not presently implemented.          */
            case FileBase.MIPAV:
                return null;

            /** RAW image data, no header.          */
            case FileBase.RAW:
                return null;

            /** TIFF file; tagged header            */
            case FileBase.TIFF:
                return null;

            /** VOI file, used to read VOIs.        */
            case FileBase.VOI_FILE:
                return null;

            /** Analyze format (Mayo).              */
            case FileBase.ANALYZE:

            /** NIFTI format */
            case FileBase.NIFTI:
                fileNameList = new Vector();

                String imgFileName = fileInfo.getFileName();
                String hdrFileName = imgFileName.replaceFirst(".img", ".hdr");
                fileNameList.add(hdrFileName);
                fileNameList.add(imgFileName);

                return fileNameList;

            /** Digital Imaging and COmmunications in Medicine file type.
             * Fully implemented versions 2 & 3.   */
            case FileBase.DICOM:
                fileNameList = new Vector();
                for (int i = 0; i < fileInfoList.length; i++) {
                    fileNameList.add(fileInfoList[i].getFileName());
                }

                return fileNameList;

            /** Medvision file type.                */
            case FileBase.MEDVISION:
                return null;

            /** Benes Trus special file type.       */
            case FileBase.MAP:
                return null;

            /** Java Image Manangement Interface file type. */
            case FileBase.JIMI:
                return null;

            /** Multiple files of TIFF images.      */
            case FileBase.TIFF_MULTIFILE:
                return null;

            /** MINC file type.  MINC is a medical imaging oriented extension
             * of the NetCDF file format. NetCDF stands for `Network Common Data Form'.  */
            case FileBase.MINC:
                fileNameList = new Vector();
                fileNameList.add(fileInfo.getFileName());

                return fileNameList;

            /** AVI file type.  Windows Media.*/
            case FileBase.AVI:
                return null;

            /** Multiple files of type analyze.     */
            case FileBase.ANALYZE_MULTIFILE:
                return null;

            /** Quicktime file type.            */
            case FileBase.QT:
                return null;

            /** Cheshire file type (a kind of Analyze).*/
            case FileBase.CHESHIRE:
                return null;

            /** Cheshire overlay file type.  Contains VOIs. */
            case FileBase.CHESHIRE_OVERLAY:
                return null;

            /** AFNI file type. */
            case FileBase.AFNI:
                fileNameList = new Vector();

                String headFileName = fileInfo.getFileName();
                String brikFileName = headFileName.replaceFirst(".HEAD", ".BRIK");
                fileNameList.add(headFileName);
                fileNameList.add(brikFileName);

                return fileNameList;

            /** FITS file type. */
            case FileBase.FITS:
                return null;

            /** MetaMorph Stack (STK) file type. */
            case FileBase.STK:
                return null;

            /** Siemens MAGNETOM VISION */
            case FileBase.MAGNETOM_VISION:
                return null;

            /** GE Genesis 5X and LX */
            case FileBase.GE_GENESIS:
                return null;

            /** MRC file format used by IMOD */
            case FileBase.MRC:
                return null;

            /** Interfile file format used in Nuclear Medicine */
            case FileBase.INTERFILE:
                return null;

            /** Micro CT format for small animal imaging */
            case FileBase.MICRO_CAT:
                return null;

            /** RAW MULTIFLE image data, no header. */
            case FileBase.RAW_MULTIFILE:
                return null;

            /** Used by the Zeiss LSM 510 Dataserver */
            case FileBase.LSM:
                return null;

            /** Used by the Zeiss LSM 510 Dataserver */
            case FileBase.LSM_MULTIFILE:
                return null;

            /** Used by the Bio-Rad Pic format */
            case FileBase.BIORAD:
                return null;

            /** Used by FreeSurfer software */
            case FileBase.COR:
                return null;

            /** Bruker file format */
            case FileBase.BRUKER:
                return null;

            /** MIPAV XML file format */
            case FileBase.XML:
                fileNameList = new Vector();

                String rawFileName = fileInfo.getFileName();
                String xmlFileName = rawFileName.replaceFirst(".raw", ".xml");
                fileNameList.add(xmlFileName);
                fileNameList.add(rawFileName);

                return fileNameList;

            /** MIPAV XML file format */
            case FileBase.XML_MULTIFILE:
                return null;

            /** SPM file format */
            case FileBase.SPM:
                return null;

            /** MIPAV project format */
            case FileBase.PROJECT:
                return null;

            /** NIFTI multi-file format */
            case FileBase.NIFTI_MULTIFILE:
                return null;

            /* Image Cytometry Standard */
            case FileBase.ICS:
                return null;

            /* Optical coherence tomography */
            case FileBase.TMG:
                return null;

            /* Washington University OSM dataset structure */
            case FileBase.OSM:
                return null;

            /** MIPAV Surface XML file format */
            case FileBase.SURFACE_XML:
                return null;

            /** Gatan's Digital Micrograph version 3 file format */
            case FileBase.DM3: /** Image modality unknown. */
                return null;
        }

        return null;
    }    
    
    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * The dialog used to transfer files.
     *
     * @author  Hailong Wang (04/19/2006)
     */
    public class JDialogPickFiles extends JDialog implements ActionListener, KeyListener {

        /** DOCUMENT ME! */
        private JButton cancelButton = null;

        /** The default dialog title. */
        private String defaultDialogTitle = "Transfer Files";

        /** DOCUMENT ME! */
        private JButton sourceBrowseButton = null;

        /** Instance variables. */
        private JTextField sourceFilesField = null;

        /** DOCUMENT ME! */
        private JComboBox sourceSchemaComboBox = null;

        /** DOCUMENT ME! */
        private JButton targetBrowseButton = null;

        /** DOCUMENT ME! */
        private JTextField targetFilesField = null;

        /** DOCUMENT ME! */
        private JComboBox targetSchemaComboBox = null;

        /** DOCUMENT ME! */
        private JButton transferButton = null;

        /**
         * Creates a new JDialogPickFiles object.
         *
         * @param  dialogTitle  the dialog title.
         */
        public JDialogPickFiles(String dialogTitle) {
            super(ViewUserInterface.getReference().getMainFrame(), false);

            /**
             * Creates the panel manager used to create the UI items.
             */
            PanelManager manager = new PanelManager();

            manager.getConstraints().insets = new Insets(3, 3, 3, 3);
            manager.add(WidgetFactory.buildLabel("Choose Source Files "));
            sourceSchemaComboBox = new JComboBox(SCHEMAS);
            sourceSchemaComboBox.setSelectedItem(SCHEMAS[0]);

            /**
             * We have to repond to the selection change of this component, because the selection change will invalidate
             * the selected files.
             */
            sourceSchemaComboBox.addActionListener(this);
            sourceSchemaComboBox.setActionCommand("sourceSchemaSelection");
            sourceSchemaComboBox.addKeyListener(this);
            manager.add(sourceSchemaComboBox);
            sourceFilesField = WidgetFactory.buildTextField("");
            sourceFilesField.setColumns(40);
            sourceFilesField.addKeyListener(this);
            manager.add(sourceFilesField);
            sourceBrowseButton = WidgetFactory.buildTextButton("Browse", "Browse srb or local filesystem.",
                                                               "sourceBrowse", this);
            sourceBrowseButton.addKeyListener(this);
            manager.add(sourceBrowseButton);

            /**
             * Builds the UI items for the target part.
             */
            manager.addOnNextLine(WidgetFactory.buildLabel("Choose Target Directory "));
            targetSchemaComboBox = new JComboBox(SCHEMAS);
            targetSchemaComboBox.setSelectedItem(SCHEMAS[1]);
            targetSchemaComboBox.addActionListener(this);
            targetSchemaComboBox.setActionCommand("targetSchemaSelection");
            targetSchemaComboBox.addKeyListener(this);
            manager.add(targetSchemaComboBox);
            targetFilesField = WidgetFactory.buildTextField("");
            targetFilesField.setColumns(40);
            targetFilesField.addKeyListener(this);
            manager.add(targetFilesField);
            targetBrowseButton = WidgetFactory.buildTextButton("Browse", "Browse srb or local filesystem.",
                                                               "targetBrowse", this);
            targetBrowseButton.addKeyListener(this);
            manager.add(targetBrowseButton);

            Container contentPane = getContentPane();
            contentPane.setLayout(new BoxLayout(contentPane, BoxLayout.Y_AXIS));
            getContentPane().add(manager.getPanel());

            manager = new PanelManager();
            manager.getConstraints().insets = new Insets(5, 10, 5, 10);
            transferButton = WidgetFactory.buildTextButton("Transfer", "Transfer files from source to target directory",
                                                           "Transfer", this);
            transferButton.setPreferredSize(new Dimension(60, 30));
            transferButton.addKeyListener(this);
            manager.add(transferButton, GridBagConstraints.CENTER);
            cancelButton = WidgetFactory.buildTextButton("Cancel", "Cancel the file transfering", "Cancel", this);
            cancelButton.setPreferredSize(new Dimension(60, 30));
            cancelButton.setMaximumSize(new Dimension(60, 30));
            cancelButton.addKeyListener(this);
            manager.add(cancelButton);
            getContentPane().add(manager.getPanel());

            if (dialogTitle == null) {
                dialogTitle = getDefaultDialogTitle();
            }

            this.setTitle(dialogTitle);

            pack();
            MipavUtil.centerOnScreen(this);

            /**
             * Always puts the setVisible() as the last statement of the function.
             */
            setVisible(true);
        }



        /**
         * Returns whether the specified JComboBox has selected the SRB schema.
         *
         * @param   comboBox  the specified JComboBox object.
         *
         * @return  whether the specified JComboBox has selected the SRB schema.
         */
        public boolean isFileSchema(JComboBox comboBox) {
            String selectedItem = (String) comboBox.getSelectedItem();

            if (selectedItem.equals(SCHEMAS[0])) {
                return true;
            }

            return false;
        }

        /**
         * Returns whether the specified JComboBox has selected the SRB schema.
         *
         * @param   comboBox  the specified JComboBox object.
         *
         * @return  whether the specified JComboBox has selected the SRB schema.
         */
        public boolean isSRBSchema(JComboBox comboBox) {
            String selectedItem = (String) comboBox.getSelectedItem();

            if (selectedItem.equals(SCHEMAS[1])) {
                return true;
            }

            return false;
        }

        /**
         * **** Action Event Listener.*****
         *
         * @param  event  an action event
         */
        public void actionPerformed(ActionEvent event) {
            String command = event.getActionCommand();

            if (command.equals("Transfer")) {

                if (sourceFilesField.getText().length() == 0) {
                    sourceFilesField.requestFocus();

                    return;
                }

                if (targetFilesField.getText().length() == 0) {
                    targetFilesField.requestFocus();

                    return;
                }

                GeneralFile[] sourceFiles = null;

                if (isFileSchema(sourceSchemaComboBox)) {
                    sourceFiles = converToFiles((GeneralFileSystem) null,
                                                                      sourceFilesField.getText());
                } else if (isSRBSchema(sourceSchemaComboBox)) {

                    if (JDialogLoginSRB.hasValidSRBFileSystem()) {
                        sourceFiles = converToFiles(JDialogLoginSRB.srbFileSystem,
                                                                          sourceFilesField.getText());
                    } else {
                        sourceFilesField.setText("");
                        targetFilesField.setText("");

                        return;
                    }
                }

                if ((sourceFiles == null) || (sourceFiles.length == 0)) {
                    return;
                }

                GeneralFile[] targetFiles = null;

                if (isFileSchema(targetSchemaComboBox)) {
                    targetFiles = converToFiles((GeneralFileSystem) null,
                                                                      targetFilesField.getText());
                } else if (isSRBSchema(targetSchemaComboBox)) {

                    if (JDialogLoginSRB.hasValidSRBFileSystem()) {
                        targetFiles = converToFiles(JDialogLoginSRB.srbFileSystem,
                                                                          targetFilesField.getText());
                    } else {
                        sourceFilesField.setText("");
                        targetFilesField.setText("");

                        return;
                    }
                }

                if ((targetFiles == null) || (targetFiles.length == 0) || (targetFiles.length != 1) ||
                        !targetFiles[0].isDirectory()) {
                    return;
                }

                if (!targetFiles[0].exists()) {
                    targetFiles[0].mkdirs();
                }

                Vector sourceFileList = new Vector();
                Vector targetFileList = new Vector();

                for (int i = 0; i < sourceFiles.length; i++) {
                    recursivelyCreateFileList(sourceFiles[i],
                                              targetFiles[0],
                                              sourceFiles[i].getParentFile(),
                                              sourceFileList,
                                              targetFileList);
                }

                FileTransferSRB fileTransfer = new FileTransferSRB(sourceFileList, targetFileList);
                fileTransfer.start();
                this.dispose();
            } else if (command.equals("Cancel")) {
                this.dispose();
            } else if (command.equals("sourceBrowse")) {

                if (isSRBSchema(sourceSchemaComboBox)) {

                    if (!JDialogLoginSRB.hasValidSRBFileSystem()) {
                        new JDialogLoginSRB("Connect to");
                        if (!JDialogLoginSRB.hasValidSRBFileSystem()) {
                            return;
                        }
                    }


                    /**
                     * Uses the JargonFileChooser to retrieve the file that the user wants to open.
                     */
                    JargonFileChooser chooser = null;

                    try {
                        chooser = new JargonFileChooser(JDialogLoginSRB.srbFileSystem);
                    } catch (OutOfMemoryError e) {
                        e.printStackTrace(System.err);
                        MipavUtil.displayError("Out of memory!");

                        return;
                    } catch (IOException e) {
                        e.printStackTrace(System.err);
                        MipavUtil.displayError(e.getMessage());

                        return;
                    }

                    chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
                    chooser.setMultiSelectionEnabled(true);

                    int returnValue = chooser.showDialog(ViewUserInterface.getReference().getMainFrame(), "Open");

                    if (returnValue == JargonFileChooser.APPROVE_OPTION) {

                        /**
                         * According to the files selected by user, tries to create the srb file list.
                         */
                        SRBFile[] files = chooser.getSelectedFiles();
                        sourceFilesField.setText(convertToString(files));
                    }
                } else if (isFileSchema(sourceSchemaComboBox)) {

                    /**
                     * Uses the JFileChooser to retrieve the file that the user wants to open.
                     */
                    JFileChooser chooser = null;

                    try {
                        chooser = new JFileChooser();
                    } catch (OutOfMemoryError e) {
                        e.printStackTrace(System.err);
                        MipavUtil.displayError("Out of memory!");

                        return;
                    }

                    chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
                    chooser.setMultiSelectionEnabled(true);

                    int returnValue = chooser.showDialog(ViewUserInterface.getReference().getMainFrame(), "Open");

                    if (returnValue == JFileChooser.APPROVE_OPTION) {
                        File[] files = chooser.getSelectedFiles();

                        sourceFilesField.setText(convertToString(files));
                    }
                } else {
                    return;
                }
            } else if (command.equals("targetBrowse")) {

                if (isSRBSchema(targetSchemaComboBox)) {

                    if (!JDialogLoginSRB.hasValidSRBFileSystem()) {
                        new JDialogLoginSRB("Connect to");
                    }

                    if (!JDialogLoginSRB.hasValidSRBFileSystem()) {
                        return;
                    }

                    /**
                     * Uses the JargonFileChooser to retrieve the file that the user wants to open.
                     */
                    JargonFileChooser chooser = null;

                    try {
                        chooser = new JargonFileChooser(JDialogLoginSRB.srbFileSystem);
                    } catch (OutOfMemoryError e) {
                        e.printStackTrace(System.err);
                        MipavUtil.displayError("Out of memory!");

                        return;
                    } catch (IOException e) {
                        e.printStackTrace(System.err);
                        MipavUtil.displayError(e.getMessage());

                        return;
                    }

                    chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                    chooser.setMultiSelectionEnabled(false);

                    int returnValue = chooser.showDialog(ViewUserInterface.getReference().getMainFrame(), "Open");

                    if (returnValue == JargonFileChooser.APPROVE_OPTION) {

                        /**
                         * According to the files selected by user, tries to create the srb file list.
                         */
                        SRBFile[] files = chooser.getSelectedFiles();
                        targetFilesField.setText(convertToString(files));
                    }
                } else if (isFileSchema(targetSchemaComboBox)) {

                    /**
                     * Uses the JFileChooser to retrieve the file that the user wants to open.
                     */
                    JFileChooser chooser = null;

                    try {
                        chooser = new JFileChooser();
                    } catch (OutOfMemoryError e) {
                        e.printStackTrace(System.err);
                        MipavUtil.displayError("Out of memory!");

                        return;
                    }

                    chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                    chooser.setMultiSelectionEnabled(false);

                    int returnValue = chooser.showDialog(ViewUserInterface.getReference().getMainFrame(), "Open");

                    if (returnValue == JFileChooser.APPROVE_OPTION) {

                        /**
                         * According to the files selected by user, tries to create the srb file list.
                         */
                        File file = chooser.getSelectedFile();

                        targetFilesField.setText(file.getPath());
                    }
                } else {
                    return;
                }
            } else if (command.equals("sourceSchemaSelection")) {
                sourceFilesField.setText("");
            } else if (command.equals("targetSchemaSelection")) {
                targetFilesField.setText("");
            }
        }

        /**
         * Return the current uri that is in the uri text field.
         *
         * @return  the current uri in the uri text field, or null if it is malformed
         */
        public URI getSourceURI() {

            try {
                return new URI(sourceFilesField.getText());
            } catch (URISyntaxException urie) {
                urie.printStackTrace(System.err);
                MipavUtil.displayError("SRB URI is malformed: " + urie.getMessage());

                return null;
            }
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public URI getTargetURI() {

            try {
                return new URI(targetFilesField.getText());
            } catch (URISyntaxException urie) {
                urie.printStackTrace(System.err);
                MipavUtil.displayError("URI is malformed: " + urie.getMessage());

                return null;
            }
        }

        /**
         * **** Key Event Listener.*****
         *
         * @param  e  DOCUMENT ME!
         */
        public void keyPressed(KeyEvent e) { }

        /**
         * DOCUMENT ME!
         *
         * @param  e  DOCUMENT ME!
         */
        public void keyReleased(KeyEvent e) { }

        /**
         * DOCUMENT ME!
         *
         * @param  e  DOCUMENT ME!
         */
        public void keyTyped(KeyEvent e) {
            int keyChar = e.getKeyChar();

            if ((keyChar == KeyEvent.VK_ENTER) && e.getSource().equals(cancelButton)) {
                this.dispose();
            } else if (keyChar == KeyEvent.VK_ENTER) {
                actionPerformed(new ActionEvent(this, 10, "Transfer"));
            } else if (keyChar == KeyEvent.VK_ESCAPE) {
                this.dispose();
            }
        }

        /**
         * Changes the URI displayed in the URI field.
         *
         * @param  uri  the url to put into the URI text field
         */
        public void setSourceURI(URI uri) {

            if (uri == null) {
                return;
            }

            String strURI = uri.toString();
            int index = strURI.indexOf("?");

            if (index >= 0) {
                strURI = strURI.substring(0, index);
            }

            sourceFilesField.setText(strURI);
        }

        /**
         * DOCUMENT ME!
         *
         * @param  uri  DOCUMENT ME!
         */
        public void setTargetURI(URI uri) {

            if (uri == null) {
                return;
            }

            String strURI = uri.toString();
            int index = strURI.indexOf("?");

            if (index >= 0) {
                strURI = strURI.substring(0, strURI.indexOf("?"));
            }

            targetFilesField.setText(strURI);
        }

        /**
         * Returns the default dialog title.
         *
         * @return  the default dialog title.
         */
        private String getDefaultDialogTitle() {
            return defaultDialogTitle;
        }

    }

    /**
     * 
     */
    public void actionPerformed(ActionEvent e){
        if(progressBar != null){
            progressBar.dispose();
        }
    }
    
    /**
     * Do nothing.
     *
     * @param  event  the window activated event
     */
    public void windowActivated(WindowEvent event) { }

    /**
     * Do nothing.
     *
     * @param  event  the window closed event
     */
    public void windowClosed(WindowEvent event) { }

    /**
     * Sets completed to false, disposes the progress bar and notifies all listeners that the algorithm is stopped.
     *
     * @param  event  event that triggered function
     */
    public void windowClosing(WindowEvent event) {
        if (progressBar != null) {
            progressBar.dispose();
        }
    }

    /**
     * Do nothing.
     *
     * @param  event  the window deactivated event
     */
    public void windowDeactivated(WindowEvent event) { }

    /**
     * Do nothing.
     *
     * @param  event  the window deiconified event
     */
    public void windowDeiconified(WindowEvent event) { }

    /**
     * Do nothing.
     *
     * @param  event  the window iconified event
     */
    public void windowIconified(WindowEvent event) { }

    /**
     * **** Window Event Listener.*****
     *
     * @param  event  DOCUMENT ME!
     */

    /**
     * Do nothing.
     *
     * @param  event  the window opened event
     */
    public void windowOpened(WindowEvent event) { }
}
