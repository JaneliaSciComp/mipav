package gov.nih.mipav.model.srb;

import java.util.Vector;
import java.util.List;

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


import edu.sdsc.grid.io.FileFactory;
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
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.PanelManager;
import gov.nih.mipav.view.components.WidgetFactory;
import gov.nih.mipav.view.srb.JDialogLoginSRB;
import gov.nih.mipav.view.srb.JargonFileChooser;
import gov.nih.mipav.model.structures.*;

public class SRBFileTransferer implements FileTransferable, Runnable, ActionListener, WindowListener {
    public static final String[] SCHEMAS = { "file", "srb" };
    public static final String TRANSFER_MODE_SEQUENTIAL = "sequential";
    public static final String TRANSFER_MODE_PARELLEL = "parellel";
    
    private static final String DEFAULT_TEMP_DIR_BASE = System.getProperty("user.dir") + File.separator + "mipav-temp" +
                                                   File.separator;
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
     * The list of the source file names.
     */
    private List sourceFileNameList;
    
    /**
     * The source file system.
     */
    private GeneralFileSystem sourceFileSystem;
    
    private GeneralFile[] sourceFiles;
    
    /**
     * The target file system.
     */
    private GeneralFileSystem targetFileSystem;
    
    private GeneralFile[] targetFiles;
    /**
     * The target directory the files will be transferred to.
     */
    private String targetDir;
    
    /**
     * Used to control the transferring mode.
     */
   
    private boolean threadSeperated;
    private File tempDir;
    public SRBFileTransferer(){
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
    
    /**
     * Set 
     * @param threadSeperated
     */
    public void setThreadSeperated(boolean threadSeperated){
        this.threadSeperated = threadSeperated;
    }
    
    /**
     * Returns the temporary directory created randomly under the temporary
     * base directory.
     * 
     * @returnthe temporary directory created randomly under the temporary
     *            base directory.
     */
    public File getTempDir(){
        return tempDir;
    }
    
    /**
     * Creates a random sub directory under the temporary base directory.
     */
    public void createTempDir(){
        this.tempDir = createRandomLocalDir(getTempDirBase());
        this.tempDir.deleteOnExit();
    }
    
    /**
     * Returns the temporary base directory which will be used to hold
     * memory images. if the temporary base directory has been setted up,
     * then this temporary base directory will be used. Otherwise the
     * default temporary base directy will be used.
     * 
     * @return  the temporary base directory.
     */
    public File getTempDirBase(){
        String tempDirBaseName = Preferences.getSRBTempDir();
        if(tempDirBaseName == null){
            tempDirBaseName = DEFAULT_TEMP_DIR_BASE;
        }
        File tempDirBase = new File(tempDirBaseName);
        if(!tempDirBase.exists()){
            tempDirBase.mkdirs();
        }
        return tempDirBase;
    }
    
    /**
     * Returns the source file list.
     */
    public GeneralFile[] getSourceFiles(){
        return sourceFiles;
    }
    
    /**
     * Sets the source file list to new file list. 
     * @param sourceFiles  the new source file list.
     */
    public void setSourceFiles(GeneralFile[] sourceFiles){
        this.sourceFiles = sourceFiles;
    }
    
    /**
     * Returns the target files which can't be directories.
     */
    public GeneralFile[] getTargetFiles(){
        return targetFiles;
    }
    
    /**
     * Sets the target files which can not be directories.
     * @param targetFiles   the target files.
     */
    public void setTargetFiles(GeneralFile[] targetFiles){
        this.targetFiles = targetFiles;
    }
    
    /**
     * Returns the source file name list which was selected by user. 
     * @return the source file name list which was selected by user.
     */
    public List getSourceFileNameList() {
        return sourceFileNameList;
    }

    /**
     * Sets the source file name list which includes the directory. 
     * @param sourceFileNameList the source file name list.
     */
    public void setSourceFileNameList(List sourceFileNameList){
        this.sourceFileNameList = sourceFileNameList;
    }
    
    /**
     * Returns the transfer mode while transferring files.
     * @return the transfer mode while transferring files.
     */
    public String getTransferMode(){
        String transferMode = Preferences.getSRBTransferMode();
        if(transferMode == null){
            transferMode = TRANSFER_MODE_SEQUENTIAL;
        }
        return transferMode;
    }
    
    /**
     * According to the uri schema, uses appropriate file chooser to select the
     * source files.
     * 
     * @param schema  the uri schema.
     * @return        the user selected source files.
     */
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
                    GeneralFile[] sourceFiles = new GeneralFile[files.length];
                    for(int i = 0; i < files.length; i++){
                        sourceFiles[i] = new LocalFile(files[i]);
                    }
                    return sourceFiles;
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
                if (!JDialogLoginSRB.hasValidSRBFileSystem()) {
                    return null;
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
    
    /**
     * According to the schema, uses appropriate file chooser to select the
     * target directory.
     * 
     * @param schema   the uri schema.
     * @return         the user selected target directory.
     */
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

            int returnValue = chooser.showDialog(ViewUserInterface.getReference().getMainFrame(), "Save");

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

            int returnValue = chooser.showDialog(ViewUserInterface.getReference().getMainFrame(), "Save");

            if (returnValue == JargonFileChooser.APPROVE_OPTION) {
                return chooser.getSelectedFile();
            }            
        }
        return null;
    }

    /**
     * Sets the target directory to the new directory.
     * @param targetDir  the new directory.
     */
    public void setTargetDir(String targetDir){
        this.targetDir = targetDir;
    }
    
    /**
     * A helper function to obtain the parent directory based on the given file name and
     * the path separator string.
     * 
     * @param fileName        the file name.
     * @param pathSeparator   the path separator string.
     * @return                the parent directory string of this file name.
     */
    public static String getParentDirectory(String fileName, String pathSeparator){
        if(fileName == null || pathSeparator == null){
            return null;
        }
        
        if(fileName.indexOf(pathSeparator) >= 0){
            return fileName.substring(0, fileName.lastIndexOf(pathSeparator));
        }
        return null;
    }
    
    /**
     * A helper function to retrieve all files contained in this directory and subdirectory
     * recursively and put into the <code>fileList</code>.
     * 
     * @param file      the given file or directory.
     * @param fileList  the file list which stores all files included in the
     *                  <code>file</code> and its subdirectory.
     */
    public static void retrieveFileList(GeneralFile file, List fileList){
        if(file == null || fileList == null){
            return;
        }
        
        if(file.isFile()){
            fileList.add(file);
            return;
        }
        GeneralFile[] children = file.listFiles();
        if(children == null){
            return;
        }
        
        for(int i = 0; i < children.length; i++){
            retrieveFileList(children[i], fileList);
        }
    }

    /**
     * A helper function to create the target file based on the target directory,
     * the source base directory and the source file.
     * 
     * @param targetDir        the target directory.
     * @param baseSourceFile   the source base directory.
     * @param sourceFile       the source file.
     * @return                 the target file.
     */
    public static GeneralFile createTargetFile(GeneralFile targetDir,
            GeneralFile baseSourceFile, GeneralFile sourceFile) {
        
        if(targetDir == null || baseSourceFile == null || sourceFile == null){
            return null;
        }
        
        String baseSourceFileName = baseSourceFile.getAbsolutePath();
        String sourceFileName = sourceFile.getAbsolutePath();
        if(sourceFileName.indexOf(baseSourceFileName) == 0){
            String sourcePathSeperator = (baseSourceFile instanceof SRBFile)?SRBFile.PATH_SEPARATOR:File.separator;
            String targetPathSeperator = (targetDir instanceof SRBFile)?SRBFile.PATH_SEPARATOR:File.pathSeparator;
            String targetFileName = sourceFileName.substring(baseSourceFileName.length() + sourcePathSeperator.length());
            if(!sourcePathSeperator.equals(targetPathSeperator)){
                targetFileName = replacePathSeparator(targetFileName, sourcePathSeperator, targetPathSeperator);
            }
            if(targetDir instanceof SRBFile){
                return new SRBFile((SRBFile)targetDir, targetFileName);
            }else{
                return new LocalFile((LocalFile)targetDir, targetFileName);
            }
        }
        return null;
    }

    /**
     * A helper function to create the target file list based on the given source file list,
     * the target directory, and the source base directory.
     * 
     * @param targetDir       the target directory.
     * @param baseSourceFile  the source base directory.
     * @param sourceFiles     the source file list.
     * @return                the target file list.
     */
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
    
    /**
     * Transfers the source file to the target file.
     * 
     * @param sourceFile the source file
     * @param targetFile the target file
     * @return true if the transfer succeeded.
     */
    public boolean transfer(GeneralFile sourceFile, GeneralFile targetFile) {
        if(sourceFile == null || targetFile == null){
            return false;
        }
        GeneralFile targetDir = targetFile.getParentFile();
        if(!targetDir.exists()){
            recursivelyMakeDir(targetDir);
        }
        if(getTransferMode().equals(TRANSFER_MODE_PARELLEL)){
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
                System.out.println("Before new byte : " + length);
                byte[] buffer = new byte[(int)length];
                System.out.println("sourceRandomAccessFile.read(buffer, 0, (int) length) ");
                sourceRandomAccessFile.read(buffer, 0, (int) length);
                System.out.println("destinationRandomAccessFile.write(buffer, 0, (int) length) ");
                destinationRandomAccessFile.write(buffer, 0, (int) length);
                System.out.println("sourceRandomAccessFile.close()");
                sourceRandomAccessFile.close();
                destinationRandomAccessFile.close();

                return true;
            } catch (IOException e) {
                e.printStackTrace(System.err);
                MipavUtil.displayError("File I/O error: " + e.getMessage());

                return false;
            } catch(OutOfMemoryError e){
                e.printStackTrace(System.err);
                MipavUtil.displayError("Out of memory error: " + e.getMessage());

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
    
    public void transferFiles(List sourceFileNames, String targetDir){
        if(sourceFileNames == null || targetDir == null){
            return;
        }
        
        Vector sourceFileList = new Vector();
        Vector targetFileList = new Vector();
        
        int index = 0;
        for(int i = 0; i < sourceFileNames.size(); i++){
            GeneralFile sourceFile = FileFactory.newFile(sourceFileSystem,
                    (String) sourceFileNames.get(i));
            if(sourceFile.isFile()){
                sourceFileList.add(index, sourceFile);
                GeneralFile targetFile = FileFactory.newFile(targetFileSystem, targetDir, sourceFile.getName());
                targetFileList.add(index++, targetFile);
            }else{
                retrieveFileList(sourceFile, sourceFileList);
                for(int j = index; j < sourceFileList.size(); j++){
                    String baseSourceDir = getParentDirectory((String) sourceFileNames.get(i), getPathSeparator(sourceFileSystem));
                    GeneralFile targetFile = createTargetFile(targetDir, baseSourceDir, ((GeneralFile)sourceFileList.get(j)).getAbsolutePath());
                    targetFileList.add(targetFile);
                }
            }
        }
        
        sourceFiles = new GeneralFile[sourceFileList.size()];
        targetFiles = new GeneralFile[targetFileList.size()];
        for(int i = 0; i < sourceFileList.size(); i++){
            sourceFiles[i] = (GeneralFile)sourceFileList.get(i);
            targetFiles[i] = (GeneralFile)targetFileList.get(i);
        }
        new Thread(this).start();
        
    }
    
    /**
     * First obtains the relative file name of the source file name related to the given
     * the source base directory name, then adjusts the path separator based on 
     * their belonged file system, finally returns the file which used the target
     * directory as its parent directory and uses this relative file name its
     * file name.
     *  
     * @param targetDirName      the target directory name.
     * @param baseSourceDirName  the source base directory.
     * @param sourceFileName     the source file name.
     * @return                   the target file consisted of the target directory
     *                           and relative file name of the source file name related
     *                           to the source base directory.
     */
    private GeneralFile createTargetFile(String targetDirName,
            String baseSourceDirName, String sourceFileName){
        if(targetDirName == null || baseSourceDirName == null || sourceFileName == null){
            return null;
        }
        
        if(sourceFileSystem == null || targetFileSystem == null){
            return null;
        }
        
        String sourcePathSeparator = getPathSeparator(sourceFileSystem);
        String targetPathSeparator = getPathSeparator(targetFileSystem);
        if(sourceFileName.indexOf(baseSourceDirName) != 0){
            return null;
        }
        
        String targetFileName = targetDirName + sourceFileName.substring(baseSourceDirName.length());
        if(!sourcePathSeparator.equals(targetPathSeparator)){
            targetFileName = replacePathSeparator(targetFileName, sourcePathSeparator, targetPathSeparator);
        }
        return FileFactory.newFile(targetFileSystem, targetFileName);
    }
    
    /**
     * A helper function to obtain the path separator based on the different file system.
     * @param fs  the file system
     * @return    the path separator used by the file system.
     */
    public static String getPathSeparator(GeneralFileSystem fs){
        if(fs == null){
            return null;
        }
        
        if(fs instanceof LocalFileSystem){
            return File.separator;
        }else{
            return SRBFile.PATH_SEPARATOR;
        }
    }
    
    /**
     * @see FileTransferable#transfer(GeneralFile[], GeneralFile[])
     */
    public boolean transfer(GeneralFile[] sourceFiles, GeneralFile[] targetFiles) {
        long startTime = System.currentTimeMillis();
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
        long spentTime = System.currentTimeMillis() - startTime;
        Preferences.debug("The time spent on the file transfer is " + spentTime);
        System.out.println("The time spent on the file transfer is " + spentTime);
        return ret;
    }

    /**
     * A top level function to accomplish the file transfer scenarios.
     */
    public void transferFiles(){
        new JDialogPickFiles(null);
    }
    
    /**
     * @see Runnable#run()
     */
    public void run() {
        transfer(sourceFiles, targetFiles);
    }

    /**
     * Saves the memory image to the target directory of the SRB server.
     * @param image            the memory image.
     * @param targetDir        the target directory of the SRB server.
     */
    public void saveToSRB(ModelImage image, String targetDir){
        GeneralFile[] sourceFiles = getFileList(image);
        if(sourceFiles == null){
            return;
        }
        /**
         * Copies the local temporary files to the directory of the SRB server.
         */
        GeneralFile[] targetFiles = createTargetFiles(FileFactory.newFile(JDialogLoginSRB.srbFileSystem, targetDir),
                sourceFiles[0].getParentFile(),
                sourceFiles);

        for (int i = 0; i < sourceFiles.length; i++) {
            LocalFile lf = (LocalFile) sourceFiles[i];
            lf.deleteOnExit();
        }
        setSourceFiles(sourceFiles);
        setTargetFiles(targetFiles);
        setThreadSeperated(true);
        new Thread(this).start();
    }
    
    /**
     * Saves the memory image to the target directory of the SRB server.
     * @param image            the memory image.
     * @param targetDir        the target directory of the SRB server.
     */
    public void saveToSRB(ModelImage image, GeneralFile targetDir){
        GeneralFile[] sourceFiles = getFileList(image);
        if(sourceFiles == null){
            return;
        }
        /**
         * Copies the local temporary files to the directory of the SRB server.
         */
        GeneralFile[] targetFiles = createTargetFiles(targetDir,
                sourceFiles[0].getParentFile(), sourceFiles);

        for (int i = 0; i < sourceFiles.length; i++) {
            LocalFile lf = (LocalFile) sourceFiles[i];
            lf.deleteOnExit();
        }
        setSourceFiles(sourceFiles);
        setTargetFiles(targetFiles);
        setThreadSeperated(true);
        new Thread(this).start();
    }
    
    
    /**
     * Saves the memory image to the srb server, the target directory will be selected
     * by the user.
     * @param image  the momery image.
     */
    public void saveToSRB(ModelImage image){
        GeneralFile targetDir = selectTargetDirectory(SRBFileTransferer.SCHEMAS[1]);
        if(targetDir == null){
            return;
        }
        saveToSRB(image, targetDir);
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
     * Saves the memory image to the local temporary files and
     * returns the file name list.
     * @param image the memory image.
     * @return      the saved file name list of this memory image.
     */
    public GeneralFile[] getFileList(ModelImage image){
        this.createTempDir();
        return getFileList(image, this.getTempDir().getAbsolutePath());
    }
    
    /**
     * Saves the memory image to the local temporary files and returns the LocalFile object list.
     * 
     * @param image         the memory image.
     * @param localTempDir  the local directory that the memory image will saved to
     * @return              the saved LocalFile object list of this memory image.
     */
    public static GeneralFile[] getFileList(ModelImage image, String tempDir){
        if (image == null) {
            return null;
        }

        if(tempDir == null || tempDir.length() == 0){
            return null;
        }
        LocalFile localTempDir = createLocalFile(tempDir);
        if(localTempDir == null){
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
        Vector fileNameList = SRBFileTransferer.getFileNameList(currentFileInfoList);
        
        Vector sourceFileList = new Vector();
        for(int i = 0; i < fileNameList.size(); i++){
            sourceFileList.add(FileFactory.newFile(localTempDir, (String)fileNameList.get(i)));
        }

        /**
         * Constructs the FileWriteOptions to prepare the file name for save.
         */
        FileWriteOptions opts = new FileWriteOptions(((LocalFile) sourceFileList.get(0)).getName(),
                                                     localTempDir.getPath() + "//", false);


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
        for(int i = 0; i < sourceFiles.length; i++){
            sourceFiles[i] = (GeneralFile)sourceFileList.get(i);
        }
        return sourceFiles;
    }
    
    /**
     * A helper function to create a random subdirectory under the parent directoy localDirBase.
     * @param localDirBase  the local parent directory.
     * @return              the created random diretory.
     */
    public static File createRandomLocalDir(File localDirBase){
        long currentTime = System.currentTimeMillis();
        File newLocalDir = new File(localDirBase, Long.toString(currentTime));
        if(!newLocalDir.exists()){
            newLocalDir.mkdirs();
        }
        return newLocalDir;
    }
    
    /**
     * A helper function to create a LocalFile object based on the file name.
     * @param fileName  a file name
     * @return          a LocalFile object.
     */
    public static LocalFile createLocalFile(String fileName){
        if(fileName == null || fileName.length() == 0){
            return null;
        }
        File file = new File(fileName);
        if(!file.exists()){
            file.mkdirs();
        }
        return new LocalFile(file);
    }
    
    /**
     * Converts the comma separated file names string into the file name list.
     * @param fileNames  the comma separated file names string. 
     * @return           the list of the file names.
     */
    public static List converToFileNameList(String fileNames){
        if(fileNames == null || fileNames.length() == 0){
            return null;
        }
        
        Vector newFileNameList = null;
        if (fileNames.indexOf(",") >= 0) {
            String[] fns = fileNames.split(",");

            if ((fns == null) || (fns.length == 0)) {
                return null;
            }

            newFileNameList = new Vector(fns.length);

            for (int i = 0; i < fns.length; i++) {
                newFileNameList.add(fns[i]);
            }
            
        } else {
            newFileNameList = new Vector(1);
            newFileNameList.add(fileNames);
        }
        return newFileNameList;
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

        if(sourceSeparator.equals(targetSeparator)){
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

    /**
     * A helper function to convert the GeneralFile list ot the GeneralFile array.
     * @param fileList  the GeneralFile list
     * @return          the GeneralFile array.
     */
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
            newFiles[i] = FileFactory.newFile(fileSystem, (String)fileNameList.get(i));
        }
        
        return newFiles;
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
    public void recursivelyCreateFileList(GeneralFile sourceFile, GeneralFile targetDir,
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

                    GeneralFile newTargetFile = FileFactory.newFile(targetDir,
                            childrenFiles[i].getPath().substring(sourceRootDir.getPath().length()));
                    
                    recursivelyCreateDirectory(newTargetFile.getParentFile());
                    targetFileList.add(newTargetFile);
                }
            }

        } else {
            sourceFileList.add(sourceFile);

            GeneralFile newTargetFile = FileFactory.newFile(targetDir,
                    sourceFile.getPath().substring(sourceRootDir.getPath().length()));
            
            recursivelyCreateDirectory(newTargetFile.getParentFile());
            targetFileList.add(newTargetFile);
        }
    }

    /**
     * Returns the primary file which was recognized by the mipav.
     * 
     * @param sourceFiles  the file list which contains the whole file list of some format image.
     * 
     * @return the primary file list.
     */
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
    
    /**
     * Returns the partner file of the specified file. Because some format image file can
     * contains several files, otherwise it can not be opened.
     * 
     * @param sourceFile  the specified file
     * @return            the partner file of the specified file.
     */
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
    
    /**
     * Creates the complete file list based on the partial file list.
     * @param sourceFiles  the partial file list.
     * @return             the complete file list.
     */
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


                sourceFileNameList = converToFileNameList(sourceFilesField.getText());


                if ((sourceFileNameList == null) || (sourceFileNameList.size() == 0)) {
                    return;
                }

                targetDir = targetFilesField.getText();


                if ((targetDir == null) || (targetDir.length() == 0)) {
                    return;
                }
                
                // Have to set up the source file system and target file system.
                if(isFileSchema(sourceSchemaComboBox)){
                    sourceFileSystem = new LocalFileSystem();
                }else{
                    sourceFileSystem = JDialogLoginSRB.srbFileSystem;
                }
                if(isFileSchema(targetSchemaComboBox)){
                    targetFileSystem = new LocalFileSystem();
                }else{
                    targetFileSystem = JDialogLoginSRB.srbFileSystem;
                }
                transferFiles(sourceFileNameList, targetDir);
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
