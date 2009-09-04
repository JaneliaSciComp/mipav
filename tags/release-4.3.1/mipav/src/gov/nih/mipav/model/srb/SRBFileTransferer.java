package gov.nih.mipav.model.srb;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;
import gov.nih.mipav.view.srb.*;

import edu.sdsc.grid.io.*;
import edu.sdsc.grid.io.local.*;
import edu.sdsc.grid.io.srb.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.net.*;

import java.util.*;
import java.util.List;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class SRBFileTransferer implements FileTransferable, Runnable, ActionListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final String[] SCHEMAS = { "file", "srb" };

    /** DOCUMENT ME! */
    public static final String TRANSFER_MODE_SEQUENTIAL = "sequential";

    /** DOCUMENT ME! */
    public static final String TRANSFER_MODE_PARALLEL = "parallel";

    /** DOCUMENT ME! */
    private static final String DEFAULT_TEMP_DIR_BASE = System.getProperty("user.home") + File.separator + "mipav" +
                                                        File.separator + "srb-temp" + File.separator;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The progress bar used to indicate the progress of reading or writing. */
    private ViewJProgressBar progressBar;

    /** The location of the progress bar. */
    private Point progressBarLocation;

    /** The flag to indicate the visibility of the progress bar. */
    private boolean progressBarVisible = true;

    /** The list of the source file names. */
    private List<String> sourceFileNameList;

    /** DOCUMENT ME! */
    private GeneralFile[] sourceFiles;

    /** The source file system. */
    private GeneralFileSystem sourceFileSystem;

    /** The target directory the files will be transferred to. */
    private String targetDir;

    /** DOCUMENT ME! */
    private GeneralFile[] targetFiles;

    /** The target file system. */
    private GeneralFileSystem targetFileSystem;

    /** DOCUMENT ME! */
    private File tempDir;

    /** */
    boolean isNDAR = false;
    
    
    /** Used to control the transferring mode. */
    private boolean threadSeperated;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new SRBFileTransferer object.
     */
    public SRBFileTransferer() {
        threadSeperated = true;
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent e) {

        if (progressBar != null) {
            progressBar.dispose();
        }
    }

    /**
     * Creates a random sub directory under the temporary base directory.
     */
    public void createTempDir() {
        this.tempDir = SRBUtility.createRandomLocalDir(getTempDirBase());
        this.tempDir.deleteOnExit();
    }

    public void initFileSystemNDARSend() {
    	sourceFileSystem = new LocalFileSystem();
    	
    	if (!JDialogLoginSRB.hasValidSRBFileSystem()) {
            new JDialogLoginSRB("Connect to", false);

            if (!JDialogLoginSRB.hasValidSRBFileSystem()) {
                return;
            }
        }
    	
    	targetFileSystem = JDialogLoginSRB.srbFileSystem;
    }
    
    /**
     * Saves the memory image to the local temporary files and returns the file name list.
     *
     * @param   image  the memory image.
     *
     * @return  the saved file name list of this memory image.
     */
    public GeneralFile[] getFileList(ModelImage image) {
        this.createTempDir();

        return SRBUtility.getFileList(image, this.getTempDir().getAbsolutePath());
    }

    /**
     * Returns the source file name list which was selected by user.
     *
     * @return  the source file name list which was selected by user.
     */
    public List<String> getSourceFileNameList() {
        return sourceFileNameList;
    }

    /**
     * Returns the source file list.
     *
     * @return  DOCUMENT ME!
     */
    public GeneralFile[] getSourceFiles() {
        return sourceFiles;
    }

    /**
     * Returns the target files which can't be directories.
     *
     * @return  DOCUMENT ME!
     */
    public GeneralFile[] getTargetFiles() {
        return targetFiles;
    }

    /**
     * Returns the temporary directory created randomly under the temporary base directory.
     *
     * @return  the temporary directory created randomly under the temporary base directory.
     */
    public File getTempDir() {
        return tempDir;
    }

    /**
     * Returns the temporary base directory which will be used to hold memory images. if the temporary base directory
     * has been set up, then this temporary base directory will be used. Otherwise the default temporary base directy
     * will be used.
     *
     * @return  the temporary base directory.
     */
    public File getTempDirBase() {
        String tempDirBaseName = Preferences.getSRBTempDir();

        if (tempDirBaseName == null) {
            tempDirBaseName = DEFAULT_TEMP_DIR_BASE;
        }

        File tempDirBase = new File(tempDirBaseName);

        if (!tempDirBase.exists()) {
            tempDirBase.mkdirs();
        }

        return tempDirBase;
    }

    /**
     * Returns the transfer mode while transferring files.
     *
     * @return  the transfer mode while transferring files.
     */
    public String getTransferMode() {
        String transferMode = Preferences.getSRBTransferMode();

        if (transferMode == null) {
            transferMode = TRANSFER_MODE_SEQUENTIAL;
        }

        return transferMode;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean isProgressBarVisible() {
        return progressBarVisible;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean isThreadSeperated() {
        return threadSeperated;
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
    public void recursivelyCreateFileList(GeneralFile sourceFile, GeneralFile targetDir, GeneralFile sourceRootDir,
                                          Vector<GeneralFile> sourceFileList, Vector<GeneralFile> targetFileList) {

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

                    SRBUtility.recursivelyMakeDir(newTargetFile.getParentFile());
                    targetFileList.add(newTargetFile);
                }
            }

        } else {
            sourceFileList.add(sourceFile);

            GeneralFile newTargetFile = FileFactory.newFile(targetDir,
                                                            sourceFile.getPath().substring(sourceRootDir.getPath().length()));

            SRBUtility.recursivelyMakeDir(newTargetFile.getParentFile());
            targetFileList.add(newTargetFile);
        }
    }

    /**
     * @see  Runnable#run()
     */
    public void run() {
        transfer(sourceFiles, targetFiles);
    }


    /**
     * Set the flag to be NDAR related (true = ndar)
     * @param isNDAR boolean use NDAR settings
     */
    public void setNDAR(boolean doNDAR) {
    	this.isNDAR = doNDAR;
    }
    
    /**
     * Saves the memory image to the srb server, the target directory will be selected by the user.
     *
     * @param  image  the momery image.
     */
    public void saveToSRB(ModelImage image) {
        GeneralFile targetDir = selectTargetDirectory(SRBFileTransferer.SCHEMAS[1]);

        if (targetDir == null) {
            return;
        }

        saveToSRB(image, targetDir);
    }

    /**
     * Saves the memory image to the target directory of the SRB server.
     *
     * @param  image      the memory image.
     * @param  targetDir  the target directory of the SRB server.
     */
    public void saveToSRB(ModelImage image, String targetDir) {
        GeneralFile[] sourceFiles = getFileList(image);

        if (sourceFiles == null) {
            return;
        }

        /**
         * Copies the local temporary files to the directory of the SRB server.
         */
        GeneralFile[] targetFiles = SRBUtility.createTargetFiles(FileFactory.newFile(JDialogLoginSRB.srbFileSystem,
                                                                                     targetDir),
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
     * Saves the memory image to the target directory of the SRB server.
     *
     * @param  image      the memory image.
     * @param  targetDir  the target directory of the SRB server.
     */
    public void saveToSRB(ModelImage image, GeneralFile targetDir) {
        GeneralFile[] sourceFiles = getFileList(image);

        if (sourceFiles == null) {
            return;
        }

        /**
         * Copies the local temporary files to the directory of the SRB server.
         */
        GeneralFile[] targetFiles = SRBUtility.createTargetFiles(targetDir, sourceFiles[0].getParentFile(),
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
     * According to the uri schema, uses appropriate file chooser to select the source files.
     *
     * @param   schema  the uri schema.
     *
     * @return  the user selected source files.
     */
    public GeneralFile[] selectSourceFiles(String schema) {

        if ((schema == null) || (schema.length() == 0)) {
            return null;
        }

        if (schema.equals(SCHEMAS[0])) {

            /**
             * Uses the JFileChooser to retrieve the file that the user wants to open.
             */
            JFileChooser chooser = new JFileChooser();

            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            chooser.setMultiSelectionEnabled(true);

            int returnValue = chooser.showDialog(ViewUserInterface.getReference().getMainFrame(), "Open");

            if (returnValue == JargonFileChooser.APPROVE_OPTION) {
                File[] files = chooser.getSelectedFiles();

                if (files != null) {
                    GeneralFile[] sourceFiles = new GeneralFile[files.length];

                    for (int i = 0; i < files.length; i++) {
                        sourceFiles[i] = new LocalFile(files[i]);
                    }

                    return sourceFiles;
                }
            }

            return null;
        } else if (schema.equals(SCHEMAS[1])) {

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
     * According to the schema, uses appropriate file chooser to select the target directory.
     *
     * @param   schema  the uri schema.
     *
     * @return  the user selected target directory.
     */
    public GeneralFile selectTargetDirectory(String schema) {

        if ((schema == null) || (schema.length() == 0)) {
            return null;
        }

        if (schema.equals(SCHEMAS[0])) {

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
        } else if (schema.equals(SCHEMAS[1])) {

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
     * DOCUMENT ME!
     *
     * @param  location  DOCUMENT ME!
     */
    public void setProgressBarLocation(Point location) {
        this.progressBarLocation = location;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  visible  DOCUMENT ME!
     */
    public void setProgressBarVisible(boolean visible) {
        this.progressBarVisible = visible;
    }

    /**
     * Sets the source file name list which includes the directory.
     *
     * @param  sourceFileNameList  the source file name list.
     */
    public void setSourceFileNameList(List sourceFileNameList) {
        this.sourceFileNameList = sourceFileNameList;
    }

    /**
     * Sets the source file list to new file list.
     *
     * @param  sourceFiles  the new source file list.
     */
    public void setSourceFiles(GeneralFile[] sourceFiles) {
        this.sourceFiles = sourceFiles;
    }

    /**
     * Sets the target directory to the new directory.
     *
     * @param  targetDir  the new directory.
     */
    public void setTargetDir(String targetDir) {
        this.targetDir = targetDir;
    }

    /**
     * Sets the target files which can not be directories.
     *
     * @param  targetFiles  the target files.
     */
    public void setTargetFiles(GeneralFile[] targetFiles) {
        this.targetFiles = targetFiles;
    }

    /**
     * Set.
     *
     * @param  threadSeperated  DOCUMENT ME!
     */
    public void setThreadSeperated(boolean threadSeperated) {
        this.threadSeperated = threadSeperated;
    }

    /**
     * Transfers the source file to the target file.
     *
     * @param   sourceFile  the source file
     * @param   targetFile  the target file
     *
     * @return  true if the transfer succeeded.
     */
    public boolean transfer(GeneralFile sourceFile, GeneralFile targetFile) {

        if ((sourceFile == null) || (targetFile == null)) {
            return false;
        }

        GeneralFile targetDir = targetFile.getParentFile();

        if (!targetDir.exists()) {
            SRBUtility.recursivelyMakeDir(targetDir);
        }

        if (getTransferMode().equals(TRANSFER_MODE_PARALLEL)) {

            try {
                sourceFile.copyTo(targetFile);

                return true;
            } catch (IOException e) {
                e.printStackTrace(System.err);
                MipavUtil.displayError("File I/O error: " + e.getMessage());

                return false;
            }
        } else {
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
            } catch (OutOfMemoryError e) {
                e.printStackTrace(System.err);
                MipavUtil.displayError("Out of memory error: " + e.getMessage());

                return false;
            }
        }
    }

    /**
     * @see  FileTransferable#transfer(GeneralFile[], GeneralFile[])
     */
    public boolean transfer(GeneralFile[] sourceFiles, GeneralFile[] targetFiles) {
        long startTime = System.currentTimeMillis();

        if ((sourceFiles == null) || (targetFiles == null)) {
            return false;
        }

        if (sourceFiles.length != targetFiles.length) {
            return false;
        }

        /**
         * Builds the progress bar for the file transferring.
         */
        if (isProgressBarVisible()) {
            buildProgressBar("Transfering " + sourceFiles[0].getName(), "Transfering files ...", 0, 100);
        }

        int mod = sourceFiles.length / 100;

        if (mod < 1) {
            mod = 1;
        }

        initProgressBar();

        boolean ret = true;

        for (int i = 0; i < sourceFiles.length; i++) {

            if ((((i) % mod) == 0)) {
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
    public void transferFiles() {
        new JDialogPickFiles(null);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  sourceFileNames  DOCUMENT ME!
     * @param  targetDir        DOCUMENT ME!
     */
    public void transferFiles(List<String> sourceFileNames, String targetDir) {

        if ((sourceFileNames == null) || (targetDir == null)) {
            return;
        }

        Vector<GeneralFile> sourceFileList = new Vector<GeneralFile>();
        Vector<GeneralFile> targetFileList = new Vector<GeneralFile>();

        int index = 0;

        for (int i = 0; i < sourceFileNames.size(); i++) {
            GeneralFile sourceFile = FileFactory.newFile(sourceFileSystem, (String) sourceFileNames.get(i));

            if (sourceFile.isFile()) {
                sourceFileList.add(index, sourceFile);

                GeneralFile targetFile = FileFactory.newFile(targetFileSystem, targetDir, sourceFile.getName());
                targetFileList.add(index++, targetFile);
            } else {
                SRBUtility.retrieveFileList(sourceFile, sourceFileList);

                for (int j = index; j < sourceFileList.size(); j++) {
                    String baseSourceDir = SRBUtility.getParentDirectory((String) sourceFileNames.get(i),
                                                                         SRBUtility.getPathSeparator(sourceFileSystem));
                    GeneralFile targetFile = createTargetFile(targetDir, baseSourceDir,
                                                              ((GeneralFile) sourceFileList.get(j)).getAbsolutePath());
                    targetFileList.add(targetFile);
                }
            }
        }

        sourceFiles = new GeneralFile[sourceFileList.size()];
        targetFiles = new GeneralFile[targetFileList.size()];

        for (int i = 0; i < sourceFileList.size(); i++) {
            sourceFiles[i] = (GeneralFile) sourceFileList.get(i);
            targetFiles[i] = (GeneralFile) targetFileList.get(i);
        }

        new Thread(this).start();

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
     * First obtains the relative file name of the source file name related to the given the source base directory name,
     * then adjusts the path separator based on their belonged file system, finally returns the file which used the
     * target directory as its parent directory and uses this relative file name its file name.
     *
     * @param   targetDirName      the target directory name.
     * @param   baseSourceDirName  the source base directory.
     * @param   sourceFileName     the source file name.
     *
     * @return  the target file consisted of the target directory and relative file name of the source file name related
     *          to the source base directory.
     */
    public GeneralFile createTargetFile(String targetDirName, String baseSourceDirName, String sourceFileName) {

        if ((targetDirName == null) || (baseSourceDirName == null) || (sourceFileName == null)) {
            return null;
        }
        
        if ((sourceFileSystem == null) || (targetFileSystem == null)) {
            return null;
        }
        
        String sourcePathSeparator = SRBUtility.getPathSeparator(sourceFileSystem);
        String targetPathSeparator = SRBUtility.getPathSeparator(targetFileSystem);

        if (sourceFileName.indexOf(baseSourceDirName) != 0) {
            return null;
        }
        String targetFileName = targetDirName + sourceFileName.substring(baseSourceDirName.length());

        if (!sourcePathSeparator.equals(targetPathSeparator)) {
            targetFileName = SRBUtility.replacePathSeparator(targetFileName, sourcePathSeparator, targetPathSeparator);
        }

        return FileFactory.newFile(targetFileSystem, targetFileName);
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * The dialog used to transfer files.
     *
     * @author  Hailong Wang (04/19/2006)
     */
    public class JDialogPickFiles extends JDialog implements ActionListener, KeyListener {

        /** DOCUMENT ME! */
        private JButton cancelButton, helpButton;

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
            manager.add(transferButton);
            cancelButton = WidgetFactory.buildTextButton("Cancel", "Cancel the file transfering", "Cancel", this);
            cancelButton.setPreferredSize(new Dimension(60, 30));
            cancelButton.setMaximumSize(new Dimension(60, 30));
            cancelButton.addKeyListener(this);
            helpButton = new JButton("Help");
            helpButton.addActionListener(this);
            helpButton.setPreferredSize(new Dimension(60, 30));
            manager.add(cancelButton);
            manager.add(helpButton);
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

                sourceFileNameList = SRBUtility.converToFileNameList(sourceFilesField.getText());


                if ((sourceFileNameList == null) || (sourceFileNameList.size() == 0)) {
                    return;
                }

                targetDir = targetFilesField.getText();


                if ((targetDir == null) || (targetDir.length() == 0)) {
                    return;
                }

                // Have to set up the source file system and target file system.
                if (isFileSchema(sourceSchemaComboBox)) {
                    sourceFileSystem = new LocalFileSystem();
                } else {
                    sourceFileSystem = JDialogLoginSRB.srbFileSystem;
                }

                if (isFileSchema(targetSchemaComboBox)) {
                    targetFileSystem = new LocalFileSystem();
                } else {
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
                        sourceFilesField.setText(SRBUtility.convertToString(files));
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

                        sourceFilesField.setText(SRBUtility.convertToString(files));
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
                        targetFilesField.setText(SRBUtility.convertToString(files));
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
            } else if (command.equals("Help")) {
                MipavUtil.showHelp("20010");
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
}
