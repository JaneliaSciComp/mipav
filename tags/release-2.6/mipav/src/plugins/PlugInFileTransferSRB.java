import gov.nih.mipav.plugins.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;
import gov.nih.mipav.view.srb.*;

import edu.sdsc.grid.io.*;
import edu.sdsc.grid.io.local.*;
import edu.sdsc.grid.io.srb.*;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.event.*;

import java.io.*;

import java.net.*;

import java.util.Vector;
import java.util.regex.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class PlugInFileTransferSRB implements PlugInFileTransfer {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final String[] schemas = { "file", "srb" };

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private GeneralFileSystem sourceFileSystem;

    /** DOCUMENT ME! */
    private GeneralFileSystem targetFileSystem;

    //~ Methods --------------------------------------------------------------------------------------------------------

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
     * DOCUMENT ME!
     *
     * @param   s                DOCUMENT ME!
     * @param   sourceSeparator  DOCUMENT ME!
     * @param   targetSeparator  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static String convertPathSeparator(String s, String sourceSeparator, String targetSeparator) {

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
     * @param   files  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
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
     * @param   files  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
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
            fileName = convertPathSeparator(fileName, File.separator, SRBFile.PATH_SEPARATOR);

            return new SRBFile((SRBFile) targetDir, fileName);
        } else if (targetDir instanceof LocalFile) {
            fileName = convertPathSeparator(fileName, SRBFile.PATH_SEPARATOR, File.separator);

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
            fileName = convertPathSeparator(fileName, SRBFile.PATH_SEPARATOR, File.separator);

            return new LocalFile(new File(fileName));
        } else if (fileSystem instanceof SRBFileSystem) {
            fileName = convertPathSeparator(fileName, File.separator, SRBFile.PATH_SEPARATOR);

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

    /**
     * @see  PlugInFileTransfer.canTransferFiles()
     */
    public boolean canTransferFiles() {
        return true;
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

        if (selectedItem.equals(schemas[0])) {
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

        if (selectedItem.equals(schemas[1])) {
            return true;
        }

        return false;
    }

    /**
     * @see  PlugInFileTransfer.transferFiles().
     */
    public void transferFiles() {
        new JDialogPickFiles(null);
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
         * @param  dialogTitle  DOCUMENT ME!
         */
        public JDialogPickFiles(String dialogTitle) {
            super(ViewUserInterface.getReference().getMainFrame(), false);

            /**
             * Creates the panel manager used to create the UI items.
             */
            PanelManager manager = new PanelManager();

            manager.getConstraints().insets = new Insets(3, 3, 3, 3);
            manager.add(WidgetFactory.buildLabel("Enter Source Files "));
            sourceSchemaComboBox = new JComboBox(schemas);
            sourceSchemaComboBox.setSelectedItem(schemas[0]);

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
            targetSchemaComboBox = new JComboBox(schemas);
            targetSchemaComboBox.setSelectedItem(schemas[1]);
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
         * **** Action Event Listener.*****
         *
         * @param  event  DOCUMENT ME!
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
                    sourceFiles = PlugInFileTransferSRB.converToFiles((GeneralFileSystem) null,
                                                                      sourceFilesField.getText());
                } else if (isSRBSchema(sourceSchemaComboBox)) {

                    if (JDialogLoginSRB.hasValidSRBFileSystem()) {
                        sourceFiles = PlugInFileTransferSRB.converToFiles(JDialogLoginSRB.srbFileSystem,
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
                    targetFiles = PlugInFileTransferSRB.converToFiles((GeneralFileSystem) null,
                                                                      targetFilesField.getText());
                } else if (isSRBSchema(targetSchemaComboBox)) {

                    if (JDialogLoginSRB.hasValidSRBFileSystem()) {
                        targetFiles = PlugInFileTransferSRB.converToFiles(JDialogLoginSRB.srbFileSystem,
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
                    PlugInFileTransferSRB.recursivelyCreateFileList(sourceFiles[i], targetFiles[0],
                                                                    sourceFiles[i].getParentFile(), sourceFileList,
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

}
