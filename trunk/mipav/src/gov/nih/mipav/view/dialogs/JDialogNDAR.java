package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.WidgetFactory;
import gov.nih.mipav.view.srb.JDialogLoginSRB;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import java.util.List;
import java.util.zip.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.*;


public class JDialogNDAR extends JDialogBase implements ActionListener, ChangeListener, ItemListener {

    /** Scrolling text area for log output */
    private WidgetFactory.ScrollTextArea logOutputArea;

    private JScrollPane listPane;

    private JButton loadGUIDsButton;

    private JTable guidTable;

    private JList sourceList;

    private JButton nextButton, previousButton, addSourceButton, removeSourceButton;

    private JTabbedPane tabbedPane;

    private JPanel guidPanel;

    private DefaultListModel sourceModel;

    private JCheckBox anonConfirmBox;

    private JTextArea privacyTextArea;

    private boolean doneAddingFiles = false;

    private static final String outputDirBase = System.getProperty("user.home") + File.separator + "mipav"
            + File.separator + "NDAR_Imaging_Submission" + File.separator;

    /** Length of the NDAR GUID */
    private static final int GUID_LENGTH = 12;

    private Hashtable<File, Boolean> multiFileTable = null;

    /** NDAR data object passed into FileWriteOptions and onto the writeXML with specific NDAR info */
    private NDARData ndarData;

    /** Static tab indices */
    private static final int TAB_MAIN = 0;

    private static final int TAB_SOURCE = 1;

    private static final int TAB_GUID = 2;

    private static final int TAB_LOG = 3;

    /** GUID table column indices */
    private static final int GUID_TABLE_IMAGE_COLUMN = 0;

    private static final int GUID_TABLE_GUID_COLUMN = 1;

    public JDialogNDAR(Frame theParentFrame) {
        super(theParentFrame, false);

        init();
        setVisible(true);

        validate();
    }

    public void actionPerformed(ActionEvent e) {

        /*
         * @todo Implement this java.awt.event.ActionListener abstract method
         */

        String command = e.getActionCommand();

        // System.err.println("size : " + this.getSize());

        if (command.equals("Next")) {
            int index = tabbedPane.getSelectedIndex();

            if (index == TAB_SOURCE) {
                if ( !doneAddingFiles) {
                    int response = JOptionPane.showConfirmDialog(this, "Done adding image datasets?",
                            "Done adding image datasets?", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

                    if (response == JOptionPane.YES_OPTION) {
                        doneAddingFiles = true;
                        generateGUIFields();
                        tabbedPane.setEnabledAt(TAB_GUID, true);
                        tabbedPane.setSelectedIndex(TAB_GUID);
                    }
                } else {
                    tabbedPane.setSelectedIndex(TAB_GUID);
                }
            } else if (index == TAB_GUID) {
                if (checkGUIDs()) {
                    tabbedPane.setEnabledAt(TAB_MAIN, true);
                    tabbedPane.setEnabledAt(TAB_SOURCE, true);
                    tabbedPane.setEnabledAt(TAB_LOG, true);

                    // move to TAB_LOG
                    tabbedPane.setSelectedIndex(index + 1);

                    nextButton.setText("Close");
                    nextButton.setEnabled(false);
                    previousButton.setEnabled(false);
                    tabbedPane.setEnabledAt(TAB_MAIN, false);
                    tabbedPane.setEnabledAt(TAB_SOURCE, false);
                    tabbedPane.setEnabledAt(TAB_GUID, false);
                    tabbedPane.setEnabledAt(TAB_LOG, true);

                    final gov.nih.mipav.SwingWorker worker = new gov.nih.mipav.SwingWorker() {
                        public Object construct() {
                            createSubmissionFiles();

                            return null;
                        }
                    };

                    worker.start();
                }
            } else if (index == TAB_LOG) {
                dispose();
            } else if (tabbedPane.getTabCount() > index + 1) {
                tabbedPane.setSelectedIndex(index + 1);
            }

        } else if (command.equals("Previous")) {
            int index = tabbedPane.getSelectedIndex();

            if (index == TAB_GUID) {
                // if (checkGUIDs()) {
                // tabbedPane.setEnabledAt(TAB_LOG, true);
                // }

                tabbedPane.setEnabledAt(TAB_MAIN, true);
                tabbedPane.setEnabledAt(TAB_SOURCE, true);
                tabbedPane.setSelectedIndex(index - 1);
            } else if (index > 0) {
                tabbedPane.setSelectedIndex(index - 1);
            }
        } else if (command.equals("AddSource")) {
            ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, false);
            fileChooser.setMulti(ViewUserInterface.getReference().getLastStackFlag());

            JFileChooser chooser = fileChooser.getFileChooser();
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));

            // default to TECH filter
            int filter = ViewImageFileFilter.TECH;

            try {
                filter = Integer.parseInt(Preferences.getProperty(Preferences.PREF_FILENAME_FILTER));
            } catch (NumberFormatException nfe) {

                // an invalid value was set in preferences -- so don't use it!
                filter = -1;
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
                javax.swing.filechooser.FileFilter found = ViewOpenFileUI.findFilter(chooser, filter);

                if (found != null) {
                    chooser.removeChoosableFileFilter(found);
                }

                // initially set to the preferences
                chooser.setFileFilter(new ViewImageFileFilter(filter));
            }

            int returnVal = chooser.showOpenDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                boolean isMultiFile = fileChooser.isMulti();

                File[] files = chooser.getSelectedFiles();
                ViewUserInterface.getReference().setDefaultDirectory(files[0].getParent());
                for (int i = 0; i < files.length; i++) {
                    if ( !sourceModel.contains(files[i])) {
                        sourceModel.addElement(files[i]);
                        multiFileTable.put(files[i], new Boolean(isMultiFile));
                    }
                }
            }
            removeSourceButton.setEnabled(sourceModel.size() > 0);
            nextButton.setEnabled(sourceModel.size() > 0);

            listPane.setBorder(buildTitledBorder(sourceModel.size() + " image(s) selected"));

        } else if (command.equals("RemoveSource")) {
            int[] selected = sourceList.getSelectedIndices();
            for (int i = selected.length - 1; i >= 0; i--) {
                sourceModel.removeElementAt(selected[i]);
                multiFileTable.remove(selected[i]);
            }
            removeSourceButton.setEnabled(sourceModel.size() > 0);
            nextButton.setEnabled(sourceModel.size() > 0);
            listPane.setBorder(buildTitledBorder(sourceModel.size() + " image(s) selected"));
        } else if (command.equals("Source")) {
            tabbedPane.setSelectedIndex(TAB_SOURCE);
        } else if (command.equals("LoadGUIDs")) {
            JFileChooser chooser = new JFileChooser();
            chooser.setMultiSelectionEnabled(false);

            chooser.setFont(MipavUtil.defaultMenuFont);

            int returnVal = chooser.showOpenDialog(null);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                loadGUIDsFromFile(chooser.getSelectedFile());

            }
        } else if (command.equals("Help")) {
            switch (tabbedPane.getSelectedIndex()) {
                case TAB_MAIN:
                    MipavUtil.showHelp("ISPMain01");
                    break;
                case TAB_SOURCE:
                    MipavUtil.showHelp("ISPImages01");
                    break;
                case TAB_GUID:
                    MipavUtil.showHelp("ISPGUID01");
                    break;
                case TAB_LOG:
                    MipavUtil.showHelp("ISPLog01");
                    break;
            }
        }
    }

    public void stateChanged(ChangeEvent e) {
        int index = tabbedPane.getSelectedIndex();
        if (index == TAB_MAIN) {
            previousButton.setEnabled(false);
        } else {
            previousButton.setEnabled(true);
        }

        if (index == TAB_SOURCE) {
            nextButton.setEnabled(sourceModel.size() > 0);
        } else if (index == TAB_GUID) {
            previousButton.setEnabled(true);
            nextButton.setEnabled(true);
            tabbedPane.setEnabledAt(TAB_MAIN, false);
            tabbedPane.setEnabledAt(TAB_SOURCE, false);
            tabbedPane.setEnabledAt(TAB_LOG, false);
        } else {
            nextButton.setEnabled(true);
        }

        addSourceButton.setEnabled(tabbedPane.getSelectedIndex() == TAB_SOURCE && !doneAddingFiles);
        removeSourceButton.setEnabled(tabbedPane.getSelectedIndex() == TAB_SOURCE && sourceModel.size() > 0
                && !doneAddingFiles);
        loadGUIDsButton.setEnabled(tabbedPane.getSelectedIndex() == TAB_GUID);
    }

    public void itemStateChanged(ItemEvent e) {
        if (e.getSource().equals(anonConfirmBox)) {
            if (anonConfirmBox.isSelected()) {
                anonConfirmBox.setEnabled(false);
                nextButton.setEnabled(true);

                tabbedPane.setEnabledAt(TAB_SOURCE, true);
                privacyTextArea.setBackground(helpButton.getBackground());
            }
        }
    }

    private void loadGUIDsFromFile(File guidFile) {
        RandomAccessFile raFile;
        try {
            raFile = new RandomAccessFile(guidFile, "r");
            String tempStr = null;
            String validGUID = null;
            int counter = 0;
            do {

                tempStr = raFile.readLine();
                if (tempStr != null) {
                    validGUID = getValidGUID(tempStr);
                    if (validGUID != null) {
                        guidTable.getModel().setValueAt(validGUID, counter, GUID_TABLE_GUID_COLUMN);
                    }
                }
                counter++;
            } while (tempStr != null);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void init() {
        setTitle("NDAR Image Submission Package Creation Tool");

        multiFileTable = new Hashtable<File, Boolean>();

        tabbedPane = new JTabbedPane();
        tabbedPane.addTab("Main", buildMainTab());
        tabbedPane.addTab("Images", buildSourcePanel());
        tabbedPane.addTab("GUIDs", buildGUIDPane());
        tabbedPane.addTab("Log", buildLogTab());

        tabbedPane.setEnabledAt(TAB_SOURCE, false);
        tabbedPane.setEnabledAt(TAB_GUID, false);
        tabbedPane.setEnabledAt(TAB_LOG, false);

        tabbedPane.addChangeListener(this);

        getContentPane().add(tabbedPane);
        getContentPane().add(buildButtonPanel(), BorderLayout.SOUTH);
        pack();
        validate();
        this.setMinimumSize(new Dimension(610, 437));
        this.setSize(new Dimension(610, 437));
    }

    private JScrollPane buildMainTab() {
        JPanel mainPanel = new JPanel(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;

        privacyTextArea = new JTextArea();
        privacyTextArea.setFont(MipavUtil.font12);
        privacyTextArea.setText(JDialogLoginSRB.NDAR_PRIVACY_NOTICE);
        privacyTextArea.setEditable(false);

        mainPanel.add(privacyTextArea, gbc);

        gbc.gridy++;
        gbc.gridx = 0;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.BOTH;
        anonConfirmBox = WidgetFactory.buildCheckBox("I agree to the above statement", false);
        anonConfirmBox.addItemListener(this);

        mainPanel.add(anonConfirmBox, gbc);

        JScrollPane privacyPane = WidgetFactory.buildScrollPane(mainPanel);

        return privacyPane;
    }

    /**
     * Build a panel for the zip and metadata file creation log.
     */
    private JPanel buildLogTab() {
        JPanel destPanel = new JPanel(new GridBagLayout());

        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.NORTHWEST;
        gbc2.fill = GridBagConstraints.BOTH;
        gbc2.weightx = 1;
        gbc2.weighty = 1;
        gbc2.gridy = 0;
        gbc2.gridx = 0;

        logOutputArea = WidgetFactory.buildScrollTextArea(Color.white);
        logOutputArea.setBorder(buildTitledBorder("Output log"));
        logOutputArea.getTextArea().setEditable(false);

        destPanel.add(logOutputArea, gbc2);

        return destPanel;
    }

    private JScrollPane buildSourcePanel() {
        JPanel sourcePanel = new JPanel();
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.BOTH;

        sourceModel = new DefaultListModel();
        sourceList = new JList(sourceModel);

        listPane = WidgetFactory.buildScrollPane(sourceList);
        listPane.setBorder(buildTitledBorder(0 + " image(s) selected"));
        sourcePanel.add(listPane, gbc);

        return listPane;
    }

    private JScrollPane buildGUIDPane() {
        guidPanel = new JPanel(new GridLayout());
        JScrollPane guidPane = WidgetFactory.buildScrollPane(guidPanel);

        return guidPane;
    }

    private void generateGUIFields() {
        int numImages = sourceModel.size();

        GUIDTableModel guidTableModel = new GUIDTableModel(numImages);
        guidTable = new JTable(guidTableModel);

        JScrollPane guidScrollPane = new JScrollPane(guidTable);

        guidTable.getTableHeader().setReorderingAllowed(false);
        guidTable.getTableHeader().setResizingAllowed(true);

        String longestValue = new String();

        for (int i = 0; i < numImages; i++) {
            guidTable.getModel().setValueAt(sourceModel.elementAt(i).toString(), i, GUID_TABLE_IMAGE_COLUMN);

            if (sourceModel.elementAt(i).toString().length() > longestValue.length()) {
                longestValue = sourceModel.elementAt(i).toString();
            }

            String guidString = getValidGUID(sourceModel.elementAt(i).toString());
            if (guidString != null) {
                guidTable.getModel().setValueAt(guidString, i, GUID_TABLE_GUID_COLUMN);
            }
        }

        // set the file name column width based on the longest file path
        TableCellRenderer headerRenderer = guidTable.getTableHeader().getDefaultRenderer();
        TableColumn column = guidTable.getColumnModel().getColumn(GUID_TABLE_IMAGE_COLUMN);
        Component comp = headerRenderer
                .getTableCellRendererComponent(null, column.getHeaderValue(), false, false, 0, 0);
        int headerWidth = comp.getPreferredSize().width;
        comp = guidTable.getDefaultRenderer(guidTableModel.getColumnClass(GUID_TABLE_IMAGE_COLUMN))
                .getTableCellRendererComponent(guidTable, longestValue, false, false, 0, GUID_TABLE_IMAGE_COLUMN);
        int cellWidth = comp.getPreferredSize().width;
        column.setPreferredWidth(Math.max(headerWidth, cellWidth));

        // the guids are of a fixed length
        guidTable.getColumnModel().getColumn(GUID_TABLE_GUID_COLUMN).setPreferredWidth(50);

        guidPanel.setBorder(buildTitledBorder("Assign a GUID to each image dataset"));
        guidPanel.add(guidScrollPane);
    }

    private String getValidGUID(String testString) {
        String validGUID = null;
        int ndarIndex = testString.indexOf("NDAR");
        if (ndarIndex != -1 && (ndarIndex + GUID_LENGTH < testString.length())) {

            validGUID = testString.substring(ndarIndex, ndarIndex + GUID_LENGTH);
            if (isValidGUID(validGUID)) {
                return validGUID;
            }
        }
        validGUID = null;
        return validGUID;
    }

    /**
     * Checks to see if the given string is a valid NDAR GUID
     * 
     * @param checkString the string to check
     * @return whether this is a valid guid
     */
    private boolean isValidGUID(String checkString) {
        if (checkString.length() != GUID_LENGTH) {
            return false;
        }

        if (isValidChar(checkString.charAt(4)) && isValidChar(checkString.charAt(5))
                && isNumChar(checkString.charAt(6)) && isNumChar(checkString.charAt(7))
                && isNumChar(checkString.charAt(8)) && isValidChar(checkString.charAt(9))
                && isValidChar(checkString.charAt(10))
                && (isNumChar(checkString.charAt(11)) || isValidChar(checkString.charAt(11)))) {
            return true;
        }
        return false;
    }

    /**
     * Is the char a valid number character
     * 
     * @param checkChar char to check
     * @return whether is a number
     */
    private boolean isNumChar(char checkChar) {
        return (checkChar >= '0' && checkChar <= '9');
    }

    /**
     * Check if this is a valid NDAR character ( no I, O, Q, or S)
     * 
     * @param checkChar char to check
     * @return is the char valid
     */
    private boolean isValidChar(char checkChar) {
        if ( (checkChar >= 'a' && checkChar <= 'z') || (checkChar >= 'A' && checkChar <= 'Z')) {
            if (checkChar != 'i' && checkChar != 'I' && checkChar != 'o' && checkChar != 'O' && checkChar != 'q'
                    && checkChar != 'Q' && checkChar != 's' && checkChar != 'S') {
                return true;
            }
        }

        return false;
    }

    /**
     * Create the ZIP(s) containing the original image files and the XML meta-data for each image dataset.
     */
    private void createSubmissionFiles() {
        if ( !new File(outputDirBase).exists()) {
            new File(outputDirBase).mkdirs();
        }

        ndarData = new NDARData();

        int numImages = sourceModel.size();
        for (int i = 0; i < numImages; i++) {
            String outputFileNameBase = System.getProperty("user.name") + "_" + System.currentTimeMillis();

            File imageFile = (File) sourceModel.elementAt(i);

            printlnToLog("Opening: " + imageFile + ", multifile: " + multiFileTable.get(imageFile));

            // ViewJFrameImage invisFrame = new ViewJFrameImage(tempImage);

            FileIO fileIO = new FileIO();
            fileIO.setQuiet(true);
            ModelImage origImage = fileIO.readImage(imageFile.getName(), imageFile.getParent() + File.separator,
                    multiFileTable.get(imageFile), null);

            List<String> origFiles = FileUtility.getFileNameList(origImage);

            String zipFilePath = outputDirBase + outputFileNameBase + ".zip";
            try {
                printlnToLog("Creating ZIP file:\t" + zipFilePath);
                for (String file : origFiles) {
                    printlnToLog("Adding file to ZIP:\t" + file);
                }

                makeZipFile(zipFilePath, origFiles);
            } catch (IOException ioe) {
                ioe.printStackTrace();
                MipavUtil.displayError("Unable to write original image dataset files to ZIP package:\n"
                        + ioe.getMessage());
                continue;
            }

            // add the name of the zip file, so that it can be included in the XML header History tag
            ndarData.zipFileName = FileUtility.getFileName(zipFilePath);

            // set the valid GUID into the NDAR data object
            ndarData.validGUID = (String) guidTable.getModel().getValueAt(i, GUID_TABLE_GUID_COLUMN);

            writeMetaDataFiles(outputDirBase, outputFileNameBase, imageFile, origImage);

            origImage.disposeLocal();

            printlnToLog("");
        }

        printlnToLog("*** Submission package processing complete. ***");

        nextButton.setEnabled(true);
        previousButton.setEnabled(false);
    }

    /**
     * Writes out the XML meta-information for a given image dataset.
     * 
     * @param outputDir Where to write the XML header.
     * @param outputFileNameBase The prefix to put on the XML header file name.
     * @param imageFile The main image file to use to read in the dataset.
     */
    private void writeMetaDataFiles(String outputDir, String outputFileNameBase, File imageFile, ModelImage image) {
        // Create the FileIO
        FileIO fileIO = new FileIO();

        // if the dicomsave.dictionary doesn't exist, all the
        if (image.getFileInfo(0) instanceof FileInfoDicom && !DicomDictionary.doesSubsetDicomTagTableExist()) {
            fileIO.setQuiet(false);
        } else {
            fileIO.setQuiet(true);
        }

        FileWriteOptions options = new FileWriteOptions(true);
        String fName;

        options.setMultiFile(multiFileTable.get(imageFile));
        options.setWriteHeaderOnly(true);
        options.setNDARData(ndarData);

        // get the image file name and add .xml to it (maintain the previous extension in the name)
        fName = outputFileNameBase + "_" + image.getImageFileName();
        if ( !fName.endsWith(".xml")) {
            fName += ".xml";
        }

        if (image.getNDims() > 2) {
            options.setBeginSlice(0);
            options.setEndSlice(image.getExtents()[2] - 1);
        }
        if (image.getNDims() > 3) {
            options.setBeginSlice(0);
            options.setEndTime(image.getExtents()[3] - 1);
        }
        options.setFileDirectory(outputDir);
        options.setFileName(fName);
        options.setFileType(FileUtility.XML);
        options.doPutInQuicklist(false);
        options.setMultiFile(false);
        options.setOptionsSet(true);

        printlnToLog("Saving XML header: " + fName + " to: " + outputDir);

        // write out only the header to userdir/mipav/temp
        fileIO.writeImage(image, options);
    }

    /**
     * Adds a set of files to a ZIP archive.
     * 
     * @param destZipFile The full path to the ZIP archive to create.
     * @param srcFiles A list of files (full paths) to include in the ZIP archive.
     * @throws IOException If there is a problem reading the srcFiles or writing to the ZIP file.
     */
    private void makeZipFile(String destZipFile, List<String> srcFiles) throws IOException {
        // Create a buffer for reading the files
        byte[] buf = new byte[1024];

        // Create the ZIP file
        ZipOutputStream out = new ZipOutputStream(new FileOutputStream(destZipFile));

        // Compress the files
        for (String file : srcFiles) {
            FileInputStream in = new FileInputStream(file);

            // Add ZIP entry to output stream.
            out.putNextEntry(new ZipEntry(FileUtility.getFileName(file)));

            // Transfer bytes from the file to the ZIP file
            int len;
            while ( (len = in.read(buf)) > 0) {
                out.write(buf, 0, len);
            }

            // Complete the entry
            out.closeEntry();
            in.close();
        }

        // Complete the ZIP file
        out.close();
    }

    /**
     * Append a line to the log output area in the Log tab.
     * 
     * @param line The line to append (do not include the trailing newline).
     */
    private void printlnToLog(String line) {
        logOutputArea.getTextArea().append(line + "\n");
    }

    /**
     * Parses each JTextField for GUIDs... highlights first bad field found (if present) and returns false, otherwise
     * returns true if all fields valid
     * 
     * @return whether the GUIDs are all valid
     */
    private boolean checkGUIDs() {
        int numImages = sourceModel.size();
        for (int i = 0; i < numImages; i++) {
            if ( !isValidGUID((String) guidTable.getModel().getValueAt(i, GUID_TABLE_GUID_COLUMN))) {
                MipavUtil.displayWarning("Invalid GUID: " + guidTable.getModel().getValueAt(i, GUID_TABLE_GUID_COLUMN));
                return false;
            }
        }
        return true;
    }

    private JPanel buildButtonPanel() {
        JPanel buttonPanel = new JPanel();
        previousButton = WidgetFactory.buildTextButton("Previous", "Go to previous tab", "Previous", this);
        nextButton = WidgetFactory.buildTextButton("Next", "Go to next tab", "Next", this);
        addSourceButton = WidgetFactory.buildTextButton("Add images", "Add image datasets", "AddSource", this);
        removeSourceButton = WidgetFactory.buildTextButton("Remove images", "Remove the selected image datasets",
                "RemoveSource", this);
        loadGUIDsButton = WidgetFactory.buildTextButton("Load GUIDs", "Parse GUIDs from text file", "LoadGUIDs", this);
        helpButton = WidgetFactory.buildTextButton("Help", "Show MIPAV help", "", this);

        previousButton.setPreferredSize(MipavUtil.defaultButtonSize);
        nextButton.setPreferredSize(MipavUtil.defaultButtonSize);
        addSourceButton.setPreferredSize(MipavUtil.defaultButtonSize);
        removeSourceButton.setPreferredSize(MipavUtil.defaultButtonSize);
        loadGUIDsButton.setPreferredSize(MipavUtil.defaultButtonSize);
        helpButton.setPreferredSize(MipavUtil.defaultButtonSize);

        previousButton.setEnabled(false);
        nextButton.setEnabled(false);
        addSourceButton.setEnabled(false);
        removeSourceButton.setEnabled(false);
        loadGUIDsButton.setEnabled(false);

        buttonPanel.add(previousButton);
        buttonPanel.add(nextButton);
        buttonPanel.add(addSourceButton);
        buttonPanel.add(removeSourceButton);
        buttonPanel.add(loadGUIDsButton);
        buttonPanel.add(helpButton);

        return buttonPanel;
    }

    public class NDARData {
        public String validGUID;

        public String zipFileName;
    }

    private class GUIDTableModel extends AbstractTableModel {
        private String[] columnNames = {"Image", "GUID"};

        private Object[][] data;

        public GUIDTableModel(int numGUIDs) {
            super();

            data = new Object[numGUIDs][columnNames.length];

            for (int i = 0; i < numGUIDs; i++) {
                data[i][GUID_TABLE_IMAGE_COLUMN] = new String();
                data[i][GUID_TABLE_GUID_COLUMN] = new String();
            }
        }

        public int getColumnCount() {
            return columnNames.length;
        }

        public int getRowCount() {
            return data.length;
        }

        public String getColumnName(int col) {
            return columnNames[col];
        }

        public Object getValueAt(int row, int col) {
            return data[row][col];
        }

        public boolean isCellEditable(int row, int col) {
            // Note that the data/cell address is constant,
            // no matter where the cell appears onscreen.
            if (col == 0) {
                return false;
            } else {
                return true;
            }
        }

        public void setValueAt(Object value, int row, int col) {
            data[row][col] = value;
            fireTableCellUpdated(row, col);
        }
    }
}
