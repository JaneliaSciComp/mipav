package gov.nih.mipav.view;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileDicomTagInfo.VR;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.JDialogSelectDICOMColumnHeaders;
import gov.nih.mipav.view.renderer.JDialogRendererAVI;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.text.NumberFormat;
import java.util.*;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;


/**
 * DICOM parser - shows pertinent DICOM tags, allows user to sort on specific values in the tags and open the proper
 * images. Can also open the images to an AVI file. Can see image as thumbnail. Extends ViewImageDirectory for the tree
 * and thumbnail.
 * 
 * @author Neva Cherniavsky
 * @see FileInfoDicom
 */
public class ViewJFrameDICOMParser extends ViewImageDirectory implements WindowListener {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1085995416072088026L;

    /** DOCUMENT ME! */
    public static final String[] DEFAULT_COLUMN_HEADERS_IMAGE_TABLE = new String[] {"Instance (formerly Image) Number",
            "Acquisition Time", "X-position", "Y-position", "Z-position"};

    /** DOCUMENT ME! */
    public static final String[] DEFAULT_COLUMN_HEADERS_STUDY_TABLE = new String[] {"Patient Name", "Patient ID",
            "Study ID", "Study Date", "Description", ""};

    /** DOCUMENT ME! */
    public static final String[] DEFAULT_COLUMN_HEADERS_SERIES_TABLE = new String[] {"Series", "Type", "# Images",
            "Time", "Mod", "Description", "", "StudyID"};

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int brightness = 0;

    /** DOCUMENT ME! */
    private JPanel brightnessContrastPanel;

    /** DOCUMENT ME! */
    private JSlider brightnessSlider, contrastSlider;

    /** DOCUMENT ME! */
    private Hashtable columnKeyTable = new Hashtable();

    /** DOCUMENT ME! */
    private ViewJComponentPreviewImage componentImageDicom = null;

    /** DOCUMENT ME! */
    private float contrast = 1;

    /** DOCUMENT ME! */
    private JLabel current, current2;

    /**
     * fileInfoVector represents images in image table but imageTableVector was needed also becasue this handles
     * multiple series in the same dir. fileInfoVector is all the images in the dir but imageTableVector represents all
     * the images that are in the image table at a particular instance or series
     */
    private Vector<FileInfoDicom> fileInfoVector;
    private Vector<FileInfoDicom> imageTableVector;

    /** DOCUMENT ME! */
    private JTable imageTable;

    /** DOCUMENT ME! */
    private SortingTableModel imageTableModel;

    /** DOCUMENT ME! */
    private TableSorter imageTableSorter;

    /** DOCUMENT ME! */
    private NumberFormat nfc;

    /** DOCUMENT ME! */
    private int origBrightness = 0;

    /** DOCUMENT ME! */
    private float origContrast = 1;

    /** DOCUMENT ME! */
    private JSplitPane rightPane;

    /** This is a hashmasp of series numbers and corresponding counter for the number of images in each series.* */
    private HashMap<String,Integer> seriesNumberCounters;

    /** DOCUMENT ME! */
    private JCheckBoxMenuItem seriesOptionBox;

    /** DOCUMENT ME! */
    private JTable seriesTable;

    /** DOCUMENT ME! */
    private SortingTableModel seriesTableModel;

    /** DOCUMENT ME! */
    private TableSorter seriesTableSorter;

    /** DOCUMENT ME! */
    private JTable studyTable;

    /** DOCUMENT ME! */
    private SortingTableModel studyTableModel;

    /** DOCUMENT ME! */
    private TableSorter studyTableSorter;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates new frame. Tables contain no data but the tree is initialized to the appropriate directory.
     * 
     * @param dir Directory.
     */
    public ViewJFrameDICOMParser(String dir) {
        super(dir, new ViewImageFileFilter(new String[] {".dcm", ".DCM", ".ima", ".IMA"}));

        addWindowListener(this);
        buildMenu();
        Preferences.setProperty(Preferences.PREF_IMAGE_DIR, dir);
        buildSourceTreeListing(true);
        // selects the directory and parses it
        directoryTree.setSelectionRow(0);
        actionPerformed(new ActionEvent(this, 0, "Parse"));
        treePanel.setPreferredSize(new Dimension(200, 200));
        imagePanel = new JPanel();
        imagePanel.setPreferredSize(new Dimension(200, 350));
        buildBrightnessContrastPanel();

        JPanel subPanel = new JPanel(new BorderLayout());
        subPanel.add(imagePanel, BorderLayout.CENTER);
        subPanel.add(brightnessContrastPanel, BorderLayout.SOUTH);

        // Use VERTICAL_SPLIT to create a horizontal split pane
        JSplitPane lspPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true, treePanel, subPanel);

        // This is very strange - JSplitPane.HORIZONTAL_SPLIT is used
        // to create a vertical split plane
        JSplitPane spPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, true, lspPane, rightPane);

        getContentPane().add(spPane, BorderLayout.CENTER);
        pack();
        MipavUtil.centerOnScreen(this);
        setVisible(true);
        if (studyTableModel.getRowCount() == 0) {
        	MipavUtil.displayError("No DICOM images were found in the directory");
        	return;
        }
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * The purpose of this method is to determine whether the parameter represents one of the special table columns. In
     * this case, X-position, Y-position, and Z-position are special columns because they are not standard DICOM tags.
     * They are parsed out from the Patient Orientation tag and populated manually.
     * 
     * @param columnName String
     * 
     * @return boolean
     */
    public static boolean isCompositeXYZPositionColumn(String columnName) {

        if (columnName.equals("X-position") || columnName.equals("Y-position") || columnName.equals("Z-position")) {
            return true;
        }

        return false;
    }

    /**
     * Recreates the tree when a new directory is selected; refreshes the tree when refresh is selected.
     * 
     * @param event Event that triggered this function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        this.setCursor(Cursor.getDefaultCursor());

        final ViewFileTreeNode node = (ViewFileTreeNode) directoryTree.getLastSelectedPathComponent();

        if (command.equals("Parse")) {

            if (node == null) {
                MipavUtil.displayError("You must select a directory or image to parse");

                return;
            }

            if (node.isDirectory()) {
                studyTableModel.setRowCount(0);
                seriesTableModel.setRowCount(0);
                fileInfoVector = new Vector<FileInfoDicom>();

                // first we pares the directories and populate the fileInfoVecot list
                parse(node.getFile());
                
                if(this.isVisible()) {
	                if (studyTableModel.getRowCount() == 0) {
	                	MipavUtil.displayError("No DICOM images were found in the directory");
	                	return;
	                }
                }

                // if there is 1 row in study table, populate the series table
                if (studyTableModel.getRowCount() == 1) {
                    String studyNo = (String) (studyTableModel.getValueAt(0, 2));

                    if (studyNo == null) {
                        studyNo = "";
                    }

                    // if there is just 1 row in the study table, lets select it
                    studyTable.setRowSelectionInterval(0, 0);

                    // add series data
                    addSeriesData(studyNo);
                }

                // if there is just 1 row in the series table, lets select it
                if (seriesTableModel.getRowCount() == 1) {
                    seriesTable.setRowSelectionInterval(0, 0);
                }

                // if there is 1 or less rows in series table and study table, populate the image table
                if ( (seriesTableModel.getRowCount() <= 1) && (studyTableModel.getRowCount() <= 1)) {

                    // Since there is just 1 or less rows in both tables, passing in empty string params
                    // so everything gets shown when reloadRows gets called
                    reloadRows("", "");
                    imageTableSorter.fireTableDataChanged();
                }

                seriesNumberCounters = null;
            }
        } else if (command.equals("New")) {

            ViewDirectoryChooser chooser = new ViewDirectoryChooser(this);
            String dir = chooser.getImageDirectory();

            if (dir != null) {
                studyTableModel.removeAllRows();
                seriesTableModel.removeAllRows();
                imageTableModel.removeAllRows();
                imagePanel.removeAll();
                imagePanel.repaint();
                Preferences.setProperty(Preferences.PREF_IMAGE_DIR, dir);
                directory = dir;
                treePanel.removeAll();
                buildSourceTreeListing(true);
                validate();
            }
        } else if (command.equals("Open")) {
            FileInfoDicom fileInfoDICOM = null;
            int[] rows = imageTable.getSelectedRows();
            String[] fileNames = new String[rows.length];

            if (rows.length == 0) {
                MipavUtil.displayError("You must select an image to open.");

                return;
            }

            for (int i = 0; i < rows.length; i++) {
                int modelRow = imageTableSorter.modelIndex(rows[i]);

                fileInfoDICOM = imageTableVector.elementAt(modelRow);

                fileNames[i] = fileInfoDICOM.getFileName();
            }

            FileIO io = new FileIO();
            io.setQuiet(true);
            io.setFileDir(fileInfoDICOM.getFileDirectory() + File.separatorChar);

            ModelImage image = io.readDicom(fileNames[0], fileNames, false);

            if (image == null) {
                return;
            }

            image.calcMinMax();
            new ViewJFrameImage(image, io.getModelLUT());

        } else if (command.equals("SeriesOption")) {

            
        } else if (command.equals("Exit")) {
            this.close();
        } else if (command.equals("Movie")) {
            int[] selectedRows = imageTable.getSelectedRows();
            String[] fileNames = new String[selectedRows.length];
            String fileName;
            String directory;
            double frameRate = 21.0;
            int microSecondsPerFrame;
            boolean applyWindowLevel = true;
            FileInfoBase[] fileInfo;

            if (selectedRows.length < 2) {
                MipavUtil.displayError("You must open at least 2 images for a 3D AVI file.");

                return;
            }

            JDialogRendererAVI aviDialog = new JDialogRendererAVI(this);

            if (aviDialog.isCancelled()) {
                return;
            } else {
                frameRate = aviDialog.getFrameRate();
                microSecondsPerFrame = (int) Math.round(1000000 / frameRate);
                applyWindowLevel = aviDialog.getApplyWindowLevel();
            }

            JFileChooser chooser = new JFileChooser();

            if (userInterface.getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(userInterface.getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {".avi"}));

            int returnVal = chooser.showSaveDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                userInterface.setDefaultDirectory(directory);
            } else {
                return;
            }

            FileInfoDicom fileInfoDICOM = null;

            for (int i = 0; i < fileNames.length; i++) {
                int modelRow = imageTableSorter.modelIndex(selectedRows[i]);

                fileInfoDICOM = imageTableVector.elementAt(modelRow);

                fileNames[i] = fileInfoDICOM.getFileName();
            }

            FileIO io = new FileIO();
            io.setQuiet(true);
            io.setFileDir(fileInfoDICOM.getFileDirectory() + File.separatorChar);

            ModelImage image = io.readDicom(fileNames[0], fileNames, false);

            if (image == null) {
                return;
            }

            image.calcMinMax();

            ModelLUT LUT = null;

            if (image.isColorImage() == false) {
                int[] dimExtentsLUT = new int[2];

                dimExtentsLUT[0] = 4;
                dimExtentsLUT[1] = 256;

                LUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);

                float min, max;

                if (image.getType() == ModelStorageBase.UBYTE) {
                    min = 0;
                    max = 255;
                } else if (image.getType() == ModelStorageBase.BYTE) {
                    min = -128;
                    max = 127;
                } else {
                    min = (float) image.getMin();
                    max = (float) image.getMax();
                }

                float imgMin = (float) image.getMin();
                float imgMax = (float) image.getMax();

                LUT.resetTransferLine(min, imgMin, max, imgMax);
                LUT.makeLUT(256);

                if (applyWindowLevel) {
                    float[] x = new float[4];
                    float[] y = new float[4];
                    float level;
                    float window;
                    x[0] = min;
                    y[0] = dimExtentsLUT[1] - 1;
                    x[3] = max;
                    y[3] = 0;

                    if (contrast == 0) {
                        contrast = 1;
                    }

                    // Use (max-min)/255 because the slider is only goes from -255 to 255
                    // and need to take all values down to min or up to max
                    level = ( (min + max) / 2.0f) - (brightness * (max - min) / 255);
                    window = (max - min) * contrast / 2.0f;

                    x[2] = level + (window / 2);

                    if (x[2] > max) {
                        y[2] = 255.0f * (x[2] - max) / window;
                        x[2] = max;
                    } else {
                        y[2] = 0.0f;
                    }

                    x[1] = level - (window / 2);

                    if (x[1] < min) {
                        y[1] = 255.0f - (255.0f * (min - x[1]) / window);
                        x[1] = min;
                    } else {
                        y[1] = 255.0f;
                    }

                    LUT.getTransferFunction().importArrays(x, y, 4);
                } // if (applyWindowLevel)
            }

            fileInfo = image.getFileInfo();

            for (int i = 0; i < fileInfo.length; i++) {
                image.getFileInfo()[i].setUnitsOfMeasure(Unit.MICROSEC.getLegacyNum(), 2);
                image.getFileInfo()[i].setResolutions(microSecondsPerFrame, 2);
            }

            try {
                FileAvi aviFile = new FileAvi(fileName, directory);

                aviFile.writeImage(image, null, LUT, null, null, null, 0, 0, 0, .3f, .5f, new BitSet(), -1);
            } catch (IOException error) {
                MipavUtil.displayError("I/O Exception while writing AVI file.");
                image.disposeLocal();
                image = null;

                return;
            }

            image.disposeLocal();
            image = null;
        } else if (command.equals("Configure DICOM table")) {
            new JDialogSelectDICOMColumnHeaders(this);
        } else if (command.equals("Select all rows")) {

            if (imageTableSorter.getTableModel().getRowCount() > 0) {
                imageTable.setRowSelectionInterval(0, imageTableSorter.getRowCount() - 1);
            }
        }
    }

    /**
     * Cleans memory.
     */
    public void finalize() {

        for (int i = 0; i < seriesTableModel.getRowCount(); i++) {
            FileInfoDicom fileInfo = (FileInfoDicom) seriesTableModel.getValueAt(i, 6);

            if (fileInfo != null) {
                fileInfo.finalize();
                fileInfo = null;
            }
        }

        for (int i = 0; i < studyTableModel.getRowCount(); i++) {
            FileInfoDicom fileInfo = (FileInfoDicom) studyTableModel.getValueAt(i, 5);

            if (fileInfo != null) {
                fileInfo.finalize();
                fileInfo = null;
            }
        }

        if (fileInfoVector != null) {

            for (int i = 0; i < fileInfoVector.size(); i++) {
                FileInfoDicom fileInfo = fileInfoVector.elementAt(i);

                if (fileInfo != null) {
                    fileInfo.finalize();
                    fileInfo = null;
                }
            }

            fileInfoVector = null;
        }

        columnKeyTable = null;

        if (componentImageDicom != null) {
            componentImageDicom.dispose(false);
        }

        componentImageDicom = null;
        imageTableModel = null;
        seriesTableModel = null;
        studyTableModel = null;

        imageTableSorter = null;
        seriesTableSorter = null;
        studyTableSorter = null;

        if (imageTable != null) {
            imageTable.removeAll();
        }

        imageTable = null;

        if (seriesTable != null) {
            seriesTable.removeAll();
        }

        seriesTable = null;

        if (studyTable != null) {
            studyTable.removeAll();
        }

        studyTable = null;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public Vector<String> getColumnNames() {
        Vector<String> columnNames = new Vector<String>();

        for (int i = 0; i < imageTableModel.getColumnCount(); i++) {
            columnNames.addElement(imageTable.getColumnName(i));
        }

        return columnNames;
    }

    public ViewJComponentPreviewImage getComponentImageDicom() {
        return componentImageDicom;
    }

    public Vector<FileInfoDicom> getFileInfoVector() {
        return fileInfoVector;
    }

    public Vector<FileInfoDicom> getImageTableVector() {
        return imageTableVector;
    }

    /**
     * reloadRows.
     */
    public void reloadRows() {
        reloadRows("", "");
    }

    /**
     * The purpose of this method is to re-parse the DICOM files to refresh the table data. It is called after the user
     * hits "apply" in the configuration dialog, or the user clicks "parse" in the toolbar.
     * 
     * @param seriesNumber DOCUMENT ME!
     * @param studyNo DOCUMENT ME!
     */
    @SuppressWarnings("unchecked")
    public void reloadRows(String seriesNumber, String studyNo) {
        ViewFileTreeNode node = (ViewFileTreeNode) directoryTree.getLastSelectedPathComponent();

        if ( (node != null) && (fileInfoVector != null)) // fileInfoVector is the Vector of DICOM headers associated
        // with the selected file
        {

            try {
                Vector<String> tableHeaderVector = imageTableModel.getColumnNames(); // Vector holding the names of the
                // columns

                int mod = (fileInfoVector.size() / 100);

                // System.err.println("mod is: " + mod);
                if (mod == 0) {
                    mod = 1;
                }

                imageTableVector = new Vector<FileInfoDicom>();

                for (int i = 0; i < fileInfoVector.size(); i++) {
                    FileInfoDicom fileInfoDICOM = fileInfoVector.elementAt(i);
                    String sliceStudyNo = (String) fileInfoDICOM.getTagTable().getValue("0020,0010");
                    if(sliceStudyNo.length() > 0) {
	                    char c = sliceStudyNo.charAt(sliceStudyNo.length() - 1);
	                	if(c == '\0') {
	                		sliceStudyNo = sliceStudyNo.substring(0, sliceStudyNo.indexOf(c));
	                	}
                    }
                    if (sliceStudyNo == null) {
                        sliceStudyNo = "";
                    }

                    if ( ( (seriesNumber == null) || seriesNumber.equals("") || (seriesNumberEqual(seriesNumber,
                            fileInfoDICOM) == true))
                            && ( (studyNo.equals("")) || (sliceStudyNo.equals(studyNo)))) {

                        Vector<Object> newRow = new Vector<Object>();

                        imageTableVector.addElement(fileInfoVector.elementAt(i));

                        // tableHeader.size() is the number of columns in the table
                        for (int j = 0; j < tableHeaderVector.size(); j++) {
                            String columnName = tableHeaderVector.elementAt(j); // get name of first column
                            String key = (String) columnKeyTable.get(columnName); // get key associated with this
                            // column

                            if (isCompositeXYZPositionColumn(columnName)) // test to see if we have to populate this
                            // column manually i.e. non-native DICOM tag
                            {
                                key = JDialogSelectDICOMColumnHeaders.CUSTOM; // assign CUSTOM to key to indicate it
                                // is
                                // non-native DICOM tag
                            } else if (key == null) {
                                key = DicomDictionary.getKeyFromTagName(columnName); // might return null
                            }

                            if ( (key != null) && columnName.equals("Instance (formerly Image) Number")) {
                                String instanceNumber = (String) fileInfoDICOM.getTagTable().getValue(key);
                                if(instanceNumber.length() > 0) {
            	                    char c = instanceNumber.charAt(instanceNumber.length() - 1);
            	                	if(c == '\0') {
            	                		instanceNumber = instanceNumber.substring(0, instanceNumber.indexOf(c));
            	                	}
                                }
                                try {
                                    Integer integer = new Integer(Integer.parseInt(instanceNumber.trim()));
                                    newRow.addElement(integer);
                                } catch (Exception e) {
                                    newRow.addElement(instanceNumber);
                                }

                                imageTableModel.setColumnClass(new Integer(0).getClass(), j);
                            } else if ( (key != null) && key.equals(JDialogSelectDICOMColumnHeaders.CUSTOM)) {

                                /*
                                 * This is a hack because DICOM puts all 3 patient orientation values into one value. To
                                 * account for the fact that the user will probably want to sort on these values
                                 * individually, we have to split up the x, y, z positions from patient orientation and
                                 * must manually populate these columns.
                                 */
                                if ( (tableHeaderVector.elementAt(j)).equals("X-position")) {
                                    Float floatObj = new Float(fileInfoDICOM.xLocation);
                                    imageTableModel.setColumnClass(floatObj.getClass(), j);

                                    newRow.addElement(floatObj);
                                } else if ( (tableHeaderVector.elementAt(j)).equals("Y-position")) {
                                    Float floatObj = new Float(fileInfoDICOM.yLocation);
                                    imageTableModel.setColumnClass(floatObj.getClass(), j);

                                    newRow.addElement(floatObj);
                                } else if ( (tableHeaderVector.elementAt(j)).equals("Z-position")) {
                                    Float floatObj = new Float(fileInfoDICOM.zLocation);
                                    imageTableModel.setColumnClass(floatObj.getClass(), j);

                                    newRow.addElement(floatObj);
                                } else {
                                    newRow.addElement(SortingTableModel.EMPTY_CELL);
                                }
                            }

                            /**
                             * This is the block of code that is the default case. Here, we are asking for the data
                             * based on the key's value. Since this is not a special column (i.e. X-position,
                             * Y-position, Z-position), the value should be read from the fileInfoDICOM.getValue()
                             * method.
                             */
                            else if (key != null) {
                                FileDicomTag dicomTag = fileInfoDICOM.getTagTable().get(key);

                                if (dicomTag == null) {
                                    newRow.addElement(SortingTableModel.EMPTY_CELL); // if data is not present, add
                                    // empty cell
                                } else {
                                    VR vr = dicomTag.getValueRepresentation();
                                    String value = (String) dicomTag.getValue(true);

                                    if (vr.equals(VR.SL) || vr.equals(VR.UL) || vr.equals(VR.SS)
                                            || vr.equals(VR.US) || vr.equals(VR.IS)) {

                                        /*
                                         * Try to create a Number object out of the data. This is important because the
                                         * type of data indicates to the TableSorter how to sort, i.e. lexically or
                                         * numerically.
                                         */
                                        try {
                                            Integer integer = new Integer(Integer.parseInt(value));
                                            imageTableModel.setColumnClass(integer.getClass(), j); // set numerical
                                            // sorting
                                            newRow.addElement(integer);

                                            continue;
                                        } catch (NumberFormatException nfe) {

                                            // at this point, its not a number, so assume String
                                            imageTableModel.setColumnClass(new String().getClass(), j); // set lexical
                                            // sorting
                                            newRow.addElement(value);
                                        }
                                    } else if (vr.equals(VR.FD) || vr.equals(VR.FL) || vr.equals(VR.DS)) {

                                        try {
                                            Double doubleObj = new Double(Double.parseDouble(value));
                                            imageTableModel.setColumnClass(doubleObj.getClass(), j); // set numerical
                                            // sorting
                                            newRow.addElement(doubleObj);

                                            continue;
                                        } catch (NumberFormatException nfe) {

                                            // at this point, its not a number, so assume String
                                            imageTableModel.setColumnClass(new String().getClass(), j); // set lexical
                                            // sorting
                                            newRow.addElement(value);
                                        }
                                    } else {
                                        imageTableModel.setColumnClass(new String().getClass(), j); // set lexical
                                        // sorting
                                        newRow.addElement(value);
                                    }
                                }
                            } else {
                                newRow.addElement(SortingTableModel.EMPTY_CELL); // data is null, add empty cell
                            }

                        }

                        imageTableModel.addRow(newRow); // the new row, after all column values have been added, is
                        // added to the table
                    }
                }

            } catch (Exception e) {
                e.printStackTrace();
            } finally {
                imageTableSorter.fireTableDataChanged();
            }
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param rightTable DOCUMENT ME!
     */
    public void setHeaderConfiguration(JTable rightTable) {
        imageTableModel = new SortingTableModel();
        imageTableSorter.setTableModel(imageTableModel);

        SortingTableModel rightTableModel = (SortingTableModel) ((TableSorter) rightTable.getModel()).getTableModel();

        for (int i = 0; i < rightTableModel.getRowCount(); i++) {
            Vector row = rightTableModel.getRow(i);

            columnKeyTable.put(row.elementAt(1), row.elementAt(0));

            imageTableModel.addColumn((String) row.elementAt(1));
        }

        imageTableSorter.fireTableStructureChanged();
    }

    /**
     * Sets values based on knob along slider.
     * 
     * @param e Event that triggered this function
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if (source == brightnessSlider) {
            brightness = brightnessSlider.getValue();
            current.setText(String.valueOf(brightness));

            // Change only the brightness and contrast of the current slice
            if (componentImageDicom != null) {
                componentImageDicom.setSliceBrightness(brightness, contrast);
            }
        } else if (source == contrastSlider) {
            contrast = (float) Math.pow(10.0, contrastSlider.getValue() / 200.0);
            current2.setText(String.valueOf(nfc.format(contrast)));

            // Change only the brightness and contrast of the current slice
            if (componentImageDicom != null) {
                componentImageDicom.setSliceBrightness(brightness, contrast);
            }
        }
    }

    /**
     * windowActivated - unchanged.
     * 
     * @param event DOCUMENT ME!
     */
    public void windowActivated(WindowEvent event) {}

    /**
     * windowClosed - unchanged.
     * 
     * @param event DOCUMENT ME!
     */
    public void windowClosed(WindowEvent event) {}

    /**
     * windowClosing - calls close.
     * 
     * @param event event that triggered function
     */
    public void windowClosing(WindowEvent event) {
        this.close();

        dispose();
    }

    /**
     * windowDeactivated - unchanged.
     * 
     * @param event DOCUMENT ME!
     */
    public void windowDeactivated(WindowEvent event) {}

    /**
     * windowDeiconified - unchanged.
     * 
     * @param event DOCUMENT ME!
     */
    public void windowDeiconified(WindowEvent event) {}

    /**
     * windowIconified - unchanged.
     * 
     * @param event DOCUMENT ME!
     */
    public void windowIconified(WindowEvent event) {}

    // ************************************************************************
    // **************************** Window Events *****************************
    // ************************************************************************

    /**
     * windowOpened - unchanged.
     * 
     * @param event DOCUMENT ME!
     */
    public void windowOpened(WindowEvent event) {}

    /**
     * Builds a toolbar with the same functionality as the menu.
     * 
     * @return DOCUMENT ME!
     */
    protected JPanel buildToolbar() {
        Border pressedBorder = BorderFactory.createLoweredBevelBorder();
        Border etchedBorder = BorderFactory.createEtchedBorder();

        JPanel panel = new JPanel(new BorderLayout());
        JToolBar tBar = new JToolBar();

        tBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        tBar.setBorder(etchedBorder);
        tBar.setBorderPainted(true);

        JButton openButton = new JButton(MipavUtil.getIcon("open.gif"));

        openButton.addActionListener(this);
        openButton.setToolTipText("Open selected image(s)");
        openButton.setActionCommand("Open");
        openButton.setBorderPainted(false);
        openButton.setRolloverEnabled(true);
        openButton.setRolloverIcon(MipavUtil.getIcon("openroll.gif"));
        openButton.setBorder(pressedBorder);
        openButton.addItemListener(this);
        openButton.setFocusPainted(false);
        tBar.add(openButton);

        JButton parseButton = new JButton(MipavUtil.getIcon("parse.gif"));

        parseButton.addActionListener(this);
        parseButton.setToolTipText("Parse directory");
        parseButton.setActionCommand("Parse");
        parseButton.setBorderPainted(false);
        parseButton.setRolloverEnabled(true);
        parseButton.setRolloverIcon(MipavUtil.getIcon("parseroll.gif"));
        parseButton.setBorder(pressedBorder);
        parseButton.addItemListener(this);
        parseButton.setFocusPainted(false);
        tBar.add(parseButton);

        JButton newButton = new JButton(MipavUtil.getIcon("new.gif"));

        newButton.addActionListener(this);
        newButton.setToolTipText("New top directory");
        newButton.setActionCommand("New");
        newButton.setBorderPainted(false);
        newButton.setRolloverEnabled(true);
        newButton.setRolloverIcon(MipavUtil.getIcon("newroll.gif"));
        newButton.setBorder(pressedBorder);
        newButton.addItemListener(this);
        newButton.setFocusPainted(false);
        tBar.add(newButton);

        JButton movieButton = new JButton(MipavUtil.getIcon("movieextraction.gif"));

        movieButton.addActionListener(this);
        movieButton.setToolTipText("Extract images to AVI movie");
        movieButton.setActionCommand("Movie");
        movieButton.setBorderPainted(false);
        movieButton.setRolloverEnabled(true);
        movieButton.setRolloverIcon(MipavUtil.getIcon("movieextractionroll.gif"));
        movieButton.setBorder(pressedBorder);
        movieButton.addItemListener(this);
        movieButton.setFocusPainted(false);
        tBar.add(movieButton);

        JButton configureColumnsButton = new JButton(MipavUtil.getIcon("configurecolumns.gif"));

        configureColumnsButton.addActionListener(this);
        configureColumnsButton.setToolTipText("Configure DICOM columns");
        configureColumnsButton.setActionCommand("Configure DICOM table");
        configureColumnsButton.setBorderPainted(false);
        configureColumnsButton.setRolloverEnabled(true);
        configureColumnsButton.setRolloverIcon(MipavUtil.getIcon("configurecolumnsroll.gif"));
        configureColumnsButton.setBorder(pressedBorder);
        configureColumnsButton.addItemListener(this);
        configureColumnsButton.setFocusPainted(false);
        tBar.add(configureColumnsButton);

        JButton selectAllRowsButton = new JButton(MipavUtil.getIcon("selectallrows.gif"));

        selectAllRowsButton.addActionListener(this);
        selectAllRowsButton.setToolTipText("Select all rows");
        selectAllRowsButton.setActionCommand("Select all rows");
        selectAllRowsButton.setBorderPainted(false);
        selectAllRowsButton.setRolloverEnabled(true);
        selectAllRowsButton.setRolloverIcon(MipavUtil.getIcon("selectallrowsroll.gif"));
        selectAllRowsButton.setBorder(pressedBorder);
        selectAllRowsButton.addItemListener(this);
        selectAllRowsButton.setFocusPainted(false);
        tBar.add(selectAllRowsButton);

        tBar.setFloatable(false);
        panel.add(tBar, BorderLayout.NORTH);

        return panel;
    }

    /**
     * Initializes scroll panes, tables, and models and adds them to the frame.
     */
    protected void init() {
        MouseListener tableListener = new TableListener();

        initializeImageTable(tableListener);

        JScrollPane imageTableScrollPane = new JScrollPane(imageTable);
        getContentPane().add(imageTableScrollPane);

        initializeStudyTable(tableListener);

        initializeSeriesTable(tableListener);

        JScrollPane studyTableScrollPane = new JScrollPane(studyTable, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        studyTableScrollPane.setPreferredSize(new Dimension(700, 200));
        studyTableScrollPane.setMinimumSize(new Dimension(40, 40));

        JScrollPane seriesTableScrollPane = new JScrollPane(seriesTable, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        seriesTableScrollPane.setPreferredSize(new Dimension(700, 200));
        seriesTableScrollPane.setMinimumSize(new Dimension(40, 40));

        JSplitPane sliderHeaderPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true, studyTableScrollPane,
                seriesTableScrollPane);

        rightPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true, sliderHeaderPane, imageTableScrollPane);

        setTitle("DICOM browser");
    }

    /**
     * AddSeriesData This method populates the series table based upon the studyID.
     * 
     * @param studyNo DOCUMENT ME!
     */
    private void addSeriesData(String studyNo) {
        seriesTableModel.removeAllRows();

        Object[] seriesData = new Object[8];
        String data = "";

        // first lets populate the rows of the series table...but not the #images attribute
        outerLoop: for (int i = 0; i < fileInfoVector.size(); i++) {
            FileInfoDicom fileInfoDICOM = fileInfoVector.elementAt(i);
            String sliceStudyNo = (String) fileInfoDICOM.getTagTable().getValue("0020,0010");
            if(sliceStudyNo.length() > 0) {
                char c = sliceStudyNo.charAt(sliceStudyNo.length() - 1);
            	if(c == '\0') {
            		sliceStudyNo = sliceStudyNo.substring(0, sliceStudyNo.indexOf(c));
            	}
            }

            if (sliceStudyNo == null) {
                sliceStudyNo = "";
            }

            if (sliceStudyNo.equals(studyNo)) {

                // Series number
                data = (String) fileInfoDICOM.getTagTable().getValue("0020,0011");
                if(data != null && data.length() > 0) {
                    char c = data.charAt(data.length() - 1);
                	if(c == '\0') {
                		data = data.substring(0, data.indexOf(c));
                	}
                }

                for (int k = 0; k < seriesTableModel.getRowCount(); k++) {
                    String seriesNo = (String) (seriesTableModel.getValueAt(k, 0));

                    if (seriesNo.equals(data)) {
                        continue outerLoop;
                    }
                }

                if (data != null) {
                    seriesData[0] = data;
                } else {
                    seriesData[0] = "";
                }

                // Series Type (PET) - why ?
                data = (String) fileInfoDICOM.getTagTable().getValue("0054,1000");
                if(data != null && data.length() > 0) {
                    char c = data.charAt(data.length() - 1);
                	if(c == '\0') {
                		data = data.substring(0, data.indexOf(c));
                	}
                }

                if (data != null) {
                    seriesData[1] = data;
                } else {
                    seriesData[1] = "";
                }

                // Series Time
                data = (String) fileInfoDICOM.getTagTable().getValue("0008,0031");

                if (data != null) {
                    seriesData[3] = data;
                } else {
                    seriesData[3] = "";
                }

                // Modality
                data = (String) fileInfoDICOM.getTagTable().getValue("0008,0060");
                if(data != null && data.length() > 0) {
                    char c = data.charAt(data.length() - 1);
                	if(c == '\0') {
                		data = data.substring(0, data.indexOf(c));
                	}
                }

                if (data != null) {
                    seriesData[4] = data;
                } else {
                    seriesData[4] = "";
                }

                // Series Description
                data = (String) fileInfoDICOM.getTagTable().getValue("0008,103E");
                if(data != null && data.length() > 0) {
                    char c = data.charAt(data.length() - 1);
                	if(c == '\0') {
                		data = data.substring(0, data.indexOf(c));
                	}
                }

                if (data != null) {
                    seriesData[5] = data;
                } else {
                    seriesData[5] = "";
                }

                seriesData[6] = fileInfoDICOM;

                seriesData[7] = studyNo;

                seriesTableModel.addRow(seriesData);

            } // end if
        } // end outerLoop for

        // we need to display the number of images in the series table
        // first init the counters to 0
        if (seriesNumberCounters == null) {
            seriesNumberCounters = new HashMap<String,Integer>();
        }

        for (int i = 0; i < seriesTableModel.getRowCount(); i++) {
            String seriesNo = (String) (seriesTableModel.getValueAt(i, 0));
            seriesNumberCounters.put(seriesNo, new Integer(0));

        }

        // go through the vector and increment the appropriate counter
        for (int i = 0; i < fileInfoVector.size(); i++) {
            FileInfoDicom fileInfoDICOM = fileInfoVector.elementAt(i);
            String ser = (String) fileInfoDICOM.getTagTable().getValue("0020,0011");
            if(ser != null && ser.length() > 0) {
                char c = ser.charAt(ser.length() - 1);
            	if(c == '\0') {
            		ser = ser.substring(0, ser.indexOf(c));
            	}
            }
            String sliceStudyNo = (String) fileInfoDICOM.getTagTable().getValue("0020,0010");
            if(sliceStudyNo != null && sliceStudyNo.length() > 0) {
                char c = sliceStudyNo.charAt(sliceStudyNo.length() - 1);
            	if(c == '\0') {
            		sliceStudyNo = sliceStudyNo.substring(0, sliceStudyNo.indexOf(c));
            	}
            }

            if (ser == null) {
                ser = "";
            }

            if (sliceStudyNo == null) {
                sliceStudyNo = "";
            }

            if ( (seriesNumberCounters.get(ser) != null) && (sliceStudyNo.equals(studyNo))) {
                Integer I = seriesNumberCounters.get(ser);
                int k = I.intValue();
                int j = ++k;
                seriesNumberCounters.put(ser, new Integer(j));
            }
        }

        // now update the series table
        for (int i = 0; i < seriesTableModel.getRowCount(); i++) {
            String seriesNo = (String) (seriesTableModel.getValueAt(i, 0));
            seriesTableModel.setValueAt(seriesNumberCounters.get(seriesNo), i, 2);
        }

    }

    /**
     * Adds the study data to the table.
     * 
     * @param fileInfo File info structure where data is stored.
     */
    private void addStudyData(FileInfoDicom fileInfo) {
        Object[] studyData = new Object[6];
        String data = "";

        // Study ID
        data = (String) fileInfo.getTagTable().getValue("0020,0010");
        if(data != null && data.length() > 0) {
            char c = data.charAt(data.length() - 1);
        	if(c == '\0') {
        		data = data.substring(0, data.indexOf(c));
        	}
        }

        for (int i = 0; i < studyTableModel.getRowCount(); i++) {
            String studyNo = (String) (studyTableModel.getValueAt(i, 2));

            if (studyNo.equals(data)) {
                return;
            } else {
                // break;
            }
        }

        // Patients name
        data = (String) fileInfo.getTagTable().getValue("0010,0010");
        if(data != null && data.length() > 0) {
            char c = data.charAt(data.length() - 1);
        	if(c == '\0') {
        		data = data.substring(0, data.indexOf(c));
        	}
        }

        if (data != null) {
            studyData[0] = data;
        } else {
            studyData[0] = "";
        }

        // Patient ID
        data = (String) fileInfo.getTagTable().getValue("0010,0020");
        if(data != null && data.length() > 0) {
            char c = data.charAt(data.length() - 1);
        	if(c == '\0') {
        		data = data.substring(0, data.indexOf(c));
        	}
        }

        if (data != null) {
            studyData[1] = data;
        } else {
            studyData[1] = "";
        }

        // Study ID
        data = (String) fileInfo.getTagTable().getValue("0020,0010");
        if(data != null && data.length() > 0) {
            char c = data.charAt(data.length() - 1);
        	if(c == '\0') {
        		data = data.substring(0, data.indexOf(c));
        	}
        }

        if (data != null) {
            studyData[2] = data;
        } else {
            studyData[2] = "";
        }

        // Study Date
        data = (String) fileInfo.getTagTable().getValue("0008,0020");
        if(data != null && data.length() > 0) {
            char c = data.charAt(data.length() - 1);
        	if(c == '\0') {
        		data = data.substring(0, data.indexOf(c));
        	}
        }

        if (data != null) {
            studyData[3] = data;
        } else {
            studyData[3] = "";
        }

        // Study Description
        data = (String) fileInfo.getTagTable().getValue("0008,1030");
        if(data != null && data.length() > 0) {
            char c = data.charAt(data.length() - 1);
        	if(c == '\0') {
        		data = data.substring(0, data.indexOf(c));
        	}
        }

        if (data != null) {
            studyData[4] = data;
        } else {
            studyData[4] = "";
        }

        studyData[5] = fileInfo;
        studyTableModel.addRow(studyData);
    }

    /**
     * Initializes GUI components and displays dialog.
     * 
     * <p>
     * For the brightnessSlider the slider values and the brightness values are identical. brightness is an offset going
     * from -255 to 255. This is enough to change all 0 values to 255 and all 255 values to 0. brightness is added to
     * all contrast scaled red, green, and blue.
     * </p>
     * 
     * <p>
     * However, for the contrastSlider the slider values are different from the contrast values. The contrast values go
     * from 0.1 to 10.0 while the slider values go from -200 to 200. contrast =
     * (float)Math.pow(10.0,contrastSlider.getValue()/200.0) The original red, green, and blue are mutliplied by
     * contrast.
     * </p>
     */
    private void buildBrightnessContrastPanel() {
        brightnessSlider = new JSlider(JSlider.HORIZONTAL, -255, 255, origBrightness);

        brightnessSlider.setMajorTickSpacing(102);
        brightnessSlider.setPaintTicks(true);
        brightnessSlider.setEnabled(true);
        brightnessSlider.addChangeListener(this);

        JLabel maximum = new JLabel(String.valueOf(255));

        maximum.setForeground(Color.black);
        maximum.setFont(serif12);

        current = new JLabel(String.valueOf(origBrightness));
        current.setForeground(Color.black);
        current.setFont(serif12B);

        JLabel minimum = new JLabel(String.valueOf( -255));

        minimum.setForeground(Color.black);
        minimum.setFont(serif12);

        JPanel sliderPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel.add(brightnessSlider, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;

        sliderPanel.add(minimum, gbc);

        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = .5;

        sliderPanel.add(current, gbc);

        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;

        sliderPanel.add(maximum, gbc);
        sliderPanel.setBorder(buildTitledBorder("Level"));

        contrastSlider = new JSlider(JSlider.HORIZONTAL, -200, 200, (int) (Math.round(86.85889638 * Math
                .log(origContrast))));

        contrastSlider.setMajorTickSpacing(80);
        contrastSlider.setPaintTicks(true);
        contrastSlider.setEnabled(true);
        contrastSlider.addChangeListener(this);

        JLabel maximum2 = new JLabel(String.valueOf(10));

        maximum2.setForeground(Color.black);
        maximum2.setFont(serif12);

        nfc = NumberFormat.getNumberInstance();
        nfc.setMaximumFractionDigits(3);

        current2 = new JLabel(String.valueOf(nfc.format(origContrast)));
        current2.setForeground(Color.black);
        current2.setFont(serif12B);

        JLabel minimum2 = new JLabel(String.valueOf(0.100));

        minimum2.setForeground(Color.black);
        minimum2.setFont(serif12);

        JPanel sliderPanel2 = new JPanel(new GridBagLayout());

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel2.add(contrastSlider, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;

        sliderPanel2.add(minimum2, gbc);

        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = .5;

        sliderPanel2.add(current2, gbc);

        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;

        sliderPanel2.add(maximum2, gbc);
        sliderPanel2.setBorder(buildTitledBorder("Window"));

        JPanel centerPanel = new JPanel(new GridBagLayout());

        GridBagConstraints gbc2 = new GridBagConstraints();

        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.fill = GridBagConstraints.BOTH;
        gbc2.weightx = 1;
        gbc2.gridheight = 2;
        centerPanel.add(sliderPanel2, gbc2);

        gbc2.gridy = 2;
        centerPanel.add(sliderPanel, gbc2);

        gbc2.gridheight = 1;
        gbc2.gridy = 4;

        brightnessContrastPanel = new JPanel(new BorderLayout());
        brightnessContrastPanel.add(centerPanel);
        brightnessContrastPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

    }

    /**
     * Builds the jmenubar and adds two options.. disregard series #s and exit/close
     */
    private void buildMenu() {
        JMenu menu;
        JMenuBar menuBar;
        JMenuItem menuItem;

        menuBar = new JMenuBar();
        menu = new JMenu("File");
        menu.setFont(MipavUtil.font12B);

        seriesOptionBox = new JCheckBoxMenuItem("Disregard series numbers", false);
        seriesOptionBox.setFont(MipavUtil.font12B);
        seriesOptionBox.addActionListener(this);
        seriesOptionBox.setActionCommand("SeriesOption");

        menu.add(seriesOptionBox);

        menuItem = new JMenuItem("Exit");
        menuItem.setFont(MipavUtil.font12B);
        menuItem.setActionCommand("Exit");
        menuItem.addActionListener(this);

        menu.add(menuItem);
        menuBar.add(menu);

        setJMenuBar(menuBar);
    }

    /**
     * DOCUMENT ME!
     */
    private void close() {
        setVisible(false);

        try {
            this.finalize();
        } finally {
            System.gc();
            this.dispose();
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param tableListener DOCUMENT ME!
     * 
     * @throws NumberFormatException DOCUMENT ME!
     */
    private void initializeImageTable(MouseListener tableListener) throws NumberFormatException {
        imageTableModel = new SortingTableModel();
        imageTableSorter = new TableSorter(imageTableModel);
        imageTable = new JTable(imageTableSorter);

        imageTableSorter.setTableHeader(imageTable.getTableHeader());

        imageTableSorter.setColumnComparator(new Integer(0).getClass(), TableSorter.COMPARABLE_COMPARATOR);
        imageTableSorter.setColumnComparator(new Double("0.0").getClass(), TableSorter.COMPARABLE_COMPARATOR);
        imageTableSorter.setColumnComparator(new Float("0.0").getClass(), TableSorter.COMPARABLE_COMPARATOR);
        imageTableSorter.setColumnComparator(new String().getClass(), TableSorter.LEXICAL_COMPARATOR);

        imageTable.addMouseListener(tableListener);

        restorePreferredColumnConfiguration();
    }

    /**
     * DOCUMENT ME!
     * 
     * @param tableListener DOCUMENT ME!
     */
    private void initializeSeriesTable(MouseListener tableListener) {
        seriesTableModel = new SortingTableModel();
        seriesTableSorter = new TableSorter(seriesTableModel);
        seriesTable = new JTable(seriesTableSorter);
        seriesTableSorter.setTableHeader(seriesTable.getTableHeader());

        for (int i = 0; i < DEFAULT_COLUMN_HEADERS_SERIES_TABLE.length; i++) {
            seriesTableModel.addColumn(DEFAULT_COLUMN_HEADERS_SERIES_TABLE[i]);
        }

        setSeriesTableMinMax();

        seriesTable.setRowSelectionAllowed(true);

        seriesTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        seriesTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        seriesTable.addMouseListener(tableListener);
        seriesTable.getTableHeader().addMouseListener(tableListener);

        seriesTable.setPreferredScrollableViewportSize(new Dimension(900, 200));
        seriesTable.setMinimumSize(new Dimension(40, 40));
    }

    /**
     * DOCUMENT ME!
     * 
     * @param tableListener DOCUMENT ME!
     */
    private void initializeStudyTable(MouseListener tableListener) {
        studyTableModel = new SortingTableModel();
        studyTableSorter = new TableSorter(studyTableModel);
        studyTable = new JTable(studyTableSorter);
        studyTableSorter.setTableHeader(studyTable.getTableHeader());

        for (int i = 0; i < DEFAULT_COLUMN_HEADERS_STUDY_TABLE.length; i++) {
            studyTableModel.addColumn(DEFAULT_COLUMN_HEADERS_STUDY_TABLE[i]);
        }

        setStudyTableMinMax();

        studyTable.setColumnSelectionAllowed(false);
        studyTable.setRowSelectionAllowed(true);

        studyTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        studyTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        studyTable.addMouseListener(tableListener);
        studyTable.getTableHeader().addMouseListener(tableListener);

        studyTable.setPreferredScrollableViewportSize(new Dimension(700, 200));
        studyTable.setMinimumSize(new Dimension(40, 40));
    }

    /**
     * Parses the files in the directory. Looks for DICOM files within several subdirectories of the file. Populates the
     * FileInfoVector
     * 
     * @param file File to start parse at.
     */
    private void parse(File file) {
        imageTableModel.removeAllRows();
        imagePanel.removeAll();
        imagePanel.repaint();

        long time = System.currentTimeMillis();

        setCursor(new Cursor(Cursor.WAIT_CURSOR));

        File[] children = file.listFiles();
        FileDicom imageFile = null;

        // used to reduce the amount of replication in the tag tables we read in (maybe). don't need to register the
        // children with the reference tag table, since the reference tag table should not be changing (no put() calls)
        // after it is read in initially
        FileInfoDicom referenceFileInfo = null;

        try {
            boolean success = false;

            for (int i = 0; i < children.length; i++) {

                if (children[i].isDirectory()) {
                    parse(children[i]);
                } else if ( !children[i].isDirectory()) {

                    try {

                        if (imageFile == null) {
                            imageFile = new FileDicom(children[i].getName(), children[i].getParent()
                                    + File.separatorChar);
                        } else {
                            imageFile.setFileName(children[i], referenceFileInfo);
                        }

                        if (imageFilter.accept(children[i])) {
                            success = imageFile.readHeader(true);
                        } else if (imageFile.isDICOM()) {
                            success = imageFile.readHeader(true);
                        } else {
                            imageFile.finalize();
                            imageFile = null;

                            continue;
                        }
                        
                    } catch (IOException error) {
                        MipavUtil.displayError("Unable to read file to parse.");
                        error.printStackTrace();

                        return;
                    }

                    if (referenceFileInfo == null) {
                        referenceFileInfo = (FileInfoDicom) imageFile.getFileInfo();
                    }

                    if (!imageFile.isDir()){ //finally check to make sure it isn't a DICOMDIR
                    	fileInfoVector.addElement((FileInfoDicom)imageFile.getFileInfo());
                    }
                    
                    if (success) {
                        addStudyData((FileInfoDicom) imageFile.getFileInfo());
                    }
                }
            }
        } catch (Exception err) {
            err.printStackTrace();
            MipavUtil.displayError("DICOM parser error: " + err);
        } finally {
            setCursor(Cursor.getDefaultCursor());

            if (imageFile != null) {
                imageFile.finalize();
                imageFile = null;
            }
        }

        Preferences.debug("Parse took: " + ( (System.currentTimeMillis() - time) / 1000f) + "\n");
    }

    /**
     * DOCUMENT ME!
     */
    private void restorePreferredColumnConfiguration() {
        Vector<String> imageNamesVector = Preferences.getDICOMBrowserTableConfiguration();

        if (imageNamesVector == null) {
            imageNamesVector = new Vector<String>(Arrays.asList(DEFAULT_COLUMN_HEADERS_IMAGE_TABLE));
        }

        for (int i = 0; i < imageNamesVector.size(); i++) {
            imageTableModel.addColumn(imageNamesVector.elementAt(i));
        }

        imageTable.setDefaultRenderer(new String().getClass(), new MIPAVTableCellRenderer());
    }

    /**
     * DOCUMENT ME!
     * 
     * @param seriesNumber DOCUMENT ME!
     * @param fileInfoDICOM DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    private boolean seriesNumberEqual(String seriesNumber, FileInfoDicom fileInfoDICOM) {
        Object object = fileInfoDICOM.getTagTable().getValue("0020,0011");

        if (object instanceof String) {

            if ( ((String) object).equals(seriesNumber)) {
                return true;
            } else {
                return false;
            }
        }

        return false;
    }

    /**
     * Sets the series table's min and max column widths.
     */
    private void setSeriesTableMinMax() {
        seriesTable.getColumn(DEFAULT_COLUMN_HEADERS_SERIES_TABLE[0]).setMinWidth(50);
        seriesTable.getColumn(DEFAULT_COLUMN_HEADERS_SERIES_TABLE[0]).setMaxWidth(50);
        seriesTable.getColumn(DEFAULT_COLUMN_HEADERS_SERIES_TABLE[1]).setMinWidth(50);
        seriesTable.getColumn(DEFAULT_COLUMN_HEADERS_SERIES_TABLE[1]).setMaxWidth(100);
        seriesTable.getColumn(DEFAULT_COLUMN_HEADERS_SERIES_TABLE[2]).setMinWidth(65);
        seriesTable.getColumn(DEFAULT_COLUMN_HEADERS_SERIES_TABLE[2]).setMaxWidth(65);
        seriesTable.getColumn(DEFAULT_COLUMN_HEADERS_SERIES_TABLE[4]).setMinWidth(35);
        seriesTable.getColumn(DEFAULT_COLUMN_HEADERS_SERIES_TABLE[4]).setMaxWidth(35);
        seriesTable.getColumn(DEFAULT_COLUMN_HEADERS_SERIES_TABLE[6]).setMinWidth(0);
        seriesTable.getColumn(DEFAULT_COLUMN_HEADERS_SERIES_TABLE[6]).setMaxWidth(0);
        seriesTable.getColumn(DEFAULT_COLUMN_HEADERS_SERIES_TABLE[7]).setMinWidth(50);
        seriesTable.getColumn(DEFAULT_COLUMN_HEADERS_SERIES_TABLE[7]).setMaxWidth(50);
    }

    /**
     * Sets the study table's min and max column widths.
     */
    private void setStudyTableMinMax() {
        studyTable.getColumn(DEFAULT_COLUMN_HEADERS_STUDY_TABLE[1]).setMinWidth(70);
        studyTable.getColumn(DEFAULT_COLUMN_HEADERS_STUDY_TABLE[1]).setMaxWidth(70);
        studyTable.getColumn(DEFAULT_COLUMN_HEADERS_STUDY_TABLE[2]).setMinWidth(55);
        studyTable.getColumn(DEFAULT_COLUMN_HEADERS_STUDY_TABLE[2]).setMaxWidth(55);
        studyTable.getColumn(DEFAULT_COLUMN_HEADERS_STUDY_TABLE[5]).setMinWidth(0);
        studyTable.getColumn(DEFAULT_COLUMN_HEADERS_STUDY_TABLE[5]).setMaxWidth(0);
    }

    // ~ Inner Classes
    // --------------------------------------------------------------------------------------------------

    /**
     * Table listener - listens for clicks on any of the three table headers or clicks within each table.
     */
    private class TableListener implements MouseListener {

        /**
         * When the user clicks on a header, sorts the column. When the user clicks on a study or series, parse. When
         * the user clicks on an image, displays a thumbnail.
         * 
         * @param e Event that triggered this function.
         */
        public void mouseClicked(MouseEvent e) {
            Object source = e.getSource();

            if (source.equals(studyTable)) {

                // we dont want to clear out the image panelthumbnail and reload series table
                // if user clicks on the same one...so...
                String studyNo = (String) studyTableModel.getValueAt(studyTable.getSelectedRow(), 2);

                if (seriesTableModel.getRowCount() > 0) {
                    String seriesTableStudyNo = (String) seriesTableModel.getValueAt(0, 7);

                    if (seriesTableStudyNo != null) {

                        if (seriesTableStudyNo.equals(studyNo)) {
                            return;
                        }
                    }
                }

                imageTableModel.removeAllRows();
                imagePanel.removeAll();
                imagePanel.repaint();
                addSeriesData(studyNo);
            } else if (source.equals(seriesTable)) {

                // we dont want to clear out the image panelthumbnail and reload image table
                // if user clicks on the same one...so...
                String seriesNumber = (String) seriesTableModel.getValueAt(seriesTableSorter.modelIndex(seriesTable
                        .getSelectedRow()), 0);

                if (imageTableVector != null) {
                    FileInfoDicom fileInfoDICOM = imageTableVector.elementAt(0);

                    if (fileInfoDICOM != null) {
                        String imageTableSeriesNumber = (String) fileInfoDICOM.getTagTable().getValue("0020,0011");
                        if(imageTableSeriesNumber.length() > 0) {
    	                    char c = imageTableSeriesNumber.charAt(imageTableSeriesNumber.length() - 1);
    	                	if(c == '\0') {
    	                		imageTableSeriesNumber = imageTableSeriesNumber.substring(0, imageTableSeriesNumber.indexOf(c));
    	                	}
                        }
                        if (imageTableSeriesNumber.equals(seriesNumber)) {
                            return;
                        }
                    }
                }

                String studyNo = (String) seriesTableModel.getValueAt(seriesTableSorter.modelIndex(seriesTable
                        .getSelectedRow()), 7);

                if (studyNo == null) {
                    studyNo = "";
                }

                imageTableModel.removeAllRows();
                imagePanel.removeAll();
                imagePanel.repaint();
                reloadRows(seriesNumber, studyNo);
            } else if (source.equals(imageTable)) {
                setCursor(new Cursor(Cursor.WAIT_CURSOR));

                int modelRow = imageTableSorter.modelIndex(imageTable.getSelectedRow());

                FileInfoDicom fileInfoDICOM = imageTableVector.elementAt(modelRow);
                //System.out.println(fileInfoDICOM.getFileName());
                buildImage(fileInfoDICOM.getFileName(), fileInfoDICOM.getFileDirectory() + File.separatorChar);
                componentImageDicom = img;
                componentImageDicom.setSliceBrightness(brightness, contrast);

                setCursor(Cursor.getDefaultCursor());
            }
        }

        /**
         * Unchanged.
         * 
         * @param e DOCUMENT ME!
         */
        @SuppressWarnings("unused")
        public void mouseDragged(MouseEvent e) {}

        /**
         * Unchanged.
         * 
         * @param e DOCUMENT ME!
         */
        public void mouseEntered(MouseEvent e) {}

        /**
         * Unchanged.
         * 
         * @param e DOCUMENT ME!
         */
        public void mouseExited(MouseEvent e) {}

        /**
         * DOCUMENT ME!
         * 
         * @param event DOCUMENT ME!
         */
        public void mousePressed(MouseEvent event) {}

        /**
         * Unchanged.
         * 
         * @param e DOCUMENT ME!
         */
        public void mouseReleased(MouseEvent e) {}
    }

}
