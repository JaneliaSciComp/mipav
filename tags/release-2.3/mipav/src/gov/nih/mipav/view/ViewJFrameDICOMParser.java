package gov.nih.mipav.view;

import gov.nih.mipav.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.renderer.JDialogRendererAVI;

import java.awt.event.*;
import java.awt.*;
import java.io.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.border.*;
import java.text.NumberFormat;
import java.util.*;


/**
 *   DICOM parser - shows pertinent DICOM tags, allows user to sort on specific
 *   values in the tags and open the proper images.  Can also open the images to
 *   an AVI file.  Can see image as thumbnail.  Extends ViewImageDirectory for
 *   the tree and thumbnail.
 *
 *   @author     Neva Cherniavsky
 *   @see        FileInfoDicom
 */
public class ViewJFrameDICOMParser extends ViewImageDirectory implements
        WindowListener
{
    private JTable studyTable;
    private SortingTableModel studyTableModel;
    private TableSorter studyTableSorter;

    private JTable seriesTable;
    private SortingTableModel seriesTableModel;
    private TableSorter seriesTableSorter;

    private JTable imageTable;
    private SortingTableModel imageTableModel;
    private TableSorter imageTableSorter;

    public static final String[] DEFAULT_COLUMN_HEADERS_IMAGE_TABLE = new String[]
                     {"Instance (formerly Image) Number", "Acquisition Time", "X-position", "Y-position", "Z-position"};
    public static final String [] DEFAULT_COLUMN_HEADERS_STUDY_TABLE = new String[]
                     {"Patient Name", "Patient ID", "Study ID", "Study Date", "Description", ""};
    public static final String [] DEFAULT_COLUMN_HEADERS_SERIES_TABLE = new String[] {"Series", "Type", "# Images", "Time", "Mod",
                      "Description", ""};

    private JSplitPane rightPane;

    private int brightness = 0;
    private float contrast = 1;
    private int origBrightness = 0;
    private float origContrast = 1;

    private JLabel current, current2;
    private ViewJComponentPreviewImage componentImageDicom = null;
    private NumberFormat nfc;
    private JSlider brightnessSlider, contrastSlider;

    private JPanel brightnessContrastPanel;

    private JCheckBoxMenuItem seriesOptionBox;
    private boolean disregardSeries;

    private Vector fileInfoVector;

    private Hashtable columnKeyTable = new Hashtable();

    /**
     *   Creates new frame.  Tables contain no data but the tree is initialized
     *   to the appropriate directory.
     *   @param ui   User interface.
     *   @param dir  Directory.
     */
    public ViewJFrameDICOMParser(ViewUserInterface ui, String dir)
    {
        super(ui, dir, new ViewImageFileFilter(new String[]
                                               {"dcm", "DCM", "ima", "IMA"}));

        addWindowListener(this);
        buildMenu();
        Preferences.setProperty("ImageDirectory", dir);
        buildSourceTreeListing(true);
        treePanel.setPreferredSize(new Dimension(200, 200));
        imagePanel = new JPanel();
        imagePanel.setPreferredSize(new Dimension(200, 350));
        buildBrightnessContrastPanel();

        JPanel subPanel = new JPanel(new BorderLayout());
        subPanel.add(imagePanel, BorderLayout.CENTER);
        subPanel.add(brightnessContrastPanel, BorderLayout.SOUTH);

        // Use VERTICAL_SPLIT to create a horizontal split pane
        JSplitPane lspPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true,
                                            treePanel, subPanel);
        // This is very strange - JSplitPane.HORIZONTAL_SPLIT is used
        // to create a vertical split plane
        JSplitPane spPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, true,
                                           lspPane, rightPane);

        getContentPane().add(spPane, BorderLayout.CENTER);
        pack();
        MipavUtil.centerOnScreen(this);
        setVisible(true);
    }

    /**
     *   Initializes scroll panes, tables, and models and adds them to the frame.
     */
    protected void init()
    {
        MouseListener tableListener = new TableListener();

        initializeImageTable(tableListener);

        JScrollPane imageTableScrollPane = new JScrollPane(imageTable);
        getContentPane().add(imageTableScrollPane);

        initializeStudyTable(tableListener);

        initializeSeriesTable(tableListener);

        JScrollPane studyTableScrollPane = new JScrollPane(studyTable,
                                                 JScrollPane.
                                                 VERTICAL_SCROLLBAR_AS_NEEDED,
                                                 JScrollPane.
                                                 HORIZONTAL_SCROLLBAR_AS_NEEDED);

        studyTableScrollPane.setPreferredSize(new Dimension(700, 200));
        studyTableScrollPane.setMinimumSize(new Dimension(40, 40));


        JScrollPane seriesTableScrollPane = new JScrollPane(seriesTable,
                                                  JScrollPane.
                                                  VERTICAL_SCROLLBAR_AS_NEEDED,
                                                  JScrollPane.
                                                  HORIZONTAL_SCROLLBAR_AS_NEEDED);

        seriesTableScrollPane.setPreferredSize(new Dimension(700, 200));
        seriesTableScrollPane.setMinimumSize(new Dimension(40, 40));

        JSplitPane sliderHeaderPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true,
                studyTableScrollPane, seriesTableScrollPane);

        rightPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true,
                                   sliderHeaderPane, imageTableScrollPane);

        setTitle("DICOM browser");
    }

    private void initializeSeriesTable(MouseListener tableListener)
    {
        seriesTableModel = new SortingTableModel();
        seriesTableSorter = new TableSorter(seriesTableModel);
        seriesTable = new JTable(seriesTableSorter);
        seriesTableSorter.setTableHeader(seriesTable.getTableHeader());

        for (int i = 0; i < DEFAULT_COLUMN_HEADERS_SERIES_TABLE.length; i++)
        {
            seriesTableModel.addColumn(DEFAULT_COLUMN_HEADERS_SERIES_TABLE[i]);
        }

        setSeriesTableMinMax();

        seriesTable.setAutoResizeMode(seriesTable.AUTO_RESIZE_ALL_COLUMNS);
        seriesTable.setSelectionMode(ListSelectionModel.
                                     MULTIPLE_INTERVAL_SELECTION);
        seriesTable.addMouseListener(tableListener);
        seriesTable.getTableHeader().addMouseListener(tableListener);

        seriesTable.setPreferredScrollableViewportSize(new Dimension(900, 200));
        seriesTable.setMinimumSize(new Dimension(40, 40));
    }

    private void initializeStudyTable(MouseListener tableListener)
    {
        studyTableModel = new SortingTableModel();
        studyTableSorter = new TableSorter(studyTableModel);
        studyTable = new JTable(studyTableSorter);
        studyTableSorter.setTableHeader(studyTable.getTableHeader());

        for (int i = 0; i < DEFAULT_COLUMN_HEADERS_STUDY_TABLE.length; i++)
        {
            studyTableModel.addColumn(DEFAULT_COLUMN_HEADERS_STUDY_TABLE[i]);
        }

        setStudyTableMinMax();

        studyTable.setColumnSelectionAllowed(false);

        studyTable.setAutoResizeMode(studyTable.AUTO_RESIZE_ALL_COLUMNS);
        studyTable.setSelectionMode(ListSelectionModel.
                                    MULTIPLE_INTERVAL_SELECTION);
        studyTable.addMouseListener(tableListener);
        studyTable.getTableHeader().addMouseListener(tableListener);

        studyTable.setPreferredScrollableViewportSize(new Dimension(700, 200));
        studyTable.setMinimumSize(new Dimension(40, 40));
    }

    private void initializeImageTable(MouseListener tableListener) throws NumberFormatException
    {
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

    private void restorePreferredColumnConfiguration()
    {
        Vector imageNamesVector = Preferences.getDICOMBrowserTableConfiguration();
        if (imageNamesVector == null)
        {
            imageNamesVector = new Vector(Arrays.asList(DEFAULT_COLUMN_HEADERS_IMAGE_TABLE));
        }

        for (int i = 0; i < imageNamesVector.size(); i++)
        {
            imageTableModel.addColumn((String) imageNamesVector.elementAt(i));
        }

        imageTable.setDefaultRenderer(new String().getClass(), new MIPAVTableCellRenderer());
    }


    private void close()
    {
        setVisible(false);
        try
        {
            this.finalize();
        }
        finally
        {
            System.gc();
            this.dispose();
        }
    }

    /**
     * Cleans  memory.
     */
    public void finalize()
    {
        for (int i = 0; i < seriesTableModel.getRowCount(); i++)
        {
            FileInfoDicom fileInfo = (FileInfoDicom) seriesTableModel.getValueAt(i,
                    6);
            if (fileInfo != null)
            {
                fileInfo.finalize();
                fileInfo = null;
            }
        }

        for (int i = 0; i < studyTableModel.getRowCount(); i++)
        {
            FileInfoDicom fileInfo = (FileInfoDicom) studyTableModel.getValueAt(i, 5);
            if (fileInfo != null)
            {
                fileInfo.finalize();
                fileInfo = null;
            }
        }

        if (fileInfoVector != null)
        {
            for (int i = 0; i < fileInfoVector.size(); i++)
            {
                FileInfoDicom fileInfo = (FileInfoDicom) fileInfoVector.elementAt(i);
                if (fileInfo != null)
                {
                    fileInfo.finalize();
                    fileInfo = null;
                }
            }

            fileInfoVector = null;
        }

        columnKeyTable = null;

        if (componentImageDicom != null)
        {
            componentImageDicom.dispose(false);
        }
        componentImageDicom = null;
        imageTableModel = null;
        seriesTableModel = null;
        studyTableModel = null;

        imageTableSorter = null;
        seriesTableSorter = null;
        studyTableSorter = null;

        if (imageTable != null)
        {
            imageTable.removeAll();
        }
        imageTable = null;

        if (seriesTable != null)
        {
            seriesTable.removeAll();
        }
        seriesTable = null;

        if (studyTable != null)
        {
            studyTable.removeAll();
        }
        studyTable = null;
    }

    /**
     *   Sets the study table's min and max column widths.
     */
    private void setStudyTableMinMax()
    {
        studyTable.getColumn(DEFAULT_COLUMN_HEADERS_STUDY_TABLE[1]).setMinWidth(70);
        studyTable.getColumn(DEFAULT_COLUMN_HEADERS_STUDY_TABLE[1]).setMaxWidth(70);
        studyTable.getColumn(DEFAULT_COLUMN_HEADERS_STUDY_TABLE[2]).setMinWidth(55);
        studyTable.getColumn(DEFAULT_COLUMN_HEADERS_STUDY_TABLE[2]).setMaxWidth(55);
        studyTable.getColumn(DEFAULT_COLUMN_HEADERS_STUDY_TABLE[5]).setMinWidth(0);
        studyTable.getColumn(DEFAULT_COLUMN_HEADERS_STUDY_TABLE[5]).setMaxWidth(0);
    }

    /**
     *   Sets the series table's min and max column widths.
    */
    private void setSeriesTableMinMax()
    {
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
    }


    /**
     *   Recreates the tree when a new directory is selected;
     *   refreshes the tree when refresh is selected.
     *   @param event    Event that triggered this function.
     */
    public void actionPerformed(ActionEvent event)
    {
        String command = event.getActionCommand();

        this.setCursor(Cursor.getDefaultCursor());
        final ViewFileTreeNode node = (ViewFileTreeNode) directoryTree.
                                getLastSelectedPathComponent();

        if (command.equals("Parse"))
        {
            if (node == null)
            {
                MipavUtil.displayError(
                        "You must select a directory or image to parse");
                return;
            }
            if (node.isDirectory())
            {
                studyTableModel.setRowCount(0);
                seriesTableModel.setRowCount(0);
                parse(node.getFile(), true, true, null);
            }
        }
        else if (command.equals("New"))
        {
            ViewDirectoryChooser chooser = new ViewDirectoryChooser(
                    userInterface, this);
            String dir = chooser.getImageDirectory();

            if (dir != null)
            {
                Preferences.setProperty("ImageDirectory", dir);
                directory = dir;
                treePanel.removeAll();
                buildSourceTreeListing(true);
                validate();
            }
        }
        else if (command.equals("Open"))
        {
            FileInfoDicom fileInfoDICOM = null;

            int[] rows = imageTable.getSelectedRows();
            String[] fileNames = new String[rows.length];

            if (rows.length == 0)
            {
                MipavUtil.displayError("You must select an image to open.");
                return;
            }

            for (int i = 0; i < rows.length; i++)
            {
                int modelRow = imageTableSorter.modelIndex(rows[i]);

                fileInfoDICOM = (FileInfoDicom) fileInfoVector.elementAt(modelRow);

                fileNames[i] = fileInfoDICOM.getFileName();
            }

            FileIO io = new FileIO();

            io.setFileDir(fileInfoDICOM.getFileDirectory() + File.separatorChar);

            ModelImage image = io.readDicom(fileNames);

            if (image == null)
            {
                return;
            }
            image.calcMinMax();
            new ViewJFrameImage(image, io.getModelLUT());

        }
        else if (command.equals("SeriesOption"))
        {
            if (seriesOptionBox.isSelected())
            {
                disregardSeries = true;
            }
            else
            {
                disregardSeries = false;
            }
        }
        else if (command.equals("Exit"))
        {
            this.close();
        }
        else if (command.equals("Movie"))
        {
            int[] selectedRows = imageTable.getSelectedRows();
            String[] fileNames = new String[selectedRows.length];
            String fileName;
            String directory;
            double frameRate = 21.0;
            int microSecondsPerFrame;
            boolean applyWindowLevel = true;
            FileInfoBase[] fileInfo;

            if (selectedRows.length < 2)
            {
                MipavUtil.displayError("You must open at least 2 images for a 3D AVI file.");
                return;
            }

            JDialogRendererAVI aviDialog = new JDialogRendererAVI(this,
                    userInterface);

            if (aviDialog.isCancelled())
            {
                return;
            }
            else {
                frameRate = aviDialog.getFrameRate();
                microSecondsPerFrame = (int)Math.round(1000000/frameRate);
                applyWindowLevel = aviDialog.getApplyWindowLevel();
            }

            JFileChooser chooser = new JFileChooser();

            if (userInterface.getDefaultDirectory() != null)
            {
                chooser.setCurrentDirectory(new File(userInterface.
                        getDefaultDirectory()));
            }
            else
            {
                chooser.setCurrentDirectory(new File(System.getProperties().
                        getProperty("user.dir")));
            }
            chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[]
                    {"avi"}));
            int returnVal = chooser.showSaveDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION)
            {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) +
                            File.separatorChar;
                userInterface.setDefaultDirectory(directory);
            }
            else
            {
                return;
            }

            FileInfoDicom fileInfoDICOM = null;

            for (int i = 0; i < fileNames.length; i++)
            {
                int modelRow = imageTableSorter.modelIndex(selectedRows[i]);

                fileInfoDICOM = (FileInfoDicom) fileInfoVector.elementAt(modelRow);

                fileNames[i] = fileInfoDICOM.getFileName();
            }
            FileIO io = new FileIO();

            io.setFileDir(fileInfoDICOM.getFileDirectory() + File.separatorChar);
            ModelImage image = io.readDicom(fileNames);

            if (image == null)
            {
                return;
            }
            image.calcMinMax();
            ModelLUT LUT = null;

            if (image.isColorImage() == false)
            {
                int[] dimExtentsLUT = new int[2];

                dimExtentsLUT[0] = 4;
                dimExtentsLUT[1] = 256;

                LUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
                float min, max;

                if (image.getType() == ModelStorageBase.UBYTE)
                {
                    min = 0;
                    max = 255;
                }
                else if (image.getType() == ModelStorageBase.BYTE)
                {
                    min = -128;
                    max = 127;
                }
                else
                {
                    min = (float) image.getMin();
                    max = (float) image.getMax();
                }
                float imgMin = (float) image.getMin();
                float imgMax = (float) image.getMax();

                LUT.resetTransferLine(min, imgMin, max, imgMax);
                LUT.makeLUT(256);
                if (applyWindowLevel) {
                    float x[] = new float[4];
                    float y[] = new float[4];
                    float z[] = new float[4];
                    float level;
                    float window;
                    x[0] = min;
                    y[0] = dimExtentsLUT[1] - 1;
                    x[3] = max;
                    y[3] = 0;
                    if ( contrast == 0 ) {
                        contrast = 1;
                    }

                    // Use (max-min)/255 because the slider is only goes from -255 to 255
                    // and need to take all values down to min or up to max
                    level = (min + max)/2.0f - brightness*(max-min)/255;
                    window = (max - min)*contrast/2.0f;

                    x[2] = level + window / 2;
                    if (x[2] > max) {
                        y[2] = 255.0f * (x[2] - max) / window;
                        x[2] = max;
                    } else {
                        y[2] = 0.0f;
                    }

                    x[1] = level - window / 2;
                    if (x[1] < min) {
                        y[1] = 255.0f - 255.0f * (min - x[1]) / window;
                        x[1] = min;
                    } else {
                        y[1] = 255.0f;
                    }

                    LUT.getTransferFunction().importArrays( x, y, 4 );
                } // if (applyWindowLevel)
            }

            fileInfo = image.getFileInfo();
            for (int i = 0; i < fileInfo.length; i++) {
                image.getFileInfo()[i].setUnitsOfMeasure(FileInfoBase.MICROSEC,2);
                image.getFileInfo()[i].setResolutions(microSecondsPerFrame,2);
            }

            try
            {
                FileAvi aviFile = new FileAvi(userInterface, fileName,
                                              directory);

                aviFile.writeImage(image, null, LUT, null, null, null, 0, 0, 0,
                                   .3f, .5f, new BitSet(), -1);
            }
            catch (IOException error)
            {
                MipavUtil.displayError("I/O Exception while writing AVI file.");
                image.disposeLocal();
                image = null;
                return;
            }
            image.disposeLocal();
            image = null;
        }
        else if (command.equals("Configure DICOM table"))
        {
            new JDialogSelectDICOMColumnHeaders(this);
        }
        else if (command.equals("Select all rows"))
        {
            if (imageTableSorter.getTableModel().getRowCount() > 0)
            {
                imageTable.setRowSelectionInterval(0, imageTableSorter.getRowCount()-1);
            }
        }
    }

    public Vector getColumnNames()
    {
        Vector columnNames = new Vector();

        for (int i = 0; i < imageTableModel.getColumnCount(); i++)
        {
            columnNames.addElement(imageTable.getColumnName(i));
        }

        return columnNames;
    }

    public void setHeaderConfiguration(JTable rightTable)
    {
        imageTableModel = new SortingTableModel();
        imageTableSorter.setTableModel(imageTableModel);

        SortingTableModel rightTableModel = (SortingTableModel) ((TableSorter) rightTable.getModel()).getTableModel();

        for (int i = 0; i < rightTableModel.getRowCount(); i++)
        {
            Vector row = rightTableModel.getRow(i);

            columnKeyTable.put(row.elementAt(1), row.elementAt(0));

            imageTableModel.addColumn((String) row.elementAt(1));
        }

        imageTableSorter.fireTableStructureChanged();
    }

    public void reloadRows()
    {
        reloadRows(null);
    }

    /**
     * The purpose of this method is to re-parse the DICOM files to refresh the table data. It is called after
     * the user hits "apply" in the configuration dialog, or the user clicks "parse" in the toolbar.
     *
     */
    public void reloadRows(String seriesNumber)
    {
        ViewFileTreeNode node = (ViewFileTreeNode) directoryTree.getLastSelectedPathComponent();

        if (node != null && fileInfoVector != null) // fileInfoVector is the Vector of DICOM headers associated with the selected file
        {
            try
            {
               // set the progess background color to dark gray
               //System.err.println("setting to dark gray");
               progressPanel.setValueImmed(0);
               progressPanel.getProgressBar().setBackground(Color.DARK_GRAY);
               //progressPanel.repaint();
               //this.repaint();
                Vector tableHeaderVector = imageTableModel.getColumnNames(); // Vector holding the names of the columns

                int mod = (fileInfoVector.size() / 100);
                //System.err.println("mod is: " + mod);
                if ( mod == 0 ) mod = 1;
               // progressPanel.setValueImmed(100 * (i + 1) / (fileInfoVector.size()));

                // fileInfoVector.size() is the number of rows in the table.
                for (int i = 0; i < fileInfoVector.size(); i++)
                {
                    FileInfoDicom fileInfoDICOM = (FileInfoDicom) fileInfoVector.elementAt(i);

                    if (seriesNumber == null || seriesNumber.equals("") || seriesNumberEqual(seriesNumber, fileInfoDICOM) == true)
                    {
                        Vector newRow = new Vector();

                        // tableHeader.size() is the number of columns in the table
                        for (int j = 0; j < tableHeaderVector.size(); j++)
                        {
                            String columnName = (String) tableHeaderVector.elementAt(j); // get name of first column
                            String key = (String) columnKeyTable.get(columnName); // get key associated with this column

                            if (isCompositeXYZPositionColumn(columnName)) // test to see if we have to populate this column manually i.e. non-native DICOM tag
                            {
                                key = JDialogSelectDICOMColumnHeaders.CUSTOM; // assign CUSTOM to key to indicate it is non-native DICOM tag
                            }
                            else if (key == null)
                            {
                                key = DICOMDictionaryBuilder.getKeyFromTagName(columnName); // might return null
                            }

                            if (key != null && columnName.equals("Instance (formerly Image) Number"))
                            {
                                FileDicomTag dicomTag = fileInfoDICOM.getTag(key);
                                String instanceNumber = (String) ((FileDicomTag) dicomTag).getValue(true);

                                try
                                {
                                    Integer integer = new Integer(Integer.parseInt(instanceNumber.trim()));
                                    newRow.addElement(integer);
                                }
                                catch (Exception e)
                                {
                                    newRow.addElement(instanceNumber);
                                }

                                imageTableModel.setColumnClass(new Integer(0).getClass(), j);
                            }

                            else if (key != null && key.equals(JDialogSelectDICOMColumnHeaders.CUSTOM))
                            {
                                /* This is a hack because DICOM puts all 3 patient orientation values into one value.
                                 * To account for the fact that the user will probably want to sort on these values individually,
                                 * we have to split up the x, y, z positions from patient orientation and must manually
                                 * populate these columns.
                                 */
                                if (((String) tableHeaderVector.elementAt(j)).equals("X-position"))
                                {
                                    Float floatObj = new Float(fileInfoDICOM.xLocation);
                                    imageTableModel.setColumnClass(floatObj.getClass(), j);

                                    newRow.addElement(floatObj);
                                }
                                else if (((String) tableHeaderVector.elementAt(j)).equals(
                                        "Y-position"))
                                {
                                    Float floatObj = new Float(fileInfoDICOM.yLocation);
                                    imageTableModel.setColumnClass(floatObj.getClass(), j);

                                    newRow.addElement(floatObj);
                                }
                                else if (((String) tableHeaderVector.elementAt(j)).equals(
                                        "Z-position"))
                                {
                                    Float floatObj = new Float(fileInfoDICOM.zLocation);
                                    imageTableModel.setColumnClass(floatObj.getClass(), j);

                                    newRow.addElement(floatObj);
                                }
                                else
                                {
                                    newRow.addElement(SortingTableModel.EMPTY_CELL);
                                }
                            }

                            /**
                             * This is the block of code that is the default case. Here, we are asking for the data
                             * based on the key's value. Since this is not a special column (i.e. X-position, Y-position,
                             * Z-position), the value should be read from the fileInfoDICOM.getValue() method.
                             */
                            else if (key != null)
                            {
                                FileDicomTag dicomTag = fileInfoDICOM.getTag(key);

                                if (dicomTag == null)
                                {
                                    newRow.addElement(SortingTableModel.EMPTY_CELL); // if data is not present, add empty cell
                                }
                                else
                                {
                                    if (dicomTag.getType().equals("SL") ||
                                        dicomTag.getType().equals("UL") ||
                                        dicomTag.getType().equals("SS") ||
                                        dicomTag.getType().equals("US"))
                                    {
                                        /**
                                         * Try to create a Number object out of the data. This is important because the type
                                         * of data indicates to the TableSorter how to sort, i.e. lexically or numerically.
                                         */
                                        try
                                        {
                                            Integer integer = new Integer(Integer.parseInt(((String) ((
                                                    FileDicomTag) dicomTag).getValue(true))));
                                            imageTableModel.setColumnClass(integer.getClass(), j); // set numerical sorting
                                            newRow.addElement(integer);
                                            continue;
                                        }
                                        catch (NumberFormatException nfe)
                                        {
                                            // at this point, its not a number, so assume String
                                            imageTableModel.setColumnClass(new String().getClass(),
                                                    j); // set lexical sorting
                                            newRow.addElement(dicomTag.getValue(true));
                                        }
                                    }

                                    else if (dicomTag.getType().equals("FD") ||
                                             dicomTag.getType().equals("FL"))
                                    {
                                        try
                                        {
                                            Double doubleObj = new Double(Double.parseDouble(((
                                                    String) ((FileDicomTag) dicomTag).getValue(true))));
                                            imageTableModel.setColumnClass(doubleObj.getClass(), j); // set numerical sorting
                                            newRow.addElement(doubleObj);
                                            continue;
                                        }
                                        catch (NumberFormatException nfe)
                                        {
                                            // at this point, its not a number, so assume String
                                            imageTableModel.setColumnClass(new String().getClass(),
                                                    j); // set lexical sorting
                                            newRow.addElement(dicomTag.getValue(true));
                                        }
                                    }
                                    else
                                    {
                                        imageTableModel.setColumnClass(new String().getClass(), j); // set lexical sorting
                                        newRow.addElement(((FileDicomTag) dicomTag).getValue(true));
                                    }
                                }
                            }
                            else
                            {
                                newRow.addElement(SortingTableModel.EMPTY_CELL); // data is null, add empty cell
                            }



                        }
                        if (i % mod == 0) {
                            progressPanel.setValueImmed(100 * (i + 1) / (fileInfoVector.size()));
                           // System.err.println("pbar val to: " + (100 * (i + 1) / (fileInfoVector.size())));
                        }
                        imageTableModel.addRow(newRow); // the new row, after all column values have been added, is added to the table
                    }
                }

                //imageTableSorter.fireTableDataChanged();
                //this.repaint();
                progressPanel.getProgressBar().setBorderPainted(false);
                progressPanel.getProgressBar().setBackground(this.getBackground());
                progressPanel.getProgressBar().setForeground(this.getBackground());

            }
            catch (Exception e)
            {
                e.printStackTrace();
            }
            finally
            {
                imageTableSorter.fireTableDataChanged();
            }
        }
    }

    private boolean seriesNumberEqual(String seriesNumber, FileInfoDicom fileInfoDICOM)
    {
        FileDicomTag tag = fileInfoDICOM.getTag("0020,0011");

        Object object = tag.getValue(true);

        if (object instanceof String)
        {
            if (((String) object).equals(seriesNumber))
            {
                return true;
            }
            else
            {
                return false;
            }
        }

        return false;
    }

    /**
     *   Parses the files in the directory.  Looks for DICOM files within
     * several subdirectories of the file.  Most DICOM directory-sets are
     * stored in a three-directory tree structure, where the base-directory
     * (the Study level directory), contains sets of subdirectories (the
     * series level) which are themselves containers of directories for
     * images.
     *
     *   @param file         File to start parse at.
     *   @param addStudy     Whether or not to update study info in table.
     *   @param addSeries    Whether or not to update series info in table.
     */
    private void parse(File file, boolean addStudy, boolean addSeries,
                       FileInfoDicom fileInfoRef)
    {
        imageTableModel.removeAllRows();

        fileInfoVector = new Vector();
        long time = System.currentTimeMillis();

        setCursor(new Cursor(Cursor.WAIT_CURSOR));
        int n = 0;

        File[] children = file.listFiles();
        File[] parent = new File[0];
        File[] grandparent = new File[]
                             {file};
        File[] test = file.listFiles();
        FileDicom imageFile = null;

        boolean first = true;
        boolean imageLevel = true;

        test = null;

        if (addSeries == true)
        {
            for (n = 0; n < children.length; n++)
            {
                if (!children[n].isDirectory() && imageFilter.accept(children[n]))
                {
                    break;
                }
            }

            if (n < children.length || children.length == 0)
            {
                test = null;
            }
            else
            {
                test = children[0].listFiles(); // Looking for directory -- if so then not at image level
            }
        }

        //ViewJProgressBar progressBar = null;
        boolean once = false;
        try
        {
            while (test != null)
            { // If test != null then we are not at the image level.
                imageLevel = false;
                if (once)
                {
                    grandparent = parent;
                }
                parent = children;
                children = test;
                if (children.length > 0)
                {
                    test = children[0].listFiles();
                }
                else
                {
                    test = null;
                }
                once = true;
            }

            // grandparent should be study level.
            // parent should be series level.
            boolean success = false;


            //set the progressPanel (bar) to dark gray
            progressPanel.setValueImmed(0);
            progressPanel.getProgressBar().setBackground(Color.DARK_GRAY);
          //  progressPanel.repaint();
          //  this.repaint();

            if (!imageLevel)
            {
               // progressBar = new ViewJProgressBar("Reading directory information", "Loading...", 0, 100, false, this, this);
               // MipavUtil.centerOnScreen(progressBar);
               // progressBar.setVisible(true);


               for (int j = 0; j < grandparent.length; j++)
                {
                    parent = grandparent[j].listFiles();
                    first = true;
                    for (int i = 0; parent != null && i < parent.length; i++)
                    {
                        test = parent[i].listFiles();
                        if (test != null && test.length == 0)
                        {
                            test = null;
                        }

                        if (test != null)
                        {
                            for (n = 0; n < test.length; n++)
                            {
                                if (!test[n].isDirectory())
                                {
                                    break;
                                }
                            }
                        }
                        if (test != null && !test[n].isDirectory())
                        {
                            try
                            {
                                if (first)
                                {
                                    imageFile = new FileDicom(
                                            test[n].getName(),
                                            test[n].getParent() +
                                            File.separatorChar);
                                }
                                else
                                {
                                    imageFile.setFileName(test[n]);
                                }

                                if (imageFilter.accept(test[n]))
                                {
                                    success = imageFile.readParserHeader();
                                }
                                else if (imageFile.isDICOM())
                                {
                                    success = imageFile.readParserHeader();
                                }
                                else
                                {
                                    continue;
                                }

                            }
                            catch (IOException error)
                            {
                                MipavUtil.displayError(
                                        "Unable to read file to parse.");
                                return;
                            }

                            FileInfoDicom fileInfo = (FileInfoDicom) imageFile.
                                    getFileInfo();

                            if (first && addStudy && success)
                            {
                                addStudyData(fileInfo);
                                first = false;
                            }

                            if (first && success)
                            {
                                first = false;
                            }

                            if (addSeries && success)
                            {
                                int count = 0;

                                for (int ii = 0; ii < test.length; ii++)
                                {
                                    if (imageFilter.accept(test[ii]) &&
                                        !test[ii].isDirectory())
                                    {
                                        count++;
                                    }
                                }
                                addSeriesData(fileInfo, null, count);
                            }
                        }
                    }
                    addSeries = false;
                //    System.err.println("updating to: " + (100 * (j + 1) / (grandparent.length)));
                progressPanel.updateValueImmed(100 * (j + 1) / (grandparent.length));
//                    progressBar.updateValue(100 * (j + 1) / (grandparent.length), false);
                }
                if (addStudy)
                {
                    studyTable.setRowSelectionInterval(0, 0);
                }
            }
            else
            {
            //    progressBar = new ViewJProgressBar("Reading directory information", "Loading...", 0, 100, false, this, this);
            //    MipavUtil.centerOnScreen(progressBar);
             //   progressBar.setVisible(true);

                // image level
                for (int i = 0; i < children.length; i++)
                {
                    if (!children[i].isDirectory())
                    {
                        try
                        {
                            if (imageFile == null)
                            {
                                imageFile = new FileDicom(
                                        children[i].getName(),
                                        children[i].getParent() +
                                        File.separatorChar);
                            }
                            else
                            {
                                imageFile.setFileName(children[i]);
                            }

                            if (imageFilter.accept(children[i]))
                            {
                                success = imageFile.readParserHeader();
                            }
                            else if (imageFile.isDICOM())
                            {
                                success = imageFile.readParserHeader();
                            }
                            else
                            {
                                continue;
                            }
                        }
                        catch (IOException error)
                        {
                            MipavUtil.displayError(
                                    "Unable to read file to parse.");
                            error.printStackTrace();
                            return;
                        }

                        FileInfoDicom fileInfo = (FileInfoDicom) imageFile.getFileInfo();
                        fileInfoVector.addElement(fileInfo);

                        if (success)
                        {
                            if (addStudy)
                            {
                                addStudyData(fileInfo);
                            }
                            int count = 0;

                            if (addSeries)
                            {
                                addSeriesData(fileInfo, fileInfoRef, count);
                            }
                        }
                    }
             //       System.err.println("updating to: " + (100 * (i + 1) / (children.length)));
                    progressPanel.updateValueImmed(100 * (i + 1) / (children.length));
                }

                if (fileInfoRef != null)
                {
                    // do something
                    FileDicomTag tag = fileInfoRef.getTag("0020,0011");
                    Object object = tag.getValue(true);
                    if (object instanceof String)
                    {
                        reloadRows((String) object);
                    }
                    else
                    {
                        reloadRows();
                    }
                }
                else
                {
                    reloadRows();
                }
            }
            progressPanel.getProgressBar().setBorderPainted(false);
            progressPanel.getProgressBar().setBackground(this.getBackground());
            progressPanel.getProgressBar().setForeground(this.getBackground());

            imageTableSorter.fireTableDataChanged();
        }
        catch (Exception err)
        {
            err.printStackTrace();
            MipavUtil.displayError("DICOM parser error: " + err );
        }
        finally
        {
            setCursor(Cursor.getDefaultCursor());
        }
        Preferences.debug("Parse took: " +
                          (System.currentTimeMillis() - time) / 1000f + "\n");

    }

    /**
     *   Adds the study data to the table.
     *   @param fileInfo File info structure where data is stored.
     */
    private void addStudyData(FileInfoDicom fileInfo)
    {
        Object[] studyData = new Object[6];
        String data = "";

        // Patients name
        data = (String) fileInfo.getValue("0020,0010");
        for (int i = 0; i < studyTableModel.getRowCount(); i++)
        {
            String studyNo = (String) (studyTableModel.getValueAt(i, 2));
            if (studyNo.equals(data))
            {
                return;
            }
            else
            {
                break;
            }
        }

        data = (String) fileInfo.getValue("0010,0010");
        if (data != null)
        {
            studyData[0] = data;
        }
        else
        {
            studyData[0] = "";
        }

        // Patient ID
        data = (String) fileInfo.getValue("0010,0020");
        if (data != null)
        {
            studyData[1] = data;
        }
        else
        {
            studyData[1] = "";
        }

        // Study ID
        data = (String) fileInfo.getValue("0020,0010");
        if (data != null)
        {
            studyData[2] = data;
        }
        else
        {
            studyData[2] = "";
        }

        // Study Date
        data = (String) fileInfo.getValue("0008,0020");
        if (data != null)
        {
            studyData[3] = data;
        }
        else
        {
            studyData[3] = "";
        }

        // Study Description
        data = (String) fileInfo.getValue("0008,1030");
        if (data != null)
        {
            studyData[4] = data;
        }
        else
        {
            studyData[4] = "";
        }

        studyData[5] = fileInfo;
        studyTableModel.addRow(studyData);
    }

    /**
     *   Adds the series data to the table.
     *   @param fileInfo File info structure where data is stored.
     */
    private void addSeriesData(FileInfoDicom fileInfo,
                               FileInfoDicom fileInfoRef, int length)
    {
        Object[] seriesData = new Object[7];
        String data = "";

        if (disregardSeries == true)
        {
            return;
        }
        // Study number
        data = (String) fileInfo.getValue("0020,0010");
        if (fileInfoRef == null)
        {
            String studyNo = (String) (studyTableModel.getValueAt(0, 2));
            if (studyNo.equals(data))
            {

            }
            else
            {
                return;
            }
        }
        else
        {
            String studyNo = (String) fileInfoRef.getValue("0020,0010");
            if (studyNo.equals(data))
            {

            }
            else
            {
                return;
            }
        }

        // Series number
        data = (String) fileInfo.getValue("0020,0011");
        for (int i = 0; i < seriesTableModel.getRowCount(); i++)
        {
            String seriesNo = (String) (seriesTableModel.getValueAt(i, 0));
            if (seriesNo.equals(data))
            {
                return;
            }
        }

        if (data != null)
        {
            seriesData[0] = data;
        }
        else
        {
            seriesData[0] = "";
        }

        // Series Type (PET) - why ?
        data = (String) fileInfo.getValue("0054,1000");
        if (data != null)
        {
            seriesData[1] = data;
        }
        else
        {
            seriesData[1] = "";
        }
        seriesData[2] = String.valueOf(length);

        // Series Time
        data = (String) fileInfo.getValue("0008,0031");
        if (data != null)
        {
            seriesData[3] = data;
        }
        else
        {
            seriesData[3] = "";
        }

        // Modality
        data = (String) fileInfo.getValue("0008,0060");
        if (data != null)
        {
            seriesData[4] = data;
        }
        else
        {
            seriesData[4] = "";
        }

        // Series Description
        data = (String) fileInfo.getValue("0008,103E");
        if (data != null)
        {
            seriesData[5] = data;
        }
        else
        {
            seriesData[5] = "";
        }

        seriesData[6] = fileInfo;
        seriesTableModel.addRow(seriesData);
    }

    /**
     * Builds the jmenubar and adds two options.. disregard series #s and
     * exit/close
     */
    private void buildMenu()
    {
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


    /* Initializes GUI components and displays dialog.<p>
     *   For the brightnessSlider the slider values and the brightness values are identical.
     *   brightness is an offset going from -255 to 255.  This is enough to change all 0 values
     *   to 255 and all 255 values to 0.  brightness is added to all contrast scaled red,
     *   green, and blue.<p>
     *   However, for the constrastSlider the slider values are different from the contrast values.
     *   The contrast values go from 0.1 to 10.0 while the slider values go from -200 to 200.
     *   contrast = (float)Math.pow(10.0,contrastSlider.getValue()/200.0)
     *   The original red, green, and blue are mutliplied by contrast.
     *   @param brightness   Initial brightness.
     *   @param contrast     Initial contrast.
     */
    private void buildBrightnessContrastPanel()
    {
        brightnessSlider = new JSlider(JSlider.HORIZONTAL, -255, 255,
                                       origBrightness);

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
        gbc.fill = gbc.HORIZONTAL;

        sliderPanel.add(brightnessSlider, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = gbc.WEST;
        gbc.fill = gbc.NONE;

        sliderPanel.add(minimum, gbc);

        gbc.gridx = 1;
        gbc.anchor = gbc.CENTER;
        gbc.weightx = .5;

        sliderPanel.add(current, gbc);

        gbc.gridx = 2;
        gbc.anchor = gbc.EAST;
        gbc.weightx = 0;

        sliderPanel.add(maximum, gbc);
        sliderPanel.setBorder(buildTitledBorder("Level"));

        contrastSlider = new JSlider(JSlider.HORIZONTAL, -200, 200,
                                     (int) (Math.round(86.85889638 *
                Math.log(origContrast))));

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
        gbc.fill = gbc.HORIZONTAL;

        sliderPanel2.add(contrastSlider, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = gbc.WEST;
        gbc.fill = gbc.NONE;

        sliderPanel2.add(minimum2, gbc);

        gbc.gridx = 1;
        gbc.anchor = gbc.CENTER;
        gbc.weightx = .5;

        sliderPanel2.add(current2, gbc);

        gbc.gridx = 2;
        gbc.anchor = gbc.EAST;
        gbc.weightx = 0;

        sliderPanel2.add(maximum2, gbc);
        sliderPanel2.setBorder(buildTitledBorder("Window"));

        JPanel centerPanel = new JPanel(new GridBagLayout());

        GridBagConstraints gbc2 = new GridBagConstraints();

        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.fill = gbc.BOTH;
        gbc2.weightx = 1;
        gbc2.gridheight = 2;
        centerPanel.add(sliderPanel2, gbc2);

        gbc2.gridy = 2;
        centerPanel.add(sliderPanel, gbc2);

        gbc2.gridheight = 1;
        gbc2.gridy = 4;

        progressPanel = new JPanelProgressBar(0, 100);
        centerPanel.add(progressPanel, gbc2);

        progressPanel.getProgressBar().setBackground(this.getBackground());
        progressPanel.getProgressBar().setForeground(this.getBackground());
        progressPanel.getProgressBar().setBorderPainted(false);
        //progressPanel.getProgressBar().setIndeterminate(true);

        brightnessContrastPanel = new JPanel(new BorderLayout());
        brightnessContrastPanel.add(centerPanel);
        brightnessContrastPanel.setBorder(BorderFactory.createEmptyBorder(5, 5,
                5, 5));

    }

    /**
     *   Sets values based on knob along slider
     *   @param e    Event that triggered this function
     */
    public void stateChanged(ChangeEvent e)
    {
        Object source = e.getSource();

        if (source == brightnessSlider)
        {
            brightness = brightnessSlider.getValue();
            current.setText(String.valueOf(brightness));
            // Change only the brightness and contrast of the current slice
            if (componentImageDicom != null)
            {
                componentImageDicom.setSliceBrightness(brightness, contrast);
            }
        }
        else if (source == contrastSlider)
        {
            contrast = (float) Math.pow(10.0, contrastSlider.getValue() / 200.0);
            current2.setText(String.valueOf(nfc.format(contrast)));
            // Change only the brightness and contrast of the current slice
            if (componentImageDicom != null)
            {
                componentImageDicom.setSliceBrightness(brightness, contrast);
            }
        }
    }

    /**
     *   Builds a toolbar with the same functionality as the menu.
     */
    protected JPanel buildToolbar()
    {
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

        JButton movieButton = new JButton(MipavUtil.getIcon(
                "movieextraction.gif"));

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

        JButton configureColumnsButton = new JButton(MipavUtil.getIcon(
                "configurecolumns.gif"));

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

        JButton selectAllRowsButton = new JButton(MipavUtil.getIcon(
                "selectallrows.gif"));

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
     *   Table listener - listens for clicks on any of the three
     *   table headers or clicks within each table.
     */
    private class TableListener implements MouseListener
    {

        /**
         *   When the user clicks on a header, sorts the column.
         *   When the user clicks on a study or series, parse.
         *   When the user clicks on an image, displays a thumbnail.
         *   @param e    Event that triggered this function.
         */
        public void mouseClicked(MouseEvent e)
        {
            Object source = e.getSource();

            if (source.equals(studyTable))
            {
                // this functionality temporarily disabled
                /*
                FileInfoDicom fileInfo = (FileInfoDicom) studyTableModel.getValueAt(
                        studyTable.getSelectedRow(), 5);

                seriesTableModel.setRowCount(0);
                parse((new File(fileInfo.getFileDirectory()).getParentFile()), false, true, fileInfo);
                */
            }
            else if (source.equals(seriesTable))
            {
                FileInfoDicom fileInfo = (FileInfoDicom) seriesTableModel.getValueAt(seriesTableSorter.modelIndex(seriesTable.getSelectedRow()), 6);

                parse(new File(fileInfo.getFileDirectory()), true, false, fileInfo);
            }
            else if (source.equals(imageTable))
            {
                int modelRow = imageTableSorter.modelIndex(imageTable.getSelectedRow());

                FileInfoDicom fileInfoDICOM = (FileInfoDicom) fileInfoVector.elementAt(modelRow);

                buildImage(fileInfoDICOM.getFileName(),  fileInfoDICOM.getFileDirectory() + File.separatorChar);
                componentImageDicom = img;
                componentImageDicom.setSliceBrightness(brightness, contrast);
            }
        }


        /** Unchanged */
        public void mouseDragged(MouseEvent e)
        {}

        /** Unchanged */
        public void mouseReleased(MouseEvent e)
        {}

        /** Unchanged */
        public void mouseExited(MouseEvent e)
        {}

        /** Unchanged */
        public void mouseEntered(MouseEvent e)
        {}


        public void mousePressed(MouseEvent event)
        {
        }
    }


    // ************************************************************************
    // **************************** Window Events *****************************
    // ************************************************************************

    /**
     *  windowOpened - unchanged
     */
    public void windowOpened(WindowEvent event)
    {}

    /**
     *  windowClosing - calls close
     *  @param event    event that triggered function
     */
    public void windowClosing(WindowEvent event)
    {
        this.close();

        dispose();
    }

    /**
     *  windowClosed - unchanged
     */
    public void windowClosed(WindowEvent event)
    {
    }

    /**
     *  windowIconified - unchanged
     */
    public void windowIconified(WindowEvent event)
    {}

    /**
     *  windowDeiconified - unchanged
     */
    public void windowDeiconified(WindowEvent event)
    {}

    /**
     *  windowActivated - unchanged
     */
    public void windowActivated(WindowEvent event)
    {
    }

    /**
     *  windowDeactivated - unchanged
     */
    public void windowDeactivated(WindowEvent event)
    {}

    /**
     * The purpose of this method is to determine whether the parameter represents one of the special
     * table columns. In this case, X-position, Y-position, and Z-position are special columns because
     * they are not standard DICOM tags. They are parsed out from the Patient Orientation tag and
     * populated manually.
     *
     * @param columnName String
     * @return boolean
     */
    public static boolean isCompositeXYZPositionColumn(String columnName)
    {
        if (columnName.equals("X-position") || columnName.equals("Y-position") || columnName.equals("Z-position"))
        {
            return true;
        }

        return false;
    }

}
