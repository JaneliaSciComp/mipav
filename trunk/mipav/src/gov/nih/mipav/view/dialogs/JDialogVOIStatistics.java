package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.event.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;


/**
 * Dialog for calculating statistics of a (set of) Volumes of Interest. User selects a Volume of Interest (or more than
 * one) to act on, selects the statistics from a selectable checklist panel. The User selects "Calculate"; the output
 * from the statistics is reported in a "log-panel" and may be sent to a file.
 *
 * <p>$Logfile: /mipav/src/gov/nih/mipav/view/dialogs/JDialogVOIStatistics.java $</p>
 */

public class JDialogVOIStatistics extends JDialogBase
        implements AlgorithmInterface, VOIStatisticList, VOIVectorListener, ScriptableInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1354965054401181781L;

    /** DOCUMENT ME! */
    public static final int NO_RANGE = -1;

    /** DOCUMENT ME! */
    public static final int BETWEEN = 0;

    /** DOCUMENT ME! */
    public static final int OUTSIDE = 1;

    /** VOI Tab. */
    private static final int VOI_TAB = 0;

    /** Statistics Tab. */
    private static final int STAT_TAB = 1;

    /** Logging Tab. */
    private static final int LOG_TAB = 2;

    /** File handler output mode - write. */
    private static final int WRITE = 0;

    /** File handler output mode - append. */
    private static final int APPEND = 1;

    /** File handler output mode - overwrite. */
    private static final int OVERWRITE = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Panel to push/pull VOIs from full list to selectable list. */
    private JPanelAddRemoveVOI addremove;

    /** Panel holding statistics options. */
    private JPanelStatisticsList checkBoxPanel;

    // onscreen objects
    /** Tabbed pane that holds all components. */
    private JTabbedPane everything;

    /** Panel to select files. */
    private JPanelFileSelection fileSelectorPanel;

    /** Operator to provide listener access... could be done by /this/ */
    private VOIHighlighter highlighter;

    /** Global image reference to the currently active image during script running and recording. */
    private ModelImage image;

    /** The text of the log process to be written to file. */
    private StringBuffer logFileText;

    /** Model where data is stored for the table. */
    private ViewTableModel logModel;

    /** Table holding log of statistics calculated. */
    private JTable logTable;

    /** Flag where <code>true</code> means notifying the user of stupid errors. */
    private boolean noisyProcess = true;

    /** Panel holding statistics output options. */
    private JPanelStatisticsOptions outputOptionsPanel;

    /** Check box item indicating if we're to always overwrite the statistics file. */
    private JCheckBoxMenuItem overwrite;

    /** DOCUMENT ME! */
    private int rangeFlag = NO_RANGE;

    /** List of selected VOIs. */
    private JList selectedList = new JList();

    // directory file objects
    /** Log file destination. */
    private File tableDestination = null;

    /** Indicates options for WRITE, OVERWRITE, APPEND for the xlat file. */
    private int tableDestinationUsage;

    /** Default filename for the statistic table. */
    private String tableName = "voi.statistics.table";

    /** Toolbar. */
    private JToolBar toolBar;

    /** Flag for top level. */
    private boolean toplevelOnly = false;

    /** Icon and log access. */
    private ViewUserInterface ui;

    // actual things we can see...
    /** List of available VOIs. */
    private JList volumesList = new JList();

    /** DOCUMENT ME! */
    private int xUnits, yUnits, zUnits;

    /** Number of digits after decimal place to allow*/
    private int precision = 4;

    /** force precision to display at maximum */
    private boolean doForce = false;


    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogVOIStatistics() {
        this.addWindowListener(new WindowAdapter() {
                public void windowClosing(WindowEvent event) {
                    cleanUpAndDispose();
                }
            });
    }

    /**
     * builds and packs the frame. does <i>not</I> set it visible.
     *
     * <p>install the panels of source directory, destination directory, the checkbox for approving the
     * translation-table file and the panel containing the ok and cancel buttons. Installs the checkbox panel.</p>
     *
     * @param  ui  DOCUMENT ME!
     */
    public JDialogVOIStatistics(ViewUserInterface ui) {
        this(ui, ui.getActiveImageFrame().getComponentImage().getActiveImage().getVOIs());
    }

    /**
     * builds and packs the frame. does <i>not</I> set it visible.
     *
     * <p>install the panels of source directory, destination directory, the checkbox for approving the
     * translation-table file and the panel containing the ok and cancel buttons. Installs the checkbox panel.</p>
     *
     * @param  ui       DOCUMENT ME!
     * @param  voiList  DOCUMENT ME!
     */
    public JDialogVOIStatistics(ViewUserInterface ui, VOIVector voiList) {
        super(ui.getMainFrame(), false);
        setTitle("Calculate Statistics on VOI groups");
        setJMenuBar(buildMenuEntries());
        buildToolBar();
        this.ui = ui;
        image = ui.getActiveImageFrame().getComponentImage().getActiveImage();
        xUnits = image.getFileInfo(0).getUnitsOfMeasure()[0];
        yUnits = image.getFileInfo(0).getUnitsOfMeasure()[1];
        zUnits = FileInfoBase.UNKNOWN_MEASURE;
        if (image.getNDims() > 2) {
            zUnits = image.getFileInfo(0).getUnitsOfMeasure()[2];
        }
        // need to take out line VOIs, polyline VOIs, point VOIs

        everything = new JTabbedPane(JTabbedPane.TOP);
        everything.setFont(MipavUtil.font12B);
        everything.insertTab("VOI selection", null, buildVOIPanel(voiList), // we must store this panel so we can
                                                                            // create a new listing later
                             "Choose VOIs and statistics file", VOI_TAB);

        JPanel statPanel = new JPanel(new BorderLayout());
        checkBoxPanel = new JPanelStatisticsList();
        outputOptionsPanel = new JPanelStatisticsOptions();

        if (ui.getActiveImageFrame().getComponentImage().getActiveImage().getNDims() == 2) {
            outputOptionsPanel.setBySliceEnabled(false);
        }

        statPanel.add(outputOptionsPanel, BorderLayout.EAST);
        statPanel.add(checkBoxPanel, BorderLayout.CENTER);
        everything.insertTab("Statistics Options", null, statPanel, "Statistic Selection", STAT_TAB);

        everything.insertTab("Logging", null, buildLogPanel(), "Output Log", LOG_TAB);

        getContentPane().add(toolBar, BorderLayout.NORTH);
        getContentPane().add(everything, BorderLayout.CENTER);
        getContentPane().add(buildOKCancelPanel(), BorderLayout.SOUTH); // build OK/Cancel button Panel

        pack();
        setSize(800, 500); // decent size??

        this.addWindowListener(new WindowAdapter() {
                public void windowClosing(WindowEvent event) {
                    cleanUpAndDispose();
                }
            });
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * when a button is clicked.
     *
     * @param  ae  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent ae) {
        Object source = ae.getSource();
        String command = ae.getActionCommand();

        if (command.equals("clear log")) {
            logModel.setRowCount(0);
            logModel.setColumnCount(0);
        } else if (command.equals("overwrite")) {
            Preferences.setProperty("OverwriteStatistics", String.valueOf(overwrite.isSelected()));
        } else if (source == OKButton) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (source == cancelButton) {
            cleanUpAndDispose();
        }
    }

    /**
     * resets the volumes list to the current VOIVector. adds the highlighter to the new VOI.
     *
     * @param  voiEvent  DOCUMENT ME!
     */
    public void addedVOI(VOIVectorEvent voiEvent) {
        VOIVector voiList = (VOIVector) voiEvent.getSource();
        Vector volumesVector = new Vector();

        for (int i = 0; i < voiList.size(); i++) {

            if (voiList.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                volumesVector.add(voiList.elementAt(i));
            }
        }

        volumesList.setListData(volumesVector);
        voiEvent.getVOI().addVOIListener(highlighter);
    }

    /**
     * the standard thread-done event for <code>AlgorithmBase.</code>
     *
     * @param  event  the event
     */
    public void algorithmPerformed(AlgorithmBase event) {
        int totalCount = 0;
        String str;

        // notification will turn buttons back on
        cancelButton.setEnabled(true);
        OKButton.setEnabled(true);

        // get output data out of the notifier
        // getStatisticsData((AlgorithmVOIProps)event);
        AlgorithmVOIProps calculator = (AlgorithmVOIProps) event;
        VOIStatisticalProperties properties;
        Vector[] contours;

        if (calculator.isCalculatedBySlice() || calculator.isCalculatedByContour()) {
            ListModel list = selectedList.getModel();

            if (logModel.getColumnIndex("Name, Slice, Contour") == -1) {
                logModel.addColumn("Name, Slice, Contour");
            }

            for (int i = 0; i < VOIStatisticList.numberOfStatistics; i++) {

                if (checkBoxPanel.getSelectedList(VOIStatisticList.statisticDescription[i])) {

                    if (logModel.getColumnIndex(VOIStatisticList.statisticDescription[i]) == -1) {
                        if ((VOIStatisticList.statisticDescription[i].indexOf("Volume") != -1) && (xUnits == yUnits) &&
                                (xUnits == zUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = image.getFileInfo(0).getVolumeUnitsOfMeasureStr();
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                        } else if ((VOIStatisticList.statisticDescription[i].indexOf("Area") != -1) &&
                                       (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = image.getFileInfo(0).getAreaUnitsOfMeasureStr();
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                        } else if ((VOIStatisticList.statisticDescription[i].indexOf("Perimeter") != -1) &&
                                       (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                        } else if (VOIStatisticList.statisticDescription[i].indexOf("Principal Axis") != -1) {
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (degrees)");
                        } else if ((VOIStatisticList.statisticDescription[i].indexOf("Major axis length") != -1) &&
                                       (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                        } else if ((VOIStatisticList.statisticDescription[i].indexOf("Minor axis length") != -1) &&
                                       (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                        } else {
                            logModel.addColumn(VOIStatisticList.statisticDescription[i]);
                        }
                    }

                    // total count used for total # of data elemets, need to add 3 if color
                    // image and intensity related (R,G,B)
                    totalCount++;

                    if (calculator.isColor() && (VOIStatisticList.statisticDescription[i].indexOf("Intensity") != -1)) {
                        totalCount += 2;
                    }
                }
            }

            // for each element in the list ....
            for (int i = 0; i < list.getSize(); i++) {
                properties = calculator.getVOIProperties((VOI) list.getElementAt(i));
                contours = ((VOI) list.getElementAt(i)).getCurves();

                String[] rowData = new String[logModel.getColumnCount()];
                String[] totalData = new String[logModel.getColumnCount()];

                String[] logRowData = new String[rowData.length];
                String[] logTotalData = new String[rowData.length];

                for (int slice = 0; slice < contours.length; slice++) {
                    int count = 0;
                    int stop = 1;
                    String end = slice + ";";

                    if (calculator.isCalculatedByContour()) {
                        stop = contours[slice].size();
                    }

                    if (contours[slice].size() < 1) {
                        stop = 0;
                    }

                    // for each contour only print titles and calculations once,
                    // if not "calculate by contour" (ie., if we only want totals):
                    for (int num = 0; num < stop; num++) {

                        // first: set up row title:
                        rowData[0] = list.getElementAt(i).toString() + ", " + // VOI name
                                     (slice + 1) + ", " + // slice #, irrellevent to where contour is in image
                                     ((VOIBase) contours[slice].get(num)).getLabel(); // contour #, held in label
                        totalData[0] = "Totals:";

                        logRowData[0] = new String(rowData[0]);
                        logTotalData[0] = new String(totalData[0]);

                        if (calculator.isCalculatedByContour()) {
                            end = slice + ";" + num;
                        }

                        // for each column in the row, print the statistic:
                        for (int k = 0; k < statisticDescription.length; k++) {
                            if (logModel.getColumnBaseIndex(VOIStatisticList.statisticDescription[k]) != -1) {
                                count++;
                            }

                            if (checkBoxPanel.getSelectedList(properties.statisticDescription[k])) {

                                // if it's a color image and the property is min intensity, max intensity, avg
                                // intensity, or standard deviation of intensity, those properties were entered as Red,
                                // Green, Blue and we should display them differently.
                                if (calculator.isColor() &&
                                        (properties.statisticDescription[k].indexOf("Intensity") != -1)) {
                                    String temp = "R: " + properties.getProperty(statisticDescription[k] + "Red" + end);
                                    temp += " G: " + properties.getProperty(statisticDescription[k] + "Green" + end);
                                    temp += " B: " + properties.getProperty(statisticDescription[k] + "Blue" + end);
                                    rowData[count] = temp;
                                    logRowData[count] = temp;

                                    if (outputOptionsPanel.isShowTotals()) {
                                        temp = " R: " + properties.getProperty(statisticDescription[k] + "RedTotal");
                                        temp += " G: " + properties.getProperty(statisticDescription[k] + "GreenTotal");
                                        temp += " B: " + properties.getProperty(statisticDescription[k] + "BlueTotal");
                                        totalData[count] = temp;
                                        logTotalData[count] = temp;
                                    }
                                } else {

                                    rowData[count] = properties.getProperty(statisticDescription[k] + end).replaceAll("\t", ", ");
                                    logRowData[count] = properties.getProperty(statisticDescription[k] + end);

                                    if (outputOptionsPanel.isShowTotals()) {
                                        totalData[count] = properties.getProperty(statisticDescription[k] + "Total").replaceAll("\t", ", ");
                                        logTotalData[count] = properties.getProperty(statisticDescription[k] + "Total");
                                    }
                                }
                            }
                        } // end for each column

                        count = 0;
                        logModel.addRow(rowData);

                        String logText = "";

                        for (int j = 0; j < rowData.length; j++) {
                            logText += logRowData[j] + "\t";
                        }


                        writeLogfileEntry(logText);
                    } // end for contours
                }

                if (outputOptionsPanel.isShowTotals()) {
                    logModel.addRow(totalData);
                    String logText = "";
                    for (int j = 0; j < logTotalData.length; j++) {
                        logText += logTotalData[j] + "\t";
                    }
                    writeLogfileEntry(logText);
                }

                for (int k = 0; k < rowData.length; k++) {
                    rowData[k] = "";
                }

                logModel.addRow(rowData);
            }
        } else { // whole 3D VOI data

            ListModel list = selectedList.getModel();

            if (logModel.getColumnIndex("Name, Slice, Contour") == -1) {
                logModel.addColumn("Name, Slice, Contour");
            }

            // add any columns which will be displayed, but not already displayed:
            for (int i = 0; i < VOIStatisticList.numberOfStatistics; i++) {

                if (checkBoxPanel.getSelectedList(VOIStatisticList.statisticDescription[i])) {

                    if (logModel.getColumnIndex(VOIStatisticList.statisticDescription[i]) == -1) {
                        if ((VOIStatisticList.statisticDescription[i].indexOf("Volume") != -1) && (xUnits == yUnits) &&
                                (xUnits == zUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = image.getFileInfo(0).getVolumeUnitsOfMeasureStr();
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                        } else if ((VOIStatisticList.statisticDescription[i].indexOf("Area") != -1) &&
                                       (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = image.getFileInfo(0).getAreaUnitsOfMeasureStr();
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                        } else if ((VOIStatisticList.statisticDescription[i].indexOf("Perimeter") != -1) &&
                                       (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                        } else if (VOIStatisticList.statisticDescription[i].indexOf("Principal Axis") != -1) {
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (degrees)");
                        } else if ((VOIStatisticList.statisticDescription[i].indexOf("Major axis length") != -1) &&
                                       (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                        } else if ((VOIStatisticList.statisticDescription[i].indexOf("Minor axis length") != -1) &&
                                       (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                        } else {
                            logModel.addColumn(VOIStatisticList.statisticDescription[i]);
                        }
                    }

                    // total count used for total # of data elemets, need to add 3 if color image and intensity related
                    // (R,G,B)
                    totalCount++;

                    if (calculator.isColor() && (VOIStatisticList.statisticDescription[i].indexOf("Intensity") != -1)) {
                        totalCount += 2;
                    }
                }
            }

            // for each element in the list print properties of each VOI,
            // column-by-column:
            for (int i = 0; i < list.getSize(); i++) {
                properties = calculator.getVOIProperties((VOI) list.getElementAt(i));
                contours = ((VOI) list.getElementAt(i)).getCurves();

                String[] rowData = new String[logModel.getColumnCount()];
                rowData[0] = list.getElementAt(i).toString();

                int count = 0;

                for (int k = 0; k < statisticDescription.length; k++) {

                    if (logModel.getColumnBaseIndex(VOIStatisticList.statisticDescription[k]) != -1) {
                        count++;
                    }

                    if (checkBoxPanel.getSelectedList(properties.statisticDescription[k])) {

                        // if it's a color image and the property is min intensity, max intensity, avg intensity,
                        // or standard deviation of intensity, those properties were entered as Red, Green, Blue and
                        // we should display them differently.
                        if (calculator.isColor() && (properties.statisticDescription[k].indexOf("Intensity") != -1)) {
                            String temp = "R: " + properties.getProperty(statisticDescription[k] + "Red");
                            temp += " G: " + properties.getProperty(statisticDescription[k] + "Green");
                            temp += " B: " + properties.getProperty(statisticDescription[k] + "Blue");
                            rowData[count] = temp;
                        } else {
                            rowData[count] = properties.getProperty(statisticDescription[k]);
                        }
                    }
                } // end for each column

                count = 0;
                logModel.addRow(rowData);

                String logText = "";

                for (int j = 0; j < rowData.length; j++) {
                    logText += rowData[j] + "\t";
                }

                writeLogfileEntry(logText);

                for (int k = 0; k < rowData.length; k++) {
                    rowData[k] = "";
                }

                logModel.addRow(rowData);
            }
        }

        // finalise the output details
        writeStatisticFile();
        insertScriptLine(event);
        System.gc(); // to reclaim lost land.
    }


    /**
     * The method is called by the scriptRun(). It initializes variables in order to run the script. The method should
     * function as the constructor, however, the script calling mechanism force it can not be invoked at the beginning.
     * So, scriptRun() call this method directly. More work need to be done for the calling mechanism in the future.
     *
     * @param  ui             ViewUserInterface
     * @param  runFromScript  boolean
     */
    public void createDialog(ViewUserInterface ui, boolean runFromScript) {

        // super(ui.getMainFrame(), false);
        setTitle("Calculate Statistics on VOI groups");
        setJMenuBar(buildMenuEntries());
        this.ui = ui;
        // need to take out line VOIs, polyline VOIs, point VOIs

        // initialize the variables.
        VOIVector voiList = ui.getActiveImageFrame().getComponentImage().getActiveImage().getVOIs();

        everything = new JTabbedPane(JTabbedPane.TOP);
        everything.setFont(MipavUtil.font12B);
        everything.insertTab("VOI selection", null, buildVOIPanel(voiList), // we must store this panel so we can
                                                                            // create a new listing later
                             "Choose VOIs and statistics file", VOI_TAB);

        JPanel statPanel = new JPanel(new BorderLayout());
        checkBoxPanel = new JPanelStatisticsList();
        outputOptionsPanel = new JPanelStatisticsOptions();
        image = ui.getActiveImageFrame().getComponentImage().getActiveImage();

        if (ui.getActiveImageFrame().getComponentImage().getActiveImage().getNDims() == 2) {
            outputOptionsPanel.setBySliceEnabled(false);
        }

        statPanel.add(outputOptionsPanel, BorderLayout.EAST);
        statPanel.add(checkBoxPanel, BorderLayout.CENTER);
        everything.insertTab("Statistics Options", null, statPanel, "Statistic Selection", STAT_TAB);

        everything.insertTab("Logging", null, buildLogPanel(), "Output Log", LOG_TAB);

        getContentPane().add(everything, BorderLayout.CENTER);
        getContentPane().add(buildOKCancelPanel(), BorderLayout.SOUTH); // build OK/Cancel button Panel

        selectedList.setListData(voiList);

        pack();
        setSize(800, 500); // decent size??
    }

    /**
     * Construct a delimited string that contains the parameters to this algorithm.
     *
     * @param   delim  the parameter delimiter (defaults to " " if empty)
     *
     * @return  the parameter string
     */
    public String getParameterString(String delim) {

        if (delim.equals("")) {
            delim = " ";
        }

        String str = new String();
        boolean[] checkList = checkBoxPanel.getSelectedList();

        // Check box selection list
        str += checkList[0] + delim; // # of voxesl
        str += checkList[1] + delim; // volume
        str += checkList[2] + delim; // Area
        str += checkList[3] + delim;
        str += checkList[4] + delim; // min intensity
        str += checkList[5] + delim; // max intensity
        str += checkList[6] + delim; // Avg Voxel intensity
        str += checkList[7] + delim; // Std dev of intensity
        str += checkList[8] + delim; // Center of Mass
        str += checkList[9] + delim; // Principle Axis
        str += checkList[10] + delim; // Eccentricity
        str += checkList[11] + delim; // Major axis length
        str += checkList[12] + delim; // Minor axis length

        // radio button selection
        if (outputOptionsPanel.isByVOI()) {
            str += 0 + delim;
        } else if (outputOptionsPanel.isByContour()) {
            str += 1 + delim;
        } else if (outputOptionsPanel.isBySlice()) {
            str += 2 + delim;
        }

        // show all voxels
        str += outputOptionsPanel.isShowTotals() + delim;

        // Pixel Exclusion
        str += rangeFlag + delim; // rangeFlag;

        if (rangeFlag != NO_RANGE) {
            str += outputOptionsPanel.getMinimumExclude().floatValue() + delim; // min exclude value
            str += outputOptionsPanel.getMaximumExclude().floatValue(); // max exclude value
        }

        return str;
    }


    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     *
     * @param  algo  the algorithm to make an entry for
     */
    public void insertScriptLine(AlgorithmBase algo) {

        if (algo.isCompleted()) {

            if (ui.isScriptRecording()) {

                // check to see if the match image is already in the ImgTable
                if (ui.getScriptDialog().getImgTableVar(image.getImageName()) == null) {

                    if (ui.getScriptDialog().getActiveImgTableVar(image.getImageName()) == null) {
                        ui.getScriptDialog().putActiveVar(image.getImageName());
                    }
                }

                String line = "VOIStatistics " + ui.getScriptDialog().getVar(image.getImageName()) + " " +
                              getParameterString(" ") + "\n";
                ;
                ui.getScriptDialog().append(line);
            }
        }
    }

    /**
     * resets the volumes list to the current VOIVector. removes the highlighter from the removed VOI.
     *
     * @param  voiEvent  DOCUMENT ME!
     */
    public void removedVOI(VOIVectorEvent voiEvent) {
        VOIVector voiList = (VOIVector) voiEvent.getSource();
        Vector volumesVector = new Vector();

        for (int i = 0; i < voiList.size(); i++) {

            if (voiList.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                volumesVector.add(voiList.elementAt(i));
            }
        }

        volumesList.setListData(volumesVector);

        /* we cannot delete VOIs out of the selected VOI list if there are no more
         * VOIs in the image. -- voiEvent.getSource() is null when 'removeAll' is called. getVOI is to return,
         * specifically, the new VOI that has changed.  Since all VOIs are now null, we must recognise that the VOI that
         * is new is the empty VOI.  (one of these a-ha! moments.  silly comments left undone.)
         */
        if (voiEvent.getVOI() == null) {
            return;
        }

        if (voiEvent.getVOI().getCurveType() == VOI.CONTOUR) {
            voiEvent.getVOI().removeVOIListener(highlighter);

            // ensuring that the selected VOI is not in
            // the selected list.
            selectedList.setSelectedValue(voiEvent.getVOI(), true);
            addremove.performDelete();
        }
    }

    /**
     * Run this algorithm from a script.
     *
     * @param   parser  the script parser we get the state from
     *
     * @throws  IllegalArgumentException  if there is something wrong with the arguments in the script
     */
    public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException {
        setScriptRunning(true);

        String srcImageKey = null;

        try {
            srcImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        ModelImage im = parser.getImage(srcImageKey);

        image = im;
        ui = image.getUserInterface();
        parentFrame = image.getParentFrame();

        createDialog(ui, true);

        // parse script
        try {
            boolean[] checkList = new boolean[13];

            for (int i = 0; i < 13; i++) {
                checkList[i] = parser.getNextBoolean();
            }

            checkBoxPanel.setSelectedList(checkList);

            outputOptionsPanel.setOutputType(parser.getNextInteger());
            outputOptionsPanel.setShowTotals(parser.getNextBoolean());

            rangeFlag = parser.getNextInteger();

            if (rangeFlag != NO_RANGE) {
                outputOptionsPanel.setMinimumExclude(parser.getNextString());
                outputOptionsPanel.setMaximumExclude(parser.getNextString());
            }
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        setSeparateThread(false);
        overwrite.setSelected(true);

        if (setVariables()) {
            callAlgorithm();
        }
    }

    /**
     * un-implemented.
     *
     * @param  voiEvent  DOCUMENT ME!
     */
    public void vectorSelected(VOIVectorEvent voiEvent) { }

    /**
     * Clean up some things done by the dialog which may affect other parts of MIPAV.
     */
    protected void cleanUpAndDispose() {

        // make sure all voi listeners are removed from the image's list, if that isn't done problems happen when
        // the image is serialized (e.g. if it's cloned)
        for (int i = 0; i < image.getVOIs().size(); i++) {
            image.getVOIs().VOIAt(i).removeVOIListener(highlighter);
        }
        image.getVOIs().removeVectorListener(this);

        dispose();
    }

    /**
     * creates a panel for the output log.
     *
     * @return  DOCUMENT ME!
     */
    private JPanel buildLogPanel() {
        JPanel logpan = new JPanel(new BorderLayout());

        logModel = new ViewTableModel();
        logTable = new JTable(logModel);
        logTable.setFont(MipavUtil.font12);

        JScrollPane lpane = new JScrollPane(logTable);
        lpane.getVerticalScrollBar().addAdjustmentListener(new ScrollCorrector());
        logpan.add(lpane, BorderLayout.CENTER);

        return logpan;
    }

    /**
     * Builds a small menu with "Clear log" and "Overwrite" options.
     *
     * @return  DOCUMENT ME!
     */
    private JMenuBar buildMenuEntries() {
        JMenuBar anonBar = new JMenuBar();
        JMenu entry = new JMenu("Options");
        JMenuItem logClear = new JMenuItem("Clear Log Window");
        overwrite = new JCheckBoxMenuItem("Overwrite file automatically");

        entry.setFont(MipavUtil.font12B);
        entry.setMnemonic(KeyEvent.VK_E);
        logClear.setFont(MipavUtil.font12B);
        logClear.setActionCommand("clear log");
        logClear.setAccelerator(KeyStroke.getKeyStroke('C', java.awt.Event.ALT_MASK));
        logClear.setMnemonic(KeyEvent.VK_C);
        logClear.addActionListener(this);

        overwrite.setFont(MipavUtil.font12B);
        overwrite.setActionCommand("overwrite");
        overwrite.setAccelerator(KeyStroke.getKeyStroke('O', java.awt.Event.ALT_MASK));
        overwrite.setMnemonic(KeyEvent.VK_C);
        overwrite.setSelected(Preferences.is(Preferences.PREF_OVERWRITE_VOI_STATS));
        overwrite.addActionListener(this);

        entry.add(logClear);
        entry.add(overwrite);
        anonBar.add(entry);

        return anonBar;
    }

    /**
     * creates the panel which consists of the OKAY button and the Cancel button.
     *
     * @return  DOCUMENT ME!
     */
    private JPanel buildOKCancelPanel() {
        JPanel ocp = new JPanel(); // flow layout

        buildOKButton();
        OKButton.setText("Calculate");
        ocp.add(OKButton);

        buildCancelButton();
        cancelButton.setText("Close");
        ocp.add(cancelButton);

        return ocp;
    }

    /**
     * creates the visual display in which to list all selected directories in the directory tree. The panel is 240
     * pixels wide though that is <i>supposed</i> to be the minimum size
     *
     * @return  the panel which is to hold the list of selected items
     */
    private JPanel buildSelectedListing() {

        // define an outside panel to hold all these components.
        JPanel selp = new JPanel(new BorderLayout());
        selp.add(Box.createHorizontalStrut(240), BorderLayout.NORTH); // width of text area.  seems to start out very
                                                                      // skinny.

        // this list to hold things so that they may be selectable/removable
        // panel to hold list access.
        selectedList.setListData(new Vector()); // = new JList();
        selp.add(new JScrollPane(selectedList), BorderLayout.CENTER);

        // build default arrowpanel
        if (addremove == null) {
            addremove = new JPanelAddRemoveVOI();
            addremove.setLeftList(volumesList);
            addremove.setRightList(selectedList);

            volumesList.addListSelectionListener(addremove);
            selectedList.addListSelectionListener(addremove);

            selp.add(addremove, BorderLayout.WEST);
        }

        return selp;
    }

    /**
     * Creates the panel holding the directory tree.
     *
     * @param   VOIlist  DOCUMENT ME!
     *
     * @return  Panel.
     */
    private JPanel buildSourceListingPanel(VOIVector VOIlist) {
        JPanel srctreep = new JPanel(new BorderLayout());
        Vector volumesVector = new Vector();
        highlighter = new VOIHighlighter();

        for (int i = 0; i < VOIlist.size(); i++) {

            if (VOIlist.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                volumesVector.add(VOIlist.elementAt(i));

                // add a listener to each VOI so we know about selection.
                VOIlist.VOIAt(i).addVOIListener(highlighter);
            }
        }

        volumesList.setListData(volumesVector);
        volumesList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        volumesList.addListSelectionListener(highlighter);

        JScrollPane jsp = new JScrollPane(volumesList);
        srctreep.add(jsp, BorderLayout.CENTER);

        // now let's listen to this vector:
        VOIlist.addVectorListener(this);

        return srctreep;
    }

    /**
     * creates the source panel which consists of the directory line, the browse button, and a check box approving the
     * anonymize in sub-directories.
     *
     * @param   VOIlist  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private JPanel buildSourcePanel(VOIVector VOIlist) {
        JPanel srcp = new JPanel(new GridLayout(1, 2));
        srcp.setBorder(buildTitledBorder("VOI group list"));

        srcp.add(buildSourceListingPanel(VOIlist), BorderLayout.CENTER); // list of VOIs in the image.
        srcp.add(buildSelectedListing(), BorderLayout.EAST); // list of selected items

        return srcp;
    }

    /**
     * Build the toolbar control.
     */
    private void buildToolBar() {
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.gridx = 0;
        gbc.gridy = 0;

        Border etchedBorder = BorderFactory.createEtchedBorder();

        toolBar = new JToolBar();
        toolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        toolBar.setBorder(etchedBorder);
        toolBar.setFloatable(false);

        JButton eraserButton = new JButton(MipavUtil.getIcon("eraser.gif"));

        eraserButton.addActionListener(this);
        eraserButton.setToolTipText("Clear log window");
        eraserButton.setActionCommand("clear log");
        eraserButton.setBorderPainted(false);
        eraserButton.setRolloverEnabled(true);
        eraserButton.setRolloverIcon(MipavUtil.getIcon("eraserroll.gif"));
        eraserButton.setFocusPainted(false);
        eraserButton.setEnabled(true);
        toolBar.add(eraserButton);

        toolBar.add(ViewToolBarBuilder.makeSeparator());

    }


    /**
     * creates the source panel for the VOI tab which consists of the directory line, the browse button, and a check box
     * approving the anonymize in sub-directories. Also includes the file-format selection for the output file.
     *
     * @param   VOIlist  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private JPanel buildVOIPanel(VOIVector VOIlist) {
        JPanel imagePanel = new JPanel(new BorderLayout());

        // we must store sourcePanel so we can create a new directory listing later
        imagePanel.add(buildSourcePanel(VOIlist), BorderLayout.CENTER);

        JPanel destinationsPanel = new JPanel(new BorderLayout());

        fileSelectorPanel = new JPanelFileSelection(new File(image.getFileInfo(0).getFileDirectory() + File.separator +
                                                             image.getImageName() + ".table"),
                                                    "VOI Statistic File Destination");

        destinationsPanel.add(fileSelectorPanel, BorderLayout.CENTER);

        JPanel fileFormatPanel = new JPanelStatisticFileFormatOptions();
        destinationsPanel.add(fileFormatPanel, BorderLayout.SOUTH);

        JPanel lowerPanel = new JPanel(new BorderLayout());
        lowerPanel.add(destinationsPanel, BorderLayout.CENTER);

        // lowerPanel.add(buildRandSelectionPanel(), BorderLayout.EAST);
        imagePanel.add(lowerPanel, BorderLayout.SOUTH);

        return imagePanel;
    }

    /**
     * Once all the necessary variables are set, call the VOI Props algorithm to run the statistic calculation.
     */
    private void callAlgorithm() {
        AlgorithmVOIProps da = new AlgorithmVOIProps(ui.getActiveImageFrame().getComponentImage().getActiveImage(),
                                                     checkBoxPanel, outputOptionsPanel.isBySlice(), rangeFlag);
        da.setPrecisionDisplay(precision, doForce);
        da.addListener(this);

        da.setVOIList(selectedList.getModel());
        da.setPerContour(outputOptionsPanel.isByContour());
        da.setPerSlice(outputOptionsPanel.isBySlice());
        da.setShowTotals(outputOptionsPanel.isShowTotals());
        // da.addTextUpdateListener(this);   // unimplemented -- meant to permit messaging between running thread and
        // this' logging pane.

        // notification will turn buttons back on
        everything.setSelectedIndex(LOG_TAB);
        cancelButton.setEnabled(false);
        OKButton.setEnabled(false);
        da.setRunningInSeparateThread(false);

        if (!ui.isAppFrameVisible()) {
            da.setProgressBarVisible(false);
        }

        da.run();
    }


    /**
     * creates a new keylog, writing which tags are to be removed from the image information; the table header for the
     * image read/write logging is added. the string created here is not automatically turned into the keylog string.
     * that must be done by the caller.
     *
     * @return  the new KeyLog String.
     */
    private String createNewLogfile() {
        int i;
        String kl = "#\tMIPAV will alter.  Conducting statistical computation.\n";
        String str;

        // output the labels of the list of statistics to be produced.
        String[] checklist = checkBoxPanel.makeCheckboxLabels();
        kl += "Name, Slice, Contour\t";

        for (i = 0; i < checklist.length; i++) {
            if (checkBoxPanel.getSelectedList(i)) {
                if ((checklist[i].equals("Volume")) && (xUnits == yUnits) && (xUnits == zUnits) &&
                        (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                    str = image.getFileInfo(0).getVolumeUnitsOfMeasureStr();
                    kl += checklist[i] + " (" + str + ")" + "\t";
                } else if ((checklist[i].equals("Area")) && (xUnits == yUnits) &&
                               (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                    str = image.getFileInfo(0).getAreaUnitsOfMeasureStr();
                    kl += checklist[i] + " (" + str + ")" + "\t";
                } else if ((checklist[i].equals("Perimeter")) && (xUnits == yUnits) &&
                               (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                    str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
                    kl += checklist[i] + " (" + str + ")" + "\t";
                } else if (checklist[i].equals("Principal Axis")) {
                    kl += checklist[i] + " (degrees)" + "\t";
                } else if ((checklist[i].equals("Major axis length")) && (xUnits == yUnits) &&
                               (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                    str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
                    kl += checklist[i] + " (" + str + ")" + "\t";
                } else if ((checklist[i].equals("Minor axis length")) && (xUnits == yUnits) &&
                               (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                    str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
                    kl += checklist[i] + " (" + str + ")" + "\t";
                } else {
                    kl += checklist[i] + "\t";
                }
            }
        }

        kl += "\n";

        return kl;
    }

    /**
     * checks to see if the checklist has had any selections made to it. If it hasn't, then the "Statistics Options" tab
     * is brought to the front (it contains the checklist), and the warning message <i>"No statistics were selected!
     * Select a statistic."</i> is displayed.
     *
     * @return  boolean if a selection in the JPanelAnonymizeImage has been made, returns <code>true</code>. Otherwise,
     *          returns <code>false</code>.
     *
     * @see     JPanelAnonymizeImage
     */
    private boolean isStatisticSelectionOkay() {

        if (checkBoxPanel.getNumberSelected() == 0) {
            everything.setSelectedIndex(STAT_TAB);
            MipavUtil.displayWarning("No statistics were selected!  Select a statistic.");

            return false;
        }

        return true;
    }

    /**
     * Locates the translation/key-file destination directory. If the directory is not there already, the directory is
     * made. If there is a problem in creating the destination directory, the "Directory" tab is brought to the front;
     * the warning message describing the error is displayed; then the text field with the destination directory is
     * given focus, and all available text is displayed. If the translation/key-file is already present in the chosen
     * directory, there will be an options dialog allowing the user to Overwrite the old translation/key-file, to append
     * the the new translation/key to the end of the current file, or to cancel the entire operation (to look for a new
     * location).
     *
     * @return  boolean if the selected destination has been made, returns <code>true</code>. Otherwise, returns <code>
     *          false</code>.
     */
    private boolean isTableDestinationOkay() {
        tableDestination = fileSelectorPanel.getSelectedFile();

        // check that the table destination file can be written.
        // if not, then warn appropriatly
        // if yes, then make an empty file called for by tableDestination
        try {
            setupNew(tableDestination);
        } catch (IOException ioFail) {
            everything.setSelectedIndex(VOI_TAB);

            if (noisyProcess) {
                MipavUtil.displayWarning(ioFail.getMessage());
            }

            fileSelectorPanel.highlight();

            return false;
        } catch (SecurityException securityFail) {
            everything.setSelectedIndex(VOI_TAB);

            if (noisyProcess) {
                MipavUtil.displayWarning("Security prevents adjusting this file.\n" + securityFail.getMessage());
            }

            return false;
        }

        return true;
    }

    /**
     * makes the submitted directory as given by the inputted File, or the text as given in the given given textField.
     * Priority is given to the text in the textfield, so if the File's path does not agree with the path in the text
     * field, the directory will be made with the path in the text field. Should the text field not have <i>any</i>
     * path, an IOException will be thrown indicating this problem. If the directory does not exist, it will be created;
     * if there are errors in creating the directory, an IOException will be thrown, with error message describing the
     * problem, with a possible remedy as a suggestion.
     *
     * @param      selected  DOCUMENT ME!
     *
     * @return     File a directory
     *
     * @exception  IOException        -- failure to create the directory
     * @throws     SecurityException  DOCUMENT ME!
     */
    private File setupNew(File selected) throws IOException, SecurityException {

        // make a 'destDirectory' file from the dir text if it isn't already made.
        if (selected == null) {
            throw new IOException("No file selected!");
        }

        if (selected.exists() && (selected.length() != 0) && !overwrite.isSelected()) {

            // ask permission to replace the file
            if (noisyProcess) {
                String[] possibilities = { "Overwrite", "Cancel" };
                int result = JOptionPane.showOptionDialog(this,
                                                          "\"" + selected.getPath() +
                                                          "\" already exists.\nWhat do you want to do with it?",
                                                          "File exists...", JOptionPane.YES_NO_OPTION,
                                                          JOptionPane.QUESTION_MESSAGE, null, possibilities,
                                                          new Integer(0));

                if (result == 0) {

                    if (!selected.delete()) {
                        throw new IOException("File cannot be deleted.");
                    }
                } else {
                    throw new IOException("File already exists.  Not deleted.");
                }
            }
        }

        if (!selected.getParentFile().canWrite()) {
            throw new IOException("Unable to write the file.  \n" + "Destination is not writeable.");
        }

        // verify the destination directory exists, and make it if it doesn't.
        if (selected.isDirectory()) {
            throw new IOException("Destination is a directory!  \n" + "Select a file.");
        }

        // do we have rights to write here?  and an error if we can't?
        if (!selected.canWrite()) {

            if (!selected.createNewFile()) {
                throw new IOException("Error in creating destination.  " + "Write rights maybe?");
            }
        }

        return selected;
    }

    /**
     * use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code>otherwise.
     */
    private boolean setVariables() {

        // check that there is a VOI selected
        if (selectedList.getModel().getSize() == 0) {

            if (noisyProcess) {
                MipavUtil.displayWarning("No volumes of interest were selected!  Select a VOI.");
            }

            return false;
        }

        // check that there is an destination path selected
        if (!isTableDestinationOkay()) {
            return false; // error notification already done.
        }

        // check to make sure at least one statistic has
        // been selected to calculate
        if (!isStatisticSelectionOkay()) {
            return false; // error notification already done.
        }

        logFileText = new StringBuffer(createNewLogfile());

        // sets the VOIs with the selected exclude range, if it exists,
        // and does this for each VOI in the selected list.
        // NOTE: it might be better to do the setting through either the
        // algorithm or the VOI vector.
        for (int i = 0; i < selectedList.getModel().getSize(); i++) {

            try {
                ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnore(outputOptionsPanel.getMaximumExclude().floatValue());
            } catch (NullPointerException noMax) {
                ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnore(Float.MAX_VALUE);
            }

            try {
                ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnore(outputOptionsPanel.getMinimumExclude().floatValue());
            } catch (NullPointerException noMax) {
                ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnore(-Float.MAX_VALUE);
            }
        }

        precision = outputOptionsPanel.getPrecision();
        doForce = outputOptionsPanel.doForcePrecision();

        return true;
    }

    /**
     * places the input String at the end of the output log; appends a trailing newline.
     *
     * @param  logentry  DOCUMENT ME!
     */
    private void writeLogfileEntry(String logentry) {
        logFileText.append(logentry);
        logFileText.append('\n');
    }

    /**
     * Any further writes to the keyLog will be writing into a fresh, new keyLog. Writing to the keyFile later will
     * overwrite the keyFile already there.
     */
    private void writeStatisticFile() {
        FileWriter statFW;
        File statFile = new File(tableDestination.getAbsolutePath());

        try {

            if (statFile.exists()) {

                if (tableDestinationUsage == OVERWRITE) {
                    statFile.delete();
                }
            }
        } catch (SecurityException se) {

            if (noisyProcess) {
                MipavUtil.displayError("security violation incurred while creating \"" + tableDestination.getName() +
                                       "\"; \n" + "is destination directory still writable?  " +
                                       "Table file not written.");
            }

            Preferences.debug("security violation incurred while creating \"" + tableDestination.getName() + "\";\n");

            return;
        }

        try {

            if (!statFile.createNewFile()) { /*there was an error here!*/
            }
        } catch (IOException io) {
            Preferences.debug("IOexception error in creating statFile!  threw " +
                              "exception rather than createNewFile == false;\n" + io);
            io.printStackTrace();
            Preferences.debug("IO exception while writing VOIStatistic's \"" + tableDestination.getAbsolutePath() +
                              "\"\n");

            return;
        } catch (SecurityException se) {

            if (noisyProcess) {
                MipavUtil.displayError("security violation incurred while creating \"" +
                                       tableDestination.getAbsolutePath() + "\"; \n" +
                                       "is destination directory still writable?  " + "Table file not written.");
            }

            Preferences.debug("security violation incurred while creating \"" + tableDestination.getAbsolutePath() +
                              "\";\n");

            return;
        }

        try {

            if (tableDestinationUsage == OVERWRITE) {
                statFW = new FileWriter(statFile.getAbsolutePath(), false);
                statFW.write(logFileText.toString(), 0, logFileText.length());
            } else if (tableDestinationUsage == APPEND) {
                statFW = new FileWriter(statFile.getAbsolutePath(), true);
                statFW.write(logFileText.toString());
            } else { // WRITE
                statFW = new FileWriter(statFile.getAbsolutePath());
                statFW.write(logFileText.toString(), 0, logFileText.length());
            }

            statFW.close();
        } catch (IOException ioe) {

            if (noisyProcess) {
                MipavUtil.displayError("error writing the logging to \"" + tableDestination.getAbsolutePath() + "\""); // figure out where to store somewhere else?
            }
        }

        logFileText.delete(0, logFileText.length() - 1); // empty out the buffer
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * controllas the lists between left and right side.
     */
    public class JPanelAddRemoveVOI extends JPanelListController {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -2076771728355899710L;

        /**
         * Sets the Add/Remove VOI panel to Y-Axis layout and with images.
         */
        JPanelAddRemoveVOI() {
            super();
            setBackArrowEnabled(false);
            setDeleteEnabled(true);
        }

        /**
         * Checks if all super's action commands are used, and ensures that the delete button removes items from listB,
         * and that duplicate items in listB are not repeated.
         *
         * @param  ae  DOCUMENT ME!
         */
        public void actionPerformed(ActionEvent ae) {

            // the buttons won't work getting lists if either are null.
            if ((getRightList() == null) || (getLeftList() == null)) {
                return;
            }

            // check on other actions.
            super.actionPerformed(ae);

            String command = ae.getActionCommand();

            if (command.equalsIgnoreCase("delete")) {
                deleteFrom(listB);
            }

            // prevent duplicate selections in listB:
            if (command.equalsIgnoreCase("listB")) {
                removeRepeatedElements(listB);
            }
        }
    }

    /**
     * A dialog to permits choosing a range (positive or negative, and floating-point) of values with the intention of
     * the selection indicating a range of values. Includes &quot;Between&quot; cut-off value ranges. It is coded to
     * display the ranges as values for exclusion, but minor rewriting this class to allow operation specific titles.
     */
    public class JPanelExclusionSelector extends JPanel implements ActionListener {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 1471564039185960351L;

        /** DOCUMENT ME! */
        private JTextField boundA;

        /** DOCUMENT ME! */
        private JTextField boundB;

        /** DOCUMENT ME! */
        private JLabel boundLabelA;

        /** DOCUMENT ME! */
        private JLabel boundLabelB;

        /** DOCUMENT ME! */
        private JComboBox excludeSelection;

        /** DOCUMENT ME! */
        private JPanel exclusionPanel;

        /** DOCUMENT ME! */
        private Float lowerLimit;

        /** DOCUMENT ME! */
        private JCheckBox permitExclusion;

        /** held for switching between states of the exclusion. */
        private Float upperLimit;

        /**
         * Creates an exclusion panel which has a checkbox to make the range controls available, a selector to choose
         * the range controls (&quot;Between&quot;, &quot;Above&quot; and &quot;Below&quot;), and the range inputs for
         * these controls.
         */
        public JPanelExclusionSelector() {
            super(new BorderLayout());
            this.setBorder(new TitledBorder(new EtchedBorder(), "Pixel Exclusion", TitledBorder.DEFAULT_JUSTIFICATION,
                                            TitledBorder.DEFAULT_POSITION, MipavUtil.font12B));

            // add a checkbox to enable the enter-panel
            permitExclusion = new JCheckBox("Exclude Pixels from Calculation");
            permitExclusion.setFont(MipavUtil.font12);
            add(permitExclusion, BorderLayout.NORTH);
            permitExclusion.addActionListener(this);

            exclusionPanel = new JPanel(new BorderLayout());

            JPanel selection = new JPanel(new BorderLayout());
            JPanel excludeSelectionPanel = new JPanel(new BorderLayout());
            JLabel excludeLabel = new JLabel("Exclude Pixels");
            excludeLabel.setFont(MipavUtil.font12);
            excludeSelectionPanel.add(excludeLabel, BorderLayout.NORTH);

            String[] selectors = { "Between", "Above", "Below", "Outside" };
            excludeSelection = new JComboBox(selectors);
            excludeSelection.setActionCommand("Exclusion Range");
            excludeSelection.setEditable(false);
            excludeSelection.setEnabled(false);
            excludeSelection.addActionListener(this);
            excludeSelectionPanel.add(excludeSelection, BorderLayout.CENTER);
            selection.add(excludeSelectionPanel, BorderLayout.NORTH);

            JPanel values = new JPanel();
            boundA = new JTextField(8);
            MipavUtil.makeNumericsOnly(boundA, true, true);
            boundA.setEnabled(false);
            values.add(boundA);

            values.add(new JLabel(" - "));

            boundB = new JTextField(8);
            MipavUtil.makeNumericsOnly(boundB, true, true);
            boundB.setEnabled(false);
            values.add(boundB);

            selection.add(values, BorderLayout.CENTER);
            exclusionPanel.add(selection, BorderLayout.CENTER);
            add(exclusionPanel, BorderLayout.CENTER);
        }

        /**
         * When state changes in some elements of the panel, the panel must make display changes; these changes are
         * registered here. When state the state of the checkbox changes (from checked to un- or vice-versa), the
         * excluded selection's state is changed and the Exlusion range property is reset.
         *
         * <p>Checks state of:</p>
         *
         * <ul>
         *   <li>Enables or disables the exclusion drop-down and the text boxes based on the state of the checkbox</li>
         *   <li>Changes the visibility of the text-boxes based on the state of the exclusion dropdown;
         *     &quot;Between&quot; displays both text boxes, &quot;Above&quot; only displays the lower cutoff box, and
         *     &quot;Below&quot; displays only the upper cutoff box.</li>
         * </ul>
         *
         * @param  e  the ChangeEvent to watch.
         */
        public void actionPerformed(ActionEvent e) {

            if (e.getSource().equals(permitExclusion)) {

                if (permitExclusion.isSelected()) {
                    excludeSelection.setEnabled(true);
                    selectRangeInput();
                } else {
                    excludeSelection.setEnabled(false);
                    boundA.setEnabled(false);
                    boundB.setEnabled(false);
                    rangeFlag = NO_RANGE;

                    // storeLimitValues();  // store before blanking the values
                    boundA.setText("");
                    boundB.setText("");
                }
            }

            if (e.getActionCommand().equals("Exclusion Range")) {
                selectRangeInput();
            }
        }

        /**
         * Returns the lower bound text as a number. May be too negative for some applications.
         *
         * <p>There is a side-effect in that when the permitExclusion checkbox is unchecked, the lower bound returned is
         * <CODE>null</CODE>. This means that relying on the upperbound also means relying on the text fields being
         * editable as well.</p>
         *
         * @return  lower bound text as a Float; null is returned if the panel is not set to be used or one of the text
         *          entries is empty or not a number.
         */
        public Float getLowerBound() {

            if (!permitExclusion.isSelected()) {
                return null;
            }

            try {

                if (Float.parseFloat(boundA.getText()) < Float.parseFloat(boundB.getText())) {
                    return new Float(boundA.getText());
                } else {
                    return new Float(boundB.getText());
                }
            } catch (NumberFormatException notANumber) {
                return null;
            } catch (NullPointerException noNumber) {
                return null;
            }
        }

        /**
         * Returns the upper bound text as a number. May be too positive for some applications.
         *
         * <p>There is a side-effect in that when the permitExclusion checkbox is unchecked, the upper bound returned is
         * <CODE>null</CODE>. This means that relying on the upperbound also means relying on the text fields being
         * editable as well.</p>
         *
         * @return  upper bound text as a Float; null is returned if the panel is not set to be used or one of the text
         *          entries is empty or not a number.
         */
        public Float getUpperBound() {

            if (!permitExclusion.isSelected()) {
                return null;
            }

            try {

                if (Float.parseFloat(boundA.getText()) > Float.parseFloat(boundB.getText())) {
                    return new Float(boundA.getText());
                } else {
                    return new Float(boundB.getText());
                }
            } catch (NumberFormatException notANumber) {
                return null;
            } catch (NullPointerException noNumber) {
                return null;
            }
        }


        /**
         * Sets the fields' value and accessability based on the state of the drop-down. &quot;Above&quot; will display
         * an uneditable &quot;max&quot; value and the lesser of the two values, &quot;Below&quot; will display an
         * uneditable &quot;min&quot; value and the larger of the two values and &quot;Between&quot; will display the
         * largest possible values, if the fields have not been set, or will reset the fields to the stored values.
         */
        public void selectRangeInput() {
            rangeFlag = NO_RANGE;

            if (excludeSelection.getSelectedItem().equals("Above")) {
                rangeFlag = BETWEEN;
                storeLimitValues();
                boundB.setEnabled(false);
                boundB.setText(Float.toString(Float.MAX_VALUE));
                boundA.setEnabled(true);

                try {
                    boundA.setText(lowerLimit.toString());
                } catch (NullPointerException noLower) {
                    boundA.setText(Float.toString(-Float.MAX_VALUE));
                }
            } else if (excludeSelection.getSelectedItem().equals("Below")) {
                rangeFlag = BETWEEN;
                storeLimitValues();
                boundA.setEnabled(false);
                boundA.setText(Float.toString(-Float.MAX_VALUE));
                boundB.setEnabled(true);

                try {
                    boundB.setText(upperLimit.toString());
                } catch (NullPointerException noUpper) {
                    boundB.setText(Float.toString(Float.MAX_VALUE));
                }
            } else if (excludeSelection.getSelectedItem().equals("Between")) {

                // set both text-inputs as needed, then make them editable
                rangeFlag = BETWEEN;
                storeLimitValues();

                if (lowerLimit != null) {
                    boundA.setText(lowerLimit.toString());
                } else {
                    boundA.setText(Float.toString(-Float.MAX_VALUE));
                }

                if (upperLimit != null) {
                    boundB.setText(upperLimit.toString());
                } else {
                    boundB.setText(Float.toString(Float.MAX_VALUE));
                }

                boundA.setEnabled(true);
                boundB.setEnabled(true);
            } else if (excludeSelection.getSelectedItem().equals("Outside")) {
                rangeFlag = OUTSIDE;
                storeLimitValues();

                // set both text-inputs as needed, then make them editable
                if (lowerLimit != null) {
                    boundA.setText(lowerLimit.toString());
                } else {
                    boundA.setText(Float.toString(-Float.MAX_VALUE));
                }

                if (upperLimit != null) {
                    boundB.setText(upperLimit.toString());
                } else {
                    boundB.setText(Float.toString(Float.MAX_VALUE));
                }

                boundA.setEnabled(true);
                boundB.setEnabled(true);
            }
        }

        /**
         * Set the lower bound from the script dialog.
         *
         * @param  floatValue  lower bound string
         */
        public void setLowerBound(String floatValue) {
            boundA.setText(floatValue);
        }

        /**
         * Set the upper bound from the script dialog.
         *
         * @param  floatValue  Maximum value string
         */
        public void setUpperBound(String floatValue) {
            boundB.setText(floatValue);
        }

        /**
         * Tries to store the values held in the text areas to temporary storage. It only does so if there are valid
         * (that is, numbers and that they are neither infinite nor at the maximum or minimum value.
         *
         * @see  Float#MAX_VALUE
         * @see  Float#MIN_VALUE
         */
        protected void storeLimitValues() {

            /* try to store the upper and lower bounds;
             * only do so if they are valid values to store
             */
            try {

                if (!getUpperBound().isInfinite() && !getUpperBound().isNaN() &&
                        (getUpperBound().floatValue() != Float.MAX_VALUE)) {
                    upperLimit = getUpperBound();
                }
            } catch (NullPointerException inValidNumber) {
                /* nothing t do */
            }

            try {

                if (!getLowerBound().isInfinite() && !getLowerBound().isNaN() &&
                        (getLowerBound().floatValue() != -Float.MAX_VALUE)) {
                    lowerLimit = getLowerBound();
                }
            } catch (NullPointerException inValidNumber) {
                /* nothing to do */
            }
        }
    }

    /**
     * File-format options panel to select output format for the display of the statistics output file. Bordered with
     * &quot;Output format&quot;, it permits options for tab-delimited or XML-formatted output file.
     */
    public class JPanelStatisticFileFormatOptions extends JPanel {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -4726615904924397356L;

        /** Radio button to choose a tab-delimited file. */
        JRadioButton tabDelimited;

        /** Radio button to choose a XML file. */
        JRadioButton xml;

        /**
         * Creates a default layout, of the radio button options laid-out vertically. Currently, &quot;XML&quot; is not
         * selectable.
         */
        public JPanelStatisticFileFormatOptions() {
            ButtonGroup outputFormat = new ButtonGroup();
            setBorder(buildTitledBorder("Output Format"));
            setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
            tabDelimited = new JRadioButton("Tab-Delimited", true);
            tabDelimited.setFont(MipavUtil.font12);
            xml = new JRadioButton("XML", false);
            xml.setFont(MipavUtil.font12);

            // remove the following line to enable XML support (when functional!!)
            xml.setEnabled(false);

            // add to grouping
            outputFormat.add(tabDelimited);
            outputFormat.add(xml);

            add(tabDelimited);
            add(xml);
        }

        /**
         * Provides the state of the tab-delimited option.
         *
         * @return  the state of the tab-delimited option.
         */
        public boolean isTabDelimited() {
            return tabDelimited.isSelected();
        }
    }

    /**
     * Panel holding all statistics options and allowing the user-selection. This panel has a border labelled,
     * "Statistics Options" and contains a set of radio buttons for selecting which properties about a VOI are to be
     * reported: by contour and slice, by slice, or by total VOI; an option exists to display all totals, as well. There
     * is also an exclusion selector panel. The panel only permits programmatic setting of slice-only calculation, but
     * there is a retrieval facility available for other options.
     */
    public class JPanelStatisticsOptions extends JPanel {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 6577076069976323730L;

        /** A radio button to select calculation by VOI contour. */
        JRadioButton byContour;

        /** A radio button to select calculation by VOI slice. */
        JRadioButton bySlice;

        /** A radio button to select calculation by total VOI. */
        JRadioButton byTotalVOI;

        /** The Exclusion selector. */
        JDialogVOIStatistics.JPanelExclusionSelector excluder;

        /** A check box to opt for VOI totals. */
        JCheckBox showTotals;

        /** User can choose the precision to display */
        JComboBox precisionBox;

        JCheckBox forceDecimal;

        /**
         * Creates a default view of the panel, including all options displayed. The option to calculate for the total
         * VOI only is selected.
         */
        public JPanelStatisticsOptions() {
            setBorder(buildTitledBorder("Statistics options"));
            setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
            byContour = new JRadioButton("By contour & slice");
            byContour.setFont(MipavUtil.font12);
            bySlice = new JRadioButton("By slice only");
            bySlice.setFont(MipavUtil.font12);
            byTotalVOI = new JRadioButton("By total VOI");
            byTotalVOI.setFont(MipavUtil.font12);
            byTotalVOI.setSelected(true);
            showTotals = new JCheckBox("Show all totals");
            showTotals.setFont(MipavUtil.font12);

            String [] precisionStr = new String[11];
            for (int i = 0; i < precisionStr.length; i++) {
                precisionStr[i] = String.valueOf(i);
            }
            precisionBox = new JComboBox(precisionStr);
            precisionBox.setFont(MipavUtil.font12);
            precisionBox.setSelectedIndex(4);

            forceDecimal = new JCheckBox("Force decimal display", false);
            forceDecimal.setFont(MipavUtil.font12);

            JPanel precisionPanel = new JPanel();
            precisionPanel.setBorder(buildTitledBorder("Precision"));
            precisionPanel.add(precisionBox);
            precisionPanel.add(forceDecimal);


            ButtonGroup group = new ButtonGroup();
            group.add(bySlice);
            group.add(byContour);
            group.add(byTotalVOI);
            excluder = new JDialogVOIStatistics.JPanelExclusionSelector();

            add(byContour);
            add(bySlice);
            add(byTotalVOI);
            add(showTotals);
            add(precisionPanel);
            add(excluder);
        }

        /**
         * Method to retrieve the value for the exclusion panel's maximum.
         *
         * @return  DOCUMENT ME!
         */
        public Float getMaximumExclude() {

            try {
                return excluder.getUpperBound();
            } catch (NullPointerException noUpper) {
                return null;
            }
        }

        /**
         * Method to retrieve the value for the exclusion panel's minimum.
         *
         * @return  DOCUMENT ME!
         */
        public Float getMinimumExclude() {

            try {
                return excluder.getLowerBound();
            } catch (NullPointerException noLower) {
                return null;
            }
        }

        /**
         * Method to retrieve the selected value for contour-only calculation.
         *
         * @return  DOCUMENT ME!
         */
        public boolean isByContour() {
            return byContour.isSelected();
        }

        /**
         * Method to retrieve the selected value for slice-only calculation.
         *
         * @return  DOCUMENT ME!
         */
        public boolean isBySlice() {
            return bySlice.isSelected();
        }

        /**
         * Method to retrieve the selected value for VOI-only calculation.
         *
         * @return  <code>true</code> VOI only calculation <code>false</code> otherwise.
         */
        public boolean isByVOI() {
            return byTotalVOI.isSelected();
        }

        /**
         * Method to retrieve the selected value for totals calculation.
         *
         * @return  DOCUMENT ME!
         */
        public boolean isShowTotals() {
            return showTotals.isSelected();
        }

        /**
         * Method to set the selected value for slice-only calculation.
         *
         * @param  flag  DOCUMENT ME!
         */
        public void setBySliceEnabled(boolean flag) {
            bySlice.setEnabled(flag);
        }

        /**
         * Set the upper bound value.
         *
         * @param  floatValue  float value string
         */
        public void setMaximumExclude(String floatValue) {

            try {
                excluder.setUpperBound(floatValue);
            } catch (NullPointerException noUpper) {
                return;
            }
        }

        /**
         * Set the lower bound value.
         *
         * @param  floatValue  float value string
         */
        public void setMinimumExclude(String floatValue) {

            try {
                excluder.setLowerBound(floatValue);
            } catch (NullPointerException noLower) {
                return;
            }
        }

        /**
         * Set the output calculation by type.
         *
         * @param  type  VOI, Contour or Slice
         */
        public void setOutputType(int type) {

            if (type == 0) {
                byTotalVOI.setSelected(true);
            } else if (type == 1) {
                byContour.setSelected(true);
            } else if (type == 2) {
                bySlice.setSelected(true);
            }
        }

        /**
         * Set the showTotal calculation flag.
         *
         * @param  flag  <code>true</code> show total voxels <code>false</code> otherwise
         */
        public void setShowTotals(boolean flag) {
            showTotals.setSelected(flag);
        }

        public boolean doForcePrecision() {
            return forceDecimal.isSelected();
        }

        public int getPrecision() {
            return precisionBox.getSelectedIndex();
        }

    }

    /**
     * An active listener for VOIEvents and ListSelectionEvents, this class will ensure the selected state of the VOI
     * and its name in a dialog's list is the same. It provides that when the name of the VOI is selected in a dialog's
     * list, the VOI itself is selected inside the image. Likewise, a selected VOI will highlight the name in the list
     * of the appropriate dialog.
     */
    public class VOIHighlighter implements ListSelectionListener, VOIListener {

        /**
         * We are not interested in adding Curves, so this method is empty.
         *
         * @param  added  DOCUMENT ME!
         */
        public void addedCurve(VOIEvent added) {
            /* not interested in adding curves */
        }

        /**
         * We are not interested in removing Curves, so this method is empty.
         *
         * @param  added  DOCUMENT ME!
         */
        public void removedCurve(VOIEvent added) {
            /* not interested in removing curves */
        }

        /**
         * Handles the VOI being selected. -- a state-change.
         *
         * @param  selection  DOCUMENT ME!
         */
        public void selectedVOI(VOIEvent selection) {
            volumesList.setSelectedValue(selection.getSource(), selection.getState());
        }

        /**
         * Goes through the list selection event's JList source to find selected VOI list items. The selected VOIs are
         * then instructed to be &quot;active&quot;. Any other VOIs in the list are set to be not active.
         *
         * @param  lse  DOCUMENT ME!
         */
        public void valueChanged(ListSelectionEvent lse) {
            JList imageVOIlist = (JList) lse.getSource();

            // go through all VOIs in the list.  if the item is
            // selected, highlight the corresponding VOI,
            // otherwise, deselect it.
            // System.out.println("VOIHighlighter active");
            for (int i = 0; i < imageVOIlist.getModel().getSize(); i++) {

                if (imageVOIlist.isSelectedIndex(i)) {
                    ((VOI) imageVOIlist.getModel().getElementAt(i)).setAllActive(true);
                } else {
                    ((VOI) imageVOIlist.getModel().getElementAt(i)).setAllActive(false);
                }
            }

            // ui.getActiveImageFrame().repaint();
            ui.getActiveImageFrame().updateImages();
        }
    }

}
