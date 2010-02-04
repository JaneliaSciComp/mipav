package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.event.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.Vector;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.table.JTableHeader;


/**
 * Dialog for calculating statistics of a (set of) Volumes of Interest. User selects a Volume of Interest (or more than
 * one) to act on, selects the statistics from a selectable checklist panel. The User selects "Calculate"; the output
 * from the statistics is reported in a "log-panel" and may be sent to a file.
 * 
 * <p>
 * $Logfile: /mipav/src/gov/nih/mipav/view/dialogs/JDialogVOIStatistics.java $
 * </p>
 */

public class JDialogVOIStatistics extends JDialogScriptableBase implements AlgorithmInterface, VOIStatisticList,
        VOIVectorListener {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1354965054401181781L;

    /** DOCUMENT ME! */
    public static final int NO_RANGE = -1;

    /** DOCUMENT ME! */
    public static final int BETWEEN = 0;

    /** DOCUMENT ME! */
    public static final int OUTSIDE = 1;

    /** VOI Tab. */
    protected static final int VOI_TAB = 0;

    /** Statistics Tab. */
    protected static final int STAT_TAB = 1;

    /** Logging Tab. */
    protected static final int LOG_TAB = 2;

    /** File handler output mode - append. */
    private static final int APPEND = 0;

    /** File handler output mode - overwriteBox. */
    private static final int OVERWRITE = 1;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Panel to push/pull VOIs from full list to selectable list. */
    private JPanelAddRemoveVOI addremove;

    /** DOCUMENT ME! */
    protected AlgorithmVOIProps calculator = null;

    /** Panel holding statistics options. */
    protected JPanelStatisticsList checkBoxPanel;

    /** boolean array mirroring checkbox panel's selection. */
    protected boolean[] checkList = null;

    /** force precision to display at maximum. */
    private boolean doForce = false;

    // onscreen objects
    /** Tabbed pane that holds all components. */
    protected JTabbedPane everything;

    /** Panel to select files. */
    private JPanelFileSelection fileSelectorPanel;

    /** Operator to provide listener access... could be done by /this/ */
    private VOIHighlighter highlighter;

    /** Global image reference to the currently active image during script running and recording. */
    protected ModelImage image;

    /** The text of the log process to be written to file. */
    private StringBuffer logFileText;

    /** Model where data is stored for the table. */
    protected ViewTableModel logModel;

    /** Table holding log of statistics calculated. */
    private JTable logTable;

    /** Flag where <code>true</code> means notifying the user of stupid errors. */
    private final boolean noisyProcess = true;

    /** Panel holding statistics output options. */
    protected JPanelStatisticsOptions outputOptionsPanel;

    /** Check box item indicating if we're to always overwriteBox the statistics file. */
    private JCheckBoxMenuItem overwriteBox;

    /** Number of digits after decimal place to allow. */
    private int precision = 4;

    /** DOCUMENT ME! */
    protected int processType = AlgorithmVOIProps.PROCESS_PER_VOI;

    /** DOCUMENT ME! */
    private int rangeFlag = JDialogVOIStatistics.NO_RANGE;

    /** DOCUMENT ME! */
    private float rangeMaximum = 0f;

    /** DOCUMENT ME! */
    private float rangeMinimum = 0f;

    /** List of selected VOIs. */
    protected JList selectedList = new JList();

    /** DOCUMENT ME! */
    protected boolean showTotals = false;

    // directory file objects
    /** Log file destination. */
    private File tableDestination = null;

    /** Indicates options for OVERWRITE, APPEND for the xlat file. */
    private int tableDestinationUsage;

    /** Toolbar. */
    protected JToolBar toolBar;

    /** Icon and log access. */
    protected ViewUserInterface userInterface;

    // actual things we can see...
    /** List of available VOIs. */
    private final JList volumesList = new JList();

    /** DOCUMENT ME! */
    protected int xUnits, yUnits, zUnits;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogVOIStatistics() {
        this.addWindowListener(new WindowAdapter() {
            public void windowClosing(final WindowEvent event) {
                cleanUpAndDispose();
            }
        });
    }

    /**
     * builds and packs the frame. does <i>not</I> set it visible.
     * 
     * <p>
     * install the panels of source directory, destination directory, the checkbox for approving the translation-table
     * file and the panel containing the ok and cancel buttons. Installs the checkbox panel.
     * </p>
     * 
     * @param voiList DOCUMENT ME!
     */
    public JDialogVOIStatistics(final VOIVector voiList) {
        super(ViewUserInterface.getReference().getMainFrame(), false);

        buildDialog(voiList);

        this.addWindowListener(new WindowAdapter() {
            public void windowClosing(final WindowEvent event) {
                cleanUpAndDispose();
            }
        });
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * when a button is clicked.
     * 
     * @param ae DOCUMENT ME!
     */
    public void actionPerformed(final ActionEvent ae) {
        final Object source = ae.getSource();
        final String command = ae.getActionCommand();

        if (command.equals("clear log")) {
            logModel.setRowCount(0);
            logModel.setColumnCount(0);
        } else if (command.equals("overwrite")) {
            Preferences.setProperty(Preferences.PREF_OVERWRITE_STATISTICS, String.valueOf(overwriteBox.isSelected()));
        } else if (source == OKButton) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (source == cancelButton) {
            cleanUpAndDispose();
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("GroupStat001");
        }
    }

    /**
     * resets the volumes list to the current VOIVector. adds the highlighter to the new VOI.
     * 
     * @param voiEvent DOCUMENT ME!
     */
    public void addedVOI(final VOIVectorEvent voiEvent) {
        final VOIVector voiList = (VOIVector) voiEvent.getSource();
        final Vector volumesVector = new Vector();

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
     * @param event the event
     */
    public void algorithmPerformed(final AlgorithmBase event) {
        // if script is running, do not update GUI

        if ( !isScriptRunning()) {
            updateDialog();
            insertScriptLine();
            writeStatisticFile();
        }

        System.gc(); // to reclaim lost land.
    }

    /**
     * Refreshes the list of available and selected VOIs.
     * 
     * @param VOIlist imageActive's current VOIVector
     */
    public void refreshVOIList(final VOIVector VOIlist) {
        selectedList.setListData(new Vector());

        final Vector volumesVector = new Vector();
        highlighter = new VOIHighlighter();

        for (int i = 0; i < VOIlist.size(); i++) {

            if (VOIlist.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                volumesVector.add(VOIlist.elementAt(i));

                // add a listener to each VOI so we know about selection.
                VOIlist.VOIAt(i).addVOIListener(highlighter);
            }
        }

        volumesList.setListData(volumesVector);

    }

    /**
     * resets the volumes list to the current VOIVector. removes the highlighter from the removed VOI.
     * 
     * @param voiEvent DOCUMENT ME!
     */
    public void removedVOI(final VOIVectorEvent voiEvent) {
        final VOIVector voiList = (VOIVector) voiEvent.getSource();
        final Vector volumesVector = new Vector();

        for (int i = 0; i < voiList.size(); i++) {

            if (voiList.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                volumesVector.add(voiList.elementAt(i));
            }
        }

        volumesList.setListData(volumesVector);

        /*
         * we cannot delete VOIs out of the selected VOI list if there are no more VOIs in the image. --
         * voiEvent.getSource() is null when 'removeAll' is called. getVOI is to return, specifically, the new VOI that
         * has changed. Since all VOIs are now null, we must recognise that the VOI that is new is the empty VOI. (one
         * of these a-ha! moments. silly comments left undone.)
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
     * un-implemented.
     * 
     * @param voiEvent DOCUMENT ME!
     */
    public void vectorSelected(final VOIVectorEvent voiEvent) {}

    /**
     * Once all the necessary variables are set, call the VOI Props algorithm to run the statistic calculation.
     */
    protected void callAlgorithm() {
        ViewVOIVector processList = new ViewVOIVector(selectedList.getModel().getSize());
        for(int i=0; i<selectedList.getModel().getSize(); i++) {
            processList.add((VOI)selectedList.getModel().getElementAt(i));
        }
        
        calculator = new AlgorithmVOIProps(image, processType, rangeFlag, processList);
        calculator.setPrecisionDisplay(precision, doForce);
        calculator.addListener(this);

        // only calculate these if appropriate box is checked for speed.
        int largestDistanceIndex = -1, largestSliceDistanceIndex = -1;
        for (int i = 0; i < VOIStatisticList.numberOfStatistics; i++) {
            if (VOIStatisticList.statisticDescription[i].equals(VOIStatisticList.largestDistanceDescription)) {
                largestDistanceIndex = i;
            } else if (VOIStatisticList.statisticDescription[i]
                    .equals(VOIStatisticList.largestSliceDistanceDescription)) {
                largestSliceDistanceIndex = i;
            }
        }

        calculator.setDistanceFlag(checkList[largestDistanceIndex]);
        calculator.setSliceDistanceFlag(checkList[largestSliceDistanceIndex]);

        createProgressBar(image.getImageName(), calculator);

        calculator.setVOIList(selectedList.getModel());
        calculator.setShowTotals(showTotals);
        // da.addTextUpdateListener(this); // unimplemented -- meant to permit messaging between running thread and
        // this' logging pane.
        
        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still have user interface work fast.
            if (calculator.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {

            calculator.run();
        }
    }

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
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {

        // this will create the String to be written to the log file...
        // no updating of the GUI table (never created)

        VOIStatisticalProperties properties;
        Vector[] contours;

        int numStats = 1;

        for (final boolean element : checkList) {

            if (element) {
                numStats++;
            }
        }

        if ( (processType == AlgorithmVOIProps.PROCESS_PER_SLICE)
                || (processType == AlgorithmVOIProps.PROCESS_PER_SLICE_AND_CONTOUR)) {
            final ListModel list = selectedList.getModel();

            // for each element in the list ....
            for (int i = 0; i < list.getSize(); i++) {
                properties = calculator.getVOIProperties((VOI) list.getElementAt(i));
                contours = ((VOI) list.getElementAt(i)).getCurves();

                final String[] logRowData = new String[numStats];
                final String[] logTotalData = new String[numStats];

                for (int slice = 0; slice < contours.length; slice++) {
                    int count = 0;
                    int stop = 1;
                    String end = slice + ";";

                    if (processType == AlgorithmVOIProps.PROCESS_PER_SLICE_AND_CONTOUR) {
                        stop = contours[slice].size();
                    }

                    if (contours[slice].size() < 1) {
                        stop = 0;
                    }

                    // for each contour only print titles and calculations once,
                    // if not "calculate by contour" (ie., if we only want totals):
                    for (int num = 0; num < stop; num++) {

                        // first: set up row title:
                        logRowData[0] = list.getElementAt(i).toString() + ", " + // VOI name
                                (slice) + ", " + // slice #, irrellevent to where contour is in image
                                ((VOIBase) contours[slice].get(num)).getLabel(); // contour #, held in label
                        logTotalData[0] = "Totals:";

                        if (calculator.getProcessType() == AlgorithmVOIProps.PROCESS_PER_SLICE_AND_CONTOUR) {
                            end = slice + ";" + num;
                        }

                        // for each column in the row, print the statistic:
                        for (int k = 0; k < VOIStatisticList.statisticDescription.length; k++) {

                            if (checkList[k]) {

                                // if it's a color image and the property is min intensity, max intensity, avg
                                // intensity, or standard deviation of intensity, those properties were entered as Red,
                                // Green, Blue and we should display them differently.
                                if (calculator.isColor()
                                        && (VOIStatisticList.statisticDescription[k].indexOf("Intensity") != -1)) {
                                    String temp = "R: "
                                            + properties.getProperty(VOIStatisticList.statisticDescription[k] + "Red"
                                                    + end);
                                    temp += " G: "
                                            + properties.getProperty(VOIStatisticList.statisticDescription[k] + "Green"
                                                    + end);
                                    temp += " B: "
                                            + properties.getProperty(VOIStatisticList.statisticDescription[k] + "Blue"
                                                    + end);
                                    logRowData[count] = temp;

                                    if (showTotals) {
                                        temp = " R: "
                                                + properties.getProperty(VOIStatisticList.statisticDescription[k]
                                                        + "RedTotal");
                                        temp += " G: "
                                                + properties.getProperty(VOIStatisticList.statisticDescription[k]
                                                        + "GreenTotal");
                                        temp += " B: "
                                                + properties.getProperty(VOIStatisticList.statisticDescription[k]
                                                        + "BlueTotal");
                                        logTotalData[count] = temp;
                                    }
                                } else {

                                    logRowData[count] = properties.getProperty(VOIStatisticList.statisticDescription[k]
                                            + end);

                                    if (showTotals) {
                                        logTotalData[count] = properties
                                                .getProperty(VOIStatisticList.statisticDescription[k] + "Total");
                                    }
                                }

                                count++;
                            }
                        } // end for each column

                        count = 0;

                        String logText = "";

                        for (final String element : logRowData) {
                            logText += element + "\t";
                        }

                        writeLogfileEntry(logText);
                    } // end for contours
                }

                if (showTotals) {
                    String logText = "";

                    for (final String element : logTotalData) {
                        logText += element + "\t";
                    }

                    writeLogfileEntry(logText);
                }

                for (int k = 0; k < logTotalData.length; k++) {
                    logTotalData[k] = "";
                }

            }
        } else { // whole 3D VOI data

            final ListModel list = selectedList.getModel();

            // for each element in the list print properties of each VOI,
            // column-by-column:
            for (int i = 0; i < list.getSize(); i++) {
                properties = calculator.getVOIProperties((VOI) list.getElementAt(i));
                contours = ((VOI) list.getElementAt(i)).getCurves();

                final String[] rowData = new String[numStats];
                rowData[0] = list.getElementAt(i).toString();

                int count = 0;

                for (int k = 0; k < VOIStatisticList.statisticDescription.length; k++) {

                    if (checkList[k]) {
                        count++;

                        // if it's a color image and the property is min intensity, max intensity, avg intensity,
                        // or standard deviation of intensity, those properties were entered as Red, Green, Blue and
                        // we should display them differently.
                        if (calculator.isColor()
                                && (VOIStatisticList.statisticDescription[k].indexOf("Intensity") != -1)) {
                            String temp = "R: "
                                    + properties.getProperty(VOIStatisticList.statisticDescription[k] + "Red");
                            temp += " G: " + properties.getProperty(VOIStatisticList.statisticDescription[k] + "Green");
                            temp += " B: " + properties.getProperty(VOIStatisticList.statisticDescription[k] + "Blue");
                            rowData[count] = temp;
                        } else {
                            rowData[count] = properties.getProperty(VOIStatisticList.statisticDescription[k]);
                        }
                    }
                } // end for each column

                count = 0;

                String logText = "";

                for (final String element : rowData) {
                    logText += element + "\t";
                }

                writeLogfileEntry(logText);

                for (int k = 0; k < rowData.length; k++) {
                    rowData[k] = "";
                }
            }
        }

        writeStatisticFile();
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        setScriptRunning(true);
        image = scriptParameters.retrieveInputImage();

        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        final VOIVector voiVec = image.getVOIs();

        if (voiVec.size() < 1) {
            this.dispose();

            return;
        }

        for (int i = 0; i < voiVec.size(); i++) {
            voiVec.VOIAt(i).setAllActive(true);
        }

        selectedList.setListData(image.getVOIs());

        // createDialog(userInterface, true);

        rangeFlag = scriptParameters.getParams().getInt("do_use_exclusion_range");

        if (rangeFlag != JDialogVOIStatistics.NO_RANGE) {
            rangeMinimum = scriptParameters.getParams().getFloat("exclusion_range_min");
            rangeMaximum = scriptParameters.getParams().getFloat("exclusion_range_max");

            for (int i = 0; i < selectedList.getModel().getSize(); i++) {

                try {
                    ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnore(rangeMaximum);
                } catch (final NullPointerException noMax) {
                    ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnore(Float.MAX_VALUE);
                }

                try {
                    ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnore(rangeMinimum);
                } catch (final NullPointerException noMax) {
                    ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnore( -Float.MAX_VALUE);
                }
            }
        }

        checkList = scriptParameters.getParams().getList("stat_checklist").getAsBooleanArray();

        logFileText = new StringBuffer(createNewLogfile());

        tableDestinationUsage = scriptParameters.getParams().getInt("output_writing_behavior");
        if (scriptParameters.getParams().containsParameter("voi_stats_output_file")) {
            tableDestination = new File(scriptParameters.getParams().getString("voi_stats_output_file"));
        } else {
            tableDestination = new File(image.getFileInfo(0).getFileDirectory() + File.separator + image.getImageName()
                    + ".table");
        }

        processType = scriptParameters.getParams().getInt("processing_level");
        showTotals = scriptParameters.getParams().getBoolean("do_show_totals");
        precision = scriptParameters.getParams().getInt("output_precision");
        doForce = scriptParameters.getParams().getBoolean("do_force_precision");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.getParams().put(ParameterFactory.newParameter("stat_checklist", checkList));

        scriptParameters.getParams().put(ParameterFactory.newParameter("processing_level", processType));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_show_totals", showTotals));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_use_exclusion_range", rangeFlag));

        if (rangeFlag != JDialogVOIStatistics.NO_RANGE) {
            scriptParameters.getParams().put(
                    ParameterFactory.newParameter("exclusion_range_min", outputOptionsPanel.getMinimumExclude()
                            .floatValue()));
            scriptParameters.getParams().put(
                    ParameterFactory.newParameter("exclusion_range_max", outputOptionsPanel.getMaximumExclude()
                            .floatValue()));
        }

        scriptParameters.getParams().put(ParameterFactory.newParameter("output_precision", precision));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_force_precision", doForce));
        scriptParameters.getParams().put(
                ParameterFactory.newParameter("output_writing_behavior", tableDestinationUsage));
    }

    /**
     * Builds main dialog.
     */
    protected void buildDialog(final VOIVector voiList) {
        setTitle("Calculate Statistics on VOI groups");
        setJMenuBar(buildMenuEntries());
        buildToolBar();
        this.userInterface = ViewUserInterface.getReference();
        image = ViewUserInterface.getReference().getActiveImageFrame().getComponentImage().getActiveImage();
        xUnits = image.getFileInfo(0).getUnitsOfMeasure()[0];
        yUnits = image.getFileInfo(0).getUnitsOfMeasure()[1];
        zUnits = FileInfoBase.UNKNOWN_MEASURE;

        if (image.getNDims() > 2) {
            zUnits = image.getFileInfo(0).getUnitsOfMeasure()[2];
        }
        // need to take out line VOIs, polyline VOIs, point VOIs

        everything = new JTabbedPane(SwingConstants.TOP);
        everything.setFont(MipavUtil.font12B);
        everything.insertTab("VOI selection", null, buildVOIPanel(voiList), // we must store this panel so we can
                // create a new listing later
                "Choose VOIs and statistics file", JDialogVOIStatistics.VOI_TAB);

        final JPanel statPanel = new JPanel(new BorderLayout());
        checkBoxPanel = new JPanelStatisticsList();
        
        try {
            checkBoxPanel.setSliceCount(image.getExtents()[2]);
        } catch (ArrayIndexOutOfBoundsException aioobe) {

            // otherwise, this must be a 2d image.
            checkBoxPanel.setSliceCount(1);
        } finally {
            checkBoxPanel.setCheckBoxesEnabled();
        }
        
        
        outputOptionsPanel = new JPanelStatisticsOptions();

        if (ViewUserInterface.getReference().getActiveImageFrame().getComponentImage().getActiveImage().getNDims() == 2) {
            outputOptionsPanel.setBySliceEnabled(false);
        }

        statPanel.add(outputOptionsPanel, BorderLayout.EAST);
        statPanel.add(checkBoxPanel, BorderLayout.CENTER);
        everything.insertTab("Statistics Options", null, statPanel, "Statistic Selection",
                JDialogVOIStatistics.STAT_TAB);

        everything.insertTab("Logging", null, buildLogPanel(), "Output Log", JDialogVOIStatistics.LOG_TAB);

        getContentPane().add(toolBar, BorderLayout.NORTH);
        getContentPane().add(everything, BorderLayout.CENTER);
        getContentPane().add(buildOKCancelPanel(), BorderLayout.SOUTH); // build OK/Cancel button Panel

        pack();
        setSize(800, 500); // decent size??
    }

    /**
     * creates a panel for the output log.
     * 
     * @return DOCUMENT ME!
     */
    protected JScrollPane buildLogPanel() {
        final JPanel logpan = new JPanel(new BorderLayout());

        logModel = new ViewTableModel();
        logTable = new JTable(logModel);
        final JTableHeader header = logTable.getTableHeader();
        logTable.setFont(MipavUtil.font12);

        final Box scrollingBox = new Box(BoxLayout.Y_AXIS);

        scrollingBox.add(header);
        scrollingBox.add(logTable);

        final JScrollPane lpane = new JScrollPane(scrollingBox, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        lpane.getVerticalScrollBar().addAdjustmentListener(new ScrollCorrector());
        lpane.getHorizontalScrollBar().addAdjustmentListener(new ScrollCorrector());
        logpan.add(lpane, BorderLayout.CENTER);

        return lpane;
    }

    /**
     * Builds a small menu with "Clear log" and "Overwrite" options.
     * 
     * @return DOCUMENT ME!
     */
    protected JMenuBar buildMenuEntries() {
        final JMenuBar anonBar = new JMenuBar();
        final JMenu entry = new JMenu("Options");
        final JMenuItem logClear = new JMenuItem("Clear Log Window");
        overwriteBox = new JCheckBoxMenuItem("Overwrite file automatically");

        entry.setFont(MipavUtil.font12B);
        entry.setMnemonic(KeyEvent.VK_E);
        logClear.setFont(MipavUtil.font12B);
        logClear.setActionCommand("clear log");
        logClear.setAccelerator(KeyStroke.getKeyStroke('C', java.awt.Event.ALT_MASK));
        logClear.setMnemonic(KeyEvent.VK_C);
        logClear.addActionListener(this);

        overwriteBox.setFont(MipavUtil.font12B);
        overwriteBox.setActionCommand("overwrite");
        overwriteBox.setAccelerator(KeyStroke.getKeyStroke('O', java.awt.Event.ALT_MASK));
        overwriteBox.setMnemonic(KeyEvent.VK_C);
        overwriteBox.setSelected(Preferences.is(Preferences.PREF_OVERWRITE_VOI_STATS));
        overwriteBox.addActionListener(this);

        entry.add(logClear);
        entry.add(overwriteBox);
        anonBar.add(entry);

        return anonBar;
    }

    /**
     * creates the panel which consists of the OKAY button and the Cancel button.
     * 
     * @return DOCUMENT ME!
     */
    protected JPanel buildOKCancelPanel() {
        final JPanel ocp = new JPanel(); // flow layout

        buildOKButton();
        OKButton.setText("Calculate");
        ocp.add(OKButton);

        buildCancelButton();
        cancelButton.setText("Close");
        ocp.add(cancelButton);

        buildHelpButton();
        helpButton.setText("Help");
        ocp.add(helpButton);

        return ocp;
    }

    /**
     * creates the visual display in which to list all selected directories in the directory tree. The panel is 240
     * pixels wide though that is <i>supposed</i> to be the minimum size
     * 
     * @return the panel which is to hold the list of selected items
     */
    private JPanel buildSelectedListing() {

        // define an outside panel to hold all these components.
        final JPanel selp = new JPanel(new BorderLayout());
        selp.add(Box.createHorizontalStrut(240), BorderLayout.NORTH); // width of text area. seems to start out very
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
     * @param VOIlist DOCUMENT ME!
     * 
     * @return Panel.
     */
    private JPanel buildSourceListingPanel(final VOIVector VOIlist) {
        final JPanel srctreep = new JPanel(new BorderLayout());
        final Vector volumesVector = new Vector();
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

        final JScrollPane jsp = new JScrollPane(volumesList);
        srctreep.add(jsp, BorderLayout.CENTER);

        // now let's listen to this vector:
        VOIlist.addVectorListener(this);

        return srctreep;
    }

    /**
     * creates the source panel which consists of the directory line, the browse button, and a check box approving the
     * anonymize in sub-directories.
     * 
     * @param VOIlist DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    private JPanel buildSourcePanel(final VOIVector VOIlist) {
        final JPanel srcp = new JPanel(new GridLayout(1, 2));
        srcp.setBorder(buildTitledBorder("VOI group list"));

        srcp.add(buildSourceListingPanel(VOIlist), BorderLayout.CENTER); // list of VOIs in the image.
        srcp.add(buildSelectedListing(), BorderLayout.EAST); // list of selected items

        return srcp;
    }

    /**
     * Build the toolbar control.
     */
    protected void buildToolBar() {
        final GridBagConstraints gbc = new GridBagConstraints();

        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.gridx = 0;
        gbc.gridy = 0;

        final Border etchedBorder = BorderFactory.createEtchedBorder();

        toolBar = new JToolBar();
        toolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        toolBar.setBorder(etchedBorder);
        toolBar.setFloatable(false);

        final JButton eraserButton = new JButton(MipavUtil.getIcon("eraser.gif"));

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
     * @param VOIlist DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    protected JPanel buildVOIPanel(final VOIVector VOIlist) {
        final JPanel imagePanel = new JPanel(new BorderLayout());

        // we must store sourcePanel so we can create a new directory listing later
        imagePanel.add(buildSourcePanel(VOIlist), BorderLayout.CENTER);

        final JPanel destinationsPanel = new JPanel(new BorderLayout());

        fileSelectorPanel = new JPanelFileSelection(new File(image.getFileInfo(0).getFileDirectory() + File.separator
                + image.getImageName() + ".table"), "VOI Statistic File Destination");

        destinationsPanel.add(fileSelectorPanel, BorderLayout.CENTER);

        final JPanel fileFormatPanel = new JPanelStatisticFileFormatOptions();
        destinationsPanel.add(fileFormatPanel, BorderLayout.SOUTH);

        final JPanel lowerPanel = new JPanel(new BorderLayout());
        lowerPanel.add(destinationsPanel, BorderLayout.CENTER);

        // lowerPanel.add(buildRandSelectionPanel(), BorderLayout.EAST);
        imagePanel.add(lowerPanel, BorderLayout.SOUTH);

        return imagePanel;
    }

    /**
     * creates a new keylog, writing which tags are to be removed from the image information; the table header for the
     * image read/write logging is added. the string created here is not automatically turned into the keylog string.
     * that must be done by the caller.
     * 
     * @return the new KeyLog String.
     */
    protected String createNewLogfile() {
        int i;
        String kl = "#\tMIPAV will alter.  Conducting statistical computation.\n";
        String str;

        // output the labels of the list of statistics to be produced.
        final String[] checklistLabels = JPanelStatisticsList.getCheckboxLabels();
        kl += "Name, Slice, Contour\t";

        for (i = 0; i < checklistLabels.length; i++) {

            if (checkList[i]) {

                if ( (checklistLabels[i].equals("Volume")) && (xUnits == yUnits) && (xUnits == zUnits)
                        && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                    str = image.getFileInfo(0).getVolumeUnitsOfMeasureStr();
                    kl += checklistLabels[i] + " (" + str + ")" + "\t";
                } else if ( (checklistLabels[i].equals("Area")) && (xUnits == yUnits)
                        && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                    str = image.getFileInfo(0).getAreaUnitsOfMeasureStr();
                    kl += checklistLabels[i] + " (" + str + ")" + "\t";
                } else if ( (checklistLabels[i].equals("Perimeter")) && (xUnits == yUnits)
                        && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                    str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
                    kl += checklistLabels[i] + " (" + str + ")" + "\t";
                } else if (checklistLabels[i].equals("Principal Axis")) {
                    kl += checklistLabels[i] + " (degrees)" + "\t";
                } else if ( (checklistLabels[i].equals("Major axis length")) && (xUnits == yUnits)
                        && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                    str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
                    kl += checklistLabels[i] + " (" + str + ")" + "\t";
                } else if ( (checklistLabels[i].equals("Minor axis length")) && (xUnits == yUnits)
                        && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                    str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
                    kl += checklistLabels[i] + " (" + str + ")" + "\t";
                } else if ( (checklistLabels[i].equals("Largest slice distance")) && (xUnits == yUnits)
                        && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                    str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
                    kl += checklistLabels[i] + " (" + str + ")" + "\t";
                } else if ( (checklistLabels[i].equals("Largest distance")) && (xUnits == yUnits) && (xUnits == zUnits)
                        && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                    str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
                    kl += checklistLabels[i] + " (" + str + ")" + "\t";
                } else {
                    kl += checklistLabels[i] + "\t";
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
     * @return boolean if a selection in the JPanelAnonymizeImage has been made, returns <code>true</code>.
     *         Otherwise, returns <code>false</code>.
     * 
     * @see JPanelAnonymizeImage
     */
    private boolean isStatisticSelectionOkay() {

        if (checkBoxPanel.getNumberSelected() == 0) {
            everything.setSelectedIndex(JDialogVOIStatistics.STAT_TAB);
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
     * @return boolean if the selected destination has been made, returns <code>true</code>. Otherwise, returns
     *         <code>
     *          false</code>.
     */
    private boolean isTableDestinationOkay() {
        tableDestination = fileSelectorPanel.getSelectedFile();

        // check that the table destination file can be written.
        // if not, then warn appropriatly
        // if yes, then make an empty file called for by tableDestination
        try {
            setupNew(tableDestination);
        } catch (final IOException ioFail) {
            everything.setSelectedIndex(JDialogVOIStatistics.VOI_TAB);

            if (noisyProcess) {
                MipavUtil.displayWarning(ioFail.getMessage());
            }

            fileSelectorPanel.highlight();

            return false;
        } catch (final SecurityException securityFail) {
            everything.setSelectedIndex(JDialogVOIStatistics.VOI_TAB);

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
     * @param selected DOCUMENT ME!
     * 
     * @return File a directory
     * 
     * @exception IOException -- failure to create the directory
     * @throws SecurityException DOCUMENT ME!
     */
    private File setupNew(final File selected) throws IOException, SecurityException {

        // make a 'destDirectory' file from the dir text if it isn't already made.
        if (selected == null) {
            throw new IOException("No file selected!");
        }

        if (selected.exists() && (selected.length() != 0) && !overwriteBox.isSelected()) {

            // ask permission to replace the file
            if (noisyProcess) {
                final String[] possibilities = {"Overwrite", "Cancel"};
                final int result = JOptionPane.showOptionDialog(this, "\"" + selected.getPath()
                        + "\" already exists.\nWhat do you want to do with it?", "File exists...",
                        JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE, null, possibilities, new Integer(0));

                if (result == 0) {

                    if ( !selected.delete()) {
                        throw new IOException("File cannot be deleted.");
                    }
                } else {
                    throw new IOException("File already exists.  Not deleted.");
                }
            }
        }

        if ( !selected.getParentFile().canWrite()) {
            throw new IOException("Unable to write the file.  \n" + "Destination is not writeable.");
        }

        // verify the destination directory exists, and make it if it doesn't.
        if (selected.isDirectory()) {
            throw new IOException("Destination is a directory!  \n" + "Select a file.");
        }

        // do we have rights to write here? and an error if we can't?
        if ( !selected.canWrite()) {

            if ( !selected.createNewFile()) {
                throw new IOException("Error in creating destination.  " + "Write rights maybe?");
            }
        }

        return selected;
    }

    /**
     * use the GUI results to set up the variables needed to run the algorithm.
     * 
     * @return <code>true</code> if parameters set successfully, <code>false</code>otherwise.
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
        if ( !isTableDestinationOkay()) {
            return false; // error notification already done.
        }

        // check to make sure at least one statistic has
        // been selected to calculate
        if ( !isStatisticSelectionOkay()) {
            return false; // error notification already done.
        }

        checkList = checkBoxPanel.getSelectedList();
        logFileText = new StringBuffer(createNewLogfile());

        // sets the VOIs with the selected exclude range, if it exists,
        // and does this for each VOI in the selected list.
        // NOTE: it might be better to do the setting through either the
        // algorithm or the VOI vector.
        for (int i = 0; i < selectedList.getModel().getSize(); i++) {

            try {
                System.out.println( ((VOI) selectedList.getModel().getElementAt(i)).getName());
                ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnore(outputOptionsPanel.getMaximumExclude()
                        .floatValue());
            } catch (final NullPointerException noMax) {
                ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnore(Float.MAX_VALUE);
            }

            try {
                System.out.println( ((VOI) selectedList.getModel().getElementAt(i)).getName());
                ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnore(outputOptionsPanel.getMinimumExclude()
                        .floatValue());
            } catch (final NullPointerException noMax) {
                ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnore( -Float.MAX_VALUE);
            }
        }

        processType = outputOptionsPanel.getProcessType();
        precision = outputOptionsPanel.getPrecision();
        doForce = outputOptionsPanel.doForcePrecision();
        showTotals = outputOptionsPanel.isShowTotals();

        if (overwriteBox.isSelected()) {
            tableDestinationUsage = JDialogVOIStatistics.OVERWRITE;
        } else {
            tableDestinationUsage = JDialogVOIStatistics.APPEND;
        }

        // notification will turn buttons back on
        everything.setSelectedIndex(JDialogVOIStatistics.LOG_TAB);
        cancelButton.setEnabled(false);
        OKButton.setEnabled(false);

        return true;
    }

    /**
     * Method for updating the table and GUI after the algorithm has completed (Not for script-running).
     */
    protected void updateDialog() {

        // notification will turn buttons back on
        cancelButton.setEnabled(true);
        OKButton.setEnabled(true);

        // get output data out of the notifier
        // getStatisticsData((AlgorithmVOIProps)event);

        if ( !calculator.isCompleted()) {
            return;
        }

        int totalCount = 0;
        String str;
        VOIStatisticalProperties properties;
        Vector[] contours;

        if ( (processType == AlgorithmVOIProps.PROCESS_PER_SLICE)
                || (processType == AlgorithmVOIProps.PROCESS_PER_SLICE_AND_CONTOUR)) {
            final ListModel list = selectedList.getModel();

            if (logModel.getColumnIndex("Name, Slice, Contour") == -1) {
                logModel.addColumn("Name, Slice, Contour");
            }

            for (int i = 0; i < VOIStatisticList.numberOfStatistics; i++) {

                if (checkList[i]) {

                    if (logModel.getColumnStartsWithIndex(VOIStatisticList.statisticDescription[i]) == -1) {

                        if ( (VOIStatisticList.statisticDescription[i].indexOf("Volume") != -1) && (xUnits == yUnits)
                                && (xUnits == zUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = image.getFileInfo(0).getVolumeUnitsOfMeasureStr();
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                        } else if ( (VOIStatisticList.statisticDescription[i].indexOf("Area") != -1)
                                && (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = image.getFileInfo(0).getAreaUnitsOfMeasureStr();
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                        } else if ( (VOIStatisticList.statisticDescription[i].indexOf("Perimeter") != -1)
                                && (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                        } else if (VOIStatisticList.statisticDescription[i].indexOf("Principal Axis") != -1) {
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (degrees)");
                        } else if ( (VOIStatisticList.statisticDescription[i].indexOf("Major axis length") != -1)
                                && (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                        } else if ( (VOIStatisticList.statisticDescription[i].indexOf("Minor axis length") != -1)
                                && (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
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

                final String[] rowData = new String[logModel.getColumnCount()];
                final String[] totalData = new String[logModel.getColumnCount()];

                final String[] logRowData = new String[rowData.length];
                final String[] logTotalData = new String[rowData.length];

                for (int slice = 0; slice < contours.length; slice++) {
                    int count = 0;
                    int stop = 1;
                    String end = slice + ";";

                    if (calculator.getProcessType() == AlgorithmVOIProps.PROCESS_PER_SLICE_AND_CONTOUR) {
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
                                (slice) + ", " + // slice #, irrellevent to where contour is in image
                                ((VOIBase) contours[slice].get(num)).getLabel(); // contour #, held in label
                        totalData[0] = "Totals:";

                        logRowData[0] = new String(rowData[0]);
                        logTotalData[0] = new String(totalData[0]);

                        if (calculator.getProcessType() == AlgorithmVOIProps.PROCESS_PER_SLICE_AND_CONTOUR) {
                            end = slice + ";" + num;
                        }

                        // for each column in the row, print the statistic:
                        for (int k = 0; k < VOIStatisticList.statisticDescription.length; k++) {

                            if (logModel.getColumnBaseIndex(VOIStatisticList.statisticDescription[k]) != -1) {
                                count++;
                            }

                            if (checkList[k]) {

                                // if it's a color image and the property is min intensity, max intensity, avg
                                // intensity, or standard deviation of intensity, those properties were entered as Red,
                                // Green, Blue and we should display them differently.
                                if (calculator.isColor()
                                        && (VOIStatisticList.statisticDescription[k].indexOf("Intensity") != -1)) {
                                    String temp = "R: "
                                            + properties.getProperty(VOIStatisticList.statisticDescription[k] + "Red"
                                                    + end);
                                    temp += " G: "
                                            + properties.getProperty(VOIStatisticList.statisticDescription[k] + "Green"
                                                    + end);
                                    temp += " B: "
                                            + properties.getProperty(VOIStatisticList.statisticDescription[k] + "Blue"
                                                    + end);
                                    rowData[count] = temp;
                                    logRowData[count] = temp;

                                    if (showTotals) {
                                        temp = " R: "
                                                + properties.getProperty(VOIStatisticList.statisticDescription[k]
                                                        + "RedTotal");
                                        temp += " G: "
                                                + properties.getProperty(VOIStatisticList.statisticDescription[k]
                                                        + "GreenTotal");
                                        temp += " B: "
                                                + properties.getProperty(VOIStatisticList.statisticDescription[k]
                                                        + "BlueTotal");
                                        totalData[count] = temp;
                                        logTotalData[count] = temp;
                                    }
                                } else {
                                    if (k != 18) {
                                        // Exclude largest distance
                                        rowData[count] = properties.getProperty(
                                                VOIStatisticList.statisticDescription[k] + end).replaceAll("\t", ", ");
                                        logRowData[count] = properties
                                                .getProperty(VOIStatisticList.statisticDescription[k] + end);
                                    }

                                    if (showTotals) {
                                        totalData[count] = properties.getProperty(
                                                VOIStatisticList.statisticDescription[k] + "Total").replaceAll("\t",
                                                ", ");
                                        logTotalData[count] = properties
                                                .getProperty(VOIStatisticList.statisticDescription[k] + "Total");
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

                if (showTotals) {
                    logModel.addRow(totalData);

                    String logText = "";

                    for (final String element : logTotalData) {
                        logText += element + "\t";
                    }

                    writeLogfileEntry(logText);
                }

                for (int k = 0; k < rowData.length; k++) {
                    rowData[k] = "";
                }

                logModel.addRow(rowData);
            }
        } else { // whole 3D VOI data

            final ListModel list = selectedList.getModel();

            if (logModel.getColumnIndex("Name, Slice, Contour") == -1) {
                logModel.addColumn("Name, Slice, Contour");
            }

            // add any columns which will be displayed, but not already displayed:
            for (int i = 0; i < VOIStatisticList.numberOfStatistics; i++) {

                if (checkBoxPanel.getSelectedList(VOIStatisticList.statisticDescription[i])) {

                    if (logModel.getColumnIndex(VOIStatisticList.statisticDescription[i]) == -1) {

                        if ( (VOIStatisticList.statisticDescription[i].indexOf("Volume") != -1) && (xUnits == yUnits)
                                && (xUnits == zUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = image.getFileInfo(0).getVolumeUnitsOfMeasureStr();
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                        } else if ( (VOIStatisticList.statisticDescription[i].indexOf("Area") != -1)
                                && (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = image.getFileInfo(0).getAreaUnitsOfMeasureStr();
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                        } else if ( (VOIStatisticList.statisticDescription[i].indexOf("Perimeter") != -1)
                                && (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                        } else if (VOIStatisticList.statisticDescription[i].indexOf("Principal Axis") != -1) {
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (degrees)");
                        } else if ( (VOIStatisticList.statisticDescription[i].indexOf("Major axis length") != -1)
                                && (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                        } else if ( (VOIStatisticList.statisticDescription[i].indexOf("Minor axis length") != -1)
                                && (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                        } else if ( (VOIStatisticList.statisticDescription[i].indexOf("Largest slice distance") != -1)
                                && (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                        } else if ( (VOIStatisticList.statisticDescription[i].indexOf("Largest distance") != -1)
                                && (xUnits == zUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
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

                final String[] rowData = new String[logModel.getColumnCount()];
                rowData[0] = list.getElementAt(i).toString();

                int count = 0;

                for (int k = 0; k < VOIStatisticList.statisticDescription.length; k++) {

                    if (logModel.getColumnBaseIndex(VOIStatisticList.statisticDescription[k]) != -1) {
                        count++;
                    }

                    if (checkList[k]) {

                        // if it's a color image and the property is min intensity, max intensity, avg intensity,
                        // or standard deviation of intensity, those properties were entered as Red, Green, Blue and
                        // we should display them differently.
                        if (calculator.isColor()
                                && (VOIStatisticList.statisticDescription[k].indexOf("Intensity") != -1)) {
                            String temp = "R: "
                                    + properties.getProperty(VOIStatisticList.statisticDescription[k] + "Red");
                            temp += " G: " + properties.getProperty(VOIStatisticList.statisticDescription[k] + "Green");
                            temp += " B: " + properties.getProperty(VOIStatisticList.statisticDescription[k] + "Blue");
                            rowData[count] = temp;
                        } else {
                            rowData[count] = properties.getProperty(VOIStatisticList.statisticDescription[k]);
                        }
                    }
                } // end for each column

                count = 0;
                logModel.addRow(rowData);

                String logText = "";

                for (final String element : rowData) {
                    logText += element + "\t";
                }

                writeLogfileEntry(logText);

                for (int k = 0; k < rowData.length; k++) {
                    rowData[k] = "";
                }

            }
        }
        // finalise the output details
    }

    /**
     * places the input String at the end of the output log; appends a trailing newline.
     * 
     * @param logentry DOCUMENT ME!
     */
    protected void writeLogfileEntry(final String logentry) {
        logFileText.append(logentry + '\n');
    }

    /**
     * Any further writes to the keyLog will be writing into a fresh, new keyLog. Writing to the keyFile later will
     * overwrite the keyFile already there.
     */
    private void writeStatisticFile() {
        FileWriter statFW;
        final File statFile = new File(tableDestination.getAbsolutePath());

        try {

            if (statFile.exists()) {

                if (tableDestinationUsage == JDialogVOIStatistics.OVERWRITE) {
                    statFile.delete();
                }
            }
        } catch (final SecurityException se) {

            if (noisyProcess) {
                MipavUtil.displayError("security violation incurred while creating \"" + tableDestination.getName()
                        + "\"; \n" + "is destination directory still writable?  " + "Table file not written.");
            }

            Preferences.debug("security violation incurred while creating \"" + tableDestination.getName() + "\";\n");

            return;
        }

        try {

            if ( !statFile.createNewFile()) { /* there was an error here! */
            }
        } catch (final IOException io) {
            Preferences.debug("IOexception error in creating statFile!  threw "
                    + "exception rather than createNewFile == false;\n" + io);
            io.printStackTrace();
            Preferences.debug("IO exception while writing VOIStatistic's \"" + tableDestination.getAbsolutePath()
                    + "\"\n");

            return;
        } catch (final SecurityException se) {

            if (noisyProcess) {
                MipavUtil.displayError("security violation incurred while creating \""
                        + tableDestination.getAbsolutePath() + "\"; \n" + "is destination directory still writable?  "
                        + "Table file not written.");
            }

            Preferences.debug("security violation incurred while creating \"" + tableDestination.getAbsolutePath()
                    + "\";\n");

            return;
        }

        try {

            if (tableDestinationUsage == JDialogVOIStatistics.OVERWRITE) {
                statFW = new FileWriter(statFile.getAbsolutePath(), false);
                statFW.write(logFileText.toString(), 0, logFileText.length());
            } else if (tableDestinationUsage == JDialogVOIStatistics.APPEND) {
                statFW = new FileWriter(statFile.getAbsolutePath(), true);
                statFW.write(logFileText.toString());
            } else { // WRITE
                statFW = new FileWriter(statFile.getAbsolutePath());
                statFW.write(logFileText.toString(), 0, logFileText.length());
            }

            statFW.close();
        } catch (final IOException ioe) {

            if (noisyProcess) {
                MipavUtil.displayError("error writing the logging to \"" + tableDestination.getAbsolutePath() + "\""); // figure
                // out
                // where
                // to
                // store
                // somewhere
                // else?
            }
        }

        logFileText.delete(0, logFileText.length() - 1); // empty out the buffer
    }

    // ~ Inner Classes
    // --------------------------------------------------------------------------------------------------

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
            setBackArrowVisble(false);
            setDeleteEnabled(true);
        }

        /**
         * Checks if all super's action commands are used, and ensures that the delete button removes items from listB,
         * and that duplicate items in listB are not repeated.
         * 
         * @param ae DOCUMENT ME!
         */
        public void actionPerformed(final ActionEvent ae) {

            // the buttons won't work getting lists if either are null.
            if ( (getRightList() == null) || (getLeftList() == null)) {
                return;
            }

            // check on other actions.
            super.actionPerformed(ae);

            final String command = ae.getActionCommand();

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
        private final JTextField boundA;

        /** DOCUMENT ME! */
        private final JTextField boundB;

        /** DOCUMENT ME! */
        private final JComboBox excludeSelection;

        /** DOCUMENT ME! */
        private final JPanel exclusionPanel;

        /** DOCUMENT ME! */
        private Float lowerLimit;

        /** DOCUMENT ME! */
        private final JCheckBox permitExclusion;

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

            final JPanel selection = new JPanel(new BorderLayout());
            final JPanel excludeSelectionPanel = new JPanel(new BorderLayout());
            final JLabel excludeLabel = new JLabel("Exclude Pixels");
            excludeLabel.setFont(MipavUtil.font12);
            excludeSelectionPanel.add(excludeLabel, BorderLayout.NORTH);

            final String[] selectors = {"Between", "Above", "Below", "Outside"};
            excludeSelection = new JComboBox(selectors);
            excludeSelection.setActionCommand("Exclusion Range");
            excludeSelection.setEditable(false);
            excludeSelection.setEnabled(false);
            excludeSelection.addActionListener(this);
            excludeSelectionPanel.add(excludeSelection, BorderLayout.CENTER);
            selection.add(excludeSelectionPanel, BorderLayout.NORTH);

            final JPanel values = new JPanel();
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
         * <p>
         * Checks state of:
         * </p>
         * 
         * <ul>
         * <li>Enables or disables the exclusion drop-down and the text boxes based on the state of the checkbox</li>
         * <li>Changes the visibility of the text-boxes based on the state of the exclusion dropdown;
         * &quot;Between&quot; displays both text boxes, &quot;Above&quot; only displays the lower cutoff box, and
         * &quot;Below&quot; displays only the upper cutoff box.</li>
         * </ul>
         * 
         * @param e the ChangeEvent to watch.
         */
        public void actionPerformed(final ActionEvent e) {

            if (e.getSource().equals(permitExclusion)) {

                if (permitExclusion.isSelected()) {
                    excludeSelection.setEnabled(true);
                    selectRangeInput();
                } else {
                    excludeSelection.setEnabled(false);
                    boundA.setEnabled(false);
                    boundB.setEnabled(false);
                    rangeFlag = JDialogVOIStatistics.NO_RANGE;

                    // storeLimitValues(); // store before blanking the values
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
         * <p>
         * There is a side-effect in that when the permitExclusion checkbox is unchecked, the lower bound returned is
         * <CODE>null</CODE>. This means that relying on the upperbound also means relying on the text fields being
         * editable as well.
         * </p>
         * 
         * @return lower bound text as a Float; null is returned if the panel is not set to be used or one of the text
         *         entries is empty or not a number.
         */
        public Float getLowerBound() {

            if ( !permitExclusion.isSelected()) {
                return null;
            }

            try {

                if (Float.parseFloat(boundA.getText()) < Float.parseFloat(boundB.getText())) {
                    return new Float(boundA.getText());
                } else {
                    return new Float(boundB.getText());
                }
            } catch (final NumberFormatException notANumber) {
                return null;
            } catch (final NullPointerException noNumber) {
                return null;
            }
        }

        /**
         * Returns the upper bound text as a number. May be too positive for some applications.
         * 
         * <p>
         * There is a side-effect in that when the permitExclusion checkbox is unchecked, the upper bound returned is
         * <CODE>null</CODE>. This means that relying on the upperbound also means relying on the text fields being
         * editable as well.
         * </p>
         * 
         * @return upper bound text as a Float; null is returned if the panel is not set to be used or one of the text
         *         entries is empty or not a number.
         */
        public Float getUpperBound() {

            if ( !permitExclusion.isSelected()) {
                return null;
            }

            try {

                if (Float.parseFloat(boundA.getText()) > Float.parseFloat(boundB.getText())) {
                    return new Float(boundA.getText());
                } else {
                    return new Float(boundB.getText());
                }
            } catch (final NumberFormatException notANumber) {
                return null;
            } catch (final NullPointerException noNumber) {
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
            rangeFlag = JDialogVOIStatistics.NO_RANGE;

            if (excludeSelection.getSelectedItem().equals("Above")) {
                rangeFlag = JDialogVOIStatistics.BETWEEN;
                storeLimitValues();
                boundB.setEnabled(false);
                boundB.setText(Float.toString(Float.MAX_VALUE));
                boundA.setEnabled(true);

                try {
                    boundA.setText(lowerLimit.toString());
                } catch (final NullPointerException noLower) {
                    boundA.setText(Float.toString( -Float.MAX_VALUE));
                }
            } else if (excludeSelection.getSelectedItem().equals("Below")) {
                rangeFlag = JDialogVOIStatistics.BETWEEN;
                storeLimitValues();
                boundA.setEnabled(false);
                boundA.setText(Float.toString( -Float.MAX_VALUE));
                boundB.setEnabled(true);

                try {
                    boundB.setText(upperLimit.toString());
                } catch (final NullPointerException noUpper) {
                    boundB.setText(Float.toString(Float.MAX_VALUE));
                }
            } else if (excludeSelection.getSelectedItem().equals("Between")) {

                // set both text-inputs as needed, then make them editable
                rangeFlag = JDialogVOIStatistics.BETWEEN;
                storeLimitValues();

                if (lowerLimit != null) {
                    boundA.setText(lowerLimit.toString());
                } else {
                    boundA.setText(Float.toString( -Float.MAX_VALUE));
                }

                if (upperLimit != null) {
                    boundB.setText(upperLimit.toString());
                } else {
                    boundB.setText(Float.toString(Float.MAX_VALUE));
                }

                boundA.setEnabled(true);
                boundB.setEnabled(true);
            } else if (excludeSelection.getSelectedItem().equals("Outside")) {
                rangeFlag = JDialogVOIStatistics.OUTSIDE;
                storeLimitValues();

                // set both text-inputs as needed, then make them editable
                if (lowerLimit != null) {
                    boundA.setText(lowerLimit.toString());
                } else {
                    boundA.setText(Float.toString( -Float.MAX_VALUE));
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
         * @param floatValue lower bound string
         */
        public void setLowerBound(final String floatValue) {
            boundA.setText(floatValue);
        }

        /**
         * Set the upper bound from the script dialog.
         * 
         * @param floatValue Maximum value string
         */
        public void setUpperBound(final String floatValue) {
            boundB.setText(floatValue);
        }

        /**
         * Tries to store the values held in the text areas to temporary storage. It only does so if there are valid
         * (that is, numbers and that they are neither infinite nor at the maximum or minimum value.
         * 
         * @see Float#MAX_VALUE
         * @see Float#MIN_VALUE
         */
        protected void storeLimitValues() {

            /*
             * try to store the upper and lower bounds; only do so if they are valid values to store
             */
            try {

                if ( !getUpperBound().isInfinite() && !getUpperBound().isNaN()
                        && (getUpperBound().floatValue() != Float.MAX_VALUE)) {
                    upperLimit = getUpperBound();
                }
            } catch (final NullPointerException inValidNumber) {
                /* nothing t do */
            }

            try {

                if ( !getLowerBound().isInfinite() && !getLowerBound().isNaN()
                        && (getLowerBound().floatValue() != -Float.MAX_VALUE)) {
                    lowerLimit = getLowerBound();
                }
            } catch (final NullPointerException inValidNumber) {
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
            final ButtonGroup outputFormat = new ButtonGroup();
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
         * @return the state of the tab-delimited option.
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

        /** DOCUMENT ME! */
        JCheckBox forceDecimal;

        /** User can choose the precision to display. */
        JComboBox precisionBox;

        /** A check box to opt for VOI totals. */
        JCheckBox showTotals;

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

            final String[] precisionStr = new String[11];

            for (int i = 0; i < precisionStr.length; i++) {
                precisionStr[i] = String.valueOf(i);
            }

            precisionBox = new JComboBox(precisionStr);
            precisionBox.setFont(MipavUtil.font12);
            precisionBox.setSelectedIndex(4);

            forceDecimal = new JCheckBox("Force decimal display", false);
            forceDecimal.setFont(MipavUtil.font12);

            final JPanel precisionPanel = new JPanel();
            precisionPanel.setBorder(buildTitledBorder("Precision"));
            precisionPanel.add(precisionBox);
            precisionPanel.add(forceDecimal);

            final ButtonGroup group = new ButtonGroup();
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
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public boolean doForcePrecision() {
            return forceDecimal.isSelected();
        }

        /**
         * Method to retrieve the value for the exclusion panel's maximum.
         * 
         * @return DOCUMENT ME!
         */
        public Float getMaximumExclude() {

            try {
                return excluder.getUpperBound();
            } catch (final NullPointerException noUpper) {
                return null;
            }
        }

        /**
         * Method to retrieve the value for the exclusion panel's minimum.
         * 
         * @return DOCUMENT ME!
         */
        public Float getMinimumExclude() {

            try {
                return excluder.getLowerBound();
            } catch (final NullPointerException noLower) {
                return null;
            }
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int getPrecision() {
            return precisionBox.getSelectedIndex();
        }

        /**
         * Gets the output calculation type.
         * 
         * @return type VOI, Contour, Slice
         */
        public int getProcessType() {

            if (byTotalVOI.isSelected()) {
                return AlgorithmVOIProps.PROCESS_PER_VOI;
            } else if (byContour.isSelected()) {
                return AlgorithmVOIProps.PROCESS_PER_SLICE_AND_CONTOUR;
            } else {
                return AlgorithmVOIProps.PROCESS_PER_SLICE;
            }
        }

        /**
         * Method to retrieve the selected value for contour-only calculation.
         * 
         * @return DOCUMENT ME!
         */
        public boolean isByContour() {
            return byContour.isSelected();
        }

        /**
         * Method to retrieve the selected value for slice-only calculation.
         * 
         * @return DOCUMENT ME!
         */
        public boolean isBySlice() {
            return bySlice.isSelected();
        }

        /**
         * Method to retrieve the selected value for VOI-only calculation.
         * 
         * @return <code>true</code> VOI only calculation <code>false</code> otherwise.
         */
        public boolean isByVOI() {
            return byTotalVOI.isSelected();
        }

        /**
         * Method to retrieve the selected value for totals calculation.
         * 
         * @return DOCUMENT ME!
         */
        public boolean isShowTotals() {
            return showTotals.isSelected();
        }

        /**
         * Method to set the selected value for slice-only calculation.
         * 
         * @param flag DOCUMENT ME!
         */
        public void setBySliceEnabled(final boolean flag) {
            bySlice.setEnabled(flag);
        }

        /**
         * Set the upper bound value.
         * 
         * @param floatValue float value string
         */
        public void setMaximumExclude(final String floatValue) {

            try {
                excluder.setUpperBound(floatValue);
            } catch (final NullPointerException noUpper) {
                return;
            }
        }

        /**
         * Set the lower bound value.
         * 
         * @param floatValue float value string
         */
        public void setMinimumExclude(final String floatValue) {

            try {
                excluder.setLowerBound(floatValue);
            } catch (final NullPointerException noLower) {
                return;
            }
        }

        /**
         * Set the output calculation by type.
         * 
         * @param type VOI, Contour or Slice
         */
        public void setProcessType(final int type) {

            if (type == AlgorithmVOIProps.PROCESS_PER_VOI) {
                byTotalVOI.setSelected(true);
            } else if (type == AlgorithmVOIProps.PROCESS_PER_SLICE_AND_CONTOUR) {
                byContour.setSelected(true);
            } else if (type == AlgorithmVOIProps.PROCESS_PER_SLICE) {
                bySlice.setSelected(true);
            }
        }

        /**
         * Set the showTotal calculation flag.
         * 
         * @param flag <code>true</code> show total voxels <code>false</code> otherwise
         */
        public void setShowTotals(final boolean flag) {
            showTotals.setSelected(flag);
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
         * @param added DOCUMENT ME!
         */
        public void addedCurve(final VOIEvent added) {
        /* not interested in adding curves */
        }

        /**
         * We are not interested in removing Curves, so this method is empty.
         * 
         * @param added DOCUMENT ME!
         */
        public void removedCurve(final VOIEvent added) {
        /* not interested in removing curves */
        }

        public void colorChanged(final Color c) {
        /* not interested in color change */
        }

        /**
         * Handles the VOI being selected. -- a state-change.
         * 
         * @param selection DOCUMENT ME!
         */
        public void selectedVOI(final VOIEvent selection) {
            volumesList.setSelectedValue(selection.getSource(), selection.getState());
        }

        /**
         * Goes through the list selection event's JList source to find selected VOI list items. The selected VOIs are
         * then instructed to be &quot;active&quot;. Any other VOIs in the list are set to be not active.
         * 
         * @param lse DOCUMENT ME!
         */
        public void valueChanged(final ListSelectionEvent lse) {
            final JList imageVOIlist = (JList) lse.getSource();

            // go through all VOIs in the list. if the item is
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

            // userInterface.getActiveImageFrame().repaint();
            userInterface.getActiveImageFrame().updateImages();
        }
    }

}
