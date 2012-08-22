package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.event.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JPanelPixelExclusionSelector.RangeType;

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
    
    private int activeVolume;
    
    private int initialActiveVolume;
    
    private int tDim;
    
    private AlgorithmSubset subsetAlgo;
    
    private ModelImage subsetImage = null;
    
    private boolean doAllVolumes = false;
    
    private ViewVOIVector processList = null;
    
    private RangeType r = RangeType.NO_RANGE;

    /** DOCUMENT ME! */
    private float rangeMaximum = 0f;

    /** DOCUMENT ME! */
    private float rangeMinimum = 0f;
    
    private float rangeMaximumR = 0f;
    
    private float rangeMinimumR = 0f;
    
    private float rangeMaximumG = 0f;
    
    private float rangeMinimumG  = 0f;
    
    private float rangeMaximumB = 0f;
    
    private float rangeMinimumB = 0f;

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

    /** The units printed to the logModel, set in createNewLogile */
    protected int xUnits = -1, yUnits = -1, zUnits = -1;

    /** When running as a script, holds the pixel exclusion range. */
    private RangeType scriptRange;

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
        
        //this model contains all relevant statistics calculations
        logModel = new ViewTableModel();
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
            //MipavUtil.showHelp("GroupStat001");
            MipavUtil.showWebHelp("Calculating_statistics_on_VOI_groups");
        }
    }

    /**
     * resets the volumes list to the current VOIVector. adds the highlighter to the new VOI.
     * 
     * @param voiEvent DOCUMENT ME!
     */
    public void addedVOI(final VOIVectorEvent voiEvent) {
        final VOIVector voiList = (VOIVector) voiEvent.getSource();
        final Vector<VOI> volumesVector = new Vector<VOI>();

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
    	
    	//update stat log to include calculations from AlgorithmVOIProps (or alternate statistics computer)
    	updateStatLog();
    	
        if (( !isScriptRunning()) && ((!doAllVolumes) || (doAllVolumes && (activeVolume == tDim-1)))) {
            updateDialog(); //do not update GUI when in script
        	insertScriptLine();
        }
        
        //write stat log to file specified by user
        if ((!doAllVolumes) || (doAllVolumes && (activeVolume == tDim-1))) {
            writeStatisticFile();
        }
        
        if (doAllVolumes && (activeVolume < tDim - 1)) {
            anotherCall();    
         }
         else if (image.getNDims() >= 4) {
             subsetImage.disposeLocal();
             subsetImage = null;
         }

        System.gc(); // to reclaim lost land.
    }
    
    private void anotherCall() {
        activeVolume++;
        subsetAlgo = new AlgorithmSubset(image, subsetImage, AlgorithmSubset.REMOVE_T, activeVolume); 
        subsetAlgo.run();
        
        calculator = new AlgorithmVOIProps(subsetImage, processType, r, processList);
        calculator.setPrecisionDisplay(precision, doForce);
        calculator.setSelectedStatistics( checkList );
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
     * Refreshes the list of available and selected VOIs.
     * 
     * @param VOIlist imageActive's current VOIVector
     */
    public void refreshVOIList(final VOIVector VOIlist) {
        selectedList.setListData(new Vector<Object>());

        final Vector<VOI> volumesVector = new Vector<VOI>();
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
        final Vector<VOI> volumesVector = new Vector<VOI>();

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
        processList = new ViewVOIVector(selectedList.getModel().getSize());
        for(int i=0; i<selectedList.getModel().getSize(); i++) {
            processList.add((VOI)selectedList.getModel().getElementAt(i));
        }
        
        r = RangeType.NO_RANGE;
        if(isScriptRunning()) {
            r = scriptRange;
        } else {
            r = outputOptionsPanel.getExcluder().getRangeFlag();
        }
        doAllVolumes = false;
        tDim = 1;
        int destExtents[] = null;
        activeVolume = 0;
        if (image.getNDims() >= 4) {
            tDim = image.getExtents()[3];
            destExtents = new int[3];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];
            // Make result image of same image-type (eg., BOOLEAN, FLOAT, INT)
            subsetImage = new ModelImage(image.getType(), destExtents, image.getImageName());
            // Set doAllVolumes
            outputOptionsPanel.getVolumeType();
            if (!doAllVolumes) {
                activeVolume = image.getParentFrame().getComponentImage().getTimeSlice();
            }
        }
        initialActiveVolume = activeVolume;
        
        if (tDim == 1) {
            subsetImage = image;
        }
        
            
        if (tDim > 1) {
            subsetAlgo = new AlgorithmSubset(image, subsetImage, AlgorithmSubset.REMOVE_T, activeVolume); 
            subsetAlgo.run();
        }
        calculator = new AlgorithmVOIProps(subsetImage, processType, r, processList);
        calculator.setPrecisionDisplay(precision, doForce);
        calculator.setSelectedStatistics( checkList );
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

        ViewUserInterface.getReference().getMessageFrame().append("A table file containing all statistics has been written to:\n "+
        															tableDestination.getAbsolutePath()+"\n", ViewJFrameMessage.DATA);
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

        String rangeFlag = scriptParameters.getParams().getString("do_use_exclusion_range");

        scriptRange = RangeType.valueOf(rangeFlag);
        
        if (scriptRange != RangeType.NO_RANGE) {
            if (image.isColorImage()) {
                rangeMinimumR = scriptParameters.getParams().getFloat("exclusion_range_minr");
                rangeMaximumR = scriptParameters.getParams().getFloat("exclusion_range_maxr");
                rangeMinimumG = scriptParameters.getParams().getFloat("exclusion_range_ming");
                rangeMaximumG = scriptParameters.getParams().getFloat("exclusion_range_maxg");
                rangeMinimumB = scriptParameters.getParams().getFloat("exclusion_range_minb");
                rangeMaximumB = scriptParameters.getParams().getFloat("exclusion_range_maxb");
    
                for (int i = 0; i < selectedList.getModel().getSize(); i++) {
    
                    try {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnoreR(rangeMaximumR);
                    } catch (final NullPointerException noMax) {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnoreR(Float.MAX_VALUE);
                    }
    
                    try {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnoreR(rangeMinimumR);
                    } catch (final NullPointerException noMax) {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnoreR( -Float.MAX_VALUE);
                    }
                    
                    try {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnoreG(rangeMaximumG);
                    } catch (final NullPointerException noMax) {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnoreG(Float.MAX_VALUE);
                    }
    
                    try {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnoreG(rangeMinimumG);
                    } catch (final NullPointerException noMax) {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnoreG( -Float.MAX_VALUE);
                    }
                    
                    try {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnoreB(rangeMaximumB);
                    } catch (final NullPointerException noMax) {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnoreB(Float.MAX_VALUE);
                    }
    
                    try {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnoreB(rangeMinimumB);
                    } catch (final NullPointerException noMax) {
                        ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnoreB( -Float.MAX_VALUE);
                    }
                }    
            } // if (image.isColorImage())
            else { // black and white image
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
            } // else black and white image
        }

        checkList = scriptParameters.getParams().getList("stat_checklist").getAsBooleanArray();

        createNewLogModel();

        tableDestinationUsage = scriptParameters.getParams().getInt("output_writing_behavior");
        if (scriptParameters.getParams().containsParameter("voi_stats_output_file")) {
            tableDestination = new File(scriptParameters.getParams().getString("voi_stats_output_file"));
        } else {
            tableDestination = new File(image.getFileInfo(0).getFileDirectory() + File.separator + image.getImageName()
                    + ".table");
        }

        processType = scriptParameters.getParams().getInt("processing_level");
        doAllVolumes = scriptParameters.getParams().getBoolean("do_all_volumes");
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
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_all_volumes", doAllVolumes));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_show_totals", showTotals));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_use_exclusion_range", outputOptionsPanel.getExcluder().getRangeFlag().name()));

        if (outputOptionsPanel.getExcluder().getRangeFlag() != RangeType.NO_RANGE) {
            if (image.isColorImage()) {
                scriptParameters.getParams().put(
                        ParameterFactory.newParameter("exclusion_range_minr", outputOptionsPanel.getExcluder().getLowerBoundR()
                                .floatValue()));
                scriptParameters.getParams().put(
                        ParameterFactory.newParameter("exclusion_range_maxr", outputOptionsPanel.getExcluder().getUpperBoundR()
                                .floatValue()));    
                scriptParameters.getParams().put(
                        ParameterFactory.newParameter("exclusion_range_ming", outputOptionsPanel.getExcluder().getLowerBoundG()
                                .floatValue()));
                scriptParameters.getParams().put(
                        ParameterFactory.newParameter("exclusion_range_maxg", outputOptionsPanel.getExcluder().getUpperBoundG()
                                .floatValue()));   
                scriptParameters.getParams().put(
                        ParameterFactory.newParameter("exclusion_range_minb", outputOptionsPanel.getExcluder().getLowerBoundB()
                                .floatValue()));
                scriptParameters.getParams().put(
                        ParameterFactory.newParameter("exclusion_range_maxb", outputOptionsPanel.getExcluder().getUpperBoundB()
                                .floatValue()));    
            }
            else {
                scriptParameters.getParams().put(
                        ParameterFactory.newParameter("exclusion_range_min", outputOptionsPanel.getExcluder().getLowerBound()
                                .floatValue()));
                scriptParameters.getParams().put(
                        ParameterFactory.newParameter("exclusion_range_max", outputOptionsPanel.getExcluder().getUpperBound()
                                .floatValue()));
            }
        }

        scriptParameters.getParams().put(ParameterFactory.newParameter("output_precision", precision));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_force_precision", doForce));
        scriptParameters.getParams().put(
                ParameterFactory.newParameter("output_writing_behavior", tableDestinationUsage));
    }
    
    protected void buildCheckBoxPanel() {
        checkBoxPanel = new JPanelStatisticsList();
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
        
        // need to take out line VOIs, polyline VOIs, point VOIs

        everything = new JTabbedPane(SwingConstants.TOP);
        everything.setFont(MipavUtil.font12B);
        everything.insertTab("VOI selection", null, buildVOIPanel(voiList), // we must store this panel so we can
                // create a new listing later
                "Choose VOIs and statistics file", JDialogVOIStatistics.VOI_TAB);

        final JPanel statPanel = new JPanel(new BorderLayout());
        buildCheckBoxPanel();
        
        try {
            checkBoxPanel.setSliceCount(image.getExtents()[2]);
        } catch (ArrayIndexOutOfBoundsException aioobe) {

            // otherwise, this must be a 2d image.
            checkBoxPanel.setSliceCount(1);
        } finally {
            checkBoxPanel.setCheckBoxesEnabled();
        }
        
        boolean allClosed = true;
        for(int i=0; i<voiList.size(); i++) {
            VOIBaseVector v = voiList.get(i).getCurves();
            for(int j=0; j<v.size(); j++) {
                if(!v.get(j).isClosed()) {
                    allClosed = false;
                }
            }
        }
        checkBoxPanel.isOpenContour(!allClosed); //if all the contours are not closed, then some statistics need to be disabled
        
        
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
        setSize(800, 525); // decent size??
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
        selectedList.setListData(new Vector<Object>()); // = new JList();
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
        final Vector<VOI> volumesVector = new Vector<VOI>();
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
     * This method resets the logModel in preparation for writing new statistics values
     * 
     * @return the new logModel.
     */
    protected void createNewLogModel() {
        logFileText = new StringBuffer();
        logModel = new ViewTableModel();
        if(!isScriptRunning()) {
            logTable.setModel(logModel);
        }
        
        xUnits = image.getFileInfo(0).getUnitsOfMeasure()[0];
        yUnits = image.getFileInfo(0).getUnitsOfMeasure()[1];
        zUnits = Unit.UNKNOWN_MEASURE.getLegacyNum();

        if (image.getNDims() > 2) {
            zUnits = image.getFileInfo(0).getUnitsOfMeasure()[2];
        }
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
                        + "\" already exists.\nDo you want to overwrite this file?", "File exists...",
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
        createNewLogModel();

        // sets the VOIs with the selected exclude range, if it exists,
        // and does this for each VOI in the selected list.
        // NOTE: it might be better to do the setting through either the
        // algorithm or the VOI vector.
        for (int i = 0; i < selectedList.getModel().getSize(); i++) {
            
            if (image.isColorImage()) {
                try {
                    //System.out.println( ((VOI) selectedList.getModel().getElementAt(i)).getName());
                    ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnoreR(outputOptionsPanel.getExcluder().getUpperBoundR()
                            .floatValue());
                } catch (final NullPointerException noMax) {
                    ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnoreR(Float.MAX_VALUE);
                }
    
                try {
                    //System.out.println( ((VOI) selectedList.getModel().getElementAt(i)).getName());
                    ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnoreR(outputOptionsPanel.getExcluder().getLowerBoundR()
                            .floatValue());
                } catch (final NullPointerException noMax) {
                    ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnoreR( -Float.MAX_VALUE);
                }    
                
                try {
                    //System.out.println( ((VOI) selectedList.getModel().getElementAt(i)).getName());
                    ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnoreG(outputOptionsPanel.getExcluder().getUpperBoundG()
                            .floatValue());
                } catch (final NullPointerException noMax) {
                    ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnoreG(Float.MAX_VALUE);
                }
    
                try {
                    //System.out.println( ((VOI) selectedList.getModel().getElementAt(i)).getName());
                    ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnoreG(outputOptionsPanel.getExcluder().getLowerBoundG()
                            .floatValue());
                } catch (final NullPointerException noMax) {
                    ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnoreG( -Float.MAX_VALUE);
                }
                
                try {
                    //System.out.println( ((VOI) selectedList.getModel().getElementAt(i)).getName());
                    ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnoreB(outputOptionsPanel.getExcluder().getUpperBoundB()
                            .floatValue());
                } catch (final NullPointerException noMax) {
                    ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnoreB(Float.MAX_VALUE);
                }
    
                try {
                    //System.out.println( ((VOI) selectedList.getModel().getElementAt(i)).getName());
                    ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnoreB(outputOptionsPanel.getExcluder().getLowerBoundB()
                            .floatValue());
                } catch (final NullPointerException noMax) {
                    ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnoreB( -Float.MAX_VALUE);
                }
            } // if (image.isColorImage())
            else { // black and white image
                try {
                    //System.out.println( ((VOI) selectedList.getModel().getElementAt(i)).getName());
                    ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnore(outputOptionsPanel.getExcluder().getUpperBound()
                            .floatValue());
                } catch (final NullPointerException noMax) {
                    ((VOI) selectedList.getModel().getElementAt(i)).setMaximumIgnore(Float.MAX_VALUE);
                }
    
                try {
                    //System.out.println( ((VOI) selectedList.getModel().getElementAt(i)).getName());
                    ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnore(outputOptionsPanel.getExcluder().getLowerBound()
                            .floatValue());
                } catch (final NullPointerException noMax) {
                    ((VOI) selectedList.getModel().getElementAt(i)).setMinimumIgnore( -Float.MAX_VALUE);
                }
            } // else black and white image
        }

        processType = outputOptionsPanel.getProcessType();
        // Sets doAllVolumes
        outputOptionsPanel.getVolumeType();
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
     * Method for updating the GUI after the algorithm has completed, should not be called when scripting.
     */
    protected void updateDialog() {
    	
    	// notification of completion will turn buttons back on
    	cancelButton.setEnabled(true);
        OKButton.setEnabled(true);
    }
    
    /**
     * Method for updating the table after the algorithm has completed.  All relevant statistics are stored in the log model.
     */
    @SuppressWarnings("unchecked")
    protected void updateStatLog() {
        // get output data out of the notifier
        // getStatisticsData((AlgorithmVOIProps)event);

        if ( !calculator.isCompleted()) {
            Preferences.debug("Statistics are still being calculated.");
            return;
        }
        
        VOIStatisticalProperties properties;
        Vector<VOIBase> contours;

        final ListModel list = selectedList.getModel();
        
        if (activeVolume == initialActiveVolume) {
            //the stat log is always rewritten unless specified by user //TODO:add append option
            createNewLogModel();
            
            writeLogHeader();
        }

        // for each element in the list ....
        for (int i = 0; i < list.getSize(); i++) {
            properties = calculator.getVOIProperties((VOI) list.getElementAt(i));
            final String[] rowData = new String[logModel.getColumnCount()];
            final String[] totalData = new String[logModel.getColumnCount()];
            
            if ( processType == AlgorithmVOIProps.PROCESS_PER_CONTOUR ) {   
                contours = ((VOI) list.getElementAt(i)).getCurves();
                updateDialogRow((VOI) list.getElementAt(i), new Vector[]{contours}, properties, list, i, rowData, totalData);
            } else if(processType == AlgorithmVOIProps.PROCESS_PER_VOI) {
                if (image.getNDims() >= 4) {
                    rowData[0] = String.valueOf(activeVolume);
                    rowData[1] = list.getElementAt(i).toString();
                    logModel.addRow(updateRowStatistics((VOI) list.getElementAt(i), properties, rowData, totalData, "", 2));
                }
                else {
                    rowData[0] = list.getElementAt(i).toString();
                    logModel.addRow(updateRowStatistics((VOI) list.getElementAt(i), properties, rowData, totalData, "", 1));
                }
            } else { //processType is by slice or by slice-and-contour
                Vector<VOIBase>[] sortedContoursZ = ((VOI) list.getElementAt(i)).getSortedCurves(VOIBase.ZPLANE, image.getExtents()[2]);
                updateDialogRow((VOI) list.getElementAt(i), sortedContoursZ, properties, list, i, rowData, totalData);
                Vector<VOIBase>[] sortedContoursX = ((VOI) list.getElementAt(i)).getSortedCurves(VOIBase.XPLANE, image.getExtents()[0]);
                updateDialogRow((VOI) list.getElementAt(i), sortedContoursX, properties, list, i, rowData, totalData);
                Vector<VOIBase>[] sortedContoursY = ((VOI) list.getElementAt(i)).getSortedCurves(VOIBase.YPLANE, image.getExtents()[1]);
                updateDialogRow((VOI) list.getElementAt(i), sortedContoursY, properties, list, i, rowData, totalData);
            }

            if (showTotals) {
                logModel.addRow(totalData);
            }
        }
    }
    
    /**
     * Writes the statistic data for contours
     * @param voi 
     */
    protected void updateDialogRow(VOI voi, Vector<VOIBase>[] contours, 
            VOIStatisticalProperties properties, ListModel list, int i, 
            String[] rowData, String[] totalData) {
   
        for (int slice = 0; slice < contours.length; slice++) {
            if ( contours[slice].size() <= 0 ) {
                continue;
            }
            int count = 0;
            int stop = 1;
            String end = new String();

            if (processType == AlgorithmVOIProps.PROCESS_PER_SLICE_AND_CONTOUR || processType == AlgorithmVOIProps.PROCESS_PER_CONTOUR) {
                stop = contours[slice].size();
            }
            
            //this for loop produces different statistics depending on the processType, the statistics fetched are determined by the "end" string
            for (int num = 0; num < stop; num++) {
                if (image.getNDims() >= 4) {
                    rowData[count] = String.valueOf(activeVolume);
                    totalData[count] = String.valueOf(activeVolume);
                    count++;
                }
                rowData[count] = list.getElementAt(i).toString();
                totalData[count] = "Totals:";
                count++;
                
                if(processType == AlgorithmVOIProps.PROCESS_PER_SLICE || processType == AlgorithmVOIProps.PROCESS_PER_SLICE_AND_CONTOUR) {
                    rowData[count] = String.valueOf(slice);
                    totalData[count] = "";
                    end = slice + ";";
                    count++;
                } 
                if(processType == AlgorithmVOIProps.PROCESS_PER_CONTOUR || processType == AlgorithmVOIProps.PROCESS_PER_SLICE_AND_CONTOUR) {
                    rowData[count] = contours[slice].elementAt(num).getLabel();
                    totalData[count] = "";
                    end += contours[slice].elementAt(num).getLabel();
                    count++;
                }
                
                logModel.addRow(updateRowStatistics(voi, properties, rowData, totalData, end, count));
                rowData = new String[rowData.length];
                count = 0;
                end = new String();
            } // end for contours
        }
    }
    
    /**
     * Writes general statistics data for any VOI structure given a valid "end" modifier to specify
     * the property being fetched.
     * @param voi 
     */
    protected String[] updateRowStatistics(VOI voi, VOIStatisticalProperties properties, 
                                    String[] rowData, String[] totalData, String end, int count) {
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
                    rowData[count] = temp.replaceAll("[\\t+]", ", ").replaceAll("[\\n\\r+]", ":");

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
                        totalData[count] = temp.replaceAll("[\\t+]", ", ").replaceAll("[\\n\\r+]", ":");
                    }
                } else {
                	rowData[count] = properties.getProperty(
                			VOIStatisticList.statisticDescription[k] + end).replaceAll("[\\t+]", ", ").replaceAll("[\\n\\r+]", ":");

                    if (showTotals) {
                        totalData[count] = properties.getProperty(
                                VOIStatisticList.statisticDescription[k] + "Total").replaceAll("[\\t+]",
                                ", ").replaceAll("[\\n\\r+]", ":");
                    }
                }
                count++;
            }
        } // end for each row
        return rowData;
    }
    
    /**
     * Writes the column titles of selected statistics calculations to the logModel.
     */
    protected void writeLogHeader() {
        Vector<String> logModelCol = new Vector<String>();
        if (image.getNDims() >= 4) {
            logModelCol.add("Volume");
        }
        logModelCol.add("Name");
        if(processType == AlgorithmVOIProps.PROCESS_PER_SLICE || processType == AlgorithmVOIProps.PROCESS_PER_SLICE_AND_CONTOUR) {
            logModelCol.add("Slice");
        } 
        if(processType == AlgorithmVOIProps.PROCESS_PER_CONTOUR || processType == AlgorithmVOIProps.PROCESS_PER_SLICE_AND_CONTOUR) {
            logModelCol.add("Contour");
        }
        
        int totalCount = 0;
        String str;
        
        for (int i = 0; i < VOIStatisticList.numberOfStatistics; i++) {
    
            //add statistic to column list if selected by user
            if (checkList[i]) {
                if ( (VOIStatisticList.statisticDescription[i].indexOf("Volume") != -1) && (xUnits == yUnits)
                        && (xUnits == zUnits) && (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
                    str = image.getFileInfo(0).getVolumeUnitsOfMeasureStr().trim();
                    logModelCol.add(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                } else if ( (VOIStatisticList.statisticDescription[i].indexOf("Area") != -1)
                        && (xUnits == yUnits) && (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
                    str = image.getFileInfo(0).getAreaUnitsOfMeasureStr().trim();
                    logModelCol.add(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                } else if ( (VOIStatisticList.statisticDescription[i].indexOf("Perimeter") != -1)
                        && (xUnits == yUnits) && (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
                    str = (Unit.getUnitFromLegacyNum(xUnits)).getAbbrev();
                    logModelCol.add(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                } else if (VOIStatisticList.statisticDescription[i].indexOf("Principal Axis") != -1) {
                    logModelCol.add(VOIStatisticList.statisticDescription[i] + " (degrees)");
                } else if ( (VOIStatisticList.statisticDescription[i].indexOf("Major axis length") != -1)
                        && (xUnits == yUnits) && (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
                    str = (Unit.getUnitFromLegacyNum(xUnits)).getAbbrev();
                    logModelCol.add(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                } else if ( (VOIStatisticList.statisticDescription[i].indexOf("Minor axis length") != -1)
                        && (xUnits == yUnits) && (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
                    str = (Unit.getUnitFromLegacyNum(xUnits)).getAbbrev();
                    logModelCol.add(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                } else {
                    logModelCol.add(VOIStatisticList.statisticDescription[i]);
                }
    
                // total count used for total # of data elemets, need to add 3 if color
                // image and intensity related (R,G,B)
                totalCount++;
    
                if (calculator.isColor() && (VOIStatisticList.statisticDescription[i].indexOf("Intensity") != -1)) {
                    totalCount += 2;
                }
            }
        }
        logModel.setColumnIdentifiers(logModelCol);
    }

    /**
     * Converts the current logModel into either a tab-delimited text file or an XML file.
     * 
     * TODO: Fix XML functionality.
     * 
     * @return
     */
    @SuppressWarnings("unchecked")
    protected StringBuffer writeLogModelToString() {
        StringBuffer total = new StringBuffer();
        String newLine = System.getProperty("line.separator");
        //get column names
        for(int i=0; i<logModel.getColumnCount(); i++) {
            total.append(logModel.getColumnName(i)).append("\t");
        }
        total.append(newLine);
        
        //get total data
        Vector<Vector<?>> column = logModel.getDataVector();
        Vector<?> row;
        String cellEntry;
        for(int i=0; i<column.size(); i++) {
            row = column.get(i);
            for(int j=0; j<row.size(); j++) {
                if(row.get(j) == null || row.get(j).toString().length() == 0) {
                    cellEntry = " ";
                } else {
                    cellEntry = row.get(j).toString();
                }
                total.append(cellEntry).append("\t");
            }
            total.append(newLine);
        }
        
        return total;
    }

    /**
     * Writes out the statistics file based on the current logModel
     */
    protected void writeStatisticFile() {
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
            } else if (tableDestinationUsage == JDialogVOIStatistics.APPEND) {
                statFW = new FileWriter(statFile.getAbsolutePath(), true);           
            } else { // WRITE
                statFW = new FileWriter(statFile.getAbsolutePath());
            }
        
            logFileText = writeLogModelToString();
            statFW.write(logFileText.toString());
            statFW.flush();
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
        
        /** A radio button to select calculation by VOI contour. */
        JRadioButton byContourSlice;

        /** A radio button to select calculation by VOI slice. */
        JRadioButton bySlice;

        /** A radio button to select calculation by total VOI. */
        JRadioButton byTotalVOI;

        /** The Exclusion selector. */
        JPanelPixelExclusionSelector excluder;

        /** DOCUMENT ME! */
        JCheckBox forceDecimal;

        /** User can choose the precision to display. */
        JComboBox precisionBox;

        /** A check box to opt for VOI totals. */
        JCheckBox showTotals;
        
        /** A radio button to select calculation only for the active volume */
        JRadioButton activeVolumeButton = null;
        
        /** A radio button to select calculation for all volumes */
        JRadioButton allVolumesButton = null;

        /**
         * Creates a default view of the panel, including all options displayed. The option to calculate for the total
         * VOI only is selected.
         */
        public JPanelStatisticsOptions() {
            setBorder(buildTitledBorder("Statistics options"));
            setLayout(new GridBagLayout());
            GridBagConstraints gbc = new GridBagConstraints();
            gbc.weightx = 1;
            gbc.weighty = 1;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            StatisticsOptionsActionListener statOptionsListener = new StatisticsOptionsActionListener();
            byContour = new JRadioButton("By contour");
            byContour.setFont(MipavUtil.font12);
            byContour.addActionListener(statOptionsListener);
            byContourSlice = new JRadioButton("By contour & slice");
            byContourSlice.setFont(MipavUtil.font12);
            byContourSlice.addActionListener(statOptionsListener);
            bySlice = new JRadioButton("By slice only");
            bySlice.setFont(MipavUtil.font12);
            bySlice.addActionListener(statOptionsListener);

            showTotals = new JCheckBox("Show all totals");
            showTotals.setFont(MipavUtil.font12);

            byTotalVOI = new JRadioButton("By total VOI");
            byTotalVOI.setFont(MipavUtil.font12);
            byTotalVOI.addActionListener(statOptionsListener);
            
            //setting initial appearance
            byTotalVOI.setSelected(true);
            showTotals.setEnabled(false);
            
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
            group.add(byContourSlice);
            group.add(byTotalVOI);
            excluder = new JPanelPixelExclusionSelector(checkBoxPanel, image.isColorImage());

            if (image.getNDims() < 4) {
                gbc.gridx = 0;
                gbc.gridy = 0;
                gbc.gridwidth = 1;
                add(byContour, gbc);
                gbc.gridy = 1;
                add(byContourSlice, gbc);
                gbc.gridy = 2;
                add(bySlice, gbc);
                gbc.gridy = 3;
                add(byTotalVOI, gbc);
            }
            else { // image.getNDims() >= 4
                ButtonGroup volumeGroup = new ButtonGroup();
                activeVolumeButton = new JRadioButton("Active volume only");
                activeVolumeButton.setFont(MipavUtil.font12);
                volumeGroup.add(activeVolumeButton);
                
                allVolumesButton = new JRadioButton("Across all volumes");
                allVolumesButton.setFont(MipavUtil.font12);
                volumeGroup.add(allVolumesButton);
                
                activeVolumeButton.setSelected(true);
                allVolumesButton.setSelected(false);
                
                gbc.gridx = 0;
                gbc.gridy = 0;
                gbc.gridwidth = 1;
                add(activeVolumeButton, gbc);
                gbc.gridx = 1;
                add(byContour, gbc);
                gbc.gridx = 0;
                gbc.gridy = 1;
                add(allVolumesButton, gbc);
                gbc.gridx = 1;
                add(byContourSlice, gbc);
                JLabel blankLabel = new JLabel("   ");
                gbc.gridx = 0;
                gbc.gridy = 2;
                add(blankLabel, gbc);
                gbc.gridx = 1;
                add(bySlice, gbc);
                JLabel blankLabel2 = new JLabel("   ");
                gbc.gridx = 0;
                gbc.gridy = 3;
                add(blankLabel2, gbc);
                gbc.gridx = 1;
                add(byTotalVOI, gbc);
                gbc.gridwidth = 2;
                
            } // else image.getNDims() >= 4
            gbc.gridx = 0;
            gbc.gridy = 4;
            add(showTotals, gbc);
            gbc.gridy = 5;
            add(precisionPanel, gbc);
            gbc.gridy = 6;
            add(excluder, gbc);
            
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
         * @return the panel for listing pixel exclusion options
         */
        public JPanelPixelExclusionSelector getExcluder() {
            return excluder;
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
                return AlgorithmVOIProps.PROCESS_PER_CONTOUR;
            } else if (byContourSlice.isSelected()) {
                return AlgorithmVOIProps.PROCESS_PER_SLICE_AND_CONTOUR;
            } else {
                return AlgorithmVOIProps.PROCESS_PER_SLICE;
            }
        }
        
        public void getVolumeType() {
            if ((activeVolumeButton != null) && (allVolumesButton != null)) {
                if (activeVolumeButton.isSelected()) {
                    doAllVolumes = false;
                }
                else {
                    doAllVolumes = true;
                }
            }
            else {
                doAllVolumes = false;
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
            byContourSlice.setEnabled(flag);
            bySlice.setEnabled(flag);
        }

        /**
         * Set the output calculation by type.
         * 
         * @param type VOI, Contour or Slice
         */
        public void setProcessType(final int type) {

            if (type == AlgorithmVOIProps.PROCESS_PER_VOI) {
                byTotalVOI.setSelected(true);
            } else if (type == AlgorithmVOIProps.PROCESS_PER_CONTOUR) {
                byContour.setSelected(true);
            }  else if (type == AlgorithmVOIProps.PROCESS_PER_SLICE_AND_CONTOUR) {
                byContourSlice.setSelected(true);
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
        
        /**
         * An action listener for the statistics options that also controls which statistics can
         * be calculated.  (Some statistics should not be reported in a 3D setting)
         * 
         * @author senseneyj
         *
         */
        private class StatisticsOptionsActionListener implements ActionListener {

			public void actionPerformed(ActionEvent e) {
				int num = 1;
				if(byTotalVOI.isSelected()) {
					showTotals.setSelected(false);
					showTotals.setEnabled(false);
					if(image.getNDims() > 2) {
						num = image.getExtents()[2];
					}
				} else {
					showTotals.setEnabled(true);
				}

		        checkBoxPanel.setSliceCount(num);
			}
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
            //System.err.println( "VOIHighlighter.selectedVOI " + selection.getSource() + " " + selection.getState());
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
            if ( userInterface.getActiveImageFrame() != null ) {
                userInterface.getActiveImageFrame().updateImages();
            }
        }
    }

}
