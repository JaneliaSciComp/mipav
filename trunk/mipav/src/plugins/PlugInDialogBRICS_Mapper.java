import gov.nih.mipav.plugins.JDialogStandalonePlugin;

import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.WidgetFactory;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.tbi.commons.model.*;
import gov.nih.tbi.dictionary.model.DictionaryRestServiceModel.DataStructureList;
import gov.nih.tbi.dictionary.model.hibernate.*;
import gov.nih.tbi.repository.model.SubmissionType;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.*;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.text.WordUtils;
import org.apache.cxf.jaxrs.client.WebClient;
import org.apache.cxf.transport.http.HTTPConduit;

public class PlugInDialogBRICS_Mapper extends JFrame implements ActionListener, ChangeListener, TreeSelectionListener, MouseListener,
        PreviewImageContainer, WindowListener, FocusListener {
    private static final long serialVersionUID = -5516621806537554154L;

    private final Font serif12 = MipavUtil.font12, serif12B = MipavUtil.font12B;

    private WidgetFactory.ScrollTextArea logOutputArea;

    private JPanel dsMainPanel;
    private JScrollPane listPane;

    private JTable deTable;
    private ViewTableModel deTableModel; 
    private FormStructure formStructure;
    private List<FormStructure> formStructureList;
    private FormStructureData fsInfo = null;

    private JButton selectStructButton, loadCSVButton, finishButton, saveMapButton, editDataElementsButton, outputDirButton;

    private JTextField outputDirTextField;
    private String outputDirBase;
    
    private String csvFileDir;
    private File csvFile;
    private ArrayList<String> csvFieldNames;
    private Hashtable<String, String> csvStructRowData;
    
    private final Hashtable<RepeatableGroup, JPanel> groupPanelTable = new Hashtable<RepeatableGroup, JPanel>();
    private final Hashtable<RepeatableGroup, JButton> groupRemoveButtonTable = new Hashtable<RepeatableGroup, JButton>();

    private static final String STRUCT_GUID_SEPERATOR 	= "_-_";
    private static final String CSV_OUTPUT_DELIM 		= ",";
    private static final String BROWSE_NONIMG_DELIM 	= ";;;;";
    private static final String MULTI_SELECT_VALUE_DELIM = ";";
    
    private static final int MULTI_SELECT_VISIBLE_ROWS = 5;

    private final ArrayList<ArrayList<File>> allOtherFilesAL = new ArrayList<ArrayList<File>>();
    private final ArrayList<FormStructureData> fsDataList = new ArrayList<FormStructureData>();

    private ArrayList<DataElementValue> errors;

    private int fixErrors = FIX_ERRORS_LATER;
    private static final int FIX_ERRORS_NOW = 0;
    private static final int FIX_ERRORS_LATER = 1;
    private static final int FIX_ERRORS_CANCEL = -1;

    /** Dev data dictionary server. */
    @SuppressWarnings("unused")
    private static final String ddDevServer = "http://fitbir-dd-dev.cit.nih.gov/";

    /** Dev portal auth server. */
    @SuppressWarnings("unused")
    private static final String authDevServer = "http://fitbir-portal-dev.cit.nih.gov/";

    /** Stage data dictionary server. */
    @SuppressWarnings("unused")
    private static final String ddStageServer = "http://fitbir-dd-stage.cit.nih.gov/";

    /** Stage portal auth server. */
    @SuppressWarnings("unused")
    private static final String authStageServer = "http://fitbir-portal-stage.cit.nih.gov/";

    /** Demo data dictionary server. */
    @SuppressWarnings("unused")
    private static final String ddDemoServer = "http://fitbir-dd-demo.cit.nih.gov/";

    /** Demo portal auth server. */
    @SuppressWarnings("unused")
    private static final String authDemoServer = "http://fitbir-portal-demo.cit.nih.gov/";

    /** Prod data dictionary server. */
    private static final String ddProdServer = "https://dictionary.fitbir.nih.gov/";

    /** Prod portal auth server. */
    private static final String authProdServer = "https://fitbir.nih.gov/";

    /** File name of server configuration. */
    private static final String configFileName = "brics_config.properties";

    /** Property for reading the dd environment name from the BRICS config file. */
    private static final String ddEnvNameProp = "ddEnvName";

    /** Property for reading the dd server url from the BRICS config file. */
    private static final String ddServerURLProp = "ddServerURL";

    /** Property for reading the auth server url from the BRICS config file. */
    private static final String authServerURLProp = "authServerURL";

    /** Property for reading the dd authentication user name from the BRICS config file. */
    private static final String ddAuthUserProp = "ddAuthUser";

    /** Property for reading the dd authentication password from the BRICS config file. */
    private static final String ddAuthPassProp = "ddAuthPass";

    /**
     * Property for reading whether to use the (slower) authenticated web service, which allows testing against draft
     * forms.
     */
    private static final String ddUseAuthServiceProp = "ddUseAuthService";

    /** DD server environment name (Prod, Demo, Stage, or Dev). */
    private static String ddEnvName = "Prod";

    /** Full data dictionary server url */
    private static String ddServerURL = ddProdServer;

    /** Full authentication server url */
    private static String authServerURL = authProdServer;
    private static String ddAuthUser = "";
    private static String ddAuthPass = "";
    private static boolean ddUseAuthService = false;   

    private final ArrayList<String> tempDirs = new ArrayList<String>();

    private boolean isFinished = false;

    /**
     * Indicates how to resolve conflicts between csv and image header values. 0 = no choice made/ask always, 1 = csv, 2
     * = image
     */
    private int resolveConflictsUsing = RESOLVE_CONFLICT_ASK;
    private static final int RESOLVE_CONFLICT_ASK = 0;
    private static final int RESOLVE_CONFLICT_CSV = 1;
    private static final int RESOLVE_CONFLICT_IMG = 2;

    private static final String svnVersion 		= "$Rev: 15178 $";
    private static final String svnLastUpdate 	= "$Date: 2017-10-10 14:17:11 -0400 (Tue, 10 Oct 2017) $";
    //private static final String pluginVersion = MipavUtil.getSVNChangedDate(svnLastUpdate);
    private static final String pluginVersion 	= "Beta version 0.1";

    private static final String VALUE_OTHER_SPECIFY 		= "Other, specify";
    private static final String VALUE_YES_SPECIFY 			= "Yes, specify";
    private static final String ELEM_OTHER_SPECIFY_SUFFIX 	= "OTH";
    private static final String ELEM_YES_SPECIFY_SUFFIX 	= "ST";
    private static final String GUID_ELEMENT_NAME 			= "GUID";
    static final String recordIndicatorColumn 				= "record";
    private static final String recordIndicatorValue 		= "x";

    private javax.swing.SwingWorker<Object, Object> fileWriterWorkerThread;

    /**
     * Text of the privacy notice displayed to the user before the plugin can be used.
     */
    public static final String PRIVACY_NOTICE = "BRICS is a collaborative environment with privacy rules that pertain to the collection\n"
            + "and display of imaging data. Before accessing and using BRICS, please ensure that you\n"
            + "familiarize yourself with our privacy rules, available through the BRICS Rules of Behavior\n" + "document and supporting documentation.\n"
            + "\n" + "Collection of this information is authorized under 42 U.S.C. 241, 242, 248, 281(a)(b)(1)(P)\n"
            + "and 44 U.S.C. 3101. The primary use of this information is to facilitate medical research.\n"
            + "This information may be disclosed to researchers for research purposes, and to system \n"
            + "administrators for evaluation and data normalization.\n" + "\n"
            + "Rules governing submission of this information are based on the data sharing rules defined in\n"
            + "the Notice of Grant Award (NOGA). If you do not have a grant defining data sharing requirements,\n"
            + "data submission is voluntary. Data entered into BRICS will be used solely for scientific and\n"
            + "research purposes. Significant system update information may be posted on\n" + "the BRICS site as required.";

    private static final Comparator dataElementCompare = new Comparator<DataElementValue>() {
        @Override
        public int compare(final DataElementValue o1, final DataElementValue o2) {
            return new Integer(o1.getPosition()).compareTo(new Integer(o2.getPosition()));
        }
    };

    private static final Comparator mapElementCompare = new Comparator<MapElement>() {
        @Override
        public int compare(final MapElement o1, final MapElement o2) {
            return new Integer(o1.getPosition()).compareTo(new Integer(o2.getPosition()));
        }
    };

    private static final Comparator groupCompare = new Comparator<RepeatableGroup>() {
        @Override
        public int compare(final RepeatableGroup o1, final RepeatableGroup o2) {
            return new Integer(o1.getPosition()).compareTo(new Integer(o2.getPosition()));
        }
    };

    private static final Comparator valueRangeCompare = new Comparator<ValueRange>() {
        @Override
        public int compare(final ValueRange o1, final ValueRange o2) {
            return o1.compareTo(o2);
        }
    };
    
    
    /** Mapper constructor  */
    public PlugInDialogBRICS_Mapper () {
        super();

        	// MATT Need new dir for mapper
        outputDirBase = Preferences.getProperty(Preferences.PREF_BRICS_PLUGIN_OUTPUT_DIR);
        if (outputDirBase == null) {
            outputDirBase = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "BRICS_Mapping" + File.separator;
            Preferences.setProperty(Preferences.PREF_BRICS_PLUGIN_OUTPUT_DIR, outputDirBase);
        }
        
        // Matt Need new dir for source CSV
        csvFileDir = Preferences.getProperty(Preferences.PREF_BRICS_PLUGIN_CSV_DIR);
        if (csvFileDir == null) {
            csvFileDir = ViewUserInterface.getReference().getDefaultDirectory();
        }

        // try to read the server config from disk, if it is there.
        // otherwise the value set above at initialization is used.
        readConfig();

        init();
        
        setVisible(true);
        validate();

        final int response = JOptionPane.showConfirmDialog(this, PlugInDialogBRICS_Mapper .PRIVACY_NOTICE, "Data Mapping Tool",
                JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

        if (response == JOptionPane.NO_OPTION) {
            if (ViewUserInterface.getReference() != null && ViewUserInterface.getReference().isPlugInFrameVisible()) {
                System.gc();
                System.exit(0);
            } else {
                return;
            }
        }
        
        // Gets full list of form structures from the BRICS data dictionary
        final Thread thread = new FormListRESTThread(this);
        thread.start();
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        final String command = e.getActionCommand();

        if (command.equalsIgnoreCase("SelectStruct")) {

            new ChooseFormStructDialog(this);

            //saveMapButton.setEnabled(deTableModel.getRowCount() > 0);
            //editDataElementsButton.setEnabled(deTableModel.getRowCount() > 0);

        } else if (command.equalsIgnoreCase("LoadCSV")) {
            final JFileChooser chooser = new JFileChooser();
            chooser.setCurrentDirectory(new File(csvFileDir));
            chooser.setDialogTitle("Choose CSV file");
            chooser.setFileFilter(new FileNameExtensionFilter("Comma separated value files (.csv)", "csv"));
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                csvFile = chooser.getSelectedFile();
                readCSVFile();

                csvFileDir = chooser.getSelectedFile().getAbsolutePath() + File.separator;
                Preferences.setProperty(Preferences.PREF_BRICS_PLUGIN_CSV_DIR, csvFileDir);
                saveMapButton.setEnabled(deTableModel.getRowCount() > 0);
            }
            listPane.setBorder(JDialogBase.buildTitledBorder(deTableModel.getRowCount() + " Form Structure(s) "));
        } else if (command.equalsIgnoreCase("SaveMapFile")) {
        	
            
        } else if (command.equalsIgnoreCase("HelpWeb")) {
            showCDE(null);

        } else if (command.equalsIgnoreCase("Finish")) {

            if (isFinished && fileWriterWorkerThread != null && fileWriterWorkerThread.isDone()) {
                dispose();
                if (JDialogStandalonePlugin.isExitRequired()) {
                    System.gc();
                    System.exit(0);
                }
            } else {

                fileWriterWorkerThread = new javax.swing.SwingWorker<Object, Object>() {
                    @Override
                    public Object doInBackground() {
                        try {
                            //createSubmissionFiles();
                        } catch (final Throwable e) {
                            e.printStackTrace();
                        }

                        return null;
                    }

                    @Override
                    public void done() {
                        if (isFinished) {
                            finishButton.setText("Close");
                            finishButton.setEnabled(true);
                        }
                    }
                };
            }
        } else if (command.equalsIgnoreCase("EditDataElements")) {

            final String dsName = (String) deTableModel.getValueAt(deTable.getSelectedRow(), 0);
            new populateBRICSStructureTable(this, dsName);

        } else if (command.equalsIgnoreCase("OutputDirBrowse")) {
            final JFileChooser chooser = new JFileChooser(outputDirBase);

            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            chooser.setDialogTitle("Choose output directory for mapping result files");
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                outputDirTextField.setText(chooser.getSelectedFile().getAbsolutePath() + File.separator);

                outputDirBase = chooser.getSelectedFile().getAbsolutePath() + File.separator;
                Preferences.setProperty(Preferences.PREF_BRICS_PLUGIN_OUTPUT_DIR, outputDirBase);
            }
        } 
    }

    @Override
    public void windowActivated(final WindowEvent e) {}

    @Override
    public void windowClosed(final WindowEvent e) {}

    @Override
    public void windowClosing(final WindowEvent e) {
        if (tempDirs.size() > 0) {
            for (int i = 0; i < tempDirs.size(); i++) {
                final String dir = tempDirs.get(i);
                final File f = new File(dir);
                if (f.exists()) {
                    try {
                        FileUtils.deleteDirectory(f);
                    } catch (final IOException ioe) {
                        ioe.printStackTrace();
                    }
                }
            }
        }

        if (JDialogStandalonePlugin.isExitRequired()) {
            ViewUserInterface.getReference().windowClosing(e);
        }
    }

    @Override
    public void windowDeactivated(final WindowEvent e) {}

    @Override
    public void windowDeiconified(final WindowEvent e) {}

    @Override
    public void windowIconified(final WindowEvent e) {}

    @Override
    public void windowOpened(final WindowEvent e) {}

    

    /**
     * called after validation is done
     */
    public void complete(final FormStructureData fsData, final boolean isComplete) {
        String value = "";
        String guid = "";
        final ArrayList<File> allOtherFiles = new ArrayList<File>();
        final boolean launchedFromInProcessState = false;

        for (final RepeatableGroup group : fsData.getStructInfo().getRepeatableGroups()) {
            for (final GroupRepeat repeat : fsData.getAllGroupRepeats(group.getName())) {
                for (final DataElementValue deVal : repeat.getDataElements()) {
                    final JLabel label = deVal.getLabel();
                    final JComponent comp = deVal.getComp();

                    if (label.getName().equalsIgnoreCase(GUID_ELEMENT_NAME)) {
                        guid = ((JTextField) comp).getText().trim();
                    }

                    if (comp instanceof JTextField) {
                        value = ((JTextField) comp).getText().trim();
                        // ok...all files will go into the allOtherFiles AL

                        final File f = new File(value);
                        if (f.isFile()) {
                            allOtherFiles.add(f);
                        }

                    } else if (comp instanceof JComboBox) {
                        value = (String) ( ((JComboBox) comp).getSelectedItem());
                        if (value.equalsIgnoreCase(VALUE_OTHER_SPECIFY) || value.equalsIgnoreCase(VALUE_YES_SPECIFY)) {
                            // value = deVal.getOtherSpecifyField().getText().trim();
                        }
                    } else if (comp instanceof JList) {
                        value = "";
                        final int[] selectedIndicies = ((JList) comp).getSelectedIndices();
                        for (final int index : selectedIndicies) {
                            if (value == "") {
                                value = (String) ((JList) comp).getModel().getElementAt(index);
                            } else {
                                value += MULTI_SELECT_VALUE_DELIM + (String) ((JList) comp).getModel().getElementAt(index);
                            }
                        }
                    }

                    /*
                     * if(!value.equals("")) { System.out.println("the key is " + key);
                     * System.out.println("the value is " + value); }
                     */

                    deVal.setValue(value);
                }
            }
        }

        // boolean guidKnown = true;
        // if (guid != null && !guid.trim().equalsIgnoreCase("")) {
        // guidKnown = false;
        // }

        String name = "";

        if (guid != null && !guid.trim().equalsIgnoreCase("")) {
            name = fsData.getStructInfo().getShortName() + STRUCT_GUID_SEPERATOR + guid;
        } else {
            name = fsData.getStructInfo().getShortName() + STRUCT_GUID_SEPERATOR + "UNKNOWNGUID";
        }

        if (launchedFromInProcessState) {
            final int selectedRow = deTable.getSelectedRow();

            deTableModel.setValueAt(name, selectedRow, 0);
            if (isComplete) {
            	deTableModel.setValueAt("Yes", selectedRow, 1);
            } else {
            	deTableModel.setValueAt("No", selectedRow, 1);
            }

            fsDataList.set(selectedRow, fsData);

            allOtherFilesAL.set(selectedRow, allOtherFiles);

        } else {

            fsDataList.set(fsDataList.size() - 1, fsData);
            final Vector<String> rowData = new Vector<String>();
            rowData.add(name);
            if (isComplete) {
                rowData.add("Yes");
            } else {
                rowData.add("No");
            }
            deTableModel.addRow(rowData);
            deTable.setRowSelectionInterval(deTableModel.getRowCount() - 1, deTableModel.getRowCount() - 1);

            allOtherFilesAL.set(allOtherFilesAL.size() - 1, allOtherFiles);
        }
    }


    @Override
    public void focusGained(final FocusEvent e) {}

    @Override
    public void focusLost(final FocusEvent e) {
        //validateFields();
    }

  

    private boolean readCSVFile() {
        BufferedReader br = null;

        try {
            String str;
            final FileInputStream fis = new FileInputStream(csvFile);
            br = new BufferedReader(new InputStreamReader(fis));

            // first line is data structure name and version
            str = br.readLine();
            String[] arr = str.split(CSV_OUTPUT_DELIM);
            final String dsName = arr[0].trim();
            // final String version = arr[1].trim();

            // second line are the field names
            str = br.readLine().trim();
            final String[] csvFieldNamesWithRecord = str.split(CSV_OUTPUT_DELIM);

            // List of records, each record consisting of 1+ lines, split into field values
            final ArrayList<ArrayList<ArrayList<String>>> recordList = new ArrayList<ArrayList<ArrayList<String>>>();

            csvFieldNames = new ArrayList<String>(csvFieldNamesWithRecord.length - 1);
            int recordFieldIndex = -1;
            for (int i = 0; i < csvFieldNamesWithRecord.length; i++) {
                if (csvFieldNamesWithRecord[i].equalsIgnoreCase(recordIndicatorColumn)
                        || csvFieldNamesWithRecord[i].equalsIgnoreCase("\"" + recordIndicatorColumn + "\"")) {
                    recordFieldIndex = i;
                } else {
                    // don't add fields without a name (error in the middle of the CSV, ignore at the end)
                    if ( !csvFieldNamesWithRecord[i].trim().equals("")) {
                        // if the names are surrounded by quotes, remove them before adding.
                        csvFieldNames.add(csvFieldNamesWithRecord[i].trim().replaceAll("^\"|\"$", ""));
                    } else {
                        // TODO: ignore if no more real field names (and no data values for the column). otherwise show
                        // error
                        boolean allEmpty = true;
                        for (int j = i; j < csvFieldNamesWithRecord.length; j++) {
                            if ( !csvFieldNamesWithRecord[j].trim().equals("")) {
                                allEmpty = false;
                            }
                        }

                        // show error if the blank field is not at the end
                        if ( !allEmpty) {
                            MipavUtil.displayError("Empty CSV header field found in the middle of the row.  Check your CSV file.");
                            return false;
                        }
                    }
                }
            }

            final String otherThanQuote = " [^\"] ";
            final String quotedString = String.format(" \" %s* \" ", otherThanQuote);
            final String csvRegex = String.format("(?x) " + // enable comments, ignore white spaces
                    CSV_OUTPUT_DELIM + "                         " + // match a comma
                    "(?=                       " + // start positive look ahead
                    "  (                       " + // start group 1
                    "    %s*                   " + // match 'otherThanQuote' zero or more times
                    "    %s                    " + // match 'quotedString'
                    "  )*                      " + // end group 1 and repeat it zero or more times
                    "  %s*                     " + // match 'otherThanQuote'
                    "  $                       " + // match the end of the string
                    ")                         ", // stop positive look ahead
                    otherThanQuote, quotedString, otherThanQuote);

            ArrayList<String> csvParams;
            while ( (str = br.readLine()) != null) {
                str = str.trim();
                arr = str.split(csvRegex, -1);

                csvParams = new ArrayList<String>(csvFieldNamesWithRecord.length);
                for (int i = 0; i < arr.length; i++) {
                    // if the value was surrounded by quotes because of a comma inside, remove the quotes
                    if (arr[i].matches("^\".*\"$")) {
                        csvParams.add(arr[i].substring(1, arr[i].length() - 1));
                    } else {
                        csvParams.add(arr[i]);
                    }
                }

                // if not enough values, fill out with blanks until we hit the number of fields
                for (int i = arr.length; i < csvFieldNamesWithRecord.length; i++) {
                    csvParams.add("");
                }

                if (recordFieldIndex != -1 && csvParams.get(recordFieldIndex).equalsIgnoreCase(recordIndicatorValue)) {
                    // new record
                    final ArrayList<ArrayList<String>> record = new ArrayList<ArrayList<String>>();
                    csvParams.remove(recordFieldIndex);
                    record.add(csvParams);
                    recordList.add(record);
                } else if (recordFieldIndex == -1) {
                    // no record field, assume always new record
                    final ArrayList<ArrayList<String>> record = new ArrayList<ArrayList<String>>();
                    record.add(csvParams);
                    recordList.add(record);
                } else {
                    // no record indicator value found but the column is there, so this is a group repeat of the last
                    // record
                    csvParams.remove(recordFieldIndex);
                    recordList.get(recordList.size() - 1).add(csvParams);
                }
            }

            final ViewJProgressBar progressBar = new ViewJProgressBar("Reading CSV file", "Reading CSV file...", 0, 100, false);
            progressBar.setVisible(true);
            progressBar.updateValue(5);
            final long csvReadStartTime = System.currentTimeMillis();
            final int progressInc = 95 / recordList.size();
            final int rowsPerInc = (recordList.size() / 95) + 1;
            int i = 1;
            Vector<Vector<FileDicomTag>> csvProblemTagList = new Vector<Vector<FileDicomTag>>(recordList.size());
            Vector<String> csvProblemFileDirList = new Vector<String>(recordList.size());
            Vector<String> csvProblemFileNameList = new Vector<String>(recordList.size());
            for (final ArrayList<ArrayList<String>> record : recordList) {
                progressBar.setMessage("Reading CSV row " + i + " of " + recordList.size());
                //InfoDialog csvDialog = new InfoDialog(this, dsName, false, false, record);
                if (progressInc > 0) {
                    progressBar.updateValue(progressBar.getValue() + progressInc);
                } else if ( (i % rowsPerInc) == 0) {
                    progressBar.updateValue(progressBar.getValue() + 1);
                }

                // change i counter to 0-based for problem lists
                //csvProblemTagList.add(i - 1, csvDialog.getProblemTags());
                //csvProblemFileDirList.add(i - 1, csvDialog.getProblemFileDir());
                //csvProblemFileNameList.add(i - 1, csvDialog.getProblemFileName());

                i++;
            }
            final long csvReadEndTime = System.currentTimeMillis();
            System.out.println("CSV input read took " + ( (csvReadEndTime - csvReadStartTime) / 1000) + " seconds (" + recordList.size() + " records)");

            progressBar.dispose();

            fis.close();
        } catch (final Exception e) {
            e.printStackTrace();
            return false;
        } finally {
            if (br != null) {
                try {
                    br.close();
                } catch (IOException e) {
                    System.err.println("Problem closing CSV file handle.");
                    e.printStackTrace();
                }
            }
        }

        return true;

    }

    /**
     * Sets values based on knob along slider.
     * 
     * @param e Event that triggered this function
     */
    @Override
    public void stateChanged(final ChangeEvent e) {
        final Object source = e.getSource();
      
        /**if (source == xyz) {
            
        } else if (source == abc) {
            
        }*/
    }

    
    /**  Setup Mapping tool layout */
    private void init() {
        setTitle("Data Mapping Tool - " + pluginVersion + " (" + ddEnvName + ")");
        //dsMainPanel = new JPanel(new GridBagLayout());
        //final JScrollPane tabScrollPane = new JScrollPane(dsMainPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
        //        ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        
        try {
            setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (final Exception e) {
            // setIconImage() is not part of the Java 1.5 API - catch any
            // runtime error on those systems
        }

        if (JDialogStandalonePlugin.isExitRequired()) {
            setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        } else {
            setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        }

        addWindowListener(this);

        final JMenuBar menuBar = new JMenuBar();
        final JMenu menu = new JMenu("Help");
        menuBar.add(menu);

        final JMenuItem helpMenuItem = new JMenuItem("Help web page");
        helpMenuItem.setActionCommand("HelpWeb");
        helpMenuItem.addActionListener(this);
        menu.add(helpMenuItem);

        final JMenuItem jvmMenuItem = new JMenuItem("JVM information");
        jvmMenuItem.setActionCommand("AboutJava");
        jvmMenuItem.addActionListener(ViewUserInterface.getReference());
        menu.add(jvmMenuItem);

        this.setJMenuBar(menuBar);
        
        final JPanel topPanel = new JPanel(new GridBagLayout());
        final GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.NORTHWEST;

        gbc2.gridy = 0;
        gbc2.gridx = 1;
        gbc2.weightx = 1;
        gbc2.weighty = 1;
        gbc2.fill = GridBagConstraints.BOTH;
        topPanel.add(buildStructurePanel(), gbc2);
    
        getContentPane().add(buildButtonPanel(), BorderLayout.NORTH);
        getContentPane().add(topPanel, BorderLayout.CENTER);
        getContentPane().add(buildLogPanel(), BorderLayout.SOUTH);

        pack();
        validate();
        this.setMinimumSize(this.getSize());
        this.setResizable(true);
        MipavUtil.centerOnScreen(this);
    }

    
    private JPanel buildButtonPanel() {

        final JPanel buttonPanel = new JPanel(new GridBagLayout());

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;

        selectStructButton = new JButton("Select Form Structure");
        selectStructButton.setToolTipText("Select BRICS Form Structure ");
        selectStructButton.addActionListener(this);
        selectStructButton.setActionCommand("SelectStruct");
        
        loadCSVButton = new JButton("Load Source DEs");
        loadCSVButton.setToolTipText("Load Source Data Elements CSV File");
        loadCSVButton.addActionListener(this);
        loadCSVButton.setActionCommand("LoadCSV");

        finishButton = new JButton("Generate Files");
        finishButton.setToolTipText("Generate Files");
        finishButton.addActionListener(this);
        finishButton.setActionCommand("Finish");

        editDataElementsButton = new JButton("Edit Data Elements");
        editDataElementsButton.setToolTipText("Edit data elements for selected Form Structure");
        editDataElementsButton.addActionListener(this);
        editDataElementsButton.setActionCommand("EditDataElements");

        //selectStructButton.setPreferredSize(MipavUtil.defaultButtonSize);
        finishButton.setPreferredSize(MipavUtil.defaultButtonSize);
        editDataElementsButton.setPreferredSize(MipavUtil.defaultButtonSize);

        selectStructButton.setEnabled(false);
        loadCSVButton.setEnabled(false);
        editDataElementsButton.setEnabled(false);
        finishButton.setEnabled(false);

        gbc.gridx = 0;
        buttonPanel.add(selectStructButton, gbc);
        gbc.gridx++;
        buttonPanel.add(loadCSVButton, gbc);
        gbc.gridx++;
        buttonPanel.add(editDataElementsButton, gbc);
        gbc.gridx++;
        buttonPanel.add(finishButton, gbc);

        return buttonPanel;
    }
    
    
    /**
     * 
     * @return JScrollPane of the table...
     */
    private JScrollPane buildStructurePanel() {
    	deTableModel = new ViewTableModel();
    	deTableModel.addColumn("Group");
    	deTableModel.addColumn("Element Name");
    	deTableModel.addColumn("Title");
    	deTableModel.addColumn("Reference PVs");
    	deTableModel.addColumn("Source Name");
    	deTableModel.addColumn("Source PVs");
    	deTableModel.addColumn("PV Mappings");
    
        
        deTable = new JTable(deTableModel) {
            private static final long serialVersionUID = 3053232611901005303L;

            @Override
            public String getToolTipText(final MouseEvent e) {
                String tooltip = "";

                final java.awt.Point p = e.getPoint();
                final int rowIndex = rowAtPoint(p);
                final int colIndex = columnAtPoint(p);

                if(colIndex < 4) {
	                //tip = (String) deTableModel.getValueAt(rowIndex, colIndex);
	                DataElement de = formStructure.getSortedDataElementList().get(rowIndex);
	                
	                tooltip = "<html><p><b> Name: </b> " + de.getName() + "<br/>";
	                tooltip += "<b> CDE Description: </b>" + WordUtils.wrap(de.getDescription(), 100, "<br/>", false) + "<br/>";
	                tooltip += "<b> Type: </b> " + de.getType() + "<br/>";
	                tooltip += "<b> PV - PV Description: </b> " + WordUtils.wrap(de.getValueRangeList().toString().trim(), 100, "<br/>", false) + "<br/>";
	
	                if (de.getMeasuringUnit() != null) {
	                    tooltip += "<p><b> Unit of measure: </b> " + de.getMeasuringUnit().getDisplayName() + "</p>";
	                }
	
	                //if (de.getNinds() != null && !de.getNinds().getValue().equals("")) {
	                //    tooltip += "<p><b> NINDS CDE ID: </b> " + de.getNinds().getValue() + "<br/>";
	                //}
	                //if (de.getGuidelines() != null && !de.getGuidelines().trim().equals("")) {
	                //    tooltip += "<p><b> Guidelines & Instructions: </b></br>"
	                 //           + WordUtils.wrap(removeRedundantDiseaseInfo(de.getGuidelines()), 100, "<br/>", false) + "</p>";
	                //}
	                //if (de.getNotes() != null && !de.getNotes().trim().equals("")) {
	                //    tooltip += "<p><b> Notes: </b><br/>" + WordUtils.wrap(removeRedundantDiseaseInfo(de.getNotes()), 100, "<br/>", false) + "</p>";
	                //}
	                ToolTipManager.sharedInstance().setDismissDelay(12000);

	                return tooltip;
                }
                else return null;
            }
        };
        
        deTable.addMouseListener(this);
        deTable.setPreferredScrollableViewportSize(new Dimension(850, 300));
        deTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

        deTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        deTable.getColumn("Group").setMinWidth(175);
        deTable.getColumn("Group").setMaxWidth(200);
        //deTable.getColumn("Group").setCellRenderer(new CellRenderer());
        deTable.getColumn("Element Name").setMinWidth(185);
        deTable.getColumn("Element Name").setMaxWidth(200);
        //deTable.getColumn("Element Name").setCellRenderer(new CellRenderer());
        
        listPane = WidgetFactory.buildScrollPane(deTable);
        listPane.setBorder(JDialogBase.buildTitledBorder(" Reference Form Structure:   "));
   
        return listPane;
    }
    
    
    private class CellRenderer extends DefaultTableCellRenderer {

    	@Override
        public Component getTableCellRendererComponent(final JTable table, final Object value,
                final boolean isSelected, final boolean hasFocus, final int row, final int column) {
    		
    		final Color gray = new Color (225,225,225);
            final Component cell = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
            //cell.setBackground(Color.GREEN);
            TableCellRenderer refRenderer = table.getCellRenderer(row, column);
            if (refRenderer instanceof CellRenderer) { 
	            if (row % 2 ==  1 ) {
	                //cell.setBackground(gray);
	                cell.setBackground(Color.WHITE);
	            }   
	            else {
	            	cell.setBackground(Color.WHITE);
	            }
            }
            return cell;
    	}
    }
    
    
    /**
     * Build a panel log.
     */
    private JPanel buildLogPanel() {
        final JPanel destPanel = new JPanel(new GridBagLayout());

        final GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.CENTER;

        gbc2.gridy = 0;
        gbc2.gridx = 0;
        
        final JPanel outputDirPanel = new JPanel();
        final JLabel outputDirLabel = new JLabel("Output Directory for mapping result files: ");
        outputDirTextField = new JTextField(20);
        outputDirTextField.setEditable(false);
        outputDirTextField.setToolTipText(outputDirBase);
        outputDirTextField.setText(outputDirBase);
        outputDirButton = WidgetFactory.buildTextButton("Browse", "Choose Output Directory for mapping result files", "OutputDirBrowse", this);
        outputDirButton.setPreferredSize(MipavUtil.defaultButtonSize);
        
        saveMapButton = new JButton("Save");
        saveMapButton.setToolTipText("Save Mapping File (Source DEs to BRICS DEs");
        saveMapButton.addActionListener(this);
        saveMapButton.setActionCommand("SaveMapFile");
        saveMapButton.setPreferredSize(MipavUtil.defaultButtonSize);
        saveMapButton.setEnabled(false);

        outputDirPanel.add(outputDirLabel);
        outputDirPanel.add(outputDirTextField);
        outputDirPanel.add(outputDirButton);
        outputDirPanel.add(saveMapButton);
        destPanel.add(outputDirPanel, gbc2);

        logOutputArea = WidgetFactory.buildScrollTextArea(Color.white);
        logOutputArea.setBorder(JDialogBase.buildTitledBorder("Output log"));
        logOutputArea.getTextArea().setEditable(false);
        logOutputArea.getTextArea().setRows(5);
        
        gbc2.gridy = 1;
        gbc2.fill = GridBagConstraints.BOTH;
        gbc2.weightx = 1;
        gbc2.weighty = 1;
        gbc2.anchor = GridBagConstraints.NORTHWEST;
        destPanel.add(logOutputArea, gbc2);

        return destPanel;
    }

  
   
   
    /**
     * writes out csv file
     * 
     * @param outputDirBase
     * @param outputFileNameBase
     * @param imageFile
     */
    private final String getCSVDataRow(final String outputDirBase, final String outputFileNameBase, final FormStructureData fsData, final int repeatNum) {
        String csvRow = new String();

        if (fsData == null) {
            return csvRow;
        }

        final ArrayList<RepeatableGroup> orderedGroupList = new ArrayList(fsData.getStructInfo().getRepeatableGroups());
        Collections.sort(orderedGroupList, groupCompare);

        for (final RepeatableGroup group : orderedGroupList) {
            if (fsData.isGroupRepeatSet(group.getName(), repeatNum)) {
                final GroupRepeat repeat = fsData.getGroupRepeat(group.getName(), repeatNum);
                final Vector<DataElementValue> deList = repeat.getDataElements();
                Collections.sort(deList, dataElementCompare);

                for (final DataElementValue deVal : deList) {
                    final String deName = deVal.getName();
                    String value = deVal.getValue();

                    // final File f = new File(value);
                    // if (f.isFile() || value.endsWith("_collision")) {
                    // if (value.endsWith("_collision")) {
                    // value = value.substring(0, value.indexOf("_collision"));
                    // final String filename = value.substring(value.lastIndexOf(File.separator) + 1, value.length());
                    // value = outputFileNameBase + File.separator + filename;
                    // } else {
                    // final String filename = f.getName();
                    // value = outputFileNameBase + File.separator + filename;
                    // }
                    // }

                    // escape commas in values - if there's a comma, put quotes
                    // around the value and double up any existing quotes
                    if (value.contains(CSV_OUTPUT_DELIM)) {
                        value = "\"" + value.replaceAll("\"", "\"\"") + "\"";
                    }

                    if (csvRow.length() == 0) {
                        csvRow = value;
                    } else {
                        csvRow += CSV_OUTPUT_DELIM + value;
                    }
                }
            } else {
                for (int i = 0; i < group.getSize(); i++) {
                    if (group.getPosition() != 0 || i != 0) {
                        csvRow += CSV_OUTPUT_DELIM;
                    }
                }
            }
        }

        return csvRow;
    }


    /**
     * Append a line to the log output area in the Log tab.
     * 
     * @param line The line to append (do not include the trailing newline).
     */
    private void printlnToLog(final String line) {
        logOutputArea.getTextArea().append(line + "\n");
    }


    @Override
    public Dimension getPanelSize() {
        return null;
    }
    
    @Override
    public void valueChanged(final TreeSelectionEvent e) {

    }

    @Override
    public void mouseClicked(final MouseEvent e) {
        final Component c = e.getComponent();
        if (c instanceof JTable) {
            if (deTable.getSelectedRow() == -1) {
                editDataElementsButton.setEnabled(false);
                saveMapButton.setEnabled(false);
                return;
            } else {
                if ( !isFinished) {
                    editDataElementsButton.setEnabled(true);
                    saveMapButton.setEnabled(true);
                }
            }

            

            if (e.getClickCount() == 2) {
                if ( !isFinished) {
                    final String dsName = (String) deTableModel.getValueAt(deTable.getSelectedRow(), 0);
                    //new InfoDialog(this, dsName, true, true, null);
                }
            }
        }
    }

    @Override
    public void mouseEntered(final MouseEvent e) {}

    @Override
    public void mouseExited(final MouseEvent e) {}

    @Override
    public void mousePressed(final MouseEvent e) {}

    @Override
    public void mouseReleased(final MouseEvent e) {}

    public boolean contains(final File f) {
        boolean contains = false;

        for (int i = 0; i < deTableModel.getRowCount(); i++) {
            final File f1 = (File) deTableModel.getValueAt(i, 0);
            if (f1.getAbsolutePath().equalsIgnoreCase(f.getAbsolutePath())) {
                contains = true;
                break;
            }
        }
        return contains;
    }

    public void enableDisableFinishButton() {
        // boolean allCompleted = true;

        // for (int i = 0; i < sourceTableModel.getRowCount(); i++) {
        // final String completed = (String) sourceTableModel.getValueAt(i, 1);
        // if (completed.equalsIgnoreCase("No")) {
        // allCompleted = false;
        // break;
        // }
        // }

        if (deTableModel.getRowCount() == 0) {
            finishButton.setEnabled(false);
        } else {
            // changed to always enabled if some data added
            finishButton.setEnabled(true);
            // if (allCompleted) {
            // finishButton.setEnabled(true);
            // } else {
            // finishButton.setEnabled(false);
            // }
        }

    }

   

    /**
     * Tries to read server configuration from BRICS config file on local disk.
     */
    private void readConfig() {
        final InputStream in = getClass().getResourceAsStream(configFileName);
        if (in != null) {
            final Properties prop = new Properties();
            try {
                prop.load(in);
            } catch (final IOException e) {
                Preferences.debug("Unable to load BRICS preferences file: " + configFileName + "\n", Preferences.DEBUG_MINOR);
                e.printStackTrace();
            }
            // use pre-set, hardcoded values as defaults if properties are not found
            ddEnvName = prop.getProperty(ddEnvNameProp, ddEnvName);
            System.out.println("ddEnvName:\t" + ddEnvName);
            authServerURL = prop.getProperty(authServerURLProp, authServerURL);
            System.out.println("authServer:\t" + authServerURL);
            ddServerURL = prop.getProperty(ddServerURLProp, ddServerURL);
            System.out.println("ddServer:\t" + ddServerURL);
            ddAuthUser = prop.getProperty(ddAuthUserProp, ddAuthUser);
            System.out.println("ddAuthUser:\t" + ddAuthUser);
            ddAuthPass = prop.getProperty(ddAuthPassProp, ddAuthPass);
            System.out.println("ddAuthPass:\t" + ddAuthPass);
            ddUseAuthService = Boolean.parseBoolean(prop.getProperty(ddUseAuthServiceProp, "" + ddUseAuthService));
            System.out.println("ddUseAuthService:\t" + ddUseAuthService);
        }
    }


    private static final String removeRedundantDiseaseInfo(final String field) {
        final ArrayList<String> diseaseStrings = new ArrayList<String>();
        final ArrayList<String> fieldStrings = new ArrayList<String>();

        final Matcher m = Pattern.compile("(\\w[\\w\\s\\/()]*):?\\s+(.+)\\s+-----?\\s+").matcher(field);

        // get the first info
        if (m.find()) {
            diseaseStrings.add(m.group(1));
            fieldStrings.add(m.group(2));

            int lastMatchIndex = m.end();
            while (m.find()) {
                boolean foundFieldMatch = false;
                for (int i = 0; i < diseaseStrings.size() && !foundFieldMatch; i++) {
                    if (fieldStrings.get(i).equals(m.group(2))) {
                        // only add if the disease isn't a duplicate
                        boolean foundDisease = false;
                        final String[] split = diseaseStrings.get(i).split("/");
                        for (final String s : split) {
                            if (s.equals(m.group(1))) {
                                foundDisease = true;
                            }
                        }
                        if ( !foundDisease) {
                            diseaseStrings.set(i, diseaseStrings.get(i) + "/" + m.group(1));
                        }
                        foundFieldMatch = true;
                    }
                }

                // didn't find a matching field value, add a new one
                if ( !foundFieldMatch) {
                    diseaseStrings.add(m.group(1));
                    fieldStrings.add(m.group(2));
                }

                lastMatchIndex = m.end();
            }

            // many fields don't have ----- at the end of their last value, or are cut off by the character limit, so do
            // one last field value
            // this might still have problems if the text cuts off in the disease name
            if (lastMatchIndex != field.length()) {
                boolean foundFieldMatch = false;

                final Matcher m2 = Pattern.compile("(\\w[\\w\\s\\/()]*):?\\s+(.+)\\s*-*").matcher(field.substring(lastMatchIndex));

                if (m2.matches()) {
                    for (int i = 0; i < diseaseStrings.size() && !foundFieldMatch; i++) {
                        if (fieldStrings.get(i).equals(m2.group(2))) {
                            // only add if the disease isn't a duplicate
                            boolean foundDisease = false;
                            final String[] split = diseaseStrings.get(i).split("/");
                            for (final String s : split) {
                                if (s.equals(m2.group(1))) {
                                    foundDisease = true;
                                }
                            }
                            if ( !foundDisease) {
                                diseaseStrings.set(i, diseaseStrings.get(i) + "/" + m2.group(1));
                            }
                            foundFieldMatch = true;
                        }
                    }

                    // didn't find a matching field value, add a new one
                    if ( !foundFieldMatch) {
                        diseaseStrings.add(m2.group(1));
                        fieldStrings.add(m2.group(2));
                    }
                } else {
                    System.err.println("## Didn't match:\t" + field.substring(lastMatchIndex));
                }
            }

            // compile string collections into one string
            String str = "";
            for (int i = 0; i < diseaseStrings.size(); i++) {
                str += "<p><b>" + diseaseStrings.get(i) + "</b>: " + fieldStrings.get(i) + "</p";
            }
            return str;
        } else {
            return field;
        }
    }

    
    
    /**
     * 
     * @author mcmatt
     *
     */
    private class ChooseFormStructDialog extends JDialog implements ActionListener {
        private static final long serialVersionUID = 4199199899439094828L;

        private final PlugInDialogBRICS_Mapper owner;

        private ViewTableModel structsModel;

        private JTable structsTable;

        private final ArrayList<String> descAL 		= new ArrayList<String>();
        private final ArrayList<String> shortNameAL = new ArrayList<String>();
        private final ArrayList<String> versionAL 	= new ArrayList<String>();
        private final ArrayList<String> statusAL 	= new ArrayList<String>();
        private final ArrayList<String> diseaseAL 	= new ArrayList<String>();

        private JScrollPane structsScrollPane;

        public ChooseFormStructDialog(final PlugInDialogBRICS_Mapper owner) {
            super(owner, true);
            this.owner = owner;

            init();
        }

        private void init() {
            setTitle("Choose Form Structure");
            final int numColumns = 5;
            final String[] columnNames = {"Name", "Description", "Version", "Status", "Disease"};
            structsModel = new ViewTableModel();
            structsTable = new JTable(structsModel) {
                private static final long serialVersionUID = 3053232611901005303L;

                @Override
                public String getToolTipText(final MouseEvent e) {
                    String tip = "";

                    final java.awt.Point p = e.getPoint();
                    final int rowIndex = rowAtPoint(p);
                    final int colIndex = columnAtPoint(p);

                    tip = (String) structsModel.getValueAt(rowIndex, colIndex);
                    return tip;

                }
            };

            structsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

            for (final String element : columnNames) {
                structsModel.addColumn(element);
            }

            structsTable.getColumn("Name").setMinWidth(150);
            structsTable.getColumn("Description").setMinWidth(300);
            structsTable.getColumn("Version").setPreferredWidth(20);
            structsTable.getColumn("Status").setPreferredWidth(40);

            // new way of doing web service
            for (final FormStructure fs : formStructureList) {
                if (fs.getShortName().equals("")) {
                    // something is wrong. a shortname is required. this is to work around an apparent stage DDT problem
                    continue;
                }
                final String desc 		= fs.getDescription();
                final String shortname 	= fs.getShortName();
                final String version 	= fs.getVersion().toString();
                final String status 	= fs.getStatus().toString();
                final String disease 	= fs.getDiseaseStructureString();

                descAL.add(desc);
                shortNameAL.add(shortname);
                versionAL.add(version);
                statusAL.add(status);
                diseaseAL.add(disease);
            }

            // make sure we found a structure for imaging
            if (shortNameAL.size() == 0) {
                MipavUtil.displayWarning("No structures were found in the data dictionary.");
                return;
            }

            // before sorting, remove structs that are not the most current
            for (int i = 0; i < shortNameAL.size(); i++) {
                for (int j = i + 1; j < shortNameAL.size(); j++) {
                    // check for multiple versions of the same struct
                    if (shortNameAL.get(i).equalsIgnoreCase(shortNameAL.get(j))) {
                        // remove any later struct with a lower version number
                        if (Integer.parseInt(versionAL.get(i)) > Integer.parseInt(versionAL.get(j))) {
                            shortNameAL.remove(j);
                            descAL.remove(j);
                            versionAL.remove(j);
                            statusAL.remove(j);
                            diseaseAL.remove(j);
                        } else {
                            shortNameAL.remove(i);
                            descAL.remove(i);
                            versionAL.remove(i);
                            statusAL.remove(i);
                            diseaseAL.remove(i);
                        }
                    }
                }
            }

            final TreeSet<String> sortedNamesSet = new TreeSet<String>(new AlphabeticalComparator());
            for (int i = 0; i < shortNameAL.size(); i++) {
                sortedNamesSet.add(shortNameAL.get(i));
            }

            final Object[] rowData = new Object[numColumns];
            final Iterator<String> iter = sortedNamesSet.iterator();

            while (iter.hasNext()) {
                final String name = iter.next();

                for (int i = 0; i < shortNameAL.size(); i++) {
                    if (name.equals(shortNameAL.get(i))) {
                        rowData[0] = shortNameAL.get(i);
                        rowData[1] = descAL.get(i);
                        rowData[2] = versionAL.get(i);
                        rowData[3] = statusAL.get(i);
                        rowData[4] = diseaseAL.get(i);
                        structsModel.addRow(rowData);

                        break;
                    }
                }
            }

            structsTable.setRowSelectionInterval(0, 0);
            structsTable.setAutoCreateRowSorter(true);
            structsTable.getRowSorter().toggleSortOrder(0);

            structsScrollPane = new JScrollPane(structsTable, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

            structsScrollPane.setPreferredSize(new Dimension(600, 300));

            final JPanel OKPanel = new JPanel();
            final JButton OKButton = new JButton("Select");
            OKButton.setActionCommand("ChooseStructOK");
            OKButton.addActionListener(this);
            OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
            OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
            OKButton.setFont(serif12B);
            OKPanel.add(OKButton);

            getContentPane().add(structsScrollPane, BorderLayout.CENTER);
            getContentPane().add(OKPanel, BorderLayout.SOUTH);

            pack();

            this.setMinimumSize(this.getSize());
            this.setSize(new Dimension(owner.getSize().width, this.getSize().height));
            MipavUtil.centerInWindow(owner, this);

            setVisible(true);

        }

        /**
         * action performed
         */
        @Override
        public void actionPerformed(final ActionEvent e) {
            final String command = e.getActionCommand();
            if (command.equalsIgnoreCase("ChooseStructOK")) {
                final int selectedRow = structsTable.getSelectedRow();
                if (selectedRow != -1) {
                    this.dispose();
                    final String fsName = (String) structsModel.getValueAt(selectedRow, 0);
                    new populateBRICSStructureTable(owner, fsName);
                }
            }
        }

        /**
         * This inner class is used to sort the list by instance number
         */
        private class AlphabeticalComparator implements Comparator<String> {
            @Override
            public int compare(final String a, final String b) {
                return (a.toLowerCase().compareTo(b.toLowerCase()));

            }
        }
    }
    
    
    /**
     * Gets data elements for the selected form and copies them to the BRICS reference data element table
     * 
     */
    private class populateBRICSStructureTable {
        private static final long serialVersionUID = 859201819000159789L;
 
        //private FormStructure formStructure;
           
		/**
		 * Gets data elements for the selected form and copies them to the BRICS reference data element table
		 * @param owner	- main GUI
		 * @param fsName - Form structure name for the selected form structure.
		 */
        public populateBRICSStructureTable(final PlugInDialogBRICS_Mapper owner, final String fsName) {
            
            // Goes and gets the selected form structure
        	final FormDataElementsRESTThread thread = new FormDataElementsRESTThread(owner, fsName, true);
            thread.run();
            formStructure = thread.getFullFormStructure();
 
            if (formStructure == null) {
                MipavUtil.displayError("Form structure not found in Data Dictionary: " + fsName);
                dispose();
                return;
            }

            copyFormStructureToTable();
            listPane.setBorder(JDialogBase.buildTitledBorder("  Reference Form Structure:  " + fsName));
            loadCSVButton.setEnabled(true);
        }
        
        /**
         * Copies the selected form structure data elements into the BRICS reference data element table
         */
        private void copyFormStructureToTable() {
        	
        	deTableModel.setRowCount(formStructure.getDataElements().size());
        	
        	// Since repeatable groups doesn't provide the groups in order that they are presented in the actual form
        	// we calculate the group position to properly display in the correct order in the table
        	// rowPos will contain indexes to the starting rows in the table for each group
        	int[] rowPos = new int[formStructure.getRepeatableGroups().size()]; 
        	for (final RepeatableGroup group : formStructure.getRepeatableGroups() ) {	
	        	rowPos[group.getPosition()]= group.getSize();
        	}
        	
        	for (int j = rowPos.length-1; j >= 0; j--) {
        		int sum = 0;
        		for (int i =0; i < j; i++) {
        			sum = sum + rowPos[i];
        		}
        		rowPos[j]=sum;
        	}
            
        	// TODO:  to URL in DDT.
        	
        	FormStructureData fsInfo = new FormStructureData(formStructure);
        	for (final RepeatableGroup group : formStructure.getRepeatableGroups() ) {
        		int i=0;
	        	for (final MapElement mapde : group.getDataElements()) {
	        		DataElement de = fsInfo.getDataElement(mapde.getStructuralDataElement());
	        		int row = rowPos[group.getPosition()]+i;

	        		deTableModel.setValueAt(group.getName(), row, 0);										// Group
	        		deTableModel.setValueAt(mapde.getStructuralDataElement().getName(), row, 1);			// CDE Variable name
	        		deTableModel.setValueAt(de.getTitle(), row, 2);											// CDE title
	        	
	        		String strPVs = new String();
	                for (ValueRange vr : de.getValueRangeList() ) {
	                	strPVs += vr.getValueRange() + "; ";
	                }
	        		deTableModel.setValueAt(strPVs, row, 3);  // PV list
	        		
	        		//System.out.println(" URI " + de.getUri());  URI seems problematic. - maybe because on staging or demo

	        		
	        		i++;
	        	}
        	}    	     	
        }
    }

    
    /**
     *  MATT   To be deleted.
     * 
     * @author pandyan
     * 
     */
    private class InfoDialogx extends JDialog implements ActionListener, WindowListener, ItemListener, FocusListener {
        private static final long serialVersionUID = 859201819000159789L;

        private final PlugInDialogBRICS_Mapper owner;

        private JPanel dsMainPanel;

        private final Hashtable<RepeatableGroup, JPanel> groupPanelTable = new Hashtable<RepeatableGroup, JPanel>();
        private final Hashtable<RepeatableGroup, JButton> groupRemoveButtonTable = new Hashtable<RepeatableGroup, JButton>();

        private String guid = "";

        private boolean launchedFromInProcessState = false;

        private JLabel requiredLabel;

        private String dataStructureName;

        private FormStructureData fsData;

        private Vector<FileDicomTag> problemTags = null;

        private String problemTagsFileDir;

        private String problemTagsFileName;

        private final ArrayList<File> allOtherFiles = new ArrayList<File>();

        private FormStructure dataStructure;

        private final boolean setInitialVisible;

        private final ArrayList<ArrayList<String>> record;

        private String currFile;


        public InfoDialogx(final PlugInDialogBRICS_Mapper owner, final String name, final boolean launchedFromInProcessState, final boolean setInitialVisible,
                final ArrayList<ArrayList<String>> record) {
            super(owner, false);

            this.owner = owner;
            this.launchedFromInProcessState = launchedFromInProcessState;
            this.setInitialVisible = setInitialVisible;
            this.record = record;

            if (launchedFromInProcessState) {
               /** if (containsGuid(name)) {
                    this.dataStructureName = getStructFromString(name);
                } else {
                    this.dataStructureName = name.substring(0, name.lastIndexOf(STRUCT_GUID_SEPERATOR));
                }*/
            } else {
                //structRowImgFileInfoList.add(null);
                fsDataList.add(null);
                allOtherFilesAL.add(null);
                this.dataStructureName = name;
            }

            for (final FormStructure ds : formStructureList) {
                if (ds.getShortName().equalsIgnoreCase(dataStructureName)) {
                    if (ds.getDataElements().size() == 0) {
                        final FormDataElementsRESTThread thread = new FormDataElementsRESTThread(owner, ds.getShortName(), true);
                        thread.run();

                        dataStructure = thread.getFullFormStructure();
                    } else {
                        dataStructure = ds;
                    }
                }
            }

            if (dataStructure == null) {
                MipavUtil.displayError("Form structure not found in Data Dictionary: aaaaa" + dataStructureName);
                dispose();
                return;
            }

            init();
        }

        private void init() {
            setTitle("Edit Data Elements - " + dataStructureName);
            addWindowListener(this);
            final JPanel mainPanel = new JPanel(new GridBagLayout());

            dsMainPanel = new JPanel(new GridBagLayout());
            final JScrollPane tabScrollPane = new JScrollPane(dsMainPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

            final GridBagConstraints gbc = new GridBagConstraints();

            try {
                setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
            } catch (final Exception e) {
                // setIconImage() is not part of the Java 1.5 API - catch any
                // runtime error on those systems
            }

            try {
                if (launchedFromInProcessState) {
                    final int selectedRow = deTable.getSelectedRow();

                    fsData = fsDataList.get(selectedRow);

                    parseForInitLabelsAndComponents(fsData);
                } else {
                    fsData = new FormStructureData(dataStructure);

                    parseDataStructure(dataStructure, fsData, record);

                    parseForInitLabelsAndComponents(fsData);

                    if ( !setInitialVisible) {
                        // convert any dates found into proper ISO format
                        for (int i = 0; i < csvFieldNames.size(); i++) {
                            /*final String[] deGroupAndName = splitFieldString(csvFieldNames.get(i));

                            StructuralDataElement de = null;
                            for (final GroupRepeat repeat : fsData.getAllGroupRepeats(deGroupAndName[0])) {
                                for (final DataElementValue deVal : repeat.getDataElements()) {
                                    if (deVal.getName().equalsIgnoreCase(deGroupAndName[1])) {
                                        de = deVal.getDataElementInfo();
                                        break;
                                    }
                                }
                            }

                            /**for (final ArrayList<String> values : record) {
                                // check value not empty and check type of field for date
                                if ( !values.get(i).trim().equals("") && de.getType().equals(DataType.DATE)) {
                                    values.set(i, convertDateToISOFormat(values.get(i)));
                                }
                            }*/
                        }

                        // this means it was launched via the csv file
                        // populateFieldsFromCSV(fsData, record);
                    }
                }
            } catch (final Exception e) {
                e.printStackTrace();
            }

            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.insets = new Insets(10, 5, 10, 25);
            gbc.gridwidth = 1;

            final JPanel OKPanel = new JPanel();

            final JButton OKButton = new JButton("Save");
            OKButton.setActionCommand("StructDialogOK");
            OKButton.addActionListener(this);
            OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
            OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
            OKButton.setFont(serif12B);

            final JButton cancelButton = new JButton("Cancel");
            cancelButton.setActionCommand("StructDialogCancel");
            cancelButton.addActionListener(this);
            cancelButton.setMinimumSize(MipavUtil.defaultButtonSize);
            cancelButton.setPreferredSize(MipavUtil.defaultButtonSize);
            cancelButton.setFont(serif12B);

            OKPanel.add(OKButton);
            OKPanel.add(cancelButton);

            requiredLabel = new JLabel(
                    "<html>Mouse over data element name for a description.<br/>Mouse over the data element fields for more information on filling them in.<br/>* Required data elements are in <font color=\"red\">red</font></html>");

            gbc.fill = GridBagConstraints.BOTH;
            gbc.anchor = GridBagConstraints.EAST;
            gbc.weightx = 0;
            gbc.gridx = 0;
            gbc.gridy = 0;
            mainPanel.add(requiredLabel, gbc);

            gbc.gridy = 2;
            gbc.weightx = 1;
            gbc.weighty = 1;
            mainPanel.add(tabScrollPane, gbc);
            gbc.weightx = 0;
            gbc.weighty = 0;
            gbc.gridy = 3;
            mainPanel.add(OKPanel, gbc);

            getContentPane().add(mainPanel);

            final Dimension dim = getContentPane().getPreferredSize();
            if (dim.height > 500) {
                dim.height = 500;
            }
            tabScrollPane.setPreferredSize(dim);

            pack();
            MipavUtil.centerInWindow(owner, this);
            if (setInitialVisible) {
                setVisible(true);
            }
        }


        public Vector<FileDicomTag> getProblemTags() {
            return problemTags;
        }

        public String getProblemFileDir() {
            return problemTagsFileDir;
        }

        public String getProblemFileName() {
            return problemTagsFileName;
        }

        private void parseForInitLabelsAndComponents(final FormStructureData fsData) {
            final GridBagConstraints gbc = new GridBagConstraints();
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.insets = new Insets(2, 5, 2, 5);

            for (final RepeatableGroup g : fsData.getStructInfo().getRepeatableGroups()) {
                final JPanel groupPanel = new JPanel(new GridBagLayout());

                final GridBagConstraints egbc = new GridBagConstraints();
                egbc.insets = new Insets(2, 5, 2, 5);
                egbc.fill = GridBagConstraints.HORIZONTAL;
                egbc.weightx = 1;
                egbc.gridx = 0;
                egbc.anchor = GridBagConstraints.WEST;
                egbc.gridy = 0;

                for (final GroupRepeat repeat : fsData.getAllGroupRepeats(g.getName())) {
                    final JPanel repeatPanel = buildGroupRepeatPanel(repeat);
                    repeatPanel.setBorder(JDialogBase.buildTitledBorder("Repeat number " + (repeat.getRepeatNumber() + 1)));

                    if (repeatPanel.getComponentCount() > 0) {
                        groupPanel.add(repeatPanel, egbc);
                        egbc.gridy++;
                    }
                }

                final JPanel groupPanelWithControls = new JPanel(new BorderLayout());
                groupPanelWithControls.setBorder(JDialogBase.buildTitledBorder(g.getName()));
                groupPanelWithControls.add(groupPanel, BorderLayout.NORTH);
                groupPanelWithControls.add(buildRepeatControlPanel(g), BorderLayout.SOUTH);

                if (groupPanel.getComponentCount() > 0) {
                    gbc.gridy = g.getPosition(); // group position is 0-based (unlike data element position)
                    dsMainPanel.add(groupPanelWithControls, gbc);
                    groupPanelTable.put(g, groupPanel);
                }
            }
        }

        private JPanel buildRepeatControlPanel(final RepeatableGroup group) {
            final JPanel repeatControlPanel = new JPanel(new GridBagLayout());
            final GridBagConstraints gbc = new GridBagConstraints();
            gbc.insets = new Insets(2, 5, 2, 5);
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.weightx = 1;
            gbc.gridx = 0;
            gbc.anchor = GridBagConstraints.WEST;
            gbc.gridy = 0;

            JLabel repeatLabel;
            final JButton addRepeatButton = new JButton("Add repeat");
            addRepeatButton.setActionCommand("AddRepeat_-_" + group.getName());
            addRepeatButton.addActionListener(this);
            final JButton removeRepeatButton = new JButton("Remove repeat");
            removeRepeatButton.setActionCommand("RemoveRepeat_-_" + group.getName());
            removeRepeatButton.addActionListener(this);
            groupRemoveButtonTable.put(group, removeRepeatButton);
            if (group.getThreshold() == 0) {
                repeatLabel = new JLabel("Optional group");
            } else {
                switch (group.getType()) {
                    case MORETHAN:
                        repeatLabel = new JLabel("At least " + group.getThreshold() + " repeat(s) required");
                        if (fsData.getNumGroupRepeats(group.getName()) == 1) {
                            removeRepeatButton.setEnabled(false);
                        }
                        break;
                    case LESSTHAN:
                        repeatLabel = new JLabel("Less than " + group.getThreshold() + " repeat(s) allowed");
                        if (fsData.getNumGroupRepeats(group.getName()) == 1) {
                            removeRepeatButton.setEnabled(false);
                        }
                        break;
                    case EXACTLY:
                        repeatLabel = new JLabel("Exactly " + group.getThreshold() + " repeat(s) allowed");
                        addRepeatButton.setEnabled(false);
                        removeRepeatButton.setEnabled(false);
                        break;
                    default:
                        repeatLabel = new JLabel(group.getType() + " " + group.getThreshold());
                }
            }
            repeatLabel.setFont(serif12);
            repeatControlPanel.add(repeatLabel, gbc);

            gbc.gridx++;
            repeatControlPanel.add(addRepeatButton, gbc);

            gbc.gridx++;
            repeatControlPanel.add(removeRepeatButton, gbc);

            return repeatControlPanel;
        }
        
        /**
         * 
         * @param repeat
         * @return
         */
        private JPanel buildGroupRepeatPanel(final GroupRepeat repeat) {
            final JPanel repeatPanel = new JPanel(new GridBagLayout());

            final GridBagConstraints gbc = new GridBagConstraints();
            gbc.insets = new Insets(2, 5, 2, 5);
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.weightx = 1;
            gbc.gridx = 0;
            gbc.anchor = GridBagConstraints.WEST;

            for (final DataElementValue deVal : repeat.getDataElements()) {
                if ( (fixErrors == FIX_ERRORS_NOW && errors.contains(deVal)) || fixErrors == FIX_ERRORS_LATER || fixErrors == FIX_ERRORS_CANCEL) {
                    final JPanel elementPanel = new JPanel(new GridBagLayout());
                    final GridBagConstraints egbc = new GridBagConstraints();
                    egbc.insets = new Insets(2, 5, 2, 5);
                    egbc.fill = GridBagConstraints.HORIZONTAL;
                    egbc.anchor = GridBagConstraints.EAST;
                    egbc.weightx = 0;
                    // elementPanel.add(l, egbc);

                    egbc.weightx = 1;
                    egbc.gridy = 0;
                    egbc.gridx = 0;
                    egbc.anchor = GridBagConstraints.WEST;

                    final StructuralDataElement deInfo = deVal.getDataElementInfo();

                    if (deInfo.getType().equals(DataType.FILE) || deInfo.getType().equals(DataType.TRIPLANAR)) {
                        elementPanel.add(deVal.getComp(), egbc);
                        egbc.gridx++;
                        final JButton browseButton = new JButton("Browse");
                        browseButton.addActionListener(this);
                        browseButton.setActionCommand("browse_-_" + repeat.getGroupInfo().getName() + "_-_" + repeat.getRepeatNumber() + "_-_"
                                + deInfo.getName());
                        elementPanel.add(browseButton, egbc);
                    } else {
                        egbc.gridwidth = 2;
                        if (deInfo.getRestrictions() == InputRestrictions.MULTIPLE) {
                            // the stored component is the JList of option. instead, add the scrollpane containing it (a
                            // viewport is in between)
                            elementPanel.add(deVal.getComp().getParent().getParent(), egbc);
                        } else {
                            elementPanel.add(deVal.getComp(), egbc);
                        }

                        //if (isLegacyOtherSpecifyField(deVal)) {
                        //    egbc.gridy++;
                        //    elementPanel.add(deVal.getOtherSpecifyField(), egbc);
                        //}
                    }

                    // gbc.gridy++;
                    gbc.insets = new Insets(2, 5, 2, 5);
                    gbc.fill = GridBagConstraints.HORIZONTAL;
                    gbc.anchor = GridBagConstraints.NORTHWEST;
                    gbc.weightx = 0;
                    gbc.gridx = 0;
                    gbc.gridy = deVal.getPosition() - 1; // data element position is 1-based
                    // gbc.gridy++;
                    repeatPanel.add(deVal.getLabel(), gbc);
                    gbc.gridx = 1;
                    gbc.anchor = GridBagConstraints.NORTHEAST;
                    gbc.weightx = 1;
                    repeatPanel.add(elementPanel, gbc);

                    // gridYCounter = gridYCounter + 1;
                    // gbc.gridy = gridYCounter;
                    gbc.gridx = 0;
                    gbc.gridwidth = 1;
                }
            }

            return repeatPanel;
        }

        /**
         * 
         * @param dataStructure
         * @param fsData
         * @param record
         */
        private void parseDataStructure(final FormStructure dataStructure, final FormStructureData fsData, final ArrayList<ArrayList<String>> record) {
            // setup the group bins in the form data
            for (final RepeatableGroup g : dataStructure.getRepeatableGroups()) {
                // if the group repeats an exact number of times, create them now
                // otherwise, start with just one (threshold of 0 ==> optional)
                int numRepeats = 0;
                if (g.getType().equals(RepeatableType.EXACTLY) && g.getThreshold() > 0) {
                    numRepeats = g.getThreshold();
                } else if (record != null) {
                    // check the row values to see how many repeats
                    for (final ArrayList<String> row : record) {
                        boolean foundValue = false;
                        for (int i = 0; i < row.size(); i++) {
                            //if (getFieldGroup(csvFieldNames.get(i)).equalsIgnoreCase(g.getName())) {
                            //    if ( !row.get(i).equals("")) {
                            //        foundValue = true;
                             //       break;
                             //   }
                            //}
                        }

                        // found something in this row for this group
                        if (foundValue) {
                            numRepeats++;
                        }
                    }
                }

                // if no values or threshold of 0, build at least one repeat
                if (numRepeats == 0) {
                    numRepeats = 1;
                }

                fsData.addGroup(g.getName());
                for (int i = 0; i < numRepeats; i++) {
                    final GroupRepeat repeat = parseGroupRepeat(fsData, g, i);

                    fsData.addGroupRepeat(g.getName(), repeat);
                }
            }
        }

        
        /**
         * 
         * @param fsData
         * @param group
         * @param repeatNum
         * @return
         */
        private GroupRepeat parseGroupRepeat(final FormStructureData fsData, final RepeatableGroup group, final int repeatNum) {
            final GroupRepeat repeat = new GroupRepeat(group, fsData, repeatNum);

            for (final MapElement de : group.getDataElements()) {
                final DataElementValue newDeVal = new DataElementValue(repeat, de);
                final DataElement deFullInfo = fsData.getDataElement(de.getStructuralDataElement());

                JLabel l;

                l = new JLabel(deFullInfo.getTitle());
                // l = new JLabel(de.getStructuralDataElement().getName());

                l.setFont(MipavUtil.font12);
                l.setName(de.getStructuralDataElement().getName());

                String tooltip = "<html><p><b>Name:</b> " + de.getStructuralDataElement().getName() + "<br/>";
                tooltip += "<b>Required?:</b> " + de.getRequiredType().getValue() + "<br/>";
                tooltip += "<b>Description:</b><br/>" + WordUtils.wrap(deFullInfo.getDescription(), 80, "<br/>", false);
                tooltip += "</p></html>";
                l.setToolTipText(tooltip);

                for (final Alias a : de.getStructuralDataElement().getAliasList()) {
                    System.out.println(a);
                }

                // if valuerange is enumeration, create a combo box...otherwise create a textfield

                if (de.getStructuralDataElement().getValueRangeList() != null && de.getStructuralDataElement().getValueRangeList().size() > 0
                        && de.getStructuralDataElement().getType() != null && !de.getStructuralDataElement().getType().equals(DataType.DATE)) {
                    if (de.getStructuralDataElement().getRestrictions() == InputRestrictions.SINGLE
                            || de.getStructuralDataElement().getRestrictions() == InputRestrictions.FREE_FORM) {
                        final JComboBox cb = new JComboBox();
                        cb.setName(de.getStructuralDataElement().getName());
                        cb.setFont(MipavUtil.font12);

                        cb.addItem("");
                        final List<ValueRange> valuesList = new ArrayList<ValueRange>();
                        valuesList.addAll(de.getStructuralDataElement().getValueRangeList());
                        Collections.sort(valuesList);
                        for (final ValueRange val : valuesList) {
                            cb.addItem(val.getValueRange());
                        }
                        cb.addItemListener(this);

                        if (de.getRequiredType().equals(RequiredType.REQUIRED)) {
                            l.setForeground(Color.red);
                        }

                        tooltip = "<html>";
                        if (de.getStructuralDataElement().getMeasuringUnit() != null) {
                            tooltip += "<p><b>Unit of measure:</b> " + de.getStructuralDataElement().getMeasuringUnit() + "</p>";
                        }

                        if (deFullInfo.getNinds() != null && !deFullInfo.getNinds().getValue().equals("")) {
                            tooltip += "<p><b>NINDS CDE ID:</b> " + deFullInfo.getNinds().getValue() + "</p>";
                        }
                        if (deFullInfo.getGuidelines() != null && !deFullInfo.getGuidelines().trim().equals("")) {
                            tooltip += "<p><b>Guidelines & Instructions:</b></br>"
                                    + WordUtils.wrap(removeRedundantDiseaseInfo(deFullInfo.getGuidelines()), 80, "<br/>", false) + "</p>";
                        }
                        if (deFullInfo.getNotes() != null && !deFullInfo.getNotes().trim().equals("")) {
                            tooltip += "<p><b>Notes:</b><br/>" + WordUtils.wrap(removeRedundantDiseaseInfo(deFullInfo.getNotes()), 80, "<br/>", false) + "</p>";
                        }
                        tooltip += "</html>";
                        if ( !tooltip.equals("<html></html>")) {
                            cb.setToolTipText(tooltip);
                        }

                        newDeVal.setLabel(l);
                        newDeVal.setComp(cb);
                    } else if (de.getStructuralDataElement().getRestrictions() == InputRestrictions.MULTIPLE) {
                        final List<ValueRange> valuesList = new ArrayList<ValueRange>();
                        valuesList.addAll(de.getStructuralDataElement().getValueRangeList());
                        Collections.sort(valuesList);
                        final String[] valStrList = new String[valuesList.size()];
                        int i = 0;
                        for (final ValueRange val : valuesList) {
                            valStrList[i] = val.getValueRange();
                            i++;
                        }

                        final JList list = new JList(valStrList);
                        list.setName(de.getStructuralDataElement().getName());
                        list.setFont(MipavUtil.font12);
                        list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
                        list.setLayoutOrientation(JList.VERTICAL);
                        list.setVisibleRowCount(MULTI_SELECT_VISIBLE_ROWS);

                        final JScrollPane listScroller = new JScrollPane(list);
                        listScroller.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
                        // listScroller.setPreferredSize(new Dimension(250, 80));

                        if (de.getRequiredType().equals(RequiredType.REQUIRED)) {
                            l.setForeground(Color.red);
                        }

                        tooltip = "<html>";
                        if (de.getStructuralDataElement().getMeasuringUnit() != null) {
                            tooltip += "<p><b>Unit of measure:</b> " + de.getStructuralDataElement().getMeasuringUnit() + "</p>";
                        }

                        if (deFullInfo.getNinds() != null && !deFullInfo.getNinds().getValue().equals("")) {
                            tooltip += "<p><b>NINDS CDE ID:</b> " + deFullInfo.getNinds().getValue() + "</p>";
                        }
                        if (deFullInfo.getGuidelines() != null && !deFullInfo.getGuidelines().trim().equals("")) {
                            tooltip += "<p><b>Guidelines & Instructions:</b></br>"
                                    + WordUtils.wrap(removeRedundantDiseaseInfo(deFullInfo.getGuidelines()), 80, "<br/>", false) + "</p>";
                        }
                        if (deFullInfo.getNotes() != null && !deFullInfo.getNotes().trim().equals("")) {
                            tooltip += "<p><b>Notes:</b><br/>" + WordUtils.wrap(removeRedundantDiseaseInfo(deFullInfo.getNotes()), 80, "<br/>", false) + "</p>";
                        }
                        tooltip += "</html>";
                        if ( !tooltip.equals("<html></html>")) {
                            list.setToolTipText(tooltip);
                        }

                        newDeVal.setLabel(l);
                        newDeVal.setComp(list);
                    }
                } else {
                    final JTextField tf = new JTextField(20);
                    tf.setName(de.getStructuralDataElement().getName());
                    tf.setFont(MipavUtil.font12);

                    tf.addMouseListener(new ContextMenuMouseListener());

                    tooltip = "<html><p><b>Type:</b> " + de.getStructuralDataElement().getType().getValue();
                    if (de.getStructuralDataElement().getType().equals(DataType.ALPHANUMERIC) && de.getStructuralDataElement().getSize() != null) {
                        tooltip += " (" + de.getStructuralDataElement().getSize() + ")";
                    }
                    tooltip += "</p>";

                    if (de.getStructuralDataElement().getType().equals(DataType.NUMERIC)
                            || de.getStructuralDataElement().getType().equals(DataType.ALPHANUMERIC)) {
                        if (de.getStructuralDataElement().getMinimumValue() != null || de.getStructuralDataElement().getMaximumValue() != null) {
                            tooltip += "<p>";
                            if (de.getStructuralDataElement().getMinimumValue() != null) {
                                tooltip += "<b>Min:</b> " + de.getStructuralDataElement().getMinimumValue() + " ";
                            }
                            if (de.getStructuralDataElement().getMaximumValue() != null) {
                                tooltip += "<b>Max:</b> " + de.getStructuralDataElement().getMaximumValue();
                            }
                            tooltip += "</p>";
                        }
                    }

                    if (de.getStructuralDataElement().getMeasuringUnit() != null) {
                        tooltip += "<p><b>Unit of measure:</b> " + de.getStructuralDataElement().getMeasuringUnit() + "</p>";
                    }

                    if (deFullInfo.getNinds() != null && !deFullInfo.getNinds().getValue().equals("")) {
                        tooltip += "<p><b>NINDS CDE ID:</b> " + deFullInfo.getNinds().getValue() + "</p>";
                    }
                    if (deFullInfo.getGuidelines() != null && !deFullInfo.getGuidelines().trim().equals("")) {
                        tooltip += "<p><b>Guidelines & Instructions:</b></br>"
                                + WordUtils.wrap(removeRedundantDiseaseInfo(deFullInfo.getGuidelines()), 80, "<br/>", false) + "</p>";
                    }
                    if (deFullInfo.getNotes() != null && !deFullInfo.getNotes().trim().equals("")) {
                        tooltip += "<p><b>Notes:</b><br/>" + WordUtils.wrap(removeRedundantDiseaseInfo(deFullInfo.getNotes()), 80, "<br/>", false) + "</p>";
                    }
                    tooltip += "</html>";
                    tf.setToolTipText(tooltip);
                    tf.addFocusListener(this);

                    if (de.getRequiredType().equals(RequiredType.REQUIRED)) {
                        l.setForeground(Color.red);
                    }

                    newDeVal.setLabel(l);
                    newDeVal.setComp(tf);
                }

                repeat.addDataElement(newDeVal);
            }

            // now that all DEs are added, find sister elements
            DataElementValue bigSister = null;
            for (final DataElementValue deVal : repeat.getDataElements()) {
               /** if (isNewOtherSpecifyField(deVal)) {
                    bigSister = deVal;
                } else if (bigSister != null && isSisterField(deVal)) {
                    // assume that they need to be next to each other and the specify field comp is a text field
                    if (deVal.getPosition() == bigSister.getPosition() + 1 && deVal.getComp() instanceof JTextField) {
                        bigSister.setOtherSpecifyField((JTextField) deVal.getComp());
                    }
                } else {
                    // didn't find a sister, so reset the saved big sister
                    bigSister = null;
                }*/
            }

            return repeat;
        }       

        /**
         * action performed
         */
        @Override
        public void actionPerformed(final ActionEvent e) {
            final String command = e.getActionCommand();
            ArrayList<String> errs;
            final StringBuffer errors = new StringBuffer();
            if (command.equalsIgnoreCase("StructDialogOK")) {
                errs = validateFields();
                boolean isComplete = true;
                if (errs.size() != 0) {
                    for (int i = 0; i < errs.size(); i++) {
                        errors.append(" - " + errs.get(i) + "\n");
                    }
                    final Object[] options = {"Fix now", "Fix later"};
                    fixErrors = JOptionPane.showOptionDialog(null, errors.toString(), "Warning", JOptionPane.DEFAULT_OPTION, JOptionPane.WARNING_MESSAGE, null,
                            options, options[0]);
                    isComplete = false;
                }

                complete(fsData, isComplete);
                enableDisableFinishButton();
                dispose();

                if (fixErrors == FIX_ERRORS_NOW && errs.size() != 0) {
                    fixErrors();
                }

            } else if (command.equalsIgnoreCase("StructDialogCancel")) {
                if ( !launchedFromInProcessState) {
                    //structRowImgFileInfoList.remove(structRowImgFileInfoList.size() - 1);
                    fsDataList.remove(fsDataList.size() - 1);
                    allOtherFilesAL.remove(allOtherFilesAL.size() - 1);
                }
                enableDisableFinishButton();
                dispose();
            } else if (command.startsWith("browse_-_")) {
                // changed format of command to browse_-_GROUPNAME_-_REPNUM_-_DENAME from browse_DENAME
                if (currFile == null) {
                    currFile = "";
                }

                final String[] commandSplit = command.split("_-_");
                final String groupName = commandSplit[1];
                final int repeatNum = Integer.parseInt(commandSplit[2]);
                final String deName = commandSplit[3];

                boolean isMultiFile = false;
                // System.out.println(dataStructureName);
                int junk = 0;
				if ( junk == 0 ) { /** Matt isMainImagingFileElement(groupName, deName)) { */
                    
                } else {
                    final JFileChooser chooser = new JFileChooser();
                    chooser.setDialogTitle("Choose file");
                    chooser.setMultiSelectionEnabled(true);
                    chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
                    final int returnValue = chooser.showOpenDialog(this);
                    if (returnValue == JFileChooser.APPROVE_OPTION) {
                        final File[] files = chooser.getSelectedFiles();

                        String allFilePaths = files[0].getAbsolutePath();
                        for (int i = 1; i < files.length; i++) {
                            allFilePaths += BROWSE_NONIMG_DELIM + files[i].getAbsolutePath();
                        }

                        for (final DataElementValue deVal : fsData.getGroupRepeat(groupName, repeatNum).getDataElements()) {
                            if (deVal.getName().equalsIgnoreCase(deName)) {
                                final JTextField tf = (JTextField) deVal.getComp();
                                tf.setText(allFilePaths);
                                tf.setEnabled(false);
                                break;
                            }
                        }
                    }
                }
            } else if (command.startsWith("AddRepeat_-_")) {
                final String[] commandSplit = command.split("_-_");
                final String groupName = commandSplit[1];

                final GroupRepeat lastRepeat = fsData.getCurrentGroupRepeat(groupName);

                final GroupRepeat newRepeat = parseGroupRepeat(fsData, lastRepeat.getGroupInfo(), lastRepeat.getRepeatNumber() + 1);
                fsData.addGroupRepeat(groupName, newRepeat);

                final GridBagConstraints egbc = new GridBagConstraints();
                egbc.insets = new Insets(2, 5, 2, 5);
                egbc.fill = GridBagConstraints.HORIZONTAL;
                egbc.weightx = 1;
                egbc.gridx = 0;
                egbc.anchor = GridBagConstraints.WEST;
                egbc.gridy = newRepeat.getRepeatNumber();

                final JPanel groupPanel = groupPanelTable.get(lastRepeat.getGroupInfo());
                final JPanel repeatPanel = buildGroupRepeatPanel(newRepeat);
                repeatPanel.setBorder(JDialogBase.buildTitledBorder("Repeat number " + (newRepeat.getRepeatNumber() + 1)));
                groupPanel.add(repeatPanel, egbc);

                final Window parentWindow = SwingUtilities.getWindowAncestor(groupPanel);
                // don't force the re-packing if the window hasn't been shown/created yet
                if (parentWindow != null) {
                    parentWindow.pack();
                    parentWindow.validate();
                    parentWindow.repaint();
                }

                // if more than one repeat, enable removal
                if (fsData.getNumGroupRepeats(groupName) > 1) {
                    // TODO: also check against repeat type/threshold
                    groupRemoveButtonTable.get(lastRepeat.getGroupInfo()).setEnabled(true);
                }
            } else if (command.startsWith("RemoveRepeat_-_")) {
                final String[] commandSplit = command.split("_-_");
                final String groupName = commandSplit[1];

                final int response = JOptionPane.showConfirmDialog(this, "Are you sure you want to remove the last repeat?", "Remove repeat?",
                        JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

                if (response == JOptionPane.YES_OPTION) {
                    final GroupRepeat lastRepeat = fsData.getCurrentGroupRepeat(groupName);

                    final JPanel groupPanel = groupPanelTable.get(lastRepeat.getGroupInfo());

                    groupPanel.remove(lastRepeat.getRepeatNumber());

                    fsData.removeLastGroupRepeat(groupName);

                    final Window parentWindow = SwingUtilities.getWindowAncestor(groupPanel);
                    // don't force the re-packing if the window hasn't been shown/created yet
                    if (parentWindow != null) {
                        parentWindow.pack();
                        parentWindow.validate();
                        parentWindow.repaint();
                    }
                }

                // don't allow removal of the last repeat
                if (fsData.getNumGroupRepeats(groupName) == 1) {
                    // TODO: also check against repeat type/threshold
                    ((JButton) e.getSource()).setEnabled(false);
                }
            }
        }

        /**
         * Creates new dialog containing only fields that contained errors
         */
        private void fixErrors() {
            final String dsName = (String) deTableModel.getValueAt(deTable.getSelectedRow(), 0);
            //new InfoDialog(owner, dsName, true, true, null);
            fixErrors = FIX_ERRORS_LATER;
        }

        /**
         * DELETE - validates fields
         * 
         * @return
         */
        public ArrayList<String> validateFields() {
            final ArrayList<String> errs = new ArrayList<String>();

            parseDataStructForValidation(fsData, errs);

            return errs;
        }

        /**
         * DELETE - validates fields
         */
        public void parseDataStructForValidation(final FormStructureData fsData, final ArrayList<String> errs) {
            errors = new ArrayList<DataElementValue>();
            String value = "";
            final String key = "";
            RequiredType required = null;
            DataType type = null;
            String title = "";

            for (final RepeatableGroup group : fsData.getStructInfo().getRepeatableGroups()) {
                for (final GroupRepeat repeat : fsData.getAllGroupRepeats(group.getName())) {
                    for (final DataElementValue deVal : repeat.getDataElements()) {
                        final StructuralDataElement deInfo = deVal.getDataElementInfo();

                        final JComponent deComp = deVal.getComp();
                        if (deComp instanceof JTextField) {
                            value = ((JTextField) deComp).getText().trim();
                        } else if (deComp instanceof JComboBox) {
                            value = (String) ( ((JComboBox) deComp).getSelectedItem());
                        } else if (deComp instanceof JList) {
                            value = "";
                            final int[] selectedIndicies = ((JList) deComp).getSelectedIndices();
                            for (final int index : selectedIndicies) {
                                if (value == "") {
                                    value = (String) ((JList) deComp).getModel().getElementAt(index);
                                } else {
                                    value += MULTI_SELECT_VALUE_DELIM + (String) ((JList) deComp).getModel().getElementAt(index);
                                }
                            }
                        }

                        // now we need to validate
                        required = deVal.getRequiredType();
                        type = deInfo.getType();
                        title = fsData.getDataElement(deInfo).getTitle();

                        if (required.equals(RequiredType.REQUIRED)) {
                            if (value.trim().equalsIgnoreCase("")) {
                                errs.add(title + " is a required field");
                                errors.add(deVal);
                            } else {
                                if (key.equalsIgnoreCase(GUID_ELEMENT_NAME)) {
                                   /** if ( !isGuid(value.trim())) {
                                        errs.add(title + " must begin with a valid GUID prefix");
                                        errors.add(deVal);
                                    }*/
                                }
                            }
                        }

                        if (type.equals(DataType.NUMERIC)) {
                            if ( !value.trim().equalsIgnoreCase("")) {
                                try {
                                    final float floatValue = Float.valueOf(value.trim()).floatValue();
                                    if (deInfo.getMinimumValue() != null && floatValue < deInfo.getMinimumValue().floatValue()) {
                                        errs.add(title + " must be in the range of " + deInfo.getMinimumValue().floatValue() + " to "
                                                + deInfo.getMaximumValue().floatValue());
                                        errors.add(deVal);
                                    } else if (deInfo.getMaximumValue() != null && floatValue > deInfo.getMaximumValue().floatValue()) {
                                        errs.add(title + " must be in the range of " + deInfo.getMinimumValue().floatValue() + " to "
                                                + deInfo.getMaximumValue().floatValue());
                                        errors.add(deVal);
                                    }
                                } catch (final NumberFormatException e) {
                                    errs.add(title + " must be a number");
                                    errors.add(deVal);
                                }
                            }
                        }
                        if (type.equals(DataType.ALPHANUMERIC)) {
                            if (deInfo.getSize() != null && deInfo.getSize() > 0 && value.length() > deInfo.getSize()) {
                                errs.add(title + " must not exceed " + deInfo.getSize() + " in length");
                                errors.add(deVal);
                            }
                        }
                    }
                }
            }
        }

        /**
         * DELETE - called after validation is done
         */
        public void complete(final FormStructureData fsData, final boolean isComplete) {
            String value = "";

            for (final RepeatableGroup group : fsData.getStructInfo().getRepeatableGroups()) {
                for (final GroupRepeat repeat : fsData.getAllGroupRepeats(group.getName())) {
                    for (final DataElementValue deVal : repeat.getDataElements()) {
                        final JLabel label = deVal.getLabel();
                        final JComponent comp = deVal.getComp();

                        if (label.getName().equalsIgnoreCase(GUID_ELEMENT_NAME)) {
                            guid = ((JTextField) comp).getText().trim();
                        }

                        if (comp instanceof JTextField) {
                            value = ((JTextField) comp).getText().trim();
                            // ok...all files will go into the allOtherFiles AL

                            final File f = new File(value);
                            if (f.isFile()) {
                                allOtherFiles.add(f);
                            }

                        } else if (comp instanceof JComboBox) {
                            value = (String) ( ((JComboBox) comp).getSelectedItem());
                            if (value.equalsIgnoreCase(VALUE_OTHER_SPECIFY) || value.equalsIgnoreCase(VALUE_YES_SPECIFY)) {
                                // value = deVal.getOtherSpecifyField().getText().trim();
                            }
                        } else if (comp instanceof JList) {
                            value = "";
                            final int[] selectedIndicies = ((JList) comp).getSelectedIndices();
                            for (final int index : selectedIndicies) {
                                if (value == "") {
                                    value = (String) ((JList) comp).getModel().getElementAt(index);
                                } else {
                                    value += MULTI_SELECT_VALUE_DELIM + (String) ((JList) comp).getModel().getElementAt(index);
                                }
                            }
                        }

                        /*
                         * if(!value.equals("")) { System.out.println("the key is " + key);
                         * System.out.println("the value is " + value); }
                         */

                        deVal.setValue(value);
                    }
                }
            }

            // boolean guidKnown = true;
            // if (guid != null && !guid.trim().equalsIgnoreCase("")) {
            // guidKnown = false;
            // }

            String name = "";

            if (guid != null && !guid.trim().equalsIgnoreCase("")) {
                name = fsData.getStructInfo().getShortName() + STRUCT_GUID_SEPERATOR + guid;
            } else {
                name = fsData.getStructInfo().getShortName() + STRUCT_GUID_SEPERATOR + "UNKNOWNGUID";
            }

            if (launchedFromInProcessState) {
                final int selectedRow = deTable.getSelectedRow();

                deTableModel.setValueAt(name, selectedRow, 0);
                if (isComplete) {
                	deTableModel.setValueAt("Yes", selectedRow, 1);
                } else {
                	deTableModel.setValueAt("No", selectedRow, 1);
                }

                fsDataList.set(selectedRow, fsData);

                allOtherFilesAL.set(selectedRow, allOtherFiles);

            } else {
                fsDataList.set(fsDataList.size() - 1, fsData);
                final Vector<String> rowData = new Vector<String>();
                rowData.add(name);
                if (isComplete) {
                    rowData.add("Yes");
                } else {
                    rowData.add("No");
                }
                deTableModel.addRow(rowData);
                deTable.setRowSelectionInterval(deTableModel.getRowCount() - 1, deTableModel.getRowCount() - 1);

                allOtherFilesAL.set(allOtherFilesAL.size() - 1, allOtherFiles);
            }
        }

        @Override
        public void windowActivated(final WindowEvent e) {}

        @Override
        public void windowClosed(final WindowEvent e) {}

        @Override
        public void windowClosing(final WindowEvent e) {
            enableDisableFinishButton();
        }

        @Override
        public void windowDeactivated(final WindowEvent e) {}

        @Override
        public void windowDeiconified(final WindowEvent e) {}

        @Override
        public void windowIconified(final WindowEvent e) {}

        @Override
        public void windowOpened(final WindowEvent e) {}

        @Override
        public void itemStateChanged(final ItemEvent e) {
            validateFields();
        }

        @Override
        public void focusGained(final FocusEvent e) {}

        @Override
        public void focusLost(final FocusEvent e) {
            validateFields();
        }
    }

    
    /**
     * Filter out Imaging and Archived form structures leaving only CLINCICAL form structures
     * @param fullList
     * @return
     */
    private List<FormStructure> filterDataStructures(final List<FormStructure> fullList) {
        final List<FormStructure> filteredList = new ArrayList<FormStructure>();

        for (final FormStructure ds : fullList) {
            if (ds.getShortName().equals("")) {
                // something is wrong. a shortname is required. this is to work around an apparent stage DDT problem
                continue;
            }

            if (ds.getFileType().equals(SubmissionType.IMAGING)) {
                // only include non-archived structures
                if ( !ds.getStatus().equals(StatusType.ARCHIVED)) {
                    filteredList.add(ds);
                }
            }
        }

        // Returning clinical form structures
        return filteredList;
    }


    
    /**
     * Connects to BRICS data dictionary web service (via RESTful API) to retrieve the list of form
     * structures (without their attached data elements).
     */
    public class FormListRESTThread extends Thread implements ActionListener {
        private static final String ddAuthBase = "/portal/ws/webstart/dictionary/formStructure/details";

        private static final String ddRequestBase = "/portal/ws/ddt/dictionary/FormStructure";

        // private static final String ddStructListRequest = ddRequestBase + "/Published/list?type=CLINICAL";
        //private static final String ddStructListRequest = ddRequestBase + "/Published/list?type=CLINICAL&incDEs=false";
        private static final String ddStructListRequest = ddRequestBase + "/Published/list?type=IMAGING&incDEs=false";

        private JButton progressCancelButton;

        private final PlugInDialogBRICS_Mapper parent;

        public FormListRESTThread(final PlugInDialogBRICS_Mapper parent) {
            super();
            this.parent = parent;
        }

        @Override
        public void run() {
            ViewJProgressBar progressBar = null;
            try {
                progressBar = new ViewJProgressBar("BRICS", "Retrieving form structures from BRICS data dictionary...", 0, 100, true);
                progressBar.setVisible(true);
                progressBar.updateValue(20);
                progressBar.setIndeterminate(true);
                progressCancelButton = progressBar.getCancelButton();
                progressCancelButton.addActionListener(this);

                // should have already read in the config file (moved from here to be before the GUI init() call)

                WebClient client;
                if (ddUseAuthService) {
                    client = WebClient.create(ddServerURL + ddAuthBase);
                } else {
                    client = WebClient.create(ddServerURL + ddStructListRequest);
                }

                final HTTPConduit conduit = WebClient.getConfig(client).getHttpConduit();
                conduit.getClient().setReceiveTimeout(0);

                if ( !ddAuthUser.equals("") && !ddAuthPass.equals("")) {
                    client.header("userName", ddAuthUser);
                    client.header("pass", ddAuthPass);
                }

                final long startTime = System.currentTimeMillis();
                List<FormStructure> fullList;
                if (ddUseAuthService) {
                    fullList = (List<FormStructure>) client.accept("text/xml").getCollection(FormStructure.class);
                } else {
                    final DataStructureList dsl = client.accept("text/xml").get(DataStructureList.class);
                    fullList = dsl.getList();
                }
                final long endTime = System.currentTimeMillis();

                System.out.println("Webservice request (sec):\t" + ( (endTime - startTime) / 1000));

                formStructureList = filterDataStructures(fullList);

                // for (final FormStructure ds : dataStructureList) {
                // System.out.println("FS title:\t" + ds.getTitle() + "\tversion:\t" + ds.getVersion() + "\tpub:\t" +
                // ds.getStatus());
                // }

                progressBar.updateValue(100);
                progressBar.setVisible(false);
                progressBar.dispose();
                printlnToLog("Successful retrieval of form structures.");

                selectStructButton.setEnabled(true);
            } catch (final Exception e) {
                e.printStackTrace();
                if (progressBar != null) {
                    progressBar.setVisible(false);
                    progressBar.dispose();
                    MipavUtil.displayError("Error in connecting to web service");
                    parent.dispose();
                    if (JDialogStandalonePlugin.isExitRequired()) {
                        System.gc();
                        System.exit(0);
                    }
                }
            }
        }

        @Override
        public void actionPerformed(final ActionEvent e) {
            if (e.getSource() == progressCancelButton) {
                dispose();
                if (JDialogStandalonePlugin.isExitRequired()) {
                    System.gc();
                    System.exit(0);
                }
            }
        }
    }

    /**
     * Class that connects to BRICS data dictionary web service (via RESTful API) to retrieve the data elements for a
     * given form structure.
     */
    private class FormDataElementsRESTThread extends Thread implements ActionListener {

        private static final String ddRequestBase = "/portal/ws/ddt/dictionary/FormStructure";

        private JButton progressCancelButton;

        private final PlugInDialogBRICS_Mapper parent;

        private final String formStructName;

        private FormStructure fullFormStructure;

        private final boolean addProgressBar;
        
        /**
         * 
         * @param parent
         * @param formStructName
         * @param addProgressBar
         */
        public FormDataElementsRESTThread(final PlugInDialogBRICS_Mapper parent, final String formStructName, final boolean addProgressBar) {
            super();
            this.parent = parent;
            this.formStructName = formStructName;
            this.addProgressBar = addProgressBar;
        }

        @Override
        public void run() {
            ViewJProgressBar progressBar = null;
            try {
                if (addProgressBar) {
                    progressBar = new ViewJProgressBar("BRICS", "Retrieving data elements for form structure: " + formStructName, 0, 100, true);
                    progressBar.setVisible(true);
                    progressBar.updateValue(20);
                    progressBar.setIndeterminate(true);
                    progressCancelButton = progressBar.getCancelButton();
                    progressCancelButton.addActionListener(this);
                }

                // should have already read in the config file (moved from here to be before the GUI init() call)

                WebClient client;
                client = WebClient.create(ddServerURL + ddRequestBase + "/" + formStructName);

                final HTTPConduit conduit = WebClient.getConfig(client).getHttpConduit();
                conduit.getClient().setReceiveTimeout(0);

                final long startTime = System.currentTimeMillis();
                fullFormStructure = client.accept("text/xml").get(FormStructure.class);
                final long endTime = System.currentTimeMillis();

                System.out.println("Webservice request (sec):\t" + ( (endTime - startTime) / 1000));

                for (int i = 0; i < formStructureList.size(); i++) {
                    final FormStructure curFS = formStructureList.get(i);
                    if (curFS.getShortName().equals(fullFormStructure.getShortName())) {
                    	formStructureList.set(i, fullFormStructure);
                        break;
                    }
                }

                // for (final FormStructure ds : dataStructureList) {
                // System.out.println("FS title:\t" + ds.getTitle() + "\tversion:\t" + ds.getVersion() + "\tpub:\t"
                // +
                // ds.getStatus());
                // }

                if (addProgressBar) {
                    progressBar.updateValue(100);
                    progressBar.setVisible(false);
                    progressBar.dispose();
                }
                printlnToLog("Successful retrieval of data elements for form structure: " + formStructName);
            } catch (final Exception e) {
                e.printStackTrace();
                if (progressBar != null) {
                    progressBar.setVisible(false);
                    progressBar.dispose();
                    MipavUtil.displayError("Error in connecting to web service");
                    parent.dispose();
                    if (JDialogStandalonePlugin.isExitRequired()) {
                        System.gc();
                        System.exit(0);
                    }
                }
            }
        }

        @Override
        public void actionPerformed(final ActionEvent e) {
            if (e.getSource() == progressCancelButton) {
                dispose();
                if (JDialogStandalonePlugin.isExitRequired()) {
                    System.gc();
                    System.exit(0);
                }
            }
        }

        public FormStructure getFullFormStructure() {
            return fullFormStructure;
        }
    }

    
    /**
     * Pops up the Data dictionary page. If cdePage is null go to default DDT page: "https://fitbir.nih.gov/content/data-dictionary".
     * 
     * @param cdePage The name of the CDE page.
     */
    private static void showCDE(final String cdePage) {
    	final URI ddtURI;
        final String ddtPage = "https://fitbir.nih.gov/content/data-dictionary";
        try {
            if (cdePage == null) {
            	ddtURI = new URI(ddtPage);
            } else {
            	ddtURI = new URI(cdePage);
            }
            
            Desktop.getDesktop().browse(ddtURI);
        } catch (final URISyntaxException e) {
            e.printStackTrace();
            MipavUtil.displayError("Unable to display Data Dictionary page : " + ddtPage);
            return;
        } catch (final IOException e) {
            e.printStackTrace();
            MipavUtil.displayError("Unable to display Data Dictionary page : " + ddtPage);
            return;
        }
    }

    
    
    /**
     * 
     * @author mcmatt
     *
     */
    public class DataElementValue {
        private GroupRepeat deGroup;

        private JLabel deLabel;
        private JComponent deComp;
        private JTextField otherSpecifyField;
        private StructuralDataElement deInfo;
        private String deValue;
        private int dePosition;

        private RequiredType deRequiredType;

        public DataElementValue(final GroupRepeat group, final JLabel label, final JComponent comp, final MapElement info, final String val) {
            deGroup = group;
            deLabel = label;
            setComp(comp);
            deInfo = info.getStructuralDataElement();
            dePosition = info.getPosition();
            deRequiredType = info.getRequiredType();
            deValue = val;
        }

        public DataElementValue(final GroupRepeat group, final MapElement info) {
            deGroup = group;
            deInfo = info.getStructuralDataElement();
            dePosition = info.getPosition();
            deRequiredType = info.getRequiredType();
        }

        public String getGroupName() {
            return deGroup.getGroupInfo().getName();
        }

        public GroupRepeat getGroup() {
            return deGroup;
        }

        public void setGroup(final GroupRepeat deGroup) {
            this.deGroup = deGroup;
        }

        public JLabel getLabel() {
            return deLabel;
        }

        public void setLabel(final JLabel deLabel) {
            this.deLabel = deLabel;
        }

        public JComponent getComp() {
            return deComp;
        }

        public void setComp(final JComponent deComp) {
            this.deComp = deComp;

           /** if (isLegacyOtherSpecifyField(this)) {
                otherSpecifyField = new JTextField();
                otherSpecifyField.setVisible(false);
                ((JComboBox) deComp).addActionListener(new OtherSpecifyListener(otherSpecifyField));
            }*/
        }

        public JTextField getOtherSpecifyField() {
            return otherSpecifyField;
        }

        public void setOtherSpecifyField(final JTextField tf) {
            otherSpecifyField = tf;
        }

        public String getName() {
            return deInfo.getName();
        }

        public StructuralDataElement getDataElementInfo() {
            return deInfo;
        }

        public void setDataElementInfo(final StructuralDataElement deInfo) {
            this.deInfo = deInfo;
        }

        public int getPosition() {
            return dePosition;
        }

        public void setPosition(final int dePosition) {
            this.dePosition = dePosition;
        }

        public RequiredType getRequiredType() {
            return deRequiredType;
        }

        public void setRequiredType(final RequiredType deRequiredType) {
            this.deRequiredType = deRequiredType;
        }

        public String getValue() {
            return deValue;
        }

        public void setValue(final String deValue) {
            this.deValue = deValue;
        }
    }
    
    /**
     * 
     * @author mcmatt
     *
     */

    public class GroupRepeat {
        private RepeatableGroup groupInfo;

        private FormStructureData parentStruct;

        private Vector<DataElementValue> dataElements;

        private int repeatNumber;

        public GroupRepeat(final RepeatableGroup groupInfo, final FormStructureData parentStruct, final Vector<DataElementValue> dataElements,
                final int repeatNumber) {
            this.groupInfo = groupInfo;
            this.parentStruct = parentStruct;
            this.dataElements = dataElements;
            this.repeatNumber = repeatNumber;
        }

        public GroupRepeat(final RepeatableGroup groupInfo, final FormStructureData parentStruct, final int repeatNumber) {
            this.groupInfo = groupInfo;
            this.parentStruct = parentStruct;
            this.repeatNumber = repeatNumber;
            this.dataElements = new Vector<DataElementValue>();
        }

        public RepeatableGroup getGroupInfo() {
            return groupInfo;
        }

        public void setGroupInfo(final RepeatableGroup groupInfo) {
            this.groupInfo = groupInfo;
        }

        public FormStructureData getParentStruct() {
            return parentStruct;
        }

        public void setParentStruct(final FormStructureData parentStruct) {
            this.parentStruct = parentStruct;
        }

        public void addDataElement(final DataElementValue de) {
            dataElements.add(de);
        }

        public Vector<DataElementValue> getDataElements() {
            return dataElements;
        }

        public void setDataElements(final Vector<DataElementValue> dataElements) {
            this.dataElements = dataElements;
        }

        public int getRepeatNumber() {
            return repeatNumber;
        }

        public void setRepeatNumber(final int repeatNumber) {
            this.repeatNumber = repeatNumber;
        }
    }

    /**
     * 
     * @author mcmatt
     *
     */
    private class FormStructureData {
        
    	private FormStructure structInfo;
    	private final Hashtable<String, Vector<GroupRepeat>> groupTable;

    	
        public FormStructureData(final FormStructure structInfo) {
            this.structInfo = structInfo;
            groupTable = new Hashtable<String, Vector<GroupRepeat>>();
        }

        public void addGroup(final String groupName) {
            groupTable.put(groupName, new Vector<GroupRepeat>());
        }

        public void addGroupRepeat(final String groupName, final GroupRepeat repeat) {
            repeat.setRepeatNumber(groupTable.get(groupName).size());
            groupTable.get(groupName).add(repeat);
        }

        public void removeLastGroupRepeat(final String groupName) {
            final int numRepeats = groupTable.get(groupName).size();
            if (numRepeats > 0) {
                groupTable.get(groupName).removeElementAt(numRepeats - 1);
            }
        }

        public Vector<GroupRepeat> getAllGroupRepeats(final String groupName) {
            return groupTable.get(groupName);
        }

        public int getNumGroupRepeats(final String groupName) {
            return groupTable.get(groupName).size();
        }

        public GroupRepeat getCurrentGroupRepeat(final String groupName) {
            return groupTable.get(groupName).lastElement();
        }

        public boolean isGroupRepeatSet(final String groupName, final int groupNum) {
            return groupNum < groupTable.get(groupName).size();
        }

        public GroupRepeat getGroupRepeat(final String groupName, final int groupNum) {
            return groupTable.get(groupName).get(groupNum);
        }

        public FormStructure getStructInfo() {
            return structInfo;
        }

        public void setStructInfo(final FormStructure structInfo) {
            this.structInfo = structInfo;
        }

        public DataElement getDataElement(final StructuralDataElement deInfo) {
            return getStructInfo().getDataElements().get(deInfo.getNameAndVersion());
        }

        public boolean isDataElementInForm(final String groupName, final String deName) {
            return (structInfo.getRepeatableGroupByName(groupName) != null)
                    && (structInfo.getRepeatableGroupByName(groupName).getMapElementByName(deName) != null);
        }
    }

    

}
