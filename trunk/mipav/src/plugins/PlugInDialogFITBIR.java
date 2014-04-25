import gov.nih.mipav.plugins.JDialogStandalonePlugin;

import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.LightboxGenerator;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBConcat;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.WidgetFactory;
import gov.nih.mipav.view.dialogs.JDialogBase;

import gov.nih.tbi.commons.model.DataType;
import gov.nih.tbi.commons.model.RepeatableType;
import gov.nih.tbi.commons.model.RequiredType;
import gov.nih.tbi.commons.model.StatusType;
import gov.nih.tbi.dictionary.model.DictionaryRestServiceModel.DataStructureList;
import gov.nih.tbi.dictionary.model.hibernate.*;
import gov.nih.tbi.repository.model.SubmissionType;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.MemoryImageSource;
import java.io.*;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.text.NumberFormat;
import java.util.*;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.DefaultTableCellRenderer;

import org.apache.commons.lang3.text.WordUtils;
import org.apache.cxf.jaxrs.client.WebClient;
import org.apache.cxf.transport.http.HTTPConduit;
import org.bouncycastle.util.encoders.Hex;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;

import com.sun.jimi.core.Jimi;
import com.sun.jimi.core.JimiException;


/**
 * TODO: Not properly dealing with versions if there are multiple for the same form structure (in reading CSV or pulling
 * struct info from DDT).
 * 
 * TODO: Need to test reading group repeats from CSV.
 * 
 * TODO: Need to test writing group repeats to CSV.
 * 
 * TODO: Add non-ImgFile support for selecting multiple files and have them automatically zipped together?
 */
public class PlugInDialogFITBIR extends JFrame implements ActionListener, ChangeListener, ItemListener, TreeSelectionListener, MouseListener,
        PreviewImageContainer, WindowListener {
    private static final long serialVersionUID = -5516621806537554154L;

    private final Font serif12 = MipavUtil.font12, serif12B = MipavUtil.font12B;

    private WidgetFactory.ScrollTextArea logOutputArea;

    private JScrollPane listPane;

    private JTable structTable;

    private JButton addStructButton, loadCSVButton, finishButton, removeStructButton, editDataElementsButton, outputDirButton;

    private JTextField outputDirTextField;

    private ViewTableModel structTableModel;

    private String outputDirBase;

    private String csvFileDir;

    private Hashtable<String, String> csvStructRowData;

    private static final String CSV_OUTPUT_DELIM = ",";

    private final ArrayList<ViewJComponentPreviewImage> previewImages = new ArrayList<ViewJComponentPreviewImage>();

    private final ArrayList<File> imageFiles = new ArrayList<File>();

    private final ArrayList<ArrayList<File>> allOtherFilesAL = new ArrayList<ArrayList<File>>();

    private final ArrayList<Boolean> multifiles = new ArrayList<Boolean>();

    private final ArrayList<FormStructureData> fsDataList = new ArrayList<FormStructureData>();

    private ArrayList<DataElementValue> errors;

    private int fixErrors = FIX_ERRORS_LATER;

    private static final int FIX_ERRORS_NOW = 0;

    private static final int FIX_ERRORS_LATER = 1;

    private static final int FIX_ERRORS_CANCEL = -1;

    private JPanel previewImgPanel;

    private ViewJComponentPreviewImage previewImg;

    private float previewImgContrast = 1;

    private int previewImgBrightness = 0;

    private JLabel previewImgBrightnessLabel, previewImgContrastLabel;

    private JSlider previewImgBrightnessSlider, previewImgContrastSlider;

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

    /** Property for reading the dd server url from the fitbir config file. */
    private static final String ddServerURLProp = "ddServerURL";

    /** Property for reading the auth server url from the fitbir config file. */
    private static final String authServerURLProp = "authServerURL";

    /** Property for reading the dd authentication user name from the fitbir config file. */
    private static final String ddAuthUserProp = "ddAuthUser";

    /** Property for reading the dd authentication password from the fitbir config file. */
    private static final String ddAuthPassProp = "ddAuthPass";

    /**
     * Property for reading whether to use the (slower) authenticated web service, which allows testing against draft
     * forms.
     */
    private static final String ddUseAuthServiceProp = "ddUseAuthService";

    /** Full data dictionary server url */
    private static String ddServerURL = ddProdServer;

    /** Full authentication server url */
    private static String authServerURL = authProdServer;

    private static String ddAuthUser = "";

    private static String ddAuthPass = "";

    private static boolean ddUseAuthService = false;

    private List<DataStructure> dataStructureList;

    private File csvFile;

    private ArrayList<String> csvFieldNames;

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

    private static final String pluginVersion = "0.15";

    private static final String VALUE_OTHER_SPECIFY = "Other, specify";

    private static final String GUID_ELEMENT_NAME = "GUID";

    private static final String IMG_IMAGE_INFO_GROUP = "Image Information";

    private static final String IMG_FILE_ELEMENT_NAME = "ImgFile";

    private static final String IMG_PREVIEW_ELEMENT_NAME = "ImgPreviewFile";

    private static final String IMG_HASH_CODE_ELEMENT_NAME = "ImgFileHashCode";

    private static final String recordIndicatorColumn = "record";

    private static final String recordIndicatorValue = "x";

    private static final String PDBP_IMAGING_STRUCTURE_PREFIX = "PDBPImag";

    private static final String SITE_NAME_ELEMENT_NAME = "SiteName";

    private static final String[] PDBP_ALLOWED_SITE_NAMES = {"Brigham and Women's", "Columbia University", "Emory University", "Johns Hopkins University",
            "Pennsylvania State University (Hershey)", "Pacific Northwest National Laboratory", "University of Alabama (Birmingham)",
            "University of Pennsylvania", "University of Florida (Gainesville)", "University of Washington", "UT-Southwestern Medical Center",};

    private static final String[] allowedGuidPrefixes = new String[] {"TBI", "PD"};

    private static final String[] imagingStructurePrefixes = new String[] {"Imag", PDBP_IMAGING_STRUCTURE_PREFIX};

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

    public PlugInDialogFITBIR() {
        super();

        outputDirBase = Preferences.getProperty(Preferences.PREF_BRICS_PLUGIN_OUTPUT_DIR);
        if (outputDirBase == null) {
            outputDirBase = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "BRICS_Imaging_Submission" + File.separator;
            Preferences.setProperty(Preferences.PREF_BRICS_PLUGIN_OUTPUT_DIR, outputDirBase);
        }

        csvFileDir = Preferences.getProperty(Preferences.PREF_BRICS_PLUGIN_CSV_DIR);
        if (csvFileDir == null) {
            csvFileDir = ViewUserInterface.getReference().getDefaultDirectory();
        }

        init();
        setVisible(true);
        validate();

        final int response = JOptionPane.showConfirmDialog(this, PlugInDialogFITBIR.PRIVACY_NOTICE, "Image Submission Package Creation Tool",
                JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

        if (response == JOptionPane.NO_OPTION) {
            if (ViewUserInterface.getReference() != null && ViewUserInterface.getReference().isPlugInFrameVisible()) {
                System.exit(0);
            } else {
                return;
            }
        }

        final Thread thread = new RESTThread(this);
        thread.start();
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        final String command = e.getActionCommand();

        if (command.equalsIgnoreCase("AddStruct")) {

            new ChooseDataStructDialog(this);

            removeStructButton.setEnabled(structTableModel.getRowCount() > 0);
            editDataElementsButton.setEnabled(structTableModel.getRowCount() > 0);
            listPane.setBorder(JDialogBase.buildTitledBorder(structTableModel.getRowCount() + " Form Structure(s) "));

        } else if (command.equalsIgnoreCase("LoadCSV")) {
            final JFileChooser chooser = new JFileChooser();
            chooser.setCurrentDirectory(new File(csvFileDir));
            chooser.setDialogTitle("Choose CSV file");
            chooser.addChoosableFileFilter(new FileNameExtensionFilter("Comma separated value files (.csv)", "csv"));
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                csvFile = chooser.getSelectedFile();
                readCSVFile();

                csvFileDir = chooser.getSelectedFile().getAbsolutePath() + File.separator;
                Preferences.setProperty(Preferences.PREF_BRICS_PLUGIN_CSV_DIR, csvFileDir);
            }
        } else if (command.equalsIgnoreCase("RemoveStruct")) {
            final int selected = structTable.getSelectedRow();
            structTableModel.removeRow(selected);
            previewImages.remove(selected);
            imageFiles.remove(selected);
            multifiles.remove(selected);
            fsDataList.remove(selected);

            previewImgPanel.removeAll();
            previewImgPanel.repaint();

            if (structTable.getRowCount() >= 1) {

                if (selected == 0) {
                    structTable.setRowSelectionInterval(0, 0);
                } else {
                    structTable.setRowSelectionInterval(selected - 1, selected - 1);
                }

                if (previewImages.get(structTable.getSelectedRow()) != null) {
                    previewImgPanel.add(previewImages.get(structTable.getSelectedRow()));
                    previewImages.get(structTable.getSelectedRow()).setSliceBrightness(previewImgBrightness, previewImgContrast);
                    previewImgPanel.validate();
                    previewImgPanel.repaint();
                }
            }

            removeStructButton.setEnabled(structTableModel.getRowCount() > 0);
            editDataElementsButton.setEnabled(structTableModel.getRowCount() > 0);
            if (structTableModel.getRowCount() > 0) {
                enableDisableFinishButton();

                if (selected >= structTableModel.getRowCount()) {
                    structTable.setRowSelectionInterval(structTableModel.getRowCount() - 1, structTableModel.getRowCount() - 1);
                } else {
                    structTable.setRowSelectionInterval(selected, selected);
                }

            } else {
                finishButton.setEnabled(false);

            }
            listPane.setBorder(JDialogBase.buildTitledBorder(structTableModel.getRowCount() + " Form Structure(s) "));
        } else if (command.equalsIgnoreCase("HelpWeb")) {

            MipavUtil.showWebHelp("Image_submission_plug-in");

        } else if (command.equalsIgnoreCase("Finish")) {

            if (isFinished) {
                dispose();
                if (JDialogStandalonePlugin.isExitRequired()) {
                    System.exit(0);
                }
            } else {

                final javax.swing.SwingWorker<Object, Object> worker = new javax.swing.SwingWorker<Object, Object>() {
                    @Override
                    public Object doInBackground() {
                        try {
                            createSubmissionFiles();
                        } catch (final Throwable e) {
                            e.printStackTrace();
                        }

                        // need to fix this so it actually works
                        if (isFinished) {
                            finishButton.setText("Close");
                            finishButton.setEnabled(true);
                        }

                        return null;
                    }
                };
                final int response = JOptionPane.showConfirmDialog(this, "Done adding image datasets?", "Done adding image datasets?",
                        JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

                // we're now letting the fields be enforced by the validation
                // tool
                // final int numRows = sourceTableModel.getRowCount();
                // boolean areAllCompleted = true;
                // for (int i = 0; i < numRows; i++) {
                // if ( ((String) sourceTableModel.getValueAt(i,
                // 1)).equalsIgnoreCase("No")) {
                // areAllCompleted = false;
                // break;
                // }
                // }
                //
                // if ( !areAllCompleted) {
                // MipavUtil.displayError("Please complete required fields for all Form Structures");
                // return;
                // }

                // instead, just require that the GUIDs are filled in
                final int numRows = structTableModel.getRowCount();
                int validGuids = 1;
                for (int i = 0; i < numRows; i++) {
                    final String struct = (String) structTableModel.getValueAt(i, 0);
                    if (struct.endsWith("_UNKNOWNGUID")) {
                        validGuids = -1;
                        break;
                    } else {
                        final String guidTester = struct.substring(struct.indexOf("_") + 1, struct.length() - 1);
                        if ( !isGuid(guidTester)) {
                            validGuids = 0;
                            break;
                        }
                    }
                }

                if (response == JOptionPane.YES_OPTION) {

                    if (validGuids == -1) {
                        MipavUtil.displayError("Please complete GUID field for all Form Structures");
                        return;
                    } else if (validGuids == 0) {
                        MipavUtil.displayError("One or more GUID is invalid");
                        return;
                    }

                    removeStructButton.setEnabled(false);
                    finishButton.setEnabled(false);
                    outputDirButton.setEnabled(false);
                    addStructButton.setEnabled(false);
                    editDataElementsButton.setEnabled(false);
                    loadCSVButton.setEnabled(false);

                    worker.execute();
                }
            }
        } else if (command.equalsIgnoreCase("EditDataElements")) {

            final String dsName = (String) structTableModel.getValueAt(structTable.getSelectedRow(), 0);
            new InfoDialog(this, dsName, true, true, null);

        } else if (command.equalsIgnoreCase("OutputDirBrowse")) {
            final JFileChooser chooser = new JFileChooser(outputDirBase);

            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            chooser.setDialogTitle("Choose output directory for Validation Tool files");
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
                    final String[] list = f.list();
                    if (list != null) {
                        for (int k = 0; k < list.length; k++) {
                            final File entry = new File(f, list[k]);
                            entry.delete();
                        }
                    }
                    f.delete();
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

    private boolean readCSVFile() {
        try {
            String str;
            final FileInputStream fis = new FileInputStream(csvFile);
            final BufferedReader br = new BufferedReader(new InputStreamReader(fis));

            // first line is data structure name and version
            str = br.readLine().trim();
            String[] arr = str.split(CSV_OUTPUT_DELIM);
            final String dsName = arr[0];
            final String version = arr[1];

            // second line are the field names
            str = br.readLine().trim();
            final String[] csvFieldNamesWithRecord = str.split(CSV_OUTPUT_DELIM);

            // List of records, each record consisting of 1+ lines, split into field values
            final ArrayList<ArrayList<ArrayList<String>>> recordList = new ArrayList<ArrayList<ArrayList<String>>>();

            csvFieldNames = new ArrayList<String>(csvFieldNamesWithRecord.length - 1);
            int recordFieldIndex = -1;
            for (int i = 0; i < csvFieldNamesWithRecord.length; i++) {
                if (csvFieldNamesWithRecord[i].equalsIgnoreCase(recordIndicatorColumn)) {
                    recordFieldIndex = i;
                } else {
                    csvFieldNames.add(csvFieldNamesWithRecord[i]);
                }
            }

            ArrayList<String> csvParams;
            while ( (str = br.readLine()) != null) {
                str = str.trim();
                arr = str.split(CSV_OUTPUT_DELIM);

                csvParams = new ArrayList<String>(csvFieldNamesWithRecord.length);
                for (int i = 0; i < arr.length; i++) {
                    csvParams.add(arr[i]);
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

            for (final ArrayList<ArrayList<String>> record : recordList) {
                new InfoDialog(this, dsName, false, false, record);
            }

            fis.close();
        } catch (final Exception e) {
            e.printStackTrace();
            return false;
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

        if (source == previewImgBrightnessSlider) {
            previewImgBrightness = previewImgBrightnessSlider.getValue();
            previewImgBrightnessLabel.setText(String.valueOf(previewImgBrightness));

            // Change only the brightness and contrast of the current slice
            if (previewImg != null) {
                previewImages.get(structTable.getSelectedRow()).setSliceBrightness(previewImgBrightness, previewImgContrast);
            }
        } else if (source == previewImgContrastSlider) {
            previewImgContrast = (float) Math.pow(10.0, previewImgContrastSlider.getValue() / 200.0);
            previewImgContrastLabel.setText(String.valueOf(NumberFormat.getNumberInstance().format(previewImgContrast)));

            // Change only the brightness and contrast of the current slice
            if (previewImg != null) {
                previewImages.get(structTable.getSelectedRow()).setSliceBrightness(previewImgBrightness, previewImgContrast);
            }
        }
    }

    @Override
    public void itemStateChanged(final ItemEvent e) {

    }

    private void init() {
        setTitle("Image Submission Package Creation Tool v" + pluginVersion);

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
        // dataStructures = new ArrayList<DataStruct>();

        final JPanel topPanel = new JPanel(new GridBagLayout());
        final GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.NORTHWEST;

        final JPanel brightnessContrastPanel = buildBrightnessContrastPanel();

        previewImgPanel = new JPanel();
        previewImgPanel.setBorder(JDialogBase.buildTitledBorder("Preview image"));
        previewImgPanel.setPreferredSize(new Dimension(200, 250));

        gbc2.gridy = 0;
        gbc2.gridx = 0;
        gbc2.fill = GridBagConstraints.BOTH;
        gbc2.weightx = 0;
        gbc2.weighty = 0;
        topPanel.add(previewImgPanel, gbc2);
        gbc2.gridy = 1;
        topPanel.add(brightnessContrastPanel, gbc2);

        gbc2.gridy = 0;
        gbc2.gridx = 1;
        gbc2.weightx = 1;
        gbc2.weighty = 1;
        gbc2.gridheight = 2;
        topPanel.add(buildStructuePanel(), gbc2);

        final JMenuBar menuBar = new JMenuBar();
        final JMenu menu = new JMenu("Help");
        menuBar.add(menu);

        final JMenuItem helpMenuItem = new JMenuItem("Help web page");
        helpMenuItem.setActionCommand("HelpWeb");
        helpMenuItem.addActionListener(this);
        menu.add(helpMenuItem);

        final JMenuItem memMenuItem = new JMenuItem("Memory usage");
        memMenuItem.setActionCommand("MemoryUsage");
        memMenuItem.addActionListener(ViewUserInterface.getReference());
        menu.add(memMenuItem);

        final JMenuItem jvmMenuItem = new JMenuItem("JVM information");
        jvmMenuItem.setActionCommand("AboutJava");
        jvmMenuItem.addActionListener(ViewUserInterface.getReference());
        menu.add(jvmMenuItem);

        this.setJMenuBar(menuBar);

        getContentPane().add(topPanel, BorderLayout.NORTH);

        getContentPane().add(buildLogPanel(), BorderLayout.CENTER);
        getContentPane().add(buildButtonPanel(), BorderLayout.SOUTH);
        pack();
        validate();
        this.setMinimumSize(this.getSize());
        this.setResizable(true);
        MipavUtil.centerOnScreen(this);
    }

    /**
     * Build a panel for the zip and metadata file creation log.
     */
    private JPanel buildLogPanel() {
        final JPanel destPanel = new JPanel(new GridBagLayout());

        final GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.CENTER;

        gbc2.gridy = 0;
        gbc2.gridx = 0;

        logOutputArea = WidgetFactory.buildScrollTextArea(Color.white);
        logOutputArea.setBorder(JDialogBase.buildTitledBorder("Output log"));
        logOutputArea.getTextArea().setEditable(false);
        logOutputArea.getTextArea().setRows(10);
        final JPanel outputDirPanel = new JPanel();
        final JLabel outputDirLabel = new JLabel("Output Directory for Validation Tool ");
        outputDirTextField = new JTextField(30);
        outputDirTextField.setEditable(false);
        outputDirTextField.setToolTipText(outputDirBase);
        outputDirTextField.setText(outputDirBase);
        outputDirButton = WidgetFactory.buildTextButton("Browse", "Choose Output Directory for Validation Tool files", "OutputDirBrowse", this);
        outputDirButton.setPreferredSize(MipavUtil.defaultButtonSize);
        outputDirPanel.add(outputDirLabel);
        outputDirPanel.add(outputDirTextField);
        outputDirPanel.add(outputDirButton);

        destPanel.add(outputDirPanel, gbc2);

        gbc2.gridy = 1;
        gbc2.fill = GridBagConstraints.BOTH;
        gbc2.weightx = 1;
        gbc2.weighty = 1;
        gbc2.anchor = GridBagConstraints.NORTHWEST;
        destPanel.add(logOutputArea, gbc2);

        return destPanel;
    }

    @Override
    public void valueChanged(final TreeSelectionEvent e) {

    }

    private JScrollPane buildStructuePanel() {
        structTableModel = new ViewTableModel();
        structTableModel.addColumn("Form Structure Name");
        structTableModel.addColumn("Completed?");

        structTable = new JTable(structTableModel);
        structTable.addMouseListener(this);
        structTable.setPreferredScrollableViewportSize(new Dimension(650, 300));
        structTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

        structTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        structTable.getColumn("Completed?").setMinWidth(100);
        structTable.getColumn("Completed?").setMaxWidth(100);

        structTable.getColumn("Completed?").setCellRenderer(new MyRightCellRenderer());

        listPane = WidgetFactory.buildScrollPane(structTable);
        listPane.setBorder(JDialogBase.buildTitledBorder(0 + " Form Structure(s) "));

        return listPane;
    }

    private ModelImage createThumbnailImage(final ModelImage origImage) {
        ModelImage thumbnailImage = null;

        // create a thumbnail image...4 colums, 2 rows
        // grab the middle 8 slices from the image for the thumbnail
        // need to determine by what percentage...so...need to figure out by
        // what percebtahe the xdim will go down
        // to 128
        // startSLice will be 3 less than middle slice
        // endSlice will be 4 more than middle slice
        final int xDim = origImage.getExtents()[0];
        int percentage = 100;
        if (xDim > 128) {
            final float perc = 128f / xDim * 100;
            percentage = (int) Math.floor(perc);
        }
        final int columns = 4;
        final int rows = 2;
        final int rBorderVal = 255;
        final int gBorderVal = 0;
        final int bBorderVal = 0;
        final int borderThick = 1;
        int startSlice = 0;
        int endSlice = 0;
        int numSlices = 0;
        int middleSlice = 0;
        LightboxGenerator lightGen;

        if (origImage.is2DImage()) {
            // Creating a blank TransMatrix for resampling
            final TransMatrix percentSizer = new TransMatrix(4);
            percentSizer.set(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1);

            // Resample image size based on percent inputted
            final AlgorithmTransform transformer = new AlgorithmTransform(origImage, percentSizer, 1,
                    (float) (origImage.getResolutions(0)[0] / (percentage * .01)), (float) (origImage.getResolutions(0)[1] / (percentage * .01)),
                    (int) (origImage.getExtents()[0] * percentage * .01), (int) (origImage.getExtents()[1] * percentage * .01), origImage.getUnitsOfMeasure(),
                    false, true, false, true, origImage.getImageCentermm(false));
            transformer.runAlgorithm();
            thumbnailImage = transformer.getTransformedImage();
            thumbnailImage.calcMinMax();
            // convert this image to color image if it is not
            if ( !thumbnailImage.isColorImage()) {
                final ModelImage newRGB = new ModelImage(ModelStorageBase.ARGB, thumbnailImage.getExtents(), thumbnailImage.getImageName());
                final AlgorithmRGBConcat mathAlgo = new AlgorithmRGBConcat(thumbnailImage, thumbnailImage, thumbnailImage, newRGB, true, true, 255.0f, true);
                mathAlgo.run();
                thumbnailImage.disposeLocal();
                thumbnailImage = null;
                thumbnailImage = newRGB;
            }
        } else if (origImage.is3DImage()) {
            numSlices = origImage.getExtents()[2];
            numSlices = numSlices - 1; // its 0 based
            middleSlice = numSlices / 2;
            startSlice = middleSlice - 3;
            if (startSlice < 0) {
                startSlice = 0;
            }
            endSlice = middleSlice + 4;
            if (endSlice > numSlices - 1) {
                endSlice = numSlices - 1;
            }

            try {
                // Make algorithm
                lightGen = new LightboxGenerator(origImage, startSlice, endSlice, percentage, rows, columns, rBorderVal, gBorderVal, bBorderVal, false,
                        borderThick);
                lightGen.run();
                thumbnailImage = lightGen.getImage();
                thumbnailImage.calcMinMax();
            } catch (final Exception e) {
                e.printStackTrace();
            }
        } else if (origImage.is4DImage()) {
            // get middle time volume
            final int[] destExtents = new int[3];
            final int xSlices = origImage.getExtents()[0];
            final int ySlices = origImage.getExtents()[1];
            final int zSlices = origImage.getExtents()[2];
            destExtents[0] = xSlices;
            destExtents[1] = ySlices;
            destExtents[2] = zSlices;

            ModelImage timeImage = new ModelImage(origImage.getType(), destExtents, "");

            final int tSlices = origImage.getExtents()[3];
            int middleVol = (int) Math.floor(tSlices / 2);
            if (middleVol > 0) {
                middleVol = middleVol - 1; // 0 based
            }
            final AlgorithmSubset subsetAlgo = new AlgorithmSubset(origImage, timeImage, AlgorithmSubset.REMOVE_T, middleVol);
            subsetAlgo.run();

            numSlices = timeImage.getExtents()[2];
            numSlices = numSlices - 1; // its 0 based
            middleSlice = numSlices / 2;
            startSlice = middleSlice - 3;
            if (startSlice < 0) {
                startSlice = 0;
            }
            endSlice = middleSlice + 4;
            if (endSlice > numSlices - 1) {
                endSlice = numSlices - 1;
            }
            try {
                // Make algorithm
                lightGen = new LightboxGenerator(timeImage, startSlice, endSlice, percentage, rows, columns, rBorderVal, gBorderVal, bBorderVal, false,
                        borderThick);
                lightGen.run();
                thumbnailImage = lightGen.getImage();
                thumbnailImage.calcMinMax();
                if (timeImage != null) {
                    timeImage.disposeLocal();
                    timeImage = null;
                }
            } catch (final Exception e) {

            }
        }

        return thumbnailImage;
    }

    /**
     * Create the ZIP(s) containing the original image files and the meta-data for each image dataset.
     */
    private void createSubmissionFiles() {

        final File outputDirFile = new File(outputDirBase);
        if ( !outputDirFile.exists()) {
            outputDirFile.mkdirs();
        }

        final int numDataStructs = structTableModel.getRowCount();

        // for each data structure chosen by the user, create a place to put
        // rows of csv data
        csvStructRowData = new Hashtable<String, String>();
        for (int i = 0; i < numDataStructs; i++) {
            final String tableName = (String) structTableModel.getValueAt(i, 0);
            // format: "structname_PREFIXGUID"
            final String lowerName = getStructFromString(tableName).toLowerCase();
            if ( !csvStructRowData.containsKey(lowerName)) {
                DataStructure dsInfo = null;
                for (final FormStructureData fs : fsDataList) {
                    if (fs.getStructInfo().getShortName().toLowerCase().equals(lowerName)) {
                        dsInfo = fs.getStructInfo();
                        break;
                    }
                }

                String n = lowerName;

                final char c1 = n.charAt(n.length() - 1);
                if (Character.isDigit(c1)) {
                    n = n.substring(0, n.length() - 1);
                }
                final char c2 = n.charAt(n.length() - 1);
                if (Character.isDigit(c2)) {
                    n = n.substring(0, n.length() - 1);
                }

                // # commas at end = # fields in struct - 2 (for name & version) + 1 (record column)
                String cStr = "";
                int numFields = 0;
                for (final RepeatableGroup g : dsInfo.getRepeatableGroups()) {
                    numFields += g.getSize();
                }
                for (int j = 0; j < numFields - 1; j++) {
                    cStr += CSV_OUTPUT_DELIM;
                }

                // make sure the ordering respects the proper positions of the groups/elements
                final ArrayList<RepeatableGroup> orderedGroupList = new ArrayList(dsInfo.getRepeatableGroups());
                Collections.sort(orderedGroupList, groupCompare);

                final ArrayList<ArrayList<MapElement>> orderedElementListsByGroup = new ArrayList<ArrayList<MapElement>>();
                for (final RepeatableGroup g : orderedGroupList) {
                    final ArrayList<MapElement> elemList = new ArrayList(g.getDataElements());
                    Collections.sort(elemList, mapElementCompare);
                    orderedElementListsByGroup.add(elemList);
                }

                String elementHeader = recordIndicatorColumn;
                for (int groupNum = 0; groupNum < orderedElementListsByGroup.size(); groupNum++) {
                    final RepeatableGroup g = orderedGroupList.get(groupNum);
                    final ArrayList<MapElement> deList = orderedElementListsByGroup.get(groupNum);
                    for (int deNum = 0; deNum < deList.size(); deNum++) {
                        elementHeader += CSV_OUTPUT_DELIM + g.getName() + "." + deList.get(deNum).getName();
                    }
                }

                String structHeader = dsInfo.getShortName() + CSV_OUTPUT_DELIM + dsInfo.getVersion() + cStr + "\n";
                structHeader += elementHeader + "\n";
                csvStructRowData.put(lowerName, structHeader);
            }
        }

        for (final String lowerName : csvStructRowData.keySet()) {
            System.out.println("**** " + lowerName);
            System.out.println(csvStructRowData.get(lowerName));
        }

        for (int i = 0; i < numDataStructs; i++) {
            int collisionCounter = 1;
            final String name = (String) structTableModel.getValueAt(i, 0);

            final String guid = getGuidFromString(name);

            final File imageFile = imageFiles.get(i);
            String outputFileNameBase;

            if (imageFile != null) {

                // this means we are working with the image datastructure
                printlnToLog("Creating submission file for " + name);

                // TODO: would be best to only open the image once to pull out header info, get file list, and generate
                // thumbnail
                printlnToLog("Opening: " + imageFile + ", multifile: " + multifiles.get(i));

                final FileIO fileIO = new FileIO();
                fileIO.setQuiet(true);
                final ModelImage origImage = fileIO.readImage(imageFile.getName(), imageFile.getParent() + File.separator, multifiles.get(i), null);

                final List<String> origFiles = FileUtility.getFileNameList(origImage);

                final String dsName = getStructFromString(name);

                outputFileNameBase = guid + "_" + dsName + "_" + System.currentTimeMillis();

                final String zipFilePath = outputDirBase + outputFileNameBase + ".zip";

                printlnToLog("Creating thumbnail image:\t" + outputDirBase + outputFileNameBase + ".jpg");

                ModelImage thumbnailImage = createThumbnailImage(origImage);

                final FileWriteOptions opts = new FileWriteOptions(outputFileNameBase + ".jpg", outputDirBase, true);
                writeThumbnailJIMI(thumbnailImage, opts);
                if (thumbnailImage != null) {
                    thumbnailImage.disposeLocal();
                    thumbnailImage = null;
                }

                try {
                    printlnToLog("Creating ZIP file:\t" + zipFilePath);
                    for (final String file : origFiles) {
                        printlnToLog("Adding file to ZIP:\t" + file);
                    }

                    makeZipFile(zipFilePath, origFiles);
                } catch (final IOException ioe) {
                    ioe.printStackTrace();
                    MipavUtil.displayError("Unable to write original image dataset files to ZIP package:\n" + ioe.getMessage());
                    continue;
                }

                // calculate hash of the zip file and then put it into the image
                // file hash code CDE (if it exists in the struct)
                String hashCode = CSV_OUTPUT_DELIM;
                try {
                    hashCode = computeFileHash(zipFilePath);
                } catch (final IOException e) {
                    e.printStackTrace();
                    MipavUtil.displayError("Unable calculate hash code of ZIP file:\n" + e.getMessage());
                    continue;
                }

                final FormStructureData fsData = fsDataList.get(i);
                int maxRepeatNum = 0;
                for (final RepeatableGroup group : fsData.getStructInfo().getRepeatableGroups()) {
                    final int numRepeats = fsData.getNumGroupRepeats(group.getName());
                    if (maxRepeatNum < numRepeats) {
                        maxRepeatNum = numRepeats;
                    }
                }

                for (int curRepeat = 0; curRepeat < maxRepeatNum; curRepeat++) {
                    final String newRow = getCSVDataRow(outputDirBase, outputFileNameBase, fsData, imageFile, origImage, curRepeat, hashCode);
                    if ( !newRow.equals("")) {
                        final String lowerName = dsName.toLowerCase();
                        String data = csvStructRowData.get(lowerName);
                        if (curRepeat == 0) {
                            data += recordIndicatorValue + CSV_OUTPUT_DELIM + newRow + "\n";
                        } else {
                            data += "" + CSV_OUTPUT_DELIM + newRow + "\n";
                        }
                        csvStructRowData.put(lowerName, data);
                    }
                }

                origImage.disposeLocal();

                printlnToLog("");

            } else {

                // this means that this is another data structure besides image

                printlnToLog("Creating submission file for " + name);

                final String dsName = getStructFromString(name);

                outputFileNameBase = guid + "_" + dsName + "_" + System.currentTimeMillis();

                // if the data_structure contains image_file or
                // image_thumbnail_file, just copy them over to submission
                // package

                final FormStructureData fsData = fsDataList.get(i);

                String value;
                File f;
                String csvDir = "";
                String copyFromImageFilePath = "";
                String copyToImageFilePath = "";
                String copyToImageThumbnailPath = "";

                for (final RepeatableGroup group : fsData.getStructInfo().getRepeatableGroups()) {
                    for (final GroupRepeat repeat : fsData.getAllGroupRepeats(group.getName())) {
                        for (final DataElementValue deVal : repeat.getDataElements()) {
                            value = deVal.getValue();
                            if (deVal.getName().equalsIgnoreCase(IMG_FILE_ELEMENT_NAME)) {
                                value = value.replace("\\", File.separator);
                                value = value.replace("/", File.separator);
                                f = new File(value);
                                if ( !f.exists()) {
                                    // must be a relative path based on csv file
                                    if (csvFile != null) {
                                        csvDir = csvFile.getParentFile().getAbsolutePath() + File.separator;
                                    }
                                    f = new File(csvDir + value);
                                    if (f.exists()) {
                                        copyFromImageFilePath = csvDir + value;
                                        copyToImageFilePath = value;
                                    }
                                } else {
                                    copyFromImageFilePath = value;
                                    copyToImageFilePath = value.substring(value.lastIndexOf(File.separator) + 1, value.length());
                                }
                            } else if (deVal.getName().equalsIgnoreCase(IMG_PREVIEW_ELEMENT_NAME)) {
                                value = value.replace("\\", File.separator);
                                value = value.replace("/", File.separator);
                                f = new File(value);
                                if ( !f.exists()) {
                                    // must be a relative path based on csv file
                                    if (csvFile != null) {
                                        csvDir = csvFile.getParentFile().getAbsolutePath() + File.separator;
                                    }
                                    f = new File(csvDir + value);
                                    if (f.exists()) {
                                        copyToImageThumbnailPath = value;
                                    }
                                } else {
                                    copyToImageThumbnailPath = value.substring(value.lastIndexOf(File.separator) + 1, value.length());
                                }
                            }
                        }
                    }
                }

                if (copyToImageFilePath.contains(File.separator)) {
                    // make directories
                    final String dir = outputDirBase + File.separator + copyToImageFilePath.substring(0, copyToImageFilePath.lastIndexOf(File.separator));
                    final File f1 = new File(dir);
                    f1.mkdirs();

                    final File f2 = new File(outputDirBase + File.separator + copyToImageFilePath);

                    try {
                        final InputStream in = new FileInputStream(copyFromImageFilePath);
                        final OutputStream out = new FileOutputStream(f2);

                        final byte[] buf = new byte[1024];
                        int len;
                        while ( (len = in.read(buf)) > 0) {
                            out.write(buf, 0, len);
                        }
                        in.close();
                        out.close();
                    } catch (final Exception e) {
                        e.printStackTrace();
                    }

                } else {
                    // just copy it over without making directories
                    final File f2 = new File(outputDirBase + File.separator + copyToImageFilePath);

                    try {
                        final InputStream in = new FileInputStream(copyFromImageFilePath);
                        final OutputStream out = new FileOutputStream(f2);

                        final byte[] buf = new byte[1024];
                        int len;
                        while ( (len = in.read(buf)) > 0) {
                            out.write(buf, 0, len);
                        }
                        in.close();
                        out.close();
                    } catch (final Exception e) {
                        e.printStackTrace();
                    }
                }

                if (copyToImageThumbnailPath.contains(File.separator)) {
                    // make directories
                    final String dir = outputDirBase + File.separator
                            + copyToImageThumbnailPath.substring(0, copyToImageThumbnailPath.lastIndexOf(File.separator));
                    final File f1 = new File(dir);
                    f1.mkdirs();

                    final File f2 = new File(outputDirBase + File.separator + copyToImageThumbnailPath);

                    try {
                        final InputStream in = new FileInputStream(copyFromImageFilePath);
                        final OutputStream out = new FileOutputStream(f2);

                        final byte[] buf = new byte[1024];
                        int len;
                        while ( (len = in.read(buf)) > 0) {
                            out.write(buf, 0, len);
                        }
                        in.close();
                        out.close();
                    } catch (final Exception e) {
                        e.printStackTrace();
                    }

                } else {
                    // just copy it over without making directories
                    final File f2 = new File(outputDirBase + File.separator + copyToImageThumbnailPath);

                    try {
                        final InputStream in = new FileInputStream(copyFromImageFilePath);
                        final OutputStream out = new FileOutputStream(f2);

                        final byte[] buf = new byte[1024];
                        int len;
                        while ( (len = in.read(buf)) > 0) {
                            out.write(buf, 0, len);
                        }
                        in.close();
                        out.close();
                    } catch (final Exception e) {
                        e.printStackTrace();
                    }
                }

                // copy all other file to this new dir
                final File allOtherFilesFile = new File(outputDirBase + outputFileNameBase);

                final ArrayList<File> files = allOtherFilesAL.get(i);
                if (files != null && files.size() > 0) {

                    if (allOtherFilesFile.mkdir()) {

                        if (files != null && files.size() > 0) {
                            for (int k = 0; k < files.size(); k++) {
                                f = files.get(k);

                                File destFile = new File(outputDirBase + outputFileNameBase + File.separator + f.getName());
                                // check for collision
                                if (destFile.exists()) {
                                    // collision!
                                    String prefix = f.getName().substring(0, f.getName().lastIndexOf("."));
                                    String suffix = f.getName().substring(f.getName().lastIndexOf(".") + 1, f.getName().length());
                                    destFile = new File(outputDirBase + outputFileNameBase + File.separator + prefix + "_" + collisionCounter + "." + suffix);

                                    for (final RepeatableGroup group : fsData.getStructInfo().getRepeatableGroups()) {
                                        for (final GroupRepeat repeat : fsData.getAllGroupRepeats(group.getName())) {
                                            for (final DataElementValue deVal : repeat.getDataElements()) {
                                                value = deVal.getValue();
                                                if (value.equals(f.getAbsolutePath())) {
                                                    prefix = value.substring(0, value.lastIndexOf("."));
                                                    suffix = value.substring(value.lastIndexOf(".") + 1, value.length());
                                                    deVal.setValue(prefix + "_" + collisionCounter + "." + suffix + "_collision");
                                                }
                                            }
                                        }
                                    }

                                    collisionCounter++;
                                }

                                printlnToLog("Copying " + f.getName() + " to " + destFile.getAbsolutePath());

                                try {
                                    final InputStream in = new FileInputStream(f);
                                    final OutputStream out = new FileOutputStream(destFile);

                                    final byte[] buf = new byte[1024];
                                    int len;
                                    while ( (len = in.read(buf)) > 0) {
                                        out.write(buf, 0, len);
                                    }
                                    in.close();
                                    out.close();
                                } catch (final Exception e) {
                                    e.printStackTrace();
                                }
                            }

                        }
                    }
                }

                int maxRepeatNum = 0;
                for (final RepeatableGroup group : fsData.getStructInfo().getRepeatableGroups()) {
                    final int numRepeats = fsData.getNumGroupRepeats(group.getName());
                    if (maxRepeatNum < numRepeats) {
                        maxRepeatNum = numRepeats;
                    }
                }

                for (int curRepeat = 0; curRepeat < maxRepeatNum; curRepeat++) {
                    final String newRow = getCSVDataRow(outputDirBase, outputFileNameBase, fsData, imageFile, null, curRepeat, CSV_OUTPUT_DELIM);
                    if ( !newRow.equals("")) {
                        final String lowerName = dsName.toLowerCase();
                        String data = csvStructRowData.get(lowerName);
                        if (curRepeat == 0) {
                            data += recordIndicatorValue + CSV_OUTPUT_DELIM + newRow + "\n";
                        } else {
                            data += "" + CSV_OUTPUT_DELIM + newRow + "\n";
                        }
                        csvStructRowData.put(lowerName, data);
                    }
                }

                printlnToLog("");
            }
        }

        // write out the built up CSV data for each struct
        FileWriter fw = null;
        BufferedWriter bw = null;
        try {
            for (final String lowerName : csvStructRowData.keySet()) {
                final String csvFileName = lowerName + "_output_" + System.currentTimeMillis() + ".csv";

                printlnToLog("Writing " + lowerName + " to CSV file: " + outputDirBase + csvFileName);

                final File csvFile = new File(outputDirBase + csvFileName);
                fw = new FileWriter(csvFile);
                bw = new BufferedWriter(fw);

                bw.write(csvStructRowData.get(lowerName));

                // System.out.println(csvStructRowData.get(lowerName) +
                // "  ||||  ");

                bw.close();
            }

            printlnToLog("");
        } catch (final IOException ioe) {
            ioe.printStackTrace();
            printlnToLog("Unable to write CSV output file(s).");
        } finally {
            try {
                if (bw != null) {
                    bw.close();
                }
            } catch (final Exception e) {
                // Do nothing
            }
        }

        printlnToLog("*** Submission pre-processing complete. ***");
        printlnToLog("*** Output files have been generated in directory " + outputDirBase + " ***");
        printlnToLog("*** To submit to BRICS, run the BRICS Validation Tool to package the files for submission. ***");

        // need to delete all tempDirs that were created
        if (tempDirs.size() > 0) {
            for (int i = 0; i < tempDirs.size(); i++) {
                final String dir = tempDirs.get(i);
                final File f = new File(dir);
                if (f.exists()) {
                    final String[] list = f.list();
                    if (list != null) {
                        for (int k = 0; k < list.length; k++) {
                            final File entry = new File(f, list[k]);
                            entry.delete();
                        }
                    }
                    f.delete();
                }
            }
        }

        isFinished = true;
    }

    /**
     * writes out csv file
     * 
     * @param outputDirBase
     * @param outputFileNameBase
     * @param imageFile
     * @param origImage
     */
    private final String getCSVDataRow(final String outputDirBase, final String outputFileNameBase, final FormStructureData fsData, final File imageFile,
            final ModelImage origImage, final int repeatNum, final String hashCode) {
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
                    String value = "";
                    if (imageFile != null) {
                        if (deName.equalsIgnoreCase(IMG_FILE_ELEMENT_NAME)) {
                            value = outputFileNameBase + ".zip";
                        } else if (deName.equalsIgnoreCase(IMG_PREVIEW_ELEMENT_NAME)) {
                            value = outputFileNameBase + ".jpg";
                        } else if (deName.equalsIgnoreCase(IMG_HASH_CODE_ELEMENT_NAME)) {
                            value = hashCode;
                        } else {
                            value = deVal.getValue();
                        }
                    } else {
                        value = deVal.getValue();
                    }

                    final File f = new File(value);
                    if (f.isFile() || value.endsWith("_collision")) {
                        if (value.endsWith("_collision")) {
                            value = value.substring(0, value.indexOf("_collision"));
                            final String filename = value.substring(value.lastIndexOf(File.separator) + 1, value.length());
                            value = outputFileNameBase + File.separator + filename;
                        } else {
                            final String filename = f.getName();
                            value = outputFileNameBase + File.separator + filename;
                        }
                    }

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
     * Adds a set of files to a ZIP archive.
     * 
     * @param destZipFile The full path to the ZIP archive to create.
     * @param srcFiles A list of files (full paths) to include in the ZIP archive.
     * @throws IOException If there is a problem reading the srcFiles or writing to the ZIP file.
     */
    private void makeZipFile(final String destZipFile, final List<String> srcFiles) throws IOException {
        // Create a buffer for reading the files
        final byte[] buf = new byte[1024];

        // Create the ZIP file
        final ZipOutputStream out = new ZipOutputStream(new FileOutputStream(destZipFile));

        // Compress the files
        for (final String file : srcFiles) {
            final FileInputStream in = new FileInputStream(file);

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
     * Calculates the SHA-256 hash of a file.
     * 
     * @param filePath The full path of the file to calculate the hash of.
     * @return The SHA-256 digest of the given file, or an empty string if the SHA-256 algorithm was not found.
     * @throws IOException If there is a problem reading the file specified.
     */
    private String computeFileHash(final String filePath) throws IOException {
        String hashCode;
        try {
            final MessageDigest shaDigest = MessageDigest.getInstance("SHA-256");

            final RandomAccessFile file = new RandomAccessFile(filePath, "r");

            // 64k buffer
            final int buffSize = 65536;

            final byte[] buffer = new byte[buffSize];

            long read = 0;
            final long offset = file.length();
            int size;
            while (read < offset) {
                size = (int) ( ( (offset - read) >= buffSize) ? buffSize : (offset - read));
                file.read(buffer, 0, size);
                shaDigest.update(buffer, 0, size);
                read += size;
            }
            file.close();

            hashCode = new String(Hex.encode(shaDigest.digest()));

            return hashCode;
        } catch (final NoSuchAlgorithmException e) {
            e.printStackTrace();
            MipavUtil.displayError("Unable to generate file hash: SHA-256 algorithm not found.");
            return new String();
        }
    }

    /**
     * Append a line to the log output area in the Log tab.
     * 
     * @param line The line to append (do not include the trailing newline).
     */
    private void printlnToLog(final String line) {
        logOutputArea.getTextArea().append(line + "\n");
    }

    private JPanel buildButtonPanel() {

        final JPanel buttonPanel1 = new JPanel(new GridBagLayout());

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;

        addStructButton = new JButton("Add Form Structure");
        addStructButton.setToolTipText("Add Form Structure");
        addStructButton.addActionListener(this);
        addStructButton.setActionCommand("AddStruct");

        loadCSVButton = new JButton("Load CSV File");
        loadCSVButton.setToolTipText("Load CSV File");
        loadCSVButton.addActionListener(this);
        loadCSVButton.setActionCommand("LoadCSV");

        removeStructButton = new JButton("Remove Form Structure");
        removeStructButton.setToolTipText("Remove the selected Form Structure");
        removeStructButton.addActionListener(this);
        removeStructButton.setActionCommand("RemoveStruct");

        finishButton = new JButton("Generate Files");
        finishButton.setToolTipText("Generate Files");
        finishButton.addActionListener(this);
        finishButton.setActionCommand("Finish");

        editDataElementsButton = new JButton("Edit Data Elements");
        editDataElementsButton.setToolTipText("Edit data elements for selected Form Structure");
        editDataElementsButton.addActionListener(this);
        editDataElementsButton.setActionCommand("EditDataElements");

        addStructButton.setPreferredSize(MipavUtil.defaultButtonSize);
        removeStructButton.setPreferredSize(MipavUtil.defaultButtonSize);
        finishButton.setPreferredSize(MipavUtil.defaultButtonSize);
        editDataElementsButton.setPreferredSize(MipavUtil.defaultButtonSize);

        addStructButton.setEnabled(false);
        loadCSVButton.setEnabled(false);
        removeStructButton.setEnabled(false);
        editDataElementsButton.setEnabled(false);
        finishButton.setEnabled(false);

        gbc.gridx = 0;
        buttonPanel1.add(loadCSVButton, gbc);
        gbc.gridx = 1;
        buttonPanel1.add(addStructButton, gbc);
        gbc.gridx = 2;
        buttonPanel1.add(removeStructButton, gbc);
        gbc.gridx = 3;
        buttonPanel1.add(editDataElementsButton, gbc);
        gbc.gridx = 4;
        buttonPanel1.add(finishButton, gbc);

        return buttonPanel1;
    }

    @Override
    public Dimension getPanelSize() {
        return new Dimension(previewImgPanel.getBounds().width, previewImgPanel.getBounds().height);
    }

    /**
     * Writes a JIMI file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeThumbnailJIMI(final ModelImage image, final FileWriteOptions options) {
        final int imageSize = image.getExtents()[0] * image.getExtents()[1];
        final int[] paintBuffer = new int[imageSize];
        final ColorRGBA colorMappedA = new ColorRGBA();
        final float[] imageBufferA = new float[image.getExtents()[0] * image.getExtents()[1] * 4];
        final int length = imageBufferA.length;
        final ColorRGB[] m_akOffset = {new ColorRGB(0.0f, 0.0f, 0.0f), new ColorRGB(0.0f, 0.0f, 0.0f)};
        float fMaxColor = 255;
        final float[] m_afNormColor = {1, 1};

        if (image.getMinR() < 0.0) {
            fMaxColor = (float) (image.getMaxR() - image.getMinR());
            m_akOffset[0].R = (float) ( -image.getMinR());
        } else {
            fMaxColor = (float) image.getMaxR();
        }

        if (image.getMinG() < 0.0) {
            fMaxColor = Math.max((float) (image.getMaxG() - image.getMinG()), fMaxColor);
            m_akOffset[0].G = (float) ( -image.getMinG());
        } else {
            fMaxColor = Math.max((float) image.getMaxG(), fMaxColor);
        }

        if (image.getMinB() < 0.0) {
            fMaxColor = Math.max((float) (image.getMaxB() - image.getMinB()), fMaxColor);
            m_akOffset[0].B = (float) ( -image.getMinB());
        } else {
            fMaxColor = Math.max((float) image.getMaxB(), fMaxColor);
        }
        m_afNormColor[0] = 255 / fMaxColor;

        try {
            image.exportData(0, length, imageBufferA);
        } catch (final Exception e) {

        }
        for (int j = 0; j < image.getExtents()[1]; j++) {

            for (int i = 0; i < image.getExtents()[0]; i++) {
                final int ind4 = (j * image.getExtents()[0]) + i;
                final int index = 4 * ind4;
                int pixValue;

                colorMappedA.R = 0;
                colorMappedA.G = 0;
                colorMappedA.B = 0;
                colorMappedA.A = imageBufferA[index];

                colorMappedA.R = (imageBufferA[index + 1] + m_akOffset[0].R) * m_afNormColor[0];
                colorMappedA.G = (imageBufferA[index + 2] + m_akOffset[0].G) * m_afNormColor[0];
                colorMappedA.B = (imageBufferA[index + 3] + m_akOffset[0].B) * m_afNormColor[0];

                pixValue = 0xff000000 | ((int) (colorMappedA.R) << 16) | ((int) (colorMappedA.G) << 8) | ((int) (colorMappedA.B));

                paintBuffer[ind4] = pixValue;
            }
        }

        final MemoryImageSource memImageA = new MemoryImageSource(image.getExtents()[0], image.getExtents()[1], paintBuffer, 0, image.getExtents()[0]);

        final int extIndex = options.getFileName().indexOf(".");
        final String prefix = options.getFileName().substring(0, extIndex); // Used
                                                                            // for
                                                                            // setting
                                                                            // file
                                                                            // name
        final String fileSuffix = options.getFileName().substring(extIndex);
        String name;

        final Image img = createImage(memImageA);

        name = options.getFileDirectory() + prefix + fileSuffix;

        try {
            Jimi.putImage(img, name);
        } catch (final JimiException jimiException) {
            Preferences.debug("JIMI write error: " + jimiException + "\n", Preferences.DEBUG_FILEIO);

            jimiException.printStackTrace();

            return false;
        }

        return true;
    }

    @Override
    public void mouseClicked(final MouseEvent e) {
        final Component c = e.getComponent();
        if (c instanceof JTable) {
            if (structTable.getSelectedRow() == -1) {
                editDataElementsButton.setEnabled(false);
                removeStructButton.setEnabled(false);
                return;
            } else {
                if ( !isFinished) {
                    editDataElementsButton.setEnabled(true);
                    removeStructButton.setEnabled(true);
                }
            }

            previewImgPanel.removeAll();
            previewImgPanel.repaint();

            if (previewImages.get(structTable.getSelectedRow()) != null) {
                previewImgPanel.add(previewImages.get(structTable.getSelectedRow()));
                previewImages.get(structTable.getSelectedRow()).setSliceBrightness(previewImgBrightness, previewImgContrast);
                previewImgPanel.validate();
                previewImgPanel.repaint();
            }

            if (e.getClickCount() == 2) {
                if ( !isFinished) {
                    final String dsName = (String) structTableModel.getValueAt(structTable.getSelectedRow(), 0);
                    new InfoDialog(this, dsName, true, true, null);
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

        for (int i = 0; i < structTableModel.getRowCount(); i++) {
            final File f1 = (File) structTableModel.getValueAt(i, 0);
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

        if (structTableModel.getRowCount() == 0) {
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

    private JPanel buildBrightnessContrastPanel() {
        final int defaultBrightness = 0;
        final int defaultContrast = 1;

        previewImgBrightnessSlider = new JSlider(SwingConstants.HORIZONTAL, -255, 255, defaultBrightness);

        previewImgBrightnessSlider.setMajorTickSpacing(102);
        previewImgBrightnessSlider.setPaintTicks(true);
        previewImgBrightnessSlider.setEnabled(true);
        previewImgBrightnessSlider.addChangeListener(this);

        final JLabel maximum = new JLabel(String.valueOf(255));

        maximum.setForeground(Color.black);
        maximum.setFont(serif12);

        previewImgBrightnessLabel = new JLabel(String.valueOf(defaultBrightness));
        previewImgBrightnessLabel.setForeground(Color.black);
        previewImgBrightnessLabel.setFont(serif12B);

        final JLabel minimum = new JLabel(String.valueOf( -255));

        minimum.setForeground(Color.black);
        minimum.setFont(serif12);

        final JPanel sliderPanel = new JPanel(new GridBagLayout());
        final GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel.add(previewImgBrightnessSlider, gbc);

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

        sliderPanel.add(previewImgBrightnessLabel, gbc);

        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;

        sliderPanel.add(maximum, gbc);
        sliderPanel.setBorder(JDialogBase.buildTitledBorder("Level"));

        previewImgContrastSlider = new JSlider(SwingConstants.HORIZONTAL, -200, 200, (int) (Math.round(86.85889638 * Math.log(defaultContrast))));

        previewImgContrastSlider.setMajorTickSpacing(80);
        previewImgContrastSlider.setPaintTicks(true);
        previewImgContrastSlider.setEnabled(true);
        previewImgContrastSlider.addChangeListener(this);

        final JLabel maximum2 = new JLabel(String.valueOf(10));

        maximum2.setForeground(Color.black);
        maximum2.setFont(serif12);

        previewImgContrastLabel = new JLabel(String.valueOf(NumberFormat.getNumberInstance().format(defaultContrast)));
        previewImgContrastLabel.setForeground(Color.black);
        previewImgContrastLabel.setFont(serif12B);

        final JLabel minimum2 = new JLabel(String.valueOf(0.100));

        minimum2.setForeground(Color.black);
        minimum2.setFont(serif12);

        final JPanel sliderPanel2 = new JPanel(new GridBagLayout());

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel2.add(previewImgContrastSlider, gbc);

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

        sliderPanel2.add(previewImgContrastLabel, gbc);

        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;

        sliderPanel2.add(maximum2, gbc);
        sliderPanel2.setBorder(JDialogBase.buildTitledBorder("Window"));

        final JPanel centerPanel = new JPanel(new GridBagLayout());

        final GridBagConstraints gbc2 = new GridBagConstraints();

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

        final JPanel brightnessContrastPanel = new JPanel(new BorderLayout());
        brightnessContrastPanel.add(centerPanel);
        brightnessContrastPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        return brightnessContrastPanel;
    }

    /**
     * Tries to read server configuration from brics config file on local disk.
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

    /**
     * Checks if the given string starts with an allowed GUID prefix.
     * 
     * @param str A string to check.
     * @return True if the string starts with one of the BIRCS prefixes (case sensitive).
     */
    private static final boolean isGuid(final String str) {
        final String pattern = "^[\\w]+$";
        final Pattern p = Pattern.compile(pattern);
        final Matcher m = p.matcher(str.trim());
        if ( !m.matches()) {
            return false;
        }

        for (final String prefix : allowedGuidPrefixes) {
            if (str.startsWith(prefix)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Checks to see if the given string contains an allowed GUID prefix.
     * 
     * @param str A string to check.
     * @return True if one of the allowed BRICS prefixes is found anywhere in the string (case sensitive).
     */
    private static final boolean containsGuid(final String str) {
        for (final String prefix : allowedGuidPrefixes) {
            if (str.contains("_" + prefix)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Extracts the form structure name from a string in the format 'structname_BRICSGUID'.
     * 
     * @param str A string in the format 'structname_BRICSGUID'.
     * @return The form structure name from the given string or null if it could not be found (if no GUID prefix was
     *         found to initiate the parsing).
     */
    private static final String getStructFromString(final String str) {
        for (final String prefix : allowedGuidPrefixes) {
            if (str.contains("_" + prefix)) {
                return str.substring(0, str.indexOf("_" + prefix));
            }
        }

        return null;
    }

    /**
     * Extracts the GUID from a string in the format 'structname_BRICSGUID'.
     * 
     * @param str A string in the format 'structname_BRICSGUID'.
     * @return The GUID from the given string or null if it could not be found (if no GUID prefix was found to initiate
     *         the parsing).
     */
    private static final String getGuidFromString(final String str) {
        for (final String prefix : allowedGuidPrefixes) {
            if (str.contains("_" + prefix)) {
                return str.substring(str.indexOf("_" + prefix) + 1, str.length());
            }
        }

        return null;
    }

    /**
     * Converts from DICOM/US date format (MM/DD/YYYY) or M/D/YYYY to ISO 8601 format (YYYY-MM-DDThh:mm:ss).
     * 
     * @param date A date string in the format MM/DD/YYYY or M/D/YYYY.
     * @return An ISO 8601 formatted version of the given date (or the original string if not in the DICOM/US date
     *         format).
     */
    private static final String convertDateToISOFormat(final String date) {
        final String pattern = "^(\\d{1,2})[/-]*(\\d{1,2})[/-]*(\\d{4})$";
        final Pattern p = Pattern.compile(pattern);
        final Matcher m = p.matcher(date);
        if (m.find()) {
            String month = m.group(1);
            String day = m.group(2);
            final String year = m.group(3);
            // add leading zeroes, if necessary
            if (month.length() == 1) {
                month = "0" + month;
            }
            if (day.length() == 1) {
                day = "0" + day;
            }
            return year + "-" + month + "-" + day;
        }

        return date;
    }

    /**
     * Converts from DICOM/US date and time format (MM/DD/YYYY) or M/D/YYYY to ISO 8601 format (YYYY-MM-DDThh:mm:ss).
     * 
     * @param date A date string in the format MM/DD/YYYY or M/D/YYYY.
     * @param time A time string in the format hh:mm:ss.fract.
     * @return An ISO 8601 formatted version of the given date and time (or the original string if not in the DICOM/US
     *         date format).
     */
    private static final String convertDateTimeToISOFormat(final String date, final String time) {
        String isoDate = date.trim();
        String isoTime = time.trim();

        final String datePattern = "^(\\d{1,2})[/-]*(\\d{1,2})[/-]*(\\d{4})$";
        final String timePattern = "^(\\d{1,2}):(\\d{1,2}):(\\d{1,2})\\.\\d+$";

        Pattern p = Pattern.compile(datePattern);
        Matcher m = p.matcher(isoDate);
        if (m.find()) {
            String month = m.group(1);
            String day = m.group(2);
            final String year = m.group(3);
            // add leading zeroes, if necessary
            if (month.length() == 1) {
                month = "0" + month;
            }
            if (day.length() == 1) {
                day = "0" + day;
            }
            isoDate = year + "-" + month + "-" + day;
        }

        p = Pattern.compile(timePattern);
        m = p.matcher(isoTime);
        if (m.find()) {
            String hour = m.group(1);
            String min = m.group(2);
            String sec = m.group(3);
            // add leading zeroes, if necessary
            if (hour.length() == 1) {
                hour = "0" + hour;
            }
            if (min.length() == 1) {
                min = "0" + min;
            }
            if (sec.length() == 1) {
                sec = "0" + sec;
            }
            isoTime = hour + ":" + min + ":" + sec;
        }

        if (isoTime.equals("")) {
            return isoDate;
        } else {
            return isoDate + "T" + isoTime;
        }
    }

    /**
     * Tries to convert a MIPAV/DICOM modality string to the equivalent BRICS CDE value. Still needs a good bit of
     * work/addtions/integration with MR sequence type.
     * 
     * @param mipavModality The MIPAV modality description string.
     * @return The BRICS ImgModltyTyp CDE value, or an empty string if no matching modality was found.
     */
    private static final String convertModalityToBRICS(final String mipavModality) {
        if (mipavModality.equalsIgnoreCase("Unknown Modality")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Biomagnetic Imaging")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Color Flow Doppler")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Computed Radiography")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Computed Tomography")) {
            return "CT";
        } else if (mipavModality.equalsIgnoreCase("Duplex Doppler")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Diaphanography")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Digital Radiography")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Endoscopy")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("General Microscopy")) {
            return "Microscopy";
        } else if (mipavModality.equalsIgnoreCase("Intraoral Radiography")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Laser Surface Scan")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Magnetic Resonance Angiography")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Mammography")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Magnetic Resonance")) {
            return "MRI";
        } else if (mipavModality.equalsIgnoreCase("Magnetic Resonance Spectroscopy")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Nuclear Medicine")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Other")) {
            return VALUE_OTHER_SPECIFY;
        } else if (mipavModality.equalsIgnoreCase("Positron Emission Tomography")) {
            return "PET";
        } else if (mipavModality.equalsIgnoreCase("Panoramic XRay")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Radio Fluoroscopy")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Radiographic Imaging")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Radiotherapy Dose")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Radiotherapy Image")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Radiotherapy Plan")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Radiotherapy Record")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Radiotherapy Structure Set")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Slide Microscopy")) {
            return "Microscopy";
        } else if (mipavModality.equalsIgnoreCase("Single Photon Emission Computed Tomography")) {
            return "SPECT";
        } else if (mipavModality.equalsIgnoreCase("Thermography")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Ultrasound")) {
            return "Ultrasound";
        } else if (mipavModality.equalsIgnoreCase("XRay Angiography")) {
            return "X-Ray Angiography";
        } else if (mipavModality.equalsIgnoreCase("External Camera Photography")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Red Free")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("FA")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("ICG")) {
            return "";
        }

        return "";
    }

    /**
     * Converts a magnetic field strength number to the format used by BRICS (for example 3.0T or 1.5T).
     * 
     * @param magField A magnetic field strength number string.
     * @return The magnetic field strength with T appended, and .0 if it was an integer value originally.
     */
    private static final String convertMagFieldStrengthToBRICS(final String magField) {
        if (magField != null && !magField.equals("")) {
            final float magFieldNum = Float.parseFloat(magField);
            final NumberFormat nf = NumberFormat.getNumberInstance();
            nf.setMinimumFractionDigits(1);
            nf.setMaximumFractionDigits(1);
            return nf.format(magFieldNum) + "T";
        }

        return "";
    }

    /**
     * Attempts to convert from the DICOM manufacturer name to BRICS scanner manufacturer permissible values. If nothing
     * matches, return the original name.
     * 
     * @param manuf The DICOM manufacturer name.
     * @return The DICOM manufacturer name to BRICS scanner manufacturer permissible values. If nothing matches, return
     *         the original name.
     */
    private static final String convertManufNameToBRICS(final String manuf) {
        final String upperManuf = manuf.toUpperCase();
        if (upperManuf.startsWith("AGFA")) {
            return "Agfa";
        } else if (upperManuf.startsWith("CARESTREAM")) {
            return "Carestream";
        } else if (upperManuf.startsWith("GE")) {
            return "GE";
        } else if (upperManuf.startsWith("HITACHI")) {
            return "Hitachi";
        } else if (upperManuf.startsWith("HOLOGIC")) {
            return "Hologic";
        } else if (upperManuf.startsWith("KONICA")) {
            return "Konica Minolta";
        } else if (upperManuf.startsWith("PHILIPS")) {
            return "Philips";
        } else if (upperManuf.startsWith("SIEMENS")) {
            return "Siemens";
        } else if (upperManuf.startsWith("TOSHIBA")) {
            return "Toshiba";
        }

        return manuf;
    }

    /**
     * Attempts to convert from the DICOM model name to BRICS scanner model permissible values. If nothing matches,
     * return the original name.
     * 
     * @param model The DICOM model name.
     * @return The DICOM model name to BRICS scanner model permissible values. If nothing matches, return the original
     *         name.
     */
    private static final String convertModelNameToBRICS(final String model) {
        final String upperModel = model.toUpperCase();
        if (upperModel.contains("ACHIEVA")) {
            return "Achieva";
        } else if (upperModel.contains("SIGNA")) {
            return "Signa";
        } else if (upperModel.contains("SYMPHONY")) {
            return "Symphony";
        } else if (upperModel.contains("TRIO")) {
            return "Trio";
        }

        return model;
    }

    /**
     * Attempts to convert from the Nifti description field (hopefully [MANUF] [MODEL] [VERSION]) to BRICS scanner model
     * permissible values. If nothing matches, return an empty string.
     * 
     * @param description The Nifti description string (hopefully in the format [MANUF] [MODEL] [VERSION]).
     * @return The scanner model name. If nothing matches, return an empty string.
     */
    private static final String convertNiftiDescToBRICSManuf(final String description) {
        final String upperDescription = description.toUpperCase();
        if (upperDescription.startsWith("AGFA")) {
            return "Agfa";
        } else if (upperDescription.startsWith("CARESTREAM")) {
            return "Carestream";
        } else if (upperDescription.startsWith("GE")) {
            return "GE";
        } else if (upperDescription.startsWith("HITACHI")) {
            return "Hitachi";
        } else if (upperDescription.startsWith("HOLOGIC")) {
            return "Hologic";
        } else if (upperDescription.startsWith("KONICA")) {
            return "Konica Minolta";
        } else if (upperDescription.startsWith("PHILIPS")) {
            return "Philips";
        } else if (upperDescription.startsWith("SIEMENS")) {
            return "Siemens";
        } else if (upperDescription.startsWith("TOSHIBA")) {
            return "Toshiba";
        }

        return "";
    }

    /**
     * Attempts to convert from the Nifti description field (hopefully [MANUF] [MODEL] [VERSION]) to BRICS scanner
     * scanner version number. If nothing matches, return an empty string.
     * 
     * @param description The Nifti description string (hopefully in the format [MANUF] [MODEL] [VERSION]).
     * @return The scanner version number. If nothing matches, return an empty string.
     */
    private static final String convertNiftiDescToBRICSVer(final String description) {
        final String pattern = "\\s+([.-_\\d]+)$";
        final Pattern p = Pattern.compile(pattern);
        final Matcher m = p.matcher(description.trim());
        if (m.find()) {
            return m.group(1);
        }

        return "";
    }

    /**
     * Attempts to convert from the Nifti description field (hopefully [MANUF] [MODEL] [VERSION]) to BRICS scanner model
     * permissible values. If nothing matches, return an empty string.
     * 
     * @param description The Nifti description string (hopefully in the format [MANUF] [MODEL] [VERSION]).
     * @return The model name. If nothing matches, return an empty string.
     */
    private static final String convertNiftiDescToBRICSModel(final String description) {
        final String upperDescription = description.toUpperCase();
        if (upperDescription.contains("ACHIEVA")) {
            return "Achieva";
        } else if (upperDescription.contains("SIGNA")) {
            return "Signa";
        } else if (upperDescription.contains("SYMPHONY")) {
            return "Symphony";
        } else if (upperDescription.contains("TRIO")) {
            return "Trio";
        }

        return "";
    }

    /**
     * Attempts to convert from DICOM patient age tag (in format xxx[DWMY]) to the BRICS AgeVal (in months). Day and
     * week to month conversions are approximations.
     * 
     * @param dicomAge The DICOM patient age tag value (hopefully in the format xxx[DWMY]).
     * @return The patient age in months, as close as possible with the DICOM age given. If it does not match the
     *         format, return an empty string.
     */
    private static final String convertDicomAgeToBRICS(final String dicomAge) {
        final String temp = dicomAge.substring(0, dicomAge.length() - 6);

        if (dicomAge.contains("D")) {
            return Double.toString(Integer.parseInt(temp) / 30.4166666667);
        } else if (dicomAge.contains("W")) {
            return Double.toString(Integer.parseInt(temp) / 4.34523809524);
        } else if (dicomAge.contains("M")) {
            return temp;
        } else if (dicomAge.contains("Y")) {
            return Double.toString(Integer.parseInt(temp) * 12);
        }

        return "";
    }

    private static final void setElementComponentValue(final DataElementValue deVal, final String value) {
        if (value != null && !value.equals("")) {
            final JComponent comp = deVal.getComp();
            if (comp instanceof JTextField) {
                ((JTextField) comp).setText(value);
            } else if (comp instanceof JComboBox) {
                boolean found = false;
                boolean foundOtherInDE = false;
                final JComboBox jc = (JComboBox) comp;
                for (int k = 0; k < jc.getItemCount(); k++) {
                    final String item = (String) jc.getItemAt(k);
                    if (item.equalsIgnoreCase(VALUE_OTHER_SPECIFY)) {
                        foundOtherInDE = true;
                    }

                    if (item.equalsIgnoreCase(value)) {
                        jc.setSelectedIndex(k);
                        found = true;
                    }
                }
                if ( !found) {
                    if (foundOtherInDE) {
                        jc.setSelectedItem(VALUE_OTHER_SPECIFY);
                        deVal.getOtherSpecifyField().setText(value);
                    } else {
                        System.err.println("Value not found. DE:\t" + comp.getName() + "\t" + value);
                    }
                }
            } else {
                System.err.println("Unrecognized component type (" + comp.getName() + "):\t" + comp.getClass().getName());
            }
        }
    }

    private static final boolean isOtherSpecifyField(final JComponent comp) {
        if (comp instanceof JComboBox) {
            final JComboBox combo = (JComboBox) comp;
            for (int i = 0; i < combo.getItemCount(); i++) {
                if ( ((String) combo.getItemAt(i)).trim().equalsIgnoreCase(VALUE_OTHER_SPECIFY)) {
                    return true;
                }
            }
        }

        return false;
    }

    private static final boolean checkElementDescrepancy(final String fieldName, String csvValue, String headerValue) {
        if (csvValue != null) {
            csvValue = csvValue.trim();
        }

        if (headerValue != null && !headerValue.trim().equals("")) {
            headerValue = headerValue.trim();

            if (fieldName.equalsIgnoreCase("ImgDimensionTyp")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgDim1ExtentVal")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgDim2ExtentVal")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgDim3ExtentVal")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgDim4ExtentVal")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgDim5ExtentVal")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgDim1UoMVal")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgDim2UoMVal")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgDim3UoMVal")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgDim4UoMVal")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgDim5UoMVal")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgDim1ResolVal")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgDim2ResolVal")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgDim3ResolVal")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgDim4ResolVal")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgDim5ResolVal")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgModltyTyp")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgSliceThicknessVal")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgSliceOrientTyp")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("AgeVal")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("AgeYrs")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("VisitDate")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("SiteName")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgStdyDateTime")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgSliceOverSampVal")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgGapBetwnSlicesMeasr")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgAntmicSite")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgFOVMeasrDescTxt")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgScannerManufName")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgScannerModelName")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgScannerSftwrVrsnNum")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgHeadPostnTxt")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgBandwidthVal")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgScannerStrgthVal")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgMRIT1T2SeqName")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgRepetitionGapVal")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgEchoDur")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgFlipAngleMeasr")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgInversionTime")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgEchoTrainLngthMeasr")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgPhasEncdeDirctTxt")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgSignalAvgNum")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            } else if (fieldName.equalsIgnoreCase("ImgRFCoilName")) {
                if ( !csvValue.equals(headerValue)) {
                    return true;
                }
            }
        }

        return false;
    }

    private static final boolean isMainImagingFileElement(final String groupName, final String deName) {
        if (groupName.equalsIgnoreCase(IMG_IMAGE_INFO_GROUP) && deName.equalsIgnoreCase(IMG_FILE_ELEMENT_NAME)) {
            return true;
        }

        return false;
    }

    /**
     * Returns whether the given structure name indicates that it is an imaging structure that should be processed
     * specially (mainly, this means that the plugin looks for the Imaging file DE to pull out header info).
     * 
     * @param structureName The name of the form structure.
     * @return True if the structure name starts with one of the imagingStructurePrefixes.
     */
    private static final boolean isImagingStructure(final String structureName) {
        for (final String prefix : imagingStructurePrefixes) {
            if (structureName.toLowerCase().startsWith(prefix.toLowerCase())) {
                return true;
            }
        }

        return false;
    }

    /**
     * Returns whether the given structure name indicates that it is a PDBP imaging structure that should be processed
     * specially.
     * 
     * @param structureName The name of the form structure.
     * @return True if the structure name starts with the PDBP imaging prefix.
     */
    private static final boolean isPDBPImagingStructure(final String structureName) {
        if (structureName.toLowerCase().startsWith(PDBP_IMAGING_STRUCTURE_PREFIX.toLowerCase())) {
            return true;
        }

        return false;
    }

    private static final String[] splitFieldString(final String deNameAndGroup) {
        return deNameAndGroup.split("\\.");
    }

    private static final String getFieldGroup(final String deNameAndGroup) {
        return splitFieldString(deNameAndGroup)[0];
    }

    private static final String getFieldName(final String deNameAndGroup) {
        return splitFieldString(deNameAndGroup)[1];
    }

    private static final String removeRedundantDiseaseInfo(final String field) {
        final ArrayList<String> diseaseStrings = new ArrayList<String>();
        final ArrayList<String> fieldStrings = new ArrayList<String>();

        final Matcher m = Pattern.compile("(\\w[\\w\\s()]*):\\s+(.+)\\s+-----\\s+").matcher(field);

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

                final Matcher m2 = Pattern.compile("(\\w[\\w\\s()]*):\\s+(.+)\\s*-*").matcher(field.substring(lastMatchIndex));

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
     * Inner class Right Renderer
     * 
     * @author pandyan
     */
    private class MyRightCellRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = -7905716122046419275L;

        @Override
        public Component getTableCellRendererComponent(final JTable table, final Object value, final boolean isSelected, final boolean hasFocus, final int row,
                final int column) {
            final Component comp = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
            setHorizontalAlignment(SwingConstants.CENTER);

            if (column == 1 && ((String) value).equalsIgnoreCase("No")) {
                setForeground(Color.red);
            } else {
                setForeground(Color.black);
            }

            return comp;
        }

    }

    private class ChooseDataStructDialog extends JDialog implements ActionListener {
        private static final long serialVersionUID = 4199199899439094828L;

        private final PlugInDialogFITBIR owner;

        private ViewTableModel structsModel;

        private JTable structsTable;

        private final ArrayList<String> descAL = new ArrayList<String>();

        private final ArrayList<String> shortNameAL = new ArrayList<String>();

        private final ArrayList<String> versionAL = new ArrayList<String>();

        private final ArrayList<String> statusAL = new ArrayList<String>();

        private JScrollPane structsScrollPane;

        public ChooseDataStructDialog(final PlugInDialogFITBIR owner) {
            super(owner, true);

            this.owner = owner;

            init();

        }

        private void init() {
            setTitle("Choose Form Structure");
            final int numColumns = 4;
            final String[] columnNames = {"Name", "Description", "Version", "Status"};
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

            // new way of doing web service
            for (final DataStructure ds : dataStructureList) {
                if (ds.getShortName().equals("")) {
                    // something is wrong. a shortname is required. this is to work around an apparent stage DDT problem
                    continue;
                }
                final String desc = ds.getDescription();
                final String shortname = ds.getShortName();
                final String version = ds.getVersion().toString();
                final String status = ds.getStatus().toString();

                if (ds.getFileType().equals(SubmissionType.IMAGING)) {
                    // only include non-archived structures
                    if ( !ds.getStatus().equals(StatusType.ARCHIVED)) {
                        descAL.add(desc);
                        shortNameAL.add(shortname);
                        versionAL.add(version);
                        statusAL.add(status);
                    }
                }
            }

            // make sure we found a structure for imaging
            if (shortNameAL.size() == 0) {
                MipavUtil.displayWarning("No Imaging structures were found in the data dictionary.");
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
                        } else {
                            shortNameAL.remove(i);
                            descAL.remove(i);
                            versionAL.remove(i);
                            statusAL.remove(i);
                        }
                    }
                }
            }

            final TreeSet<String> sortedNamesSet = new TreeSet<String>(new AlphabeticalComparator());
            for (int i = 0; i < shortNameAL.size(); i++) {
                sortedNamesSet.add(shortNameAL.get(i));
            }

            // we only want to list the most recent versions
            // so remove the less recent versions from this list
            // String[] sortedNamesArray = (String[]) (sortedNamesSet.toArray(new String[0]));
            // for (int i = sortedNamesArray.length - 1; i > 0; i--) {
            // String name = sortedNamesArray[i];
            // if (!name.equals("")) {
            // // BRICS short name doesn't appear to have version number
            // // included
            // // String nameWithoutVersion = name.substring(0,
            // // name.length() - 2);
            // for (int k = i - 1; k >= 0; k--) {
            // String checkName = sortedNamesArray[k];
            // // BRICS short name doesn't appear to have version
            // // number included
            // // String checkNameWithoutVersion =
            // // checkName.substring(0, checkName.length() - 2);
            // if (name.equals(checkName)) {
            // sortedNamesArray[k] = "";
            // }
            // }
            // }
            // }
            // sortedNamesSet = new TreeSet<String>(new AlphabeticalComparator());
            // for (int i = 0; i < sortedNamesArray.length; i++) {
            // if (!sortedNamesArray[i].equals("")) {
            // sortedNamesSet.add(sortedNamesArray[i]);
            // }
            // }
            // now we only have the most recent versions

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
                        structsModel.addRow(rowData);

                        break;
                    }
                }
            }

            structsTable.setRowSelectionInterval(0, 0);

            // structsTable.setAutoCreateRowSorter(true);
            // structsTable.getRowSorter().toggleSortOrder(0);

            structsScrollPane = new JScrollPane(structsTable, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

            structsScrollPane.setPreferredSize(new Dimension(600, 300));

            final JPanel OKPanel = new JPanel();
            final JButton OKButton = new JButton("Add");
            OKButton.setActionCommand("ChooseStructOK");
            OKButton.addActionListener(this);
            OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
            OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
            OKButton.setFont(serif12B);

            OKPanel.add(OKButton);

            getContentPane().add(structsScrollPane, BorderLayout.CENTER);

            getContentPane().add(OKPanel, BorderLayout.SOUTH);

            pack();

            MipavUtil.centerInWindow(owner, this);
            this.setMinimumSize(this.getSize());

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
                    final String dsName = (String) structsModel.getValueAt(selectedRow, 0);
                    new InfoDialog(owner, dsName, false, true, null);
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
     * launches the dialog to add info
     * 
     * @author pandyan
     * 
     */
    private class InfoDialog extends JDialog implements ActionListener, WindowListener, ItemListener, FocusListener {
        private static final long serialVersionUID = 859201819000159789L;

        private final PlugInDialogFITBIR owner;

        private JPanel dsMainPanel;

        private final Hashtable<RepeatableGroup, JPanel> groupPanelTable = new Hashtable<RepeatableGroup, JPanel>();

        private final Hashtable<RepeatableGroup, JButton> groupRemoveButtonTable = new Hashtable<RepeatableGroup, JButton>();

        private String guid = "";

        private boolean launchedFromInProcessState = false;

        private JLabel requiredLabel;

        private String dataStructureName;

        private FormStructureData fsData;

        private final ArrayList<File> allOtherFiles = new ArrayList<File>();

        private boolean addedPreviewImage = false;

        private DataStructure dataStructure;

        private final boolean setInitialVisible;

        private final ArrayList<ArrayList<String>> record;

        private final String[] unchangableElements = new String[] {IMG_HASH_CODE_ELEMENT_NAME, "ImgDimensionTyp", "ImgDim1ExtentVal", "ImgDim2ExtentVal",
                "ImgDim3ExtentVal", "ImgDim4ExtentVal", "ImgDim5ExtentVal", IMG_FILE_ELEMENT_NAME, IMG_PREVIEW_ELEMENT_NAME};

        private String currFile;

        private boolean validFile;

        public InfoDialog(final PlugInDialogFITBIR owner, final String name, final boolean launchedFromInProcessState, final boolean setInitialVisible,
                final ArrayList<ArrayList<String>> record) {
            super(owner, true);

            this.owner = owner;
            this.launchedFromInProcessState = launchedFromInProcessState;
            this.setInitialVisible = setInitialVisible;
            this.record = record;

            if (launchedFromInProcessState) {
                if (containsGuid(name)) {
                    this.dataStructureName = getStructFromString(name);
                } else {
                    this.dataStructureName = name.substring(0, name.lastIndexOf("_"));
                }
            } else {
                previewImages.add(null);
                imageFiles.add(null);
                multifiles.add(new Boolean(false));
                fsDataList.add(null);
                allOtherFilesAL.add(null);
                this.dataStructureName = name;
            }

            for (final DataStructure ds : dataStructureList) {
                if (ds.getShortName().equalsIgnoreCase(dataStructureName)) {
                    dataStructure = ds;
                }
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
                    final int selectedRow = structTable.getSelectedRow();

                    fsData = fsDataList.get(selectedRow);

                    parseForInitLabelsAndComponents(fsData);
                } else {
                    fsData = new FormStructureData(dataStructure);

                    parseDataStructure(dataStructure, fsData, record);

                    parseForInitLabelsAndComponents(fsData);

                    if ( !setInitialVisible) {
                        // convert any dates found into proper ISO format
                        for (int i = 0; i < csvFieldNames.size(); i++) {
                            final String[] deGroupAndName = splitFieldString(csvFieldNames.get(i));

                            DataElement de = null;
                            for (final GroupRepeat repeat : fsData.getAllGroupRepeats(deGroupAndName[0])) {
                                for (final DataElementValue deVal : repeat.getDataElements()) {
                                    if (deVal.getName().equalsIgnoreCase(deGroupAndName[1])) {
                                        de = deVal.getDataElementInfo();
                                        break;
                                    }
                                }
                            }

                            for (final ArrayList<String> values : record) {
                                // check value not empty and check type of field for date
                                if ( !values.get(i).trim().equals("") && de.getType().equals(DataType.DATE)) {
                                    values.set(i, convertDateToISOFormat(values.get(i)));
                                }
                            }
                        }

                        // this means it was launched via the csv file
                        populateFieldsFromCSV(fsData, record);
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

        private void populateFieldsFromCSV(final FormStructureData fsData, final ArrayList<ArrayList<String>> record) {
            if (isImagingStructure(dataStructureName)) {
                // first check to see if main image file was supplied in the csv
                int imageFileIndex = -1;
                for (int i = 0; i < csvFieldNames.size(); i++) {
                    final String[] deGroupAndName = splitFieldString(csvFieldNames.get(i).trim());

                    if (isMainImagingFileElement(deGroupAndName[0], deGroupAndName[1])) {
                        imageFileIndex = i;
                        break;
                    }
                }

                for (int curRepeatNum = 0; curRepeatNum < record.size(); curRepeatNum++) {
                    final ArrayList<String> repeatValues = record.get(curRepeatNum);

                    ModelImage srcImage = null;

                    // if image file set in this repeat, read in the image
                    if (imageFileIndex != -1 && !repeatValues.get(imageFileIndex).trim().equals("")) {
                        // if image_file is in zip format....first unzip it temporarily
                        final String imageFile = repeatValues.get(imageFileIndex);

                        srcImage = readImgFromCSV(csvFile.getParentFile().getAbsolutePath(), imageFile);

                        if (srcImage != null) {
                            for (final RepeatableGroup group : fsData.getStructInfo().getRepeatableGroups()) {
                                if (group.getName().equalsIgnoreCase(IMG_IMAGE_INFO_GROUP)) {
                                    final GroupRepeat repeat = fsData.getGroupRepeat(group.getName(), curRepeatNum);
                                    for (final DataElementValue deVal : repeat.getDataElements()) {
                                        if (isMainImagingFileElement(group.getName(), deVal.getName())) {
                                            final JTextField tf = (JTextField) deVal.getComp();
                                            tf.setText(imageFile);
                                            tf.setEnabled(false);
                                        }
                                    }
                                }
                            }

                            // need to determine if there are any entries in csv that has things like image extents
                            // or resolutions that are different than the ones determined by header....if there are,
                            // then prompt a warning

                            resolveConflictsUsing = determineImageHeaderDescrepencies(srcImage, repeatValues);
                        }
                    }

                    if (resolveConflictsUsing == RESOLVE_CONFLICT_CSV && srcImage != null) {
                        populateFields(fsData, srcImage);
                    }

                    for (int i = 0; i < csvFieldNames.size(); i++) {
                        final String[] deGroupAndName = splitFieldString(csvFieldNames.get(i).trim());
                        final String value = repeatValues.get(i).trim();
                        if (i != imageFileIndex && !value.equals("")) {
                            final GroupRepeat curRepeat = fsData.getGroupRepeat(deGroupAndName[0], curRepeatNum);
                            for (final DataElementValue deVal : curRepeat.getDataElements()) {
                                final JComponent comp = deVal.getComp();
                                if (deVal.getName().equalsIgnoreCase(deGroupAndName[1])) {
                                    if (comp instanceof JTextField) {
                                        final JTextField t = (JTextField) comp;
                                        t.setText(value);
                                    } else if (comp instanceof JComboBox) {
                                        final JComboBox combo = (JComboBox) comp;

                                        boolean isOther = true;

                                        for (int k = 0; k < combo.getItemCount(); k++) {
                                            final String item = (String) combo.getItemAt(k);
                                            if (value.equalsIgnoreCase(item)) {
                                                combo.setSelectedIndex(k);
                                                isOther = false;
                                            }
                                        }

                                        if (isOther) {
                                            deVal.getOtherSpecifyField().setText(value);
                                            combo.setSelectedItem(VALUE_OTHER_SPECIFY);
                                        }
                                    }

                                    // found the DE, move to next column in CSV values
                                    break;
                                }
                            }
                        }
                    }

                    if ( (resolveConflictsUsing == RESOLVE_CONFLICT_ASK || resolveConflictsUsing == RESOLVE_CONFLICT_IMG) && srcImage != null) {
                        populateFields(fsData, srcImage);
                    }

                    // if the image was read in, clean it up
                    if (srcImage != null) {
                        srcImage.disposeLocal();
                        srcImage = null;
                    }
                }
            } else {
                // this means its not an imaging data structure
                for (int curRepeatNum = 0; curRepeatNum < record.size(); curRepeatNum++) {
                    final ArrayList<String> repeatValues = record.get(curRepeatNum);
                    for (int i = 0; i < csvFieldNames.size(); i++) {
                        final String[] deGroupAndName = splitFieldString(csvFieldNames.get(i).trim());
                        final String value = repeatValues.get(i).trim();
                        if ( !value.equals("")) {
                            final GroupRepeat curRepeat = fsData.getGroupRepeat(deGroupAndName[0], curRepeatNum);
                            for (final DataElementValue deVal : curRepeat.getDataElements()) {
                                final JComponent comp = deVal.getComp();
                                if (deVal.getName().equalsIgnoreCase(deGroupAndName[1])) {
                                    if (comp instanceof JTextField) {
                                        final JTextField t = (JTextField) comp;
                                        t.setText(value);
                                    } else if (comp instanceof JComboBox) {
                                        final JComboBox combo = (JComboBox) comp;

                                        boolean isOther = true;

                                        for (int k = 0; k < combo.getItemCount(); k++) {
                                            final String item = (String) combo.getItemAt(k);
                                            if (value.equalsIgnoreCase(item)) {
                                                combo.setSelectedIndex(k);
                                                isOther = false;
                                            }
                                        }

                                        if (isOther) {
                                            deVal.getOtherSpecifyField().setText(value);
                                            combo.setSelectedItem(VALUE_OTHER_SPECIFY);
                                        }
                                    }

                                    // found the DE, move to next column in CSV values
                                    break;
                                }
                            }
                        }
                    }
                }
            }

            // need to validate and then close window
            ArrayList<String> errs;
            final StringBuffer errors = new StringBuffer();
            errs = validateFields();

            boolean isComplete = true;
            if (errs.size() != 0) {
                for (int i = 0; i < errs.size(); i++) {
                    errors.append(" - " + errs.get(i) + "\n");
                }
                isComplete = false;
            }

            if (validFile) {
                complete(fsData, isComplete);
            } else {
                if ( !launchedFromInProcessState) {
                    previewImages.remove(previewImages.size() - 1);
                    imageFiles.remove(imageFiles.size() - 1);
                    multifiles.remove(multifiles.size() - 1);
                    fsDataList.remove(fsDataList.size() - 1);
                    allOtherFilesAL.remove(allOtherFilesAL.size() - 1);
                    if (addedPreviewImage) {
                        previewImgPanel.removeAll();
                        previewImgPanel.repaint();
                    }
                }
            }

            enableDisableFinishButton();
            dispose();
        }

        private ModelImage readImgFromCSV(final String parentDir, final String imageFile) {
            String filePath;
            boolean isMultifile;
            final FileIO fileIO = new FileIO();
            fileIO.setQuiet(true);
            ModelImage srcImage = null;
            validFile = true;

            try {
                if (imageFile.endsWith(".zip")) {

                    String destName = imageFile.replace("/", File.separator);
                    destName = destName.replace("\\", File.separator);
                    destName = destName.substring(destName.lastIndexOf(File.separator) + 1, destName.lastIndexOf("."));
                    // String destDirName =
                    final String tempDir = parentDir + File.separator + destName + "_temp_" + System.currentTimeMillis();
                    tempDirs.add(tempDir);
                    final File imageZipFile = new File(parentDir + File.separator + imageFile);
                    String fileName = "";
                    // try {
                    final FileInputStream fis = new FileInputStream(imageZipFile);
                    final ZipInputStream zin = new ZipInputStream(new BufferedInputStream(fis));
                    FileOutputStream fout;
                    ZipEntry entry;
                    BufferedOutputStream dest = null;
                    final int BUFFER = 2048;
                    int count;
                    final byte[] data = new byte[BUFFER];
                    File f;
                    // while we are at it, find the first file that does not
                    // have a .raw extension, so we can
                    // open it

                    while ( (entry = zin.getNextEntry()) != null) {
                        f = new File(tempDir);
                        if ( !f.exists()) {
                            f.mkdir();
                        }

                        if (entry.isDirectory()) {
                            // if a directory, create it instead of trying to write it to disk
                            f = new File(tempDir + File.separator + entry.getName());
                            if ( !f.exists()) {
                                f.mkdirs();
                            }
                        } else {
                            // not a directory, so write out the file contents to disk from the zip and remember the
                            // first non-raw file name we find
                            if (fileName.equals("")) {
                                if ( !entry.getName().endsWith(".raw")) {
                                    fileName = entry.getName();
                                }
                            }

                            fout = new FileOutputStream(tempDir + File.separator + entry.getName());
                            dest = new BufferedOutputStream(fout, BUFFER);
                            while ( (count = zin.read(data, 0, BUFFER)) != -1) {
                                dest.write(data, 0, count);
                            }
                            dest.flush();
                            dest.close();
                        }
                    }
                    zin.close();
                    // } catch (FileNotFoundException f) {
                    // MipavUtil.displayError("The system cannot find the file specified");
                    // } catch (final Exception e) {
                    // e.printStackTrace();
                    // }

                    // now that everything has been unzipped, open the image
                    // from the tempDir
                    filePath = tempDir + File.separator + fileName;
                    isMultifile = true;
                } else {
                    // try to only open as a single file, since it wasn't zipped
                    filePath = parentDir + File.separator + imageFile;
                    isMultifile = false;
                }

                // TODO: would be best to only open the image once to pull out header info, get file list, and generate
                // thumbnail
                final File file = new File(filePath);
                srcImage = fileIO.readImage(file.getName(), file.getParent() + File.separator, isMultifile, null);

                final int[] extents = new int[] {srcImage.getExtents()[0], srcImage.getExtents()[1]};

                previewImg = new ViewJComponentPreviewImage(srcImage, extents, owner);
                int slice = 0;
                if ( !srcImage.is2DImage()) {
                    slice = (srcImage.getExtents()[2] / 2);
                }
                previewImg.createImg(slice);

                previewImgPanel.removeAll();
                previewImgPanel.repaint();

                previewImgPanel.add(previewImg);

                addedPreviewImage = true;

                if (launchedFromInProcessState) {
                    final int selectedRow = structTable.getSelectedRow();
                    previewImages.set(selectedRow, previewImg);
                    previewImages.get(selectedRow).setSliceBrightness(previewImgBrightness, previewImgContrast);
                    imageFiles.set(selectedRow, file);
                    multifiles.set(selectedRow, new Boolean(isMultifile));
                } else {
                    final int size = previewImages.size();
                    previewImages.set(size - 1, previewImg);
                    previewImages.get(size - 1).setSliceBrightness(previewImgBrightness, previewImgContrast);
                    imageFiles.set(size - 1, file);
                    multifiles.set(size - 1, new Boolean(isMultifile));
                }

                previewImgPanel.validate();
                previewImgPanel.repaint();

            } catch (final FileNotFoundException e) {
                MipavUtil.displayError("The system cannot find the file specified");
                validFile = false;
            } catch (final NullPointerException e) {
                MipavUtil.displayError("The system cannot find the file specified");
                validFile = false;
            } catch (final Exception e) {
                e.printStackTrace();
            }

            return srcImage;
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

                    final DataElement deInfo = deVal.getDataElementInfo();

                    if (deInfo.getType().equals(DataType.FILE)) {
                        elementPanel.add(deVal.getComp(), egbc);
                        egbc.gridx++;
                        final JButton browseButton = new JButton("Browse");
                        browseButton.addActionListener(this);
                        browseButton.setActionCommand("browse_-_" + repeat.getGroupInfo().getName() + "_-_" + repeat.getRepeatNumber() + "_-_"
                                + deInfo.getName());
                        elementPanel.add(browseButton, egbc);
                    } else {
                        egbc.gridwidth = 2;
                        elementPanel.add(deVal.getComp(), egbc);

                        if (isOtherSpecifyField(deVal.getComp())) {
                            egbc.gridy++;
                            elementPanel.add(deVal.getOtherSpecifyField(), egbc);
                        }
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

        private void parseDataStructure(final DataStructure dataStructure, final FormStructureData fsData, final ArrayList<ArrayList<String>> record) {
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
                            if (getFieldGroup(csvFieldNames.get(i)).equalsIgnoreCase(g.getName())) {
                                if ( !row.get(i).equals("")) {
                                    foundValue = true;
                                    break;
                                }
                            }
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

        private GroupRepeat parseGroupRepeat(final FormStructureData fsData, final RepeatableGroup group, final int repeatNum) {
            final GroupRepeat repeat = new GroupRepeat(group, fsData, repeatNum);

            for (final MapElement de : group.getDataElements()) {
                final DataElementValue newDeVal = new DataElementValue(repeat, de);

                JLabel l;

                l = new JLabel(de.getTitle());
                l.setFont(MipavUtil.font12);
                l.setName(de.getName());

                String tooltip = "<html><p><b>Name:</b> " + de.getName() + "<br/>";
                tooltip += "<b>Required?:</b> " + de.getRequiredType().getValue() + "<br/>";
                tooltip += "<b>Description:</b><br/>" + WordUtils.wrap(de.getDescription(), 80, "<br/>", false);
                tooltip += "</p></html>";
                l.setToolTipText(tooltip);

                // if valuerange is enumeration, create a combo box...otherwise create a textfield

                // special handling of SiteName for PDBP, where they want to use a set of permissible values
                // with
                // the free-form DE
                if (isPDBPImagingStructure(dataStructure.getShortName()) && de.getName().equalsIgnoreCase(SITE_NAME_ELEMENT_NAME)) {
                    final JComboBox cb = new JComboBox();
                    cb.setName(de.getName());
                    cb.setFont(MipavUtil.font12);
                    final String[] items = PDBP_ALLOWED_SITE_NAMES;
                    cb.addItem("");
                    for (final String element : items) {
                        final String item = element.trim();
                        cb.addItem(item);
                    }
                    cb.addItemListener(this);
                    if (de.getRequiredType().equals(RequiredType.REQUIRED)) {
                        l.setForeground(Color.red);
                    }

                    tooltip = "<html>";
                    if (de.getMeasuringUnit() != null) {
                        tooltip += "<p><b>Unit of measure:</b> " + de.getMeasuringUnit() + "</p>";
                    }
                    if (de.getNinds() != null && !de.getNinds().getValue().equals("")) {
                        tooltip += "<p><b>NINDS CDE ID:</b> " + de.getNinds().getValue() + "</p>";
                    }
                    if (de.getGuidelines() != null && !de.getGuidelines().trim().equals("")) {
                        tooltip += "<p><b>Guidelines & Instructions:</b></br>"
                                + WordUtils.wrap(removeRedundantDiseaseInfo(de.getGuidelines()), 80, "<br/>", false) + "</p>";
                    }
                    if (de.getNotes() != null && !de.getNotes().trim().equals("")) {
                        tooltip += "<p><b>Notes:</b><br/>" + WordUtils.wrap(removeRedundantDiseaseInfo(de.getNotes()), 80, "<br/>", false) + "</p>";
                    }
                    tooltip += "</html>";
                    if ( !tooltip.equals("<html></html>")) {
                        cb.setToolTipText(tooltip);
                    }

                    newDeVal.setLabel(l);
                    newDeVal.setComp(cb);
                } else if (de.getValueRangeList() != null && de.getValueRangeList().size() > 0 && de.getType() != null && !de.getType().equals(DataType.DATE)) {
                    final JComboBox cb = new JComboBox();
                    cb.setName(de.getName());
                    cb.setFont(MipavUtil.font12);

                    cb.addItem("");
                    for (final ValueRange val : de.getValueRangeList()) {
                        cb.addItem(val.getValueRange());
                    }
                    cb.addItemListener(this);

                    if (de.getRequiredType().equals(RequiredType.REQUIRED)) {
                        l.setForeground(Color.red);
                    }

                    tooltip = "<html>";
                    if (de.getMeasuringUnit() != null) {
                        tooltip += "<p><b>Unit of measure:</b> " + de.getMeasuringUnit() + "</p>";
                    }
                    if (de.getNinds() != null && !de.getNinds().getValue().equals("")) {
                        tooltip += "<p><b>NINDS CDE ID:</b> " + de.getNinds().getValue() + "</p>";
                    }
                    if (de.getGuidelines() != null && !de.getGuidelines().trim().equals("")) {
                        tooltip += "<p><b>Guidelines & Instructions:</b></br>"
                                + WordUtils.wrap(removeRedundantDiseaseInfo(de.getGuidelines()), 80, "<br/>", false) + "</p>";
                    }
                    if (de.getNotes() != null && !de.getNotes().trim().equals("")) {
                        tooltip += "<p><b>Notes:</b><br/>" + WordUtils.wrap(removeRedundantDiseaseInfo(de.getNotes()), 80, "<br/>", false) + "</p>";
                    }
                    tooltip += "</html>";
                    if ( !tooltip.equals("<html></html>")) {
                        cb.setToolTipText(tooltip);
                    }

                    newDeVal.setLabel(l);
                    newDeVal.setComp(cb);
                } else {
                    final JTextField tf = new JTextField(20);
                    tf.setName(de.getName());
                    tf.setFont(MipavUtil.font12);

                    tf.addMouseListener(new ContextMenuMouseListener());

                    tooltip = "<html><p><b>Type:</b> " + de.getType().getValue();
                    if (de.getType().equals(DataType.ALPHANUMERIC) && de.getSize() != null) {
                        tooltip += " (" + de.getSize() + ")";
                    }
                    tooltip += "</p>";

                    if (de.getType().equals(DataType.NUMERIC) || de.getType().equals(DataType.ALPHANUMERIC)) {
                        if (de.getMinimumValue() != null || de.getMaximumValue() != null) {
                            tooltip += "<p>";
                            if (de.getMinimumValue() != null) {
                                tooltip += "<b>Min:</b> " + de.getMinimumValue() + " ";
                            }
                            if (de.getMaximumValue() != null) {
                                tooltip += "<b>Max:</b> " + de.getMaximumValue();
                            }
                            tooltip += "</p>";
                        }
                    }

                    if (de.getMeasuringUnit() != null) {
                        tooltip += "<p><b>Unit of measure:</b> " + de.getMeasuringUnit() + "</p>";
                    }
                    if (de.getNinds() != null && !de.getNinds().getValue().equals("")) {
                        tooltip += "<p><b>NINDS CDE ID: </b>" + de.getNinds().getValue() + "</p>";
                    }
                    if (de.getGuidelines() != null && !de.getGuidelines().trim().equals("")) {
                        tooltip += "<p><b>Guidelines & Instructions:</b><br/>"
                                + WordUtils.wrap(removeRedundantDiseaseInfo(de.getGuidelines()), 80, "<br/>", false) + "</p>";
                    }
                    if (de.getNotes() != null && !de.getNotes().trim().equals("")) {
                        tooltip += "<p><b>Notes:</b><br/>" + WordUtils.wrap(removeRedundantDiseaseInfo(de.getNotes()), 80, "<br/>", false) + "</p>";
                    }
                    tooltip += "</html>";
                    tf.setToolTipText(tooltip);
                    tf.addFocusListener(this);

                    disableUnchangableFields(de.getName(), tf);

                    if (de.getName().equalsIgnoreCase(IMG_HASH_CODE_ELEMENT_NAME)) {
                        tf.setText("Automatically generated from selected image files.");
                    } else if (de.getName().equalsIgnoreCase(IMG_PREVIEW_ELEMENT_NAME)) {
                        tf.setText("Automatically generated from selected image files.");
                    }

                    if (de.getRequiredType().equals(RequiredType.REQUIRED)) {
                        l.setForeground(Color.red);
                    }

                    newDeVal.setLabel(l);
                    newDeVal.setComp(tf);
                }

                repeat.addDataElement(newDeVal);
            }

            return repeat;
        }

        private void disableUnchangableFields(final String elementName, final Component c) {
            for (final String e : unchangableElements) {
                if (elementName.equalsIgnoreCase(e)) {
                    c.setEnabled(false);
                }
            }
        }

        public int determineImageHeaderDescrepencies(final ModelImage img, final ArrayList<String> repeatValues) {
            final float[] res = img.getResolutions(0);
            final int[] units = img.getUnitsOfMeasure();
            final int exts[] = img.getExtents();
            final int nDims = img.getNDims();
            final int modality = img.getFileInfo(0).getModality();
            final String modalityString = FileInfoBase.getModalityStr(modality);
            final float sliceThickness = img.getFileInfo(0).getSliceThickness();
            final int orient = img.getFileInfo(0).getImageOrientation();
            final String orientation = FileInfoBase.getImageOrientationStr(orient);

            final int fileFormatInt = img.getFileInfo(0).getFileFormat();
            final String fileFormatString = FileUtility.getFileTypeStr(fileFormatInt);

            final ArrayList<String> csvFList = new ArrayList<String>();
            final ArrayList<String> csvPList = new ArrayList<String>();
            final ArrayList<String> headerList = new ArrayList<String>();

            for (int i = 0; i < csvFieldNames.size(); i++) {

                if ( !repeatValues.get(i).trim().equals("")) {
                    // TODO: switch to helper method - checkElementDescrepancy
                    if (csvFieldNames.get(i).equalsIgnoreCase("ImgDimensionTyp")) {
                        if ( !repeatValues.get(i).trim().equals(String.valueOf(nDims) + "D") && String.valueOf(nDims) != null) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(nDims));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgDim1ExtentVal") && String.valueOf(exts[0]) != null) {

                        if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == exts[0])) {

                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(exts[0]));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgDim2ExtentVal") && String.valueOf(exts[1]) != null) {
                        if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == exts[1])) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(exts[1]));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgDim3ExtentVal") && String.valueOf(exts[2]) != null) {
                        if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == exts[2])) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(exts[2]));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgDim4ExtentVal") && String.valueOf(exts[3]) != null) {
                        if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == exts[3])) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(exts[3]));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgDim5ExtentVal") && String.valueOf(exts[4]) != null) {
                        if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == exts[4])) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(exts[4]));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgDim1UoMVal") && Unit.getUnitFromLegacyNum(units[0]) != null) {
                        if ( !repeatValues.get(i).trim().equals(Unit.getUnitFromLegacyNum(units[0]).toString())) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(Unit.getUnitFromLegacyNum(units[0]).toString());
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgDim2UoMVal") && Unit.getUnitFromLegacyNum(units[1]) != null) {
                        if ( !repeatValues.get(i).trim().equals(Unit.getUnitFromLegacyNum(units[1]).toString())) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(Unit.getUnitFromLegacyNum(units[1]).toString());
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgDim3UoMVal") && Unit.getUnitFromLegacyNum(units[2]) != null) {
                        if ( !repeatValues.get(i).trim().equals(Unit.getUnitFromLegacyNum(units[2]).toString())) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(Unit.getUnitFromLegacyNum(units[2]).toString());
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgDim4UoMVal") && Unit.getUnitFromLegacyNum(units[3]) != null) {
                        if ( !repeatValues.get(i).trim().equals(Unit.getUnitFromLegacyNum(units[3]).toString())) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(Unit.getUnitFromLegacyNum(units[3]).toString());
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgDim5UoMVal") && Unit.getUnitFromLegacyNum(units[4]) != null) {
                        if ( !repeatValues.get(i).trim().equals(Unit.getUnitFromLegacyNum(units[4]).toString())) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(Unit.getUnitFromLegacyNum(units[4]).toString());
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgDim1ResolVal") && String.valueOf(res[0]) != null) {
                        if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == res[0])) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(res[0]));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgDim2ResolVal") && String.valueOf(res[1]) != null) {
                        if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == res[1])) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(res[1]));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgDim3ResolVal") && String.valueOf(res[2]) != null) {
                        if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == res[2])) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(res[2]));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgDim4ResolVal") && String.valueOf(res[3]) != null) {
                        if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == res[3])) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(res[3]));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgDim5ResolVal") && String.valueOf(res[4]) != null) {
                        if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == res[4])) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(res[4]));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgModltyTyp") && modalityString != null) {
                        if ( !repeatValues.get(i).trim().equals(convertModalityToBRICS(modalityString))) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(convertModalityToBRICS(modalityString));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgSliceThicknessVal") && String.valueOf(sliceThickness) != null) {
                        if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == sliceThickness)) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(sliceThickness));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgSliceOrientTyp") && orientation != null) {
                        if ( !repeatValues.get(i).trim().equals(orientation)) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(orientation);
                        }
                    }

                    if (fileFormatString.equalsIgnoreCase("dicom")) {
                        final FileInfoDicom fileInfoDicom = (FileInfoDicom) img.getFileInfo(0);

                        final String ageVal = (String) (fileInfoDicom.getTagTable().getValue("0010,1010"));
                        final String siteName = (String) (fileInfoDicom.getTagTable().getValue("0008,0080"));
                        final String visitDate = convertDateToISOFormat((String) (fileInfoDicom.getTagTable().getValue("0008,0020")));
                        final String visitTime = (String) (fileInfoDicom.getTagTable().getValue("0008,0030"));
                        final String sliceOversample = (String) (fileInfoDicom.getTagTable().getValue("0018,0093"));
                        final String gap = (String) (fileInfoDicom.getTagTable().getValue("0018,0088"));
                        final String bodyPart = (String) (fileInfoDicom.getTagTable().getValue("0018,0015"));

                        final String fieldOfView = (String) (fileInfoDicom.getTagTable().getValue("0018,1100"));
                        final String manufacturer = (String) (fileInfoDicom.getTagTable().getValue("0008,0070"));
                        final String softwareVersion = (String) (fileInfoDicom.getTagTable().getValue("0018,1020"));
                        final String patientPosition = (String) (fileInfoDicom.getTagTable().getValue("0018,5100"));

                        final String scannerModel = (String) (fileInfoDicom.getTagTable().getValue("0008,1090"));
                        final String bandwidth = (String) (fileInfoDicom.getTagTable().getValue("0018,0095"));

                        if (csvFieldNames.get(i).equalsIgnoreCase("AgeVal") && ageVal != null) {
                            final String ageInMonths = convertDicomAgeToBRICS(ageVal);

                            if ( !repeatValues.get(i).trim().equals(ageInMonths)) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(ageInMonths);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("AgeYrs") && ageVal != null) {
                            final String ageInYears = String.valueOf(Float.valueOf(convertDicomAgeToBRICS(ageVal)) / 12);
                            if ( !repeatValues.get(i).trim().equals(ageInYears)) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(ageInYears);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("visitDate") && !visitDate.equals("")) {
                            if ( !repeatValues.get(i).trim().equals(visitDate)) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(visitDate);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("SiteName") && !siteName.equals("")) {
                            if ( !repeatValues.get(i).trim().equals(siteName)) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(siteName);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgStdyDateTime") && ( !visitDate.equals("") || !visitTime.equals(""))) {
                            if ( !repeatValues.get(i).trim().equals(convertDateTimeToISOFormat(visitDate, visitTime))) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(convertDateTimeToISOFormat(visitDate, visitTime));
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgSliceOverSampVal") && !sliceOversample.equals("")) {
                            if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == (Float.parseFloat(sliceOversample)))) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(sliceOversample);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgGapBetwnSlicesMeasr") && !gap.equals("")) {
                            if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == (Float.parseFloat(gap)))) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(gap);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgAntmicSite") && !bodyPart.equals("")) {
                            if ( !repeatValues.get(i).trim().equals(bodyPart)) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(bodyPart);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgFOVMeasrDescTxt") && !fieldOfView.equals("")) {
                            if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == (Float.parseFloat(fieldOfView)))) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(fieldOfView);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgScannerManufName") && !manufacturer.equals("")) {
                            if ( !convertManufNameToBRICS(manufacturer).equals(repeatValues.get(i).trim())) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(convertManufNameToBRICS(manufacturer));
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgScannerSftwrVrsnNum") && softwareVersion != null) {
                            if ( !repeatValues.get(i).trim().equals(softwareVersion)) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(softwareVersion);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgHeadPostnTxt") && !patientPosition.equals("")) {
                            if ( !repeatValues.get(i).trim().equals(patientPosition)) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(patientPosition);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgScannerModelName") && !scannerModel.equals("")) {
                            if ( !repeatValues.get(i).trim().equals(convertModelNameToBRICS(scannerModel))) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(convertModelNameToBRICS(scannerModel));
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgBandwidthVal") && !bandwidth.equals("")) {
                            if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == (Float.parseFloat(bandwidth)))) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(bandwidth);
                            }
                        }

                        if (modalityString.equalsIgnoreCase("magnetic resonance")) {

                            final String echoTime = (String) (fileInfoDicom.getTagTable().getValue("0018,0081"));
                            final String repetitionTime = (String) (fileInfoDicom.getTagTable().getValue("0018,0080"));
                            final String magnaticFieldStrength = (String) (fileInfoDicom.getTagTable().getValue("0018,0087"));
                            final String flipAngle = (String) (fileInfoDicom.getTagTable().getValue("0018,1314"));

                            final String mriT1T2Name = (String) (fileInfoDicom.getTagTable().getValue("0018,0024"));
                            final String inversionTime = (String) (fileInfoDicom.getTagTable().getValue("0018,0082"));
                            final String echoTrainMeas = (String) (fileInfoDicom.getTagTable().getValue("0018,0091"));
                            final String phaseEncode = (String) (fileInfoDicom.getTagTable().getValue("0018,1312"));
                            final String numAverages = (String) (fileInfoDicom.getTagTable().getValue("0018,0083"));
                            final String receiveCoilName = (String) (fileInfoDicom.getTagTable().getValue("0018,1250"));

                            if (csvFieldNames.get(i).equalsIgnoreCase("ImgEchoDur")) {
                                if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == (Float.parseFloat(echoTime)))) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(echoTime);
                                }
                            } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgRepetitionGapVal")) {

                                if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == (Float.parseFloat(repetitionTime)))) {

                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(repetitionTime);
                                }
                            } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgScannerStrgthVal")) {
                                if ( !repeatValues.get(i).trim().equals(convertMagFieldStrengthToBRICS(magnaticFieldStrength))) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(convertMagFieldStrengthToBRICS(magnaticFieldStrength));
                                }
                            } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgFlipAngleMeasr")) {
                                if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == (Float.parseFloat(flipAngle)))) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(flipAngle);
                                }
                            } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgMRIT1T2SeqName")) {
                                if ( !repeatValues.get(i).trim().equals(mriT1T2Name)) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(mriT1T2Name);
                                }
                            } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgInversionTime")) {
                                if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == (Float.parseFloat(inversionTime)))) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(inversionTime);
                                }
                            } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgEchoTrainLngthMeasr")) {
                                if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == (Float.parseFloat(echoTrainMeas)))) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(echoTrainMeas);
                                }
                            } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgPhasEncdeDirctTxt")) {
                                if ( !repeatValues.get(i).trim().equals(phaseEncode)) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(phaseEncode);
                                }
                            } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgSignalAvgNum")) {
                                if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == (Float.parseFloat(numAverages)))) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(numAverages);
                                }
                            } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgRFCoilName")) {
                                if ( !repeatValues.get(i).trim().equals(receiveCoilName)) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(receiveCoilName);
                                }
                            }
                        }
                    } else if (fileFormatString.equalsIgnoreCase("nifti")) {
                        // Description = Philips Medical Systems Achieva 3.2.1 (from .nii T1 of Dr. Vaillancourt's)
                        final FileInfoNIFTI fileInfoNifti = (FileInfoNIFTI) img.getFileInfo(0);
                        final String description = fileInfoNifti.getDescription();

                        final String manuf = convertNiftiDescToBRICSManuf(description);
                        final String model = convertNiftiDescToBRICSModel(description);
                        final String scannerVer = convertNiftiDescToBRICSVer(description);

                        if (csvFieldNames.get(i).equalsIgnoreCase("ImgScannerManufName")) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(manuf);
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgScannerModelName")) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(model);
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("ImgScannerSftwrVrsnNum")) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(scannerVer);
                        }
                    }
                }
            }

            if (csvFList.size() > 0) {
                if (resolveConflictsUsing == RESOLVE_CONFLICT_ASK) {
                    String message = "Certain image info in the csv do not match with info obtained from header : \n";
                    for (int i = 0; i < csvFList.size(); i++) {
                        final String fieldName = csvFList.get(i);
                        final String param = csvPList.get(i);
                        final String headerInfo = headerList.get(i);

                        message = message + fieldName + " : " + "      csv:" + param + "     header:" + headerInfo + "\n";
                    }

                    UIManager.put("OptionPane.yesButtonText", "Use CSV");
                    UIManager.put("OptionPane.noButtonText", "Use Image Header");

                    final JCheckBox checkbox = new JCheckBox("Do not show this message again", false);
                    final Object[] content = {message, checkbox};

                    // I'd like to create a merge option using the selected
                    // button as the dominating input - Sara

                    final int response = JOptionPane.showConfirmDialog(null, content, "", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);

                    UIManager.put("OptionPane.yesButtonText", "Yes");
                    UIManager.put("OptionPane.noButtonText", "No");

                    if (response == JOptionPane.YES_OPTION) {
                        if (checkbox.isSelected()) {
                            resolveConflictsUsing = RESOLVE_CONFLICT_CSV;
                        }
                        return RESOLVE_CONFLICT_CSV;
                    } else {
                        if (checkbox.isSelected()) {
                            resolveConflictsUsing = RESOLVE_CONFLICT_IMG;
                        }
                        return RESOLVE_CONFLICT_IMG;
                    }
                } else {
                    return resolveConflictsUsing;
                }
            } else {
                return RESOLVE_CONFLICT_ASK;
            }
        }

        /**
         * prepopulates some of the fields with info from image header
         */
        public void populateFields(final FormStructureData fsData, final ModelImage img) {
            final float[] res = img.getResolutions(0);
            final int[] units = img.getUnitsOfMeasure();
            final int exts[] = img.getExtents();
            int modality = img.getFileInfo(0).getModality();
            String modalityString = FileInfoBase.getModalityStr(modality);
            final int fileFormatInt = img.getFileInfo(0).getFileFormat();

            String fileFormatString = FileUtility.getFileTypeStr(fileFormatInt);
            if (fileFormatString.equalsIgnoreCase("xml")) {
                fileFormatString = "mipav xml";
            } else if (fileFormatString.equalsIgnoreCase("mat")) {
                fileFormatString = "matlab";
            }

            // if no modality, try to guess from structure being used
            if (modality == FileInfoBase.UNKNOWN_MODALITY) {
                final String upperStructureName = dataStructureName.toUpperCase();
                if (upperStructureName.endsWith("IMAGINGMR")) {
                    modality = FileInfoBase.MAGNETIC_RESONANCE;
                } else if (upperStructureName.endsWith("IMAGINGCT")) {
                    modality = FileInfoBase.COMPUTED_TOMOGRAPHY;
                }
                modalityString = FileInfoBase.getModalityStr(modality);
            }

            final float sliceThickness = img.getFileInfo(0).getSliceThickness();
            final int orient = img.getFileInfo(0).getImageOrientation();
            final String orientation = FileInfoBase.getImageOrientationStr(orient);

            for (final RepeatableGroup group : fsData.getStructInfo().getRepeatableGroups()) {
                for (final GroupRepeat repeat : fsData.getAllGroupRepeats(group.getName())) {
                    for (final DataElementValue deVal : repeat.getDataElements()) {
                        final String deName = deVal.getName();

                        if (deName.equalsIgnoreCase("ImgDimensionTyp")) {
                            setElementComponentValue(deVal, exts.length + "D");
                        } else if (deName.equalsIgnoreCase("ImgDim1ExtentVal")) {
                            setElementComponentValue(deVal, String.valueOf(exts[0]));
                        } else if (deName.equalsIgnoreCase("ImgDim2ExtentVal")) {
                            setElementComponentValue(deVal, String.valueOf(exts[1]));
                        } else if (deName.equalsIgnoreCase("ImgDim3ExtentVal")) {
                            if (img.getNDims() > 2) {
                                setElementComponentValue(deVal, String.valueOf(exts[2]));
                            }
                        } else if (deName.equalsIgnoreCase("ImgDim4ExtentVal")) {
                            if (img.getNDims() > 3) {
                                setElementComponentValue(deVal, String.valueOf(exts[3]));
                            }
                        } else if (deName.equalsIgnoreCase("ImgDim5ExtentVal")) {
                            // for now...nothing
                        } else if (deName.equalsIgnoreCase("ImgDim1UoMVal")) {
                            setElementComponentValue(deVal, Unit.getUnitFromLegacyNum(units[0]).toString());
                        } else if (deName.equalsIgnoreCase("ImgDim2UoMVal")) {
                            setElementComponentValue(deVal, Unit.getUnitFromLegacyNum(units[1]).toString());
                        } else if (deName.equalsIgnoreCase("ImgDim3UoMVal")) {
                            if (img.getNDims() > 2) {
                                setElementComponentValue(deVal, Unit.getUnitFromLegacyNum(units[2]).toString());
                            }
                        } else if (deName.equalsIgnoreCase("ImgDim4UoMVal")) {
                            if (img.getNDims() > 3) {
                                setElementComponentValue(deVal, Unit.getUnitFromLegacyNum(units[3]).toString());
                            }
                        } else if (deName.equalsIgnoreCase("ImgDim5UoMVal")) {
                            // for now...nothing
                        } else if (deName.equalsIgnoreCase("ImgDim1ResolVal")) {
                            setElementComponentValue(deVal, String.valueOf(res[0]));
                        } else if (deName.equalsIgnoreCase("ImgDim2ResolVal")) {
                            setElementComponentValue(deVal, String.valueOf(res[1]));
                        } else if (deName.equalsIgnoreCase("ImgDim3ResolVal")) {
                            if (img.getNDims() > 2) {
                                setElementComponentValue(deVal, String.valueOf(res[2]));
                            }
                        } else if (deName.equalsIgnoreCase("ImgDim4ResolVal")) {
                            if (img.getNDims() > 3) {
                                setElementComponentValue(deVal, String.valueOf(res[3]));
                            }
                        } else if (deName.equalsIgnoreCase("ImgDim5ResolVal")) {
                            // for now...nothing
                        } else if (deName.equalsIgnoreCase("ImgModltyTyp")) {
                            setElementComponentValue(deVal, convertModalityToBRICS(modalityString));
                        } else if (deName.equalsIgnoreCase("ImgFileFormatTyp")) {
                            setElementComponentValue(deVal, fileFormatString);
                        } else if (deName.equalsIgnoreCase("ImgSliceThicknessVal")) {
                            String thicknessStr = "";
                            if (sliceThickness > 0) {
                                thicknessStr = String.valueOf(sliceThickness);
                            }
                            setElementComponentValue(deVal, thicknessStr);
                        } else if (deName.equalsIgnoreCase("ImgSliceOrientTyp")) {
                            setElementComponentValue(deVal, orientation);
                        }

                        if (fileFormatString.equalsIgnoreCase("dicom")) {
                            final FileInfoDicom fileInfoDicom = (FileInfoDicom) img.getFileInfo(0);

                            final String ageVal = (String) (fileInfoDicom.getTagTable().getValue("0010,1010"));
                            final String siteName = (String) (fileInfoDicom.getTagTable().getValue("0008,0080"));
                            final String visitDate = (String) (fileInfoDicom.getTagTable().getValue("0008,0020"));
                            final String visitTime = (String) (fileInfoDicom.getTagTable().getValue("0008,0030"));
                            final String sliceOversample = (String) (fileInfoDicom.getTagTable().getValue("0018,0093"));
                            final String gap = (String) (fileInfoDicom.getTagTable().getValue("0018,0088"));
                            final String bodyPart = (String) (fileInfoDicom.getTagTable().getValue("0018,0015"));

                            final String fieldOfView = (String) (fileInfoDicom.getTagTable().getValue("0018,1100"));
                            final String manufacturer = (String) (fileInfoDicom.getTagTable().getValue("0008,0070"));
                            final String softwareVersion = (String) (fileInfoDicom.getTagTable().getValue("0018,1020"));
                            final String patientPosition = (String) (fileInfoDicom.getTagTable().getValue("0018,5100"));

                            final String scannerModel = (String) (fileInfoDicom.getTagTable().getValue("0008,1090"));
                            final String bandwidth = (String) (fileInfoDicom.getTagTable().getValue("0018,0095"));

                            final String patientName = (String) (fileInfoDicom.getTagTable().getValue("0010,0010"));
                            final String patientID = (String) (fileInfoDicom.getTagTable().getValue("0010,0020"));

                            if (deName.equalsIgnoreCase("AgeVal") && ageVal != null && !ageVal.equals("")) {
                                final String ageInMonths = convertDicomAgeToBRICS(ageVal);
                                if (Float.parseFloat(ageInMonths) != 0) {
                                    setElementComponentValue(deVal, ageInMonths);
                                }
                            } else if (deName.equalsIgnoreCase("AgeYrs") && ageVal != null && !ageVal.equals("")) {
                                final Float ageInMonths = Float.valueOf(convertDicomAgeToBRICS(ageVal));
                                if (ageInMonths != 0) {
                                    final String ageInYears = String.valueOf(ageInMonths / 12);
                                    setElementComponentValue(deVal, ageInYears);
                                }
                            } else if (deName.equalsIgnoreCase("SiteName")) {
                                setElementComponentValue(deVal, siteName);
                            } else if (deName.equalsIgnoreCase("VisitDate")) {
                                setElementComponentValue(deVal, convertDateToISOFormat(visitDate));
                            } else if (deName.equalsIgnoreCase("ImgAntmicSite")) {
                                setElementComponentValue(deVal, bodyPart);
                            } else if (deName.equalsIgnoreCase("ImgStdyDateTime")) {
                                setElementComponentValue(deVal, convertDateTimeToISOFormat(visitDate, visitTime));
                            } else if (deName.equalsIgnoreCase("ImgSliceOverSampVal")) {
                                setElementComponentValue(deVal, sliceOversample);
                            } else if (deName.equalsIgnoreCase("ImgGapBetwnSlicesMeasr")) {
                                setElementComponentValue(deVal, gap);
                            } else if (deName.equalsIgnoreCase("ImgFOVMeasrDescTxt")) {
                                setElementComponentValue(deVal, fieldOfView);
                            } else if (deName.equalsIgnoreCase("ImgScannerManufName")) {
                                setElementComponentValue(deVal, convertManufNameToBRICS(manufacturer));
                            } else if (deName.equalsIgnoreCase("ImgScannerSftwrVrsnNum")) {
                                setElementComponentValue(deVal, softwareVersion);
                            } else if (deName.equalsIgnoreCase("ImgHeadPostnTxt")) {
                                setElementComponentValue(deVal, patientPosition);
                            } else if (deName.equalsIgnoreCase("ImgScannerModelName")) {
                                setElementComponentValue(deVal, convertModelNameToBRICS(scannerModel));
                            } else if (deName.equalsIgnoreCase("ImgBandwidthVal")) {
                                setElementComponentValue(deVal, bandwidth);
                            } else if (deName.equalsIgnoreCase("GUID")) {
                                if (isGuid(patientID)) {
                                    setElementComponentValue(deVal, patientID);
                                } else if (isGuid(patientName)) {
                                    setElementComponentValue(deVal, patientName);
                                }
                            }

                            if (modalityString.equalsIgnoreCase("magnetic resonance")) {
                                final String echoTime = (String) (fileInfoDicom.getTagTable().getValue("0018,0081"));
                                final String repetitionTime = (String) (fileInfoDicom.getTagTable().getValue("0018,0080"));
                                final String magnaticFieldStrength = (String) (fileInfoDicom.getTagTable().getValue("0018,0087"));
                                final String flipAngle = (String) (fileInfoDicom.getTagTable().getValue("0018,1314"));

                                final String mriT1T2Name = (String) (fileInfoDicom.getTagTable().getValue("0018,0024"));
                                final String inversionTime = (String) (fileInfoDicom.getTagTable().getValue("0018,0082"));
                                final String echoTrainMeas = (String) (fileInfoDicom.getTagTable().getValue("0018,0091"));
                                final String phaseEncode = (String) (fileInfoDicom.getTagTable().getValue("0018,1312"));
                                final String numAverages = (String) (fileInfoDicom.getTagTable().getValue("0018,0083"));
                                final String receiveCoilName = (String) (fileInfoDicom.getTagTable().getValue("0018,1250"));

                                if (deName.equalsIgnoreCase("ImgEchoDur")) {
                                    setElementComponentValue(deVal, echoTime);
                                } else if (deName.equalsIgnoreCase("ImgRepetitionGapVal")) {
                                    setElementComponentValue(deVal, repetitionTime);
                                } else if (deName.equalsIgnoreCase("ImgScannerStrgthVal")) {
                                    setElementComponentValue(deVal, convertMagFieldStrengthToBRICS(magnaticFieldStrength));
                                } else if (deName.equalsIgnoreCase("ImgMRIT1T2SeqName")) {
                                    setElementComponentValue(deVal, mriT1T2Name);
                                } else if (deName.equalsIgnoreCase("ImgSignalAvgNum")) {
                                    setElementComponentValue(deVal, numAverages);
                                } else if (deName.equalsIgnoreCase("ImgFlipAngleMeasr")) {
                                    setElementComponentValue(deVal, flipAngle);
                                } else if (deName.equalsIgnoreCase("ImgEchoTrainLngthMeasr")) {
                                    setElementComponentValue(deVal, echoTrainMeas);
                                } else if (deName.equalsIgnoreCase("ImgInversionTime")) {
                                    setElementComponentValue(deVal, inversionTime);
                                } else if (deName.equalsIgnoreCase("ImgRFCoilName")) {
                                    setElementComponentValue(deVal, receiveCoilName);
                                } else if (deName.equalsIgnoreCase("ImgPhasEncdeDirctTxt")) {
                                    setElementComponentValue(deVal, phaseEncode);
                                }
                            }
                        } else if (fileFormatString.equalsIgnoreCase("nifti")) {
                            // Description = Philips Medical Systems Achieva 3.2.1 (from .nii T1 of Dr. Vaillancourt's)
                            final FileInfoNIFTI fileInfoNifti = (FileInfoNIFTI) img.getFileInfo(0);
                            final String description = fileInfoNifti.getDescription();

                            final String manuf = convertNiftiDescToBRICSManuf(description);
                            final String model = convertNiftiDescToBRICSModel(description);
                            final String scannerVer = convertNiftiDescToBRICSVer(description);

                            if (deName.equalsIgnoreCase("ImgScannerManufName")) {
                                setElementComponentValue(deVal, manuf);
                            } else if (deName.equalsIgnoreCase("ImgScannerModelName")) {
                                setElementComponentValue(deVal, model);
                            } else if (deName.equalsIgnoreCase("ImgScannerSftwrVrsnNum")) {
                                setElementComponentValue(deVal, scannerVer);
                            }
                        }
                    }
                }
            }
        }

        /**
         * clear populated fields
         * 
         * @param labelsAndComps
         */
        public void clearFields(final FormStructureData fsData) {
            for (final RepeatableGroup group : fsData.getStructInfo().getRepeatableGroups()) {
                for (final GroupRepeat repeat : fsData.getAllGroupRepeats(group.getName())) {
                    for (final DataElementValue deVal : repeat.getDataElements()) {
                        if ( !deVal.getName().equalsIgnoreCase(IMG_FILE_ELEMENT_NAME)) {
                            try {
                                ((JTextField) deVal.getComp()).setText(null);
                            } catch (final ClassCastException e) {
                                try {
                                    ((JComboBox) deVal.getComp()).setSelectedItem(null);
                                } catch (final ClassCastException f) {

                                }
                            }
                        }
                    }
                }
            }
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
                    previewImages.remove(previewImages.size() - 1);
                    imageFiles.remove(imageFiles.size() - 1);
                    multifiles.remove(multifiles.size() - 1);
                    fsDataList.remove(fsDataList.size() - 1);
                    allOtherFilesAL.remove(allOtherFilesAL.size() - 1);
                    if (addedPreviewImage) {
                        previewImgPanel.removeAll();
                        previewImgPanel.repaint();
                    }
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
                if (isImagingStructure(dataStructureName) && isMainImagingFileElement(groupName, deName)) {
                    final ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, false);
                    fileChooser.setMulti(ViewUserInterface.getReference().getLastStackFlag());

                    final JFileChooser chooser = fileChooser.getFileChooser();
                    chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));

                    // default to TECH filter
                    int filter = ViewImageFileFilter.TECH;

                    try {
                        filter = Integer.parseInt(Preferences.getProperty(Preferences.PREF_FILENAME_FILTER));
                    } catch (final NumberFormatException nfe) {

                        // an invalid value was set in preferences -- so don't
                        // use it!
                        filter = -1;
                    }

                    chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
                    chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
                    chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
                    chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

                    if (filter != -1) {
                        // it seems that the set command adds the filter
                        // again...
                        // chooser.addChoosableFileFilter(new
                        // ViewImageFileFilter(filter));

                        // if filter is something we already added, then remove
                        // it before
                        // setting it..... (kludgy, kludgy....)
                        final javax.swing.filechooser.FileFilter found = ViewOpenFileUI.findFilter(chooser, filter);

                        if (found != null) {
                            chooser.removeChoosableFileFilter(found);
                        }

                        // initially set to the preferences
                        chooser.setFileFilter(new ViewImageFileFilter(filter));
                    }

                    final int returnVal = chooser.showOpenDialog(this);

                    if (returnVal == JFileChooser.APPROVE_OPTION) {
                        isMultiFile = fileChooser.isMulti();

                        final File file = chooser.getSelectedFile();
                        ViewUserInterface.getReference().setDefaultDirectory(file.getParent());

                        // TODO: would be best to only open the image once to pull out header info, get file list, and
                        // generate thumbnail
                        ModelImage srcImage = null;
                        if (file.getName().endsWith(".zip")) {
                            // if the user selects a zip file containing a dataset, try to open it as if pointed to from
                            // CSV
                            srcImage = readImgFromCSV(file.getParent(), file.getName());
                        } else {
                            final FileIO fileIO = new FileIO();
                            fileIO.setQuiet(true);
                            srcImage = fileIO.readImage(file.getName(), file.getParent() + File.separator, isMultiFile, null);

                            final int[] extents = new int[] {srcImage.getExtents()[0], srcImage.getExtents()[1]};

                            previewImg = new ViewJComponentPreviewImage(srcImage, extents, owner);
                            int slice = 0;
                            if ( !srcImage.is2DImage()) {
                                slice = (srcImage.getExtents()[2] / 2);
                            }
                            previewImg.createImg(slice);

                            previewImgPanel.removeAll();
                            previewImgPanel.repaint();

                            previewImgPanel.add(previewImg);

                            addedPreviewImage = true;

                            if (launchedFromInProcessState) {
                                final int selectedRow = structTable.getSelectedRow();
                                previewImages.set(selectedRow, previewImg);
                                previewImages.get(selectedRow).setSliceBrightness(previewImgBrightness, previewImgContrast);
                                imageFiles.set(selectedRow, file);
                                multifiles.set(selectedRow, new Boolean(isMultiFile));

                            } else {
                                final int size = previewImages.size();
                                previewImages.set(size - 1, previewImg);
                                previewImages.get(size - 1).setSliceBrightness(previewImgBrightness, previewImgContrast);
                                imageFiles.set(size - 1, file);
                                multifiles.set(size - 1, new Boolean(isMultiFile));
                            }

                            previewImgPanel.validate();
                            previewImgPanel.repaint();
                        }

                        if (srcImage != null) {
                            String tempName = currFile;

                            for (final DataElementValue deVal : fsData.getGroupRepeat(groupName, repeatNum).getDataElements()) {
                                final String curDeName = deVal.getName();
                                if (curDeName.equalsIgnoreCase(IMG_PREVIEW_ELEMENT_NAME)) {
                                    final JTextField tf = (JTextField) deVal.getComp();
                                    tf.setText("Automatically generated from selected image files.");
                                } else if (curDeName.equalsIgnoreCase(deName)) {
                                    final JTextField tf = (JTextField) deVal.getComp();
                                    tf.setText(file.getName());
                                    tempName = file.getName();
                                    tf.setEnabled(false);
                                }
                            }

                            if ( !currFile.equals(tempName) && !currFile.equals("")) {
                                clearFields(fsData);
                            }
                            populateFields(fsData, srcImage);

                            srcImage.disposeLocal();
                            srcImage = null;
                        }
                    }
                } else {
                    final JFileChooser chooser = new JFileChooser();
                    chooser.setDialogTitle("Choose file");
                    final int returnValue = chooser.showOpenDialog(this);
                    if (returnValue == JFileChooser.APPROVE_OPTION) {
                        final File file = chooser.getSelectedFile();

                        for (final DataElementValue deVal : fsData.getGroupRepeat(groupName, repeatNum).getDataElements()) {
                            if (deVal.getName().equalsIgnoreCase(deName)) {
                                final JTextField tf = (JTextField) deVal.getComp();
                                tf.setText(file.getAbsolutePath());
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
            final String dsName = (String) structTableModel.getValueAt(structTable.getSelectedRow(), 0);
            new InfoDialog(owner, dsName, true, true, null);
            fixErrors = FIX_ERRORS_LATER;
        }

        /**
         * validates fields
         * 
         * @return
         */
        public ArrayList<String> validateFields() {
            final ArrayList<String> errs = new ArrayList<String>();

            parseDataStructForValidation(fsData, errs);

            return errs;
        }

        /**
         * validates fields
         */
        public void parseDataStructForValidation(final FormStructureData fsData, final ArrayList<String> errs) {
            errors = new ArrayList<DataElementValue>();
            String value = "";
            final String key = "";
            RequiredType required = null;
            DataType type = null;

            for (final RepeatableGroup group : fsData.getStructInfo().getRepeatableGroups()) {
                for (final GroupRepeat repeat : fsData.getAllGroupRepeats(group.getName())) {
                    for (final DataElementValue deVal : repeat.getDataElements()) {
                        final DataElement deInfo = deVal.getDataElementInfo();

                        final JComponent deComp = deVal.getComp();
                        if (deComp instanceof JTextField) {
                            value = ((JTextField) deComp).getText().trim();
                        } else if (deComp instanceof JComboBox) {
                            value = (String) ( ((JComboBox) deComp).getSelectedItem());
                        }

                        // now we need to validate
                        required = deVal.getRequiredType();
                        type = deInfo.getType();

                        if (required.equals(RequiredType.REQUIRED)) {
                            if (value.trim().equalsIgnoreCase("")) {
                                errs.add(deInfo.getTitle() + " is a required field");
                                errors.add(deVal);
                            } else {
                                if (key.equalsIgnoreCase(GUID_ELEMENT_NAME)) {
                                    if ( !isGuid(value.trim())) {
                                        errs.add(deInfo.getTitle() + " must begin with a valid GUID prefix");
                                        errors.add(deVal);
                                    }
                                }
                            }
                        }

                        if (type.equals(DataType.NUMERIC)) {
                            if ( !value.trim().equalsIgnoreCase("")) {
                                try {
                                    final float floatValue = Float.valueOf(value.trim()).floatValue();
                                    if (deInfo.getMinimumValue() != null && floatValue < deInfo.getMinimumValue().floatValue()) {
                                        errs.add(deInfo.getTitle() + " must be in the range of " + deInfo.getMinimumValue().floatValue() + " to "
                                                + deInfo.getMaximumValue().floatValue());
                                        errors.add(deVal);
                                    } else if (deInfo.getMaximumValue() != null && floatValue > deInfo.getMaximumValue().floatValue()) {
                                        errs.add(deInfo.getTitle() + " must be in the range of " + deInfo.getMinimumValue().floatValue() + " to "
                                                + deInfo.getMaximumValue().floatValue());
                                        errors.add(deVal);
                                    }
                                } catch (final NumberFormatException e) {
                                    errs.add(deInfo.getTitle() + " must be a number");
                                    errors.add(deVal);
                                }
                            }
                        }
                        if (type.equals(DataType.ALPHANUMERIC)) {
                            if (deInfo.getSize() != null && deInfo.getSize() > 0 && value.length() > deInfo.getSize()) {
                                errs.add(deInfo.getTitle() + " must not exceed " + deInfo.getSize() + " in length");
                                errors.add(deVal);
                            }
                        }
                    }
                }
            }
        }

        /**
         * called after validation is done
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
                            if (value.equalsIgnoreCase(VALUE_OTHER_SPECIFY)) {
                                value = deVal.getOtherSpecifyField().getText().trim();
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
                name = fsData.getStructInfo().getShortName() + "_" + guid;
            } else {
                name = fsData.getStructInfo().getShortName() + "_UNKNOWNGUID";
            }

            if (launchedFromInProcessState) {
                final int selectedRow = structTable.getSelectedRow();

                structTableModel.setValueAt(name, selectedRow, 0);
                if (isComplete) {
                    structTableModel.setValueAt("Yes", selectedRow, 1);
                } else {
                    structTableModel.setValueAt("No", selectedRow, 1);
                }

                fsDataList.set(selectedRow, fsData);

                allOtherFilesAL.set(selectedRow, allOtherFiles);

            } else {
                if ( !addedPreviewImage) {
                    previewImgPanel.removeAll();
                    previewImgPanel.repaint();
                }

                fsDataList.set(fsDataList.size() - 1, fsData);
                final Vector<String> rowData = new Vector<String>();
                rowData.add(name);
                if (isComplete) {
                    rowData.add("Yes");
                } else {
                    rowData.add("No");
                }
                structTableModel.addRow(rowData);
                structTable.setRowSelectionInterval(structTableModel.getRowCount() - 1, structTableModel.getRowCount() - 1);

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
     * Class that connects to BRICS data dictionary web service (via RESTful API).
     */
    public class RESTThread extends Thread implements ActionListener {
        private static final String ddAuthBase = "/portal/ws/webstart/dictionary/formStructure/details";

        private static final String ddRequestBase = "/portal/ws/ddt/dictionary/FormStructure";

        private static final String ddStructListRequest = ddRequestBase + "/Published/list?page=1&pageSize=100000&ascending=false&sort=shortName";

        private JButton progressCancelButton;

        private final PlugInDialogFITBIR parent;

        public RESTThread(final PlugInDialogFITBIR parent) {
            super();
            this.parent = parent;
        }

        @Override
        public void run() {
            ViewJProgressBar progressBar = null;
            try {
                progressBar = new ViewJProgressBar("BRICS", "Connecting to BRICS data dictionary web service...", 0, 100, true);
                progressBar.setVisible(true);
                progressBar.updateValue(20);
                progressBar.setIndeterminate(true);
                progressCancelButton = progressBar.getCancelButton();
                progressCancelButton.addActionListener(this);

                // try to read the server config from disk, if it is there.
                // otherwise the value set above at initialization is used.
                readConfig();

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
                if (ddUseAuthService) {
                    dataStructureList = (List<DataStructure>) client.accept("text/xml").getCollection(DataStructure.class);
                } else {
                    final DataStructureList dsl = client.accept("text/xml").get(DataStructureList.class);
                    dataStructureList = dsl.getList();
                }
                final long endTime = System.currentTimeMillis();

                System.out.println("Webservice request (sec):\t" + ( (endTime - startTime) / 1000));

                // for (final DataStructure ds : dataStructureList) {
                // System.out.println("FS title:\t" + ds.getTitle() + "\tversion:\t" + ds.getVersion() + "\tpub:\t" +
                // ds.getStatus());
                // }

                progressBar.updateValue(80);

                progressBar.updateValue(100);
                progressBar.setVisible(false);
                progressBar.dispose();
                printlnToLog("Successful connection to BRICS data dictionary web service");

                addStructButton.setEnabled(true);
                loadCSVButton.setEnabled(true);
            } catch (final Exception e) {
                e.printStackTrace();
                if (progressBar != null) {
                    progressBar.setVisible(false);
                    progressBar.dispose();
                    MipavUtil.displayError("Error in connecting to web service");
                    parent.dispose();
                    if (JDialogStandalonePlugin.isExitRequired()) {
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
                    System.exit(0);
                }
            }
        }
    }

    public class DataElementValue {
        private GroupRepeat deGroup;

        private JLabel deLabel;

        private JComponent deComp;

        private JTextField otherSpecifyField;

        private DataElement deInfo;

        private String deValue;

        private int dePosition;

        private RequiredType deRequiredType;

        public DataElementValue(final GroupRepeat group, final JLabel label, final JComponent comp, final MapElement info, final String val) {
            deGroup = group;
            deLabel = label;
            setComp(comp);
            deInfo = info.getDataElement();
            dePosition = info.getPosition();
            deRequiredType = info.getRequiredType();
            deValue = val;
        }

        public DataElementValue(final GroupRepeat group, final MapElement info) {
            deGroup = group;
            deInfo = info.getDataElement();
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

            if (isOtherSpecifyField(deComp)) {
                otherSpecifyField = new JTextField();
                otherSpecifyField.setVisible(false);
                ((JComboBox) deComp).addActionListener(new OtherSpecifyListener(otherSpecifyField));
            }
        }

        public JTextField getOtherSpecifyField() {
            return otherSpecifyField;
        }

        public String getName() {
            return deInfo.getName();
        }

        public DataElement getDataElementInfo() {
            return deInfo;
        }

        public void setDataElementInfo(final DataElement deInfo) {
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

    public class FormStructureData {
        private DataStructure structInfo;

        private final Hashtable<String, Vector<GroupRepeat>> groupTable;

        public FormStructureData(final DataStructure structInfo) {
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

        public DataStructure getStructInfo() {
            return structInfo;
        }

        public void setStructInfo(final DataStructure structInfo) {
            this.structInfo = structInfo;
        }
    }

    public class OtherSpecifyListener implements ActionListener {
        private final JTextField otherSpecifyField;

        public OtherSpecifyListener(final JTextField otherSpecifyField) {
            this.otherSpecifyField = otherSpecifyField;
        }

        @Override
        public void actionPerformed(final ActionEvent e) {
            final Object source = e.getSource();

            // sanity check before casts
            if (source instanceof JComboBox) {
                otherSpecifyField.setVisible( ((JComboBox) source).getSelectedItem().equals(VALUE_OTHER_SPECIFY));
                final Window parentWindow = SwingUtilities.getWindowAncestor(otherSpecifyField);
                // don't force the re-packing if the window hasn't been shown/created yet (as in CSV read)
                if (parentWindow != null) {
                    parentWindow.pack();
                    parentWindow.validate();
                    parentWindow.repaint();
                }
            }
        }
    }
}
