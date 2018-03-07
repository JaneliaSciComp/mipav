import gov.nih.mipav.plugins.JDialogStandalonePlugin;

import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.LightboxGenerator;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBConcat;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoBase.UnitType;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.WidgetFactory;
import gov.nih.mipav.view.dialogs.JDialogBase;

import gov.nih.tbi.commons.model.*;
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
import java.util.zip.GZIPInputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.*;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.text.WordUtils;
import org.apache.cxf.jaxrs.client.WebClient;
import org.apache.cxf.transport.http.HTTPConduit;
import org.bouncycastle.util.encoders.Hex;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;

import com.ice.tar.TarEntry;
import com.ice.tar.TarInputStream;
import com.sun.jimi.core.Jimi;
import com.sun.jimi.core.JimiException;


public class PlugInDialogFITBIR extends JFrame implements ActionListener, ChangeListener, ItemListener, TreeSelectionListener, MouseListener,
        PreviewImageContainer, WindowListener, FocusListener {
    private static final long serialVersionUID = -5516621806537554154L;

    private final Font serif12 = MipavUtil.font12, serif12B = MipavUtil.font12B;

    private WidgetFactory.ScrollTextArea logOutputArea;

    private JScrollPane listPane;

    private JTable structTable;

    private JButton addStructButton, loadCSVButton, selectBIDSButton, finishButton, removeStructButton, editDataElementsButton, outputDirButton;

    private JTextField outputDirTextField;

    private ViewTableModel structTableModel;

    private String outputDirBase;

    private String csvFileDir;

    private String BIDSFileDir;

    private final Hashtable<RepeatableGroup, JPanel> groupPanelTable = new Hashtable<RepeatableGroup, JPanel>();

    private final Hashtable<RepeatableGroup, JButton> groupRemoveButtonTable = new Hashtable<RepeatableGroup, JButton>();

    private Hashtable<String, String> csvStructRowData;

    private static final String STRUCT_GUID_SEPERATOR = "_-_";

    private static final String CSV_OUTPUT_DELIM = ",";

    private static final String BROWSE_NONIMG_DELIM = ";;;;";

    private static final String MULTI_SELECT_VALUE_DELIM = ";";

    private static final int MULTI_SELECT_VISIBLE_ROWS = 5;

    private final ArrayList<ViewJComponentPreviewImage> previewImages = new ArrayList<ViewJComponentPreviewImage>();

    private final ArrayList<ImgFileInfo> structRowImgFileInfoList = new ArrayList<ImgFileInfo>();

    private final ArrayList<ArrayList<File>> allOtherFilesAL = new ArrayList<ArrayList<File>>();

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

    /** Property for reading the dd environment name from the brics config file. */
    private static final String ddEnvNameProp = "ddEnvName";

    /** Property for reading the dd server url from the brics config file. */
    private static final String ddServerURLProp = "ddServerURL";

    /** Property for reading the auth server url from the brics config file. */
    private static final String authServerURLProp = "authServerURL";

    /** Property for reading the dd authentication user name from the brics config file. */
    private static final String ddAuthUserProp = "ddAuthUser";

    /** Property for reading the dd authentication password from the brics config file. */
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

    private List<FormStructure> dataStructureList;

    private File csvFile;

    private File BIDSFile;

    private JPanel dsMainPanel;

    private String dataStructureName;

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

    private static final String svnVersion = "$Rev$";

    private static final String svnLastUpdate = "$Date$";

    private static final String pluginVersion = MipavUtil.getSVNChangedDate(svnLastUpdate);

    private static final String VALUE_OTHER_SPECIFY = "Other, specify";

    private static final String VALUE_YES_SPECIFY = "Yes, specify";

    private static final String ELEM_OTHER_SPECIFY_SUFFIX = "OTH";

    private static final String ELEM_YES_SPECIFY_SUFFIX = "ST";

    private static final String GUID_ELEMENT_NAME = "GUID";

    private static final String IMG_IMAGE_INFO_GROUP = "Image Information";

    private static final String IMG_FILE_ELEMENT_NAME = "ImgFile";

    private static final String IMG_PREVIEW_ELEMENT_NAME = "ImgPreviewFile";

    private static final String IMG_HASH_CODE_ELEMENT_NAME = "ImgFileHashCode";

    private static final String recordIndicatorColumn = "record";

    private static final String recordIndicatorValue = "x";

    private static final String[] PDBP_IMAGING_STRUCTURE_PREFIX_LIST = {"PDBPImag", "PDBP_Imag"};

    private static final String SITE_NAME_ELEMENT_NAME = "SiteName";

    private static final String[] PDBP_ALLOWED_SITE_NAMES = {
        "Brigham and Women's",
        "Cleveland Clinic",
        "Columbia University",
        "Emory University",
        "Florida Atlantic University",
        "Johns Hopkins University",
        "Mayo Clinic-FL",
        "Mayo Clinic-MN",
        "Northwestern University",
        "Pennsylvania State University (Hershey)",
        "Pacific Northwest National Laboratory",
        "Rush University",
        "Thomas Jefferson University",
        "University of Alabama (Birmingham)",
        "University of California San Diego",
        "University of Florida (Gainesville)",
        "University of Michigan",
        "University of North Carolina",
        "University of Pennsylvania",
        "University of Pittsburgh",
        "University of Washington",
        "UT-Southwestern Medical Center",
        "VA-Pugent Sound Health Care System/University of Washington",};

    private static final String[] allowedGuidPrefixes = new String[] {"TBI", "PD", "NEI"};

    private static final String[] imagingStructurePrefixes;
    static {
        imagingStructurePrefixes = new String[1 + PDBP_IMAGING_STRUCTURE_PREFIX_LIST.length];
        imagingStructurePrefixes[0] = "Imag";
        for (int i = 0; i < PDBP_IMAGING_STRUCTURE_PREFIX_LIST.length; i++) {
            imagingStructurePrefixes[i + 1] = PDBP_IMAGING_STRUCTURE_PREFIX_LIST[i];
        }
    }

    private FormStructureData fsData = null;

    private javax.swing.SwingWorker<Object, Object> fileWriterWorkerThread;

    private JLabel requiredLabel;

    private JDialog deidentDialog;

    public static final String[] anonymizeTagIDs = {
            // "0008,0014", // instance creator UID
            // "0008,0018", // SOP instance UID
            // "0008,0050", // accession number
            // "0008,0080", // institution name
            // "0008,0081", // institution address
            "0008,0090", // referring physician's name
            "0008,0092", // referring physician's address
            "0008,0094", // referring physician's telephone numbers
            // "0008,1010", // station name
            // "0008,1030", // study description
            // "0008,103E", // series description
            // "0008,1040", // institutional department name
            "0008,1048", // physician(s) of record
            "0008,1050", // performing physician's name
            "0008,1060", // name of physician reading study
            "0008,1070", // operator's name
            "0008,1080", // admitting diagnoses description
            // "0008,1155", // Referenced SOP instance UID
            "0008,2111", // derivation description

            "0010,0010", // patient name
            "0010,0020", // patient ID
            "0010,0030", // patient's birth date
            "0010,0032", // patient's birth time
            // "0010,0040", // patient's sex
            "0010,0050", // patient Insurance plan code sequence
            "0010,1000", // other patient IDs
            "0010,1001", // other patient names
            "0010,1005", // patient's birth name
            // "0010,1010", // patient's age
            // "0010,1020", // patient's size
            // "0010,1030", // patient's weight
            "0010,1040", // patient's address
            "0010,1060", // patient's mother's birth name
            "0010,1090", // medical record locator
            "0010,2154", // patient's telephone numbers
            // "0010,2160", // patient ethnic group
            "0010,2180", // occupation
            "0010,21B0", // additional patient's history
            "0010,21D0", // patient's last menstrual date
            "0010,21F0", // patient religious preference
            "0010,4000", // patient comments.

            // "0018,1000", // device serial number
            // "0018,1030", // protocol name

            // "0020,000D", // study instance UID
            // "0020,000E", // series instance UID
            // "0020,0010", // study ID
            // "0020,0052", // frame of reference UID
            // "0020,0200", // synchronization frame of reference UID
            "0020,4000", // image comments

            // "0040,0275",// request attributes sequence
            // "0040,A124", // UID
            "0040,A730", // content sequence

    // "0088,0140", // storage media file-set UID

    // "3006,0024", // referenced frame of reference UID
    // "3006,00C2", // related frame of reference UID
    };

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
        BIDSFileDir = Preferences.getProperty(Preferences.PREF_BRICS_PLUGIN_BIDS_DIR);
        if (BIDSFileDir == null) {
            BIDSFileDir = ViewUserInterface.getReference().getDefaultDirectory();
        }

        // try to read the server config from disk, if it is there.
        // otherwise the value set above at initialization is used.
        readConfig();

        init();
        setVisible(true);
        validate();

        final int response = JOptionPane.showConfirmDialog(this, PlugInDialogFITBIR.PRIVACY_NOTICE, "Image Submission Package Creation Tool",
                JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

        if (response == JOptionPane.NO_OPTION) {
            if (ViewUserInterface.getReference() != null && ViewUserInterface.getReference().isPlugInFrameVisible()) {
                System.gc();
                System.exit(0);
            } else {
                return;
            }
        }

        final Thread thread = new FormListRESTThread(this);
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
            chooser.setFileFilter(new FileNameExtensionFilter("Comma separated value files (.csv)", "csv"));
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                csvFile = chooser.getSelectedFile();
                readCSVFile();

                csvFileDir = chooser.getSelectedFile().getAbsolutePath() + File.separator;
                Preferences.setProperty(Preferences.PREF_BRICS_PLUGIN_CSV_DIR, csvFileDir);
            }
            listPane.setBorder(JDialogBase.buildTitledBorder(structTableModel.getRowCount() + " Form Structure(s) "));
        } else if (command.equalsIgnoreCase("SelectBIDS")) {
            final JFileChooser chooser = new JFileChooser();
            chooser.setCurrentDirectory(new File(BIDSFileDir));
            chooser.setDialogTitle("Choose root BIDS directory");
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            chooser.setMultiSelectionEnabled(false);
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                BIDSFile = chooser.getSelectedFile();
                processBIDSDirectories(false);

                BIDSFileDir = chooser.getSelectedFile().getAbsolutePath() + File.separator;
                Preferences.setProperty(Preferences.PREF_BRICS_PLUGIN_BIDS_DIR, BIDSFileDir);
            }
            listPane.setBorder(JDialogBase.buildTitledBorder(structTableModel.getRowCount() + " Form Structure(s) "));
        } else if (command.equalsIgnoreCase("RemoveStruct")) {
            final int selected = structTable.getSelectedRow();
            structTableModel.removeRow(selected);
            previewImages.remove(selected);
            structRowImgFileInfoList.remove(selected);
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
                            createSubmissionFiles();
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
                    if (struct.endsWith(STRUCT_GUID_SEPERATOR + "UNKNOWNGUID")) {
                        validGuids = -1;
                        break;
                    } else {
                        final String guidTester = struct.substring(struct.lastIndexOf(STRUCT_GUID_SEPERATOR) + 3, struct.length() - 1);
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
                    selectBIDSButton.setEnabled(false);

                    fileWriterWorkerThread.execute();
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
        } else if (command.equalsIgnoreCase("okayPII")) {
            // continue w/ image load. close PII de-id dialog
            if (deidentDialog != null) {
                deidentDialog.dispose();
                deidentDialog = null;
            }
        } else if (command.equalsIgnoreCase("cancelPII")) {
            // PII not de-id, close everything
            windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
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

    private boolean processBIDSDirectories(final boolean setInitialVisible) {
        int numberSubjects = 0;
        File[] files;
        File[] files2;
        File subjectFiles[];
        int i;
        int j;
        int k;
        int m;
        int n;
        int p;
        int numberSessions[];
        int maximumNumberSessions = 0;
        File sessionFiles[][];
        int anatNumber = 0;
        int funcNumber = 0;
        int dwiNumber = 0;
        int fmapNumber = 0;
        int numberSessionDirectoryTSV[][] = null;
        int numberSessionDirectoryScansTSV[][] = null;
        int numberSubjectDirectoryTSV[];
        int numberSubjectDirectoryScansTSV[];
        int numberSubjectDirectorySessionsTSV[];
        int totalSessionTSVFiles = 0;
        int totalSessionScansTSVFiles = 0;
        int totalSubjectTSVFiles = 0;
        int totalSubjectScansTSVFiles = 0;
        int totalSubjectSessionsTSVFiles = 0;
        File anatFiles[][][] = null;
        File funcFiles[][][] = null;
        File dwiFiles[][][] = null;
        File fmapFiles[][][] = null;
        File tsvSessionDirectoryFiles[][][] = null;
        File scanstsvSessionDirectoryFiles[][][] = null;
        File tsvSubjectDirectoryFiles[][] = null;
        File scanstsvSubjectDirectoryFiles[][] = null;
        File sessionstsvSubjectDirectoryFiles[][] = null;
        boolean found;
        FormStructure ds = null;
        FormStructure dsInfo = null;

        final int maximumSessionFileNumber = 15;
        final ModelImage srcImage[] = new ModelImage[maximumSessionFileNumber];
        final File jsonFile[] = new File[maximumSessionFileNumber];
        final File bvalFile[] = new File[maximumSessionFileNumber];
        final File bvecFile[] = new File[maximumSessionFileNumber];
        final File eventsFile[] = new File[maximumSessionFileNumber];
        final File physioTsvFile[] = new File[maximumSessionFileNumber];
        final File physioJsonFile[] = new File[maximumSessionFileNumber];
        final FileIO fileIO = new FileIO();
        int anatImagesRead;
        int anatJsonRead;
        int funcImagesRead;
        int funcJsonRead;
        int funcEventsRead;
        int funcPhysioTsvRead;
        int funcPhysioJsonRead;
        int dwiImagesRead;
        int dwiJsonRead;
        int dwiBvalRead;
        int dwiBvecRead;
        int index;
        String baseName;
        String jsonName;
        String bvalName;
        String bvecName;
        String eventsBaseName;
        String eventsName;
        int sessionImagesRead;
        int sessionJsonRead;
        int sessionBvalRead;
        int sessionBvecRead;
        int sessionEventsRead;
        int sessionPhysioTsvRead;
        int sessionPhysioJsonRead;
        File participantsFile = null;
        RandomAccessFile raFile = null;
        boolean processFile;
        String line = null;
        boolean readMoreLines;
        String tokens[];
        int participant_id_index;
        int age_index;
        String participant_id_array[] = null;
        String age_array[] = null;
        String age = null;
        int participant_id_read = 0;
        int age_read = 0;
        int subjectsRead = 0;
        long fileLength = 0;
        long filePos;
        boolean indexRead;
        int subject_id_index;
        String subject_id_array[] = null;
        String subject_id = null;
        int headerTokenNumber;
        int sessionTokenNumber;
        int sessionsRead;
        int subject_id_read = 0;
        int numberBoldJsonFiles = 0;
        String boldJsonFilenames[] = null;
        String boldJsonPathnames[] = null;
        int numberPhysioJsonFiles = 0;
        String physioJsonFilenames[] = null;
        String physioJsonPathnames[] = null;
        byte bufferByte[] = null;
        String jsonString;
        String openBracket;
        String closeBracket;
        JSONObject jsonObject = null;
        double effectiveEchoSpacing[] = null;
        double echoTime[] = null;
        String BIDSString = null;
        String fullPath = null;
        String dwibvalString = null;
        String dwibvecString = null;
        // The JSON file gives the time in seconds between the beginning of an acquisition of one volume and the
        // beginning of the acquisition of the volume following it (TR). Pleases note that this definition includes
        // time between scans (when no data has been acquired) in case of sparse acquisition schemes. This value
        // needs to be consistent with the 'pixdim[4]' field (after accounting for units stored in 'xyzt_units'
        // field) in the NIFTI header.
        double repetitionTime[] = null;
        // Possible values, "i", "j", "k", "i-", "j-", "k-". The letters "i", "j", "k" correspond to the first, second,
        // and third, axis of the data in the NIFTI file. The polarity of the phase encoding is assumed to go from zero
        // index to maximum index unless '-' sign is present(then the order is reversed - starting from highest index
        // instead of zero).
        // PhaseEncodingDirection is defined as the direction along which phase was modulated which may result in
        // visible distortions. Note that this is not the same as the DICOM term inPlanePhaseEncodingDirection
        // which can have "ROW" or "COL" values.
        String phaseEncodingDirection[] = null;
        String imagingFMRIAuxiliaryFile[] = null;
        int fMRIAuxiliaryFileNumber = 0;
        int numberEventsTSVFiles = 0;
        String eventsTSVFilenames[] = null;
        String eventsTSVPathnames[] = null;
        int subdirectoriesFound = 0;
        int subdirectoriesRead = 0;
        int pValue;
        final JPanel mainPanel = new JPanel(new GridBagLayout());

        fileIO.setQuiet(true);
        final ViewJProgressBar progressBar = new ViewJProgressBar("Reading BIDS directories", "Reading BIDS directories...", 0, 100, false);
        progressBar.setVisible(true);
        progressBar.updateValue(5);

        dsMainPanel = new JPanel(new GridBagLayout());
        final JScrollPane tabScrollPane = new JScrollPane(dsMainPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        final GridBagConstraints gbc = new GridBagConstraints();

        setTitle("Edit Data Elements - " + dataStructureName);
        addWindowListener(this);

        try {
            setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (final Exception e) {
            // setIconImage() is not part of the Java 1.5 API - catch any
            // runtime error on those systems
        }
        // Read directory and find no. of images
        files = BIDSFile.listFiles();
        BIDSString = BIDSFile.getName();
        Arrays.sort(files, new fileComparator());
        for (i = 0; i < files.length; i++) {
            if ( (files[i].isDirectory()) && (files[i].getName().substring(0, 3).equalsIgnoreCase("SUB"))) {
                numberSubjects++;
            } else if ( (files[i].isFile()) && (files[i].getName().equalsIgnoreCase("participants.tsv"))) {
                participantsFile = files[i];
            } else if ( (files[i].isFile()) && (files[i].getName().toLowerCase().endsWith("_bold.json"))) {
                numberBoldJsonFiles++;
            } else if ( (files[i].isFile()) && (files[i].getName().toLowerCase().endsWith("_physio.json"))) {
                numberPhysioJsonFiles++;
            } else if ( (files[i].isFile()) && (files[i].getName().toLowerCase().endsWith("_events_tsv"))) {
                numberEventsTSVFiles++;
            } else if ( (files[i].isFile()) && (files[i].getName().equalsIgnoreCase("dwi.bval"))) {
                fullPath = files[i].getAbsolutePath();
                index = fullPath.indexOf(BIDSString);
                dwibvalString = fullPath.substring(index);
                printlnToLog("dwi.bval read");

            } else if ( (files[i].isFile()) && (files[i].getName().equalsIgnoreCase("dwi.bvec"))) {
                fullPath = files[i].getAbsolutePath();
                index = fullPath.indexOf(BIDSString);
                dwibvecString = fullPath.substring(index);
                printlnToLog("dwi.bvec read");
            }
        }
        if (numberEventsTSVFiles > 0) {
            eventsTSVFilenames = new String[numberEventsTSVFiles];
            eventsTSVPathnames = new String[numberEventsTSVFiles];
            for (i = 0, j = 0; i < files.length; i++) {
                if ( (files[i].isFile()) && (files[i].getName().toLowerCase().endsWith("_events_tsv"))) {
                    index = files[i].getName().toLowerCase().indexOf("_events_tsv");
                    eventsTSVFilenames[j] = files[i].getName().substring(0, index);
                    fullPath = files[i].getAbsolutePath();
                    index = fullPath.indexOf(BIDSString);
                    eventsTSVPathnames[j++] = fullPath.substring(index);
                } // if ((files[i].isFile()) && (files[i].getName().toLowerCase().endsWith("_events_tsv")))
            } // for (i = 0; i < files.length; i++)
        } // if (numberEventsTSVFiles > 0)
        if (numberBoldJsonFiles > 0) {
            boldJsonFilenames = new String[numberBoldJsonFiles];
            boldJsonPathnames = new String[numberBoldJsonFiles];
            effectiveEchoSpacing = new double[numberBoldJsonFiles];
            echoTime = new double[numberBoldJsonFiles];
            repetitionTime = new double[numberBoldJsonFiles];
            phaseEncodingDirection = new String[numberBoldJsonFiles];
            for (i = 0; i < numberBoldJsonFiles; i++) {
                effectiveEchoSpacing[i] = Double.NaN;
                echoTime[i] = Double.NaN;
                repetitionTime[i] = Double.NaN;
            }
            for (i = 0, j = 0; i < files.length; i++) {
                if ( (files[i].isFile()) && (files[i].getName().toLowerCase().endsWith("_bold.json"))) {
                    index = files[i].getName().lastIndexOf("_bold.json");
                    boldJsonFilenames[j] = files[i].getName().substring(0, index);
                    fullPath = files[i].getAbsolutePath();
                    index = fullPath.indexOf(BIDSString);
                    boldJsonPathnames[j] = fullPath.substring(index);
                    try {
                        raFile = new RandomAccessFile(files[i], "r");
                        processFile = true;
                    } catch (final FileNotFoundException e) {
                        System.err.println("FileNotFoundException " + e);
                        processFile = false;
                    }
                    if (processFile) {
                        try {
                            fileLength = raFile.length();
                        } catch (final IOException e) {
                            System.err.println("IOException " + e);
                            processFile = false;
                        }
                    } // if (processFile)
                    if (processFile) {
                        bufferByte = new byte[(int) fileLength];
                        try {
                            raFile.read(bufferByte);
                        } catch (final IOException e) {
                            System.err.println("IOException " + e);
                            processFile = false;
                        }
                    } // if (processFile)
                    if (processFile) {
                        openBracket = new String(bufferByte, 0, 1);
                        if (openBracket.equals("{")) {
                            // Padded to 16 bytes, so could be a }, }<sp>, }<sp><sp>, or }<sp><sp><sp>, etc.
                            closeBracket = new String(bufferByte, (int) (fileLength - 17), 16);
                            if (closeBracket.trim().endsWith("}")) {
                                processFile = true;
                            } else {
                                processFile = false;
                            }
                        } else {
                            processFile = false;
                        }
                    } // if (processFile)
                    if (processFile) {
                        jsonString = new String(bufferByte, 0, (int) fileLength);
                        try {
                            jsonObject = new JSONObject(jsonString);
                        } catch (final JSONException e) {
                            System.err.println("JSONException " + e + " on new JSONObject(jsonString)");
                            processFile = false;
                        }
                    } // if (processFile)
                    if (processFile) {
                        try {
                            // Change seconds to milliseconds
                            effectiveEchoSpacing[j] = 1000.0 * jsonObject.getDouble("EffectiveEchoSpacing");
                        } catch (final JSONException e) {
                            System.err.println("JSONException " + e + " jsonObject.getDouble EffectiveEchoSpacing");
                        }
                        try {
                            echoTime[j] = 1000.0 * jsonObject.getDouble("EchoTime");
                        } catch (final JSONException e) {
                            System.err.println("JSONException " + e + " jsonObject.getDouble EchoTime");
                        }
                        try {
                            repetitionTime[j] = 1000.0 * jsonObject.getDouble("RepetitionTime");
                        } catch (final JSONException e) {
                            System.err.println("JSONException " + e + "jsonObject.getDouble RepetitionTime");
                        }
                        try {
                            phaseEncodingDirection[j] = jsonObject.getString("PhaseEncodingDirection");
                        } catch (final JSONException e) {
                            System.err.println("JSONException " + e + " jsonObject.getString PhaseEncodingDirection");
                        }
                    } // if (processFile)
                    j++;
                    try {
                        raFile.close();
                    } catch (final IOException e) {
                        System.err.println(" IOException " + e + " on raFile.close()");
                    }
                }
            }
        } // if (numberBoldJsonFiles > 0)
        if (numberPhysioJsonFiles > 0) {
            physioJsonFilenames = new String[numberPhysioJsonFiles];
            physioJsonPathnames = new String[numberPhysioJsonFiles];
            for (i = 0, j = 0; i < files.length; i++) {
                if ( (files[i].isFile()) && (files[i].getName().toLowerCase().endsWith("_physio.json"))) {
                    index = files[i].getName().lastIndexOf("_physio.json");
                    physioJsonFilenames[j] = files[i].getName().substring(0, index);
                    fullPath = files[i].getAbsolutePath();
                    index = fullPath.indexOf(BIDSString);
                    physioJsonPathnames[j++] = fullPath.substring(index);
                }
            }
        } // if (numberPhysioJsonFiles > 0)
        printlnToLog("Number of subjects = " + numberSubjects);
        if (numberSubjects == 0) {
            return false;
        }
        if (participantsFile != null) {
            try {
                raFile = new RandomAccessFile(participantsFile, "r");
                processFile = true;
            } catch (final FileNotFoundException e) {
                System.err.println("FileNotFoundException " + e);
                processFile = false;
            }
            if (processFile) {
                try {
                    fileLength = raFile.length();
                } catch (final IOException e) {
                    System.err.println("IOException " + e);
                    processFile = false;
                }
            } // if (processFile)
            if (processFile) {
                try {
                    line = raFile.readLine();
                    readMoreLines = true;
                } catch (final IOException e) {
                    System.err.println("IOException " + e);
                    readMoreLines = false;
                }
                if (readMoreLines) {
                    tokens = line.split("\t");
                    participant_id_index = -1;
                    age_index = -1;
                    for (i = 0; i < tokens.length; i++) {
                        if ( (tokens[i].equalsIgnoreCase("participant_id")) || (tokens[i].equalsIgnoreCase("subject_id"))) {
                            participant_id_index = i;
                            participant_id_array = new String[numberSubjects];
                        } else if ( (tokens[i].equalsIgnoreCase("age_at_first_scan_years")) || (tokens[i].equalsIgnoreCase("age"))) {
                            age_index = i;
                            age_array = new String[numberSubjects];
                        }
                    } // // for (i = 0; i < tokens.length; i++)
                    if ( (participant_id_index >= 0) || (age_index >= 0)) {
                        subjectsRead = 0;
                        try {
                            filePos = raFile.getFilePointer();
                        } catch (final IOException e) {
                            System.err.println("IOException " + e);
                            filePos = fileLength;
                        }
                        while (readMoreLines && (subjectsRead < numberSubjects) && (filePos < fileLength)) {
                            try {
                                line = raFile.readLine();
                                readMoreLines = true;
                            } catch (final IOException e) {
                                System.err.println("IOException " + e);
                                readMoreLines = false;
                            }
                            tokens = line.split("\t");
                            indexRead = false;
                            if ( (participant_id_index >= 0) && (tokens.length - 1 >= participant_id_index)) {
                                participant_id_array[subjectsRead] = tokens[participant_id_index];
                                indexRead = true;
                                participant_id_read++;
                            }
                            if ( (age_index >= 0) && (tokens.length - 1 >= age_index)) {
                                age_array[subjectsRead] = tokens[age_index];
                                indexRead = true;
                                age_read++;
                            }
                            if (indexRead) {
                                subjectsRead++;
                            }
                            try {
                                filePos = raFile.getFilePointer();
                            } catch (final IOException e) {
                                System.err.println("IOException " + e);
                                filePos = fileLength;
                            }
                        } // while (readMoreLines && (subjectsRead < numberSubjects) && (filePos < fileLength))
                    } // if ((participant_id_index >= 0)|| (age_at_first_scan_years_index >= 0))
                } // if (readMoreLines)
                printlnToLog(participant_id_read + " participant_id read from participants.tsv");
                printlnToLog(age_read + " age or age_at_first_scan_years read from participants.tsv");
            } // if (processFile)
            if (processFile) {
                try {
                    raFile.close();
                } catch (final IOException e) {
                    System.err.println("IOException " + e);
                }
            }
        } // if (participantsFile != null)
        subjectFiles = new File[numberSubjects];
        for (i = 0, j = 0; i < files.length; i++) {
            if ( (files[i].isDirectory()) && (files[i].getName().substring(0, 3).equalsIgnoreCase("SUB"))) {
                subjectFiles[j++] = files[i];
            }
        }
        numberSessions = new int[numberSubjects];
        numberSubjectDirectoryTSV = new int[numberSubjects];
        numberSubjectDirectoryScansTSV = new int[numberSubjects];
        numberSubjectDirectorySessionsTSV = new int[numberSubjects];
        for (i = 0; i < numberSubjects; i++) {
            files = subjectFiles[i].listFiles();
            Arrays.sort(files, new fileComparator());
            for (j = 0; j < files.length; j++) {
                if ( (files[j].isDirectory()) && (files[j].getName().substring(0, 3).equalsIgnoreCase("SES"))) {
                    numberSessions[i]++;
                } else if ( (files[j].isFile()) && (files[j].getName().toLowerCase().endsWith(".tsv"))) {
                    numberSubjectDirectoryTSV[i]++;
                    totalSubjectTSVFiles++;
                    if (files[j].getName().toLowerCase().endsWith("_scans.tsv")) {
                        numberSubjectDirectoryScansTSV[i]++;
                        totalSubjectScansTSVFiles++;
                    } else if (files[j].getName().toLowerCase().endsWith("_sessions.tsv")) {
                        numberSubjectDirectorySessionsTSV[i]++;
                        totalSubjectSessionsTSVFiles++;
                    }
                }
            }
            if (numberSessions[i] > maximumNumberSessions) {
                maximumNumberSessions = numberSessions[i];
            }
        }
        printlnToLog("Maximum number of sessions for any subject = " + maximumNumberSessions);
        if (maximumNumberSessions > 0) {
            for (i = 0; i < numberSubjects; i++) {
                if (numberSessions[i] == 0) {
                    printlnToLog("Illegal skipped session for subject " + subjectFiles[i].getName());
                }
            }
        }
        sessionFiles = new File[numberSubjects][];
        tsvSubjectDirectoryFiles = new File[numberSubjects][];
        scanstsvSubjectDirectoryFiles = new File[numberSubjects][];
        sessionstsvSubjectDirectoryFiles = new File[numberSubjects][];
        if (maximumNumberSessions == 0) {
            for (i = 0; i < numberSubjects; i++) {
                sessionFiles[i] = new File[1];
                sessionFiles[i][0] = subjectFiles[i];
                tsvSubjectDirectoryFiles[i] = new File[numberSubjectDirectoryTSV[i]];
                scanstsvSubjectDirectoryFiles[i] = new File[numberSubjectDirectoryScansTSV[i]];
                sessionstsvSubjectDirectoryFiles[i] = new File[numberSubjectDirectorySessionsTSV[i]];
                files = subjectFiles[i].listFiles();
                for (j = 0, k = 0, m = 0, n = 0; j < files.length; j++) {
                    if (files[j].isFile() && (files[j].getName().toLowerCase().endsWith(".tsv"))) {
                        tsvSubjectDirectoryFiles[i][k++] = files[j];
                        if (files[j].getName().toLowerCase().endsWith("_scans.tsv")) {
                            scanstsvSubjectDirectoryFiles[i][m++] = files[j];
                        } else if (files[j].getName().toLowerCase().endsWith("_sessions.tsv")) {
                            sessionstsvSubjectDirectoryFiles[i][n++] = files[j];
                        }
                    }
                }
            }
        } // if (maximumNumberSessions == 0)
        else { // maximumNumberSessions > 0
            for (i = 0; i < numberSubjects; i++) {
                sessionFiles[i] = new File[numberSessions[i]];
                tsvSubjectDirectoryFiles[i] = new File[numberSubjectDirectoryTSV[i]];
                scanstsvSubjectDirectoryFiles[i] = new File[numberSubjectDirectoryScansTSV[i]];
                sessionstsvSubjectDirectoryFiles[i] = new File[numberSubjectDirectorySessionsTSV[i]];
                files = subjectFiles[i].listFiles();
                for (j = 0, k = 0, m = 0, n = 0, p = 0; j < files.length; j++) {
                    if ( (files[j].isDirectory()) && (files[j].getName().substring(0, 3).equalsIgnoreCase("SES"))) {
                        sessionFiles[i][k++] = files[j];
                    } else if ( (files[j].isFile()) && (files[j].getName().toLowerCase().endsWith(".tsv"))) {
                        tsvSubjectDirectoryFiles[i][m++] = files[j];
                        if (files[j].getName().toLowerCase().endsWith("_scans.tsv")) {
                            scanstsvSubjectDirectoryFiles[i][n++] = files[j];
                        } else if (files[j].getName().toLowerCase().endsWith("_sessions.tsv")) {
                            sessionstsvSubjectDirectoryFiles[i][p++] = files[j];
                        }
                    }
                }
            }
        } // else maximumNumberSessions > 0

        if (totalSubjectSessionsTSVFiles > 0) {
            subject_id_array = new String[numberSubjects];
            for (i = 0; i < numberSubjects; i++) {
                for (j = 0; j < sessionstsvSubjectDirectoryFiles[i].length; j++) {
                    try {
                        raFile = new RandomAccessFile(tsvSubjectDirectoryFiles[i][j], "r");
                        processFile = true;
                    } catch (final FileNotFoundException e) {
                        System.err.println("FileNotFoundException " + e);
                        processFile = false;
                    }
                    if (processFile) {
                        try {
                            fileLength = raFile.length();
                        } catch (final IOException e) {
                            System.err.println("IOException " + e);
                            processFile = false;
                        }
                    } // if (processFile)
                    if (processFile) {
                        try {
                            line = raFile.readLine();
                            readMoreLines = true;
                        } catch (final IOException e) {
                            System.err.println("IOException " + e);
                            readMoreLines = false;
                        }
                        if (readMoreLines) {
                            tokens = line.split("\t");
                            headerTokenNumber = tokens.length;
                            subject_id_index = -1;
                            for (k = 0; k < tokens.length; k++) {
                                if (tokens[k].equalsIgnoreCase("subject_id")) {
                                    subject_id_index = k;
                                }
                            } // // for (k = 0; k < tokens.length; k++)
                            if (subject_id_index >= 0) {
                                try {
                                    filePos = raFile.getFilePointer();
                                } catch (final IOException e) {
                                    System.err.println("IOException " + e);
                                    filePos = fileLength;
                                }
                                sessionsRead = 0;
                                indexRead = false;
                                while (readMoreLines && (sessionsRead < sessionFiles[i].length) && (filePos < fileLength)) {
                                    try {
                                        line = raFile.readLine();
                                        readMoreLines = true;
                                    } catch (final IOException e) {
                                        System.err.println("IOException " + e);
                                        readMoreLines = false;
                                    }
                                    tokens = line.split("\t");
                                    sessionTokenNumber = tokens.length;
                                    if (headerTokenNumber != sessionTokenNumber) {
                                        System.err.println("Header token number = " + headerTokenNumber + ", but session token number = " + sessionTokenNumber);
                                    }
                                    if (tokens.length - 1 >= subject_id_index) {
                                        if ( !indexRead) {
                                            subject_id_array[i] = tokens[subject_id_index];
                                            indexRead = true;
                                            subject_id_read++;
                                        } else if (Integer.valueOf(subject_id_array[i]).intValue() != Integer.valueOf(tokens[subject_id_index]).intValue()) {
                                            System.err.println("subject_id number varies across sessions for subject subdirectory " + i);
                                            System.err.println("subject_id_array[" + i + "] = " + subject_id_array[i]);
                                            System.err.println("tokens[" + subject_id_index + "] = " + tokens[subject_id_index]);
                                        }
                                        sessionsRead++;
                                    }

                                    try {
                                        filePos = raFile.getFilePointer();
                                    } catch (final IOException e) {
                                        System.err.println("IOException " + e);
                                        filePos = fileLength;
                                    }
                                } // while (readMoreLines && (sessionsRead < sessionFiles[i].length) && (filePos <
                                  // fileLength))
                            } // if (subject_id_index >= 0)
                        } // if (readMoreLines)
                    } // if (processFile)
                    if (processFile) {
                        try {
                            raFile.close();
                        } catch (final IOException e) {
                            System.err.println("IOException " + e);
                        }
                    }
                }
            }
            printlnToLog(subject_id_read + " subject id read from _sessions.tsv files in subject subdirectories");
        } // if (totalSubjectSessionsTSVFiles > 0)

        if (maximumNumberSessions > 0) {
            numberSessionDirectoryTSV = new int[numberSubjects][];
            numberSessionDirectoryScansTSV = new int[numberSubjects][];
        }
        for (i = 0; i < numberSubjects; i++) {
            if (maximumNumberSessions > 0) {
                numberSessionDirectoryTSV[i] = new int[numberSessions[i]];
                numberSessionDirectoryScansTSV[i] = new int[numberSessions[i]];
            }
            for (j = 0; j < sessionFiles[i].length; j++) {
                files = sessionFiles[i][j].listFiles();
                for (k = 0; k < files.length; k++) {
                    if (files[k].isDirectory()) {
                        if ( (files[k].getName().equalsIgnoreCase("ANAT")) && (files[k].listFiles().length > 0)) {
                            anatNumber++;
                        } else if ( (files[k].getName().equalsIgnoreCase("FUNC")) && (files[k].listFiles().length > 0)) {
                            funcNumber++;
                        } else if ( (files[k].getName().equalsIgnoreCase("DWI")) && (files[k].listFiles().length > 0)) {
                            dwiNumber++;
                        } else if ( (files[k].getName().equalsIgnoreCase("FMAP")) && (files[k].listFiles().length > 0)) {
                            fmapNumber++;
                        }
                    } // if (files[k].isDirectory())
                    else if ( (files[k].getName().toLowerCase().endsWith(".tsv")) && (maximumNumberSessions > 0)) {
                        numberSessionDirectoryTSV[i][j]++;
                        totalSessionTSVFiles++;
                        if (files[k].getName().toLowerCase().endsWith("_scans.tsv")) {
                            numberSessionDirectoryScansTSV[i][j]++;
                            totalSessionScansTSVFiles++;
                        }
                    }
                }
            }
        }
        // subdirectoriesFound = anatNumber + funcNumber + dwiNumber + fmapNumber;
        subdirectoriesFound = anatNumber + funcNumber + dwiNumber;

        printlnToLog(anatNumber + " nonempty anat subdirectories were found");
        printlnToLog(funcNumber + " nonempty func subdirectories were found");
        printlnToLog(dwiNumber + " nonempty dwi subdirectories were found");
        printlnToLog(fmapNumber + " nonempty fmap subdirectories were found");
        printlnToLog("Number of subject subdirectory .tsv files = " + totalSubjectTSVFiles);
        printlnToLog("Number of subject subdirectory _scans.tsv files = " + totalSubjectScansTSVFiles);
        printlnToLog("Number of subject subdirectory _sessions.tsv files = " + totalSubjectSessionsTSVFiles);
        printlnToLog("Number of session subdirectory .tsv files = " + totalSessionTSVFiles);
        printlnToLog("Number of session subdirectory _scans.tsv files = " + totalSessionScansTSVFiles);

        if (totalSessionTSVFiles > 0) {
            tsvSessionDirectoryFiles = new File[numberSubjects][][];
            for (i = 0; i < numberSubjects; i++) {
                tsvSessionDirectoryFiles[i] = new File[sessionFiles[i].length][];
                for (j = 0; j < sessionFiles[i].length; j++) {
                    files = sessionFiles[i][j].listFiles();
                    tsvSessionDirectoryFiles[i][j] = new File[numberSessionDirectoryTSV[i][j]];
                    for (k = 0, m = 0; k < files.length; k++) {
                        if ( (files[k].isFile()) && (files[k].getName().toLowerCase().endsWith(".tsv"))) {
                            tsvSessionDirectoryFiles[i][j][m++] = files[k];
                        }
                    }
                }
            }
        } // if (totalSessionTSVFiles > 0)

        if (totalSessionScansTSVFiles > 0) {
            scanstsvSessionDirectoryFiles = new File[numberSubjects][][];
            for (i = 0; i < numberSubjects; i++) {
                scanstsvSessionDirectoryFiles[i] = new File[sessionFiles[i].length][];
                for (j = 0; j < sessionFiles[i].length; j++) {
                    files = sessionFiles[i][j].listFiles();
                    scanstsvSessionDirectoryFiles[i][j] = new File[numberSessionDirectoryScansTSV[i][j]];
                    for (k = 0, m = 0; k < files.length; k++) {
                        if ( (files[k].isFile()) && (files[k].getName().toLowerCase().endsWith("_scans.tsv"))) {
                            scanstsvSessionDirectoryFiles[i][j][m++] = files[k];
                        }
                    }
                }
            }
        } // if (totalSessionScansTSVFiles > 0)

        if (anatNumber > 0) {
            anatFiles = new File[numberSubjects][][];
            for (i = 0; i < numberSubjects; i++) {
                anatFiles[i] = new File[sessionFiles[i].length][];
                for (j = 0; j < sessionFiles[i].length; j++) {
                    files = sessionFiles[i][j].listFiles();
                    for (k = 0; k < files.length; k++) {
                        if ( (files[k].isDirectory()) && (files[k].getName().equalsIgnoreCase("ANAT"))) {
                            files2 = files[k].listFiles();
                            anatFiles[i][j] = new File[files2.length];
                            for (m = 0; m < files2.length; m++) {
                                anatFiles[i][j][m] = files2[m];
                            }
                            Arrays.sort(anatFiles[i][j], new fileComparator());
                        }
                    }
                }
            }
        } // if (anatNumber > 0)

        if (funcNumber > 0) {
            funcFiles = new File[numberSubjects][][];
            for (i = 0; i < numberSubjects; i++) {
                funcFiles[i] = new File[sessionFiles[i].length][];
                for (j = 0; j < sessionFiles[i].length; j++) {
                    files = sessionFiles[i][j].listFiles();
                    for (k = 0; k < files.length; k++) {
                        if ( (files[k].isDirectory()) && (files[k].getName().equalsIgnoreCase("FUNC"))) {
                            files2 = files[k].listFiles();
                            funcFiles[i][j] = new File[files2.length];
                            for (m = 0; m < files2.length; m++) {
                                funcFiles[i][j][m] = files2[m];
                            }
                            Arrays.sort(funcFiles[i][j], new fileComparator());
                        }
                    }
                }
            }
        } // if (funcNumber > 0)

        if (dwiNumber > 0) {
            dwiFiles = new File[numberSubjects][][];
            for (i = 0; i < numberSubjects; i++) {
                dwiFiles[i] = new File[sessionFiles[i].length][];
                for (j = 0; j < sessionFiles[i].length; j++) {
                    files = sessionFiles[i][j].listFiles();
                    for (k = 0; k < files.length; k++) {
                        if ( (files[k].isDirectory()) && (files[k].getName().equalsIgnoreCase("DWI"))) {
                            files2 = files[k].listFiles();
                            dwiFiles[i][j] = new File[files2.length];
                            for (m = 0; m < files2.length; m++) {
                                dwiFiles[i][j][m] = files2[m];
                            }
                            Arrays.sort(dwiFiles[i][j], new fileComparator());
                        }
                    }
                }
            }
        } // if (dwiNumber > 0)

        if (fmapNumber > 0) {
            fmapFiles = new File[numberSubjects][][];
            for (i = 0; i < numberSubjects; i++) {
                fmapFiles[i] = new File[sessionFiles[i].length][];
                for (j = 0; j < sessionFiles[i].length; j++) {
                    files = sessionFiles[i][j].listFiles();
                    for (k = 0; k < files.length; k++) {
                        if ( (files[k].isDirectory()) && (files[k].getName().equalsIgnoreCase("FMAP"))) {
                            files2 = files[k].listFiles();
                            fmapFiles[i][j] = new File[files2.length];
                            for (m = 0; m < files2.length; m++) {
                                fmapFiles[i][j][m] = files2[m];
                            }
                            Arrays.sort(fmapFiles[i][j], new fileComparator());
                        }
                    }
                }
            }
        } // if (fmapNumber > 0)

        progressBar.updateValue(20);
        if (anatNumber > 0) {
            found = false;
            for (i = 0; i < dataStructureList.size() && ( !found); i++) {
                if (dataStructureList.get(i).getShortName().equalsIgnoreCase("ImagingMR")) {
                    found = true;
                    ds = dataStructureList.get(i);
                    dataStructureName = "ImagingMR";
                    if (ds.getDataElements().size() == 0) {
                        progressBar.setMessage("Retrieving data elements for form structure: " + ds.getShortName());
                        final FormDataElementsRESTThread thread = new FormDataElementsRESTThread(this, ds.getShortName(), false);
                        thread.run();

                        dsInfo = thread.getFullFormStructure();
                    } else {
                        dsInfo = ds;
                    }
                }
            }
            anatImagesRead = 0;
            anatJsonRead = 0;
            for (i = 0; i < numberSubjects; i++) {
                for (j = 0; j < anatFiles[i].length; j++) {
                    pValue = 20 + 80 * subdirectoriesRead / subdirectoriesFound;
                    progressBar.updateValue(pValue);
                    if ( (anatFiles[i][j] != null) && (anatFiles[i][j].length > 0)) {
                        subdirectoriesRead++;
                        sessionImagesRead = 0;
                        sessionJsonRead = 0;
                        previewImages.add(null);
                        structRowImgFileInfoList.add(null);
                        fsDataList.add(null);
                        allOtherFilesAL.add(null);
                        fsData = new FormStructureData(dsInfo);
                        for (k = 0; k < anatFiles[i][j].length; k++) {
                            if ( (anatFiles[i][j][k].getName().endsWith("nii.gz")) || (anatFiles[i][j][k].getName().endsWith(".nii"))) {
                                progressBar.setMessage("Reading " + anatFiles[i][j][k].getName());
                                progressBar.updateValue(pValue + k * 80 / (subdirectoriesFound * anatFiles[i][j].length));
                                if (anatFiles[i][j][k].getName().endsWith("nii.gz")) {
                                    srcImage[sessionImagesRead] = fileIO.readNIFTI(anatFiles[i][j][k].getName(), anatFiles[i][j][k].getParentFile()
                                            .getAbsolutePath(), false, true, true, true);
                                    index = anatFiles[i][j][k].getName().indexOf("nii.gz");
                                } else {
                                    srcImage[sessionImagesRead] = fileIO.readNIFTI(anatFiles[i][j][k].getName(), anatFiles[i][j][k].getParentFile()
                                            .getAbsolutePath(), false, false, true, true);
                                    index = anatFiles[i][j][k].getName().indexOf("nii");
                                }
                                anatImagesRead++;
                                baseName = anatFiles[i][j][k].getName().substring(0, index);
                                jsonName = baseName + "json";
                                found = false;
                                for (n = 0; n < anatFiles[i][j].length && ( !found); n++) {
                                    if (anatFiles[i][j][n].getName().equals(jsonName)) {
                                        found = true;
                                        jsonFile[sessionImagesRead] = anatFiles[i][j][n];
                                        anatJsonRead++;
                                        sessionJsonRead++;
                                    }
                                }
                                sessionImagesRead++;
                            } // if ((anatFiles[i][j][k].getName().endsWith("nii.gz")) ||
                        } // for (k = 0; k < anatFiles[i][j].length; k++)
                        parseDataStructure(dsInfo, sessionImagesRead, fMRIAuxiliaryFileNumber);
                        parseForInitLabelsAndComponents();
                        if (subject_id_array != null) {
                            subject_id = subject_id_array[i];
                        } else if (participant_id_array != null) {
                            subject_id = participant_id_array[i];
                        } else {
                            subject_id = null;
                        }
                        if (age_array != null) {
                            age = age_array[i];
                        } else {
                            age = null;
                        }
                        populateFields(srcImage, sessionImagesRead, boldJsonFilenames, effectiveEchoSpacing, echoTime, repetitionTime, subject_id, age,
                                imagingFMRIAuxiliaryFile, dwibvalString, dwibvecString);
                        if ( !setInitialVisible) {
                            // convert any dates found into proper ISO format
                            /*
                             * for (i = 0; i < csvFieldNames.size(); i++) { final String[] deGroupAndName =
                             * splitFieldString(csvFieldNames.get(i));
                             * 
                             * StructuralDataElement de = null; for (final GroupRepeat repeat :
                             * fsData.getAllGroupRepeats(deGroupAndName[0])) { for (final DataElementValue deVal :
                             * repeat.getDataElements()) { if (deVal.getName().equalsIgnoreCase(deGroupAndName[1])) { de
                             * = deVal.getDataElementInfo(); break; } } }
                             * 
                             * for (final ArrayList<String> values : record) { // check value not empty and check type
                             * of field for date if ( !values.get(i).trim().equals("") &&
                             * de.getType().equals(DataType.DATE)) { values.set(i,
                             * convertDateToISOFormat(values.get(i))); } } }
                             * 
                             * // this means it was launched via the csv file populateFieldsFromCSV(fsData, record);
                             */
                        }
                        for (k = 0; k < sessionImagesRead; k++) {
                            srcImage[k].disposeLocal();
                            srcImage[k] = null;
                            jsonFile[k] = null;
                        }
                    } // if ((anatFiles[i][j] != null) && (anatFiles[i][j].length > 0))
                }
            }
            printlnToLog("anat subdirectory images read = " + anatImagesRead);
            printlnToLog("anat subdirectory JSON files read = " + anatJsonRead);
        } // if (anatNumber > 0)

        if (funcNumber > 0) {
            found = false;
            for (i = 0; i < dataStructureList.size() && ( !found); i++) {
                if (dataStructureList.get(i).getShortName().equalsIgnoreCase("ImagingFunctionalMR")) {
                    found = true;
                    ds = dataStructureList.get(i);
                    dataStructureName = "ImagingFunctionalMR";
                    if (ds.getDataElements().size() == 0) {
                        progressBar.setMessage("Retrieving data elements for form structure: " + ds.getShortName());
                        final FormDataElementsRESTThread thread = new FormDataElementsRESTThread(this, ds.getShortName(), false);
                        thread.run();

                        dsInfo = thread.getFullFormStructure();
                    } else {
                        dsInfo = ds;
                    }
                }
            }
            funcImagesRead = 0;
            funcJsonRead = 0;
            funcEventsRead = 0;
            funcPhysioTsvRead = 0;
            funcPhysioJsonRead = 0;
            for (i = 0; i < numberSubjects; i++) {
                for (j = 0; j < funcFiles[i].length; j++) {
                    pValue = 20 + 80 * subdirectoriesRead / subdirectoriesFound;
                    progressBar.updateValue(pValue);
                    if ( (funcFiles[i][j] != null) && (funcFiles[i][j].length > 0)) {
                        subdirectoriesRead++;
                        sessionImagesRead = 0;
                        sessionJsonRead = 0;
                        sessionEventsRead = 0;
                        sessionPhysioTsvRead = 0;
                        sessionPhysioJsonRead = 0;
                        previewImages.add(null);
                        structRowImgFileInfoList.add(null);
                        fsDataList.add(null);
                        allOtherFilesAL.add(null);
                        fsData = new FormStructureData(dsInfo);

                        for (k = 0; k < funcFiles[i][j].length; k++) {
                            if ( (funcFiles[i][j][k].getName().endsWith("nii.gz")) || (funcFiles[i][j][k].getName().endsWith(".nii"))) {
                                if (funcFiles[i][j][k].getName().endsWith("nii.gz")) {
                                    progressBar.setMessage("Reading " + funcFiles[i][j][k].getName());
                                    progressBar.updateValue(pValue + k * 80 / (subdirectoriesFound * funcFiles[i][j].length));
                                    srcImage[sessionImagesRead] = fileIO.readNIFTI(funcFiles[i][j][k].getName(), funcFiles[i][j][k].getParentFile()
                                            .getAbsolutePath(), false, true, true, true);
                                    index = funcFiles[i][j][k].getName().indexOf("nii.gz");
                                } else {
                                    srcImage[sessionImagesRead] = fileIO.readNIFTI(funcFiles[i][j][k].getName(), funcFiles[i][j][k].getParentFile()
                                            .getAbsolutePath(), false, false, true, true);
                                    index = funcFiles[i][j][k].getName().indexOf("nii");
                                }
                                funcImagesRead++;
                                baseName = funcFiles[i][j][k].getName().substring(0, index);
                                jsonName = baseName + "json";
                                found = false;
                                for (n = 0; n < funcFiles[i][j].length && ( !found); n++) {
                                    if (funcFiles[i][j][n].getName().equals(jsonName)) {
                                        found = true;
                                        jsonFile[sessionImagesRead] = funcFiles[i][j][n];
                                        funcJsonRead++;
                                        sessionJsonRead++;
                                    }
                                }
                                index = baseName.lastIndexOf("_");
                                eventsBaseName = baseName.substring(0, index + 1);
                                eventsName = eventsBaseName + "events.tsv";
                                found = false;
                                for (n = 0; n < funcFiles[i][j].length && ( !found); n++) {
                                    if (funcFiles[i][j][n].getName().equals(eventsName)) {
                                        found = true;
                                        eventsFile[sessionImagesRead] = funcFiles[i][j][n];
                                        funcEventsRead++;
                                        sessionEventsRead++;
                                    }
                                }
                                found = false;
                                for (n = 0; n < funcFiles[i][j].length && ( !found); n++) {
                                    if ( (funcFiles[i][j][n].getName().length() >= index + 1)
                                            && (funcFiles[i][j][n].getName().substring(0, index + 1).equals(eventsBaseName))
                                            && ( (funcFiles[i][j][n].getName().endsWith("tsv")) || (funcFiles[i][j][n].getName().endsWith("tsv.gz")))) {
                                        found = true;
                                        physioTsvFile[sessionImagesRead] = funcFiles[i][j][n];
                                        funcPhysioTsvRead++;
                                        sessionPhysioTsvRead++;
                                    }
                                }
                                found = false;
                                for (n = 0; n < funcFiles[i][j].length && ( !found); n++) {
                                    if ( (funcFiles[i][j][n].getName().length() >= index + 1)
                                            && (funcFiles[i][j][n].getName().substring(0, index + 1).equals(eventsBaseName))
                                            && (funcFiles[i][j][n].getName().endsWith("json"))) {
                                        found = true;
                                        physioJsonFile[sessionImagesRead] = funcFiles[i][j][n];
                                        funcPhysioJsonRead++;
                                        sessionPhysioJsonRead++;
                                    }
                                }
                                sessionImagesRead++;
                            } // if ((funcFiles[i][j][k].getName().endsWith("nii.gz")) ||
                        } // for (k = 0; k < funcFiles[i][j].length; k++)
                        imagingFMRIAuxiliaryFile = null;
                        fMRIAuxiliaryFileNumber = funcFiles[i][j].length - sessionImagesRead;
                        if (participantsFile != null) {
                            fMRIAuxiliaryFileNumber++;
                        }
                        if (scanstsvSubjectDirectoryFiles != null) {
                            fMRIAuxiliaryFileNumber += scanstsvSubjectDirectoryFiles[i].length;
                        }
                        if (sessionstsvSubjectDirectoryFiles != null) {
                            fMRIAuxiliaryFileNumber += sessionstsvSubjectDirectoryFiles[i].length;
                        }
                        if (scanstsvSessionDirectoryFiles != null) {
                            fMRIAuxiliaryFileNumber += scanstsvSessionDirectoryFiles[i][j].length;
                        }
                        if (eventsTSVFilenames != null) {
                            for (n = 0; n < eventsTSVFilenames.length; n++) {
                                for (k = 0, found = false; k < funcFiles[i][j].length && !found; k++) {
                                    if ( (funcFiles[i][j][k].getName().endsWith("nii.gz")) || (funcFiles[i][j][k].getName().endsWith(".nii"))) {
                                        if (funcFiles[i][j][k].getName().contains(eventsTSVFilenames[n])) {
                                            fMRIAuxiliaryFileNumber++;
                                            found = true;
                                        }
                                    }
                                }
                            } // for (n = 0; n < eventsTSVFilenames.length; n++)
                        } // if (eventsTSVFilenames != null)
                        if (boldJsonFilenames != null) {
                            for (n = 0; n < boldJsonFilenames.length; n++) {
                                for (k = 0, found = false; k < funcFiles[i][j].length && !found; k++) {
                                    if ( (funcFiles[i][j][k].getName().endsWith("nii.gz")) || (funcFiles[i][j][k].getName().endsWith(".nii"))) {
                                        if (funcFiles[i][j][k].getName().contains(boldJsonFilenames[n])) {
                                            fMRIAuxiliaryFileNumber++;
                                            found = true;
                                        }
                                    }
                                }
                            }
                        } // if (boldJsonFilenames != null)
                        if (physioJsonFilenames != null) {
                            for (n = 0; n < physioJsonFilenames.length; n++) {
                                for (k = 0, found = false; k < funcFiles[i][j].length && !found; k++) {
                                    if ( (funcFiles[i][j][k].getName().endsWith("nii.gz")) || (funcFiles[i][j][k].getName().endsWith(".nii"))) {
                                        if (funcFiles[i][j][k].getName().contains(physioJsonFilenames[n])) {
                                            fMRIAuxiliaryFileNumber++;
                                            found = true;
                                        }
                                    }
                                }
                            }
                        } // if (physioJsonFilenames != null)
                        if (fMRIAuxiliaryFileNumber > 0) {
                            imagingFMRIAuxiliaryFile = new String[fMRIAuxiliaryFileNumber];
                            m = 0;
                            if (participantsFile != null) {
                                fullPath = participantsFile.getAbsolutePath();
                                index = fullPath.indexOf(BIDSString);
                                imagingFMRIAuxiliaryFile[m++] = fullPath.substring(index);
                            }
                            if (scanstsvSubjectDirectoryFiles != null) {
                                for (k = 0; k < scanstsvSubjectDirectoryFiles[i].length; k++) {
                                    fullPath = scanstsvSubjectDirectoryFiles[i][k].getAbsolutePath();
                                    index = fullPath.indexOf(BIDSString);
                                    imagingFMRIAuxiliaryFile[m++] = fullPath.substring(index);
                                }
                            }
                            if (sessionstsvSubjectDirectoryFiles != null) {
                                for (k = 0; k < sessionstsvSubjectDirectoryFiles[i].length; k++) {
                                    fullPath = sessionstsvSubjectDirectoryFiles[i][k].getAbsolutePath();
                                    index = fullPath.indexOf(BIDSString);
                                    imagingFMRIAuxiliaryFile[m++] = fullPath.substring(index);
                                }
                            }
                            if (scanstsvSessionDirectoryFiles != null) {
                                for (k = 0; k < scanstsvSessionDirectoryFiles[i][j].length; k++) {
                                    fullPath = scanstsvSessionDirectoryFiles[i][j][k].getAbsolutePath();
                                    index = fullPath.indexOf(BIDSString);
                                    imagingFMRIAuxiliaryFile[m++] = fullPath.substring(index);
                                }
                            }
                            if (eventsTSVFilenames != null) {
                                for (n = 0; n < eventsTSVFilenames.length; n++) {
                                    for (k = 0, found = false; k < funcFiles[i][j].length && !found; k++) {
                                        if ( (funcFiles[i][j][k].getName().endsWith("nii.gz")) || (funcFiles[i][j][k].getName().endsWith(".nii"))) {
                                            if (funcFiles[i][j][k].getName().contains(eventsTSVFilenames[n])) {
                                                imagingFMRIAuxiliaryFile[m++] = eventsTSVPathnames[n];
                                                found = true;
                                            }
                                        }
                                    }
                                } // for (n = 0; n < eventsTSVFilenames.length; n++)
                            } // if (eventsTSVFilenames != null)
                            if (boldJsonFilenames != null) {
                                for (n = 0; n < boldJsonFilenames.length; n++) {
                                    for (k = 0, found = false; k < funcFiles[i][j].length && !found; k++) {
                                        if ( (funcFiles[i][j][k].getName().endsWith("nii.gz")) || (funcFiles[i][j][k].getName().endsWith(".nii"))) {
                                            if (funcFiles[i][j][k].getName().contains(boldJsonFilenames[n])) {
                                                imagingFMRIAuxiliaryFile[m++] = boldJsonPathnames[n];
                                                found = true;
                                            }
                                        }
                                    }
                                }
                            } // if (boldJsonFilenames != null)
                            if (physioJsonFilenames != null) {
                                for (n = 0; n < physioJsonFilenames.length; n++) {
                                    for (k = 0, found = false; k < funcFiles[i][j].length && !found; k++) {
                                        if ( (funcFiles[i][j][k].getName().endsWith("nii.gz")) || (funcFiles[i][j][k].getName().endsWith(".nii"))) {
                                            if (funcFiles[i][j][k].getName().contains(physioJsonFilenames[n])) {
                                                imagingFMRIAuxiliaryFile[m++] = physioJsonPathnames[n];
                                                found = true;
                                            }
                                        }
                                    }
                                }
                            } // if (physioJsonFilenames != null)
                        } // if (fMRIAuxiliaryFileNumber > 0)
                        parseDataStructure(dsInfo, sessionImagesRead, fMRIAuxiliaryFileNumber);
                        parseForInitLabelsAndComponents();
                        if (subject_id_array != null) {
                            subject_id = subject_id_array[i];
                        } else if (participant_id_array != null) {
                            subject_id = participant_id_array[i];
                        } else {
                            subject_id = null;
                        }
                        if (age_array != null) {
                            age = age_array[i];
                        } else {
                            age = null;
                        }
                        populateFields(srcImage, sessionImagesRead, boldJsonFilenames, effectiveEchoSpacing, echoTime, repetitionTime, subject_id, age,
                                imagingFMRIAuxiliaryFile, dwibvalString, dwibvecString);
                        for (k = 0; k < sessionImagesRead; k++) {
                            srcImage[k].disposeLocal();
                            srcImage[k] = null;
                            jsonFile[k] = null;
                            eventsFile[k] = null;
                            physioTsvFile[k] = null;
                            physioJsonFile[k] = null;
                        }
                    }
                }
            }
            printlnToLog("func subdirectory images read = " + funcImagesRead);
            printlnToLog("func subdirectory JSON files read = " + funcJsonRead);
            printlnToLog("func subdirectory events.tsv files read = " + funcEventsRead);
            printlnToLog("func subdirectory physio.tsv or physio.tsv.gz files read = " + funcPhysioTsvRead);
            printlnToLog("func subdirectory physio json files read = " + funcPhysioJsonRead);
        } // if (funcNumber > 0)
        imagingFMRIAuxiliaryFile = null;
        fMRIAuxiliaryFileNumber = 0;

        if (dwiNumber > 0) {
            found = false;
            for (i = 0; i < dataStructureList.size() && ( !found); i++) {
                if (dataStructureList.get(i).getShortName().equalsIgnoreCase("ImagingDiffusion")) {
                    found = true;
                    ds = dataStructureList.get(i);
                    dataStructureName = "ImagingDiffusion";
                    if (ds.getDataElements().size() == 0) {
                        progressBar.setMessage("Retrieving data elements for form structure: " + ds.getShortName());
                        final FormDataElementsRESTThread thread = new FormDataElementsRESTThread(this, ds.getShortName(), false);
                        thread.run();

                        dsInfo = thread.getFullFormStructure();
                    } else {
                        dsInfo = ds;
                    }
                }
            }
            dwiImagesRead = 0;
            dwiJsonRead = 0;
            dwiBvalRead = 0;
            dwiBvecRead = 0;
            for (i = 0; i < numberSubjects; i++) {
                for (j = 0; j < funcFiles[i].length; j++) {
                    pValue = 20 + 80 * subdirectoriesRead / subdirectoriesFound;
                    progressBar.updateValue(pValue);
                    if ( (dwiFiles[i][j] != null) && (dwiFiles[i][j].length > 0)) {
                        subdirectoriesRead++;
                        sessionImagesRead = 0;
                        sessionJsonRead = 0;
                        sessionBvalRead = 0;
                        sessionBvecRead = 0;
                        previewImages.add(null);
                        structRowImgFileInfoList.add(null);
                        fsDataList.add(null);
                        allOtherFilesAL.add(null);
                        fsData = new FormStructureData(dsInfo);
                        for (k = 0; k < dwiFiles[i][j].length; k++) {
                            if ( (dwiFiles[i][j][k].getName().endsWith("nii.gz")) || (dwiFiles[i][j][k].getName().endsWith(".nii"))) {
                                if (dwiFiles[i][j][k].getName().endsWith("nii.gz")) {
                                    progressBar.setMessage("Reading " + dwiFiles[i][j][k].getName());
                                    progressBar.updateValue(pValue + k * 80 / (subdirectoriesFound * dwiFiles[i][j].length));
                                    srcImage[sessionImagesRead] = fileIO.readNIFTI(dwiFiles[i][j][k].getName(), dwiFiles[i][j][k].getParentFile()
                                            .getAbsolutePath(), false, true, true, true);
                                    index = dwiFiles[i][j][k].getName().indexOf("nii.gz");
                                } else {
                                    srcImage[sessionImagesRead] = fileIO.readNIFTI(dwiFiles[i][j][k].getName(), dwiFiles[i][j][k].getParentFile()
                                            .getAbsolutePath(), false, false, true, true);
                                    index = dwiFiles[i][j][k].getName().indexOf("nii");
                                }
                                dwiImagesRead++;
                                baseName = dwiFiles[i][j][k].getName().substring(0, index);
                                jsonName = baseName + "json";
                                found = false;
                                for (n = 0; n < dwiFiles[i][j].length && ( !found); n++) {
                                    if (dwiFiles[i][j][n].getName().equals(jsonName)) {
                                        found = true;
                                        jsonFile[sessionImagesRead] = dwiFiles[i][j][n];
                                        dwiJsonRead++;
                                        sessionJsonRead++;
                                    }
                                }
                                bvalName = baseName + "bval";
                                found = false;
                                for (n = 0; n < dwiFiles[i][j].length && ( !found); n++) {
                                    if (dwiFiles[i][j][n].getName().equals(bvalName)) {
                                        found = true;
                                        bvalFile[sessionImagesRead] = dwiFiles[i][j][n];
                                        dwiBvalRead++;
                                        sessionBvalRead++;
                                    }
                                }
                                bvecName = baseName + "bvec";
                                found = false;
                                for (n = 0; n < dwiFiles[i][j].length && ( !found); n++) {
                                    if (dwiFiles[i][j][n].getName().equals(bvecName)) {
                                        found = true;
                                        bvecFile[sessionImagesRead] = dwiFiles[i][j][n];
                                        dwiBvecRead++;
                                        sessionBvalRead++;
                                    }
                                }
                                sessionImagesRead++;
                            } // if ((dwiFiles[i][j][k].getName().endsWith("nii.gz")) ||
                        } // for (k = 0; k < dwiFiles[i][j].length; k++)
                        parseDataStructure(dsInfo, sessionImagesRead, fMRIAuxiliaryFileNumber);
                        parseForInitLabelsAndComponents();
                        if (subject_id_array != null) {
                            subject_id = subject_id_array[i];
                        } else if (participant_id_array != null) {
                            subject_id = participant_id_array[i];
                        } else {
                            subject_id = null;
                        }
                        if (age_array != null) {
                            age = age_array[i];
                        } else {
                            age = null;
                        }
                        populateFields(srcImage, sessionImagesRead, boldJsonFilenames, effectiveEchoSpacing, echoTime, repetitionTime, subject_id, age,
                                imagingFMRIAuxiliaryFile, dwibvalString, dwibvecString);
                        for (k = 0; k < sessionImagesRead; k++) {
                            srcImage[k].disposeLocal();
                            srcImage[k] = null;
                            jsonFile[k] = null;
                            bvalFile[k] = null;
                            bvecFile[k] = null;
                        }
                    }
                }
            }
            printlnToLog("dwi subdirectory images read = " + dwiImagesRead);
            printlnToLog("dwi subdirectory json files read = " + dwiJsonRead);
            printlnToLog("dwi subdirectory bval files read = " + dwiBvalRead);
            printlnToLog("dwi subdirectory bvec files read = " + dwiBvecRead);
        } // if (dwiNumber > 0)

        // finishButton.setEnabled(true);
        enableDisableFinishButton();
        progressBar.updateValue(100);
        progressBar.dispose();
        // dispose();

        /*
         * gbc.gridx = 0; gbc.gridy = 0; gbc.insets = new Insets(10, 5, 10, 25); gbc.gridwidth = 1;
         * 
         * final JPanel OKPanel = new JPanel();
         * 
         * final JButton OKButton = new JButton("Save"); OKButton.setActionCommand("StructDialogOK");
         * OKButton.addActionListener(this); OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
         * OKButton.setPreferredSize(MipavUtil.defaultButtonSize); OKButton.setFont(serif12B);
         * 
         * final JButton cancelButton = new JButton("Cancel"); cancelButton.setActionCommand("StructDialogCancel");
         * cancelButton.addActionListener(this); cancelButton.setMinimumSize(MipavUtil.defaultButtonSize);
         * cancelButton.setPreferredSize(MipavUtil.defaultButtonSize); cancelButton.setFont(serif12B);
         * 
         * OKPanel.add(OKButton); OKPanel.add(cancelButton);
         * 
         * requiredLabel = new JLabel(
         * "<html>Mouse over data element name for a description.<br/>Mouse over the data element fields for more information on filling them in.<br/>* Required data elements are in <font color=\"red\">red</font></html>"
         * );
         * 
         * gbc.fill = GridBagConstraints.BOTH; gbc.anchor = GridBagConstraints.EAST; gbc.weightx = 0; gbc.gridx = 0;
         * gbc.gridy = 0; mainPanel.add(requiredLabel, gbc);
         * 
         * gbc.gridy = 2; gbc.weightx = 1; gbc.weighty = 1; mainPanel.add(tabScrollPane, gbc); gbc.weightx = 0;
         * gbc.weighty = 0; gbc.gridy = 3; mainPanel.add(OKPanel, gbc);
         * 
         * getContentPane().add(mainPanel);
         * 
         * final Dimension dim = getContentPane().getPreferredSize(); if (dim.height > 500) { dim.height = 500; }
         * tabScrollPane.setPreferredSize(dim);
         * 
         * pack(); MipavUtil.centerInWindow(this, this); if (setInitialVisible) { setVisible(true); }
         */

        return true;
    }

    /**
     * called after validation is done
     */
    public void complete(final FormStructureData fsData, final boolean isComplete) {
        String value = "";
        String guid = "";
        final ArrayList<File> allOtherFiles = new ArrayList<File>();
        final boolean launchedFromInProcessState = false;
        final boolean addedPreviewImage = false;

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

    /**
     * prepopulates some of the fields with info from image headers
     */
    public void populateFields(final ModelImage img[], final int numImages, final String boldJsonFilenames[], final double effectiveEchoSpacing[],
            final double echoTimeDouble[], final double repetitionTimeDouble[], final String subject_id, final String age,
            final String imagingFMRIAuxiliaryFile[], final String dwibvalString, final String dwibvecString) {
        final float[][] res = new float[numImages][];
        final int[][] units = new int[numImages][];
        final int[][] exts = new int[numImages][];
        final int[] modality = new int[numImages];
        final String[] modalityString = new String[numImages];
        final String[] imageFileName = new String[numImages];
        final int[] fileFormatInt = new int[numImages];
        final String[] fileFormatString = new String[numImages];
        final String[] upperStructureName = new String[numImages];
        final float[] sliceThickness = new float[numImages];
        final int[] orient = new int[numImages];
        final String[] orientation = new String[numImages];
        String[] ageVal = new String[numImages];
        final String[] siteName = new String[numImages];
        final String[] visitDate = new String[numImages];
        final String[] visitTime = new String[numImages];
        final String[] sliceOversample = new String[numImages];
        final String[] gap = new String[numImages];
        final String[] bodyPart = new String[numImages];

        final String[] fieldOfView = new String[numImages];
        final String[] manufacturer = new String[numImages];
        final String[] softwareVersion = new String[numImages];
        final String[] patientPosition = new String[numImages];

        final String[] scannerModel = new String[numImages];
        final String[] bandwidth = new String[numImages];

        final String[] scanOptions = new String[numImages];
        final String[] flowCompensation = new String[numImages];

        final String[] patientName = new String[numImages];
        final String[] patientID = new String[numImages];

        final String[] echoTime = new String[numImages];
        final String[] repetitionTime = new String[numImages];
        final String[] magneticFieldStrength = new String[numImages];
        final String[] flipAngle = new String[numImages];

        final String[] mriT1T2Name = new String[numImages];
        final String[] inversionTime = new String[numImages];
        final String[] echoTrainMeas = new String[numImages];
        final String[] phaseEncode = new String[numImages];
        final String[] numAverages = new String[numImages];
        final String[] receiveCoilName = new String[numImages];

        final String[] contrastAgent = new String[numImages];
        final String[] contrastMethod = new String[numImages];
        final String[] contrastTime = new String[numImages];
        final String[] contrastDose = new String[numImages];
        final String[] contrastRate = new String[numImages];
        final String[] contrastUsedInd = new String[numImages];
        final boolean[] contrastUsed = new boolean[numImages];

        final String[] ctKVP = new String[numImages];
        final String[] ctMA = new String[numImages];

        String description;
        final String[] seriesDescription = new String[numImages];
        final boolean[] isRestingFMRI = new boolean[numImages];
        final String[] thicknessStr = new String[numImages];
        final String[] ageInMonths = new String[numImages];
        final String[] ageMonthsStr = new String[numImages];
        final Integer[] ageInMonthsInt = new Integer[numImages];
        final String[] ageInYears = new String[numImages];
        final String[] imagingEchoSpacing = new String[numImages];
        FileInfoDicom fileInfoDicom;
        FileInfoNIFTI fileInfoNifti;
        int i;
        int j;
        // If true, the NIFTI file has a DcmMeta extension header using JSON encoding
        boolean haveDcmMeta;
        Unit tUnit;
        double tResol;
        double diff;

        for (i = 0; i < numImages; i++) {
            imageFileName[i] = img[i].getImageFileName();
            res[i] = img[i].getResolutions(0);
            units[i] = img[i].getUnitsOfMeasure();
            exts[i] = img[i].getExtents();
            modality[i] = img[i].getFileInfo(0).getModality();
            modalityString[i] = FileInfoBase.getModalityStr(modality[i]);
            fileFormatInt[i] = img[i].getFileInfo(0).getFileFormat();

            fileFormatString[i] = FileUtility.getFileTypeStr(fileFormatInt[i]);
            if (fileFormatString[i].equalsIgnoreCase("xml")) {
                fileFormatString[i] = "mipav xml";
            } else if (fileFormatString[i].equalsIgnoreCase("mat")) {
                fileFormatString[i] = "matlab";
            }
            
            modality[i] = determineModality(modality[i], dataStructureName);
            modalityString[i] = FileInfoBase.getModalityStr(modality[i]);

            sliceThickness[i] = img[i].getFileInfo(0).getSliceThickness();
            orient[i] = img[i].getFileInfo(0).getImageOrientation();
            orientation[i] = FileInfoBase.getImageOrientationStr(orient[i]);

            if (fileFormatString[i].equalsIgnoreCase("dicom")) {
                fileInfoDicom = (FileInfoDicom) img[i].getFileInfo(0);

                ageVal[i] = (String) (fileInfoDicom.getTagTable().getValue("0010,1010", false));
                // put in to skip erroneous values set in some TRACK-TBI Pilot CT data
                if (isValueSet(ageVal[i]) && ageVal[i].equalsIgnoreCase("135Y")) {
                    ageVal = null;
                }
                siteName[i] = (String) (fileInfoDicom.getTagTable().getValue("0008,0080"));
                // CNRM anonymization of the institution tag sets the value to Institution instead of clearing the value
                if (isValueSet(siteName[i]) && siteName[i].trim().equalsIgnoreCase("Institution")) {
                    siteName[i] = "";
                }
                visitDate[i] = (String) (fileInfoDicom.getTagTable().getValue("0008,0020"));
                visitTime[i] = (String) (fileInfoDicom.getTagTable().getValue("0008,0030"));
                sliceOversample[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,0093"));
                gap[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,0088"));
                bodyPart[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,0015"));

                fieldOfView[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,1100"));
                manufacturer[i] = (String) (fileInfoDicom.getTagTable().getValue("0008,0070"));
                softwareVersion[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,1020"));
                patientPosition[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,5100"));

                scannerModel[i] = (String) (fileInfoDicom.getTagTable().getValue("0008,1090"));
                bandwidth[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,0095"));

                patientName[i] = (String) (fileInfoDicom.getTagTable().getValue("0010,0010"));
                patientID[i] = (String) (fileInfoDicom.getTagTable().getValue("0010,0020"));

                contrastAgent[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,0010"));
                contrastMethod[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,1040"));
                if (isValueSet(contrastMethod[i])) {
                    System.err.println(patientName[i] + "\tContrast route: " + contrastMethod[i]);
                    if (contrastMethod[i].equalsIgnoreCase("IV") || contrastMethod[i].equalsIgnoreCase("Oral & IV")) {
                        contrastMethod[i] = "Infusion";
                    }
                }
                contrastTime[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,1042"));
                if (isValueSet(contrastTime[i]) && isValueSet(visitDate[i])) {
                    contrastTime[i] = convertDateTimeToISOFormat(visitDate[i], contrastTime[i]);
                }
                contrastDose[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,1044"));
                contrastRate[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,1046"));

                if (isValueSet(contrastAgent[i]) || isValueSet(contrastMethod[i]) || isValueSet(contrastTime[i]) || isValueSet(contrastDose[i])
                        || isValueSet(contrastRate[i])) {
                    contrastUsedInd[i] = "Yes";
                    contrastUsed[i] = true;
                }

                if (modalityString[i].equalsIgnoreCase("magnetic resonance")) {
                    seriesDescription[i] = ((String) (fileInfoDicom.getTagTable().getValue("0008,103E")));
                    if (isValueSet(seriesDescription[i]) && seriesDescription[i].toLowerCase().contains("rest")) {
                        isRestingFMRI[i] = true;
                    }

                    echoTime[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,0081"));
                    repetitionTime[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,0080"));
                    magneticFieldStrength[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,0087"));
                    flipAngle[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,1314"));

                    mriT1T2Name[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,0024"));
                    inversionTime[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,0082"));
                    echoTrainMeas[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,0091"));
                    phaseEncode[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,1312"));
                    numAverages[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,0083"));
                    receiveCoilName[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,1250"));

                    scanOptions[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,0022"));
                    // FC ==> flow compensation
                    if (isValueSet(scanOptions[i]) && scanOptions[i].contains("FC")) {
                        flowCompensation[i] = "Yes";
                    }
                } else if (modalityString[i].equalsIgnoreCase("computed tomography")) {
                    ctKVP[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,0060"));
                    ctMA[i] = (String) (fileInfoDicom.getTagTable().getValue("0018,1151"));
                }
            } else if (fileFormatString[i].equalsIgnoreCase("nifti")) {
                // Description = Philips Medical Systems Achieva 3.2.1 (from .nii T1 of Dr. Vaillancourt's)
                fileInfoNifti = (FileInfoNIFTI) img[i].getFileInfo(0);
                description = fileInfoNifti.getDescription();

                if ( (description != null) && (description.length() > 0)) {
                    manufacturer[i] = convertNiftiDescToBRICSManuf(description);
                    scannerModel[i] = convertNiftiDescToBRICSModel(description);
                    softwareVersion[i] = convertNiftiDescToBRICSVer(description);
                } // if ((description != null) && (description.length() > 0))

                haveDcmMeta = fileInfoNifti.getHaveDcmMeta();
                if (haveDcmMeta) {
                    visitTime[i] = fileInfoNifti.getStudyTime();
                    manufacturer[i] = convertManufNameToBRICS(fileInfoNifti.getManufacturer());
                    scannerModel[i] = fileInfoNifti.getManufacturerModelName();
                    softwareVersion[i] = fileInfoNifti.getSoftwareVersions();
                    bandwidth[i] = String.valueOf(fileInfoNifti.getPixelBandwidth());
                    scanOptions[i] = fileInfoNifti.getScanOptions();
                    gap[i] = String.valueOf(fileInfoNifti.getSpacingBetweenSlices());
                    echoTime[i] = String.valueOf(fileInfoNifti.getEchoTime());
                    repetitionTime[i] = String.valueOf(fileInfoNifti.getRepetitionTime());
                    magneticFieldStrength[i] = String.valueOf(fileInfoNifti.getMagneticFieldStrength());
                    flipAngle[i] = String.valueOf(fileInfoNifti.getFlipAngle());
                    echoTrainMeas[i] = String.valueOf(fileInfoNifti.getEchoTrainLength());
                    phaseEncode[i] = fileInfoNifti.getInPlanePhaseEncodingDirection();
                    numAverages[i] = String.valueOf(fileInfoNifti.getNumberOfAverages());
                    mriT1T2Name[i] = fileInfoNifti.getSequenceName();
                } // if (haveDcmMeta)

            } // else if (fileFormatString[i].equalsIgnoreCase("nifti"))

            if ( (imageFileName[i].contains("_bold")) && (boldJsonFilenames != null) && (boldJsonFilenames.length > 0)) {
                for (j = 0; j < boldJsonFilenames.length; j++) {
                    if (imageFileName[i].contains(boldJsonFilenames[j])) {
                        if ( (effectiveEchoSpacing != null) && (effectiveEchoSpacing.length >= j + 1) && ( !Double.isNaN(effectiveEchoSpacing[j]))) {
                            imagingEchoSpacing[i] = String.valueOf(effectiveEchoSpacing[j]);
                        }
                        if ( (echoTime[i] == null) && (echoTimeDouble != null) && (echoTimeDouble.length >= j + 1) && ( !Double.isNaN(echoTimeDouble[j]))) {
                            echoTime[i] = String.valueOf(echoTimeDouble[j]);
                        }
                        if ( (repetitionTimeDouble != null) && (repetitionTimeDouble.length >= j + 1) && ( !Double.isNaN(repetitionTimeDouble[j]))) {
                            tUnit = Unit.getUnitFromLegacyNum(units[i][3]);
                            tResol = tUnit.convertTo(res[i][3], Unit.MILLISEC);
                            diff = Math.abs(tResol - repetitionTimeDouble[j]) / repetitionTimeDouble[j];
                            if (diff >= 1.0E-2) {
                                System.err.println("JSON repetition time = " + repetitionTimeDouble[j] + " does not equal image time resolution = " + tResol);
                            }
                        }
                        if ( (repetitionTime[i] == null) && (repetitionTimeDouble != null) && (repetitionTimeDouble.length >= j + 1)
                                && ( !Double.isNaN(repetitionTimeDouble[j]))) {
                            repetitionTime[i] = String.valueOf(repetitionTimeDouble[j]);
                        }
                    } // if (imageFileName[i].contains(boldJsonFilenames[j]))
                } // for (j = 0; j < boldJsonFilenames.length; j++)
            } // if ((imageFileName[i].contains("_bold")) && (boldJsonFilenames != null) && (boldJsonFilenames.length >
              // 0))

        } // for (i = 0; i < numImages; i++)

        for (final RepeatableGroup group : fsData.getStructInfo().getRepeatableGroups()) {
            i = -1;
            for (final GroupRepeat repeat : fsData.getAllGroupRepeats(group.getName())) {
                i++;
                for (final DataElementValue deVal : repeat.getDataElements()) {
                    final String deName = deVal.getName();
                    if (deName.equalsIgnoreCase("ImgFile")) {
                        setElementComponentValue(deVal, imageFileName[i]);
                    } else if (deName.equalsIgnoreCase("ImgDimensionTyp")) {
                        setElementComponentValue(deVal, exts[i].length + "D");
                    } else if (deName.equalsIgnoreCase("ImgDim1ExtentVal")) {
                        setElementComponentValue(deVal, String.valueOf(exts[i][0]));
                    } else if (deName.equalsIgnoreCase("ImgDim2ExtentVal")) {
                        setElementComponentValue(deVal, String.valueOf(exts[i][1]));
                    } else if (deName.equalsIgnoreCase("ImgDim3ExtentVal")) {
                        if (img[i].getNDims() > 2) {
                            setElementComponentValue(deVal, String.valueOf(exts[i][2]));
                        }
                    } else if (deName.equalsIgnoreCase("ImgDim4ExtentVal")) {
                        if (img[i].getNDims() > 3) {
                            setElementComponentValue(deVal, String.valueOf(exts[i][3]));
                        }
                    } else if (deName.equalsIgnoreCase("ImgDim5ExtentVal")) {
                        // for now...nothing
                    } else if (deName.equalsIgnoreCase("ImgDim1UoMVal")) {
                        setElementComponentValue(deVal, Unit.getUnitFromLegacyNum(units[i][0]).toString());
                    } else if (deName.equalsIgnoreCase("ImgDim2UoMVal")) {
                        setElementComponentValue(deVal, Unit.getUnitFromLegacyNum(units[i][1]).toString());
                    } else if (deName.equalsIgnoreCase("ImgDim3UoMVal")) {
                        if (img[i].getNDims() > 2) {
                            setElementComponentValue(deVal, Unit.getUnitFromLegacyNum(units[i][2]).toString());
                        }
                    } else if (deName.equalsIgnoreCase("ImgDim4UoMVal")) {
                        if (img[i].getNDims() > 3) {
                            setElementComponentValue(deVal, Unit.getUnitFromLegacyNum(units[i][3]).toString());
                        }
                    } else if (deName.equalsIgnoreCase("ImgDim5UoMVal")) {
                        // for now...nothing
                    } else if (deName.equalsIgnoreCase("ImgDim4ExtentTyp")) {
                        if (img[i].getNDims() > 3 && Unit.getUnitFromLegacyNum(units[i][3]).getType() == UnitType.TIME) {
                            setElementComponentValue(deVal, "Time");
                        }
                    } else if (deName.equalsIgnoreCase("ImgDim1ResolVal")) {
                        setElementComponentValue(deVal, String.valueOf(res[i][0]));
                    } else if (deName.equalsIgnoreCase("ImgDim2ResolVal")) {
                        setElementComponentValue(deVal, String.valueOf(res[i][1]));
                    } else if (deName.equalsIgnoreCase("ImgDim3ResolVal")) {
                        if (img[i].getNDims() > 2) {
                            setElementComponentValue(deVal, String.valueOf(res[i][2]));
                        }
                    } else if (deName.equalsIgnoreCase("ImgDim4ResolVal")) {
                        if (img[i].getNDims() > 3) {
                            setElementComponentValue(deVal, String.valueOf(res[i][3]));
                        }
                    } else if (deName.equalsIgnoreCase("ImgDim5ResolVal")) {
                        // for now...nothing
                    } else if (deName.equalsIgnoreCase("ImgModltyTyp")) {
                        setElementComponentValue(deVal, convertModalityToBRICS(modalityString[i], contrastUsed[i]));
                    } else if (deName.equalsIgnoreCase("ImgFileFormatTyp")) {
                        setElementComponentValue(deVal, fileFormatString[i]);
                    } else if (deName.equalsIgnoreCase("ImgSliceThicknessVal")) {
                        thicknessStr[i] = "";
                        if (sliceThickness[i] > 0) {
                            thicknessStr[i] = String.valueOf(sliceThickness[i]);
                        }
                        setElementComponentValue(deVal, thicknessStr[i]);
                    } else if (deName.equalsIgnoreCase("ImgSliceOrientTyp")) {
                        setElementComponentValue(deVal, orientation[i]);
                    }

                    if ( (i < numImages) && (fileFormatString[i].equalsIgnoreCase("dicom"))) {
                        if (deName.equalsIgnoreCase("AgeVal") && ageVal[i] != null && !ageVal[i].equals("")) {
                            ageInMonths[i] = convertDicomAgeToBRICS(ageVal[i]);
                            if (Float.parseFloat(ageInMonths[i]) != 0) {
                                setElementComponentValue(deVal, ageInMonths[i]);
                            }
                        } else if (deName.equalsIgnoreCase("AgeYrs") && ageVal[i] != null && !ageVal[i].equals("")) {
                            ageMonthsStr[i] = convertDicomAgeToBRICS(ageVal[i]);
                            if (ageMonthsStr[i] != null && !ageMonthsStr[i].trim().equals("")) {
                                ageInMonthsInt[i] = Integer.valueOf(ageMonthsStr[i]);
                                if (ageInMonthsInt[i] != 0) {
                                    ageInYears[i] = String.valueOf(ageInMonthsInt[i] / 12);
                                    setElementComponentValue(deVal, ageInYears[i]);
                                }
                            }
                        } else if (deName.equalsIgnoreCase("SiteName")) {
                            setElementComponentValue(deVal, siteName[i]);
                        } else if (deName.equalsIgnoreCase("VisitDate")) {
                            setElementComponentValue(deVal, convertDateToISOFormat(visitDate[i]));
                        } else if (deName.equalsIgnoreCase("ImgAntmicSite")) {
                            setElementComponentValue(deVal, bodyPart[i]);
                        } else if (deName.equalsIgnoreCase("ImgStdyDateTime")) {
                            setElementComponentValue(deVal, convertDateTimeToISOFormat(visitDate[i], visitTime[i]));
                        } else if (deName.equalsIgnoreCase("ImgSliceOverSampVal")) {
                            setElementComponentValue(deVal, sliceOversample[i]);
                        } else if (deName.equalsIgnoreCase("ImgGapBetwnSlicesMeasr")) {
                            setElementComponentValue(deVal, gap[i]);
                        } else if (deName.equalsIgnoreCase("ImgFOVMeasrDescTxt")) {
                            setElementComponentValue(deVal, fieldOfView[i]);
                        } else if (deName.equalsIgnoreCase("ImgScannerManufName")) {
                            setElementComponentValue(deVal, convertManufNameToBRICS(manufacturer[i]));
                        } else if (deName.equalsIgnoreCase("ImgScannerSftwrVrsnNum")) {
                            setElementComponentValue(deVal, softwareVersion[i]);
                        } else if (deName.equalsIgnoreCase("ImgHeadPostnTxt")) {
                            setElementComponentValue(deVal, patientPosition[i]);
                        } else if (deName.equalsIgnoreCase("ImgScannerModelName")) {
                            setElementComponentValue(deVal, convertModelNameToBRICS(scannerModel[i]));
                        } else if (deName.equalsIgnoreCase("ImgBandwidthVal")) {
                            setElementComponentValue(deVal, bandwidth[i]);
                        } else if (deName.equalsIgnoreCase("GUID")) {
                            if (isGuid(patientID[i])) {
                                setElementComponentValue(deVal, patientID[i]);
                            } else if (isGuid(patientName[i])) {
                                setElementComponentValue(deVal, patientName[i]);
                            }
                        } else if (deName.equalsIgnoreCase("ImgContrastAgentUsedInd")) {
                            setElementComponentValue(deVal, contrastUsedInd[i]);
                        } else if (deName.equalsIgnoreCase("ImgContrastAgentName")) {
                            setElementComponentValue(deVal, contrastAgent[i]);
                        } else if (deName.equalsIgnoreCase("ImgContrastAgentMethodTyp")) {
                            setElementComponentValue(deVal, contrastMethod[i]);
                        } else if (deName.equalsIgnoreCase("ImgContrastAgentInjctnTime")) {
                            setElementComponentValue(deVal, contrastTime[i]);
                        } else if (deName.equalsIgnoreCase("ImgContrastAgentDose")) {
                            setElementComponentValue(deVal, contrastDose[i]);
                        } else if (deName.equalsIgnoreCase("ImgContrastAgentRate")) {
                            setElementComponentValue(deVal, contrastRate[i]);
                        }

                        if (modalityString[i].equalsIgnoreCase("magnetic resonance")) {
                            if (deName.equalsIgnoreCase("ImgEchoDur")) {
                                setElementComponentValue(deVal, echoTime[i]);
                            } else if (deName.equalsIgnoreCase("ImgRepetitionGapVal")) {
                                setElementComponentValue(deVal, repetitionTime[i]);
                            } else if (deName.equalsIgnoreCase("ImgScannerStrgthVal")) {
                                setElementComponentValue(deVal, convertMagFieldStrengthToBRICS(magneticFieldStrength[i]));
                            } else if (deName.equalsIgnoreCase("ImgMRIT1T2SeqName")) {
                                setElementComponentValue(deVal, mriT1T2Name[i]);
                            } else if (deName.equalsIgnoreCase("ImgSignalAvgNum")) {
                                setElementComponentValue(deVal, numAverages[i]);
                            } else if (deName.equalsIgnoreCase("ImgFlipAngleMeasr")) {
                                setElementComponentValue(deVal, flipAngle[i]);
                            } else if (deName.equalsIgnoreCase("ImgEchoTrainLngthMeasr")) {
                                setElementComponentValue(deVal, echoTrainMeas[i]);
                            } else if (deName.equalsIgnoreCase("ImgInversionTime")) {
                                setElementComponentValue(deVal, inversionTime[i]);
                            } else if (deName.equalsIgnoreCase("ImgRFCoilName")) {
                                setElementComponentValue(deVal, receiveCoilName[i]);
                            } else if (deName.equalsIgnoreCase("ImgPhasEncdeDirctTxt")) {
                                setElementComponentValue(deVal, phaseEncode[i]);
                            } else if (deName.equalsIgnoreCase("ImgFlowCompnsatnInd")) {
                                setElementComponentValue(deVal, flowCompensation[i]);
                            }

                            // ImagingFunctionalMR FS
                            if (deName.equalsIgnoreCase("ImgPulseSeqTyp")) {
                                if (fsData.getStructInfo().getShortName().equalsIgnoreCase("ImagingFunctionalMR")) {
                                    setElementComponentValue(deVal, "fMRI");
                                }
                            } else if (deName.equalsIgnoreCase("ImgFMRITaskTyp")) {
                                if (isRestingFMRI[i]) {
                                    setElementComponentValue(deVal, "Rest");
                                }
                            }
                        } else if (modalityString[i].equalsIgnoreCase("computed tomography")) {
                            if (deName.equalsIgnoreCase("ImgCTkVp")) {
                                setElementComponentValue(deVal, ctKVP[i]);
                            } else if (deName.equalsIgnoreCase("ImgCTmA")) {
                                setElementComponentValue(deVal, ctMA[i]);
                            }
                        }
                    } else if ( (i < numImages) && (fileFormatString[i].equalsIgnoreCase("nifti"))) {
                        if ( (deName.equalsIgnoreCase("ImgScannerManufName")) && (manufacturer[i] != null)) {
                            setElementComponentValue(deVal, manufacturer[i]);
                        } else if ( (deName.equalsIgnoreCase("ImgScannerModelName")) && (scannerModel[i] != null)) {
                            setElementComponentValue(deVal, scannerModel[i]);
                        } else if ( (deName.equalsIgnoreCase("ImgScannerSftwrVrsnNum")) && (softwareVersion[i] != null)) {
                            setElementComponentValue(deVal, softwareVersion[i]);
                        } else if ( (deName.equalsIgnoreCase("ImgBandwidthVal")) && (bandwidth[i] != null)) {
                            setElementComponentValue(deVal, bandwidth[i]);
                        } else if ( (deName.equalsIgnoreCase("ImgGapBetwnSlicesMeasr")) && (gap[i] != null)) {
                            setElementComponentValue(deVal, gap[i]);
                        } else if ( (deName.equalsIgnoreCase("ImgScannerStrgthVal")) && (magneticFieldStrength[i] != null)) {
                            setElementComponentValue(deVal, convertMagFieldStrengthToBRICS(magneticFieldStrength[i]));
                        } else if ( (deName.equalsIgnoreCase("ImgFlipAngleMeasr")) && (flipAngle[i] != null)) {
                            setElementComponentValue(deVal, flipAngle[i]);
                        } else if ( (deName.equalsIgnoreCase("ImgEchoTrainLngthMeasr")) && (echoTrainMeas[i] != null)) {
                            setElementComponentValue(deVal, echoTrainMeas[i]);
                        } else if ( (deName.equalsIgnoreCase("ImgSignalAvgNum")) && (numAverages[i] != null)) {
                            setElementComponentValue(deVal, numAverages[i]);
                        } else if ( (deName.equalsIgnoreCase("ImgMRIT1T2SeqName")) && (mriT1T2Name[i] != null)) {
                            setElementComponentValue(deVal, mriT1T2Name[i]);
                        } else if (deName.equalsIgnoreCase("ImgModltyTyp")) {
                            setElementComponentValue(deVal, convertModalityToBRICS(modalityString[i], false));
                        }
                        // ImagingFunctionalMR FS
                        else if (deName.equalsIgnoreCase("ImgPulseSeqTyp")) {
                            if (fsData.getStructInfo().getShortName().equalsIgnoreCase("ImagingFunctionalMR")) {
                                setElementComponentValue(deVal, "fMRI");
                            }
                        }
                    }
                    if ( (deName.equalsIgnoreCase("ImgEchoDur")) && (echoTime[i] != null)) {
                        setElementComponentValue(deVal, echoTime[i]);
                    } else if ( (deName.equalsIgnoreCase("ImgRepetitionGapVal")) && (repetitionTime != null) && (repetitionTime[i] != null)) {
                        setElementComponentValue(deVal, repetitionTime[i]);
                    } else if ( (deName.equalsIgnoreCase("ImgPhasEncdeDirctTxt")) && (phaseEncode != null) && (phaseEncode[i] != null)) {
                        setElementComponentValue(deVal, phaseEncode[i]);
                    } else if ( (deName.equalsIgnoreCase("ImgEchoSpcVal")) && (imagingEchoSpacing != null) && (imagingEchoSpacing[i] != null)) {
                        setElementComponentValue(deVal, imagingEchoSpacing[i]);
                    } else if ( (deName.equalsIgnoreCase("SubjectIDNum")) && (subject_id != null)) {
                        setElementComponentValue(deVal, subject_id);
                    } else if ( (deName.equalsIgnoreCase("AgeYrs")) && (age != null)) {
                        setElementComponentValue(deVal, age);
                    } else if ( (deName.equalsIgnoreCase("ImgFMRIAuxFile")) && (imagingFMRIAuxiliaryFile != null) && (imagingFMRIAuxiliaryFile[i] != null)) {
                        setElementComponentValue(deVal, imagingFMRIAuxiliaryFile[i]);
                    } else if ( (deName.equalsIgnoreCase("ImgDIffusionBValFile")) && (dwibvalString != null)) {
                        setElementComponentValue(deVal, dwibvalString);
                    } else if ( (deName.equalsIgnoreCase("ImgDIffusionBVecFile")) && (dwibvecString != null)) {
                        setElementComponentValue(deVal, dwibvecString);
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
            for (i = 0; i < errs.size(); i++) {
                errors.append(" - " + errs.get(i) + "\n");
            }
            isComplete = false;
        }

        complete(fsData, isComplete);
    }

    private class fileComparator implements Comparator<File> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        @Override
        public int compare(final File o1, final File o2) {
            final String a = o1.getName();
            final String b = o2.getName();
            return a.compareToIgnoreCase(b);
        }

    }

    private void parseDataStructure(final FormStructure dataStructure, final int sessionImagesRead, final int fMRIAuxiliaryFileNumber) {
        // setup the group bins in the form data
        for (final RepeatableGroup g : dataStructure.getRepeatableGroups()) {
            // if the group repeats an exact number of times, create them now
            // otherwise, start with just one (threshold of 0 ==> optional)
            int numRepeats = 0;
            if (g.getType().equals(RepeatableType.EXACTLY) && g.getThreshold() > 0) {
                numRepeats = g.getThreshold();
            } else {
                if (g.getName().equalsIgnoreCase("Main")) {
                    numRepeats = 1;
                } else if (g.getName().equalsIgnoreCase("Image Information")) {
                    numRepeats = sessionImagesRead;
                } else if (g.getName().equalsIgnoreCase("Image pixel information and dimensions")) {
                    numRepeats = sessionImagesRead;
                } else if (g.getName().equalsIgnoreCase("Image QA & QC")) {
                    numRepeats = sessionImagesRead;
                } else if (g.getName().equalsIgnoreCase("fMRI Auxiliary Files")) {
                    numRepeats = fMRIAuxiliaryFileNumber;
                } else {
                    numRepeats = 1;
                }
            }

            // if no values or threshold of 0, build at least one repeat
            if (numRepeats == 0) {
                numRepeats = 1;
            }

            fsData.addGroup(g.getName());
            for (int i = 0; i < numRepeats; i++) {
                final GroupRepeat repeat = parseGroupRepeat(dataStructure, fsData, g, i);

                fsData.addGroupRepeat(g.getName(), repeat);
            }
        }
    }

    private GroupRepeat parseGroupRepeat(final FormStructure dataStructure, final FormStructureData fsData, final RepeatableGroup group, final int repeatNum) {
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

            // special handling of SiteName for PDBP, where they want to use a set of permissible values
            // with the free-form DE
            if (isPDBPImagingStructure(dataStructure.getShortName()) && de.getStructuralDataElement().getName().equalsIgnoreCase(SITE_NAME_ELEMENT_NAME)) {
                final JComboBox cb = new JComboBox();
                cb.setName(de.getStructuralDataElement().getName());
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
            } else if (de.getStructuralDataElement().getValueRangeList() != null && de.getStructuralDataElement().getValueRangeList().size() > 0
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

                if (de.getStructuralDataElement().getType().equals(DataType.NUMERIC) || de.getStructuralDataElement().getType().equals(DataType.ALPHANUMERIC)) {
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

                disableUnchangableFields(de.getStructuralDataElement().getName(), tf);

                if (de.getStructuralDataElement().getName().equalsIgnoreCase(IMG_HASH_CODE_ELEMENT_NAME)) {
                    tf.setText("Automatically generated from selected image files.");
                } else if (de.getStructuralDataElement().getName().equalsIgnoreCase(IMG_PREVIEW_ELEMENT_NAME)) {
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

        // now that all DEs are added, find sister elements
        DataElementValue bigSister = null;
        for (final DataElementValue deVal : repeat.getDataElements()) {
            if (isNewOtherSpecifyField(deVal)) {
                bigSister = deVal;
            } else if (bigSister != null && isSisterField(deVal)) {
                // assume that they need to be next to each other and the specify field comp is a text field
                if (deVal.getPosition() == bigSister.getPosition() + 1 && deVal.getComp() instanceof JTextField) {
                    bigSister.setOtherSpecifyField((JTextField) deVal.getComp());
                }
            } else {
                // didn't find a sister, so reset the saved big sister
                bigSister = null;
            }
        }

        return repeat;
    }

    private void disableUnchangableFields(final String elementName, final Component c) {
        final String[] unchangableElements = new String[] {IMG_HASH_CODE_ELEMENT_NAME, IMG_FILE_ELEMENT_NAME, IMG_PREVIEW_ELEMENT_NAME};
        for (final String e : unchangableElements) {
            if (elementName.equalsIgnoreCase(e)) {
                c.setEnabled(false);
            }
        }
    }

    @Override
    public void focusGained(final FocusEvent e) {}

    @Override
    public void focusLost(final FocusEvent e) {
        validateFields();
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
                                if ( !isGuid(value.trim())) {
                                    errs.add(title + " must begin with a valid GUID prefix");
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

    private void parseForInitLabelsAndComponents() {
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
                    browseButton.setActionCommand("browse_-_" + repeat.getGroupInfo().getName() + "_-_" + repeat.getRepeatNumber() + "_-_" + deInfo.getName());
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

                    if (isLegacyOtherSpecifyField(deVal)) {
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
                InfoDialog csvDialog = new InfoDialog(this, dsName, false, false, record);
                if (progressInc > 0) {
                    progressBar.updateValue(progressBar.getValue() + progressInc);
                } else if ( (i % rowsPerInc) == 0) {
                    progressBar.updateValue(progressBar.getValue() + 1);
                }

                // change i counter to 0-based for problem lists
                csvProblemTagList.add(i - 1, csvDialog.getProblemTags());
                csvProblemFileDirList.add(i - 1, csvDialog.getProblemFileDir());
                csvProblemFileNameList.add(i - 1, csvDialog.getProblemFileName());

                i++;
            }
            final long csvReadEndTime = System.currentTimeMillis();
            System.out.println("CSV input read took " + ( (csvReadEndTime - csvReadStartTime) / 1000) + " seconds (" + recordList.size() + " records)");

            for (int j = 0; j < csvProblemTagList.size(); j++) {
                final Vector<FileDicomTag> problemTags = csvProblemTagList.get(j);
                if (problemTags != null) {
                    boolean isDeidentified = deidentificationDialogDicom(csvProblemFileDirList.get(j), csvProblemFileNameList.get(j), problemTags);

                    if ( !isDeidentified) {
                        // should have already exited
                        continue;
                    }
                }
            }

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
        setTitle("Image Submission Package Creation Tool - " + pluginVersion + " (" + ddEnvName + ")");
        dsMainPanel = new JPanel(new GridBagLayout());
        final JScrollPane tabScrollPane = new JScrollPane(dsMainPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
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

        class MyListener implements ProgressChangeListener {
            @Override
            public void progressStateChanged(final ProgressChangeEvent e) {
                // do nothing
            }
        }
        ;
        final MyListener listener = new MyListener();

        if (origImage.is2DImage()) {
            // Creating a blank TransMatrix for resampling
            final TransMatrix percentSizer = new TransMatrix(4);
            percentSizer.set(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1);

            // Resample image size based on percent inputted
            final AlgorithmTransform transformer = new AlgorithmTransform(origImage, percentSizer, 1,
                    (float) (origImage.getResolutions(0)[0] / (percentage * .01)), (float) (origImage.getResolutions(0)[1] / (percentage * .01)),
                    (int) (origImage.getExtents()[0] * percentage * .01), (int) (origImage.getExtents()[1] * percentage * .01), origImage.getUnitsOfMeasure(),
                    false, true, false, true, origImage.getImageCentermm(false));
            transformer.addProgressChangeListener(listener);
            transformer.runAlgorithm();
            thumbnailImage = transformer.getTransformedImage();
            thumbnailImage.calcMinMax();
            // convert this image to color image if it is not
            if ( !thumbnailImage.isColorImage()) {
                final ModelImage newRGB = new ModelImage(ModelStorageBase.ARGB, thumbnailImage.getExtents(), thumbnailImage.getImageName());
                final AlgorithmRGBConcat mathAlgo = new AlgorithmRGBConcat(thumbnailImage, thumbnailImage, thumbnailImage, newRGB, true, true, 255.0f, true);
                mathAlgo.addProgressChangeListener(listener);
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
                lightGen.addProgressChangeListener(listener);
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
            subsetAlgo.addProgressChangeListener(listener);
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
                lightGen.addProgressChangeListener(listener);
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
            // format: "structname_-_PREFIXGUID"
            final String lowerName = getStructFromString(tableName).toLowerCase();
            if ( !csvStructRowData.containsKey(lowerName)) {
                FormStructure dsInfo = null;
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
                        elementHeader += CSV_OUTPUT_DELIM + g.getName() + "." + deList.get(deNum).getStructuralDataElement().getName();
                    }
                }

                String structHeader = dsInfo.getShortName() + CSV_OUTPUT_DELIM + dsInfo.getVersion() + cStr + "\n";
                structHeader += elementHeader + "\n";
                csvStructRowData.put(lowerName, structHeader);
            }
        }

        // base names for the output csv files and the directories that contain the attached files
        final Hashtable<String, String> structOutputBaseNameList = new Hashtable<String, String>();
        for (final String lowerName : csvStructRowData.keySet()) {
            structOutputBaseNameList.put(lowerName, lowerName + "_output_" + System.currentTimeMillis());
        }

        // for (final String lowerName : csvStructRowData.keySet()) {
        // System.out.println("**** " + lowerName);
        // System.out.println(csvStructRowData.get(lowerName));
        // }

        for (int i = 0; i < numDataStructs; i++) {
            int collisionCounter = 1;
            final String name = (String) structTableModel.getValueAt(i, 0);

            final String guid = getGuidFromString(name);
            final String dsName = getStructFromString(name);

            final String outputFileNameBase = guid + "_" + dsName + "_" + System.currentTimeMillis();

            // create subdir where attached files will be created/copied
            final String outputSubDir = outputDirBase + File.separator + structOutputBaseNameList.get(dsName.toLowerCase()) + File.separator;
            final File outputSubDirFile = new File(outputSubDir);
            if ( !outputSubDirFile.exists()) {
                outputSubDirFile.mkdirs();
            }

            final ImgFileInfo imgFileInfo = structRowImgFileInfoList.get(i);

            if (isImagingStructure(dsName) && imgFileInfo != null) {

                // this means we are working with the image datastructure
                printlnToLog("Creating submission file for " + name);

                // printlnToLog("Opening: " + imageFile + ", multifile: " + multifiles.get(i));

                final List<String> origFiles = imgFileInfo.getOrigFiles();

                printlnToLog("Creating thumbnail image:\t" + outputSubDir + outputFileNameBase + ".jpg");

                MemoryImageSource thumbnailImageData = imgFileInfo.getThumbnailImgData();

                if (thumbnailImageData != null) {
                    final FileWriteOptions opts = new FileWriteOptions(outputFileNameBase + ".jpg", outputSubDir, true);
                    writeThumbnailJIMI(thumbnailImageData, opts);
                }

                String imagePath = null;
                if ( (imgFileInfo.getImgFilePath().endsWith(".zip") && new File(imgFileInfo.getImgFilePath()).exists())) {
                    // the files were already zipped - so don't zip again. copy to output dir
                    try {
                        final File srcFile = new File(imgFileInfo.getImgFilePath());
                        final File destFile = new File(outputSubDir + outputFileNameBase + "_" + srcFile.getName());
                        printlnToLog("Copying original image zip file into output directory:\t" + destFile.getAbsolutePath());
                        FileUtils.copyFile(srcFile, destFile);
                        imagePath = destFile.getAbsolutePath();
                    } catch (final IOException e) {
                        MipavUtil.displayError("Unable to copy image zip file into output directory");
                        e.printStackTrace();
                    }
                } else if ( ( (imgFileInfo.getImgFilePath().endsWith(".tar.gz") || imgFileInfo.getImgFilePath().endsWith(".tgz")) && new File(
                        imgFileInfo.getImgFilePath()).exists())) {
                    // the files were already tarballed - so don't zip again. copy to output dir
                    try {
                        final File srcFile = new File(imgFileInfo.getImgFilePath());
                        final File destFile = new File(outputSubDir + outputFileNameBase + "_" + srcFile.getName());
                        printlnToLog("Copying original image tarball file into output directory:\t" + destFile.getAbsolutePath());
                        FileUtils.copyFile(srcFile, destFile);
                        imagePath = destFile.getAbsolutePath();
                    } catch (final IOException e) {
                        MipavUtil.displayError("Unable to copy image tarball file into output directory");
                        e.printStackTrace();
                    }
                } else if (imgFileInfo.getOrigFiles().size() == 1 && new File(imgFileInfo.getOrigFiles().get(0)).exists()) {
                    // the image is just one file, so don't zip. copy to output dir
                    try {
                        final File srcFile = new File(imgFileInfo.getOrigFiles().get(0));
                        File destFile;
                        
                        // make sure the single file we're copying over has an extension.  
                        // a bug in the validation tool requires that 'Triplanar' type data elements have an extension
                        if (FilenameUtils.getExtension(srcFile.getName()).equals("")) {
                            String ext = FileTypeTable.getFileTypeInfo(imgFileInfo.getFileFormat()).getDefaultExtension();
                            if (ext == null || ext.equals("")) {
                                ext = ".ima";
                            }
                            
                            destFile = new File(outputSubDir + outputFileNameBase + "_" + srcFile.getName() + ext);
                        } else {
                            destFile = new File(outputSubDir + outputFileNameBase + "_" + srcFile.getName());
                        }
                        
                        printlnToLog("Copying original image file into output directory:\t" + destFile.getAbsolutePath());
                        FileUtils.copyFile(srcFile, destFile);
                        imagePath = destFile.getAbsolutePath();
                    } catch (final IOException e) {
                        MipavUtil.displayError("Unable to copy original image file into output directory");
                        e.printStackTrace();
                    }
                } else {
                    // need to create a zip file with the original image files
                    try {
                        final String zipFilePath = outputSubDir + outputFileNameBase + ".zip";
                        printlnToLog("Creating ZIP file:\t" + zipFilePath);
                        for (final String file : origFiles) {
                            printlnToLog("Adding file to ZIP:\t" + file);
                        }

                        makeZipFile(zipFilePath, origFiles);
                        imagePath = zipFilePath;
                    } catch (final IOException ioe) {
                        ioe.printStackTrace();
                        MipavUtil.displayError("Unable to write original image dataset files to ZIP package:\n" + ioe.getMessage());
                        continue;
                    }
                }

                // calculate hash of the zip file and then put it into the image
                // file hash code CDE (if it exists in the struct)
                String hashCode = null;
                try {
                    hashCode = computeFileHash(imagePath);
                } catch (final IOException e) {
                    e.printStackTrace();
                    MipavUtil.displayError("Unable to calculate hash code of image file:\n" + e.getMessage());
                    continue;
                }

                final FormStructureData fsData = fsDataList.get(i);
                int maxRepeatNum = 0;
                for (final RepeatableGroup group : fsData.getStructInfo().getRepeatableGroups()) {
                    final int numRepeats = fsData.getNumGroupRepeats(group.getName());
                    if (maxRepeatNum < numRepeats) {
                        maxRepeatNum = numRepeats;
                    }

                    for (final GroupRepeat repeat : fsData.getAllGroupRepeats(group.getName())) {
                        for (final DataElementValue deVal : repeat.getDataElements()) {
                            // Main imaging file, thumbnail, hashcode, other files
                            if (isMainImagingFileElement(deVal)) {
                                deVal.setValue(imagePath);
                            } else if (deVal.getName().equalsIgnoreCase(IMG_PREVIEW_ELEMENT_NAME)) {
                                deVal.setValue(structOutputBaseNameList.get(dsName.toLowerCase()) + File.separator + outputFileNameBase + ".jpg");
                            } else if (deVal.getName().equalsIgnoreCase(IMG_HASH_CODE_ELEMENT_NAME) && hashCode != null) {
                                deVal.setValue(hashCode);
                            } else if ( (deVal.getDataElementInfo().getType() == DataType.FILE || deVal.getDataElementInfo().getType() == DataType.TRIPLANAR)
                                    && !deVal.getValue().equals("")) {
                                final String srcFileValue = deVal.getValue();

                                if (srcFileValue.contains(BROWSE_NONIMG_DELIM)) {
                                    // if the user selected multiple files, zip them all up
                                    try {
                                        final String[] filePaths = srcFileValue.split(BROWSE_NONIMG_DELIM);
                                        final String newZipPath = outputSubDir + outputFileNameBase + "_" + group.getName() + "_" + repeat.getRepeatNumber()
                                                + "_" + deVal.getName() + ".zip";
                                        printlnToLog("Creating ZIP file:\t" + newZipPath);
                                        final List<String> filePathList = new ArrayList<String>();
                                        for (final String file : filePaths) {
                                            printlnToLog("Adding file to ZIP:\t" + file);
                                            filePathList.add(file);
                                        }
                                        makeZipFile(newZipPath, filePathList);

                                        // now that the zip file is created, set the de value to the zip file path
                                        deVal.setValue(newZipPath);
                                    } catch (final IOException ioe) {
                                        ioe.printStackTrace();
                                        MipavUtil.displayError("Unable to write files to ZIP package:\n" + ioe.getMessage());
                                        continue;
                                    }
                                } else {
                                    // only one file selected; copy it
                                    final File srcFile = new File(srcFileValue);
                                    try {
                                        final File destFile = new File(outputSubDir + outputFileNameBase + "_" + srcFile.getName());
                                        printlnToLog("Copying attached file into output directory:\t" + destFile.getAbsolutePath());
                                        FileUtils.copyFile(srcFile, destFile);
                                        deVal.setValue(destFile.getAbsolutePath());
                                    } catch (final IOException e) {
                                        MipavUtil.displayError("Unable to copy file into output directory");
                                        e.printStackTrace();
                                    }
                                }
                            }
                        }
                    }
                }

                for (int curRepeat = 0; curRepeat < maxRepeatNum; curRepeat++) {
                    final String newRow = getCSVDataRow(outputSubDir, outputFileNameBase, fsData, curRepeat);
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
            } else {

                // this means that this is another data structure besides image

                printlnToLog("Creating submission file for " + name);

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
                            if (isMainImagingFileElement(deVal)) {
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
                    final String newRow = getCSVDataRow(outputDirBase, outputFileNameBase, fsData, curRepeat);
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
                final String csvFileName = structOutputBaseNameList.get(lowerName) + ".csv";

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
                    try {
                        FileUtils.deleteDirectory(f);
                    } catch (final IOException e) {
                        e.printStackTrace();
                    }
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

        selectBIDSButton = new JButton("Select BIDS Directory");
        selectBIDSButton.setToolTipText("Select BIDS Root Directory");
        selectBIDSButton.addActionListener(this);
        selectBIDSButton.setActionCommand("SelectBIDS");

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

        selectBIDSButton.setPreferredSize(MipavUtil.defaultButtonSize);
        addStructButton.setPreferredSize(MipavUtil.defaultButtonSize);
        removeStructButton.setPreferredSize(MipavUtil.defaultButtonSize);
        finishButton.setPreferredSize(MipavUtil.defaultButtonSize);
        editDataElementsButton.setPreferredSize(MipavUtil.defaultButtonSize);

        addStructButton.setEnabled(false);
        selectBIDSButton.setEnabled(false);
        loadCSVButton.setEnabled(false);
        removeStructButton.setEnabled(false);
        editDataElementsButton.setEnabled(false);
        finishButton.setEnabled(false);

        gbc.gridx = 0;
        buttonPanel1.add(loadCSVButton, gbc);
        gbc.gridx++;
        buttonPanel1.add(selectBIDSButton, gbc);
        gbc.gridx++;
        buttonPanel1.add(addStructButton, gbc);
        gbc.gridx++;
        buttonPanel1.add(removeStructButton, gbc);
        gbc.gridx++;
        buttonPanel1.add(editDataElementsButton, gbc);
        gbc.gridx++;
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
     * @param imgData The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeThumbnailJIMI(final MemoryImageSource imgData, final FileWriteOptions options) {

        final int extIndex = options.getFileName().indexOf(".");
        final String prefix = options.getFileName().substring(0, extIndex); // Used
                                                                            // for
                                                                            // setting
                                                                            // file
                                                                            // name
        final String fileSuffix = options.getFileName().substring(extIndex);
        String name;

        name = options.getFileDirectory() + prefix + fileSuffix;

        try {
            Image img = createImage(imgData);
            Jimi.putImage(img, name);
            img.flush();
            img = null;
        } catch (final JimiException jimiException) {
            Preferences.debug("JIMI write error: " + jimiException + "\n", Preferences.DEBUG_FILEIO);

            jimiException.printStackTrace();

            return false;
        }

        return true;
    }

    /**
     * Creates a set of thumbnail image data from a lightbox ModelImage.
     * 
     * @param image The lightbox image to create an Image from.
     * 
     * @return The image data, ready for rendering/JIMI writing.
     */
    private MemoryImageSource createThumbnailDataForWriting(final ModelImage image) {
        if (image == null) {
            return null;
        }

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

        return new MemoryImageSource(image.getExtents()[0], image.getExtents()[1], paintBuffer, 0, image.getExtents()[0]);
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

    /**
     * Checks if the given string starts with an allowed GUID prefix.
     * 
     * @param str A string to check.
     * @return True if the string starts with one of the BRICS prefixes (case sensitive).
     */
    private static final boolean isGuid(final String str) {
        if (str == null || str.equals("")) {
            return false;
        }

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
            if (str.contains(STRUCT_GUID_SEPERATOR + prefix)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Extracts the form structure name from a string in the format 'structname_-_BRICSGUID'.
     * 
     * @param str A string in the format 'structname_-_BRICSGUID'.
     * @return The form structure name from the given string or null if it could not be found (if no GUID prefix was
     *         found to initiate the parsing).
     */
    private static final String getStructFromString(final String str) {
        for (final String prefix : allowedGuidPrefixes) {
            if (str.contains(STRUCT_GUID_SEPERATOR + prefix)) {
                return str.substring(0, str.lastIndexOf(STRUCT_GUID_SEPERATOR + prefix));
            }
        }

        return null;
    }

    /**
     * Extracts the GUID from a string in the format 'structname_-_BRICSGUID'.
     * 
     * @param str A string in the format 'structname_-_BRICSGUID'.
     * @return The GUID from the given string or null if it could not be found (if no GUID prefix was found to initiate
     *         the parsing).
     */
    private static final String getGuidFromString(final String str) {
        for (final String prefix : allowedGuidPrefixes) {
            if (str.contains(STRUCT_GUID_SEPERATOR + prefix)) {
                return str.substring(str.lastIndexOf(STRUCT_GUID_SEPERATOR + prefix) + 3, str.length());
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
        if (date == null) {
            return "";
        }

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
     * @param time A time string in the format hh:mm:ss.fract or hhmmss.fract.
     * @return An ISO 8601 formatted version of the given date and time (or the original string if not in the DICOM/US
     *         date format).
     */
    private static final String convertDateTimeToISOFormat(final String date, String time) {
        if (date == null) {
            return "";
        }
        if (time == null) {
            time = "";
        }

        String isoDate = date.trim();
        String isoTime = time.trim();

        final String datePattern = "^(\\d{1,2})[/-]*(\\d{1,2})[/-]*(\\d{4})$";
        final String timePattern = "^(\\d{2})[:]?(\\d{2})[:]?(\\d{2})[.]?(\\d*)$";

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
            final String frac = m.group(4);
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
            if (frac.length() > 0) {
                isoTime += "." + frac;
            }
        }

        if (isoTime.equals("")) {
            return isoDate;
        } else {
            return isoDate + "T" + isoTime;
        }
    }

    /**
     * Tries to convert a MIPAV/DICOM modality string to the equivalent BRICS CDE value. Still needs a good bit of
     * work/additions/integration with MR sequence type.
     * 
     * @param mipavModality The MIPAV modality description string.
     * @param contrastUsed Whether a contrast agent was used in the scan.
     * @return The BRICS ImgModltyTyp CDE value, or an empty string if no matching modality was found.
     */
    private static final String convertModalityToBRICS(final String mipavModality, final boolean contrastUsed) {
        if (mipavModality.equalsIgnoreCase("Unknown Modality")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Biomagnetic Imaging")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Color Flow Doppler")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Computed Radiography")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Computed Tomography")) {
            if (contrastUsed) {
                return "Contrast CT";
            } else {
                return "Non-contrast CT";
            }
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
            return "MR Angiography";
        } else if (mipavModality.equalsIgnoreCase("Mammography")) {
            return "";
        } else if (mipavModality.equalsIgnoreCase("Magnetic Resonance")) {
            if (contrastUsed) {
                return "Contrast MRI";
            } else {
                return "MRI";
            }
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
    
    private static final int determineModality(int curModality, String structName) {
        int newModality = curModality;
        
        final String upperStructureName = structName.toUpperCase();
        
        // if no modality, try to guess from structure being used
        if (curModality == FileInfoBase.UNKNOWN_MODALITY) {
            
            if (upperStructureName.endsWith("IMAGINGMR")) {
                newModality = FileInfoBase.MAGNETIC_RESONANCE;
            } else if (upperStructureName.endsWith("IMAGINGCT")) {
                newModality = FileInfoBase.COMPUTED_TOMOGRAPHY;
            } else if (upperStructureName.endsWith("IMAGINGDIFFUSION")) {
                newModality = FileInfoBase.MAGNETIC_RESONANCE;
            } else if (upperStructureName.endsWith("IMAGINGFUNCTIONALMR")) {
                newModality = FileInfoBase.MAGNETIC_RESONANCE;
            } else if (upperStructureName.endsWith("IMAGINGPET")) {
                newModality = FileInfoBase.POSITRON_EMISSION_TOMOGRAPHY;
            } else if (upperStructureName.endsWith("IMAGINGSPECT")) {
                newModality = FileInfoBase.SINGLE_PHOTON_EMISSION_COMPUTED_TOMOGRAPHY;
            }
        } else if (curModality == FileInfoBase.NUCLEAR_MEDICINE && upperStructureName.endsWith("IMAGINGSPECT")) {
            // some PDBP/LBP SPECT data reports as the dicom modality "nuclear medicine"
            newModality = FileInfoBase.SINGLE_PHOTON_EMISSION_COMPUTED_TOMOGRAPHY;
        }
        
        return newModality;
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
        if (manuf == null || manuf.equals("")) {
            return manuf;
        }

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
        if (model == null || model.equals("")) {
            return model;
        }

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
        final String num = dicomAge.substring(0, dicomAge.length() - 1);
        final String type = dicomAge.substring(dicomAge.length() - 1, dicomAge.length());

        if (type.equalsIgnoreCase("D")) {
            return Integer.toString((int) (Integer.parseInt(num) / 30.4166666667));
        } else if (type.equalsIgnoreCase("W")) {
            return Integer.toString((int) (Integer.parseInt(num) / 4.34523809524));
        } else if (type.equalsIgnoreCase("M")) {
            return Integer.valueOf(num).toString();
        } else if (type.equalsIgnoreCase("Y")) {
            return Integer.toString(Integer.parseInt(num) * 12);
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
                    if (item.equalsIgnoreCase(VALUE_OTHER_SPECIFY) || item.equalsIgnoreCase(VALUE_YES_SPECIFY)) {
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
                        if (deVal.getOtherSpecifyField() != null) {
                            deVal.getOtherSpecifyField().setText(value);
                        }
                        System.err.println("Other specify\t" + deVal.getName() + "\t\t" + value);
                    } else {
                        System.err.println("Value not found. DE:\t" + comp.getName() + "\t" + value);
                    }
                }
            } else if (comp instanceof JList) {
                final JList list = (JList) comp;
                final ListModel listModel = list.getModel();

                boolean found = false;
                int otherSpecifyIndex = -1;
                boolean foundOtherInDE = false;

                // values are assumed to never contain semi-colons since that's the delimiter
                final String[] valueSplit = value.split(MULTI_SELECT_VALUE_DELIM);

                final ArrayList<Integer> selectedIndicies = new ArrayList<Integer>();
                for (final String val : valueSplit) {
                    for (int k = 0; k < listModel.getSize(); k++) {
                        final String item = (String) listModel.getElementAt(k);
                        if ( !foundOtherInDE && item.equalsIgnoreCase(VALUE_OTHER_SPECIFY) || item.equalsIgnoreCase(VALUE_YES_SPECIFY)) {
                            foundOtherInDE = true;
                            otherSpecifyIndex = k;
                        }

                        if (item.equalsIgnoreCase(val)) {
                            selectedIndicies.add(k);
                            found = true;
                            break;
                        }
                    }

                    if ( !found) {
                        if (foundOtherInDE) {
                            selectedIndicies.add(otherSpecifyIndex);
                            if (deVal.getOtherSpecifyField() != null) {
                                deVal.getOtherSpecifyField().setText(val);
                            }
                            System.err.println("Other specify\t" + deVal.getName() + "\t\t" + val);
                        } else {
                            System.err.println("Value not found. DE:\t" + comp.getName() + "\t" + val);
                        }
                    }
                }

                final int[] intArray = new int[selectedIndicies.size()];
                for (int i = 0; i < selectedIndicies.size(); i++) {
                    intArray[i] = selectedIndicies.get(i);
                }

                list.setSelectedIndices(intArray);
                if (intArray.length > 0) {
                    list.ensureIndexIsVisible(intArray[0]);
                }
            } else {
                System.err.println("Unrecognized component type (" + comp.getName() + "):\t" + comp.getClass().getName());
            }
        }
    }

    /**
     * Returns whether this field is a legacy other specify field, where the specified value is put in as free form
     * instead of being a separate DE.
     */
    private static final boolean isLegacyOtherSpecifyField(final DataElementValue deVal) {
        if (deVal.getDataElementInfo().getRestrictions() == InputRestrictions.FREE_FORM && deVal.getComp() instanceof JComboBox) {
            final JComboBox combo = (JComboBox) deVal.getComp();
            for (int i = 0; i < combo.getItemCount(); i++) {
                if ( ((String) combo.getItemAt(i)).trim().equalsIgnoreCase(VALUE_OTHER_SPECIFY)
                        || ((String) combo.getItemAt(i)).trim().equalsIgnoreCase(VALUE_YES_SPECIFY)) {
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * Returns whether this field is a new other specify field.
     */
    private static final boolean isNewOtherSpecifyField(final DataElementValue deVal) {
        if (deVal.getDataElementInfo().getRestrictions() == InputRestrictions.SINGLE
                || deVal.getDataElementInfo().getRestrictions() == InputRestrictions.MULTIPLE) {
            if (deVal.getComp() instanceof JComboBox) {
                final JComboBox combo = (JComboBox) deVal.getComp();
                for (int i = 0; i < combo.getItemCount(); i++) {
                    if ( ((String) combo.getItemAt(i)).trim().equalsIgnoreCase(VALUE_OTHER_SPECIFY)
                            || ((String) combo.getItemAt(i)).trim().equalsIgnoreCase(VALUE_YES_SPECIFY)) {
                        return true;
                    }
                }
            } else if (deVal.getComp() instanceof JList) {
                final ListModel listModel = ((JList) deVal.getComp()).getModel();
                for (int k = 0; k < listModel.getSize(); k++) {
                    final String item = (String) listModel.getElementAt(k);
                    if (item.trim().equalsIgnoreCase(VALUE_OTHER_SPECIFY) || item.trim().equalsIgnoreCase(VALUE_YES_SPECIFY)) {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    /**
     * Returns whether this field is an Other, specify or Yes, specify value entry field (sister field).
     */
    private static final boolean isSisterField(final DataElementValue deVal) {
        if (deVal.getName().endsWith(ELEM_OTHER_SPECIFY_SUFFIX) || deVal.getName().endsWith(ELEM_YES_SPECIFY_SUFFIX)) {
            return true;
        }

        return false;
    }

    private static final boolean isValueSet(final String val) {
        if (val != null && !val.trim().equals("")) {
            return true;
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

    private static final boolean isMainImagingFileElement(final DataElementValue deVal) {
        return isMainImagingFileElement(deVal.getGroupName(), deVal.getName());
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
        for (final String prefix : PDBP_IMAGING_STRUCTURE_PREFIX_LIST) {
            if (structureName.toLowerCase().startsWith(prefix.toLowerCase())) {
                return true;
            }
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

        private final ArrayList<String> diseaseAL = new ArrayList<String>();

        private JScrollPane structsScrollPane;

        public ChooseDataStructDialog(final PlugInDialogFITBIR owner) {
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
            for (final FormStructure ds : dataStructureList) {
                if (ds.getShortName().equals("")) {
                    // something is wrong. a shortname is required. this is to work around an apparent stage DDT problem
                    continue;
                }
                final String desc = ds.getDescription();
                final String shortname = ds.getShortName();
                final String version = ds.getVersion().toString();
                final String status = ds.getStatus().toString();
                final String disease = ds.getDiseaseStructureString();

                descAL.add(desc);
                shortNameAL.add(shortname);
                versionAL.add(version);
                statusAL.add(status);
                diseaseAL.add(disease);
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
                        rowData[4] = diseaseAL.get(i);
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

        private Vector<FileDicomTag> problemTags = null;

        private String problemTagsFileDir;

        private String problemTagsFileName;

        private final ArrayList<File> allOtherFiles = new ArrayList<File>();

        private boolean addedPreviewImage = false;

        private FormStructure dataStructure;

        private final boolean setInitialVisible;

        private final ArrayList<ArrayList<String>> record;

        private final String[] unchangableElements = new String[] {IMG_HASH_CODE_ELEMENT_NAME, IMG_FILE_ELEMENT_NAME, IMG_PREVIEW_ELEMENT_NAME};

        private String currFile;

        private boolean validFile;

        public InfoDialog(final PlugInDialogFITBIR owner, final String name, final boolean launchedFromInProcessState, final boolean setInitialVisible,
                final ArrayList<ArrayList<String>> record) {
            super(owner, false);

            this.owner = owner;
            this.launchedFromInProcessState = launchedFromInProcessState;
            this.setInitialVisible = setInitialVisible;
            this.record = record;

            if (launchedFromInProcessState) {
                if (containsGuid(name)) {
                    this.dataStructureName = getStructFromString(name);
                } else {
                    this.dataStructureName = name.substring(0, name.lastIndexOf(STRUCT_GUID_SEPERATOR));
                }
            } else {
                previewImages.add(null);
                structRowImgFileInfoList.add(null);
                fsDataList.add(null);
                allOtherFilesAL.add(null);
                this.dataStructureName = name;
            }

            for (final FormStructure ds : dataStructureList) {
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
                MipavUtil.displayError("Form structure not found in Data Dictionary: " + dataStructureName);
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

                            StructuralDataElement de = null;
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
                            // basic check that image data is de-identified
                            problemTags = deidentificationCheckDicomTags(srcImage);
                            problemTagsFileDir = srcImage.getFileInfo(0).getFileDirectory();
                            problemTagsFileName = srcImage.getFileInfo(0).getFileName();

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

                        if ( !fsData.isDataElementInForm(deGroupAndName[0], deGroupAndName[1])) {
                            MipavUtil.displayError("Unable to find CSV data element in form: " + csvFieldNames.get(i).trim());
                            continue;
                        }

                        String value = repeatValues.get(i).trim();
                        if (i != imageFileIndex && !value.equals("")) {
                            final GroupRepeat curRepeat = fsData.getGroupRepeat(deGroupAndName[0], curRepeatNum);
                            for (final DataElementValue deVal : curRepeat.getDataElements()) {
                                final JComponent comp = deVal.getComp();
                                if (deVal.getName().equalsIgnoreCase(deGroupAndName[1])) {
                                    if (comp instanceof JTextField) {
                                        // if file type, check for relative vs. absolute path
                                        if ( (deVal.getDataElementInfo().getType() == DataType.FILE || deVal.getDataElementInfo().getType() == DataType.TRIPLANAR)
                                                && ! (new File(value).isAbsolute())) {
                                            value = csvFile.getParentFile().getAbsolutePath() + File.separator + value;
                                        }
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
                                            if (deVal.getOtherSpecifyField() != null) {
                                                deVal.getOtherSpecifyField().setText(value);
                                            }
                                            combo.setSelectedItem(VALUE_OTHER_SPECIFY);
                                            System.err.println("Other specify\t" + deVal.getName() + "\t\t" + value);
                                        }
                                    } else if (comp instanceof JList) {
                                        final JList list = (JList) comp;
                                        final ListModel listModel = list.getModel();

                                        boolean found = false;
                                        int otherSpecifyIndex = -1;
                                        boolean foundOtherInDE = false;

                                        // values are assumed to never contain semi-colons since that's the delimiter
                                        final String[] valueSplit = value.split(MULTI_SELECT_VALUE_DELIM);

                                        final ArrayList<Integer> selectedIndicies = new ArrayList<Integer>();
                                        for (final String val : valueSplit) {
                                            for (int k = 0; k < listModel.getSize(); k++) {
                                                final String item = (String) listModel.getElementAt(k);
                                                if ( !foundOtherInDE
                                                        && (item.equalsIgnoreCase(VALUE_OTHER_SPECIFY) || item.equalsIgnoreCase(VALUE_YES_SPECIFY))) {
                                                    foundOtherInDE = true;
                                                    otherSpecifyIndex = k;
                                                }

                                                if (item.equalsIgnoreCase(val)) {
                                                    selectedIndicies.add(k);
                                                    found = true;
                                                    break;
                                                }
                                            }

                                            if ( !found) {
                                                if (foundOtherInDE) {
                                                    selectedIndicies.add(otherSpecifyIndex);
                                                    if (deVal.getOtherSpecifyField() != null) {
                                                        deVal.getOtherSpecifyField().setText(val);
                                                    }
                                                    System.err.println("Other specify\t" + deVal.getName() + "\t\t" + val);
                                                } else {
                                                    System.err.println("Value not found. DE:\t" + comp.getName() + "\t" + val);
                                                }
                                            }
                                        }

                                        final int[] intArray = new int[selectedIndicies.size()];
                                        for (int k = 0; k < selectedIndicies.size(); k++) {
                                            intArray[k] = selectedIndicies.get(k);
                                        }

                                        list.setSelectedIndices(intArray);
                                        if (intArray.length > 0) {
                                            list.ensureIndexIsVisible(intArray[0]);
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

                        if ( !fsData.isDataElementInForm(deGroupAndName[0], deGroupAndName[1])) {
                            MipavUtil.displayError("Unable to find CSV data element in form: " + csvFieldNames.get(i).trim());
                            continue;
                        }

                        String value = repeatValues.get(i).trim();
                        if ( !value.equals("")) {
                            final GroupRepeat curRepeat = fsData.getGroupRepeat(deGroupAndName[0], curRepeatNum);
                            for (final DataElementValue deVal : curRepeat.getDataElements()) {
                                final JComponent comp = deVal.getComp();
                                if (deVal.getName().equalsIgnoreCase(deGroupAndName[1])) {
                                    if (comp instanceof JTextField) {
                                        // if file type, check for relative vs. absolute path
                                        if ( (deVal.getDataElementInfo().getType() == DataType.FILE || deVal.getDataElementInfo().getType() == DataType.TRIPLANAR)
                                                && ! (new File(value).isAbsolute())) {
                                            value = csvFile.getParentFile().getAbsolutePath() + File.separator + value;
                                        }
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
                                            if (deVal.getOtherSpecifyField() != null) {
                                                deVal.getOtherSpecifyField().setText(value);
                                            }
                                            combo.setSelectedItem(VALUE_OTHER_SPECIFY);
                                            // System.err.println("Other specify\t" + deVal.getName() + "\t\t" + value);
                                        }
                                    } else if (comp instanceof JList) {
                                        final JList list = (JList) comp;
                                        final ListModel listModel = list.getModel();

                                        boolean found = false;
                                        int otherSpecifyIndex = -1;
                                        boolean foundOtherInDE = false;

                                        // values are assumed to never contain semi-colons since that's the delimiter
                                        final String[] valueSplit = value.split(MULTI_SELECT_VALUE_DELIM);

                                        final ArrayList<Integer> selectedIndicies = new ArrayList<Integer>();
                                        for (final String val : valueSplit) {
                                            for (int k = 0; k < listModel.getSize(); k++) {
                                                final String item = (String) listModel.getElementAt(k);
                                                if ( !foundOtherInDE
                                                        && (item.equalsIgnoreCase(VALUE_OTHER_SPECIFY) || item.equalsIgnoreCase(VALUE_YES_SPECIFY))) {
                                                    foundOtherInDE = true;
                                                    otherSpecifyIndex = k;
                                                }

                                                if (item.equalsIgnoreCase(val)) {
                                                    selectedIndicies.add(k);
                                                    found = true;
                                                    break;
                                                }
                                            }

                                            if ( !found) {
                                                if (foundOtherInDE) {
                                                    selectedIndicies.add(otherSpecifyIndex);
                                                    if (deVal.getOtherSpecifyField() != null) {
                                                        deVal.getOtherSpecifyField().setText(val);
                                                    }
                                                    System.err.println("Other specify\t" + deVal.getName() + "\t\t" + val);
                                                } else {
                                                    System.err.println("Value not found. DE:\t" + comp.getName() + "\t" + val);
                                                }
                                            }
                                        }

                                        final int[] intArray = new int[selectedIndicies.size()];
                                        for (int k = 0; k < selectedIndicies.size(); k++) {
                                            intArray[k] = selectedIndicies.get(k);
                                        }

                                        list.setSelectedIndices(intArray);
                                        if (intArray.length > 0) {
                                            list.ensureIndexIsVisible(intArray[0]);
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
                    structRowImgFileInfoList.remove(structRowImgFileInfoList.size() - 1);
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
            // fileIO.setQuiet(true);
            ModelImage srcImage = null;
            validFile = true;
            File origSrcFile;

            try {
                if (imageFile.endsWith(".zip")) {

                    String destName = imageFile.replace("/", File.separator);
                    destName = destName.replace("\\", File.separator);
                    destName = destName.substring(destName.lastIndexOf(File.separator) + 1, destName.lastIndexOf("."));
                    // String destDirName =
                    final String tempDir = parentDir + File.separator + destName + "_temp_" + System.currentTimeMillis();
                    tempDirs.add(tempDir);

                    origSrcFile = new File(imageFile);
                    if ( !origSrcFile.isAbsolute()) {
                        origSrcFile = new File(parentDir + File.separator + imageFile);
                    }

                    String fileName = "";
                    // try {
                    final FileInputStream fis = new FileInputStream(origSrcFile);
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

                        final String entryName = entry.getName();

                        // implied directory in the zip file entry; create it
                        if (entryName.contains("/") || entryName.contains("\\")) {
                            final File entryFile = new File(tempDir + File.separator + entryName);
                            if ( !entryFile.getParent().equalsIgnoreCase(tempDir)) {
                                if ( !entryFile.getParentFile().exists()) {
                                    entryFile.getParentFile().mkdirs();
                                }
                            }
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
                                if ( !entry.getName().endsWith(".raw") && !entry.getName().equalsIgnoreCase(".METADATA")) {
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
                } else if (imageFile.endsWith(".tar.gz") || imageFile.endsWith(".tgz")) {

                    String destName = imageFile.replace("/", File.separator);
                    destName = destName.replace("\\", File.separator);
                    destName = destName.substring(destName.lastIndexOf(File.separator) + 1, destName.lastIndexOf(".t"));
                    // String destDirName =
                    final String tempDir = parentDir + File.separator + destName + "_temp_" + System.currentTimeMillis();
                    tempDirs.add(tempDir);

                    origSrcFile = new File(imageFile);
                    if ( !origSrcFile.isAbsolute()) {
                        origSrcFile = new File(parentDir + File.separator + imageFile);
                    }

                    String fileName = "";
                    // try {
                    final FileInputStream fis = new FileInputStream(origSrcFile);
                    final GZIPInputStream zin = new GZIPInputStream(new BufferedInputStream(fis));
                    final TarInputStream tin = new TarInputStream(zin);
                    FileOutputStream fout;
                    TarEntry entry;
                    BufferedOutputStream dest = null;
                    final int BUFFER = 2048;
                    int count;
                    final byte[] data = new byte[BUFFER];
                    File f;
                    // while we are at it, find the first file that does not
                    // have a .raw extension, so we can
                    // open it

                    while ( (entry = tin.getNextEntry()) != null) {
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
                                if ( !entry.getName().endsWith(".raw") && !entry.getName().equalsIgnoreCase(".METADATA")) {
                                    fileName = entry.getName();
                                }
                            }

                            fout = new FileOutputStream(tempDir + File.separator + entry.getName());
                            dest = new BufferedOutputStream(fout, BUFFER);
                            while ( (count = tin.read(data, 0, BUFFER)) != -1) {
                                dest.write(data, 0, count);
                            }
                            dest.flush();
                            dest.close();
                        }
                    }
                    tin.close();
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
                    origSrcFile = new File(imageFile);
                    if ( !origSrcFile.isAbsolute()) {
                        origSrcFile = new File(parentDir + File.separator + imageFile);
                    }
                    filePath = origSrcFile.getAbsolutePath();

                    // if directory, try to open all files within as multifile
                    if (origSrcFile.isDirectory()) {
                        isMultifile = true;
                        File[] fileList = origSrcFile.listFiles();
                        if (fileList.length > 0) {
                            origSrcFile = fileList[0];
                            filePath = origSrcFile.getAbsolutePath();

                            System.out.println("Opening from dir:\t" + filePath);
                        } else {
                            MipavUtil.displayError("Unable to open image files in specified directory: " + filePath);
                            validFile = false;
                            return null;
                        }
                    } else {
                        isMultifile = false;
                    }
                }

                final File file = new File(filePath);
                System.out.println(file);
                srcImage = fileIO.readImage(file.getName(), file.getParent() + File.separator, isMultifile, null);

                if (srcImage == null) {
                    MipavUtil.displayError("Unable to open image file specified: " + imageFile);
                    validFile = false;
                    return null;
                }

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

                ModelImage thumbnailImage = createThumbnailImage(srcImage);

                if (launchedFromInProcessState) {
                    final int selectedRow = structTable.getSelectedRow();
                    previewImages.set(selectedRow, previewImg);
                    previewImages.get(selectedRow).setSliceBrightness(previewImgBrightness, previewImgContrast);
                    structRowImgFileInfoList.set(selectedRow, new ImgFileInfo(origSrcFile.getAbsolutePath(), isMultifile,
                            FileUtility.getFileNameList(srcImage), srcImage.getFileInfo(0).getFileFormat(), createThumbnailDataForWriting(thumbnailImage)));
                } else {
                    final int size = previewImages.size();
                    previewImages.set(size - 1, previewImg);
                    previewImages.get(size - 1).setSliceBrightness(previewImgBrightness, previewImgContrast);
                    structRowImgFileInfoList.set(size - 1, new ImgFileInfo(origSrcFile.getAbsolutePath(), isMultifile, FileUtility.getFileNameList(srcImage),
                            srcImage.getFileInfo(0).getFileFormat(), createThumbnailDataForWriting(thumbnailImage)));
                }

                // cleanup thumbnail modelimage
                if (thumbnailImage != null) {
                    thumbnailImage.disposeLocal();
                    thumbnailImage = null;
                }

                previewImgPanel.validate();
                previewImgPanel.repaint();

            } catch (final FileNotFoundException e) {
                MipavUtil.displayError("The system cannot find the file specified: " + imageFile);
                e.printStackTrace();
                validFile = false;
            } catch (final NullPointerException e) {
                MipavUtil.displayError("Unable to open image file specified: " + imageFile);
                e.printStackTrace();
                validFile = false;
            } catch (final Exception e) {
                e.printStackTrace();
            }

            return srcImage;
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

                        if (isLegacyOtherSpecifyField(deVal)) {
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

                // special handling of SiteName for PDBP, where they want to use a set of permissible values
                // with the free-form DE
                if (isPDBPImagingStructure(dataStructure.getShortName()) && de.getStructuralDataElement().getName().equalsIgnoreCase(SITE_NAME_ELEMENT_NAME)) {
                    final JComboBox cb = new JComboBox();
                    cb.setName(de.getStructuralDataElement().getName());
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
                } else if (de.getStructuralDataElement().getValueRangeList() != null && de.getStructuralDataElement().getValueRangeList().size() > 0
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

                    disableUnchangableFields(de.getStructuralDataElement().getName(), tf);

                    if (de.getStructuralDataElement().getName().equalsIgnoreCase(IMG_HASH_CODE_ELEMENT_NAME)) {
                        tf.setText("Automatically generated from selected image files.");
                    } else if (de.getStructuralDataElement().getName().equalsIgnoreCase(IMG_PREVIEW_ELEMENT_NAME)) {
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

            // now that all DEs are added, find sister elements
            DataElementValue bigSister = null;
            for (final DataElementValue deVal : repeat.getDataElements()) {
                if (isNewOtherSpecifyField(deVal)) {
                    bigSister = deVal;
                } else if (bigSister != null && isSisterField(deVal)) {
                    // assume that they need to be next to each other and the specify field comp is a text field
                    if (deVal.getPosition() == bigSister.getPosition() + 1 && deVal.getComp() instanceof JTextField) {
                        bigSister.setOtherSpecifyField((JTextField) deVal.getComp());
                    }
                } else {
                    // didn't find a sister, so reset the saved big sister
                    bigSister = null;
                }
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
            int modality = img.getFileInfo(0).getModality();
            String modalityString = FileInfoBase.getModalityStr(modality);
            final float sliceThickness = img.getFileInfo(0).getSliceThickness();
            final int orient = img.getFileInfo(0).getImageOrientation();
            final String orientation = FileInfoBase.getImageOrientationStr(orient);

            final int fileFormatInt = img.getFileInfo(0).getFileFormat();
            final String fileFormatString = FileUtility.getFileTypeStr(fileFormatInt);
            
            modality = determineModality(modality, dataStructureName);
            modalityString = FileInfoBase.getModalityStr(modality);

            final ArrayList<String> csvFList = new ArrayList<String>();
            final ArrayList<String> csvPList = new ArrayList<String>();
            final ArrayList<String> headerList = new ArrayList<String>();

            String ageVal = null;
            String siteName = null;
            String visitDate = null;
            String visitTime = null;
            String sliceOversample = null;
            String gap = null;
            String bodyPart = null;

            String fieldOfView = null;
            String manufacturer = null;
            String softwareVersion = null;
            String patientPosition = null;

            String scannerModel = null;
            String bandwidth = null;

            String scanOptions = null;
            String flowCompensation = null;

            String patientName = null;
            String patientID = null;

            String echoTime = null;
            String repetitionTime = null;
            String magneticFieldStrength = null;
            String flipAngle = null;

            String mriT1T2Name = null;
            String inversionTime = null;
            String echoTrainMeas = null;
            String phaseEncode = null;
            String numAverages = null;
            String receiveCoilName = null;

            String manuf = null;
            String model = null;
            String scannerVer = null;

            String contrastAgent = null;
            String contrastMethod = null;
            String contrastTime = null;
            String contrastDose = null;
            String contrastRate = null;
            String contrastUsedInd = null;
            boolean contrastUsed = false;

            String ctKVP = null;
            String ctMA = null;

            String seriesDescription = null;
            boolean isRestingFMRI = false;

            String unit4DType = null;
            if (units.length > 3 && Unit.getUnitFromLegacyNum(units[3]).getType() == UnitType.TIME) {
                unit4DType = "Time";
            }

            if (fileFormatString.equalsIgnoreCase("dicom")) {
                final FileInfoDicom fileInfoDicom = (FileInfoDicom) img.getFileInfo(0);

                ageVal = (String) (fileInfoDicom.getTagTable().getValue("0010,1010", false));
                // put in to skip erroneous values set in some TRACK-TBI Pilot CT data
                if (isValueSet(ageVal) && ageVal.equalsIgnoreCase("135Y")) {
                    ageVal = null;
                }
                siteName = (String) (fileInfoDicom.getTagTable().getValue("0008,0080"));
                // CNRM anonymization of the institution tag sets the value to Institution instead of clearing the value
                if (isValueSet(siteName) && siteName.trim().equalsIgnoreCase("Institution")) {
                    siteName = "";
                }
                visitDate = convertDateToISOFormat((String) (fileInfoDicom.getTagTable().getValue("0008,0020")));
                visitTime = (String) (fileInfoDicom.getTagTable().getValue("0008,0030"));
                sliceOversample = (String) (fileInfoDicom.getTagTable().getValue("0018,0093"));
                gap = (String) (fileInfoDicom.getTagTable().getValue("0018,0088"));
                bodyPart = (String) (fileInfoDicom.getTagTable().getValue("0018,0015"));

                fieldOfView = (String) (fileInfoDicom.getTagTable().getValue("0018,1100"));
                manufacturer = (String) (fileInfoDicom.getTagTable().getValue("0008,0070"));
                softwareVersion = (String) (fileInfoDicom.getTagTable().getValue("0018,1020"));
                patientPosition = (String) (fileInfoDicom.getTagTable().getValue("0018,5100"));

                scannerModel = (String) (fileInfoDicom.getTagTable().getValue("0008,1090"));
                bandwidth = (String) (fileInfoDicom.getTagTable().getValue("0018,0095"));

                patientName = (String) (fileInfoDicom.getTagTable().getValue("0010,0010"));
                patientID = (String) (fileInfoDicom.getTagTable().getValue("0010,0020"));

                contrastAgent = (String) (fileInfoDicom.getTagTable().getValue("0018,0010"));
                contrastMethod = (String) (fileInfoDicom.getTagTable().getValue("0018,1040"));
                if (isValueSet(contrastMethod)) {
                    System.err.println(patientName + "\tContrast route: " + contrastMethod);
                    if (contrastMethod.equalsIgnoreCase("IV") || contrastMethod.equalsIgnoreCase("Oral & IV")) {
                        contrastMethod = "Infusion";
                    }
                }
                contrastTime = (String) (fileInfoDicom.getTagTable().getValue("0018,1042"));
                if (isValueSet(contrastTime) && isValueSet(visitDate)) {
                    contrastTime = convertDateTimeToISOFormat(visitDate, contrastTime);
                }
                contrastDose = (String) (fileInfoDicom.getTagTable().getValue("0018,1044"));
                contrastRate = (String) (fileInfoDicom.getTagTable().getValue("0018,1046"));

                if (isValueSet(contrastAgent) || isValueSet(contrastMethod) || isValueSet(contrastTime) || isValueSet(contrastDose) || isValueSet(contrastRate)) {
                    contrastUsedInd = "Yes";
                    contrastUsed = true;
                }

                if (modalityString.equalsIgnoreCase("magnetic resonance")) {
                    seriesDescription = ((String) (fileInfoDicom.getTagTable().getValue("0008,103E")));
                    if (isValueSet(seriesDescription) && seriesDescription.toLowerCase().contains("rest")) {
                        isRestingFMRI = true;
                    }

                    echoTime = (String) (fileInfoDicom.getTagTable().getValue("0018,0081"));
                    repetitionTime = (String) (fileInfoDicom.getTagTable().getValue("0018,0080"));
                    magneticFieldStrength = (String) (fileInfoDicom.getTagTable().getValue("0018,0087"));
                    flipAngle = (String) (fileInfoDicom.getTagTable().getValue("0018,1314"));

                    mriT1T2Name = (String) (fileInfoDicom.getTagTable().getValue("0018,0024"));
                    inversionTime = (String) (fileInfoDicom.getTagTable().getValue("0018,0082"));
                    echoTrainMeas = (String) (fileInfoDicom.getTagTable().getValue("0018,0091"));
                    phaseEncode = (String) (fileInfoDicom.getTagTable().getValue("0018,1312"));
                    numAverages = (String) (fileInfoDicom.getTagTable().getValue("0018,0083"));
                    receiveCoilName = (String) (fileInfoDicom.getTagTable().getValue("0018,1250"));

                    scanOptions = (String) (fileInfoDicom.getTagTable().getValue("0018,0022"));
                    // FC ==> flow compensation
                    if (isValueSet(scanOptions) && scanOptions.contains("FC")) {
                        flowCompensation = "Yes";
                    }
                } else if (modalityString.equalsIgnoreCase("computed tomography")) {
                    ctKVP = (String) (fileInfoDicom.getTagTable().getValue("0018,0060"));
                    ctMA = (String) (fileInfoDicom.getTagTable().getValue("0018,1151"));
                }
            } else if (fileFormatString.equalsIgnoreCase("nifti")) {
                // Description = Philips Medical Systems Achieva 3.2.1 (from .nii T1 of Dr. Vaillancourt's)
                final FileInfoNIFTI fileInfoNifti = (FileInfoNIFTI) img.getFileInfo(0);

                final String description = fileInfoNifti.getDescription();

                manuf = convertNiftiDescToBRICSManuf(description);
                model = convertNiftiDescToBRICSModel(description);
                scannerVer = convertNiftiDescToBRICSVer(description);
            }

            for (int i = 0; i < csvFieldNames.size(); i++) {

                if ( !repeatValues.get(i).trim().equals("")) {
                    // TODO: switch to helper method - checkElementDescrepancy
                    if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgDimensionTyp")) {
                        if ( !repeatValues.get(i).trim().equals(String.valueOf(nDims) + "D") && String.valueOf(nDims) != null) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(nDims));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgDim1ExtentVal") && exts.length > 0
                            && String.valueOf(exts[0]) != null) {
                        if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == exts[0])) {

                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(exts[0]));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgDim2ExtentVal") && exts.length > 1
                            && String.valueOf(exts[1]) != null) {
                        if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == exts[1])) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(exts[1]));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgDim3ExtentVal") && exts.length > 2
                            && String.valueOf(exts[2]) != null) {
                        if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == exts[2])) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(exts[2]));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgDim4ExtentVal") && exts.length > 3
                            && String.valueOf(exts[3]) != null) {
                        if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == exts[3])) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(exts[3]));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgDim5ExtentVal") && exts.length > 4
                            && String.valueOf(exts[4]) != null) {
                        if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == exts[4])) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(exts[4]));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgDim4ExtentTyp") && units.length > 3
                            && unit4DType != null) {
                        if ( !repeatValues.get(i).trim().equals(unit4DType)) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(unit4DType);
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgDim1UoMVal") && units.length > 0
                            && Unit.getUnitFromLegacyNum(units[0]) != null) {
                        if ( !repeatValues.get(i).trim().equals(Unit.getUnitFromLegacyNum(units[0]).toString())) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(Unit.getUnitFromLegacyNum(units[0]).toString());
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgDim2UoMVal") && units.length > 1
                            && Unit.getUnitFromLegacyNum(units[1]) != null) {
                        if ( !repeatValues.get(i).trim().equals(Unit.getUnitFromLegacyNum(units[1]).toString())) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(Unit.getUnitFromLegacyNum(units[1]).toString());
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgDim3UoMVal") && units.length > 2
                            && Unit.getUnitFromLegacyNum(units[2]) != null) {
                        if ( !repeatValues.get(i).trim().equals(Unit.getUnitFromLegacyNum(units[2]).toString())) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(Unit.getUnitFromLegacyNum(units[2]).toString());
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgDim4UoMVal") && units.length > 3
                            && Unit.getUnitFromLegacyNum(units[3]) != null) {
                        if ( !repeatValues.get(i).trim().equals(Unit.getUnitFromLegacyNum(units[3]).toString())) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(Unit.getUnitFromLegacyNum(units[3]).toString());
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgDim5UoMVal") && units.length > 4
                            && Unit.getUnitFromLegacyNum(units[4]) != null) {
                        if ( !repeatValues.get(i).trim().equals(Unit.getUnitFromLegacyNum(units[4]).toString())) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(Unit.getUnitFromLegacyNum(units[4]).toString());
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgDim1ResolVal") && res.length > 0
                            && String.valueOf(res[0]) != null) {
                        if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == res[0])) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(res[0]));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgDim2ResolVal") && res.length > 1
                            && String.valueOf(res[1]) != null) {
                        if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == res[1])) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(res[1]));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgDim3ResolVal") && res.length > 2
                            && String.valueOf(res[2]) != null) {
                        if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == res[2])) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(res[2]));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgDim4ResolVal") && res.length > 3
                            && String.valueOf(res[3]) != null) {
                        if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == res[3])) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(res[3]));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgDim5ResolVal") && res.length > 4
                            && String.valueOf(res[4]) != null) {
                        if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == res[4])) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(res[4]));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("Image Information.ImgModltyTyp") && isValueSet(modalityString)) {
                        if ( !repeatValues.get(i).trim().equals(convertModalityToBRICS(modalityString, contrastUsed))) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(convertModalityToBRICS(modalityString, contrastUsed));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgSliceThicknessVal")
                            && String.valueOf(sliceThickness) != null) {
                        if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == sliceThickness)) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(String.valueOf(sliceThickness));
                        }
                    } else if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgSliceOrientTyp") && isValueSet(orientation)) {
                        if ( !repeatValues.get(i).trim().equals(orientation)) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(orientation);
                        }
                    }

                    if (fileFormatString.equalsIgnoreCase("dicom")) {
                        if (csvFieldNames.get(i).equalsIgnoreCase("Main.AgeVal") && isValueSet(ageVal)) {
                            final String ageInMonths = convertDicomAgeToBRICS(ageVal);

                            if ( !repeatValues.get(i).trim().equals(ageInMonths)) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(ageInMonths);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("Main.AgeYrs") && isValueSet(ageVal)) {
                            final String ageInYears = String.valueOf(Integer.valueOf(convertDicomAgeToBRICS(ageVal)) / 12);
                            if ( !repeatValues.get(i).trim().equals(ageInYears)) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(ageInYears);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("Main.VisitDate") && isValueSet(visitDate)) {
                            if ( !repeatValues.get(i).trim().equals(visitDate)) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(visitDate);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("Main.SiteName") && isValueSet(siteName)) {
                            if ( !repeatValues.get(i).trim().equals(siteName)) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(siteName);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("Image Information.ImgStdyDateTime")
                                && (isValueSet(visitDate) || isValueSet(visitTime))) {
                            if ( !repeatValues.get(i).trim().equals(convertDateTimeToISOFormat(visitDate, visitTime))) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(convertDateTimeToISOFormat(visitDate, visitTime));
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgSliceOverSampVal")
                                && isValueSet(sliceOversample)) {
                            if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == (Float.parseFloat(sliceOversample)))) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(sliceOversample);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgGapBetwnSlicesMeasr") && isValueSet(gap)) {
                            if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == (Float.parseFloat(gap)))) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(gap);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("Image Information.ImgAntmicSite") && isValueSet(bodyPart)) {
                            if ( !repeatValues.get(i).trim().equals(bodyPart)) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(bodyPart);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgFOVMeasrDescTxt")
                                && isValueSet(fieldOfView)) {
                            if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == (Float.parseFloat(fieldOfView)))) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(fieldOfView);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("Image Information.ImgScannerManufName") && isValueSet(manufacturer)) {
                            if ( !convertManufNameToBRICS(manufacturer).equals(repeatValues.get(i).trim())) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(convertManufNameToBRICS(manufacturer));
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("Image Information.ImgScannerSftwrVrsnNum") && isValueSet(softwareVersion)) {
                            if ( !repeatValues.get(i).trim().equals(softwareVersion)) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(softwareVersion);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("Image Information.ImgHeadPostnTxt") && isValueSet(patientPosition)) {
                            if ( !repeatValues.get(i).trim().equals(patientPosition)) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(patientPosition);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("Image Information.ImgScannerModelName") && isValueSet(scannerModel)) {
                            if ( !repeatValues.get(i).trim().equals(convertModelNameToBRICS(scannerModel))) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(convertModelNameToBRICS(scannerModel));
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions.ImgBandwidthVal") && isValueSet(bandwidth)) {
                            if ( !repeatValues.get(i).trim().equals(bandwidth)) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(bandwidth);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("Main.GUID")) {
                            if (isValueSet(patientID) && isGuid(patientID)) {
                                if ( !repeatValues.get(i).trim().equals(patientID)) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(patientID);
                                }
                            } else if (isValueSet(patientName) && isGuid(patientName)) {
                                if ( !repeatValues.get(i).trim().equals(patientID)) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(patientID);
                                }
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("Image Information.ImgContrastAgentUsedInd") && isValueSet(contrastUsedInd)) {
                            if ( !repeatValues.get(i).trim().equals(contrastUsedInd)) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(contrastUsedInd);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("Image Information.ImgContrastAgentName") && isValueSet(contrastAgent)) {
                            if ( !repeatValues.get(i).trim().equals(contrastAgent)) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(contrastAgent);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("Image Information.ImgContrastAgentMethodTyp") && isValueSet(contrastAgent)) {
                            if ( !repeatValues.get(i).trim().equals(contrastAgent)) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(contrastAgent);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("Image Information.ImgContrastAgentInjctnTime") && isValueSet(contrastTime)) {
                            if ( !repeatValues.get(i).trim().equals(contrastTime)) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(contrastTime);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("Image Information.ImgContrastAgentDose") && isValueSet(contrastDose)) {
                            if ( !repeatValues.get(i).trim().equals(contrastDose)) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(contrastDose);
                            }
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("Image Information.ImgContrastAgentRate") && isValueSet(contrastRate)) {
                            if ( !repeatValues.get(i).trim().equals(contrastRate)) {
                                csvFList.add(csvFieldNames.get(i));
                                csvPList.add(repeatValues.get(i));
                                headerList.add(contrastRate);
                            }
                        }

                        if (modalityString.equalsIgnoreCase("magnetic resonance")) {
                            if (csvFieldNames.get(i).equalsIgnoreCase("Magnetic Resonance Information.ImgEchoDur") && isValueSet(echoTime)) {
                                if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == (Float.parseFloat(echoTime)))) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(echoTime);
                                }
                            } else if (csvFieldNames.get(i).equalsIgnoreCase("Magnetic Resonance Information.ImgRepetitionGapVal")
                                    && isValueSet(repetitionTime)) {

                                if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == (Float.parseFloat(repetitionTime)))) {

                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(repetitionTime);
                                }
                            } else if (csvFieldNames.get(i).equalsIgnoreCase("Magnetic Resonance Information.ImgScannerStrgthVal")
                                    && isValueSet(magneticFieldStrength)) {
                                if ( !repeatValues.get(i).trim().equals(convertMagFieldStrengthToBRICS(magneticFieldStrength))) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(convertMagFieldStrengthToBRICS(magneticFieldStrength));
                                }
                            } else if (csvFieldNames.get(i).equalsIgnoreCase("Magnetic Resonance Information.ImgFlipAngleMeasr") && isValueSet(flipAngle)) {
                                if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == (Float.parseFloat(flipAngle)))) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(flipAngle);
                                }
                            } else if (csvFieldNames.get(i).equalsIgnoreCase("Magnetic Resonance Information.ImgMRIT1T2SeqName") && isValueSet(mriT1T2Name)) {
                                if ( !repeatValues.get(i).trim().equals(mriT1T2Name)) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(mriT1T2Name);
                                }
                            } else if (csvFieldNames.get(i).equalsIgnoreCase("Magnetic Resonance Information.ImgInversionTime") && isValueSet(inversionTime)) {
                                if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == (Float.parseFloat(inversionTime)))) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(inversionTime);
                                }
                            } else if (csvFieldNames.get(i).equalsIgnoreCase("Magnetic Resonance Information.ImgEchoTrainLngthMeasr")
                                    && isValueSet(echoTrainMeas)) {
                                if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == (Float.parseFloat(echoTrainMeas)))) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(echoTrainMeas);
                                }
                            } else if (csvFieldNames.get(i).equalsIgnoreCase("Magnetic Resonance Information.ImgPhasEncdeDirctTxt") && isValueSet(phaseEncode)) {
                                if ( !repeatValues.get(i).trim().equals(phaseEncode)) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(phaseEncode);
                                }
                            } else if (csvFieldNames.get(i).equalsIgnoreCase("Magnetic Resonance Information.ImgSignalAvgNum") && isValueSet(numAverages)) {
                                if ( ! (Float.parseFloat(repeatValues.get(i).trim()) == (Float.parseFloat(numAverages)))) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(numAverages);
                                }
                            } else if (csvFieldNames.get(i).equalsIgnoreCase("Magnetic Resonance RF Coil.ImgRFCoilName") && isValueSet(receiveCoilName)) {
                                if ( !repeatValues.get(i).trim().equals(receiveCoilName)) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(receiveCoilName);
                                }
                            } else if (csvFieldNames.get(i).equalsIgnoreCase("Magnetic Resonance Information.ImgFlowCompnsatnInd")
                                    && isValueSet(flowCompensation)) {
                                if ( !repeatValues.get(i).trim().equals(flowCompensation)) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(flowCompensation);
                                }
                            }

                            // TODO add in handling of FMRI pulse seq and Resting state task type
                        } else if (modalityString.equalsIgnoreCase("computed tomography")) {
                            if (csvFieldNames.get(i).equalsIgnoreCase("CT Information.ImgCTkVp") && isValueSet(ctKVP)) {
                                if ( !repeatValues.get(i).trim().equals(ctKVP)) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(ctKVP);
                                }
                            } else if (csvFieldNames.get(i).equalsIgnoreCase("CT Information.ImgCTmA") && isValueSet(ctMA)) {
                                if ( !repeatValues.get(i).trim().equals(ctMA)) {
                                    csvFList.add(csvFieldNames.get(i));
                                    csvPList.add(repeatValues.get(i));
                                    headerList.add(ctMA);
                                }
                            }
                        }
                    } else if (fileFormatString.equalsIgnoreCase("nifti")) {
                        if (csvFieldNames.get(i).equalsIgnoreCase("Image Information.ImgScannerManufName") && isValueSet(manuf)) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(manuf);
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("Image Information.ImgScannerModelName") && isValueSet(model)) {
                            csvFList.add(csvFieldNames.get(i));
                            csvPList.add(repeatValues.get(i));
                            headerList.add(model);
                        } else if (csvFieldNames.get(i).equalsIgnoreCase("Image Information.ImgScannerSftwrVrsnNum") && isValueSet(scannerVer)) {
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
                }
            }

            return resolveConflictsUsing;
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
            
            modality = determineModality(modality, dataStructureName);
            modalityString = FileInfoBase.getModalityStr(modality);

            final float sliceThickness = img.getFileInfo(0).getSliceThickness();
            final int orient = img.getFileInfo(0).getImageOrientation();
            final String orientation = FileInfoBase.getImageOrientationStr(orient);

            String ageVal = null;
            String siteName = null;
            String visitDate = null;
            String visitTime = null;
            String sliceOversample = null;
            String gap = null;
            String bodyPart = null;

            String fieldOfView = null;
            String manufacturer = null;
            String softwareVersion = null;
            String patientPosition = null;

            String scannerModel = null;
            String bandwidth = null;

            String scanOptions = null;
            String flowCompensation = null;

            String patientName = null;
            String patientID = null;

            String echoTime = null;
            String repetitionTime = null;
            String magnaticFieldStrength = null;
            String flipAngle = null;

            String mriT1T2Name = null;
            String inversionTime = null;
            String echoTrainMeas = null;
            String phaseEncode = null;
            String numAverages = null;
            String receiveCoilName = null;

            String manuf = null;
            String model = null;
            String scannerVer = null;

            String contrastAgent = null;
            String contrastMethod = null;
            String contrastTime = null;
            String contrastDose = null;
            String contrastRate = null;
            String contrastUsedInd = null;
            boolean contrastUsed = false;

            String ctKVP = null;
            String ctMA = null;

            String seriesDescription = null;
            boolean isRestingFMRI = false;

            if (fileFormatString.equalsIgnoreCase("dicom")) {
                final FileInfoDicom fileInfoDicom = (FileInfoDicom) img.getFileInfo(0);

                ageVal = (String) (fileInfoDicom.getTagTable().getValue("0010,1010", false));
                // put in to skip erroneous values set in some TRACK-TBI Pilot CT data
                if (isValueSet(ageVal) && ageVal.equalsIgnoreCase("135Y")) {
                    ageVal = null;
                }
                siteName = (String) (fileInfoDicom.getTagTable().getValue("0008,0080"));
                // CNRM anonymization of the institution tag sets the value to Institution instead of clearing the value
                if (isValueSet(siteName) && siteName.trim().equalsIgnoreCase("Institution")) {
                    siteName = "";
                }
                visitDate = (String) (fileInfoDicom.getTagTable().getValue("0008,0020"));
                visitTime = (String) (fileInfoDicom.getTagTable().getValue("0008,0030"));
                sliceOversample = (String) (fileInfoDicom.getTagTable().getValue("0018,0093"));
                gap = (String) (fileInfoDicom.getTagTable().getValue("0018,0088"));
                bodyPart = (String) (fileInfoDicom.getTagTable().getValue("0018,0015"));

                fieldOfView = (String) (fileInfoDicom.getTagTable().getValue("0018,1100"));
                manufacturer = (String) (fileInfoDicom.getTagTable().getValue("0008,0070"));
                softwareVersion = (String) (fileInfoDicom.getTagTable().getValue("0018,1020"));
                patientPosition = (String) (fileInfoDicom.getTagTable().getValue("0018,5100"));

                scannerModel = (String) (fileInfoDicom.getTagTable().getValue("0008,1090"));
                bandwidth = (String) (fileInfoDicom.getTagTable().getValue("0018,0095"));

                patientName = (String) (fileInfoDicom.getTagTable().getValue("0010,0010"));
                patientID = (String) (fileInfoDicom.getTagTable().getValue("0010,0020"));

                contrastAgent = (String) (fileInfoDicom.getTagTable().getValue("0018,0010"));
                contrastMethod = (String) (fileInfoDicom.getTagTable().getValue("0018,1040"));
                if (isValueSet(contrastMethod)) {
                    System.err.println(patientName + "\tContrast route: " + contrastMethod);
                    if (contrastMethod.equalsIgnoreCase("IV") || contrastMethod.equalsIgnoreCase("Oral & IV")) {
                        contrastMethod = "Infusion";
                    }
                }
                contrastTime = (String) (fileInfoDicom.getTagTable().getValue("0018,1042"));
                if (isValueSet(contrastTime) && isValueSet(visitDate)) {
                    contrastTime = convertDateTimeToISOFormat(visitDate, contrastTime);
                }
                contrastDose = (String) (fileInfoDicom.getTagTable().getValue("0018,1044"));
                contrastRate = (String) (fileInfoDicom.getTagTable().getValue("0018,1046"));

                if (isValueSet(contrastAgent) || isValueSet(contrastMethod) || isValueSet(contrastTime) || isValueSet(contrastDose) || isValueSet(contrastRate)) {
                    contrastUsedInd = "Yes";
                    contrastUsed = true;
                }

                if (modalityString.equalsIgnoreCase("magnetic resonance")) {
                    seriesDescription = ((String) (fileInfoDicom.getTagTable().getValue("0008,103E")));
                    if (isValueSet(seriesDescription) && seriesDescription.toLowerCase().contains("rest")) {
                        isRestingFMRI = true;
                    }

                    echoTime = (String) (fileInfoDicom.getTagTable().getValue("0018,0081"));
                    repetitionTime = (String) (fileInfoDicom.getTagTable().getValue("0018,0080"));
                    magnaticFieldStrength = (String) (fileInfoDicom.getTagTable().getValue("0018,0087"));
                    flipAngle = (String) (fileInfoDicom.getTagTable().getValue("0018,1314"));

                    mriT1T2Name = (String) (fileInfoDicom.getTagTable().getValue("0018,0024"));
                    inversionTime = (String) (fileInfoDicom.getTagTable().getValue("0018,0082"));
                    echoTrainMeas = (String) (fileInfoDicom.getTagTable().getValue("0018,0091"));
                    phaseEncode = (String) (fileInfoDicom.getTagTable().getValue("0018,1312"));
                    numAverages = (String) (fileInfoDicom.getTagTable().getValue("0018,0083"));
                    receiveCoilName = (String) (fileInfoDicom.getTagTable().getValue("0018,1250"));

                    scanOptions = (String) (fileInfoDicom.getTagTable().getValue("0018,0022"));
                    // FC ==> flow compensation
                    if (isValueSet(scanOptions) && scanOptions.contains("FC")) {
                        flowCompensation = "Yes";
                    }
                } else if (modalityString.equalsIgnoreCase("computed tomography")) {
                    ctKVP = (String) (fileInfoDicom.getTagTable().getValue("0018,0060"));
                    ctMA = (String) (fileInfoDicom.getTagTable().getValue("0018,1151"));
                }
            } else if (fileFormatString.equalsIgnoreCase("nifti")) {
                // Description = Philips Medical Systems Achieva 3.2.1 (from .nii T1 of Dr. Vaillancourt's)
                final FileInfoNIFTI fileInfoNifti = (FileInfoNIFTI) img.getFileInfo(0);
                final String description = fileInfoNifti.getDescription();

                manuf = convertNiftiDescToBRICSManuf(description);
                model = convertNiftiDescToBRICSModel(description);
                scannerVer = convertNiftiDescToBRICSVer(description);
            }

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
                        } else if (deName.equalsIgnoreCase("ImgDim4ExtentTyp")) {
                            if (img.getNDims() > 3 && Unit.getUnitFromLegacyNum(units[3]).getType() == UnitType.TIME) {
                                setElementComponentValue(deVal, "Time");
                            }
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
                            setElementComponentValue(deVal, convertModalityToBRICS(modalityString, contrastUsed));
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
                            if (deName.equalsIgnoreCase("AgeVal") && ageVal != null && !ageVal.equals("")) {
                                final String ageInMonths = convertDicomAgeToBRICS(ageVal);
                                if (Float.parseFloat(ageInMonths) != 0) {
                                    setElementComponentValue(deVal, ageInMonths);
                                }
                            } else if (deName.equalsIgnoreCase("AgeYrs") && ageVal != null && !ageVal.equals("")) {
                                final String ageMonthsStr = convertDicomAgeToBRICS(ageVal);
                                if (ageMonthsStr != null && !ageMonthsStr.trim().equals("")) {
                                    final Integer ageInMonths = Integer.valueOf(ageMonthsStr);
                                    if (ageInMonths != 0) {
                                        final String ageInYears = String.valueOf(ageInMonths / 12);
                                        setElementComponentValue(deVal, ageInYears);
                                    }
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
                            } else if (deName.equalsIgnoreCase("ImgContrastAgentUsedInd")) {
                                setElementComponentValue(deVal, contrastUsedInd);
                            } else if (deName.equalsIgnoreCase("ImgContrastAgentName")) {
                                setElementComponentValue(deVal, contrastAgent);
                            } else if (deName.equalsIgnoreCase("ImgContrastAgentMethodTyp")) {
                                setElementComponentValue(deVal, contrastMethod);
                            } else if (deName.equalsIgnoreCase("ImgContrastAgentInjctnTime")) {
                                setElementComponentValue(deVal, contrastTime);
                            } else if (deName.equalsIgnoreCase("ImgContrastAgentDose")) {
                                setElementComponentValue(deVal, contrastDose);
                            } else if (deName.equalsIgnoreCase("ImgContrastAgentRate")) {
                                setElementComponentValue(deVal, contrastRate);
                            }

                            if (modalityString.equalsIgnoreCase("magnetic resonance")) {
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
                                } else if (deName.equalsIgnoreCase("ImgFlowCompnsatnInd")) {
                                    setElementComponentValue(deVal, flowCompensation);
                                }

                                // ImagingFunctionalMR FS
                                if (deName.equalsIgnoreCase("ImgPulseSeqTyp")) {
                                    if (fsData.getStructInfo().getShortName().equalsIgnoreCase("ImagingFunctionalMR")) {
                                        setElementComponentValue(deVal, "fMRI");
                                    }
                                } else if (deName.equalsIgnoreCase("ImgFMRITaskTyp")) {
                                    if (isRestingFMRI) {
                                        setElementComponentValue(deVal, "Rest");
                                    }
                                }
                            } else if (modalityString.equalsIgnoreCase("computed tomography")) {
                                if (deName.equalsIgnoreCase("ImgCTkVp")) {
                                    setElementComponentValue(deVal, ctKVP);
                                } else if (deName.equalsIgnoreCase("ImgCTmA")) {
                                    setElementComponentValue(deVal, ctMA);
                                }
                            }
                        } else if (fileFormatString.equalsIgnoreCase("nifti")) {
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
                        if ( !isMainImagingFileElement(deVal)) {
                            if (deVal.getComp() instanceof JTextField) {
                                ((JTextField) deVal.getComp()).setText(null);
                            } else if (deVal.getComp() instanceof JComboBox) {
                                ((JComboBox) deVal.getComp()).setSelectedItem(null);
                            } else if (deVal.getComp() instanceof JList) {
                                ((JList) deVal.getComp()).clearSelection();
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
                    structRowImgFileInfoList.remove(structRowImgFileInfoList.size() - 1);
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

                    int filter = ViewImageFileFilter.ALL;
                    
//                    try {
//                        filter = Integer.parseInt(Preferences.getProperty(Preferences.PREF_FILENAME_FILTER));
//                    } catch (final NumberFormatException nfe) {
//
//                        // an invalid value was set in preferences -- so don't
//                        // use it!
//                        filter = -1;
//                    }

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

                        ModelImage srcImage = null;
                        boolean isDeidentified = false;
                        if (file.getName().endsWith(".zip") || file.getName().endsWith(".tar.gz") || file.getName().endsWith(".tgz")) {
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

                            ModelImage thumbnailImage = createThumbnailImage(srcImage);

                            if (launchedFromInProcessState) {
                                final int selectedRow = structTable.getSelectedRow();
                                previewImages.set(selectedRow, previewImg);
                                previewImages.get(selectedRow).setSliceBrightness(previewImgBrightness, previewImgContrast);

                                ImgFileInfo imgInfo = new ImgFileInfo(file.getAbsolutePath(), isMultiFile, FileUtility.getFileNameList(srcImage),
                                        srcImage.getFileInfo(0).getFileFormat(), createThumbnailDataForWriting(thumbnailImage));

                                structRowImgFileInfoList.set(selectedRow, imgInfo);
                            } else {
                                final int size = previewImages.size();
                                previewImages.set(size - 1, previewImg);
                                previewImages.get(size - 1).setSliceBrightness(previewImgBrightness, previewImgContrast);

                                ImgFileInfo imgInfo = new ImgFileInfo(file.getAbsolutePath(), isMultiFile, FileUtility.getFileNameList(srcImage),
                                        srcImage.getFileInfo(0).getFileFormat(), createThumbnailDataForWriting(thumbnailImage));

                                structRowImgFileInfoList.set(size - 1, imgInfo);
                            }

                            // cleanup thumbnail modelimage
                            if (thumbnailImage != null) {
                                thumbnailImage.disposeLocal();
                                thumbnailImage = null;
                            }

                            previewImgPanel.validate();
                            previewImgPanel.repaint();
                        }

                        if (srcImage != null) {
                            // basic check that image data is de-identified
                            Vector<FileDicomTag> problemTags = deidentificationCheckDicomTags(srcImage);
                            if (problemTags != null) {
                                isDeidentified = deidentificationDialogDicom(srcImage.getFileInfo(0).getFileDirectory(), srcImage.getFileInfo(0).getFileName(),
                                        problemTags);
                            }

                            if (isDeidentified) {
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
                            }

                            srcImage.disposeLocal();
                            srcImage = null;
                        }
                    }
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
                                    if ( !isGuid(value.trim())) {
                                        errs.add(title + " must begin with a valid GUID prefix");
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

        return filteredList;
    }

    private Vector<FileDicomTag> deidentificationCheckDicomTags(ModelImage img) {
        final String fileFormatString = FileUtility.getFileTypeStr(img.getFileInfo(0).getFileFormat());

        if (fileFormatString.equalsIgnoreCase("dicom")) {
            Vector<FileDicomTag> problemTags = new Vector<FileDicomTag>();
            // for (FileInfoBase fInfo : img.getFileInfo()) {
            FileInfoBase fInfo = img.getFileInfo(0);
            FileInfoDicom dicomInfo = (FileInfoDicom) fInfo;

            FileDicomTagTable tagTable = dicomInfo.getTagTable();

            for (String anonTagKey : anonymizeTagIDs) {
                FileDicomTag tag = tagTable.get(anonTagKey);
                if (tag != null && tag.getValue(true) != null && ! ((String) tag.getValue(true)).trim().equals("")) {
                    problemTags.add(tag);
                }
            }
            // }

            return problemTags;
        }

        return null;
    }

    private boolean deidentificationDialogDicom(String fDir, String fName, Vector<FileDicomTag> problemTags) {
        if (problemTags != null) {
            if (problemTags.size() > 0) {
                String disclaimerText = new String(
                        "<html>"
                                + "The table below lists fields in the loaded image data with potential Personally Identifiable Information (PII) or Protected Health Information."
                                + "<br><br>"
                                + "Please review all the fields below.  If any fields contain PII/PHI, exit the Imaging Tool and fully de-identify your image data."
                                + "<br><br>"
                                + "There may be fields in your data that contain PII/PHI that are not highlighted in this table.  DICOM private tags, and sequence tags are not examined."
                                + "<br><br>"
                                + "<b>Remember, <em>YOU</em> are responible for the de-identification of all submitted data.</b>  This table is for informational purposes only."
                                + "<br><br>" + "Base file loaded:" + "<br>" + fDir + fName + "</html>");
                JLabel disclaimerLabel = new JLabel(disclaimerText);
                disclaimerLabel.setFont(serif12);

                TagTableModel tableModel = new TagTableModel(problemTags);
                JTable table = new JTable(tableModel);
                table.setFont(serif12);
                table.getTableHeader().setFont(serif12);
                table.setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN);
                table.setDefaultRenderer(String.class, new LineWrapCellRenderer());
                TableColumnModel colModel = table.getColumnModel();
                colModel.getColumn(0).setMaxWidth(120);

                final int fontHeight = table.getFontMetrics(this.getFont()).getHeight();
                final int fontMaxAdv = table.getFontMetrics(this.getFont()).getMaxAdvance();
                for (int i = 0; i < table.getRowCount(); i++) {
                    int textLength = ((String) table.getModel().getValueAt(i, 2)).length();
                    int lines = textLength / fontMaxAdv + 1;
                    int height = fontHeight * lines;

                    table.setRowHeight(i, height + 5);
                }

                DefaultTableCellRenderer textRenderer = new DefaultTableCellRenderer();
                textRenderer.setHorizontalAlignment(SwingConstants.LEFT);
                textRenderer.setVerticalAlignment(SwingConstants.TOP);
                for (int i = 0; i < table.getColumnCount(); i++) {
                    table.getColumnModel().getColumn(i).setCellRenderer(textRenderer);
                }

                JPanel buttonPanel = new JPanel(new GridLayout(1, 2));

                JButton okButton = new JButton("I have reviewed the data and no PII/PHI is present");
                okButton.setActionCommand("okayPII");
                okButton.addActionListener(this);
                JButton cancelButton = new JButton("Exit the Imaging Tool");
                cancelButton.setActionCommand("cancelPII");
                cancelButton.addActionListener(this);
                buttonPanel.add(okButton);
                buttonPanel.add(cancelButton);

                GridBagConstraints gbc = new GridBagConstraints();
                gbc.insets = new Insets(2, 5, 2, 5);
                gbc.fill = GridBagConstraints.BOTH;
                gbc.weightx = 0;
                gbc.weighty = 0;
                gbc.gridx = 0;
                gbc.anchor = GridBagConstraints.NORTH;
                gbc.gridy = 0;

                JPanel dialogPanel = new JPanel(new GridBagLayout());
                dialogPanel.add(disclaimerLabel, gbc);

                gbc.weighty = 1;
                gbc.weighty = 1;
                gbc.gridy++;
                dialogPanel.add(new JScrollPane(table), gbc);

                gbc.weightx = 0;
                gbc.weighty = 0;
                gbc.gridy++;
                gbc.fill = GridBagConstraints.NONE;
                gbc.anchor = GridBagConstraints.SOUTH;
                dialogPanel.add(buttonPanel, gbc);

                deidentDialog = new JDialog(this, true);
                deidentDialog.add(dialogPanel);
                deidentDialog.setSize(1000, 800);
                MipavUtil.centerInWindow(this, deidentDialog);
                deidentDialog.setTitle("De-identification review: " + fName);
                deidentDialog.setVisible(true);
            }
        }

        return true;
    }

    private class TagTableModel extends AbstractTableModel {
        private static final long serialVersionUID = 51886231625427264L;

        private String[] columnNames = new String[] {"DICOM Tag", "Name", "Value"};

        private Vector<Vector<String>> rowData;

        public TagTableModel(Vector<FileDicomTag> problemTags) {
            rowData = new Vector<Vector<String>>();

            for (FileDicomTag tag : problemTags) {
                Vector<String> colData = new Vector<String>();
                colData.add(tag.getKey().toString());
                colData.add(tag.getName());
                colData.add("<html>" + (String) tag.getValue(true) + "</html>");
                rowData.add(colData);
            }
        }

        public int getColumnCount() {
            return columnNames.length;
        }

        public int getRowCount() {
            return rowData.size();
        }

        public String getColumnName(int col) {
            return columnNames[col];
        }

        public Object getValueAt(int row, int col) {
            return rowData.get(row).get(col);
        }

        public Class getColumnClass(int c) {
            return getValueAt(0, c).getClass();
        }

        public boolean isCellEditable(int row, int col) {
            return false;
        }
    }

    private class LineWrapCellRenderer extends JTextArea implements TableCellRenderer {
        private static final long serialVersionUID = -942403827312154298L;

        @Override
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
            this.setText((String) value);
            this.setWrapStyleWord(true);
            this.setLineWrap(true);

            /*
             * int fontHeight = this.getFontMetrics(this.getFont()).getHeight(); int textLength =
             * this.getText().length(); int lines = textLength / this.getColumns() + 1;
             * 
             * final int height = fontHeight * lines;
             * 
             * final int rowIndex = row; final JTable jTable = table;
             * 
             * SwingUtilities.invokeLater(new Runnable() {
             * 
             * @Override public void run() { jTable.setRowHeight(rowIndex, height); } }); table.setRowHeight(row,
             * height);
             */

            return this;
        }

    }

    /**
     * Class that connects to BRICS data dictionary web service (via RESTful API) to retrieve the list of imaging form
     * structures (without their attached data elements).
     */
    public class FormListRESTThread extends Thread implements ActionListener {
        private static final String ddAuthBase = "/portal/ws/webstart/dictionary/formStructure/details";

        private static final String ddRequestBase = "/portal/ws/ddt/dictionary/FormStructure";

        // private static final String ddStructListRequest = ddRequestBase + "/Published/list?type=IMAGING";
        private static final String ddStructListRequest = ddRequestBase + "/Published/list?type=IMAGING&incDEs=false";

        private JButton progressCancelButton;

        private final PlugInDialogFITBIR parent;

        public FormListRESTThread(final PlugInDialogFITBIR parent) {
            super();
            this.parent = parent;
        }

        @Override
        public void run() {
            ViewJProgressBar progressBar = null;
            try {
                progressBar = new ViewJProgressBar("BRICS", "Retrieving imaging form structures from BRICS data dictionary...", 0, 100, true);
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

                dataStructureList = filterDataStructures(fullList);

                // for (final FormStructure ds : dataStructureList) {
                // System.out.println("FS title:\t" + ds.getTitle() + "\tversion:\t" + ds.getVersion() + "\tpub:\t" +
                // ds.getStatus());
                // }

                progressBar.updateValue(80);

                progressBar.updateValue(100);
                progressBar.setVisible(false);
                progressBar.dispose();
                printlnToLog("Successful retrieval of imaging form structures.");

                addStructButton.setEnabled(true);
                loadCSVButton.setEnabled(true);
                selectBIDSButton.setEnabled(true);
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
    public class FormDataElementsRESTThread extends Thread implements ActionListener {

        private static final String ddRequestBase = "/portal/ws/ddt/dictionary/FormStructure";

        private JButton progressCancelButton;

        private final PlugInDialogFITBIR parent;

        private final String formStructName;

        private FormStructure fullFormStructure;

        private final boolean addProgressBar;

        public FormDataElementsRESTThread(final PlugInDialogFITBIR parent, final String formStructName, final boolean addProgressBar) {
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

                for (int i = 0; i < dataStructureList.size(); i++) {
                    final FormStructure curFS = dataStructureList.get(i);
                    if (curFS.getShortName().equals(fullFormStructure.getShortName())) {
                        dataStructureList.set(i, fullFormStructure);
                        break;
                    }
                }

                // for (final FormStructure ds : dataStructureList) {
                // System.out.println("FS title:\t" + ds.getTitle() + "\tversion:\t" + ds.getVersion() + "\tpub:\t"
                // +
                // ds.getStatus());
                // }

                if (addProgressBar) {
                    progressBar.updateValue(80);

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

            if (isLegacyOtherSpecifyField(this)) {
                otherSpecifyField = new JTextField();
                otherSpecifyField.setVisible(false);
                ((JComboBox) deComp).addActionListener(new OtherSpecifyListener(otherSpecifyField));
            }
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
                otherSpecifyField.setVisible( ((JComboBox) source).getSelectedItem().equals(VALUE_OTHER_SPECIFY)
                        || ((JComboBox) source).getSelectedItem().equals(VALUE_YES_SPECIFY));
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

    public class ImgFileInfo {
        public List<String> origFiles;

        public MemoryImageSource thumbnailImgData;

        public boolean isMultifile;

        public String imgFilePath;
        
        public int fileFormat;

/*        public ImgFileInfo(final String imgFilePath, final boolean isMultifile) {
            super();
            this.isMultifile = isMultifile;
            this.imgFilePath = imgFilePath;
        }*/

        public ImgFileInfo(final String imgFilePath, final boolean isMultifile, final List<String> origFiles, final int format, final MemoryImageSource thumbnailImgData) {
            super();
            this.origFiles = origFiles;
            this.thumbnailImgData = thumbnailImgData;
            this.isMultifile = isMultifile;
            this.imgFilePath = imgFilePath;
            this.fileFormat = format;
        }

        public List<String> getOrigFiles() {
            return origFiles;
        }

        public void setOrigFiles(final List<String> origFiles) {
            this.origFiles = origFiles;
        }

        public MemoryImageSource getThumbnailImgData() {
            return thumbnailImgData;
        }

        public void setThumbnailImgData(final MemoryImageSource thumbnailImgData) {
            this.thumbnailImgData = thumbnailImgData;
        }

        public boolean isMultifile() {
            return isMultifile;
        }

        public void setMultifile(final boolean isMultifile) {
            this.isMultifile = isMultifile;
        }

        public String getImgFilePath() {
            return imgFilePath;
        }

        public void setImgFilePath(final String imgFilePath) {
            this.imgFilePath = imgFilePath;
        }
        
        public int getFileFormat() {
            return fileFormat;
        }
        
        public void setFileFormat(final int format) {
            fileFormat = format;
        }
        
        public String getFileFormatString() {
            return FileUtility.getFileTypeStr(fileFormat);
        }
    }
}
