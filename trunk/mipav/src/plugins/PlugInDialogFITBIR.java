import gov.nih.mipav.plugins.JDialogStandalonePlugin;

import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.LightboxGenerator;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBConcat;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoBase.UnitType;
import gov.nih.mipav.model.scripting.VariableTable;
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
import java.awt.image.BufferedImage;
import java.awt.image.MemoryImageSource;
import java.io.*;
import java.nio.file.Files;
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

import javax.imageio.ImageIO;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.*;
import javax.swing.tree.DefaultMutableTreeNode;

import org.apache.commons.csv.*;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.text.WordUtils;
import org.apache.cxf.jaxrs.client.WebClient;
import org.apache.cxf.transport.http.HTTPConduit;
import org.bouncycastle.util.encoders.Hex;
import org.codehaus.jettison.json.*;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;

import com.ice.tar.TarEntry;
import com.ice.tar.TarInputStream;
import com.sun.jimi.core.Jimi;
import com.sun.jimi.core.JimiException;


public class PlugInDialogFITBIR extends JFrame
        implements ActionListener, ChangeListener, ItemListener, TreeSelectionListener, MouseListener, PreviewImageContainer, WindowListener {
    private static final long serialVersionUID = -5516621806537554154L;

    private final Font serif12 = MipavUtil.font12, serif12B = MipavUtil.font12B;

    private WidgetFactory.ScrollTextArea logOutputArea;

    private JScrollPane listPane;

    private JTable structTable;

    private JButton addStructButton, loadCSVButton, selectBIDSButton, finishButton, removeStructButton, editDataElementsButton, outputDirButton;

    private JTextField outputDirTextField;

    private ViewTableModel structTableModel;

    private String outputDirBase;
    
    private int csvRecordCount = -1;

    private String csvFileDir;

    private String BIDSFileDir;

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
    
    private static final String jsonConfigFileName = "brics_config.json";

    /**
     * Property for reading whether to use the (slower) authenticated web service, which allows testing against draft
     * forms.
     */
    private static final String ddUseAuthServiceProp = "ddUseAuthService";
    
    /** Full data dictionary server url */
    private static String ddServerURL = null;

    /** Full authentication server url */
    private static String authServerURL = null;

    private static String ddAuthUser = "";

    private static String ddAuthPass = "";

    private static boolean ddUseAuthService = false;
    
    private enum BricsInstance {
        FITBIR, PDBP, NEI_BRICS, NINR, CNRM, CISTAR, GRDR, COVID19, NTRR, BRICS;
    }
    
    private enum BricsEnv {
        Prod, Demo, Stage, UAT, Dev, Intramural;
    }
    
    private BricsInstance selectedDictionaryInstance = BricsInstance.FITBIR;
    
    private BricsEnv selectedDictionaryEnv = BricsEnv.Prod;
    
    protected HashMap<String, DictionaryConfigItem> dictionaryConfigTable = null;
    
    protected static final String dictionaryInstanceCmdLineVar = "BricsInstance";
    
    protected static final String dictionaryEnvCmdLineVar = "BricsEnvironment";

    private List<FormStructure> dataStructureList;
    
    private int chosenDiseaseFilterIndex = -1;

    private File csvFile;

    private ArrayList<String> csvFieldNames;

    private final ArrayList<String> tempDirs = new ArrayList<String>();

    private boolean isFinished = false;

    /** Map between file paths and DTI info extracted from their DICOM headers. */
    private Hashtable<String, DTIParameters> dicomDtiHeaderData = new Hashtable<String, DTIParameters>();

    /**
     * Indicates how to resolve conflicts between csv and image header values. 0 = no choice made/ask always, 1 = csv, 2
     * = image
     */
    private int resolveConflictsUsing = RESOLVE_CONFLICT_ASK;

    // variables for auto-running on a csv from the command line and logging to files
    private String cmdLineCsvVar = "BricsCsvFile";

    private String cmdLineOutputVar = "BricsOutputDir";

    private String cmdLineCsvExitVar = "BricsExit";

    private boolean cmdLineCsvExit = false;

    private boolean cmdLineCsvFlag = false;

    private String cmdLineCsvFile = null;

    private String cmdLineOutputDir = null;

    private File mainLogFile = null;

    private PrintStream mainLogFileOut = null;

    private PrintStream logOnlyOut = null;

    private PrintStream logOnlyErr = null;

    private File readErrorFile = null;

    private PrintStream readErrorFileOut = null;

    private File piiWarningFile = null;

    private PrintStream piiWarningFileOut = null;

    private boolean bidsPackageFlag = false;

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

    private static final String SPECTROSCOPY_FS_SUFFIX = "spectrosc";

    private static final String MRI_FS_NAME = "imagingmr";

    private static final String FMRI_FS_SUFFIX = "functionalmr";

    private static final String DTI_FS_SUFFIX = "diffusion";

    private static final String IMG_IMAGE_INFO_GROUP = "Image Information";

    private static final String IMG_FILE_ELEMENT_NAME = "ImgFile";

    private static final String IMG_PREVIEW_ELEMENT_NAME = "ImgPreviewFile";

    private static final String IMG_HASH_CODE_ELEMENT_NAME = "ImgFileHashCode";

    private static final String IMG_DIFF_GROUP = "Diffusion Direction Data";

    private static final String IMG_DIFF_BVAL_ELEMENT_NAME = "ImgDiffusionBValFile";

    private static final String IMG_DIFF_BVEC_ELEMENT_NAME = "ImgDiffusionBVecFile";

    private static final String IMG_MR_GROUP = "Magnetic Resonance Information";

    private static final String IMG_PULSE_SEQ_ELEMENT_NAME = "ImgPulseSeqTyp";

    private static final String recordIndicatorColumn = "record";

    private static final String recordIndicatorValue = "x";

    private static final String[] PDBP_IMAGING_STRUCTURE_PREFIX_LIST = {"PDBPImag", "PDBP_Imag"};

    private static final String SITE_NAME_ELEMENT_NAME = "SiteName";

    private static final String[] PDBP_ALLOWED_SITE_NAMES = {"Brigham and Women's", "Cleveland Clinic", "Columbia University", "Emory University", "Florida Atlantic University",
            "Johns Hopkins University", "Mayo Clinic-FL", "Mayo Clinic-MN", "Northwestern University", "Pennsylvania State University (Hershey)",
            "Pacific Northwest National Laboratory", "Rush University", "Thomas Jefferson University", "University of Alabama (Birmingham)", "University of California San Diego",
            "University of Florida (Gainesville)", "University of Michigan", "University of North Carolina", "University of Pennsylvania", "University of Pittsburgh",
            "University of Washington", "UT-Southwestern Medical Center", "VA-Pugent Sound Health Care System/University of Washington",};

    private static final String[] allowedGuidPrefixes = new String[] {"TBI", "PD", "NEI", "NTI", "NIA", "NINDS", "NIH", "NINR"};

    private static final String[] imagingStructurePrefixes;
    static {
        imagingStructurePrefixes = new String[1 + PDBP_IMAGING_STRUCTURE_PREFIX_LIST.length];
        imagingStructurePrefixes[0] = "Imag";
        for (int i = 0; i < PDBP_IMAGING_STRUCTURE_PREFIX_LIST.length; i++) {
            imagingStructurePrefixes[i + 1] = PDBP_IMAGING_STRUCTURE_PREFIX_LIST[i];
        }
    }

    private javax.swing.SwingWorker<Object, Object> fileWriterWorkerThread;

    private JDialog deidentDialog;

    private boolean csvDeidentDontAsk = false;

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
            + "familiarize yourself with our privacy rules, available through the BRICS Rules of Behavior\n" + "document and supporting documentation.\n" + "\n"
            + "Collection of this information is authorized under 42 U.S.C. 241, 242, 248, 281(a)(b)(1)(P)\n"
            + "and 44 U.S.C. 3101. The primary use of this information is to facilitate medical research.\n"
            + "This information may be disclosed to researchers for research purposes, and to system \n" + "administrators for evaluation and data normalization.\n" + "\n"
            + "Rules governing submission of this information are based on the data sharing rules defined in\n"
            + "the Notice of Grant Award (NOGA). If you do not have a grant defining data sharing requirements,\n"
            + "data submission is voluntary. Data entered into BRICS will be used solely for scientific and\n"
            + "research purposes. Significant system update information may be posted on\n" + "the BRICS site as required.";

    private static final Comparator<DataElementValue> dataElementCompare = new Comparator<DataElementValue>() {
        @Override
        public int compare(final DataElementValue o1, final DataElementValue o2) {
            return new Integer(o1.getPosition()).compareTo(new Integer(o2.getPosition()));
        }
    };

    private static final Comparator<MapElement> mapElementCompare = new Comparator<MapElement>() {
        @Override
        public int compare(final MapElement o1, final MapElement o2) {
            return new Integer(o1.getPosition()).compareTo(new Integer(o2.getPosition()));
        }
    };

    private static final Comparator<RepeatableGroup> groupCompare = new Comparator<RepeatableGroup>() {
        @Override
        public int compare(final RepeatableGroup o1, final RepeatableGroup o2) {
            return new Integer(o1.getPosition()).compareTo(new Integer(o2.getPosition()));
        }
    };

    private static final Comparator<ValueRange> valueRangeCompare = new Comparator<ValueRange>() {
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
        //readConfig();
        readCmdLineDictionarySelection();
        readDictionaryConfig();

        init();
        setVisible(true);
        validate();

        // if run from the command line with some variables set via the -d option, auto-load the csv
        if (VariableTable.getReference().isVariableSet(cmdLineCsvVar)) {
            cmdLineCsvFlag = true;
            cmdLineCsvFile = VariableTable.getReference().interpolate(cmdLineCsvVar);
            System.err.println("Command line CSV: " + cmdLineCsvFile);

            if ( ! (new File(cmdLineCsvFile).exists())) {
                MipavUtil.displayError("Command line CSV not found: " + cmdLineCsvFile);
                if (ViewUserInterface.getReference() != null && ViewUserInterface.getReference().isPlugInFrameVisible()) {
                    System.gc();
                    System.exit(0);
                } else {
                    return;
                }
            }

            if (VariableTable.getReference().isVariableSet(cmdLineOutputVar)) {
                cmdLineOutputDir = VariableTable.getReference().interpolate(cmdLineOutputVar);
                System.err.println("Command line output dir: " + cmdLineOutputDir);
            }

            if (VariableTable.getReference().isVariableSet(cmdLineCsvExitVar)) {
                cmdLineCsvExit = Boolean.parseBoolean(VariableTable.getReference().interpolate(cmdLineCsvExitVar));
                System.err.println("Exit after generation: " + cmdLineCsvExit);
            }
        }

        if ( !cmdLineCsvFlag) {
            final int response = JOptionPane.showConfirmDialog(this, PlugInDialogFITBIR.PRIVACY_NOTICE, "Image Submission Package Creation Tool", JOptionPane.YES_NO_OPTION,
                    JOptionPane.QUESTION_MESSAGE);

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
        } else {
            final Thread thread = new FormListRESTThread(this);
            thread.run();

            csvFile = new File(cmdLineCsvFile);

            // change output dir
            if (cmdLineOutputDir != null) {
                // revert to default if there is a problem with the output dir
                if ( ! (new File(cmdLineOutputDir)).exists()) {
                    System.err.println("Command line output dir does not exist (" + cmdLineOutputDir + "), reverting to default.");
                    outputDirBase = getNonCollidingNumberedFilePath(outputDirBase + File.separator + csvFile.getName(), 2) + File.separator;
                } else if ( ! (new File(cmdLineOutputDir)).canWrite()) {
                    System.err.println("Command line output dir is not writable (" + cmdLineOutputDir + "), reverting to default.");
                    outputDirBase = getNonCollidingNumberedFilePath(outputDirBase + File.separator + csvFile.getName(), 2) + File.separator;
                } else {
                    outputDirBase = getNonCollidingNumberedFilePath(cmdLineOutputDir + File.separator + csvFile.getName(), 2) + File.separator;
                }
            } else {
                outputDirBase = getNonCollidingNumberedFilePath(outputDirBase + File.separator + csvFile.getName(), 2) + File.separator;
            }
            outputDirTextField.setText(outputDirBase);

            // setup output and error logs and duplicate stderr/out to go to output
            try {
                if ( !new File(outputDirBase).exists()) {
                    new File(outputDirBase).mkdirs();
                }

                mainLogFile = new File(outputDirBase + "log_file_output.log");
                mainLogFile.createNewFile();
                readErrorFile = new File(outputDirBase + "log_image_read_error.log");
                readErrorFile.createNewFile();
                piiWarningFile = new File(outputDirBase + "log_pii_warnings.log");
                piiWarningFile.createNewFile();

                mainLogFileOut = new PrintStream(new FileOutputStream(mainLogFile, true));
                readErrorFileOut = new PrintStream(new FileOutputStream(readErrorFile, true));
                piiWarningFileOut = new PrintStream(new FileOutputStream(piiWarningFile, true));

                logOnlyOut = new CmdLineOutputStream(System.out, mainLogFileOut);
                logOnlyErr = new CmdLineOutputStream(System.err, mainLogFileOut);

                System.setOut(logOnlyOut);
                System.setErr(logOnlyErr);
            } catch (IOException e) {
                e.printStackTrace();
            }

            readCSVFile();

            csvFileDir = csvFile.getAbsolutePath() + File.separator;
            Preferences.setProperty(Preferences.PREF_BRICS_PLUGIN_CSV_DIR, csvFileDir);

            // generate files
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

            boolean readyForSub = checkRecordsForSubmission();

            if (readyForSub) {
                removeStructButton.setEnabled(false);
                finishButton.setEnabled(false);
                outputDirButton.setEnabled(false);
                addStructButton.setEnabled(false);
                editDataElementsButton.setEnabled(false);
                loadCSVButton.setEnabled(false);
                selectBIDSButton.setEnabled(false);

                fileWriterWorkerThread.run();

                // reset the outputs
                System.setOut(System.out);
                System.setErr(System.err);
                if (mainLogFileOut != null) {
                    mainLogFileOut.close();
                    mainLogFileOut = null;
                }
                if (readErrorFileOut != null) {
                    readErrorFileOut.close();
                    readErrorFileOut = null;
                }
                if (piiWarningFileOut != null) {
                    piiWarningFileOut.close();
                    piiWarningFileOut = null;
                }

                if (cmdLineCsvExit) {
                    System.exit(0);
                }
            } else {
                // reset the outputs
                System.setOut(System.out);
                System.setErr(System.err);
                if (mainLogFileOut != null) {
                    mainLogFileOut.close();
                    mainLogFileOut = null;
                }
                if (readErrorFileOut != null) {
                    readErrorFileOut.close();
                    readErrorFileOut = null;
                }
                if (piiWarningFileOut != null) {
                    piiWarningFileOut.close();
                    piiWarningFileOut = null;
                }
            }
        }
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
                processBIDSDirectories(chooser.getSelectedFile());

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

                boolean readyForSub = checkRecordsForSubmission();

                if (readyForSub) {
                    // fine so far, ask if the user is done
                    int response;
                    if ( !cmdLineCsvFlag) {
                        response = JOptionPane.showConfirmDialog(this, "Done adding image datasets?", "Done adding image datasets?", JOptionPane.YES_NO_OPTION,
                                JOptionPane.QUESTION_MESSAGE);
                    } else {
                        response = JOptionPane.YES_OPTION;
                    }

                    if (response == JOptionPane.YES_OPTION) {
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
        } else if (command.equalsIgnoreCase("dontAskPII")) {
            AbstractButton cb = (AbstractButton) e.getSource();
            csvDeidentDontAsk = cb.isSelected();
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

    private File findBIDSRootDir(final File selectedDir) {
        File rootDir = null;

        File[] dirFiles = selectedDir.listFiles();
        for (int i = 0; rootDir == null && i < dirFiles.length; i++) {
            File file = dirFiles[i];
            if ( !isFileToIgnore(file)) {
                if (file.isDirectory()) {
                    rootDir = findBIDSRootDir(file);
                    break;
                } else {
                    if (isBIDSKeyFile(file)) {
                        rootDir = selectedDir;
                        break;
                    }
                }
            }
        }

        return rootDir;
    }

    private final boolean isFileToIgnore(final File file) {
        if (file.getName().equalsIgnoreCase(".DS_Store") || file.getName().equalsIgnoreCase("__MACOSX")) {
            return true;
        }

        return false;
    }

    private final boolean isBIDSKeyFile(final File file) {
        if (file.getName().equalsIgnoreCase("participants.tsv") || file.getName().equalsIgnoreCase("dataset_description.json")
                || file.getName().substring(0, 3).equalsIgnoreCase("SUB")) {
            return true;
        }

        return false;
    }

    private boolean processBIDSDirectories(final File selectedDir) {
        File bidsDir = findBIDSRootDir(selectedDir);

        if (bidsDir == null) {
            MipavUtil.displayError("No BIDS root directory found in selected path: " + selectedDir);
            return false;
        }

        BIDSPackage bidsPackage = new BIDSPackage(bidsDir);

        final ViewJProgressBar progressBar = new ViewJProgressBar("Reading BIDS directories", "Reading BIDS directories...", 0, 100, false);
        progressBar.setVisible(true);
        progressBar.updateValue(5);

        // Read directory and find no. of images
        File[] bidsDirFiles = bidsDir.listFiles();
        Arrays.sort(bidsDirFiles, new fileComparator());
        for (int i = 0; i < bidsDirFiles.length; i++) {
            if ( (bidsDirFiles[i].isDirectory()) && (bidsDirFiles[i].getName().substring(0, 3).equalsIgnoreCase("SUB"))) {
                bidsPackage.addSubject(new BIDSSubject(bidsPackage, bidsDirFiles[i]));
            } else if ( (bidsDirFiles[i].isFile()) && (bidsDirFiles[i].getName().equalsIgnoreCase("participants.tsv"))) {
                bidsPackage.setParticipantsFile(bidsDirFiles[i]);
            } else if ( (bidsDirFiles[i].isFile()) && (bidsDirFiles[i].getName().equalsIgnoreCase("dataset_description.json"))) {
                bidsPackage.setDatasetDescriptionFile(bidsDirFiles[i]);
            } else if (bidsDirFiles[i].isFile() && bidsDirFiles[i].getName().endsWith(".json")) {
                bidsPackage.addAdditionalFile(bidsDirFiles[i]);
            } else if (bidsDirFiles[i].isFile() && (bidsDirFiles[i].getName().endsWith("_tsv") || bidsDirFiles[i].getName().endsWith(".tsv"))) {
                bidsPackage.addAdditionalFile(bidsDirFiles[i]);
            } else if (bidsDirFiles[i].isFile() && (bidsDirFiles[i].getName().endsWith(".bval") || bidsDirFiles[i].getName().endsWith(".bvec"))) {
                bidsPackage.addAdditionalFile(bidsDirFiles[i]);
            }
        }

        progressBar.setMessage("Reading participants.tsv");
        progressBar.updateValue(10);

        if (bidsPackage.getParticipantsFile() != null) {
            Reader tsvIn;
            Iterable<CSVRecord> participantRecords = null;
            try {
                tsvIn = new FileReader(bidsPackage.getParticipantsFile());
                participantRecords = CSVFormat.TDF.withHeader().parse(tsvIn);
            } catch (IOException e1) {
                e1.printStackTrace();
            }
            if (participantRecords != null) {
                for (CSVRecord record : participantRecords) {
                    String subjID = record.get("participant_id");
                    if (subjID == null || subjID.trim().equals("")) {
                        subjID = record.get("subject_id");
                    }
                    BIDSSubject subj = bidsPackage.getSubject(subjID);
                    if (subj != null) {
                        subj.setSubjectID(subjID);
                        if (record.isSet("guid")) {
                            subj.setGuid(record.get("guid"));
                        }
                        if (record.isSet("age")) {
                            subj.setAge(record.get("age"));
                        }
                        if (record.isSet("age_at_first_scan_years")) {
                            subj.setAge(record.get("age_at_first_scan_years"));
                        }
                    }
                }
            }

            for (BIDSSubject subj : bidsPackage.getSubjectList()) {
                File[] subjFiles = subj.getSubjectDirFile().listFiles();
                Arrays.sort(subjFiles, new fileComparator());

                for (int i = 0; i < subjFiles.length; i++) {
                    if ( (subjFiles[i].isDirectory()) && (subjFiles[i].getName().substring(0, 3).equalsIgnoreCase("SES"))) {
                        subj.addSession(new BIDSSession(subj, subjFiles[i]));
                    } else if ( (subjFiles[i].isFile()) && (subjFiles[i].getName().toLowerCase().endsWith(".tsv"))) {
                        subj.addAdditionalFile(subjFiles[i]);
                    }
                }
            }
        }

        // TODO - disabled support for session-specific id tsv
//        if (totalSubjectSessionsTSVFiles > 0) {
//            subject_id_array = new String[numberSubjects];
//            for (i = 0; i < numberSubjects; i++) {
//                for (j = 0; j < sessionstsvSubjectDirectoryFiles[i].length; j++) {
//                    try {
//                        raFile = new RandomAccessFile(tsvSubjectDirectoryFiles[i][j], "r");
//                        processFile = true;
//                    } catch (final FileNotFoundException e) {
//                        System.err.println("FileNotFoundException " + e);
//                        processFile = false;
//                    }
//                    if (processFile) {
//                        try {
//                            fileLength = raFile.length();
//                        } catch (final IOException e) {
//                            System.err.println("IOException " + e);
//                            processFile = false;
//                        }
//                    } // if (processFile)
//                    if (processFile) {
//                        try {
//                            line = raFile.readLine();
//                            readMoreLines = true;
//                        } catch (final IOException e) {
//                            System.err.println("IOException " + e);
//                            readMoreLines = false;
//                        }
//                        if (readMoreLines) {
//                            tokens = line.split("\t");
//                            headerTokenNumber = tokens.length;
//                            subject_id_index = -1;
//                            for (k = 0; k < tokens.length; k++) {
//                                if (tokens[k].equalsIgnoreCase("subject_id")) {
//                                    subject_id_index = k;
//                                }
//                            } // // for (k = 0; k < tokens.length; k++)
//                            if (subject_id_index >= 0) {
//                                try {
//                                    filePos = raFile.getFilePointer();
//                                } catch (final IOException e) {
//                                    System.err.println("IOException " + e);
//                                    filePos = fileLength;
//                                }
//                                sessionsRead = 0;
//                                indexRead = false;
//                                while (readMoreLines && (sessionsRead < sessionFiles[i].length) && (filePos < fileLength)) {
//                                    try {
//                                        line = raFile.readLine();
//                                        readMoreLines = true;
//                                    } catch (final IOException e) {
//                                        System.err.println("IOException " + e);
//                                        readMoreLines = false;
//                                    }
//                                    tokens = line.split("\t");
//                                    sessionTokenNumber = tokens.length;
//                                    if (headerTokenNumber != sessionTokenNumber) {
//                                        System.err.println("Header token number = " + headerTokenNumber + ", but session token number = " + sessionTokenNumber);
//                                    }
//                                    if (tokens.length - 1 >= subject_id_index) {
//                                        if ( !indexRead) {
//                                            subject_id_array[i] = tokens[subject_id_index];
//                                            indexRead = true;
//                                            subject_id_read++;
//                                        } else if (Integer.valueOf(subject_id_array[i]).intValue() != Integer.valueOf(tokens[subject_id_index]).intValue()) {
//                                            System.err.println("subject_id number varies across sessions for subject subdirectory " + i);
//                                            System.err.println("subject_id_array[" + i + "] = " + subject_id_array[i]);
//                                            System.err.println("tokens[" + subject_id_index + "] = " + tokens[subject_id_index]);
//                                        }
//                                        sessionsRead++;
//                                    }
//
//                                    try {
//                                        filePos = raFile.getFilePointer();
//                                    } catch (final IOException e) {
//                                        System.err.println("IOException " + e);
//                                        filePos = fileLength;
//                                    }
//                                } // while (readMoreLines && (sessionsRead < sessionFiles[i].length) && (filePos <
//                                  // fileLength))
//                            } // if (subject_id_index >= 0)
//                        } // if (readMoreLines)
//                    } // if (processFile)
//                    if (processFile) {
//                        try {
//                            raFile.close();
//                        } catch (final IOException e) {
//                            System.err.println("IOException " + e);
//                        }
//                    }
//                }
//            }
//            printlnToLog(subject_id_read + " subject id read from _sessions.tsv files in subject subdirectories");
//        } // if (totalSubjectSessionsTSVFiles > 0)

        progressBar.setMessage("Sorting files");
        progressBar.updateValue(15);

        int totalScanNum = 0;

        for (BIDSSubject subj : bidsPackage.getSubjectList()) {
            for (BIDSSession session : subj.getSessionList()) {
                File[] sessionFiles = session.getSessionDirFile().listFiles();
                for (int i = 0; i < sessionFiles.length; i++) {
                    File curSessionFile = sessionFiles[i];
                    if (curSessionFile.isDirectory()) {
                        BIDSScanType scanType = BIDSScanType.valueOf(curSessionFile.getName());
                        BIDSScanCategory newScanCat = new BIDSScanCategory(session, curSessionFile, scanType);

                        File[] scanCatFileArray = curSessionFile.listFiles();
                        Arrays.sort(scanCatFileArray, new fileComparator());
                        Vector<File> scanCatFileList = new Vector<File>(Arrays.asList(scanCatFileArray));

                        Vector<File> remainingFiles = new Vector<File>(scanCatFileList);

                        // find all the image files
                        Vector<File> imageFiles = new Vector<File>();
                        for (File curScanCatFile : scanCatFileList) {
                            if (curScanCatFile.getName().endsWith("nii.gz") || curScanCatFile.getName().endsWith(".nii")) {
                                imageFiles.add(curScanCatFile);
                                remainingFiles.removeElement(curScanCatFile);
                            }
                        }

                        // for each image, find similar files and add them as new scans
                        for (File imgFile : imageFiles) {
                            String imgFileBasename = imgFile.getName().substring(0, imgFile.getName().indexOf(".nii"));

                            BIDSScan newScan = new BIDSScan(newScanCat, imgFile);

                            for (File nonImgFile : scanCatFileList) {
                                String nonImgFileName = nonImgFile.getName();
                                if (nonImgFileName.startsWith(imgFileBasename)) {
                                    if (nonImgFileName.endsWith(".json")) {
                                        newScan.setScanJsonFile(nonImgFile);
                                        remainingFiles.removeElement(nonImgFile);
                                    } else if ( !imgFile.equals(nonImgFile)) {
                                        newScan.addAdditionalFile(nonImgFile);
                                        remainingFiles.removeElement(nonImgFile);
                                    }
                                }
                            }

                            newScanCat.addScan(newScan);

                            totalScanNum++;
                        }

                        // add other files as category aux files
                        for (File file : remainingFiles) {
                            newScanCat.addAdditionalFile(file);
                        }

                        session.addScanCategory(newScanCat);
                    } else if (curSessionFile.getName().toLowerCase().endsWith(".tsv")) {
                        session.addAdditionalFile(curSessionFile);
                    }
                }
            }
        }

        progressBar.setMessage("Reading images");
        progressBar.updateValue(20);

        int progressPerScan = (int) (80.0 / totalScanNum);

        Vector<Vector<FileDicomTag>> problemTagList = new Vector<Vector<FileDicomTag>>();
        Vector<String> problemFileDirList = new Vector<String>();
        Vector<String> problemFileNameList = new Vector<String>();

        bidsPackageFlag = true;

        int curScanNum = 0;
        for (BIDSSubject subj : bidsPackage.getSubjectList()) {
            for (BIDSSession session : subj.getSessionList()) {
                for (BIDSScanCategory scanCat : session.getScanCategoryList()) {
                    for (BIDSScan scan : scanCat.getScanList()) {
                        ArrayList<ArrayList<String>> record;
                        InfoDialog csvDialog;

                        curScanNum++;
                        progressBar.setMessage("Processing scan " + curScanNum + " of " + totalScanNum);
                        progressBar.updateValue(20 + (progressPerScan * curScanNum));

                        switch (scan.getParent().getScanType()) {
                            case ANAT:
                            case anat:
                                // builds class var csvFieldNames and returns record list to match
                                record = buildInputCSVRowFromBIDS(scan);

                                csvDialog = new InfoDialog(this, "ImagingMR", false, false, record);

                                // change i counter to 0-based for problem lists
                                problemTagList.add(csvDialog.getProblemTags());
                                problemFileDirList.add(csvDialog.getProblemFileDir());
                                problemFileNameList.add(csvDialog.getProblemFileName());

                                break;
                            case DWI:
                            case dwi:
                                // builds class var csvFieldNames and returns record list to match
                                record = buildInputCSVRowFromBIDS(scan);

                                csvDialog = new InfoDialog(this, "ImagingDiffusion", false, false, record);

                                // change i counter to 0-based for problem lists
                                problemTagList.add(csvDialog.getProblemTags());
                                problemFileDirList.add(csvDialog.getProblemFileDir());
                                problemFileNameList.add(csvDialog.getProblemFileName());

                                break;
                            case FUNC:
                            case func:
                                // builds class var csvFieldNames and returns record list to match
                                record = buildInputCSVRowFromBIDS(scan);

                                csvDialog = new InfoDialog(this, "ImagingFunctionalMR", false, false, record);

                                // change i counter to 0-based for problem lists
                                problemTagList.add(csvDialog.getProblemTags());
                                problemFileDirList.add(csvDialog.getProblemFileDir());
                                problemFileNameList.add(csvDialog.getProblemFileName());
                                break;
                            case FMAP:
                            case fmap:
                                // TODO - ?
                                break;
                        }
                    }
                }
            }
        }

        // after all the fake CSVs are loaded, check for de-identification issues
        for (int j = 0; j < problemTagList.size(); j++) {
            final Vector<FileDicomTag> problemTags = problemTagList.get(j);
            if (problemTags != null) {
                boolean isDeidentified = deidentificationDialogDicom(problemFileDirList.get(j), problemFileNameList.get(j), problemTags, true);

                // if the user certified that all data is okay, stop checking
                if (csvDeidentDontAsk) {
                    break;
                }

                if ( !isDeidentified) {
                    // should have already exited
                    continue;
                }
            }
        }

//
//        if (funcNumber > 0) {
//            found = false;
//            for (i = 0; i < dataStructureList.size() && ( !found); i++) {
//                if (dataStructureList.get(i).getShortName().equalsIgnoreCase("ImagingFunctionalMR")) {
//                    found = true;
//                    ds = dataStructureList.get(i);
//                    dataStructureName = "ImagingFunctionalMR";
//                    if (ds.getDataElements().size() == 0) {
//                        progressBar.setMessage("Retrieving data elements for form structure: " + ds.getShortName());
//                        final FormDataElementsRESTThread thread = new FormDataElementsRESTThread(this, ds.getShortName(), false);
//                        thread.run();
//
//                        dsInfo = thread.getFullFormStructure();
//                    } else {
//                        dsInfo = ds;
//                    }
//                }
//            }
//            funcImagesRead = 0;
//            funcJsonRead = 0;
//            funcEventsRead = 0;
//            funcPhysioTsvRead = 0;
//            funcPhysioJsonRead = 0;
//            for (i = 0; i < numberSubjects; i++) {
//                for (j = 0; j < funcFiles[i].length; j++) {
//                    pValue = 20 + 80 * subdirectoriesRead / subdirectoriesFound;
//                    progressBar.updateValue(pValue);
//                    if ( (funcFiles[i][j] != null) && (funcFiles[i][j].length > 0)) {
//                        subdirectoriesRead++;
//                        sessionImagesRead = 0;
//                        sessionJsonRead = 0;
//                        sessionEventsRead = 0;
//                        sessionPhysioTsvRead = 0;
//                        sessionPhysioJsonRead = 0;
//
//                        for (k = 0; k < funcFiles[i][j].length; k++) {
//                            if ( (funcFiles[i][j][k].getName().endsWith("nii.gz")) || (funcFiles[i][j][k].getName().endsWith(".nii"))) {
//                                if (funcFiles[i][j][k].getName().endsWith("nii.gz")) {
//                                    progressBar.setMessage("Reading " + funcFiles[i][j][k].getName());
//                                    progressBar.updateValue(pValue + k * 80 / (subdirectoriesFound * funcFiles[i][j].length));
//                                    sessionImageList[sessionImagesRead] = fileIO.readNIFTI(funcFiles[i][j][k].getName(), funcFiles[i][j][k].getParentFile()
//                                            .getAbsolutePath(), false, true, true, true);
//                                    index = funcFiles[i][j][k].getName().indexOf("nii.gz");
//                                } else {
//                                    sessionImageList[sessionImagesRead] = fileIO.readNIFTI(funcFiles[i][j][k].getName(), funcFiles[i][j][k].getParentFile()
//                                            .getAbsolutePath(), false, false, true, true);
//                                    index = funcFiles[i][j][k].getName().indexOf("nii");
//                                }
//                                funcImagesRead++;
//                                baseName = funcFiles[i][j][k].getName().substring(0, index);
//                                jsonName = baseName + "json";
//                                found = false;
//                                for (n = 0; n < funcFiles[i][j].length && ( !found); n++) {
//                                    if (funcFiles[i][j][n].getName().equals(jsonName)) {
//                                        found = true;
//                                        jsonFile[sessionImagesRead] = funcFiles[i][j][n];
//                                        funcJsonRead++;
//                                        sessionJsonRead++;
//                                    }
//                                }
//                                index = baseName.lastIndexOf("_");
//                                eventsBaseName = baseName.substring(0, index + 1);
//                                eventsName = eventsBaseName + "events.tsv";
//                                found = false;
//                                for (n = 0; n < funcFiles[i][j].length && ( !found); n++) {
//                                    if (funcFiles[i][j][n].getName().equals(eventsName)) {
//                                        found = true;
//                                        eventsFile[sessionImagesRead] = funcFiles[i][j][n];
//                                        funcEventsRead++;
//                                        sessionEventsRead++;
//                                    }
//                                }
//                                found = false;
//                                for (n = 0; n < funcFiles[i][j].length && ( !found); n++) {
//                                    if ( (funcFiles[i][j][n].getName().length() >= index + 1)
//                                            && (funcFiles[i][j][n].getName().substring(0, index + 1).equals(eventsBaseName))
//                                            && ( (funcFiles[i][j][n].getName().endsWith("tsv")) || (funcFiles[i][j][n].getName().endsWith("tsv.gz")))) {
//                                        found = true;
//                                        physioTsvFile[sessionImagesRead] = funcFiles[i][j][n];
//                                        funcPhysioTsvRead++;
//                                        sessionPhysioTsvRead++;
//                                    }
//                                }
//                                found = false;
//                                for (n = 0; n < funcFiles[i][j].length && ( !found); n++) {
//                                    if ( (funcFiles[i][j][n].getName().length() >= index + 1)
//                                            && (funcFiles[i][j][n].getName().substring(0, index + 1).equals(eventsBaseName))
//                                            && (funcFiles[i][j][n].getName().endsWith("json"))) {
//                                        found = true;
//                                        physioJsonFile[sessionImagesRead] = funcFiles[i][j][n];
//                                        funcPhysioJsonRead++;
//                                        sessionPhysioJsonRead++;
//                                    }
//                                }
//                                sessionImagesRead++;
//                            } // if ((funcFiles[i][j][k].getName().endsWith("nii.gz")) ||
//                        } // for (k = 0; k < funcFiles[i][j].length; k++)
//                        imagingFMRIAuxiliaryFile = null;
//                        fMRIAuxiliaryFileNumber = funcFiles[i][j].length - sessionImagesRead;
//                        if (participantsFile != null) {
//                            fMRIAuxiliaryFileNumber++;
//                        }
//                        if (scanstsvSubjectDirectoryFiles != null) {
//                            fMRIAuxiliaryFileNumber += scanstsvSubjectDirectoryFiles[i].length;
//                        }
//                        if (sessionstsvSubjectDirectoryFiles != null) {
//                            fMRIAuxiliaryFileNumber += sessionstsvSubjectDirectoryFiles[i].length;
//                        }
//                        if (scanstsvSessionDirectoryFiles != null) {
//                            fMRIAuxiliaryFileNumber += scanstsvSessionDirectoryFiles[i][j].length;
//                        }
//                        if (eventsTSVFilenames != null) {
//                            for (n = 0; n < eventsTSVFilenames.length; n++) {
//                                for (k = 0, found = false; k < funcFiles[i][j].length && !found; k++) {
//                                    if ( (funcFiles[i][j][k].getName().endsWith("nii.gz")) || (funcFiles[i][j][k].getName().endsWith(".nii"))) {
//                                        if (funcFiles[i][j][k].getName().contains(eventsTSVFilenames[n])) {
//                                            fMRIAuxiliaryFileNumber++;
//                                            found = true;
//                                        }
//                                    }
//                                }
//                            } // for (n = 0; n < eventsTSVFilenames.length; n++)
//                        } // if (eventsTSVFilenames != null)
//                        if (boldJsonFilenames != null) {
//                            for (n = 0; n < boldJsonFilenames.length; n++) {
//                                for (k = 0, found = false; k < funcFiles[i][j].length && !found; k++) {
//                                    if ( (funcFiles[i][j][k].getName().endsWith("nii.gz")) || (funcFiles[i][j][k].getName().endsWith(".nii"))) {
//                                        if (funcFiles[i][j][k].getName().contains(boldJsonFilenames[n])) {
//                                            fMRIAuxiliaryFileNumber++;
//                                            found = true;
//                                        }
//                                    }
//                                }
//                            }
//                        } // if (boldJsonFilenames != null)
//                        if (physioJsonFilenames != null) {
//                            for (n = 0; n < physioJsonFilenames.length; n++) {
//                                for (k = 0, found = false; k < funcFiles[i][j].length && !found; k++) {
//                                    if ( (funcFiles[i][j][k].getName().endsWith("nii.gz")) || (funcFiles[i][j][k].getName().endsWith(".nii"))) {
//                                        if (funcFiles[i][j][k].getName().contains(physioJsonFilenames[n])) {
//                                            fMRIAuxiliaryFileNumber++;
//                                            found = true;
//                                        }
//                                    }
//                                }
//                            }
//                        } // if (physioJsonFilenames != null)
//                        if (fMRIAuxiliaryFileNumber > 0) {
//                            imagingFMRIAuxiliaryFile = new String[fMRIAuxiliaryFileNumber];
//                            m = 0;
//                            if (participantsFile != null) {
//                                fullPath = participantsFile.getAbsolutePath();
//                                index = fullPath.indexOf(BIDSString);
//                                imagingFMRIAuxiliaryFile[m++] = fullPath.substring(index);
//                            }
//                            if (scanstsvSubjectDirectoryFiles != null) {
//                                for (k = 0; k < scanstsvSubjectDirectoryFiles[i].length; k++) {
//                                    fullPath = scanstsvSubjectDirectoryFiles[i][k].getAbsolutePath();
//                                    index = fullPath.indexOf(BIDSString);
//                                    imagingFMRIAuxiliaryFile[m++] = fullPath.substring(index);
//                                }
//                            }
//                            if (sessionstsvSubjectDirectoryFiles != null) {
//                                for (k = 0; k < sessionstsvSubjectDirectoryFiles[i].length; k++) {
//                                    fullPath = sessionstsvSubjectDirectoryFiles[i][k].getAbsolutePath();
//                                    index = fullPath.indexOf(BIDSString);
//                                    imagingFMRIAuxiliaryFile[m++] = fullPath.substring(index);
//                                }
//                            }
//                            if (scanstsvSessionDirectoryFiles != null) {
//                                for (k = 0; k < scanstsvSessionDirectoryFiles[i][j].length; k++) {
//                                    fullPath = scanstsvSessionDirectoryFiles[i][j][k].getAbsolutePath();
//                                    index = fullPath.indexOf(BIDSString);
//                                    imagingFMRIAuxiliaryFile[m++] = fullPath.substring(index);
//                                }
//                            }
//                            if (eventsTSVFilenames != null) {
//                                for (n = 0; n < eventsTSVFilenames.length; n++) {
//                                    for (k = 0, found = false; k < funcFiles[i][j].length && !found; k++) {
//                                        if ( (funcFiles[i][j][k].getName().endsWith("nii.gz")) || (funcFiles[i][j][k].getName().endsWith(".nii"))) {
//                                            if (funcFiles[i][j][k].getName().contains(eventsTSVFilenames[n])) {
//                                                imagingFMRIAuxiliaryFile[m++] = eventsTSVPathnames[n];
//                                                found = true;
//                                            }
//                                        }
//                                    }
//                                } // for (n = 0; n < eventsTSVFilenames.length; n++)
//                            } // if (eventsTSVFilenames != null)
//                            if (boldJsonFilenames != null) {
//                                for (n = 0; n < boldJsonFilenames.length; n++) {
//                                    for (k = 0, found = false; k < funcFiles[i][j].length && !found; k++) {
//                                        if ( (funcFiles[i][j][k].getName().endsWith("nii.gz")) || (funcFiles[i][j][k].getName().endsWith(".nii"))) {
//                                            if (funcFiles[i][j][k].getName().contains(boldJsonFilenames[n])) {
//                                                imagingFMRIAuxiliaryFile[m++] = boldJsonPathnames[n];
//                                                found = true;
//                                            }
//                                        }
//                                    }
//                                }
//                            } // if (boldJsonFilenames != null)
//                            if (physioJsonFilenames != null) {
//                                for (n = 0; n < physioJsonFilenames.length; n++) {
//                                    for (k = 0, found = false; k < funcFiles[i][j].length && !found; k++) {
//                                        if ( (funcFiles[i][j][k].getName().endsWith("nii.gz")) || (funcFiles[i][j][k].getName().endsWith(".nii"))) {
//                                            if (funcFiles[i][j][k].getName().contains(physioJsonFilenames[n])) {
//                                                imagingFMRIAuxiliaryFile[m++] = physioJsonPathnames[n];
//                                                found = true;
//                                            }
//                                        }
//                                    }
//                                }
//                            } // if (physioJsonFilenames != null)
//                        } // if (fMRIAuxiliaryFileNumber > 0)
//
//                        if (subject_id_array != null) {
//                            subject_id = subject_id_array[i];
//                        } else if (participant_id_array != null) {
//                            subject_id = participant_id_array[i];
//                        } else {
//                            subject_id = null;
//                        }
//                        if (age_array != null) {
//                            age = age_array[i];
//                        } else {
//                            age = null;
//                        }
//                        if (guid_array != null) {
//                            guid = guid_array[i];
//                        } else {
//                            guid = null;
//                        }
//                        
//                        for (int curVol = 0; curVol < sessionImagesRead; curVol++) {
//                            previewImages.add(null);
//                            structRowImgFileInfoList.add(null);
//                            fsDataList.add(null);
//                            allOtherFilesAL.add(null);
//                            fsData = new FormStructureData(dsInfo);
//                            
//                            parseDataStructure(dsInfo, fMRIAuxiliaryFileNumber);
//                            
//                            parseForInitLabelsAndComponents();
//                            
//                            populateFieldsFromBIDS(sessionImageList[curVol], boldJsonFilenames, effectiveEchoSpacing, echoTime, repetitionTime, subject_id, guid, age,
//                                    imagingFMRIAuxiliaryFile, dwibvalString, dwibvecString);
//                        }
//                        
//                        for (k = 0; k < sessionImagesRead; k++) {
//                            sessionImageList[k].disposeLocal();
//                            sessionImageList[k] = null;
//                            jsonFile[k] = null;
//                            eventsFile[k] = null;
//                            physioTsvFile[k] = null;
//                            physioJsonFile[k] = null;
//                        }
//                    }
//                }
//            }
//            printlnToLog("func subdirectory images read = " + funcImagesRead);
//            printlnToLog("func subdirectory JSON files read = " + funcJsonRead);
//            printlnToLog("func subdirectory events.tsv files read = " + funcEventsRead);
//            printlnToLog("func subdirectory physio.tsv or physio.tsv.gz files read = " + funcPhysioTsvRead);
//            printlnToLog("func subdirectory physio json files read = " + funcPhysioJsonRead);
//        } // if (funcNumber > 0)
//        imagingFMRIAuxiliaryFile = null;
//        fMRIAuxiliaryFileNumber = 0;

        progressBar.updateValue(100);
        progressBar.dispose();

        return true;
    }

    @SuppressWarnings("unused")
    private class BIDSPackage {
        private File baseDir;

        private File participantsFile;

        private File datasetDescriptionFile;

        private JSONObject datasetDescriptionJson;

        private Vector<BIDSSubject> subjectList;

        private Vector<File> additionalFileList;

        public BIDSPackage(File baseDir) {
            super();

            subjectList = new Vector<BIDSSubject>();
            this.baseDir = baseDir;
            this.additionalFileList = new Vector<File>();
        }

        public File getBaseDir() {
            return baseDir;
        }

        public void setBaseDir(File baseDir) {
            this.baseDir = baseDir;
        }

        public File getParticipantsFile() {
            return participantsFile;
        }

        public void setParticipantsFile(File participantsFile) {
            this.participantsFile = participantsFile;
        }

        public File getDatasetDescriptionFile() {
            return datasetDescriptionFile;
        }

        public void setDatasetDescriptionFile(File datasetDescriptionFile) {
            this.datasetDescriptionFile = datasetDescriptionFile;
        }

        public JSONObject getDatasetDescriptionJson() {
            if (datasetDescriptionJson == null && datasetDescriptionFile != null) {
                datasetDescriptionJson = readJsonFile(datasetDescriptionFile);
            }
            return datasetDescriptionJson;
        }

        public void setDatasetDescriptionJson(JSONObject datasetDescriptionJson) {
            this.datasetDescriptionJson = datasetDescriptionJson;
        }

        public Vector<BIDSSubject> getSubjectList() {
            return subjectList;
        }

        public void setSubjectList(Vector<BIDSSubject> subjectList) {
            this.subjectList = subjectList;
        }

        public void addSubject(BIDSSubject subject) {
            this.subjectList.add(subject);
        }

        public BIDSSubject getSubject(String subjectID) {
            for (BIDSSubject subj : subjectList) {
                if (subj.getSubjectDirFile().getName().equalsIgnoreCase(subjectID)) {
                    return subj;
                }
            }

            return null;
        }

        public Vector<File> getAdditionalFileList() {
            return additionalFileList;
        }

        public void setAdditionalFileList(Vector<File> additionalFileList) {
            this.additionalFileList = additionalFileList;
        }

        public void addAdditionalFile(File file) {
            this.additionalFileList.add(file);
        }
    }

    @SuppressWarnings("unused")
    private class BIDSSubject {
        private BIDSPackage parent;

        private File subjectDirFile;

        private Vector<BIDSSession> sessionList;

        private String subjectID;

        private String guid;

        private String age;

        private Vector<File> additionalFileList;

        public BIDSSubject(BIDSPackage parent) {
            super();
            this.parent = parent;
            this.sessionList = new Vector<BIDSSession>();
            this.additionalFileList = new Vector<File>();
        }

        public BIDSSubject(BIDSPackage parent, File subjectDir) {
            this(parent);
            this.subjectDirFile = subjectDir;
        }

        public void addSession(BIDSSession newSession) {
            sessionList.add(newSession);
        }

        public String getSubjectID() {
            return subjectID;
        }

        public void setSubjectID(String subjectID) {
            this.subjectID = subjectID;
        }

        public String getGuid() {
            return guid;
        }

        public void setGuid(String guid) {
            this.guid = guid;
        }

        public String getAge() {
            return age;
        }

        public void setAge(String age) {
            this.age = age;
        }

        public File getSubjectDirFile() {
            return subjectDirFile;
        }

        public void setSubjectDirFile(File subjectDirFile) {
            this.subjectDirFile = subjectDirFile;
        }

        public void addAdditionalFile(File file) {
            this.additionalFileList.add(file);
        }

        public Vector<BIDSSession> getSessionList() {
            return sessionList;
        }

        public void setSessionList(Vector<BIDSSession> sessionList) {
            this.sessionList = sessionList;
        }

        public Vector<File> getAdditionalFileList() {
            return additionalFileList;
        }

        public void setAdditionalFileList(Vector<File> additionalFileList) {
            this.additionalFileList = additionalFileList;
        }

        public BIDSPackage getParent() {
            return parent;
        }

        public void setParent(BIDSPackage parent) {
            this.parent = parent;
        }
    }

    @SuppressWarnings("unused")
    private class BIDSSession {
        private BIDSSubject parent;

        private File sessionDirFile;

        private Vector<BIDSScanCategory> scanCategoryList;

        private Vector<File> additionalFileList;

        public BIDSSession(BIDSSubject parent) {
            super();
            this.parent = parent;
            this.scanCategoryList = new Vector<BIDSScanCategory>();
            this.additionalFileList = new Vector<File>();
        }

        public BIDSSession(BIDSSubject parent, File sessionDir) {
            this(parent);

            this.sessionDirFile = sessionDir;
        }

        public Vector<BIDSScanCategory> getScanCategoryList() {
            return scanCategoryList;
        }

        public void setScanCategoryList(Vector<BIDSScanCategory> scanCatList) {
            this.scanCategoryList = scanCatList;
        }

        public void addScanCategory(BIDSScanCategory newScanCat) {
            this.scanCategoryList.add(newScanCat);
        }

        public File getSessionDirFile() {
            return sessionDirFile;
        }

        public void setSessionDirFile(File sessionDirFile) {
            this.sessionDirFile = sessionDirFile;
        }

        public Vector<File> getAdditionalFileList() {
            return additionalFileList;
        }

        public void setAdditionalFileList(Vector<File> additionalFileList) {
            this.additionalFileList = additionalFileList;
        }

        public void addAdditionalFile(File file) {
            this.additionalFileList.add(file);
        }

        public BIDSSubject getParent() {
            return parent;
        }

        public void setParent(BIDSSubject parent) {
            this.parent = parent;
        }

        public BIDSPackage getPackage() {
            return parent.getParent();
        }
    }

    @SuppressWarnings("unused")
    private class BIDSScanCategory {
        private BIDSSession parent;

        private File scanCategoryDirFile;

        private BIDSScanType scanType;

        private Vector<BIDSScan> scanList;

        private Vector<File> additionalFileList;

        public BIDSScanCategory(BIDSSession parent, File scanCatDir) {
            super();

            this.parent = parent;
            scanCategoryDirFile = scanCatDir;
            scanList = new Vector<BIDSScan>();
            additionalFileList = new Vector<File>();
        }

        public BIDSScanCategory(BIDSSession parent, File scanCatDir, BIDSScanType scanType) {
            this(parent, scanCatDir);

            this.scanType = scanType;
        }

        public BIDSScanType getScanType() {
            return scanType;
        }

        public void setScanType(BIDSScanType scanType) {
            this.scanType = scanType;
        }

        public void setScanType(String scanDir) {
            this.scanType = BIDSScanType.valueOf(scanDir);
        }

        public void addAdditionalFile(File file) {
            this.additionalFileList.add(file);
        }

        public Vector<File> getAdditionalFileList() {
            return additionalFileList;
        }

        public void setAdditionalFileList(Vector<File> additionalFileList) {
            this.additionalFileList = additionalFileList;
        }

        public File getScanCategoryDirFile() {
            return scanCategoryDirFile;
        }

        public void setScanCategoryDirFile(File scanCategoryDirFile) {
            this.scanCategoryDirFile = scanCategoryDirFile;
        }

        public Vector<BIDSScan> getScanList() {
            return scanList;
        }

        public void setScanList(Vector<BIDSScan> scanList) {
            this.scanList = scanList;
        }

        public void addScan(BIDSScan scan) {
            this.scanList.add(scan);
        }

        public BIDSSession getParent() {
            return parent;
        }

        public void setParent(BIDSSession parent) {
            this.parent = parent;
        }

        public BIDSPackage getPackage() {
            return parent.getPackage();
        }

        public BIDSSubject getSubject() {
            return parent.getParent();
        }
    }

    @SuppressWarnings("unused")
    private class BIDSScan {
        private BIDSScanCategory parent;

        private File scanJsonFile;

        private JSONObject scanJson;

        private File scanImgFile;

        private Vector<File> additionalFileList;

        public BIDSScan(BIDSScanCategory parent) {
            super();

            this.parent = parent;
            additionalFileList = new Vector<File>();
        }

        public BIDSScan(BIDSScanCategory parent, File imgFile) {
            this(parent);

            scanImgFile = imgFile;
        }

        public BIDSScan(BIDSScanCategory parent, File scanJsonFile, File scanImgFile) {
            this(parent, scanImgFile);
            this.scanJsonFile = scanJsonFile;
        }

        public File getScanJsonFile() {
            return scanJsonFile;
        }

        public void setScanJsonFile(File scanJsonFile) {
            this.scanJsonFile = scanJsonFile;
        }

        public File getScanImgFile() {
            return scanImgFile;
        }

        public void setScanImgFile(File scanImgFile) {
            this.scanImgFile = scanImgFile;
        }

        public JSONObject getScanJson() {
            if (scanJson == null && scanJsonFile != null) {
                scanJson = readJsonFile(scanJsonFile);
            }
            return scanJson;
        }

        public void setScanJson(JSONObject scanJson) {
            this.scanJson = scanJson;
        }

        public void addAdditionalFile(File file) {
            this.additionalFileList.add(file);
        }

        public Vector<File> getAdditionalFileList() {
            return additionalFileList;
        }

        public void setAdditionalFileList(Vector<File> additionalFileList) {
            this.additionalFileList = additionalFileList;
        }

        public BIDSScanCategory getParent() {
            return parent;
        }

        public void setParent(BIDSScanCategory parent) {
            this.parent = parent;
        }

        public BIDSPackage getPackage() {
            return parent.getPackage();
        }

        public BIDSSubject getSubject() {
            return parent.getSubject();
        }

        public BIDSSession getSession() {
            return parent.getParent();
        }
    }

    @SuppressWarnings("unused")
    private enum BIDSScanType {
        anat("anat"), ANAT("anat"), dwi("dwi"), DWI("dwi"), func("func"), FUNC("func"), fmap("fmap"), FMAP("fmap");

        private final String dirName;

        BIDSScanType(final String dir) {
            dirName = dir;
        }

        public String getDirectoryName() {
            return dirName;
        }
    }

    private ArrayList<ArrayList<String>> buildInputCSVRowFromBIDS(BIDSScan scan) {
        csvFieldNames = new ArrayList<String>();

        ArrayList<ArrayList<String>> record = new ArrayList<ArrayList<String>>();

        ArrayList<String> recordRepeat = new ArrayList<String>();

        record.add(recordRepeat);

        addCSVEntry(recordRepeat, "Main.GUID", scan.getSubject().getGuid());

        addCSVEntry(recordRepeat, "Main.SubjectIDNum", scan.getSubject().getSubjectID());

        addCSVEntry(recordRepeat, "Main.AgeYrs", scan.getSubject().getAge());

//      "InstitutionName": "CTE_MAYO",
        addCSVEntry(scan.getScanJson(), "InstitutionName", recordRepeat, "Main.SiteName");

        addCSVEntry(recordRepeat, "Image Information.ImgFile", scan.getScanImgFile().getAbsolutePath());

        readGlobalFieldsFromJson(recordRepeat, scan.getPackage().getDatasetDescriptionJson());

        readGenericFieldsFromJson(recordRepeat, scan.getScanJson());

        readMRFieldsFromJson(recordRepeat, scan.getScanJson(), scan.getScanImgFile().getName());

        switch (scan.getParent().getScanType()) {
            case DWI:
            case dwi:
                readDwiFieldsFromJson(recordRepeat, scan.getScanJson());
                setBvalBvecFields(recordRepeat, scan.getAdditionalFileList());
                break;
            case FUNC:
            case func:
                readFuncFieldsFromJson(recordRepeat, scan.getScanJson());
                setAdditionalFuncFiles(record, scan.getAdditionalFileList());
                break;
            default:
                break;
        }

        return record;
    }

    private void readGlobalFieldsFromJson(ArrayList<String> recordRepeat, JSONObject jsonData) {
        addCSVEntry(jsonData, "ImgQAQCPerfInd", recordRepeat, "Image QA & QC.ImgQAQCPerfInd");
    }

    private void readGlobalFieldsFromJson(HashMap<String, String> extractedFields, JSONObject jsonData) {
        addExtractedField(jsonData, "ImgQAQCPerfInd", extractedFields, "Image QA & QC.ImgQAQCPerfInd");
    }

    private void readGenericFieldsFromJson(ArrayList<String> recordRepeat, JSONObject jsonData) {
//      "Manufacturer": "Siemens",
//      "ManufacturersModelName": "Skyra",
//      "SoftwareVersions": "syngo_MR_E11",
//      "SliceThickness": 1,

//      "SpacingBetweenSlices": 2,

        String groupName = "Image Information.";

        addCSVEntry(jsonData, "BodyPartExamined", recordRepeat, groupName + "ImgAntmicSite");

        addCSVEntry(jsonData, "Manufacturer", recordRepeat, groupName + "ImgScannerManufName");

        addCSVEntry(jsonData, "ManufacturersModelName", recordRepeat, groupName + "ImgScannerModelName");

        addCSVEntry(jsonData, "SoftwareVersions", recordRepeat, groupName + "ImgScannerSftwrVrsnNum");

        String jsonContents = jsonData.toString();

        addCSVEntry(recordRepeat, groupName + "ImgNotesTxt", jsonContents);

        groupName = "Image pixel information and dimensions.";

        addCSVEntry(jsonData, "SliceThickness", recordRepeat, groupName + "ImgSliceThicknessVal");

        addCSVEntry(jsonData, "SpacingBetweenSlices", recordRepeat, groupName + "ImgGapBetwnSlicesMeasr");
    }

    private void readGenericFieldsFromJson(HashMap<String, String> extractedFields, JSONObject jsonData) {
//      "Manufacturer": "Siemens",
//      "ManufacturersModelName": "Skyra",
//      "SoftwareVersions": "syngo_MR_E11",
//      "SliceThickness": 1,

//      "SpacingBetweenSlices": 2,

        String groupName = "Image Information.";

        addExtractedField(jsonData, "BodyPartExamined", extractedFields, groupName + "ImgAntmicSite");

        addExtractedField(jsonData, "Manufacturer", extractedFields, groupName + "ImgScannerManufName");

        addExtractedField(jsonData, "ManufacturersModelName", extractedFields, groupName + "ImgScannerModelName");

        addExtractedField(jsonData, "SoftwareVersions", extractedFields, groupName + "ImgScannerSftwrVrsnNum");

        String jsonContents = jsonData.toString();

        addExtractedField(extractedFields, groupName + "ImgNotesTxt", jsonContents);

        groupName = "Image pixel information and dimensions.";

        addExtractedField(jsonData, "SliceThickness", extractedFields, groupName + "ImgSliceThicknessVal");

        addExtractedField(jsonData, "SpacingBetweenSlices", extractedFields, groupName + "ImgGapBetwnSlicesMeasr");
    }

    private void readMRFieldsFromJson(ArrayList<String> recordRepeat, JSONObject jsonData, String imgFileName) {
//            "MagneticFieldStrength": 3,
//            "SeriesDescription": "SAG_3D_T2_FLAIR",
//            "ProtocolName": "SAG_3D_T2_FLAIR",
//            "EchoTime": 0.388,
//            "RepetitionTime": 5,
//            "InversionTime": 1.8,
//            "FlipAngle": 120,
//            "PartialFourier": 1,
//            "BaseResolution": 256,
//            "PhaseResolution": 1,
//            "ReceiveCoilName": "HeadNeck_20",
//            "ReceiveCoilActiveElements": "HE1-4;NE1,2",
//            "PercentPhaseFOV": 100,
//            "EchoTrainLength": 251,
//            "PhaseEncodingSteps": 229,
//            "AcquisitionMatrixPE": 256,
//            "ReconMatrixPE": 256,
//            "ParallelReductionFactorInPlane": 2,
//            "PixelBandwidth": 750,
//            "InPlanePhaseEncodingDirectionDICOM": "ROW",

//        "EffectiveEchoSpacing": 0.00047001,

        String groupName = "Magnetic Resonance Information.";

        // TODO BIDS now has PulseSequenceType, but it probably requires mapping

        String pulseSeq = getJsonString(jsonData, "ImgPulseSeqTyp");
        if ( !isValueSet(pulseSeq)) {
            // guess value based on the image file name
            pulseSeq = guessPulseSeqFromFileName(imgFileName);
        }

        // set either through json value or file name guessing
        if (isValueSet(pulseSeq)) {
            csvFieldNames.add(groupName + "ImgPulseSeqTyp");
            recordRepeat.add(pulseSeq);
        }

        csvFieldNames.add(groupName + "ImgScannerStrgthVal");
        recordRepeat.add(convertMagFieldStrengthToBRICS(getJsonString(jsonData, "MagneticFieldStrength")));

        // convert all times from sec to millisec?
        addCSVEntryMillisec(jsonData, "EchoTime", recordRepeat, groupName + "ImgEchoDur");

        addCSVEntryMillisec(jsonData, "RepetitionTime", recordRepeat, groupName + "ImgRepetitionGapVal");

        addCSVEntryMillisec(jsonData, "InversionTime", recordRepeat, groupName + "ImgInversionTime");

        addCSVEntry(jsonData, "FlipAngle", recordRepeat, groupName + "ImgFlipAngleMeasr");

        String partialFourier = getJsonString(jsonData, "PartialFourier");
        if (partialFourier != null && !partialFourier.equals("")) {
            csvFieldNames.add(groupName + "ImgPhasPartialFourierInd");
            recordRepeat.add("Yes");

            csvFieldNames.add(groupName + "ImgPhasPartialFourierVal");
            recordRepeat.add(partialFourier);
        }

        addCSVEntry(jsonData, "BaseResolution", recordRepeat, groupName + "ImgBaseResolutionVal");

        addCSVEntry(jsonData, "PercentPhaseFOV", recordRepeat, groupName + "ImgPhasResltnPercentVal");

        addCSVEntryMillisec(jsonData, "EffectiveEchoSpacing", recordRepeat, groupName + "ImgEchoSpcVal");

        addCSVEntry(jsonData, "EchoTrainLength", recordRepeat, groupName + "ImgEchoTrainLngthMeasr");

        addCSVEntry(jsonData, "InPlanePhaseEncodingDirectionDICOM", recordRepeat, groupName + "ImgPhasEncdeDirctTxt");

        // TODO - ?
//        addCSVEntry(jsonData, "PhaseEncodingDirection", recordRepeat, groupName + "");

        groupName = "Magnetic Resonance RF Coil.";

        addCSVEntry(jsonData, "ReceiveCoilName", recordRepeat, groupName + "ImgRFCoilName");
    }

    private void readMRFieldsFromJson(HashMap<String, String> extractedFields, JSONObject jsonData, String imgFileName) {
//      "MagneticFieldStrength": 3,
//      "SeriesDescription": "SAG_3D_T2_FLAIR",
//      "ProtocolName": "SAG_3D_T2_FLAIR",
//      "EchoTime": 0.388,
//      "RepetitionTime": 5,
//      "InversionTime": 1.8,
//      "FlipAngle": 120,
//      "PartialFourier": 1,
//      "BaseResolution": 256,
//      "PhaseResolution": 1,
//      "ReceiveCoilName": "HeadNeck_20",
//      "ReceiveCoilActiveElements": "HE1-4;NE1,2",
//      "PercentPhaseFOV": 100,
//      "EchoTrainLength": 251,
//      "PhaseEncodingSteps": 229,
//      "AcquisitionMatrixPE": 256,
//      "ReconMatrixPE": 256,
//      "ParallelReductionFactorInPlane": 2,
//      "PixelBandwidth": 750,
//      "InPlanePhaseEncodingDirectionDICOM": "ROW",

//  "EffectiveEchoSpacing": 0.00047001,

        String groupName = "Magnetic Resonance Information.";

        // TODO BIDS now has PulseSequenceType, but it probably requires mapping

        String pulseSeq = getJsonString(jsonData, "ImgPulseSeqTyp");
        if ( !isValueSet(pulseSeq)) {
            // guess value based on the image file name
            pulseSeq = guessPulseSeqFromFileName(imgFileName);
        }

        // set either through json value or file name guessing
        if (isValueSet(pulseSeq)) {
            extractedFields.put(groupName + "ImgPulseSeqTyp", pulseSeq);
        }

        extractedFields.put(groupName + "ImgScannerStrgthVal", convertMagFieldStrengthToBRICS(getJsonString(jsonData, "MagneticFieldStrength")));

        // convert all times from sec to millisec?
        addExtractedFieldMillisec(jsonData, "EchoTime", extractedFields, groupName + "ImgEchoDur");

        addExtractedFieldMillisec(jsonData, "RepetitionTime", extractedFields, groupName + "ImgRepetitionGapVal");

        addExtractedFieldMillisec(jsonData, "InversionTime", extractedFields, groupName + "ImgInversionTime");

        addExtractedField(jsonData, "FlipAngle", extractedFields, groupName + "ImgFlipAngleMeasr");

        String partialFourier = getJsonString(jsonData, "PartialFourier");
        if (partialFourier != null && !partialFourier.equals("")) {
            extractedFields.put(groupName + "ImgPhasPartialFourierInd", "Yes");

            extractedFields.put(groupName + "ImgPhasPartialFourierVal", partialFourier);
        }

        addExtractedField(jsonData, "BaseResolution", extractedFields, groupName + "ImgBaseResolutionVal");

        addExtractedField(jsonData, "PercentPhaseFOV", extractedFields, groupName + "ImgPhasResltnPercentVal");

        addExtractedFieldMillisec(jsonData, "EffectiveEchoSpacing", extractedFields, groupName + "ImgEchoSpcVal");

        addExtractedField(jsonData, "EchoTrainLength", extractedFields, groupName + "ImgEchoTrainLngthMeasr");

        addExtractedField(jsonData, "InPlanePhaseEncodingDirectionDICOM", extractedFields, groupName + "ImgPhasEncdeDirctTxt");

        // TODO - ?
        // addCSVEntry(jsonData, "PhaseEncodingDirection", extractedFields, groupName + "");

        groupName = "Magnetic Resonance RF Coil.";

        addExtractedField(jsonData, "ReceiveCoilName", extractedFields, groupName + "ImgRFCoilName");
    }

    private void readDwiFieldsFromJson(ArrayList<String> recordRepeat, JSONObject jsonData) {
        // nothing to extract right now
    }

    private void readDwiFieldsFromJson(HashMap<String, String> extractedFields, JSONObject jsonData) {
        // nothing to extract right now
    }

    private void setBvalBvecFields(ArrayList<String> recordRepeat, Vector<File> fileList) {
        String groupName = IMG_DIFF_GROUP + ".";

        File bvalFile = null;
        File bvecFile = null;

        Vector<File> otherFiles = new Vector<File>();
        for (File file : fileList) {
            if (file.getName().endsWith(".bval")) {
                bvalFile = file;
            } else if (file.getName().endsWith(".bvec")) {
                bvecFile = file;
            } else {
                otherFiles.add(file);
            }
        }

        if (bvalFile != null) {
            HashMap<String, String> deTable = readBvalFile(bvalFile);
            for (String deName : deTable.keySet()) {
                if (isValueSet(deName) && isValueSet(deTable.get(deName))) {
                    addCSVEntry(recordRepeat, deName, deTable.get(deName));
                }
            }

            addCSVEntry(recordRepeat, groupName + IMG_DIFF_BVAL_ELEMENT_NAME, bvalFile.getAbsolutePath());
        }

        if (bvecFile != null) {
            addCSVEntry(recordRepeat, groupName + IMG_DIFF_BVEC_ELEMENT_NAME, bvalFile.getAbsolutePath());
        }

        // TODO - need to handle any additional files and put into Diffusion Derived Data.ImgFile?
        for (File file : otherFiles) {
            System.err.println("DWI other file:\t" + file);
        }
    }

    private HashMap<String, String> readBvalFile(File bvalFile) {
        String groupName = IMG_DIFF_GROUP + ".";

        if (bvalFile == null) {
            return null;
        }

        int numDirections = 0;
        HashMap<Double, Integer> bvalList = new HashMap<Double, Integer>();

        HashMap<String, String> deTable = new HashMap<String, String>();

        if (bvalFile != null) {
            FileReader reader = null;
            try {
                reader = new FileReader(bvalFile);
                BufferedReader bufferedReader = new BufferedReader(reader);
                String line;
                while ( (line = bufferedReader.readLine()) != null) {
                    String[] split = line.split(" ");
                    for (String valStr : split) {
                        Double val = new Double(valStr);

                        // only non-zero bvals be counted as part of directions
                        if ( !val.equals(new Double(0))) {
                            numDirections++;
                        }
                        if (bvalList.containsKey(val)) {
                            bvalList.replace(val, bvalList.get(val) + 1);
                        } else {
                            bvalList.put(val, 1);
                        }
                    }
                }

                reader.close();
            } catch (IOException e) {
                e.printStackTrace();

                if (reader != null) {
                    try {
                        reader.close();
                    } catch (IOException e1) {}
                }

                return null;
            }

            deTable.put(groupName + "ImgDiffusionDirCt", "" + numDirections);

            int bvalCount = 0;
            Double[] bvals = (Double[]) bvalList.keySet().toArray(new Double[0]);
            Arrays.sort(bvals);
            for (int i = 0; i < bvals.length; i++) {
                if (bvals[i] != 0) {
                    bvalCount++;

                    if (bvalCount == 1) {
                        deTable.put(groupName + "ImgDiffusionFirstBVal", "" + bvals[i]);
                    } else if (bvalCount == 2) {
                        deTable.put(groupName + "ImgDiffusionSecondBVal", "" + bvals[i]);
                    } else if (bvalCount == 3) {
                        deTable.put(groupName + "ImgDiffusionThirdBVal", "" + bvals[i]);
                    } else if (bvalCount == 4) {
                        deTable.put(groupName + "ImgDiffusionFourthBVal", "" + bvals[i]);
                    } else if (bvalCount == 5) {
                        deTable.put(groupName + "ImgDiffusionFifthBVal", "" + bvals[i]);
                    } else if (bvalCount == 6) {
                        deTable.put(groupName + "ImgDiffusionSixthBVal", "" + bvals[i]);
                    } else {
                        System.err.println("Found more than 6 bVals: " + bvalCount + " " + bvals[i]);
                    }
                }
            }
            deTable.put(groupName + "ImgDiffusionBValCt", "" + bvalCount);
        }

        return deTable;
    }

    private void readFuncFieldsFromJson(ArrayList<String> recordRepeat, JSONObject jsonData) {
//        "TaskName":"Resting State",
//        "SliceTiming": [
//                        0,
//                        1.275,
//                        0.0675,
//                        1.3425,
//                        0.135,
//                        1.4075,
//                        0.2025,
//                        1.475,
//                        0.27,
//                        1.5425,
//                        0.335,
//                        1.61,
//                        0.4025,
//                        1.6775,
//                        0.47,
//                        1.7425,
//                        0.5375,
//                        1.81,
//                        0.605,
//                        1.8775,
//                        0.6725,
//                        1.945,
//                        0.7375,
//                        2.0125,
//                        0.805,
//                        2.0775,
//                        0.8725,
//                        2.145,
//                        0.94,
//                        2.2125,
//                        1.0075,
//                        2.28,
//                        1.0725,
//                        2.3475,
//                        1.14,
//                        2.415,
//                        1.2075  ],

        String groupName = "fMRI Information.";

        String taskType = mapTaskNameBIDS(getJsonString(jsonData, "TaskName"));

        addCSVEntry(recordRepeat, groupName + "ImgFMRITaskTyp", taskType);
    }

    private void readFuncFieldsFromJson(HashMap<String, String> extractedFields, JSONObject jsonData) {
//      "TaskName":"Resting State",
//      "SliceTiming": [
//                      0,
//                      1.275,
//                      0.0675,
//                      1.3425,
//                      0.135,
//                      1.4075,
//                      0.2025,
//                      1.475,
//                      0.27,
//                      1.5425,
//                      0.335,
//                      1.61,
//                      0.4025,
//                      1.6775,
//                      0.47,
//                      1.7425,
//                      0.5375,
//                      1.81,
//                      0.605,
//                      1.8775,
//                      0.6725,
//                      1.945,
//                      0.7375,
//                      2.0125,
//                      0.805,
//                      2.0775,
//                      0.8725,
//                      2.145,
//                      0.94,
//                      2.2125,
//                      1.0075,
//                      2.28,
//                      1.0725,
//                      2.3475,
//                      1.14,
//                      2.415,
//                      1.2075  ],

        String groupName = "fMRI Information.";

        String taskType = mapTaskNameBIDS(getJsonString(jsonData, "TaskName"));

        addExtractedField(extractedFields, groupName + "ImgFMRITaskTyp", taskType);
    }

    private String mapTaskNameBIDS(String taskName) {
        if (taskName == null || taskName.equals("")) {
            return null;
        }

//        Affective Stroop
//        Autobiographical Memory
//        Classical Conditioning
//        Finger tapping
//        Go/No-Go
//        N-back
//        Orienting (auditory)
//        Orienting (visual)
//        Real Time NeuroFeedback
//        Rest
//        Retinotopy
//        Stop-Signal
//        Visual Attention
//        Visual Attention/Inhibition of Return

        String lowerTaskName = taskName.toLowerCase();

        if (lowerTaskName.contains("n back") || lowerTaskName.contains("n-back")) {
            return "N-back";
        } else if (lowerTaskName.startsWith("rest")) {
            return "Rest";
        } else {
            return taskName;
        }
    }

    private void setAdditionalFuncFiles(ArrayList<ArrayList<String>> record, Vector<File> files) {
        // if no files, exit
        if (files.size() == 0) {
            return;
        }

        String groupName = "fMRI Auxiliary Files.";
        String auxFileDeName = "ImgFMRIAuxFile";

        csvFieldNames.add(groupName + auxFileDeName);

        int curRowIndex = 0;
        for (File file : files) {
            // TODO: try to determine the ImgFMRIAuxFileTyp from the file name

            // if the row already exists in the record, add the file path to the end
            if (record.get(curRowIndex) != null) {
                record.get(curRowIndex).add(file.getAbsolutePath());
            } else {
                // no row already in the record, so create a new one
                // add blanks for the rest of the fields in the repeat until we get to the aux file column/element
                ArrayList<String> repeatRow = new ArrayList<String>(csvFieldNames.size() + 1);
                for (int i = 0; i < csvFieldNames.size(); i++) {
                    repeatRow.add("");
                }

                repeatRow.add(file.getAbsolutePath());
                record.add(repeatRow);
            }

            curRowIndex++;
        }
    }

    private boolean addCSVEntry(ArrayList<String> recordRepeat, String bricsCsvFieldName, String value) {
        if (value != null && !value.equals("")) {
            csvFieldNames.add(bricsCsvFieldName);
            recordRepeat.add(value);
            return true;
        }

        return false;
    }

    private boolean addCSVEntry(JSONObject jsonData, String jsonFieldName, ArrayList<String> recordRepeat, String bricsCsvFieldName) {
        String val = getJsonString(jsonData, jsonFieldName);
        if (val != null && !val.equals("")) {
            csvFieldNames.add(bricsCsvFieldName);
            recordRepeat.add(val);
            return true;
        }

        return false;
    }

    private boolean addCSVEntryMillisec(JSONObject jsonData, String jsonFieldName, ArrayList<String> recordRepeat, String bricsCsvFieldName) {
        double val = getJsonDouble(jsonData, jsonFieldName);
        if ( !Double.isNaN(val)) {
            csvFieldNames.add(bricsCsvFieldName);
            recordRepeat.add("" + (1000.0 * val));
            return true;
        }

        return false;
    }

    private boolean addExtractedField(HashMap<String, String> extractedFields, String bricsCsvFieldName, String value) {
        if (value != null && !value.equals("")) {
            extractedFields.put(bricsCsvFieldName, value);
            return true;
        }

        return false;
    }

    private boolean addExtractedField(JSONObject jsonData, String jsonFieldName, HashMap<String, String> extractedFields, String bricsCsvFieldName) {
        String val = getJsonString(jsonData, jsonFieldName);
        if (val != null && !val.equals("")) {
            extractedFields.put(bricsCsvFieldName, val);
            return true;
        }

        return false;
    }

    private boolean addExtractedFieldMillisec(JSONObject jsonData, String jsonFieldName, HashMap<String, String> extractedFields, String bricsCsvFieldName) {
        double val = getJsonDouble(jsonData, jsonFieldName);
        if ( !Double.isNaN(val)) {
            extractedFields.put(bricsCsvFieldName, "" + (1000.0 * val));
            return true;
        }

        return false;
    }

    private double getJsonDouble(JSONObject jsonData, String fieldName) {
        double val = Double.NaN;

        try {
            val = jsonData.getDouble(fieldName);
        } catch (final JSONException e) {
            // System.err.println("JSONException retrieving (" + fieldName + ") " + e);
        }

        return val;
    }

    private String getJsonString(JSONObject jsonData, String fieldName) {
        String val = null;

        try {
            val = jsonData.getString(fieldName);
        } catch (final JSONException e) {
            // System.err.println("JSONException retrieving (" + fieldName + ") " + e);
        }

        return val;
    }

    private String getFileAsString(final File file) {
        String fileContents = null;
        RandomAccessFile raFile = null;
        long fileLength;
        byte[] bufferByte;

        try {
            raFile = new RandomAccessFile(file, "r");
        } catch (final FileNotFoundException e) {
            System.err.println("FileNotFoundException " + e);
            e.printStackTrace();
            return null;
        }

        try {
            fileLength = raFile.length();
        } catch (final IOException e) {
            System.err.println("IOException " + e);
            e.printStackTrace();
            try {
                if (raFile != null) {
                    raFile.close();
                }
            } catch (final IOException closeE) {}
            return null;
        }

        bufferByte = new byte[(int) fileLength];
        try {
            raFile.read(bufferByte);
        } catch (final IOException e) {
            System.err.println("IOException " + e);
            e.printStackTrace();
            try {
                if (raFile != null) {
                    raFile.close();
                }
            } catch (final IOException closeE) {}
            return null;
        }

        fileContents = new String(bufferByte, 0, (int) fileLength).trim();

        try {
            raFile.close();
        } catch (final IOException e) {
            System.err.println(" IOException " + e + " on raFile.close()");
            e.printStackTrace();
            try {
                if (raFile != null) {
                    raFile.close();
                }
            } catch (final IOException closeE) {}
            return null;
        }

        return fileContents;
    }
    
    private String getFileAsString(final InputStream stream) {
        String fileContents = null;
        
        try {
            fileContents = IOUtils.toString(stream);
        } catch (final IOException e) {
            System.err.println("Error reading JSON stream");
            e.printStackTrace();
            return null;
        }

        if (fileContents == null) {
            return null;
        }
        
        return fileContents.trim();
    }

    private JSONObject readJsonFile(final File jsonFile) {
        JSONObject jsonObject = null;

        String fileContents = getFileAsString(jsonFile);

        if ( !fileContents.startsWith("{") || !fileContents.endsWith("}")) {
            return null;
        }

        try {
            jsonObject = new JSONObject(fileContents);
        } catch (final JSONException e) {
            System.err.println("JSONException " + e + " on new JSONObject(jsonString)");
            e.printStackTrace();
            return null;
        }

        return jsonObject;
    }
    
    private JSONObject readJsonFile(final InputStream inputStream) {
        JSONObject jsonObject = null;

        String fileContents = getFileAsString(inputStream);

        if ( !fileContents.startsWith("{") || !fileContents.endsWith("}")) {
            return null;
        }

        try {
            jsonObject = new JSONObject(fileContents);
        } catch (final JSONException e) {
            System.err.println("JSONException " + e + " on new JSONObject(jsonString)");
            e.printStackTrace();
            return null;
        }

        return jsonObject;
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

    private boolean readCSVFile() {
        BufferedReader br = null;
        FileInputStream fis = null;

        try {
            String str;
            fis = new FileInputStream(csvFile);
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

            FormStructure dataStructure = null;
            for (final FormStructure ds : dataStructureList) {
                if (ds.getShortName().equalsIgnoreCase(dsName)) {
                    if (ds.getDataElements().size() == 0) {
                        final FormDataElementsRESTThread thread = new FormDataElementsRESTThread(this, dsName, true);
                        thread.run();

                        dataStructure = thread.getFullFormStructure();
                    } else {
                        dataStructure = ds;
                    }
                }
            }

            csvFieldNames = new ArrayList<String>(csvFieldNamesWithRecord.length - 1);
            int recordFieldIndex = -1;
            for (int i = 0; i < csvFieldNamesWithRecord.length; i++) {
                if (csvFieldNamesWithRecord[i].equalsIgnoreCase(recordIndicatorColumn) || csvFieldNamesWithRecord[i].equalsIgnoreCase("\"" + recordIndicatorColumn + "\"")) {
                    recordFieldIndex = i;
                } else {
                    // don't add fields without a name (error in the middle of the CSV, ignore at the end)
                    if ( !csvFieldNamesWithRecord[i].trim().equals("")) {
                        String csvField = csvFieldNamesWithRecord[i].trim().replaceAll("^\"|\"$", "");

                        // if the names are surrounded by quotes, remove them before adding.
                        csvFieldNames.add(csvField);

                        // check that the field name matches one of the group/de combos from the form structure
                        final String[] deGroupAndName = splitFieldString(csvField);

                        boolean foundField = false;

                        // all data elements belong to a group. if there is no group, this is always an error
                        if (deGroupAndName.length == 2) {
                            DE_SEARCH: for (RepeatableGroup group : dataStructure.getRepeatableGroups()) {
                                if (group.getName().equalsIgnoreCase(deGroupAndName[0])) {
                                    for (MapElement de : group.getDataElements()) {
                                        if (de.getStructuralDataElement().getName().equalsIgnoreCase(deGroupAndName[1])) {
                                            foundField = true;
                                            break DE_SEARCH;
                                        }
                                    }
                                }
                            }

                            if ( !foundField) {
                                MipavUtil.displayError("Unrecogzied data element in CSV: " + csvField + ".  Check that all group and data element names match those in " + dsName);
                                return false;
                            }
                        } else {
                            MipavUtil.displayError(
                                    "Data element without a specified group in CSV: " + csvField + ".  Check " + dsName + " in the Data Dictionary to find the appropriate group.");
                            return false;
                        }
                    } else {
                        // ignore if no more real field names (and no data values for the column). otherwise show error
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
            int recordNum = 1;
            int csvExtraRows = 2;
            Vector<Vector<FileDicomTag>> csvProblemTagList = new Vector<Vector<FileDicomTag>>(recordList.size());
            Vector<String> csvProblemFileDirList = new Vector<String>(recordList.size());
            Vector<String> csvProblemFileNameList = new Vector<String>(recordList.size());
            for (final ArrayList<ArrayList<String>> record : recordList) {
                progressBar.setMessage("Reading CSV record " + recordNum + " of " + recordList.size() + " (row " + (recordNum + csvExtraRows) + ")");
                System.out.println("Reading CSV record " + recordNum + " of " + recordList.size() + " (row " + (recordNum + csvExtraRows) + ")");

                if (readErrorFileOut != null) {
                    readErrorFileOut.println("Reading CSV record " + recordNum + " of " + recordList.size() + " (row " + (recordNum + csvExtraRows) + ")");
                }

                InfoDialog csvDialog = new InfoDialog(this, dsName, false, false, record);
                if (progressInc > 0) {
                    progressBar.updateValue(progressBar.getValue() + progressInc);
                } else if ( (recordNum % rowsPerInc) == 0) {
                    progressBar.updateValue(progressBar.getValue() + 1);
                }

                // change i counter to 0-based for problem lists
                csvProblemTagList.add(recordNum - 1, csvDialog.getProblemTags());
                csvProblemFileDirList.add(recordNum - 1, csvDialog.getProblemFileDir());
                csvProblemFileNameList.add(recordNum - 1, csvDialog.getProblemFileName());

                recordNum++;
                csvExtraRows += (record.size() - 1);
            }
            final long csvReadEndTime = System.currentTimeMillis();
            System.out.println("CSV input read took " + ( (csvReadEndTime - csvReadStartTime) / 1000) + " seconds (" + recordList.size() + " records)");
            System.out.println();
            
            csvRecordCount = recordList.size();

            for (int j = 0; j < csvProblemTagList.size(); j++) {
                final Vector<FileDicomTag> problemTags = csvProblemTagList.get(j);
                if (problemTags != null && !cmdLineCsvFlag) {
                    boolean isDeidentified = deidentificationDialogDicom(csvProblemFileDirList.get(j), csvProblemFileNameList.get(j), problemTags, true);

                    // if the user certified that all data is okay, stop checking
                    if (csvDeidentDontAsk) {
                        break;
                    }

                    if ( !isDeidentified) {
                        // should have already exited
                        continue;
                    }
                } else if (problemTags != null && problemTags.size() > 0) {
                    // when running from the cmd line, still make note of problematic tags in output
                    System.err.println("Tags potentially containing PII/PHI found (" + csvProblemFileDirList.get(j) + File.separator + csvProblemFileNameList.get(j) + ":");
                    piiWarningFileOut.println("Tags potentially containing PII/PHI found (" + csvProblemFileDirList.get(j) + File.separator + csvProblemFileNameList.get(j) + ":");
                    System.err.format("%-14s%-40s%s%n", "DICOM tag", "Name", "Value");
                    piiWarningFileOut.format("%-14s%-40s%s%n", "DICOM tag", "Name", "Value");
                    for (FileDicomTag tag : problemTags) {
                        System.err.format("%-14s%-40s%s%n", tag.getKey().toString(), tag.getName(), (String) tag.getValue(true));
                    }
                    for (FileDicomTag tag : problemTags) {
                        piiWarningFileOut.format("%-14s%-40s%s%n", tag.getKey().toString(), tag.getName(), (String) tag.getValue(true));
                    }
                }
            }

            progressBar.dispose();

        } catch (final Exception e) {
            e.printStackTrace();
            return false;
        } finally {
            if (fis != null) {
                try {
                    fis.close();
                } catch (IOException e) {
                    System.err.println("Problem closing CSV file handle.");
                    e.printStackTrace();
                }
            }

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
        setTitle("Image Submission Package Creation Tool - " + pluginVersion + " (" + getDisplayStrFromBricsInstance(selectedDictionaryInstance) + " - "+ selectedDictionaryEnv + ")");

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
        outputDirLabel.setFont(serif12B);
        outputDirTextField = new JTextField(30);
        outputDirTextField.setEditable(false);
        outputDirTextField.setToolTipText(outputDirBase);
        outputDirTextField.setText(outputDirBase);
        outputDirTextField.setFont(serif12);

        outputDirButton = new JButton("Browse");
        outputDirButton.addActionListener(this);
        outputDirButton.setToolTipText("Choose Output Directory for Validation Tool files");
        outputDirButton.setActionCommand("OutputDirBrowse");
        outputDirButton.setPreferredSize(MipavUtil.defaultButtonSize);
        outputDirButton.setFont(serif12B);
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
        structTable.setFont(serif12);
        structTable.getTableHeader().setFont(serif12B);

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
            final AlgorithmTransform transformer = new AlgorithmTransform(origImage, percentSizer, 1, (float) (origImage.getResolutions(0)[0] / (percentage * .01)),
                    (float) (origImage.getResolutions(0)[1] / (percentage * .01)), (int) (origImage.getExtents()[0] * percentage * .01),
                    (int) (origImage.getExtents()[1] * percentage * .01), origImage.getUnitsOfMeasure(), false, true, false, true, origImage.getImageCentermm(false));
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
                lightGen = new LightboxGenerator(origImage, startSlice, endSlice, percentage, rows, columns, rBorderVal, gBorderVal, bBorderVal, false, borderThick);
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
                lightGen = new LightboxGenerator(timeImage, startSlice, endSlice, percentage, rows, columns, rBorderVal, gBorderVal, bBorderVal, false, borderThick);
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

        final ViewJProgressBar progressBar = new ViewJProgressBar("Generating output package", "Creating output CSV file(s)", 0, 100, false);
        progressBar.setVisible(true);
        progressBar.updateValue(5);

        final File outputDirFile = new File(outputDirBase);
        if ( !outputDirFile.exists()) {
            outputDirFile.mkdirs();
        }

        final int numDataStructs = structTableModel.getRowCount();

        if (numDataStructs == 0) {
            MipavUtil.displayError("No images records are ready for package generation.  If you loaded an input CSV, check any error output/logs for image read errors.");
            return;
        }

        final int progressInc = 95 / numDataStructs;
        final int rowsPerInc = (numDataStructs / 95) + 1;

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
                final ArrayList<RepeatableGroup> orderedGroupList = new ArrayList<RepeatableGroup>(dsInfo.getRepeatableGroups());
                Collections.sort(orderedGroupList, groupCompare);

                final ArrayList<ArrayList<MapElement>> orderedElementListsByGroup = new ArrayList<ArrayList<MapElement>>();
                for (final RepeatableGroup g : orderedGroupList) {
                    final ArrayList<MapElement> elemList = new ArrayList<MapElement>(g.getDataElements());
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
            final String structSubDirName = structOutputBaseNameList.get(dsName.toLowerCase());
            final String outputSubDirFull = outputDirBase + File.separator + structSubDirName + File.separator;
            final File outputSubDirFile = new File(outputSubDirFull);
            if ( !outputSubDirFile.exists()) {
                outputSubDirFile.mkdirs();
            }

            progressBar.setMessage("Creating submission files for record #" + (i + 1));
            if (progressInc > 0) {
                progressBar.updateValue(progressBar.getValue() + progressInc);
            } else if ( (i % rowsPerInc) == 0) {
                progressBar.updateValue(progressBar.getValue() + 1);
            }

            final ImgFileInfo imgFileInfo = structRowImgFileInfoList.get(i);

            if (isImagingStructure(dsName) && imgFileInfo != null) {

                // this means we are working with the image datastructure
                printlnToLog("Creating submission file for record #" + (i + 1) + ": " + name);

                // printlnToLog("Opening: " + imageFile + ", multifile: " + multifiles.get(i));

                final List<String> origFiles = imgFileInfo.getOrigFiles();

                if (imgFileInfo.getThumbnailImgData() != null) {
                    printlnToLog("Creating thumbnail image:\t" + outputSubDirFull + outputFileNameBase + ".jpg");
                    progressBar.setMessage("Creating thumbnail for record #" + (i + 1));

                    MemoryImageSource thumbnailImageData = getThumbnailSource(imgFileInfo.getThumbnailImgData());

                    if (thumbnailImageData != null) {
                        final FileWriteOptions opts = new FileWriteOptions(outputFileNameBase + ".jpg", outputSubDirFull, true);
                        writeThumbnailJIMI(thumbnailImageData, opts);
                    }
                }

                String imagePath = null;
                String imageFileName = null;
                if ( (imgFileInfo.getImgFilePath().endsWith(".zip") && new File(imgFileInfo.getImgFilePath()).exists())) {
                    // the files were already zipped - so don't zip again. copy to output dir
                    try {
                        final File srcFile = new File(imgFileInfo.getImgFilePath());
                        final File destFile = new File(outputSubDirFull + outputFileNameBase + "_" + srcFile.getName());
                        printlnToLog("Copying original image zip file into output directory:\t" + destFile.getAbsolutePath());
                        progressBar.setMessage("Copying zip file for record #" + (i + 1));
                        FileUtils.copyFile(srcFile, destFile);
                        //imagePath = destFile.getAbsolutePath();
                        imagePath = structSubDirName + File.separator + destFile.getName();
                        imageFileName = destFile.getName();
                    } catch (final IOException e) {
                        MipavUtil.displayError("Unable to copy image zip file into output directory");
                        e.printStackTrace();
                    }
                } else if ( ( (imgFileInfo.getImgFilePath().endsWith(".tar.gz") || imgFileInfo.getImgFilePath().endsWith(".tgz"))
                        && new File(imgFileInfo.getImgFilePath()).exists())) {
                    // the files were already tarballed - so don't zip again. copy to output dir
                    try {
                        final File srcFile = new File(imgFileInfo.getImgFilePath());
                        final File destFile = new File(outputSubDirFull + outputFileNameBase + "_" + srcFile.getName());
                        printlnToLog("Copying original image tarball file into output directory:\t" + destFile.getAbsolutePath());
                        progressBar.setMessage("Copying tarball file for record #" + (i + 1));
                        FileUtils.copyFile(srcFile, destFile);
                        //imagePath = destFile.getAbsolutePath();
                        imagePath = structSubDirName + File.separator + destFile.getName();
                        imageFileName = destFile.getName();
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

                            destFile = new File(outputSubDirFull + outputFileNameBase + "_" + srcFile.getName() + ext);
                        } else {
                            destFile = new File(outputSubDirFull + outputFileNameBase + "_" + srcFile.getName());
                        }

                        printlnToLog("Copying original image file into output directory:\t" + destFile.getAbsolutePath());
                        progressBar.setMessage("Copying image file for record #" + (i + 1));
                        FileUtils.copyFile(srcFile, destFile);
                        //imagePath = destFile.getAbsolutePath();
                        imagePath = structSubDirName + File.separator + destFile.getName();
                        imageFileName = destFile.getName();
                    } catch (final IOException e) {
                        MipavUtil.displayError("Unable to copy original image file into output directory");
                        e.printStackTrace();
                    }
                } else {
                    // need to create a zip file with the original image files
                    try {
                        String zipFilePath = outputSubDirFull + outputFileNameBase + ".zip";

                        // if MR, try to put the pulse sequence type into the zip name
                        if (isMRImagingStructure(dsName)) {
                            String pulseSeq = "";
                            boolean found = false;
                            final FormStructureData fsData = fsDataList.get(i);
                            for (final RepeatableGroup group : fsData.getStructInfo().getRepeatableGroups()) {
                                if (found) {
                                    break;
                                }

                                for (final GroupRepeat repeat : fsData.getAllGroupRepeats(group.getName())) {
                                    if (found) {
                                        break;
                                    }

                                    for (final DataElementValue deVal : repeat.getDataElements()) {
                                        if (isPulseSeqTag(deVal)) {
                                            pulseSeq = "_" + cleanPulseSeqVal(deVal.getValue());
                                            found = true;
                                            break;
                                        }
                                    }
                                }
                            }

                            zipFilePath = outputSubDirFull + outputFileNameBase + pulseSeq + ".zip";
                        }

                        printlnToLog("Creating ZIP file (" + origFiles.size() + " files):\t" + zipFilePath);
                        progressBar.setMessage("Creating ZIP file for record #" + (i + 1));
//                        for (final String file : origFiles) {
//                            printlnToLog("Adding file to ZIP:\t" + file);
//                        }

                        makeZipFile(zipFilePath, origFiles);
                        //imagePath = zipFilePath;
                        imagePath = structSubDirName + File.separator + (new File(zipFilePath)).getName();
                        imageFileName = (new File(zipFilePath)).getName();
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
                    hashCode = computeFileHash(outputSubDirFull + imageFileName);
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
                                if (imgFileInfo.getThumbnailImgData() != null) {
                                    deVal.setValue(structSubDirName + File.separator + outputFileNameBase + ".jpg");
                                } else {
                                    // if no preview is generated, set it to blank to avoid the "Automatically
                                    // generated..." message from being written - ie for spectroscopy
                                    deVal.setValue("");
                                }
                            } else if (deVal.getName().equalsIgnoreCase(IMG_HASH_CODE_ELEMENT_NAME) && hashCode != null) {
                                deVal.setValue(hashCode);
                            } else if ( (deVal.getDataElementInfo().getType() == DataType.FILE || deVal.getDataElementInfo().getType() == DataType.TRIPLANAR)
                                    && !deVal.getValue().equals("")) {
                                final String srcFileValue = deVal.getValue();

                                if (srcFileValue.contains(BROWSE_NONIMG_DELIM)) {
                                    // if the user selected multiple files, zip them all up
                                    try {
                                        final String[] filePaths = srcFileValue.split(BROWSE_NONIMG_DELIM);

                                        String newZipPath = outputSubDirFull + outputFileNameBase + "_" + group.getName() + "_" + repeat.getRepeatNumber() + "_" + deVal.getName()
                                                + ".zip";

                                        // if MR, try to put the pulse sequence type into the zip name
                                        if (isMRImagingStructure(dsName)) {
                                            String pulseSeq = "";
                                            for (final RepeatableGroup groupPulse : fsData.getStructInfo().getRepeatableGroups()) {
                                                for (final GroupRepeat repeatPulse : fsData.getAllGroupRepeats(groupPulse.getName())) {
                                                    for (final DataElementValue deValPulse : repeatPulse.getDataElements()) {
                                                        if (isPulseSeqTag(deValPulse)) {
                                                            pulseSeq = "_" + cleanPulseSeqVal(deValPulse.getValue());
                                                        }
                                                    }
                                                }
                                            }

                                            newZipPath = outputSubDirFull + outputFileNameBase + pulseSeq + ".zip";
                                        }

                                        final List<String> filePathList = new ArrayList<String>();
                                        for (final String file : filePaths) {
//                                            printlnToLog("Adding file to ZIP:\t" + file);
                                            filePathList.add(file);
                                        }
                                        printlnToLog("Creating ZIP of attached files for (" + filePathList.size() + " files):\t" + newZipPath);
                                        progressBar.setMessage("Creating ZIP file for record #" + (i + 1));
                                        makeZipFile(newZipPath, filePathList);

                                        // now that the zip file is created, set the de value to the zip file path
                                        //deVal.setValue(newZipPath);
                                        deVal.setValue(structSubDirName + File.separator + (new File(newZipPath)).getName());
                                    } catch (final IOException ioe) {
                                        ioe.printStackTrace();
                                        MipavUtil.displayError("Unable to write files to ZIP package:\n" + ioe.getMessage());
                                        continue;
                                    }
                                } else {
                                    // only one file selected; copy it
                                    final File srcFile = new File(srcFileValue);
                                    try {
                                        final File destFile = new File(outputSubDirFull + outputFileNameBase + "_" + srcFile.getName());
                                        printlnToLog("Copying attached file into output directory:\t" + destFile.getAbsolutePath());
                                        progressBar.setMessage("Copying attached file for record #" + (i + 1));
                                        FileUtils.copyFile(srcFile, destFile);
                                        //deVal.setValue(destFile.getAbsolutePath());
                                        deVal.setValue(structSubDirName + File.separator + destFile.getName());
                                    } catch (final IOException e) {
                                        MipavUtil.displayError("Unable to copy file into output directory");
                                        e.printStackTrace();
                                    }
                                }
                            } else if (deVal.getDataElementInfo().getType() == DataType.DATE && !deVal.getValue().equals("")) {
                                // try to convert any date values entered to match ISO standard
                                final String dateValue = deVal.getValue();
                                deVal.setValue(convertDateToISOFormat(dateValue));
                            }
                        }
                    }
                }

                for (int curRepeat = 0; curRepeat < maxRepeatNum; curRepeat++) {
                    final String newRow = getCSVDataRow(outputSubDirFull, outputFileNameBase, fsData, curRepeat);
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
                
                // TODO this needs testing (esp. around collision detection) and conversion to relative paths

                // this means that this is another data structure besides image

                printlnToLog("Creating submission file for " + name);
                progressBar.setMessage("Creating submission files for record #" + (i + 1));

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
                    final String dir = outputDirBase + File.separator + copyToImageThumbnailPath.substring(0, copyToImageThumbnailPath.lastIndexOf(File.separator));
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
                                progressBar.setMessage("Copying attached files for record #" + (i + 1));

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
                progressBar.setMessage("Writing CSV data for FormStructure: " + lowerName);

                final File csvFile = new File(outputDirBase + csvFileName);
                fw = new FileWriter(csvFile);
                bw = new BufferedWriter(fw);

                bw.write(csvStructRowData.get(lowerName));

                // System.out.println(csvStructRowData.get(lowerName) +
                // " |||| ");

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

        progressBar.dispose();

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

        final ArrayList<RepeatableGroup> orderedGroupList = new ArrayList<RepeatableGroup>(fsData.getStructInfo().getRepeatableGroups());
        Collections.sort(orderedGroupList, groupCompare);

        for (final RepeatableGroup group : orderedGroupList) {
            if (fsData.isGroupRepeatSet(group.getName(), repeatNum)) {
                final GroupRepeat repeat = fsData.getGroupRepeat(group.getName(), repeatNum);
                final Vector<DataElementValue> deList = repeat.getDataElements();
                Collections.sort(deList, dataElementCompare);

                for (final DataElementValue deVal : deList) {
                    // final String deName = deVal.getName();
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
        System.err.println(line);
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
        addStructButton.setFont(serif12B);
        addStructButton.addActionListener(this);
        addStructButton.setActionCommand("AddStruct");

        loadCSVButton = new JButton("Load CSV File");
        loadCSVButton.setToolTipText("Load CSV File");
        loadCSVButton.setFont(serif12B);
        loadCSVButton.addActionListener(this);
        loadCSVButton.setActionCommand("LoadCSV");

        selectBIDSButton = new JButton("Load BIDS Package");
        selectBIDSButton.setToolTipText("Select BIDS Root Directory");
        selectBIDSButton.setFont(serif12B);
        selectBIDSButton.addActionListener(this);
        selectBIDSButton.setActionCommand("SelectBIDS");

        removeStructButton = new JButton("Remove Form Structure");
        removeStructButton.setToolTipText("Remove the selected Form Structure");
        removeStructButton.setFont(serif12B);
        removeStructButton.addActionListener(this);
        removeStructButton.setActionCommand("RemoveStruct");

        finishButton = new JButton("Generate Files");
        finishButton.setToolTipText("Generate Files");
        finishButton.setFont(serif12B);
        finishButton.addActionListener(this);
        finishButton.setActionCommand("Finish");

        editDataElementsButton = new JButton("Edit Data Elements");
        editDataElementsButton.setToolTipText("Edit data elements for selected Form Structure");
        editDataElementsButton.setFont(serif12B);
        editDataElementsButton.addActionListener(this);
        editDataElementsButton.setActionCommand("EditDataElements");

//        selectBIDSButton.setPreferredSize(MipavUtil.defaultButtonSize);
//        addStructButton.setPreferredSize(MipavUtil.defaultButtonSize);
//        removeStructButton.setPreferredSize(MipavUtil.defaultButtonSize);
//        finishButton.setPreferredSize(MipavUtil.defaultButtonSize);
//        editDataElementsButton.setPreferredSize(MipavUtil.defaultButtonSize);

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
        } catch (final NoSuchMethodError e) {
            System.err.println("JIMI write failed. Switching to ImageIO writer.");
            // e.printStackTrace();

            BufferedImage bimg = toBufferedImage(createImage(imgData));
            try {
                if ( !ImageIO.write(bimg, "JPG", new File(name))) {
                    MipavUtil.displayError("Failed to write thumbnail image: " + name);
                }
            } catch (IOException e1) {
                e1.printStackTrace();
            }

            // e.printStackTrace();
            return false;
        } catch (final NoClassDefFoundError e) {
            System.err.println("JIMI write failed. Switching to ImageIO writer.");
            // e.printStackTrace();

            BufferedImage bimg = toBufferedImage(createImage(imgData));
            try {
                if ( !ImageIO.write(bimg, "JPG", new File(name))) {
                    MipavUtil.displayError("Failed to write thumbnail image: " + name);
                }
            } catch (IOException e1) {
                e1.printStackTrace();
            }

            // e.printStackTrace();
            return false;
        }

        return true;
    }

    /**
     * Converts a given Image into a BufferedImage
     *
     * @param img The Image to be converted
     * @return The converted BufferedImage
     */
    public static BufferedImage toBufferedImage(Image img) {
        if (img instanceof BufferedImage) {
            return (BufferedImage) img;
        }

        // Create a buffered image with transparency
        BufferedImage bimage = new BufferedImage(img.getWidth(null), img.getHeight(null), BufferedImage.TYPE_INT_RGB);

        // Draw the image on to the buffered image
        Graphics2D bGr = bimage.createGraphics();
        bGr.drawImage(img, 0, 0, null);
        bGr.dispose();

        // Return the buffered image
        return bimage;
    }

    /**
     * Creates a set of thumbnail image data from a lightbox ModelImage.
     * 
     * @param image The lightbox image to create an Image from.
     * 
     * @return The image data, ready for rendering/JIMI writing.
     */
    private ThumbnailData createThumbnailDataForWriting(final ModelImage image) {
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

        // return new MemoryImageSource(image.getExtents()[0], image.getExtents()[1], paintBuffer, 0,
        // image.getExtents()[0]);
        return new ThumbnailData(image.getExtents()[0], image.getExtents()[1], paintBuffer);
    }

    private MemoryImageSource getThumbnailSource(final ThumbnailData thumbData) {
        return new MemoryImageSource(thumbData.width, thumbData.height, thumbData.thumbnailBuffer, 0, thumbData.width);
    }

    private class ThumbnailData {
        public int width;

        public int height;

        public int[] thumbnailBuffer;

        public ThumbnailData(int w, int h, int[] buff) {
            width = w;
            height = h;
            thumbnailBuffer = buff;
        }
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

//    /**
//     * Tries to read server configuration from brics config file on local disk.
//     */
//    private void readConfig() {
//        final InputStream in = getClass().getResourceAsStream(configFileName);
//        if (in != null) {
//            final Properties prop = new Properties();
//            try {
//                prop.load(in);
//            } catch (final IOException e) {
//                Preferences.debug("Unable to load BRICS preferences file: " + configFileName + "\n", Preferences.DEBUG_MINOR);
//                e.printStackTrace();
//            }
//            // use pre-set, hardcoded values as defaults if properties are not found
//            ddEnvName = prop.getProperty(ddEnvNameProp, ddEnvName);
//            System.out.println("ddEnvName:\t" + ddEnvName);
//            authServerURL = prop.getProperty(authServerURLProp, authServerURL);
//            System.out.println("authServer:\t" + authServerURL);
//            ddServerURL = prop.getProperty(ddServerURLProp, ddServerURL);
//            System.out.println("ddServer:\t" + ddServerURL);
//            ddAuthUser = prop.getProperty(ddAuthUserProp, ddAuthUser);
//            System.out.println("ddAuthUser:\t" + ddAuthUser);
//            ddAuthPass = prop.getProperty(ddAuthPassProp, ddAuthPass);
//            System.out.println("ddAuthPass:\t" + ddAuthPass);
//            ddUseAuthService = Boolean.parseBoolean(prop.getProperty(ddUseAuthServiceProp, "" + ddUseAuthService));
//            System.out.println("ddUseAuthService:\t" + ddUseAuthService);
//        }
//    }

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
     * @param time A time string in the format hh:mm:ss or hhmmss.
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
            //final String frac = m.group(4);
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
//            if (frac.length() > 0) {
//                isoTime += "." + frac;
//            }
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

    private static final String getModalityString(final FileInfoBase fInfo, final String structName) {
        int modality = fInfo.getModality();

        modality = determineModality(modality, structName);

        return FileInfoBase.getModalityStr(modality);
    }

    private static final boolean isMRModality(final FileInfoBase fInfo, final String structName) {
        return getModalityString(fInfo, structName).equalsIgnoreCase("magnetic resonance");
    }

    private static final boolean isCTModality(final FileInfoBase fInfo, final String structName) {
        return getModalityString(fInfo, structName).equalsIgnoreCase("computed tomography");
    }

    private static final String getFileFormatString(final FileInfoBase fInfo) {
        final int fileFormatInt = fInfo.getFileFormat();
        String fileFormatString = FileUtility.getFileTypeStr(fileFormatInt);
        if (fileFormatString.equalsIgnoreCase("xml")) {
            fileFormatString = "mipav xml";
        } else if (fileFormatString.equalsIgnoreCase("mat")) {
            fileFormatString = "matlab";
        }

        return fileFormatString;
    }

    private static final boolean isDicomFormat(final FileInfoBase fInfo) {
        return getFileFormatString(fInfo).equalsIgnoreCase("dicom");
    }

    private static final boolean isNiftiFormat(final FileInfoBase fInfo) {
        return getFileFormatString(fInfo).equalsIgnoreCase("nifti");
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
     * Attempts to convert from the DICOM body part to BRICS anatomic site permissible values. If nothing matches,
     * return the original name.
     * 
     * @param bodyPart The DICOM model name.
     * @return The DICOM model name to BRICS scanner model permissible values. If nothing matches, return the original
     *         name.
     */
    private static final String convertDicomBodyPartToBRICS(final String bodyPart) {
        if (bodyPart == null || bodyPart.equals("")) {
            return bodyPart;
        }

        if (bodyPart.equalsIgnoreCase("HEAD") || bodyPart.equalsIgnoreCase("SKULL")) {
            return "Brain";
        } else if (bodyPart.equalsIgnoreCase("CSPINE")) {
            return "Cervical spine";
        } else if (bodyPart.equalsIgnoreCase("LSPINE")) {
            return "Lumbar spine";
        } else if (bodyPart.equalsIgnoreCase("TSPINE")) {
            return "Thoracic spine";
        }

        // DICOM: SKULL, CSPINE, TSPINE, LSPINE, SSPINE, COCCYX, CHEST, CLAVICLE, BREAST, ABDOMEN, PELVIS, HIP,
        // SHOULDER, ELBOW, KNEE, ANKLE, HAND, FOOT, EXTREMITY, HEAD, HEART, NECK, LEG, ARM, JAW

        // BRICS: Brain, Calf, Cervical spine, Lumbar spine, Thigh, Thoracic spine

        return bodyPart;
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

        try {
            if (type.equalsIgnoreCase("D")) {
                return Integer.toString((int) (Integer.parseInt(num) / 30.4166666667));
            } else if (type.equalsIgnoreCase("W")) {
                return Integer.toString((int) (Integer.parseInt(num) / 4.34523809524));
            } else if (type.equalsIgnoreCase("M")) {
                return Integer.valueOf(num).toString();
            } else if (type.equalsIgnoreCase("Y")) {
                return Integer.toString(Integer.parseInt(num) * 12);
            }
        } catch (NumberFormatException e) {
            return "";
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
                final JList<String> list = (JList<String>) comp;
                final ListModel<String> listModel = list.getModel();

                int otherSpecifyIndex = -1;
                boolean foundOtherInDE = false;

                // values are assumed to never contain semi-colons since that's the delimiter
                final String[] valueSplit = value.split(MULTI_SELECT_VALUE_DELIM);

                String otherSpecifyValue = "";
                final ArrayList<Integer> selectedIndicies = new ArrayList<Integer>();
                for (final String val : valueSplit) {
                    boolean found = false;
                    for (int k = 0; k < listModel.getSize(); k++) {
                        final String item = (String) listModel.getElementAt(k);
                        if ( !foundOtherInDE && (item.equalsIgnoreCase(VALUE_OTHER_SPECIFY) || item.equalsIgnoreCase(VALUE_YES_SPECIFY))) {
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
                        if (otherSpecifyValue.equals("")) {
                            otherSpecifyValue = val;
                        } else {
                            otherSpecifyValue += MULTI_SELECT_VALUE_DELIM + val;
                        }
                    }
                }

                if (foundOtherInDE && !otherSpecifyValue.equals("")) {
                    selectedIndicies.add(otherSpecifyIndex);
                    if (deVal.getOtherSpecifyField() != null) {
                        deVal.getOtherSpecifyField().setText(otherSpecifyValue);
                    }
                    System.err.println("Other specify\t" + deVal.getName() + "\t\t" + otherSpecifyValue);
                } else if ( !foundOtherInDE && !otherSpecifyValue.equals("")) {
                    System.err.println("Value not found. DE:\t" + comp.getName() + "\t" + otherSpecifyValue);
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
            final JComboBox<String> combo = (JComboBox<String>) deVal.getComp();
            for (int i = 0; i < combo.getItemCount(); i++) {
                if (combo.getItemAt(i).trim().equalsIgnoreCase(VALUE_OTHER_SPECIFY) || combo.getItemAt(i).trim().equalsIgnoreCase(VALUE_YES_SPECIFY)) {
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
        if (deVal.getDataElementInfo().getRestrictions() == InputRestrictions.SINGLE || deVal.getDataElementInfo().getRestrictions() == InputRestrictions.MULTIPLE) {
            if (deVal.getComp() instanceof JComboBox) {
                final JComboBox<String> combo = (JComboBox<String>) deVal.getComp();
                for (int i = 0; i < combo.getItemCount(); i++) {
                    if (combo.getItemAt(i).trim().equalsIgnoreCase(VALUE_OTHER_SPECIFY) || combo.getItemAt(i).trim().equalsIgnoreCase(VALUE_YES_SPECIFY)) {
                        return true;
                    }
                }
            } else if (deVal.getComp() instanceof JList) {
                final ListModel<String> listModel = ((JList<String>) deVal.getComp()).getModel();
                for (int k = 0; k < listModel.getSize(); k++) {
                    final String item = listModel.getElementAt(k);
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

        // TODO why tie checks to field names

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

    private static final boolean isBValFileElement(final String groupName, final String deName) {
        if (groupName.equalsIgnoreCase(IMG_DIFF_GROUP) && deName.equalsIgnoreCase(IMG_DIFF_BVAL_ELEMENT_NAME)) {
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

    private static final boolean isMRImagingStructure(final String structureName) {
        if ( !isImagingStructure(structureName)) {
            return false;
        }

        return structureName.equalsIgnoreCase(MRI_FS_NAME);
    }

    private static final boolean isPulseSeqTag(final DataElementValue deVal) {
        return deVal.getGroupName().equalsIgnoreCase(IMG_MR_GROUP) && deVal.getName().equalsIgnoreCase(IMG_PULSE_SEQ_ELEMENT_NAME);
    }

    private static final String cleanPulseSeqVal(final String value) {
        return value.replace('/', '-').replace(' ', '_').replace(',', '_');
    }

    private static final boolean isSpectroscopyImagingStructure(final String structureName) {
        if ( !isImagingStructure(structureName)) {
            return false;
        }

        return structureName.toLowerCase().contains(SPECTROSCOPY_FS_SUFFIX);
    }

    private static final boolean isFMRIImagingStructure(final String structureName) {
        if ( !isImagingStructure(structureName)) {
            return false;
        }

        return structureName.toLowerCase().endsWith(FMRI_FS_SUFFIX);
    }

    private static final boolean isDtiImagingStructure(final String structureName) {
        if ( !isImagingStructure(structureName)) {
            return false;
        }

        return structureName.toLowerCase().endsWith(DTI_FS_SUFFIX);
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

    private class MyRightCellRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = -7905716122046419275L;

        @Override
        public Component getTableCellRendererComponent(final JTable table, final Object value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
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

    private class ChooseDataStructDialog extends JDialog implements ActionListener, ItemListener {
        private static final long serialVersionUID = 4199199899439094828L;

        private final PlugInDialogFITBIR owner;

        private ViewTableModel structsModel;

        private JTable structsTable;
        
        private TableRowSorter<TableModel> structsSorter;

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

            structsTable.setFont(serif12);
            structsTable.getTableHeader().setFont(serif12B);

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

            structsScrollPane = new JScrollPane(structsTable, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

            structsScrollPane.setPreferredSize(new Dimension(600, 300));
            
            structsSorter = new TableRowSorter<TableModel>(structsModel);
            structsTable.setRowSorter(structsSorter);
            
            final TreeSet<String> uniqueDiseaseList = new TreeSet<String>(new AlphabeticalComparator());
            for (final String diseaseEntry : diseaseAL) {
                String[] diseaseList = diseaseEntry.split(", ");
                for (final String disease : diseaseList) {
                    if (!uniqueDiseaseList.contains(disease)) {
                        uniqueDiseaseList.add(disease);
                    }
                }
            }
            
            JPanel diseaseFilterPanel = new JPanel();
            JLabel diseaseFilterLabel = new JLabel("Display form structures from ");
            diseaseFilterLabel.setFont(MipavUtil.font12);
            JComboBox<String> diseaseFilterCombo = new JComboBox<String>();
            diseaseFilterCombo.setName("DiseaseFilter");
            diseaseFilterCombo.setFont(MipavUtil.font12);
            
            diseaseFilterCombo.addItem("All Diseases");
            for (final String element : uniqueDiseaseList) {
                final String item = element.trim();
                diseaseFilterCombo.addItem(item);
            }
            
            int selectedIndex = 0;
            String selectedDisease = getDiseaseSelection();
            if (chosenDiseaseFilterIndex == -1 && selectedDisease != null && !selectedDisease.trim().equals("")) {
                System.err.println("Default form structure disease specified: " + selectedDisease);
                
                for (int i = 0; i < diseaseFilterCombo.getItemCount(); i++) {
                    if (diseaseFilterCombo.getItemAt(i).contains(selectedDisease)) {
                        selectedIndex = i;
                        break;
                    }
                }
                diseaseFilterCombo.setSelectedIndex(selectedIndex);

                if (selectedDisease.equals("All Diseases")) {
                    structsSorter.setRowFilter(null);
                } else {
                    RowFilter<Object,Object> filter = new RowFilter<Object,Object> () {
                       public boolean include(Entry<? extends Object, ? extends Object> entry) {
                           String diseaseVal = entry.getStringValue(4);
                           return diseaseVal.contains(selectedDisease);
                       }
                    };
                    structsSorter.setRowFilter(filter);
                }
            } else {
                diseaseFilterCombo.setSelectedIndex(chosenDiseaseFilterIndex);
            }
            
            diseaseFilterCombo.addItemListener(this);
            
            diseaseFilterPanel.add(diseaseFilterLabel);
            diseaseFilterPanel.add(diseaseFilterCombo);

            final JPanel OKPanel = new JPanel();
            final JButton OKButton = new JButton("Add");
            OKButton.setActionCommand("ChooseStructOK");
            OKButton.addActionListener(this);
            OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
            OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
            OKButton.setFont(serif12B);

            OKPanel.add(OKButton);
            
            getContentPane().add(diseaseFilterPanel, BorderLayout.NORTH);

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
                    final int selectedModelRow = structsTable.convertRowIndexToModel(selectedRow);
                    this.dispose();
                    final String dsName = (String) structsModel.getValueAt(selectedModelRow, 0);
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

        /* (non-Javadoc)
         * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
         */
        @Override
        public void itemStateChanged(ItemEvent e) {
            Component source = (Component) e.getSource();
            if (source.getName().equals("DiseaseFilter")) {
                JComboBox<String> cb = (JComboBox<String>) source;
                String selectedDisease = (String) cb.getSelectedItem();
                chosenDiseaseFilterIndex = cb.getSelectedIndex();

                if (selectedDisease.equals("All Diseases")) {
                    structsSorter.setRowFilter(null);
                } else {
                    RowFilter<Object,Object> filter = new RowFilter<Object,Object> () {
                       public boolean include(Entry<? extends Object, ? extends Object> entry) {
                           String diseaseVal = entry.getStringValue(4);
                           return diseaseVal.contains(selectedDisease);
                       }
                    };
                    structsSorter.setRowFilter(filter);
                }
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
            final JScrollPane tabScrollPane = new JScrollPane(dsMainPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

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

                            if (de != null) {
                                // if another DE was appended, make sure that each record matches the new total length
                                // even if it did not have a value for the new DE
                                for (final ArrayList<String> values : record) {
                                    int fieldDiff = csvFieldNames.size() - record.size();
                                    for (int j = 0; j < fieldDiff; j++) {
                                        values.add("");
                                    }
                                }

                                for (final ArrayList<String> values : record) {
                                    // check value not empty and check type of field for date
                                    if ( !values.get(i).trim().equals("") && de.getType().equals(DataType.DATE)) {
                                        values.set(i, convertDateToISOFormat(values.get(i)));
                                    }
                                }
                            } else {
                                MipavUtil.displayError("Unrecognized data element in CSV: " + csvFieldNames.get(i));
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
            requiredLabel.setFont(serif12B);

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

                    int curConflictSelection = resolveConflictsUsing;

                    // if image file set in this repeat, read in the image
                    if (imageFileIndex != -1 && !repeatValues.get(imageFileIndex).trim().equals("")) {
                        // if image_file is in zip format....first unzip it temporarily
                        final String imageFile = repeatValues.get(imageFileIndex);

                        if (new File(imageFile).isAbsolute()) {
                            srcImage = readImgFromCSV(new File(imageFile).getParent(), new File(imageFile).getName());
                        } else {
                            srcImage = readImgFromCSV(csvFile.getParentFile().getAbsolutePath(), imageFile);
                        }

                        if (srcImage != null) {
                            setFormImpliedRecordDataElements(fsData, srcImage, repeatValues);

                            // basic check that image data is de-identified
                            problemTags = deidentificationCheckDicomTags(srcImage.getFileInfo());
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

                            curConflictSelection = determineImageHeaderDescrepencies(srcImage, repeatValues);
                        } else {
                            // TODO problem reading img file - highlight
                        }
                    }

                    if (curConflictSelection == RESOLVE_CONFLICT_CSV && srcImage != null) {
                        populateFields(fsData, srcImage, srcImage.getFileInfo(0));
                    }

                    File bvalFile = null;

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

                                        if (isBValFileElement(deGroupAndName[0], deGroupAndName[1])) {
                                            bvalFile = new File(value);
                                        }
                                    } else if (comp instanceof JComboBox) {
                                        final JComboBox<String> combo = (JComboBox<String>) comp;

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
                                        final JList<String> list = (JList<String>) comp;
                                        final ListModel<String> listModel = list.getModel();

                                        int otherSpecifyIndex = -1;
                                        boolean foundOtherInDE = false;

                                        // values are assumed to never contain semi-colons since that's the delimiter
                                        final String[] valueSplit = value.split(MULTI_SELECT_VALUE_DELIM);

                                        String otherSpecifyValue = "";
                                        final ArrayList<Integer> selectedIndicies = new ArrayList<Integer>();
                                        for (final String val : valueSplit) {
                                            boolean found = false;
                                            for (int k = 0; k < listModel.getSize(); k++) {
                                                final String item = (String) listModel.getElementAt(k);
                                                if ( !foundOtherInDE && (item.equalsIgnoreCase(VALUE_OTHER_SPECIFY) || item.equalsIgnoreCase(VALUE_YES_SPECIFY))) {
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
                                                if (otherSpecifyValue.equals("")) {
                                                    otherSpecifyValue = val;
                                                } else {
                                                    otherSpecifyValue += MULTI_SELECT_VALUE_DELIM + val;
                                                }
                                            }
                                        }

                                        if (foundOtherInDE && !otherSpecifyValue.equals("")) {
                                            selectedIndicies.add(otherSpecifyIndex);
                                            if (deVal.getOtherSpecifyField() != null) {
                                                deVal.getOtherSpecifyField().setText(otherSpecifyValue);
                                            }
                                            System.err.println("Other specify\t" + deVal.getName() + "\t\t" + otherSpecifyValue);
                                        } else if ( !foundOtherInDE && !otherSpecifyValue.equals("")) {
                                            System.err.println("Value not found. DE:\t" + comp.getName() + "\t" + otherSpecifyValue);
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

                    // if bval file given, try to read additional info
                    if (bvalFile != null) {
                        HashMap<String, String> bvalDeTable = readBvalFile(bvalFile);

                        if (bvalDeTable == null) {
                            MipavUtil.displayWarning("Unable to read contents of BVal file: " + bvalFile.getAbsolutePath());
                        } else {
                            for (final DataElementValue deVal : fsData.getGroupRepeat(IMG_DIFF_GROUP, 0).getDataElements()) {
                                String deNameWithGroup = IMG_DIFF_GROUP + "." + deVal.getName();
                                if (bvalDeTable.containsKey(deNameWithGroup)) {
                                    // TODO check for conflicts with existing value instead of overwriting
                                    final JTextField tf = (JTextField) deVal.getComp();
                                    tf.setText(bvalDeTable.get(deNameWithGroup));
//                                        tf.setEnabled(false);
//                                        break;
                                }
                            }
                        }
                    }

//                    // TODO for MR-based data, try to set pulse sequence type for the first repeat based on available info, if not set already. check that srcImage is not null to only do it on the main row
//                    if (srcImage != null && fsData.isGroupRepeatSet(IMG_MR_GROUP, 0)) {
//                        for (final DataElementValue deVal : fsData.getGroupRepeat(IMG_MR_GROUP, 0).getDataElements()) {
//                            String pulseSequenceGuess = extractPulseSequence(fsData, srcImage, srcImage.getFileInfo(0));
//                            if (isValueSet(pulseSequenceGuess) && isPulseSeqTag(deVal) && !isValueSet(deVal.getValue())) {
//                                setElementComponentValue(deVal, pulseSequenceGuess);
//                            }
//                        }
//                    }

                    if ( (curConflictSelection == RESOLVE_CONFLICT_ASK || curConflictSelection == RESOLVE_CONFLICT_IMG) && srcImage != null) {
                        populateFields(fsData, srcImage, srcImage.getFileInfo(0));
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
                                        final JComboBox<String> combo = (JComboBox<String>) comp;

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
                                        final JList<String> list = (JList<String>) comp;
                                        final ListModel<String> listModel = list.getModel();

                                        int otherSpecifyIndex = -1;
                                        boolean foundOtherInDE = false;

                                        // values are assumed to never contain semi-colons since that's the delimiter
                                        final String[] valueSplit = value.split(MULTI_SELECT_VALUE_DELIM);

                                        String otherSpecifyValue = "";
                                        final ArrayList<Integer> selectedIndicies = new ArrayList<Integer>();
                                        for (final String val : valueSplit) {
                                            boolean found = false;
                                            for (int k = 0; k < listModel.getSize(); k++) {
                                                final String item = (String) listModel.getElementAt(k);
                                                if ( !foundOtherInDE && (item.equalsIgnoreCase(VALUE_OTHER_SPECIFY) || item.equalsIgnoreCase(VALUE_YES_SPECIFY))) {
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
                                                if (otherSpecifyValue.equals("")) {
                                                    otherSpecifyValue = val;
                                                } else {
                                                    otherSpecifyValue += MULTI_SELECT_VALUE_DELIM + val;
                                                }
                                            }
                                        }

                                        if (foundOtherInDE && !otherSpecifyValue.equals("")) {
                                            selectedIndicies.add(otherSpecifyIndex);
                                            if (deVal.getOtherSpecifyField() != null) {
                                                deVal.getOtherSpecifyField().setText(otherSpecifyValue);
                                            }
                                            System.err.println("Other specify\t" + deVal.getName() + "\t\t" + otherSpecifyValue);
                                        } else if ( !foundOtherInDE && !otherSpecifyValue.equals("")) {
                                            System.err.println("Value not found. DE:\t" + comp.getName() + "\t" + otherSpecifyValue);
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
            if (cmdLineCsvFlag) {
                fileIO.setQuiet(true);
            }

            ModelImage srcImage = null;
            validFile = true;
            File origSrcFile;

            boolean isSpectroscopy = isSpectroscopyImagingStructure(dataStructureName);
            FileInfoBase[] spectroscopyHeaderList = null;

            try {
                if (imageFile.endsWith(".zip")) {

                    String destName = imageFile.replace("/", File.separator);
                    destName = destName.replace("\\", File.separator);
                    destName = destName.substring(destName.lastIndexOf(File.separator) + 1, destName.lastIndexOf("."));

                    // String destDirName =
                    // final String tempDir = parentDir + File.separator + destName + "_temp_" +
                    // System.currentTimeMillis();

                    final String tempDir = Files.createTempDirectory(destName + "_unzip_").toString();
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
//                        if ( !f.exists()) {
//                            f.mkdir();
//                        }

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

                    // final String tempDir = parentDir + File.separator + destName + "_temp_" +
                    // System.currentTimeMillis();
                    final String tempDir = Files.createTempDirectory(destName + "_unzip_").toString();
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
//                        if ( !f.exists()) {
//                            f.mkdir();
//                        }

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
                            // if we find a likely dicom file, prefer that over other files
                            boolean foundDicom = false;
                            for (File file : fileList) {
                                if (file.isFile() && (file.getName().toLowerCase().endsWith(".dcm") || file.getName().toLowerCase().endsWith(".ima"))) {
                                    foundDicom = true;
                                    origSrcFile = file;
                                    break;
                                }
                            }
                            
                            // if we didn't find an .dcm/.ima, search for other files (maybe dicoms without extensions, or some other image format)
                            if (!foundDicom) {
                                // find first file that doesn't start with . (e.g., .DS_Store) and isn't a directory
                                for (File file : fileList) {
                                    if (file.isFile() && !file.getName().startsWith(".")) {
                                        origSrcFile = file;
                                        break;
                                    }
                                }
                            }

                            if (origSrcFile.isDirectory()) {
                                logError("No files found in specified image directory (those starting with '.' excluded): " + filePath);
                                validFile = false;
                                return null;
                            } else {
                                filePath = origSrcFile.getAbsolutePath();

                                System.out.println("Opening from dir:\t" + filePath);
                            }
                        } else {
                            logError("No files found in specified image directory: " + filePath);
                            validFile = false;
                            return null;
                        }
                    } else {
                        isMultifile = false;
                    }
                }

                if ( !isSpectroscopy) {
                    final File file = new File(filePath);

                    System.out.println(file);

//                    boolean[] prevDebugLvls = Preferences.getDebugLevels();
//                    Preferences.setDebugLevels(new boolean[] {true, true, true, true, true});

                    if (cmdLineCsvFlag) {
                        PrintStream stdoutStream = new CmdLineOutputStream(System.out, readErrorFileOut);
                        PrintStream stderrStream = new CmdLineOutputStream(System.err, readErrorFileOut);

                        System.setOut(stdoutStream);
                        System.setErr(stderrStream);
                    }

                    srcImage = fileIO.readImage(file.getName(), file.getParent() + File.separator, isMultifile, null);

                    if (cmdLineCsvFlag) {
                        System.setOut(logOnlyOut);
                        System.setErr(logOnlyErr);
                    }

//                    Preferences.setDebugLevels(prevDebugLvls);

                    if (srcImage == null) {
                        // if cmd line, highlight failure
                        logError("Unable to open image file specified: " + imageFile);
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
                        structRowImgFileInfoList.set(selectedRow, new ImgFileInfo(origSrcFile.getAbsolutePath(), isMultifile, FileUtility.getFileNameList(srcImage),
                                srcImage.getFileInfo(0).getFileFormat(), createThumbnailDataForWriting(thumbnailImage)));
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
                } else {
                    // if spectroscopy, only read image header, not the data and skip preview image
                    final File file = new File(filePath);
                    System.out.println(file);

                    int[] extents = new int[] {256, 256};

                    if (isMultifile) {
                        String[] fileList = FileUtility.getFileList(file.getParent() + File.separator, file.getName(), false);

                        spectroscopyHeaderList = new FileInfoBase[fileList.length];

                        for (int i = 0; i < fileList.length; i++) {
                            spectroscopyHeaderList[i] = fileIO.readHeader(file.getParent() + File.separator + fileList[i], isMultifile);

                            if (spectroscopyHeaderList[i] instanceof FileInfoDicom) {
                                ((FileInfoDicom) spectroscopyHeaderList[i]).setInfoFromTags();

                                String rowStr = (String) ((FileInfoDicom) spectroscopyHeaderList[i]).getTagTable().getValue("0028,0010");
                                String colStr = (String) ((FileInfoDicom) spectroscopyHeaderList[i]).getTagTable().getValue("0028,0011");
                                if (rowStr != null && colStr != null) {
                                    int rows = Integer.parseInt(rowStr);
                                    int cols = Integer.parseInt(colStr);
                                    extents = new int[] {cols, rows, fileList.length};
                                    spectroscopyHeaderList[i].setExtents(extents);
                                }
                            }
                        }
                    } else {
                        spectroscopyHeaderList = new FileInfoBase[1];
                        spectroscopyHeaderList[0] = fileIO.readHeader(file.getAbsolutePath(), isMultifile);

                        if (spectroscopyHeaderList[0] instanceof FileInfoDicom) {
                            ((FileInfoDicom) spectroscopyHeaderList[0]).setInfoFromTags();

                            String rowStr = (String) ((FileInfoDicom) spectroscopyHeaderList[0]).getTagTable().getValue("0028,0010");
                            String colStr = (String) ((FileInfoDicom) spectroscopyHeaderList[0]).getTagTable().getValue("0028,0011");
                            if (rowStr != null && colStr != null) {
                                int rows = Integer.parseInt(rowStr);
                                int cols = Integer.parseInt(colStr);
                                extents = new int[] {cols, rows};
                                spectroscopyHeaderList[0].setExtents(extents);
                            }
                        }
                    }

                    srcImage = new ModelImage(spectroscopyHeaderList[0].getDataType(), extents, file.getName());
                    srcImage.setFileInfo(spectroscopyHeaderList);

                    ModelImage blankImg = ViewUserInterface.getReference().createBlankImage((FileInfoDicom) spectroscopyHeaderList[0].clone(), false, false);

                    previewImg = new ViewJComponentPreviewImage(blankImg, extents, owner);
                    int slice = 0;
                    previewImg.createImg(slice);

                    previewImgPanel.removeAll();
                    previewImgPanel.repaint();

                    previewImgPanel.add(previewImg);

                    addedPreviewImage = true;

//                    ModelImage thumbnailImage = createThumbnailImage(srcImage);

                    if (launchedFromInProcessState) {
                        final int selectedRow = structTable.getSelectedRow();
                        previewImages.set(selectedRow, previewImg);
                        previewImages.get(selectedRow).setSliceBrightness(previewImgBrightness, previewImgContrast);

                        ImgFileInfo imgInfo = new ImgFileInfo(file.getAbsolutePath(), isMultifile, FileUtility.getFileNameList(spectroscopyHeaderList),
                                spectroscopyHeaderList[0].getFileFormat(), null);

                        structRowImgFileInfoList.set(selectedRow, imgInfo);
                    } else {
                        final int size = previewImages.size();
                        previewImages.set(size - 1, previewImg);
                        previewImages.get(size - 1).setSliceBrightness(previewImgBrightness, previewImgContrast);

                        ImgFileInfo imgInfo = new ImgFileInfo(file.getAbsolutePath(), isMultifile, FileUtility.getFileNameList(spectroscopyHeaderList),
                                spectroscopyHeaderList[0].getFileFormat(), null);

                        structRowImgFileInfoList.set(size - 1, imgInfo);
                    }

                    // cleanup blank thumbnail modelimage
                    if (blankImg != null) {
                        blankImg.disposeLocal();
                        blankImg = null;
                    }

                    previewImgPanel.validate();
                    previewImgPanel.repaint();
                }
            } catch (final FileNotFoundException e) {
                // if cmd line, output error but continue
                logError("The system cannot find the file specified: " + imageFile);
                e.printStackTrace();
                validFile = false;
            } catch (final NullPointerException e) {
                // if cmd line, output error but continue
                logError("Unable to open image file specified: " + imageFile);
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
            addRepeatButton.setFont(serif12B);
            addRepeatButton.setActionCommand("AddRepeat_-_" + group.getName());
            addRepeatButton.addActionListener(this);
            final JButton removeRepeatButton = new JButton("Remove repeat");
            removeRepeatButton.setFont(serif12B);
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
                        browseButton.setFont(serif12B);
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

        public GroupRepeat parseGroupRepeat(final FormStructureData fsData, final RepeatableGroup group, final int repeatNum) {
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
                if (isPDBPImagingStructure(fsData.getStructInfo().getShortName()) && de.getStructuralDataElement().getName().equalsIgnoreCase(SITE_NAME_ELEMENT_NAME)) {
                    final JComboBox<String> cb = new JComboBox<String>();
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

//                    if (deFullInfo.getNinds() != null && !deFullInfo.getNinds().getValue().equals("")) {
//                        tooltip += "<p><b>NINDS CDE ID:</b> " + deFullInfo.getNinds().getValue() + "</p>";
//                    }
                    if (deFullInfo.getGuidelines() != null && !deFullInfo.getGuidelines().trim().equals("")) {
                        tooltip += "<p><b>Guidelines & Instructions:</b></br>" + WordUtils.wrap(removeRedundantDiseaseInfo(deFullInfo.getGuidelines()), 80, "<br/>", false)
                                + "</p>";
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
                        final JComboBox<String> cb = new JComboBox<String>();
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

//                        if (deFullInfo.getNinds() != null && !deFullInfo.getNinds().getValue().equals("")) {
//                            tooltip += "<p><b>NINDS CDE ID:</b> " + deFullInfo.getNinds().getValue() + "</p>";
//                        }
                        if (deFullInfo.getGuidelines() != null && !deFullInfo.getGuidelines().trim().equals("")) {
                            tooltip += "<p><b>Guidelines & Instructions:</b></br>" + WordUtils.wrap(removeRedundantDiseaseInfo(deFullInfo.getGuidelines()), 80, "<br/>", false)
                                    + "</p>";
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

                        final JList<String> list = new JList<String>(valStrList);
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

//                        if (deFullInfo.getNinds() != null && !deFullInfo.getNinds().getValue().equals("")) {
//                            tooltip += "<p><b>NINDS CDE ID:</b> " + deFullInfo.getNinds().getValue() + "</p>";
//                        }
                        if (deFullInfo.getGuidelines() != null && !deFullInfo.getGuidelines().trim().equals("")) {
                            tooltip += "<p><b>Guidelines & Instructions:</b></br>" + WordUtils.wrap(removeRedundantDiseaseInfo(deFullInfo.getGuidelines()), 80, "<br/>", false)
                                    + "</p>";
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

//                    if (deFullInfo.getNinds() != null && !deFullInfo.getNinds().getValue().equals("")) {
//                        tooltip += "<p><b>NINDS CDE ID:</b> " + deFullInfo.getNinds().getValue() + "</p>";
//                    }
                    if (deFullInfo.getGuidelines() != null && !deFullInfo.getGuidelines().trim().equals("")) {
                        tooltip += "<p><b>Guidelines & Instructions:</b></br>" + WordUtils.wrap(removeRedundantDiseaseInfo(deFullInfo.getGuidelines()), 80, "<br/>", false)
                                + "</p>";
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
            HashMap<String, String> extractedFields = extractHeaderInfo(img, img.getFileInfo(0));

            final ArrayList<String> csvFList = new ArrayList<String>();
            final ArrayList<String> csvPList = new ArrayList<String>();
            final ArrayList<String> headerList = new ArrayList<String>();

            for (int i = 0; i < csvFieldNames.size(); i++) {
                String fieldNameWithGroup = csvFieldNames.get(i);
                String csvVal = repeatValues.get(i).trim();

                String groupName = getFieldGroup(fieldNameWithGroup);
                String fieldName = getFieldName(fieldNameWithGroup);

                if ( !csvVal.equals("")) {
                    if (extractedFields.containsKey(fieldName)) {
                        String extractedVal = extractedFields.get(fieldName);

                        if (isValueSet(extractedVal)) {
                            if ( !csvVal.equalsIgnoreCase(extractedVal)) {
                                // some float fields entered as ints need to be parsed
                                try {
                                    float val = Float.parseFloat(csvVal);
                                    if ( !String.valueOf(val).equalsIgnoreCase(extractedVal)) {
                                        // mismatched numbers, add to conflict list
                                        csvFList.add(fieldName);
                                        csvPList.add(csvVal);
                                        headerList.add(extractedVal);
                                    }
                                } catch (NumberFormatException e) {
                                    // not a number, so add to conflict list
                                    csvFList.add(fieldName);
                                    csvPList.add(csvVal);
                                    headerList.add(extractedVal);
                                }
                            }
                        }
                    }
                }
            }

            if (csvFList.size() > 0) {
                if (resolveConflictsUsing == RESOLVE_CONFLICT_ASK) {
                    String message = "Certain image information in the provided metadata do not match the image header : \n";
                    for (int i = 0; i < csvFList.size(); i++) {
                        final String fieldName = csvFList.get(i);
                        final String param = csvPList.get(i);
                        final String headerInfo = headerList.get(i);

                        message = message + fieldName + " : " + "      csv:" + param + "     header:" + headerInfo + "\n";
                    }

                    // when running from the command line, note the conflict but use the CSV
                    if (cmdLineCsvFlag) {
                        System.err.println(message);
                        return RESOLVE_CONFLICT_CSV;
                    }

                    if (bidsPackageFlag) {
                        System.err.println(message);
                        return RESOLVE_CONFLICT_CSV;
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
         * Populates some of the fields with info from image header
         */
        public void populateFields(final FormStructureData fsData, final ModelImage img, final FileInfoBase fInfo) {
            HashMap<String, String> extractedFields = extractHeaderInfo(img, fInfo);

            // if nifti, check for matching json file
            if (fInfo.getFileName().toLowerCase().endsWith(".nii") || fInfo.getFileName().toLowerCase().endsWith(".nii.gz")) {
                String dir = img.getImageDirectory();
                String name = fInfo.getFileName();
                String jsonName = name.replaceAll("(.nii|.nii.gz)$", ".json");
                File jsonFile = new File(dir + File.separator + jsonName);
                boolean foundJson = false;
                if (jsonFile.exists() && jsonFile.canRead()) {
                    foundJson = true;
                } else {
                    if (jsonName.contains("_defaced")) {
                        jsonName = jsonName.replaceFirst("_defaced", "");
                        jsonFile = new File(dir + File.separator + jsonName);

                        if (jsonFile.exists() && jsonFile.canRead()) {
                            foundJson = true;
                        }
                    }
                }

                if (foundJson) {
                    System.out.println("Found matching json file for NIfTI image.  Loading metadata from " + jsonFile);
                    
                    JSONObject scanJson = readJsonFile(jsonFile);

                    if (scanJson != null) {
                        readGlobalFieldsFromJson(extractedFields, scanJson);
                        readGenericFieldsFromJson(extractedFields, scanJson);
                        readMRFieldsFromJson(extractedFields, scanJson, name);
                        readDwiFieldsFromJson(extractedFields, scanJson);
                        readFuncFieldsFromJson(extractedFields, scanJson);
                    }
                }
            }

            for (final RepeatableGroup group : fsData.getStructInfo().getRepeatableGroups()) {
                for (final GroupRepeat repeat : fsData.getAllGroupRepeats(group.getName())) {
                    for (final DataElementValue deVal : repeat.getDataElements()) {
                        final String deName = deVal.getName();

                        // if any format-specific field values were found, map them onto the given data element
                        if (extractedFields != null && extractedFields.containsKey(deName)) {
                            String extVal = extractedFields.get(deName);
                            if (isValueSet(extVal)) {
                                setElementComponentValue(deVal, extVal);
                            }
                        } else if (extractedFields != null && extractedFields.containsKey(group.getName() + "." + deName)) {
                            String extVal = extractedFields.get(group.getName() + "." + deName);
                            if (isValueSet(extVal)) {
                                setElementComponentValue(deVal, extVal);
                            }
                        }
                    }
                }
            }
        }

        private HashMap<String, String> extractHeaderInfo(ModelImage img, FileInfoBase fInfo) {
            HashMap<String, String> extractedFields = new HashMap<String, String>();

            if (isDicomFormat(fInfo)) {
                FileInfoDicom fileInfoDicom = (FileInfoDicom) fInfo;

                extractDicomHeaderInfo(extractedFields, fileInfoDicom);

                extractDicomMRHeaderInfo(extractedFields, img, fileInfoDicom);

                extractDicomCTHeaderInfo(extractedFields, fileInfoDicom);
            } else if (isNiftiFormat(fInfo)) {
                extractNiftiHeaderInfo(extractedFields, (FileInfoNIFTI) fInfo);
            }

            // extract basic info after formats so that contrast info can be used for modality options
            extractBasicHeaderInfo(extractedFields, img, fInfo);

            return extractedFields;
        }

        private void extractBasicHeaderInfo(HashMap<String, String> extractedFields, ModelImage img, FileInfoBase fInfo) {
            int[] exts = null;
            if (img != null) {
                exts = img.getExtents();
            } else {
                exts = fInfo.getExtents();
            }

            final float[] res = fInfo.getResolutions();
            final int[] units = fInfo.getUnitsOfMeasure();

            final float sliceThickness = fInfo.getSliceThickness();
            final int orient = fInfo.getImageOrientation();
            final String orientStr = FileInfoBase.getImageOrientationStr(orient);

            String fileFormatString = getFileFormatString(fInfo);
            String modalityString = getModalityString(fInfo, dataStructureName);

            boolean contrastUsed = false;
            if (extractedFields != null && extractedFields.containsKey("ImgContrastAgentUsedInd")) {
                contrastUsed = extractedFields.get("ImgContrastAgentUsedInd").equalsIgnoreCase("Yes");
            }

            if (exts != null) {
                extractedFields.put("ImgDimensionTyp", exts.length + "D");
                extractedFields.put("ImgDim1ExtentVal", String.valueOf(exts[0]));
                extractedFields.put("ImgDim2ExtentVal", String.valueOf(exts[1]));
                if (exts.length > 2) {
                    extractedFields.put("ImgDim3ExtentVal", String.valueOf(exts[2]));
                }
                if (exts.length > 3) {
                    extractedFields.put("ImgDim4ExtentVal", String.valueOf(exts[3]));
                }
//                extractedFields.put("ImgDim5ExtentVal", String.valueOf(exts[4]));
            }

            if (exts != null && units != null) {
                extractedFields.put("ImgDim1UoMVal", Unit.getUnitFromLegacyNum(units[0]).toString());
                extractedFields.put("ImgDim2UoMVal", Unit.getUnitFromLegacyNum(units[1]).toString());
                if (exts.length > 2) {
                    extractedFields.put("ImgDim3UoMVal", Unit.getUnitFromLegacyNum(units[2]).toString());
                }
                if (exts.length > 3) {
                    extractedFields.put("ImgDim4UoMVal", Unit.getUnitFromLegacyNum(units[3]).toString());
                }
//                extractedFields.put("ImgDim5UoMVal", Unit.getUnitFromLegacyNum(units[4]).toString());
                if ( !extractedFields.containsKey("ImgDim4ExtentTyp") && exts.length > 3 && Unit.getUnitFromLegacyNum(units[3]).getType() == UnitType.TIME) {
                    extractedFields.put("ImgDim4ExtentTyp", "Time");
                }
            }

            if (exts != null && res != null) {
                extractedFields.put("ImgDim1ResolVal", String.valueOf(res[0]));
                extractedFields.put("ImgDim2ResolVal", String.valueOf(res[1]));
                if (exts.length > 2) {
                    extractedFields.put("ImgDim3ResolVal", String.valueOf(res[2]));
                }
                if (exts.length > 3) {
                    extractedFields.put("ImgDim4ResolVal", String.valueOf(res[3]));
                }
//                extractedFields.put("ImgDim5ResolVal", String.valueOf(res[4]));
            }

            extractedFields.put("ImgModltyTyp", convertModalityToBRICS(modalityString, contrastUsed));
            extractedFields.put("ImgFileFormatTyp", fileFormatString);

            if (sliceThickness > 0 && sliceThickness <= 99) {
                extractedFields.put("ImgSliceThicknessVal", String.valueOf(sliceThickness));
            }

            if (orient != FileInfoBase.UNKNOWN_ORIENT) {
                extractedFields.put("ImgSliceOrientTyp", orientStr);
            }
        }

        private void extractDicomHeaderInfo(HashMap<String, String> extractedFields, FileInfoDicom fInfo) {
            final FileInfoDicom fileInfoDicom = (FileInfoDicom) fInfo;

            String ageVal = (String) (fileInfoDicom.getTagTable().getValue("0010,1010", false));

            int birthYear = 0;
            String birthDate = getTagValue(fileInfoDicom, "0010,0030");
            if (isValueSet(birthDate)) {
                String[] splitStr = birthDate.trim().split("/");
                if (splitStr.length == 3) {
                    birthYear = Integer.parseInt(splitStr[2]);
                }
            }

            // put in to skip erroneous values set in some TRACK-TBI Pilot CT data
            if (isValueSet(ageVal) && (ageVal.equalsIgnoreCase("135Y") || (ageVal.equalsIgnoreCase("100Y") && birthYear >= 1910) || ageVal.startsWith("X"))) {
                ageVal = null;
            }
            if (isValueSet(ageVal)) {
                final String ageStr = convertDicomAgeToBRICS(ageVal);
                if (isValueSet(ageStr)) {
                    final float ageInMonths = Float.parseFloat(ageStr);
                    if (ageInMonths != 0) {
                        final String ageInYears = String.valueOf(ageInMonths / 12);
                        
                        if ((ageInMonths / 12) < 150) {
                            extractedFields.put("AgeVal", String.valueOf(ageInMonths));
                            extractedFields.put("AgeYrs", ageInYears);
                        }
                    }
                }
            }

            String siteName = getTagValue(fileInfoDicom, "0008,0080");
            // CNRM anonymization of the institution tag sets the value to Institution instead of clearing the value
            if (isValueSet(siteName) && (siteName.trim().equalsIgnoreCase("Institution") || siteName.trim().equalsIgnoreCase("GENERIC INSTITUTION")
                    || siteName.trim().toLowerCase().contains("anonymous"))) {
                siteName = "";
            }
            extractedFields.put("SiteName", siteName);

            String visitDate = getTagValue(fileInfoDicom, "0008,0020");
            extractedFields.put("VisitDate", convertDateToISOFormat(visitDate));
            extractedFields.put("ImgStdyDateTime", convertDateTimeToISOFormat(visitDate, getTagValue(fileInfoDicom, "0008,0030")));
            extractedFields.put("ImgSliceOverSampVal", getTagValue(fileInfoDicom, "0018,0093"));
            extractedFields.put("ImgGapBetwnSlicesMeasr", getTagValue(fileInfoDicom, "0018,0088"));
            extractedFields.put("ImgAntmicSite", convertDicomBodyPartToBRICS(getTagValue(fileInfoDicom, "0018,0015")));

            extractedFields.put("ImgFOVMeasrDescTxt", getTagValue(fileInfoDicom, "0018,1100"));
            extractedFields.put("ImgScannerManufName", convertManufNameToBRICS(getTagValue(fileInfoDicom, "0008,0070")));
            extractedFields.put("ImgScannerSftwrVrsnNum", getTagValue(fileInfoDicom, "0018,1020"));
            extractedFields.put("ImgHeadPostnTxt", getTagValue(fileInfoDicom, "0018,5100"));

            extractedFields.put("ImgScannerModelName", convertModelNameToBRICS(getTagValue(fileInfoDicom, "0008,1090")));
            extractedFields.put("ImgBandwidthVal", getTagValue(fileInfoDicom, "0018,0095"));

            String patientName = getTagValue(fileInfoDicom, "0010,0010");
            String patientID = getTagValue(fileInfoDicom, "0010,0020");
            if (isGuid(patientID)) {
                extractedFields.put("GUID", patientID);
            } else if (isGuid(patientName)) {
                extractedFields.put("GUID", patientName);
            }

            String contrastAgent = getTagValue(fileInfoDicom, "0018,0010");
            String contrastMethod = getTagValue(fileInfoDicom, "0018,1040");
            if (isValueSet(contrastMethod)) {
                // System.err.println(patientName + "\tContrast route: " + contrastMethod);
                if (contrastMethod.equalsIgnoreCase("IV") || contrastMethod.equalsIgnoreCase("Oral & IV")) {
                    contrastMethod = "Infusion";
                }
            }
            String contrastTime = getTagValue(fileInfoDicom, "0018,1042");
            if (isValueSet(contrastTime) && isValueSet(visitDate)) {
                contrastTime = convertDateTimeToISOFormat(visitDate, contrastTime);
            }
            String contrastDose = getTagValue(fileInfoDicom, "0018,1044");
            String contrastRate = getTagValue(fileInfoDicom, "0018,1046");

            if (isValueSet(contrastAgent) || isValueSet(contrastMethod) || isValueSet(contrastTime) || isValueSet(contrastDose) || isValueSet(contrastRate)) {
                extractedFields.put("ImgContrastAgentUsedInd", "Yes");
                extractedFields.put("ImgContrastAgentName", contrastAgent);
                extractedFields.put("ImgContrastAgentMethodTyp", contrastMethod);
                extractedFields.put("ImgContrastAgentInjctnTime", contrastTime);
                extractedFields.put("ImgContrastAgentDose", contrastDose);
                extractedFields.put("ImgContrastAgentRate", contrastRate);
            }
        }

        private void extractDicomMRHeaderInfo(HashMap<String, String> extractedFields, ModelImage img, FileInfoDicom fileInfoDicom) {
            if (isMRModality(fileInfoDicom, fsData.getStructInfo().getShortName())) {
                extractedFields.put("ImgEchoDur", getTagValue(fileInfoDicom, "0018,0081"));
                extractedFields.put("ImgRepetitionGapVal", getTagValue(fileInfoDicom, "0018,0080"));
                extractedFields.put("ImgScannerStrgthVal", convertMagFieldStrengthToBRICS(getTagValue(fileInfoDicom, "0018,0087")));
                extractedFields.put("ImgFlipAngleMeasr", getTagValue(fileInfoDicom, "0018,1314"));

                extractedFields.put("ImgMRIT1T2SeqName", getTagValue(fileInfoDicom, "0018,0024"));
                extractedFields.put("ImgInversionTime", getTagValue(fileInfoDicom, "0018,0082"));
                extractedFields.put("ImgEchoTrainLngthMeasr", getTagValue(fileInfoDicom, "0018,0091"));
                extractedFields.put("ImgPhasEncdeDirctTxt", getTagValue(fileInfoDicom, "0018,1312"));
                extractedFields.put("ImgSignalAvgNum", getTagValue(fileInfoDicom, "0018,0083"));
                extractedFields.put("ImgRFCoilName", getTagValue(fileInfoDicom, "0018,1250"));

                String scanOptions = getTagValue(fileInfoDicom, "0018,0022");
                // FC ==> flow compensation
                if (isValueSet(scanOptions) && scanOptions.contains("FC")) {
                    extractedFields.put("ImgFlowCompnsatnInd", "Yes");
                }

                extractDicomDtiHeaderInfo(extractedFields, img, fileInfoDicom);

                extractDicomFMRIHeaderInfo(extractedFields, fileInfoDicom);

                extractDicomSpectroscopyHeaderInfo(extractedFields, fileInfoDicom);
            }
        }

        private void extractDicomSpectroscopyHeaderInfo(HashMap<String, String> extractedFields, FileInfoDicom fileInfoDicom) {
            if (isMRModality(fileInfoDicom, fsData.getStructInfo().getShortName()) && isSpectroscopyImagingStructure(fsData.getStructInfo().getShortName())) {
                String seriesDesc = getTagValue(fileInfoDicom, "0008,103E");
                String protocolName = getTagValue(fileInfoDicom, "0018,1030");

                if ( (isValueSet(seriesDesc) && seriesDesc.toUpperCase().contains("PRESS")) || (isValueSet(protocolName) && protocolName.toUpperCase().contains("PRESS"))) {
                    extractedFields.put("ImgPulseSeqTyp", "PRESS");
                } else if ( (isValueSet(seriesDesc) && seriesDesc.toUpperCase().contains("ISIS")) || (isValueSet(protocolName) && protocolName.toUpperCase().contains("ISIS"))) {
                    extractedFields.put("ImgPulseSeqTyp", "ISIS");
                } else if ( (isValueSet(seriesDesc) && seriesDesc.toUpperCase().contains("STEAM")) || (isValueSet(protocolName) && protocolName.toUpperCase().contains("STEAM"))) {
                    extractedFields.put("ImgPulseSeqTyp", "STEAM");
                }

                String baselineCorrection = getTagValue(fileInfoDicom, "0018,9067");
                if (isValueSet(baselineCorrection)) {
                    if (baselineCorrection.equalsIgnoreCase("yes")) {
                        extractedFields.put("ImgB0SuscDistortCorrInd", "Yes");
                    } else if (baselineCorrection.equalsIgnoreCase("none")) {
                        extractedFields.put("ImgB0SuscDistortCorrInd", "No");
                    }
                }

                String waterRefPhaseCor = getTagValue(fileInfoDicom, "0018,9199");
                String spectSelectedSuppr = getTagValue(fileInfoDicom, "0018,9025");
                if ( (isValueSet(waterRefPhaseCor) && waterRefPhaseCor.equalsIgnoreCase("yes"))
                        || (isValueSet(spectSelectedSuppr) && spectSelectedSuppr.equalsIgnoreCase("water"))) {
                    extractedFields.put("SpectroscopyPostPrResidWRemInd", "Yes");
                }
            }
        }

        private void extractDicomDtiHeaderInfo(HashMap<String, String> extractedFields, ModelImage img, FileInfoDicom fileInfoDicom) {
            if (isMRModality(fileInfoDicom, fsData.getStructInfo().getShortName()) && isDtiImagingStructure(fsData.getStructInfo().getShortName())) {
//                if (img.getExtents().length > 3) {
//                    extractedFields.put("ImgDim4ExtentTyp", "DTI directions");
//                }

                // if reading the dicom filled in the dti params, use them to fill in the direction info
                DTIParameters dtiParam = img.getDTIParameters();

                if (dtiParam != null) {
                    // save extracted dti info for later verification before submission file generation
                    dicomDtiHeaderData.put(img.getImageDirectory() + File.separator + img.getImageFileName(), dtiParam);

                    double[] bvals = dtiParam.getbValues();

                    if (bvals != null) {
                        ArrayList<Double> seenBvals = new ArrayList<Double>();

                        int numDirections = 0;
                        int bvalCount = 0;
                        Arrays.sort(bvals);
                        for (int i = 0; i < bvals.length; i++) {
                            // increased threshold from 0 to 5, since some Siemens data has b-values +/- 5-20. b0s seem to be either 0 or 5.
                            if (bvals[i] > 5) {
                                numDirections++;

                                if ( !seenBvals.contains(bvals[i])) {
                                    boolean closeMatch = false;
                                    for (Double bval : seenBvals) {
                                        if (Math.abs(bvals[i] - bval) <= 20) {
                                            closeMatch = true;
                                            
                                            // prefer this bval if it is a round number
                                            if (bvals[i] % 100 == 0) {
                                                seenBvals.add(bvals[i]);
                                                seenBvals.remove(bval);
                                            }
                                        }
                                    }
                                    
                                    if (!closeMatch) {
                                        seenBvals.add(bvals[i]);
                                    }
                                }
                            }
                        }
                        
                        for (Double bval : seenBvals) {
                            bvalCount++;

                            if (bvalCount == 1) {
                                extractedFields.put("ImgDiffusionFirstBVal", "" + bval);
                            } else if (bvalCount == 2) {
                                extractedFields.put("ImgDiffusionSecondBVal", "" + bval);
                            } else if (bvalCount == 3) {
                                extractedFields.put("ImgDiffusionThirdBVal", "" + bval);
                            } else if (bvalCount == 4) {
                                extractedFields.put("ImgDiffusionFourthBVal", "" + bval);
                            } else if (bvalCount == 5) {
                                extractedFields.put("ImgDiffusionFifthBVal", "" + bval);
                            } else if (bvalCount == 6) {
                                extractedFields.put("ImgDiffusionSixthBVal", "" + bval);
                            } else {
                                System.err.println("Found more than 6 bVals: " + bvalCount + " " + bval);
                            }
                        }
                        
                        extractedFields.put("ImgDiffusionBValCt", "" + bvalCount);

                        extractedFields.put("ImgDiffusionDirCt", "" + numDirections);
                    } else {
                        System.err.println("No DICOM embedded DTI bval information found.");
                        if (cmdLineCsvFlag) {
                            readErrorFileOut.println("No DICOM embedded DTI bval information found.");
                        }
                    }
                } else {
                    System.err.println("No DICOM embedded DTI information found.");
                    if (cmdLineCsvFlag) {
                        readErrorFileOut.println("No DICOM embedded DTI information found.");
                    }
                }
            }
        }

        private void extractDicomFMRIHeaderInfo(HashMap<String, String> extractedFields, FileInfoDicom fileInfoDicom) {
            if (isMRModality(fileInfoDicom, fsData.getStructInfo().getShortName()) && isFMRIImagingStructure(fsData.getStructInfo().getShortName())) {
                String seriesDescription = getTagValue(fileInfoDicom, "0008,103E");
                if (isValueSet(seriesDescription) && seriesDescription.toLowerCase().contains("rest")) {
                    extractedFields.put("ImgFMRITaskTyp", "Rest");
                }
            }
        }

        private void extractDicomCTHeaderInfo(HashMap<String, String> extractedFields, FileInfoDicom fileInfoDicom) {
            if (isCTModality(fileInfoDicom, fsData.getStructInfo().getShortName())) {
                extractedFields.put("ImgCTkVp", getTagValue(fileInfoDicom, "0018,0060"));
                extractedFields.put("ImgCTmA", getTagValue(fileInfoDicom, "0018,1151"));
            }
        }

        private void extractNiftiHeaderInfo(HashMap<String, String> extractedFields, FileInfoNIFTI fInfo) {
            // Description = Philips Medical Systems Achieva 3.2.1 (from .nii T1 of Dr. Vaillancourt's)
            final FileInfoNIFTI fileInfoNifti = (FileInfoNIFTI) fInfo;
            final String description = fileInfoNifti.getDescription();

            extractedFields.put("ImgScannerManufName", convertNiftiDescToBRICSManuf(description));
            extractedFields.put("ImgScannerModelName", convertNiftiDescToBRICSModel(description));
            extractedFields.put("ImgScannerSftwrVrsnNum", convertNiftiDescToBRICSVer(description));
        }

        private String extractPulseSequence(FormStructureData fsData, ModelImage img, FileInfoBase fileInfo) {
            if (isMRModality(fileInfo, fsData.getStructInfo().getShortName()) && isDtiImagingStructure(fsData.getStructInfo().getShortName())) {
                return "DTI";
            } else if (isMRModality(fileInfo, fsData.getStructInfo().getShortName()) && isFMRIImagingStructure(fsData.getStructInfo().getShortName())) {
                return "fMRI";
            }

            // TODO use other information to guess MR pulse sequence?

            return null;
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
                                ((JComboBox<String>) deVal.getComp()).setSelectedItem(null);
                            } else if (deVal.getComp() instanceof JList) {
                                ((JList<String>) deVal.getComp()).clearSelection();
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
                    fixErrors = JOptionPane.showOptionDialog(null, errors.toString(), "Warning", JOptionPane.DEFAULT_OPTION, JOptionPane.WARNING_MESSAGE, null, options,
                            options[0]);
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

                    // fileChooser.setMulti(ViewUserInterface.getReference().getLastStackFlag());
                    fileChooser.setMulti(true);

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

                        boolean isSpectroscopy = isSpectroscopyImagingStructure(dataStructureName);
                        FileInfoBase[] spectroscopyHeaderList = null;

                        if (file.getName().endsWith(".zip") || file.getName().endsWith(".tar.gz") || file.getName().endsWith(".tgz")) {
                            // if the user selects a zip file containing a dataset, try to open it as if pointed to from
                            // CSV
                            srcImage = readImgFromCSV(file.getParent(), file.getName());
                        } else {
                            final FileIO fileIO = new FileIO();
                            fileIO.setQuiet(true);

                            if ( !isSpectroscopy) {
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
                            } else {
                                // read spect header, map onto data elements
                                int[] extents = new int[] {256, 256};

                                if (isMultiFile) {
                                    String[] fileList = FileUtility.getFileList(file.getParent() + File.separator, file.getName(), false);

                                    spectroscopyHeaderList = new FileInfoBase[fileList.length];

                                    for (int i = 0; i < fileList.length; i++) {
                                        spectroscopyHeaderList[i] = fileIO.readHeader(file.getParent() + File.separator + fileList[i], isMultiFile);

                                        if (spectroscopyHeaderList[i] instanceof FileInfoDicom) {
                                            ((FileInfoDicom) spectroscopyHeaderList[i]).setInfoFromTags();

                                            String rowStr = (String) ((FileInfoDicom) spectroscopyHeaderList[i]).getTagTable().getValue("0028,0010");
                                            String colStr = (String) ((FileInfoDicom) spectroscopyHeaderList[i]).getTagTable().getValue("0028,0011");
                                            if (rowStr != null && colStr != null) {
                                                int rows = Integer.parseInt(rowStr);
                                                int cols = Integer.parseInt(colStr);
                                                extents = new int[] {cols, rows, fileList.length};
                                                spectroscopyHeaderList[i].setExtents(extents);
                                            }
                                        }
                                    }
                                } else {
                                    spectroscopyHeaderList = new FileInfoBase[1];
                                    spectroscopyHeaderList[0] = fileIO.readHeader(file.getAbsolutePath(), isMultiFile);

                                    if (spectroscopyHeaderList[0] instanceof FileInfoDicom) {
                                        ((FileInfoDicom) spectroscopyHeaderList[0]).setInfoFromTags();

                                        String rowStr = (String) ((FileInfoDicom) spectroscopyHeaderList[0]).getTagTable().getValue("0028,0010");
                                        String colStr = (String) ((FileInfoDicom) spectroscopyHeaderList[0]).getTagTable().getValue("0028,0011");
                                        if (rowStr != null && colStr != null) {
                                            int rows = Integer.parseInt(rowStr);
                                            int cols = Integer.parseInt(colStr);
                                            extents = new int[] {cols, rows};
                                            spectroscopyHeaderList[0].setExtents(extents);
                                        }
                                    }
                                }

                                ModelImage blankImg = ViewUserInterface.getReference().createBlankImage(spectroscopyHeaderList[0], false, false);

                                previewImg = new ViewJComponentPreviewImage(blankImg, extents, owner);
                                int slice = 0;
                                previewImg.createImg(slice);

                                previewImgPanel.removeAll();
                                previewImgPanel.repaint();

                                previewImgPanel.add(previewImg);

                                addedPreviewImage = true;

//                                ModelImage thumbnailImage = createThumbnailImage(srcImage);

                                if (launchedFromInProcessState) {
                                    final int selectedRow = structTable.getSelectedRow();
                                    previewImages.set(selectedRow, previewImg);
                                    previewImages.get(selectedRow).setSliceBrightness(previewImgBrightness, previewImgContrast);

                                    ImgFileInfo imgInfo = new ImgFileInfo(file.getAbsolutePath(), isMultiFile, FileUtility.getFileNameList(spectroscopyHeaderList),
                                            spectroscopyHeaderList[0].getFileFormat(), null);

                                    structRowImgFileInfoList.set(selectedRow, imgInfo);
                                } else {
                                    final int size = previewImages.size();
                                    previewImages.set(size - 1, previewImg);
                                    previewImages.get(size - 1).setSliceBrightness(previewImgBrightness, previewImgContrast);

                                    ImgFileInfo imgInfo = new ImgFileInfo(file.getAbsolutePath(), isMultiFile, FileUtility.getFileNameList(spectroscopyHeaderList),
                                            spectroscopyHeaderList[0].getFileFormat(), null);

                                    structRowImgFileInfoList.set(size - 1, imgInfo);
                                }

                                // cleanup blank thumbnail modelimage
                                if (blankImg != null) {
                                    blankImg.disposeLocal();
                                    blankImg = null;
                                }

                                previewImgPanel.validate();
                                previewImgPanel.repaint();
                            }
                        }

                        if (srcImage != null) {
                            // basic check that image data is de-identified
                            Vector<FileDicomTag> problemTags = deidentificationCheckDicomTags(srcImage.getFileInfo());
                            if (problemTags != null) {
                                isDeidentified = deidentificationDialogDicom(srcImage.getFileInfo(0).getFileDirectory(), srcImage.getFileInfo(0).getFileName(), problemTags, false);
                            } else {
                                // either no problem tags found, or non-dicom format
                                isDeidentified = true;
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
                                populateFields(fsData, srcImage, srcImage.getFileInfo(0));
                            }

                            srcImage.disposeLocal();
                            srcImage = null;
                        } else if (isSpectroscopy) {
                            // basic check that image data is de-identified
                            Vector<FileDicomTag> problemTags = deidentificationCheckDicomTags(spectroscopyHeaderList);
                            if (problemTags != null) {
                                isDeidentified = deidentificationDialogDicom(spectroscopyHeaderList[0].getFileDirectory(), spectroscopyHeaderList[0].getFileName(), problemTags,
                                        false);
                            } else {
                                // either no problem tags found, or non-dicom format
                                isDeidentified = true;
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
                                populateFields(fsData, null, spectroscopyHeaderList[0]);
                            }
                        }
                    }
                } else if (isImagingStructure(dataStructureName) && isBValFileElement(groupName, deName)) {
                    // get bval file, read it, map onto data elements, and set GUI components
                    final JFileChooser chooser = new JFileChooser();
                    chooser.setDialogTitle("Choose BVal file");
                    chooser.setMultiSelectionEnabled(false);
                    chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
                    chooser.setFileFilter(new FileNameExtensionFilter("Diffusion B-value file (.bval)", "bval"));
                    final int returnValue = chooser.showOpenDialog(this);
                    if (returnValue == JFileChooser.APPROVE_OPTION) {
                        final File file = chooser.getSelectedFile();

                        HashMap<String, String> bvalDeTable = readBvalFile(file);

                        if (bvalDeTable == null) {
                            MipavUtil.displayWarning("Unable to read contents of BVal file: " + file.getAbsolutePath());
                        } else {
                            for (final DataElementValue deVal : fsData.getGroupRepeat(groupName, repeatNum).getDataElements()) {
                                String deNameWithGroup = IMG_DIFF_GROUP + "." + deVal.getName();
                                if (deVal.getName().equalsIgnoreCase(deName)) {
                                    final JTextField tf = (JTextField) deVal.getComp();
                                    tf.setText(file.getAbsolutePath());
                                    tf.setEnabled(false);
//                                    break;
                                } else if (bvalDeTable.containsKey(deNameWithGroup)) {
                                    final JTextField tf = (JTextField) deVal.getComp();
                                    tf.setText(bvalDeTable.get(deNameWithGroup));
//                                    tf.setEnabled(false);
//                                    break;
                                }
                            }
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

                final int response = JOptionPane.showConfirmDialog(this, "Are you sure you want to remove the last repeat?", "Remove repeat?", JOptionPane.YES_NO_OPTION,
                        JOptionPane.QUESTION_MESSAGE);

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
                            value = (String) ( ((JComboBox<String>) deComp).getSelectedItem());
                        } else if (deComp instanceof JList) {
                            value = "";
                            final int[] selectedIndicies = ((JList<String>) deComp).getSelectedIndices();
                            for (final int index : selectedIndicies) {
                                if (value == "") {
                                    value = (String) ((JList<String>) deComp).getModel().getElementAt(index);
                                } else {
                                    value += MULTI_SELECT_VALUE_DELIM + (String) ((JList<String>) deComp).getModel().getElementAt(index);
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
                                        errs.add(title + " must be in the range of " + deInfo.getMinimumValue().floatValue() + " to " + deInfo.getMaximumValue().floatValue());
                                        errors.add(deVal);
                                    } else if (deInfo.getMaximumValue() != null && floatValue > deInfo.getMaximumValue().floatValue()) {
                                        errs.add(title + " must be in the range of " + deInfo.getMinimumValue().floatValue() + " to " + deInfo.getMaximumValue().floatValue());
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
                            value = (String) ( ((JComboBox<String>) comp).getSelectedItem());
                            if (value.equalsIgnoreCase(VALUE_OTHER_SPECIFY) || value.equalsIgnoreCase(VALUE_YES_SPECIFY)) {
                                // value = deVal.getOtherSpecifyField().getText().trim();
                            }
                        } else if (comp instanceof JList) {
                            value = "";
                            final int[] selectedIndicies = ((JList<String>) comp).getSelectedIndices();
                            for (final int index : selectedIndicies) {
                                if (value == "") {
                                    value = (String) ((JList<String>) comp).getModel().getElementAt(index);
                                } else {
                                    value += MULTI_SELECT_VALUE_DELIM + (String) ((JList<String>) comp).getModel().getElementAt(index);
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

    private static final Vector<FileDicomTag> deidentificationCheckDicomTags(FileInfoBase[] fInfoList) {
        FileInfoBase fInfo = fInfoList[0];

        if (fInfo instanceof FileInfoDicom) {
            Vector<FileDicomTag> problemTags = new Vector<FileDicomTag>();
            // for (FileInfoBase fInfo : img.getFileInfo()) {
            FileInfoDicom dicomInfo = (FileInfoDicom) fInfo;

            FileDicomTagTable tagTable = dicomInfo.getTagTable();
            
            Vector<FileDicomTag> sequenceTags = new Vector<FileDicomTag>();
            Hashtable<FileDicomKey, FileDicomTag> tagList = tagTable.getTagList();
            for (FileDicomTag tag : tagList.values()) {
                if (tag.getType() == FileDicomTagInfo.VR.SQ) {
                    sequenceTags.add(tag);
                }
            }

            for (String anonTagKey : anonymizeTagIDs) {
                FileDicomTag tag = tagTable.get(anonTagKey);
                if (tag != null && tag.getValue(true) != null && ! ((String) tag.getValue(true)).trim().equals("")) {
                    problemTags.add(tag);
                }
                
                // also check for each tag within sequence tags to make sure they are not buried
                for (FileDicomTag seqTag : sequenceTags) {
                    // if this tag has already been added because of another pii tag, we can skip further searches
                    if (!problemTags.contains(seqTag) && doesTagExistWithValue(anonTagKey, seqTag)) {
                        problemTags.add(seqTag);
                    }
                    
//                    FileDicomSQ seqVal = (FileDicomSQ) seqTag.getValue(false);
//                    Vector<FileDicomSQItem> seqItems = seqVal.getSequence();
//                    for (FileDicomSQItem item : seqItems) {
//                        for (FileDicomTag curTag : item.getTagList().values()) {
//                            if (curTag.getType() == FileDicomTagInfo.VR.SQ) {
//                                // go into sub sequence
//                                FileDicomSQ subSeqVal = (FileDicomSQ) curTag.getValue(false);
//                                Vector<FileDicomSQItem> subSeqItems = subSeqVal.getSequence();
//                                for (FileDicomSQItem subItem : subSeqItems) {
//                                    for (FileDicomTag subCurTag : subItem.getTagList().values()) {
//                                        if (subCurTag.getType() == FileDicomTagInfo.VR.SQ) {
//                                            // TODO go into sub sequence?
//                                        } else {
//                                            if (subCurTag.getKey().toString().equals(anonTagKey) && subCurTag.getValue(true) != null && ! ((String) subCurTag.getValue(true)).trim().equals("")) {
//                                                if (!problemTags.contains(seqTag)) {
//                                                    problemTags.add(seqTag);
//                                                }
//                                            }
//                                        }
//                                    }
//                                }
//                            } else {
//                                if (curTag.getKey().toString().equals(anonTagKey) && curTag.getValue(true) != null && ! ((String) curTag.getValue(true)).trim().equals("")) {
//                                    if (!problemTags.contains(seqTag)) {
//                                        problemTags.add(seqTag);
//                                    }
//                                }
//                            }
//                        }
//                    }
                }
            }
            // }

            return problemTags;
        }

        return null;
    }
    
    private static final boolean doesTagExistWithValue(final String needleTagKey, final FileDicomTag haystackTag) {
        if (haystackTag.getType() == FileDicomTagInfo.VR.SQ) {
            Object value = haystackTag.getValue(false);
            
            if (value instanceof FileDicomSQ) {
                FileDicomSQ seqVal = (FileDicomSQ) value;
                Vector<FileDicomSQItem> seqItems = seqVal.getSequence();
                for (FileDicomSQItem item : seqItems) {
                    for (FileDicomTag curTag : item.getTagList().values()) {
                        if (doesTagExistWithValue(needleTagKey, curTag)) {
                            return true;
                        }
                    }
                }
            } else {
                return true;
            }
        } else {
            if (haystackTag.getKey().toString().equals(needleTagKey) && haystackTag.getValue(true) != null && ! ((String) haystackTag.getValue(true)).trim().equals("")) {
                return true;
            }
        }
        
        return false;
    }

    private boolean deidentificationDialogDicom(String fDir, String fName, Vector<FileDicomTag> problemTags, boolean fromCsv) {
        if (problemTags != null) {
            if (problemTags.size() > 0) {
                String disclaimerText = new String("<html>"
                        + "The table below lists fields in the loaded image data with potential Personally Identifiable Information (PII) or Protected Health Information."
                        + "<br><br>" + "Please review all the fields below.  If any fields contain PII/PHI, exit the Imaging Tool and fully de-identify your image data."
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
                okButton.setFont(serif12B);
                okButton.setActionCommand("okayPII");
                okButton.addActionListener(this);
                JButton cancelButton = new JButton("Exit the Imaging Tool");
                cancelButton.setFont(serif12B);
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

                if (fromCsv) {
                    JCheckBox csvOptionCheckbox = new JCheckBox("Do not ask again for images in this CSV/BIDS dataset.  I have reviewed all of them for PII/PHI.",
                            csvDeidentDontAsk);
                    csvOptionCheckbox.setFont(serif12B);
                    csvOptionCheckbox.setActionCommand("dontAskPII");
                    csvOptionCheckbox.addActionListener(this);

                    gbc.weightx = 0;
                    gbc.weighty = 0;
                    gbc.gridy++;
                    gbc.fill = GridBagConstraints.NONE;
                    gbc.anchor = GridBagConstraints.SOUTH;
                    dialogPanel.add(csvOptionCheckbox, gbc);
                }

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

    private static final String getTagValue(FileInfoDicom fInfo, String tag) {
        return (String) (fInfo.getTagTable().getValue(tag));
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

                if (ddServerURL == null || authServerURL == null) {
                    DictionaryConfigItem config = getDictionaryConfig(selectedDictionaryInstance, selectedDictionaryEnv);
                    
                    if (config.ddServer.equals("") || config.authServer.equals("")) {
                        MipavUtil.displayError("No BRICS Data Dictionary configuration found for instance " + selectedDictionaryInstance + " " + selectedDictionaryEnv + ".");
                        parent.dispose();
                        if (JDialogStandalonePlugin.isExitRequired()) {
                            System.gc();
                            System.exit(0);
                        }
                        return;
                    } else {
                        ddServerURL = config.ddServer;
                        authServerURL = config.authServer;
                    }
                }
                
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

                if (ddServerURL == null || authServerURL == null) {
                    DictionaryConfigItem config = getDictionaryConfig(selectedDictionaryInstance, selectedDictionaryEnv);
                    
                    if (config.ddServer.equals("") || config.authServer.equals("")) {
                        MipavUtil.displayError("No BRICS Data Dictionary configuration found for instance " + selectedDictionaryInstance + " " + selectedDictionaryEnv + ".");
                        parent.dispose();
                        if (JDialogStandalonePlugin.isExitRequired()) {
                            System.gc();
                            System.exit(0);
                        }
                        return;
                    } else {
                        ddServerURL = config.ddServer;
                        authServerURL = config.authServer;
                    }
                }
                
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
                ((JComboBox<String>) deComp).addActionListener(new OtherSpecifyListener(otherSpecifyField));
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

        public GroupRepeat(final RepeatableGroup groupInfo, final FormStructureData parentStruct, final Vector<DataElementValue> dataElements, final int repeatNumber) {
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
            return (structInfo.getRepeatableGroupByName(groupName) != null) && (structInfo.getRepeatableGroupByName(groupName).getMapElementByName(deName) != null);
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
                otherSpecifyField.setVisible(
                        ((JComboBox<String>) source).getSelectedItem().equals(VALUE_OTHER_SPECIFY) || ((JComboBox<String>) source).getSelectedItem().equals(VALUE_YES_SPECIFY));
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

        public ThumbnailData thumbnailImgData;

        public boolean isMultifile;

        public String imgFilePath;

        public int fileFormat;

        /*
         * public ImgFileInfo(final String imgFilePath, final boolean isMultifile) { super(); this.isMultifile =
         * isMultifile; this.imgFilePath = imgFilePath; }
         */

        public ImgFileInfo(final String imgFilePath, final boolean isMultifile, final List<String> origFiles, final int format, final ThumbnailData thumbnailImgData) {
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

        public ThumbnailData getThumbnailImgData() {
            return thumbnailImgData;
        }

        public void setThumbnailImgData(final ThumbnailData thumbnailImgData) {
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

    private boolean loadDicomDir(FileDicomTagTable tagTable) {
        // choose DICOMDIR file and load it

        FileDicom dicomDirReader;

        JFileChooser chooser;
        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser = new JFileChooser(ViewUserInterface.getReference().getDefaultDirectory());
        } else {
            chooser = new JFileChooser(System.getProperties().getProperty("user.dir"));
        }

        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        chooser.setDialogTitle("Open the DICOMDIR file.");

        int returnValue = chooser.showOpenDialog(this);
        File file = null;

        if (returnValue == ViewFileChooserSubsample.APPROVE_OPTION) {
            file = chooser.getSelectedFile();
        }

        if (file != null) {
            if (file.getName().toLowerCase().startsWith("dicomdir")) {
                try {
                    dicomDirReader = new FileDicom(file.getName(), file.getParent() + File.separatorChar);
                    dicomDirReader.readHeader(true);
                } catch (IOException e) {
                    MipavUtil.displayError(e.toString());
                    return false;
                }
            } else {
                MipavUtil.displayError("File must be named 'DICOMDIR'");
                return false;
            }
        } else {
            return false;
        }

        // get DICOMDIR info and parse into individual volumes/series

        FileDicomSQ dirInfo = dicomDirReader.getDirFileInfo();
        FileInfoDicom dicomInfo = (FileInfoDicom) dicomDirReader.getFileInfo();

        DefaultMutableTreeNode base = new DefaultMutableTreeNode("DICOMDIR");
        DefaultMutableTreeNode patient = null;
        DefaultMutableTreeNode study = null;
        DefaultMutableTreeNode series = null;
        DefaultMutableTreeNode image = null;

        final String itemTypeTag = "0004,1430";

        Vector<DicomDirItem> dicomDirEntries = new Vector<DicomDirItem>();
        DicomDirItem curItem = null;

        for (int i = 0; i < dirInfo.getSequenceLength(); i++) {
            String currentItemType = dirInfo.getItem(i).get(itemTypeTag).getValue(true).toString();
            if (currentItemType.startsWith("PATIENT")) {
                curItem = new DicomDirItem();
                curItem.setPatient(dirInfo.getItem(i).get("0010,0020").getValue(true).toString());
                dicomDirEntries.add(curItem);
            } else if (currentItemType.startsWith("STUDY")) {
                curItem.setStudy(dirInfo.getItem(i).get("0020,0010").getValue(true).toString());
            } else if (currentItemType.startsWith("SERIES")) {
                curItem.setSeries(dirInfo.getItem(i).get("0020,0011").getValue(true).toString());
            } else if (currentItemType.startsWith("IMAGE")) {
                curItem.addImageFile(dirInfo.getItem(i).get("0004,1500").getValue(true).toString());
            }
        }

        base.toString();

        /*
         * current = (FileDicomTagTable) currentNode.getUserObject(); String currentItemType =
         * current.get("0004,1430").getValue(true).toString(); if (currentItemType.startsWith("PATIENT")) { return
         * "Patient- " +current.get("0010,0020").getValue(true).toString(); } else
         * if(currentItemType.startsWith("STUDY")) { return "Study "
         * +current.get("0020,0010").getValue(true).toString().trim() + " (UID-" +
         * current.get("0020,000D").getValue(true).toString().trim()+")"; } else if
         * (currentItemType.startsWith("SERIES")) { return "Series "
         * +current.get("0020,0011").getValue(true).toString().trim() + " (UID-" +
         * current.get("0020,000E").getValue(true).toString().trim()+")"; } else if(currentItemType.startsWith("IMAGE"))
         * { return "Image " +current.get("0020,0013").getValue(true).toString() + " (" +
         * current.get("0004,1500").getValue(true).toString().trim()+")"; }
         */

        return true;
    }

    private class DicomDirItem {
        private String patient;

        private String study;

        private String series;

        private Vector<String> imageFiles;

        public DicomDirItem() {
            super();
            this.imageFiles = new Vector<String>();
        }

        public DicomDirItem(String patient, String study, String series) {
            super();
            this.patient = patient;
            this.study = study;
            this.series = series;
            this.imageFiles = new Vector<String>();
        }

        public DicomDirItem(String patient, String study, String series, Vector<String> imageFiles) {
            super();
            this.patient = patient;
            this.study = study;
            this.series = series;
            this.imageFiles = imageFiles;
        }

        public String getPatient() {
            return patient;
        }

        public void setPatient(final String patient) {
            this.patient = patient;
        }

        public String getStudy() {
            return study;
        }

        public void setStudy(final String study) {
            this.study = study;
        }

        public String getSeries() {
            return series;
        }

        public void setSeries(final String series) {
            this.series = series;
        }

        public Vector<String> getImageFiles() {
            return imageFiles;
        }

        public void setImageFiles(final Vector<String> imageFiles) {
            this.imageFiles = imageFiles;
        }

        public void addImageFile(final String file) {
            imageFiles.add(file);
        }
    }

    private class CmdLineOutputStream extends PrintStream {
        PrintStream logFileStream;

        public CmdLineOutputStream(PrintStream out, PrintStream logStream) {
            super(out);

            logFileStream = logStream;
        }

//        public CmdLineOutputStream(PrintStream out1, PrintStream out2) {
//            super(out1);
//            this.out = out2;
//        }

        public void write(byte buf[], int off, int len) {
            try {
                super.write(buf, off, len);
                logFileStream.write(buf, off, len);
            } catch (Exception e) {}
        }

        public void flush() {
            super.flush();
            logFileStream.flush();
        }
    }

    private void logError(String msg) {
        if ( !cmdLineCsvFlag) {
            MipavUtil.displayError(msg);
        } else {
            System.err.println(msg);
            readErrorFileOut.println(msg);
        }
    }

    private boolean checkRecordsForSubmission() {
        // when running from CSV, check to make sure that the number of records in the final list matches the number read in from the CSV
        if (csvRecordCount != -1 && csvRecordCount != structTableModel.getRowCount()) {
            final int response = JOptionPane.showConfirmDialog(this, "Not all records from the selected CSV were successfully processed. Contact the Operations or Imaging Team for help debugging the issue (" + structTableModel.getRowCount() + " / " + csvRecordCount + " records). Continue anyway?", "Error processing all CSV records", JOptionPane.YES_NO_OPTION, JOptionPane.ERROR_MESSAGE);
            
            if (response == JOptionPane.NO_OPTION) {
                return false;
            }
        }
        
        // check that required fields have some value
        final int numRows = structTableModel.getRowCount();
        for (int i = 0; i < numRows; i++) {
            if ( ((String) structTableModel.getValueAt(i, 1)).equalsIgnoreCase("No")) {
                MipavUtil.displayError("Record " + (i + 1) + ": Not all required fields have values");
                return false;
            }
        }

        // require that the GUIDs are filled in and have proper prefixes
        for (int i = 0; i < numRows; i++) {
            final String struct = (String) structTableModel.getValueAt(i, 0);
            if (struct.endsWith(STRUCT_GUID_SEPERATOR + "UNKNOWNGUID")) {
                MipavUtil.displayError("Record " + (i + 1) + ": No GUID specified");
                return false;
            } else {
                final String guidTester = struct.substring(struct.lastIndexOf(STRUCT_GUID_SEPERATOR) + 3, struct.length());
                if ( !isGuid(guidTester)) {
                    MipavUtil.displayError("Record " + (i + 1) + ": GUID value does not match the allow GUID format (" + guidTester + ")");
                    return false;
                }
            }
        }

        // check that any field with an 'Other, specify' or 'Yes, specify' value in a required field has something in
        // the sister field
        for (int i = 0; i < numRows; i++) {
            final FormStructureData fsData = fsDataList.get(i);
            for (final RepeatableGroup group : fsData.getStructInfo().getRepeatableGroups()) {
                for (final GroupRepeat repeat : fsData.getAllGroupRepeats(group.getName())) {
                    for (final DataElementValue deVal : repeat.getDataElements()) {
                        if (deVal.getRequiredType() == RequiredType.REQUIRED) {
                            JComponent comp = deVal.getComp();
                            if (comp instanceof JComboBox) {
                                final JComboBox jc = (JComboBox) comp;
                                String selectedVal = (String) jc.getSelectedItem();
                                if (selectedVal.trim().equalsIgnoreCase(VALUE_OTHER_SPECIFY) || selectedVal.trim().equalsIgnoreCase(VALUE_YES_SPECIFY)) {
                                    // check for a value in the matching OTH field in the same group
                                    if (deVal.getOtherSpecifyField() == null || deVal.getOtherSpecifyField().getText().equals("")) {
                                        MipavUtil.displayError("Record " + (i + 1) + ": 'Other, specify' or 'Yes, specify' field value found without specified value ("
                                                + deVal.getGroupName() + "." + deVal.getName() + ")");
                                        return false;
                                    }
                                }
                            } else if (comp instanceof JList) {
                                final JList<String> list = (JList<String>) comp;

                                List<String> selectedValList = list.getSelectedValuesList();

                                for (final String val : selectedValList) {
                                    if (val.trim().equalsIgnoreCase(VALUE_OTHER_SPECIFY) || val.trim().equalsIgnoreCase(VALUE_YES_SPECIFY)) {
                                        // check for a value in the matching OTH field in the same group
                                        if (deVal.getOtherSpecifyField() == null || deVal.getOtherSpecifyField().getText().equals("")) {
                                            MipavUtil.displayError("Record " + (i + 1) + ": Required 'Other, specify' or 'Yes, specify' field value found without specified value ("
                                                    + deVal.getGroupName() + "." + deVal.getName() + ")");
                                            return false;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // if DTI, check that the dicoms have the gradient info, etc or bval/bvec files are attached
        // TODO verify that all the most common DICOM DTI tags are being checked in the DICOM reader
        for (int i = 0; i < numRows; i++) {
            final FormStructureData fsData = fsDataList.get(i);
            if (isDtiImagingStructure(fsData.getStructInfo().getShortName())) {
                boolean rowHasDtiInfo = false;
                String imgFile = null;
                boolean foundBval = false;
                boolean foundBvec = false;
                BVALVEC_SEARCH: for (final RepeatableGroup group : fsData.getStructInfo().getRepeatableGroups()) {
                    if (group.getName().equals(IMG_DIFF_GROUP)) {
                        for (final GroupRepeat repeat : fsData.getAllGroupRepeats(group.getName())) {
                            for (final DataElementValue deVal : repeat.getDataElements()) {
                                // require that both bval and bvec fields are filled
                                if (deVal.getName().equals(IMG_DIFF_BVAL_ELEMENT_NAME) && new File(deVal.getValue()).exists()) {
                                    if (foundBvec) {
                                        rowHasDtiInfo = true;
                                        break BVALVEC_SEARCH;
                                    } else {
                                        foundBval = true;
                                    }
                                } else if (deVal.getName().equals(IMG_DIFF_BVEC_ELEMENT_NAME) && new File(deVal.getValue()).exists()) {
                                    if (foundBval) {
                                        rowHasDtiInfo = true;
                                        break BVALVEC_SEARCH;
                                    } else {
                                        foundBvec = true;
                                    }
                                }
                            }
                        }
                    }
                }

                ImgFileInfo imgInfo = structRowImgFileInfoList.get(i);
                imgFile = imgInfo.getOrigFiles().get(0);

                // check dicom header for different manuf gradient/etc tags - may need to save this when doing initial
                // img extraction
                if (imgFile != null && dicomDtiHeaderData.containsKey(imgFile)) {
                    // TODO what values should be checked?
                    DTIParameters dtiParam = dicomDtiHeaderData.get(imgFile);
                    if (dtiParam != null && dtiParam.getbValues() != null && (dtiParam.getbMatrixVals() != null || dtiParam.getGradients() != null)) {
                        rowHasDtiInfo = true;
                    }
                }

                if ( !rowHasDtiInfo) {
                    MipavUtil.displayError("Record " + (i + 1) + ": No DTI B-value/Gradient/Matrix information found in image header or bval/bvec files");
                    return false;
                }
            }
        }

        return true;
    }

    private void setFormImpliedRecordDataElements(final FormStructureData fsData, final ModelImage img, final ArrayList<String> repeatValues) {
        if (isFMRIImagingStructure(fsData.getStructInfo().getShortName())) {
            boolean foundDE = false;
            for (int i = 0; i < csvFieldNames.size(); i++) {
                if (csvFieldNames.get(i).equalsIgnoreCase(IMG_MR_GROUP + "." + IMG_PULSE_SEQ_ELEMENT_NAME)) {
                    foundDE = true;
                    if (repeatValues.get(i).trim().equals("")) {
                        repeatValues.set(i, "fMRI");
                    }
                }
            }

            if ( !foundDE) {
                csvFieldNames.add(IMG_MR_GROUP + "." + IMG_PULSE_SEQ_ELEMENT_NAME);
                repeatValues.add("fMRI");
            }
        } else if (isDtiImagingStructure(fsData.getStructInfo().getShortName())) {
            boolean foundDE = false;
            for (int i = 0; i < csvFieldNames.size(); i++) {
                if (csvFieldNames.get(i).equalsIgnoreCase(IMG_MR_GROUP + "." + IMG_PULSE_SEQ_ELEMENT_NAME)) {
                    foundDE = true;
                    if (repeatValues.get(i).trim().equals("")) {
                        repeatValues.set(i, "DTI");
                    }
                }
            }

            if ( !foundDE) {
                csvFieldNames.add(IMG_MR_GROUP + "." + IMG_PULSE_SEQ_ELEMENT_NAME);
                repeatValues.add("DTI");
            }

            if (img.getExtents().length > 3) {
                foundDE = false;
                for (int i = 0; i < csvFieldNames.size(); i++) {
                    if (csvFieldNames.get(i).equalsIgnoreCase("Image pixel information and dimensions" + "." + "ImgDim4ExtentTyp")) {
                        foundDE = true;
                        if (repeatValues.get(i).trim().equals("")) {
                            repeatValues.set(i, "DTI directions");
                        }
                    }
                }

                if ( !foundDE) {
                    csvFieldNames.add("Image pixel information and dimensions" + "." + "ImgDim4ExtentTyp");
                    repeatValues.add("DTI directions");
                }
            }
        }
    }

    private String guessPulseSeqFromFileName(final String imgFileName) {
        // TODO pulse seq from BIDS spec not mapped yet:
//        T1 Rho map  T1rho   Quantitative T1rho brain imaging
//        T1 map  T1map   quantitative T1 map
//        T2 map  T2map   quantitative T2 map
//        Proton density map  PDmap   
//        Angiography angio   

        String pulseSeq = null;

        if (imgFileName.contains("_T1w.")) {
            pulseSeq = "T1";
        } else if (imgFileName.contains("_inplaneT1.")) {
            pulseSeq = "T2";
        } else if (imgFileName.contains("_T2w.")) {
            pulseSeq = "T2";
        } else if (imgFileName.contains("_inplaneT2.")) {
            pulseSeq = "T2";
        } else if (imgFileName.contains("_T2star.")) {
            pulseSeq = "GRE";
        } else if (imgFileName.contains("_FLAIR.")) {
            pulseSeq = "FLAIR";
        } else if (imgFileName.contains("_FLASH.")) {
            pulseSeq = "FLASH";
        } else if (imgFileName.contains("_PD.")) {
            pulseSeq = "PD SE";
        } else if (imgFileName.contains("_PDT2.")) {
            pulseSeq = "PD/T2W FSE";
        }

        return pulseSeq;
    }

    private String getNonCollidingNumberedFilePath(String file, int digits) {
        int num = 1;

        File curFile = null;

        boolean done = false;
        while ( !done) {
            String paddedNum = String.format("%02d", num);
            curFile = new File(file + "__" + paddedNum);

            if (curFile.exists()) {
                num++;
            } else {
                done = true;
            }
        }

        return curFile.getPath();
    }

    private class DictionaryConfigItem {
        public String ddServer;
        public String authServer;
        
        public DictionaryConfigItem(final String dd, final String auth) {
            ddServer = dd;
            authServer = auth;
        }
        
        public String toString() {
            return "DD Server: " + ddServer + "\tAuth Server: " + authServer;
        }
    }
    
    protected void readCmdLineDictionarySelection() {
        String errorMessage = "";
        
        if (VariableTable.getReference().isVariableSet(dictionaryInstanceCmdLineVar)) {
            String bricsInstance = VariableTable.getReference().interpolate(dictionaryInstanceCmdLineVar);
            System.err.println("Command line BRICS instance: " + bricsInstance);
            
            try {
                BricsInstance inst = getBricsInstanceFromString(bricsInstance);
                selectedDictionaryInstance = inst;
            } catch (IllegalArgumentException e) {
                e.printStackTrace();
                System.err.println("Using default BRICS instance: " + selectedDictionaryInstance);
                errorMessage += " Instance: " + bricsInstance;
            }
        }
        
        if (VariableTable.getReference().isVariableSet(dictionaryEnvCmdLineVar)) {
            String bricsEnv = VariableTable.getReference().interpolate(dictionaryEnvCmdLineVar);
            System.err.println("Command line BRICS environment: " + bricsEnv);
            
            try {
                BricsEnv env = getBricsEnvFromString(bricsEnv);
                selectedDictionaryEnv = env;
            } catch (IllegalArgumentException e) {
                e.printStackTrace();
                System.err.println("Using default BRICS environment: " + selectedDictionaryEnv);
                errorMessage += " Environment: " + bricsEnv;
            }
        }
        
        if (!errorMessage.equals("")) {
            MipavUtil.displayError("Unrecognized BRICS Data Dictionary selection." + errorMessage + ". Using " + selectedDictionaryInstance + " " + selectedDictionaryEnv + ".");
        }
    }
    
    protected static String getDisplayStrFromBricsInstance(final BricsInstance inst) {
        switch (inst) {
            case FITBIR:
                return "FITBIR";
            case PDBP:
                return "PDBP";
            case NEI_BRICS:
                return "NEI";
            case NINR:
                return "cdRNS";
            case CNRM:
                return "CASA";
            case CISTAR:
                return "CiSTAR";
            case GRDR:
                return "GSDR";
            case COVID19:
                return "COVID-19";
            case NTRR:
                return "NTRR";
            case BRICS:
                return "BRICS";
            default:
                return inst.toString();
        }
    }
    
    protected static BricsInstance getBricsInstanceFromString(final String inst) {
        if (inst.equalsIgnoreCase(BricsInstance.FITBIR.toString())) {
            return BricsInstance.FITBIR;
        } else if (inst.equalsIgnoreCase(BricsInstance.PDBP.toString()) || inst.equalsIgnoreCase("PD")) {
            return BricsInstance.PDBP;
        } else if (inst.equalsIgnoreCase(BricsInstance.NEI_BRICS.toString()) || inst.equalsIgnoreCase("NEI")) {
            return BricsInstance.NEI_BRICS;
        } else if (inst.equalsIgnoreCase(BricsInstance.NINR.toString()) || inst.equalsIgnoreCase("cdRNS")) {
            return BricsInstance.NINR;
        } else if (inst.equalsIgnoreCase(BricsInstance.CNRM.toString())) {
            return BricsInstance.CNRM;
        } else if (inst.equalsIgnoreCase(BricsInstance.CISTAR.toString())) {
            return BricsInstance.CISTAR;
        } else if (inst.equalsIgnoreCase(BricsInstance.GRDR.toString()) || inst.equalsIgnoreCase("GSDR")) {
            return BricsInstance.GRDR;
        } else if (inst.equalsIgnoreCase(BricsInstance.COVID19.toString()) || inst.equalsIgnoreCase("COVID")) {
            return BricsInstance.COVID19;
        } else if (inst.equalsIgnoreCase(BricsInstance.NTRR.toString()) || inst.equalsIgnoreCase("NTI")) {
            return BricsInstance.NTRR;
        } else if (inst.equalsIgnoreCase(BricsInstance.BRICS.toString()) || inst.equalsIgnoreCase("BRICS")) {
            return BricsInstance.BRICS;
        }
        
        throw new IllegalArgumentException("Unknown BRICS instance selection string: " + inst);
    }
    
    protected static BricsEnv getBricsEnvFromString(final String env) {
        if (env.equalsIgnoreCase(BricsEnv.Prod.toString())) {
            return BricsEnv.Prod;
        } else if (env.equalsIgnoreCase(BricsEnv.Demo.toString())) {
            return BricsEnv.Demo;
        } else if (env.equalsIgnoreCase(BricsEnv.Stage.toString())) {
            return BricsEnv.Stage;
        } else if (env.equalsIgnoreCase(BricsEnv.UAT.toString())) {
            return BricsEnv.UAT;
        } else if (env.equalsIgnoreCase(BricsEnv.Dev.toString())) {
            return BricsEnv.Dev;
        } else if (env.equalsIgnoreCase(BricsEnv.Intramural.toString())) {
            return BricsEnv.Intramural;
        }
        
        throw new IllegalArgumentException("Unknown BRICS environment selection string: " + env);
    }
    
    protected String getDiseaseSelection() {
        switch (selectedDictionaryInstance) {
            case PDBP:
                return "Parkinson's Disease";
            case FITBIR:
            case NEI_BRICS:
            case NINR:
            case CNRM:
            case CISTAR:
            case GRDR:
            case COVID19:
            case NTRR:
            case BRICS:
            default:
                return "All Diseases";
        }
    }

    protected void readDictionaryConfig() {
        if (dictionaryConfigTable == null) {
            dictionaryConfigTable = new HashMap<String, DictionaryConfigItem>();
            
            final InputStream jsonStream = getClass().getResourceAsStream(jsonConfigFileName);
            if (jsonStream != null) {
                JSONObject dictJson = readJsonFile(jsonStream);
                
                Iterator<String> keys = dictJson.keys();
                while (keys.hasNext()) {
                    String name = keys.next();
                    try {
                        JSONObject serverVals = dictJson.getJSONArray(name).getJSONObject(0);
                        
                        String ddServ = serverVals.getString("dictionaryServer");
                        String authServ = serverVals.getString("authServer");
                        
                        if (ddServ != null && authServ != null) {
                            dictionaryConfigTable.put(name, new DictionaryConfigItem(ddServ, authServ));
                        }
                    } catch (JSONException e) {
                        e.printStackTrace();
                    }
                }
            }
        }
    }
    
    protected DictionaryConfigItem getDictionaryConfig(final BricsInstance instance, final BricsEnv env) {
//        BricsEnv env = BricsEnv.Prod;
//        for (BricsEnv e : BricsEnv.values()) {
//            if (envName.equalsIgnoreCase(e.toString())) {
//                env = e;
//            }
//        }
//        
//        BricsInstance instance = BricsInstance.FITBIR;
//        for (BricsInstance inst : BricsInstance.values()) {
//            if (instanceName.equalsIgnoreCase(inst.toString())) {
//                instance = inst;
//            }
//        }
        
        DictionaryConfigItem item = dictionaryConfigTable.get(instance.toString() + "_" + env.toString());
        
        //System.err.println(item);
        
        return item;
    }
}
