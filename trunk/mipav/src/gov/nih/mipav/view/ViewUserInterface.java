package gov.nih.mipav.view;


import gov.nih.mipav.plugins.*;
import gov.nih.mipav.util.ThreadUtil;

import gov.nih.mipav.model.algorithms.OpenCLAlgorithmBase;
import gov.nih.mipav.model.dicomcomm.DICOM_Receiver;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.provenance.*;
import gov.nih.mipav.model.provenance.actions.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.actions.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.graphVisualization.JDialogHyperGraph;
import gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork.DTIColorDisplay;
import gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork.DTIPipeline;
import gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork.JPanelDTIFiberTracking;
import gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork.JPanelDTIVisualization;
import gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork.VolumeTriPlanarInterfaceDTI;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JDialogDTIInput;
import gov.nih.mipav.view.dialogs.reportbug.*;

import ij.ImagePlus;
import ij.ImageStack;
import ij.process.ImageProcessor;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.lang.management.*;
import java.lang.reflect.Field;
import java.net.URL;
import java.util.*;
import java.util.regex.Matcher;

import javax.swing.*;
import javax.swing.border.EmptyBorder;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * This class is the _glue_ keeps a record of the present structure of the application. It keeps a list of all the image
 * frames presently being displayed and keeps a hash table of all the images (ModelImage) open in the applicaiton. In
 * addition, this class keeps a reference to the main MIPAV frame and the message frame with much of the imaging results
 * are output.
 * 
 * @version 1.0 June 1, 2005
 */
public class ViewUserInterface implements ActionListener, WindowListener, KeyListener, ScriptRecordingListener,
        CommandLineParser {

    // ~ Static fields/initializers ------------------------------------------------------------------

    /**
     * A reference to the only ViewUserInterface object in MIPAV.
     * 
     * @see #ViewUserInterface()
     * @see #getReference()
     */
    protected static ViewUserInterface userInterfaceReference;

    /** String to use as the progress bar opening prefix. */
    private static final String OPENING_STR = "Opening ";

    /** String to use as the progress bar loading prefix. */
    private static final String LOADING_STR = "Loading ";

    /** Key shortcut editor dialog. */
    private static JDialogShortcutEditor shortcutEd = null;

    /**
     * This boolean tells if the user has provided an inputDir parameter as a command line argument when running a
     * script
     */
    private static boolean providedUserDefaultDir = false;

    /** This is the inputDir path that the user entered as a command line argument when running a script * */
    private static String userDefaultDir = "";

    /**
     * This boolean tells if the user has provided an ouputDir parameter as a command line argument when running a
     * script
     */
    private static boolean providedOutputDir = false;

    /** This is the outputDir path that the user entered as a command line argument when running a script * */
    private static String outputDir = "";

    /** Mipav's optional secondary directory for plugins */
    private static File secondaryPluginsDir;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** The main menu bar that runs MIPAV. */
    protected JFrame mainFrame;

    /** The object used to build and enable/select the menus. */
    protected ViewMenuBuilder menuBuilder;

    /** Message line at the bottom of the mainFrame. */
    protected JTextField messageField;

    /**
     * Message area for multi-line text output. The message area has the ability to save/copy/paste the data to a file
     * or clip board. This frame contains the Global Data, Data, Logging and Debug text areas. The Global and Debug text
     * areas are common to all images (i.e. there is only one). The Data and logging window are unique and associated to
     * their respect image.
     */
    protected ViewJFrameMessage messageFrame;

    /** Initial menubar for MIPAV. */
    protected JMenuBar openingMenuBar;

    /** Matrix for copy/paste actions in image's or between image's matrix edit panel. */
    private TransMatrix clippedMatrix = null;

    /** DOCUMENT ME! */
    private final Vector<Vector<Vector3f>> clippedScannerVectors = new Vector<Vector<Vector3f>>();

    /** Vector to hold clipped VOIs (multiple). */
    private final ViewVOIVector clippedVOIs = new ViewVOIVector();

    /** Vector to hold clipped VOIs (multiple). */
    private Vector<VOIBase> copyVOIList = new Vector<VOIBase>();

    /** String holding the command line arguments for data provenance usage. */
    private String cmdLineArguments = new String();

    /** Reference to the DICOM receiver that listens for DICOM formatted images sent by a DICOM server. */
    private DICOM_Receiver DICOMcatcher = null;

    /** DICOM query frame for sending and receiving DICOM images. */
    private ViewJFrameDICOMQuery DICOMQueryFrame = null;

    /**
     * Location of new image frames. This location is updated with each additional image opened.
     * 
     * @see #getNewFrameLocation()
     */
    private final Dimension frameLocation = new Dimension(50, 300);

    /** Stores array of images frames the first of which is the active image frame. */
    private final Vector<Frame> imageFrameVector;

    /** A list of image models currently open in MIPAV. */
    private final CustomHashtable<ModelImage> imageHashtable;

    /** Frame that monitors the registered images. */
    private ViewJFrameRegisteredImages imgMonitorFrame = null;

    /** Whether the mipav GUI should be shown; set by the -hide command line option. */
    private boolean isAppFrameVisible = true;

    /** Whether a plugin standalone frame is visible */
    private boolean isPlugInFrameVisible = false;

    /**
     * Indicates the user's last choice of whether to open images as multi-file (stack) or single file in the file open
     * dialog.
     */
    private boolean lastStackFlag = false;

    /**
     * Dialog that allows changes to the amount of heap requested of the system by the java Virtual Machine for MIPAV
     * during java start.
     */
    private JDialogMemoryAllocation mallocFrame = null;

    /** Dialog that displays the used and available memory. */
    private ViewJFrameMemory memoryFrame = null;

    /** Dialog to display the mipav system data provenance */
    private JDialogDataProvenance dpDialog = null;

    /** The label showing the current memory usage of MIPAV. */
    private JLabel memoryUsageLabel;

    /** The button indicating that MIPAV is set to run in a threaded environment */
    private JButton btnMultiCore;

    /** The button indicating that MIPAV is set to run OpenCL -- GPU based algorithms */
    private JButton btnGpuComp;

    /**
     * The periodic thread which updates the memory usage display once every second.
     * 
     * @see #updateMemoryUsage()
     */
    private ReminderThread memoryUsageThread;

    /**
     * Dialog that allows changes to the amount of heap requested of the system by the java Virtual Machine for MIPAV
     * during java start.
     */
    private JDialogMipavOptions optionsDialog = null;

    /** Stores the plugins menu so that it can be removed/updated when plugins are installed. */
    private JMenu pluginsMenu = null;

    /** The current progress bar prefix to use. */
    private String progressBarPrefix = ViewUserInterface.OPENING_STR;

    /** Indicates whether the user is currently recording a new keyboard shortcut using the shortcut editor dialog. */
    private boolean shortcutRecording = false;

    /** System DP holder (separate from images data provenance...this has everything). */
    private ProvenanceHolder systemDPHolder;

    /** boolean to force the algorithm to replace the image rather than opening a new frame */
    private boolean forceAlgorithmInPlace = false;

    /** error handling for cmd line, if set to false will not exit on MipavUtil.displayError() */
    private boolean exitCmdLineOnError = true;

    /** if user selects to open images as tiles, then this counter tells us how many tile sheets there are* */
    private int numTileSheets = 0;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Constructs main UI frame. Accesses the .preferences file to set up variables. Sets up DICOM hashtable for reading
     * in DICOM files and starts the catcher, if appropriate. This method cannot be called directly. To use this
     * constructor, you must call createReference. This class is a singleton, which means that only one type of this
     * class is allowed to be instantiated in a single VM.
     */
    protected ViewUserInterface() {
        imageFrameVector = new Vector<Frame>();
        imageHashtable = new CustomHashtable<ModelImage>();
        initPrefsFile();

        // Read preference file
        initUsingPreferences();
        if ( !GraphicsEnvironment.isHeadless()) {
            mainFrame = new JFrame();
            initializeGui();
        }

        // listen to the script recorder so that we can pass along changes in the script recorder status to the script
        // toolbars of individual images
        ScriptRecorder.getReference().addScriptRecordingListener(this);
    }

    /**
     * Constructs main UI frame. Accesses the .preferences file to set up variables. Sets up DICOM hashtable for reading
     * in DICOM files and starts the catcher, if appropriate. This method cannot be called directly. To use this
     * constructor, you must call createReference. This class is a singleton, which means that only one type of this
     * class is allowed to be instantiated in a single VM.
     * 
     * @param forceQuiet Mipav will not display any error, warning, or info messages. If a error displays MIPAV will
     *            exit.
     */
    protected ViewUserInterface(final boolean forceQuiet) {
        System.out.println("MIPAV STARTED with forceQuite set as " + forceQuiet);
        MipavUtil.setForceQuiet(forceQuiet);

        imageFrameVector = new Vector<Frame>();
        imageHashtable = new CustomHashtable<ModelImage>();
        initPrefsFile();

        // Read preference file
        initUsingPreferences();
        if ( !GraphicsEnvironment.isHeadless()) {
            mainFrame = new JFrame();
            initializeGui();
        }

        // listen to the script recorder so that we can pass along changes in the script recorder status to the script
        // toolbars of individual images
        ScriptRecorder.getReference().addScriptRecordingListener(this);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * This method should only be called once, and it should only be called by MipavMain to during the initialization of
     * MIPAV.
     * 
     * @return ViewUserInterface
     */
    public static ViewUserInterface create() {

        if (ViewUserInterface.userInterfaceReference == null) {
            ViewUserInterface.userInterfaceReference = new ViewUserInterface();
            ProvenanceRecorder.getReference().addLine(new ActionStartMipav());
        }

        return ViewUserInterface.userInterfaceReference;
    }

    /**
     * Get a reference to the ViewUserInterface object.
     * 
     * @return ViewUserInterface
     */
    public static ViewUserInterface getReference() {
        return ViewUserInterface.userInterfaceReference;
    }

    /**
     * Creates simple dialog that describes basic info about MIPAV, with MIPAV as the title.
     */
    public void about() {
        about("About MIPAV", "about.txt");
    }

    /**
     * Creates a fairly simple plain-text viewing box.
     * 
     * @param title the title for the about frame.
     * @param filename the filename of the about file to display.
     */
    public void about(String title, String filename) {

        if ( (title == null) || title.equals("")) {
            title = "About MIPAV";
        }

        if ( (filename == null) || filename.equals("")) {
            filename = "about.txt";
        }

        final JDialogText aboutDialog = new JDialogText(mainFrame, title);
        final URL fileURL = getClass().getClassLoader().getResource(filename);

        if (fileURL == null) {
            Preferences.debug("Unable to open " + filename
                    + ".  Make sure it is in the same directory as MipavMain.class\n", Preferences.DEBUG_MINOR);
            MipavUtil.displayError(filename + " not found.\nTurn on debugging output for more information.");

            return;
        }

        BufferedReader br = null;

        try {
            br = new BufferedReader(new InputStreamReader(fileURL.openStream()));

            String aboutData = "";
            String line = null;

            while ( (line = br.readLine()) != null) {
                aboutData += line + "\n";
            }

            aboutDialog.setContent(JDialogText.PLAIN_FORMAT, aboutData);
            aboutDialog.setBounds(110, 60, 600, 450);
            aboutDialog.setVisible(true);
            aboutDialog.setScrollPaneTop();
            aboutDialog.validate();
        } catch (final IOException e) {
            MipavUtil.displayError("Error " + e);
        } catch (final NullPointerException npe) {
            Preferences.debug("NullPointerException when creating About");
        } finally {

            try {

                if (br != null) {
                    br.close();
                }
            } catch (final IOException closee) {}
        }
    }

    /**
     * Displays the system data provenance using a simple dialog with table and jtextarea (for current selection).
     * 
     * <p> .
     * </p>
     */
    public void aboutDataProvenance() {

        if (dpDialog == null) {
            dpDialog = new JDialogDataProvenance(mainFrame, null, null, this.getProvenanceHolder(), true);
        } else {
            dpDialog.setVisible(true);
            dpDialog.requestFocus();
        }
    }

    /**
     * Creates simple dialog that describes basic info about the version of Java.
     */
    public void aboutJava() {
        JDialogText aboutJavaDialog;
        String javaClassPath;

        javaClassPath = System.getProperties().getProperty("java.class.path");
        javaClassPath = javaClassPath.replace(';', '\n');

        aboutJavaDialog = new JDialogText(mainFrame, "About System"); // Title

        aboutJavaDialog.append("Java version:       " + System.getProperties().getProperty("java.version") + "\n");
        aboutJavaDialog.append("Java compiler:      " + System.getProperties().getProperty("java.compiler") + "\n");
        aboutJavaDialog.append("Java vendor:        " + System.getProperties().getProperty("java.vendor") + "\n");
        aboutJavaDialog.append("Java vendor.url:    " + System.getProperties().getProperty("java.vendor.url") + "\n");
        aboutJavaDialog.append("Java home:          " + System.getProperties().getProperty("java.home") + "\n");
        aboutJavaDialog
                .append("Java class version: " + System.getProperties().getProperty("java.class.version") + "\n");
        aboutJavaDialog.append("Java class path:    " + "\n" + javaClassPath + "\n");
        aboutJavaDialog.append("OS name:            " + System.getProperties().getProperty("os.name") + "\n");
        aboutJavaDialog.append("OS arch:            " + System.getProperties().getProperty("os.arch") + "\n");
        aboutJavaDialog.append("OS version:         " + System.getProperties().getProperty("os.version") + "\n");
        aboutJavaDialog.append("User name:          " + System.getProperties().getProperty("user.name") + "\n");
        aboutJavaDialog.append("User home:          " + System.getProperties().getProperty("user.home") + "\n");
        aboutJavaDialog.append("User directory:     " + System.getProperties().getProperty("user.dir") + "\n");

        if (aboutJavaDialog != null) {
            aboutJavaDialog.setLocation(100, 50);
            aboutJavaDialog.setVisible(true);
        }
    }

    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************

    /**
     * Calls various methods based on the user's actions.
     * 
     * @param event Event that triggered this function.
     */
    public void actionPerformed(final ActionEvent event) {
        final Object source = event.getSource();
        final String command = event.getActionCommand();

        // System.err.println("COMMAND: " + command);
        if ( (command != null) && isShorcutRecording()) {
            setShortcutRecording(false);

            if (Preferences.addShortcut(command)) {
                showShortcutEditor(true);
            }

            return;
        } else if (command.equals("gc")) {
            System.gc();
            ProvenanceRecorder.getReference().addLine(new ActionCollectGarbage());
            ScriptRecorder.getReference().addLine(new ActionCollectGarbage());
            return;
        } else if (command.equals("reportbug")) {
        	ReportBugBuilder form = new ReportBugBuilder();
        } else if (command.equals("Dicom")) {

            if (Preferences.is(Preferences.PREF_ASK_DICOM_RECEIVER)) {
                new DicomQueryListener().queryForDicomAutostart();
            }

            if (source instanceof JCheckBoxMenuItem) {

                if ( ((JCheckBoxMenuItem) source).isSelected()) {

                    if (DICOMcatcher != null) {
                        DICOMcatcher.setStop();
                    }

                    DICOMcatcher = new DICOM_Receiver();
                    menuBuilder.setMenuItemSelected("Activate DICOM receiver", DICOMcatcher.isAlive());
                } else {

                    if (DICOMcatcher != null) {
                        DICOMcatcher.setStop();
                    }
                }

            } else {

                // this was a shortcut stroke to get here...toggle the switch
                if (menuBuilder.isMenuItemSelected("Activate DICOM receiver")) {

                    if (DICOMcatcher != null) {
                        DICOMcatcher.setStop();
                    }

                    DICOMcatcher = new DICOM_Receiver();
                    menuBuilder.setMenuItemSelected("Activate DICOM receiver", DICOMcatcher.isAlive());
                } else {

                    if (DICOMcatcher != null) {
                        DICOMcatcher.setStop();
                    }
                    menuBuilder.setMenuItemSelected("Activate DICOM receiver", false);
                }

            }
        } else if (command.equals("Exit")) {
            windowClosing(null);
        } else if (command.equals("OpenNewImage")) {
            openImageFrame();
        } else if (command.equals("closeAllImages")) {
            closeAllImages();
        } else if (command.equals("BrowseImages")) {
            buildTreeDialog();
        } else if (command.equals("BrowseDICOM")) {
            buildDICOMFrame();
        } else if (command.equals("BrowseDICOMDIR")) {
            buildDICOMDIRFrame();
        } else if (command.equals("RecordScript") || command.equals("ToolbarScriptRecord")) {

            if (ScriptRecorder.getReference().getRecorderStatus() == ScriptRecorder.STOPPED) {
                new JDialogScriptRecorder();
            } else {
                MipavUtil.displayError("Cannot open script recorder dialog.  A script is already being recorded.");
            }
        } else if (command.equals("RunScript")) {

            if (ScriptRecorder.getReference().getRecorderStatus() == ScriptRecorder.RECORDING) {
                MipavUtil.displayError("Cannot run a script while recording a script.");

                return;
            } else {
                final JFileChooser chooser = new JFileChooser();
                chooser.setCurrentDirectory(new File(Preferences.getScriptsDirectory()));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.SCRIPT));
                chooser.setDialogTitle("Choose a script file to execute");

                if (chooser.showOpenDialog(getMainFrame()) == JFileChooser.APPROVE_OPTION) {
                    Preferences.setScriptsDirectory(String.valueOf(chooser.getCurrentDirectory()));

                    final String scriptFile = chooser.getSelectedFile().getAbsolutePath();
                    new JDialogRunScriptController(scriptFile);
                }
            }
        } else if (command.equals("CreateBlankImage")) {
            createBlankImage(null);
        } else if (command.equals("QueryDatabase")) {

            if (DICOMQueryFrame == null) {
                DICOMQueryFrame = new ViewJFrameDICOMQuery();
            }
        } else if (command.equalsIgnoreCase("anonymizeDirectory")) {
            buildAnonDirectoryDialog();
        } else if (command.startsWith("PlugInFileRead")) {

            Object thePlugIn = null;
            final String plugInName = "PlugIn" + command.substring(14);
            // String plugInName = ((JMenuItem) (event.getSource())).getComponent().getName();

            try {
                thePlugIn = Class.forName(plugInName).newInstance();

                if (thePlugIn instanceof PlugInFile) {

                    if ( ((PlugInFile) thePlugIn).canReadImages()) {
                        ((PlugInFile) thePlugIn).readImage();
                    } else {
                        MipavUtil.displayInfo(plugInName + " does not support the reading of images.");
                    }
                } else {
                    MipavUtil.displayError("PlugIn " + plugInName
                            + " claims to be an File PlugIn, but does not implement PlugInFile.");
                }
            } catch (final UnsupportedClassVersionError ucve) {
                Preferences
                        .debug(
                                "Unable to load plugin: "
                                        + plugInName
                                        + " -- The plugin is probably compiled for an older version of Java than MIPAV currently supports.\n",
                                Preferences.DEBUG_MINOR);
                ucve.printStackTrace();
            } catch (final ClassNotFoundException e) {
                MipavUtil.displayError("PlugIn not found: " + plugInName);
            } catch (final InstantiationException e) {
                MipavUtil.displayError("Unable to load plugin (ins)");
            } catch (final IllegalAccessException e) {
                MipavUtil.displayError("Unable to load plugin (acc)");
            }
        } else if (command.startsWith("PlugInFileWrite")) {

            Object thePlugIn = null;
            final String plugInName = "PlugIn" + command.substring(15);
            // String plugInName = ((JMenuItem) (event.getSource())).getComponent().getName();

            try {
                thePlugIn = Class.forName(plugInName).newInstance();

                if (thePlugIn instanceof PlugInFile) {

                    if ( ((PlugInFile) thePlugIn).canWriteImages()) {
                        ((PlugInFile) thePlugIn).writeImage((ModelImage) null);
                    } else {
                        MipavUtil.displayInfo(plugInName + " does not support the writing of images.");
                    }
                } else {
                    MipavUtil.displayError("PlugIn " + plugInName
                            + " claims to be an File PlugIn, but does not implement PlugInFile.");
                }
            } catch (final UnsupportedClassVersionError ucve) {
                Preferences
                        .debug(
                                "Unable to load plugin: "
                                        + plugInName
                                        + " -- The plugin is probably compiled for an older version of Java than MIPAV currently supports.\n",
                                Preferences.DEBUG_MINOR);
                ucve.printStackTrace();
            } catch (final ClassNotFoundException e) {
                MipavUtil.displayError("PlugIn not found: " + plugInName);
            } catch (final InstantiationException e) {
                MipavUtil.displayError("Unable to load plugin (ins)");
            } catch (final IllegalAccessException e) {
                MipavUtil.displayError("Unable to load plugin (acc)");
            }
        } else if (command.startsWith("PlugInFileTransfer")) {
            Object thePlugIn = null;
            final String plugInName = "PlugIn" + command.substring(18);
            // String plugInName = ((JMenuItem) (event.getSource())).getComponent().getName();

            try {
                thePlugIn = Class.forName(plugInName).newInstance();

                if (thePlugIn instanceof PlugInFileTransfer) {
                    ((PlugInFileTransfer) thePlugIn).transferFiles();
                } else {
                    MipavUtil.displayError("PlugIn " + plugInName
                            + " claims to be an File Transfer PlugIn, but does not implement PlugInFileTransfer.");
                }
            } catch (final UnsupportedClassVersionError ucve) {
                Preferences
                        .debug(
                                "Unable to load plugin: "
                                        + plugInName
                                        + " -- The plugin is probably compiled for an older version of Java than MIPAV currently supports.\n",
                                Preferences.DEBUG_MINOR);
                ucve.printStackTrace();
            } catch (final ClassNotFoundException e) {
                MipavUtil.displayError("PlugIn not found: " + plugInName);
            } catch (final InstantiationException e) {
                MipavUtil.displayError("Unable to load plugin (ins)");
            } catch (final IllegalAccessException e) {
                MipavUtil.displayError("Unable to load plugin (acc)");
            }
        } else if (command.startsWith("PlugInFile")) {
            Object thePlugIn = null;
            final String plugInName = "PlugIn" + command.substring(10);
            final String plugInNameRecall = plugInName.substring(6);
            // String plugInName = ((JMenuItem) (event.getSource())).getComponent().getName();

            try {
                thePlugIn = Class.forName(plugInName).newInstance();

                if (thePlugIn instanceof PlugInFile) {

                    if ( ((PlugInFile) thePlugIn).canWriteImages() && ((PlugInFile) thePlugIn).canReadImages()) {
                        final Object[] options = new String[2];
                        final String read = "Read image";
                        options[0] = read;
                        options[1] = "Write image";
                        final String title = "Select plugin type";
                        final String message = "This plugin can both read and write images.  "
                                + "Which action should the plugin perform?";
                        final int opt = JOptionPane.showOptionDialog(mainFrame, message, title,
                                JOptionPane.DEFAULT_OPTION, JOptionPane.INFORMATION_MESSAGE, null, options, read);
                        if (opt == 0) {
                            final ActionEvent e = new ActionEvent(event.getSource(), event.getID(), "PlugInFileRead"
                                    + plugInNameRecall);
                            this.actionPerformed(e);
                        } else if (opt == 1) {
                            final ActionEvent e = new ActionEvent(event.getSource(), event.getID(), "PlugInFileWrite"
                                    + plugInNameRecall);
                            this.actionPerformed(e);
                        }

                    } else if ( ((PlugInFile) thePlugIn).canReadImages()) {
                        final ActionEvent e = new ActionEvent(event.getSource(), event.getID(), "PlugInFileRead"
                                + plugInNameRecall);
                        this.actionPerformed(e);
                    } else if ( ((PlugInFile) thePlugIn).canWriteImages()) {
                        final ActionEvent e = new ActionEvent(event.getSource(), event.getID(), "PlugInFileWrite"
                                + plugInNameRecall);
                        this.actionPerformed(e);
                    } else {
                        MipavUtil.displayInfo(plugInName + " is a PlugInFile that neither writes nor reads images.");
                    }
                } else {
                    MipavUtil.displayError("PlugIn " + plugInName
                            + " claims to be an File PlugIn, but does not implement PlugInFile.");
                }
            } catch (final UnsupportedClassVersionError ucve) {
                Preferences
                        .debug(
                                "Unable to load plugin: "
                                        + plugInName
                                        + " -- The plugin is probably compiled for an older version of Java than MIPAV currently supports.\n",
                                Preferences.DEBUG_MINOR);
                ucve.printStackTrace();
            } catch (final ClassNotFoundException e) {
                MipavUtil.displayError("PlugIn not found: " + plugInName);
            } catch (final InstantiationException e) {
                MipavUtil.displayError("Unable to load plugin (ins)");
            } catch (final IllegalAccessException e) {
                MipavUtil.displayError("Unable to load plugin (acc)");
            }
        } else if (command.startsWith("PlugInGeneric")) {
            Object thePlugIn = null;

            final String plugInName = "PlugIn" + command.substring(13);
            // String plugInName = ((JMenuItem) (event.getSource())).getComponent().getName();

            try {
                thePlugIn = Class.forName(plugInName).newInstance();

                if (thePlugIn instanceof PlugInGeneric) {
                    ((PlugInGeneric) thePlugIn).run();
                } else {
                    MipavUtil.displayError("Plugin " + plugInName + " is not a generic plugin.");
                }
            } catch (final UnsupportedClassVersionError ucve) {
                Preferences
                        .debug(
                                "Unable to load plugin: "
                                        + plugInName
                                        + " -- The plugin is probably compiled for an older version of Java than MIPAV currently supports.\n",
                                Preferences.DEBUG_MINOR);
                ucve.printStackTrace();
            } catch (final ClassNotFoundException e) {
                MipavUtil.displayError("PlugIn not found: " + plugInName);
            } catch (final InstantiationException e) {
                MipavUtil.displayError("Unable to load plugin (ins)");
            } catch (final IllegalAccessException e) {
                MipavUtil.displayError("Unable to load plugin (acc)");
            }
        } else if (command.startsWith("PlugInImageJ")) {
            // System.out.println(command);
            Class<?> thePlugInClass = null;
            Object thePlugInInstance = null;

            final String plugInName = command.substring(12);

            try {
                thePlugInClass = Class.forName(plugInName);

                for (Class c : thePlugInClass.getInterfaces()) {
                    if ( (c.equals(ij.plugin.PlugIn.class))
                            || (thePlugInClass.getSuperclass().equals(ij.plugin.frame.PlugInFrame.class))) {
                        thePlugInInstance = Class.forName(plugInName).newInstance();
                        String args = "";
                        ((ij.plugin.PlugIn) thePlugInInstance).run(args);
                        break;
                    } else if (c.equals(ij.plugin.filter.PlugInFilter.class)) {

                        // first see if there is an active image
                        if (getActiveImageFrame() != null) {
                            ModelImage img = getActiveImageFrame().getImageA();
                            if (img.is2DImage()) {

                                ImageProcessor ip = ModelImageToImageJConversion.convert2D(img);
                                thePlugInInstance = Class.forName(plugInName).newInstance();
                                ((ij.plugin.filter.PlugInFilter) thePlugInInstance).run(ip);

                            } else {
                                MipavUtil.displayError("This plugin only works on a 2D image");
                            }
                        } else {
                            MipavUtil.displayError("There must be an active image open to run this plugin");
                            return;
                        }

                        /*
                         * ModelImage img = getActiveImageFrame().getImageA(); ImageStack is =
                         * ModelImageToImageJConversion.convert3D(img); new ImagePlus("blah",is).show();
                         */

                        break;
                    }
                }

            } catch (Exception e) {
                e.printStackTrace();
            }

        } else if (command.equals("InstallPlugin")) {
            final JDialogInstallPlugin instPlugin = new JDialogInstallPlugin(mainFrame);
            instPlugin.setVisible(true);

            final int index = openingMenuBar.getComponentIndex(pluginsMenu);
            openingMenuBar.remove(pluginsMenu);
            pluginsMenu = buildPlugInsMenu(this);
            openingMenuBar.add(pluginsMenu, index);
            getMainFrame().pack();
        } else if (command.equals("UninstallPlugin")) {
            final JDialogUninstallPlugin uninstPlugin = new JDialogUninstallPlugin(mainFrame);
            uninstPlugin.setVisible(true);

            final int index = openingMenuBar.getComponentIndex(pluginsMenu);
            openingMenuBar.remove(pluginsMenu);
            pluginsMenu = buildPlugInsMenu(this);
            openingMenuBar.add(pluginsMenu, index);
            getMainFrame().pack();

        } else if (command.equals("CompileAndRun")) {
            JFileChooser chooser = new JFileChooser();

            chooser.setDialogTitle("Select all files that are associated with this plugin");
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            chooser.setMultiSelectionEnabled(true);
            int returnValue = chooser.showOpenDialog(this.getMainFrame());
            if (returnValue == JFileChooser.APPROVE_OPTION) {

                File[] files = chooser.getSelectedFiles();
                String[] filePaths = new String[files.length];
                for (int i = 0; i < files.length; i++) {
                    if (files[i].getName().endsWith(".java")) {
                        filePaths[i] = files[i].getAbsolutePath();
                    }
                }

                String userPluginsDir = System.getProperty("user.home") + File.separator + "mipav" + File.separator
                        + "plugins" + File.separator;
                com.sun.tools.javac.Main javac = new com.sun.tools.javac.Main();

                Vector<String> v = new Vector<String>();
                v.add("-d");
                v.add(userPluginsDir);

                for (int i = 0; i < filePaths.length; i++) {
                    v.add(filePaths[i]);
                }

                String[] args = new String[v.size()];
                v.copyInto((String[]) args);
                ByteArrayOutputStream output = new ByteArrayOutputStream(4096);

                boolean compiled = javac.compile(args, new PrintWriter(output)) == 0;

                if (compiled) {
                    int index = openingMenuBar.getComponentIndex(pluginsMenu);
                    pluginsMenu = buildPlugInsMenu(this);
                    openingMenuBar.remove(index);
                    openingMenuBar.add(pluginsMenu, index);
                    getMainFrame().pack();
                    getMainFrame().repaint();

                    for (int i = 0; i < filePaths.length; i++) {
                        String name = filePaths[i];
                        name = name.substring(name.lastIndexOf(File.separator) + 1, name.lastIndexOf("."));

                        Class<?> plugin = null;
                        try {
                            plugin = Class.forName(name);

                            if (JDialogInstallPlugin.isImageJPluginClass(plugin)) {
                                Component[] comps = pluginsMenu.getMenuComponents();
                                for (int m = 0; m < comps.length; m++) {
                                    Component comp = comps[m];
                                    if (comp instanceof JMenu) {
                                        Component[] subComps = ((JMenu) comp).getMenuComponents();
                                        for (int k = 0; k < subComps.length; k++) {
                                            Component subComp = subComps[k];
                                            if (comp instanceof JMenuItem) {
                                                String menuItemName = ((JMenuItem) subComp).getName();
                                                if (menuItemName.equals(name)) {
                                                    ActionEvent e = new ActionEvent( ((JMenuItem) subComp), 0,
                                                            ((JMenuItem) subComp).getActionCommand());
                                                    this.actionPerformed(e);
                                                    return;
                                                } else {
                                                    // try extracting Plugin from the name
                                                    if (name.startsWith("PlugIn")) {
                                                        name = name.substring(6, name.length());
                                                        if (menuItemName.equals(name)) {
                                                            ActionEvent e = new ActionEvent( ((JMenuItem) subComp), 0,
                                                                    ((JMenuItem) subComp).getActionCommand());
                                                            this.actionPerformed(e);
                                                            return;
                                                        }
                                                    }

                                                }
                                            }
                                        }
                                    }

                                }
                            }
                        } catch (Exception e) {

                            e.printStackTrace();

                        }
                    }
                } else {
                    MipavUtil.displayError("Plugin Files did not compile :  " + output.toString());

                }

            }

        } else if (command.equals("About")) {
            about();
        } else if (command.equals("License")) {
            showLicense();
        } else if (command.equals("AboutJava")) {
            aboutJava();
        } else if (command.equals("DataProvenance")) {
            aboutDataProvenance();
        } else if (command.equals("Help")) {
            MipavUtil.showHelp(null);
        } else if (command.equals("MemoryUsage")) {
            memoryFrame();
        } else if (command.equals("MemoryAdjust")) {
            memoryAllocation();
        } else if (command.equals("ImageRegistryMonitor")) {
            imageRegistryMonitoring();
        } else if (command.equals("Options")) {
            options();
            if (event.getSource().equals(btnGpuComp) || event.getSource().equals(btnMultiCore)) {
                optionsDialog.showPane("Other");
            }
        } else if (command.equals("Shortcuts")) {
            showShortcutEditor(false);
        } else if (command.equals("loadLeica")) {
            // open a file chooser to select .txt header

            final JFileChooser chooser = new JFileChooser(this.getDefaultDirectory());
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            chooser.setDialogTitle("Select Leica header file");

            final int returnVal = chooser.showDialog(this.getMainFrame(), "Open");

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                new JDialogLoadLeica(chooser.getSelectedFile());
            } else {
                return;
            }
        } else if (command.equals("openImgSeq")) {
            new ViewOpenImageSequence();
        } else if (command.startsWith("LastImage")) {

            final int number = Integer.valueOf(command.substring(10)).intValue();
            openLastImage(number);
        } else if (command.equals("loadDWI")) {
            new JDialogDTIInput(JDialogDTIInput.DWI);
        } else if (command.equals("loadDTI")) {
            new JDialogDTIInput(JDialogDTIInput.DTI);
        } else if (command.equals("loadEG_FA")) {
            new JDialogDTIInput(JDialogDTIInput.EG_FA);
        } else if (command.equals("loadDTIFrame")) {
            JPanelDTIVisualization.createFrame();
        } else if (command.equals("createListFile")) {
            new JDialogDTICreateListFile();
        } else if (command.equals("dtiPipeline")) {
            new DTIPipeline();
        } else if (command.equals("dtiColor")) {
            new DTIColorDisplay(true);
        } else if (command.equals("estimateTensor")) {
            new JDialogDTIEstimateTensor();
            /*
             * } else if (command.equals("fiberTracking")) { JPanelDTIFiberTracking.createFrame(); } else if
             * (command.equals("dtiVisualization")) { JPanelDTIVisualization.createFrame();
             */
        } else if (command.equals("HyperGraph")) {
            new JDialogHyperGraph(null, null);
        } else if (command.equals("treT1")) {
            if (getActiveImageFrame() != null) {
                new JDialogTreT1(getActiveImageFrame(), getActiveImageFrame().getActiveImage());
            } else {
                MipavUtil.displayError("Images required for T1 estimation were not found.");
            }
        } else if (command.equals("treT2")) {
            if (getActiveImageFrame() != null) {
                new JDialogTreT2(getActiveImageFrame(), getActiveImageFrame().getActiveImage());
            } else {
                MipavUtil.displayError("Images required for T2 estimation were not found.");
            }
        } else if (command.equals("LogSlope")) {
            new JDialogLogSlopeMapping();
        } else if (command.equals("KMeans")) {
            new JDialogKMeans();
        }

    }

    /**
     * Creates a blank Image based on the information found in the default fileInfo object.
     * 
     * @param fileInfo This object contains the enough image information to build a ModelImage with nothing inside (eg.
     *            blank image).
     * @param image Created blank image.
     */
    public ModelImage createEmptyImage(FileInfoBase fileInfo) {
        final int[] extents = {256, 256, 32};
        final int[] units = {7, 7, 7, -1, -1};
        final float[] res = {1.0f, 1.0f, 1.0f, 1.0f, 1.0f};

        if (fileInfo == null) {
            fileInfo = new FileInfoImageXML("BlankImage", null, FileUtility.RAW);
            fileInfo.setDataType(ModelStorageBase.SHORT);
            fileInfo.setExtents(extents);
            fileInfo.setUnitsOfMeasure(units);
            fileInfo.setResolutions(res);
            fileInfo.setEndianess(false);
            fileInfo.setOffset(0);
        }

        return createBlankImage(fileInfo);
    }

    /**
     * Copies into the VOI clipboard.
     * 
     * @param voi VOI
     * @deprecated
     */
    public void copyClippedVOIs(ViewVOIVector copyList) {
        clearClippedVOIs();
        for (int i = 0; i < copyList.size(); i++) {
            clippedVOIs.add(copyList.get(i));
        }
    }

    public void copyVOIs(Vector<VOIBase> copyList) {
        copyVOIList = copyList;
    }

    /**
     * Adds a clipped VOI from a 3D image to the clipboard.
     * 
     * @param voi the voi
     * @param slice slice number
     * @param scannerPts a vector of all the VOI's points pre-converted to scanner coordinates public void
     *            addClippedScannerVOI(final VOI voi, final int slice, final Vector<Vector3f> scannerPts) {
     * 
     * if (isClippedVOI2D == true) { clearClippedVOIs(); isClippedVOI2D = false; }
     * 
     * this.clippedVOIs.add(voi); this.clippedScannerVectors.add(scannerPts); }
     */

    /**
     * Builds the anonymize directory dialog and displays it.
     */
    public void buildAnonDirectoryDialog() {

        // get the selected directory
        final ViewDirectoryChooser chooser = new ViewDirectoryChooser();
        final String dir = chooser.getImageDirectory();

        if (dir != null) { // we may create multiple instances of the same thing
            new JDialogAnonymizeDirectory(dir);
        }
    }

    /**
     * Builds the image tree dialog and displays it.
     */
    public void buildDICOMFrame() {

        // get the selected directory
        final ViewDirectoryChooser chooser = new ViewDirectoryChooser();
        final String dir = chooser.getImageDirectory();

        if (dir != null) {
            new ViewJFrameDICOMParser(dir);
        }
    }

    /**
     * Builds the image tree dialog and displays it.
     */
    public void buildDICOMDIRFrame() {

        new JDialogDicomDir(this.getMainFrame());
    }

    /**
     * Builds menus for the User Interface.
     */
    public void buildMenu() {
        menuBuilder = new ViewMenuBuilder(this);

        final ViewMenuBar menuBar = new ViewMenuBar(menuBuilder);

        openingMenuBar = new JMenuBar();
        openingMenuBar.add(menuBar.makeFileMenu(false));

        if (getRegisteredFramedImagesNum() > 0) {
            this.pluginsMenu = buildPlugInsMenu(getActiveImageFrame());
        } else {
            this.pluginsMenu = buildPlugInsMenu(this);
        }

        openingMenuBar.add(menuBar.makeSystemsAnalysisMenu());
        openingMenuBar.add(pluginsMenu);
        openingMenuBar.add(menuBar.makeScriptingMenu());
        openingMenuBar.add(menuBar.makeHelpMenu());
    }

    /**
     * Builds the message frame where user/program data can be displayed.
     */
    public void buildMessageFrame() {

        messageFrame = new ViewJFrameMessage("Output");
        messageFrame.setSize(550, 300);

        try {
            messageFrame.setIconImage(MipavUtil.getIconImage("output_16x16.gif"));
        } catch (final FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
        }

        final int frameHeight = messageFrame.getSize().height;
        final int screenHeight = Toolkit.getDefaultToolkit().getScreenSize().height;
        final int taskbarHeight = Toolkit.getDefaultToolkit().getScreenInsets(messageFrame.getGraphicsConfiguration()).bottom;
        messageFrame.setLocation(0, screenHeight - frameHeight - taskbarHeight);
    }

    /**
     * Called by either userInterface (this) or by another actionlistener (ViewJFrameImage) to build the plugins menu
     * bar.
     * 
     * @param al the listener that wants to know about actions on the plugins menu
     * 
     * @return the new plugin menu
     */
    public JMenu buildPlugInsMenu(final ActionListener al) {
        final String userPlugins = System.getProperty("user.home") + File.separator + "mipav" + File.separator
                + "plugins" + File.separator;

        final JMenu menu = menuBuilder.makeMenu("Plugins", 'P', false, new JComponent[] {});

        final File pluginsDir = new File(userPlugins);
        File[] allFiles = new File[0];
        if (pluginsDir.isDirectory() || (secondaryPluginsDir != null && secondaryPluginsDir.isDirectory())) {
            if (pluginsDir.isDirectory()) {
                allFiles = pluginsDir.listFiles(new FileFilter() {
                    public boolean accept(final File f) {

                        if (f.getPath().endsWith(".class")) {
                            return true;
                        }
                        return false;
                    }
                });
            }

            if (secondaryPluginsDir != null) {
                File[] secondaryFiles = secondaryPluginsDir.listFiles(new FileFilter() {
                    public boolean accept(final File f) {

                        if (f.getPath().endsWith(".class")) {
                            return true;
                        }
                        return false;
                    }
                });

                File[] allFilesInterm = new File[allFiles.length + secondaryFiles.length];
                int counter = 0;
                for (int i = 0; i < allFiles.length; i++, counter++) {
                    allFilesInterm[counter] = allFiles[i];
                }
                for (int i = 0; i < secondaryFiles.length; i++, counter++) {
                    allFilesInterm[counter] = secondaryFiles[i];
                }
                allFiles = allFilesInterm;
            }

            String name, pluginName;
            Field catField = null, scriptField = null;
            Class<?> plugin;
            final String catName = "CATEGORY";
            final String scriptName = "SCRIPT_PREFIX";

            for (final File allFile : allFiles) {
                JMenu currentMenu = menu;
                name = allFile.getName();

                try {
                    name = name.substring(0, name.indexOf(".class"));
                    if (name.startsWith("PlugIn")) {
                        pluginName = name.substring(name.indexOf("PlugIn") + 6, name.length());
                    } else {
                        pluginName = name;
                    }

                } catch (final Exception e) {
                    pluginName = name;
                }
                try {

                    plugin = Class.forName(name);

                    // plugin.newInstance();
                    // rather than instantiating to allow loading into SCRIPT_ACTION_LOCATIONS, see below
                    try {
                        scriptField = plugin.getField(scriptName);

                    } catch (final NoSuchFieldException e1) {
                        // The scriptname field is optional.
                    }

                    if (scriptField != null) {
                        final String scriptLoc = (String) scriptField.get(plugin);
                        if (scriptLoc != null) {
                            // the value of SCRIPT_NAME is now the short name for this plugin
                            ScriptableActionLoader.addScriptActionLocation(scriptLoc);
                        }
                    }
                    String[] hier = null;
                    boolean isImageJPlugin = false;
                    try {
                        catField = plugin.getField(catName);
                        hier = (String[]) catField.get(plugin);
                    } catch (NoSuchFieldException e1) {
                        if (JDialogInstallPlugin.isImageJPluginClass(plugin)) {
                            isImageJPlugin = true;
                            hier = new String[] {"ImageJ"};
                        }
                    }

                    for (final String element : hier) {
                        final Component[] subComp = currentMenu.getMenuComponents();
                        boolean subExists = false;
                        for (final Component element2 : subComp) {
                            if (element2 instanceof JMenu && ((JMenu) element2).getText().equals(element)) {
                                currentMenu = (JMenu) element2;
                                subExists = true;
                                break;
                            }
                        }
                        if ( !subExists) {
                            final JMenu newMenu = ViewMenuBuilder.buildMenu(element, 0, false);
                            currentMenu.add(newMenu);
                            currentMenu = newMenu;
                        }
                    }
                    String interName = "";
                    if ( !isImageJPlugin) {
                        final Class<?>[] interList = plugin.getInterfaces();

                        // find the interface name that determines the type of plugin
                        for (final Class<?> element : interList) {
                            if (element.getName().contains("PlugIn")
                                    && !element.getName().contains("BundledPlugInInfo")) {
                                interName = element.getName().substring(element.getName().indexOf("PlugIn"));
                            }
                        }

                        if (interName.length() == 0 && plugin.getSuperclass() != null) {
                            interName = getSuperInterfaces(plugin.getSuperclass());
                        }

                    } else {
                        interName = "PlugInImageJ";
                    }

                    if ( ! (al instanceof ViewUserInterface && interName.equals("PlugInAlgorithm"))) {
                        final JMenuItem pluginMenuItem = ViewMenuBuilder.buildMenuItem(pluginName, interName
                                + pluginName, 0, al, null, false);
                        pluginMenuItem.setName(pluginName);
                        pluginMenuItem.addMouseListener(ViewJPopupPlugin.getReference());
                        currentMenu.add(pluginMenuItem);
                    }

                } catch (final ClassNotFoundException e) {

                    // e.printStackTrace();
                } catch (final Exception e) {
                    // usually this means other files/folders exist in the installed plugins directory besides plugin
                    // files
                    // e.printStackTrace();
                } catch (final NoClassDefFoundError e) {
                    // components of some classes may no longer exist in the classpath.

                }
            }
        }

        if (menu.getItemCount() > 0) {
            menu.addSeparator();
        }

        deleteMenu(menu);

        menu.add(menuBuilder.buildMenuItem("Install plugin", "InstallPlugin", 0, null, false));

        menu.add(menuBuilder.buildMenuItem("Uninstall plugin", "UninstallPlugin", 0, null, false));

        menu.add(menuBuilder.buildMenuItem("Compile and run...", "CompileAndRun", 0, null, false));

        return menu;
    }

    private String getSuperInterfaces(final Class<?> plugin) {
        String interName = new String();
        final Class<?>[] interList = plugin.getInterfaces();
        for (final Class<?> element : interList) {
            if (element.getName().contains("PlugIn")) {
                interName = element.getName().substring(element.getName().indexOf("PlugIn"));
            }
        }

        if (interName.length() == 0 && plugin.getSuperclass() != null) {
            return getSuperInterfaces(plugin.getSuperclass());
        } else {
            return interName;
        }

    }

    /**
     * Recursive deletion algorithm to delete JMenus which contain no JMenuItems exclusive of JMenus in any children.
     * 
     * @param menu The menu to run through deletion
     */
    private void deleteMenu(final JMenu menu) {
        for (int i = 0; i < menu.getItemCount(); i++) {
            if (menu.getItem(i) instanceof JMenu) {
                deleteMenu( ((JMenu) menu.getItem(i)));
                if ( ((JMenu) menu.getItem(i)).getItemCount() == 0) {
                    menu.remove(i);
                }
            }
        }
    }

    /**
     * Builds the image tree dialog and displays it.
     */
    public void buildTreeDialog() {
        final JDialogFilterChoice dialog = new JDialogFilterChoice(this.getMainFrame());

        if ( !dialog.isCancelled()) {

            // get the selected directory
            final ViewDirectoryChooser chooser = new ViewDirectoryChooser(dialog);
            String dir = null;

            final String initialDirectory = Preferences.getProperty(Preferences.PREF_DEFAULT_IMAGE_BROWSER_DIR);

            if (initialDirectory == null) {
                dir = chooser.getImageDirectory();
            } else {

                // the File object is built to test whether the initialDirectory actually exists
                final File directory = new File(initialDirectory);

                if ( (directory == null) || !directory.exists() || !directory.canRead()) {
                    dir = chooser.getImageDirectory();
                } else {
                    dir = chooser.chooseDirectory(initialDirectory);
                }
            }

            if (dir != null) {
                Preferences.setProperty(Preferences.PREF_DEFAULT_IMAGE_BROWSER_DIR, dir);
                new ViewImageDirectory(dir, dialog.getFilter());
            }
        }
    }

    /**
     * {@inheritDoc}.*
     * 
     * @param recorderStatus DOCUMENT ME!
     */
    public void changeRecordingStatus(final int recorderStatus) {
        final Enumeration<ModelImage> e = this.getRegisteredImages();

        while (e.hasMoreElements()) {

            try {
                this.getFrameContainingImage(e.nextElement()).getControls().setRecording(
                        recorderStatus == ScriptRecorder.RECORDING);
            } catch (final NullPointerException ex) {
                // do nothing
            }
        }
    }

    /**
     * Accessor to clear all of data frame.
     */
    public final void clearAllDataText() {

        if (messageFrame != null) {
            messageFrame.clear(ViewJFrameMessage.DATA);
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @deprecated
     */
    public void clearClippedVOIs() {
        this.clippedVOIs.removeAllElements();
        this.clippedScannerVectors.removeAllElements();
    }

    /**
     * Creates a blank Image based on the information found in the fileInfo object and places it in a frame.
     * 
     * @param fileInfo This object contains the enough image information to build a ModelImage with nothing inside (eg.
     *            blank image).
     */
    public ModelImage createBlankImage(FileInfoBase fileInfo) {
        return createBlankImage(fileInfo, true);
    }

    /**
     * Creates a blank Image based on the information found in the fileInfo object.
     * 
     * @param fileInfo This object contains the enough image information to build a ModelImage with nothing inside (eg.
     *            blank image).
     */
    public ModelImage createBlankImage(FileInfoBase fileInfo, boolean doDisplay) {
        ModelImage image = null;

        if (fileInfo == null) {
            final JDialogRawIO rawIODialog = new JDialogRawIO(mainFrame, "Raw");
            rawIODialog.setVisible(true);

            if (rawIODialog.isCancelled() == true) {
                return null;
            }

            fileInfo = new FileInfoImageXML("BlankImage", null, FileUtility.RAW);
            fileInfo.setDataType(rawIODialog.getDataType());
            fileInfo.setExtents(rawIODialog.getExtents());
            fileInfo.setUnitsOfMeasure(rawIODialog.getUnitsOfMeasure());
            fileInfo.setResolutions(rawIODialog.getResolutions());
            fileInfo.setEndianess(rawIODialog.getEndianess());
            fileInfo.setOffset(rawIODialog.getOffset());
        }

        try {
            image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), "BlankImage");
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            MipavUtil.displayError("FileIO: " + error);

            return null;
        }

        if (fileInfo.getExtents().length > 2) { // Set file info

            for (int i = 0; i < fileInfo.getExtents()[2]; i++) {
                image.setFileInfo(fileInfo, i);
            }
        } else {
            image.setFileInfo(fileInfo, 0);
        }

        try {
            if (doDisplay) {
                new ViewJFrameImage(image, null, getNewFrameLocation(image.getExtents()[0], image.getExtents()[1]));
            }
        } catch (final OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory");

            return null;
        }

        ProvenanceRecorder.getReference().addLine(new ActionCreateBlankImage(image));
        ScriptRecorder.getReference().addLine(new ActionCreateBlankImage(image));

        return image;
    }

    /**
     * Toggles the display of the Output window and updates all JFrameImages so that the menu checkbox will reflect the
     * status of the output window.
     * 
     * @param doShowFrame Whether the output window should be shown.
     */
    public void enableOutputWindow(final boolean doShowFrame) {

        // this can be triggered by trying to "close the output window frame also"
        messageFrame.setVisible(doShowFrame);
        Preferences.setProperty(Preferences.PREF_SHOW_OUTPUT, Boolean.toString(doShowFrame));

        final Enumeration<ModelImage> e = this.getRegisteredImages();

        while (e.hasMoreElements()) {

            try {
                this.getFrameContainingImage(e.nextElement()).setOutputWindowBox(doShowFrame);
            } catch (final NullPointerException ex) { // do nothing
            }
        }
    }

    /**
     * Accessor that returns the active image frame vector. If the top frame is not an image frame, then the frame
     * vector is iterated until an image frame is found. If none is found then null is returned.
     * 
     * @return The vector that has a list of frames visible in the GUI.
     */
    public ViewJFrameImage getActiveImageFrame() {

        if (imageFrameVector.size() == 0) {
            return null;
        }

        for (int i = 0; i < imageFrameVector.size(); i++) {
            final Frame frame = imageFrameVector.elementAt(i);

            if (frame instanceof ViewJFrameImage) {
                return (ViewJFrameImage) frame;
            }
        }

        return null;

    } // end getActiveImageFrame()

    /**
     * Accessor to get the title of this application.
     * 
     * @return The title of this application.
     */
    public String getAppTitle() {
        return (Preferences.getProperty("ApplicationTitle"));
    }

    /**
     * Retrieves the clipped matrix for paste action.
     * 
     * @return clippedMatrix
     */
    public TransMatrix getClippedMatrix() {
        return this.clippedMatrix;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     * @deprecated
     */
    public Vector<Vector<Vector3f>> getClippedScannerVectors() {
        return this.clippedScannerVectors;
    }

    /**
     * Returns the VOIs copied into the clip board. For copying and pasting VOIs between images.
     * 
     * @return Vector<VOIBase>
     * @deprecated
     */
    public ViewVOIVector getClippedVOIs() {
        return this.clippedVOIs;
    }

    public Vector<VOIBase> getCopyVOIs() {
        return this.copyVOIList;
    }

    /**
     * Returns the Command line arguments (as one string, each separated by a space) .@return command line arguments
     * concatenated with spaces
     * 
     * @return DOCUMENT ME!
     */
    public String getCmdLineArguments() {
        return this.cmdLineArguments;
    }

    /**
     * Accessor to get directory location of last file access.
     * 
     * @return The last file directory
     */
    public String getDefaultDirectory() {

        final String str = Preferences.getProperty(Preferences.PREF_IMAGE_DIR);

        if (str != null) {
            if (str.charAt(str.length() - 1) != File.separatorChar) {
                final String str2 = str + File.separatorChar;
                return str2;
            }
            return str;
        } else {
            return (System.getProperties().getProperty("user.dir"));
        }
    }

    /**
     * Accessor to get directory location of script files.
     * 
     * @return The script file directory
     */
    public String getDefaultScriptDirectory() {

        String str = Preferences.getProperty(Preferences.PREF_SCRIPT_DIR);

        if (str != null) {
            return str;
        } else {
            str = Preferences.getProperty(Preferences.PREF_IMAGE_DIR);

            if (str != null) {
                return str;
            } else {
                str = System.getProperties().getProperty("user.dir");

                final File f = new File(str);

                if (f.canWrite()) {
                    return str;
                } else {
                    return (System.getProperties().getProperty("user.home"));
                }
            }
        }
    }

    /**
     * Accessor to get the DICOM receiver.
     * 
     * @return the DICOM receiver
     */
    public DICOM_Receiver getDICOMCatcher() {
        return DICOMcatcher;
    }

    /**
     * Accessor to get the DICOM query frame.
     * 
     * @return The DICOM query frame.
     */
    public ViewJFrameDICOMQuery getDICOMQueryFrame() {
        return DICOMQueryFrame;
    }

    /**
     * Accessor that returns the active image frame vector. If the top frame is not an image frame, then the frame
     * vector is iterated until an image frame is found. If none is found then null is returned.
     * 
     * @param image the image to find the frame for.
     * 
     * @return The vector that has a list of frames visible in the GUI.
     */
    public ViewJFrameImage getFrameContainingImage(final ModelImage image) {

        if (imageFrameVector.size() == 0) {
            return null;
        }

        for (int i = 0; i < imageFrameVector.size(); i++) {
            final Frame frame = imageFrameVector.elementAt(i);

            if ( (frame instanceof ViewJFrameImage) && ( ((ViewJFrameImage) (frame)).getImageA() == image)) {
                return (ViewJFrameImage) frame;
            }
        }

        return null;

    } // end getFrameContainingImage()

    /**
     * Accessor that returns frame vector.
     * 
     * @return The vector that has a list of frames visible in the GUI.
     */
    public Vector<Frame> getImageFrameVector() {
        return imageFrameVector;
    }

    /**
     * Returns the last script file used from the preferences.
     * 
     * @return LastScript
     */
    public String getLastScript() {
        return Preferences.getProperty(Preferences.PREF_LAST_SCRIPT);
    }

    /**
     * Gets the last checkbox entry for "multi-file" for opening images.
     * 
     * @return boolean
     */
    public boolean getLastStackFlag() {
        return this.lastStackFlag;
    }

    /**
     * Accessor that returns the main user interface frame.
     * 
     * @return The main user interface frame.
     */
    public JFrame getMainFrame() {
        return (mainFrame);
    }

    /**
     * Returns the interface's menu builder.
     */
    public ViewMenuBuilder getMenuBuilder() {
        return menuBuilder;
    }

    /**
     * Accessor that returns the message frame.
     * 
     * @return The message frame.
     */
    public ViewJFrameMessage getMessageFrame() {
        return (messageFrame);
    }

    /**
     * Changes location of image when first displayed.
     * 
     * @return The new location.
     */
    public Dimension getNewFrameLocation(int newImageXDim, int newImageYDim) {

        if (Preferences.is(Preferences.PREF_OPEN_IMAGES_IN_TILED_FORMAT)) {
            Vector<Frame> imageFrames = getImageFrameVector();

            if (imageFrames.size() < 1) {
                frameLocation.width = 50;
                frameLocation.height = 300;
                numTileSheets = 0;
            } else {
                Frame lastFrame = imageFrames.get(0);
                int lastFrameWidth = lastFrame.getWidth();
                frameLocation.width = frameLocation.width + lastFrameWidth + 10;
                int maxHeight = 0;
                if ( (frameLocation.width + newImageXDim + 50) > Toolkit.getDefaultToolkit().getScreenSize().width) {
                    frameLocation.width = 50 + (numTileSheets * 20);
                    // for height, we need to get the biggest frame height and add 10 to it
                    int size = imageFrames.size();
                    for (int i = 0; i < size; i++) {
                        Frame frame = imageFrames.get(i);
                        if (frame.getHeight() > maxHeight) {
                            maxHeight = frame.getHeight();
                        }
                    }
                    frameLocation.height = frameLocation.height + maxHeight + 10;
                    if (frameLocation.height + newImageYDim + 50 > Toolkit.getDefaultToolkit().getScreenSize().height) {
                        numTileSheets = numTileSheets + 1;
                        frameLocation.width = 50 + (numTileSheets * 20);
                        frameLocation.height = 300 + (numTileSheets * 20);
                    }
                }

            }
        } else {
            Vector<Frame> imageFrames = getImageFrameVector();
            if (imageFrames.size() < 1) {
                frameLocation.width = 50;
                frameLocation.height = 300;
            } else {
                frameLocation.width += 100;
                frameLocation.height += 20;

                if ( (frameLocation.width + newImageXDim + 50) > Toolkit.getDefaultToolkit().getScreenSize().width) {
                    frameLocation.width = 50;
                    frameLocation.height = 280;
                } else if ( (frameLocation.height + newImageYDim + 50) > Toolkit.getDefaultToolkit().getScreenSize().height) {
                    frameLocation.width = 50;
                    frameLocation.height = 280;
                }
            }

        }

        return frameLocation;
    }

    /**
     * Changes vertical location of image when first displayed.
     * 
     * @return The new location.
     */
    public Dimension getNewFrameYLocation() {
        frameLocation.height += 20;

        if ( (frameLocation.width + 512) > Toolkit.getDefaultToolkit().getScreenSize().width) {
            frameLocation.width = 50;
            frameLocation.height = 240;
        } else if ( (frameLocation.height + 512) > Toolkit.getDefaultToolkit().getScreenSize().height) {
            frameLocation.width = 50;
            frameLocation.height = 240;
        }

        return frameLocation;
    }

    /**
     * Gets the string "Opening " or "Loading " based on what the progress bar should be displaying.
     * 
     * @return String OPENING_STR or LOADING_STR
     */
    public String getProgressBarPrefix() {
        return progressBarPrefix;
    }

    /**
     * Accessor for the mipav's data provenance .@return mipav's data provenance holder
     * 
     * @return DOCUMENT ME!
     */
    public ProvenanceHolder getProvenanceHolder() {
        return this.systemDPHolder;
    }

    /**
     * Return an num of images with frames(elements) from the image hashtable.
     * 
     * @return images number
     * 
     * @see CustomHashtable
     */
    public int getRegisteredFramedImagesNum() {
        int size = 0;
        final Enumeration<String> e = imageHashtable.keys();

        while (e.hasMoreElements()) {
            final ModelImage image = imageHashtable.get(e.nextElement());

            if (image.getParentFrame() != null) {
                size++;
            }
        }

        return size;

    }

    /**
     * Return a registered image from the image hashtable based on the name of the image. This works as long as the
     * image was registered and the name is unique.
     * 
     * @param name The name of the image to be retrieved.
     * 
     * @return the ImageModel associated with this name.
     * 
     * @throws IllegalArgumentException if the name is not in the image hastable.
     * 
     * @see CustomHashtable
     */
    public ModelImage getRegisteredImageByName(final String name) {
        if (imageHashtable.containsKey(name)) {
            return imageHashtable.get(name);
        } else {
            throw new IllegalArgumentException("Image name " + name + "is not valid.");
            // return null;
        }
    } // end getRegisteredImageByName()

    /**
     * Return an enumeration of keys (image names) from the image hashtable.
     * 
     * @return An Enumeration containing the keys (image names).
     * 
     * @see CustomHashtable
     */
    public Enumeration<String> getRegisteredImageNames() {
        return imageHashtable.keys();

    } // end getRegisteredImageNames()

    /**
     * Return an enumeration of images (elements) from the image hashtable.
     * 
     * @return An Enumeration containing the elements (images).
     * 
     * @see CustomHashtable
     */
    public Enumeration<ModelImage> getRegisteredImages() {
        return imageHashtable.elements();

    } // end getRegisteredImages()

    /**
     * Return an num of images (elements) from the image hashtable.
     * 
     * @return images number
     * 
     * @see CustomHashtable
     */
    public int getRegisteredImagesNum() {
        return imageHashtable.size();

    } // end getRegisteredImagesNum()

    /**
     * Display image registry frame.
     */
    public void imageRegistryMonitoring() {

        if (imgMonitorFrame != null) {
            imgMonitorFrame.dispose();
        }

        imgMonitorFrame = new ViewJFrameRegisteredImages();
    }

    /**
     * Brings all images and message frame to the front.
     */
    public void imagesToFront() {
        int i;
        Frame frame = null;

        if (mainFrame != null) {
            mainFrame.removeWindowListener(this);
        }

        if (messageFrame != null) {
            messageFrame.toFront();
        }

        for (i = imageFrameVector.size() - 1; i >= 0; i--) {
            frame = imageFrameVector.elementAt(i);
            frame.setVisible(true);
        }

        if (mainFrame != null) {
            mainFrame.addWindowListener(this);
        }
    }

    /**
     * Initializes the user interface. Starts by reading out some starting options out of the preferences file; setting
     * up the main frame; building the message frame and setting the message frame into the preferences; building the
     * menu; setting controls; initialising the DICOM dictionary; setting up the message field that is used in the main
     * frame; setting the application titles; and showing the main frame.
     * 
     * <p>
     * Recommendations for how to most easily perform modifications to the initialisation procedure are included with
     * the individual method comments, although in most cases, it suffices to simply over-ride an individual method.
     * </p>
     * 
     * @see #initPrefsFile()
     * @see #initUsingPreferences()
     * @see #initSetMainFrameDefaults(java.awt.LayoutManager, boolean)
     * @see #buildMessageFrame()
     * @see #buildMenu()
     * @see #setControls()
     * @see #initCreateMessageField(String)
     * @see #initSetTitles(String, String)
     */
    public void initializeGui() {
        MipavUtil.buildDefaultFonts();
        MipavUtil.buildCursors();
        initSetMainFrameDefaults(new BorderLayout(), true);
        buildMessageFrame();
        Preferences.setMessageFrame(messageFrame);

        // set the last stack flag
        lastStackFlag = Preferences.is(Preferences.PREF_LAST_STACK_FLAG);

        buildMenu();
        setControls();

        // removed since it was very out of date. we'll rely on the user to manage JVM version selection
        // initMacintoshJDKversionCheck();

        initCreateMessageBar();
        initSetTitles("Medical Image Processing, Analysis & Visualization (MIPAV) - v" + MipavUtil.getVersion(),
                "MIPAV: ");
        initDicomReceiver();

        mainFrame.pack();
        mainFrame.toFront();

        mainFrame.setFocusable(true);
        mainFrame.addKeyListener(this);
    }

    /**
     * Accessor to check whether the application frame visible or not.
     * 
     * @return isAppFrameVisible application frame visibility flag
     */
    public boolean isAppFrameVisible() {
        return isAppFrameVisible;
    }

    /**
     * Whether or not the VOI is a 2D (true = 2d, false = 3d+).
     * 
     * @return DOCUMENT ME! public boolean isClippedVOI2D() { return this.isClippedVOI2D; }
     */

    /**
     * Indicates if the image hashtable is empty.
     * 
     * @return A boolean showing state of hashtable.
     * 
     * @see CustomHashtable
     */
    public boolean isImageHashtableEmpty() {
        return imageHashtable.isEmpty();

    } // end isImageHashtableEmpty()

    /**
     * Indicates if the image name is found in the hashtable.
     * 
     * @param imageName the image name (key).
     * 
     * @return A boolean indicating that image name.
     * 
     * @see CustomHashtable
     */
    public boolean isImageRegistered(final String imageName) {
        return imageHashtable.containsKey(imageName);

    } // end isImageRegistered()

    /**
     * Accessor to see if a stand-alone plugin frame is visible (not app frame)
     * 
     * @return
     */
    public boolean isPlugInFrameVisible() {
        return isPlugInFrameVisible;
    }

    /**
     * Determines if the UserInterface is currently recording an action command as a shortcut.
     * 
     * @return boolean is it recording
     */
    public boolean isShorcutRecording() {
        return shortcutRecording;
    }

    /**
     * Tells the application to show the MIPAV UI or suppress it (progress bars, image frames, etc).
     * 
     * @param isVisible Set to false to hide the MIPAV UI.
     */
    public void setAppFrameVisible(final boolean isVisible) {
        isAppFrameVisible = isVisible;
    }

    /**
     * Tells the UI that a standalone plugin frame is visible
     * 
     * @param isVisible is the plugin frame visible
     */
    public void setPlugInFrameVisible(final boolean isVisible) {
        this.isPlugInFrameVisible = isVisible;
    }

    /**
     * Whether or not the VOI is a 2D (true = 2d, false = 3d+).
     * 
     * @return DOCUMENT ME! public boolean isClippedVOI2D() { return this.isClippedVOI2D; }
     */

    /**
     * Pass the key event to the selected image frame (if one exists). If not, check the shortcut table and attempt to
     * handle it here.
     * 
     * @param e a key event generated by the user
     */
    public void keyPressed(final KeyEvent e) {

        if (this.getActiveImageFrame() != null) {
            getActiveImageFrame().keyPressed(e);
        } else {
            final KeyStroke ks = KeyStroke.getKeyStrokeForEvent(e);

            final String command = Preferences.getShortcutCommand(ks);

            if (command != null) {
                actionPerformed(new ActionEvent(ks, 0, command));
            }
        }
    }

    /**
     * Pass the key event to the selected image frame (if one exists).
     * 
     * @param e a key event generated by the user
     */
    public void keyReleased(final KeyEvent e) {

        if (this.getActiveImageFrame() != null) {
            getActiveImageFrame().keyReleased(e);
        }
    }

    /**
     * Pass the key event to the selected image frame (if one exists).
     * 
     * @param e a key event generated by the user
     */
    public void keyTyped(final KeyEvent e) {

        if (this.getActiveImageFrame() != null) {
            getActiveImageFrame().keyTyped(e);
        }
    }

    /**
     * Display memory allocation frame (request more from the JVM on next JVM-start).
     */
    public void memoryAllocation() {

        if (mallocFrame != null) {
            mallocFrame.dispose();
        }

        mallocFrame = new JDialogMemoryAllocation();
    }

    /**
     * Display memory usage frame.
     */
    public void memoryFrame() {

        if (memoryFrame != null) {
            memoryFrame.dispose();
        }

        memoryFrame = new ViewJFrameMemory();
    }

    /**
     * this method closes all registered images
     */
    public void closeAllImages() {

        Enumeration<String> e = ViewUserInterface.getReference().getRegisteredImageNames();

        while (e.hasMoreElements()) {
            deleteItem((String) e.nextElement(), true);
        }

    }

    /**
     * Deletes the item specified by name. If false, only the model image is deleted
     * 
     * @param name the object to delete
     * @param deleteFrame whether the frame should be deleted along with the image
     */
    private void deleteItem(String name, boolean deleteFrame) {
        // System.out.println("selected name = " + selectedName);
        if (name == null) {
            return; // log nothing.
        }

        try {
            ModelImage image = ViewUserInterface.getReference().getRegisteredImageByName(name);
            ViewJFrameImage frame = ViewUserInterface.getReference().getFrameContainingImage(image);

            // An image that has a frame is only deleted when deleteFrame == true
            if (image != null && ( (deleteFrame && frame != null) || ( !deleteFrame && frame == null))) {
                image.disposeLocal();

                if (deleteFrame && frame != null) {
                    frame.close();
                }
            }

            Runtime.getRuntime().gc();
            Runtime.getRuntime().runFinalization();
        } catch (IllegalArgumentException iae) {

            // MipavUtil.displayError("There was a problem with the " +
            // "supplied name.\n" );
            Preferences.debug("Illegal Argument Exception in " + "ViewJFrameRegisteredImages when clicking on Delete. "
                    + "Somehow the Image list sent an incorrect name to " + "the image image hashtable.  \n"
                    + iae.getMessage() + "\n", 1);
            // System.out.println("Bad argument.");
        }
    }

    /**
     * This method opens an image and puts it into a frame.
     */
    public void openImageFrame() {
        final ViewOpenFileUI openFile = new ViewOpenFileUI(true);
        final boolean stackFlag = getLastStackFlag();

        // set the filter type to the preferences saved filter
        int filter = ViewImageFileFilter.TECH;

        try {
            filter = Integer.parseInt(Preferences.getProperty(Preferences.PREF_FILENAME_FILTER));
        } catch (final NumberFormatException nfe) {

            // an invalid value was set in preferences -- so fix it!
            filter = ViewImageFileFilter.TECH;
            Preferences.setProperty(Preferences.PREF_FILENAME_FILTER, Integer.toString(filter));
        }

        openFile.setFilterType(filter);

        final ArrayList<Vector<String>> openImagesArrayList = openFile.open(stackFlag);

        if (openImagesArrayList != null) {

            for (int i = 0; i < openImagesArrayList.size(); i++) {

                final Vector<String> openImageNames = openImagesArrayList.get(i);

                // if open failed, then imageNames will be null
                if (openImageNames == null) {
                    return;
                }

                final boolean sizeChanged = false;

                // if the SaveAllOnSave preference flag is set, then
                // load all the files associated with this image (VOIs, LUTs, etc.)
                if (Preferences.is(Preferences.PREF_SAVE_ALL_ON_SAVE)) {
                    final Enumeration<String> e = openImageNames.elements();

                    while (e.hasMoreElements()) {

                        try {
                            final String name = e.nextElement();
                            final ModelImage img = this.getRegisteredImageByName(name);

                            // get frame for image
                            final ViewJFrameImage imgFrame = img.getParentFrame();

                            // if the image size was changed to FLOAT, then don't
                            // load any luts (chances are they won't work)
                            if ( !sizeChanged) {

                                // load any luts
                                imgFrame.loadLUT(true, true);
                            }

                            // load any vois
                            imgFrame.loadAllVOIs(true);
                        } catch (final IllegalArgumentException iae) {

                            // MipavUtil.displayError("There was a problem with the supplied name.\n" );
                            Preferences.debug("Illegal Argument Exception in " + "ViewUserInterface.openImageFrame(). "
                                    + "Somehow the Image list sent an incorrect name to "
                                    + "the image image hashtable. " + "\n", 1);
                            Preferences.debug("Bad argument.");
                        }
                    }
                }
            }
        }
    }

    /**
     * Open an image and put it into a new frame, given the image file name.
     * 
     * @param imageFile the image file name with the path.
     */
    public void openImageFrame(final String imageFile) {
        openImageFrame(imageFile, false);
    }

    /**
     * Open an image and put it into a new frame, given the image file name.
     * 
     * @param imageFileName the file name, without the path
     * @param imageFileDir the directory where the file is
     */
    public void openImageFrame(final String imageFileName, final String imageFileDir) {
        final String imageFile = imageFileDir + File.separator + imageFileName;
        openImageFrame(imageFile);
    }

    /**
     * Open an image or images and put it into a new frame, given the image file name.
     * 
     * @param imageFile the image file name with the path.
     * @param multiFile If true, the image is composed of image slices each in their own file.
     */
    public void openImageFrame(final String imageFile, final boolean multiFile) {
        final ViewOpenFileUI openFile = new ViewOpenFileUI(false);
        String imageName;

        imageName = openFile.open(imageFile, multiFile, null);

        // if open failed, then imageNames will be null
        if (imageName == null) {
            return;
        }

        final boolean sizeChanged = false;

        // if the SaveAllOnSave preference flag is set, then
        // load all the files associated with this image (VOIs, LUTs, etc.)
        if (Preferences.is(Preferences.PREF_SAVE_ALL_ON_SAVE)) {

            try {
                final ModelImage img = getRegisteredImageByName(imageName);

                // get frame for image
                final ViewJFrameImage imgFrame = img.getParentFrame();

                // if the image size was changed to FLOAT, then don't
                // load any luts (chances are they won't work)
                if ( !sizeChanged) {

                    // load any luts
                    imgFrame.loadLUT(true, true);
                }

                // load any vois
                imgFrame.loadAllVOIs(true);
            } catch (final IllegalArgumentException iae) {

                // MipavUtil.displayError("There was a problem with the supplied name.\n" );
                Preferences
                        .debug("Illegal Argument Exception in " + "ViewUserInterface.openImageFrame(). "
                                + "Somehow the Image list sent an incorrect name to " + "the image image hashtable. "
                                + "\n", 1);
                Preferences.debug("Bad argument.");
            }
        }
    }

    /**
     * Attempts to open an image from the quicklist.
     * 
     * @param index int index of image on quicklist
     */
    public void openLastImage(final int index) {
        boolean multiFile = false;

        String temp = Preferences.getLastImageAt(index);

        if (temp == null) {
            return;
        } else if (temp.endsWith("M")) {
            multiFile = true;
        }

        temp = temp.substring(0, temp.lastIndexOf(","));

        final int idx = temp.lastIndexOf(File.separator);
        final String dir = temp.substring(0, idx) + File.separatorChar;

        // Set the default directory to the current opened image directory.
        setDefaultDirectory(dir);

        if ( !new File(temp).exists()) {
            MipavUtil.displayWarning("File has been deleted");
            buildMenu();
            setControls();

            return;
        }

        final ViewOpenFileUI fileUI = new ViewOpenFileUI(false);

        final String name = fileUI.open(temp, multiFile, null);

        // if the SaveAllOnSave preference flag is set, then
        // load all the files associated with this image (VOIs, LUTs, etc.)
        if (Preferences.is(Preferences.PREF_SAVE_ALL_ON_SAVE)) {

            try {
                final ModelImage img = ViewUserInterface.getReference().getRegisteredImageByName(name);

                // get frame for image
                final ViewJFrameImage imgFrame = img.getParentFrame();

                // load any luts
                imgFrame.loadLUT(true, true);

                // load any vois
                imgFrame.loadAllVOIs(true);
            } catch (final IllegalArgumentException iae) {

                // MipavUtil.displayError("There was a problem with the
                // supplied name.\n" );
                Preferences
                        .debug("Illegal Argument Exception in " + "ViewUserInterface.openImageFrame(). "
                                + "Somehow the Image list sent an incorrect name to " + "the image image hashtable. "
                                + "\n", 1);
                Preferences.debug("Bad argument.", Preferences.DEBUG_MINOR);
            }
        }

    }

    /**
     * Display Options dialog for all mipav options (including image specific options) to allow user to adjust display
     * in one place.
     */
    public void options() {

        if (optionsDialog != null) {
            optionsDialog.dispose();
        }

        optionsDialog = new JDialogMipavOptions();
    }

    /**
     * Required by the CommandLineParser interface. Processes MIPAV command line arguments that require MIPAV to have
     * already been initialized. Returns the next argument to be processed (finished if returns args.length)
     */
    public int parseArguments(final String[] args, final int initArg) {
        int i = 0, j, idx, index;
        String arg;
        Vector<String> voiPerImages = new Vector<String>();
        boolean voiFollowImage = false;
        int[] voiCount;
        int imgCount = 0;

        String scriptFile = null;
        final Vector<OpenFileInfo> imageList = new Vector<OpenFileInfo>();
        final Vector<Vector<String>> voiList = new Vector<Vector<String>>();

        if (args.length == 0) {
            return args.length;
        }

        // show the arguments
        System.err.println("Command line argument list:");
        Preferences.debug("Command line argument list:\n");

        for (i = 0; i < args.length; i++) {
            cmdLineArguments += args[i] + " ";
            System.err.println("argument[" + i + "]= " + args[i]);
            Preferences.debug("argument[" + i + "]= " + args[i] + "\n");

        }

        // special case: if there is only one argument, treat it as an image file name (unless it starts with a -)
        if (args.length == 1) {

            if ( !args[0].startsWith("-")) {
                final ViewOpenFileUI openFileUI = new ViewOpenFileUI(false);

                if (openFileUI.open(args[0], false, null) == null) {
                    MipavUtil.displayError("Unable to open image file: " + args[0]);
                    printUsageAndExit();
                }
            } else {
                MipavUtil
                        .displayError("To open files starting with \"-\", use the \"-i\" option.\nSee the usage message for more information.");
                printUsageAndExit();
            }

            return args.length;
        }

        // count the number of images in the arguments
        for (i = 0; i < args.length; i++) {
            arg = args[i];

            if (arg.equalsIgnoreCase("-i") || arg.equalsIgnoreCase("-m")) {
                imgCount++;
            }
        }

        // count the number of vois matched to the last image on the command line (can be multiple vois/image)
        voiCount = new int[imgCount];
        i = 0;
        idx = 0;
        j = 0;

        while (i < args.length) {
            arg = args[i];

            if (arg.equalsIgnoreCase("-i") || arg.equalsIgnoreCase("-m")) {

                for (j = i + 1; j < args.length; j++) {

                    if (args[j] != null) {
                        arg = args[j];

                        if (arg.equalsIgnoreCase("-i") || arg.equalsIgnoreCase("-m")) {
                            i = j;

                            break;
                        } else if (arg.equalsIgnoreCase("-v")) {
                            voiCount[idx]++;
                        }
                    }
                }

                i = j;
                idx++;
            } else {
                i++;
            }
        }

        i = 0;

        int voiIdx = 0, imgIdx = 0;
        boolean isMulti;
        File checkFile;

        parse: while (i < args.length) {
            arg = args[i];

            if (arg.startsWith("-")) {

                // parse commands which require an initialized mipav
                InstanceArgument c = InstanceArgument.getCommand(arg);
                if (c == null) {
                    i++;
                    continue parse;
                }

                switch (c) {

                    case Image:
                    case MultiImage:
                        isMulti = arg.equalsIgnoreCase("-m");

                        // System.out.println("imageName = " + args[i+1]);
                        voiFollowImage = true;
                        voiIdx = 0;

                        final String imgName = args[ ++i];
                        index = imgName.lastIndexOf(File.separatorChar);

                        if (index < 0) {
                            // only the image name was provided
                            // if the user provided defaultDir, first check to see if the file is there
                            // otherwise try to find the image under user.dir property
                            // if still not there, print usage and exit...since file was not found
                            if (isProvidedUserDefaultDir()) {
                                checkFile = new File(userDefaultDir + File.separator + imgName);
                                if (checkFile.exists()) {
                                    setDefaultDirectory(userDefaultDir);
                                } else {
                                    checkFile = new File(System.getProperty("user.dir") + File.separator + imgName);
                                    if (checkFile.exists()) {
                                        setDefaultDirectory(System.getProperty("user.dir"));
                                    } else {
                                        Preferences.debug("Can not find " + imgName, Preferences.DEBUG_MINOR);
                                        System.out.println("Can not find " + imgName);
                                        printUsageAndExit(c);
                                    }
                                }
                            } else {
                                checkFile = new File(System.getProperty("user.dir") + File.separator + imgName);
                                if (checkFile.exists()) {
                                    setDefaultDirectory(System.getProperty("user.dir"));
                                } else {
                                    Preferences.debug("Can not find " + imgName, Preferences.DEBUG_MINOR);
                                    System.out.println("Can not find " + imgName);
                                    printUsageAndExit(c);
                                }
                            }
                            imageList.add(new OpenFileInfo(getDefaultDirectory(), imgName, isMulti));
                        } else {
                            // either relative path or absolute path was provided
                            final String dir = imgName.substring(0, index + 1);
                            final String name = imgName.substring(index + 1);
                            checkFile = new File(imgName);
                            if (checkFile.isAbsolute()) {
                                if (checkFile.exists()) {
                                    imageList.add(new OpenFileInfo(dir, name, isMulti));
                                } else {
                                    Preferences.debug("Can not find " + imgName, Preferences.DEBUG_MINOR);
                                    System.out.println("Can not find " + imgName);
                                    printUsageAndExit(c);
                                }
                            } else {
                                if (isProvidedUserDefaultDir()) {
                                    checkFile = new File(userDefaultDir + File.separator + imgName);
                                    if (checkFile.exists()) {
                                        setDefaultDirectory(userDefaultDir);
                                        imageList.add(new OpenFileInfo(userDefaultDir + File.separator + dir, name,
                                                isMulti));
                                    } else {
                                        checkFile = new File(imgName);
                                        if (checkFile.exists()) {
                                            imageList.add(new OpenFileInfo(dir, name, isMulti));
                                        } else {
                                            Preferences.debug("Can not find " + imgName, Preferences.DEBUG_MINOR);
                                            System.out.println("Can not find " + imgName);
                                            printUsageAndExit(c);
                                        }
                                    }
                                } else {
                                    checkFile = new File(imgName);
                                    if (checkFile.exists()) {
                                        imageList.add(new OpenFileInfo(dir, name, isMulti));
                                    } else {
                                        Preferences.debug("Can not find " + imgName, Preferences.DEBUG_MINOR);
                                        System.out.println("Can not find " + imgName);
                                        printUsageAndExit(c);
                                    }
                                }

                            }

                        }
                        // imageFileNames.add(args[++i]);
                        break;

                    case RawImage:
                        // this is for specifying raw image parameters
                        final String rawString = args[ ++i];

                        // set the openfileinfo's rawInfo variable (for raw instructions)
                        imageList.lastElement().setRawImageInfo(new RawImageInfo(rawString));
                        break;

                    case Hide:
                        isAppFrameVisible = false;
                        break;

                    case Script:
                        // System.out.println("script name = " + args[i+1]);
                        scriptFile = args[ ++i];
                        break;

                    case Voi:
                        String voiName = args[ ++i];
                        index = voiName.lastIndexOf(File.separatorChar);

                        if (index < 0) {
                            voiName = this.getDefaultScriptDirectory() + File.separatorChar + voiName;
                        }

                        voiPerImages.add(voiName);

                        if (voiFollowImage) {
                            voiIdx++;

                            if (voiIdx == voiCount[imgIdx]) {
                                voiList.add(voiPerImages);
                                imgIdx++;
                                voiFollowImage = false;
                                voiPerImages = new Vector<String>();
                            }
                        }
                        break;

                    case SavedImageName:
                        String varValue = args[ ++i];
                        Preferences
                                .debug("cmd var:\tDefining parameter variable value from -o arg "
                                        + ActionSaveBase.SAVE_FILE_NAME + " -> " + varValue + "\n",
                                        Preferences.DEBUG_SCRIPTING);
                        VariableTable.getReference().storeVariable(ActionSaveBase.SAVE_FILE_NAME, varValue);
                        break;

                    case ScriptVariable:
                        final String varName = args[ ++i];
                        varValue = args[ ++i];
                        Preferences.debug("cmd var:\tDefining parameter variable value " + varName + " -> " + varValue
                                + "\n", Preferences.DEBUG_SCRIPTING);
                        VariableTable.getReference().storeVariable(varName, varValue);
                        break;

                    case Plugin:
                        Object thePlugIn = null;

                        // grab the plugin name
                        final String plugInName = args[ ++i];
                        // String plugInName = ((JMenuItem) (event.getSource())).getComponent().getName();

                        try {
                            thePlugIn = Class.forName(plugInName).newInstance();

                            // some plugins can now process command line arguments, can also control next command to be
                            // read
                            // once processing is complete
                            if (thePlugIn instanceof CommandLineParser) {
                                // plugin is given cursor position immediately after plugin name, but this can change
                                i = ((CommandLineParser) thePlugIn).parseArguments(args, ++i);
                            }

                            if (thePlugIn instanceof PlugInGeneric) {
                                // don't exit on an error (instead show the error dialog)
                                setPlugInFrameVisible(true);
                                setExitCmdLineOnError(false);

                                ((PlugInGeneric) thePlugIn).run();
                            } else {
                                MipavUtil
                                        .displayError("Plugin "
                                                + plugInName
                                                + " must implement the PlugInGeneric interface in order to be run from the command line.");
                            }
                        } catch (final ClassNotFoundException e) {
                            MipavUtil.displayError("PlugIn not found: " + plugInName);
                            printUsageAndExit(c);
                        } catch (final InstantiationException e) {
                            MipavUtil.displayError("Unable to load plugin (ins)");
                            printUsageAndExit(c);
                        } catch (final IllegalAccessException e) {
                            MipavUtil.displayError("Unable to load plugin (acc)");
                            printUsageAndExit(c);
                        }
                        break;
                }
            }
            i++;
        }

        // scriptFile may be null if we don't have a script to run (e.g., if we just want to open images/vois specified
        // on cmd line)
        runCmdLine(scriptFile, imageList, voiList);

        return args.length;
    }

    /**
     * Method that registers an image frame by putting it in the image frame vector and does NOT loads controls.
     * 
     * @param frame Frame to be registered with this the main UI.
     */
    public void regFrame(final Frame frame) {
        imageFrameVector.addElement(frame);
    }

    /**
     * Method that registers an image frame by adding it to the vector and loads controls.
     * 
     * @param frame Frame to be registered with this the main UI. The zero element frame is the active image. Any new
     *            image registered is made the active window.
     */
    public void registerFrame(final Frame frame) {

        if (imageFrameVector.size() != 0) {

            if (imageFrameVector.elementAt(0) instanceof ViewJFrameBase) {
                ((ViewJFrameBase) imageFrameVector.elementAt(0)).removeControls();
            }
        }

        imageFrameVector.insertElementAt(frame, 0);

        if (frame instanceof ViewJFrameBase) {
            ((ViewJFrameBase) frame).setControls();
            // ((ViewJFrameBase) frame).addKeyListener(this);
        }

        setTitle(frame.getTitle());
        // mainFrame.setTitle("MIPAV: " + frame.getTitle());
    }

    /**
     * Register image model by adding it to the image hashtable. Use the image name as the key. If the name is not
     * unique then the <code>CustomHashtable</code> will attempt to make it unique. The image name will be reset with
     * the new name. Classes calling this method need to check the returned key (image name) from this method.
     * 
     * @param image Image to be registered.
     * 
     * @return the String value of the key (image name)
     * 
     * @see CustomHashtable
     */
    public String registerImage(final ModelImage image) {
        String newName = null;

        synchronized (imageHashtable) {

            try {
                newName = imageHashtable.makeUniqueKey(image.getImageName());
                imageHashtable.put(newName, image);

                // if newName is different, then reset image name
                if ( (newName != null) && !newName.equals(image.getImageName())) {
                    image.setImageNamePrivate(newName);
                }
            } catch (final NullPointerException e) {
                MipavUtil.displayError("ViewUserInterface Error: " + e.getMessage());
            }
        }

        return newName;
    }

    /**
     * Register image model by adding it to the image hashtable. Use the String provided as the key. If the key is not
     * unique then the <code>CustomHashtable</code> will attempt to make it unique. The actual key used will be
     * returned.
     * 
     * @param key the desired image key
     * @param image Image to be registered.
     * 
     * @return the actual image key used (unique)
     * 
     * @see CustomHashtable
     */
    public String registerImage(final String key, final ModelImage image) {
        String newName = null;

        synchronized (imageHashtable) {

            try {
                newName = imageHashtable.makeUniqueKey(key);
                imageHashtable.put(newName, image);

                if ( (newName != null) && !newName.equals(image.getImageName())) {
                    image.setImageNamePrivate(newName);
                }
            } catch (final NullPointerException e) {
                MipavUtil.displayError("ViewUserInterface Error: " + e.getMessage());
            }

        }

        return newName;
    }

    /**
     * Method sets the parameter frame to top and active.
     * 
     * @param frame Frame to be set active (i.e. to the top of the list).
     */
    public void setActiveFrame(final Frame frame) {
        if (imageFrameVector.size() == 0) {
            return;
        }

        // if (frame == (imageFrameVector.elementAt(0))) {
        // System.err.println( "frame = 0" );
        // return;
        // }

        final int index = imageFrameVector.indexOf(frame);

        if (index < 0) {
            return;
        }

        registerFrame(frame); // Put it at the top
        imageFrameVector.removeElementAt(index + 1); // Remove second copy
    }

    // *****
    // end of initialize() sub-methods.
    // *****

    /**
     * Sets the clipped matrix for copy/paste actions.
     * 
     * @param tMat transmatrix for copy/paste
     */
    public void setClippedMatrix(final TransMatrix tMat) {
        this.clippedMatrix = tMat;
    }

    /**
     * Sets the menu for the main frame.
     */
    public void setControls() {
        if (mainFrame != null) {
            mainFrame.setJMenuBar(openingMenuBar);
            mainFrame.pack();
        }
    }

    /**
     * Accessor to set text of data FRAME.
     * 
     * @param str String to be displayed in text panel.
     */
    public final void setDataText(final String str) {

        if (messageFrame != null) {
            messageFrame.append(str, ViewJFrameMessage.DATA);
        }
    }

    /**
     * Sets directory location of last file access.
     * 
     * @param defaultDirectory Directory to set it to.
     */
    public void setDefaultDirectory(final String defaultDirectory) {
        Preferences.setProperty(Preferences.PREF_IMAGE_DIR, defaultDirectory);
    }

    /**
     * Sets directory location of the script files.
     * 
     * @param dir Directory to set the script directory to.
     */
    public void setDefaultScriptDirectory(final String dir) {
        Preferences.setProperty(Preferences.PREF_SCRIPT_DIR, dir);
    }

    /**
     * Accessor to set the DICOM receiver.
     * 
     * @param rcv the DICOM receiver
     */
    public void setDICOMCatcher(final DICOM_Receiver rcv) {
        DICOMcatcher = rcv;
    }

    /**
     * Accessor to set the DICOM query frame.
     * 
     * @param frame The DICOM query frame.
     */
    public void setDICOMQueryFrame(final ViewJFrameDICOMQuery frame) {
        DICOMQueryFrame = frame;
    }

    /**
     * Sets MIPAV to exit (true) or not (false) on an error when running from the command line
     * 
     * @param doExit
     */
    public void setExitCmdLineOnError(final boolean doExit) {
        this.exitCmdLineOnError = doExit;
    }

    /**
     * Tells whether or not to exit on an error when running from the command line
     * 
     * @return boolean whether to exit
     */
    public boolean doExitCmdLineOnError() {
        return this.exitCmdLineOnError;
    }

    /**
     * Checks whether the dialog should force the algorithm to replace the image (no new frame)
     * 
     * @return if only algorithm image replacement is allowed
     */
    public boolean doForceInPlace() {
        return this.forceAlgorithmInPlace;
    }

    /**
     * Sets the dialogs to only replace the image (no new frame)
     * 
     * @param doForce do force the dialog to replace the image (in-place)
     */
    public void setForceInPlace(final boolean doForce) {
        this.forceAlgorithmInPlace = doForce;
    }

    /**
     * Accessor to set text of global data FRAME.
     * 
     * @param str String to be displayed in text panel.
     */
    public final void setGlobalDataText(final String str) {

        if (messageFrame != null) {
            messageFrame.append(str, ViewJFrameMessage.DATA);
        }
    }

    /**
     * Sets last used script files in preferences.
     * 
     * @param script Script to set the LastScript to.
     */
    public void setLastScript(final String script) {
        Preferences.setProperty(Preferences.PREF_LAST_SCRIPT, script);
    }

    /**
     * Sets the last value for opened multi-files (or single).
     * 
     * @param lastStackFlag boolean
     */
    public void setLastStackFlag(final boolean lastStackFlag) {
        this.lastStackFlag = lastStackFlag;

        // save the last stack flag to preferences
        Preferences.setProperty(Preferences.PREF_LAST_STACK_FLAG, Boolean.toString(lastStackFlag));
    }

    /**
     * Tells the progress bar to say "Loading" rather than "Opening" for images being loaded if true.
     * 
     * @param doLoad boolean do set progress bar to load
     */
    public void setLoad(final boolean doLoad) {

        if (doLoad) {
            progressBarPrefix = ViewUserInterface.LOADING_STR;
        } else {
            progressBarPrefix = ViewUserInterface.OPENING_STR;
        }
    }

    /**
     * Accessor to set text of message field.
     * 
     * @param str String to be displayed in text field.
     */
    public void setMessageText(final String str) {

        if (messageField != null) {
            messageField.setText(str);
        }
    }

    /**
     * Sets the UI to either be/not be recording action command.
     * 
     * @param doRecord boolean true = is recording, false = not
     */
    public void setShortcutRecording(final boolean doRecord) {
        this.shortcutRecording = doRecord;
    }

    /**
     * Gets the application title from the preference file and prepends to the string passed into the method and
     * displays the resultant string in the title of the main frame.
     * 
     * @param str the application title
     */
    public void setTitle(final String str) {
        if (getAppTitle() != null) {
            mainFrame.setTitle(getAppTitle() + "     " + str);
        } else {
            mainFrame.setTitle(str);
        }
    }

    /**
     * Sets the Preference to use VOI Names instead of labels, then updates all frames to reflect this change with their
     * VOIs.
     * 
     * @param useName boolean show name instead of label
     */
    public void setUseVOIName(final boolean useName) {
        Preferences.setProperty(Preferences.PREF_SHOW_VOI_NAME, Boolean.toString(useName));

        final Enumeration<ModelImage> e = this.getRegisteredImages();

        while (e.hasMoreElements()) {

            try {
                this.getFrameContainingImage(e.nextElement()).updateImages();
            } catch (final NullPointerException ex) { // do nothing
            }
        }

    }

    /**
     * Sets the Preference to use XOR'ing when doing VOI operations, and updates the frames so that the checkboxes will
     * reflect this change.
     * 
     * @param doXOR boolean use XOR for VOIs
     */
    /*
     * public void setUseVOIXOR(final boolean doXOR) { Preferences.setProperty(false, Boolean.toString(doXOR));
     * 
     * final Enumeration<ModelImage> e = this.getRegisteredImages();
     * 
     * while (e.hasMoreElements()) {
     * 
     * try { this.getFrameContainingImage(e.nextElement()).setUseVOIXOR(doXOR); } catch (final NullPointerException ex) { //
     * do nothing } }
     *  }
     */

    /**
     * Change whether the GUI should be visible. The order is strage because we want the main frame to be first on the
     * taskbar, but also focused when the windows show up.
     * 
     * @param visible whether the message and main frames should be shown on the screen
     */
    public void setVisible(final boolean visible) {
        if (mainFrame != null && messageFrame != null) {
            mainFrame.setVisible(visible);
            messageFrame.setVisible(visible && Preferences.is(Preferences.PREF_SHOW_OUTPUT));
            mainFrame.setVisible(visible);
        }
    }

    /**
     * Displays the MIPAV Software Transfer Agreement in a JDialogText window.
     * 
     * <p>
     * The &quot;license.html&quot; file is read (using the <code>GetPath</code> class) and displayed as HTML with a
     * JDialogText. If the file is not found, or there is a problem opening it, a notation is made in the <code>
     * Preferences.debug</code>
     * window and is otherwise ignored. A warning box is displayed when the license dialog cannot be created (and throws
     * a <code>NullPointerException</code>). Finally, the main frame does not record this item in its list of
     * windows, so many instances of this window may be made.
     * </p>
     */
    public void showLicense() {
        showLicense("MIPAV license", "license.html");
    }

    /**
     * Displays the MIPAV Software Transfer Agreement in a JDialogText window.
     * 
     * <p>
     * The &quot;license.html&quot; file is read (using the <code>GetPath</code> class) and displayed as HTML with a
     * JDialogText. If the file is not found, or there is a problem opening it, a notation is made in the <code>
     * Preferences.debug</code>
     * window and is otherwise ignored. A warning box is displayed when the license dialog cannot be created (and throws
     * a <code>NullPointerException</code>). Finally, the main frame does not record this item in its list of
     * windows, so many instances of this window may be made.
     * </p>
     * 
     * @param title The title of the frame
     * @param filename the name of the license file.
     */
    public void showLicense(final String title, final String filename) {
        final JDialogText licenseDisplay = new JDialogText(mainFrame, title);

        final URL fileURL = getClass().getClassLoader().getResource(filename);

        if (fileURL == null) {
            Preferences.debug("Unable to open " + filename
                    + ".  Make sure it is in the same directory as MipavMain.class\n", Preferences.DEBUG_MINOR);
            MipavUtil.displayError(filename + " not found.\nTurn on debugging output for more information.");

            return;
        }

        BufferedReader br = null;

        try {
            br = new BufferedReader(new InputStreamReader(fileURL.openStream()));

            String licenseData = "";
            String line = null;

            while ( (line = br.readLine()) != null) {

                // licenseData += line + "\n";
                licenseData += line;
            }

            licenseDisplay.setContent(JDialogText.HTML_FORMAT, licenseData);
            licenseDisplay.setScrollPaneTop();
            licenseDisplay.setVisible(true);
        } catch (final FileNotFoundException fnfe) {
            Preferences.debug("License file not found.  Please include License File.\n");
        } catch (final IOException ioe) {
            Preferences.debug("Problem encountered reading License file.  Fix it.\n");
        } catch (final NullPointerException npe) {
            MipavUtil.displayWarning("NullPointerException encountered; failed to present license.");
            Preferences.debug("NullPointerException encountered; failed to present license." + "\n");
        } finally {

            try {

                if (br != null) {
                    br.close();
                }
            } catch (final IOException closee) {}
        }
    }

    /**
     * Opens a dialog for viewing/modifying shortcuts.
     * 
     * @param doUpdate whether to update the shortcut table
     */
    public void showShortcutEditor(final boolean doUpdate) {

        if (ViewUserInterface.shortcutEd == null) {
            ViewUserInterface.shortcutEd = new JDialogShortcutEditor();
        } else {

            if (doUpdate) {
                ViewUserInterface.shortcutEd.updateTable();
            }

            ViewUserInterface.shortcutEd.setVisible(true);
        }
    }

    /**
     * Shows the MIPAV splash screen for a few seconds, or until the user clicks it.
     */
    public void showSplashGraphics() {
        final ViewSplashScreen splashScreen = new ViewSplashScreen();

        if (splashScreen.loadOK()) {

            synchronized (this) {

                try {
                    splashScreen.setVisible(true);
                    wait(4000);
                } catch (final InterruptedException ie) {
                    ie.printStackTrace();
                } finally {
                    splashScreen.dispose();
                }
            }
        }
    }

    /**
     * Method that unregisters an image frame by removing it from the image frame vector.
     * 
     * @param frame Frame to be unregistered with this the main UI.
     */
    public void unregisterFrame(final Frame frame) {
        Frame topFrame;

        // System.out.println("VUI.unregisterFrame");
        if (imageFrameVector.size() == 0) {
            return;
        }

        if (frame instanceof ViewJFrameBase) {
            ((ViewJFrameBase) (frame)).removeControls();
        }

        imageFrameVector.removeElement(frame);

        if (imageFrameVector.size() == 0) {
            setControls();
            setTitle(" ");
        } else {
            topFrame = imageFrameVector.elementAt(0);

            if (topFrame instanceof ViewJFrameBase) {
                ((ViewJFrameBase) (topFrame)).setControls();
            }

            // mainFrame.setTitle("MIPAV: " + topFrame.getTitle());
            setTitle(topFrame.getTitle());
        }

        setMessageText("");
        imageFrameVector.trimToSize();

    }

    /**
     * Unregister image model by removing it from the image hashtable. Assume that the imageName is the key to the
     * image. Display an error if the image key is not found.
     * 
     * @param image Image to be unregistered.
     * 
     * @throws IllegalArgumentException if image is <code>null</code>
     */
    public void unRegisterImage(final ModelImage image) {

        if (image == null) {
            throw new IllegalArgumentException();
        }

        synchronized (imageHashtable) {
            if ( !imageHashtable.containsKey(image.getImageName())) {
                return;
            }

            // otherwise, remove the image
            imageHashtable.remove(image.getImageName());
        }

    }

    /**
     * Unregister image model by removing it from the image hashtable given the key to the hashtable. Display an error
     * if the image key is not found.
     * 
     * @param imageKey The key to the image to be unregistered (a string).
     * 
     * @throws IllegalArgumentException if imageKey is <code>null</code>
     */
    public void unRegisterImage(final String imageKey) {

        if (imageKey == null) {
            throw new IllegalArgumentException();
        }

        synchronized (imageHashtable) {

            // imageVector.removeElement(image);
            if ( !imageHashtable.containsKey(imageKey)) {
                return;
            }

            // otherwise, remove the image
            imageHashtable.remove(imageKey);
        }
    }

    /**
     * This method is a callback method. The purpose is to update the memory usage readout every one second. A separate
     * thread calls this method every one second.
     */
    public void updateMemoryUsage() {
        // System.out.println(Runtime.getRuntime().totalMemory());
        // System.out.println(Runtime.getRuntime().freeMemory());

        // final long memoryInUse = ( (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()) /
        // 1048576);
        final long memoryInUse = MipavUtil.getUsedHeapMemory() / 1048576;
        final long totalMemory = MipavUtil.getMaxHeapMemory() / 1048576;

        if ( ((double) memoryInUse / (double) totalMemory) > 0.8) {
            System.gc();
            memoryUsageLabel.setForeground(Color.red);
        } else {
            memoryUsageLabel.setForeground(Color.black);
        }

        memoryUsageLabel.setText("Memory: " + memoryInUse + "M / " + totalMemory + "M");
    }

    /**
     * This method updates the "whether multi-core should be used" button when the relevant button has been pushed in
     * either the preferences pane.
     */
    public void updateMultiCoreUsage() {
        if (Preferences.isMultiThreadingEnabled()) {
            btnMultiCore.setIcon(MipavUtil.getIcon("greenbox.gif"));
        } else {
            btnMultiCore.setIcon(MipavUtil.getIcon("redbox.gif"));
        }
    }

    /**
     * This method updates the "whether algorithms will use the GPU" when the relevant button has been pushed in either
     * the preferences pane.
     */
    public void updateGpuUsage() {
        if (Preferences.isGpuCompEnabled()) {
            btnGpuComp.setIcon(MipavUtil.getIcon("greenbox.gif"));
        } else {
            btnGpuComp.setIcon(MipavUtil.getIcon("redbox.gif"));
        }
    }

    /**
     * Do nothing - required by ScriptRecordingListener interface.
     * 
     * @param newScriptText Ignored.
     */
    public void updateScript(final String newScriptText) {}

    /**
     * Do nothing.
     * 
     * @param event the window event.
     */
    public void windowActivated(final WindowEvent event) {}

    /**
     * Do nothing.
     * 
     * @param event the window event.
     */
    public void windowClosed(final WindowEvent event) {}

    /**
     * Confirms if the user really wants to exit, then closes the application.
     * 
     * @param event Event that triggered this function.
     */
    public void windowClosing(final WindowEvent event) {
        Toolkit.getDefaultToolkit().beep();

        final int reply = JOptionPane.showConfirmDialog(mainFrame, "Do you really want to exit?", "MIPAV - Exit",
                JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

        if (reply == JOptionPane.YES_OPTION) {

            if (Preferences.is(Preferences.PREF_DATA_PROVENANCE)) {
                try {
                    ProvenanceRecorder.getReference().addLine(new ActionStopMipav());
                    writeDataProvenance();
                } catch (final Exception e) {
                    // nada
                }
            }

            memoryUsageThread.shutdown();

            if (DICOMQueryFrame != null) {
                DICOMQueryFrame.cancelPendingMoves();
                DICOMQueryFrame.cancelPendingQuery();
            }

            // dispose of registered images
            final Enumeration<String> names = this.getRegisteredImageNames();

            while (names.hasMoreElements()) {

                try {
                    final String name = names.nextElement();
                    final ModelImage img = this.getRegisteredImageByName(name);
                    img.disposeLocal();
                } catch (final IllegalArgumentException iae) {

                    // MipavUtil.displayError("There was a problem with the
                    // supplied name.\n" );
                    Preferences.debug("Illegal Argument Exception in " + "ViewUserInterface.windowClosing(). "
                            + "Somehow the Image list sent an incorrect name to " + "the image image hashtable. "
                            + "\n", 1);
                    Preferences.debug("Bad argument.", Preferences.DEBUG_MINOR);
                }
            }

            for (int i = 0; i < imageFrameVector.size(); i++) {
                final Object object = imageFrameVector.elementAt(i);

                if ( (object != null) && (object instanceof ViewJFrameImage)) {
                    ((ViewJFrameImage) object).close(); // this removes object from imageFrameVector, thus the need for
                    // the i--
                    i--;
                }
            }

            mainFrame.setVisible(false);
            mainFrame.dispose();
            System.exit(0); // close the application
        }
    }

    /**
     * Do nothing.
     * 
     * @param event the window event.
     */
    public void windowDeactivated(final WindowEvent event) {}

    /**
     * Deiconify only the other frames who's last state was normal (ie- restore other frames to their lastState).
     * 
     * @param event the deiconify window event.
     */
    public void windowDeiconified(final WindowEvent event) {

        // deiconify only the other frames who's last state was normal
        // (i.e. restore other frames to their lastState)
        final Frame[] frames = Frame.getFrames();

        for (int j = frames.length - 1; j >= 0; j--) {
            final Frame frame = frames[j];

            if (frame instanceof ViewJFrameBase) {
                frame.setState( ((ViewJFrameBase) frame).getLastState());
            } else if (frame instanceof ViewJFrameMessage) {
                frame.setState( ((ViewJFrameMessage) frame).getLastState());
            } else if ( !frame.equals(mainFrame)) {
                frame.setState(Frame.NORMAL);
            }
        }
    }

    /**
     * Iconify all other frames associateed with MIPAV.
     * 
     * @param event the iconify window event.
     */
    public void windowIconified(final WindowEvent event) {

        // iconify all the other frames as well
        // but first save window's current state to lastState --
        // that way when we de-iconify, we can restore other windows
        // to their original state
        final Frame[] frames = Frame.getFrames();

        for (int j = frames.length - 1; j >= 0; j--) {
            final Frame frame = frames[j];

            if (frame instanceof ViewJFrameBase) {
                ((ViewJFrameBase) frame).setLastState(frame.getState());
            } else if (frame instanceof ViewJFrameMessage) {
                ((ViewJFrameMessage) frame).setLastState(frame.getState());
            }

            if ( !frame.equals(mainFrame)) {
                frame.setState(Frame.ICONIFIED);
            }
        }
    }

    /**
     * Do nothing.
     * 
     * @param event the window event.
     */
    public void windowOpened(final WindowEvent event) {}

    /**
     * Writes Mipav's data provenance to the default location.
     * 
     * <p> .
     * </p>
     */
    public void writeDataProvenance() {

        // only write data provenance when there is something to write
        if ( (systemDPHolder != null) && (systemDPHolder.size() > 0)) {

            String provenanceFilename = Preferences.getProperty(Preferences.PREF_DATA_PROVENANCE_FILENAME);

            if (provenanceFilename == null) {
                provenanceFilename = System.getProperty("user.home") + File.separator + "mipav" + File.separator
                        + "dataprovenance.xmp";
                Preferences.setProperty(Preferences.PREF_DATA_PROVENANCE_FILENAME, provenanceFilename);
            }

            final File pFile = new File(provenanceFilename);

            final FileDataProvenance fdp = new FileDataProvenance(pFile.getName(), pFile.getParent(), systemDPHolder);

            try {
                fdp.writeXML();
            } catch (final Exception e) {}
        }
    }

    /**
     * Construct the panel which displays the current memory usage/limit and a garbage collection button.
     * 
     * @return the memory usage panel
     */
    protected JPanel initCreateMemoryUsagePanel() {
        final JPanel panel = new JPanel();
        JToolBar bottom = new JToolBar();

        final JButton btnRecycle = new JButton(MipavUtil.getIcon("recycle.gif"));
        final JButton btnReportBug = new JButton(MipavUtil.getIcon("reportbugsmall.gif"));
        btnRecycle.setFont(MipavUtil.font12);
        btnRecycle.setActionCommand("gc");
        btnRecycle.addActionListener(this);
        btnReportBug.setFont(MipavUtil.font12);
        //btnReportBug.setBorderPainted(false);
        btnReportBug.setFocusPainted(false);
        btnReportBug.setToolTipText("Report a bug in MIPAV");
        btnReportBug.setRolloverEnabled(true);
        //btnReportBug.setRolloverIcon(MipavUtil.getIcon("reportbugroll.gif"));
        btnReportBug.setActionCommand("reportbug");
        btnReportBug.addActionListener(this);

        memoryUsageLabel = new JLabel("Memory: ");
        memoryUsageLabel.setFont(MipavUtil.font12);

        panel.add(memoryUsageLabel);
        panel.add(btnRecycle);
//        bottom.add(btnReportBug);
//        bottom.setFloatable(false);
//        bottom.setRollover(false);
//        bottom.setBorderPainted(false);
        panel.add(btnReportBug);

        try {
            memoryUsageThread = new ReminderThread(1000);
            memoryUsageThread.addSubscriber(this, this.getClass().getMethod("updateMemoryUsage", new Class[] {}));
            memoryUsageThread.start();
        } catch (final Exception e) {
            // forget about it
        }

        return panel;
    }

    /**
     * Construct the panel which displays whether the CPU and GPU could be utilized by algorithms based on the
     * preferences settings by the user.
     * 
     * @return the memory usage panel
     */
    protected JPanel initCreateMultiCoreGpuIndicatorPanel() {
        final JPanel panel = new JPanel();

        final JLabel multiCoreEnabledLabel = new JLabel("Multi-core(" + ThreadUtil.getAvailableCores() + " cores):");
        multiCoreEnabledLabel.setFont(MipavUtil.font12);

        ImageIcon backgroundMulti;
        if (Preferences.isMultiThreadingEnabled()) {
            backgroundMulti = MipavUtil.getIcon("greenbox.gif");
        } else {
            backgroundMulti = MipavUtil.getIcon("redbox.gif");
        }
        btnMultiCore = new JButton(backgroundMulti);
        btnMultiCore.setBounds(new Rectangle(17, 17));
        btnMultiCore.setBorder(new EmptyBorder(2, 2, 2, 2));
        btnMultiCore.setFocusPainted(false);

        btnMultiCore.setFont(MipavUtil.font12);
        btnMultiCore.setActionCommand("Options");
        btnMultiCore.addActionListener(this);

        final JLabel gpuCompEnabledLabel = new JLabel(" GPU: ");
        gpuCompEnabledLabel.setFont(MipavUtil.font12);

        ImageIcon backgroundGpu;
        if (Preferences.isGpuCompEnabled() && OpenCLAlgorithmBase.isOCLAvailable()) {
            backgroundGpu = MipavUtil.getIcon("greenbox.gif");
        } else {
            backgroundGpu = MipavUtil.getIcon("redbox.gif");
        }
        btnGpuComp = new JButton(backgroundGpu);
        btnGpuComp.setBounds(new Rectangle(17, 17));
        btnGpuComp.setBorder(new EmptyBorder(2, 2, 2, 2));
        btnGpuComp.setFocusPainted(false);

        btnGpuComp.setFont(MipavUtil.font12);
        btnGpuComp.setActionCommand("Options");
        btnGpuComp.addActionListener(this);

        panel.add(multiCoreEnabledLabel);
        panel.add(btnMultiCore);
        panel.add(gpuCompEnabledLabel);
        panel.add(btnGpuComp);

        return panel;
    }

    /**
     * Create the panel containing components which show the application title initially, and will later be used to show
     * image coordinates and intensities.
     */
    protected void initCreateMessageBar() {
        final GridBagConstraints gbConstraints = new GridBagConstraints();
        final GridBagLayout gbLayout = new GridBagLayout();

        final JPanel msgBarPanel = new JPanel(gbLayout);

        final JTextField messageFieldPanel = initCreateMessageField(" MIPAV");
        final JPanel memoryUsagePanel = initCreateMemoryUsagePanel();
        final JPanel performancePanel = initCreateMultiCoreGpuIndicatorPanel();

        gbConstraints.weightx = 2;
        gbConstraints.insets = new Insets(0, 4, 0, 0);
        gbConstraints.fill = GridBagConstraints.HORIZONTAL;
        gbConstraints.anchor = GridBagConstraints.WEST;
        gbLayout.setConstraints(messageFieldPanel, gbConstraints);
        msgBarPanel.add(messageFieldPanel);

        gbConstraints.weightx = 0;
        gbConstraints.gridx = 1;
        gbConstraints.insets = new Insets(0, 0, 0, 0);
        gbConstraints.anchor = GridBagConstraints.EAST;
        gbConstraints.fill = GridBagConstraints.REMAINDER;
        gbLayout.setConstraints(performancePanel, gbConstraints);

        msgBarPanel.add(performancePanel);

        gbConstraints.gridx = 2;
        gbLayout.setConstraints(memoryUsagePanel, gbConstraints);

        msgBarPanel.add(memoryUsagePanel);

        mainFrame.getContentPane().add(msgBarPanel, "South");
    }

    /**
     * Creates the message field for the main frame using the title given. For other applications, extend this method to
     * call the super method with the appropriate title.
     * 
     * @param title the string use in the newly-created message field.
     * 
     * @return the message field (displays coordinate info when an image is opened)
     */
    protected JTextField initCreateMessageField(final String title) {
        messageField = new JTextField();
        messageField.setEditable(false);
        messageField.setFont(MipavUtil.font12);
        messageField.setBackground(new Color(215, 215, 215));
        messageField.setText(title);
        messageField.setPreferredSize(new Dimension(500, messageField.getPreferredSize().height));

        return messageField;
    }

    /**
     * Starts the DICOM receiver if the flag in the preference file is <code>true</code>.
     */
    protected void initDicomReceiver() {

        if (Preferences.is(Preferences.PREF_AUTOSTART_DICOM_RECEIVER)) {
            DICOMcatcher = new DICOM_Receiver();
            menuBuilder.setMenuItemSelected("Activate DICOM receiver", DICOMcatcher.isAlive());
        } else {

            if (DICOMcatcher != null) {
                DICOMcatcher.setStop();
            }

            menuBuilder.setMenuItemSelected("Activate DICOM receiver", false);
        }
    }

    /**
     * Method checks to verify that when running on a Machintosh, the JDK version is at least 1&#0x2e;4. It sets the
     * start up file "Info.plist" to use java1.4 if it can. The check for these routines is
     * System.getProperty("os.name").indexOf("Mac"), and we simply return doing nothing if the property for <tt>
     * os.name</tt>
     * is something different. If the os.name does not contain "Mac", and the Info.plist file is found,
     * 
     * <p>
     * "Info.plist" file to hold its startup arguments, so we need that file anyway. All exceptions caught here are not
     * propogated.
     * </p>
     */
    protected void initMacintoshJDKversionCheck() {

        // ignore this check if we are not running macintosh
        if (System.getProperty("os.name").indexOf("Mac") == -1) {
            return;
        }

        try {
            final File plistFile = JDialogMemoryAllocation.getStartupFile(this);

            // MIPAV will only check the info-file if MIPAV doesn't start with
            // java version 1.4; if it doesn't, we assume that we are Java 1.3
            // and that the start-file might need to be modified to start
            // with jvm 1.4+.
            // Note that we just assume that the VM will be found by OS 10
            // (OS A? -- 0x0A, of course)

            // Note: java 1.5 is also okay now..
            final String javaVersion = System.getProperty("java.version");

            if ( (javaVersion.indexOf("1.4") == -1) && (javaVersion.indexOf("1.5") == -1)) {
                final int answer = JOptionPane
                        .showConfirmDialog(null, "Does this machine have at least Java 1.4 installed on it?",
                                "Installed Java Virtual Machine check", JOptionPane.YES_NO_OPTION,
                                JOptionPane.QUESTION_MESSAGE);

                if (answer == JOptionPane.YES_OPTION) {
                    final FileReader reader = new FileReader(plistFile);
                    final BufferedReader breader = new BufferedReader(reader);
                    String line = breader.readLine();
                    final Vector<String> fileListing = new Vector<String>();

                    // assume that 1.4 isn't specified in the startup preferences
                    // file:
                    boolean needsJavaVersionSpecifier = true;
                    boolean hasASpecification = false;

                    while (line != null) {

                        if (line.indexOf("JVMVersion") != -1) { // contains MainClass
                            hasASpecification = true;
                            fileListing.add(line);
                            line = breader.readLine(); // look for the specified VM

                            if (line.indexOf("1.4") != -1) { // specifies 1.4 VM
                                needsJavaVersionSpecifier = false;
                            } else { // specified some other version of java.
                                needsJavaVersionSpecifier = true;
                            }

                            fileListing.add(line);
                        } else {
                            fileListing.add(line);
                        }

                        line = breader.readLine();
                    }

                    breader.close();
                    reader.close();

                    if (needsJavaVersionSpecifier) {

                        // file read in, information has been done.
                        final BufferedWriter bwriter = new BufferedWriter(new FileWriter(plistFile));
                        Preferences.debug("filelisting is " + fileListing.size(), Preferences.DEBUG_MINOR);

                        for (final Enumeration<String> e = fileListing.elements(); e.hasMoreElements();) {
                            line = e.nextElement();

                            if (hasASpecification) {

                                if (line.indexOf("JVMVersion") != -1) {
                                    bwriter.write(line);
                                    bwriter.newLine();
                                    line = e.nextElement(); // removing specifier line
                                    line = "\t\t<string>1.4+</string>";
                                }
                            } else {

                                if (line.indexOf("MainClass") != -1) {
                                    bwriter.write(line);
                                    bwriter.newLine();
                                    bwriter.write(e.nextElement());
                                    bwriter.newLine();
                                    bwriter.write("\t\t<key>JVMVersion</key>");
                                    bwriter.newLine();
                                    line = "\t\t<string>1.4+</string>";
                                }
                            }

                            bwriter.write(line);
                            bwriter.newLine();
                        }

                        bwriter.close();

                        MipavUtil.displayInfo("Java 1.4.1 was detected, and the Startup file was \n"
                                + " modified to use it.\n" + "You may continue to run MIPAV, but in order to take \n"
                                + "advantage of Java 1.4, you will need to restart.");
                        Preferences.debug("Java 1.4.1 was detected, and the Startup file was \n"
                                + " modified to use it.\n" + "You may continue to run MIPAV, but in order to take \n"
                                + "advantage of Java 1.4, you will need to restart." + "\n");
                    }
                } else { // answered "NO" to the question if it has 1.4 installed.
                    MipavUtil.displayWarning("This computer doesn't appear to have Java 1.4 installed.\n"
                            + "MIPAV needs at least Java 1.4.1 to work correctly, \n"
                            + "so some functions may be disabled.");
                    Preferences.debug("This computer doesn't appear to have Java 1.4 installed.\n"
                            + "MIPAV needs Java 1.4.1 to work correctly, \n" + "so some functions may be disabled."
                            + "\n");
                }
            }
        } catch (final FileNotFoundException ffe) {
            Preferences.debug("MemoryAllocation could not open a file, and caused an "
                    + "IOException in ViewUserInterface.\nThe exception message was:\n" + ffe.getLocalizedMessage()
                    + "\n");
        } catch (final IOException ioe) {
            Preferences.debug("MemoryAllocation could not open a file, and caused an "
                    + "IOException in ViewUserInterface.\nThe exception message was:\n" + ioe.getLocalizedMessage()
                    + "\n");
        } catch (final NullPointerException npe) {
            Preferences.debug("ViewUserInterface: A null pointer exception " + "was caught while "
                    + "trying to check the java version being used " + "by an Info.plist file.  The exception message "
                    + "was:\n" + npe.getLocalizedMessage() + "\n");
        } catch (final HeadlessException he) {

            // what else can be done? Currently MIPAV will die without a head.
            Preferences.debug("HEADLESS EXCEPTION.  UNABLE TO CONTINUE.", Preferences.DEBUG_MINOR);
            Preferences.debug(he.getLocalizedMessage(), Preferences.DEBUG_MINOR);
            he.printStackTrace();
        }
    }

    /**
     * Set the preferences file to use (Preferences defaults to mipav.preferences).
     */
    protected void initPrefsFile() {}

    /**
     * Gets the TRIM and TRIM_FLAG values from the preferences file, and if it cannot find them, it sets to some default
     * value: TRIM will be zero-point-three, and TRIM_FLAG will be true.
     * 
     * <p>
     * Over-ride this method if these defaults are unnacceptable.
     * </p>
     */
    protected void initPrefsTrim() {

        if (Preferences.getProperty(Preferences.PREF_TRIM) == null) {
            Preferences.setProperty(Preferences.PREF_TRIM, "0.3");
        }

        if (Preferences.getProperty(Preferences.PREF_TRIM_FLAG) == null) {
            Preferences.setProperty(Preferences.PREF_TRIM_FLAG, "true");
        }
    }

    /**
     * Sets the layout of the Main Frame with the given, preferred layout manager; makes the main frame resizable as
     * given; makes this class a window-listener; sets the default operation to do nothing on close; and the icon is
     * set. The image icon is set by the preferences.
     * 
     * <p>
     * Over-riding classes can alter the preferred layout most easily by over-riding this method, but calling this
     * method with a different layout and desired size-setting. To change the default close-operation with an
     * over-riding class, call this super method, then set the default close operation, rather than creating a custom
     * re-implemention of this method.
     * </p>
     * 
     * @see JFrame#setDefaultCloseOperation(int)
     * @see Preferences#getIconName()
     * 
     * @param prefLayout A layout manager to handle the main frame.
     * @param resize Whether to allow resizing of the main frame.
     */
    protected void initSetMainFrameDefaults(final LayoutManager prefLayout, final boolean resize) {
        mainFrame.setResizable(resize);
        mainFrame.addWindowListener(this);
        mainFrame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);

        try {
            mainFrame.setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (final FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
        }

        mainFrame.getContentPane().setLayout(prefLayout);
    }

    /**
     * Sets the titles for for the main frame and the application using the strings given. For other applications,
     * extend this method to call the super method with the appropriate title.
     * 
     * @param mainFrameTitle The string to use as the title for the main frame.
     * @param appTitle The string to put in the main frame's bottom status field.
     */
    protected void initSetTitles(final String mainFrameTitle, final String appTitle) {
        mainFrame.setTitle(mainFrameTitle);
    }

    /**
     * Performs a variety of start-up operations based on user-preferences; First, it tries to read from the preferences
     * file; if it cannot, it sets the user directory. It then follows from the preferences file:
     * 
     * <ol>
     * <li>splash screen</li>
     * <li>checks the LAX</li>
     * <li>sets the default directory</li>
     * <li>debug output</li>
     * <li>sets the TRIM</li>
     * </ol>
     * 
     * <p>
     * Over-riding classes should not over-ride this method unless one of these intermediate operations is not desired
     * or more operations are needed. To modify the defaults used in creating a preferences file, over-ride one of the
     * called methods.
     * </p>
     * 
     * @see #setDefaultDirectory(String)
     * @see #showSplashGraphics()
     * @see #checkLaxAgainstPreferences()
     * @see Preferences#print()
     * @see #initPrefsTrim()
     */
    protected void initUsingPreferences() {

        if ( !Preferences.read()) {
            Preferences.debug("ViewUserInterface: Unable to find preference file\n");
            setDefaultDirectory(System.getProperties().getProperty("user.dir"));
        }

        if ( !Preferences.isPreferenceSet(Preferences.PREF_LAX_CHECK)) {
            Preferences.setProperty(Preferences.PREF_LAX_CHECK, "true");
        }

        if (Preferences.is(Preferences.PREF_LAX_CHECK)) {
            checkLaxAgainstPreferences();
        }

        if (Preferences.getProperty(Preferences.PREF_IMAGE_DIR) == null) {
            setDefaultDirectory(System.getProperties().getProperty("user.dir"));
        }

        if (Preferences.isDebug()) {
            Preferences.print();
        }

        // create the system data provenance holder to catch all events
        systemDPHolder = new ProvenanceHolder();

        // start the Provenance recorder here if the preference is set
        if (Preferences.is(Preferences.PREF_DATA_PROVENANCE)) {

            ProvenanceRecorder.getReference().startRecording();
        }

        initPrefsTrim();
    }

    /**
     * This method is used when running MIPAV from the command line.
     * 
     * @param scriptFile The script to run.
     * @param imageList A list of OpenImageFile objects to use in the script.
     * @param voiList A list of VOIs to put into the various images.
     * 
     * @see #printUsageAndExit()
     */
    protected void runCmdLine(final String scriptFile, final Vector<OpenFileInfo> imageList,
            final Vector<Vector<String>> voiList) {
        final ViewOpenFileUI fileOpener = new ViewOpenFileUI(false);
        final Vector<String> imageNames = new Vector<String>();

        if (imageList.size() >= 0) {

            for (int i = 0; i < imageList.size(); i++) {
                final OpenFileInfo file = imageList.elementAt(i);
                final String fileName = file.getFullFileName();
                final boolean isMulti = file.isMulti();

                Preferences.debug("cmd line image file: " + fileName + "\n", Preferences.DEBUG_MINOR);
                fileOpener.setRawImageInfo(file.getRawImageInfo());
                fileOpener.open(fileName, isMulti, null);

                imageNames.addElement(fileOpener.getImage().getImageName());

                this.setDefaultDirectory(new File(fileName).getParent());

                Preferences.debug("Default dir: " + this.getDefaultDirectory() + "\n", Preferences.DEBUG_MINOR);

                try {
                    VOI[] voi;
                    FileVOI fileVOI;
                    final ModelImage image = fileOpener.getImage();

                    if ( (voiList.size() >= 1) && (voiList.elementAt(i) != null)) {

                        for (int x = 0; x < voiList.elementAt(i).size(); x++) {
                            final String fileNameIn = voiList.elementAt(i).elementAt(x);
                            final int index = fileNameIn.lastIndexOf(File.separatorChar);

                            final String directory = fileNameIn.substring(0, index + 1);
                            final String voiFileName = fileNameIn.substring(index + 1, fileNameIn.length());
                            fileVOI = new FileVOI(voiFileName, directory, image);
                            voi = fileVOI.readVOI(false);

                            for (final VOI element : voi) {
                                image.registerVOI(element);
                            }
                        }
                    }
                } catch (final Exception e) {
                    MipavUtil.displayError("Command line executing VOI error, check file names.");

                    return;
                }
            }

            // set the output dir (if provided from script) as the default dir
            if (isProvidedOutputDir()) {
                setDefaultDirectory(outputDir);
            }

            if (scriptFile != null) {
                ScriptRunner.getReference().runScript(scriptFile, imageNames, new Vector<String>());
            }

            // script is over...set the providedOutputDir back to false
            setProvidedOutputDir(false);

        }
    }

    /**
     * Test for lax memory sizes being the same as last run in preferences: displays a user-warning that the preferences
     * &amp; LAX files disagree and presents the JDialogMemoryAllocation dialog with "use preference" buttons to quicken
     * the matching process.
     * 
     * <p>
     * Note, this method does not throw any <code>NullPointerException</code>s.
     * </p>
     */
    private void checkLaxAgainstPreferences() {
        String[] mems = null;
        
        try {
            final File laxFile = JDialogMemoryAllocation.getStartupFile(this);
            mems = JDialogMemoryAllocation.readStartupFile(laxFile);

            if ( !Preferences.getProperty(Preferences.PREF_STARTING_HEAP_SIZE).equals(mems[0])
                    || !Preferences.getProperty(Preferences.PREF_MAX_HEAP_SIZE).equals(mems[1])) {
                if ( !MipavUtil.getForceQuiet()) {
                    MipavUtil.displayWarning("Heap size settings in the " + "environment startup file do not match \n"
                            + "those in the Preferences file.\n" + "Memory Allocation will display so you can "
                            + "ensure this is correct.");
                    new JDialogMemoryAllocation(this, true);
                } else {
                    MipavUtil.displayError("Heap size settings in the " + "environment startup file do not match \n"
                            + "those in the Preferences file.\n" + "Memory Allocation will display so you can "
                            + "ensure this is correct.");
                }

            }
            // else sizes match; there are no problems
        } catch (final NullPointerException npe) { // prefs not found/invalid strings
            // pref mem limit not set in prefs file, so use what's in the lax/plist
            Preferences.setProperty(Preferences.PREF_STARTING_HEAP_SIZE, mems[0]);
            Preferences.setProperty(Preferences.PREF_MAX_HEAP_SIZE, mems[1]);
//            if ( !GraphicsEnvironment.isHeadless() && !MipavUtil.getForceQuiet()) {
//                MipavUtil.displayWarning("Heap size settings in the "
//                        + "environment startup file either do not match \n"
//                        + "those in the Preferences file, or are non-existant.\n"
//                        + "Memory Allocation will display so you can " + "ensure this is correct.");
//                new JDialogMemoryAllocation(this, true);
//            } else {
//                MipavUtil.displayError("Heap size settings in the " + "environment startup file either do not match \n"
//                        + "those in the Preferences file, or are non-existant.\n"
//                        + "Please change these settings in the mipav.lax file.");
//            }
        } catch (final FileNotFoundException fnf) { // LAX not found
            Preferences.debug(fnf.getLocalizedMessage() + "\n");
            // MipavUtil.displayWarning(fnf.getLocalizedMessage());
            System.err.println(fnf.getLocalizedMessage());
        } catch (final IOException io) {
            MipavUtil.displayError("Error while checking starting options " + "file.");
        }
    }

    private class DicomQueryListener implements ActionListener {

        private JCheckBox checkBox;

        private JDialog dialog;

        /**
         * Ask user whether the dicom receiver should be enabled on startup.
         */
        private void queryForDicomAutostart() {

            final String message = "Would you like to have the DICOM receiver begin when MIPAV starts?";
            dialog = new JDialog();
            dialog.setLayout(new BorderLayout());
            dialog.setTitle("Auto-start option");
            final JPanel messagePanel = new JPanel();
            messagePanel.add(new JLabel(message));
            dialog.add(messagePanel, BorderLayout.NORTH);
            final JPanel checkBoxPanel = new JPanel();
            checkBox = new JCheckBox("Click here to stop this message from displaying.");
            checkBoxPanel.add(checkBox);
            dialog.add(checkBoxPanel, BorderLayout.CENTER);
            final JPanel yesNoPanel = new JPanel();
            final JButton yes = new JButton("Yes");
            yes.addActionListener(this);
            final JButton no = new JButton("No");
            no.addActionListener(this);
            yesNoPanel.add(yes);
            yesNoPanel.add(no);
            dialog.add(yesNoPanel, BorderLayout.SOUTH);
            dialog.setLocationRelativeTo(null);
            dialog.pack();
            dialog.setVisible(true);

        }

        public void actionPerformed(final ActionEvent e) {
            if (e.getActionCommand().equals("Yes")) {
                Preferences.setProperty(Preferences.PREF_AUTOSTART_DICOM_RECEIVER, "true");
            } else if (e.getActionCommand().equals("No")) {
                Preferences.setProperty(Preferences.PREF_AUTOSTART_DICOM_RECEIVER, "false");
            }
            Preferences.setProperty(Preferences.PREF_ASK_DICOM_RECEIVER, Boolean.valueOf( !checkBox.isSelected())
                    .toString());

            dialog.dispose();
        }
    }

    /**
     * Displays command line help information on usage of all commands and then exits.
     */
    public static void printUsageAndExit() {
        printUsageAndExit(null);
    }

    /**
     * Displays command line help information on usage to standard out and then into an informational dialog box then
     * exits the MIPAV application. Help display just shows the different options, display help, load image, load
     * script, load VOI, and hide menu bar, as well as examples of use.
     */
    public static void printUsageAndExit(Argument c) {
        final String helpInfo;
        if (c != null) {
            helpInfo = c.generateCmdUsageInfo();
        } else {
            helpInfo = generateCmdUsageInfo();
        }

        // print this usage help to the console
        System.out.println(helpInfo);

        // print the usage help to a dialog.
        // maybe later we can make this an option...
        if ( !GraphicsEnvironment.isHeadless()) {
            final JTextArea helpArea = new JTextArea(helpInfo);
            helpArea.setFont(MipavUtil.courier12);
            helpArea.setEditable(false);
            JOptionPane.showMessageDialog(null, helpArea, "Command line help", JOptionPane.INFORMATION_MESSAGE);
        }

        System.exit(0);
    }

    /**
     * Generates automatic list of available commands.
     */
    public static String generateCmdUsageInfo() {
        StringBuilder b = new StringBuilder();
        b.append("Here are MIPAV command line arguments that you can use:\n");
        for (StaticArgument c : StaticArgument.values()) {
            b.append("-" + c.getArgument()).append("\t").append(c.getHelp()).append("\n");
        }
        for (InstanceArgument c : InstanceArgument.values()) {
            b.append("-" + c.getArgument()).append("\t").append(c.getHelp()).append("\n");
        }
        b.append("Examples:").append("\n");
        b.append("> mipav imageFileName").append("\n").append("> mipav -i imageFileName -s scriptFileName -hide")
                .append("\n");
        b
                .append("> mipav -s scriptFileName -i imageFileName1 -v voiName1 -v voiName2 -i imageFileName2 -v voiName3 -inputDir defaultImageDirectoryPath -outputDir outputImageDirectoryPath");
        b.append("\n");
        return b.toString();
    }

    /**
     * This is the getter for providedOutputDir providedOutputDir: This boolean tells if the user has provided an
     * ouputDir parameter as a command line argument when running a script
     * 
     * @return
     */
    public static boolean isProvidedOutputDir() {
        return providedOutputDir;
    }

    public static boolean isProvidedUserDefaultDir() {
        return providedUserDefaultDir;
    }

    @SuppressWarnings("unchecked")
    public static Vector<Class<ActionDiscovery>> getDiscoverableActionList() {
        final Vector<Class<ActionDiscovery>> actionList = new Vector<Class<ActionDiscovery>>();

        final Vector<String> actionLocations = ScriptableActionLoader.getScriptActionLocations();
        final Vector<String> actionPackages = new Vector<String>();
        final Vector<String> actionDirs = new Vector<String>();
        for (String p : actionLocations) {
            final int index = p.lastIndexOf(".");
            p = p.substring(0, index + 1);
            actionPackages.add(p);
            actionDirs.add(p.replaceAll("\\.", Matcher.quoteReplacement(File.separator)));
        }

        String classFileName;
        Class action;
        for (int i = 0; i < actionDirs.size(); i++) {
            final String curDir = actionDirs.elementAt(i);
            final String curPackage = actionPackages.elementAt(i);

            final File locationDir = new File(curDir);
            if (locationDir.isDirectory()) {

                final File[] allFiles = locationDir.listFiles(new FileFilter() {
                    public boolean accept(final File f) {
                        if (f.getPath().endsWith(".class")) {
                            return true;
                        }
                        return false;
                    }
                });

                for (final File file : allFiles) {
                    classFileName = file.getName();

                    classFileName = classFileName.substring(0, classFileName.indexOf(".class"));

                    action = null;
                    try {
                        action = Class.forName(curPackage + classFileName);
                    } catch (final ClassNotFoundException e) {
                        Preferences.debug("Class not found: " + e.getMessage() + "\n", Preferences.DEBUG_SCRIPTING);
                    }

                    if (action != null) {
                        final Class<?>[] interfaces = action.getInterfaces();
                        for (final Class<?> interf : interfaces) {
                            if (interf.equals(ActionDiscovery.class)) {
                                actionList.add(action);
                                break;
                            }
                        }
                    }
                }
            }
        }

        return actionList;
    }

    public static String getOutputDir() {
        return outputDir;
    }

    public static File getSecondaryPluginsDir() {
        return secondaryPluginsDir;
    }

    public static String getUserDefaultDir() {
        return userDefaultDir;
    }

    public static void setProvidedOutputDir(boolean providedOutputDir) {
        ViewUserInterface.providedOutputDir = providedOutputDir;
    }

    public static void setProvidedUserDefaultDir(boolean providedUserDefaultDir) {
        ViewUserInterface.providedUserDefaultDir = providedUserDefaultDir;
    }

    public static void setOutputDir(String outputDir) {
        ViewUserInterface.outputDir = outputDir;
    }

    public static void setSecondaryPluginsDir(File secondaryPluginsDir) {
        ViewUserInterface.secondaryPluginsDir = secondaryPluginsDir;
    }

    public static void setUserDefaultDir(String userDefaultDir) {
        ViewUserInterface.userDefaultDir = userDefaultDir;
    }

    /**
     * Stores file name and switch for multifile.
     */
    private class OpenFileInfo {

        /** Path to the file (no file name). */
        private final String directory;

        /** The filename (no path). */
        private final String fileName;

        /** Whether the file is opened as a multifile image. */
        private boolean isMulti = false;

        /** Instructions for opening raw images. */
        private RawImageInfo rawInfo = null;

        /**
         * Creates a new OpenFileInfo object.
         * 
         * @param dir The directory the file is located in.
         * @param name The file name (no path).
         * @param isMulti Whether the file is opened as a multifile image.
         */
        public OpenFileInfo(final String dir, final String name, final boolean isMulti) {
            directory = dir;
            fileName = name;
            this.isMulti = isMulti;
        }

        /**
         * Get the directory containg the file.
         * 
         * @return The directory containg the file
         */
        @SuppressWarnings("unused")
        public String getDirectory() {
            return directory;
        }

        /**
         * Get the file name (no path).
         * 
         * @return The file name (no path).
         */
        @SuppressWarnings("unused")
        public String getFileName() {
            return fileName;
        }

        /**
         * Get the full file name (path and file name).
         * 
         * @return The full file name (path and file name).
         */
        public String getFullFileName() {

            if (directory.endsWith(File.separator)) {
                return directory + fileName;
            } else {
                return directory + File.separator + fileName;
            }
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public RawImageInfo getRawImageInfo() {
            return this.rawInfo;
        }

        /**
         * Return whether the file should be opened as a multifile image.
         * 
         * @return Whether the file should be opened as a multifile image.
         */
        public boolean isMulti() {
            return isMulti;
        }

        /**
         * DOCUMENT ME!
         * 
         * @param rI DOCUMENT ME!
         */
        public void setRawImageInfo(final RawImageInfo rI) {
            this.rawInfo = rI;
        }
    }

}
