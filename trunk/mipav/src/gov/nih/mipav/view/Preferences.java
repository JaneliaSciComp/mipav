package gov.nih.mipav.view;


import gov.nih.mipav.view.dialogs.JDialogOverlay;

import java.awt.*;
import java.awt.event.InputEvent;
import java.io.*;
import java.util.*;

import javax.swing.KeyStroke;


/**
 * This class reads the MIPAV preference file. The preference file stores a number of user specific parameters of the
 * application. It is stored in the < user directory >/mipav directory so that it can be read and written to by the
 * user. It is a text file and can be manually edited - not recommended.
 */
public class Preferences {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /**
     * Indicates an unknown system architecture.
     */
    public static final int ARCH_UNKNOWN = 0;

    /**
     * Indicates an x86 system architecture.
     */
    public static final int ARCH_X86 = 1;

    /**
     * Indicates an amd64 system architecture.
     */
    public static final int ARCH_AMD64 = 2;

    /**
     * Indicates a 32-bit power pc system architecture.
     */
    public static final int ARCH_PPC = 3;

    /**
     * Indicates a 64-bit power pc system architecture.
     */
    public static final int ARCH_PPC64 = 4;

    /**
     * Indicates a mips system architecture.
     */
    public static final int ARCH_MIPS = 5;

    /**
     * Indicates an alpha system architecture.
     */
    public static final int ARCH_ALPHA = 6;

    /**
     * Indicates an ia64 (itanium) system architecture.
     */
    public static final int ARCH_IA64 = 7;

    /**
     * Indicates an arm system architecture.
     */
    public static final int ARCH_ARM = 8;

    /**
     * Indicates a sparc system architecture.
     */
    public static final int ARCH_SPARC = 9;

    /**
     * Indicates a PA RISC system architecture.
     */
    public static final int ARCH_PA_RISC = 10;

    /** Constant that indicates use of saving XML and Analyze headers. */
    public static final String PREF_SAVE_XML_ON_HDR_SAVE = "SaveXMLOnHDRSave";

    /** Constant that indicates use of saving images, vois, and transfer functions. */
    public static final String PREF_SAVE_ALL_ON_SAVE = "SaveAllOnSave";

    /** Constant that indicates the saving of the xml thumbnail during a save. */
    public static final String PREF_SAVE_XML_THUMBNAIL = "SaveXMLThumbnail";
    
    /** Constant that indicates the file temp directory. */
    public static final String PREF_FILE_TEMP_DIR = "fileTempDir";

    /** Constant that indicates if lax file should be checked on mipav start. */
    public static final String PREF_LAX_CHECK = "PerformLaxCheck";

    /** Constant that indicates if user should be prompted on closing a frame. */
    public static final String PREF_CLOSE_FRAME_CHECK = "CloseFrameCheck";

    /** Constant indicating if VOI statistics file should be overwritten. */
    public static final String PREF_OVERWRITE_VOI_STATS = "OverwriteStatistics";

    /** Constant that indicates if data provenance is turned on. */
    public static final String PREF_DATA_PROVENANCE = "RecordDataProvenance";

    /** Constant that indicates if data provenance is turned on for individual images (and saved on image-save). */
    public static final String PREF_IMAGE_LEVEL_DATA_PROVENANCE = "ImageLevelDataProvenance";

    /** Constant that indicates the path where the system data provenance should be stored */
    public static final String PREF_DATA_PROVENANCE_FILENAME = "DataProvenanceFilename";

    /** Constant for debug s. */
    public static final String PREF_DEBUG = "DEBUG";

    /** Constant for whether logging is enabled */
    public static final String PREF_LOGGING_ENABLED = "LoggingEnabled";

    /** Constant that indicates if the Quick Mask operation should output to a new image */
    public static final String PREF_QUICK_MASK_NEW = "QuickMaskNew";

    /** Constant that indicates if the Paint-to-Mask operation should output to a new image */
    public static final String PREF_PAINT_TO_MASK_NEW = "PaintToMaskNew";

    /** Constant that indicates if the zoom should be linear (otherwise geometric) */
    public static final String PREF_ZOOM_LINEAR = "true";

    /** Constant that indicates the intensity label color. */
    public static final String PREF_INTENSITY_LABEL_COLOR = "IntensityLabelColor";

    /** Constant that indicates the intensity label background color. */
    public static final String PREF_INTENSITY_LABEL_BACKGROUND_COLOR = "IntensityLabelBackgroundColor";

    /** Constant that indicates the default frame rate */
    public static final String PREF_DEFAULT_FRAME_RATE = "DefaultFrameRate";

    /** Constant that indicates if the last used parameters for algorithm dialogs should be used when they are opened. */
    public static final String PREF_SAVE_DEFAULTS = "SaveDefaults";

    /** Constant that indicates if a prompt should be shown before an image file is overwritten. */
    public static final String PREF_SAVE_PROMPT_OVERWRITE = "SavePromptOverwrite";

    /** Constant that indicates if the DICOM overlay should be shown. */
    public static final String PREF_SHOW_DICOM_OVERLAYS = "ShowDICOMOverlays";

    /** Constant that indicates if the image overlay should be shown. */
    public static final String PREF_SHOW_IMAGE_OVERLAYS = "ShowImageOverlays";

    /** Constant that indicates if the paint border shown. */
    public static final String PREF_SHOW_PAINT_BORDER = "ShowPaintBorder";

    /** Constant that indicates if the private DICOM tags are to be shown */
    public static final String PREF_SHOW_PRIVATE_TAGS = "ShowPrivateTags";

    /** Constant that indicates if the splash graphic should be shown when mipav starts. */
    public static final String PREF_SHOW_SPLASH = "SplashGraphics";

    /** Constant that indicates the pathname of the log file. */
    public static final String PREF_LOG_FILENAME = "LogFilename";

    /** Constant that indicates ... i don't know what * */
    public static final String PREF_USE_AWT = "UseAWT";

    /** Constant identifying if the miscellaneous toolbar is on by default. */
    public static final String PREF_IMAGE_TOOLBAR_ON = "ImageToolbar";

    /** Constant indicating if the Paint Toolbar is on by default. */
    public static final String PREF_PAINT_TOOLBAR_ON = "PaintToolbar";

    /** Constant that indicates if the Scripting toolbar should be shown by default. */
    public static final String PREF_SCRIPTING_TOOLBAR_ON = "Scripting";

    /** Constant indicating if the VOI Toolbar is on by default. */
    public static final String PREF_VOI_TOOLBAR_ON = "VOIToolbar";

    /** Constant indicating the application shortcuts (user defined). */
    public static final String PREF_SHORTCUTS = "Shortcuts";

    /** Constant indicating the DICOM browser configuration. */
    public static final String PREF_DICOM_BROWSER_CONFIG = "DICOMBrowserConfig";

    /**
     * Constant that indicates whether the triplanar frame should wait for a mouse released event before repainting all
     * 9 tri-planar images. Inserted here because repainting 9 images in real time gets to be tedious.
     */
    public static final String PREF_FAST_TRIPLANAR_REPAINT = "FastTriPlanarRepaint";

    /** Constant that indicates if triplanar protractor should snap to nearest multiple of 90 degrees. */
    public static final String PREF_TRIPLANAR_SNAP90 = "TriPlanarSnap90";

    /** Constant that indicates if the triplanar should scroll the original image */
    public static final String PREF_TRIPLANAR_SCROLL_ORIGINAL = "TriPlanarScrollToOriginal";

    /** Constant that indicates whether the angle of line VOIs should be shown on screen. */
    public static final String PREF_SHOW_LINE_ANGLE = "ShowLineVOIAngle";

    /** Constant that indicates whether [SHIFT] must be held down to draw continuous contours (false = hold-shift). */
    public static final String PREF_CONTINUOUS_VOI_CONTOUR = "ContinuousVOIContours";

    /** Constant indicating the look of VOI Points drawn on an image. */
    public static final String PREF_VOI_POINT_DRAW_TYPE = "VOIPointDrawType";

    /** Constant indicating if VOIs should be saved as LPS or file coordinates */
    public static final String PREF_VOI_LPS_SAVE = "VOISaveAsLPS";

    /** Constant that indicates whether the triplanar frame should use the old 2x2 layout or the newer 3x1 layout. */
    public static final String PREF_TRIPLANAR_2X2_LAYOUT = "TriPlanar2x2Layout";

    /**
     * Constant that indicates the pixel gap away from the center of the crosshair (in one direction) actual gap is 2x.
     */
    public static final String PREF_CROSSHAIR_PIXEL_GAP = "CrosshairPixelGap";

    /** Constant that indicates the menu font. */
    public static final String PREF_MENU_FONT = "MenuFont";

    /** Constant that indicates the menu font size. */
    public static final String PREF_MENU_FONT_SIZE = "MenuFontSize";

    /** Constant that indicates the menu font color. */
    public static final String PREF_MENU_FONT_COLOR = "MenuFontColor";

    /** Constant that indicates whether to show the VOI name. */
    public static final String PREF_SHOW_VOI_NAME = "DrawVOIName";

    /** Constant that indicates whether to show output. */
    public static final String PREF_SHOW_OUTPUT = "ShowOutput";

    /** Constant that indicates the last stack flag. */
    public static final String PREF_LAST_STACK_FLAG = "LastStackFlag";

    /** Constant that indicates whether to use VOI XOR'ing. */
    public static final String PREF_USE_VOI_XOR = "UseVOIXOR";

    /** Constants that indicate the SRB server login properties. */
    public static final String PREF_USERNAME_SRB = "userNameSRB";

    /** Constant that indicates the SRB server host. */
    public static final String PREF_SERVER_HOST_SRB = "serverHostSRB";

    /** Constant that indicates the SRB server domain. */
    public static final String PREF_SERVER_DOMAIN_SRB = "serverDomainSRB";

    /** Constant that indicates the SRB. */
    public static final String PREF_SERVER_PORT_SRB = "serverPortSRB";

    /** Constant that indicates the SRB server authentication size. */
    public static final String PREF_SERVER_AUTHENTICATION_SRB = "serverAuthenticationSRB";

    /** Constant that indicates the default SRB storage resource. */
    public static final String PREF_STORAGE_RESOURCE_SRB = "defaultStorageResource";

    /** Constant that indicates the SRB version. */
    public static final String PREF_SRB_VERSION = "srbVersion";

    /** Constant that indicates the SRB transfer mode. */
    public static final String PREF_SRB_TRANSFER_MODE = "srbTransferMode";

    /** Constant that indicates the SRB temp directory. */
    public static final String PREF_SRB_TEMP_DIR = "srbTempDir";

    /** Constant that indicates the initial directory in which to open the file chooser of the image browser. */
    public static final String PREF_DEFAULT_IMAGE_BROWSER_DIR = "DefaultImageBrowserDirectory";

    /** Constant that indicated if the multi-theading is enabled */
    public static final String PREF_MULTI_THREADING_ENABLED = "multiThreadingEnabled";

    public static final String PREF_NUMBER_OF_THREADS = "numberOfThreads";

    /**
     * Constant that indicates the last used paint brush so that it will be set as the default when new images are
     * opened or mipav is restarted.
     */
    public static final String PREF_LAST_PAINT_BRUSH = "LastPaintBrush";

    /** Constant that indicates the user defined file type associations. */
    public static final String PREF_USER_FILETYPE_ASSOC = "userDefinedFileTypeAssociations";

    /** Constant that indicates the user defined file types. */
    public static final String PREF_USER_FILETYPES = "userDefinedFileTypes";

    /** ? */
    public static final String PREF_USER_FILETYPES_TEXTFIELDS = "userDefinedFileTypes_textField";

    /** Constant that indicates the filename filter. */
    public static final String PREF_FILENAME_FILTER = "FilenameFilter";

    /** Constant that indicates the current image directory. */
    public static final String PREF_IMAGE_DIR = "ImageDirectory";

    /** Constant that indicates the last used VOI text color. */
    public static final String PREF_VOI_TEXT_COLOR = "VOITextColor";

    /** Constant that indicates the last used VOI text background color. */
    public static final String PREF_VOI_TEXT_BACKGROUND_COLOR = "VOITextBackgroundColor";

    /** Constant that indicates the default dicom storage directory. */
    public static final String PREF_DICOM_STORAGE_DIR = "Storage1";

    /** Constant that indicates whether to enable the dicom receiver on startup. */
    public static final String PREF_AUTOSTART_DICOM_RECEIVER = "EnableDICOMReceiver";
    
    /** Constant that indicates whether to ask about starting dicom receiver on startup. */
    public static final String PREF_ASK_DICOM_RECEIVER = "AskToEnableDICOMReceiver";

    /** Constant that indicates the active image color. */
    public static final String PREF_ACTIVE_IMAGE_COLOR = "ActiveImageColor";

    /** number of recently used images to store in quicklist. */
    public static final String PREF_QUICKLIST_NUMBER = "QuickListNumber";

    /** constant for the names of the last X number of images used to be shown in quicklist. */
    public static final String PREF_LAST_X_IMAGES = "LastXImages";

    /** constant for the names of the last X number of projects used. */
    public static final String PREF_LAST_X_PROJECTS = "LastXProjects";

    /** constant for warning the user when audio will be discarded from opened AVIs. */
    public static final String PREF_WARN_AUDIO_AVI = "WarnAudioAVI";

    /** constant on whether to overwrite the voi statistics file automatically */
    public static final String PREF_OVERWRITE_STATISTICS = "OverwriteStatistics";

    /** Constant that indicates the crosshair cursor to be used. */
    public static final String PREF_CROSSHAIR_CURSOR = "CrosshairCursor";

    /** Constant that indicates the VOI Trim level variable. */
    public static final String PREF_TRIM = "TRIM";

    /** Constant that indicates the VOI Trim flag. */
    public static final String PREF_TRIM_FLAG = "TRIM_FLAG";

    /** Constant indicating the VOI Thickness. */
    public static final String PREF_VOI_THICKNESS = "VOIThickness";

    /** the voi draw color constant. */
    public static final String PREF_VOI_DRAW_COLOR = "VOIDrawColor";

    /** the starting color for VOIs (first drawn). */
    public static final String PREF_VOI_START_COLOR = "VOIColor";

    /** constant for logging. */
    public static final String PREF_DATA_PROVENANCE_MODE = "Log";

    /** Constant indicating which channels should be used for applying paint to a RGB image */
    public static final String PREF_RGB_PAINT_COMPONENTS = "RGBPaintComponents";

    /**
     * Constant that indicates whether .hdr/.img files should always be written in analyze format (as opposed to asking
     * whether to save as interfile or nifti).
     */
    public static final String PREF_ALWAYS_SAVE_IMG_AS_ANALYZE = "AlwaysSaveImgAsAnalyze";

    /**
     * Constant that indicates whether .hdr/.img files should always be written in interfile format (as opposed to
     * asking whether to save as analyze or nifti).
     */
    public static final String PREF_ALWAYS_SAVE_IMG_AS_INTERFILE = "AlwaysSaveImgAsInterfile";

    /**
     * Constant that indicates whether .hdr/.img files should always be written in nifti format (as opposed to asking
     * whether to save as analyze or interfile).
     */
    public static final String PREF_ALWAYS_SAVE_IMG_AS_NIFTI = "AlwaysSaveImgAsNifti";

    /**
     * Constant that indicates whether .mnc files should always be written in minc1 CDF format (as opposed to asking
     * whether to save as minc1 or minc2).
     */
    public static final String PREF_ALWAYS_SAVE_MNC_AS_MINC1 = "AlwaysSaveMncAsMinc1";

    /**
     * Constant that indicates whether .mnc files should always be written in minc2 HDF5 format (as opposed to asking
     * whether to save as minc1 or minc2).
     */
    public static final String PREF_ALWAYS_SAVE_MNC_AS_MINC2 = "AlwaysSaveMncAsMinc2";

    /** Constant that indicates the script directory. */
    public static final String PREF_SCRIPT_DIR = "ScriptDirectory";

    /** Constant that indicates the last script that was run. */
    public static final String PREF_LAST_SCRIPT = "LastScript";

    /** constant that indicates the dicom save dictionary. */
    public static final String PREF_DICOM_SAVE_DICTIONARY = "DICOMSaveDictionary";

    /** The starting heap size. */
    public static final String PREF_STARTING_HEAP_SIZE = "StartingHeapSize";

    /** The maximum heap size. */
    public static final String PREF_MAX_HEAP_SIZE = "MaximumHeapSize";

    /** Constant that indicates the default raw image extents. */
    public static final String PREF_RAW_EXTENTS = "RawImageExtents";

    /** Constant that indicates the default raw image big endian. */
    public static final String PREF_RAW_BIG_ENDIAN = "RawImageBigEndianByteOrder";

    /** Constant that indicates the default raw image resolutions. */
    public static final String PREF_RAW_RESOLUTIONS = "RawImageResolutions";

    /** Constant that indicates the default raw image units. */
    public static final String PREF_RAW_UNITS = "RawImageUnits";

    /** Constant that indicates the default raw image type. */
    public static final String PREF_RAW_TYPE = "RawImageType";

    /** Constant that indicates the default raw image data offset. */
    public static final String PREF_RAW_DATA_OFFSET = "RawImageDataOffset";

    /** Constant that indicates the LightBox row dependant variable. */
    public static final String PREF_LB_ROW_DEPENDENT = "LightBoxRowDependent";

    /** Constant that indicates the LightBox grid row variable. */
    public static final String PREF_LB_GRID_ROW = "LightBoxGridRow";

    /** Constant that indicates the LightBox grid column variable. */
    public static final String PREF_LB_GRID_COL = "LightBoxGridCol";

    /** Constant that indicates the LightBox grid size variable. */
    public static final String PREF_LB_GRID_SIZE = "LightBoxGridSize";
    
    /** Constant that indicates the increment between displayed lightbox slices */
    public static final String PREF_LB_INCREMENT = "LightBoxIncrement";

    /** Constant that indicates the LightBox grid color variable. */
    public static final String PREF_LB_GRID_COLOR = "LightBoxGridColor";

    /** Constant that indicates the LightBox border size variable. */
    public static final String PREF_LB_BORDER_SIZE = "LightBoxBorderSize";

    /** Constant that indicates the LightBox border color variable. */
    public static final String PREF_LB_BORDER_COLOR = "LightBoxBorderColor";

    /** Constant that indicates the LightBox selected border size variable. */
    public static final String PREF_LB_SELECTED_BORDER_SIZE = "LightBoxSelectedBorderSize";

    /** Constant that indicates the LightBox selected border color variable. */
    public static final String PREF_LB_SELECTED_BORDER_COLOR = "LightBoxSelectedBorderColor";

    /** Constant that indicates the LightBox magnification variable. */
    public static final String PREF_LB_MAG = "LightBoxMagnification";

    /** Constant that indicates the LightBox location variable. */
    public static final String PREF_LB_LOCATION = "LightBoxLocation";

    /** Constant that indicates the LightBox individual t-slice variable. */
    public static final String PREF_LB_TSLICE = "LightBoxIndividualTSlice";
    
    /** Constant that indicates continuous update of LightBox */
    public static final String PREF_LB_CUPDATE = "LightBoxContinuousUpdate";

    /** Constant that indicates the application title. */
    public static final String PREF_APP_TITLE = "ApplicationTitle";

    /** Constant that indicates the application icon. */
    public static final String PREF_APP_ICON = "ApplicationIcon";

    /** Constant that indicates whether to show image overlays. */
    public static final String PREF_SHOW_OVERLAYS = "ShowOverlays";

    /** plugin of algorithm type. */
    public static final String PREF_PLUGIN_ALGORITHM = "PlugInAlgorithm";

    /** plugin of file type. */
    public static final String PREF_PLUGIN_FILE = "PlugInFile";

    /** plugin of view type. */
    public static final String PREF_PLUGIN_VIEW = "PlugInView";
    
    /** ? */
    public static final String PREF_NINDS_ANON_PLUGIN_INPUTDIR = "NINDSAnonymizationPlugin_inputDir";
    
    /** ? */
    public static final String PREF_NINDS_ANON_PLUGIN_OUTPUTDIR = "NINDSAnonymizationPlugin_outputDir";
    
    /** ? */
    public static final String PREF_NINDS_ANON_PLUGIN_CSVDIR = "NINDSAnonymizationPlugin_CSVDir";

    /** The character that separates items (such as in the user file type definitions). */
    public static final String ITEM_SEPARATOR = ";";

    /**
     * The character that separates an item from its value in a definition or mapping (such as in the user file type
     * definitions).
     */
    public static final String DEFINITION_SEPARATOR = ":";

    /** Operating system constant for Windows. */
    public static final int OS_WINDOWS = 2;

    /** Operating system constant for Unix - DEFAULT for unrecognized OS. */
    public static final int OS_UNIX = 1;

    /** Operating system constant for the MAC. */
    public static final int OS_MAC = 0;

    /** Constant used to identify debugging level for general output. */
    public static final int DEBUG_MINOR = 0;

    /** Constant used to identify debugging level for general output by algorithms. */
    public static final int DEBUG_ALGORITHM = 1;

    /** Constant used to identify debugging level for general output by FileIO. */
    public static final int DEBUG_FILEIO = 2;

    /** Constant used to identify debugging level for general output by communications. */
    public static final int DEBUG_COMMS = 3;

    /** Constant used to identify debugging level for output of scripting system messages. */
    public static final int DEBUG_SCRIPTING = 4;

    /** The schema for the data provenance xml files */
    public static final String DATA_PROVENANCE_SCHEMA = "dataprovenance.xsd";

    /** Dicom tags to be saved */
    public static final String SAVE_DICOM_TAGS = "SaveDicomTags";

    /** A list of MIPAV properties. */
    private static Properties mipavProps;

    /** A default list of properties used if the mipav.preferences file cannot be found. */
    private static Properties defaultProps;

    /** Reference to the message frame where debug information can be displayed. */
    private static ViewJFrameMessage messageFrame;

    /** The MIPAV preferences file name (without path). */
    private static String preferencesFileName = "mipav.preferences";

    /** The place where user-individual mipav files should be put. */
    private static String preferencesDir = System.getProperty("user.home") + File.separator + "mipav";

    /** The MIPAV preferences file path and name. */
    private static String preferencesFile = preferencesDir + File.separator + preferencesFileName;

    /** Hashtable that holds all user-defined shortcuts */
    private static Hashtable userShortcutTable;

    /** Hashtable that holds the default shortcuts */
    private static Hashtable<String, KeyStroke> defaultShortcutTable = buildDefaultShortcuts();

    /** The current shortcut */
    private static KeyStroke currentShortcut;

    /**
     * Static properties structure to store default MIPAV preferences.
     */
    static {
        defaultProps = new Properties();

        defaultProps.setProperty(Preferences.PREF_AUTOSTART_DICOM_RECEIVER, "false");
        defaultProps.setProperty(Preferences.PREF_ASK_DICOM_RECEIVER, "true");
        defaultProps.setProperty(Preferences.PREF_TRIM, "0.3");
        defaultProps.setProperty(Preferences.PREF_DEBUG, "false, false, false, false");
        defaultProps.setProperty(Preferences.PREF_LOG_FILENAME, System.getProperty("user.dir") + File.separator
                + "mipav.log");
        defaultProps.setProperty(Preferences.PREF_RAW_EXTENTS, "256,256,0,0,0");
        defaultProps.setProperty(Preferences.PREF_RAW_BIG_ENDIAN, "true");
        defaultProps.setProperty(Preferences.PREF_RAW_RESOLUTIONS, "1.0,1.0,1.0,1.0,1.0");
        defaultProps.setProperty(Preferences.PREF_RAW_UNITS, "6,6,6,6"); // 6 = millimeters
        defaultProps.setProperty(Preferences.PREF_RAW_TYPE, "3"); // 3 = ModelStorageBase.SHORT
        defaultProps.setProperty(Preferences.PREF_RAW_DATA_OFFSET, "0");
        defaultProps.setProperty(Preferences.PREF_APP_TITLE, "MIPAV: ");
        defaultProps.setProperty(Preferences.PREF_APP_ICON, "divinci.gif");
        defaultProps.setProperty(Preferences.PREF_ACTIVE_IMAGE_COLOR, "ff0000"); // red
        defaultProps.setProperty(Preferences.PREF_CROSSHAIR_CURSOR, "default");
        defaultProps.setProperty(Preferences.PREF_TRIM_FLAG, "true");
        defaultProps.setProperty(Preferences.PREF_SHOW_SPLASH, "true");
        defaultProps.setProperty(Preferences.PREF_SCRIPTING_TOOLBAR_ON, "false");
        defaultProps.setProperty(Preferences.PREF_PAINT_TOOLBAR_ON, "false");
        defaultProps.setProperty(Preferences.PREF_SHOW_OVERLAYS, "false");
        defaultProps.setProperty(Preferences.PREF_VOI_LPS_SAVE, "true");
        defaultProps.setProperty(Preferences.PREF_QUICK_MASK_NEW, "false");
        defaultProps.setProperty(Preferences.PREF_PAINT_TO_MASK_NEW, "false");
        defaultProps.setProperty(Preferences.PREF_ZOOM_LINEAR, "true");

        /** Medical Formats(*.dcm; *.ima; *.img; *.mnc; *.sig; *.xml; *.head) */
        defaultProps.setProperty(Preferences.PREF_FILENAME_FILTER, "8"); // 8 = ViewImageFileFilter.TECH
        defaultProps.setProperty(Preferences.PREF_SAVE_XML_THUMBNAIL, "false");
        defaultProps.setProperty(Preferences.PREF_SAVE_ALL_ON_SAVE, "false");
        defaultProps.setProperty(Preferences.PREF_OVERWRITE_STATISTICS, "false");
        defaultProps.setProperty(Preferences.PREF_LAST_X_IMAGES, "");
        defaultProps.setProperty(Preferences.PREF_QUICKLIST_NUMBER, "4");
        defaultProps.setProperty(Preferences.PREF_LAST_X_PROJECTS, "");
        defaultProps.setProperty(Preferences.PREF_WARN_AUDIO_AVI, "true");
        defaultProps.setProperty(Preferences.PREF_SHOW_PAINT_BORDER, "false");
        defaultProps.setProperty(Preferences.PREF_SHOW_PRIVATE_TAGS, "false");

        // lightbox properties
        defaultProps.setProperty(Preferences.PREF_LB_ROW_DEPENDENT, "true"); // display by columns
        defaultProps.setProperty(Preferences.PREF_LB_GRID_ROW, "0"); // compute 'best fit' num rows
        defaultProps.setProperty(Preferences.PREF_LB_GRID_COL, "2"); // use 2 columns
        defaultProps.setProperty(Preferences.PREF_LB_GRID_SIZE, "5");
        defaultProps.setProperty(Preferences.PREF_LB_INCREMENT, "1"); // Display every light box frame
        defaultProps.setProperty(Preferences.PREF_LB_GRID_COLOR, "000000");
        defaultProps.setProperty(Preferences.PREF_LB_BORDER_SIZE, "3");
        defaultProps.setProperty(Preferences.PREF_LB_BORDER_COLOR, "960000");
        defaultProps.setProperty(Preferences.PREF_LB_SELECTED_BORDER_SIZE, "2");
        defaultProps.setProperty(Preferences.PREF_LB_SELECTED_BORDER_COLOR, "ffff00");
        defaultProps.setProperty(Preferences.PREF_LB_MAG, "45.0");
        defaultProps.setProperty(Preferences.PREF_LB_LOCATION, "-10,-10"); // use incorrect value to compute 'best'
        // location

        // look and feel properties
        defaultProps.setProperty(Preferences.PREF_MENU_FONT, "Serif");
        defaultProps.setProperty(Preferences.PREF_MENU_FONT_SIZE, "12");

        defaultProps.setProperty(Preferences.PREF_MENU_FONT_COLOR, "BLACK");
        defaultProps.setProperty(Preferences.PREF_SHOW_OUTPUT, "true");

        // username and srb server properties
        defaultProps.setProperty(PREF_USERNAME_SRB, "");
        defaultProps.setProperty(PREF_SERVER_HOST_SRB, "");
        defaultProps.setProperty(PREF_SERVER_DOMAIN_SRB, "");
        defaultProps.setProperty(PREF_SERVER_PORT_SRB, "5544");
        defaultProps.setProperty(PREF_SERVER_AUTHENTICATION_SRB, "ENCRYPT1");
        defaultProps.setProperty(PREF_STORAGE_RESOURCE_SRB, "");

        // multi-threading property
        defaultProps.setProperty(PREF_MULTI_THREADING_ENABLED,
                (gov.nih.mipav.util.MipavUtil.getAvailableCores() > 1) ? "true" : "flase");
        defaultProps.setProperty(PREF_NUMBER_OF_THREADS, String.valueOf(gov.nih.mipav.util.MipavUtil
                .getAvailableCores()));
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Adds a new shortcut to the hashtable. if there is already a command predefined for the keystroke set from <code>
     * setShortcut()</code>,
     * it will be removed then replaced with the new command.
     * 
     * @param command the command to map
     * 
     * @return DOCUMENT ME!
     */
    public static boolean addShortcut(String command) {

        if ( (command != null) && (currentShortcut != null)) {

            if (isDefaultCommand(command)) {
                MipavUtil.displayWarning("This is a default function: shortcut can not be remapped");

                return false;
            }

            if (userShortcutTable.containsKey(command)) {
                userShortcutTable.remove(command);
            }

            if (userShortcutTable.containsValue(currentShortcut)) {
                Enumeration e = userShortcutTable.keys();

                while (e.hasMoreElements()) {
                    String key = (String) e.nextElement();

                    if (currentShortcut.equals(userShortcutTable.get(key))) {
                        userShortcutTable.remove(key);
                    }
                }
            }

            userShortcutTable.put(command, currentShortcut);

            MipavUtil.displayInfo("Shortcut captured: " + currentShortcut.toString().replaceAll("pressed", "").trim()
                    + " : " + command);

            saveShortcuts();
            currentShortcut = null;

            return true;
        }

        return false;
    }

    /**
     * Adds a new shortcut to the hashtable with a given keystroke. if there is already a command predefined for the
     * keystroke set from <code>
     * setShortcut()</code>, it will be removed then replaced with the new command.
     * 
     * @param command the command to map
     * 
     * @return DOCUMENT ME!
     */
    public static boolean addShortcut(String command, KeyStroke shortcut) {

        if ( (command != null) && (shortcut != null)) {

            System.out.println("shortcut: " + command + ": " + shortcut.toString());
            if (isDefaultCommand(command)) {
                MipavUtil.displayWarning("This is a default function: shortcut can not be remapped");

                return false;
            }

            if (userShortcutTable.containsKey(command)) {
                userShortcutTable.remove(command);
            }

            if (userShortcutTable.containsValue(shortcut)) {
                Enumeration e = userShortcutTable.keys();

                while (e.hasMoreElements()) {
                    String key = (String) e.nextElement();

                    if (shortcut.equals(userShortcutTable.get(key))) {
                        userShortcutTable.remove(key);
                    }
                }
            }

            userShortcutTable.put(command, shortcut);

            /*
             * MipavUtil.displayInfo("Shortcut captured: " + shortcut.toString().replaceAll("pressed", "").trim() + " : " +
             * command);
             */
            saveShortcuts();
            shortcut = null;

            return true;
        }

        return false;
    }

    /**
     * Builds a hashtable of actioncommands (keys) with associated keystrokes from the Preferences file.
     */
    public static final void buildShortcutTable() {
        userShortcutTable = new Hashtable();

        String str = getProperty(Preferences.PREF_SHORTCUTS);

        // System.err.println(str);
        String keyStr = null;
        String shortcutStr = null;
        StringTokenizer tok = null;

        int modifiers;

        boolean success = true;

        try {

            if (str != null) {
                tok = new StringTokenizer(str, ";");

                String token;

                while (tok.hasMoreTokens()) {
                    token = tok.nextToken();

                    // System.err.println("token is: " + token);
                    shortcutStr = token.substring(0, token.indexOf(","));

                    if ( !defaultShortcutTable.containsKey(shortcutStr)) {

                        keyStr = token.substring(token.indexOf(",") + 1, token.length()).trim();
                        modifiers = 0;

                        if (keyStr.indexOf("CTRL") != -1) {
                            modifiers += Event.CTRL_MASK;
                        }

                        if (keyStr.indexOf("ALT") != -1) {
                            modifiers += Event.ALT_MASK;
                        }

                        if (keyStr.indexOf("SHIFT") != -1) {

                            // System.err.println("KEYSTRING: " + keyStr);
                            modifiers += Event.SHIFT_MASK;
                            // System.err.println("found shift: " + keyStr.charAt(keyStr.length()-1));
                        }

                        int fIndex = 0;

                        if ( (keyStr.length() == 2) || (keyStr.length() == 3)) {
                            fIndex = Integer.parseInt(keyStr.substring(1));

                            userShortcutTable.put(shortcutStr, KeyStroke.getKeyStroke(MipavUtil.functionKeys[fIndex],
                                    0, false));
                        } else {

                            if (modifiers != 0) {

                                userShortcutTable.put(shortcutStr, KeyStroke.getKeyStroke(keyStr
                                        .charAt(keyStr.length() - 1), modifiers, false));
                            } else {
                                userShortcutTable.put(shortcutStr, KeyStroke.getKeyStroke(keyStr.charAt(0), modifiers,
                                        false));
                            }
                        }
                    }
                }

                // throw in the defaults just in case

            } else {
                success = false;
            }
        } catch (Exception e) {
            e.printStackTrace();
            MipavUtil.displayWarning("Error reading shortcut preferences:  default shortcuts restored");
            success = false;
        }

        // return table;
    }

    /**
     * If at least one debug level is turned on, output the string.
     * 
     * @param string String string to be output
     */
    public static final void debug(String string) {

        if (messageFrame == null) {
            System.err.println("Message frame is null");
        } else {
            boolean[] levels = getDebugLevels();

            if (levels[0] || levels[1] || levels[2] || levels[3] || levels[4]) {
                messageFrame.append(string, ViewJFrameMessage.DEBUG);
            }
        }
    }

    /**
     * If the debug is turned on for the given level, output to the messageFrame.
     * 
     * @param string String String to be output
     * @param level int level of debug to check
     */
    public static final void debug(String string, int level) {
        boolean[] debugLevels = getDebugLevels();

        if (messageFrame == null) {
            System.err.println("Warning: Message frame is null");
            System.out.println(string);
        } else {

            try {

                if (debugLevels[level]) {
                    messageFrame.append(string, ViewJFrameMessage.DEBUG);
                }
            } catch (Exception e) {}
        }
    }

    /**
     * Indicates whether debug is turned on for the given level.
     * 
     * @param level debugging level to check for
     * 
     * @return true if the given debugging level is activated
     */
    public static final boolean debugLevel(int level) {

        String str = getProperty(Preferences.PREF_DEBUG);

        if (str == null) {
            return false;
        } else {

            // parse out the booleans
            StringTokenizer st = new StringTokenizer(str, ",");
            int index = 0;
            String token = null;

            while (st.hasMoreTokens()) {
                token = st.nextToken().trim();

                try {

                    if (index == level) {

                        if (token.equalsIgnoreCase("TRUE") || token.equalsIgnoreCase("on")
                                || token.equalsIgnoreCase("yes")) {
                            return true;
                        } else {
                            return false;
                        }
                    }
                } catch (Exception e) {
                    System.err.println("error parsing debug levels");

                    return false;
                }

                index++;
            }
        }

        return false;
    }

    /**
     * Gets the on/off values for the 4 DEBUG levels (minor, algorithm, fileIO, comms).
     * 
     * @return boolean[] debug levels
     */
    public static final boolean[] getDebugLevels() {
        boolean[] levels = new boolean[5];

        String str = Preferences.getProperty(Preferences.PREF_DEBUG);
        StringTokenizer st = new StringTokenizer(str, ",");
        int index = 0;

        while (st.hasMoreTokens()) {

            try {
                levels[index] = Boolean.valueOf(st.nextToken()).booleanValue();
                index++;
            } catch (Exception e) {
                System.err.println("exception: " + e);
            }
        }

        return levels;
    }

    /**
     * Gets the default frame rate used in saving AVI movies. Default value is 10.0.
     * 
     * @return the default frame rate as a floating point number.
     */
    public static float getDefaultFrameRate() {
        String str = getProperty(Preferences.PREF_DEFAULT_FRAME_RATE);

        if (str != null) {

            try {
                return Float.parseFloat(str);
            } catch (Exception ex) {
                setProperty(Preferences.PREF_DEFAULT_FRAME_RATE, "10.0");
            }
        } else {
            setProperty(Preferences.PREF_DEFAULT_FRAME_RATE, "10.0");
        }

        return 10.0f;
    }

    /**
     * Accessor to get the default chat/file server host key.
     * 
     * @return default host key
     */
    public static String getDefaultHostKey() {
        String key = "Host1";
        String defaultKey = key;
        StringTokenizer tok;

        if (mipavProps == null) {
            read();
        }

        while (getProperty(key) != null) {
            tok = new StringTokenizer(getProperty(key), ";");

            if (tok.countTokens() == 5) {
                defaultKey = key;
            }

            key = key.substring(0, 4) + (Integer.valueOf(key.substring(4)).intValue() + 1);
        }

        return defaultKey;
    }

    /**
     * Accessor to get default DICOM image server key.
     * 
     * @return default server key
     */
    public static String getDefaultServerKey() {
        String key = "Server1";
        String defaultKey = key;
        StringTokenizer tok;

        if (mipavProps == null) {
            read();
        }

        while (getProperty(key) != null) {
            tok = new StringTokenizer(getProperty(key), ";");

            if (tok.countTokens() == 5) {
                defaultKey = key;
            }

            key = key.substring(0, 6) + (Integer.valueOf(key.substring(6)).intValue() + 1);
        }

        return defaultKey;
    }

    /**
     * Gets the Default Hashtable used to store the shortcut keystrokes and commands.
     * 
     * @return Hashtable the shortcut hashtable
     */
    public static Hashtable getDefaultShortcutTable() {
        return defaultShortcutTable;
    }

    /**
     * Accessor to get default DICOM image storage location.
     * 
     * @return default storage information
     */
    public static String getDefaultStorageKey() {
        String key = Preferences.PREF_DICOM_STORAGE_DIR;
        String defaultKey = key;
        StringTokenizer tok;

        if (mipavProps == null) {
            read();
        }

        while (getProperty(key) != null) {
            tok = new StringTokenizer(getProperty(key), ";");

            if (tok.countTokens() == 5) {
                defaultKey = key;
            }

            key = key.substring(0, 7) + (Integer.valueOf(key.substring(7)).intValue() + 1);
        }

        return defaultKey;
    }

    /**
     * Return the default settings for a given dialog.
     * 
     * @param dialogName String name of dialog
     * 
     * @return String space separated default parameters for dialog
     */
    public static final String getDialogDefaults(String dialogName) {

        if (is(Preferences.PREF_SAVE_DEFAULTS)) {
            return getProperty(dialogName);
        }

        return null;
    }

    /**
     * Gets the user-configured columns that indicate the DICOM tags that are displayed in the DICOM browser table.
     * 
     * @return Vector List of strings that identify the columns.
     */
    public static Vector getDICOMBrowserTableConfiguration() {

        if (mipavProps == null) {
            read();
        }

        String configStr = getProperty(PREF_DICOM_BROWSER_CONFIG);

        if (configStr == null) {
            return null;
        }

        StringTokenizer tokenizer = new StringTokenizer(configStr, "|");

        Vector configVector = new Vector();

        while (tokenizer.hasMoreElements()) {
            configVector.addElement(tokenizer.nextElement());
        }

        return configVector;
    }

    /**
     * Accessor to get the default DICOM Save Dictionary (which tags to save into XML header.
     * 
     * @return String The full path of the dictionary in which a subset of the DICOM tags are stored.
     */
    public static final String getDICOMSaveDictionary() {
        String key = getProperty(Preferences.PREF_DICOM_SAVE_DICTIONARY);
        String defaultKey = System.getProperties().getProperty("user.home") + File.separator + "mipav" + File.separator
                + "dicomsave.dictionary";

        if (key != null) {
            File file = new File(key);

            if (file.isFile() && file.canWrite() && !file.isDirectory()) {
                return key;
            }
        }

        return defaultKey;

    }

    /**
     * Returns the index of the latest used file filter.
     * 
     * @return the index of the latest used file filter.
     */
    public static int getFileFilter() {

        if (mipavProps == null) {
            read();
        }

        return Integer.parseInt(getProperty(Preferences.PREF_FILENAME_FILTER));
    }
    
    /**
     * Returns the temporary directory which is used to store files to be deleted.
     * 
     * @return the temporary directory which is used to store files to be deleted.
     */
    public static String getFileTempDir() {

        if (mipavProps == null) {
            read();
        }

        return getProperty(PREF_FILE_TEMP_DIR);

    }

    /**
     * Accessor to get host (chat/file) server Internet Protocol for selected Name.
     * 
     * @param AETitle application entity title
     * 
     * @return IP address of asssociated Name
     */
    public static String getHostIP(String AETitle) {
        String key = "Host1";
        StringTokenizer tok = null;
        String str = null;

        if (mipavProps == null) {
            read();
        }

        while (getProperty(key) != null) {
            tok = new StringTokenizer(getProperty(key), ";");
            str = tok.nextToken();

            if (AETitle.trim().equals(str.trim())) {
                tok.nextToken(); // Skip alias

                return (tok.nextToken().trim()); // return IP address from preference file
            }

            key = key.substring(0, 4) + (Integer.valueOf(key.substring(4)).intValue() + 1);
        }

        return null; // no match
    }

    /**
     * Returns the name of the application icon.
     * 
     * @return string containing the name of the system icon
     */
    public static final String getIconName() {
        String str = getProperty("ApplicationIcon");

        if (str != null) {
            return str;
        } else {

            // use default here
            return new String("divinci.gif");
        }
    }

    /**
     * Returns the image directory used last time.
     * 
     * @return the image directory used last time.
     */
    public static String getImageDirectory() {

        String str = Preferences.getProperty(Preferences.PREF_IMAGE_DIR);

        if (str != null) {
            return str;
        } else {
            return (System.getProperties().getProperty("user.dir"));
        }
    }

    /**
     * Accessor to get DICOM image server Internet Protocol for selected AETitle.
     * 
     * @param AETitle application entity title
     * 
     * @return IP address of asssociated AETitle
     */
    public static String getIP(String AETitle) {
        String key = "Server1";
        StringTokenizer tok = null;
        String str = null;

        if (mipavProps == null) {
            read();
        }

        while (getProperty(key) != null) {
            tok = new StringTokenizer(getProperty(key), ";");
            str = tok.nextToken();

            if (AETitle.trim().equals(str.trim())) {
                tok.nextToken(); // Skip alias

                return (tok.nextToken().trim()); // return IP address from preference file
            }

            key = key.substring(0, 6) + (Integer.valueOf(key.substring(6)).intValue() + 1);
        }

        return null; // no match
    }

    /**
     * Returns the image name of "last X images" at the given index. Index should be between 0 and 3.
     * 
     * @param index Index of image.
     * 
     * @return Name of image at index.
     */
    public static final String getLastImageAt(int index) {
        String str = getProperty(Preferences.PREF_LAST_X_IMAGES);
        String[] images = null;

        if (str != null) {

            try {
                StringTokenizer tok = new StringTokenizer(str, ";");

                if ( (tok != null) && (tok.countTokens() > 0)) {
                    images = new String[tok.countTokens()];

                    if (images != null) {

                        for (int i = 0; i < images.length; i++) {
                            images[i] = tok.nextToken();
                        }
                    }
                }
            } catch (Exception ex) {
                return null;
            }

            if ( (images != null) && (images.length > index)) {

                return images[index];
            }
        }

        return null;
    }

    /**
     * Used by ViewOpenImageSequence to get the saved number of channels used when opening an image sequence.
     * 
     * @return String - the value that was saved to the preferences file, represented by a String, of the number of
     *         channels that were used in the corresponding text box of this frame the last time the value was saved
     */
    public static final String getLastOpenSequenceChannels() {

        if (mipavProps == null) {
            read();
        }

        return getProperty("OpenSequenceChannels");
    }

    /**
     * Used by ViewOpenImageSequence to get the Z-C-T ordering of data used when opening an image sequence.
     * 
     * @return String - the value that was saved to the preferences file, represented by a String, of the Z-C-T ordering
     *         of data that was used in the corresponding text box of this frame the last time the value was saved
     */
    public static final String getLastOpenSequenceOrdering() {

        if (mipavProps == null) {
            read();
        }

        return getProperty("OpenSequenceOrdering");
    }

    /**
     * Used by ViewOpenImageSequence to get the last path used when opening an image sequence.
     * 
     * @return String
     */
    public static final String getLastOpenSequencePath() {

        if (mipavProps == null) {
            read();
        }

        return getProperty("OpenSequencePath");
    }

    /**
     * Used by ViewOpenImageSequence to get the saved number of slices used when opening an image sequence.
     * 
     * @return String - the value that was saved to the preferences file, represented by a String, of the number of
     *         slices that were used in the corresponding text box of this frame the last time the value was saved
     */
    public static final String getLastOpenSequenceSlices() {

        if (mipavProps == null) {
            read();
        }

        return getProperty("OpenSequenceSlices");
    }

    /**
     * Used by ViewOpenImageSequence to get the saved number of time points used when opening an image sequence.
     * 
     * @return String - the value that was saved to the preferences file, represented by a String, of the number of time
     *         points that were used in the corresponding text box of this frame the last time the value was saved
     */
    public static final String getLastOpenSequenceTimePoints() {

        if (mipavProps == null) {
            read();
        }

        return getProperty("OpenSequenceTimePoints");
    }

    /**
     * DOCUMENT ME!
     * 
     * @deprecated Returns the project of "last four projects" at the given index. Index should be between 0 and 3.
     * 
     * @param index Index of project.
     * 
     * @return Name of project at index.
     */
    public static final String getLastProjectAt(int index) {
        String str = getProperty(Preferences.PREF_LAST_X_PROJECTS);
        StringTokenizer tok;
        String[] projects = null;

        if (str != null) {

            try {
                tok = new StringTokenizer(str, ";");

                if ( (tok != null) && (tok.countTokens() > 0)) {
                    projects = new String[tok.countTokens()];

                    if (projects != null) {

                        for (int i = 0; i < projects.length; i++) {
                            projects[i] = tok.nextToken();
                        }
                    } else {
                        return null;
                    }
                } else {
                    return null;
                }
            } catch (Exception ex) {
                return null;
            }
        }

        if (projects != null) {
            return projects[index];
        } else {
            return null;
        }
    }

    /**
     * Gets the names of the last X number of images that were loaded from the Preferences file. Shortens the names so
     * the menu isn't too wide.
     * 
     * @return Array of strings representing the last images that were loaded
     */
    public static final String[] getLastXImages() {
        String str = getProperty(Preferences.PREF_LAST_X_IMAGES);
        String quickStr = getProperty(Preferences.PREF_QUICKLIST_NUMBER);
        StringTokenizer tok;
        String[] images = null;
        String testStr = null;
        String shortName = null;
        Vector tempVec = new Vector();

        boolean someRemoved = false;
        int numList = 0;
        int maxList;

        try {
            maxList = Integer.parseInt(quickStr);
        } catch (Exception e) {
            maxList = 4;
        }

        if (str != null) {

            try {
                tok = new StringTokenizer(str, ";");

                if ( (tok != null) && (tok.countTokens() > 0)) {

                    int numTokens = tok.countTokens();

                    for (int i = 0; i < numTokens; i++) {
                        testStr = tok.nextToken();
                        shortName = testStr.substring(0, testStr.indexOf(","));

                        if (new File(shortName).exists() && (numList < maxList)) {
                            numList++;
                            tempVec.add(new String(testStr));
                        } else {
                            someRemoved = true;
                        }
                    }
                } else {
                    return null;
                }

                // if some images should not be there (dont exist or more than the max allowed... rebuild the string
                if (someRemoved || (tempVec.size() < maxList)) {

                    if (tempVec.size() < 1) {
                        setProperty(Preferences.PREF_LAST_X_IMAGES, "");

                        return new String[0];
                    }

                    String newStr = "";

                    for (int i = 0; i < tempVec.size(); i++) {
                        newStr += (String) tempVec.elementAt(i) + ";";
                    }

                    // chop off the last ";"
                    newStr = newStr.substring(0, newStr.length() - 1);
                    setProperty(Preferences.PREF_LAST_X_IMAGES, newStr);
                }

                images = new String[tempVec.size()];

                for (int i = 0; i < images.length; i++) {
                    images[i] = (String) tempVec.elementAt(i);
                }

                tempVec.removeAllElements();
                tempVec = null;

                int j = 0;

                String descriptors = null;

                for (int i = 0; i < images.length; i++) {

                    if (images[i].length() > 25) {

                        descriptors = images[i].substring(images[i].indexOf(","));
                        images[i] = images[i].substring(0, images[i].indexOf(","));

                        // separate fileName and directory
                        int index = images[i].lastIndexOf(File.separatorChar);
                        int index2 = images[i].indexOf(File.separatorChar);
                        String temp = images[i].substring(0, index2 + 1) + "..." + images[i].substring(index);
                        String temp2 = images[i].substring(0, index);

                        String name = temp;

                        while (temp.length() < 25) {
                            name = temp;
                            index = temp2.lastIndexOf(File.separatorChar);
                            temp = images[i].substring(0, index2 + 1) + "..." + images[i].substring(index);
                            temp2 = images[i].substring(0, index);
                            j++;

                            if (j > 10) {
                                break;
                            }
                        }

                        images[i] = name + descriptors;
                    }
                }
            } catch (Exception ex) {}
        }

        return images;
    }

    /**
     * DOCUMENT ME!
     * 
     * @deprecated Gets the last four projects that were loaded from the Preferences file. Shortens the names so the
     *             menu isn't too wide.
     * 
     * @return Array of strings representing the last projects that were loaded, max of four.
     */
    public static final String[] getLastXProjects() {
        String str = getProperty(Preferences.PREF_LAST_X_PROJECTS);
        StringTokenizer tok;
        String[] projects = null;
        String testStr = null;
        Vector tempVec = new Vector();
        int numList = 0;
        boolean someRemoved = false;
        int maxList;

        try {
            maxList = Integer.parseInt(getProperty(Preferences.PREF_QUICKLIST_NUMBER));
        } catch (Exception e) {
            maxList = 4;
        }

        if (str != null) {

            try {
                tok = new StringTokenizer(str, ";");

                if ( (tok != null) && (tok.countTokens() > 0)) {

                    int numTokens = tok.countTokens();

                    for (int i = 0; i < numTokens; i++) {
                        testStr = tok.nextToken();

                        if (new File(testStr).exists() && (numList < maxList)) {
                            numList++;
                            tempVec.add(new String(testStr));
                        } else {
                            someRemoved = true;
                        }
                    }
                } else {
                    return null;
                }

                // if some images should not be there (dont exist or more than the max allowed... rebuild the string
                if (someRemoved || (tempVec.size() < maxList)) {

                    if (tempVec.size() < 1) {
                        setProperty(Preferences.PREF_LAST_X_PROJECTS, "");

                        return new String[0];
                    }

                    String newStr = "";

                    for (int i = 0; i < tempVec.size(); i++) {
                        newStr += (String) tempVec.elementAt(i) + ";";
                    }

                    // chop off the last ";"
                    newStr = newStr.substring(0, newStr.length() - 1);
                    setProperty(Preferences.PREF_LAST_X_PROJECTS, newStr);
                }

                projects = new String[tempVec.size()];

                for (int i = 0; i < projects.length; i++) {
                    projects[i] = (String) tempVec.elementAt(i);
                }

                tempVec.removeAllElements();
                tempVec = null;

                int j = 0;

                for (int i = 0; i < projects.length; i++) {

                    if (projects[i].length() > 25) {

                        // separate fileName and directory
                        int index = projects[i].lastIndexOf(File.separatorChar);
                        int index2 = projects[i].indexOf(File.separatorChar);
                        String temp = projects[i].substring(0, index2 + 1) + "..." + projects[i].substring(index);
                        String temp2 = projects[i].substring(0, index);
                        String name = temp;

                        while (temp.length() < 25) {
                            name = temp;
                            index = temp2.lastIndexOf(File.separatorChar);
                            temp = projects[i].substring(0, index2 + 1) + "..." + projects[i].substring(index);
                            temp2 = projects[i].substring(0, index);
                            j++;

                            if (j > 10) {
                                break;
                            }
                        }

                        projects[i] = name;
                    }
                }
            } catch (Exception ex) {
                return projects;
            }
        }

        return projects;
    }

    /**
     * Returns the next host key.
     * 
     * @return The next host key.
     */
    public static String getNextHostKey() {
        String key = "Host1";

        if (mipavProps == null) {
            read();
        }

        while (getProperty(key) != null) {
            key = key.substring(0, 4) + (Integer.valueOf(key.substring(4)).intValue() + 1);
        }

        return key;
    }

    /**
     * Returns the next server key.
     * 
     * @return The next server key.
     */
    public static String getNextServerKey() {
        String key = "Server1";

        if (mipavProps == null) {
            read();
        }

        while (getProperty(key) != null) {
            key = key.substring(0, 6) + (Integer.valueOf(key.substring(6)).intValue() + 1);
        }

        return key;
    }

    /**
     * Returns the next storage key.
     * 
     * @return The next storage key.
     */
    public static String getNextStorageKey() {
        String key = Preferences.PREF_DICOM_STORAGE_DIR;

        if (mipavProps == null) {
            read();
        }

        while (getProperty(key) != null) {
            key = key.substring(0, 7) + (Integer.valueOf(key.substring(7)).intValue() + 1);
        }

        return key;
    }

    /**
     * Gets an int representing the operating system.
     * 
     * @return int operating system
     */
    public static final int getOS() {

        String os = System.getProperty("os.name");

        if (os.startsWith("Windows")) {
            return OS_WINDOWS;
        } else if (os.startsWith("Mac")) {
            return OS_MAC;
        } else {
            return OS_UNIX;
        }
    }

    /**
     * Gets an int representing the system architecture.
     * 
     * @return the architecture
     */
    public static final int getArch() {
        String arch = System.getProperty("os.arch");

        if (arch.equalsIgnoreCase("i386") || arch.equalsIgnoreCase("i486") || arch.equalsIgnoreCase("i586")
                || arch.equalsIgnoreCase("i686") || arch.equalsIgnoreCase("x86")) {
            return ARCH_X86;
        } else if (arch.equalsIgnoreCase("amd64") || arch.equalsIgnoreCase("x86_64")) {
            return ARCH_AMD64;
        } else if (arch.equalsIgnoreCase("ppc") || arch.equalsIgnoreCase("PowerPC")) {
            return ARCH_PPC;
        } else if (arch.equalsIgnoreCase("pcc64")) {
            return ARCH_PPC64;
        } else if (arch.equalsIgnoreCase("mips") || arch.equalsIgnoreCase("MIPS4000")) {
            return ARCH_MIPS;
        } else if (arch.equalsIgnoreCase("alpha")) {
            return ARCH_ALPHA;
        } else if (arch.equalsIgnoreCase("ia64") || arch.equalsIgnoreCase("IA64N")) {
            return ARCH_IA64;
        } else if (arch.equalsIgnoreCase("arm") || arch.equalsIgnoreCase("armv41")) {
            return ARCH_ARM;
        } else if (arch.equalsIgnoreCase("sparc")) {
            return ARCH_SPARC;
        } else if (arch.equalsIgnoreCase("PA-RISC") || arch.equalsIgnoreCase("PA_RISC")
                || arch.equalsIgnoreCase("PA_RISC2.0")) {
            return ARCH_PA_RISC;
        } else {
            return ARCH_UNKNOWN;
        }
    }

    /**
     * Retrieves the list of overlay names (user can type whatever).
     * 
     * @param isDicom boolean is this a dicom image
     * 
     * @return String[] array of strings for overlay labeling
     */
    public static final String[] getOverlayNames(boolean isDicom) {
        String[] overlays = new String[16];

        String longStr = null;

        if (isDicom) {
            longStr = getProperty("DICOMOverlayNames");
        } else {
            longStr = getProperty("ImageOverlayNames");
        }

        if (longStr == null) {
            return overlays;
        }

        StringTokenizer tok = new StringTokenizer(longStr, ";");
        String token = null;
        int counter = 0;

        while (tok.hasMoreTokens() && (counter < 16)) {
            token = tok.nextToken();

            if (token.equals("-")) {
                overlays[counter] = new String("");
            } else {
                overlays[counter] = new String(token);
            }

            counter++;
        }

        return overlays;
    }

    /**
     * Gets the image overlay codes (they identify what to display in the corners of the image), from the preference
     * file.
     * 
     * @param isDicom if true loads the overlay IDs for a DICOM image
     * 
     * @return the overlay codes that identifies what should be displayed.
     */
    public static final String[] getOverlays(boolean isDicom) {
        String[] overlays = new String[16];

        String longStr = null;

        if (isDicom) {
            longStr = getProperty("DICOMOverlays");
        } else {
            longStr = getProperty("ImageOverlays");
        }

        if (longStr == null) {
            return overlays;
        }

        StringTokenizer tok = new StringTokenizer(longStr, ";");
        String token = null;
        int counter = 0;

        while (tok.hasMoreTokens() && (counter < 16)) {
            token = tok.nextToken();
            overlays[counter] = new String(token);
            counter++;
        }

        return overlays;
    }

    /**
     * Returns String indicating the directory where plugin class files are stored. If this property has not been set,
     * then the image Directory is returned.
     * 
     * @return directory name for the property PluginUserDir or the directory name for the property ImageDirectory if
     *         PluginUserDir is not set.
     */
    public static final String getPluginUserDir() {
        String str = getProperty("PluginUserDir");

        if (str == null) {
            return getProperty(Preferences.PREF_IMAGE_DIR);
        }

        return str;
    }

    /**
     * Accessor to get DICOM image server's port for selected AETitle.
     * 
     * @param AETitle application entity title
     * 
     * @return port address of asssociated AETitle
     */
    public static int getPort(String AETitle) {
        String key = "Server1";
        StringTokenizer tok = null;

        if (mipavProps == null) {
            read();
        }

        while (getProperty(key) != null) {
            tok = new StringTokenizer(getProperty(key), ";");

            if (AETitle.trim().equals(tok.nextToken().trim())) {
                tok.nextToken(); // Skip alias
                tok.nextToken(); // Skip IP

                return (Integer.valueOf(tok.nextToken().trim()).intValue()); // return port address from preference
                // file
            }

            key = key.substring(0, 6) + (Integer.valueOf(key.substring(6)).intValue() + 1);
        }

        return -1; // no match
    }

    /**
     * Return the directory where mipav-generated, user-specific files should be placed.
     * 
     * @return the place to put user-specific files
     */
    public static final String getPreferencesDir() {
        return preferencesDir;
    }

    /**
     * Accessor to return a specfic properties given a key.
     * 
     * @param key the property key.
     * 
     * @return the mipav property. The method returns null if the property is not found.
     */
    public static final String getProperty(String key) {

        if (mipavProps == null) {
            read();
        }

        return mipavProps.getProperty(key);
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public static final String getScriptFile() {
        String s = getProperty("ScriptFile");

        if (s == null) {
            s = "";
        }

        return s;
    }

    /**
     * Returns the default script directory. if null, returns the user's current working directory + "scripts"
     * 
     * @return script dir
     */
    public static final String getScriptsDirectory() {
        String str = getProperty(Preferences.PREF_SCRIPT_DIR);

        if (str == null) {

            // set the default
            str = System.getProperty("user.dir") + File.separator + "scripts";
            setProperty(Preferences.PREF_SCRIPT_DIR, str);
        }

        return str;
    }

    /**
     * Retrieves the SRB server authentication scheme.
     * 
     * @return the srb server's authentication scheme.
     */
    public static String getServerAuthSRB() {

        if (mipavProps == null) {
            read();
        }

        return getProperty(PREF_SERVER_AUTHENTICATION_SRB);
    }

    /**
     * Retrieves the domain information of the SRB server.
     * 
     * @return DOCUMENT ME!
     */
    public static String getServerDomainSRB() {

        if (mipavProps == null) {
            read();
        }

        return getProperty(PREF_SERVER_DOMAIN_SRB);
    }

    /**
     * Retrieves the ip address or DNS name of the machine which hosts the SRB server.
     * 
     * @return DOCUMENT ME!
     */
    public static String getServerHostSRB() {

        if (mipavProps == null) {
            read();
        }

        return getProperty(PREF_SERVER_HOST_SRB);
    }

    /**
     * Retrieves the SRB server port.
     * 
     * @return DOCUMENT ME!
     */
    public static int getServerPortSRB() {

        if (mipavProps == null) {
            read();
        }

        int port = -1;

        try {
            String strPort = getProperty(PREF_SERVER_PORT_SRB);

            if (strPort != null) {
                port = Integer.parseInt(strPort);
            }
        } catch (NumberFormatException e) {
            debug("Cannot retrieve previous SRB server port.", DEBUG_MINOR);
        }

        return port;
    }

    /**
     * Returns a keystroke shortcut for the given command.
     * 
     * @param command String the command to check
     * 
     * @return KeyStroke the shortcut's keystroke
     */
    public static KeyStroke getShortcut(String command) {
        KeyStroke ks = null;

        if (command != null) {
            ks = (KeyStroke) defaultShortcutTable.get(command);

            if (ks == null) {
                ks = (KeyStroke) userShortcutTable.get(command);
            }
        }

        return ks;
    }

    /**
     * Gets the command associated with the shortcut (if there is one).
     * 
     * @param ks KeyStroke the keystroke to check
     * 
     * @return String the command associated (or null if none)
     */
    public static String getShortcutCommand(KeyStroke ks) {
        KeyStroke shortcut = null;
        String command = null;

        // first check defaults
        Enumeration en = defaultShortcutTable.keys();

        while (en.hasMoreElements()) {
            command = (String) en.nextElement();
            shortcut = (KeyStroke) defaultShortcutTable.get(command);

            if (shortcut.equals(ks)) {
                return command;
            }
        }

        // next check user shortcuts
        en = userShortcutTable.keys();

        while (en.hasMoreElements()) {
            command = (String) en.nextElement();
            shortcut = (KeyStroke) userShortcutTable.get(command);

            if (shortcut.equals(ks)) {
                return command;
            }
        }

        return null;
    }

    /**
     * Returns the temporary directory which is used during file transfer.
     * 
     * @return the temporary directory which is used during file transfer.
     */
    public static String getSRBTempDir() {

        if (mipavProps == null) {
            read();
        }

        return getProperty(PREF_SRB_TEMP_DIR);

    }

    /**
     * Returns the transfer mode of srb server.
     * 
     * @return the transfer mode of srb server.
     */
    public static String getSRBTransferMode() {

        if (mipavProps == null) {
            read();
        }

        return getProperty(PREF_SRB_TRANSFER_MODE);
    }

    /**
     * Returns the srb server version which Jargon need to be adapted to.
     * 
     * @return the srb server version which Jargon need to be adapted to.
     */
    public static String getSRBVersion() {

        if (mipavProps == null) {
            read();
        }

        return getProperty(PREF_SRB_VERSION);

    }

    /**
     * Retrieves the default storage resource for the user.
     * 
     * @return DOCUMENT ME!
     */
    public static String getStorageResourceSRB() {

        if (mipavProps == null) {
            read();
        }

        return getProperty(PREF_STORAGE_RESOURCE_SRB);
    }

    /**
     * Used by ViewOpenImageSequence to get the subsample dimensions that were saved in the preferences file.
     * 
     * @return Dimension - the dimension that was last used to subsample an image while using the ViewOpenImageSequence
     *         tool, null if dimension is invalid in the preferences file
     */
    public static final Dimension getSubsampleDimensions() {

        if (mipavProps == null) {
            read();
        }

        Dimension subsampleDimension = null;

        try {
            String subsampleWidth = getProperty("subsampleWidth");
            String subsampleHeight = getProperty("subsampleHeight");

            subsampleDimension = new Dimension(Integer.parseInt(subsampleWidth), Integer.parseInt(subsampleHeight));
        } catch (NumberFormatException nfe) {
            debug("Cannot retrieve previous subsample dimension.", DEBUG_MINOR);
            subsampleDimension = null;
        }

        return subsampleDimension;
    }

    /**
     * Accessor to get the TRIM parameter (trimming VOIcontour of points). Default value is 0.3 which trims some of the
     * points of the contours.
     * 
     * @return the trim value.
     */
    public static final float getTrim() {

        if (mipavProps == null) {
            read();
        }

        String str = getProperty(Preferences.PREF_TRIM);

        if (str != null) {
            return (Float.valueOf(str).floatValue());
        }

        return 0.3f; // no match
    }

    /**
     * Accessor to get the TRIM parameter (trimming contour of points).
     * 
     * @return the trim boolean to indicate if the directly adjacient points should be trimmed.
     */
    public static boolean getTrimAdjacient() {

        if (mipavProps == null) {
            read();
        }

        String str = getProperty(Preferences.PREF_TRIM_FLAG);

        if (str != null) {
            return (Boolean.valueOf(str).booleanValue());
        }

        return true; // no match
    }

    /**
     * Retrieves the user name to login the SRB server.
     * 
     * @return DOCUMENT ME!
     */
    public static String getUserNameSRB() {

        if (mipavProps == null) {
            read();
        }

        return getProperty(PREF_USERNAME_SRB);
    }

    /**
     * Gets the User defined Hashtable used to store the shortcut keystrokes and commands.
     * 
     * @return Hashtable the shortcut hashtable
     */
    public static Hashtable getUserShortcutTable() {
        return userShortcutTable;
    }

    /**
     * Gets the VOI color increment value. Ben add here.
     * 
     * @return int
     */
    public static int getVOIColorIncrement() {
        int increment = 0;

        String str = getProperty(Preferences.PREF_VOI_START_COLOR);

        if (str != null) {

            try {
                increment = Integer.parseInt(str);
            } catch (Exception ex) {
                // do nothing.. colorIncrement still is 0
            }
        }

        return increment;
    }

    /**
     * Gets the VOI draw color.
     * 
     * @return Color color to draw with
     */
    public static Color getVOIDrawColor() {

        String str = getProperty(Preferences.PREF_VOI_DRAW_COLOR);
        Color drawColor = Color.yellow;

        if (str != null) {

            try {
                drawColor = MipavUtil.extractColor(str);
            } catch (Exception ex) {
                // do nothing.. drawColor still is yellow
            }
        }

        return drawColor;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public static int getVOIThickness() {
        int thickness = 1;
        String str = getProperty(Preferences.PREF_VOI_THICKNESS);

        if (str != null) {

            try {
                thickness = Integer.parseInt(str);
            } catch (Exception ex) {
                // do nothing.. thickness still 1
            }
        } else {
            setProperty(Preferences.PREF_VOI_THICKNESS, "1");
        }

        return thickness;
    }

    /**
     * Gets the boolean state of the property, where in the preferences file the &quot;yes&quot;, &quot;on&quot;,
     * &quot;true&quot; all return <code>true</code>, but anything else (or non-existent property) returns <code>
     * false</code>.
     * This method is provided for semantic preference, but is exactly the same as (in fact, calls) <code>
     * isPreference</code>.
     * 
     * @see #isPreference(String)
     * 
     * @return boolean <code>true</code> when the named property has the String the &quot;yes&quot;, &quot;on&quot;,
     *         &quot;true&quot;. <code>false</code> for any other string, or if the property does not exist.
     * 
     * @param propertyName Name of a property in preferences to check.
     */
    public static final boolean is(String propertyName) {
        return isPreference(propertyName);
    }

    /**
     * Returns flag indicating if the debug flag is true or false. (and in preferences v2, if debug level is zero,
     * returns false. Returns true if debug level is greater than 0.)
     * 
     * <p>
     * debug level is the amount of output given for debugging mipav.
     * </p>
     * 
     * @return returns true if either debug flag is true or level is greater than zero and false if either debug flag is
     *         false or level is zero.
     */
    public static final boolean isDebug() {
        String str = getProperty("DEBUG");

        if (str == null) {

            // do nothing
            System.err.println("Preferences debug: string is null. No property DEBUG");
        } else {

            try {

                if (Integer.parseInt(str) > 0) { // debug level > 0
                    return true;
                }
            } catch (NumberFormatException nfe) { // older version debug

                if (str.equalsIgnoreCase("true") || str.equalsIgnoreCase("on") || str.equalsIgnoreCase("yes")) { // debug
                    // YES
                    return true;
                }
            } catch (NullPointerException npe) { // property does not exist
                return false; // no property present? default to not debug mode
            }
        }

        return false; // any other case: not in debug mode
    }

    /**
     * DOCUMENT ME!
     * 
     * @param command DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public static boolean isDefaultCommand(String command) {
        return defaultShortcutTable.contains(command);
    }

    /**
     * DOCUMENT ME!
     * 
     * @param ks DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public static boolean isDefaultKeyStroke(KeyStroke ks) {

        if (ks == null) {
            return false;
        }

        Enumeration keys = defaultShortcutTable.keys();

        KeyStroke defaultKey = null;

        while (keys.hasMoreElements()) {
            defaultKey = (KeyStroke) defaultShortcutTable.get(keys.nextElement());

            if (defaultKey.equals(ks)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Gets the boolean state of the property, where in the preferences file the &quot;yes&quot;, &quot;on&quot;,
     * &quot;true&quot; all return <code>true</code>, but anything else (or non-existent property) returns <code>
     * false</code>.
     * While not as intuitive, perhaps, as some of the specially named property-retrieval methods (eg.,
     * <code>isSplashScreen</code>), this method can be used to replace all of those methods when the preferences
     * property is known. It can also be convenient for limited-use properties used in debugging.
     * 
     * @return boolean <code>true</code> when the named property has the String the &quot;yes&quot;, &quot;on&quot;,
     *         &quot;true&quot;. <code>false</code> for any other string, or if the property does not exist.
     * 
     * @param propertyName Name of a property in preferences to check.
     */
    public static final boolean isPreference(String propertyName) {

        if (propertyName == null) {
            debug("Property name: null.", Preferences.DEBUG_MINOR);

            return false;
        }

        String str = getProperty(propertyName);

        if (str != null) {

            if (str.equalsIgnoreCase("yes") || str.equalsIgnoreCase("true") || str.equalsIgnoreCase("on")) {
                return true;
            }

            return false;
        } else {
            return false;
        }
    }

    /**
     * Returns whether the user has a preference indicated for a particular property. Note that this just checks for
     * some value, not necessarily a &quot;true&quot; value.
     * 
     * @param propertyName the name of the property to check for
     * 
     * @return <code>true</code> if there is some preference set for the given property name
     */
    public static final boolean isPreferenceSet(String propertyName) {

        if (propertyName == null) {
            debug("Property name: null.", Preferences.DEBUG_MINOR);

            return false;
        }

        return getProperty(propertyName) != null;
    }

    /**
     * Prints mipav.preferences to System.out.
     */
    public static void print() {
        mipavProps.list(System.out);
    }

    /**
     * Reads the "mipav.preferences" file from the user's 'home' directory.
     * 
     * @return true if found else false
     */
    public static boolean read() {
        mipavProps = null;

        BufferedInputStream bFile = null;

        // initialze the properties to the default properties
        mipavProps = new Properties(defaultProps);

        if ( !new File(preferencesFile).exists()) {

            // nothing user-specific to override the defaults, so just save what there is.
            save();
        }

        // / TODO: maybe find pref files in other locations and delete them..

        try {
            // this will override any defaults if the user specified something different

            bFile = new BufferedInputStream(new FileInputStream(preferencesFile));
            mipavProps.load(bFile);
        } catch (IOException error) {
            MipavUtil.displayError("Error Reading preferences");

            return false;
        } finally {

            try {

                if (bFile != null) {
                    bFile.close();
                }
            } catch (IOException ioe) {}
        }

        buildShortcutTable();

        return true;
    }

    /**
     * Removes the property key from the MIPAV property list.
     * 
     * @param key the property key.
     */
    public static final void removeProperty(String key) {

        if (mipavProps == null) {
            read();
        }

        mipavProps.remove(key);
    }

    /**
     * Removes a shortcut from the table.
     * 
     * @param commandToRemove String
     */
    public static void removeShortcut(String commandToRemove) {
        userShortcutTable.remove(commandToRemove);
        saveShortcuts();
    }

    /**
     * Saves preference file to the user's 'home' directory.
     * 
     * @return boolean true if successful, false otherwise
     */
    public static boolean save() {
        BufferedOutputStream bFile = null;

        try {
            File dFile = new File(preferencesDir);

            if ( !dFile.exists() && !dFile.mkdirs()) {
                MipavUtil.displayError("Unable to create preferences directory: " + preferencesDir);

                return false;
            }

            bFile = new BufferedOutputStream(new FileOutputStream(preferencesFile));
            mipavProps.store(bFile, " MIPAV preference file");
        } catch (IOException error) {
            MipavUtil.displayError("Unable to save preference file " + error.toString());

            return false;
        } finally {

            try {

                if (bFile != null) {
                    bFile.close();
                }
            } catch (IOException ioe) {}
        }

        return true;
    }

    /**
     * Saves the last used parameters for the given dialog.
     * 
     * @param dialogName String the name of the dialog
     * @param defaultsString String the String to save for this dialog
     */
    public static final void saveDialogDefaults(String dialogName, String defaultsString) {
        setProperty(dialogName, defaultsString);
    }

    /**
     * Sets the property for shortcuts by building string from ViewUserInterface's Shortcut Hashtable.
     */
    public static final void saveShortcuts() {

        // Hashtable userShortcutTable = ViewUserInterface.getReference().getShortcutTable();
        String command = null;
        KeyStroke ks = null;
        Enumeration e = userShortcutTable.keys();

        String longStr = new String();

        String singleChar = null;

        int m = 0;

        try {

            while (e.hasMoreElements()) {
                command = (String) e.nextElement();
                ks = (KeyStroke) userShortcutTable.get(command);
                m = ks.getModifiers();

                singleChar = ks.toString().trim();

                if (singleChar.charAt(singleChar.length() - 2) != 'F') {
                    singleChar = singleChar.substring(singleChar.length() - 1, singleChar.length());
                    longStr += command + ",";

                    if ( (m & (InputEvent.SHIFT_DOWN_MASK | InputEvent.SHIFT_MASK)) != 0) {
                        longStr += "SHIFT";
                    }

                    if ( (m & (InputEvent.CTRL_DOWN_MASK | InputEvent.CTRL_MASK)) != 0) {
                        longStr += "CTRL";
                    }

                    if ( (m & (InputEvent.ALT_DOWN_MASK | InputEvent.ALT_MASK)) != 0) {
                        longStr += "ALT";
                    }
                } else {
                    longStr += command + ",";
                    singleChar = singleChar.replaceAll("pressed", "");
                }

                longStr += singleChar + ";";

            }

            setProperty(Preferences.PREF_SHORTCUTS, longStr);
        } catch (Exception ex) {
            System.err.println("error setting shortcuts in prefs");
        }
    }

    /**
     * Used by ViewOpenImageSequence to save its subsample dimensions for the next time it is opened. The dimension is
     * actually saved as its two parts, width and height
     * 
     * @param subsampleDimension Dimension - the subsample dimension to save
     */
    public static final void saveSubsampleDimensions(Dimension subsampleDimension) {

        if (subsampleDimension == null) {
            return;
        }

        setProperty("subsampleWidth", String.valueOf(subsampleDimension.width));
        setProperty("subsampleHeight", String.valueOf(subsampleDimension.height));
    }

    /**
     * Sets whether all imgs should be saved in analyze format (i.e., don't ask about whether to save as
     * analyze/interfile/nifti).
     * 
     * @param doSaveAnalyze whether to always save .hdr/.img files as analyze format files
     */
    public static final void setAlwaysSaveImgAsAnalyze(boolean doSaveAnalyze) {
        setProperty(PREF_ALWAYS_SAVE_IMG_AS_ANALYZE, Boolean.toString(doSaveAnalyze));
    }

    /**
     * Sets whether all imgs should be saved in interfile format (i.e., don't ask about whether to save as
     * analyze/interfile/nifti).
     * 
     * @param doSaveInterfile whether to always save .hdr/.img files as interfile format files
     */
    public static final void setAlwaysSaveImgAsInterfile(boolean doSaveInterfile) {
        setProperty(PREF_ALWAYS_SAVE_IMG_AS_INTERFILE, Boolean.toString(doSaveInterfile));
    }

    /**
     * Sets whether all imgs should be saved in nifti format (i.e., don't ask about whether to save as
     * analyze/interfile/nifti).
     * 
     * @param doSaveNifti whether to always save .hdr/.img files as nifti format files
     */
    public static final void setAlwaysSaveImgAsNifti(boolean doSaveNifti) {
        setProperty(PREF_ALWAYS_SAVE_IMG_AS_NIFTI, Boolean.toString(doSaveNifti));
    }

    /**
     * Sets whether all mncs should be saved in minc1 cdf format (i.e., don't ask about whether to save as minc1/minc2).
     * 
     * @param doSaveMinc1 whether to always save .mnc files as minc1 cdf format files
     */
    public static final void setAlwaysSaveMncAsMinc1(boolean doSaveMinc1) {
        setProperty(PREF_ALWAYS_SAVE_MNC_AS_MINC1, Boolean.toString(doSaveMinc1));
    }

    /**
     * Sets whether all mncs should be saved in minc2 hdf5 format (i.e., don't ask about whether to save as
     * minc1/minc2).
     * 
     * @param doSaveMinc2 whether to always save .mnc files as minc2 hdf5 format files
     */
    public static final void setAlwaysSaveMncAsMinc2(boolean doSaveMinc2) {
        setProperty(PREF_ALWAYS_SAVE_MNC_AS_MINC2, Boolean.toString(doSaveMinc2));
    }

    /**
     * Sets the current keystroke shortcut for assignment once a command has been passed in.
     * 
     * @param ks KeyStroke the shortcut keystroke
     */
    public static void setCurrentShortcut(KeyStroke ks) {
        currentShortcut = ks;
    }

    /**
     * Sets the debug levels (lowest is index 0) on/off levels[0] is MINOR levels[1] is ALGORITHM levels[2] is FILEIO
     * levels[3] is COMMS (dicom).
     * 
     * @param levels boolean[] true/false for each level of debug
     */
    public static final void setDebugLevels(boolean[] levels) {
        String str = new String();

        if (levels != null) {

            for (int i = 0; i < levels.length; i++) {
                str += levels[i];

                if (i != (levels.length - 1)) {
                    str += ",";
                }
            }

            setProperty(Preferences.PREF_DEBUG, str);
        }
    }

    /**
     * Sets the user-configured columns that indicate the DICOM tags that are displayed in the DICOM browser table.
     * 
     * @param newConfiguration Vector List of strings that identify the columns.
     * 
     * @return boolean Success or failure of the setting the columns.
     */
    public static boolean setDICOMBrowserTableConfiguration(Vector newConfiguration) {

        if (newConfiguration == null) {
            MipavUtil.displayInfo("Unable to save configuration to preferences file.");

            return false;
        }

        String configStr = "";

        for (int i = 0; i < newConfiguration.size(); i++) {
            configStr += (String) newConfiguration.elementAt(i) + "|";
        }

        return setProperty(PREF_DICOM_BROWSER_CONFIG, configStr);
    }

    /**
     * Sets the current used file filter as the latest used file filter.
     * 
     * @param fileFilterIndex the index of the current used file filter.
     */
    public static void setFileFilter(int fileFilterIndex) {
        setProperty(Preferences.PREF_FILENAME_FILTER, new Integer(fileFilterIndex));
    }
    
    /**
     * Sets the temporary directory which is used for files which will be deleted.
     * 
     * @param tempDir the temporary directory.
     */
    public static void setFileTempDir(String tempDir) {

        if (tempDir == null) {
            return;
        }

        setProperty(PREF_FILE_TEMP_DIR, tempDir);
    }

    /**
     * Sets the image directory as the latest used image directory.
     * 
     * @param imageDirectory the current used image directory.
     */
    public static void setImageDirectory(File imageDirectory) {

        if (imageDirectory == null) {
            return;
        }

        if (imageDirectory.isDirectory()) {
            setProperty(Preferences.PREF_IMAGE_DIR, imageDirectory.getAbsolutePath());
        } else {
            setProperty(Preferences.PREF_IMAGE_DIR, imageDirectory.getParent());
        }
    }

    /**
     * Sets the last image loaded in the Preferences file. If it is a duplicate, moves it up on the list.
     * 
     * @param name Full pathname of last image loaded.
     * @param multiFile true if the last image was a multi-file image, false otherwise
     * @param numDims DOCUMENT ME!
     */
    public static final void setLastImage(String name, boolean multiFile, int numDims) {

        if (name == null) {
            return;
        }

        String newName;
        String newProp;
        String str = getProperty(Preferences.PREF_LAST_X_IMAGES);
        String quickStr = getProperty(Preferences.PREF_QUICKLIST_NUMBER);
        StringTokenizer tok;

        int quickListNumber;

        try {
            quickListNumber = Integer.parseInt(quickStr);
        } catch (Exception e) {
            quickListNumber = 4;
        }

        newName = new String(name);

        newName += "," + numDims;

        if (multiFile) {
            newName += "M";
        }

        newProp = new String(newName);

        String added = null;

        int commaIndex = -1;

        String quickListToken = null;

        int tokenLength;

        if (str != null) {

            try {
                tok = new StringTokenizer(str, ";");

                int i = 1;

                if (tok != null) {

                    while ( (i < quickListNumber) && tok.hasMoreTokens()) {
                        quickListToken = tok.nextToken();

                        commaIndex = quickListToken.indexOf(",");

                        added = quickListToken.substring(0, commaIndex);
                        multiFile = quickListToken.endsWith("M");

                        tokenLength = quickListToken.length();

                        if (multiFile) {
                            numDims = Integer.parseInt(quickListToken.substring(tokenLength - 2, tokenLength - 1));
                        } else {
                            numDims = Integer.parseInt(quickListToken.substring(tokenLength - 1, tokenLength));
                        }

                        if ( !quickListToken.equals(newName) && new File(added).exists()) {
                            newProp += ";" + quickListToken;
                            i++;
                        }
                    }
                }
            } catch (Exception ex) {}
        }

        // System.err.println("New quicklist string is: " + newProp);

        setProperty(Preferences.PREF_LAST_X_IMAGES, newProp);
    }

    /**
     * Used by ViewOpenImageSequence to save the user's most recent slices, channels, time points, and ordering that
     * were specified in the component.
     * 
     * @param slices String - number of slices in the slices text box of the ViewOpenImageSequence frame
     * @param channels String - number of channels in the channels text box of the ViewOpenImageSequence frame
     * @param timePoints String - number of time points in the time points text box of the ViewOpenImageSequence frame
     * @param ordering String - ordering sequence used in the radio button panel of the ViewOpenImageSequence frame
     */
    public static final void setLastOpenSequenceParams(String slices, String channels, String timePoints,
            String ordering) {
        setProperty("OpenSequenceSlices", slices);
        setProperty("OpenSequenceChannels", channels);
        setProperty("OpenSequenceTimePoints", timePoints);
        setProperty("OpenSequenceOrdering", ordering);
    }

    /**
     * Used by ViewOpenImageSequence to save the path used when opening an image sequence.
     * 
     * @param path DOCUMENT ME!
     */
    public static final void setLastOpenSequencePath(String path) {

        if (path == null) {
            return;
        }

        setProperty("OpenSequencePath", path);
    }

    /**
     * Sets the last project loaded in the Preferences file. If it is a duplicate, moves it up on the list.
     * 
     * @param name Full pathname of last project loaded.
     */
    public static final void setLastProject(String name) {

        if (name == null) {
            return;
        }

        String str = getProperty(Preferences.PREF_LAST_X_PROJECTS);
        StringTokenizer tok;

        int quickListNumber;

        try {
            quickListNumber = Integer.parseInt(getProperty(Preferences.PREF_QUICKLIST_NUMBER));
        } catch (Exception e) {
            quickListNumber = 4;
        }

        String newProp = name;

        if (str != null) {

            try {
                tok = new StringTokenizer(str, ";");

                int i = 1;

                if (tok != null) {

                    while ( (i < quickListNumber) && tok.hasMoreTokens()) {
                        String added = tok.nextToken();

                        if ( !added.equals(name) && new File(added).exists()) {
                            newProp += ";" + added;
                            i++;
                        }
                    }
                }
            } catch (Exception ex) {}
        }

        setProperty(Preferences.PREF_LAST_X_PROJECTS, newProp);
    }

    /**
     * Sets the message frame.
     * 
     * @param mFrame Message frame to set.
     */
    public static final void setMessageFrame(ViewJFrameMessage mFrame) {
        messageFrame = mFrame;
    }

    /**
     * Sets the labels for each overlay position. The labels can be user input with any text and will be displayed
     * before the overlay values
     * 
     * @param isDicom boolean is the image DICOM
     * @param overlays String[] array of strings (16) for each of the overlay positions
     */
    public static final void setOverlayNames(boolean isDicom, String[] overlays) {
        String str = "";

        for (int i = 0; i < 16; i++) {

            if ( (overlays[i] == null) || overlays[i].equals("")) {
                str += "-;";
            } else {
                str += overlays[i] + ";";
            }
        }

        if (isDicom) {
            setProperty("DICOMOverlayNames", str);
        } else {
            setProperty("ImageOverlayNames", str);
        }
    }

    /**
     * Sets the overlay tag identifiers in the preference file.
     * 
     * @param isDicom if true identifies the tags as a list of DICOMOverlays else identifies the tags as ImageOverlays
     *            (i.e., it is not a DICOM image)
     * @param overlays an array of 16 strings (4 for each corner) that identifies what is to be displayed in the image.
     */
    public static final void setOverlays(boolean isDicom, String[] overlays) {
        String str = "";

        for (int i = 0; i < 16; i++) {

            if ( (overlays[i] != null) && !overlays[i].equals(JDialogOverlay.BLANK_OVERLAY)) {
                str += overlays[i] + ";";
            } else {
                str += "-;";
            }
        }

        if (isDicom) {
            setProperty("DICOMOverlays", str);
        } else {
            setProperty("ImageOverlays", str);
        }
    }

    /**
     * Change the file name of the preferences file we should use (without any path).
     * 
     * @param fileName the file name (eg mipav.preferences), which should be in $HOME/mipav/
     */
    public static final void setPreferencesFileName(String fileName) {
        preferencesFileName = fileName;
        preferencesFile = preferencesDir + File.separator + preferencesFileName;
    }

    /**
     * Sets the property key in the MIPAV property list and saves it to a file.
     * 
     * @param key the property key.
     * @param value the value of the property key.
     * 
     * @return true if the property was set and saved successfully, false otherwise
     */
    public static final boolean setProperty(String key, Object value) {

        if (mipavProps == null) {
            read();
        }

        mipavProps.put(key, value);

        return save();
    }

    /**
     * DOCUMENT ME!
     * 
     * @param scriptFile DOCUMENT ME!
     */
    public static final void setScriptFile(String scriptFile) {

        if (scriptFile != null) {
            setProperty("ScriptFile", scriptFile);
        }
    }

    /**
     * Sets the scripts directory to the new scripts directory.
     * 
     * @param scriptsDirectory the new scripts directory
     */
    public static final void setScriptsDirectory(String scriptsDirectory) {

        if (scriptsDirectory != null) {
            setProperty(Preferences.PREF_SCRIPT_DIR, scriptsDirectory);
        }
    }

    /**
     * Sets the server authentication schema which the SRB server is using.
     * 
     * @param serverAuthSRB the new SRB server authentication schema
     */
    public static void setServerAuthSRB(String serverAuthSRB) {
        setProperty(PREF_SERVER_AUTHENTICATION_SRB, serverAuthSRB);
    }

    /**
     * Sets the domain information of the SRB server.
     * 
     * @param serverDomainSRB DOCUMENT ME!
     */
    public static void setServerDomainSRB(String serverDomainSRB) {

        if (serverDomainSRB == null) {
            return;
        }

        setProperty(PREF_SERVER_DOMAIN_SRB, serverDomainSRB);
    }

    /**
     * Sets the ip address or DNS name of the machine which hosts the SRB server.
     * 
     * @param serverHostSRB DOCUMENT ME!
     */
    public static void setServerHostSRB(String serverHostSRB) {

        if (serverHostSRB == null) {
            return;
        }

        setProperty(PREF_SERVER_HOST_SRB, serverHostSRB);
    }

    /**
     * Sets the server port where the SRB server is listening.
     * 
     * @param serverPortSRB the new SRB server port
     */
    public static void setServerPortSRB(int serverPortSRB) {
        setProperty(PREF_SERVER_PORT_SRB, Integer.toString(serverPortSRB));
    }

    /**
     * Sets whether to show the DICOM overlays.
     * 
     * @param doShow boolean true means show DICOM overlays
     */
    public static final void setShowDICOMOverlays(boolean doShow) {
        setProperty("ShowDICOMOverlays", Boolean.toString(doShow));
    }

    /**
     * Sets whether to show the Image overlays.
     * 
     * @param doShow boolean true means show image overlays
     */
    public static final void setShowImageOverlays(boolean doShow) {
        setProperty("ShowImageOverlays", Boolean.toString(doShow));
    }

    /**
     * Sets the temporary directory which is used during file transfer to the new temporary directory.
     * 
     * @param tempDir the temporary directory.
     */
    public static void setSRBTempDir(String tempDir) {

        if (tempDir == null) {
            return;
        }

        setProperty(PREF_SRB_TEMP_DIR, tempDir);
    }

    /**
     * Sets the transfer mode of srb server to the new transfer mode.
     * 
     * @param transferMode the new transfer mode.
     */
    public static void setSRBTransferMode(String transferMode) {

        if (transferMode == null) {
            return;
        }

        setProperty(PREF_SRB_TRANSFER_MODE, transferMode);
    }

    /**
     * Sets the srb server version which Jargon need to be adapted to.
     * 
     * @param srbVersion the new srb server version.
     */
    public static void setSRBVersion(String srbVersion) {

        if (srbVersion == null) {
            return;
        }

        setProperty(PREF_SRB_VERSION, srbVersion);
    }

    /**
     * Sets the default storage resource for the user.
     * 
     * @param storageResource the new storage resource.
     */
    public static void setStorageResourceSRB(String storageResource) {
        setProperty(PREF_STORAGE_RESOURCE_SRB, storageResource);
    }

    /**
     * Sets the user name to login the SRB server.
     * 
     * @param userNameSRB the user name of the srb server.
     */
    public static void setUserNameSRB(String userNameSRB) {

        if (userNameSRB == null) {
            return;
        }

        setProperty(PREF_USERNAME_SRB, userNameSRB);
    }

    /**
     * Builds the default shortcut hashtable (not user modifiable).
     * 
     * @return default shortcut hashtable
     */
    private static Hashtable buildDefaultShortcuts() {

        defaultShortcutTable = new Hashtable<String, KeyStroke>();
        defaultShortcutTable.put("OpenNewImage", KeyStroke.getKeyStroke('F', Event.CTRL_MASK, false));
        defaultShortcutTable.put("SaveImage", KeyStroke.getKeyStroke('S', Event.CTRL_MASK, false));
        defaultShortcutTable.put("SaveImageAs", KeyStroke.getKeyStroke('S', Event.SHIFT_MASK + Event.CTRL_MASK, false));
        defaultShortcutTable.put("undoVOI", KeyStroke.getKeyStroke('Z', Event.CTRL_MASK, false));
        defaultShortcutTable.put("cutVOI", KeyStroke.getKeyStroke('X', Event.CTRL_MASK, false));
        defaultShortcutTable.put("copyVOI", KeyStroke.getKeyStroke('C', Event.CTRL_MASK, false));
        defaultShortcutTable.put("pasteVOI", KeyStroke.getKeyStroke('V', Event.CTRL_MASK, false));
        defaultShortcutTable.put("selectAllVOIs", KeyStroke.getKeyStroke('A', Event.CTRL_MASK, false));
        defaultShortcutTable.put("Tri-planar", KeyStroke.getKeyStroke('T', Event.CTRL_MASK, false));
        defaultShortcutTable.put("AboutImage", KeyStroke.getKeyStroke('H', Event.CTRL_MASK, false));
        defaultShortcutTable.put("EditImageInfo", KeyStroke.getKeyStroke('E', Event.CTRL_MASK, false));
        defaultShortcutTable.put("MemoryUsage", KeyStroke.getKeyStroke('M', Event.CTRL_MASK, false));
        defaultShortcutTable.put("ToggleImageIntensities", KeyStroke.getKeyStroke('T', 0, false));
        defaultShortcutTable.put("quickLUT", KeyStroke.getKeyStroke('Q', 0, false));

        for (int i = 0; i < 9; i++) {
            defaultShortcutTable.put("LastImage " + i, KeyStroke.getKeyStroke(Integer.toString(i + 1).charAt(0),
                    Event.CTRL_MASK, false));
        }

        return defaultShortcutTable;
    }

    public static boolean isMultiThreadingEnabled() {
        if (mipavProps == null) {
            read();
        }
        String mtEnabled = mipavProps.getProperty(PREF_MULTI_THREADING_ENABLED);
        if (mtEnabled == null) {
            mtEnabled = defaultProps.getProperty(PREF_MULTI_THREADING_ENABLED);
        }
        if ("true".equals(mtEnabled)) {
            return true;
        }
        return false;
    }

    public static int getNumberOfThreads() {
        if (mipavProps == null) {
            read();
        }
        String numberOfThreads = mipavProps.getProperty(PREF_NUMBER_OF_THREADS);
        if (numberOfThreads == null) {
            numberOfThreads = defaultProps.getProperty(PREF_NUMBER_OF_THREADS);
        }
        return Integer.parseInt(numberOfThreads);
    }
}
