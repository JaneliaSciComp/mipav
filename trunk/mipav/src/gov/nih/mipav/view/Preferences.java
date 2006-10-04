package gov.nih.mipav.view;


import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;


/**
 * This class reads the MIPAV preference file. The preference file stores a number of user specific parameters of the
 * application. It is stored in the < user directory >/mipav directory so that it can be read and written to by the
 * user. It is a text file and can be manually edited - not recommended.
 */
public class Preferences {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Constant that indicates use of saving XML and Analyze headers. */
    public static final String PREF_SAVE_XML_ON_HDR_SAVE = "SaveXMLOnHDRSave";

    /** Constant that indicates use of saving images, vois, and transfer functions. */
    public static final String PREF_SAVE_ALL_ON_SAVE = "SaveAllOnSave";

    /** Constant that indicates the saving of the xml thumbnail during a save. */
    public static final String PREF_SAVE_XML_THUMBNAIL = "SaveXMLThumbnail";

    /** Constant that indicates if lax file should be checked on mipav start. */
    public static final String PREF_LAX_CHECK = "PerformLaxCheck";

    /** Constant that indicates if user should be prompted on closing a frame. */
    public static final String PREF_CLOSE_FRAME_CHECK = "CloseFrameCheck";

    /** Constant indicating if VOI statistics file should be overwritten. */
    public static final String PREF_OVERWRITE_VOI_STATS = "OverwriteStatistics";

    /** Constant that indicates if logging is turned on. */
    public static final String PREF_LOG = "Log";

    /** Constant that indicates if the DICOM Receiver should be on by default. */
    public static final String PREF_AUTOSTART_DICOM_RECEIVER = "EnableDICOMReceiver";

    /**
     * Constant that indicates if the last used parameters for algorithm dialogs should be used when they are opened.
     */
    public static final String PREF_SAVE_DEFAULTS = "SaveDefaults";

    /** Constant that indicates if a prompt should be shown before an image file is overwritten. */
    public static final String PREF_SAVE_PROMPT_OVERWRITE = "SavePromptOverwrite";

    /** Constant that indicates if images saved as XML files should have their RAW data ZIPped. */
    public static final String PREF_SAVE_XML_ZIP = "SaveXMLZip";

    /** Constant that indicates if the DICOM overlay should be shown. */
    public static final String PREF_SHOW_DICOM_OVERLAYS = "ShowDICOMOverlays";

    /** Constant that indicates if the image overlay should be shown. */
    public static final String PREF_SHOW_IMAGE_OVERLAYS = "ShowImageOverlays";

    /** Constant that indicates if the paint border shown. */
    public static final String PREF_SHOW_PAINT_BORDER = "ShowPaintBorder";

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

    /**
     * Constant that indicates whether the triplanar frame should wait for a mouse released event before repainting all
     * 9 tri-planar images. Inserted here because repainting 9 images in real time gets to be tedious.
     */
    public static final String PREF_FAST_TRIPLANAR_REPAINT = "FastTriPlanarRepaint";

    /** Constant that indicates whether the angle of line VOIs should be shown on screen. */
    public static final String PREF_SHOW_LINE_ANGLE = "ShowLineVOIAngle";

    /** Constant that indicates whether the triplanar frame should use the old 2x2 layout or the newer 3x1 layout. */
    public static final String PREF_TRIPLANAR_2X2_LAYOUT = "TriPlanar2x2Layout";

    /** DOCUMENT ME! */
    public static final String PREF_MENU_FONT = "MenuFont";

    /** DOCUMENT ME! */
    public static final String PREF_MENU_FONT_SIZE = "MenuFontSize";

    /** DOCUMENT ME! */
    public static final String PREF_SHOW_VOI_NAME = "DrawVOIName";

    /** DOCUMENT ME! */
    public static final String PREF_SHOW_OUTPUT = "ShowOutput";

    /** DOCUMENT ME! */
    public static final String PREF_DEFAULT_SHORTCUTS = buildDefaultShortcuts();

    /** DOCUMENT ME! */
    public static final String PREF_LAST_STACK_FLAG = "LastStackFlag";

    /** DOCUMENT ME! */
    public static final String PREF_USE_VOI_XOR = "UseVOIXOR";

    /** Constants that indicate the SRB server login properties. */
    public static final String PREF_USERNAME_SRB = "userNameSRB";

    /** DOCUMENT ME! */
    public static final String PREF_SERVER_HOST_SRB = "serverHostSRB";

    /** DOCUMENT ME! */
    public static final String PREF_SERVER_DOMAIN_SRB = "serverDomainSRB";

    /** DOCUMENT ME! */
    public static final String PREF_SERVER_PORT_SRB = "serverPortSRB";

    /** DOCUMENT ME! */
    public static final String PREF_SERVER_AUTHENTICATION_SRB = "serverAuthenticationSRB";

    /** DOCUMENT ME! */
    public static final String PREF_STORAGE_RESOURCE_SRB = "defaultStorageResource";

    public static final String PREF_SRB_VERSION = "srbVersion";
    
    public static final String PREF_SRB_TRANSFER_MODE = "srbTransferMode";
    
    public static final String PREF_SRB_TEMP_DIR = "srbTempDir";
    
    /** Constant that indicates the initial directory in which to open the file chooser of the image browser. */
    public static final String PREF_DEFAULT_IMAGE_BROWSER_DIR = "DefaultImageBrowserDirectory";

    /**
     * Constant that indicates whether .img files should always be written in analyze format (as opposed to asking
     * whether to save as nifti).
     */
    public static final String PREF_ALWAYS_SAVE_IMG_AS_ANALYZE = "AlwaysSaveImgAsAnalyze";

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

    /** DOCUMENT ME! */
    private static Hashtable shortcutTable;

    /** DOCUMENT ME! */
    private static KeyStroke currentShortcut;


    /**
     * Static properties structure to store default MIPAV preferences.
     */
    static {
        defaultProps = new Properties();

        defaultProps.setProperty("EnableDICOMReceiver", "false");
        defaultProps.setProperty("TRIM", "0.3");
        defaultProps.setProperty("DEBUG", "false, false, false, false");
        defaultProps.setProperty("LogFilename", System.getProperty("user.dir") + File.separator + "mipav.log");
        defaultProps.setProperty("RawImageExtents", "256,256,0,0,0");
        defaultProps.setProperty("RawImageBigEndianByteOrder", "true");
        defaultProps.setProperty("RawImageResolutions", "1.0,1.0,1.0,1.0,1.0");
        defaultProps.setProperty("RawImageUnits", "6,6,6,6"); // 6 = millimeters
        defaultProps.setProperty("RawImageType", "3"); // 3 = ModelStorageBase.SHORT
        defaultProps.setProperty("RawImageDataOffset", "0");
        defaultProps.setProperty("ApplicationTitle", "MIPAV: ");
        defaultProps.setProperty("ApplicationIcon", "divinci.gif");
        defaultProps.setProperty("ActiveImageColor", "ff0000"); // red
        defaultProps.setProperty("CrosshairCursor", "default");
        defaultProps.setProperty("TRIM_FLAG", "true");
        defaultProps.setProperty("SplashGraphics", "true");
        defaultProps.setProperty("Scripting", "false");
        defaultProps.setProperty("PaintToolbar", "false");
        defaultProps.setProperty("ShowOverlays", "false");

        /** Medical Formats(*.dcm; *.ima; *.img; *.mnc; *.sig; *.xml; *.head) */
        defaultProps.setProperty("FilenameFilter", "8"); // 8 = ViewImageFileFilter.TECH
        defaultProps.setProperty("SaveXMLOnHDRSave", "false");
        defaultProps.setProperty("SaveXMLZip", "false");
        defaultProps.setProperty("SaveAllOnSave", "false");
        defaultProps.setProperty("OverwriteStatistics", "false");
        defaultProps.setProperty("LastXImages", "");
        defaultProps.setProperty("QuickListNumber", "4");
        defaultProps.setProperty("LastXProjects", "");
        defaultProps.setProperty("WarnAudioAVI", "true");
        defaultProps.setProperty("ShowPaintBorder", "false");

        // lightbox properties
        defaultProps.setProperty("LightBoxRowDependent", "true"); // display by columns
        defaultProps.setProperty("LightBoxGridRow", "0"); // compute 'best fit' num rows
        defaultProps.setProperty("LightBoxGridCol", "2"); // use 2 columns
        defaultProps.setProperty("LightBoxGridSize", "5");
        defaultProps.setProperty("LightBoxGridColor", "000000");
        defaultProps.setProperty("LightBoxBorderSize", "3");
        defaultProps.setProperty("LightBoxBorderColor", "960000");
        defaultProps.setProperty("LightBoxSelectedBorderSize", "2");
        defaultProps.setProperty("LightBoxSelectedBorderColor", "ffff00");
        defaultProps.setProperty("LightBoxMagnification", "45.0");
        defaultProps.setProperty("LightBoxLocation", "-10,-10"); // use incorrect value to compute 'best' location

        // look and feel properties
        defaultProps.setProperty("MenuFont", "Serif");
        defaultProps.setProperty("MenuFontSize", "12");
        // defaultProps.setProperty("MenuAcceleratorFontSize", "2");

        defaultProps.setProperty("MenuFontColor", "BLACK");
        defaultProps.setProperty("ShowOutput", "true");
        defaultProps.setProperty("Shortcuts", PREF_DEFAULT_SHORTCUTS);
        // defaultProps.setProperty("MenuAcceleratorFontColor", "");

        // username and srb server properties
        defaultProps.setProperty(PREF_USERNAME_SRB, "");
        defaultProps.setProperty(PREF_SERVER_HOST_SRB, "");
        defaultProps.setProperty(PREF_SERVER_DOMAIN_SRB, "");
        defaultProps.setProperty(PREF_SERVER_PORT_SRB, "5544");
        defaultProps.setProperty(PREF_SERVER_AUTHENTICATION_SRB, "ENCRYPT1");
        defaultProps.setProperty(PREF_STORAGE_RESOURCE_SRB, "");
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Adds a new shortcut to the hashtable. if there is already a command predefined for the keystroke set from <code>
     * setShortcut()</code>, it will be removed then replaced with the new command.
     *
     * @param   command  the command to map
     *
     * @return  DOCUMENT ME!
     */
    public static boolean addShortcut(String command) {

        if ((command != null) && (currentShortcut != null)) {

            if (isDefaultCommand(command)) {
                MipavUtil.displayWarning("This is a default function: shortcut can not be remapped");

                return false;
            }


            if (shortcutTable.containsKey(command)) {
                shortcutTable.remove(command);
            }

            if (shortcutTable.containsValue(currentShortcut)) {
                Enumeration e = shortcutTable.keys();

                while (e.hasMoreElements()) {
                    String key = (String) e.nextElement();

                    if (currentShortcut.equals(shortcutTable.get(key))) {
                        shortcutTable.remove(key);
                    }
                }
            }

            shortcutTable.put(command, currentShortcut);

            MipavUtil.displayInfo("Shortcut captured: " + currentShortcut.toString().replaceAll("pressed", "").trim() +
                                  " : " + command);

            saveShortcuts();
            currentShortcut = null;

            return true;
        }

        return false;
    }

    /**
     * Builds a hashtable of actioncommands (keys) with associated keystrokes from the Preferences file.
     */
    public static final void buildShortcutTable() {
        shortcutTable = new Hashtable();

        String str = getProperty("Shortcuts");

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

                    if ((keyStr.length() == 2) || (keyStr.length() == 3)) {
                        fIndex = Integer.parseInt(keyStr.substring(1));
                        shortcutTable.put(shortcutStr,
                                          KeyStroke.getKeyStroke(MipavUtil.functionKeys[fIndex], 0, false));
                    } else {

                        if (modifiers != 0) {

                            shortcutTable.put(shortcutStr,
                                              KeyStroke.getKeyStroke(keyStr.charAt(keyStr.length() - 1), modifiers,
                                                                     false));
                        } else {
                            shortcutTable.put(shortcutStr, KeyStroke.getKeyStroke(keyStr.charAt(0), modifiers, false));
                        }
                    }
                }

            } else {
                success = false;
            }
        } catch (Exception e) {
            e.printStackTrace();
            MipavUtil.displayWarning("Error reading shortcut preferences:  default shortcuts restored");
            success = false;
        }

        if (!success) {
            // build a default shortcut mapping and save to prefs

            shortcutTable.put("OpenNewImage", KeyStroke.getKeyStroke('F', Event.CTRL_MASK, false));
            shortcutTable.put("SaveImage", KeyStroke.getKeyStroke('S', Event.CTRL_MASK, false));
            shortcutTable.put("SaveImageAs", KeyStroke.getKeyStroke('S', Event.SHIFT_MASK + Event.CTRL_MASK, false));
            shortcutTable.put("undoVOI", KeyStroke.getKeyStroke('Z', Event.CTRL_MASK, false));
            shortcutTable.put("cutVOI", KeyStroke.getKeyStroke('X', Event.CTRL_MASK, false));
            shortcutTable.put("copyVOI", KeyStroke.getKeyStroke('C', Event.CTRL_MASK, false));
            shortcutTable.put("pasteVOI", KeyStroke.getKeyStroke('V', Event.CTRL_MASK, false));
            shortcutTable.put("selectAllVOIs", KeyStroke.getKeyStroke('A', Event.CTRL_MASK, false));
            shortcutTable.put("Tri-planar", KeyStroke.getKeyStroke('T', Event.CTRL_MASK, false));
            shortcutTable.put("AboutImage", KeyStroke.getKeyStroke('H', Event.CTRL_MASK, false));
            shortcutTable.put("EditImageInfo", KeyStroke.getKeyStroke('E', Event.CTRL_MASK, false));
            shortcutTable.put("MemoryUsage", KeyStroke.getKeyStroke('M', Event.CTRL_MASK, false));

            str = PREF_DEFAULT_SHORTCUTS;

            setProperty("Shortcuts", str);
        }

        // return table;
    }

    /**
     * If at least one debug level is turned on, output the string.
     *
     * @param  string  String string to be output
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
     * @param  string  String String to be output
     * @param  level   int level of debug to check
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
            } catch (Exception e) { }
        }
    }

    /**
     * Indicates whether debug is turned on for the given level.
     *
     * @param   level  debugging level to check for
     *
     * @return  true if the given debugging level is activated
     */
    public static final boolean debugLevel(int level) {

        String str = getProperty("DEBUG");

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

                        if (token.equalsIgnoreCase("TRUE") || token.equalsIgnoreCase("on") ||
                                token.equalsIgnoreCase("yes")) {
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
     * @return  boolean[] debug levels
     */
    public static final boolean[] getDebugLevels() {
        boolean[] levels = new boolean[5];

        String str = Preferences.getProperty("DEBUG");
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
     * @return  the default frame rate as a floating point number.
     */
    public static float getDefaultFrameRate() {
        String str = getProperty("DefaultFrameRate");

        if (str != null) {

            try {
                return Float.parseFloat(str);
            } catch (Exception ex) {
                setProperty("DefaultFrameRate", "10.0");
            }
        } else {
            setProperty("DefaultFrameRate", "10.0");
        }

        return 10.0f;
    }

    /**
     * Accessor to get the default chat/file server host key.
     *
     * @return  default host key
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
     * @return  default server key
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
     * Accessor to get default DICOM image storage location.
     *
     * @return  default storage information
     */
    public static String getDefaultStorageKey() {
        String key = "Storage1";
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
     * @param   dialogName  String name of dialog
     *
     * @return  String space separated default parameters for dialog
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
     * @return  Vector List of strings that identify the columns.
     */
    public static Vector getDICOMBrowserTableConfiguration() {

        if (mipavProps == null) {
            read();
        }

        String configStr = getProperty("DICOMBrowserConfig");

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
     * @return  String The full path of the dictionary in which a subset of the DICOM tags are stored.
     */
    public static final String getDICOMSaveDictionary() {
        String key = getProperty("DICOMSaveDictionary");
        String defaultKey = System.getProperties().getProperty("user.home") + File.separator + "mipav" +
                            File.separator + "dicomsave.dictionary";

        if (key != null) {
            File file = new File(key);

            if (file.isFile() && file.canWrite() && !file.isDirectory()) {
                return key;
            }
        }

        return defaultKey;

    }

    /**
     * Accessor to get host (chat/file) server Internet Protocol for selected Name.
     *
     * @param   AETitle  application entity title
     *
     * @return  IP address of asssociated Name
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
     * @return  string containing the name of the system icon
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
     * Accessor to get DICOM image server Internet Protocol for selected AETitle.
     *
     * @param   AETitle  application entity title
     *
     * @return  IP address of asssociated AETitle
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
     * @param   index  Index of image.
     *
     * @return  Name of image at index.
     */
    public static final String getLastImageAt(int index) {
        String str = getProperty("LastXImages");
        String[] images = null;

        if (str != null) {

            try {
                StringTokenizer tok = new StringTokenizer(str, ";");

                if ((tok != null) && (tok.countTokens() > 0)) {
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

            if ((images != null) && (images.length > index)) {


                return images[index];
            }
        }

        return null;
    }

    /**
     * Used by ViewOpenImageSequence to get the saved number of channels used when opening an image sequence.
     *
     * @return  String - the value that was saved to the preferences file, represented by a String, of the number of
     *          channels that were used in the corresponding text box of this frame the last time the value was saved
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
     * @return  String - the value that was saved to the preferences file, represented by a String, of the Z-C-T
     *          ordering of data that was used in the corresponding text box of this frame the last time the value was
     *          saved
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
     * @return  String
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
     * @return  String - the value that was saved to the preferences file, represented by a String, of the number of
     *          slices that were used in the corresponding text box of this frame the last time the value was saved
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
     * @return  String - the value that was saved to the preferences file, represented by a String, of the number of
     *          time points that were used in the corresponding text box of this frame the last time the value was saved
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
     * @deprecated  Returns the project of "last four projects" at the given index. Index should be between 0 and 3.
     *
     * @param       index  Index of project.
     *
     * @return      Name of project at index.
     */
    public static final String getLastProjectAt(int index) {
        String str = getProperty("LastXProjects");
        StringTokenizer tok;
        String[] projects = null;

        if (str != null) {

            try {
                tok = new StringTokenizer(str, ";");

                if ((tok != null) && (tok.countTokens() > 0)) {
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
     * @return  Array of strings representing the last images that were loaded
     */
    public static final String[] getLastXImages() {
        String str = getProperty("LastXImages");
        String quickStr = getProperty("QuickListNumber");
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

                if ((tok != null) && (tok.countTokens() > 0)) {

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
                        setProperty("LastXImages", "");

                        return new String[0];
                    }

                    String newStr = "";

                    for (int i = 0; i < tempVec.size(); i++) {
                        newStr += (String) tempVec.elementAt(i) + ";";
                    }

                    // chop off the last ";"
                    newStr = newStr.substring(0, newStr.length() - 1);
                    setProperty("LastXImages", newStr);
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
            } catch (Exception ex) { }
        }

        return images;
    }

    /**
     * DOCUMENT ME!
     *
     * @deprecated  Gets the last four projects that were loaded from the Preferences file. Shortens the names so the
     *              menu isn't too wide.
     *
     * @return      Array of strings representing the last projects that were loaded, max of four.
     */
    public static final String[] getLastXProjects() {
        String str = getProperty("LastXProjects");
        StringTokenizer tok;
        String[] projects = null;
        String testStr = null;
        Vector tempVec = new Vector();
        int numList = 0;
        boolean someRemoved = false;
        int maxList;

        try {
            maxList = Integer.parseInt(getProperty("QuickListNumber"));
        } catch (Exception e) {
            maxList = 4;
        }

        if (str != null) {

            try {
                tok = new StringTokenizer(str, ";");

                if ((tok != null) && (tok.countTokens() > 0)) {

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
                        setProperty("LastXProjects", "");

                        return new String[0];
                    }

                    String newStr = "";

                    for (int i = 0; i < tempVec.size(); i++) {
                        newStr += (String) tempVec.elementAt(i) + ";";
                    }

                    // chop off the last ";"
                    newStr = newStr.substring(0, newStr.length() - 1);
                    setProperty("LastXProjects", newStr);
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
     * @return  The next host key.
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
     * @return  The next server key.
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
     * @return  The next storage key.
     */
    public static String getNextStorageKey() {
        String key = "Storage1";

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
     * @return  int operating system
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
     * Retrieves the list of overlay names (user can type whatever).
     *
     * @param   isDicom  boolean is this a dicom image
     *
     * @return  String[] array of strings for overlay labeling
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
     * @param   isDicom  if true loads the overlay IDs for a DICOM image
     *
     * @return  the overlay codes that identifies what should be displayed.
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
     * @return  directory name for the property PluginUserDir or the directory name for the property ImageDirectory if
     *          PluginUserDir is not set.
     */
    public static final String getPluginUserDir() {
        String str = getProperty("PluginUserDir");

        if (str == null) {
            return getProperty("ImageDirectory");
        }

        return str;
    }

    /**
     * Accessor to get DICOM image server's port for selected AETitle.
     *
     * @param   AETitle  application entity title
     *
     * @return  port address of asssociated AETitle
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

                return (Integer.valueOf(tok.nextToken().trim()).intValue()); // return port address from preference file
            }

            key = key.substring(0, 6) + (Integer.valueOf(key.substring(6)).intValue() + 1);
        }

        return -1; // no match
    }

    /**
     * Return the directory where mipav-generated, user-specific files should be placed.
     *
     * @return  the place to put user-specific files
     */
    public static final String getPreferencesDir() {
        return preferencesDir;
    }

    /**
     * Accessor to return a specfic properties given a key.
     *
     * @param   key  the property key.
     *
     * @return  the mipav property. The method returns null if the property is not found.
     */
    public static final String getProperty(String key) {

        if (mipavProps == null) {
            read();
        }

        return mipavProps.getProperty(key);
    }

    /**
     * Returns the default script directory. if null, returns the user's current working directory + "scripts"
     *
     * @return  script dir
     */
    public static final String getScriptsDirectory() {
        String str = getProperty("ScriptsDir");

        if (str == null) {

            // set the default
            str = System.getProperty("user.dir") + File.separator + "scripts";
            setProperty("ScriptsDir", str);
        }

        return str;
    }
    
    /**
     * Sets the scripts directory to the new scripts directory.
     * @param scriptsDirectory  the new scripts directory
     */
    public static final void setScriptsDirectory(String scriptsDirectory) {
        if (scriptsDirectory != null) {
            setProperty("ScriptsDir", scriptsDirectory);
        }
    }
    
    public static final String getScriptFile(){
        String s = getProperty("ScriptFile");
        if(s == null){
            s = "";
        }
        return s;
    }

    public static final void setScriptFile(String scriptFile){
        if(scriptFile != null){
            setProperty("ScriptFile", scriptFile);
        }
    }
    /**
     * Retrieves the SRB server authentication scheme.
     *
     * @return  the srb server's authentication scheme.
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
     * @return  DOCUMENT ME!
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
     * @return  DOCUMENT ME!
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
     * @return  DOCUMENT ME!
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
     * @param   command  String the command to check
     *
     * @return  KeyStroke the shortcut's keystroke
     */
    public static KeyStroke getShortcut(String command) {

        if (command != null) {
            return (KeyStroke) shortcutTable.get(command);
        } else {
            return null;
        }
    }

    /**
     * Gets the command associated with the shortcut (if there is one).
     *
     * @param   ks  KeyStroke the keystroke to check
     *
     * @return  String the command associated (or null if none)
     */
    public static String getShortcutCommand(KeyStroke ks) {
        KeyStroke shortcut = null;
        String command = null;

        Enumeration en = shortcutTable.keys();

        while (en.hasMoreElements()) {
            command = (String) en.nextElement();
            shortcut = (KeyStroke) shortcutTable.get(command);

            if (shortcut.equals(ks)) {
                return command;
            }
        }

        return null;
    }

    /**
     * Gets the Hashtable used to store the shortcut keystrokes and commands.
     *
     * @return  Hashtable the shortcut hashtable
     */
    public static Hashtable getShortcutTable() {
        return shortcutTable;
    }

    /**
     * Retrieves the default storage resource for the user.
     *
     * @return  DOCUMENT ME!
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
     * @return  Dimension - the dimension that was last used to subsample an image while using the ViewOpenImageSequence
     *          tool, null if dimension is invalid in the preferences file
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
     * @return  the trim value.
     */
    public static final float getTrim() {

        if (mipavProps == null) {
            read();
        }

        String str = getProperty("TRIM");

        if (str != null) {
            return (Float.valueOf(str).floatValue());
        }

        return 0.3f; // no match
    }

    /**
     * Accessor to get the TRIM parameter (trimming contour of points).
     *
     * @return  the trim boolean to indicate if the directly adjacient points should be trimmed.
     */
    public static boolean getTrimAdjacient() {

        if (mipavProps == null) {
            read();
        }

        String str = getProperty("TRIM_FLAG");

        if (str != null) {
            return (Boolean.valueOf(str).booleanValue());
        }

        return true; // no match
    }

    /**
     * Retrieves the user name to login the SRB server.
     *
     * @return  DOCUMENT ME!
     */
    public static String getUserNameSRB() {

        if (mipavProps == null) {
            read();
        }

        return getProperty(PREF_USERNAME_SRB);
    }

    /**
     * Gets the VOI color increment value. Ben add here.
     *
     * @return  int
     */
    public static int getVOIColorIncrement() {
        int increment = 0;

        String str = getProperty("VOIColor");

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
     * @return  Color color to draw with
     */
    public static Color getVOIDrawColor() {

        String str = getProperty("VOIDrawColor");
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
     * Gets the boolean state of the property, where in the preferences file the &quot;yes&quot;, &quot;on&quot;,
     * &quot;true&quot; all return <code>true</code>, but anything else (or non-existent property) returns <code>
     * false</code>. This method is provided for semantic preference, but is exactly the same as (in fact, calls) <code>
     * isPreference</code>.
     *
     * @see     #isPreference(String)
     *
     * @return  boolean <code>true</code> when the named property has the String the &quot;yes&quot;, &quot;on&quot;,
     *          &quot;true&quot;. <code>false</code> for any other string, or if the property does not exist.
     *
     * @param   propertyName  Name of a property in preferences to check.
     */
    public static final boolean is(String propertyName) {
        return isPreference(propertyName);
    }

    /**
     * Returns flag indicating if the debug flag is true or false. (and in preferences v2, if debug level is zero,
     * returns false. Returns true if debug level is greater than 0.)
     *
     * <p>debug level is the amount of output given for debugging mipav.</p>
     *
     * @return  returns true if either debug flag is true or level is greater than zero and false if either debug flag
     *          is false or level is zero.
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

                if (str.equalsIgnoreCase("true") || str.equalsIgnoreCase("on") || str.equalsIgnoreCase("yes")) { // debug YES
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
     * @param   command  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static boolean isDefaultCommand(String command) {

        String str = buildDefaultShortcuts();
        StringTokenizer tok = null;
        String shortcutStr;

        tok = new StringTokenizer(str, ";");

        String token;

        while (tok.hasMoreTokens()) {
            token = tok.nextToken();
            shortcutStr = token.substring(0, token.indexOf(","));

            if (command.equals(shortcutStr)) {
                return true;
            }
        }

        return false;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   ks  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static boolean isDefaultKeyStroke(KeyStroke ks) {

        if (ks == null) {
            return false;
        }

        String str = PREF_DEFAULT_SHORTCUTS;
        StringTokenizer tok = null;
        int modifiers;

        // String shortcutStr;
        String keyStr;


        tok = new StringTokenizer(str, ";");

        String token;

        while (tok.hasMoreTokens()) {
            token = tok.nextToken();

            // shortcutStr = token.substring(0, token.indexOf(","));
            keyStr = token.substring(token.indexOf(",") + 1, token.length()).trim();
            modifiers = 0;

            if (keyStr.indexOf("CTRL") != -1) {
                modifiers += Event.CTRL_MASK;
            }

            if (keyStr.indexOf("ALT") != -1) {
                modifiers += Event.ALT_MASK;
            }

            if (keyStr.indexOf("SHIFT") != -1) {
                modifiers += Event.SHIFT_MASK;
            }

            int fIndex = 0;

            if ((keyStr.length() == 2) || (keyStr.length() == 3)) {
                fIndex = Integer.parseInt(keyStr.substring(1));

                if (ks.equals(KeyStroke.getKeyStroke(MipavUtil.functionKeys[fIndex], 0, false))) {
                    return true;
                }
            } else {

                if (modifiers != 0) {

                    if (ks.equals(KeyStroke.getKeyStroke(keyStr.charAt(keyStr.length() - 1), modifiers, false))) {
                        return true;
                    }
                } else {

                    if (ks.equals(KeyStroke.getKeyStroke(keyStr.charAt(0), modifiers, false))) {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    /**
     * Gets the boolean state of the property, where in the preferences file the &quot;yes&quot;, &quot;on&quot;,
     * &quot;true&quot; all return <code>true</code>, but anything else (or non-existent property) returns <code>
     * false</code>. While not as intuitive, perhaps, as some of the specially named property-retrieval methods (eg.,
     * <code>isSplashScreen</code>), this method can be used to replace all of those methods when the preferences
     * property is known. It can also be convenient for limited-use properties used in debugging.
     *
     * @return  boolean <code>true</code> when the named property has the String the &quot;yes&quot;, &quot;on&quot;,
     *          &quot;true&quot;. <code>false</code> for any other string, or if the property does not exist.
     *
     * @param   propertyName  Name of a property in preferences to check.
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
     * @param   propertyName  the name of the property to check for
     *
     * @return  <code>true</code> if there is some preference set for the given property name
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
     * @return  true if found else false
     */
    public static boolean read() {
        mipavProps = null;

        BufferedInputStream bFile = null;

        // initialze the properties to the default properties
        mipavProps = new Properties(defaultProps);


        if (!new File(preferencesFile).exists()) {

            // nothing user-specific to override the defaults, so just save what there is.
            save();
        }

        /// TODO: maybe find pref files in other locations and delete them..

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
            } catch (IOException ioe) { }
        }

        buildShortcutTable();

        return true;
    }

    /**
     * Removes the property key from the MIPAV property list.
     *
     * @param  key  the property key.
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
     * @param  commandToRemove  String
     */
    public static void removeShortcut(String commandToRemove) {
        shortcutTable.remove(commandToRemove);
        saveShortcuts();
    }

    /**
     * Saves preference file to the user's 'home' directory.
     *
     * @return  boolean true if successful, false otherwise
     */
    public static boolean save() {
        BufferedOutputStream bFile = null;

        try {
            File dFile = new File(preferencesDir);

            if (!dFile.exists() && !dFile.mkdirs()) {
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
            } catch (IOException ioe) { }
        }

        return true;
    }

    /**
     * Saves the last used parameters for the given dialog.
     *
     * @param  dialogName      String the name of the dialog
     * @param  defaultsString  String the String to save for this dialog
     */
    public static final void saveDialogDefaults(String dialogName, String defaultsString) {
        setProperty(dialogName, defaultsString);
    }

    /**
     * Sets the property for shortcuts by building string from ViewUserInterface's Shortcut Hashtable.
     */
    public static final void saveShortcuts() {

        // Hashtable shortcutTable = ViewUserInterface.getReference().getShortcutTable();
        String command = null;
        KeyStroke ks = null;
        Enumeration e = shortcutTable.keys();

        String longStr = new String();

        String singleChar = null;

        int m = 0;


        try {

            while (e.hasMoreElements()) {
                command = (String) e.nextElement();
                ks = (KeyStroke) shortcutTable.get(command);
                m = ks.getModifiers();

                singleChar = ks.toString().trim();

                if (singleChar.charAt(singleChar.length() - 2) != 'F') {
                    singleChar = singleChar.substring(singleChar.length() - 1, singleChar.length());
                    longStr += command + ",";

                    if ((m & (InputEvent.SHIFT_DOWN_MASK | InputEvent.SHIFT_MASK)) != 0) {
                        longStr += "SHIFT";
                    }

                    if ((m & (InputEvent.CTRL_DOWN_MASK | InputEvent.CTRL_MASK)) != 0) {
                        longStr += "CTRL";
                    }

                    if ((m & (InputEvent.ALT_DOWN_MASK | InputEvent.ALT_MASK)) != 0) {
                        longStr += "ALT";
                    }
                } else {
                    longStr += command + ",";
                    singleChar = singleChar.replaceAll("pressed", "");
                }

                longStr += singleChar + ";";


            }

            setProperty("Shortcuts", longStr);
        } catch (Exception ex) {
            System.err.println("error setting shortcuts in prefs");
        }
    }

    /**
     * Used by ViewOpenImageSequence to save its subsample dimensions for the next time it is opened. The dimension is
     * actually saved as its two parts, width and height
     *
     * @param  subsampleDimension  Dimension - the subsample dimension to save
     */
    public static final void saveSubsampleDimensions(Dimension subsampleDimension) {

        if (subsampleDimension == null) {
            return;
        }

        setProperty("subsampleWidth", String.valueOf(subsampleDimension.width));
        setProperty("subsampleHeight", String.valueOf(subsampleDimension.height));
    }

    /**
     * Sets whether all imgs should be saved in analyze format (i.e., don't ask about whether to save as analyze/nifti).
     *
     * @param  doSaveAnalyze  whether to always save .img files as analyze format files
     */
    public static final void setAlwaysSaveImgAsAnalyze(boolean doSaveAnalyze) {
        setProperty(PREF_ALWAYS_SAVE_IMG_AS_ANALYZE, Boolean.toString(doSaveAnalyze));
    }

    /**
     * Sets the current keystroke shortcut for assignment once a command has been passed in.
     *
     * @param  ks  KeyStroke the shortcut keystroke
     */
    public static void setCurrentShortcut(KeyStroke ks) {
        currentShortcut = ks;
    }

    /**
     * Sets the debug levels (lowest is index 0) on/off levels[0] is MINOR levels[1] is ALGORITHM levels[2] is FILEIO
     * levels[3] is COMMS (dicom).
     *
     * @param  levels  boolean[] true/false for each level of debug
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

            setProperty("DEBUG", str);
        }
    }

    /**
     * Sets the user-configured columns that indicate the DICOM tags that are displayed in the DICOM browser table.
     *
     * @param   newConfiguration  Vector List of strings that identify the columns.
     *
     * @return  boolean Success or failure of the setting the columns.
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

        return setProperty("DICOMBrowserConfig", configStr);
    }

    /**
     * Sets the last image loaded in the Preferences file. If it is a duplicate, moves it up on the list.
     *
     * @param  name       Full pathname of last image loaded.
     * @param  multiFile  true if the last image was a multi-file image, false otherwise
     * @param  numDims    DOCUMENT ME!
     */
    public static final void setLastImage(String name, boolean multiFile, int numDims) {

        if (name == null) {
            return;
        }

        String newName;
        String newProp;
        String str = getProperty("LastXImages");
        String quickStr = getProperty("QuickListNumber");
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

                    while ((i < quickListNumber) && tok.hasMoreTokens()) {
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

                        if (!quickListToken.equals(newName) && new File(added).exists()) {
                            newProp += ";" + quickListToken;
                            i++;
                        }
                    }
                }
            } catch (Exception ex) { }
        }

        // System.err.println("New quicklist string is: " + newProp);

        setProperty("LastXImages", newProp);
    }

    /**
     * Used by ViewOpenImageSequence to save the user's most recent slices, channels, time points, and ordering that
     * were specified in the component.
     *
     * @param  slices      String - number of slices in the slices text box of the ViewOpenImageSequence frame
     * @param  channels    String - number of channels in the channels text box of the ViewOpenImageSequence frame
     * @param  timePoints  String - number of time points in the time points text box of the ViewOpenImageSequence frame
     * @param  ordering    String - ordering sequence used in the radio button panel of the ViewOpenImageSequence frame
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
     * @param  path  DOCUMENT ME!
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
     * @param  name  Full pathname of last project loaded.
     */
    public static final void setLastProject(String name) {

        if (name == null) {
            return;
        }

        String str = getProperty("LastXProjects");
        StringTokenizer tok;

        int quickListNumber;

        try {
            quickListNumber = Integer.parseInt(getProperty("QuickListNumber"));
        } catch (Exception e) {
            quickListNumber = 4;
        }

        String newProp = name;

        if (str != null) {

            try {
                tok = new StringTokenizer(str, ";");

                int i = 1;

                if (tok != null) {

                    while ((i < quickListNumber) && tok.hasMoreTokens()) {
                        String added = tok.nextToken();

                        if (!added.equals(name) && new File(added).exists()) {
                            newProp += ";" + added;
                            i++;
                        }
                    }
                }
            } catch (Exception ex) { }
        }

        setProperty("LastXProjects", newProp);
    }

    /**
     * Sets the message frame.
     *
     * @param  mFrame  Message frame to set.
     */
    public static final void setMessageFrame(ViewJFrameMessage mFrame) {
        messageFrame = mFrame;
    }

    /**
     * Sets the labels for each overlay position. The labels can be user input with any text and will be displayed
     * before the overlay values
     *
     * @param  isDicom   boolean is the image DICOM
     * @param  overlays  String[] array of strings (16) for each of the overlay positions
     */
    public static final void setOverlayNames(boolean isDicom, String[] overlays) {
        String str = "";

        for (int i = 0; i < 16; i++) {

            if ((overlays[i] == null) || overlays[i].equals("")) {
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
     * @param  isDicom   if true identifies the tags as a list of DICOMOverlays else identifies the tags as
     *                   ImageOverlays (i.e., it is not a DICOM image)
     * @param  overlays  an array of 16 strings (4 for each corner) that identifies what is to be displayed in the
     *                   image.
     */
    public static final void setOverlays(boolean isDicom, String[] overlays) {
        String str = "";

        for (int i = 0; i < 16; i++) {

            if ((overlays[i] != null) && !overlays[i].equals(JDialogOverlay.BLANK_OVERLAY)) {
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
     * @param  fileName  the file name (eg mipav.preferences), which should be in $HOME/mipav/
     */
    public static final void setPreferencesFileName(String fileName) {
        preferencesFileName = fileName;
        preferencesFile = preferencesDir + File.separator + preferencesFileName;
    }

    /**
     * Sets the property key in the MIPAV property list and saves it to a file.
     *
     * @param   key    the property key.
     * @param   value  the value of the property key.
     *
     * @return  true if the property was set and saved successfully, false otherwise
     */
    public static final boolean setProperty(String key, Object value) {

        if (mipavProps == null) {
            read();
        }

        mipavProps.put(key, value);

        return save();
    }

    /**
     * Sets the server authentication schema which the SRB server is using.
     *
     * @param  serverAuthSRB  the new SRB server authentication schema
     */
    public static void setServerAuthSRB(String serverAuthSRB) {
        setProperty(PREF_SERVER_AUTHENTICATION_SRB, serverAuthSRB);
    }

    /**
     * Sets the domain information of the SRB server.
     *
     * @param  serverDomainSRB  DOCUMENT ME!
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
     * @param  serverHostSRB  DOCUMENT ME!
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
     * @param  serverPortSRB  the new SRB server port
     */
    public static void setServerPortSRB(int serverPortSRB) {
        setProperty(PREF_SERVER_PORT_SRB, Integer.toString(serverPortSRB));
    }

    /**
     * Sets whether to show the DICOM overlays.
     *
     * @param  doShow  boolean true means show DICOM overlays
     */
    public static final void setShowDICOMOverlays(boolean doShow) {
        setProperty("ShowDICOMOverlays", Boolean.toString(doShow));
    }

    /**
     * Sets whether to show the Image overlays.
     *
     * @param  doShow  boolean true means show image overlays
     */
    public static final void setShowImageOverlays(boolean doShow) {
        setProperty("ShowImageOverlays", Boolean.toString(doShow));
    }

    /**
     * Sets the default storage resource for the user.
     *
     * @param  storageResource  the new storage resource.
     */
    public static void setStorageResourceSRB(String storageResource) {
        setProperty(PREF_STORAGE_RESOURCE_SRB, storageResource);
    }

    /**
     * Sets the user name to login the SRB server.
     *
     * @param  userNameSRB  the user name of the srb server.
     */
    public static void setUserNameSRB(String userNameSRB) {

        if (userNameSRB == null) {
            return;
        }

        setProperty(PREF_USERNAME_SRB, userNameSRB);
    }

    /**
     * Returns the transfer mode of srb server.
     * @return the transfer mode of srb server.
     */
    public static String getSRBTransferMode(){

        if (mipavProps == null) {
            read();
        }

        return getProperty(PREF_SRB_TRANSFER_MODE);
    }
    
    /**
     * Sets the transfer mode of srb server to the new transfer mode.
     * @param transferMode the new transfer mode.
     */
    public static void setSRBTransferMode(String transferMode){
        if(transferMode == null){
            return;
        }
        setProperty(PREF_SRB_TRANSFER_MODE, transferMode);
    }
    
    /**
     * Returns the srb server version which Jargon need to be adapted to.
     * @return  the srb server version which Jargon need to be adapted to.
     */
    public static String getSRBVersion(){

        if (mipavProps == null) {
            read();
        }

        return getProperty(PREF_SRB_VERSION);
        
    }
    
    /**
     * Sets the srb server version which Jargon need to be adapted to.
     * @param srbVersion  the new srb server version.
     */
    public static void setSRBVersion(String srbVersion){
        if(srbVersion == null){
            return;
        }
        setProperty(PREF_SRB_VERSION, srbVersion);
    }
    
    /**
     * Returns the temporary directory which is used during file transfer.
     * @return the temporary directory which is used during file transfer.
     */
    public static String getSRBTempDir(){

        if (mipavProps == null) {
            read();
        }

        return getProperty(PREF_SRB_TEMP_DIR);
        
    }

    /**
     * Sets the temporary directory which is used during file transfer to the
     * new temporary directory.
     * @param tempDir the temporary directory.
     */
    public static void setSRBTempDir(String tempDir){
        if(tempDir == null){
            return;
        }
        setProperty(PREF_SRB_TEMP_DIR, tempDir);
    }
    
    /**
     * Returns the index of the latest used file filter.
     * @return the index of the latest used file filter.
     */
    public static int getFileFilter(){
        if(mipavProps == null){
            read();
        }
        
        return Integer.parseInt(getProperty("FilenameFilter"));
    }
    
    /**
     * Sets the current used file filter as the latest used file filter.
     * @param fileFilterIndex  the index of the current used file filter.
     */
    public static void setFileFilter(int fileFilterIndex){
        setProperty("FilenameFilter", new Integer(fileFilterIndex));
    }
    
    /**
     * Returns the image directory used last time.
     * @return  the image directory used last time.
     */
    public static String getImageDirectory() {

        String str = Preferences.getProperty("ImageDirectory");

        if (str != null) {
            return str;
        } else {
            return (System.getProperties().getProperty("user.dir"));
        }
    }
    
    /**
     * Sets the image directory as the latest used image directory. 
     * @param imageDirectory the current used image directory.
     */
    public static void setImageDirectory(File imageDirectory){
        if(imageDirectory == null){
            return;
        }
        
        if(imageDirectory.isDirectory()){
            setProperty("ImageDirectory", imageDirectory.getAbsolutePath());
        }else{
            setProperty("ImageDirectory", imageDirectory.getParent());
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private static String buildDefaultShortcuts() {
        String str = new String("OpenNewImage,CTRLF;");
        str += "SaveImage,CTRLS;";
        str += "SaveImageAs,CTRLSHIFTS;";
        str += "undoVOI,CTRLZ;";
        str += "cutVOI,CTRLX;";
        str += "copyVOI,CTRLC;";
        str += "pasteVOI,CTRLZ;";
        str += "selectAllVOIs,CTRLA;";
        str += "Tri-planar,CTRLT;";
        str += "AboutImage,CTRLH;";
        str += "MemoryUsage,CTRLM;";
        str += "quickLUT,Q;";
        str += "ShowPaintBorder,B;";


        // build quicklist shortcuts
        for (int i = 0; i < 9; i++) {
            str += "LastImage " + i + ",CTRL" + (i + 1) + ";";
        }

        return str;
    }
    
}
