package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TokenizerException;

import gov.nih.mipav.view.dialogs.JPanelPixelExclusionSelector.RangeType;
import gov.nih.mipav.view.icons.PlaceHolder;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.*;
import java.lang.management.ManagementFactory;
import java.lang.management.MemoryUsage;
import java.lang.reflect.Method;
import java.net.*;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.help.HelpBroker;
import javax.help.HelpSet;
import javax.swing.*;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

import libnative.MipavJarLoader;


/**
 * A collection of static methods and frequently used and useful constants are in this utility class.
 */
public class MipavUtil extends JComponent {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4180127127881019813L;

    /** A 12 point, serif font. */
    public static Font defaultMenuFont = new Font("Serif", 0, 12);

    /** A 10 point, serif font. */
    public static Font defaultAcceleratorFont = new Font("Serif", Font.BOLD, 10);

    /** A 10 point, plain, serif font. */
    public static Font font10 = new Font("Serif", Font.PLAIN, 10);

    /** A 12 point, plain, serif font. */
    public static Font font12 = new Font("Serif", Font.PLAIN, 12);

    /** A 12 point, bold, serif font. */
    public static Font font12B = new Font("Serif", Font.BOLD, 12);

    /** A 12 point, italic, serif font. */
    public static Font font12I = new Font("Serif", Font.ITALIC, 12);

    /** A 13 point, plain, serif font. */
    public static Font font13 = new Font("Serif", Font.PLAIN, 13);

    /** A 13 point, bold, serif font. */
    public static Font font13B = new Font("Serif", Font.BOLD, 13);

    /** A 13 point, italic, serif font. */
    public static Font font13I = new Font("Serif", Font.ITALIC, 13);

    /** A 14 point, plain, serif font. */
    public static Font font14 = new Font("Serif", Font.PLAIN, 14);

    /** A 14 point, bold, serif font. */
    public static Font font14B = new Font("Serif", Font.BOLD, 14);

    /** A 14 point, italic, serif font. */
    public static Font font14I = new Font("Serif", Font.ITALIC, 14);

    /** A 16 point, bold, serif font. */
    public static Font font16B = new Font("Serif", Font.BOLD, 16);

    /** A 18 point, bold, serif font. */
    public static Font font18B = new Font("Serif", Font.BOLD, 18);

    /** A 10 point, plain, courier font. */
    public static Font courier10 = new Font("Courier", Font.PLAIN, 10);

    /** A 12 point, plain, courier font. */
    public static Font courier12 = new Font("Courier", Font.PLAIN, 12);

    /** A 12 point, bold, courier font. */
    public static Font courier12B = new Font("Courier", Font.BOLD, 12);

    /** A 13 point, plain, courier font. */
    public static Font courier13 = new Font("Courier", Font.PLAIN, 13);

    /** A 13 point, bold, courier font. */
    public static Font courier13B = new Font("Courier", Font.BOLD, 13);

    /** A 13 point, plain, arial font. */
    public static Font arial13 = new Font("Arial", Font.PLAIN, 13);

    /** A 13 point, bold, arial font. */
    public static Font arial13B = new Font("Arial", Font.BOLD, 13);

    /** Standard cursor: default. */
    public static final Cursor defaultCursor = new Cursor(Cursor.DEFAULT_CURSOR);

    /** A crosshair cursor. */
    public static final Cursor crosshairCursor = new Cursor(Cursor.CROSSHAIR_CURSOR);

    /** A move cursor. */
    public static final Cursor moveCursor = new Cursor(Cursor.MOVE_CURSOR);

    /** Standard cursor: point (resize, NE). */
    public static final Cursor pointCursor = new Cursor(Cursor.NE_RESIZE_CURSOR);

    /** A resizing cursor. */
    public static final Cursor resizeCursor = new Cursor(Cursor.E_RESIZE_CURSOR);

    /** A hand cursor. */
    public static final Cursor handCursor = new Cursor(Cursor.HAND_CURSOR);

    /** Custom cursor: no cursor. */
    public static Cursor blankCursor;

    /** Custom cursor: magnify region. */
    public static Cursor magRegionCursor;

    /** Standard cursor: add a point (hand). */
    public static final Cursor addPointCursor = new Cursor(Cursor.HAND_CURSOR);

    /** Custom cursor: small pointer. */
    public static Cursor smallPointerCursor;

    /** DOCUMENT ME! */
    public static Cursor probeCursor;

    /** DOCUMENT ME! */
    public static Cursor magnifyCursor;

    /** DOCUMENT ME! */
    public static Cursor unmagnifyCursor;

    /** Custom cursor: quick LUT. */
    public static Cursor quickLUTcursor = MipavUtil.defaultCursor; // be sure to change when there is a decent image

    // available.

    /** Cursor for doing annotations. */
    public static final Cursor textCursor = new Cursor(Cursor.TEXT_CURSOR);

    /** Standard cursor: wait. */
    public static final Cursor waitCursor = new Cursor(Cursor.WAIT_CURSOR);

    /** Standard cursor: wand (hand). */
    public static final Cursor wandCursor = new Cursor(Cursor.HAND_CURSOR);

    /** DOCUMENT ME! */
    public static Cursor winLevelCursor;

    /** The current version number, coded as a String, read and then cached by getVersion(). */
    protected static String version = null;

    /** The default size that all buttons should be. */
    public static final Dimension defaultButtonSize = new Dimension(90, 30);

    /** The horizontally widen size that all buttons should be. */
    public static final Dimension widenButtonSize = new Dimension(110, 30);

    /** DOCUMENT ME! */
    public static final int DEFAULT_ICON_WIDTH = 40;

    /** DOCUMENT ME! */
    public static final int DEFAULT_ICON_HEIGHT = 24;

    /** DOCUMENT ME! */
    public static int MENU_Y_PADDING = 27;

    private static boolean forceQuiet = false;

    /** DOCUMENT ME! */
    public static final int[] functionKeys = new int[] {0, KeyEvent.VK_F1, KeyEvent.VK_F2, KeyEvent.VK_F3, KeyEvent.VK_F4, KeyEvent.VK_F5, KeyEvent.VK_F6,
            KeyEvent.VK_F7, KeyEvent.VK_F8, KeyEvent.VK_F9, KeyEvent.VK_F10, KeyEvent.VK_F11, KeyEvent.VK_F12};

    /**
     * Displays the Java Help dialog indexed directly to the section identified by the ID passed in.
     * 
     * @param ID the index ID indicating what section the Java Help dialop should display.
     */
    static HelpSet hs;

    /** DOCUMENT ME! */
    static HelpBroker helpBroker;

    private static boolean isEyeTrackingEnabled = false;

    /** eye tracking outstream writer . */
    private static BufferedWriter eyetrackingOutStream;

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public static void buildCursors() {

        // build custom mouse pointer cursors
        Image cursorImage = null;

        // magnify region cursor : TRY to get the image cursor.
        // if you can't (and coders may have EXTRA probs with this!)
        // then notify the user of the prob and get a default.
        // This try must be seperate for all diff cursors
        try { // this try does not work... sun didn't bother to propogate the exception ... maybe someday ...

            // img = Toolkit.getDefaultToolkit().getImage(PlaceHolder.class.getResource("emptycursor.gif"));
            cursorImage = MipavUtil.getIconImage("emptycursor.gif");
            MipavUtil.magRegionCursor = Toolkit.getDefaultToolkit().createCustomCursor(cursorImage, new Point(12, 12), "Magnification");
            MipavUtil.blankCursor = Toolkit.getDefaultToolkit().createCustomCursor(cursorImage, new Point(12, 12), "Blank Cursor");
            // System.err.println("created blank cursor");
        } catch (final FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() + ">.  Check that this file is available.\n", Preferences.DEBUG_FILEIO);
            MipavUtil.magRegionCursor = MipavUtil.crosshairCursor;
            MipavUtil.blankCursor = MipavUtil.crosshairCursor;
        }

        // small pointer cursor : TRY to get the image cursor. if you can't (and coders may have EXTRA probs with
        // this!) then notify the user of the prob and get a default. This try must be seperate for all diff cursors
        try { // this try does not work... sun didn't bother to propogate the exception ... maybe someday ...
            cursorImage = MipavUtil.getIconImage("smpointercursor.gif");
            MipavUtil.smallPointerCursor = Toolkit.getDefaultToolkit().createCustomCursor(cursorImage, new Point(0, 0), "SmallPointer");
        } catch (final FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() + ">.  Check that this file is available.\n", Preferences.DEBUG_FILEIO);
            MipavUtil.smallPointerCursor = MipavUtil.pointCursor;
        }

        try { // this try does not work... sun didn't bother to propogate the exception ... maybe someday ...
            cursorImage = MipavUtil.getIconImage("qkwinlevel.gif");

            MipavUtil.winLevelCursor = Toolkit.getDefaultToolkit().createCustomCursor(cursorImage, new Point(12, 12), "WinLevel");
        } catch (final FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() + ">.  Check that this file is available.\n", Preferences.DEBUG_FILEIO);
            MipavUtil.winLevelCursor = MipavUtil.crosshairCursor;
        }

        try {
            cursorImage = MipavUtil.getIconImage("probepoint.gif");

            MipavUtil.probeCursor = Toolkit.getDefaultToolkit().createCustomCursor(cursorImage, new Point(15, 15), "Probe");
        } catch (final FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() + ">.  Check that this file is available.\n", Preferences.DEBUG_FILEIO);
            MipavUtil.probeCursor = MipavUtil.crosshairCursor;
        }

        try {
            final Toolkit toolkit = Toolkit.getDefaultToolkit();
            MipavUtil.magnifyCursor = toolkit.createCustomCursor(MipavUtil.getIcon("zoomin.gif").getImage(), new Point(10, 10), "zoomin");
            MipavUtil.unmagnifyCursor = toolkit.createCustomCursor(MipavUtil.getIcon("zoomout.gif").getImage(), new Point(10, 10), "zoomout");
        } catch (final Exception error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() + ">.  Check that this file is available.\n", Preferences.DEBUG_FILEIO);
            MipavUtil.magnifyCursor = MipavUtil.crosshairCursor;
        }

    }

    /**
     * This should only be called once when MIPAV starts, and then if the user changes the font options through Program
     * Options to rebuild the fonts used in GUI building.
     */
    public static void buildDefaultFonts() {
        // only necessary when changing default fonts

        String fontName = "Serif";

        if (Preferences.getProperty(Preferences.PREF_MENU_FONT) != null) {
            fontName = Preferences.getProperty(Preferences.PREF_MENU_FONT);
        }

        int size = 12;

        if (Preferences.getProperty(Preferences.PREF_MENU_FONT_SIZE) != null) {
            size = Integer.parseInt(Preferences.getProperty(Preferences.PREF_MENU_FONT_SIZE));
        }

        MipavUtil.defaultMenuFont = new Font(fontName, Font.BOLD, size);
        MipavUtil.defaultAcceleratorFont = new Font(fontName, Font.PLAIN, size - 3);

        MipavUtil.font10 = new Font(fontName, Font.PLAIN, size - 2);
        MipavUtil.font12 = new Font(fontName, Font.PLAIN, size);
        MipavUtil.font12B = new Font(fontName, Font.BOLD, size);

        MipavUtil.font12I = new Font(fontName, Font.ITALIC, size);
        MipavUtil.font13B = new Font(fontName, Font.BOLD, size + 1);
        MipavUtil.font14 = new Font(fontName, Font.PLAIN, size + 2);
        MipavUtil.font14B = new Font(fontName, Font.BOLD, size + 2);
        MipavUtil.font16B = new Font(fontName, Font.BOLD, size + 4);
        MipavUtil.font18B = new Font(fontName, Font.BOLD, size + 6);

        // build one "dummy" menu item to get the correct Y padding with the font
        final JMenuItem dummy = ViewMenuBuilder.buildMenuItem("dummy", "dummy", 0, null, "save.gif", true);
        MipavUtil.MENU_Y_PADDING = dummy.getPreferredSize().height;
    }

    /**
     * Builds a titled border with the given title, an etched border, and the proper font and color.
     * 
     * @param title Title of the border
     * 
     * @return The titled border.
     */
    public static final TitledBorder buildTitledBorder(final String title) {
        return new TitledBorder(new EtchedBorder(), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
    }

    /**
     * Sets the location of the dialog to the center of the parent component.
     * 
     * @param parentComponent the parent component.
     * @param dialog the dialog which is to be displayed.
     */
    public static void centerInComponent(final Component parentComponent, final JDialog dialog) {
        final Point loc = parentComponent.getLocationOnScreen();
        final Dimension dim = parentComponent.getSize();

        final Dimension dim2 = dialog.getSize();

        final int x = loc.x + ((int) (dim.getWidth() - dim2.getWidth()) / 2);
        final int y = loc.y + ((int) (dim.getHeight() - dim2.getHeight()) / 2);
        dialog.setLocation(x, y);

    }

    /**
     * Sets the location of the window to the center of the parent window.
     * 
     * @param parentWindow the window where the child will be centered on.
     * @param childWindow the window that is to be displayed centered on the parent window
     */
    public static void centerInWindow(final Window parentWindow, final Window childWindow) {
        final Point parentTopLeftPoint = parentWindow.getLocationOnScreen();
        final Dimension parentSize = parentWindow.getSize();

        childWindow.setLocation( (parentTopLeftPoint.x + (parentSize.width / 2)) - (childWindow.getSize().width / 2),
                (parentTopLeftPoint.y + (parentSize.height / 2)) - (childWindow.getSize().height / 2));
    }

    /**
     * Sets the location of the window to the center of the screen.
     * 
     * @param window Window that is to be displayed
     */
    public static void centerOnScreen(final Window window) {
        final Toolkit toolkit = Toolkit.getDefaultToolkit();

        final Dimension screenSize = toolkit.getScreenSize();

        int x_location = 0;
        int y_location = 0;

        if (screenSize.getSize().width > window.getSize().width) {
            x_location = (screenSize.width / 2) - (window.getSize().width / 2);
        }

        if (screenSize.getSize().height > window.getSize().height) {
            y_location = (screenSize.height / 2) - (window.getSize().height / 2);
        }

        window.setLocation(x_location, y_location);
    }

    /**
     * Convert from a filename to a file URL.
     * 
     * @param filename the file name to convert
     * 
     * @return the url for the file in string form
     * 
     * @throws MalformedURLException if there is a problem converting the file name
     */
    public static final String convertToFileURL(final String filename) throws MalformedURLException {
        final String path = new File(filename).toURI().toURL().toString();

        return path;
    }

    /**
     * Creates a label in the proper font and color.
     * 
     * @param title The title of the label.
     * 
     * @return The new label.
     */
    public static final JLabel createSliderLabel(final String title) {
        final JLabel label = new JLabel(title);
        label.setFont(MipavUtil.font12);
        label.setForeground(Color.black);

        return label;
    }

    /**
     * Pops up a message dialog to display an error.
     * 
     * <p>
     * Use when an operation has failed, preventing some operation critical for MIPAV to continue running normally or an
     * operation can neither be completed nor its errors accomodated.
     * </p>
     * 
     * @param error the message text of the error
     * 
     * @see JOptionPane#showMessageDialog(java.awt.Component, java.lang.Object, java.lang.String, int, javax.swing.Icon)
     */
    public static void displayError(final String error) {
        if ( !MipavUtil.forceQuiet && !GraphicsEnvironment.isHeadless()) {
            if ( (ViewUserInterface.getReference() != null)
                    && (ViewUserInterface.getReference().isAppFrameVisible() || ViewUserInterface.getReference().isPlugInFrameVisible())) {

                try {
                    JOptionPane.getRootFrame().setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
                    JOptionPane.showMessageDialog(null, error, "Error", JOptionPane.ERROR_MESSAGE);
                } catch (final FileNotFoundException ex) {
                    Preferences.debug("Exception ocurred while getting <" + ex.getMessage() + ">.  Check that this file is available.\n",
                            Preferences.DEBUG_FILEIO);
                    System.err.println("Exception ocurred while getting <" + ex.getMessage() + ">.  Check that this file is available.\n");
                }
            } else {

                if (ViewUserInterface.getReference() != null) {

                    // the user has enabled the -hide option
                    System.err.println("Error: " + error);

                    // exit with an abnormal return value to indicate a problem
                    // (useful when running mipav from the command line and testing
                    // whether the algorithm completed successfully)
                    if (ViewUserInterface.getReference().doExitCmdLineOnError()) {
                        System.exit(1);
                    }
                } else {

                    try {

                        // an error occurred before the ui could be set up, so assume that the user is not using -hide..
                        JOptionPane.getRootFrame().setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
                        JOptionPane.showMessageDialog(null, error, "Error", JOptionPane.ERROR_MESSAGE);
                    } catch (final FileNotFoundException ex) {
                        Preferences.debug("Exception ocurred while getting <" + ex.getMessage() + ">.  Check that this file is available.\n",
                                Preferences.DEBUG_FILEIO);
                        System.err.println("Exception ocurred while getting <" + ex.getMessage() + ">.  Check that this file is available.\n");
                    }
                }
            }
        } else {
            System.err.println("Error: " + error);
            System.exit(1);
        }

    }

    /**
     * Pops up a message dialog to display information.
     * 
     * <p>
     * Use for brief notices such as completion of an operation or where to find logging information. It is not meant to
     * notify a user of an error in processing.
     * </p>
     * 
     * @param info the information string
     */
    public static void displayInfo(final String info) {
        if ( !MipavUtil.forceQuiet && !GraphicsEnvironment.isHeadless()) {
            if ( (ViewUserInterface.getReference() != null)
                    && (ViewUserInterface.getReference().isAppFrameVisible() || ViewUserInterface.getReference().isPlugInFrameVisible())) {

                try {
                    JOptionPane.getRootFrame().setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
                    JOptionPane.showMessageDialog(null, info, "Information", JOptionPane.INFORMATION_MESSAGE);
                } catch (final FileNotFoundException ex) {
                    Preferences.debug("Exception ocurred while getting <" + ex.getMessage() + ">.  Check that this file is available.\n",
                            Preferences.DEBUG_FILEIO);
                    System.err.println("Exception ocurred while getting <" + ex.getMessage() + ">.  Check that this file is available.\n");
                }
            } else {

                if (ViewUserInterface.getReference() != null) {

                    // the user has enabled the -hide option
                    System.err.println("Info: " + info);
                } else {

                    try {

                        // an error occurred before the ui could be set up, so assume that the user is not using -hide..
                        JOptionPane.getRootFrame().setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
                        JOptionPane.showMessageDialog(null, info, "Information", JOptionPane.INFORMATION_MESSAGE);
                    } catch (final FileNotFoundException ex) {
                        Preferences.debug("Exception ocurred while getting <" + ex.getMessage() + ">.  Check that this file is available.\n",
                                Preferences.DEBUG_FILEIO);
                        System.err.println("Exception ocurred while getting <" + ex.getMessage() + ">.  Check that this file is available.\n");
                    }
                }
            }
        } else {
            System.err.println("Info: " + info);
        }
    }

    /**
     * Pops up a message dialog to display a warning.
     * 
     * <p>
     * Use when an operation has failed, but the can be completed but the output may display inaccuratly; ie., errors
     * can be accomodated.
     * </p>
     * 
     * @param warning the message text of the warning.
     */
    public static void displayWarning(final String warning) {
        if ( !MipavUtil.forceQuiet && !GraphicsEnvironment.isHeadless()) {
            if ( (ViewUserInterface.getReference() != null)
                    && (ViewUserInterface.getReference().isAppFrameVisible() || ViewUserInterface.getReference().isPlugInFrameVisible())) {

                try {
                    JOptionPane.getRootFrame().setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
                    JOptionPane.showMessageDialog(null, warning, "Warning", JOptionPane.WARNING_MESSAGE);
                } catch (final FileNotFoundException ex) {
                    Preferences.debug("Exception ocurred while getting <" + ex.getMessage() + ">.  Check that this file is available.\n",
                            Preferences.DEBUG_FILEIO);
                    System.err.println("Exception ocurred while getting <" + ex.getMessage() + ">.  Check that this file is available.\n");
                }
            } else {

                if (ViewUserInterface.getReference() != null) {

                    // the user has enabled the -hide option
                    System.err.println("Warning: " + warning);
                } else {

                    try {

                        // an error occurred before the ui could be set up, so assume that the user is not using -hide..
                        JOptionPane.getRootFrame().setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
                        JOptionPane.showMessageDialog(null, warning, "Warning", JOptionPane.WARNING_MESSAGE);
                    } catch (final FileNotFoundException ex) {
                        Preferences.debug("Exception ocurred while getting <" + ex.getMessage() + ">.  Check that this file is available.\n",
                                Preferences.DEBUG_FILEIO);
                        System.err.println("Exception ocurred while getting <" + ex.getMessage() + ">.  Check that this file is available.\n");
                    }
                }
            }
        } else {
            System.err.println("Warning: " + warning);
        }
    }

    /**
     * Makes a series of ints, corresponding to a color string stored in the Mipav.preferences file which looks like a
     * color string defined in web pages ("RRGGBB"). and returns a java.awt.Color based on those values.
     * 
     * <p>
     * Call with extractColor(Preferences.getProperty("OneOfThoseColors"));
     * </p>
     * 
     * <p>
     * if preferencesColorString is null, or incomplete, returns black.
     * </p>
     * 
     * @return java.awt.Color
     * 
     * @see java.awt.Color
     * 
     * @param preferencesColorString -- this class pre-arranges the colors to be
     */
    public static Color extractColor(final String preferencesColorString) {
        final int[] RGB = new int[3];
        Color prefColor;

        try {

            if (preferencesColorString != null) {

                // so long as the string is available, pull out expected color string.
                RGB[0] = Integer
                        .parseInt(String.valueOf(preferencesColorString.toCharArray()[0]) + String.valueOf(preferencesColorString.toCharArray()[1]), 16);
                RGB[1] = Integer
                        .parseInt(String.valueOf(preferencesColorString.toCharArray()[2]) + String.valueOf(preferencesColorString.toCharArray()[3]), 16);
                RGB[2] = Integer
                        .parseInt(String.valueOf(preferencesColorString.toCharArray()[4]) + String.valueOf(preferencesColorString.toCharArray()[5]), 16);
            }
        } catch (final Exception npe) {
            Preferences.debug("MipavOptions: Color string was improper.  String was '" + preferencesColorString + "'", Preferences.DEBUG_FILEIO);

            // reset all three values with
            RGB[0] = 0;
            RGB[1] = 0;
            RGB[2] = 0;
        } finally {
            prefColor = new Color(RGB[0], RGB[1], RGB[2]);
        }

        return prefColor;
    }

    /**
     * Get a boolean value from a string tokenizer.
     * 
     * @param st the tokenizer to get the boolean from
     * 
     * @throws TokenizerException thrown if the string retrieved is not a boolean value
     * 
     * @return true if the token equals "true" without regard to case
     */
    public static final boolean getBoolean(final StringTokenizer st) throws TokenizerException {
        final String str = st.nextToken();

        // should either be a variation of true or false; otherwise we want to know that something is wrong
        if ( !str.equalsIgnoreCase("true") && !str.equalsIgnoreCase("false")) {
            throw new TokenizerException("Unable to parse boolean value: " + str);
        }

        // returns true if str.equalsIgnoreCase( "true" )
        return Boolean.parseBoolean(str);
    }

    /**
     * Get a float value from a string tokenizer.
     * 
     * @param st the tokenizer to get the float from
     * 
     * @throws TokenizerException thrown if the string retrieved is not a float value
     * 
     * @return a float value
     */
    public static final float getFloat(final StringTokenizer st) throws TokenizerException {
        final String str = st.nextToken();

        try {
            return Float.parseFloat(str);
        } catch (final NumberFormatException nfe) {
            throw new TokenizerException("Unable to parse float value: " + str);
        }
    }

    /**
     * Get a double value from a string tokenizer.
     * 
     * @param st the tokenizer to get the double from
     * 
     * @throws TokenizerException thrown if the string retrieved is not a double value
     * 
     * @return a double value
     */
    public static final double getDouble(final StringTokenizer st) throws TokenizerException {
        final String str = st.nextToken();

        try {
            return Double.parseDouble(str);
        } catch (final NumberFormatException nfe) {
            throw new TokenizerException("Unable to parse double value: " + str);
        }
    }

    /**
     * Finds the icon of the specified name. Uses the PlaceHolder class, which is in the same directory as the icons, to
     * locate the icons.
     * 
     * @param name name of the icon
     * 
     * @return the icon
     */
    public static ImageIcon getIcon(final String name) {
        URL res;
        ImageIcon icon = null;

        res = PlaceHolder.class.getResource(name);

        if (res != null) {
            icon = new ImageIcon(res);
        } else {
            System.err.println("Unable to find icon: " + name);
        }

        return icon;
    }

    /**
     * Returns whether all error/warning/info dialogs should be suppressed.
     * 
     * @return true/false
     */
    public static boolean getForceQuiet() {
        return MipavUtil.forceQuiet;
    }

    /**
     * Finds the image of the specified name. Uses the PlaceHolder class, which is in the same directory as the icons
     * images, to locate the images.
     * 
     * @param name name of the image
     * 
     * @return the image
     * 
     * @throws FileNotFoundException if we can't find the icon file
     */
    public static Image getIconImage(final String name) throws FileNotFoundException {
        URL res;
        Image img = null;

        res = PlaceHolder.class.getResource(name);

        if (res != null) {
            img = new ImageIcon(res).getImage();
        } else {

            // System.err.println( "Unable to find image: " + name );
            throw new FileNotFoundException(name);
        }

        return img;
    }

    /**
     * Get an integer value from a string tokenizer.
     * 
     * @param st the tokenizer to get the integer from
     * 
     * @throws TokenizerException thrown if the string retrieved is not an integer value
     * 
     * @return a ninteger value
     */
    public static final int getInt(final StringTokenizer st) throws TokenizerException {
        final String str = st.nextToken();

        try {
            return Integer.parseInt(str);
        } catch (final NumberFormatException nfe) {
            throw new TokenizerException("Unable to parse integer value: " + str);
        }
    }

    /**
     * Gets the MIPAV Version number from the about.txt file.
     * 
     * @return String version number (in string format)
     */
    public static String getVersion() {

        // if we've already read in the version from about.txt, then don't do it again..
        if (MipavUtil.version != null) {
            return MipavUtil.version;
        }

        String verString = new String("");

        final String filename = "about.txt";

        BufferedReader input = null;

        try {

            // use this long call instead of ClassLoader.getSystemResource() to work properly from a jnlp launch
            final URL fileURL = Thread.currentThread().getContextClassLoader().getResource(filename);

            if (fileURL == null) {
                Preferences.debug("Unable to open " + filename + ".  Make sure it is in the same directory as MipavMain.class\n", Preferences.DEBUG_MINOR);

                return verString;
            }

            // use buffering
            // this implementation reads one line at a time
            input = new BufferedReader(new InputStreamReader(fileURL.openStream()));

            String line = null; // not declared within while loop

            while ( (line = input.readLine()) != null) {

                if (line.startsWith("Version:")) {

                    // remove "Version:" and the two tabs
                    line = line.substring(10).trim();
                    MipavUtil.version = line.substring(0, line.indexOf(" "));
                    verString = MipavUtil.version;

                    break;
                }

            }
        } catch (final Exception ex) {} finally {

            try {

                if (input != null) {
                    input.close();
                    input = null;
                }
            } catch (final IOException closee) {}
        }

        return verString;
    }

    /**
     * Determines if a value is within the given pixel intensity range.
     * 
     * @param min minimum intensity.
     * @param max maximum intensity.
     * @param num value to test.
     * @param rangeFlag (no test, between, outside) the min and max.
     * @return true if num satisfies the test.
     */
    public static boolean inRange(final float min, final float max, final float num, final RangeType rangeFlag) {
        if (rangeFlag == null) {
            return false;
        }

        switch (rangeFlag) {
            case BETWEEN:
                if ( (num >= min) && (num <= max)) {
                    return true; // pixel is within min to max
                }
                return false;

            case OUTSIDE:
                if ( (num < min) || (num > max)) {
                    return true; // pixel is either below min or above max
                }
                return false;

            case NO_RANGE:
            default:
                return false; // no range for pixel to be in

        }
    }

    /**
     * Loads a dynamic library of the given name and path and places it in the Java temp directory.
     */
    public static void loadDynamicLib(final String path, String name) {
        final String is64 = System.getProperty("sun.arch.data.model");
        final String osName = System.getProperty("os.name").toLowerCase();
        name = new String(name);
        if (is64.equals("64") && osName.startsWith("windows")) {
            name = name + "_64.dll";
        } else if (osName.startsWith("windows")) {
            name = name + ".dll";
        } else if (osName.contains("mac")) {
            name = name + "-osx.jnilib";
        } else if (is64.equals("64")) { // "must" be linux
            name = name + "-linux64.so";
        } else {
            name = name + "-linux.so";
        }

        InputStream source = null;
        FileOutputStream destination = null;

        try {
            try {
                final byte[] buffer = new byte[102400];
                URL url = MipavJarLoader.class.getResource(path + name);
                if (url == null && is64.equals("64") && osName.startsWith("windows")) {
                    name = name.substring(0, name.indexOf("_64")) + ".dll";
                    url = MipavJarLoader.class.getResource(path + name);
                }
                source = MipavJarLoader.class.getResourceAsStream(path + name);
                final File fileOut = new File(Preferences.getPreferencesDir() + File.separator + "plugins" + File.separator + path + name);
                if ( !fileOut.getParentFile().exists()) {
                    final boolean success = fileOut.getParentFile().mkdirs();
                    if ( !success) {
                        throw new Exception("Unable to create plugins directory");
                    }
                }
                // running a second time causes the libs to be written out from themselves to themselves (which results
                // in a 0 byte file)
                // so only write out the lib if it's not already there
                if ( !fileOut.exists()) {
                    System.out.println("Writing file to: " + fileOut);
                    destination = new FileOutputStream(fileOut);
                    int len = 0;
                    while ( (len = source.read(buffer)) != -1) {
                        destination.write(buffer, 0, len);
                    }
                    destination.flush();
                    destination.close(); // for file locks
                }

                System.load(fileOut.toString());
            } catch (final Exception e) {
                e.printStackTrace();
                Preferences.debug("Failed to load library: " + name, Preferences.DEBUG_MINOR);
            } finally {
                if (source != null) {
                    source.close();
                }
                if (destination != null) {
                    destination.close();
                }
            }
        } catch (final Exception e) {
            Preferences.debug("Failed to load library: " + name, Preferences.DEBUG_MINOR);
        } catch (final Error e) {
            Preferences.debug("Failed to load library: " + name, Preferences.DEBUG_MINOR);
        }
    }

    /**
     * Takes a java.awt.Color and forms a string representing its color. the string appears as 6 hex digits and looks
     * like the color coding used in html files, as in: "RRGGBB".
     * 
     * @param aColor the color to be converted to a hexidecimal
     * 
     * @return hexa-decimal string representing the 8-bit values of an RGB color, in the form of "RRGGBB".
     */
    public static String makeColorString(final Color aColor) {
        final String[] rgbString = new String[3];
        int n;

        rgbString[0] = Integer.toString(aColor.getRed(), 16);
        rgbString[1] = Integer.toString(aColor.getGreen(), 16);
        rgbString[2] = Integer.toString(aColor.getBlue(), 16);

        for (n = 0; n < 3; n++) {

            if ( (rgbString[n]).length() == 1) {
                rgbString[n] = "0" + rgbString[n];
            }
        }

        return (rgbString[0] + rgbString[1] + rgbString[2]);
    }

    /**
     * Makes a string of a float with a specific number of decimal points.
     * 
     * @param number number to be converted to a string
     * @param decPts the number of decimal points
     * 
     * @return string representation of the number
     */
    public static final String makeFloatString(final float number, final int decPts) {
        String str = null;

        try {
            str = String.valueOf(number);
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("MipavUtil.makeString: out of memory");

            return null;
        }

        final int index = str.indexOf(".");
        final int indexE = str.indexOf("E");
        final int length = str.length();

        if ( ( (index + decPts) < length) && (indexE == -1)) {
            str = str.substring(0, index + decPts + 1);
        } else if (indexE != -1) {
            if ( (indexE - index) > decPts) {
                str = str.substring(0, index + decPts + 1) + str.substring(indexE);
            }
        }

        return str;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param color Color
     * @param name String
     * @param size int
     * @param style int
     * @param doU boolean
     * @param text String
     * 
     * @return String
     */
    public static final String makeHTMLFontString(final Color color, final String name, final int size, final int style, final boolean doU, final String text) {

        String colorStr = "#";

        if (Integer.toHexString(color.getRed()).length() == 1) {
            colorStr += Integer.toHexString(color.getRed()) + "0";
        } else {
            colorStr += Integer.toHexString(color.getRed());
        }

        if (Integer.toHexString(color.getGreen()).length() == 1) {
            colorStr += Integer.toHexString(color.getGreen()) + "0";
        } else {
            colorStr += Integer.toHexString(color.getGreen());
        }

        if (Integer.toHexString(color.getBlue()).length() == 1) {
            colorStr += Integer.toHexString(color.getBlue()) + "0";
        } else {
            colorStr += Integer.toHexString(color.getBlue());
        }

        String fontString = "<font " + "face=\"" + name + "\" " + "size=\"" + size + "\" " + "color=\"" + colorStr + "\">";

        if ( (style & Font.BOLD) != 0) {
            fontString += "<b>";
        }

        if ( (style & Font.ITALIC) != 0) {
            fontString += "<i>";
        }

        if (doU) {
            fontString += "<u>";
        }

        fontString += text;

        if (doU) {
            fontString += "</u>";
        }

        if ( (style & Font.ITALIC) != 0) {
            fontString += "</i>";
        }

        if ( (style & Font.BOLD) != 0) {
            fontString += "</b>";
        }

        fontString += "</font>";

        // System.err.println(fontString);

        return fontString;
    }

    /**
     * Takes a text field and forces the text field to accept numbers, backspace and delete-key entries.
     * 
     * @param txt Text field to modify.
     * @param allowFloatingPoint <code>true</code> will force the text field to also allow the use of the '.' key to
     *            permit entering floating point numbers.
     */
    public static void makeNumericsOnly(final JTextField txt, final boolean allowFloatingPoint) {
        MipavUtil.makeNumericsOnly(txt, allowFloatingPoint, true);
    }

    /**
     * Takes a txt field, and forces the textfield to accept numbers, backspace and delete-key entries. Arguments to the
     * method can permit the text field to also allow the entry to be negative or give it a floating point decimal
     * value. All other characters are ignored, so if further action is required, it will need to be custom-assigned by
     * the calling object.
     * 
     * @param txt the text field to make only accept numeric text
     * @param allowFloatingPoint true will forces the text field to also allow the use of the '.' key to permit entering
     *            floating point numbers
     * @param allowNegativeNumbers true causes textfield to allow the entry of a single minus sign ('-') in front of the
     *            text. An additional '-' removes minus sign from the field to give it the opposite sign.
     */
    public static void makeNumericsOnly(final JTextField txt, final boolean allowFloatingPoint, final boolean allowNegativeNumbers) {

        if (allowFloatingPoint && allowNegativeNumbers) {
            txt.addKeyListener(new KeyAdapter() { // make the field
                @Override
                public void keyTyped(final KeyEvent evt) { // not accept letters

                    final JTextField t = (JTextField) evt.getComponent();
                    final char ch = evt.getKeyChar();

                    if (ch == '.') {

                        if (t.getSelectedText() != null) {

                            if (t.getText().length() == t.getSelectedText().length()) {
                                t.setText("0.");
                                evt.consume();
                            } else if ( (t.getText().indexOf('.') != -1) && (t.getSelectedText().indexOf('.') == -1)) { // there
                                                                                                                        // is
                                                                                                                        // a
                                                                                                                        // '.',
                                                                                                                        // but
                                                                                                                        // not
                                // in the selected text
                                evt.consume(); // ignore
                            }
                        } else if (t.getText().indexOf('.') != -1) {
                            evt.consume();
                        } else if (t.getText().length() == 0) {
                            t.setText("0.");
                            evt.consume();
                        } else {
                            final StringBuffer sb = new StringBuffer(t.getText());
                            t.setText(sb.insert(t.getCaretPosition(), ".").toString());
                            evt.consume();
                        }
                    } else if (ch == '-') {
                        String text = t.getText().trim();
                        final int minusPlace = text.indexOf('-');

                        if (minusPlace != -1) { // text does has a '-'
                            text = text.substring(minusPlace + 1);

                            // put minus in front of text after '-'...
                            // t.setText(text);
                            t.setText("-");
                        } else {
                            t.setText("-" + text);
                        }

                        evt.consume();
                    } else if ( ( (ch < '0') || (ch > '9')) && (ch != KeyEvent.VK_DELETE) && (ch != KeyEvent.VK_BACK_SPACE)) {

                        // if is the case that ch is outside the bounds of a
                        // number AND it is the case that ch is neither a BS
                        // or a DE, then...
                        // key is not a digit or a deletion char
                        evt.consume();
                    }
                }
            });
        } else if (allowFloatingPoint && !allowNegativeNumbers) {
            txt.addKeyListener(new KeyAdapter() { // make the field
                @Override
                public void keyTyped(final KeyEvent evt) { // not accept letters

                    final JTextField t = (JTextField) evt.getComponent();
                    final char ch = evt.getKeyChar();

                    if (ch == '.') {

                        if (t.getSelectedText() != null) {

                            if (t.getText().length() == t.getSelectedText().length()) {
                                t.setText("0.");
                                evt.consume(); // ignore the rest
                            } else if ( (t.getText().indexOf('.') != -1) && (t.getSelectedText().indexOf('.') == -1)) { // there
                                                                                                                        // is
                                                                                                                        // a
                                                                                                                        // '.',
                                                                                                                        // but
                                                                                                                        // not
                                // in the selected text
                                evt.consume(); // ignore
                            }
                        } else if (t.getText().indexOf('.') != -1) {
                            evt.consume();
                        } else if (t.getText().length() == 0) {
                            t.setText("0.");
                            evt.consume(); // ignore the rest
                        } else {
                            final StringBuffer sb = new StringBuffer(t.getText());
                            t.setText(sb.insert(t.getCaretPosition(), ".").toString());
                            evt.consume(); // ignore the rest
                        }
                    }
                    /* else if (ch == '-') {...} */
                    // negatives are not allowed
                    else if ( ( (ch < '0') || (ch > '9')) && (ch != KeyEvent.VK_DELETE) && (ch != KeyEvent.VK_BACK_SPACE)) {

                        // if is the case that ch is outside the bounds of a
                        // number AND it is the case that ch is neither a BS
                        // or a DE, then...key is not a digit or a deletion char
                        evt.consume();
                    }
                }
            });
        } else if ( !allowFloatingPoint && allowNegativeNumbers) {
            txt.addKeyListener(new KeyAdapter() { // make the field
                @Override
                public void keyTyped(final KeyEvent evt) { // not accept letters

                    final JTextField t = (JTextField) evt.getComponent();
                    final char ch = evt.getKeyChar();

                    /* else if (ch == '.') {..} */
                    // decimal not allowed
                    if (ch == '-') {
                        String text = t.getText().trim();
                        final int minusPlace = text.indexOf('-');

                        if (minusPlace != -1) { // text does has a '-'
                            text = text.substring(minusPlace + 1); // only text after '-'...
                            t.setText(text);
                        } else {
                            t.setText("-" + text);
                        }

                        evt.consume();
                    } else if ( ( (ch < '0') || (ch > '9')) && (ch != KeyEvent.VK_DELETE) && (ch != KeyEvent.VK_BACK_SPACE)) {

                        // if is the case that ch is outside the bounds of a
                        // number AND it is the case that ch is neither a BS
                        // or a DE, then...key is not a digit or a deletion char
                        evt.consume();
                    }
                }
            });
        } else { // if (!allowFloatingPoint && !allowNegativeNumbers) {
            txt.addKeyListener(new KeyAdapter() { // make the field
                @Override
                public void keyTyped(final KeyEvent evt) { // not accept letters

                    final char ch = evt.getKeyChar();

                    /* else if (ch == '.') {...} */
                    // floating point not allowed
                    /* else if (ch == '-') {...} */
                    // negatives are not allowed
                    if ( ( (ch < '0') || (ch > '9')) && (ch != KeyEvent.VK_DELETE) && (ch != KeyEvent.VK_BACK_SPACE)) {

                        // if is the case that ch is outside the bounds of a
                        // number AND it is the case that ch is neither a BS
                        // nor a DE, then...
                        // key is not a digit or a deletion char
                        evt.consume();
                    }
                }
            });
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param container DOCUMENT ME!
     * @param enabled DOCUMENT ME!
     */
    public static final void setComponentsEnabled(final Container container, final boolean enabled) {
        final Component[] comps = container.getComponents();

        for (final Component element : comps) {
            element.setEnabled(enabled);
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param comp DOCUMENT ME!
     */
    public static final void setFonts(final Component[] comp) {

        for (final Component element : comp) {

            if (element instanceof Container) {
                MipavUtil.setFonts( ((Container) element).getComponents());
            }

            try {
                element.setFont(MipavUtil.defaultMenuFont);
            } catch (final Exception e) {} // do nothing
        }
    }

    /**
     * Sets whether all MIPAV error/warning/info messages should be suppressed (used to skip vm heap max size/plist warning on
     * startup).
     * 
     * @param force Should mipav suppress all errors
     */
    public static final void setForceQuiet(final boolean force) {
        MipavUtil.forceQuiet = force;
    }

    /**
     * Pops up the MIPAV help for a given wiki help page.
     * 
     * @param wikiPage The name of the wiki help topic to open.
     */
    public static void showWebHelp(final String wikiPage) {
        final String wikiBase = "https://mipav.cit.nih.gov/pubwiki/index.php/";
        openURLInBrowser(wikiBase + wikiPage);
    }
    
    /**
     * Opens a URL in the user's default browser
     * 
     * @param URLString The full URL of the web page to open.
     */
    public static void openURLInBrowser(final String URLString) {
        try {
            final URI wikiURI = new URI(URLString);
            Desktop.getDesktop().browse(wikiURI);
        } catch (final URISyntaxException e) {
            e.printStackTrace();
            MipavUtil.displayError("Web page URL syntax error: " + URLString);
            return;
        } catch (final IOException e) {
            e.printStackTrace();
            MipavUtil.displayError("Error opening web page in browser: " + URLString);
            return;
        }
        
        // old method of opening something from the web that was replicated in various locations
//        final String osName = System.getProperty("os.name");
//        try {
//            if (osName.startsWith("Mac OS")) {
//                final Class fileMgr = Class.forName("com.apple.eio.FileManager");
//                final Method openURL = fileMgr.getDeclaredMethod("openURL", new Class[] {String.class});
//                openURL.invoke(null, new Object[] {url});
//            } else if (osName.startsWith("Windows")) {
//                Runtime.getRuntime().exec("rundll32 url.dll,FileProtocolHandler " + url);
//            } else { // assume Unix or Linux
//                final String[] browsers = {"firefox", "opera", "konqueror", "epiphany", "mozilla", "netscape"};
//                String browser = null;
//                for (int count = 0; count < browsers.length && browser == null; count++) {
//                    if (Runtime.getRuntime().exec(new String[] {"which", browsers[count]}).waitFor() == 0) {
//                        browser = browsers[count];
//                    }
//                }
//                if (browser == null) {
//                    System.out.println("Can not find web browser");
//                } else {
//                    Runtime.getRuntime().exec(new String[] {browser, url});
//                }
//            }
//        } catch (final Exception e) {
//            System.out.println("Can not find web browser");
//        }
    }

    /**
     * Tests that the entered parameter is in range.
     * 
     * @param str the value entered by the user
     * @param minValue the minimum value this variable may be set to
     * @param maxValue the maximum value this variable may be set to
     * 
     * @return boolean result of test
     */
    public static final boolean testParameter(final String str, final double minValue, final double maxValue) {
        double tmp;

        try {
            tmp = Double.valueOf(str).doubleValue();

            if ( (tmp > maxValue) || (tmp < minValue)) {
                MipavUtil.displayError("Value is out of range: " + String.valueOf(minValue) + " , " + String.valueOf(maxValue));

                return false;
            } else {
                return true;
            }
        } catch (final NumberFormatException error) {
            MipavUtil.displayError("Must enter numeric value");

            return false;
        }
    }

    /**
     * Get an object containing information on the current memory usage.
     * 
     * @return Heap memory usage object.
     */
    public static MemoryUsage getHeapMemoryUsage() {
        return ManagementFactory.getMemoryMXBean().getHeapMemoryUsage();
    }

    /**
     * Return the amount of heap memory still available.
     * 
     * @return The amount of heap memory still available.
     */
    public static long getFreeHeapMemory() {
        return getMaxHeapMemory() - getUsedHeapMemory();
    }

    /**
     * Return the maximum amount of heap memory that MIPAV can use.
     * 
     * @return The maximum amount of heap memory available.
     */
    public static long getMaxHeapMemory() {
        return getHeapMemoryUsage().getMax();
    }

    /**
     * Return the amount of heap memory that MIPAV is currently using.
     * 
     * @return The amount of heap memory currently being used.
     */
    public static long getUsedHeapMemory() {
        return getHeapMemoryUsage().getUsed();
    }

    // ~ Inner Classes
    // --------------------------------------------------------------------------------------------------

    /**
     * Static abstract class similar to mouseAdapter to allow the catching of actionevents from components without
     * having to implement ActionListener.
     * 
     * @author linkb
     */
    public abstract static class ActionAdapter extends Object implements ActionListener {

        /**
         * Creates a new ActionAdapter object.
         */
        public ActionAdapter() {}

        /**
         * DOCUMENT ME!
         * 
         * @param ae DOCUMENT ME!
         */
        @Override
        public abstract void actionPerformed(ActionEvent ae);
    }

    /**
     * Check eye tracking is enabled or not.
     * 
     * @return
     */
    public static final boolean isEyeTrackingEnabled() {
        return isEyeTrackingEnabled;
    }

    /**
     * When the plug-in eye tracker record button is clicked, it re-initial the eye tracker csv file recording stream.
     * If the stop button is clicked, stop the current csv file recording stream.
     * 
     * @param enable enable flag
     * @param fileDir user selected csv file directory.
     */
    public static final void setEyeTrackingEnabled(final boolean enable, final String fileDir) {
        isEyeTrackingEnabled = enable;
        if (enable) {
            initEyeTrackingLogfile(fileDir);
        } else {
            closeEyeTrackingLogfile();
        }
    }

    /**
     * When the plug-in eye tracker record button is clicked, it re-initial the eye tracker csv file recording stream.
     * If the stop button is clicked, stop the current csv file recording stream.
     * 
     * @param enable enable flag
     * @param fileDir user selected csv file directory.
     */
    public static final void setEyeTrackingEnabled(final boolean enable, final String fileDir, final ViewJComponentEditImage imageComp) {
        isEyeTrackingEnabled = enable;
        if (enable) {
            initEyeTrackingLogfile(fileDir, imageComp);
        } else {
            closeEyeTrackingLogfile();
        }
    }

    /**
     * Initialize the file IO for eye tracking log file
     */
    private static void initEyeTrackingLogfile(final String defaultDirectory, final ViewJComponentEditImage imgComp) {
        try {
            // System.err.println(defaultDirectory);
            final File file = new File(defaultDirectory);
            if ( !file.exists()) {
                file.createNewFile();
            }
            eyetrackingOutStream = new BufferedWriter(new FileWriter(file.getAbsoluteFile()));

            final SimpleDateFormat sdfDate = new SimpleDateFormat("d MMM yyyy HH:mm:ss EEE");
            final Date now = new Date();
            final String strDate = sdfDate.format(now);
            final ModelImage activeImage = imgComp.getActiveImage();
            final String imageName = activeImage.getImageDirectory() + activeImage.getImageFileName();
            final int[] dim = activeImage.getExtents();
            // eyetrackingOutStream.write("Time, ActiveImage, ActiveSlice, frameMinX, frameMinY, frameMaxX, frameMaxY, Event, MouseEvent, Action, value, MouseCoordX, MouseCoordY\n");
            eyetrackingOutStream.write("ImageName:," + imageName + "\n");
            eyetrackingOutStream.write("Date:," + strDate + "\n");
            eyetrackingOutStream.write("ImageSize:," + dim[0] + "x" + dim[1] + "x" + dim[2] + "\n");
            eyetrackingOutStream.write("\n");
            eyetrackingOutStream.write("\n");
            eyetrackingOutStream.write("\n");
            eyetrackingOutStream
                    .write("Time, ImageActive, ActionName, SliceNumber, X_UpperLeft, Y_UpperLeft, X_UpperRight, Y_UpperRight, X_LowerLeft, Y_LowerLeft, X_LowerRight, Y_LowerRight, Window, Level, X_MouseClickLocation, Y_MouseClickLocation, Length, X_LineStart, Y_LineStart, X_LineEnd, Y_LineEnd"
                            + "\n");
        } catch (final IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Initialize the file IO for eye tracking log file
     */
    private static void initEyeTrackingLogfile(final String defaultDirectory) {
        try {
            // System.err.println(defaultDirectory);
            final File file = new File(defaultDirectory);
            if ( !file.exists()) {
                file.createNewFile();
            }
            eyetrackingOutStream = new BufferedWriter(new FileWriter(file.getAbsoluteFile()));
            // eyetrackingOutStream.write("Time, ActiveImage, ActiveSlice, frameMinX, frameMinY, frameMaxX, frameMaxY, Event, MouseEvent, Action, value, MouseCoordX, MouseCoordY\n");
        } catch (final IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Record the eye tracking log message.
     * 
     * @param msg
     */
    public static final void writeEyeTrackingLog(final String msg) {
        if (isEyeTrackingEnabled) {
            try {
                eyetrackingOutStream.write(msg + "\n");
                eyetrackingOutStream.flush();
            } catch (final IOException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Record the eye tracking log message.
     * 
     * @param msg
     */
    public static final void writeEyeTrackingLog(final String msg, final ViewJComponentEditImage imgComp) {
        if (isEyeTrackingEnabled) {
            try {
                eyetrackingOutStream.write(msg + "\n");
                eyetrackingOutStream.flush();
            } catch (final IOException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * close the eye tracking log.
     */
    private static void closeEyeTrackingLogfile() {
        try {
            eyetrackingOutStream.close();
        } catch (final IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Get the revision number from auto populated SVN revision string.
     * 
     * @param svnRevStr String with format $Rev: ##### $
     * @return Only the revision number from the given string.
     */
    public static final String getSVNRevisionNum(final String svnRevStr) {
        final Pattern p = Pattern.compile("\\$Rev\\:+\\s+(\\d+)\\s*\\#?\\$");
        final Matcher m = p.matcher(svnRevStr);
        if (m.find() && m.groupCount() > 0) {
            return m.group(1);
        } else {
            return "";
        }
    }

    /**
     * Get the date from auto populated SVN date string.
     * 
     * @param svnRevStr String with format resembling $Date: YYYY-MM-DD [...] $
     * @return Only the revision number from the given string.
     */
    public static final String getSVNChangedDate(final String svnRevStr) {
        final Pattern p = Pattern.compile("\\$(?:Date|LastChangedDate)\\:+\\s+([\\d\\-]+)[\\s\\w,\\-:()]*\\#?\\$");
        final Matcher m = p.matcher(svnRevStr);
        if (m.find() && m.groupCount() > 0) {
            return m.group(1);
        } else {
            return "";
        }
    }
    
    /**
     * Decode a URL string using UTF-8 character encoding.
     * @param urlStr The URL string to decode.
     * @return Decoded URL string or empty string if there was a decoding error.
     */
    public static final String decodeStr(String urlStr) {
        try {
            return URLDecoder.decode(urlStr, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
            return "";
        }
    }
}
