package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.icons.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.net.*;

import java.util.*;
import java.util.jar.*;
import java.util.zip.*;

import javax.help.*;

import javax.swing.*;
import javax.swing.border.*;


/**
 * A collection of static methods and frequently used and useful constants are in this utility class.
 */
public class MipavUtil extends JComponent {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4180127127881019813L;

    /** DOCUMENT ME! */
    public static Font defaultMenuFont = new Font("Serif", 0, 12);

    /** DOCUMENT ME! */
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
    public static Cursor quickLUTcursor = defaultCursor; // be sure to change when there is a decent image available.


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

    /** DOCUMENT ME! */
    public static final int[] functionKeys = new int[] {
                                                 0, KeyEvent.VK_F1, KeyEvent.VK_F2, KeyEvent.VK_F3, KeyEvent.VK_F4,
                                                 KeyEvent.VK_F5, KeyEvent.VK_F6, KeyEvent.VK_F7, KeyEvent.VK_F8,
                                                 KeyEvent.VK_F9, KeyEvent.VK_F10, KeyEvent.VK_F11, KeyEvent.VK_F12
                                             };

    /**
     * Displays the Java Help dialog indexed directly to the section identified by the ID passed in.
     *
     * @param  ID  the index ID indicating what section the Java Help dialop should display.
     */
    static HelpSet hs;

    /** DOCUMENT ME! */
    static HelpBroker helpBroker;

    //~ Methods --------------------------------------------------------------------------------------------------------


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
            cursorImage = getIconImage("emptycursor.gif");
            magRegionCursor = Toolkit.getDefaultToolkit().createCustomCursor(cursorImage, new Point(12, 12),
                                                                             "Magnification");
            blankCursor = Toolkit.getDefaultToolkit().createCustomCursor(cursorImage, new Point(12, 12),
                                                                         "Blank Cursor");
            // System.err.println("created blank cursor");
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            magRegionCursor = crosshairCursor;
            blankCursor = crosshairCursor;
        }

        // small pointer cursor : TRY to get the image cursor.  if you can't (and coders may have EXTRA probs with
        // this!) then notify the user of the prob and get a default.  This try must be seperate for all diff cursors
        try { // this try does not work... sun didn't bother to propogate the exception ... maybe someday ...
            cursorImage = getIconImage("smpointercursor.gif");
            smallPointerCursor = Toolkit.getDefaultToolkit().createCustomCursor(cursorImage, new Point(0, 0),
                                                                                "SmallPointer");
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            smallPointerCursor = pointCursor;
        }

        try { // this try does not work... sun didn't bother to propogate the exception ... maybe someday ...
            cursorImage = getIconImage("qkwinlevel.gif");

            winLevelCursor = Toolkit.getDefaultToolkit().createCustomCursor(cursorImage, new Point(12, 12), "WinLevel");
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            winLevelCursor = crosshairCursor;
        }

        try {
            cursorImage = getIconImage("probepoint.gif");

            probeCursor = Toolkit.getDefaultToolkit().createCustomCursor(cursorImage, new Point(15, 15), "Probe");
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            probeCursor = crosshairCursor;
        }

        try {
            Toolkit toolkit = Toolkit.getDefaultToolkit();
            magnifyCursor = toolkit.createCustomCursor(MipavUtil.getIcon("zoomin.gif").getImage(), new Point(10, 10),
                                                       "zoomin");
            unmagnifyCursor = toolkit.createCustomCursor(MipavUtil.getIcon("zoomout.gif").getImage(), new Point(10, 10),
                                                         "zoomout");
        } catch (Exception error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            magnifyCursor = crosshairCursor;
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

        defaultMenuFont = new Font(fontName, Font.BOLD, size);
        defaultAcceleratorFont = new Font(fontName, Font.PLAIN, size - 3);

        font10 = new Font(fontName, Font.PLAIN, size - 2);
        font12 = new Font(fontName, Font.PLAIN, size);
        font12B = new Font(fontName, Font.BOLD, size);

        font12I = new Font(fontName, Font.ITALIC, size);
        font13B = new Font(fontName, Font.BOLD, size + 1);
        font14 = new Font(fontName, Font.PLAIN, size + 2);
        font14B = new Font(fontName, Font.BOLD, size + 2);
        font16B = new Font(fontName, Font.BOLD, size + 4);
        font18B = new Font(fontName, Font.BOLD, size + 6);


        // build one "dummy" menu item to get the correct Y padding with the font
        JMenuItem dummy = ViewMenuBuilder.buildMenuItem("dummy", "dummy", 0, null, "save.gif", true);
        MENU_Y_PADDING = dummy.getPreferredSize().height;
    }

    /**
     * Builds a titled border with the given title, an etched border, and the proper font and color.
     *
     * @param   title  Title of the border
     *
     * @return  The titled border.
     */
    public static final TitledBorder buildTitledBorder(String title) {
        return new TitledBorder(new EtchedBorder(), title, TitledBorder.LEFT, TitledBorder.CENTER, font12B,
                                Color.black);
    }

    /**
     * Sets the location of the dialog to the center of the parent component.
     *
     * @param  parentComponent  the parent component.
     * @param  dialog           the dialog which is to be displayed.
     */
    public static void centerInComponent(Component parentComponent, JDialog dialog) {
        Point loc = parentComponent.getLocationOnScreen();
        Dimension dim = parentComponent.getSize();

        Dimension dim2 = dialog.getSize();

        int x = loc.x + ((int) (dim.getWidth() - dim2.getWidth()) / 2);
        int y = loc.y + ((int) (dim.getHeight() - dim2.getHeight()) / 2);
        dialog.setLocation(x, y);

    }

    /**
     * Sets the location of the window to the center of the parent window.
     *
     * @param  parentWindow  the window where the child will be centered on.
     * @param  childWindow   the window that is to be displayed centered on the parent window
     */
    public static void centerInWindow(Window parentWindow, Window childWindow) {
        Point parentTopLeftPoint = parentWindow.getLocationOnScreen();
        Dimension parentSize = parentWindow.getSize();

        childWindow.setLocation((parentTopLeftPoint.x + (parentSize.width / 2)) - (childWindow.getSize().width / 2),
                                (parentTopLeftPoint.y + (parentSize.height / 2)) - (childWindow.getSize().height / 2));
    }

    /**
     * Sets the location of the window to the center of the screen.
     *
     * @param  window  Window that is to be displayed
     */
    public static void centerOnScreen(Window window) {
        Toolkit toolkit = Toolkit.getDefaultToolkit();

        Dimension screenSize = toolkit.getScreenSize();

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
     * @param   filename  the file name to convert
     *
     * @return  the url for the file in string form
     *
     * @throws  MalformedURLException  if there is a problem converting the file name
     */
    public static final String convertToFileURL(String filename) throws MalformedURLException {
        String path = new File(filename).toURI().toURL().toString();

        return path;
    }

    /**
     * Pops up a message dialog to display an error.
     *
     * <p>Use when an operation has failed, preventing some operation critical for MIPAV to continue running normally or
     * an operation can neither be completed nor its errors accomodated.</p>
     *
     * @param  error  the message text of the error
     *
     * @see    JOptionPane#showMessageDialog(java.awt.Component, java.lang.Object, java.lang.String, int,
     *         javax.swing.Icon)
     */
    public static void displayError(String error) {

        if ((ViewUserInterface.getReference() != null) &&
                (ViewUserInterface.getReference().isAppFrameVisible() == true)) {

            try {
                JOptionPane.getRootFrame().setIconImage(getIconImage(Preferences.getIconName()));
                JOptionPane.showMessageDialog(null, error, "Error", JOptionPane.ERROR_MESSAGE);
            } catch (FileNotFoundException ex) {
                Preferences.debug("Exception ocurred while getting <" + ex.getMessage() +
                                  ">.  Check that this file is available.\n");
                System.err.println("Exception ocurred while getting <" + ex.getMessage() +
                                   ">.  Check that this file is available.\n");
            }
        } else {

            if (ViewUserInterface.getReference() != null) {

                // the user has enabled the -hide option
                System.err.println("Error: " + error);

                // exit with an abnormal return value to indicate a problem
                // (useful when running mipav from the command line and testing
                // whether the algorithm completed successfully)
                System.exit(1);
            } else {

                try {

                    // an error occurred before the ui could be set up, so assume that the user is not using -hide..
                    JOptionPane.getRootFrame().setIconImage(getIconImage(Preferences.getIconName()));
                    JOptionPane.showMessageDialog(null, error, "Error", JOptionPane.ERROR_MESSAGE);
                } catch (FileNotFoundException ex) {
                    Preferences.debug("Exception ocurred while getting <" + ex.getMessage() +
                                      ">.  Check that this file is available.\n");
                    System.err.println("Exception ocurred while getting <" + ex.getMessage() +
                                       ">.  Check that this file is available.\n");
                }
            }

        }
    }

    /**
     * Pops up a message dialog to display information.
     *
     * <p>Use for brief notices such as completion of an operation or where to find logging information. It is not meant
     * to notify a user of an error in processing.</p>
     *
     * @param  info  the information string
     */
    public static void displayInfo(String info) {

        if ((ViewUserInterface.getReference() != null) &&
                (ViewUserInterface.getReference().isAppFrameVisible() == true)) {

            try {
                JOptionPane.getRootFrame().setIconImage(getIconImage(Preferences.getIconName()));
                JOptionPane.showMessageDialog(null, info, "Information", JOptionPane.INFORMATION_MESSAGE);
            } catch (FileNotFoundException ex) {
                Preferences.debug("Exception ocurred while getting <" + ex.getMessage() +
                                  ">.  Check that this file is available.\n");
                System.err.println("Exception ocurred while getting <" + ex.getMessage() +
                                   ">.  Check that this file is available.\n");
            }
        } else {

            if (ViewUserInterface.getReference() != null) {

                // the user has enabled the -hide option
                System.err.println("Info: " + info);
            } else {

                try {

                    // an error occurred before the ui could be set up, so assume that the user is not using -hide..
                    JOptionPane.getRootFrame().setIconImage(getIconImage(Preferences.getIconName()));
                    JOptionPane.showMessageDialog(null, info, "Information", JOptionPane.INFORMATION_MESSAGE);
                } catch (FileNotFoundException ex) {
                    Preferences.debug("Exception ocurred while getting <" + ex.getMessage() +
                                      ">.  Check that this file is available.\n");
                    System.err.println("Exception ocurred while getting <" + ex.getMessage() +
                                       ">.  Check that this file is available.\n");
                }
            }
        }
    }

    /**
     * Pops up a message dialog to display a warning.
     *
     * <p>Use when an operation has failed, but the can be completed but the output may display inaccuratly; ie., errors
     * can be accomodated.</p>
     *
     * @param  warning  the message text of the warning.
     */
    public static void displayWarning(String warning) {

        if ((ViewUserInterface.getReference() != null) &&
                (ViewUserInterface.getReference().isAppFrameVisible() == true)) {

            try {
                JOptionPane.getRootFrame().setIconImage(getIconImage(Preferences.getIconName()));
                JOptionPane.showMessageDialog(null, warning, "Warning", JOptionPane.WARNING_MESSAGE);
            } catch (FileNotFoundException ex) {
                Preferences.debug("Exception ocurred while getting <" + ex.getMessage() +
                                  ">.  Check that this file is available.\n");
                System.err.println("Exception ocurred while getting <" + ex.getMessage() +
                                   ">.  Check that this file is available.\n");
            }
        } else {

            if (ViewUserInterface.getReference() != null) {

                // the user has enabled the -hide option
                System.err.println("Warning: " + warning);
            } else {

                try {

                    // an error occurred before the ui could be set up, so assume that the user is not using -hide..
                    JOptionPane.getRootFrame().setIconImage(getIconImage(Preferences.getIconName()));
                    JOptionPane.showMessageDialog(null, warning, "Warning", JOptionPane.WARNING_MESSAGE);
                } catch (FileNotFoundException ex) {
                    Preferences.debug("Exception ocurred while getting <" + ex.getMessage() +
                                      ">.  Check that this file is available.\n");
                    System.err.println("Exception ocurred while getting <" + ex.getMessage() +
                                       ">.  Check that this file is available.\n");
                }
            }
        }

    }

    /**
     * Makes a series of ints, corresponding to a color string stored in the Mipav.preferences file which looks like a
     * color string defined in web pages ("RRGGBB"). and returns a java.awt.Color based on those values.
     *
     * <p>Call with extractColor(Preferences.getProperty("OneOfThoseColors"));</p>
     *
     * <p>if preferencesColorString is null, or incomplete, returns black.</p>
     *
     * @return  java.awt.Color
     *
     * @see     java.awt.Color
     *
     * @param   preferencesColorString  -- this class pre-arranges the colors to be
     */
    public static Color extractColor(String preferencesColorString) {
        int[] RGB = new int[3];
        Color prefColor;

        try {

            if (preferencesColorString != null) {

                // so long as the string is available, pull out expected color string.
                RGB[0] = Integer.parseInt(String.valueOf(preferencesColorString.toCharArray()[0]) +
                                          String.valueOf(preferencesColorString.toCharArray()[1]), 16);
                RGB[1] = Integer.parseInt(String.valueOf(preferencesColorString.toCharArray()[2]) +
                                          String.valueOf(preferencesColorString.toCharArray()[3]), 16);
                RGB[2] = Integer.parseInt(String.valueOf(preferencesColorString.toCharArray()[4]) +
                                          String.valueOf(preferencesColorString.toCharArray()[5]), 16);
            }
        } catch (Exception npe) {
            Preferences.debug("MipavOptions: Color string was improper.  String was '" + preferencesColorString + "'");

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
     * @param   st  the tokenizer to get the boolean from
     *
     * @throws  TokenizerException  thrown if the string retrieved is not a boolean value
     *
     * @return  true if the token equals "true" without regard to case
     */
    public static final boolean getBoolean(StringTokenizer st) throws TokenizerException {
        String str = st.nextToken();

        // should either be a variation of true or false; otherwise we want to know that something is wrong
        if (!str.equalsIgnoreCase("true") && !str.equalsIgnoreCase("false")) {
            throw new TokenizerException("Unable to parse boolean value: " + str);
        }

        // returns true if str.equalsIgnoreCase( "true" )
        return new Boolean(str).booleanValue();
    }

    /**
     * Get a float value from a string tokenizer.
     *
     * @param   st  the tokenizer to get the float from
     *
     * @throws  TokenizerException  thrown if the string retrieved is not a float value
     *
     * @return  a float value
     */
    public static final float getFloat(StringTokenizer st) throws TokenizerException {
        String str = st.nextToken();

        try {
            return Float.parseFloat(str);
        } catch (NumberFormatException nfe) {
            throw new TokenizerException("Unable to parse float value: " + str);
        }
    }


    /**
     * Finds the icon of the specified name. Uses the PlaceHolder class, which is in the same directory as the icons, to
     * locate the icons.
     *
     * @param   name  name of the icon
     *
     * @return  the icon
     */
    public static ImageIcon getIcon(String name) {
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
     * Finds the image of the specified name. Uses the PlaceHolder class, which is in the same directory as the icons
     * images, to locate the images.
     *
     * @param   name  name of the image
     *
     * @return  the image
     *
     * @throws  FileNotFoundException  if we can't find the icon file
     */
    public static Image getIconImage(String name) throws FileNotFoundException {
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
     * @param   st  the tokenizer to get the integer from
     *
     * @throws  TokenizerException  thrown if the string retrieved is not an integer value
     *
     * @return  a ninteger value
     */
    public static final int getInt(StringTokenizer st) throws TokenizerException {
        String str = st.nextToken();

        try {
            return Integer.parseInt(str);
        } catch (NumberFormatException nfe) {
            throw new TokenizerException("Unable to parse integer value: " + str);
        }
    }

    /**
     * Gets the MIPAV Version number from the about.txt file.
     *
     * @return  String version number (in string format)
     */
    public static String getVersion() {

        // if we've already read in the version from about.txt, then don't do it again..
        if (version != null) {
            return version;
        }

        String verString = new String("");

        String filename = "about.txt";

        BufferedReader input = null;

        try {

            // use this long call instead of ClassLoader.getSystemResource() to work properly from a jnlp launch
            URL fileURL = Thread.currentThread().getContextClassLoader().getResource(filename);

            if (fileURL == null) {
                Preferences.debug("Unable to open " + filename +
                                  ".  Make sure it is in the same directory as MipavMain.class\n",
                                  Preferences.DEBUG_MINOR);

                return verString;
            }

            // use buffering
            // this implementation reads one line at a time
            input = new BufferedReader(new InputStreamReader(fileURL.openStream()));

            String line = null; // not declared within while loop

            while ((line = input.readLine()) != null) {

                if (line.startsWith("Version:")) {

                    // remove "Version:" and the two tabs
                    line = line.substring(10);
                    line.trim();
                    version = line.substring(0, line.indexOf(" "));
                    verString = version;

                    break;
                }

            }
        } catch (Exception ex) { }
        finally {

            try {

                if (input != null) {
                    input.close();
                    input = null;
                }
            } catch (IOException closee) { }
        }


        return verString;
    }


    /**
     * Takes a java.awt.Color and forms a string representing its color. the string appears as 6 hex digits and looks
     * like the color coding used in html files, as in: "RRGGBB".
     *
     * @param   aColor  the color to be converted to a hexidecimal
     *
     * @return  hexa-decimal string representing the 8-bit values of an RGB color, in the form of "RRGGBB".
     */
    public static String makeColorString(Color aColor) {
        String[] rgbString = new String[3];
        int n;

        rgbString[0] = Integer.toString(aColor.getRed(), 16);
        rgbString[1] = Integer.toString(aColor.getGreen(), 16);
        rgbString[2] = Integer.toString(aColor.getBlue(), 16);

        for (n = 0; n < 3; n++) {

            if ((rgbString[n]).length() == 1) {
                rgbString[n] = "0" + rgbString[n];
            }
        }

        return (rgbString[0] + rgbString[1] + rgbString[2]);
    }

    /**
     * Makes a string of a float with a specific number of decimal points.
     *
     * @param   number  number to be converted to a string
     * @param   decPts  the number of decimal points
     *
     * @return  string representation of the number
     */
    public static final String makeFloatString(float number, int decPts) {
        String str = null;

        try {
            str = String.valueOf(number);
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("MipavUtil.makeString: out of memory");

            return null;
        }

        int index = str.indexOf(".");
        int indexE = str.indexOf("E");
        int length = str.length();

        if (((index + decPts) < length) && (indexE == -1)) {
            str = str.substring(0, index + decPts + 1);
        } else if (indexE != -1) {
            if ((indexE - index) > decPts) {
                str = str.substring(0, index + decPts + 1) + str.substring(indexE);
            }
        }

        return str;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   color  Color
     * @param   name   String
     * @param   size   int
     * @param   style  int
     * @param   doU    boolean
     * @param   text   String
     *
     * @return  String
     */
    public static final String makeHTMLFontString(Color color, String name, int size, int style, boolean doU,
                                                  String text) {

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


        String fontString = "<font " + "face=\"" + name + "\" " + "size=\"" + size + "\" " + "color=\"" + colorStr +
                            "\">";

        if ((style & Font.BOLD) != 0) {
            fontString += "<b>";
        }

        if ((style & Font.ITALIC) != 0) {
            fontString += "<i>";
        }

        if (doU) {
            fontString += "<u>";
        }

        fontString += text;

        if (doU) {
            fontString += "</u>";
        }

        if ((style & Font.ITALIC) != 0) {
            fontString += "</i>";
        }

        if ((style & Font.BOLD) != 0) {
            fontString += "</b>";
        }

        fontString += "</font>";

        // System.err.println(fontString);

        return fontString;
    }

    /**
     * Takes a text field and forces the text field to accept numbers, backspace and delete-key entries.
     *
     * @param  txt                 Text field to modify.
     * @param  allowFloatingPoint  <code>true</code> will force the text field to also allow the use of the '.' key to
     *                             permit entering floating point numbers.
     */
    public static void makeNumericsOnly(JTextField txt, boolean allowFloatingPoint) {
        makeNumericsOnly(txt, allowFloatingPoint, true);
    }

    /**
     * Takes a txt field, and forces the textfield to accept numbers, backspace and delete-key entries. Arguments to the
     * method can permit the text field to also allow the entry to be negative or give it a floating point decimal
     * value. All other characters are ignored, so if further action is required, it will need to be custom-assigned by
     * the calling object.
     *
     * @param  txt                   the text field to make only accept numeric text
     * @param  allowFloatingPoint    true will forces the text field to also allow the use of the '.' key to permit
     *                               entering floating point numbers
     * @param  allowNegativeNumbers  true causes textfield to allow the entry of a single minus sign ('-') in front of
     *                               the text. An additional '-' removes minus sign from the field to give it the
     *                               opposite sign.
     */
    public static void makeNumericsOnly(JTextField txt, boolean allowFloatingPoint, boolean allowNegativeNumbers) {

        if (allowFloatingPoint && allowNegativeNumbers) {
            txt.addKeyListener(new KeyAdapter() { // make the field
                    public void keyTyped(KeyEvent evt) { // not accept letters

                        JTextField t = (JTextField) evt.getComponent();
                        char ch = evt.getKeyChar();

                        if (ch == '.') {

                            if (t.getSelectedText() != null) {

                                if (t.getText().length() == t.getSelectedText().length()) {
                                    t.setText("0.");
                                    evt.consume();
                                } else if ((t.getText().indexOf('.') != -1) &&
                                               (t.getSelectedText().indexOf('.') == -1)) { // there is a '.', but not
                                                                                           // in the selected text
                                    evt.consume(); // ignore
                                }
                            } else if (t.getText().indexOf('.') != -1) {
                                evt.consume();
                            } else if (t.getText().length() == 0) {
                                t.setText("0.");
                                evt.consume();
                            } else {
                                StringBuffer sb = new StringBuffer(t.getText());
                                t.setText(sb.insert(t.getCaretPosition(), ".").toString());
                                evt.consume();
                            }
                        } else if (ch == '-') {
                            String text = t.getText().trim();
                            int minusPlace = text.indexOf('-');

                            if (minusPlace != -1) { // text does has a '-'
                                text = text.substring(minusPlace + 1);

                                // put minus in front of text after '-'...
                                // t.setText(text);
                                t.setText("-");
                            } else {
                                t.setText("-" + text);
                            }

                            evt.consume();
                        } else if (((ch < '0') || (ch > '9')) && (ch != KeyEvent.VK_DELETE) &&
                                       (ch != KeyEvent.VK_BACK_SPACE)) {

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
                    public void keyTyped(KeyEvent evt) { // not accept letters

                        JTextField t = (JTextField) evt.getComponent();
                        char ch = evt.getKeyChar();

                        if (ch == '.') {

                            if (t.getSelectedText() != null) {

                                if (t.getText().length() == t.getSelectedText().length()) {
                                    t.setText("0.");
                                    evt.consume(); // ignore the rest
                                } else if ((t.getText().indexOf('.') != -1) &&
                                               (t.getSelectedText().indexOf('.') == -1)) { // there is a '.', but not
                                                                                           // in the selected text
                                    evt.consume(); // ignore
                                }
                            } else if (t.getText().indexOf('.') != -1) {
                                evt.consume();
                            } else if (t.getText().length() == 0) {
                                t.setText("0.");
                                evt.consume(); // ignore the rest
                            } else {
                                StringBuffer sb = new StringBuffer(t.getText());
                                t.setText(sb.insert(t.getCaretPosition(), ".").toString());
                                evt.consume(); // ignore the rest
                            }
                        }
                        /* else if (ch == '-') {...}*/
                        // negatives are not allowed
                        else if (((ch < '0') || (ch > '9')) && (ch != KeyEvent.VK_DELETE) &&
                                     (ch != KeyEvent.VK_BACK_SPACE)) {

                            // if is the case that ch is outside the bounds of a
                            // number AND it is the case that ch is neither a BS
                            // or a DE, then...key is not a digit or a deletion char
                            evt.consume();
                        }
                    }
                });
        } else if (!allowFloatingPoint && allowNegativeNumbers) {
            txt.addKeyListener(new KeyAdapter() { // make the field
                    public void keyTyped(KeyEvent evt) { // not accept letters

                        JTextField t = (JTextField) evt.getComponent();
                        char ch = evt.getKeyChar();

                        /* else if (ch == '.') {..}*/
                        // decimal not allowed
                        if (ch == '-') {
                            String text = t.getText().trim();
                            int minusPlace = text.indexOf('-');

                            if (minusPlace != -1) { // text does has a '-'
                                text = text.substring(minusPlace + 1); // only text after '-'...
                                t.setText(text);
                            } else {
                                t.setText("-" + text);
                            }

                            evt.consume();
                        } else if (((ch < '0') || (ch > '9')) && (ch != KeyEvent.VK_DELETE) &&
                                       (ch != KeyEvent.VK_BACK_SPACE)) {

                            // if is the case that ch is outside the bounds of a
                            // number AND it is the case that ch is neither a BS
                            // or a DE, then...key is not a digit or a deletion char
                            evt.consume();
                        }
                    }
                });
        } else { // if (!allowFloatingPoint && !allowNegativeNumbers) {
            txt.addKeyListener(new KeyAdapter() { // make the field
                    public void keyTyped(KeyEvent evt) { // not accept letters

                        char ch = evt.getKeyChar();

                        /* else if (ch == '.') {...}*/
                        // floating point not allowed
                        /* else if (ch == '-') {...}*/
                        // negatives are not allowed

                        if (((ch < '0') || (ch > '9')) && (ch != KeyEvent.VK_DELETE) &&
                                (ch != KeyEvent.VK_BACK_SPACE)) {

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
     * @param  container  DOCUMENT ME!
     * @param  enabled    DOCUMENT ME!
     */
    public static final void setComponentsEnabled(Container container, boolean enabled) {
        Component[] comps = container.getComponents();

        for (int y = 0; y < comps.length; y++) {
            comps[y].setEnabled(enabled);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  comp  DOCUMENT ME!
     */
    public static final void setFonts(Component[] comp) {

        for (int x = 0; x < comp.length; x++) {

            if (comp[x] instanceof Container) {
                setFonts(((Container) comp[x]).getComponents());
            }

            try {
                comp[x].setFont(MipavUtil.defaultMenuFont);
            } catch (Exception e) { } // do nothing
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  ID  DOCUMENT ME!
     */
    public static void showHelp(String ID) {


        try {
            ClassLoader cloader = help.PlaceHolderHelp.class.getClassLoader();

            // the help.jar must be in the classpath !!!!!!!!!!
            URL hsURL = HelpSet.findHelpSet(cloader, getHelpSetInJar("mipav_help.jar"));

            // System.out.println(" URL = " + hsURL.toString());
            if (hsURL != null) {

                if ((hs == null) || (helpBroker == null)) {
                    hs = new HelpSet(cloader, hsURL);
                    helpBroker = hs.createHelpBroker();
                }

                if (ID != null) {
                    helpBroker.setCurrentID(ID);
                }

                helpBroker.setSize(new java.awt.Dimension(1000, 600));
                helpBroker.setLocation(new java.awt.Point(200, 300));

                // helpBroker.getFrame().setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
                helpBroker.setDisplayed(true);
                // hs = null;
                // helpBroker = null;
            } else {
                Preferences.debug("Help file URL is " + hsURL + "\n");
                MipavUtil.displayError("Unable to find helpset.");
            }
        } catch (NullPointerException npe) {
            Preferences.debug("MIPAV Help cannot be found." + "\n", 2);
            MipavUtil.displayError("MIPAV Help cannot be found.");
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory error opening help.");
        } catch (HelpSetException error) {
            Preferences.debug("HelpSet error = " + error);
        } catch (BadIDException error) {
            MipavUtil.displayError("HelpSet ID error = " + error);
            showHelp(10000);
        }
    }

    /**
     * Displays the Java Help dialog indexed directly to the section identified by the ID passed in.
     *
     * @param  ID  the index ID indicating what section the Java Help dialop should display.
     */
    public static void showHelp(int ID) {
        HelpSet hs;
        HelpBroker helpBroker;

        try {
            ClassLoader cloader = help.PlaceHolderHelp.class.getClassLoader();

            // the help.jar must be in the classpath !!!!!!!!!!
            URL hsURL = HelpSet.findHelpSet(cloader, getHelpSetInJar("mipav_help.jar"));

            // System.out.println(" URL = " + hsURL.toString());
            if (hsURL != null) {
                hs = new HelpSet(cloader, hsURL);
                helpBroker = hs.createHelpBroker();

                // if( ID != null) helpBroker.setCurrentID(ID);
                // helpBroker.setCurrentID(Map.ID.create();
                helpBroker.setSize(new java.awt.Dimension(1000, 600));
                helpBroker.setLocation(new java.awt.Point(200, 300));

                // helpBroker.getFrame().setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
                helpBroker.setDisplayed(true);
            } else {
                Preferences.debug("Help file URL is " + hsURL + "\n");
                MipavUtil.displayError("Unable to find helpset.");
            }
        } catch (NullPointerException npe) {
            Preferences.debug("MIPAV Help cannot be found." + "\n", 2);
            MipavUtil.displayError("MIPAV Help cannot be found.");
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory error opening help.");
        } catch (HelpSetException error) {
            Preferences.debug("HelpSet error = " + error);
        }
    }

    /**
     * Tests that the entered parameter is in range.
     *
     * @param   str       the value entered by the user
     * @param   minValue  the minimum value this variable may be set to
     * @param   maxValue  the maximum value this variable may be set to
     *
     * @return  boolean result of test
     */
    public static final boolean testParameter(String str, double minValue, double maxValue) {
        double tmp;

        try {
            tmp = Double.valueOf(str).doubleValue();

            if ((tmp > maxValue) || (tmp < minValue)) {
                MipavUtil.displayError("Value is out of range: " + String.valueOf(minValue) + " , " +
                                       String.valueOf(maxValue));

                return false;
            } else {
                return true;
            }
        } catch (NumberFormatException error) {
            MipavUtil.displayError("Must enter numeric value");

            return false;
        }
    }

    /**
     * Finds the help set file within the JAR file. The help set file should end in ".hs"
     *
     * @param   jarFile  the name of the JAR file which contain help information
     *
     * @return  the help set file name
     */
    private static String getHelpSetInJar(String jarFile) {
        String hsName = null;
        String jarFullPath = null;

        try {
            URL urlToHelpJar = help.PlaceHolderHelp.class.getResource(jarFile);

            jarFullPath = URLDecoder.decode(urlToHelpJar.getPath(), "UTF-8");
        } catch (Exception error) {
            Preferences.debug("Problems finding help.jar file.");

            return null;
        }

        try {
            JarFile jar = new JarFile(new File(jarFullPath));
            Enumeration entries = jar.entries();

            while (entries.hasMoreElements()) {
                ZipEntry entry = (ZipEntry) entries.nextElement();
                String entryName = entry.getName();

                // System.out.println("file name in jar = " + entryName);
                if (entryName.endsWith(".hs")) {
                    hsName = entryName;

                    break;
                }
            }
            // MipavUtil.displayError("Unable to find helpset file.hs.");
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory error opening help.");
        } catch (IOException ee) {
            Preferences.debug("Problems opening help.jar file.");
        }

        // System.out.println("Help set file name in jar = " + hsName);
        return hsName;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Static abstract class similar to mouseAdapter to allow the catching of actionevents from components without
     * having to implement ActionListener.
     *
     * @author  linkb
     */
    public abstract static class ActionAdapter extends Object implements ActionListener {

        /**
         * Creates a new ActionAdapter object.
         */
        public ActionAdapter() { }

        /**
         * DOCUMENT ME!
         *
         * @param  ae  DOCUMENT ME!
         */
        public abstract void actionPerformed(ActionEvent ae);
    }

}
