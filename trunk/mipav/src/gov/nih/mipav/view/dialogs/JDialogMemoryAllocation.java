package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to alter memory allocation of the runtime environment. The runtime memory allocation for the InstallAnywhere
 * executable can be found in the LAX file: &quot;<tt>mipav.lax</tt>&quot; or &quot;<tt>iaso.lax</tt>&quot; within most
 * environments or the file &quot;<tt>Info.plist</tt>&quot; in a Darwin/Mac OS 10 environment.
 *
 * <p>Reads the InstallAnywhere start up file then parses it, line-by-line, to come up with the memory options for the
 * dialog. Waits for user input to declare what memory settings should be applied into the start-up file.
 * InstallAnywhere will read those settings for the next time it restarts the application.</p>
 */
public class JDialogMemoryAllocation extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1470759041555523857L;

    /** Indictes that a particular file is a &quot;lax&quot;-format file. Literally, <code>LAX</code> */
    private static final String laxType = "LAX";

    /** Indictes that a particular file is a &quot;xml&quot;-format file. Literally, <code>XML</code> */
    private static final String xmlType = "XML";

    // java startup options used in XML file
    /** Flag to the java-runtime (as used in the XML-files) to indicate a memory-option. Literally, <code>-X</code>. */
    private static final String optionFlag = "-X";

    /**
     * Flag to the java-runtime (as used in the XML-files) to indicate an initial heap-size memory-option. Literally,
     * <code>ms</code>.
     */
    private static final String initHeapOption = "ms";

    /**
     * Flag to the java-runtime (as used in the XML-files) to indicate an maximum heap-size memory-option. Literally,
     * <code>mx</code>.
     */
    private static final String maxHeapOption = "mx";

    // as found in the lax file.  Hope it doesn't change, or none of this will work
    /**
     * Flag to the java-runtime (as used in the LAX-files) to indicate an initial heap-size memory-option. Literally,
     * <code>lax.nl.java.option.java.heap.size.initial=</code>.
     */
    private static final String initHeapLAX = "lax.nl.java.option.java.heap.size.initial=";

    /**
     * Flag to the java-runtime (as used in the LAX-files) to indicate an maximum heap-size memory-option. Literally,
     * <code>lax.nl.java.option.java.heap.size.max=</code>.
     */
    private static final String maxHeapLAX = "lax.nl.java.option.java.heap.size.max=";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** filename based on the application name: mipav.lax or iaso.lax. */
    private String filename = null;

    /** DOCUMENT ME! */
    private String fileType;

    /** DOCUMENT ME! */
    private JTextField initHeapText;

    /** DOCUMENT ME! */
    private int initOffset = -1; // offset value of line with inital valu

    /** DOCUMENT ME! */
    private Vector laxContents = null;

    /** DOCUMENT ME! */
    private JTextField maxHeapText;

    /** DOCUMENT ME! */
    private int maxOffset = -1; // offset value of line with max value

    /** DOCUMENT ME! */
    private File startupFile; // contains all java properties of the MIPAV app (used at least when loading!)

    /** DOCUMENT ME! */
    private JButton usePreferencesButton;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * builds the dialog interface to get user to change the lax-file settings for the JVM to use on java start-up.
     * calls to read the InstallAnywhere start up file then parses it, line-by-line to come up with the memory options
     * for the dialog. Waits for user input to declare what memory settings should be applied into the start-up file.
     * InstallAnywhere will read those settings for the next time it restarts the application.
     *
     * <p>Failure to read the startup file will dispose the dialog before it is displayed.</p>
     */
    public JDialogMemoryAllocation() {
        super(ViewUserInterface.getReference().getMainFrame(), true); // enforce modality

        userInterface = ViewUserInterface.getReference();
        setTitle("Change java-runtime Memory Allocation");

        // determine the file name based on the application name
        try {
            startupFile = getStartupFile(ViewUserInterface.getReference());
        } catch (FileNotFoundException fnf) {
            MipavUtil.displayError(fnf.getLocalizedMessage());

            return;
        }

        if (startupFile.getName().toLowerCase().endsWith("lax")) {
            fileType = laxType;
        } else if (startupFile.getName().toLowerCase().endsWith("list")) {
            fileType = xmlType;
        } else {
            return;
        }

        this.getContentPane().add(createInputPanel(false), BorderLayout.CENTER);
        this.getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        try {

            if (!readStartupFile()) { // lax file holds java runtime startup info
                dispose();

                return;
            }
        } catch (FileNotFoundException fnfe) {
            String errMsg = "";

            if (fnfe.getMessage() != null) {
                errMsg = "\n" + fnfe.getMessage();
            }

            MipavUtil.displayError(filename + " not found!\n" + "Can't find the java run-time startup information." +
                                   errMsg);

            return;
        } catch (IOException ioe) {
            MipavUtil.displayError("IOException!");

            return;
        }

        pack();
        setVisible(true);
    }

    /**
     * strictly reads the memory settings from the LAX-file.
     *
     * @param  checkOnPreferences  DOCUMENT ME!
     */
    public JDialogMemoryAllocation(boolean checkOnPreferences) {
        super(ViewUserInterface.getReference().getMainFrame(), true); // enforce modality

        userInterface = ViewUserInterface.getReference();

        if (checkOnPreferences) {
            setTitle("Check java-runtime Memory Allocation");
        } else {
            setTitle("Change java-runtime Memory Allocation");
        }

        // determine the file name based on the application name
        try {
            startupFile = getStartupFile(ViewUserInterface.getReference());
        } catch (FileNotFoundException fnf) {
            MipavUtil.displayError(fnf.getLocalizedMessage());

            return;
        }

        // this.getContentPane().add(createInputPanel(checkOnPreferences), BorderLayout.CENTER);
        // this.getContentPane().add(buildButtons(checkOnPreferences), BorderLayout.SOUTH);
        this.getContentPane().add(createInputPanel(true), BorderLayout.CENTER);
        this.getContentPane().add(buildButtons(true), BorderLayout.SOUTH);

        if (startupFile.getName().toLowerCase().endsWith("lax")) {
            fileType = laxType;
        } else if (startupFile.getName().toLowerCase().endsWith("list")) {
            fileType = xmlType;
        } else {
            return;
        }


        try {

            if (!readStartupFile()) { // lax file holds java runtime startup info
                dispose();

                return;
            }
        } catch (FileNotFoundException fnfe) {
            String errMsg = "";

            if (fnfe.getMessage() != null) {
                errMsg = "\n" + fnfe.getMessage();
            }

            MipavUtil.displayError(filename + " not found!\n" + "Can't find the java run-time startup information." +
                                   errMsg);

            return;
        } catch (IOException ioe) {
            MipavUtil.displayError("IOException! --JDialogMemoryAllocation");

            return;
        }

        // this.setSize(350, 105);
        pack();
        setVisible(true);

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * This method returns the startup file which contains the MIPAV start-up options. For most systems, the options are
     * kept in a file called &quot;mipav.lax&quot;. For Macintosh OS 10 (X), the application in the application menu is
     * actually a directory &quot;mipav.app&quot; In this case, the start-up file is in
     * &quot;mipav.app/Contents/Info.plist&quot;. This method gets the application name (&quot;mipav&quot; or
     * &quot;iaso&quot;) and looks for a .lax file with that name; if it cannot find a .lax file, it then looks for a
     * file &quot;Info.plist&quot; in the directory with the name of the application (.app)/Contents/.
     *
     * <p>Ideally, the GetPath would look in application.app/Contents, but for now that location is found here.</p>
     *
     * @param   ui  The main user-interface.
     *
     * @return  DOCUMENT ME!
     *
     * @throws  FileNotFoundException  when the app title is not in the preferences file or the ViewUserInterface is
     *                                 <code>null</code>.
     */
    public static File getStartupFile(ViewUserInterface ui) throws FileNotFoundException {
        String app = null;
        String tmp = null;

        try {
            tmp = ui.getAppTitle();

            int index = tmp.indexOf(':');

            if (index >= 0) {
                app = tmp.substring(0, index).toLowerCase();
            } else {
                app = tmp.toLowerCase();
            }
        } catch (NullPointerException npe) {
            throw new FileNotFoundException("Startup filename cannot be found.");
        }

        String fName = new String(app + ".lax");
        String startPath = GetPath.getPath(fName, GetPath.FOR_READING);

        if (startPath == null) {
            fName = new String(app + ".app" + File.separator + "Contents" + File.separator + "Info.plist"); // Macintosh!
            Preferences.debug("JDialogMemoryAllocation: Looking for " + "Info.plist as the startfile: " + startPath +
                              "\n");
            startPath = GetPath.getPath(fName, GetPath.FOR_READING);

            if (startPath == null) {
                throw new FileNotFoundException("Starting options file cannot " +
                                                "be found.  Check path and filename.");
            }
        }

        return new File(startPath, fName);
    }


    /**
     * Reads the InstallAnywhere startup file and returns the start up heap memory strings. Method can read either LAX
     * (Window,UNIX) or Info preferences list (Mac OS X) file, finds only the two entries defined by
     * {@link JDialogMemoryAllocation#initHeap} and {@link JDialogMemoryAllocation#maxHeap}. It returns the values of
     * the associated entries in Megabytes.
     *
     * @param   lax  The File referring to the InstallAnywhere startup options LAX or PLIST file
     *
     * @return  String[0], the initial heap size-text, String[1], the maximum heap size-text
     *
     * @throws  IOException            when the LAX file cannot be read.
     * @throws  FileNotFoundException  When the LAX file cannot be found.
     */
    public static String[] readStartupFile(File lax) throws IOException, FileNotFoundException {
        int initOffset = 0; // offset value of line with inital valu
        int maxOffset = 0; // offset value of line with max valu
        String line;
        String[] heapMemories = new String[2];
        BufferedReader readFile;
        int i = 0;
        String type = "";

        if (!lax.canRead()) {
            throw new IOException(lax.getAbsolutePath() + " cannot be read.");
        }

        readFile = new BufferedReader(new FileReader(lax));

        if (lax.getName().toLowerCase().endsWith("lax")) {
            type = laxType;
        } else if (lax.getName().toLowerCase().endsWith("list")) {
            type = xmlType;
        }

        // read lax file
        line = readFile.readLine(); // IOException thrown

        String returnLine = "";

        String[] lines = new String[2];

        while (line != null) {

            if (type.equals(laxType)) {
                lines = interpretLAX(line);

                if (lines[1].equals(initHeapLAX)) {
                    heapMemories[0] = lines[0];
                } else if (lines[1].equals(maxHeapLAX)) {
                    heapMemories[1] = lines[0];
                }
            } else if (type.equals(xmlType)) {
                lines = interpretXML(line);

                if (lines[1].equals(initHeapOption)) {
                    heapMemories[0] = lines[0];
                } else if (lines[1].equals(maxHeapOption)) {
                    heapMemories[1] = lines[0];
                }
            }

            // get next line;  add to list...
            line = readFile.readLine();
            i++;
        }

        readFile.close();

        return heapMemories;
    }

    /**
     * Reads event from one of the buttons to perform that buttons action. Buttons include OK, Cancel, Use Preferences
     * and HELP
     *
     * @param  ae  The button's fired action event.
     */
    public void actionPerformed(ActionEvent ae) {
        Object source = ae.getSource(); // whatever the user clicked on

        // ok: verify that max is at least as large init, then try to write.
        if (source == OKButton) {
            long heapStart = 0;
            long heapMax = 0;

            try {

                // we are now setting the init to be the same as the Max
                initHeapText.setText(maxHeapText.getText());
                heapStart = Long.parseLong(initHeapText.getText());
            } catch (NumberFormatException nfe) {
                initHeapText.selectAll();
                JOptionPane.showMessageDialog(this, "Not a valid number.");
                initHeapText.requestFocus();

                return;
            }

            try {
                heapMax = Long.parseLong(maxHeapText.getText());
            } catch (NumberFormatException nfe) {
                maxHeapText.selectAll();
                JOptionPane.showMessageDialog(this, "Not a valid number.");
                maxHeapText.requestFocus();

                return;
            }

            if (heapStart <= heapMax) {

                try {

                    // then start writing init & max as we would expect
                    writeStartOptionsFile();
                } catch (IOException ioe) {
                    MipavUtil.displayError("Error writing to starting options file.  No changes were made.");
                    dispose();

                    return;
                }

                String progName;

                if (userInterface.getAppTitle().startsWith("RFAST")) {
                    progName = "rfast";
                } else {
                    progName = "mipav";
                }

                // we are now setting the init to be the same as the Max
                Preferences.setProperty("StartingHeapSize", initHeapText.getText());
                Preferences.setProperty("MaximumHeapSize", maxHeapText.getText());

                int os = Preferences.getOS();

                if (os != Preferences.OS_MAC) {
                    int response = JOptionPane.showConfirmDialog(this,
                                                                 "Restart " + progName.toUpperCase() +
                                                                 " to apply memory changes?", "Restart needed",
                                                                 JOptionPane.YES_NO_OPTION,
                                                                 JOptionPane.INFORMATION_MESSAGE);

                    if (response == JOptionPane.YES_OPTION) {

                        try {
                            Runtime.getRuntime().exec("./" + progName);
                            System.exit(0);
                        } catch (IOException ioe) {
                            MipavUtil.displayError("Error restarting the application (./" + progName +
                                                   ").  Please exit and start " + progName.toUpperCase() +
                                                   " again manually to apply the new settings.");
                            dispose();

                            return;
                        }
                    }
                } else {
                    JOptionPane.showMessageDialog(this,
                                                  "Settings are being changed.\n" +
                                                  "The changes will take effect the next time " +
                                                  progName.toUpperCase() + " is run.", "Changing settings",
                                                  JOptionPane.INFORMATION_MESSAGE);
                }

                dispose();

                return;
            } else { // max should be the largest possible value, and in this case was smaller
                JOptionPane.showMessageDialog(this,
                                              "The initial heap size may be no larger than the maximum heap size!",
                                              "Initial heap size is larger than the maximum",
                                              JOptionPane.ERROR_MESSAGE);
                initHeapText.requestFocus();
                initHeapText.selectAll();

                return;
            }
        } else if (source == cancelButton) { // cancel button
            dispose();
        } else if (source == usePreferencesButton) {

            // we are now setting the init to be the same as the Max
            initHeapText.setText(Preferences.getProperty("MaximumHeapSize"));
            maxHeapText.setText(Preferences.getProperty("MaximumHeapSize"));
            OKButton.doClick();
        } else if (ae.getActionCommand().equals("Help")) {
            MipavUtil.showHelp("10091");
        }
    }

    /**
     * creates the buttons, USE PREFERENCES, OKAY, CANCEL, and HELP. Over-rides {@link JDialog#buildButtons()} if
     * includesPrefs is <tt>false</tt>, otherwise, it returns what JDialog#buildButtons() returns.
     *
     * @param   includePrefs  If the preferences are available, the &quot;Use Preferences&quot; button is added to the
     *                        panel when it is returned.
     *
     * @return  A JPanel which holds the buttons for user input.
     */
    public JPanel buildButtons(boolean includePrefs) {
        JPanel buttonPanel;

        if (!includePrefs) {
            buttonPanel = buildButtons();
        } else {
            buttonPanel = new JPanel();
            usePreferencesButton = new JButton("Use Preferences");
            usePreferencesButton.addActionListener(this);
            usePreferencesButton.setToolTipText("Accept values in the Preferences");
            usePreferencesButton.setMinimumSize(MipavUtil.defaultButtonSize);

            // usePreferencesButton.setPreferredSize(MipavUtil.defaultButtonSize);
            usePreferencesButton.setFont(serif12B);

            if ((Preferences.getProperty("StartingHeapSize") == null) ||
                    (Preferences.getProperty("MaximumHeapSize") == null)) {
                usePreferencesButton.setEnabled(false);
            }

            buttonPanel.add(usePreferencesButton);
            buttonPanel.add(buildOKButton());
            buttonPanel.add(buildCancelButton());
            buttonPanel.add(buildHelpButton());
        }

        return buttonPanel;
    }

    /**
     * convert the number given by the string (assuming it is a number indicating a quantity of bytes) to a number
     * indicating a quantity of megabytes. If the input is improperly formatted it throws a NumberFormatException.
     *
     * @param   byteString  a number indicating a quantity of bytes
     *
     * @return  String a number indicating the rounded value of the input as a megabyte
     *
     * @throws  NumberFormatException  if the byteString cannot be represented as a number.
     */
    protected static String convertBytesToMBytes(String byteString) throws NumberFormatException {
        long byteValue;
        long megabyteValue;

        if (byteString == null) {
            return null;
        }

        byteValue = Long.parseLong(byteString.toString());
        megabyteValue = Math.round(byteValue / Math.pow(1024, 2));

        return (String.valueOf(megabyteValue));
    }

    /**
     * convert the number given by the string (assuming it is a number indicating a quantity of kilabytes) to a number
     * indicating a quantity of megabytes. If the input is improperly formatted it throws a NumberFormatException.
     *
     * @param   kilobyteString  a number indicating a quantity of kilobytes
     *
     * @return  String a number indicating the rounded value of the input as a byte
     *
     * @throws  NumberFormatException  DOCUMENT ME!
     */
    protected static String convertKBytesToMBytes(String kilobyteString) throws NumberFormatException {
        long megabyteValue;
        long kilobyteValue;

        if (kilobyteString == null) {
            return null;
        }

        kilobyteValue = Long.parseLong(kilobyteString.toString());
        megabyteValue = Math.round(kilobyteValue / 1024);

        return (String.valueOf(megabyteValue));
    }

    /**
     * convert the number given by the string (assuming it is a number indicating a quantity of megabytes) to a number
     * indicating a quantity of bytes. If the input is improperly formatted it throws a NumberFormatException.
     *
     * @param   megabyteString  a number indicating a quantity of megabytes
     *
     * @return  String a number indicating the rounded value of the input as a byte
     *
     * @throws  NumberFormatException  DOCUMENT ME!
     */
    protected static String convertMBytesToBytes(String megabyteString) throws NumberFormatException {
        long megabyteValue;
        long byteValue;

        if (megabyteString == null) {
            return null;
        }

        megabyteValue = Long.parseLong(megabyteString.toString());
        byteValue = Math.round(megabyteValue * Math.pow(1024, 2));

        return (String.valueOf(byteValue));
    }

    /**
     * reads the given line of a LAX file to search for the initial heap size and maximum heap size Strings.
     *
     * <p>if the line starts with initial or max size strings given by initHeapLAX or maxHeapLAX, the value is converted
     * into megabytes and loaded into the appropriate textBox.</p>
     *
     * <p>If the value found is not possible (ie., the conversion threw a NumberFormatException), the substitute "1" is
     * given.</p>
     *
     * @param   line  A line of a LAX file which is to be searched for the memory strings.
     *
     * @return  the heap string, including LAX file variable name, the value in the text box as it would be found in the
     *          file.
     */
    protected static String[] interpretLAX(String line) {
        String[] returnLine = new String[2];
        String memorySpec = "";

        if (line.startsWith(initHeapLAX)) {

            try {

                // initial heap size
                memorySpec = convertBytesToMBytes(line.substring(initHeapLAX.length()));
                // initOffset = lineNumber;         // hang on to this offset
                // initHeapText.setText(convertBytesToMBytes(line.substring(initHeapLAX.length())));
            } catch (NumberFormatException nfe) {
                MipavUtil.displayError("Cannot convert initial memory value.\n" +
                                       "Substituting for a known acceptable value.");
                Preferences.debug("JDialogMemoryAllocation: Cannot convert " + " initial memory value.  " +
                                  "Substituting for a known acceptable value.\n", 3);

                // initHeapText.setText("1"); // the known acceptable value (1 megabyte)
                memorySpec = "1";
            }

            // return new String(initHeapLAX + convertMBytesToBytes(initHeapText.getText()));
            returnLine[0] = memorySpec;
            returnLine[1] = initHeapLAX;
        } else if (line.startsWith(maxHeapLAX)) {

            try {

                // max heap size
                memorySpec = convertBytesToMBytes(line.substring(maxHeapLAX.length()));
                // maxOffset = lineNumber;
                // maxHeapText.setText(convertBytesToMBytes(line.substring(maxHeapLAX.length())));
            } catch (NumberFormatException nfe) {
                MipavUtil.displayError("JDialogMemoryAllocation: Cannot " + " convert maximum memory value.\n" +
                                       "Substituting for a known acceptable value.");
                Preferences.debug("JDialogMemoryAllocation: Cannot convert " + " maximum memory value.  " +
                                  "Substituting for a known acceptable value.\n", 3);

                // maxHeapText.setText("1");  // the known acceptable value (1 megabyte)
                memorySpec = "1";
            }

            // return new String(maxHeapLAX + convertMBytesToBytes(maxHeapText.getText()));
            returnLine[0] = memorySpec;
            returnLine[1] = maxHeapLAX;
        } else {
            returnLine[0] = line;
            returnLine[1] = "";
        }

        return returnLine;
    }

    /**
     * reads the given line of a plist file to search for the initial heap size and maximum heap size Strings.
     *
     * <p>If the file contains the &quot;-X&quot; java option the value is parsed out. the value is converted into
     * megabytes, if needed and loaded into the appropriate textBox.</p>
     *
     * <p>If the value found is not possible (ie., the conversion threw a NumberFormatException), the substitute "1" is
     * given.</p>
     *
     * @param   line  A line of an XML file which is to be searched for the memory strings.
     *
     * @return  the heap string, including XML formatting, the value in the text box as it would be found in the file.
     */
    protected static String[] interpretXML(String line) {
        String[] returnLine = new String[2];
        String memorySpec = "";

        if (line.indexOf(optionFlag + initHeapOption) != -1) { // "-Xms"

            // this line should have a java option, so look for it
            // "<string>-Xms999m</string>" is a good example of what we see here

            // look for ^ms[0-9]*[km$].
            String javaOption = line.substring(line.indexOf(initHeapOption), line.lastIndexOf("<")).toLowerCase();
            // check on ending suffix: "", "k", "m"  (bytes, kilobytes, megabytes)

            char sizeDescriptor = javaOption.charAt(javaOption.length() - 1); // descriptor can be "","k","m"

            try {

                switch (sizeDescriptor) {

                    // first 2 chars are heap option ('ms' or 'mx') and
                    // so memorySpec skips those two chars
                    case 'k':
                        memorySpec = convertBytesToMBytes(javaOption.substring(2, javaOption.length() - 1));
                        ;
                        break;

                    case 'm':
                        memorySpec = javaOption.substring(2, javaOption.length() - 1); // length-1 is size descriptor
                        break;

                    default:

                        // assume that the last character was a
                        // number.  if it wasn't, the convert will
                        // throw a NumberFormatException and we'll
                        // take care of the problems there.
                        memorySpec = convertBytesToMBytes(javaOption.substring(2, javaOption.length()));
                        break;
                }
            } catch (NumberFormatException nfe) {
                MipavUtil.displayError("Cannot convert initial memory value.\n" +
                                       "Substituting for a known acceptable value.");
                Preferences.debug("JDialogMemoryAllocation: Cannot convert " + " initial memory value.  " +
                                  "Substituting for a known acceptable value.\n", 3);
                memorySpec = "1"; // the known acceptable value (1 megabyte)
            }

            // initOffset = lineNumber;
            returnLine[0] = memorySpec;
            returnLine[1] = initHeapOption;
        } else if (line.indexOf(optionFlag + maxHeapOption) != -1) { // "-Xmx"

            // this line should have a java option, so look for it
            // "<string>-Xms999m</string>" is a good example of what we see here

            // look for ^ms[0-9]*[km$].
            String javaOption = line.substring(line.indexOf(maxHeapOption), line.lastIndexOf("<")).toLowerCase();
            // check on ending suffix: "", "k", "m"  (bytes, kilobytes, megabytes)

            char sizeDescriptor = javaOption.charAt(javaOption.length() - 1); // descriptor can be "","k","m"

            try {

                switch (sizeDescriptor) {

                    // first 2 chars are heap option ('ms' or 'mx') and
                    // so memorySpec skips those two chars
                    case 'k':
                        memorySpec = convertBytesToMBytes(javaOption.substring(2, javaOption.length() - 1)); // length-1 is size descriptor
                        break;

                    case 'm':
                        memorySpec = javaOption.substring(2, javaOption.length() - 1); // length-1 is size descriptor

                        break;

                    default:

                        // assume that the last character was a
                        // number.  if it wasn't, the convert will
                        // throw a NumberFormatException and we'll
                        // take care of the problems there.
                        memorySpec = convertBytesToMBytes(javaOption.substring(2, javaOption.length()));
                        break;
                }
            } catch (NumberFormatException nfe) {
                MipavUtil.displayError("Cannot convert maximum memory value.\n" +
                                       "Substituting for a known acceptable value.");
                Preferences.debug("JDialogMemoryAllocation: Cannot convert " + " maximum memory value.  " +
                                  "Substituting for a known acceptable value.\n", 3);

                // initHeapText.setText("1"); // the known acceptable value (1 megabyte)
                memorySpec = "1";
            }

            // initOffset = lineNumber;
            returnLine[0] = memorySpec;
            returnLine[1] = maxHeapOption;
            // return new String("<string>-Xms"+initHeapText.getText()+"M</string>\n");
        } else {
            returnLine[0] = line;
            returnLine[1] = "";
        }

        return returnLine;
    }

    /**
     * creates the main panel with all the inputs.. argument <code>true</code> includes preferences if available.
     * argument <code>false</code> does not include preferences ever.
     *
     * @param   usePrefs  Whether or not to include the preferences, if available.
     *
     * @return  DOCUMENT ME!
     */
    protected JPanel createInputPanel(boolean usePrefs) {
        JPanel mainPanel = new JPanel();
        mainPanel.setBorder(BorderFactory.createEmptyBorder(10, 50, 10, 50));
        setForeground(Color.black);

        // set layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        mainPanel.setLayout(gbl);

        gbc.anchor = GridBagConstraints.NORTHWEST;

        // make content, place into layout
        // initial heap value
        JLabel initHeapLabel = new JLabel("Initial heap size:");
        initHeapLabel.setFont(serif12);
        initHeapLabel.setForeground(Color.black);
        initHeapLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;

        // gbl.setConstraints(initHeapLabel, gbc);
        // mainPanel.add(initHeapLabel);
        // mainPanel.add(Box.createHorizontalStrut(10));   // text/label spacer
        if (usePrefs) {
            JLabel initPref;

            try {
                initPref = new JLabel("(" + Preferences.getProperty("StartingHeapSize") + ")");
                initPref.setFont(serif12);
                initPref.setRequestFocusEnabled(false);
                // gbl.setConstraints(initPref, gbc);
                // mainPanel.add(initPref);
                // mainPanel.add(Box.createHorizontalStrut(10)); // label spacer
            } catch (NullPointerException npe) {
                MipavUtil.displayError("null pointer when making prefrences starting heap property label");
            }
        }

        initHeapText = new JTextField(5); // init value from file
        initHeapText.setHorizontalAlignment(JTextField.RIGHT);
        initHeapText.addActionListener(this);
        MipavUtil.makeNumericsOnly(initHeapText, false);
        gbc.gridwidth = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        // gbl.setConstraints(initHeapText, gbc);
        // mainPanel.add(initHeapText);
        // mainPanel.add(Box.createHorizontalStrut(5));
        JLabel units = new JLabel("megabytes"); // show units
        units.setFont(serif12);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        // gbl.setConstraints(units, gbc);
        // mainPanel.add(units);


        // max heap value
        JLabel maxHeapLabel = new JLabel("Maximum heap size:");
        maxHeapLabel.setFont(serif12);
        maxHeapLabel.setForeground(Color.black);
        maxHeapLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(maxHeapLabel, gbc);
        mainPanel.add(maxHeapLabel);
        mainPanel.add(Box.createHorizontalStrut(10)); // text/label spacer

        if (usePrefs) {
            JLabel maxPref;

            try {
                maxPref = new JLabel("(" + Preferences.getProperty("MaximumHeapSize") + ")");
                maxPref.setFont(serif12);

                // maxPref.setForeground(Color.DARK_GRAY);
                maxPref.setRequestFocusEnabled(false);
                gbl.setConstraints(maxPref, gbc);
                mainPanel.add(maxPref);
                mainPanel.add(Box.createHorizontalStrut(10)); // label spacer
            } catch (NullPointerException npe) {
                MipavUtil.displayError("null pointer encountered while making the preferences max-heap size label");
            }
        }

        maxHeapText = new JTextField(5); // max value from file
        maxHeapText.setHorizontalAlignment(JTextField.RIGHT);
        maxHeapText.addActionListener(this);
        MipavUtil.makeNumericsOnly(maxHeapText, false);
        gbc.gridwidth = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbl.setConstraints(maxHeapText, gbc);
        mainPanel.add(maxHeapText);
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        mainPanel.add(Box.createHorizontalStrut(5));
        units = new JLabel("megabytes"); // show units
        units.setFont(serif12);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(units, gbc);
        mainPanel.add(units);

        return mainPanel;
    }

    /**
     * Reads the startup file that has already been found. Searches for, and sets the values of if found, the initial
     * &amp; maximum heap text fields. If the Preferences indicate that debug is on, the entire startup file is
     * displayed to verify that the dialog indicates what is in the file.
     *
     * @return     if the file can be read from & written to.
     *
     * @exception  IOException            FileNotFoundException
     * @throws     FileNotFoundException  DOCUMENT ME!
     */
    protected boolean readStartupFile() throws IOException, FileNotFoundException {
        int i = 0; // counter
        BufferedReader readFile;

        if (!startupFile.canRead()) {
            MipavUtil.displayError("Not able to read the InstallAnywhere start-" + "up file: " +
                                   startupFile.getAbsolutePath() + "\n" + "To alter the memory allocation, either " +
                                   "set the permissions or \n" + "contact the system administrator.");
            Preferences.debug("JDialogMemoryAllocation:Not able to read the " + "InstallAnywhere start-up file: " +
                              startupFile.getAbsolutePath() + "\n", 2);

            return false; // no point in continuing if we can't do both
        }

        Preferences.debug("JDialogMemoryAllocation: Reading from \"" + startupFile.getAbsolutePath() + "\"\n", 5);

        readFile = new BufferedReader(new FileReader(startupFile));
        // FileNotFoundException thrown

        laxContents = new Vector();

        String line;

        // read lax file
        line = readFile.readLine(); // IOExcepotion thrown

        String[] lines = new String[2];

        // for each line of file, process to reset the init & max lines as
        // they are found.
        while (line != null) {

            // add line to end of manual file buffer
            laxContents.addElement(line);

            if (fileType.equals(laxType)) {
                lines = interpretLAX(line);

                if (lines[1].equals(initHeapLAX)) {
                    initHeapText.setText(lines[0]);
                } else if (lines[1].equals(maxHeapLAX)) {
                    maxHeapText.setText(lines[0]);
                }
            } else if (fileType.equals(xmlType)) {
                lines = interpretXML(line);

                if (lines[1].equals(initHeapOption)) {
                    initHeapText.setText(lines[0]);
                } else if (lines[1].equals(maxHeapOption)) {
                    maxHeapText.setText(lines[0]);
                }

            }

            // get next line;  add to list...
            line = readFile.readLine();
            i++;
        }

        readFile.close();

        // when DEBUG mode is on, display the LAXfile in its own window
        // in the same way the file was written
        if (Preferences.debugLevel(Preferences.DEBUG_MINOR)) {
            JDialogText fileWindow = new JDialogText(null, startupFile.getPath());
            fileWindow.setMessage("");

            for (i = 0; i < laxContents.size(); i++) {
                line = (String) laxContents.elementAt(i);
                fileWindow.append(line);
                fileWindow.append("\n");
            }

            fileWindow.setSize(400, 600); // make it big enough to read some text

            // fileWindow.componentResized(null); // tell it that it is resized!
            fileWindow.setVisible(true);
        }

        return true;
    }

    /**
     * write startup options file which is used during InstallAnywhere to run the executable. The starting options file
     * it writes out is the one which would be native to the system (that is, on Windows or UNIX systems, the startup
     * file is a LAX file; on the Macintosh OS 10 systems, it is the Info.plist file) as determined when by
     * {@see getStartupFile} during dialog instantiation. This method writes initHeapText and maxHeapText to the
     * appropriate start and max values.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    protected void writeStartOptionsFile() throws IOException {

        // temporarily leave this in the "reading" path until
        // we can figure out how to tell installAnywhere where
        // this file is!
        if (startupFile == null) {
            throw new IOException("Unable to open " + startupFile.getAbsolutePath() + " for writing.");
        }

        if (!startupFile.getParentFile().canWrite()) {
            MipavUtil.displayError("Not allowed to alter the java runtime start up file.\n" +
                                   "To alter the memory allocation, either set the permissions or \n" +
                                   "contact the system administrator.");

            return;
        }

        // make sure we actually got data from the read first!
        if (laxContents == null) {
            MipavUtil.displayError("Problem reading lax file.  Unable to save new contents.\n" +
                                   "To alter the memory allocation, either set the permissions or \n" +
                                   "contact the system administrator.");

            return;
        }

        BufferedWriter outFile = new BufferedWriter(new FileWriter(startupFile));

        int i;
        String line;

        for (i = 0; i < laxContents.size(); i++) {
            line = (String) laxContents.elementAt(i);

            if (fileType == laxType) {

                if (line.indexOf(initHeapLAX) != -1) {

                    try {
                        line = initHeapLAX + convertMBytesToBytes(initHeapText.getText());
                    } catch (NumberFormatException nfe) {
                        MipavUtil.displayError("JDialogMemoryAllocation: Cannot convert the value.\n" +
                                               "Substituting for a known acceptable value.");
                        line = initHeapLAX + "1"; // the known acceptable value (1 megabyte)
                    }
                } else if (line.indexOf(maxHeapLAX) != -1) {

                    try {
                        line = maxHeapLAX + convertMBytesToBytes(maxHeapText.getText());
                    } catch (NumberFormatException nfe) {
                        MipavUtil.displayError("JDialogMemoryAllocation: Cannot convert the value.\n" +
                                               "Substituting for a known acceptable value.");
                        line = maxHeapLAX + "1"; // the known acceptable value (1 megabyte)
                    }
                }
            } else if (fileType == xmlType) {

                if (line.indexOf(initHeapOption) != -1) {
                    line = "\t\t\t\t" + "<string>-Xms" + initHeapText.getText() + "M</string>";
                } else if (line.indexOf(maxHeapOption) != -1) {
                    line = "\t\t\t\t" + "<string>-Xmx" + maxHeapText.getText() + "M</string>";
                }
            }

            outFile.write(line);
            outFile.newLine();
        }

        outFile.close();
    }

}
