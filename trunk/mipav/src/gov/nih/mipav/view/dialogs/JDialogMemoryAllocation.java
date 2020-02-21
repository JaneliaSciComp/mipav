package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.GetPath.Purpose;
import gov.nih.mipav.view.Preferences.OperatingSystem;
import gov.nih.mipav.view.Preferences.SystemArchitecture;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.io.*;
import java.util.regex.*;

import javax.swing.*;


/**
 * Dialog to alter memory allocation of the runtime environment. The runtime memory allocation for the InstallAnywhere
 * executable can be found in the LAX file: &quot;<tt>mipav.lax</tt>&quot; or &quot;<tt>iaso.lax</tt>&quot; within most
 * environments or the file &quot;<tt>Info.plist</tt>&quot; in a Darwin/Mac OS 10 environment.
 * 
 * <p>
 * Reads the InstallAnywhere start up file then parses it, line-by-line, to come up with the memory options for the
 * dialog. Waits for user input to declare what memory settings should be applied into the start-up file.
 * InstallAnywhere will read those settings for the next time it restarts the application.
 * </p>
 */
public class JDialogMemoryAllocation extends JDialogBase {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1470759041555523857L;
    
    public enum VMConfigType {
        LAX(".lax"),
        PLIST(".plist"),
        VMOPTIONS(".vmoptions");
        
        private final String extension;
        
        VMConfigType(String ext) {
            extension = ext;
        }
        
        public String getExtention() {
            return extension;
        }
    }

    // java startup options used in XML file
    /** Flag to the java-runtime (as used in the XML-files) to indicate a memory-option. Literally, <code>-X</code>. */
    private static final String optionFlag = "-X";

    /**
     * Flag to the java-runtime (as used in the XML-files) to indicate an maximum heap-size memory-option. Literally,
     * <code>mx</code>.
     */
    private static final String maxHeapOption = "mx";

    // as found in the lax file. Hope it doesn't change, or none of this will work
    /**
     * Flag to the java-runtime (as used in the LAX-files) to indicate an maximum heap-size memory-option. Literally,
     * <code>lax.nl.java.option.java.heap.size.max=</code>.
     */
    private static final String maxHeapLAX = "lax.nl.java.option.java.heap.size.max=";
    
    private static final String vmoptionsExt = ".vmoptions";
    private static final String laxExt = ".lax";
    private static final String plistExt = ".plist";
    
    private static final String debugLauncherStr = ".command";
    
    private static final String macAppVMOptions = "vmoptions.txt";
    
    private static final int MIN_HEAP_SIZE = 300;
    
    private static VMConfigType vmType;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JTextField maxHeapText;

    /** Contains all java properties of the MIPAV app (used at least when loading!) */
    private File startupFile;

    /** DOCUMENT ME! */
    private JButton usePreferencesButton;

    /** DOCUMENT ME! */
    private final ViewUserInterface userInterface;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * builds the dialog interface to get user to change the lax-file settings for the JVM to use on java start-up.
     * calls to read the InstallAnywhere start up file then parses it, line-by-line to come up with the memory options
     * for the dialog. Waits for user input to declare what memory settings should be applied into the start-up file.
     * InstallAnywhere will read those settings for the next time it restarts the application.
     * 
     * <p>
     * Failure to read the startup file will dispose the dialog before it is displayed.
     * </p>
     */
    public JDialogMemoryAllocation() {
        super(ViewUserInterface.getReference().getMainFrame(), true); // enforce modality

        userInterface = ViewUserInterface.getReference();
        setTitle("Change java-runtime Memory Allocation");

        // determine the file name based on the application name
        try {
            startupFile = JDialogMemoryAllocation.getStartupFile();
        } catch (final FileNotFoundException fnf) {
            // MipavUtil.displayError(fnf.getLocalizedMessage());
            System.err.println(fnf.getLocalizedMessage());

            return;
        }

        if (startupFile == null) {
            return;
        }

        this.getContentPane().add(createInputPanel(false), BorderLayout.CENTER);
        this.getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        try {

            if ( !setHeapMaxField()) {
                dispose();

                return;
            }
        } catch (final FileNotFoundException fnfe) {
            MipavUtil.displayError("Error reading VM config file: " + fnfe.getLocalizedMessage());

            return;
        } catch (final IOException ioe) {
            MipavUtil.displayError("Error reading VM config file: " + ioe.getLocalizedMessage());

            return;
        }

        pack();
        setVisible(true);
    }

    /**
     * strictly reads the memory settings from the LAX-file.
     * 
     * @param ui A reference to the VUI. Must be passed in since this dialog may be called while the VUI is still being
     *            constructed (can't use VUI.getReference()).
     * @param checkOnPreferences Whether this is a preferences-vs-lax file check.
     */
    public JDialogMemoryAllocation(final ViewUserInterface ui, final boolean checkOnPreferences) {
        super(ui.getMainFrame(), true); // enforce modality

        userInterface = ui;

        if (checkOnPreferences) {
            setTitle("Check java-runtime Memory Allocation");
        } else {
            setTitle("Change java-runtime Memory Allocation");
        }

        // determine the file name based on the application name
        try {
            startupFile = JDialogMemoryAllocation.getStartupFile();
        } catch (final FileNotFoundException fnf) {
            // MipavUtil.displayError(fnf.getLocalizedMessage());
            System.err.println(fnf.getLocalizedMessage());

            return;
        }

        // this.getContentPane().add(createInputPanel(checkOnPreferences), BorderLayout.CENTER);
        // this.getContentPane().add(buildButtons(checkOnPreferences), BorderLayout.SOUTH);
        this.getContentPane().add(createInputPanel(true), BorderLayout.CENTER);
        this.getContentPane().add(buildButtons(true), BorderLayout.SOUTH);

        try {
            if ( !setHeapMaxField()) { // lax file holds java runtime startup info
                dispose();

                return;
            }
        } catch (final FileNotFoundException fnfe) {
            MipavUtil.displayError("Error reading VM config file: " + fnfe.getLocalizedMessage());

            return;
        } catch (final IOException ioe) {
            MipavUtil.displayError("Error reading VM config file: " + ioe.getLocalizedMessage());

            return;
        }

        // this.setSize(350, 105);
        pack();
        setVisible(true);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * This method returns the startup file which contains the MIPAV start-up options. For most systems, the options are
     * kept in a file called &quot;mipav.lax&quot;. For Macintosh OS 10 (X), the application in the application menu is
     * actually a directory &quot;mipav.app&quot; In this case, the start-up file is in
     * &quot;mipav.app/Contents/Info.plist&quot;. This method gets the application name (&quot;mipav&quot; or
     * &quot;iaso&quot;) and looks for a .lax file with that name; if it cannot find a .lax file, it then looks for a
     * file &quot;Info.plist&quot; in the directory with the name of the application (.app)/Contents/.
     * 
     * <p>
     * Ideally, the GetPath would look in application.app/Contents, but for now that location is found here.
     * </p>
     * 
     * @param ui The main user-interface.
     * 
     * @return DOCUMENT ME!
     * 
     * @throws FileNotFoundException when the app title is not in the preferences file or the ViewUserInterface is
     *             <code>null</code>.
     */
    public static File getStartupFile() throws FileNotFoundException {
        String app = getAppName();
        
        File startupFile = getVMOptionsStartupFile(app, false);
        if (startupFile != null) {
            vmType = VMConfigType.VMOPTIONS;
            return startupFile;
        }
        
        startupFile = getLaxStartupFile(app);
        if (startupFile != null) {
            vmType = VMConfigType.LAX;
            return startupFile;
        }
        
        startupFile = getPlistStartupFile(app);
        if (startupFile != null) {
            vmType = VMConfigType.PLIST;
            return startupFile;
        }
        
        return null;
    }
    
    public static String getAppName() throws FileNotFoundException {
        String app = null;
        String tmp = null;

        try {
            tmp = Preferences.getProperty("ApplicationTitle");

            final int index = tmp.indexOf(':');

            if (index >= 0) {
                app = tmp.substring(0, index).toLowerCase();
            } else {
                app = tmp.toLowerCase();
            }
        } catch (final NullPointerException npe) {
            FileNotFoundException e = new FileNotFoundException("Startup filename cannot be found.");
            e.initCause(npe);
            throw e;
        }
        
        return app;
    }
    
    public static File getVMOptionsStartupFile(final String app, boolean debug) throws FileNotFoundException {
        String appName = app;
        if (debug) {
            appName += debugLauncherStr;
        }
        
        System.err.println(appName);

        String fName = new String(appName + vmoptionsExt);
        String startPath = GetPath.getPath(fName, Purpose.FOR_READING);

        if (startPath == null) {
            fName = System.getProperty("mipav.file.vmoptions");
            if (fName != null) {
                Preferences.debug("JDialogMemoryAllocation: Looking for command line lax file: " + fName + "\n");
                startPath = GetPath.getPath(fName, Purpose.FOR_READING);
            }
        }

        if (startPath == null) {
            fName = new String(appName + ".app" + File.separator + "Contents" + File.separator + macAppVMOptions); // Macintosh!
            Preferences.debug("JDialogMemoryAllocation: Looking for " + macAppVMOptions + " as the startfile: " + fName + "\n");
            startPath = GetPath.getPath(fName, Purpose.FOR_READING);

            if (startPath == null) {
                throw new FileNotFoundException("Starting options file cannot be found.  Check path and filename.");
            }
        }
        
        System.err.println(new File(startPath, fName).getAbsolutePath());
        
        return new File(startPath, fName);
    }
    
    /**
     * This method returns the startup file which contains the MIPAV start-up options. For most systems, the options are
     * kept in a file called &quot;mipav.lax&quot;. This method gets the application name (&quot;mipav&quot; or
     * &quot;iaso&quot;) and looks for a .lax file with that name.
     * 
     * @param app   The application name.
     * 
     * @return DOCUMENT ME!
     * 
     * @throws FileNotFoundException when the app title is not in the preferences file or the ViewUserInterface is
     *             <code>null</code>.
     */
    public static File getLaxStartupFile(final String app) throws FileNotFoundException {
        String fName = new String(app + ".lax");
        String startPath = GetPath.getPath(fName, Purpose.FOR_READING);

        if (startPath == null) {
            fName = System.getProperty("mipav.file.lax");
            if (fName != null) {
                Preferences.debug("JDialogMemoryAllocation: Looking for command line lax file: " + fName + "\n");
                startPath = GetPath.getPath(fName, Purpose.FOR_READING);
            }
        }

        if (startPath == null) {
            fName = new String(app + ".app" + File.separator + "Contents" + File.separator + "Info.plist"); // Macintosh!
            Preferences.debug("JDialogMemoryAllocation: Looking for Info.plist as the startfile: " + fName + "\n");
            startPath = GetPath.getPath(fName, Purpose.FOR_READING);

            if (startPath == null) {
                throw new FileNotFoundException("Starting options file cannot be found.  Check path and filename.");
            }
        }

        return new File(startPath, fName);
    }
    
    /**
     * This method returns the startup file which contains the MIPAV start-up options. For most systems, the options are
     * kept in a file called &quot;mipav.lax&quot;. For Macintosh OS 10 (X), the application in the application menu is
     * actually a directory &quot;mipav.app&quot; In this case, the start-up file is in
     * &quot;mipav.app/Contents/Info.plist&quot;. This method gets the application name (&quot;mipav&quot; or
     * &quot;iaso&quot;) and looks for a .lax file with that name; if it cannot find a .lax file, it then looks for a
     * file &quot;Info.plist&quot; in the directory with the name of the application (.app)/Contents/.
     * 
     * <p>
     * Ideally, the GetPath would look in application.app/Contents, but for now that location is found here.
     * </p>
     * 
     * @param app   The application name.
     * 
     * @return DOCUMENT ME!
     * 
     * @throws FileNotFoundException when the app title is not in the preferences file or the ViewUserInterface is
     *             <code>null</code>.
     */
    public static File getPlistStartupFile(final String app) throws FileNotFoundException {
        String fName = new String(app + ".lax");
        String startPath = GetPath.getPath(fName, Purpose.FOR_READING);

        if (startPath == null) {
            fName = System.getProperty("mipav.file.lax");
            if (fName != null) {
                Preferences.debug("JDialogMemoryAllocation: Looking for command line lax file: " + fName + "\n");
                startPath = GetPath.getPath(fName, Purpose.FOR_READING);
            }
        }

        if (startPath == null) {
            fName = new String(app + ".app" + File.separator + "Contents" + File.separator + "Info.plist"); // Macintosh!
            Preferences.debug("JDialogMemoryAllocation: Looking for Info.plist as the startfile: " + fName + "\n");
            startPath = GetPath.getPath(fName, Purpose.FOR_READING);

            if (startPath == null) {
                throw new FileNotFoundException("Starting options file cannot be found.  Check path and filename.");
            }
        }

        return new File(startPath, fName);
    }
    
    protected boolean setHeapMaxField() throws IOException, FileNotFoundException {
        String maxVal = readHeapMax(startupFile);
            
        if (maxVal != null) {
            maxHeapText.setText(maxVal);
        } else {
            return false;
        }

        return true;
    }
    
    public static String readHeapMax(final File file) throws IOException, FileNotFoundException {
        if (file == null) {
            return null;
        }
        
        String maxVal = null;
        switch (vmType) {
            case VMOPTIONS:
                maxVal = readVMOptionsFileHeapMax(file);
                break;
            case LAX:
                maxVal = readLaxFileHeapMax(file);
                break;
            case PLIST:
                maxVal = readPlistFileHeapMax(file);
                break;
            default:
                System.err.println("No VM Type.");
        }
        
        if (!maxVal.equals("")) System.err.println(maxVal);
        
        return maxVal;
    }
    
    protected static String readVMOptionsFileHeapMax(final File file) throws IOException, FileNotFoundException {
        String maxVal = null;
        
        BufferedReader readFile;

        if ( !file.canRead()) {
            MipavUtil.displayError("Not able to read the InstallAnywhere start-" + "up file: " + file.getAbsolutePath() + "\n"
                    + "To alter the memory allocation, either " + "set the permissions or \n" + "contact the system administrator.");
            Preferences.debug("JDialogMemoryAllocation:Not able to read the " + "InstallAnywhere start-up file: " + file.getAbsolutePath() + "\n",
                    Preferences.DEBUG_FILEIO);

            return null; // no point in continuing if we can't do both
        }
        
        if (!isVMOptionsFile(file)) {
            return null;
        }

        Preferences.debug("JDialogMemoryAllocation: Reading from \"" + file.getAbsolutePath() + "\"\n", Preferences.DEBUG_MINOR);

        readFile = new BufferedReader(new FileReader(file));
        // FileNotFoundException thrown

        String line;

        // read vmoptions file
        line = readFile.readLine(); // IOExcepotion thrown

        // for each line of file, process to reset the init & max lines as
        // they are found.
        while (line != null) {
            String val = JDialogMemoryAllocation.getVMOptionsMaxHeap(line);
            
            if (val != null && !val.trim().equals("")) {
                maxVal = val;
            }
            
            // get next line; add to list...
            line = readFile.readLine();
        }

        readFile.close();

        return maxVal;
    }
    
    /**
     * Reads the InstallAnywhere startup file and returns the start up heap memory string. Method can read LAX
     * (Window,UNIX) finds the entry {@link JDialogMemoryAllocation#maxHeap}. It returns the values of
     * the associated entry in Megabytes.
     * 
     * @param lax The File referring to the InstallAnywhere startup options LAX
     * 
     * @return The maximum heap size-text
     * 
     * @throws IOException when the LAX file cannot be read.
     * @throws FileNotFoundException When the LAX file cannot be found.
     */
    public static String readLaxFileHeapMax(final File lax) throws IOException, FileNotFoundException {
        String line;
        String maxVal = null;
        BufferedReader readFile;
        
        if (!JDialogMemoryAllocation.isLaxFile(lax)) {
            return null;
        }

        if ( !lax.canRead()) {
            throw new IOException(lax.getAbsolutePath() + " cannot be read.");
        }
        final FileReader reader = new FileReader(lax);
        readFile = new BufferedReader(reader);

        // read lax file
        line = readFile.readLine(); // IOException thrown

        String[] lines = new String[2];

        while (line != null) {
            lines = JDialogMemoryAllocation.interpretLAX(line);

            if (lines[1].equals(JDialogMemoryAllocation.maxHeapLAX)) {
                maxVal = lines[0];
            }

            // get next line; add to list...
            line = readFile.readLine();
        }

        readFile.close();
        reader.close();

        return maxVal;
    }
    
    /**
     * Reads the InstallAnywhere startup file and returns the start up heap memory string. Method can reads Info 
     * preferences list (Mac OS X) file, and finds {@link JDialogMemoryAllocation#maxHeap}. It returns the values of
     * the associated entry in Megabytes.
     * 
     * @param lax The File referring to the InstallAnywhere startup options PLIST file
     * 
     * @return The maximum heap size-text
     * 
     * @throws IOException when the PLIST file cannot be read.
     * @throws FileNotFoundException When the LAX PLIST cannot be found.
     */
    public static String readPlistFileHeapMax(final File file) throws IOException, FileNotFoundException {
        String line;
        String maxVal = null;
        BufferedReader readFile;
        
        if (!JDialogMemoryAllocation.isPListFile(file)) {
            return null;
        }

        if ( !file.canRead()) {
            throw new IOException(file.getAbsolutePath() + " cannot be read.");
        }
        final FileReader reader = new FileReader(file);
        readFile = new BufferedReader(reader);

        // read plist file
        line = readFile.readLine(); // IOException thrown

        String[] lines = new String[2];

        while (line != null) {
            lines = JDialogMemoryAllocation.interpretXML(line);

            if (lines[1].equals(JDialogMemoryAllocation.maxHeapOption)) {
                maxVal = lines[0];
            }

            // get next line; add to list...
            line = readFile.readLine();
        }

        readFile.close();
        reader.close();

        return maxVal;
    }
    
    protected static String readFileContents(final File file) throws IOException, FileNotFoundException {
        String fileContents = new String();
        
        BufferedReader readFile;

        if ( !file.canRead()) {
            MipavUtil.displayError("Not able to read the VM configuration file: " + file.getAbsolutePath() + "\n"
                    + "To alter the memory allocation, either " + "set the permissions or \n" + "contact the system administrator.");
            Preferences.debug("JDialogMemoryAllocation: Not able to read the VM configuration file: " + file.getAbsolutePath() + "\n",
                    Preferences.DEBUG_FILEIO);

            return null; // no point in continuing if we can't do both
        }

        Preferences.debug("JDialogMemoryAllocation: Reading from \"" + file.getAbsolutePath() + "\"\n", Preferences.DEBUG_MINOR);

        readFile = new BufferedReader(new FileReader(file));
        // FileNotFoundException thrown

        String line;

        // read vmoptions file
        line = readFile.readLine(); // IOExcepotion thrown

        // for each line of file, process to reset the init & max lines as
        // they are found.
        while (line != null) {
            fileContents += line + System.lineSeparator();
            
            // get next line; add to list...
            line = readFile.readLine();
        }

        readFile.close();

        return fileContents;
    }
    
    protected static final String getVMOptionsMaxHeap(final String line) {
        String maxVal = "";
        
        Pattern pattern = Pattern.compile("^" + optionFlag + maxHeapOption + "(\\d+)([MmGgKk]?)");
        Matcher matcher = pattern.matcher(line);
        
        if (matcher.find()) {
            String unit = matcher.group(2);
            if (unit.equalsIgnoreCase("M")) {
                maxVal = matcher.group(1);
            } else if (unit.equalsIgnoreCase("G")) {
                maxVal = convertGBytesToMBytes(matcher.group(1));
            } else if (unit.equalsIgnoreCase("K")) {
                maxVal = convertKBytesToMBytes(matcher.group(1));
            } else if (unit.equals("")) {
                maxVal = convertBytesToMBytes(matcher.group(1));
            } else {
                MipavUtil.displayError("Unrecognized unit (" + unit + ") in vmoptions file");
                return maxVal;
            }
        }
        
        if (!maxVal.equals("")) System.err.println(maxVal);
        
        return maxVal;
    }
    
    protected static final boolean isVMOptionsFile(final File file) {
        if (file.getName().endsWith(vmoptionsExt)) {
            return true;
        } else if (file.getName().equalsIgnoreCase(macAppVMOptions)) {
            return true;
        }
        
        return false;
    }
    
    protected static final boolean isLaxFile(final File file) {
        if (file.getName().toUpperCase().endsWith(laxExt)) {
            return true;
        }
        
        return false;
    }
    
    protected static final boolean isPListFile(final File file) {
        if (file.getName().endsWith(plistExt)) {
            return true;
        }
        
        return false;
    }

    /**
     * Reads event from one of the buttons to perform that buttons action. Buttons include OK, Cancel, Use Preferences
     * and HELP
     * 
     * @param ae The button's fired action event.
     */
    @Override
    public void actionPerformed(final ActionEvent ae) {
        final Object source = ae.getSource(); // whatever the user clicked on

        // ok: verify that max is at least as large init, then try to write.
        if (source == OKButton) {
            long heapMax = 0;

            try {
                heapMax = Long.parseLong(maxHeapText.getText());
            } catch (final NumberFormatException nfe) {
                maxHeapText.selectAll();
                JOptionPane.showMessageDialog(this, "Not a valid number.");
                maxHeapText.requestFocus();

                return;
            }

            if (MIN_HEAP_SIZE <= heapMax) {
                try {
                    writeStartOptionsFile();
                } catch (final IOException ioe) {
                    MipavUtil.displayError("Error writing to starting options file.  No changes were made.");
                    ioe.printStackTrace();
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
                //Preferences.setProperty(Preferences.PREF_STARTING_HEAP_SIZE, initHeapText.getText());
                Preferences.setProperty(Preferences.PREF_MAX_HEAP_SIZE, maxHeapText.getText());

                final OperatingSystem os = OperatingSystem.getOS();
                final SystemArchitecture arch = SystemArchitecture.getArch();

                final String execName = "./" + progName;

                // cannot automatically restart on Mac or Win
                if (os.equals(OperatingSystem.OS_UNIX)) {
                    final int response = JOptionPane.showConfirmDialog(this, "Restart " + progName.toUpperCase() + " to apply memory changes?",
                            "Restart needed", JOptionPane.YES_NO_OPTION, JOptionPane.INFORMATION_MESSAGE);

                    if (response == JOptionPane.YES_OPTION) {

                        try {
                            Runtime.getRuntime().exec(execName);
                            System.exit(0);
                        } catch (final IOException ioe) {
                            MipavUtil.displayError("Error restarting the application (" + execName + ").  Please exit and start " + progName.toUpperCase()
                                    + " again manually to apply the new settings.");
                            dispose();

                            return;
                        }
                    }
                } else {
                    JOptionPane.showMessageDialog(this,
                            "Settings are being changed.\n" + "The changes will take effect the next time " + progName.toUpperCase() + " is run.",
                            "Changing settings", JOptionPane.INFORMATION_MESSAGE);
                }

                dispose();

                return;
            } else { // max should be the largest possible value, and in this case was smaller
                JOptionPane.showMessageDialog(this, "The maximum heap size must be higher than " + MIN_HEAP_SIZE + " megabytes.", "Maximum heap size too low", JOptionPane.ERROR_MESSAGE);

                return;
            }
        } else if (source == cancelButton) { // cancel button
            dispose();
        } else if (source == usePreferencesButton) {
            // initHeapText.setText(Preferences.getProperty(Preferences.PREF_MAX_HEAP_SIZE));
            maxHeapText.setText(Preferences.getProperty(Preferences.PREF_MAX_HEAP_SIZE));
            OKButton.doClick();
        } else if (ae.getActionCommand().equals("Help")) {
            // MipavUtil.showHelp("10091");
            MipavUtil.showWebHelp("Allocating_Memory_in_MIPAV");
        } else {
            super.actionPerformed(ae);
        }
    }

    /**
     * creates the buttons, USE PREFERENCES, OKAY, CANCEL, and HELP. Over-rides {@link JDialog#buildButtons()} if
     * includesPrefs is <tt>false</tt>, otherwise, it returns what JDialog#buildButtons() returns.
     * 
     * @param includePrefs If the preferences are available, the &quot;Use Preferences&quot; button is added to the
     *            panel when it is returned.
     * 
     * @return A JPanel which holds the buttons for user input.
     */
    public JPanel buildButtons(final boolean includePrefs) {
        JPanel buttonPanel;

        if ( !includePrefs) {
            buttonPanel = buildButtons();
        } else {
            buttonPanel = new JPanel();
            usePreferencesButton = new JButton("Use Preferences");
            usePreferencesButton.addActionListener(this);
            usePreferencesButton.setToolTipText("Accept values in the Preferences");
            usePreferencesButton.setMinimumSize(MipavUtil.defaultButtonSize);

            // usePreferencesButton.setPreferredSize(MipavUtil.defaultButtonSize);
            usePreferencesButton.setFont(serif12B);

            if ( (Preferences.getProperty(Preferences.PREF_STARTING_HEAP_SIZE) == null) || (Preferences.getProperty(Preferences.PREF_MAX_HEAP_SIZE) == null)) {
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
     * @param byteString a number indicating a quantity of bytes
     * 
     * @return String a number indicating the rounded value of the input as a megabyte
     * 
     * @throws NumberFormatException if the byteString cannot be represented as a number.
     */
    protected static String convertBytesToMBytes(final String byteString) throws NumberFormatException {
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
     * convert the number given by the string (assuming it is a number indicating a quantity of bytes) to a number
     * indicating a quantity of gigabytes. If the input is improperly formatted it throws a NumberFormatException.
     * 
     * @param byteString a number indicating a quantity of bytes
     * 
     * @return String a number indicating the rounded value of the input as a gigabyte
     * 
     * @throws NumberFormatException if the byteString cannot be represented as a number.
     */
    protected static String convertBytesToGBytes(final String byteString) throws NumberFormatException {
        long byteValue;
        long gigabyteValue;

        if (byteString == null) {
            return null;
        }

        byteValue = Long.parseLong(byteString.toString());
        gigabyteValue = Math.round(byteValue / Math.pow(1024, 3));

        return (String.valueOf(gigabyteValue));
    }

    /**
     * convert the number given by the string (assuming it is a number indicating a quantity of kilabytes) to a number
     * indicating a quantity of megabytes. If the input is improperly formatted it throws a NumberFormatException.
     * 
     * @param kilobyteString a number indicating a quantity of kilobytes
     * 
     * @return String a number indicating the rounded value of the input as a byte
     * 
     * @throws NumberFormatException DOCUMENT ME!
     */
    protected static String convertKBytesToMBytes(final String kilobyteString) throws NumberFormatException {
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
     * indicating a quantity of gigabytes. If the input is improperly formatted it throws a NumberFormatException.
     * 
     * @param kilobyteString a number indicating a quantity of megabytes
     * 
     * @return String a number indicating the rounded value of the input in gigabytes
     * 
     * @throws NumberFormatException DOCUMENT ME!
     */
    protected static String convertMBytesToGBytes(final String megabyteString) throws NumberFormatException {
        long megabyteValue;
        long gigabyteValue;

        if (megabyteString == null) {
            return null;
        }

        megabyteValue = Long.parseLong(megabyteString.toString());
        gigabyteValue = Math.round(megabyteValue / 1024);

        return (String.valueOf(gigabyteValue));
    }
    
    /**
     * convert the number given by the string (assuming it is a number indicating a quantity of gigabytes) to a number
     * indicating a quantity of megabytes. If the input is improperly formatted it throws a NumberFormatException.
     * 
     * @param kilobyteString a number indicating a quantity of gigabytes
     * 
     * @return String a number indicating the rounded value of the input in megabytes
     * 
     * @throws NumberFormatException DOCUMENT ME!
     */
    protected static String convertGBytesToMBytes(final String gigabyteString) throws NumberFormatException {
        long megabyteValue;
        long gigabyteValue;

        if (gigabyteString == null) {
            return null;
        }

        gigabyteValue = Long.parseLong(gigabyteString.toString());
        megabyteValue = Math.round(gigabyteValue * 1024);

        return (String.valueOf(megabyteValue));
    }
    
    /**
     * convert the number given by the string (assuming it is a number indicating a quantity of kilabytes) to a number
     * indicating a quantity of gigabytes. If the input is improperly formatted it throws a NumberFormatException.
     * 
     * @param kilobyteString a number indicating a quantity of kilobytes
     * 
     * @return String a number indicating the rounded value of the input as gigabytes
     * 
     * @throws NumberFormatException DOCUMENT ME!
     */
    protected static String convertKBytesToGBytes(final String kilobyteString) throws NumberFormatException {
        long gigabyteValue;
        long kilobyteValue;

        if (kilobyteString == null) {
            return null;
        }

        kilobyteValue = Long.parseLong(kilobyteString.toString());
        gigabyteValue = Math.round(kilobyteValue / Math.pow(1024, 2));

        return (String.valueOf(gigabyteValue));
    }

    /**
     * convert the number given by the string (assuming it is a number indicating a quantity of megabytes) to a number
     * indicating a quantity of bytes. If the input is improperly formatted it throws a NumberFormatException.
     * 
     * @param megabyteString a number indicating a quantity of megabytes
     * 
     * @return String a number indicating the rounded value of the input as a byte
     * 
     * @throws NumberFormatException DOCUMENT ME!
     */
    protected static String convertMBytesToBytes(final String megabyteString) throws NumberFormatException {
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
     * convert the number given by the string (assuming it is a number indicating a quantity of gigabytes) to a number
     * indicating a quantity of bytes. If the input is improperly formatted it throws a NumberFormatException.
     * 
     * @param megabyteString a number indicating a quantity of gigabytes
     * 
     * @return String a number indicating the rounded value of the input as a byte
     * 
     * @throws NumberFormatException DOCUMENT ME!
     */
    protected static String convertGBytesToBytes(final String gigabyteString) throws NumberFormatException {
        long gigabyteValue;
        long byteValue;

        if (gigabyteString == null) {
            return null;
        }

        gigabyteValue = Long.parseLong(gigabyteString.toString());
        byteValue = Math.round(gigabyteValue * Math.pow(1024, 3));

        return (String.valueOf(byteValue));
    }

    /**
     * reads the given line of a LAX file to search for the initial heap size and maximum heap size Strings.
     * 
     * <p>
     * if the line starts with initial or max size strings given by initHeapLAX or maxHeapLAX, the value is converted
     * into megabytes and loaded into the appropriate textBox.
     * </p>
     * 
     * <p>
     * If the value found is not possible (ie., the conversion threw a NumberFormatException), the substitute "1" is
     * given.
     * </p>
     * 
     * @param line A line of a LAX file which is to be searched for the memory strings.
     * 
     * @return the heap string, including LAX file variable name, the value in the text box as it would be found in the
     *         file.
     */
    protected static String[] interpretLAX(final String line) {
        final String[] returnLine = new String[2];
        String memorySpec = "";

        if (line.startsWith(JDialogMemoryAllocation.maxHeapLAX)) {
            try {

                // max heap size
                if (line.endsWith("M") || line.endsWith("m")) {
                    memorySpec = line.substring(JDialogMemoryAllocation.maxHeapLAX.length(), line.length() - 1);
                } else {
                    memorySpec = JDialogMemoryAllocation.convertBytesToMBytes(line.substring(JDialogMemoryAllocation.maxHeapLAX.length()));
                }
                // maxOffset = lineNumber;
                // maxHeapText.setText(convertBytesToMBytes(line.substring(maxHeapLAX.length())));
            } catch (final NumberFormatException nfe) {
                MipavUtil.displayError("JDialogMemoryAllocation: Cannot " + " convert maximum memory value.\n" + "Substituting for a known acceptable value.");
                Preferences.debug("JDialogMemoryAllocation: Cannot convert " + " maximum memory value.  " + "Substituting for a known acceptable value.\n", 3);

                // maxHeapText.setText("10"); // the known acceptable value (1 megabyte)
                memorySpec = "" + MIN_HEAP_SIZE;
            }

            // return new String(maxHeapLAX + convertMBytesToBytes(maxHeapText.getText()));
            returnLine[0] = memorySpec;
            returnLine[1] = JDialogMemoryAllocation.maxHeapLAX;
        } else {
            returnLine[0] = line;
            returnLine[1] = "";
        }

        return returnLine;
    }

    /**
     * reads the given line of a plist file to search for the initial heap size and maximum heap size Strings.
     * 
     * <p>
     * If the file contains the &quot;-X&quot; java option the value is parsed out. the value is converted into
     * megabytes, if needed and loaded into the appropriate textBox.
     * </p>
     * 
     * <p>
     * If the value found is not possible (ie., the conversion threw a NumberFormatException), the substitute "1" is
     * given.
     * </p>
     * 
     * @param line A line of an XML file which is to be searched for the memory strings.
     * 
     * @return the heap string, including XML formatting, the value in the text box as it would be found in the file.
     */
    protected static String[] interpretXML(final String line) {
        final String[] returnLine = new String[2];
        String memorySpec = "";

        if (line.indexOf(JDialogMemoryAllocation.optionFlag + JDialogMemoryAllocation.maxHeapOption) != -1) { // "-Xmx"

            // this line should have a java option, so look for it
            // "<string>-Xms999m</string>" is a good example of what we see here

            // look for ^ms[0-9]*[km$].
            final String javaOption = line.substring(line.indexOf(JDialogMemoryAllocation.maxHeapOption), line.lastIndexOf("<")).toLowerCase();
            // check on ending suffix: "", "k", "m" (bytes, kilobytes, megabytes)

            final char sizeDescriptor = javaOption.charAt(javaOption.length() - 1); // descriptor can be "","k","m"

            try {

                switch (sizeDescriptor) {

                // first 2 chars are heap option ('ms' or 'mx') and
                // so memorySpec skips those two chars
                    case 'k':
                        memorySpec = JDialogMemoryAllocation.convertBytesToMBytes(javaOption.substring(2, javaOption.length() - 1)); // length-1
                        // is
                        // size
                        // descriptor
                        break;

                    case 'm':
                        memorySpec = javaOption.substring(2, javaOption.length() - 1); // length-1 is size descriptor

                        break;

                    default:

                        // assume that the last character was a
                        // number. if it wasn't, the convert will
                        // throw a NumberFormatException and we'll
                        // take care of the problems there.
                        memorySpec = JDialogMemoryAllocation.convertBytesToMBytes(javaOption.substring(2, javaOption.length()));
                        break;
                }
            } catch (final NumberFormatException nfe) {
                MipavUtil.displayError("Cannot convert maximum memory value.\n" + "Substituting for a known acceptable value.");
                Preferences.debug("JDialogMemoryAllocation: Cannot convert " + " maximum memory value.  " + "Substituting for a known acceptable value.\n", 3);

                // initHeapText.setText("1"); // the known acceptable value (1 megabyte)
                memorySpec = "" + MIN_HEAP_SIZE;
            }

            // initOffset = lineNumber;
            returnLine[0] = memorySpec;
            returnLine[1] = JDialogMemoryAllocation.maxHeapOption;
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
     * @param usePrefs Whether or not to include the preferences, if available.
     * 
     * @return DOCUMENT ME!
     */
    protected JPanel createInputPanel(final boolean usePrefs) {
        final JPanel mainPanel = new JPanel();
        mainPanel.setBorder(BorderFactory.createEmptyBorder(10, 50, 10, 50));
        setForeground(Color.black);

        // set layout
        final GridBagLayout gbl = new GridBagLayout();
        final GridBagConstraints gbc = new GridBagConstraints();
        mainPanel.setLayout(gbl);

        gbc.anchor = GridBagConstraints.NORTHWEST;
        
        // max heap value
        final JLabel maxHeapLabel = new JLabel("Maximum heap size:");
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
                maxPref = new JLabel("(Preferences: " + Preferences.getProperty(Preferences.PREF_MAX_HEAP_SIZE) + ")");
                maxPref.setFont(serif12);

                // maxPref.setForeground(Color.DARK_GRAY);
                maxPref.setRequestFocusEnabled(false);
                gbl.setConstraints(maxPref, gbc);
                mainPanel.add(maxPref);
                mainPanel.add(Box.createHorizontalStrut(10)); // label spacer
            } catch (final NullPointerException npe) {
                MipavUtil.displayError("null pointer encountered while making the preferences max-heap size label");
            }
        }

        maxHeapText = new JTextField(5); // max value from file
        maxHeapText.setHorizontalAlignment(SwingConstants.RIGHT);
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
        JLabel units = new JLabel("megabytes"); // show units
        units.setFont(serif12);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(units, gbc);
        mainPanel.add(units);

        return mainPanel;
    }
    
    protected void writeStartOptionsFile() throws IOException {
        String app = getAppName();
        
        switch (vmType) {
            case VMOPTIONS:
                File vmoptionsFile = getVMOptionsStartupFile(app, false);
                if (vmoptionsFile != null) {
                    writeVMOptionsFile(vmoptionsFile);
                }
                vmoptionsFile = getVMOptionsStartupFile(app, true);
                if (vmoptionsFile != null) {
                    writeVMOptionsFile(vmoptionsFile);
                }
                break;
            case LAX:
                writeLaxOptionsFile();
                break;
            case PLIST:
                writePlistOptionsFile();
                break;
        }
    }
    
    /**
     * Write out changes to the vmoptions file used to set the VM max heap size.
     * 
     * @throws IOException DOCUMENT ME!
     */
    protected void writeVMOptionsFile(final File vmoptionsFile) throws IOException {

        // temporarily leave this in the "reading" path until
        // we can figure out how to tell installAnywhere where
        // this file is!
        if (vmoptionsFile == null) {
            throw new IOException("Unable to open VMOptions file for writing.");
        }

        // System.out.println(startupFile.toURI());
        // System.out.println(startupFile.getParentFile().toURI());

        if ( !vmoptionsFile.canWrite()) {
            MipavUtil.displayError("Not allowed to alter the java runtime start up file.\n"
                    + "To alter the memory allocation, either set the permissions or \n" + "contact the system administrator.");

            return;
        }
        
        String vmoptionsContents = readFileContents(vmoptionsFile);

        // make sure we actually got data from the read first!
        if (vmoptionsContents == null) {
            MipavUtil.displayError("Problem reading vmoptions file.  Unable to save new contents.\n"
                    + "To alter the memory allocation, either set the permissions or \n" + "contact the system administrator.");

            return;
        }

        final BufferedWriter outFile = new BufferedWriter(new FileWriter(vmoptionsFile));

        String newContents = replacePatternMultiline("^" + optionFlag + maxHeapOption + "\\d+[MmKkGg]?", vmoptionsContents, optionFlag + maxHeapOption + maxHeapText.getText() + "M");
        
        if (newContents != null) {
            outFile.write(newContents);
        } else {
            MipavUtil.displayError("Error editing the vmoptions file contents. No change has been made.");
        }

        outFile.close();
    }

    /**
     * write startup options file which is used during InstallAnywhere to run the executable. The starting options file
     * it writes out is the one which would be native to the system (that is, on Windows or UNIX systems, the startup
     * file is a LAX file; on the Macintosh OS 10 systems, it is the Info.plist file) as determined when by {@see
     * getStartupFile} during dialog instantiation. This method writes initHeapText and maxHeapText to the appropriate
     * start and max values.
     * 
     * @throws IOException DOCUMENT ME!
     */
    protected void writePlistOptionsFile() throws IOException {

        // temporarily leave this in the "reading" path until
        // we can figure out how to tell installAnywhere where
        // this file is!
        if (startupFile == null) {
            throw new IOException("Unable to open PList file for writing.");
        }

        // System.out.println(startupFile.toURI());
        // System.out.println(startupFile.getParentFile().toURI());

        if ( !startupFile.canWrite()) {
            MipavUtil.displayError("Not allowed to alter the java runtime start up file.\n"
                    + "To alter the memory allocation, either set the permissions or \n" + "contact the system administrator.");

            return;
        }
        
        String plistContents = readFileContents(startupFile);

        // make sure we actually got data from the read first!
        if (plistContents == null) {
            MipavUtil.displayError("Problem reading lax file.  Unable to save new contents.\n"
                    + "To alter the memory allocation, either set the permissions or \n" + "contact the system administrator.");

            return;
        }

        final BufferedWriter outFile = new BufferedWriter(new FileWriter(startupFile));

        //line = "\t\t\t\t" + "<string>-Xmx" + maxHeapText.getText() + "M</string>";
        String newContents = replacePatternMultiline("^" + optionFlag + maxHeapOption + "\\d+[MmKkGg]?", plistContents, optionFlag + maxHeapOption + maxHeapText.getText() + "M");
        
        if (newContents != null) {
            outFile.write(newContents);
        } else {
            MipavUtil.displayError("Error editing the plist file contents. No change has been made.");
        }
        
        outFile.close();
    }
    
    /**
     * write startup options file which is used during InstallAnywhere to run the executable. The starting options file
     * it writes out is the one which would be native to the system (that is, on Windows or UNIX systems, the startup
     * file is a LAX file; on the Macintosh OS 10 systems, it is the Info.plist file) as determined when by {@see
     * getStartupFile} during dialog instantiation. This method writes initHeapText and maxHeapText to the appropriate
     * start and max values.
     * 
     * @throws IOException DOCUMENT ME!
     */
    protected void writeLaxOptionsFile() throws IOException {

        // temporarily leave this in the "reading" path until
        // we can figure out how to tell installAnywhere where
        // this file is!
        if (startupFile == null) {
            throw new IOException("Unable to open LAX file for writing.");
        }

        // System.out.println(startupFile.toURI());
        // System.out.println(startupFile.getParentFile().toURI());

        if ( !startupFile.canWrite()) {
            MipavUtil.displayError("Not allowed to alter the java runtime start up file.\n"
                    + "To alter the memory allocation, either set the permissions or \n" + "contact the system administrator.");

            return;
        }
        
        String laxContents = readFileContents(startupFile);

        // make sure we actually got data from the read first!
        if (laxContents == null) {
            MipavUtil.displayError("Problem reading lax file.  Unable to save new contents.\n"
                    + "To alter the memory allocation, either set the permissions or \n" + "contact the system administrator.");

            return;
        }

        final BufferedWriter outFile = new BufferedWriter(new FileWriter(startupFile));

        //line = JDialogMemoryAllocation.maxHeapLAX + maxHeapText.getText() + "M";
        String newContents = replacePatternMultiline("^" + JDialogMemoryAllocation.maxHeapLAX + "\\d+[MmKkGg]?", laxContents, JDialogMemoryAllocation.maxHeapLAX + maxHeapText.getText() + "M");
        
        if (newContents != null) {
            outFile.write(newContents);
        } else {
            MipavUtil.displayError("Error editing the lax file contents. No change has been made.");
        }
        
        outFile.close();
    }

    protected static String replacePatternMultiline(final String regex, final String origStr, final String replaceVal) {
        Pattern pattern = Pattern.compile(regex, Pattern.MULTILINE);
        Matcher matcher = pattern.matcher(origStr);
        
        if (matcher.find()) {
            return matcher.replaceAll(replaceVal);
        }
        
        return null;
    }
}
