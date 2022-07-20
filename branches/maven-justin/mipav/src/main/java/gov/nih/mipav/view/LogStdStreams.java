package gov.nih.mipav.view;


import java.io.*;


/**
 * <p>Title:</p>
 *
 * <p>Description:</p>
 *
 * <p>Copyright: Copyright (c) 2003</p>
 *
 * <p>Company:</p>
 *
 * @author   not attributable
 * @version  1.0
 */

public class LogStdStreams extends PrintStream {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** The name of the output log file. */
    private static String logFileName;

    /** The only instance of this class. */
    public static LogStdStreams logStream = null;

    /** DOCUMENT ME! */
    private static final PrintStream stdSystemErr = System.err;

    /** DOCUMENT ME! */
    private static final PrintStream stdSystemOut = System.out;


    /** True if System.Out is being redirected. */
    private static boolean redirectSystemOut = true;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Private constructor to create PrintStream an redirect standard streams.
     *
     * @param  logFile    the file output stream to use
     * @param  logStdOut  true if the System.out to be redirected also
     */
    private LogStdStreams(FileOutputStream logFile, boolean logStdOut) {
        super(logFile, true);

        // autoflush
        System.setErr(this);
        redirectSystemOut = logStdOut;

        if (redirectSystemOut) {

            // redirect stdOut as well
            System.setOut(this);
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Get the name of the logfile.
     *
     * @return  true if System.Out is being redirected
     *
     * @throws  RuntimeException  if initializeErrorLogging() has not been called yet
     */
    public static String getLogFileName() {

        if (logStream == null) {
            throw new RuntimeException("initializeErrorLogging() has not been called yet.");
        }

        return logFileName;
    }


    /**
     * Get the PrintStream being used for this logfile.
     *
     * @return  the PrintStream System.err is being redirected to
     *
     * @throws  RuntimeException  if initializeErrorLogging() has not been called yet
     */
    public static PrintStream getLogStream() {

        if (logStream == null) {
            throw new RuntimeException("initializeErrorLogging() has not been called yet.");
        }

        return logStream;
    }


    /**
     * The static initialization method<br>
     * Also redirects System.out, rewrites output file.
     *
     * @param  fileName  the name of the log file
     */

    public static void initializeErrorLogging(String fileName) {
        initializeErrorLogging(fileName, null, true, false);
    }


    /**
     * The static initialization method which writes heading<br>
     * Also redirects System.out, rewrites output file.
     *
     * @param  fileName    the name of the log file
     * @param  initialStr  the heading string to write to the log file when opened
     */
    public static void initializeErrorLogging(String fileName, String initialStr) {
        initializeErrorLogging(fileName, initialStr, true, false);
    }


    /**
     * The static initialization method which writes heading<br>
     * Rewrites output file.
     *
     * @param  fileName    the name of the log file
     * @param  initialStr  the heading string to write to the log file when opened
     * @param  logStdOut   true if System.Out to redirected to log file also
     */
    public static void initializeErrorLogging(String fileName, String initialStr, boolean logStdOut) {
        initializeErrorLogging(fileName, initialStr, true, false);
    }


    /**
     * The static initialization method.
     *
     * @param   fileName    the name of the log file
     * @param   initialStr  the heading string to write to the log file when opened
     * @param   logStdOut   true if System.Out to redirected to log file also
     * @param   append      true if to append to existing log, false to rewrite new log.
     *
     * @throws  RuntimeException  if initializeErrorLogging() has already been called yet
     */
    public static void initializeErrorLogging(String fileName, String initialStr, boolean logStdOut, boolean append) {

        if (logStream != null) {
            Preferences.debug("Reinitializing logging", Preferences.DEBUG_MINOR);
        }

        logFileName = fileName;

        try {
            logStream = new LogStdStreams(new FileOutputStream(logFileName, append), logStdOut);
            // append
        } catch (IOException ex) {
            System.err.println("Could not open output file " + logFileName);
            System.exit(1);
        }

        if ((initialStr != null) && (initialStr.length() != 0)) {

            // write heading
            System.err.println(initialStr);
        }
    }


    /**
     * Is the System.Out being redirected to this logfile.
     *
     * @return  true if System.Out is being redirected
     *
     * @throws  RuntimeException  if initializeErrorLogging() has not been called yet
     */
    public static boolean isSystemOutRedirected() {

        if (logStream == null) {
            throw new RuntimeException("initializeErrorLogging() has not been called yet.");
        }

        return redirectSystemOut;
    }

    /**
     * DOCUMENT ME!
     */
    public static void turnOffLogging() {

        if (logStream == null) {
            return;
        } else {
            System.setErr(stdSystemErr);
            System.setOut(stdSystemOut);
            logStream.close();
            logStream = null;
        }
    }

}
