package gov.nih.mipav.view;

import java.io.*;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2003</p>
 * <p>Company: </p>
 * @author not attributable
 * @version 1.0
 */

public class LogStdStreams extends PrintStream {
    /**
   *  The name of the output log file
   */
  private static String logFileName;

  /**
   *  The only instance of this class
   */
  private static LogStdStreams logStream = null;

  private static final PrintStream stdSystemErr = System.err;
  private static final PrintStream stdSystemOut = System.out;


  /**
   *  True if System.Out is being redirected
   */
  private static boolean redirectSystemOut = true;


  /**
   *  Private constructor to create PrintStream an redirect standard streams
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


  /**
   *  Get the PrintStream being used for this logfile
   *
   * @return    the PrintStream System.err is being redirected to
   * @throws    RuntimeException if initializeErrorLogging() has not been called
   *      yet
   */
  public static PrintStream getLogStream() {
    if (logStream == null) {
      throw new RuntimeException("initializeErrorLogging() has not been called yet.");
    }
    return logStream;
  }


  /**
   *  Is the System.Out being redirected to this logfile
   *
   * @return    true if System.Out is being redirected
   * @throws    RuntimeException if initializeErrorLogging() has not been called
   *      yet
   */
  public static boolean isSystemOutRedirected() {
    if (logStream == null) {
      throw new RuntimeException("initializeErrorLogging() has not been called yet.");
    }
    return redirectSystemOut;
  }


  /**
   *  Get the name of the logfile
   *
   * @return    true if System.Out is being redirected
   * @throws    RuntimeException if initializeErrorLogging() has not been called
   *      yet
   */
  public static String getLogFileName() {
    if (logStream == null) {
      throw new RuntimeException("initializeErrorLogging() has not been called yet.");
    }
    return logFileName;
  }


  /**
   *  The static initialization method <br>
   *  Also redirects System.out, rewrites output file
   *
   * @param  fileName  the name of the log file
   * @throws           RuntimeException if initializeErrorLogging() has already
   *      been called yet
   */

  public static void initializeErrorLogging(String fileName) {
    initializeErrorLogging(fileName, null, true, false);
  }


  /**
   *  The static initialization method which writes heading <br>
   *  Also redirects System.out, rewrites output file
   *
   * @param  fileName    the name of the log file
   * @param  initialStr  the heading string to write to the log file when opened
   * @throws             RuntimeException if initializeErrorLogging() has already
   *      been called yet
   */
  public static void initializeErrorLogging(String fileName, String initialStr) {
    initializeErrorLogging(fileName, initialStr, true, false);
  }


  /**
   *  The static initialization method which writes heading <br>
   *  Rewrites output file
   *
   * @param  fileName    the name of the log file
   * @param  initialStr  the heading string to write to the log file when opened
   * @param  logStdOut   true if System.Out to redirected to log file also
   * @throws             RuntimeException if initializeErrorLogging() has already
   *      been called yet
   */
  public static void initializeErrorLogging(String fileName, String initialStr,
      boolean logStdOut) {
    initializeErrorLogging(fileName, initialStr, true, false);
  }


  /**
   *  The static initialization method
   *
   * @param  fileName    the name of the log file
   * @param  initialStr  the heading string to write to the log file when opened
   * @param  logStdOut   true if System.Out to redirected to log file also
   * @param  append      true if to append to existing log, false to rewrite new
   *      log.
   * @throws             RuntimeException if initializeErrorLogging() has already
   *      been called yet
   */
  public static void initializeErrorLogging(String fileName, String initialStr,
      boolean logStdOut, boolean append) {
    if (logStream != null) {
      throw new RuntimeException("initializeErrorLogging() has already been called.");
    }

    logFileName = fileName;
    try {
      logStream = new LogStdStreams(new FileOutputStream(logFileName, append),
          logStdOut);
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

  public static void turnOffLogging() {
      if (logStream == null) {
          return;
      }
      else {
          System.setErr(stdSystemErr);
          System.setOut(stdSystemOut);
          logStream.close();
          logStream = null;
      }
  }

}