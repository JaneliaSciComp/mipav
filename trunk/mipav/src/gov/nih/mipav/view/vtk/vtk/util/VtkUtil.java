package gov.nih.mipav.view.vtk.vtk.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Properties;

/**
 * Simple utility class for retrieving values of environment variables.
 * SUN's recommended way of passing environment variables to Java VM is by use of
 * <code>-D</code> option at command prompt, for instance, value of variable
 * VTK_DATA_ROOT can be passed like this:
 * <pre>
 *   java -DVTK_DATA_ROOT=/path/to/VTKData Medical1
 * </pre>
 * or using environment variable <code>VTK_DATA_ROOT</code>
 * <pre>
 *   java -DVTK_DATA_ROOT=${VTK_DATA_ROOT} Medical1
 * </pre>
 * Though recommended it is not always convenient. Methods in this class attempt
 * to retrieve actual values of environment variables without use of <coder>-D</code>
 * option. Where environment variables are stored and how to access them differs from
 * platform to platform, so <code>getVtkDataRoot</code> or
 * <code>getEnvironmentVariable</code> may not work on all systems.
 * 
 * @author Jarek Sacha
 * @version $Revision: 1.11 $
 */
public class VtkUtil {


  private static final String VTK_DATA_ROOT = "VTK_DATA_ROOT";

  private VtkUtil() {

  }

  /**
   * Retrieves value of an environment variable. Preference is given to values
   * defined at command prompt using option <code>-D</code>, for instance
   * <pre>
   *   java -DVTK_DATA_ROOT=${HOME}/src/VTKData Medical1
   * </pre>
   * 
   * @param variableName Name of the variable to retrieve.
   * @return Value of the environment variable.
   * @throws RuntimeException when unable to retrieve value of requested variable
   */
  public static String getEnvironmentVariable(String variableName)
      throws RuntimeException {

    // Check is variable was defined at command prompt
    String value = System.getProperty(VTK_DATA_ROOT);
    if (value != null) {
      return value;
    }

    String osName = System.getProperty("os.name");
    String[] command = null;

    if (osName.startsWith("Windows 9") || osName.equalsIgnoreCase("Windows ME")) {
      // Assome Windows 95, 98, or ME
      command = new String[3];
      command[0] = "command.com";
      command[1] = "/C";
      command[2] = "set";
    } else if (osName.startsWith("Windows")) {
      // Assome Windows NT, 2000, or XP
      command = new String[3];
      command[0] = "cmd.exe";
      command[1] = "/C";
      command[2] = "set";
    } else {
      // Assume UNIX like OS
      // Is it going to work for Mac OS X?
      command = new String[1];
      command[0] = "/usr/bin/printenv";
    }

    // Check environment variables
    try {
      Process process = Runtime.getRuntime().exec(command);
      StreamGrabber errorStreamGrabber = new StreamGrabber(process.getErrorStream());
      StreamGrabber outputStreamGrabber = new StreamGrabber(process.getInputStream());

      errorStreamGrabber.start();
      outputStreamGrabber.start();

      int r = process.waitFor();
      if (r == 0) {
        // Wait for outputStreamGrabber to complete
        outputStreamGrabber.join();
        // Extract properties in the output stream
        Properties properties = extractProperties(outputStreamGrabber.getData());
        value = properties.getProperty(VTK_DATA_ROOT);
        value = "vtkData";
        if (value == null) {
          throw new RuntimeException("Property '" + variableName + "' not defined. "
              + "Use command line option '-D" + variableName
              + "=value' to define 'VTK_DATA_ROOT'.");
        }
      } else {
        throw new RuntimeException("Cannot extract environment variable '" + VTK_DATA_ROOT + "'." +
            " Lookup thread terminated with code " + r + ".");
      }
    } catch (IOException e) {
      throw new RuntimeException(
          "Error extracting environment variable: '" + variableName + "'.", e);
    } catch (InterruptedException e) {
      throw new RuntimeException(
          "Error extracting environment variable: '" + variableName + "'.", e);
    }


    return value;
  }

  /**
   * Looks for strings that conatin '=', convert them to properties.
   * 
   * @param strings that may contain properties in form 'key=value'
   * @return extracted properties.
   */
  private static Properties extractProperties(String[] strings) {
    Properties properties = new Properties();
    for (int i = 0; i < strings.length; ++i) {
      String string = strings[i];
      int separatorIndex = string.indexOf('=');
      if (separatorIndex > 0) {
        String key = string.substring(0, separatorIndex);
        String value = string.substring(separatorIndex + 1);
        properties.setProperty(key.trim(), value.trim());
      }
    }
    return properties;
  }

  /**
   * Convenience method for retrieving environment variable VTK_DATA_ROOT.
   * 
   * @return Value of environment variable VTK_DATA_ROOT.
   * @throws RuntimeException when unable to retrieve value of VTK_DATA_ROOT
   */
  public static String getVtkDataRoot() throws RuntimeException {
    return getEnvironmentVariable(VTK_DATA_ROOT);
  }

  /**
   * Print all system properties.
   */
  static public void printProperties() {
    Properties properties = System.getProperties();
    Enumeration propertyNames = properties.propertyNames();
    while (propertyNames.hasMoreElements()) {
      String name = (String) propertyNames.nextElement();
      System.out.println(name + " : " + properties.getProperty(name));
    }
  }

  /**
   * Utility class for grabbing process outputs.
   */
  private static class StreamGrabber extends Thread {
    InputStream inputStream;
    ArrayList lines = new ArrayList();

    StreamGrabber(InputStream inputStream) {
      this.inputStream = inputStream;
    }

    public void run() {
      try {
        BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));
        String line = null;
        while ((line = reader.readLine()) != null) {
          lines.add(line);
        }
        reader.close();
      } catch (IOException exception) {
        exception.printStackTrace();
      }
    }

    public String[] getData() {
      return (String[]) lines.toArray(new String[lines.size()]);
    }
  }

}
