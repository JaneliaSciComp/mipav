package gov.nih.mipav.view;


import java.io.*;


/**
 * This is a static helper class for finding the path to a number of important files to the MIPAV application. For
 * example, the mipav.preference file is stored in the < user directory >/mipav.
 */
public class GetPath {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    public enum Purpose {
        /** File will be used only for reading. */
        FOR_READING,
        /** File will be used for writing, and could be used for reading. */
        FOR_WRITING;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * * Determine the best path for the indicated file. The* best path depends on the purpose: reading or writing* the
     * file. When reading a file, first check the user's* home for a user specific version of the file. If no file* is
     * found there, then try the working directory (user.dir),* if the file still can't be found, then look in the class
     * path,* and lastly look in the directory containing the main class.** When writing a file, the file should be
     * saved in the user's* home directory (user.home). The user should always have write* permissions here and this
     * allows for different preferences for* different users.**
     *
     * @param   fileName  - the name of the file whose path needs to be found.*
     * @param   purpose   - the purpose for getting this path -- use the GetPath.FOR_READING or GetPath.FOR_WRITING*
     *
     * @return  the path to the file for reading or writing. Returns _null_ if the file cannot be found.
     */
    public static String getPath(String fileName, Purpose purpose) {
        String userDir = "";
        String userHome = "";
        String newClassPath = "";
        String jarDir = "";
        String pathName = null;
        FileInputStream testFile = null;
        boolean found = false;

        // if this file needs to be read check for its existence in various places
        if (purpose == Purpose.FOR_READING) {

            // first try finding the file in the user's home
            try {
                userHome = System.getProperty("user.home");
                testFile = new FileInputStream(userHome + File.separatorChar + "mipav" + File.separatorChar + fileName);
                pathName = userHome + File.separatorChar + "mipav" + File.separatorChar;
                found = true;
            } catch (FileNotFoundException e) { }

            if (!found) {

                // next try finding file in the working directory
                try {
                    userDir = System.getProperty("user.dir");
                    testFile = new FileInputStream(userDir + File.separatorChar + File.separatorChar + fileName);
                    pathName = userDir + "" + File.separatorChar;
                    found = true;
                    testFile.close();
                } catch (FileNotFoundException e) { }
                catch (IOException e) { }
            }

            if (!found) {
                

                // next try finding file in the class path
                String classPath = System.getProperty("java.class.path");

                String newFile = new String();
                int i = classPath.indexOf(File.pathSeparatorChar);
                int lastClassPath = -1;
                while(!found && lastClassPath+1 < classPath.length() && i != -1) {
                    i = classPath.indexOf(File.pathSeparatorChar, lastClassPath+1);
                    if(i != -1) {
                        newClassPath = classPath.substring(lastClassPath+1, i);
                        lastClassPath = i;
                        if(newClassPath.indexOf(fileName) != -1) {
                            newFile = newClassPath;
                        } else {
                            newFile = newClassPath + File.separatorChar + fileName;
                        }
                        try {
                            testFile = new FileInputStream(newFile);
                            pathName = newClassPath;
                            found = true;
                            testFile.close();
                        } // when the classpath is not specified:
                    catch (StringIndexOutOfBoundsException e) { }
                    catch (FileNotFoundException e) { }
                    catch (IOException e) { }
                    }
                }
            }

            if (!found) {
                String appTmp2;
                String appName;

                // next try finding file in the main class path
                String classPath = System.getProperty("java.class.path");

                // get the application name, remove any extraneous characters,
                // then make the first letter upper case, the rest lower case
                String appTmp = Preferences.getProperty("ApplicationTitle");

                if (appTmp != null) {
                    appTmp2 = appTmp.substring(0, appTmp.indexOf(":"));
                } else {
                    Preferences.debug("GetPath.getPath: Could not find " + fileName + ".  Checked " + userDir + ", " +
                                      userHome + ", " + newClassPath + ", and " + jarDir + "\n");

                    return null;
                }

                if (appTmp2 != null) {
                    appName = new String(appTmp2.substring(0, 1).toUpperCase() +
                                         appTmp2.substring(1, appTmp2.length()).toLowerCase());
                } else {
                    Preferences.debug("GetPath.getPath: Could not find " + fileName + ".  Checked " + userDir + ", " +
                                      userHome + ", " + newClassPath + ", and " + jarDir + "\n");

                    return null;
                }

                int index1 = 0, index2 = 0;

                index2 = classPath.indexOf(new String(appName + "Main.jar"));

                if (index2 > 0) {
                    index1 = classPath.lastIndexOf(System.getProperty("path.separator"), index2);

                    if (index1 < 0) {
                        jarDir = classPath.substring(0, index2);
                    } else {
                        jarDir = classPath.substring(index1 + 1, index2);
                    }

                    try {
                        testFile = new FileInputStream(jarDir + "" + File.separatorChar + fileName);
                        pathName = jarDir + "" + File.separatorChar;
                        testFile.close();
                        found = true;
                    } catch (FileNotFoundException e) { }
                    catch (IOException e) { }
                }
            }

            if (!found) {
                Preferences.debug("GetPath.getPath: Could not find " + fileName + ".  Checked " + userDir + ", " +
                                  userHome + ", " + newClassPath + ", and " + jarDir + "\n");

                // check to see if the preferences file is in the old location
                if (fileName.equals("mipav.preferences")) {

                    try {
                        userHome = System.getProperty("user.home");
                        testFile = new FileInputStream(userHome + File.separatorChar + fileName);
                        pathName = userHome + File.separatorChar;
                        found = true;
                    } catch (FileNotFoundException e) { }
                }
            }
        } else {

            // looking for the path for WRITING a file
            // always want to write the file in the user's home
            // directory.  It doesn't matter if the file doesn't
            // exist -- but do check for write permissions.
            try {
                userHome = System.getProperty("user.home");

                // we actually only check the parent directory for the file
                // to determine write permissions

                new File(userHome + File.separator + "mipav").mkdirs();

                File f = new File(userHome + File.separatorChar + "mipav" + File.separatorChar + fileName);

                if (f.canWrite()) {
                    pathName = userHome + File.separatorChar + "mipav" + File.separatorChar;
                    found = true;
                }
            } catch (SecurityException e) {
                Preferences.debug("GetPath.getPath: Write permissions denied for " + fileName + " in " + userHome +
                                  "\n");
            }

            if (!found) {
                Preferences.debug("GetPath.getPath: Unable to write " + fileName + ": " + userHome +
                                  " mipav directory not found." + "\n");
            }

        } // end if for reading or writing

        return pathName;

    }
}
