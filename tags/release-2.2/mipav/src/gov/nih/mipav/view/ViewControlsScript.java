package gov.nih.mipav.view;

import gov.nih.mipav.model.algorithms.AlgorithmScriptParser;

import java.io.*;
import java.util.*;

/**
 *   This is a control to run a script on an active image or an image file.
 *	It calls the script parser on the image after setting up appropriate
 *   variables in the parser.
 *
 *		@version    1.0 Dec. 17, 2001
 *		@author     Lynne Pusanik
 *		@see		AlgorithmScriptParser
 *
 */
public class ViewControlsScript {

    private ViewUserInterface userInterface;

    private String scriptFileName;
    private String scriptDirectory;
    private int numImages;
    private Vector images;
    private Vector voiNames;
    private Vector imageFileNames;
    private Vector dirNames;
    private Vector imageNames;
    private String savedImageFileName;

    /**
     *	Constructs a new ViewControlsScript instance.
     *  @param ui			User interface (dialog uses main frame from UI as parent).
     */
    public ViewControlsScript(ViewUserInterface ui) {
        userInterface = ui;
    }

    //-------------------------------------------------------
    //  Access Methods
    //-------------------------------------------------------
    public void setScriptFileName(String filename) {
        scriptFileName = filename;
    }

    public void setScriptDirectory(String directory) {
        scriptDirectory = directory;
    }

    public void setImages(Vector images) {
        this.images = images;
    }

    public String getScriptFileName() {
        return this.scriptFileName;
    }

    public String getScriptDirectory() {
        return this.scriptDirectory;
    }

    public void setVoiFileName(Vector voi) {
        voiNames = voi;
    }

    public void setImgFileName(Vector img) {
        imageFileNames = img;
    }

    public void setImageNames(Vector imageNames) {
        this.imageNames = imageNames;
    }

    public void setDirName(Vector dir) {
        dirNames = dir;
    }

    public Vector getImages() {
        return this.images;
    }

    public void setSavedImageFileName(String _name) {
        savedImageFileName = _name;
    }

    //-------------------------------------------------------
    //  Control Methods
    //-------------------------------------------------------
    public void runScript() {
        // verify that the script exists
        if (scriptDirectory == null && scriptFileName == null) {
            // nothing has been identified
            MipavUtil.displayError("Scripting error:  no script identified.");
            return;
        }

        //System.out.println("script directory = " + scriptDirectory + " script name = " + scriptFileName);
        File scriptFile = new File(scriptDirectory + File.separatorChar + scriptFileName);
        if (!scriptFile.exists()) {
            // no script file
            MipavUtil.displayError("Scripting error:  script not found:\n" +
                                   scriptFile.getName());
            return;
        }
        else if (!scriptFile.canRead()) {
            // can't read file
            MipavUtil.displayError("Scripting error:  unable to read script:\n" +
                                   scriptFile.getName());
            return;
        }

        // pre parse to see how many images we need
        AlgorithmScriptParser parse = new AlgorithmScriptParser(scriptFileName, scriptDirectory, userInterface);
        numImages = parse.preParse();

        int numActiveImages = parse.preParseActiveImages();
        int openedImgNum = userInterface.getRegisteredImagesNum();

        String convertedString = new String();

        // if $images required, see if there are enough active images
        //  to run the script.  if there are not, then request
        if (numImages > 0) {

            //if there are enough images open then convert the script
            if (numActiveImages + numImages <= openedImgNum) {
                convertedString = convertToActive(scriptFileName, scriptDirectory);

                parse = null;
                parse = new AlgorithmScriptParser(convertedString, userInterface);
                numActiveImages = parse.preParseActiveImages();
            } else {
                MipavUtil.displayError("You have to open " + ((numActiveImages + numImages)- openedImgNum)
                                       + " more images.");
                return;
            }
        }



        if (openedImgNum < numActiveImages) {
            MipavUtil.displayError("You have to open " + (numActiveImages - openedImgNum) + " more images.");
            return;
        }
        if (numActiveImages > 0) {
            parse.preSetupActiveImages();
        }

        // ok now to run the script
        Vector fileNames = new Vector();
        Vector dirs = new Vector();

        parse.setFileNames(fileNames);
        parse.setFileDirs(dirs);
        if (imageNames != null) {
            parse.setImageNames(imageNames);
        }
        if (voiNames != null) {
            parse.setVoiNames(voiNames);
        }
        parse.setSavePrefix("new");
        if (parse.startMethod(Thread.MIN_PRIORITY) == false) {
            MipavUtil.displayError("A thread is already running on this object");
        }
        System.gc();

    } // end runScript()

    public void runScript(String scriptStr) {
        if (scriptStr == null || scriptStr.equalsIgnoreCase("")) {
            // nothing has been identified
            MipavUtil.displayError("Scripting error:  empty script given.");
            return;
        }

        // pre parse to see how many images we need
        AlgorithmScriptParser parse = new AlgorithmScriptParser(scriptStr, userInterface);

        numImages = parse.preParse();

        int numActiveImages = parse.preParseActiveImages();
        int openedImgNum = userInterface.getRegisteredImagesNum();

        String convertedString = new String();

        // if $images required, see if there are enough active images
        //  to run the script.  if there are not, then request
        if (numImages > 0) {

            //if there are enough images open then convert the script
            if (numActiveImages + numImages <= openedImgNum) {
                convertedString = convertToActive(scriptStr, null);

                parse = null;
                parse = new AlgorithmScriptParser(convertedString, userInterface);
                numActiveImages = parse.preParseActiveImages();
            } else {
                MipavUtil.displayError("You have to open " + ((numActiveImages + numImages)- openedImgNum)
                                       + " more images.");
                return;
            }
        }

        if (openedImgNum < numActiveImages) {
            MipavUtil.displayError("You have to open " + (numActiveImages - openedImgNum) + " more images.");
            return;
        }
        if (numActiveImages > 0) {
            parse.preSetupActiveImages();
        }

        // ok now to run the script
        Vector fileNames = new Vector();
        Vector dirs = new Vector();

        parse.setFileNames(fileNames);
        parse.setFileDirs(dirs);
        if (imageNames != null) {
            parse.setImageNames(imageNames);
        }
        parse.setSavePrefix("new");
        if (parse.startMethod(Thread.MIN_PRIORITY) == false) {
            MipavUtil.displayError("A thread is already running on this object");
        }
        System.gc();
    }

    public void runScriptFromCmdLine() {
        // verify that the script exists
        if (scriptDirectory == null && scriptFileName == null) {
            // nothing has been identified
            MipavUtil.displayError("Scripting error:  no script identified.");
            return;
        }

        //System.out.println("script directory = " + scriptDirectory + " script name = " + scriptFileName);
        File scriptFile = new File(scriptDirectory + File.separatorChar + scriptFileName);
        if (!scriptFile.exists()) {
            // no script file
            MipavUtil.displayError("Scripting error:  script not found:\n" +
                                   scriptFile.getName());
            return;
        }
        else if (!scriptFile.canRead()) {
            // can't read file
            MipavUtil.displayError("Scripting error:  unable to read script:\n" +
                                   scriptFile.getName());
            return;
        }

        // pre parse to see how many images we need
        AlgorithmScriptParser parse = new AlgorithmScriptParser(scriptFileName, scriptDirectory, userInterface);
        if (savedImageFileName != null) {
            parse.setSavedImageFileName(savedImageFileName);
        }
        numImages = parse.preParse();

        int numActiveImages = parse.preParseActiveImages();
        int openedImgNum = userInterface.getRegisteredImagesNum();

        String convertedString = new String();

        // if $images required, see if there are enough active images
        //  to run the script.  if there are not, then request
        if (numImages > 0) {

            //if there are enough images open then convert the script
            if (numActiveImages + numImages <= openedImgNum) {
                convertedString = convertToActive(scriptFileName, scriptDirectory);

                parse = null;
                parse = new AlgorithmScriptParser(convertedString, userInterface);
                numActiveImages = parse.preParseActiveImages();
                if (savedImageFileName != null) {
                    parse.setSavedImageFileName(savedImageFileName);
                }
            } else {
                MipavUtil.displayError("You have to open " + ((numActiveImages + numImages)- openedImgNum)
                                       + " more images.");
                return;
            }
        }
        if (openedImgNum < numActiveImages) {
            MipavUtil.displayError("You have to open " + (numActiveImages - openedImgNum) + " more images.");
            return;
        }
        if (numActiveImages > 0) {
            parse.preSetupActiveImages();
        }


        // ok now to run the script
        //        Vector fileNames = new Vector();
        //         Vector dirs = new Vector();

        // parse.setFileNames(fileNames);
        // parse.setFileDirs(dirs);
        parse.setParseType(1);
        if (voiNames != null) {
            parse.setVoiNames(voiNames);
        }
        if (imageFileNames != null) {
            parse.setFileNames(imageFileNames);
        }
        if (dirNames != null) {
            parse.setFileDirs(dirNames);
        }
        if (imageNames != null) {
            parse.setImageNames(imageNames);
        }
        parse.setSavePrefix("new");
        if (parse.startMethod(Thread.MIN_PRIORITY) == false) {
            MipavUtil.displayError("A thread is already running on this object");
        }
        System.gc();

    } // end runScript()


    /**
     * Converts a non-active script into an active on the fly and returns
     * the modified active script string
     * @param fileName String file name (or script string)
     * @param fileDir String file directory (can be null if using a script string)
     * @return String the modified active-script
     */
    private String convertToActive(String fileName, String fileDir) {

        BufferedReader instream;

        String command;
        StringTokenizer tokens;

        String newScriptString = new String();

        Hashtable table = new Hashtable();

        int currentActiveIndex = 1;
        int currentImageIndex = 1;

        boolean needsActive = false;

        try {
            instream = new BufferedReader( new FileReader( fileDir + fileName ) );
        } catch (Exception e) {
            System.err.println("not a file, trying as a string");

            instream = new BufferedReader( new StringReader(fileName) );
        }


        try {
            String line;
            String changedLine = null;
            line = instream.readLine();
            while ( line != null ) {

                changedLine = "";
                if ( !line.startsWith( "#" ) ) {

                    needsActive = false;

                    tokens = new StringTokenizer( line );
                    try {
                        while ( tokens.hasMoreTokens() ) {
                            command = tokens.nextToken();


                            //remove lines that have any of these commands
                            // but also add these images to the list of
                            // needed actives
                            if ( command.equals( "OpenImage" ) ||
                                 command.equals( "OpenMultiFile" ) ||
                                 command.equals( "LoadImage" ) ) {

                                needsActive = true;

                            }

                            else if ( command.startsWith( "$active" ) ) {

                                if (!table.containsKey(command)) {

                                    while(table.containsValue("$active" + currentActiveIndex)) {
                                        currentActiveIndex++;
                                    }
                                    table.put(command, new String("$active" + currentActiveIndex));
                                }

                                changedLine += table.get(command) + " ";

                            } else if (command.startsWith("$image")) {

                                if (needsActive) {
                                    //here we switch this to an active
                                    while(table.containsValue("$active" + currentActiveIndex)) {
                                        currentActiveIndex++;
                                    }
                                    table.put(command, new String("$active" + currentActiveIndex));
                                } else if (!table.containsKey(command)) {

                                    //add this command to the lowest index because it isn't being
                                    // switched to an active image
                                    while(table.containsValue("$image" + currentImageIndex)) {
                                        currentImageIndex++;
                                    }
                                    table.put(command, new String("$image" + currentImageIndex));

                                }

                                changedLine += (String)table.get(command) + " ";
                            } else {
                                changedLine += command + " ";
                            }
                        }
                    } catch ( NoSuchElementException e ) {
                        // Empty line, and we're okay with that.
                        break;
                    }

                    //only add the lines that don't have OpenImage etc
                    if (!needsActive) {
                        newScriptString += changedLine + "\n";
                    }
                }
                line = instream.readLine();
            }
            instream.close();
        } catch ( FileNotFoundException e ) {
            MipavUtil.displayError( "Error getting file." );

        } catch ( NoSuchElementException e ) {
            MipavUtil.displayError( "Error in the formatting in script file." );

        } catch ( Exception e ) {
            MipavUtil.displayError( "Error while reading script." );

        }

        //build information string for user
        String infoString = new String();

        Enumeration keys = table.keys();

        while(keys.hasMoreElements()) {
            command = (String)keys.nextElement();
            infoString = command + " has been changed to " + (String)table.get(command) + "\n" + infoString;
        }

       // MipavUtil.displayInfo(infoString);
        MipavUtil.displayInfo(newScriptString);


        return newScriptString;

    }

} // end ViewControlsScript class
