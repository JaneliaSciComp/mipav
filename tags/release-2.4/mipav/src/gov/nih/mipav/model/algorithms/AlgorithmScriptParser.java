package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;
import java.util.*;
import java.awt.event.ActionEvent;
import javax.swing.*;


/**
 *	This class parses a script and runs it.  A script is parsed based on the newline charater;
 *	thus, each line starts with either a command or a "#", the "#" indicating a comment.  Any
 *	lines beginning with a "#" are ignored.  Commands are strings like "    Image".  Tokens
 *	necessary for running the command, such as "$image1" for "OpenImage", are separated by spaces or tabs.
 *
 *	@version	1.0 June 1, 2001
 *	@author		Neva Cherniavsky (primary)
 *	@author     Harman Singh
 *	@see		JDialogMultiple
 *	@see		JDialogScript
 */
public class AlgorithmScriptParser extends AlgorithmBase {

    protected VariableHashtable variableTable;
    protected VariableHashtable voiTable;
    protected static Vector fileNames;
    protected static Vector fileDirs;
    protected static Vector imageNames;
    protected Vector voiNames;
    protected static String savePrefix;
    protected static String saveSuffix = "";
    protected Vector fileTypes;
    protected int currFileIndex = -1;
    protected int currVoiIndex = -1;
    protected int saveIndex = 1;
    protected String scriptName;
    protected String scriptDir;
    protected ViewUserInterface UI;
    protected boolean isRunning = false;
    protected StringTokenizer tokens;
    protected String scriptStr;
    protected int parseType = 0;
    public static int VERTICAL_PARSE = 0, HORIZONTAL_PARSE = 1;
    protected Hashtable activeImagesTable;
    protected int activeImageNum = 1;
    protected int actImgInScript = 0;
    protected String savedImageFileName;
    protected String prefix;

    /**
     *	Constructs the parser and initializes the hashtable for holding the variables.
     *	@param fileName	File name of the script file.
     *	@param fileDir	Directory of the script file.
     */
    public AlgorithmScriptParser( String fileName, String fileDir ) {
        scriptName = fileName;
        scriptDir = fileDir;
        UI = ViewUserInterface.getReference();
        variableTable = new VariableHashtable();
        voiTable = new VariableHashtable();
        activeImagesTable = new VariableHashtable();
    }

    /**
     *	Constructs the parser and initializes the hashtable for holding the variables.
     *	@param script	text script to parse
     */
    public AlgorithmScriptParser( String script ) {
        scriptStr = script;
        UI = ViewUserInterface.getReference();
        variableTable = new VariableHashtable();
        voiTable = new VariableHashtable();
        activeImagesTable = new VariableHashtable();

        // no file needs to be accessed
        scriptName = null;
        scriptDir = null;
    }

    /**
     *	Runs the script parser.  Opens the script file and calls <code>parseLine</code> for each line
     *	in the script.
     */
    public void runAlgorithm() {
        // parsing a script file
        if ( scriptDir != null && scriptName != null ) {
            BufferedReader instream;

            isRunning = true;

            try {
                instream = new BufferedReader( new FileReader( scriptDir + scriptName ) );
                String line;

                line = instream.readLine();
                while ( line != null ) {
                    if ( !parseLine( line ) ) {
                        return;
                    }
                    line = instream.readLine();
                }
                instream.close();
                isRunning = false;
            } catch ( FileNotFoundException e ) {
                MipavUtil.displayError( "Error getting file." );
                isRunning = false;
                return;
            } catch ( NoSuchElementException e ) {
                MipavUtil.displayError( "Error in the formatting in script file." );
                isRunning = false;
                return;
            } catch ( IOException e ) {
                MipavUtil.displayError( "Error while reading script." );
                isRunning = false;
                return;
            }
        } // not parsing a file, parsing a string already passed in
        else if ( scriptStr != null ) {
            isRunning = true;
            StringTokenizer st = new StringTokenizer( scriptStr, "\n" );
            String line;

            while ( st.hasMoreTokens() ) {
                line = st.nextToken();
                if ( !parseLine( line ) ) {
                    return;
                }
            }
            isRunning = false;
        }
    }

    /**
     *	preParses the files to find out how many secondary images need to be loaded.
     *	@return	The number of images that need to be loaded.
     */
    public int preParse() {
        isRunning = true;
        int numImages = 0;
        String command;

        if ( scriptDir != null && scriptName != null ) {
            BufferedReader instream;

            try {
                instream = new BufferedReader( new FileReader( scriptDir + scriptName ) );
                String line;

                line = instream.readLine();
                while ( line != null ) {

                    if ( !line.startsWith( "#" ) ) {
                        tokens = new StringTokenizer( line );
                        try {
                            command = tokens.nextToken();
                        } catch ( NoSuchElementException e ) {
                            // Empty line, and we're okay with that.
                            break;
                        }
                        if ( command.equals( "OpenImage" ) || command.equals( "OpenMultiFile" )
                                || command.equals( "LoadImage" ) ) {
                            numImages++;
                        }
                    }
                    line = instream.readLine();
                }
                instream.close();
                isRunning = false;
            } catch ( FileNotFoundException e ) {
                MipavUtil.displayError( "Error getting file." );
                isRunning = false;
                return 0;
            } catch ( NoSuchElementException e ) {
                MipavUtil.displayError( "Error in the formatting in script file." );
                isRunning = false;
                return 0;
            } catch ( Exception e ) {
                MipavUtil.displayError( "Error while reading script." );
                isRunning = false;
                return 0;
            }
        } else if ( scriptStr != null ) {
            StringTokenizer st = new StringTokenizer( scriptStr, "\n" );
            String line;

            while ( st.hasMoreTokens() ) {
                line = st.nextToken();

                if ( !line.startsWith( "#" ) ) {
                    tokens = new StringTokenizer( line );
                    try {
                        command = tokens.nextToken();
                    } catch ( NoSuchElementException e ) {
                        // Empty line, and we're okay with that.
                        break;
                    }
                    if ( command.equals( "OpenImage" ) || command.equals( "OpenMultiFile" )
                            || command.equals( "LoadImage" ) ) {
                        numImages++;
                    }
                }
            }
            isRunning = false;
        }

        return numImages;
    }

    /**
     * preParses the files to find out how many active images need to be loaded.
     * @return	The number of active images that need to be loaded.
     */
    public int preParseActiveImages() {
        isRunning = true;
        int numImages = 0;
        String command;
        VariableHashtable actImgTable = new VariableHashtable();

        if ( scriptDir != null && scriptName != null ) {
            BufferedReader instream;

            try {
                instream = new BufferedReader( new FileReader( scriptDir + scriptName ) );
                String line;

                line = instream.readLine();
                while ( line != null ) {

                    if ( !line.startsWith( "#" ) ) {
                        tokens = new StringTokenizer( line );
                        try {
                            while ( tokens.hasMoreTokens() ) {
                                command = tokens.nextToken();
                                if ( command.startsWith( "$active" ) ) {
                                    actImgTable.put( command, "" );
                                } else if ( command.equals("Prefix") ) {
                                  try {
                                    prefix = tokens.nextToken();
                                  }
                                  catch (NoSuchElementException e) {
                                    // Empty line, and we're okay with that.
                                    break;
                                  }

                                }
                            }
                        } catch ( NoSuchElementException e ) {
                            // Empty line, and we're okay with that.
                            break;
                        }
                    }
                    line = instream.readLine();
                }
                instream.close();
                numImages = actImgTable.size();
                isRunning = false;
            } catch ( FileNotFoundException e ) {
                MipavUtil.displayError( "Error getting file." );
                isRunning = false;
                return 0;
            } catch ( NoSuchElementException e ) {
                MipavUtil.displayError( "Error in the formatting in script file." );
                isRunning = false;
                return 0;
            } catch ( Exception e ) {
                MipavUtil.displayError( "Error while reading script." );
                isRunning = false;
                return 0;
            }
        } else if ( scriptStr != null ) {
            StringTokenizer st = new StringTokenizer( scriptStr, "\n" );
            String line;

            while ( st.hasMoreTokens() ) {
                line = st.nextToken();

                if ( !line.startsWith( "#" ) ) {
                    tokens = new StringTokenizer( line );
                    try {
                        while ( tokens.hasMoreTokens() ) {
                            command = tokens.nextToken();
                            if ( command.startsWith( "$active" ) ) {
                                actImgTable.put( command, "" );
                            }
                        }
                    } catch ( NoSuchElementException e ) {
                        // Empty line, and we're okay with that.
                        break;
                    }
                }
            }
            isRunning = false;
            numImages = actImgTable.size();
        }
        actImgInScript = numImages;
        return numImages;
    }

    /**
     * The dialog ask user to set up the acive images before the script run.
     */
    public void preSetupActiveImages() {
        // add the current active images into a temporary buffer
        int numActImg = actImgInScript - 1;
        Vector potentialActives = new Vector();
        Enumeration e = UI.getRegisteredImages();

        while ( e.hasMoreElements() ) {
            ModelImage image = (ModelImage) e.nextElement();
            ViewJFrameImage frame = UI.getFrameContainingImage( image );

            if ( frame != null ) {
                potentialActives.add( image.getImageName() );
            }
        }
        String name = (String) UI.getActiveImageFrame().getComponentImage().getActiveImage().getImageName();

        potentialActives.remove( name );

        variableTable.put( "$active1", name );

        JOptionPane pane = new JOptionPane();
        int k = 0;

        activeImagesTable = new VariableHashtable();
        while ( potentialActives.size() > 0 && numActImg > 0 ) {
            Object[] possibleValues = new Object[potentialActives.size()];

            for ( int i = 0; i < potentialActives.size(); i++ ) {
                possibleValues[i] = potentialActives.elementAt( i );
            }
            if ( potentialActives.size() == 1 ) {
                variableTable.put( "$active" + ( k + 2 ), (String) potentialActives.elementAt( 0 ) );
                return;
            }
            String msg = "Choose $active" + ( k + 2 ) + ":";
            Object selectedValue = pane.showInputDialog( null, msg, "Input", JOptionPane.INFORMATION_MESSAGE, null,
                    possibleValues, possibleValues[0] );

            if ( selectedValue != null ) {

                variableTable.put( "$active" + ( k + 2 ), (String) selectedValue );
                // putActiveVar( (String) selectedValue);
            }
            k++;
            numActImg--;
            potentialActives.remove( selectedValue );
        }
    }

    /**
     *	preParses the files to find out OpenVOI command line.
     *	@return	flag  script contain OpenVOI command line.
     */
    public boolean hasOpenVOI() {
        isRunning = true;
        int numImages = 0;
        String command;

        if ( scriptDir != null && scriptName != null ) {
            BufferedReader instream;

            try {
                instream = new BufferedReader( new FileReader( scriptDir + scriptName ) );
                String line;

                line = instream.readLine();
                while ( line != null ) {

                    if ( !line.startsWith( "#" ) ) {
                        tokens = new StringTokenizer( line );
                        try {
                            command = tokens.nextToken();
                        } catch ( NoSuchElementException e ) {
                            // Empty line, and we're okay with that.
                            break;
                        }
                        if ( command.equals( "OpenVOI" ) ) {
                            return true;
                        }
                    }
                    line = instream.readLine();
                }
                instream.close();
                isRunning = false;
            } catch ( FileNotFoundException e ) {
                MipavUtil.displayError( "Error getting file." );
                isRunning = false;
                return false;
            } catch ( NoSuchElementException e ) {
                MipavUtil.displayError( "Error in the formatting in script file." );
                isRunning = false;
                return false;
            } catch ( Exception e ) {
                MipavUtil.displayError( "Error while reading script." );
                isRunning = false;
                return false;
            }
        } else if ( scriptStr != null ) {
            StringTokenizer st = new StringTokenizer( scriptStr, "\n" );
            String line;

            while ( st.hasMoreTokens() ) {
                line = st.nextToken();

                if ( !line.startsWith( "#" ) ) {
                    tokens = new StringTokenizer( line );
                    try {
                        command = tokens.nextToken();
                    } catch ( NoSuchElementException e ) {
                        // Empty line, and we're okay with that.
                        break;
                    }
                    if ( command.equals( "OpenVOI" ) ) {
                        return true;
                    }
                }
            }
            isRunning = false;
        }

        return false;
    }

    /**
     * Set the alg script parsing type
     * @param type  parseing type, either VERTICAL or HORIZONTAL
     */
    public void setParseType( int type ) {
        parseType = type;
    }

    /**
     *	Sets the file name for current script parsing.  This method is called by JDialogMultiple, which
     *	goes through a series of files and runs a script on them.
     *	@param names		The files to use
     */
    public void setFileNames( Vector names ) {
        fileNames = names;
    }

    /**
     * set image names
     * @param imageNames Vector
     */
    public void setImageNames( Vector imageNames) {
        this.imageNames = imageNames;

        for (int i = 0; i < imageNames.size(); i++) {
            variableTable.put("$image" + String.valueOf(i+1),imageNames.elementAt(i).toString());
        }
    }

    /**
     * Sets the voi names corresponding to each image.
     * @param names  voi names array
     */
    public void setVoiNames( Vector names ) {

        /*
         voiNames = new Vector[names.length];
         for (int i = 0; i < names.length; i++) {
         voiNames[i] = new Vector();
         }
         */
        voiNames = names;
    }

    /**
     *	Sets the directory for current script parsing.  This method is called by JDialogMultiple, which
     *	goes through a series of files and runs a script on them.
     *	@param names		the directories containing the files to use
     */
    public void setFileDirs( Vector names ) {
        fileDirs = names;
    }

    /**
     *	Sets the file type of the image for current script parsing.  No one is calling this right now but we made need
     *	it in the future.
     *	@param types		the types of the files to use
     *	@see FileBase
     */
    public void setFileTypes( Vector types ) {
        fileTypes = types;
    }

    /**
     *	Sets the save extension for current script parsing.  This method is called by JDialogMultiple, which
     *	goes through a series of files and runs a script on them.  When the files are saved, this will be
     *	prepended to the file name minus the extension.
     *	@param name		Extension to set to.
     *	@return			The frame that contains the image whose name is <code>name</code>.
     */
    public void setSavePrefix( String name ) {
        savePrefix = name;
    }

    /**
     *	Sets the save extension for current script parsing.  This method is called by JDialogMultiple, which
     *	goes through a series of files and runs a script on them.  When the files are saved, this will be
     *	prepended to the file name minus the extension.
     *	@param name		Extension to set to.
     *	@return			The frame that contains the image whose name is <code>name</code>.
     */
    public void setSaveSuffix( String name ) {
        saveSuffix = name;
    }

    /**
     *	Sets the script string for current script parsing.
     *	@param str	script string
     */
    public void setScriptString( String str ) {
        scriptStr = str;
    }

    /**
     *	Returns the script string.
     */
    public String getScriptString() {
        return scriptStr;
    }

    /**
     *	Parses the line with a series of "if...else..." statements on the command.  The command is the first
     *	token of the string <code>line</code>.  If the line begins with a "#", everything after that is ignored;
     *	"#" is meant to indicate a comment.
     *	@return	A flag indicating whether or not the line was parsed properly.
     */
    protected boolean parseLine( String line ) {
        String command;
        String key;

        if ( line.startsWith( "#" ) ) {
            command = "Comment";
        } else {
            tokens = new StringTokenizer( line );
            try {
                command = tokens.nextToken();
            } catch ( NoSuchElementException e ) {
                // Empty line, and we're okay with that.
                return true;
            }
        }

        if ( command.equals("Prefix") ) {
        }
        else if ( command.equals( "Comment" ) ) {
            return true;
        } else if ( command.equals( "Exit" ) ) {
            System.exit( 0 );
        } else if ( command.equals( "OpenImage" ) ) {
            try {
                currFileIndex++;
                String name;
                ViewOpenFileUI fileUI = new ViewOpenFileUI( UI, false );
                FileIO io;

                io = new FileIO();
                key = getNextString();

                if ( io.getFileType( (String) fileNames.elementAt( currFileIndex ),
                        ( (String) fileDirs.elementAt( currFileIndex ) ) )
                        == FileBase.RAW ) {
                    name = fileUI.open(
                            ( (String) fileDirs.elementAt( currFileIndex ) )
                                    + ( (String) fileNames.elementAt( currFileIndex ) ),
                                    false,
                                    getRawFileInfo( false ) );
                } else {
                    name = fileUI.open(
                            ( (String) fileDirs.elementAt( currFileIndex ) )
                                    + ( (String) fileNames.elementAt( currFileIndex ) ),
                                    false,
                                    null );
                }
                // if the SaveAllOnSave preference flag is set, then
                // load all the files associated with this image (VOIs, LUTs, etc.)
                if ( Preferences.is(Preferences.PREF_SAVE_ALL_ON_SAVE) ) {
                    ModelImage img = UI.getRegisteredImageByName( name );
                    // get frame for image
                    ViewJFrameImage imgFrame = img.getParentFrame();

                    // load any luts
                    imgFrame.loadLUT( true, true );
                    // load any vois
                    imgFrame.loadAllVOIs( true );
                }
                variableTable.put( key, name );
            } catch ( Exception e ) {
                e.printStackTrace();
                MipavUtil.displayError( "Error in script file near \"OpenImage\"\n" );
                isRunning = false;
                return false;
            }
        } else if ( command.equals( "LoadImage" ) ) {
            try {
                currFileIndex++;
                FileIO io;

                io = new FileIO();
                key = getNextString();
                ModelImage image;

                if ( io.getFileType( (String) fileNames.elementAt( currFileIndex ),
                        ( (String) fileDirs.elementAt( currFileIndex ) ) )
                        == FileBase.RAW ) {
                    image = io.readImage( ( (String) fileNames.elementAt( currFileIndex ) ),
                            ( (String) fileDirs.elementAt( currFileIndex ) ), false, getRawFileInfo( false ) );
                } else {
                    image = io.readImage( ( (String) fileNames.elementAt( currFileIndex ) ),
                            ( (String) fileDirs.elementAt( currFileIndex ) ), false, null );
                }
                variableTable.put( key, image.getImageName() );
            } catch ( Exception e ) {
                MipavUtil.displayError( "Error in script file near \"LoadImage\"\n" );
                isRunning = false;
                return false;
            }
        } else if ( command.equals( "OpenMultiFile" ) ) {
            try {
                currFileIndex++;
                String name;
                FileIO io;

                io = new FileIO();
                ViewOpenFileUI fileUI = new ViewOpenFileUI( UI, false );

                key = getNextString();
                if ( io.getFileType( (String) fileNames.elementAt( currFileIndex ),
                        ( (String) fileDirs.elementAt( currFileIndex ) ) )
                        == FileBase.RAW ) {
                    name = fileUI.open(
                            ( (String) fileDirs.elementAt( currFileIndex ) )
                                    + ( (String) fileNames.elementAt( currFileIndex ) ),
                                    true,
                                    getRawFileInfo( true ) );
                } else {
                    name = fileUI.open(
                            ( (String) fileDirs.elementAt( currFileIndex ) )
                                    + ( (String) fileNames.elementAt( currFileIndex ) ),
                                    true,
                                    null );
                }
                variableTable.put( key, name );
            } catch ( Exception e ) {
                MipavUtil.displayError( "Error in script file near \"OpenImage\"\n" );
                isRunning = false;
                return false;
            }
        } else if ( command.equals( "CloseFrame" ) ) {
            String name = null;

            try {
                name = (String) variableTable.get( getNextString() );
                getFrameFromName( name ).windowClosing( null );
            } catch ( Exception e ) {
                MipavUtil.displayError( "Error in script file near \"CloseFrame\".  Image " + name + " not found.\n" );
                isRunning = false;
                return false;
            }
        } else if ( command.equals( "SaveImage" ) ) {
            String name = null;

            try {
                name = (String) variableTable.get( getNextString() );
                getFrameFromName( name ).saveImageInfo();
                getFrameFromName( name ).save( getWriteOptions( name, false ), -1 );
            } catch ( Exception e ) {
                e.printStackTrace();
                MipavUtil.displayError(
                        "Error in script file near \"SaveImage\".  Image " + name + " not found or error in options.\n" );
                isRunning = false;
                return false;
            }
        } else if ( command.equals( "SaveImageAs" ) ) {
            String name = null;

            if ( prefix != null ) {
                setSavePrefix( prefix + "_" );
            }

            try {
                name = (String) variableTable.get( getNextString() );

                if ( savedImageFileName != null ) {
                  FileWriteOptions fOption = getWriteOptions(prefix + name, true);
                  fOption.setFileName(savedImageFileName);
                  getFrameFromName(name).save(fOption, -1);
                } else {
                  getFrameFromName(name).saveImageInfo();
                  getFrameFromName(name).save(getWriteOptions(name, true), -1);
                }
            } catch ( Exception e ) {
                MipavUtil.displayError(
                        "Error in script file near \"SaveImageAs\".  Image " + name
                        + " not found or error in options.\n" );
                isRunning = false;
                return false;
            }

        } else if (command.equals("ChangeName") ) {
          try {
            String var1 = getNextString();
            String newName = getNextString();

            boolean keepReading = true;
            while (keepReading) {
              try {
                newName += " " + getNextString();
              }
              catch (Exception ex) {
                keepReading = false;
              }
            }

            String name1 = (String) variableTable.get( var1 );
            ModelImage image = UI.getRegisteredImageByName( name1 );

            image.updateFileName(newName);
            variableTable.remove(var1);
            variableTable.put(var1, newName);

            } catch ( Exception e ) {
                MipavUtil.displayError( "Error in script file near \"ChangeName\"." );
                isRunning = false;
                return false;
            }

        }
        else if ( command.equals( "SaveTab" ) ) {
            try {
                String var1 = getNextString();
                UI.getMessageFrame().save(var1);
            } catch (Exception e) {

            }
        }
        else if ( command.equals( "CreateBlankImage" ) ) {
            try {
                tokens.nextToken(); // get rid of the word "attributes:" so tokenizer is in the right place.
                UI.createBlankImage( getRawFileInfo( false ) );
            } catch ( Exception e ) {
                MipavUtil.displayError( "Error in script file near \"CreateBlankImage\".\n" );
                isRunning = false;
                return false;
            }
        } else if ( command.equals( "OpenVOI" ) ) {
            try {
                String var1 = getNextString();
                String imgName = (String) variableTable.get( var1 );
                ModelImage image = UI.getRegisteredImageByName( imgName );
                String fileName;
                String directory;
                String fileNameIn;
                int index = 0;

                currVoiIndex++;
                // Ruida
                /*
                 ViewOpenVOIUI voiUI = new ViewOpenVOIUI(UI);
                 VOI vois[], firstVoi;
                 vois = voiUI.open(image);
                 firstVoi = vois[0];
                 String voiName = firstVoi.getName();
                 */
                key = getNextString();
                // System.out.println("key = " + key + "  voiName = "  + voiName);
                // / System.out.println("key = " + key + " voiName = " + voiNames[currVoiIndex].elementAt(0));
                // voiTable.put(key, voiNames[currVoiIndex].elementAt(0));
                VOI[] voi;
                FileVOI fileVOI;

                /*
                 System.out.println("ruida voi");
                 for ( int j =0; j < voiNames.size(); j++ ) {
                 System.out.println("voiNames = " + voiNames.elementAt(j));
                 }
                 */
                if ( parseType == VERTICAL_PARSE ) {
                    for ( int i = 0; i < voiNames.size(); i++ ) {
                        for ( int x = 0; x < ( (Vector) ( voiNames.elementAt( i ) ) ).size(); x++ ) {
                            // fileNameIn = (String) (voiNames.elementAt(i));
                            fileNameIn = (String) ( ( (Vector) ( voiNames.elementAt( i ) ) ).elementAt( x ) );
                            index = fileNameIn.lastIndexOf( File.separatorChar );
                            directory = fileNameIn.substring( 0, index + 1 );
                            fileName = fileNameIn.substring( index + 1, fileNameIn.length() );
                            fileVOI = new FileVOI( fileName, directory, image );
                            voi = fileVOI.readVOI();
                            // image.registerVOI(voiNames[currVoiIndex].elementAt(i));
                            for ( int j = 0; j < voi.length; j++ ) {
                                image.registerVOI( voi[j] );
                            }
                        }
                    }
                } else if ( parseType == HORIZONTAL_PARSE ) {
                    int k = 0;

                    for ( k = 0; k < fileNames.size(); k++ ) {
                        String fName = (String) ( fileNames.elementAt( k ) );
                        int idx = fName.lastIndexOf( "." );

                        if ( fName.substring( 0, idx ).equals( imgName ) ) {
                            break;
                        }
                    }
                    for ( int x = 0; x < ( (Vector) ( voiNames.elementAt( k ) ) ).size(); x++ ) {
                        fileNameIn = (String) ( ( (Vector) ( voiNames.elementAt( k ) ) ).elementAt( x ) );
                        index = fileNameIn.lastIndexOf( File.separatorChar );
                        directory = fileNameIn.substring( 0, index + 1 );
                        fileName = fileNameIn.substring( index + 1, fileNameIn.length() );
                        fileVOI = new FileVOI( fileName, directory, image );
                        voi = fileVOI.readVOI();
                        // image.registerVOI(voiNames[currVoiIndex].elementAt(i));
                        for ( int j = 0; j < voi.length; j++ ) {
                            image.registerVOI( voi[j] );
                        }
                    }

                }
                image.notifyImageDisplayListeners();
            } catch ( Exception e ) {
                e.printStackTrace();
                MipavUtil.displayError( "Error in script file near \"OpenVOI\"\n" );
                isRunning = false;
                return false;
            }
        } else if ( command.equals( "OpenAllVOIs" ) ) {
            String name = null;

            try {
                name = (String) variableTable.get( getNextString() );
                getFrameFromName( name ).loadAllVOIs( false );

                // this code selects the top VOI so that any algorithms run that require a selected
                // VOI (like extract surface) will run on the first VOI selected.  other algorithms,
                // that just run on all VOIs (Gaussian blur) will ignore the fact that the VOI is selected.
                ViewVOIVector VOIs = UI.getRegisteredImageByName( name ).getVOIs();

                VOIs.VOIAt( 0 ).setAllActive( true );
                getFrameFromName( name ).getComponentImage().setVOI_IDs( VOIs.VOIAt( 0 ).getID(),
                        VOIs.VOIAt( 0 ).getUID() );
                getFrameFromName( name ).getComponentImage().fireVOISelectionChange( VOIs.VOIAt( 0 ),
                        ( (VOIBase) VOIs.VOIAt( 0 ).getCurves()[getFrameFromName( name ).getComponentImage().getSlice()].elementAt(
                        0 ) ) );
                ;
            } catch ( Exception e ) {
                MipavUtil.displayError(
                        "Error in script file near \"OpenAllVOIs\".  Image " + name
                        + " not found or error in options.\n" );
                isRunning = false;
                return false;
            }
        } else if ( command.equals( "SaveAllVOIs" ) ) {
            try {
                String var1 = getNextString();
                String name = (String)variableTable.get(var1);
                
                ModelImage image = UI.getRegisteredImageByName(name);
                ViewJFrameImage frame = image.getParentFrame();
                
                frame.saveAllVOIs();
            } catch ( Exception e ) {
                MipavUtil.displayError( "Error in script file near \"SaveAllVOIs\"." );
                isRunning = false;
                return false;
            }
        } else if ( command.equals( "SaveAllVOIsTo" ) ) {
            try {
                String var1 = getNextString();
                String name = (String)variableTable.get(var1);
                
                ModelImage image = UI.getRegisteredImageByName(name);
                ViewJFrameImage frame = image.getParentFrame();
                
                String saveDir = getNextString();
                frame.saveAllVOIsTo(saveDir);
            } catch ( Exception e ) {
                MipavUtil.displayError( "Error in script file near \"SaveAllVOIs\"." );
                isRunning = false;
                return false;
            }
        } else if ( command.equals( "Clone" ) ) {
            try {
                String var1 = getNextString();
                String var2 = getNextString();
                String name1 = (String) variableTable.get( var1 );
                ModelImage image = UI.getRegisteredImageByName( name1 );

                ViewJFrameImage imageFrameClone;
                ModelImage clonedImage;

                clonedImage = (ModelImage) image.clone();
                // the clone method has been changed so that the
                // existing name is appended with "_clone" ... so
                // don't need to set this image name
                // clonedImage.setImageName("Cloned");
                variableTable.put( var2, clonedImage.getImageName() );

                imageFrameClone = new ViewJFrameImage(clonedImage);
            } catch ( Exception e ) {
                MipavUtil.displayError( "Error in script file near \"Clone\"." );
                isRunning = false;
                return false;
            }
        } else if ( command.equals( "CollectGarbage" ) || command.equals( "UpdateMemory" ) ) {
            System.gc();
        } else if ( command.equals( "VOI_to_BinaryMask" ) ) {
            try {
                String var1 = getNextString();
                String var2 = getNextString();
                String name = (String) variableTable.get( var1 );

                ModelImage image, maskImage;

                image = UI.getRegisteredImageByName( name );
                try {
                    maskImage = image.generateBinaryImage();
                    if ( maskImage != null ) {
                        maskImage.setImageName( "Binary image" );
                        new ViewJFrameImage(maskImage, null, null, false );
                        this.putVariable(var2, maskImage.getImageName());
                    }
                } catch ( OutOfMemoryError error ) {
                    MipavUtil.displayError( "Out of memory: unable to open new frame" );
                    isRunning = false;
                    throw error;
                }
            } catch ( Exception e ) {
                MipavUtil.displayError( "Error in script file near \"VOI_to_BinaryMask\"." );
                isRunning = false;
                return false;
            }
        } else if ( command.equals( "VOI_to_ShortMask" ) ) {
            try {
                String var1 = getNextString();
                String var2 = getNextString();
                String name = (String) variableTable.get( var1 );

                ModelImage image, shortImage;

                image = UI.getRegisteredImageByName( name );
                try {
                    shortImage = image.generateShortImage( 1 );
                    if ( shortImage != null ) {
                        shortImage.setImageName( "Short image" );
                        new ViewJFrameImage( shortImage, null, null, false );
                        this.putVariable(var2, shortImage.getImageName());
                    }
                } catch ( OutOfMemoryError error ) {
                    MipavUtil.displayError( "Out of memory: unable to open new frame" );
                    isRunning = false;
                    throw error;
                }
            } catch ( Exception e ) {
                MipavUtil.displayError( "Error in script file near \"VOI_to_ShortMask\"." );
                isRunning = false;
                return false;
            }
        } else if ( command.equals( "VOI_to_UnsignedByteMask" ) ) {
            try {
                String var1 = getNextString();
                String var2 = getNextString();
                String name = (String) variableTable.get( var1 );

                ModelImage image, ubyteImage;

                image = UI.getRegisteredImageByName( name );
                try {
                    ubyteImage = image.generateUnsignedByteImage( 1 );
                    if ( ubyteImage != null ) {
                        ubyteImage.setImageName( "UBYTE image" );
                        new ViewJFrameImage( ubyteImage, null, null, false );
                        this.putVariable(var2, ubyteImage.getImageName());
                    }
                } catch ( OutOfMemoryError error ) {
                    MipavUtil.displayError( "Out of memory: unable to open new frame" );
                    isRunning = false;
                    throw error;
                }
            } catch ( Exception e ) {
                MipavUtil.displayError( "Error in script file near \"VOI_to_UnsignedByteMask\"." );
                isRunning = false;
                return false;
            }
        } else if (command.equals("MaskToVOI")) {
            try {
                String var1 = getNextString();
                String name = (String) variableTable.get( var1 );

                ModelImage image = UI.getRegisteredImageByName( name );
                ViewJFrameImage frame = image.getParentFrame();
                
                frame.actionPerformed(new ActionEvent(frame, 0, command));
            } catch ( Exception e ) {
                MipavUtil.displayError( "Error in script file near \"MaskToVOI\"." );
                isRunning = false;
                return false;
            }
        } else if (command.equals("MaskToPaint")) {
            try {
                String var1 = getNextString();
                String name = (String) variableTable.get( var1 );

                ModelImage image = UI.getRegisteredImageByName( name );
                ViewJFrameImage frame = image.getParentFrame();
                
                frame.actionPerformed(new ActionEvent(frame, 0, command));
            } catch ( Exception e ) {
                MipavUtil.displayError( "Error in script file near \"MaskToPaint\"." );
                isRunning = false;
                return false;
            }
        } else if (command.equals("PaintToVOI")) {
            try {
                String var1 = getNextString();
                String name = (String) variableTable.get( var1 );

                ModelImage image = UI.getRegisteredImageByName( name );
                ViewJFrameImage frame = image.getParentFrame();
                
                frame.actionPerformed(new ActionEvent(frame, 0, command));
            } catch ( Exception e ) {
                MipavUtil.displayError( "Error in script file near \"PaintToVOI\"." );
                isRunning = false;
                return false;
            }
        } else if (command.equals("PaintToShortMask")) {
            try {
                String var1 = getNextString();
                String var2 = getNextString();
                String name = (String) variableTable.get( var1 );

                ModelImage image = UI.getRegisteredImageByName( name );
                ViewJFrameImage frame = image.getParentFrame();
                
                String maskImageName = frame.getComponentImage().commitPaintToMask();
                
                this.putVariable(var2, maskImageName);
            } catch ( Exception e ) {
                MipavUtil.displayError( "Error in script file near \"PaintToShortMask\"." );
                isRunning = false;
                return false;
            }
        }
        else {
            // some algorithm which we try to load dynamically
            try {
                ScriptableInterface scriptable = loadScriptableClass( command );

                scriptable.scriptRun( this );
            } catch ( ClassNotCreatedException e ) {
                MipavUtil.displayError( "Error loading class needed to execute script command: " + command );
                Preferences.debug( e.getMessage() );
                isRunning = false;
                return false;
            } catch ( IllegalArgumentException e ) {
                MipavUtil.displayError( "Error in arguments to script command " + command );
                isRunning = false;
                return false;
            } catch ( Exception e ) {
                MipavUtil.displayError( "Error encountered executing script command " + command );
                e.printStackTrace();
                isRunning = false;
                return false;
            }
        }

        return true;
    }

    /**
     * Get the class which should be used to run a specific algorithm command.
     * @param algoString the which algorithm to run
     * @throws ClassNotCreatedException if there was a problem instanciating the class
     * @return ScriptableBase an instance of the class indicated by the <code>algoString</code>
     */
    protected ScriptableInterface loadScriptableClass( String algoString )
        throws ClassNotCreatedException {
        ScriptableInterface scriptable;

        // dynamic loading of the algorithm classes.
        try {
            Class scriptableClass = Class.forName( "gov.nih.mipav.view.dialogs.JDialog" + algoString );
            scriptable = (ScriptableInterface) scriptableClass.newInstance();
        } catch ( ClassNotFoundException e ) {
            try {
                // see if it's a plugin

                Class scriptableClass = Class.forName( algoString );
                System.err.println("Got here");
                scriptable = (ScriptableInterface) scriptableClass.newInstance();
            } catch ( ClassCastException err ) {
                // plugin does not implement scriptable interface
                throw new ClassNotCreatedException( err.getMessage() + " does not allow itself to be scripted.  See ScriptableInterface." );
            } catch ( IllegalAccessException err ) {
                throw new ClassNotCreatedException( "Access denied -- " + err.getMessage() );
            } catch ( InstantiationException err ) {
                throw new ClassNotCreatedException( "Unable to instantiate class " + err.getMessage() + ". An empty constructor is likely needed." );
            } catch ( Exception err ) {
                // can't load class
                throw new ClassNotCreatedException( err.getMessage() );
            }
        } catch ( IllegalAccessException e ) {
            throw new ClassNotCreatedException( "Access denied -- " + e.getMessage() );
        } catch ( InstantiationException e ) {
            throw new ClassNotCreatedException( "Unable to instantiate class " + e.getMessage() + ". An empty constructor is likely needed." );
        } catch ( ClassCastException e ) {
            throw new ClassNotCreatedException( "JDialog" + algoString + " does not allow itself to be scripted.  See ScriptableInterface." );
        }

        return scriptable;
    }

    /**
     * Set the saved image file name from the cmd line script.
     * @param _name  image file name
     */
    public void setSavedImageFileName(String _name ) {
      savedImageFileName = _name;
    }

    /**
     * Function for algorithm dialogs to get the image they are supposed to work on.
     * @param var the image key in the variable table
     * @return ModelImage a reference to the requested image
     */
    public ModelImage getImage( String var ) {
        return UI.getRegisteredImageByName( (String) variableTable.get( var ) );
    }

    /**
     * Put a variable's name in the variable table.
     * @param key the variable key
     * @param val the variable name
     */
    public void putVariable( String key, String val ) {
        variableTable.put( key, val );
    }

    /**
     * Register the active image into the active images table
     * @param key  image name
     */
    public void putActiveVar( String key ) {
        if ( activeImagesTable.get( key ) == null ) {
            String varName = "$active" + activeImageNum;

            activeImageNum++;
            activeImagesTable.put( key, varName );
        }
    }

    /**
     * Get the active image name from the active images table
     * @param varName  $active#
     * @return  the corresponding active image name.
     */
    public Object getActiveImgName( String varName ) {
        if ( activeImagesTable.containsValue( varName ) ) {
            Enumeration keys = activeImagesTable.keys();

            while ( keys.hasMoreElements() ) {
                String name = (String) keys.nextElement();

                if ( activeImagesTable.get( name ).equals( varName ) ) {
                    return name;
                }
            }
            return null;
        }
        return null;
    }

    /**
     *	Gets the frame in the user interface which holds the image with image name <code>name</code>.
     *	@param name		Image name.
     */
    protected ViewJFrameImage getFrameFromName( String name ) {

        Vector imageFrames = UI.getImageFrameVector();
        String test;

        for ( int i = 0; i < imageFrames.size(); i++ ) {
            test = ( (ViewJFrameImage) ( imageFrames.elementAt( i ) ) ).getComponentImage().getActiveImage().getImageName();
            if ( name.equals( test ) ) {
                return ( (ViewJFrameImage) ( imageFrames.elementAt( i ) ) );
            }
        }
        return null;
    }

    /**
     *	Reads in the necessary file info to read a RAW file.
     *	@return	The file info necessary to read in the RAW file.
     */
    protected FileInfoXML getRawFileInfo( boolean multiFile ) {
        FileInfoXML fileInfo = null;

        try {
            getNextString(); // "Attributes"
            if ( multiFile ) {
                fileInfo = new FileInfoImageXML( (String) fileNames.elementAt( currFileIndex ),
                        (String) fileDirs.elementAt( currFileIndex ), FileBase.RAW_MULTIFILE );
            } else {
                fileInfo = new FileInfoImageXML( (String) fileNames.elementAt( currFileIndex ),
                        (String) fileDirs.elementAt( currFileIndex ), FileBase.RAW );
            }
            fileInfo.setDataType( getNextInteger() );
            fileInfo.setEndianess( getNextBoolean() );
            fileInfo.setOffset( getNextInteger() );
            int length = getNextInteger();
            int[] extents = new int[length];
            float[] res = new float[length];
            int[] measure = new int[length];

            for ( int i = 0; i < length; i++ ) {
                extents[i] = getNextInteger();
                res[i] = getNextFloat();
                measure[i] = getNextInteger();
            }
            fileInfo.setExtents( extents );
            fileInfo.setUnitsOfMeasure( measure );
            fileInfo.setResolutions( res );
            return fileInfo;
        } catch ( Exception e ) {
            MipavUtil.displayError(
                    "Error in the formatting in script file.\nCheck RAW attributes of "
                            + ( (String) fileDirs.elementAt( currFileIndex ) )
                            + ( (String) fileNames.elementAt( currFileIndex ) ) );
            return null;
        }
    }

    /**
     *      	*	Sets up the write options for a save call.  Reads in the necessary information from the string
     *      	*	tokenizer.
     *      	*	@param name		Name of the image to save.
     *      	*	@param saveAs	Whether this file options should be a save or save as call.
     *      	*	@return			New structure holding the file write options.
     */
    protected FileWriteOptions getWriteOptions( String name, boolean saveAs )
        throws Exception {

        FileWriteOptions opts;

        if ( activeImage ) {
            opts = new FileWriteOptions( savePrefix + stripExt( name ) + saveSuffix, UI.getDefaultDirectory(), saveAs );
        } else {
            opts = new FileWriteOptions( savePrefix + stripExt( name ) + saveSuffix,
                    (String) fileDirs.elementAt( currFileIndex ), saveAs );
        }
        ModelImage image = UI.getRegisteredImageByName( name );
        FileIO fileIO = new FileIO();
        int num;
        int fileType;

        opts.setActiveImage( activeImage );

        String tempString = getNextString();
        String ext = tempString;
        String root, fname;
        int iExt;

        if ( tempString.charAt( 0 ) == '.' ) { // script provides an extension, not a file name
            ext = tempString;

            fname = savePrefix + stripExt( name ) + saveSuffix + ext;
            // Check if file name exists.  If it does append a saveIndex string.
            File fTemp = new File( UI.getDefaultDirectory(), fname );

            while ( fTemp.exists() ) {
                saveIndex++;
                // System.out.println(fname +" already exists. ");
                Preferences.debug( fname + " already exists. " );
                fname = savePrefix + stripExt( name ) + saveSuffix + "_" + Integer.toString( saveIndex ) + ext;
                fTemp = null;
                fTemp = new File( UI.getDefaultDirectory(), fname );
                Preferences.debug( "file will be named " + fname + "." );
            }

            fTemp = null;
        } else { // script provides a file name, so assume that user wants that name
            fname = tempString;
            File fTemp = new File( UI.getDefaultDirectory(), fname );

            while ( fTemp.exists() ) {
                Preferences.debug( fname + " already exists. " );
                saveIndex++;
                iExt = fname.lastIndexOf( '.' );
                root = fname.substring( 0, iExt );
                ext = fname.substring( iExt, fname.length() );
                fname = root + "_" + Integer.toString( saveIndex ) + ext;
                fTemp = null;
                fTemp = new File( UI.getDefaultDirectory(), fname );
                Preferences.debug( "file will be named " + fname + "." );
            }
            fTemp = null;
        }

        if ( activeImage ) {
            opts = new FileWriteOptions( fname, UI.getDefaultDirectory() + File.separator, saveAs );
        } else {
            opts = new FileWriteOptions( fname, (String) fileDirs.elementAt( currFileIndex ) + File.separator, saveAs );
        }

        opts.setActiveImage( activeImage );
        fileType = fileIO.getFileType( opts.getFileName(), opts.getFileDirectory() );
        Preferences.debug( "File type is: " + fileType );
        opts.setFileType( fileType );
        opts.setPackBitEnabled(
                ( fileType == FileBase.TIFF )
                        && ( ( image.getFileInfo( 0 ).getDataType() == ModelStorageBase.BYTE )
                                || ( image.getFileInfo( 0 ).getDataType() == ModelStorageBase.UBYTE ) ) );

        num = tokens.countTokens();

        if ( num != 0 && opts.isPackBitEnabled() ) {
            opts.setWritePackBit( getNextBoolean() );
        }

        num = tokens.countTokens();
        // Defaults.  So save entire range of images.  For MINC and TIFF, set respective defaults and set time slice to 0.
        if ( num == 0 ) {
            if ( image.getNDims() == 3 ) {
                opts.setBeginSlice( 0 );
                opts.setEndSlice( image.getExtents()[2] - 1 );
                if ( fileType == FileBase.TIFF ) {
                    opts.setWritePackBit( false );
                    opts.setMultiFile( false );
                } else if ( fileType == FileBase.MINC ) {
                    JDialogSaveMinc minc = new JDialogSaveMinc( UI.getMainFrame(), image.getFileInfo( 0 ), opts );

                    opts = minc.setOptionsDefault();
                }
            } else if ( image.getNDims() == 4 ) {
                opts.setBeginSlice( 0 );
                opts.setEndSlice( image.getExtents()[2] - 1 );
                opts.setBeginTime( 0 );
                opts.setEndTime( image.getExtents()[3] - 1 );
                opts.setTimeSlice( 0 );
                if ( fileType == FileBase.TIFF ) {
                    opts.setWritePackBit( false );
                    opts.setMultiFile( false );
                } else if ( fileType == FileBase.MINC ) {
                    JDialogSaveMinc minc = new JDialogSaveMinc( UI.getMainFrame(), image.getFileInfo( 0 ), opts );

                    opts = minc.setOptionsDefault();
                }
            }
        } // if two more tokens, should be integers indicating what slice range to write on 3D image
        else if ( num == 2 && image.getNDims() == 3 ) {
            opts.setBeginSlice( getNextInteger() );
            opts.setEndSlice( getNextInteger() );
        } // if four more tokens and 3D TIFF file, ints indicating range, then ints for multi file save
        else if ( num == 4 && image.getNDims() == 3 && fileType == FileBase.TIFF ) {
            opts.setBeginSlice( getNextInteger() );
            opts.setEndSlice( getNextInteger() );
            opts.setStartNumber( getNextInteger() );
            opts.setDigitNumber( getNextInteger() );
            opts.setMultiFile( true );
        } // if four more tokens and 4D image, ints indicating range of slices and time
        else if ( num == 4 && image.getNDims() == 4 ) {
            opts.setBeginSlice( getNextInteger() );
            opts.setEndSlice( getNextInteger() );
            opts.setBeginTime( getNextInteger() );
            opts.setEndTime( getNextInteger() );
        } // if three more tokens and 4D image saved as TIFF or MINC, range and time slice.
        else if ( num == 3 && image.getNDims() == 4 && ( fileType == FileBase.TIFF || fileType == FileBase.MINC ) ) {
            opts.setBeginSlice( getNextInteger() );
            opts.setEndSlice( getNextInteger() );
            opts.setTimeSlice( getNextInteger() );
        } // if five more tokens and 4D image saved as TIFF, multi file save
        else if ( num == 5 && image.getNDims() == 4 && fileType == FileBase.TIFF ) {
            opts.setBeginSlice( getNextInteger() );
            opts.setEndSlice( getNextInteger() );
            opts.setTimeSlice( getNextInteger() );
            opts.setStartNumber( getNextInteger() );
            opts.setDigitNumber( getNextInteger() );
            opts.setMultiFile( true );
        } // if 12 and 3D image saved as MINC, these are MINC variables
        else if ( num == 12 && image.getNDims() == 3 && fileType == FileBase.MINC ) {
            opts.setBeginSlice( getNextInteger() );
            opts.setEndSlice( getNextInteger() );
            opts.setOrientation( getNextInteger() );
            opts.setXStart( getNextFloat() );
            opts.setXSpace( getNextFloat() );
            opts.setYStart( getNextFloat() );
            opts.setYSpace( getNextFloat() );
            opts.setZStart( getNextFloat() );
            opts.setZSpace( getNextFloat() );
            opts.setLeftToRight( getNextBoolean() );
            opts.setPosToAnt( getNextBoolean() );
            opts.setInfToSup( getNextBoolean() );
        } // if 13 and 4D image saved as MINC, these are MINC variables
        else if ( num == 13 && image.getNDims() == 4 && fileType == FileBase.MINC ) {
            opts.setBeginSlice( getNextInteger() );
            opts.setEndSlice( getNextInteger() );
            opts.setTimeSlice( getNextInteger() );
            opts.setOrientation( getNextInteger() );
            opts.setXStart( getNextFloat() );
            opts.setXSpace( getNextFloat() );
            opts.setYStart( getNextFloat() );
            opts.setYSpace( getNextFloat() );
            opts.setZStart( getNextFloat() );
            opts.setZSpace( getNextFloat() );
            opts.setLeftToRight( getNextBoolean() );
            opts.setPosToAnt( getNextBoolean() );
            opts.setInfToSup( getNextBoolean() );
        } else if ( num == 1 ) {
            if ( image.getNDims() == 3 ) {
                opts.setBeginSlice( 0 );
                opts.setEndSlice( image.getExtents()[2] - 1 );
                opts.setAVICompression( getNextInteger() );
            }

        } else {
            throw new Exception( "Incorrect arguments after Save in script." );
        }
        opts.setOptionsSet( true );

        opts.setIsScript( true );
        return opts;
    }

    /**
     *	Helper method to strip the image name of the extension, so when we save we don't have double extensions
     *	(like genormcor.img.tif).
     *	@param name		Original name.
     *	@return			Name without extension, or original name if there was no extension.
     */
    protected String stripExt( String name ) {
        int index = name.lastIndexOf( "." );

        if ( index != -1 ) {
            return name.substring( 0, index );
        } else {
            return name;
        }

    }

    /**
     *	Get the next token from the string tokenizer for the current line and changes it into an integer.
     *	@return		The next token from the tokenizer, changed into type int.
     */
    public int getNextInteger()
        throws Exception {
        int value;

        try {
            value = ( Integer.valueOf( tokens.nextToken() ) ).intValue();
            return value;
        } catch ( Exception e ) {
            isRunning = false;
            throw e;
        }
    }

    /**
     *	Get the next token from the string tokenizer for the current line and changes it into a boolean.
     *	@return		The next token from the tokenizer, changed into type boolean.
     */
    public boolean getNextBoolean()
        throws Exception {
        boolean value;

        try {
            value = ( Boolean.valueOf( tokens.nextToken() ) ).booleanValue();
            return value;
        } catch ( Exception e ) {
            // isRunning = false;
            // MipavUtil.displayError("Error in the formatting of the script file; could not get boolean.");
            throw e;
        }
    }

    /**
     *      	*	Get the next token from the string tokenizer for the current line and changes it into a float.
     *      	*	@return		The next token from the tokenizer, changed into type float.
     */
    public float getNextFloat()
        throws Exception {
        float value;

        try {
            value = ( Float.valueOf( tokens.nextToken() ) ).floatValue();
            return value;
        } catch ( Exception e ) {
            // isRunning = false;
            // MipavUtil.displayError("Error in the formatting of the script file; could not get float.");
            throw e;
        }
    }

    /**
     *      	*	Get the next token from the string tokenizer for the current line and changes it into a double.
     *      	*	@return		The next token from the tokenizer, changed into type double.
     */
    public double getNextDouble()
        throws Exception {
        double value;

        try {
            value = ( Double.valueOf( tokens.nextToken() ) ).doubleValue();
            return value;
        } catch ( Exception e ) {
            // isRunning = false;
            // MipavUtil.displayError("Error in the formatting of the script file; could not get double.");
            throw e;
        }
    }

    /**
     *      	*	Get the next token from the string tokenizer for the current line.
     *      	*	@return		The next token from the tokenizer.
     */
    public String getNextString()
        throws Exception {
        String value;

        value = tokens.nextToken();
        return value;
    }

    class VariableHashtable extends Hashtable {

        // --------------------------------------------------------
        // Constructors
        // --------------------------------------------------------

        /**
         *   Constructs a new hashtable with the same mappings
         *   as the given Map. The hashtable is created with a
         *   capacity of twice the number of entries in the given
         *   Map or 11 (whichever is greater), and a default load
         *   factor, which is 0.75.
         *   @param map      Map to base the hashtable on
         */
        public VariableHashtable( Map t ) {
            super( t );
        }

        /**
         *   Constructs a new, empty hashtable with the specified
         *   initial capacity and the specified load factor.
         *
         *   @param capacity     initial capacity of the hashtable
         *   @param loadFactor   the load factor of the hashtable
         */
        public VariableHashtable( int capacity, float loadFactor ) {
            super( capacity, loadFactor );
        }

        /**
         *   Constructs a new, empty hashtable with the specified
         *   initial capacity and default load factor, which is 0.75.
         *
         *   @param  capacity    initial capacity of the hashtable
         */
        public VariableHashtable( int capacity ) {
            super( capacity );
        }

        /**
         *   Constructs a new, empty hashtable with a default capacity
         *   and load factor, which is 0.75.
         */
        public VariableHashtable() {
            super();
        }

        // --------------------------------------------------------
        // Other Methods
        // --------------------------------------------------------

        /**
         *   Override the get method for Hashtable.
         *   Get the image assigned to the variable and return it.
         *   A special case is the $active variable which returns the
         *   name of the current active image.
         *
         *   @param  key -   the key to the hashtable
         */
        public Object get( Object key ) {

            Object name = null;
            String var = (String) key;

            if ( key == null || var.equals( "null" ) || var.equals( "$active" ) ) {
                // check and see if $active was stored in the variable table
                // if it was, then return that name
                if ( super.containsKey( new String( "$active" ) ) ) {
                    name = (String) super.get( key );
                } else {
                    // otherwise, add the name of the active image to the varible table
                    // and then return the name of the active image
                    name = (String) UI.getActiveImageFrame().getComponentImage().getActiveImage().getImageName();
                    super.put( new String( "$active" ), name );
                }
            } else {
                // return the name from the variable table
                name = super.get( key );
            }
            return name;

        } // end get()
    } // end class VariableHashtable


    /**
     * Exception to group together all of the ones which might be generated while loading a class dynamically.
     */
    protected class ClassNotCreatedException extends Exception {

        /**
         * Constructor with a message for this error.
         * @param message the error message
         */
        ClassNotCreatedException( String message ) {
            super( message );
        }
    }

} // end class AlgorithmScriptParser
