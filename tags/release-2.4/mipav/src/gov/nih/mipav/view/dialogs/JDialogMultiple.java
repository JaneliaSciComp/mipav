package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.model.algorithms.AlgorithmScriptParser;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;

import java.awt.event.*;
import java.awt.*;
import java.io.*;
import java.util.*;


/**
 *   This is a dialog to run a script on multiple files.  It allows the user to choose
 *   multiple files on which to run the script, then choose the script, then choose a file
 *   extension to save result images, if necessary.  It calls the script parser on each
 *   image file after setting up appropriate variables in the parser.  The result image will be saved in the directory
 *   that the original image was loaded from.  However, the user can specify a different file
 *   format save by changing the <code>saveExt</code> field.  This should be used with caution, because
 *   if the script does not contain the necessary variables for the chosen file format, an error
 *   will be displayed and the script will not save the file.  However, that's only really an issue
 *   with .tif images, which require more information than most image formats.
 *
 *   @version    1.0 June 1, 2001
 *   @author     Neva Cherniavsky
 *   @see	AlgorithmScriptParser
 *
 */
public class JDialogMultiple extends JDialogBase
    implements ActionListener, ListSelectionListener {

    /** Image panel that holds the image list. */
    private JPanel imageSelectPanel;

    /** The script panel that let the user to choose which script file to run. */
    private JPanel scriptPanel;

    /** Button panel that hold the add, remove buttons of the image list. */
    private JPanel buttonsPanel;

    /** Button panel that holds the voi add, remove buttons. */
    private JPanel voiButtonsPanel;

    /** Button panel that holds the back, next and finish buttons. */
    private JPanel buttons2Panel;

    /** Script selection panel hold the prefix, suffix textfields. */
    private JPanel scriptSelectPanel;

    /** The main panel that holds the all the panels. */
    private JPanel contentPanel;

    /** Panel that holds the image list and voi list */
    private JPanel imagesVOIsPanel;

    /** Image list add button. */
    private JButton addButton;

    /** Image list add multiple button */
    private JButton addMultipleButton;

    /** Image list remove button. */
    private JButton removeButton;

    /** VOI list add button. */
    private JButton voiAddButton;

    /** VOI list remove button. */
    private JButton voiRemoveButton;

    /** Load script button. */
    private JButton chooseButton;

    /** next dialog button. */
    private JButton nextButton;

    /** previous dialog button. */
    private JButton backButton;

    /** Script name textfield. */
    private JTextField scriptName;

    /** Scroll pane that holds the image list. */
    private JScrollPane scrollPane;

    /** Scroll pane that holds the VOI list. */
    private JScrollPane voiPane;

    /** Image list. */
    private JList imagesList;

    /** VOI list. */
    private JList voiList;

    /** Prefix save label. */
    private JLabel saveLabel;

    /** next step label that guides the user through the script running. */
    private JLabel nextLabel;

    /** Finish step label guides the user through the final step. */
    private JLabel finishLabel;

    /** Save prefix textfield. */
    private JTextField savePrefix;

    /** Save suffix label field. */
    private JLabel saveLabelSuffix;

    /** Save suffix text field. */
    private JTextField saveSuffix;

    /** Image list panel border. */
    private TitledBorder imgBorder;

    /** parent user interface reference. */
    private ViewUserInterface userInterface;

    /** Image set vector that hold a list of sets of images. */
    private Vector[] imageSet;

    /** VOI set vector that hold a list of sets of VOIs. */
    private VOISetVector[] voiSet;

    /** Script file name. */
    private String scriptFileName;

    /** Script file directory. */
    private String scriptDirectory;

    /** number of images. */
    private int numImages;

    /** Current highlighted image index. */
    private int currImageIndex = -1;

    /** Current highlighted image set index. */
    private int currImageSet = 0;

    /** Vector that hold the currently selected index list. */
    private Vector selectIndex;

    /** VOI panel view port. */
    private JViewport voiViewPort;

    /** VOI panel dimension. */
    private Dimension voiPaneDimen;

    /** Script parsing types, vertical parsing of horizontal passing. */
    static int VERTICAL_PARSE = 0, HORIZONTAL_PARSE = 1;

    /** The script parse type. */
    private int parseType = HORIZONTAL_PARSE;

    /** Tells whether this script has been converted to non-active */
    private boolean didConvert = false;

    /** String with converted script */
    private String convertedString = null;

    /**
     *	Constructs a new dialog and displays it.
     *  @param title         Title of dialog frame.
     *  @param ui	 User interface (dialog uses main frame from UI as parent).
     */
    public JDialogMultiple( String title, ViewUserInterface ui ) {
        super( ui.getMainFrame(), false );
        setModal( false );
        setResizable( true );
        userInterface = ui;
        init( title );
    }

    /**
     * Reacts to the following actions:<br>
     * Add - adds a new image to the list of images by calling an open file dialog<br>
     * Remove - removes the selected image files from the list<br>
     * Choose - asks the user to choose a script file to run on the images<br>
     * Execute - executes the script by calling <code>AlgorithmScriptParser</code> on each
     *		 image in turn<br>
     * Cancel - closes the dialog without running the script
     *	@param e Event that called this function.
     */
    public void actionPerformed( ActionEvent e ) {
        String command = e.getActionCommand();
        //System.err.println(imagesVOIsPanel.getSize());
        if ( command.equals( "Add" ) ||
             command.equals("AddMultiple")) {

            boolean doMultiple = command.equals("AddMultiple");

            if ( Preferences.is(Preferences.PREF_USE_AWT) ) {
                FileDialog fd = new FileDialog( userInterface.getMainFrame(), "Choose image" );
                try {
                    fd.setDirectory( userInterface.getDefaultDirectory() );
                } catch ( Exception ex ) {
                    fd.setDirectory( System.getProperty( "user.dir" ) );
                }
                Dimension d = new Dimension( 700, 400 );

                fd.setSize( d );
                fd.setVisible(true);

                if ( fd.getFile() != null ) {
                    userInterface.setDefaultDirectory( fd.getDirectory() );

                    int numToAdd = 1;
                    if (doMultiple) {
                        boolean keepGoing = true;
                        String ans;
                        while(keepGoing) {
                            ans = JOptionPane.showInputDialog(null, "Number of times to add the image");
                            try {
                                numToAdd = Integer.parseInt(ans);
                                if (numToAdd > 0) {
                                    keepGoing = false;
                                }
                            } catch (Exception ex) {

                            }
                        }
                    }

                    for (int i = 0; i < numToAdd; i++) {
                        imageSet[currImageIndex].addElement(new String(fd.getDirectory() + fd.getFile()));
                        voiSet[currImageIndex].getVOIVector().addElement(
                            new VOIVector(new String(fd.getDirectory() + fd.getFile())));
                    }
                    imagesList.setListData( imageSet[currImageIndex] );
                    removeButton.setEnabled( true );

                    if ( ( currImageIndex + 1 ) == numImages ) {
                        nextButton.setEnabled( false );
                    } else {
                        nextButton.setEnabled(true);
                        nextButton.setToolTipText( "Click Next to add another image set." );
                    }

                    OKButton.setEnabled(enoughImages());

                } else {
                    return;
                }
            } else {
                JFileChooser chooser = new JFileChooser();

                chooser.setMultiSelectionEnabled( true );

                if ( userInterface.getDefaultDirectory() != null ) {
                    chooser.setCurrentDirectory( new File( userInterface.getDefaultDirectory() ) );
                } else {
                    chooser.setCurrentDirectory( new File( System.getProperties().getProperty( "user.dir" ) ) );
                }

                // first get the default filename filter from preferences
                int filter = Integer.parseInt( Preferences.getProperty( "FilenameFilter" ) );

                ViewImageFileFilter f1 = new ViewImageFileFilter( ViewImageFileFilter.GEN );
                ViewImageFileFilter f2 = new ViewImageFileFilter( ViewImageFileFilter.TECH );
                ViewImageFileFilter f3 = new ViewImageFileFilter( ViewImageFileFilter.MISC );
                ViewImageFileFilter f4 = new ViewImageFileFilter( ViewImageFileFilter.MICROSCOPY );

                chooser.addChoosableFileFilter( f1 );
                chooser.addChoosableFileFilter( f2 );
                chooser.addChoosableFileFilter( f4 );
                chooser.addChoosableFileFilter( f3 );
                switch ( filter ) {
                case ViewImageFileFilter.GEN:
                    chooser.setFileFilter( f1 );
                    break;

                case ViewImageFileFilter.TECH:
                    chooser.setFileFilter( f2 );
                    break;

                case ViewImageFileFilter.MISC:
                    chooser.setFileFilter( f3 );
                    break;

                case ViewImageFileFilter.MICROSCOPY:
                    chooser.setFileFilter( f4 );
                    break;

                default:
                    chooser.addChoosableFileFilter( new ViewImageFileFilter( filter ) );
                }
                int returnVal = chooser.showOpenDialog( this );

                if ( returnVal == JFileChooser.APPROVE_OPTION ) {
                    File[] files = chooser.getSelectedFiles();

                    int numToAdd = 1;
                    if (doMultiple) {
                        boolean keepGoing = true;
                        String ans;
                        while(keepGoing) {
                            ans = JOptionPane.showInputDialog(null, "Number of times to add the image");
                            try {
                                numToAdd = Integer.parseInt(ans);
                                if (numToAdd > 0) {
                                    keepGoing = false;
                                }
                            } catch (Exception ex) {

                            }
                        }
                    }

                    for (int j = 0; j < numToAdd; j++) {
                        for (int i = 0; i < files.length; i++) {
                            imageSet[currImageIndex].addElement(
                                new String(
                                    "" + chooser.getCurrentDirectory() + "" + File.separatorChar + ""
                                    + files[i].getName()));
                            voiSet[currImageIndex].getVOIVector().addElement(
                                new VOIVector(
                                    new String(
                                        "" + chooser.getCurrentDirectory() + "" + File.separatorChar + ""
                                        + files[i].getName())));
                        }
                    }
                    imagesList.setListData( imageSet[currImageIndex] );
                    imagesList.setSelectedIndex( 0 );
                    removeButton.setEnabled( true );
                    OKButton.setEnabled(enoughImages());
                    if ( ( currImageIndex + 1 ) == numImages ) {
                        nextButton.setEnabled( false );
                    } else {
                        nextButton.setEnabled(true);
                        nextButton.setToolTipText( "Click Next to add another image set." );
                    }


                } else {
                    return;
                }
                userInterface.setDefaultDirectory( String.valueOf( chooser.getCurrentDirectory() ) + File.separatorChar );

            }
        } else if ( command.equals( "AddVOI" ) ) {
            JFileChooser chooser = new JFileChooser();

            chooser.setMultiSelectionEnabled( true );

            if ( userInterface.getDefaultDirectory() != null ) {
                chooser.setCurrentDirectory( new File( userInterface.getDefaultDirectory() ) );
            } else {
                chooser.setCurrentDirectory( new File( System.getProperties().getProperty( "user.dir" ) ) );
            }

            // first get the default filename filter from preferences
            int filter = Integer.parseInt( Preferences.getProperty( "FilenameFilter" ) );

            chooser.addChoosableFileFilter( new ViewImageFileFilter( new String[] { "xml", "voi" } ) );
            int returnVal = chooser.showOpenDialog( this );

            int idx;

            if ( returnVal == JFileChooser.APPROVE_OPTION ) {
                File[] files = chooser.getSelectedFiles();

                for ( int j = 0; j < selectIndex.size(); j++ ) {
                    for ( int i = 0; i < files.length; i++ ) {
                        idx = ( (Integer) ( selectIndex.elementAt( j ) ) ).intValue();
                        ( (VOIVector) ( voiSet[currImageIndex].getVOIVector().get( idx ) ) ).getVOIs().addElement(
                                new String(
                                        "" + chooser.getCurrentDirectory() + "" + File.separatorChar + ""
                                        + files[i].getName() ) );

                    }
                }
                idx = ( (Integer) ( selectIndex.elementAt( 0 ) ) ).intValue();
                voiList.setListData( ( (VOIVector) ( voiSet[currImageIndex].getVOIVector().get( idx ) ) ).getVOIs() );
                voiRemoveButton.setEnabled( true );
                voiPane.setViewportView( voiList );
                voiViewPort = voiPane.getViewport();
            } else {
                return;
            }
            userInterface.setDefaultDirectory( String.valueOf( chooser.getCurrentDirectory() ) + File.separatorChar );

        } else if ( command.equals( "Next" ) ) {

            if ( scriptSelectPanel.isVisible() ) {
                int index;

                scriptFileName = scriptName.getText();
                index = scriptFileName.lastIndexOf( File.separatorChar );
                if ( index <= 0 ) {
                    MipavUtil.displayError( "Must select a script file." );
                    return;
                }

                scriptDirectory = scriptFileName.substring( 0, index + 1 ); // ends with File.separator
                scriptFileName = scriptFileName.substring( index + 1, scriptFileName.length() );
                AlgorithmScriptParser parse = new AlgorithmScriptParser( scriptFileName, scriptDirectory );

                // System.err.println("setting active image to false");
                parse.setActiveImage( false );

                //check to see if there are active images
                int numActive = parse.preParseActiveImages();

                //if there are active images we must convert the script
                // so that $active goes to $image and OpenImage is called
                //  for all active images
                if (numActive > 0) {
                    parse = null;
                    convertedString = convertFromActive(scriptFileName, scriptDirectory);

                    parse = new AlgorithmScriptParser(convertedString);
                    didConvert = true;

                }


                numImages = parse.preParse();
                if ( !parse.hasOpenVOI() ) {
                    voiPane.setViewport( null );
                    voiPane.setPreferredSize( voiPaneDimen );
                    voiAddButton.setEnabled( false );
                    voiRemoveButton.setEnabled( false );
                } else {
                    voiPane.setViewport( voiViewPort );
                    voiPane.setPreferredSize( voiPaneDimen );
                    voiAddButton.setEnabled( true );
                    voiRemoveButton.setEnabled( true );
                }
                currImageIndex = 0;
                imageSet = new Vector[numImages];
                for ( int i = 0; i < numImages; i++ ) {
                    imageSet[i] = new Vector();
                }
                voiSet = new VOISetVector[numImages];
                for ( int i = 0; i < numImages; i++ ) {
                    voiSet[i] = new VOISetVector();
                }

                // if no images are required, then don't display
                // the imageSelect panel.  Just enable the finish
                // button and return
                if ( numImages == 0 ) {
                    // if there are no active images, then this
                    // script can't be run -- so display an error
                    if ( userInterface.getActiveImageFrame() == null ) {
                        MipavUtil.displayError(
                                "This script requires an active image.\n"
                                        + "Please open an image before running this script." );
                        return;
                    }
                    //finishLabel.setText( "Click Finish to execute script." );
                    OKButton.setEnabled( true );


                    // disable the next button
                    //nextLabel.setText( " " );
                    nextButton.setEnabled( false );
                    return;
                }

                scriptSelectPanel.setVisible( false );
                imageSelectPanel.setVisible( true );
            } else {
                if ( ( currImageIndex + 1 ) == numImages ) {
                    currImageSet++;
                    currImageIndex = 0;
                } else {
                    currImageIndex++;
                }

            }

            nextButton.setEnabled(false);
            OKButton.setEnabled(enoughImages());

            if ( ( currImageSet == 0 || currImageIndex > 0 ) && currImageIndex != -1 ) {
                backButton.setEnabled( true );
            } else {
                backButton.setEnabled( false );
            }

            imgBorder.setTitle(
                    "Select image " + ( currImageIndex + 1 )
                    + " for this set (script runs once per image in list below)" );
            imageSelectPanel.repaint();
            // nextLabel.setText(" ");

            if ( numImages != 0 ) {
                removeButton.setEnabled( true );
            }

            if ( currImageIndex < imageSet.length ) {
                imagesList.setListData( imageSet[currImageIndex] );
                if ( imageSet[currImageIndex].size() != 0 ) {
                    imagesList.setSelectedIndex( 0 );
                }
                if ( ( voiSet[currImageIndex].getVOIVector().size() ) != 0 ) {
                    voiList.setListData( ( (VOIVector) ( voiSet[currImageIndex].getVOIVector().get( 0 ) ) ).getVOIs() );
                    // voiList.setSelectedIndex(0);
                } else {
                    voiList.setListData( new Vector() );
                }
                imageSelectPanel.setVisible( false );
                imageSelectPanel.setVisible( true );
            }
        } else if ( command.equals( "Back" ) ) {

            nextButton.setEnabled( true );

            if ( currImageIndex == 0 ) {
                scriptSelectPanel.setVisible( true );
                imageSelectPanel.setVisible( false );
                backButton.setEnabled( false );
                currImageIndex--;
                nextButton.setToolTipText( "" );
            } else {
                currImageIndex--;
                imagesList.setListData( imageSet[currImageIndex] );
                if ( imageSet[currImageIndex].size() != 0 ) {
                    imagesList.setSelectedIndex( 0 );
                }
                if ( ( voiSet[currImageIndex].getVOIVector().size() ) != 0 ) {
                    voiList.setListData( ( (VOIVector) ( voiSet[currImageIndex].getVOIVector().get( 0 ) ) ).getVOIs() );
                    // voiList.setSelectedIndex(0);
                } else {
                    voiList.setListData( new Vector() );
                }
                nextButton.setToolTipText( "Click Next to add another image set." );
            }

            if ( ( currImageSet == 0 || currImageIndex > 0 ) && currImageIndex != -1 ) {
                backButton.setEnabled( true );
            } else {
                backButton.setEnabled( false );
            }

            OKButton.setEnabled(enoughImages());

            imgBorder.setTitle(
                    "Select image " + ( currImageIndex + 1 )
                    + " for this set (script runs once per image in list below)" );
            imageSelectPanel.repaint();
        } else if ( command.equals( "Remove" ) ) {
            int idx = 0;
            Object[] selections = imagesList.getSelectedValues();

            for ( int i = 0; i < selections.length; i++ ) {
                idx = imageSet[currImageIndex].indexOf( selections[i] );
                voiSet[currImageIndex].getVOIVector().remove( idx );
                imageSet[currImageIndex].removeElement( selections[i] );
            }
            imagesList.setListData( imageSet[currImageIndex] );
            if ( imageSet[currImageIndex].size() != 0 ) {
                imagesList.setSelectedIndex( 0 );
            }
            if ( ( voiSet[currImageIndex].getVOIVector().size() ) != 0 ) {
                voiList.setListData( ( (VOIVector) ( voiSet[currImageIndex].getVOIVector().get( 0 ) ) ).getVOIs() );
            } else {
                voiList.setListData( new Vector() );
            }

            OKButton.setEnabled(enoughImages());

        } else if ( command.equals( "RemoveVOI" ) ) {
            Object selection = imagesList.getSelectedValue();
            int idx = imageSet[currImageIndex].indexOf( selection );
            Object[] voiSelections = voiList.getSelectedValues();

            for ( int i = 0; i < voiSelections.length; i++ ) {
                ( (VOIVector) ( voiSet[currImageIndex].getVOIVector().get( idx ) ) ).getVOIs().removeElement(
                        voiSelections[i] );
            }
            voiList.setListData( ( (VOIVector) ( voiSet[currImageIndex].getVOIVector().get( idx ) ) ).getVOIs() );
        } else if ( command.equals( "Choose" ) ) {

            if ( Preferences.is(Preferences.PREF_USE_AWT) ) {
                FileDialog fd = new FileDialog( userInterface.getMainFrame(), "Choose script" );

                try {
                    fd.setDirectory( new File( Preferences.getScriptsDirectory() ).getPath() );
                } catch ( Exception ex ) {
                    fd.setDirectory( new File( System.getProperty( "user.dir" ) ).getPath() );
                }
                Dimension d = new Dimension( 700, 400 );

                fd.setSize( d );

                fd.setVisible(true);

                if ( fd.getFile() != null ) {
                    Preferences.setProperty( "ScriptsDir", fd.getDirectory() );
                    scriptName.setText( new String( "" + fd.getDirectory() + "" + fd.getFile() ) );
                } else {
                    return;
                }
            } else {
                JFileChooser chooser = new JFileChooser();
                chooser.setCurrentDirectory( new File( Preferences.getScriptsDirectory() ) );

                chooser.addChoosableFileFilter( new ViewImageFileFilter( ViewImageFileFilter.SCRIPT ) );

                int returnVal = chooser.showOpenDialog( this );

                if ( returnVal == JFileChooser.APPROVE_OPTION ) {
                    scriptDirectory = chooser.getCurrentDirectory().getPath();
                    scriptFileName = chooser.getSelectedFile().getName();
                    Preferences.setProperty( "ScriptsDir", chooser.getCurrentDirectory().getPath() );
                    scriptName.setText(
                            new String(
                                    "" + chooser.getCurrentDirectory() + "" + File.separatorChar + ""
                                    + chooser.getSelectedFile().getName() ) );
                } else {
                    return;
                }
            }
            nextButton.setEnabled(true);
        } else if ( command.equals( "OK" ) ) {

            if ( numImages == 0 ) {
                // this is an active script.. run it like as one
                ViewControlsScript scriptControls = new ViewControlsScript( userInterface );

                scriptControls.setScriptDirectory( scriptDirectory );
                scriptControls.setScriptFileName( scriptFileName );
                scriptControls.runScript();
                dispose();
                return;

            }
            if ( !enoughImages() ) {
                MipavUtil.displayError( "Need to have equal number of images in each set!" );
                return;
            }

            if ( savePrefix.getText().lastIndexOf( "." ) != -1 ) {
                MipavUtil.displayError(
                        "Prepended string cannot contain a \".\"  The file extension is set in the script file." );
                savePrefix.requestFocus();
                savePrefix.selectAll();
            }

            if ( saveSuffix.getText().lastIndexOf( "." ) != -1 ) {
                MipavUtil.displayError(
                        "Suffix string cannot contain a \".\"  The file extension is set in the script file." );
                saveSuffix.requestFocus();
                saveSuffix.selectAll();
            } else {
                dispose();
                if ( parseType == VERTICAL_PARSE ) {
                    runMultiple();
                } else if ( parseType == HORIZONTAL_PARSE ) {
                    runMultipleHorizontal();
                    // userInterface.setScriptRunning(false);
                }
            }
        } else if ( command.equals( "Vertical" ) ) {
            parseType = VERTICAL_PARSE;
        } else if ( command.equals( "Horizontal" ) ) {
            parseType = HORIZONTAL_PARSE;
        } else if ( command.equals( "Cancel" ) ) {
            dispose();
        } else if ( command.equals( "Help" ) ) {
            MipavUtil.showHelp( "10704" );
        }
    }

    /*
     * Returns true if there is a proper number of images for each set.
     */
    boolean enoughImages() {
        if ( numImages == 0 ) {
            return true;
        } else if (numImages == 1) {
            return (imageSet[0].size() > 0);
        }

        if ( imageSet.length <= 0 ) {
            System.err.println( "images.length is: " + imageSet.length );
            return true;
        }

        // int numSets = imageSet[0].size();
        int numSets = imageSet.length;

        if ( numSets == 0 ) {
            return false;
        }



        for (int i = 1; i < numImages; i++) {
            if (imageSet[i].size() == 0 ||
                imageSet[i].size() != imageSet[i-1].size()) {
                return false;
            }
        }

        return true;
    }

    /**
     * Converts the active script to a load-image script
     * @param fileName String file's name (or the actual script string)
     * @param dir String directory for script file (can be null if script string is used)
     * @return String converted non-active script
     */
    private String convertFromActive(String fileName, String dir) {

        //first count the total number of image registers used (# of different $image's

        BufferedReader instream;

        String command;
        StringTokenizer tokens;

        String newScriptString = new String();

        Hashtable table = new Hashtable();

        Vector holder = new Vector();

        int currentIndex = 1;

        try {
            instream = new BufferedReader(new FileReader(dir + fileName));
        }
        catch (Exception e) {
            System.err.println("not a file, trying as a string");

            instream = new BufferedReader(new StringReader(fileName));
        }

        try {
            String line;
            String changedLine = null;
            line = instream.readLine();
            while ( line != null ) {

                //remove all active images from the vector
                holder.removeAllElements();
                changedLine = "";
                if ( !line.startsWith( "#" ) ) {
                    tokens = new StringTokenizer( line );
                    try {
                        while ( tokens.hasMoreTokens() ) {
                            command = tokens.nextToken();
                            if ( command.startsWith( "$active" ) ) {

                                if (!table.containsKey(command)) {

                                    while(table.containsValue("$image" + currentIndex)) {
                                        currentIndex++;
                                    }
                                    table.put(command, new String("$image" + currentIndex));

                                    holder.addElement((String)table.get(command));
                                }

                                changedLine += table.get(command) + " ";

                            } else if (command.startsWith("$image")) {
                                //System.err.println("command is: " + command);
                                if (!table.containsKey(command)) {
                                    while(table.containsValue("$image" + currentIndex)) {
                                        currentIndex++;
                                    }
                                    table.put(command, new String("$image" + currentIndex));
                                }
                                changedLine += table.get(command) + " ";

                            } else {
                                changedLine += command + " ";
                            }
                        }
                    } catch ( NoSuchElementException e ) {
                        // Empty line, and we're okay with that.
                        break;
                    }

                    //add OpenImage for each active
                    for (int i = 0; i < holder.size(); i++) {
                        newScriptString += "OpenImage " + (String)holder.elementAt(i) + "\n";
                    }
                    newScriptString += changedLine + "\n";

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

      //  while(keys.hasMoreElements()) {
      //      command = (String)keys.nextElement();
      //      infoString = command + " has been changed to " + (String)table.get(command) + "\n" + infoString;
      //  }

       // MipavUtil.displayInfo(infoString);
        MipavUtil.displayInfo("Active script changed to:\n\n" + newScriptString);


        return newScriptString;
    }

    /**
     *  Initializes the dialog box and adds the components.
     *  @param title Title of the dialog box.
     */
    private void init( String title ) {
        setTitle( title );
        GridBagConstraints gbc = new GridBagConstraints();

        imagesList = new JList();
        imagesList.setCellRenderer( new MyCellRenderer() );
        imagesList.addListSelectionListener( this );
        voiList = new JList();
        voiList.setCellRenderer( new MyCellRenderer() );

        scrollPane = new JScrollPane( imagesList );

        addButton = new JButton( "Add" );
        addButton.addActionListener( this );
        addButton.setActionCommand( "Add" );
        addButton.setFont( serif12B );
        addButton.setPreferredSize( MipavUtil.defaultButtonSize );
        addButton.setMinimumSize( MipavUtil.defaultButtonSize );
        addButton.setToolTipText("Add a single image to the current set");

        addMultipleButton = new JButton( "Add x N" );
        addMultipleButton.addActionListener( this );
        addMultipleButton.setActionCommand( "AddMultiple" );
        addMultipleButton.setFont( serif12B );
        addMultipleButton.setPreferredSize( MipavUtil.defaultButtonSize );
        addMultipleButton.setMinimumSize( MipavUtil.defaultButtonSize );
        addMultipleButton.setToolTipText("Add multiple copies of the same image to the current set");

        removeButton = new JButton( "Remove" );
        removeButton.addActionListener( this );
        removeButton.setActionCommand( "Remove" );
        removeButton.setFont( serif12B );
        removeButton.setPreferredSize( MipavUtil.defaultButtonSize );
        removeButton.setMinimumSize( MipavUtil.defaultButtonSize );
        removeButton.setToolTipText("Remove image(s) from current set");

        voiPane = new JScrollPane( voiList );
        voiViewPort = voiPane.getViewport();
        voiPaneDimen = voiPane.getPreferredSize();

        // ruida
        imagesVOIsPanel = new JPanel();
        imagesVOIsPanel.setPreferredSize(new Dimension( 530, 210));
        imagesVOIsPanel.setLayout( new BoxLayout( imagesVOIsPanel, BoxLayout.X_AXIS ) );


        JPanel top = new JPanel();

        top.setLayout( new BoxLayout( top, BoxLayout.Y_AXIS ) );
        //top.setPreferredSize( new Dimension( 500, 210 ) );
        JPanel imageContainer = new JPanel();

        imageContainer.setLayout( new BorderLayout() );
        imageContainer.setPreferredSize( new Dimension( 290, 200 ) );
        imageContainer.setMinimumSize( new Dimension( 290, 200 ) );
        imageContainer.setBorder( BorderFactory.createTitledBorder( "Images" ) );
        imageContainer.add( scrollPane, BorderLayout.CENTER );

        buttonsPanel = new JPanel();
        buttonsPanel.add( addButton );
        buttonsPanel.add( addMultipleButton );
        buttonsPanel.add( removeButton );
        imageContainer.add( buttonsPanel, BorderLayout.SOUTH );

        JPanel voiContainer = new JPanel();

        voiContainer.setLayout( new BorderLayout() );
        voiContainer.setPreferredSize( new Dimension( 200, 200 ) );
        voiContainer.setBorder( BorderFactory.createTitledBorder( "VOIs" ) );
        voiContainer.add( voiPane, BorderLayout.CENTER );
        voiAddButton = new JButton( "Add" );
        voiAddButton.addActionListener( this );
        voiAddButton.setActionCommand( "AddVOI" );
        voiAddButton.setFont( serif12B );
        voiAddButton.setPreferredSize( MipavUtil.defaultButtonSize );
        voiAddButton.setMinimumSize( MipavUtil.defaultButtonSize );

        voiRemoveButton = new JButton( "Remove" );
        voiRemoveButton.addActionListener( this );
        voiRemoveButton.setActionCommand( "RemoveVOI" );
        voiRemoveButton.setFont( serif12B );
        voiRemoveButton.setPreferredSize( MipavUtil.defaultButtonSize );
        voiRemoveButton.setMinimumSize( MipavUtil.defaultButtonSize );

        voiButtonsPanel = new JPanel();
        voiButtonsPanel.add( voiAddButton );
        voiButtonsPanel.add( voiRemoveButton );
        voiContainer.add( voiButtonsPanel, BorderLayout.SOUTH );

        top.setBorder( BorderFactory.createEmptyBorder( 5, 5, 0, 5 ) );
        imagesVOIsPanel.add( imageContainer );
        imagesVOIsPanel.add( voiContainer );
        // top.add(radioButtonPanel);
        top.add( imagesVOIsPanel );

        nextLabel = new JLabel( " " );
        nextLabel.setFont( serif12B );
        nextLabel.setForeground( Color.black );

        finishLabel = new JLabel( " " );
        finishLabel.setFont( serif12B );
        finishLabel.setForeground( Color.black );

        imageSelectPanel = new JPanel();
        imageSelectPanel.setLayout( new BorderLayout() );
        imageSelectPanel.add( top, BorderLayout.CENTER );
        // imageSelectPanel.add(buttonsPanel, BorderLayout.SOUTH);

        imgBorder = new TitledBorder( "" );
        imgBorder.setTitleColor( Color.black );
        imgBorder.setBorder( new EtchedBorder() );
        imgBorder.setTitleFont( serif12B );
        imageSelectPanel.setBorder( imgBorder );

        chooseButton = new JButton( "Load script" );
        chooseButton.addActionListener( this );
        chooseButton.setActionCommand( "Choose" );
        chooseButton.setFont( serif12B );

        scriptName = new JTextField( 15 );
        scriptName.setFont( serif12 );
        scriptName.setForeground( Color.black );

        saveLabel = new JLabel( "Add prefix to saved image file name (optional):  " );
        saveLabel.setFont( serif12 );
        saveLabel.setForeground( Color.black );

        savePrefix = new JTextField( 15 );
        savePrefix.setText( "" );
        savePrefix.setFont( serif12 );
        savePrefix.setForeground( Color.black );

        saveLabelSuffix = new JLabel( "Add suffix to saved image file name (optional):  " );
        saveLabelSuffix.setFont( serif12 );
        saveLabelSuffix.setForeground( Color.black );

        saveSuffix = new JTextField( 15 );
        saveSuffix.setText( "" );
        saveSuffix.setFont( serif12 );
        saveSuffix.setForeground( Color.black );

        scriptPanel = new JPanel();
        scriptPanel.setLayout( new GridBagLayout() );

        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = gbc.WEST;
        scriptPanel.add( chooseButton, gbc );

        scriptPanel.add( chooseButton );

        gbc.gridx = 1;
        gbc.fill = gbc.HORIZONTAL;
        scriptPanel.add( scriptName, gbc );

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 2;
        gbc.gridheight = 1;
        scriptPanel.add( Box.createVerticalStrut( 5 ), gbc );

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        scriptPanel.add( saveLabel, gbc );

        gbc.gridx = 1;
        gbc.gridheight = 1;
        scriptPanel.add( savePrefix, gbc );

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        scriptPanel.add( saveLabelSuffix, gbc );

        gbc.gridx = 1;
        gbc.gridheight = 1;
        scriptPanel.add( saveSuffix, gbc );

        scriptSelectPanel = new JPanel();
        scriptSelectPanel.setLayout( new BorderLayout() );
        scriptSelectPanel.add( scriptPanel );

        scriptSelectPanel.setBorder( buildTitledBorder( "Select script" ) );
        scriptSelectPanel.setPreferredSize(new Dimension(200, 200));

        backButton = new JButton( "Back" );
        backButton.addActionListener( this );
        backButton.setActionCommand( "Back" );
        backButton.setEnabled( false );
        backButton.setFont( serif12B );
        backButton.setPreferredSize( MipavUtil.defaultButtonSize );
        backButton.setMinimumSize( MipavUtil.defaultButtonSize );

        nextButton = new JButton( "Next" );
        nextButton.addActionListener( this );
        nextButton.setActionCommand( "Next" );
        nextButton.setFont( serif12B );
        nextButton.setPreferredSize( MipavUtil.defaultButtonSize );
        nextButton.setMinimumSize( MipavUtil.defaultButtonSize );
        nextButton.setEnabled(false);

        buildOKButton();
        OKButton.setText( "Finish" );
        OKButton.setActionCommand( "OK" );
        OKButton.setEnabled( false );
        OKButton.setToolTipText("Click Finish to execute the script.");

        buildCancelButton();

        buttons2Panel = new JPanel( new GridBagLayout() );
        gbc = new GridBagConstraints();
        gbc.anchor = gbc.WEST;
        gbc.insets = new Insets( 1, 1, 1, 1 );
        gbc.gridwidth = gbc.REMAINDER;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
      //  buttons2Panel.add( nextLabel, gbc );
      //  buttons2Panel.add( finishLabel, gbc );
        gbc.anchor = gbc.EAST;
        gbc.gridwidth = 1;
        gbc.fill = gbc.HORIZONTAL;

        buttons2Panel.add( cancelButton, gbc );
        buttons2Panel.add( buildHelpButton(), gbc );
        buttons2Panel.add( backButton, gbc );
        buttons2Panel.add( nextButton, gbc );
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        buttons2Panel.add( OKButton, gbc );

        buttons2Panel.setBorder( BorderFactory.createEmptyBorder( 2, 0, 0, 0 ) );

        contentPanel = new JPanel();
        contentPanel.setLayout( new GridBagLayout() );
        contentPanel.setBorder( new EmptyBorder( 10, 10, 10, 10 ) );

        gbc.fill = gbc.BOTH;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.weighty = 2;
        contentPanel.add( imageSelectPanel, gbc );

        gbc.fill = gbc.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.weighty = 0;

        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.weighty = 1;
        contentPanel.add( scriptSelectPanel, gbc );

        gbc.gridy = 4;
        gbc.weightx = 0;
        gbc.weighty = 0;
        contentPanel.add( Box.createVerticalStrut( 5 ), gbc );

        gbc.anchor = gbc.CENTER;
        gbc.gridwidth = gbc.REMAINDER;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.fill = gbc.NONE;
        contentPanel.add( buttons2Panel, gbc );

        scriptSelectPanel.setVisible( false );
        getContentPane().add( contentPanel );
        pack();

        imageSelectPanel.setVisible( false );
        scriptSelectPanel.setVisible( true );

    }

    /**
     *	Runs the chosen script on the multiple image files.  Calls <code>AlgorithmScriptParser</code>
     *	with the appropriate script file and file name and directory parameters to do so.
     */
    private void runMultiple() {
        int index;
        Vector fileNames;
        Vector dirs;
        Vector voiNames;
        int i, j, k;
        int y, z;

        for ( j = 0; j < imageSet.length; j++ ) {
            AlgorithmScriptParser parse = new AlgorithmScriptParser( scriptFileName, scriptDirectory );

            parse.setActiveImage( false );
            fileNames = new Vector();
            dirs = new Vector();
            voiNames = new Vector();

            for ( i = 0; i < imageSet[j].size(); i++ ) {
                Vector voisPerImage = new Vector();
                String fileNamesIn = (String) imageSet[j].elementAt( i );

                index = fileNamesIn.lastIndexOf( File.separatorChar );
                if ( index <= 0 ) {
                    return;
                }
                dirs.addElement( fileNamesIn.substring( 0, index + 1 ) ); // ends with File.separator
                fileNames.addElement( fileNamesIn.substring( index + 1, fileNamesIn.length() ) );
                int size = ( (VOIVector) ( voiSet[j].getVOIVector().get( i ) ) ).getVOIs().size();

                for ( z = 0; z < size; z++ ) {
                    Object voiElem = ( (VOIVector) ( voiSet[j].getVOIVector().get( i ) ) ).getVOIs().elementAt( z );
                    String voiFileName = (String) voiElem;

                    voisPerImage.addElement( voiFileName );
                }
                voiNames.addElement( voisPerImage );
            }
            parse.setParseType( parseType );
            parse.setFileNames( fileNames );
            parse.setFileDirs( dirs );
            parse.setVoiNames( voiNames );
            parse.setSavePrefix( savePrefix.getText() );
            parse.setSaveSuffix( saveSuffix.getText() );
            if ( !userInterface.isAppFrameVisible() ) {
                parse.setProgressBarVisible( false );
            }
            parse.run();
            System.gc();
        }
    }

    /**
     * Runing the script in the horizontal order.
     */
    private void runMultipleHorizontal() {
        int index;
        Vector fileNames;
        Vector dirs;
        Vector voiNames;
        int i, j, z;

        int maxLength = 0;

        for ( i = 0; i < imageSet.length - 1; i++ ) {
            if ( imageSet[i].size() != imageSet[i + 1].size() ) {
                MipavUtil.displayError( "Each image set should have the same number of images." );
                return;
            }
        }

        for ( i = 0; i < imageSet.length; i++ ) {
            if ( imageSet[i].size() > maxLength ) {
                maxLength = imageSet[i].size();
            }
        }

        for ( i = 0; i < maxLength; i++ ) {
            AlgorithmScriptParser parse = null;

            if (didConvert) {
                parse = new AlgorithmScriptParser(convertedString);
            } else {
                parse = new AlgorithmScriptParser( scriptFileName, scriptDirectory );
            }

            parse.setActiveImage( false );
            // System.err.println("setting active image to false");
            fileNames = new Vector();
            dirs = new Vector();
            voiNames = new Vector();

            // System.out.println("imageSet.length = " + imageSet.length);
            for ( j = 0; j < imageSet.length; j++ ) {
                // System.out.println("j = " + j + " i = " + i);
                Vector voisPerImage = new Vector();
                String fileNamesIn = (String) imageSet[j].elementAt( i );

                index = fileNamesIn.lastIndexOf( File.separatorChar );
                if ( index <= 0 ) {
                    return;
                }
                dirs.addElement( fileNamesIn.substring( 0, index + 1 ) ); // ends with File.separator
                fileNames.addElement( fileNamesIn.substring( index + 1, fileNamesIn.length() ) );
                int size = ( (VOIVector) ( voiSet[j].getVOIVector().get( i ) ) ).getVOIs().size();

                for ( z = 0; z < size; z++ ) {
                    Object voiElem = ( (VOIVector) ( voiSet[j].getVOIVector().get( i ) ) ).getVOIs().elementAt( z );
                    String voiFileName = (String) voiElem;

                    // voiNames.addElement(voiFileName);
                    voisPerImage.addElement( voiFileName );
                }
                voiNames.addElement( voisPerImage );
            }
            parse.setParseType( parseType );
            parse.setFileNames( fileNames );
            parse.setFileDirs( dirs );
            parse.setVoiNames( voiNames );
            parse.setSavePrefix( savePrefix.getText() );
            parse.setSaveSuffix( saveSuffix.getText() );
            if ( !userInterface.isAppFrameVisible() ) {
                parse.setProgressBarVisible( false );
            }
            parse.run();
            System.gc();
        }
    }

    /**
     * Value changed event invoked by the highlighting of the image list.
     * @param e ListSelectionEvent   highlight event
     */
    public void valueChanged( ListSelectionEvent e ) {
        selectIndex = new Vector();
        // JList kList = (JList) e.getSource();
        if ( e.getSource() == imagesList ) {
            ListSelectionModel lsm = imagesList.getSelectionModel();

            if ( lsm.isSelectionEmpty() ) {// MipavUtil.displayError("Selected image list is empty.");
            } else {
                int minIndex = lsm.getMinSelectionIndex();
                int maxIndex = lsm.getMaxSelectionIndex();

                for ( int i = minIndex; i <= maxIndex; i++ ) {
                    if ( lsm.isSelectedIndex( i ) ) {
                        selectIndex.add( new Integer( i ) );
                    }
                }
                int idx = ( (Integer) ( selectIndex.elementAt( 0 ) ) ).intValue();

                voiList.setListData( ( (VOIVector) ( voiSet[currImageIndex].getVOIVector().get( idx ) ) ).getVOIs() );

                voiPane.setViewportView( voiList );
                voiViewPort = voiPane.getViewport();
                if ( ( (VOIVector) ( voiSet[currImageIndex].getVOIVector().get( idx ) ) ).getVOIs().size() == 0 ) {
                    voiPane.setViewport( null );
                    voiPane.setPreferredSize( voiPaneDimen );
                    // voiAddButton.setEnabled(false);
                    voiRemoveButton.setEnabled( false );
                } else {
                    voiPane.setViewport( voiViewPort );
                    voiPane.setPreferredSize( voiPaneDimen );
                    voiAddButton.setEnabled( true );
                    voiRemoveButton.setEnabled( true );
                }
            }
        }
    }

    /**
     * <p>Title: VOIVecoter</p>
     * <p>Description: VOI list object that hold the basic attributes of VOI. </p>
     * @author Ruida Cheng
     */
    class VOIVector extends Vector {
        /** VOI vector that holds that VOIs. */
        private Vector vois;

        /** VOI name. */
        private Object imgName;

        /**
         * Initialize the VOIslist.
         */
        public VOIVector() {
            vois = new Vector();
            imgName = new Object();
        }

        /**
         * Inialize the VOIs list with the given voi name
         * @param name  void name
         */
        public VOIVector( Object name ) {
            vois = new Vector();
            imgName = new Object();
            imgName = name;
        }

        /**
         * Get the VOI list reference.
         * @return Vector  VOIs list vector.
         */
        public Vector getVOIs() {
            return vois;
        }
    }

    /**
     * <p>Title: VOI set vector </p>
     * <p>Description: Create a vector to hold the VOI sets list.</p>
     * @author Ruida Cheng
     */
    class VOISetVector extends Vector {
        /** VOI set vector. */
        private Vector voiVector;

        /** Initialize the VOI sets list vector. */
        public VOISetVector() {
            voiVector = new Vector();
        }

        /** Get the VOI sets list vector reference. */
        public Vector getVOIVector() {
            return voiVector;
        }

    }


    class MyCellRenderer extends JLabel
        implements ListCellRenderer {
        public Component getListCellRendererComponent(
                JList list,
                Object value, // value to display
                int index, // cell index
                boolean isSelected, // is the cell selected
                boolean cellHasFocus ) // the list and the cell have the focus
        {
            String s = value.toString();

            this.setToolTipText( s );

            int j = 0;

            if ( s.length() > 25 ) {
                // separate fileName and directory
                int index1 = s.lastIndexOf( File.separatorChar );
                int index2 = s.indexOf( File.separatorChar );
                String temp = s.substring( 0, index2 + 1 ) + "..." + s.substring( index1 );
                String temp2 = s.substring( 0, index1 );

                String name = temp;

                while ( temp.length() < 25 ) {
                    name = temp;
                    index = temp2.lastIndexOf( File.separatorChar );
                    temp = s.substring( 0, index2 + 1 ) + "..." + s.substring( index1 );
                    temp2 = s.substring( 0, index1 );
                    j++;
                    if ( j > 10 ) {
                        break;
                    }
                }
                s = name;
            }

            setText( s );
            if ( isSelected ) {
                setBackground( list.getSelectionBackground() );
                setForeground( list.getSelectionForeground() );
            } else {
                setBackground( list.getBackground() );
                setForeground( list.getForeground() );
            }
            setEnabled( list.isEnabled() );
            setFont( list.getFont() );
            setOpaque( true );
            return this;
        }
    }
}
