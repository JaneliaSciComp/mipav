package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;

import javax.swing.*;

import java.lang.*;
import java.io.*;


/**
 *		User interface to open a VOI.
 *
 *		@version    0.1 Feb 24, 1998
 *		@author     Matthew J. McAuliffe, Ph.D.
 *
 */

public class ViewOpenVOIUI {
    /**
     * The main user interface.
     */
    private ViewUserInterface UI;

    /**
     *   Constructor to allow user to open a file.
     *   @param  ui            Main user interface
     */
    public ViewOpenVOIUI( ViewUserInterface ui ) {
        UI = ui;
    }

    /**
     *   Open a VOI array based on the suffix of the file.
     *   @param image        Image to open the VOI on.
     *   @return             the opened VOIs
     */
    public VOI[] open( ModelImage image ) {
        VOI[] voi;
        FileCheshireVOI fileCheshireVOI;
        FileVOI fileVOI;
        String extension = " ";
        String fileName;
        String directory;
        int i;

        JFileChooser chooser = new JFileChooser();

        chooser.setDialogTitle( "Open VOI" );
        if ( UI.getDefaultDirectory() != null ) {
            File file = new File( UI.getDefaultDirectory() );

            if ( file != null ) {
                chooser.setCurrentDirectory( file );
            } else {
                chooser.setCurrentDirectory( new File( System.getProperty( "user.dir" ) ) );
            }
        } else {
            chooser.setCurrentDirectory( new File( System.getProperty( "user.dir" ) ) );
        }

        chooser.addChoosableFileFilter( new ViewImageFileFilter( new String[] { "xml", "voi" } ) );

        int returnValue = chooser.showOpenDialog( UI.getMainFrame() );

        if ( returnValue == JFileChooser.APPROVE_OPTION ) {
            fileName = chooser.getSelectedFile().getName();
            directory = String.valueOf( chooser.getCurrentDirectory() ) + File.separatorChar;
            UI.setDefaultDirectory( directory );
        } else {
            return null;
        }

        try {
            i = fileName.lastIndexOf( '.' );
            if ( i > 0 && i < fileName.length() - 1 ) {
                extension = fileName.substring( i + 1 ).toLowerCase();
            }
            if ( extension.equals( "oly" ) ) {
                fileCheshireVOI = new FileCheshireVOI( fileName, directory, image );
                voi = fileCheshireVOI.readVOI();
            } else {
                fileVOI = new FileVOI( fileName, directory, image );
                voi = fileVOI.readVOI();
            }
        } catch ( IOException error ) {
            MipavUtil.displayError( "VOI IO: " + error );
            return null;
        }

        if ( voi == null ) {
            MipavUtil.displayError( "VOI is null" );
            return null;
        }

        // UI.getController().registerModel(voi);
        for ( i = 0; i < voi.length; i++ ) {
            image.registerVOI( voi[i] );
        }
        image.notifyImageDisplayListeners();
        return ( voi );
    }
}
