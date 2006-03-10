package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;

import java.awt.event.*;
import java.awt.*;


/**
 * Dialog to get user input, then call the midsagittal alignment algorithm.
 *
 * @version    0.1 Dec 30, 2004
 * @author     Evan McCreedy
 */
public class JDialogMidsagittal extends JDialogBase
    implements AlgorithmInterface, ScriptableInterface, DialogDefaultsInterface {

    private AlgorithmMidsagittal alignAlgo;

    private ModelImage image; // source image
    private ModelImage resultImage = null; // result image

    private ViewUserInterface userInterface;

    long start, end;

    /**
     * Set up the dialog (nothing right now), and run the algorithm (since there really is no dialog).
     * @param theParentFrame  Parent frame.
     * @param im              Source image.
     */
    public JDialogMidsagittal( Frame theParentFrame, ModelImage im ) {
        super( theParentFrame, false );
        image = im;
        userInterface = ( (ViewJFrameBase) ( parentFrame ) ).getUserInterface();
        init();
        loadDefaults();

        callAlgorithm();
    }

    /**
     * Used primarily for the script to store variables and run the algorithm.  No
     * actual dialog will appear but the set up info and result image will be stored here.
     * @param UI   The user interface, needed to create the image frame.
     * @param im	Source image.
     */
    public JDialogMidsagittal( ViewUserInterface UI, ModelImage im ) {
        super();
        userInterface = UI;
        image = im;
        parentFrame = image.getParentFrame();
    }

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogMidsagittal() {}

    /**
     * Run this algorithm from a script.
     * @param parser the script parser we get the state from
     * @throws IllegalArgumentException if there is something wrong with the arguments in the script
     */
    public void scriptRun( AlgorithmScriptParser parser ) throws IllegalArgumentException {
        setScriptRunning( true );

        String srcImageKey = null;
        String destImageKey = null;

        try {
            srcImageKey = parser.getNextString();
        } catch ( Exception e ) {
            throw new IllegalArgumentException();
        }
        ModelImage im = parser.getImage( srcImageKey );

        image = im;
        userInterface = image.getUserInterface();
        parentFrame = image.getParentFrame();

        // the result image
        try {
            destImageKey = parser.getNextString();
        } catch ( Exception e ) {
            throw new IllegalArgumentException();
        }

        try {
        } catch ( Exception e ) {
            throw new IllegalArgumentException();
        }

        setActiveImage( parser.isActiveImage() );
        setSeparateThread( false );
        callAlgorithm();
        if ( !srcImageKey.equals( destImageKey ) ) {
            parser.putVariable( destImageKey, getResultImage().getImageName() );
        }
    }

    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     * @param algo the algorithm to make an entry for
     */
    public void insertScriptLine( AlgorithmBase algo ) {
        if ( algo.isCompleted() ) {
            if ( userInterface.isScriptRecording() ) {
                //check to see if the match image is already in the ImgTable
                if ( userInterface.getScriptDialog().getImgTableVar( image.getImageName() ) == null ) {
                    if ( userInterface.getScriptDialog().getActiveImgTableVar( image.getImageName() ) == null ) {
                        userInterface.getScriptDialog().putActiveVar( image.getImageName() );
                    }
                }

                String line = "Midsagittal " + userInterface.getScriptDialog().getVar( image.getImageName() ) + " ";
                userInterface.getScriptDialog().putVar( resultImage.getImageName() );
                line += userInterface.getScriptDialog().getVar( resultImage.getImageName() ) + " " + getParameterString( " " ) + "\n";

                userInterface.getScriptDialog().append( line );
            }
        }
    }

    /**
     * Construct a delimited string that contains the parameters to this algorithm.
     * @param delim  the parameter delimiter (defaults to " " if empty)
     * @return       the parameter string
     */
    public String getParameterString( String delim ) {
        if ( delim.equals( "" ) ) {
            delim = " ";
        }

        String str = new String();

        return str;
    }

    /**
     *  Loads the default settings from Preferences to set up the dialog
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults( getDialogName() );
    }

    /**
     * Saves the default settings into the Preferences file
     */
    public void saveDefaults() {
        String defaultsString = new String( getParameterString( "," ) );
    }

    /**
     *	Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setVisible( false );
    }

    /**
     *  Accessor that returns the image.
     *  @return          The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     *  Closes dialog box when the OK button is pressed and calls the algorithm.
     *  @param event       Event that triggers function.
     */
    public void actionPerformed( ActionEvent event ) {
        String command = event.getActionCommand();

        if ( command.equals( "OK" ) ) {
            if ( setVariables() ) {
                callAlgorithm();
            }
        } else if ( command.equals( "Cancel" ) ) {
            dispose();
        } else if ( command.equals( "Help" ) ) {
            //MipavUtil.showHelp( "10009" );
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     *	This method is required if the AlgorithmPerformed interface is implemented.
     *   It is called by the algorithm when it has completed or failed to to complete,
     *   so that the dialog can be display the result image and/or clean up.
     *   @param algorithm   Algorithm that caused the event.
     */
    public void algorithmPerformed( AlgorithmBase algorithm ) {

        ViewJFrameImage imageFrame = null;

        if ( Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && this.getOwner() != null && !isScriptRunning() ) {
            saveDefaults();
        }

        if ( algorithm instanceof AlgorithmMidsagittal ) {
            end = System.currentTimeMillis();
            Preferences.debug( "Midsagittal elapsed: " + ( end - start ) );

            if ( alignAlgo.isCompleted() == true ) {
                resultImage = alignAlgo.getResultImage();

                // The algorithm has completed and produced a new image to be displayed.
                if ( resultImage.isColorImage() ) {
                    updateFileInfo( image, resultImage );
                }
                resultImage.clearMask();

                try {
                    imageFrame = new ViewJFrameImage( resultImage, null, userInterface.getNewFrameLocation() );
                } catch ( OutOfMemoryError error ) {
                    System.gc();
                    MipavUtil.displayError( "Out of memory: unable to open new frame" );
                }
            }
            insertScriptLine( algorithm );
        }

        if ( alignAlgo != null ) {
            alignAlgo.finalize();
            alignAlgo = null;
        }
        dispose();
    }

    /**
     *	Use the GUI results to set up the variables needed to run the algorithm.
     *	@return		<code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        return true;
    }

    /**
     *	Once all the necessary variables are set, call the Gaussian Blur
     *	algorithm based on what type of image this is and whether or not there
     *	is a separate destination image.
     */
    private void callAlgorithm() {
        start = System.currentTimeMillis();

        try {
            // Make algorithm
            alignAlgo = new AlgorithmMidsagittal( image );
            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            alignAlgo.addListener( this );

            // Hide dialog
            setVisible( false );

            if ( runInSeparateThread ) {
                // Start the thread as a low priority because we wish to still have user interface work fast.
                if ( alignAlgo.startMethod( Thread.MIN_PRIORITY ) == false ) {
                    MipavUtil.displayError( "A thread is already running on this object" );
                }
            } else {
                alignAlgo.setActiveImage( isActiveImage );
                if ( !userInterface.isAppFrameVisible() ) {
                    alignAlgo.setProgressBarVisible( false );
                }
                alignAlgo.run();
            }
        } catch ( OutOfMemoryError x ) {
            System.gc();
            MipavUtil.displayError( "Dialog midsagittal: unable to allocate enough memory" );
            return;
        }
    }
}
