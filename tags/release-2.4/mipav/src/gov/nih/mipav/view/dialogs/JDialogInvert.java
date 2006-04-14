package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import java.util.*;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;


/**
 * Simple dialog to invert an image - substitute light intensities for dark
 * intensities and dark intensities for light intensities.
 *
 * @version    1.0 May 24, 2005
 * @author     Matthew J. McAuliffe, Ph.D.
 */
public class JDialogInvert extends JDialogBase
    implements AlgorithmInterface, ScriptableInterface, ItemListener {

    private int dataType;

    private ViewUserInterface userInterface;

    private JRadioButton replaceImage;
    private JRadioButton newImage;

    private double inTempMin, inTempMax;
    private double outTempMin, outTempMax;

    private ModelImage image;
    private ModelImage resultImage = null; // result image
    private AlgorithmChangeType changeTypeAlgo;
    private String[] titles;

    private boolean endianess;
    private boolean useDefaultRanges = true; // only applies for input ranges

    private int displayLoc;
    // Flag indicating if a new image is to be generated
    // or if the source image is to be replaced

    /**
     *  Creates new dialog for converting type of image.
     *  @param theParentFrame Parent frame.
     *  @param _image         Source image.
     */
    public JDialogInvert( Frame theParentFrame, ModelImage _image ) {
        super( theParentFrame, false );
        image = _image;
        userInterface = ( (ViewJFrameBase) ( parentFrame ) ).getUserInterface();
        init();
    }

    /**
     *	Used primarily for the script to store variables and run the algorithm.  No
     *	actual dialog will appear but the set up info and result image will be stored here.
     *	@param UI   The user interface, needed to create the image frame.
     *	@param im	Source image.
     */
    public JDialogInvert( ViewUserInterface UI, ModelImage im ) {
        super( false );
        userInterface = UI;
        image = im;
        parentFrame = im.getParentFrame();
    }

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogInvert() {}

    /**
     * Run this algorithm from a script.
     * @param parser the script parser we get the state from
     * @throws IllegalArgumentException if there is something wrong with the arguments in the script
     */
    public void scriptRun( AlgorithmScriptParser parser )
        throws IllegalArgumentException {
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

        if ( srcImageKey.equals( destImageKey ) ) {
            this.setDisplayLocReplace();
        } else {
            this.setDisplayLocNew();
        }

        try {
            setDataType( parser.getNextInteger() );
            setEndianess( parser.getNextBoolean() );
            if ( parser.getNextBoolean() ) {
                setUseDefaultRanges( true );
                setDefaultRanges();
            } else {
                setUseDefaultRanges( false );
                setInputRangeMin( parser.getNextDouble() );
                setInputRangeMax( parser.getNextDouble() );

            }
            setOutputRangeMin( parser.getNextDouble() );
            setOutputRangeMax( parser.getNextDouble() );
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

                if ( useDefaultRanges == true ) {
                    userInterface.getScriptDialog().append(
                            "ConvertType " + userInterface.getScriptDialog().getVar( image.getImageName() ) + " " );
                    if ( displayLoc == NEW ) {
                        userInterface.getScriptDialog().putVar( resultImage.getImageName() );
                        userInterface.getScriptDialog().append(
                                userInterface.getScriptDialog().getVar( resultImage.getImageName() ) + " " + dataType
                                + " " + endianess + " " + useDefaultRanges + " " + outTempMin + " " + outTempMax + "\n" );
                    } else {
                        userInterface.getScriptDialog().append(
                                userInterface.getScriptDialog().getVar( image.getImageName() ) + " " + dataType + " "
                                + endianess + " " + useDefaultRanges + " " + outTempMin + " " + outTempMax + "\n" );
                    }
                } else { // not using default ranges
                    userInterface.getScriptDialog().append(
                            "ConvertType " + userInterface.getScriptDialog().getVar( image.getImageName() ) + " " );
                    if ( displayLoc == NEW ) {
                        userInterface.getScriptDialog().putVar( resultImage.getImageName() );
                        userInterface.getScriptDialog().append(
                                userInterface.getScriptDialog().getVar( resultImage.getImageName() ) + " " + dataType
                                + " " + endianess + " " + useDefaultRanges + " " + inTempMin + " " + inTempMax + " "
                                + outTempMin + " " + outTempMax + "\n" );
                    } else {
                        userInterface.getScriptDialog().append(
                                userInterface.getScriptDialog().getVar( image.getImageName() ) + " " + dataType + " "
                                + endianess + " " + useDefaultRanges + " " + inTempMin + " " + inTempMax + " "
                                + outTempMin + " " + outTempMax + "\n" );

                    }
                } // end if useDefaultRanges
            }
        }
    }

    /**
     *	Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground( Color.black );
        setTitle( "Invert image" );

        GridBagConstraints gbc = new GridBagConstraints();

        JPanel destinationPanel = new JPanel( new GridBagLayout() );

        destinationPanel.setForeground( Color.black );
        destinationPanel.setBorder( buildTitledBorder( "Destination" ) );

        ButtonGroup destinationGroup = new ButtonGroup();

        newImage = new JRadioButton( "New image", true );
        newImage.setFont( serif12 );
        destinationGroup.add( newImage );

        replaceImage = new JRadioButton( "Replace image", false );
        replaceImage.setFont( serif12 );
        destinationGroup.add( replaceImage );

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = gbc.REMAINDER;
        gbc.gridheight = 1;
        gbc.fill = gbc.HORIZONTAL;
        gbc.weightx = 1;
        gbc.anchor = gbc.WEST;
        destinationPanel.add( newImage, gbc );
        gbc.gridy = 1;
        destinationPanel.add( replaceImage, gbc );

        if ( image.getLockStatus() == ModelStorageBase.UNLOCKED ) { // Only if the image is unlocked can it be replaced.
            replaceImage.setEnabled( true );
        } else {
            replaceImage.setEnabled( false );
        }

        JPanel buttonPanel = new JPanel();

        buttonPanel.add( buildButtons() );

        JPanel mainPanel = new JPanel();

        mainPanel.setLayout( new BoxLayout( mainPanel, BoxLayout.Y_AXIS ) );
        mainPanel.add( destinationPanel );


        getContentPane().add( mainPanel );
        getContentPane().add( buttonPanel, BorderLayout.SOUTH );
        pack();
        setVisible( true );
    }

    /**
     *   Accessor that returns the data type
     *   @return        the data type
     */
    public int getDataType() {
        return dataType;
    }

    /**
     *    Closes dialog box when the OK button is pressed and
     *    sets the variables
     *    @param event      Event that triggers this function
     */
    public void actionPerformed( ActionEvent event ) {
        String command = event.getActionCommand();

        if ( command.equals( "OK" ) ) {
            if ( setVariables() ) {
                callAlgorithm();
            }
        } else if ( command.equals( "Cancel" ) ) {
            cancelFlag = true;
            dispose();
        } else if ( command.equals( "Help" ) ) {
            MipavUtil.showHelp( "10069" );
        }

    } // end actionPerformed()

    /**
     *  Accessor that returns the image.
     *  @return          The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     *	Accessor that sets the display loc variable to replace, so the current image
     *	is replaced once the algorithm completes.
     */
    public void setDisplayLocReplace() {
        displayLoc = REPLACE;
    }

    /**
     *	Accessor that sets the display loc variable to new, so that a new image
     *	is created once the algorithm completes.
     */
    public void setDisplayLocNew() {
        displayLoc = NEW;
    }

    /**
     *   Accessor that sets the useDefaultRanges to the parameter.
     *   @param useDefault  Value for useDefaultRanges variable.
     */
    public void setUseDefaultRanges( boolean useDefault ) {
        useDefaultRanges = useDefault;
    }

    /**
     *   Accessor that sets the minimum input range to the parameter.
     *   @param min  Minimum input range.
     */
    public void setInputRangeMin( double min ) {
        inTempMin = min;
    }

    /**
     *   Accessor that sets the maximum input range to the parameter
     *   @param max  Maximum input range.
     */
    public void setInputRangeMax( double max ) {
        inTempMax = max;
    }

    /**
     *   Accessor that sets the minimum output range to the parameter.
     *   @param min  Minimum output range.
     */
    public void setOutputRangeMin( double min ) {
        outTempMin = min;
    }

    /**
     *   Accessor that sets the maximum output range to the parameter
     *   @param max  Maximum output range.
     */
    public void setOutputRangeMax( double max ) {
        outTempMax = max;
    }

    /**
     *   Accessor that sets the data type for what the converted image is to be.
     *   @param type New data type.
     */
    public void setDataType( int type ) {
        dataType = type;
    }

    /**
     *   Sets the default values for the input and output range
     */
    public void setDefaultRanges() {
        inTempMin = (float) image.getMin();
        inTempMax = (float) image.getMax();
    }

    /**
     *   Accessor that sets the endianess
     *   @param endns    Endianess.
     */
    public void setEndianess( boolean endns ) {
        endianess = endns;
    }

    /**
     *	Use the GUI results to set up the variables needed to run the algorithm.
     *	@return		<code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        System.gc();

        dataType = image.getType();

        if ( replaceImage.isSelected() ) {
            displayLoc = REPLACE;
        } else if ( newImage.isSelected() ) {
            displayLoc = NEW;
        }

        endianess = image.getFileInfo(0).getEndianess();

        this.setDefaultRanges();

        outTempMin = image.getMax();
        outTempMax = image.getMin();

        return true;
    }

    /**
     *	Once all the necessary variables are set, call the Change Type
     *	algorithm based on what type of image this is and whether or not there
     *	is a separate destination image.
     */
    private void callAlgorithm() {

        if ( image.getNDims() == 2 ) { // source image is 2D
            int[] destExtents = new int[2];

            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            image.getFileInfo( 0 ).setEndianess( endianess );

            if ( displayLoc == NEW ) {
                try {
                    // Make result image of the new data type
                    resultImage = new ModelImage( dataType, destExtents,
                            makeImageName( image.getImageName(), "_invert" ), userInterface );

                    // Make algorithm
                    changeTypeAlgo = new AlgorithmChangeType( resultImage, image, inTempMin, inTempMax, outTempMin,
                            outTempMax, false );
                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    changeTypeAlgo.addListener( this );
                    // Hide dialog
                    setVisible( false );

                    if ( runInSeparateThread ) {
                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if ( changeTypeAlgo.startMethod( Thread.MIN_PRIORITY ) == false ) {
                            MipavUtil.displayError( "A thread is already running on this object" );
                        }
                    } else {
                        changeTypeAlgo.setActiveImage( isActiveImage );
                        if ( !userInterface.isAppFrameVisible() ) {
                            changeTypeAlgo.setProgressBarVisible( false );
                        }
                        changeTypeAlgo.run();
                    }
                } catch ( OutOfMemoryError x ) {
                    MipavUtil.displayError( "Dialog invert: unable to allocate enough memory" );
                    if ( resultImage != null ) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }
                    return;
                }
            } else {
                try {
                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    changeTypeAlgo = new AlgorithmChangeType( image, dataType, inTempMin, inTempMax, outTempMin,
                            outTempMax, false );
                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    changeTypeAlgo.addListener( this );
                    // Hide the dialog since the algorithm is about to run.
                    setVisible( false );

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector imageFrames = image.getImageFrameVector();

                    titles = new String[imageFrames.size()];
                    for ( int i = 0; i < imageFrames.size(); i++ ) {
                        titles[i] = ( (Frame) ( imageFrames.elementAt( i ) ) ).getTitle();
                        ( (Frame) ( imageFrames.elementAt( i ) ) ).setTitle( "Locked: " + titles[i] );
                        ( (Frame) ( imageFrames.elementAt( i ) ) ).setEnabled( false );
                        userInterface.unregisterFrame( (Frame) ( imageFrames.elementAt( i ) ) );
                    }

                    if ( runInSeparateThread ) {
                        // Start the thread as a low priority because we wish to still have user interface.
                        if ( changeTypeAlgo.startMethod( Thread.MIN_PRIORITY ) == false ) {
                            MipavUtil.displayError( "A thread is already running on this object" );
                        }
                    } else {
                        changeTypeAlgo.setActiveImage( isActiveImage );
                        if ( !userInterface.isAppFrameVisible() ) {
                            changeTypeAlgo.setProgressBarVisible( false );
                        }
                        changeTypeAlgo.run();
                    }
                } catch ( OutOfMemoryError x ) {
                    MipavUtil.displayError( "Dialog invert: unable to allocate enough memory" );
                    return;
                }
            }
        } else if ( ( image.getNDims() == 3 ) || ( image.getNDims() == 4 ) ) {
            int[] destExtents;

            if ( image.getNDims() == 3 ) {
                destExtents = new int[3];
                destExtents[0] = image.getExtents()[0];
                destExtents[1] = image.getExtents()[1];
                destExtents[2] = image.getExtents()[2];

                for ( int n = 0; n < image.getExtents()[2]; n++ ) {
                    image.getFileInfo( n ).setEndianess( endianess );
                }
            } else {
                destExtents = new int[4];
                destExtents[0] = image.getExtents()[0];
                destExtents[1] = image.getExtents()[1];
                destExtents[2] = image.getExtents()[2];
                destExtents[3] = image.getExtents()[3];

                for ( int n = 0; n < image.getExtents()[2] * image.getExtents()[3]; n++ ) {
                    image.getFileInfo( n ).setEndianess( endianess );
                }
            }

            if ( displayLoc == NEW ) {
                try {
                    // Make result image of the new data type
                    resultImage = new ModelImage( dataType, destExtents,
                            makeImageName( image.getImageName(), "_invert" ), userInterface );
                    // Make algorithm
                    System.out.println( inTempMin + " " + inTempMax + " " + outTempMin + " " + outTempMax );
                    changeTypeAlgo = new AlgorithmChangeType( resultImage, image, inTempMin, inTempMax, outTempMin,
                            outTempMax, false );
                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    changeTypeAlgo.addListener( this );
                    // Hide dialog
                    setVisible( false );

                    if ( runInSeparateThread ) {
                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if ( changeTypeAlgo.startMethod( Thread.MIN_PRIORITY ) == false ) {
                            MipavUtil.displayError( "A thread is already running on this object" );
                        }
                    } else {
                        changeTypeAlgo.setActiveImage( isActiveImage );
                        if ( !userInterface.isAppFrameVisible() ) {
                            changeTypeAlgo.setProgressBarVisible( false );
                        }
                        changeTypeAlgo.run();
                    }
                } catch ( OutOfMemoryError x ) {
                    MipavUtil.displayError( "Dialog invert: unable to allocate enough memory" );
                    if ( resultImage != null ) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }
                    return;
                }
            } else {
                try {
                    // Make algorithm
                    System.out.println( inTempMin + " " + inTempMax + " " + outTempMin + " " + outTempMax );
                    changeTypeAlgo = new AlgorithmChangeType( image, dataType, inTempMin, inTempMax, outTempMin,
                            outTempMax, false );
                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    changeTypeAlgo.addListener( this );
                    // Hide dialog
                    setVisible( false );

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector imageFrames = image.getImageFrameVector();

                    titles = new String[imageFrames.size()];
                    for ( int i = 0; i < imageFrames.size(); i++ ) {
                        titles[i] = ( (Frame) ( imageFrames.elementAt( i ) ) ).getTitle();
                        ( (Frame) ( imageFrames.elementAt( i ) ) ).setTitle( "Locked: " + titles[i] );
                        ( (Frame) ( imageFrames.elementAt( i ) ) ).setEnabled( false );
                        userInterface.unregisterFrame( (Frame) ( imageFrames.elementAt( i ) ) );
                    }

                    if ( runInSeparateThread ) {
                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if ( changeTypeAlgo.startMethod( Thread.MIN_PRIORITY ) == false ) {
                            MipavUtil.displayError( "A thread is already running on this object" );
                        }
                    } else {
                        changeTypeAlgo.setActiveImage( isActiveImage );
                        if ( !userInterface.isAppFrameVisible() ) {
                            changeTypeAlgo.setProgressBarVisible( false );
                        }
                        changeTypeAlgo.run();
                    }
                } catch ( OutOfMemoryError x ) {
                    MipavUtil.displayError( "Dialog invert: unable to allocate enough memory" );
                    return;
                }
            }
        }
    }

    /**
     *   Sets the flags for the checkboxes and resets labels.
     *   @param event       Event that triggered this function.
     */
    public synchronized void itemStateChanged( ItemEvent event ) {
         }

    //************************************************************************
    //************************** Algorithm Events ****************************
    //************************************************************************

    /**
     *	This method is required if the AlgorithmPerformed interface is implemented. It is called by the
     *   algorithms when it has completed or failed to to complete, so that the dialog can be display
     *   the result image and/or clean up.
     *   @param algorithm   Algorithm that caused the event.
     */
    public void algorithmPerformed( AlgorithmBase algorithm ) {

        ViewJFrameImage imageFrame = null;

        if ( algorithm instanceof AlgorithmChangeType ) {
            if ( changeTypeAlgo.isCompleted() == true && resultImage != null ) {
                updateFileInfo( image, resultImage );
                //The algorithm has completed and produced a new image to be displayed.
                try {
                    imageFrame = new ViewJFrameImage( resultImage, null, new Dimension( 610, 200 ) );
                } catch ( OutOfMemoryError error ) {
                    MipavUtil.displayError( "Out of memory: unable to open new frame" );
                }
            } else if ( resultImage == null ) {
                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector imageFrames = image.getImageFrameVector();

                for ( int i = 0; i < imageFrames.size(); i++ ) {
                    ( (Frame) ( imageFrames.elementAt( i ) ) ).setTitle( titles[i] );
                    ( (Frame) ( imageFrames.elementAt( i ) ) ).setEnabled( true );
                    if ( ( (Frame) ( imageFrames.elementAt( i ) ) ) != parentFrame ) {
                        userInterface.registerFrame( (Frame) ( imageFrames.elementAt( i ) ) );
                    }
                }
                if ( parentFrame != null ) {
                    userInterface.registerFrame( parentFrame );
                }
                image.notifyImageDisplayListeners( null, true );
            } else if ( resultImage != null ) {
                //algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        }

        insertScriptLine( algorithm );

        // Update frame
        //((ViewJFrameBase)parentFrame).updateImages(true);
        changeTypeAlgo.finalize();
        changeTypeAlgo = null;
        dispose();
        System.gc();
    }

}
