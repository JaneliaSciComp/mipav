package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;

import java.awt.event.*;
import java.awt.*;
import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user is able to control
 * the degree of blurring in all dimensions and indicate if a correction factor be
 * applied to the z-dimension to account for differing resolutions between the
 * xy resolutions (intra-plane) and the z resolution (inter-plane). The user has the
 * option to generate a new image or replace the source image. User
 * can indicate whether to have algorithm applied to whole image or to the
 * VOI regions. Algorithms are executed in their own thread.
 *
 * @version   0.1 Nov 17, 1998
 * @author    Matthew J. McAuliffe, Ph.D.
 * @see       AlgorithmLaplacian
 */
public class JDialogLaplacian extends JDialogBase
    implements AlgorithmInterface, ScriptableInterface, DialogDefaultsInterface {

    private AlgorithmLaplacian laplacianAlgo;
    private ModelImage image; // source image
    private ModelImage resultImage = null; // result image
    private ViewUserInterface userInterface;

    private float scaleX;
    private float scaleY;
    private float scaleZ;
    private float ampFactor = 1.0f;
    private boolean image25D = false;
    private float normFactor = 1; // normalization factor to adjust for resolution
    //  difference between x,y resolutions (in plane)
    //  and z resolution (between planes)
    private int displayLoc; // Flag indicating if a new image is to be generated
    // or if the source image is to be replaced
    private boolean regionFlag; // true = apply algorithm to the whole image
    // false = apply algorithm only to VOI regions

    private String[] titles;

    private JPanel scalePanel;
    private JTextField textGaussX;
    private JLabel labelGaussX;

    private JTextField textGaussY;
    private JLabel labelGaussY;

    private JTextField textGaussZ;
    private JLabel labelGaussZ;

    private JTextField textAmpFact;
    private JLabel labelAmpFact;

    private JCheckBox resolutionCheckbox;
    private JLabel labelCorrected;

    private JCheckBox image25DCheckbox;

    private JPanel destinationPanel;
    private ButtonGroup destinationGroup;
    private JRadioButton replaceImage;
    private JRadioButton newImage;

    private JPanel imageVOIPanel;
    private ButtonGroup imageVOIGroup;
    private JRadioButton wholeImage;
    private JRadioButton VOIRegions;

    public long start, end;

    /**
     *  @param parent          Parent frame.
     *  @param im              Source image.
     */
    public JDialogLaplacian( Frame theParentFrame, ModelImage im ) {
        super( theParentFrame, false );
        image = im;
        userInterface = ( (ViewJFrameBase) ( parentFrame ) ).getUserInterface();
        init();
        loadDefaults();
        setVisible( true );
    }

    /**
     *	Used primarily for the script to store variables and run the algorithm.  No
     *	actual dialog will appear but the set up info and result image will be stored here.
     *	@param UI   The user interface, needed to create the image frame.
     *	@param im	Source image.
     */
    public JDialogLaplacian( ViewUserInterface UI, ModelImage im ) {
        super();
        userInterface = UI;
        image = im;
        parentFrame = image.getParentFrame();
    }

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogLaplacian() {}

    /**
     * Run this algorithm from a script.
     * @param parser the script parser we get the state from
     * @throws IllegalArgumentException if there is something wrong with the arguments in the script
     */
    public void scriptRun( AlgorithmScriptParser parser )
        throws IllegalArgumentException {
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

        if ( srcImageKey.equals( destImageKey ) ) {
            this.setDisplayLocReplace();
        } else {
            this.setDisplayLocNew();
        }

        try {
            setRegionFlag( parser.getNextBoolean() );
            setImage25D( parser.getNextBoolean() );
            setScaleX( parser.getNextFloat() );
            setScaleY( parser.getNextFloat() );
            setScaleZ( parser.getNextFloat() );
            setAmpFactor( parser.getNextFloat() );
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

                userInterface.getScriptDialog().append(
                        "Laplacian " + userInterface.getScriptDialog().getVar( image.getImageName() ) + " " );
                if ( displayLoc == NEW ) {
                    userInterface.getScriptDialog().putVar( resultImage.getImageName() );
                    userInterface.getScriptDialog().append(
                            userInterface.getScriptDialog().getVar( resultImage.getImageName() ) + " " + getParameterString(" ") );
                } else {
                    userInterface.getScriptDialog().append(
                            userInterface.getScriptDialog().getVar( image.getImageName() ) + " " + getParameterString(" ") );
                }
                userInterface.getScriptDialog().append( "\n" );
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
        str += regionFlag + delim;
        str += image25D + delim;
        str += scaleX + delim;
        str += scaleY + delim;
        str += textGaussZ.getText() + delim;
        str += ampFactor;

        return str;
    }

    /**
     *  Loads the default settings from Preferences to set up the dialog
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults( getDialogName() );

        if ( defaultsString != null && VOIRegions != null ) {

            try {
                StringTokenizer st = new StringTokenizer( defaultsString, "," );

                if ( MipavUtil.getBoolean( st ) ) {
                    wholeImage.setSelected( true );
                } else {
                    VOIRegions.setSelected( true );
                }

                image25DCheckbox.setSelected( MipavUtil.getBoolean( st ) );

                textGaussX.setText( st.nextToken() );
                textGaussY.setText( st.nextToken() );
                textGaussZ.setText( st.nextToken() );

                textAmpFact.setText( st.nextToken() );

                resolutionCheckbox.setSelected( MipavUtil.getBoolean( st ) );

                if ( MipavUtil.getBoolean( st ) ) {
                    newImage.setSelected( true );
                } else {
                    replaceImage.setSelected( true );
                }
            } catch ( Exception ex ) {
                // since there was a problem parsing the defaults string, start over with the original defaults
                Preferences.debug( "Resetting defaults for dialog: " + getDialogName() );
                Preferences.removeProperty( getDialogName() );
                ex.printStackTrace();
            }
        }

    }

    /**
     * Saves the default settings into the Preferences file
     */
    public void saveDefaults() {
        String defaultsString = new String( getParameterString(",") + "," + resolutionCheckbox.isSelected() + "," + newImage.isSelected() );
        Preferences.saveDialogDefaults( getDialogName(), defaultsString );
    }

    /**
     *	Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
     */
    private void init() {
        setForeground( Color.black );

        getContentPane().setLayout( new BorderLayout() );
        setTitle( "Laplacian" );

        JPanel mainPanel;

        mainPanel = new JPanel();
        mainPanel.setBorder( BorderFactory.createEmptyBorder( 3, 3, 3, 3 ) );
        mainPanel.setLayout( new GridBagLayout() );
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = gbc.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets( 3, 3, 3, 3 );
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        scalePanel = new JPanel( new GridLayout( 4, 2 ) );
        scalePanel.setForeground( Color.black );
        scalePanel.setBorder( buildTitledBorder( "Scale of the Gaussian" ) );
        mainPanel.add( scalePanel, gbc );

        labelGaussX = createLabel( "X dimension (0.0 - 10.0) " );
        scalePanel.add( labelGaussX );
        textGaussX = createTextField("1.0");
        scalePanel.add( textGaussX );

        labelGaussY = createLabel( "Y dimension (0.0 - 10.0) " );
        scalePanel.add( labelGaussY );
        textGaussY = createTextField("1.0");
        scalePanel.add( textGaussY );

        labelGaussZ = createLabel( "Z dimension (0.0 - 10.0) " );
        scalePanel.add( labelGaussZ );
        textGaussZ = createTextField("1.0");
        scalePanel.add( textGaussZ );

        labelAmpFact = createLabel( "Amplification factor (1.0 - 2.0) " );
        scalePanel.add( labelAmpFact );
        textAmpFact = createTextField("1.0");
        scalePanel.add( textAmpFact );

        JPanel resPanel = new JPanel( new BorderLayout() );

        resPanel.setBorder( buildTitledBorder( "Resolution options" ) );
        resolutionCheckbox = new JCheckBox( "Use image resolutions to normalize Z scale" );
        resolutionCheckbox.setFont( serif12 );
        resPanel.add( resolutionCheckbox, BorderLayout.NORTH );
        resolutionCheckbox.setSelected( true );

        image25DCheckbox = new JCheckBox( "Process each slice independently (2.5D)" );
        image25DCheckbox.setFont( serif12 );
        resPanel.add( image25DCheckbox, BorderLayout.SOUTH );
        image25DCheckbox.setSelected( false );
        image25DCheckbox.addItemListener( this );

        if ( image.getNDims() == 3 ) { // if the source image is 3D then allow
            resolutionCheckbox.setEnabled( true ); // the user to indicate if it wishes to
            resolutionCheckbox.addItemListener( this ); // use the correction factor
            textGaussZ.addFocusListener( this );
            textGaussZ.setEnabled( true );
        } else {
            resolutionCheckbox.setEnabled( false ); // Image is only 2D, thus this checkbox
            labelGaussZ.setEnabled( false ); // is not relevent
            textGaussZ.setEnabled( false );
            image25DCheckbox.setEnabled( false );
        }

        if ( image.getNDims() == 3 ) { // Source image is 3D, thus show correction factor
            int index = image.getExtents()[2] / 2;
            float xRes = image.getFileInfo( index ).getResolutions()[0];
            float zRes = image.getFileInfo( index ).getResolutions()[2];

            normFactor = xRes / zRes; // Calculate correction factor
            labelCorrected = new JLabel(
                    "      Corrected scale = "
                            + String.valueOf( normFactor * Float.valueOf( textGaussZ.getText() ).floatValue() ) );
            labelCorrected.setForeground( Color.black );
            labelCorrected.setFont( serif12 );
            resPanel.add( labelCorrected, BorderLayout.CENTER );
        }
        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add( resPanel, gbc );

        JPanel outputOptPanel = new JPanel( new GridLayout( 1, 2 ) );

        destinationPanel = new JPanel( new BorderLayout() );
        destinationPanel.setForeground( Color.black );
        destinationPanel.setBorder( buildTitledBorder( "Destination" ) );
        outputOptPanel.add( destinationPanel );

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton( "New image", true );
        newImage.setBounds( 10, 16, 120, 25 );
        newImage.setFont( serif12 );
        destinationGroup.add( newImage );
        destinationPanel.add( newImage, BorderLayout.NORTH );

        replaceImage = new JRadioButton( "Replace image", false );
        replaceImage.setFont( serif12 );
        destinationGroup.add( replaceImage );
        destinationPanel.add( replaceImage, BorderLayout.CENTER );

        // Only if the image is unlocked can it be replaced.
        if ( image.getLockStatus() == ModelStorageBase.UNLOCKED ) {
            replaceImage.setEnabled( true );
        } else {
            replaceImage.setEnabled( false );
        }

        imageVOIPanel = new JPanel();
        imageVOIPanel.setLayout( new BorderLayout() );
        imageVOIPanel.setForeground( Color.black );
        imageVOIPanel.setBorder( buildTitledBorder( "Process" ) );
        outputOptPanel.add( imageVOIPanel );

        imageVOIGroup = new ButtonGroup();
        wholeImage = new JRadioButton( "Whole image", true );
        wholeImage.setFont( serif12 );
        imageVOIGroup.add( wholeImage );
        imageVOIPanel.add( wholeImage, BorderLayout.NORTH );

        VOIRegions = new JRadioButton( "VOI region(s)", false );
        VOIRegions.setFont( serif12 );
        imageVOIGroup.add( VOIRegions );
        imageVOIPanel.add( VOIRegions, BorderLayout.CENTER );
        gbc.gridx = 0;
        gbc.gridy = 2;
        mainPanel.add( outputOptPanel, gbc );

        getContentPane().add( mainPanel, BorderLayout.CENTER );
        getContentPane().add( buildButtons(), BorderLayout.SOUTH );
        pack();
        setResizable( true );
        // setVisible(true);

        System.gc();
    }

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
     *	Accessor that sets the region flag.
     *	@param flag		<code>true</code> indicates the whole image is blurred, <code>false</code> indicates a region.
     */
    public void setRegionFlag( boolean flag ) {
        regionFlag = flag;
    }

    /**
     *	Accessor that sets the slicing flag.
     *	@param flag		<code>true</code> indicates slices should be blurred independently.
     */
    public void setImage25D( boolean flag ) {
        image25D = flag;
    }

    /**
     *	Accessor that sets the x scale.
     *	@param scale	Value to set x scale to (should be between 0.0 and 10.0).
     */
    public void setScaleX( float scale ) {
        scaleX = scale;
    }

    /**
     *	Accessor that sets the y scale.
     *	@param scale	Value to set y scale to (should be between 0.0 and 10.0).
     */
    public void setScaleY( float scale ) {
        scaleY = scale;
    }

    /**
     *	Accessor that sets the z scale.
     *	@param scale	Value to set z scale to (should be between 0.0 and 10.0).
     */
    public void setScaleZ( float scale ) {
        scaleZ = scale;
    }

    /**
     *	Accessor that sets the amplication factor.
     *	@param ampFact		should range between 0.5 and 2.
     */
    public void setAmpFactor( float ampFact ) {
        ampFactor = ampFact;
    }

    /**
     *	Closes dialog box when the OK button is pressed, sets variables and calls algorithm.
     *	@param event       Event that triggers function.
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
            MipavUtil.showHelp( "10013" );
        }

    }

    /**
     *	Use the GUI results to set up the variables needed to run the algorithm.
     *	@return		<code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        System.gc();
        if ( replaceImage.isSelected() ) {
            displayLoc = REPLACE;
        } else if ( newImage.isSelected() ) {
            displayLoc = NEW;
        }

        if ( wholeImage.isSelected() ) {
            regionFlag = true;
        } else if ( VOIRegions.isSelected() ) {
            regionFlag = false;
        }

        if ( image25DCheckbox.isSelected() ) {
            image25D = true;
        }

        tmpStr = textGaussX.getText();
        if ( testParameter( tmpStr, 0.0, 10.0 ) ) {
            scaleX = Float.valueOf( tmpStr ).floatValue();
        } else {
            textGaussX.requestFocus();
            textGaussX.selectAll();
            return false;
        }

        tmpStr = textGaussY.getText();
        if ( testParameter( tmpStr, 0.0, 10.0 ) ) {
            scaleY = Float.valueOf( tmpStr ).floatValue();
        } else {
            textGaussY.requestFocus();
            textGaussY.selectAll();
            return false;
        }

        tmpStr = textGaussZ.getText();
        if ( testParameter( tmpStr, 0.0, 10.0 ) ) {
            scaleZ = Float.valueOf( tmpStr ).floatValue();
        } else {
            textGaussZ.requestFocus();
            textGaussZ.selectAll();
            return false;
        }

        tmpStr = textAmpFact.getText();
        if ( testParameter( tmpStr, 1.0, 2.0 ) ) {
            ampFactor = Float.valueOf( tmpStr ).floatValue();
        } else {
            textAmpFact.requestFocus();
            textAmpFact.selectAll();
            return false;
        }

        // Apply normalization if requested!
        if ( resolutionCheckbox.isSelected() ) {
            scaleZ = scaleZ * normFactor;
        }

        return true;
    }

    /**
     *	Once all the necessary variables are set, call the Gaussian Blur
     *	algorithm based on what type of image this is and whether or not there
     *	is a separate destination image.
     */
    private void callAlgorithm() {
        String name = makeImageName( image.getImageName(), "_laplacian" );

        start = System.currentTimeMillis();
        if ( image.getNDims() == 2 ) { // source image is 2D

            float[] sigmas = new float[2];

            sigmas[0] = scaleX; // set standard deviations (sigma) in X and Y
            sigmas[1] = scaleY;

            if ( displayLoc == NEW ) {
                try {
                    // Make result image of float type
                    resultImage = new ModelImage( ModelImage.FLOAT, image.getExtents(), name, userInterface );
                    //resultImage = (ModelImage)image.clone();
                    //resultImage.setImageName(name);
                    if ( ( resultImage.getFileInfo()[0] ).getFileFormat() == FileBase.DICOM ) {
                        ( (FileInfoDicom) ( resultImage.getFileInfo( 0 ) ) ).setValue( "0002,0002",
                                "1.2.840.10008.5.1.4.1.1.7 ", 26 ); // Secondary Capture SOP UID
                        ( (FileInfoDicom) ( resultImage.getFileInfo( 0 ) ) ).setValue( "0008,0016",
                                "1.2.840.10008.5.1.4.1.1.7 ", 26 );
                        ( (FileInfoDicom) ( resultImage.getFileInfo( 0 ) ) ).setValue( "0002,0012", "1.2.840.34379.17",
                                16 ); // bogus Implementation UID made up by Matt
                        ( (FileInfoDicom) ( resultImage.getFileInfo( 0 ) ) ).setValue( "0002,0013", "MIPAV--NIH", 10 ); //
                    }
                    // Make algorithm
                    laplacianAlgo = new AlgorithmLaplacian( resultImage, image, sigmas, regionFlag, false, ampFactor );
                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    laplacianAlgo.addListener( this );
                    // Hide dialog
                    setVisible( false );

                    if ( runInSeparateThread ) {
                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if ( laplacianAlgo.startMethod( Thread.MIN_PRIORITY ) == false ) {
                            MipavUtil.displayError( "A thread is already running on this object" );
                        }
                    } else {
                        laplacianAlgo.setActiveImage( isActiveImage );
                        if ( !userInterface.isAppFrameVisible() ) {
                            laplacianAlgo.setProgressBarVisible( false );
                        }
                        laplacianAlgo.run();
                    }
                } catch ( OutOfMemoryError x ) {
                    MipavUtil.displayError( "Dialog laplacian: unable to allocate enough memory" );
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
                    laplacianAlgo = new AlgorithmLaplacian( image, sigmas, regionFlag, false, ampFactor );
                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    laplacianAlgo.addListener( this );
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
                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if ( laplacianAlgo.startMethod( Thread.MIN_PRIORITY ) == false ) {
                            MipavUtil.displayError( "A thread is already running on this object" );
                        }
                    } else {
                        laplacianAlgo.setActiveImage( isActiveImage );
                        if ( !userInterface.isAppFrameVisible() ) {
                            laplacianAlgo.setProgressBarVisible( false );
                        }
                        laplacianAlgo.run();
                    }
                } catch ( OutOfMemoryError x ) {
                    MipavUtil.displayError( "Dialog laplacian: unable to allocate enough memory" );
                    return;
                }
            }
        } else if ( image.getNDims() == 3 ) {

            float[] sigmas = new float[3];

            sigmas[0] = scaleX;
            sigmas[1] = scaleY;
            sigmas[2] = scaleZ; // normalized  - scaleZ * resolutionX/resolutionZ; !!!!!!!

            if ( displayLoc == NEW ) {
                try {
                    // Make result image of float type

                    //resultImage = new ModelImage(ModelImage.FLOAT, image.getExtents(), name,
                    //                             userInterface);
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName( name );
                    if ( ( resultImage.getFileInfo()[0] ).getFileFormat() == FileBase.DICOM ) {
                        for ( int i = 0; i < resultImage.getExtents()[2]; i++ ) {
                            ( (FileInfoDicom) ( resultImage.getFileInfo( i ) ) ).setValue( "0002,0002",
                                    "1.2.840.10008.5.1.4.1.1.7 ", 26 ); // Secondary Capture SOP UID
                            ( (FileInfoDicom) ( resultImage.getFileInfo( i ) ) ).setValue( "0008,0016",
                                    "1.2.840.10008.5.1.4.1.1.7 ", 26 );
                            ( (FileInfoDicom) ( resultImage.getFileInfo( i ) ) ).setValue( "0002,0012",
                                    "1.2.840.34379.17", 16 ); // bogus Implementation UID made up by Matt
                            ( (FileInfoDicom) ( resultImage.getFileInfo( i ) ) ).setValue( "0002,0013", "MIPAV--NIH", 10 ); //
                        }
                    }
                    // Make algorithm
                    laplacianAlgo = new AlgorithmLaplacian( resultImage, image, sigmas, regionFlag, image25D, ampFactor );
                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    laplacianAlgo.addListener( this );
                    // Hide dialog
                    setVisible( false );

                    if ( runInSeparateThread ) {
                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if ( laplacianAlgo.startMethod( Thread.MIN_PRIORITY ) == false ) {
                            MipavUtil.displayError( "A thread is already running on this object" );
                        }
                    } else {
                        laplacianAlgo.setActiveImage( isActiveImage );
                        if ( !userInterface.isAppFrameVisible() ) {
                            laplacianAlgo.setProgressBarVisible( false );
                        }
                        laplacianAlgo.run();
                    }
                } catch ( OutOfMemoryError x ) {
                    MipavUtil.displayError( "Dialog laplacian: unable to allocate enough memory" );
                    if ( resultImage != null ) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }
                    return;
                }
            } else {
                try {
                    // Make algorithm
                    laplacianAlgo = new AlgorithmLaplacian( image, sigmas, regionFlag, image25D, ampFactor );

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    laplacianAlgo.addListener( this );
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
                        if ( laplacianAlgo.startMethod( Thread.MIN_PRIORITY ) == false ) {
                            MipavUtil.displayError( "A thread is already running on this object" );
                        }
                    } else {
                        laplacianAlgo.setActiveImage( isActiveImage );
                        if ( !userInterface.isAppFrameVisible() ) {
                            laplacianAlgo.setProgressBarVisible( false );
                        }
                        laplacianAlgo.run();
                    }
                } catch ( OutOfMemoryError x ) {
                    MipavUtil.displayError( "Dialog laplacian: unable to allocate enough memory" );
                    return;
                }
            }
        }

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
        ModelImage edgeImage;
        ViewJFrameImage imageFrame = null;

        end = System.currentTimeMillis();
        Preferences.debug( "Laplacian: " + ( end - start ) );

        if ( Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && this.getOwner() != null && !isScriptRunning() ) {
            saveDefaults();
        }

        if ( algorithm instanceof AlgorithmLaplacian ) {
            image.clearMask();
            if ( laplacianAlgo.isCompleted() == true && resultImage != null ) {

                updateFileInfo( image, resultImage );
                resultImage.clearMask();
                // The algorithm has completed and produced a new image to be displayed.
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

        if ( laplacianAlgo != null ) {
            laplacianAlgo.finalize();
            laplacianAlgo = null;
        }
        dispose();
    }

    //*******************************************************************
    //************************* Item Events ****************************
    //*******************************************************************

    /**
     *  Resets labels if checkboxes are checked or unchecked.
     *  @param event         Event that cause the method to fire.
     */
    public void itemStateChanged( ItemEvent event ) {
        Object source = event.getSource();
        float tempNum;

        if ( source == resolutionCheckbox ) {
            if ( resolutionCheckbox.isSelected() ) {
                tempNum = normFactor * Float.valueOf( textGaussZ.getText() ).floatValue();
                labelCorrected.setText( "      Corrected scale = " + makeString( tempNum, 3 ) );
            } else {
                labelCorrected.setText( " " );
            }
        } else if ( source == image25DCheckbox ) {
            if ( image25DCheckbox.isSelected() ) {
                resolutionCheckbox.setEnabled( false ); // Image is only 2D or 2.5D, thus this checkbox
                labelGaussZ.setEnabled( false ); // is not relevent
                textGaussZ.setEnabled( false );
                labelCorrected.setEnabled( false );
            } else {
                resolutionCheckbox.setEnabled( true );
                labelGaussZ.setEnabled( true );
                textGaussZ.setEnabled( true );
                labelCorrected.setEnabled( true );
            }
        }
    }

    /**
     *  When the user clicks the mouse out of a text field, resets the necessary variables.
     *  @param event   Event that triggers this function.
     */
    public void focusLost( FocusEvent event ) {
        Object source = event.getSource();
        JTextField field;
        String text;
        float tempNum;

        if ( source == textGaussZ ) {
            field = (JTextField) source;
            text = field.getText();
            if ( resolutionCheckbox.isSelected() ) {
                tempNum = normFactor * Float.valueOf( textGaussZ.getText() ).floatValue();
                labelCorrected.setText( "      Corrected scale = " + makeString( tempNum, 3 ) );
            } else {
                labelCorrected.setText( " " );
            }
        }
    }

}
