package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.algorithms.*;

import java.awt.event.*;
import java.awt.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user is able to control
 * the degree of blurring in all dimensions. User can indicate whether
 * to have algorithm applied to whole image or to the
 * VOI regions. Algorithms are executed in their own thread.
 *
 * @version    0.1 Nov 17, 1998
 * @author     Matthew J. McAuliffe, Ph.D.
 * @see        AlgorithmGaussianBlur
 */
public class JDialogEdgeLaplacian extends JDialogBase
    implements AlgorithmInterface, ScriptableInterface {

    private ViewUserInterface userInterface;
    private AlgorithmEdgeLaplacian laplacianAlgo;
    private AlgorithmEdgeLaplacianSep laplacianSepAlgo;
    private ModelImage image; // source image
    private ModelImage resultImage = null; // result image

    private String[] titles;

    private JPanel scalePanel;
    private JTextField textGaussX;
    private JLabel labelGaussX;
    private float scaleX;

    private JTextField textGaussY;
    private JLabel labelGaussY;
    private float scaleY;

    private JTextField textGaussZ;
    private JLabel labelGaussZ;
    private float scaleZ;

    private float normFactor = 1; // normalization factor to adjust for resolution
    //  difference between x,y resolutions (in plane)
    //  and z resolution (between planes)
    private JCheckBox resolutionCheckbox;
    private JLabel labelCorrected;

    private JCheckBox sepCheckBox;
    private boolean separable = true;
    private JCheckBox image25DCheckbox;

    private JPanel destinationPanel;
    private ButtonGroup destinationGroup;
    private JRadioButton replaceImage;
    private JRadioButton newImage;

    private JPanel imageVOIPanel;
    private ButtonGroup imageVOIGroup;
    private JRadioButton wholeImage;
    private JRadioButton VOIRegions;

    private int displayLoc; // Flag indicating if a new image is to be generated
    // or if the source image is to be replaced
    private boolean regionFlag; // true = apply algorithm to the whole image
    // false = apply algorithm only to VOI regions
    private boolean image25D = false; // Flag for applying to every slice

    /**
     *  Creates new dialog and displays it.
     *  @param parent          Parent frame.
     *  @param im              Source image.
     */
    public JDialogEdgeLaplacian( Frame theParentFrame, ModelImage im ) {
        super( theParentFrame, true );
        image = im;
        userInterface = ( (ViewJFrameBase) ( parentFrame ) ).getUserInterface();
        init();
    }

    /**
     *	Used primarily for the script to store variables and run the algorithm.  No
     *	actual dialog will appear but the set up info and result image will be stored here.
     *	@param UI   The user interface, needed to create the image frame.
     *	@param im	Source image.
     */
    public JDialogEdgeLaplacian( ViewUserInterface UI, ModelImage im ) {
        super();
        userInterface = UI;
        image = im;
        parentFrame = image.getParentFrame();
    }

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogEdgeLaplacian() {}

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
            setRegionFlag( parser.getNextBoolean() );
            setSeparable( parser.getNextBoolean() );
            setImage25D( parser.getNextBoolean() );
            setScaleX( parser.getNextFloat() );
            setScaleY( parser.getNextFloat() );
            setScaleZ( parser.getNextFloat() );
            //setLoThres(parser.getNextFloat());
            //setHiThres(parser.getNextFloat());
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
                        "EdgeLaplacian " + userInterface.getScriptDialog().getVar( image.getImageName() ) + " " );
                if ( displayLoc == NEW ) {
                    userInterface.getScriptDialog().putVar( resultImage.getImageName() );
                    userInterface.getScriptDialog().append(
                            userInterface.getScriptDialog().getVar( resultImage.getImageName() ) + " " + regionFlag
                            + " " + separable + " " + image25D + " " + scaleX + " " + scaleY + " " + scaleZ );
                } else {
                    userInterface.getScriptDialog().append(
                            userInterface.getScriptDialog().getVar( image.getImageName() ) + " " + regionFlag + " "
                            + separable + " " + image25D + " " + scaleX + " " + scaleY + " " + scaleZ );
                }
                //            userInterface.getScriptDialog().append(" " + loThres + " " + hiThres +
                userInterface.getScriptDialog().append( "\n" );
            }
        }
    }

    /**
     *	Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground( Color.black );

        getContentPane().setLayout( new BorderLayout() );
        setTitle( "EdgeLap" );

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

        scalePanel = new JPanel( new GridLayout( 3, 2 ) );
        scalePanel.setForeground( Color.black );
        scalePanel.setBorder( buildTitledBorder( "Scale of the Gaussian" ) );
        mainPanel.add( scalePanel, gbc );

        labelGaussX = new JLabel( "X Dimension (0.5 - 5.0) " );
        labelGaussX.setForeground( Color.black );
        labelGaussX.setFont( serif12 );
        scalePanel.add( labelGaussX );

        textGaussX = new JTextField();
        textGaussX.setText( "1.0" );
        textGaussX.setFont( serif12 );
        scalePanel.add( textGaussX );

        labelGaussY = new JLabel( "Y Dimension (0.5 - 5.0) " );
        labelGaussY.setForeground( Color.black );
        labelGaussY.setFont( serif12 );
        scalePanel.add( labelGaussY );

        textGaussY = new JTextField();
        textGaussY.setText( "1.0" );
        textGaussY.setBounds( 10, 55, 44, 30 );
        textGaussY.setFont( serif12 );
        scalePanel.add( textGaussY );

        labelGaussZ = new JLabel( "Z Dimension (0.5 - 5.0) " );
        labelGaussZ.setForeground( Color.black );
        labelGaussZ.setFont( serif12 );
        scalePanel.add( labelGaussZ );

        textGaussZ = new JTextField();
        textGaussZ.setText( "1.0" );
        textGaussZ.setFont( serif12 );
        scalePanel.add( textGaussZ );

        JPanel resPanel = new JPanel( new GridBagLayout() );
        GridBagConstraints gbc3 = new GridBagConstraints();

        gbc3.gridwidth = 1;
        gbc3.gridheight = 1;
        gbc3.anchor = gbc.WEST;
        gbc3.weightx = 1;
        gbc3.insets = new Insets( 3, 3, 3, 3 );
        gbc3.fill = GridBagConstraints.HORIZONTAL;
        resPanel.setBorder( buildTitledBorder( "Options" ) );

        sepCheckBox = new JCheckBox( "Use separable convolution kernels" );
        sepCheckBox.setFont( serif12 );
        gbc3.gridx = 0;
        gbc3.gridy = 0;
        resPanel.add( sepCheckBox, gbc3 );
        sepCheckBox.setSelected( true );

        resolutionCheckbox = new JCheckBox( "Use image resolutions to normalize Z scale." );
        resolutionCheckbox.setFont( serif12 );
        gbc3.gridx = 0;
        gbc3.gridy = 1;
        resPanel.add( resolutionCheckbox, gbc3 );
        resolutionCheckbox.setSelected( true );

        image25DCheckbox = new JCheckBox( "Process each slice independently (2.5D)." );
        image25DCheckbox.setFont( serif12 );
        gbc3.gridx = 0;
        gbc3.gridy = 3;
        resPanel.add( image25DCheckbox, gbc3 );
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
            gbc3.gridx = 0;
            gbc3.gridy = 2;
            resPanel.add( labelCorrected, gbc3 );
        }
        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add( resPanel, gbc );

        JPanel outputOptPanel = new JPanel( new GridLayout( 1, 1 ) );

        /*       destinationPanel = new JPanel(new BorderLayout());
         destinationPanel.setForeground(Color.black);
         destinationPanel.setBorder(buildTitledBorder("Destination"));
         outputOptPanel.add(destinationPanel);
         destinationGroup = new ButtonGroup();
         newImage = new JRadioButton("New image", true);
         newImage.setFont(serif12);
         destinationGroup.add(newImage);
         destinationPanel.add(newImage, BorderLayout.NORTH);
         replaceImage = new JRadioButton("Replace image",false);
         replaceImage.setFont(serif12);
         destinationGroup.add(replaceImage);
         destinationPanel.add(replaceImage, BorderLayout.CENTER);
         // Only if the image is unlocked can it be replaced.
         if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
         replaceImage.setEnabled(true);
         }
         else {
         replaceImage.setEnabled(false);
         }
         */
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

        JPanel buttonPanel = new JPanel();

        buildOKButton();
        buttonPanel.add( OKButton );
        buildCancelButton();
        buttonPanel.add( cancelButton );

        getContentPane().add( mainPanel, BorderLayout.CENTER );
        getContentPane().add( buttonPanel, BorderLayout.SOUTH );
        pack();
        setResizable( false );
        setVisible( true );

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
     *	@param scale	Value to set x scale to (should be between 0.5 and 5.0).
     */
    public void setScaleX( float scale ) {
        scaleX = scale;
    }

    /**
     *	Accessor that sets the y scale.
     *	@param scale	Value to set y scale to (should be between 0.5 and 5.0).
     */
    public void setScaleY( float scale ) {
        scaleY = scale;
    }

    /**
     *	Accessor that sets the z scale.
     *	@param scale	Value to set z scale to (should be between 0.5 and 5.0).
     */
    public void setScaleZ( float scale ) {
        scaleZ = scale;
    }

    /**
     *	Accessor that sets the low threshold for the edge image.
     *	@param scale	Value to set low threshold to (should be between -10000.0 and 0.0).
     */
    //  public void setLoThres(float scale) {
    //    loThres = scale;
    //  }

    /**
     *	Accessor that sets the high threshold for the edge image.
     *	@param scale	Value to set high threshold to (should be between 0.0 and 10000.0).
     */
    //  public void setHiThres(float scale) {
    //    hiThres = scale;
    //  }

    /**
     *    Accessor that sets whether or not the separable convolution kernel is used
     *    @param separable
     */
    public void setSeparable( boolean separable ) {
        this.separable = separable;
    }

    /**
     *  Closes dialog box when the OK button is pressed and calls the algorithm
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
        }
    }

    /**
     *	Use the GUI results to set up the variables needed to run the algorithm.
     *	@return		<code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        //if (replaceImage.isSelected())    displayLoc = REPLACE;
        //else if (newImage.isSelected())   displayLoc = NEW;

        if ( wholeImage.isSelected() ) {
            regionFlag = true;
        } else if ( VOIRegions.isSelected() ) {
            regionFlag = false;
        }

        if ( image25DCheckbox.isSelected() ) {
            image25D = true;
        }

        tmpStr = textGaussX.getText();
        if ( testParameter( tmpStr, 0.5, 5.0 ) ) {
            scaleX = Float.valueOf( tmpStr ).floatValue();
        } else {
            textGaussX.requestFocus();
            textGaussX.selectAll();
            return false;
        }

        tmpStr = textGaussY.getText();
        if ( testParameter( tmpStr, 0.5, 5.0 ) ) {
            scaleY = Float.valueOf( tmpStr ).floatValue();
        } else {
            textGaussY.requestFocus();
            textGaussY.selectAll();
            return false;
        }
        tmpStr = textGaussZ.getText();
        if ( testParameter( tmpStr, 0.5, 5.0 ) ) {
            scaleZ = Float.valueOf( tmpStr ).floatValue();
        } else {
            textGaussZ.requestFocus();
            textGaussZ.selectAll();
            return false;
        }

        separable = sepCheckBox.isSelected();
        if ( resolutionCheckbox.isSelected() ) {
            scaleZ = scaleZ * normFactor;
        }

        return true;
    }

    /**
     *	Once all the necessary variables are set, call the
     *	algorithm based on what type of image this is and whether or not there
     *	is a separate destination image.
     */
    private void callAlgorithm() {
        System.gc();
        String name = makeImageName( image.getImageName(), "_edgeLap" );

        if ( ( image.getNDims() == 2 ) && separable ) { // source image is 2D and separable convolution
            int[] destExtents = new int[2];

            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            float[] sigmas = new float[2];

            sigmas[0] = scaleX; // set standard deviations (sigma) in X and Y
            sigmas[1] = scaleY;

            //if (displayLoc == NEW) {
            try {
                // Make result image of float type
                // Do not clone because you must have the ability to generate negative
                // values and this will not happen if your image is UBYTE or USHORT
                resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name,
                                             userInterface);
                if ( ( resultImage.getFileInfo()[0] ).getFileFormat() == FileBase.DICOM ) {
                    ( (FileInfoDicom) ( resultImage.getFileInfo( 0 ) ) ).setValue( "0002,0002",
                            "1.2.840.10008.5.1.4.1.1.7 ", 26 ); // Secondary Capture SOP UID
                    ( (FileInfoDicom) ( resultImage.getFileInfo( 0 ) ) ).setValue( "0008,0016",
                            "1.2.840.10008.5.1.4.1.1.7 ", 26 );
                    ( (FileInfoDicom) ( resultImage.getFileInfo( 0 ) ) ).setValue( "0002,0012", "1.2.840.34379.17", 16 ); // bogus Implementation UID made up by Matt
                    ( (FileInfoDicom) ( resultImage.getFileInfo( 0 ) ) ).setValue(
                            "0002,0013", "MIPAV--NIH", 10 ); //
                }

                // Make algorithm
                //        laplacianSepAlgo = new AlgorithmEdgeLaplacianSep(resultImage, image, sigmas,
                //                                             regionFlag, image25D, loThres,
                //                                             hiThres);
                laplacianSepAlgo = new AlgorithmEdgeLaplacianSep( resultImage, image, sigmas, regionFlag, image25D );

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                laplacianSepAlgo.addListener( this );
                // Hide dialog
                setVisible( false );

                if ( runInSeparateThread ) {
                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if ( laplacianSepAlgo.startMethod( Thread.MIN_PRIORITY ) == false ) {
                        MipavUtil.displayError( "A thread is already running on this object" );
                    }
                } else {
                    laplacianSepAlgo.setActiveImage( isActiveImage );
                    if ( !userInterface.isAppFrameVisible() ) {
                        laplacianSepAlgo.setProgressBarVisible( false );
                    }
                    laplacianSepAlgo.run();
                }
            } catch ( OutOfMemoryError x ) {
                MipavUtil.displayError( "Dialog laplacian: unable to allocate enough memory" );
                if ( resultImage != null ) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }
                return;
            }
        } else if ( ( image.getNDims() == 3 ) && separable ) { // separable convolution kernels
            int[] destExtents = new int[3];

            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];

            float[] sigmas = new float[3];

            sigmas[0] = scaleX;
            sigmas[1] = scaleY;
            sigmas[2] = scaleZ;

            //if (displayLoc == NEW) {
            try {
                // Make result image of float type
                // Do not clone because you must have the ability to generate negative
                // values and this will not happen if your image is UBYTE or USHORT
                resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name,
                                             userInterface);
                if ( ( resultImage.getFileInfo()[0] ).getFileFormat() == FileBase.DICOM ) {
                    for ( int i = 0; i < resultImage.getExtents()[2]; i++ ) {
                        ( (FileInfoDicom) ( resultImage.getFileInfo( i ) ) ).setValue( "0002,0002",
                                "1.2.840.10008.5.1.4.1.1.7 ", 26 ); // Secondary Capture SOP UID
                        ( (FileInfoDicom) ( resultImage.getFileInfo( i ) ) ).setValue( "0008,0016",
                                "1.2.840.10008.5.1.4.1.1.7 ", 26 );
                        ( (FileInfoDicom) ( resultImage.getFileInfo( i ) ) ).setValue( "0002,0012", "1.2.840.34379.17",
                                16 ); // bogus Implementation UID made up by Matt
                        ( (FileInfoDicom) ( resultImage.getFileInfo( i ) ) ).setValue( "0002,0013", "MIPAV--NIH", 10 ); //
                    }
                }

                // Make algorithm
                //       laplacianSepAlgo = new AlgorithmEdgeLaplacianSep(resultImage, image, sigmas,
                //                                             regionFlag, image25D,
                //                                             loThres, hiThres);
                laplacianSepAlgo = new AlgorithmEdgeLaplacianSep( resultImage, image, sigmas, regionFlag, image25D );

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                laplacianSepAlgo.addListener( this );
                // Hide dialog
                setVisible( false );

                if ( runInSeparateThread ) {
                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if ( laplacianSepAlgo.startMethod( Thread.MIN_PRIORITY ) == false ) {
                        MipavUtil.displayError( "A thread is already running on this object" );
                    }
                } else {
                    laplacianSepAlgo.setActiveImage( isActiveImage );
                    if ( !userInterface.isAppFrameVisible() ) {
                        laplacianSepAlgo.setProgressBarVisible( false );
                    }

                    laplacianSepAlgo.run();
                }
            } catch ( OutOfMemoryError x ) {
                MipavUtil.displayError( "Dialog laplacian: unable to allocate enough memory" );
                if ( resultImage != null ) {
                    resultImage.disposeLocal(); // Clean up image memory
                    resultImage = null;
                }
                return;
            }
        } else if ( image.getNDims() == 2 ) { // source image is 2D and convolution kernel not separable
            int[] destExtents = new int[2];

            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            float[] sigmas = new float[2];

            sigmas[0] = scaleX; // set standard deviations (sigma) in X and Y
            sigmas[1] = scaleY;

            //if (displayLoc == NEW) {
            try {
                // Make result image of float type
                // Do not clone because you must have the ability to generate negative
                // values and this will not happen if your image is UBYTE or USHORT
                resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name,
                                             userInterface);
                if ( ( resultImage.getFileInfo()[0] ).getFileFormat() == FileBase.DICOM ) {
                    ( (FileInfoDicom) ( resultImage.getFileInfo( 0 ) ) ).setValue( "0002,0002",
                            "1.2.840.10008.5.1.4.1.1.7 ", 26 ); // Secondary Capture SOP UID
                    ( (FileInfoDicom) ( resultImage.getFileInfo( 0 ) ) ).setValue( "0008,0016",
                            "1.2.840.10008.5.1.4.1.1.7 ", 26 );
                    ( (FileInfoDicom) ( resultImage.getFileInfo( 0 ) ) ).setValue( "0002,0012", "1.2.840.34379.17", 16 ); // bogus Implementation UID made up by Matt
                    ( (FileInfoDicom) ( resultImage.getFileInfo( 0 ) ) ).setValue(
                            "0002,0013", "MIPAV--NIH", 10 ); //
                }

                // Make algorithm
                //laplacianAlgo = new AlgorithmEdgeLaplacian(resultImage, image, sigmas,
                //                                     regionFlag, image25D, loThres,
                //                                     hiThres);
                laplacianAlgo = new AlgorithmEdgeLaplacian( resultImage, image, sigmas, regionFlag, image25D );
                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                laplacianAlgo.addListener( this );
                // Hide dialog
                setVisible( false );

                if ( runInSeparateThread ) {
                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if ( laplacianAlgo.startMethod( Thread.MIN_PRIORITY ) == false ) {
                        MipavUtil.displayError( "A thread is already running on this object" );
                    }
                } else {
                    laplacianAlgo.setActiveImage( isActiveImage );
                    if ( !userInterface.isAppFrameVisible() ) {
                        laplacianSepAlgo.setProgressBarVisible( false );
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
        } else if ( image.getNDims() == 3 ) { // convolution kernel is not separable
            int[] destExtents = new int[3];

            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];

            float[] sigmas = new float[3];

            sigmas[0] = scaleX;
            sigmas[1] = scaleY;
            sigmas[2] = scaleZ;

            //if (displayLoc == NEW) {
            try {
                // Make result image of float type
                // Do not clone because you must have the ability to generate negative
                // values and this will not happen if your image is UBYTE or USHORT
                resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name,
                                             userInterface);
                if ( ( resultImage.getFileInfo()[0] ).getFileFormat() == FileBase.DICOM ) {
                    for ( int i = 0; i < resultImage.getExtents()[2]; i++ ) {
                        ( (FileInfoDicom) ( resultImage.getFileInfo( i ) ) ).setValue( "0002,0002",
                                "1.2.840.10008.5.1.4.1.1.7 ", 26 ); // Secondary Capture SOP UID
                        ( (FileInfoDicom) ( resultImage.getFileInfo( i ) ) ).setValue( "0008,0016",
                                "1.2.840.10008.5.1.4.1.1.7 ", 26 );
                        ( (FileInfoDicom) ( resultImage.getFileInfo( i ) ) ).setValue( "0002,0012", "1.2.840.34379.17",
                                16 ); // bogus Implementation UID made up by Matt
                        ( (FileInfoDicom) ( resultImage.getFileInfo( i ) ) ).setValue( "0002,0013", "MIPAV--NIH", 10 ); //
                    }
                }

                // Make algorithm
                //        laplacianAlgo = new AlgorithmEdgeLaplacian(resultImage, image, sigmas,
                //                                             regionFlag, image25D,
                //                                             loThres, hiThres);
                laplacianAlgo = new AlgorithmEdgeLaplacian( resultImage, image, sigmas, regionFlag, image25D );
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
                        laplacianSepAlgo.setProgressBarVisible( false );
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

        if ( algorithm instanceof AlgorithmEdgeLaplacianSep ) {
            image.clearMask();
            if ( laplacianSepAlgo.isCompleted() == true && resultImage != null ) {

                //updateFileInfo(image, resultImage);
                //resultImage.clearMask();
                //The algorithm has completed and produced a new image to be displayed.
                try {

                    edgeImage = laplacianSepAlgo.getZeroXMask();
                    resultImage.disposeLocal();
                    resultImage = null;
                    imageFrame = new ViewJFrameImage(edgeImage);

                } catch ( OutOfMemoryError error ) {
                    MipavUtil.displayError( "Out of memory: unable to open new frame" );
                }
            } // if (laplacianSepAlgo.isCompleted() == true && resultImage != null)
            else if ( resultImage != null ) {
                //algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        } // if (algorithm instanceof AlgorithmEdgeLaplacianSep)

        if ( algorithm instanceof AlgorithmEdgeLaplacian ) {
            image.clearMask();
            if ( laplacianAlgo.isCompleted() == true && resultImage != null ) {
                //The algorithm has completed and produced a new image to be displayed.
                try {
                    //resultImage.setImageName("EdgeLap");
                    //imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610,200), userInterface);

                    edgeImage = laplacianAlgo.getZeroXMask();
                    resultImage.disposeLocal();
                    imageFrame = new ViewJFrameImage(edgeImage);

                } catch ( OutOfMemoryError error ) {
                    MipavUtil.displayError( "Out of memory: unable to open new frame" );
                }
            } // if (laplacianAlgo.isCompleted() == true && resultImage != null)
            else if ( resultImage != null ) {
                //algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        } // if (algorithm instanceof AlgorithmEdgeLaplacian)

        insertScriptLine( algorithm );

        if ( laplacianSepAlgo != null ) {
            laplacianSepAlgo.finalize();
            laplacianSepAlgo = null;
        }
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
     *  itemStateChanged -   method to handle item events
     *  @param event         event that cause the method to fire
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
     *   setThresholdOptions enable or disable threshold options
     *   @param  flag indicating if labels and text fields should be enabled.
     */
    //  private void setThresholdOptions(boolean able) {
    //
    //    labelThres1.setEnabled(able);
    //    labelThres2.setEnabled(able);
    //    textThres1.setEnabled(able);
    //    textThres2.setEnabled(able);
    //  }

    /**
     *  focusLost    - when the user clicks the mouse out of a text field,
     *                 resets the neccessary variables.
     *  @param event   event that triggers this function
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
