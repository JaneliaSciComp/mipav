package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.asc.*;
import gov.nih.mipav.model.algorithms.*;

import java.awt.event.*;
import java.awt.*;
import java.io.*;

import javax.swing.*;


/**
 *  Extracts a surface using Tetrahedron Extraction. Triangle decimation can be
 *  invoked to reduce triangle count. The decimation algorithm produces a
 *  continious level of detail (clod) structure that can be used to optimize the
 *  the visualization of the surface. The input to this algorithm is typically a
 *  mask image where 0 = background and 100 = object (i.e. interior to a VOI).
 *  The mask image is then blurred slightly and the level (50) is extracted.
 *  A greyscale image may also be input and a surface is extracted given a level.
 *  The steps are:<p>
 *      <ol>
 *      <li> Build mask image of VOI (i.e. all point interior to VOI are set to 100. All points
 *          exterior are = 0.</li>
 *      <li> Blur mask image if not greyscale</li>
 *      <li> Extract level surface at 50 or user defined level</li>
 *      <li> Save surface ( ".sur")</li>
 *      <li> If decimate then decimate surface and save (".sur")</li>
 *      </ol>
 *		@version    0.1 Aug 2002
 *      @author  Matthew J. McAuliffe, Ph.D.
 *      @author  David H. Eberly, Ph.D. wrote all the extraction and decimation code found in
 *               the SurfaceExtration associated classes.
 *      @see AlgorithmExtractSurface
 *      @see ModelSurfaceExtractor
 *      @see ModelSurfaceDecimator
 *      @see ModelTriangleMesh
 */
public class JDialogASC extends JDialogBase
    implements AlgorithmInterface, ScriptableInterface {

    /**
     *	Extract surface from VOI.
     */
    public final static int VOI_MODE = 0;

    /**
     *	Extract surface from mask image.
     */
    public final static int MASK_MODE = 1;

    /**
     *	Extract surface based on intensity level.
     */
    public final static int LEVEL_MODE = 2;

    private ViewUserInterface userInterface;
    private ASC_Climb3DDecompose extractSurAlgo;
    private ModelImage image; // source image

    private JTextField fileTF;
    private JButton fileButton;
    private String fileName = null;
    private String directory = null;

    private JCheckBox multiResSurfaceCB;
    private JCheckBox blurCheck;
    private JRadioButton maskImageRB;
    private JRadioButton VOIRegionsRB;
    private JRadioButton intensityLevelRB;
    private JTextField intensityTF;
    private JTextField blurTF;
    private JTextField coarseTF;

    private JRadioButton adjRB;
    private JRadioButton smoothRB;
    private JRadioButton noneRB;

    private boolean blurFlag;
    private float blurValue = 0.5f;
    private float level = 0;
    private int coarseValue;
    private int coarseEnd;
    private int mode;
    private int triMode;

    /**
     *   Create a dialog to set variables to extract surface.
     *   @param theParentFrame   Parent frame.
     *   @param im               Source image.
     */
    public JDialogASC( JFrame theParentFrame, ModelImage im ) {
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
    public JDialogASC( ViewUserInterface UI, ModelImage im ) {
        super();
        userInterface = UI;
        image = im;
    }

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogASC() {}

    /**
     * Run this algorithm from a script.
     * @param parser the script parser we get the state from
     * @throws IllegalArgumentException if there is something wrong with the arguments in the script
     */
    public void scriptRun( AlgorithmScriptParser parser )
        throws IllegalArgumentException {
        String srcImageKey = null;

        try {
            srcImageKey = parser.getNextString();
        } catch ( Exception e ) {
            throw new IllegalArgumentException();
        }
        ModelImage im = parser.getImage( srcImageKey );

        image = im;
        userInterface = image.getUserInterface();
        parentFrame = image.getParentFrame();

        try {
            setFileName( parser.getNextString() );
            String mode = parser.getNextString();
            if ( mode.equals( "VOI" ) ) {
                setMode( VOI_MODE );
            } else if ( mode.equals( "MASK" ) ) {
                setMode( MASK_MODE );
            } else if ( mode.equals( "LEVEL" ) ) {
                setMode( LEVEL_MODE );
                setLevel( parser.getNextFloat() );
            } else {
                throw new Exception( "Illegal mode variable." );
            }

            mode = parser.getNextString();
            if ( mode.equals( "NONE" ) ) {
                setTriangleMode( AlgorithmExtractSurface.NONE_MODE );
            } else if ( mode.equals( "ADJACIENT" ) ) {
                setTriangleMode( AlgorithmExtractSurface.ADJ_MODE );
            } else if ( mode.equals( "SMOOTH" ) ) {
                setTriangleMode( AlgorithmExtractSurface.SMOOTH_MODE );
            } else {
                throw new Exception( "Illegal mode variable." );
            }

            setBlurFlag( parser.getNextBoolean() );
            setBlurValue( parser.getNextFloat() );
            setCoarseness( parser.getNextInteger() );
        } catch ( Exception e ) {
            throw new IllegalArgumentException();
        }

        setActiveImage( parser.isActiveImage() );
        setSeparateThread( false );
        callAlgorithm();
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
                        "ASC " + userInterface.getScriptDialog().getVar( image.getImageName() ) + " " + fileName + " " );
                String modeStr;
                if ( mode == VOI_MODE ) {
                    modeStr = "VOI\n";
                } else if ( mode == MASK_MODE ) {
                    modeStr = "MASK\n";
                } else {
                    modeStr = "LEVEL " + level + " ";
                }

                String triModeStr;
                if ( triMode == ASC_Climb3DDecompose.NONE_MODE ) {
                    triModeStr = "NONE ";
                } else if ( triMode == ASC_Climb3DDecompose.ADJ_MODE ) {
                    triModeStr = "ADJACIENT ";
                } else {
                    triModeStr = "SMOOTH ";
                }
                //** need to add matt 4/2003
                //extractSurAlgo.setFLevel (level);
                //if (coarseValue > 7) coarseValue = 7;
                //extractSurAlgo.setIDepth (coarseValue);
                //extractSurAlgo.setIOrientTriangles(-1);

                modeStr = modeStr + triModeStr + blurFlag + " " + blurValue + " " + coarseValue + "\n";
                userInterface.getScriptDialog().append( modeStr );
            }
        }
    }

    /**
     *	Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
     */
    private void init() {
        setForeground( Color.black );
        setTitle( "Adaptive Skeleton Climbing" );

        JPanel imageVOIPanel = new JPanel( new GridBagLayout() );
        imageVOIPanel.setForeground( Color.black );
        imageVOIPanel.setBorder( buildTitledBorder( "Find surface of:" ) );

        ButtonGroup imageVOIGroup = new ButtonGroup();
        maskImageRB = new JRadioButton( "Mask image", false );
        maskImageRB.setFont( serif12 );
        imageVOIGroup.add( maskImageRB );
        maskImageRB.addItemListener( this );

        VOIRegionsRB = new JRadioButton( "VOI region", true );
        VOIRegionsRB.setFont( serif12 );
        imageVOIGroup.add( VOIRegionsRB );
        VOIRegionsRB.addItemListener( this );

        intensityLevelRB = new JRadioButton( "Intensity level", false );
        intensityLevelRB.setFont( serif12 );
        imageVOIGroup.add( intensityLevelRB );
        intensityLevelRB.addItemListener( this );

        intensityTF = new JTextField();
        intensityTF.setText( "50" );
        intensityTF.setEnabled( false );
        intensityTF.setFont( serif12 );

        blurCheck = new JCheckBox( "Blur by (Std. Dev)", false );
        blurCheck.setForeground( Color.black );
        blurCheck.setFont( serif12 );
        blurCheck.addItemListener( this );

        blurTF = new JTextField();
        blurTF.setText( "0.5" );
        blurTF.setFont( serif12 );

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 2;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.anchor = gbc.WEST;

        gbc.gridx = 0;
        gbc.gridy = 0;
        imageVOIPanel.add( VOIRegionsRB, gbc );
        gbc.gridy = 1;
        imageVOIPanel.add( maskImageRB, gbc );
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        imageVOIPanel.add( intensityLevelRB, gbc );
        gbc.gridx = 1;
        gbc.fill = gbc.HORIZONTAL;
        gbc.weightx = 1;
        gbc.insets = new Insets( 0, 10, 0, 0 );
        imageVOIPanel.add( intensityTF, gbc );

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.insets = new Insets( 0, 0, 0, 0 );
        imageVOIPanel.add( blurCheck, gbc );
        gbc.gridx = 1;
        gbc.fill = gbc.HORIZONTAL;
        gbc.weightx = 1;
        gbc.insets = new Insets( 0, 10, 0, 0 );
        imageVOIPanel.add( blurTF, gbc );

        gbc.insets = new Insets( 0, 0, 0, 0 );

        JPanel multiResPanel = new JPanel( new GridBagLayout() );
        multiResPanel.setForeground( Color.black );
        multiResPanel.setBorder( buildTitledBorder( "Surface coarseness" ) );

        int maxDim = image.getExtents()[0];
        if ( image.getExtents()[1] > maxDim ) {
            maxDim = image.getExtents()[1];
        }
        if ( image.getExtents()[2] > maxDim ) {
            maxDim = image.getExtents()[2];
        }

        int nBlock = 8;
        if ( maxDim >= 256 ) {
            nBlock = 8;
        }
        if ( maxDim > 128 && maxDim < 256 ) {
            nBlock = 8;
        }
        if ( maxDim > 64 && maxDim <= 128 ) {
            nBlock = 7;
        }
        if ( maxDim > 32 && maxDim <= 64 ) {
            nBlock = 6;
        }
        if ( maxDim > 16 && maxDim <= 32 ) {
            nBlock = 5;
        }
        if ( maxDim > 8 && maxDim <= 16 ) {
            nBlock = 4;
        }

        coarseEnd = nBlock;
        if ( nBlock >= 8 ) {
            coarseEnd = 7;
        }
        JLabel coarseLabel = new JLabel( "Coarseness [ -1 (most coarse) to " + coarseEnd + " ]" );
        coarseLabel.setForeground( Color.black );
        coarseLabel.setFont( serif12 );

        coarseTF = new JTextField( 5 );
        coarseTF.setText( "7" );
        coarseTF.setFont( serif12 );

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.anchor = gbc.WEST;
        gbc.gridx = 0;
        gbc.gridy = 0;
        multiResPanel.add( coarseLabel, gbc );
        gbc.gridx = 1;
        multiResPanel.add( coarseTF, gbc );

        JPanel imageFixTrianglesPanel = new JPanel( new GridBagLayout() );
        imageFixTrianglesPanel.setForeground( Color.black );
        imageFixTrianglesPanel.setBorder( buildTitledBorder( "Consistant triangle options:" ) );

        ButtonGroup triangleGroup = new ButtonGroup();
        adjRB = new JRadioButton( "Use adjacency method", true );
        adjRB.setFont( serif12 );
        triangleGroup.add( adjRB );
        adjRB.addItemListener( this );

        //smoothRB = new JRadioButton("Use normal smoothing method", false);
        //smoothRB.setFont(serif12);
        //triangleGroup.add(smoothRB);
        //smoothRB.addItemListener(this);

        noneRB = new JRadioButton( "No consistancy check", false );
        noneRB.setFont( serif12 );
        triangleGroup.add( noneRB );
        noneRB.addItemListener( this );

        gbc = new GridBagConstraints();
        gbc.gridwidth = 2;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.anchor = gbc.WEST;

        gbc.gridx = 0;
        gbc.gridy = 0;
        imageFixTrianglesPanel.add( adjRB, gbc );
        gbc.gridy = 1;
        //imageFixTrianglesPanel.add(smoothRB, gbc);
        //gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        imageFixTrianglesPanel.add( noneRB, gbc );

        JPanel filePanel = new JPanel( new GridBagLayout() );
        filePanel.setForeground( Color.black );
        filePanel.setBorder( buildTitledBorder( "Save surface" ) );

        fileButton = new JButton( "Choose..." );
        fileButton.setPreferredSize( MipavUtil.defaultButtonSize );
        fileButton.setFont( serif12B );
        fileButton.addActionListener( this );
        fileButton.setActionCommand( "File" );

        fileTF = new JTextField();
        fileTF.setText( makeImageName( image.getImageName(), ".sur" ) );
        fileTF.setFont( serif12 );

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = gbc.NONE;
        gbc.weightx = 0;
        filePanel.add( fileButton, gbc );
        gbc.gridx = 1;
        gbc.fill = gbc.HORIZONTAL;
        gbc.weightx = 1;
        gbc.insets = new Insets( 0, 10, 0, 0 );
        filePanel.add( fileTF, gbc );
        gbc.insets = new Insets( 0, 0, 0, 0 );

        JPanel mainPanel = new JPanel( new GridBagLayout() );
        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add( imageVOIPanel, gbc );
        gbc.gridy = 1;
        mainPanel.add( multiResPanel, gbc );
        gbc.gridy = 2;
        mainPanel.add( imageFixTrianglesPanel, gbc );
        gbc.gridy = 3;
        mainPanel.add( filePanel, gbc );
        mainPanel.setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add( OKButton );
        buildCancelButton();
        buttonPanel.add( cancelButton );

        mainDialogPanel.add( mainPanel );
        mainDialogPanel.add( buttonPanel, BorderLayout.SOUTH );

        getContentPane().add( mainDialogPanel );

        setResizable( false );
        pack();
        setVisible( true );
    }

    /**
     *	Accessor that sets the image where the surface is to be extracted
     *	@param im   the image (3D image)
     */
    public void setImage( ModelImage im ) {
        image = im;
    }

    /**
     *	Accessor that sets the mode (VOI_MODE, MASK_MODE, or LEVEL_MODE).
     *	@param mode	Mode to set to.
     */
    public void setMode( int mode ) {
        this.mode = mode;
    }

    /**
     *	Accessor that sets the mode (NONE_MODE, ADJ_MODE, or SMOOTH_MODE).
     *	@param triMode	Sets the triangle mode for the surface extraction.
     */
    public void setTriangleMode( int triMode ) {
        this.triMode = triMode;
    }

    /**
     *	Accessor that sets the file name. Only the file name and should end in ".sur"
     *	@param name     name of the file where the surface is to be saved.
     */
    public void setFileName( String name ) {
        fileName = name;
    }

    /**
     *	Accessor that sets the blurring flag (if the surface is generated from a VOI or mask
     *   the surface image will need to be blurred by the Extraction algo before the surface mesh
     *   is extracted. Typically not required in the level surface of grayscale image is needed.
     *	@param flag		true indicates that the surface image should be blurred
     */
    public void setBlurFlag( boolean flag ) {
        blurFlag = flag;
    }

    /**
     *	Accessor that sets the blurring amount
     *	@param sigma the amount of blurring (std. Dev. [0.5 - 5.0]
     */
    public void setBlurValue( float sigma ) {
        blurValue = sigma;
    }

    /**
     *	Accessor that sets the intensity level that defines the surface that is to be extracted
     *	@param intenLevel defines the level surface
     */
    public void setLevel( float intenLevel ) {
        level = intenLevel;
    }

    /**
     *	Accessor that sets the mesh coarseness value.
     *	@param mode	Coarsness of triangle surface (-1 most coarse - 7 finest mesh)
     */
    public void setCoarseness( int coarseness ) {
        coarseValue = coarseness;
    }

    /**
     *  Closes dialog box when the OK button is pressed and calls the algorithm
     *  @param event  event that triggers function
     */
    public void actionPerformed( ActionEvent event ) {
        String command = event.getActionCommand();

        if ( command.equals( "File" ) ) {
            try {
                JFileChooser chooser = new JFileChooser();
                if ( userInterface.getDefaultDirectory() != null ) {
                    chooser.setCurrentDirectory( new File( userInterface.getDefaultDirectory() ) );
                } else {
                    chooser.setCurrentDirectory( new File( System.getProperties().getProperty( "user.dir" ) ) );
                }
                chooser.addChoosableFileFilter( new ViewImageFileFilter( ViewImageFileFilter.SURFACE ) );
                int returnVal = chooser.showSaveDialog( this );
                if ( returnVal == JFileChooser.APPROVE_OPTION ) {
                    fileName = chooser.getSelectedFile().getName();
                    directory = chooser.getCurrentDirectory().getName() + File.separatorChar;
                    fileTF.setText( fileName );
                    userInterface.setDefaultDirectory( "" + chooser.getCurrentDirectory() + File.separatorChar );
                    chooser.setVisible( false );
                }
                return;
            } catch ( OutOfMemoryError e ) {
                MipavUtil.displayError( "Out of memory!" );
                return;
            }
        } else if ( command.equals( "OK" ) ) {
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
        blurFlag = blurCheck.isSelected();
        fileName = fileTF.getText();
        if ( fileName.equals( "" ) ) {
            MipavUtil.displayError( "Enter a file name." );
            fileTF.requestFocus();
            return false;
        }

        String tmpStr = blurTF.getText();
        if ( testParameter( tmpStr, 0.5f, 5.0f ) ) {
            blurValue = Float.valueOf( tmpStr ).floatValue();
        } else {
            intensityTF.requestFocus();
            intensityTF.selectAll();
            return false;
        }

        tmpStr = coarseTF.getText();
        if ( testParameter( tmpStr, -1, coarseEnd ) ) {
            coarseValue = Integer.valueOf( tmpStr ).intValue();
        } else {
            coarseTF.requestFocus();
            coarseTF.selectAll();
            return false;
        }

        if ( VOIRegionsRB.isSelected() ) {
            mode = VOI_MODE;
            int i;
            ViewVOIVector VOIs = image.getVOIs();
            int nVOI;

            nVOI = VOIs.size();
            if ( nVOI == 0 ) {
                MipavUtil.displayError( "Must create a contour VOI" );
                return false;
            }

            short oldID = 0;
            int contourVOI = 0;
            int activeContourVOI = 0;
            for ( i = 0; i < nVOI; i++ ) {
                if ( VOIs.VOIAt( i ).getCurveType() == VOI.CONTOUR ) {
                    contourVOI++;
                    if ( VOIs.VOIAt( i ).isActive() == true ) {
                        activeContourVOI++;
                        // VOI IDs start at 0 therefore ensure VOI ID is > 0
                        //oldID = VOIs.VOIAt(i).getID();
                        //VOIs.VOIAt(i).setID( (short)(oldID + 1));
                    }
                }
            }
            if ( contourVOI == 0 ) {
                MipavUtil.displayError( "Must create a contour VOI" );
                return false;
            }
            if ( ( contourVOI > 1 ) && ( activeContourVOI != 1 ) ) {
                MipavUtil.displayError( "VOI must be selected" );
                return false;
            }

            level = 50;
        } else if ( maskImageRB.isSelected() ) {
            mode = MASK_MODE;
            level = 1;
        } else {
            mode = LEVEL_MODE;
            String levelStr = intensityTF.getText();
            if ( testParameter( levelStr, image.getMin(), image.getMax() ) ) {
                level = Float.valueOf( levelStr ).floatValue();
            } else {
                intensityTF.requestFocus();
                intensityTF.selectAll();
                return false;
            }
        }

        if ( adjRB.isSelected() ) {
            triMode = ASC_Climb3DDecompose.ADJ_MODE;
        } //else if(smoothRB.isSelected()) {
        //    triMode = ASC_Climb3DDecompose.SMOOTH_MODE;
        //}
        else {
            triMode = ASC_Climb3DDecompose.NONE_MODE;
        }
        return true;
    }

    /**
     *	Once all the necessary variables are set, call the Gaussian Blur
     *	algorithm based on what type of image this is and whether or not there
     *	is a separate destination image.
     */
    private void callAlgorithm() {

        // Make algorithm
        extractSurAlgo = new ASC_Climb3DDecompose( image, mode, triMode, blurFlag, blurValue, fileName );
        extractSurAlgo.setFLevel( level );
        if ( coarseValue > 7 ) {
            coarseValue = 7;
        }
        extractSurAlgo.setIDepth( coarseValue );
        extractSurAlgo.setIOrientTriangles( -1 );

        // This is very important. Adding this object as a listener allows the algorithm to
        // notify this object when it has completed of failed. See algorithm performed event.
        // This is made possible by implementing AlgorithmedPerformed interface
        extractSurAlgo.addListener( this );
        // Hide dialog
        setVisible( false );

        if ( runInSeparateThread ) {
            // Start the thread as a low priority because we wish to still have user interface work fast
            if ( extractSurAlgo.startMethod( Thread.MIN_PRIORITY ) == false ) {
                MipavUtil.displayError( "A thread is already running on this object" );
            }
        } else {
            extractSurAlgo.setActiveImage( isActiveImage );
            if ( !userInterface.isAppFrameVisible() ) {
                extractSurAlgo.setProgressBarVisible( false );
            }
            extractSurAlgo.run();
        }
    }

    //************************************************************************
    //************************** Algorithm Events ****************************
    //************************************************************************

    /**
     *  This method is required if the AlgorithmPerformed interface is implemented.
     *  It is called by the  algorithms when it has completed or failed to
     *  to complete, so that the dialog can be display the result image and/or clean up.
     *   @param algorithm   algorithm that caused the event.
     */
    public void algorithmPerformed( AlgorithmBase algorithm ) {

        ViewJFrameImage imageFrame = null;

        if ( algorithm instanceof ASC_Climb3DDecompose ) {
            image.clearMask();
        }

        insertScriptLine( algorithm );

        dispose();
    }

    //*******************************************************************
    //************************* Item Events ****************************
    //*******************************************************************

    /**
     *  Sets text field enabled or disabled depending on source.
     *  @param event Event that triggered this method.
     */
    public void itemStateChanged( ItemEvent event ) {
        Object source = event.getSource();

        if ( source == intensityLevelRB ) {
            intensityTF.setEnabled( true );
        } else {
            intensityTF.setEnabled( false );
        }

        if ( source == blurCheck ) {
            blurTF.setEnabled( blurCheck.isSelected() );
        }
    }
}
