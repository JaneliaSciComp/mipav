package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;

import java.awt.event.*;
import java.awt.*;
import java.util.*;

import javax.swing.*;


/**
 *   Dialog to get user input, then call the algorithm.  The user has the
 *   option to generate a new image or replace the source image. In addition the user
 *   can indicate if you wishes to have the algorithm applied to whole image or to the
 *   VOI regions. In should be noted, that the algorithms are executed in their own
 *   thread.
 *
 *		@version    0.1 Dec 21, 1999
 *		@author     Matthew J. McAuliffe, Ph.D.
 */
public class JDialogImageMath extends JDialogBase
    implements AlgorithmInterface, ScriptableInterface, DialogDefaultsInterface {

    private AlgorithmImageMath mathAlgo;
    private ModelImage image; // source image
    private ModelImage resultImage = null; // result image
    private ViewUserInterface userInterface;

    private String[] titles;

    private JPanel inputPanel;
    private JTextField textValue;
    private JLabel labelValue;
    private float value;
    private boolean useComplex = false;
    private JTextField textValueI;
    private JLabel labelValueI;
    private float valueI = 0.0f;

    private JComboBox comboBoxOperator;
    private JLabel labelOperator;
    private int opType;

    private ButtonGroup group;
    private JRadioButton radioClip;
    private JRadioButton radioPromote;

    private JPanel destinationPanel;
    private ButtonGroup destinationGroup;
    private JRadioButton replaceImage;
    private JRadioButton newImage;

    private JPanel imageVOIPanel;
    private ButtonGroup imageVOIGroup;
    private JRadioButton wholeImage;
    private JRadioButton VOIRegions;

    private int clipMode = AlgorithmImageMath.CLIP;
    private int displayLoc; // Flag indicating if a new image is to be generated
    // or if the source image is to be replaced
    private boolean regionFlag; // true = apply algorithm to the whole image
    // false = apply algorithm only to VOI regions

    /**
     *  Creates new dialog.
     *  @param theParentFrame    Parent frame
     *  @param im                Source image
     */
    public JDialogImageMath( Frame theParentFrame, ModelImage im ) {
        super( theParentFrame, true );
        image = im;
        if ((image.getType() == ModelStorageBase.COMPLEX) ||
            (image.getType() == ModelStorageBase.DCOMPLEX)) {
            useComplex = true;
        }
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
    public JDialogImageMath( ViewUserInterface UI, ModelImage im ) {
        super();
        userInterface = UI;
        image = im;
        if ((image.getType() == ModelStorageBase.COMPLEX) ||
            (image.getType() == ModelStorageBase.DCOMPLEX)) {
            useComplex = true;
        }
        parentFrame = image.getParentFrame();
    }

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogImageMath() {}

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
            setValue( parser.getNextFloat() );
            setValueI( parser.getNextFloat() );
            setOperator( parser.getNextInteger() );
            setClipMode( parser.getNextInteger() );
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
                        "ImageMath " + userInterface.getScriptDialog().getVar( image.getImageName() ) + " " );
                if ( displayLoc == NEW ) {
                    userInterface.getScriptDialog().putVar( resultImage.getImageName() );
                    userInterface.getScriptDialog().append(
                            userInterface.getScriptDialog().getVar( resultImage.getImageName() ) + " " + getParameterString(" ") + "\n" );
                } else {
                    userInterface.getScriptDialog().append(
                            userInterface.getScriptDialog().getVar( image.getImageName() ) + " " + getParameterString(" ") + "\n" );

                }
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
        str += value + delim;
        str += valueI + delim;
        str += opType + delim;
        str += clipMode;

        return str;
    }

    /**
     *  Loads the default settings from Preferences to set up the dialog
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults( getDialogName() );

        if ( defaultsString != null && comboBoxOperator != null ) {
            try {
                StringTokenizer st = new StringTokenizer( defaultsString, "," );
             //   System.err.println("defaultsSTring is: "+ defaultsString);

                if ( MipavUtil.getBoolean( st ) ) {
                    wholeImage.setSelected( true );
                } else {
                    VOIRegions.setSelected( true );
                }

                textValue.setText( st.nextToken() );

                String iString = st.nextToken();
               if (useComplex) {
                    textValueI.setText(iString);
                }

                int selection = MipavUtil.getInt( st );
                if ( comboBoxOperator.getItemCount() > selection) {
                    comboBoxOperator.setSelectedIndex(selection);
                }

                int mode = MipavUtil.getInt( st );
                if ( mode == AlgorithmImageMath.CLIP ) {
                    radioClip.setSelected( true );
                } else {
                    radioPromote.setSelected( true );
                }

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
        String defaultsString = new String( getParameterString(",") + "," + newImage.isSelected() );

        Preferences.saveDialogDefaults( getDialogName(), defaultsString );
    }

    /**
     *	Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground( Color.black );
        setTitle( "Image Math" );

        inputPanel = new JPanel( new GridBagLayout() );
        inputPanel.setForeground( Color.black );
        inputPanel.setBorder( buildTitledBorder( "Input parameters" ) );

        if (useComplex) {
            labelValue = new JLabel("Real value");
        }
        else {
            labelValue = new JLabel("Value");
        }
        labelValue.setForeground( Color.black );
        labelValue.setFont( serif12 );

        textValue = new JTextField( 5 );
        textValue.setText( "1.0" );
        textValue.setFont( serif12 );

        if (useComplex) {
            labelValueI = new JLabel( "Imaginary value" );
            labelValueI.setForeground( Color.black );
            labelValueI.setFont( serif12 );

            textValueI = new JTextField( 5 );
            textValueI.setText( "0.0" );
            textValueI.setFont( serif12 );
        } // if (useComplex)

        labelOperator = new JLabel( "Operator" );
        labelOperator.setForeground( Color.black );
        labelOperator.setFont( serif12 );

        comboBoxOperator = new JComboBox();
        comboBoxOperator.setFont( serif12 );
        comboBoxOperator.setBackground( Color.white );

        comboBoxOperator.addItem( "Absolute Value" );
        comboBoxOperator.addItem( "Add" );
        comboBoxOperator.addItem("Average");
        comboBoxOperator.addItem( "Constant" );
        comboBoxOperator.addItem( "Divide" );
        comboBoxOperator.addItem( "Log" );
        comboBoxOperator.addItem( "Multiply" );
        comboBoxOperator.addItem( "Square" );
        comboBoxOperator.addItem( "Square Root" );
        comboBoxOperator.addItem( "Subtract" );
        comboBoxOperator.addItem( "Sum" );

        comboBoxOperator.addItemListener( this );
        comboBoxOperator.setSelectedIndex(0);

        group = new ButtonGroup();
        radioClip = new JRadioButton( "Clip", true );
        radioClip.setFont( serif12 );
        group.add( radioClip );

        radioPromote = new JRadioButton( "Promote image type", false );
        radioPromote.setFont( serif12 );
        group.add( radioPromote );

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = gbc.WEST;
        gbc.insets = new Insets( 5, 5, 5, 5 );

        int yPos = 0;
        gbc.gridx = 0;
        gbc.gridy = yPos;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        inputPanel.add( labelValue, gbc );
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        inputPanel.add( textValue, gbc );
        if (useComplex) {
            gbc.gridx = 0;
            gbc.gridy = yPos;
            gbc.weightx = 0;
            gbc.fill = gbc.NONE;
            inputPanel.add( labelValueI, gbc );
            gbc.gridx = 1;
            gbc.gridy = yPos++;
            gbc.weightx = 1;
            gbc.fill = gbc.HORIZONTAL;
            inputPanel.add( textValueI, gbc );
        } // if (useComplex)
        gbc.gridx = 0;
        gbc.gridy = yPos;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        inputPanel.add( labelOperator, gbc );
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        inputPanel.add( comboBoxOperator, gbc );
        gbc.gridx = 0;
        gbc.gridy = yPos++;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        gbc.gridwidth = 2;
        gbc.insets = new Insets( 0, 0, 0, 0 );
        inputPanel.add( radioClip, gbc );
        gbc.gridy = yPos++;
        inputPanel.add( radioPromote, gbc );

        destinationPanel = new JPanel( new GridBagLayout() );
        destinationPanel.setForeground( Color.black );
        destinationPanel.setBorder( buildTitledBorder( "Destination" ) );

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton( "New image", true );
        newImage.setFont( serif12 );
        destinationGroup.add( newImage );

        replaceImage = new JRadioButton( "Replace image", false );
        replaceImage.setFont( serif12 );
        destinationGroup.add( replaceImage );

        // Only if the image is unlocked can it be replaced.
        if ( image.getLockStatus() == ModelStorageBase.UNLOCKED ) {
            replaceImage.setEnabled( true );
        } else {
            replaceImage.setEnabled( false );
        }

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        destinationPanel.add( newImage, gbc );
        gbc.gridy = 1;
        destinationPanel.add( replaceImage, gbc );

        imageVOIPanel = new JPanel( new GridBagLayout() );
        imageVOIPanel.setForeground( Color.black );
        imageVOIPanel.setBorder( buildTitledBorder( "Process" ) );

        imageVOIGroup = new ButtonGroup();
        wholeImage = new JRadioButton( "Whole image", true );
        wholeImage.setFont( serif12 );
        imageVOIGroup.add( wholeImage );

        VOIRegions = new JRadioButton( "VOI region(s)", false );
        VOIRegions.setFont( serif12 );
        imageVOIGroup.add( VOIRegions );

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        imageVOIPanel.add( wholeImage, gbc );
        gbc.gridy = 1;
        imageVOIPanel.add( VOIRegions, gbc );

        JPanel mainPanel = new JPanel( new GridBagLayout() );
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 2;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        mainPanel.add( inputPanel, gbc );
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.fill = gbc.BOTH;
        mainPanel.add( destinationPanel, gbc );
        gbc.gridx = 1;
        mainPanel.add( imageVOIPanel, gbc );

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add( OKButton );
        buildCancelButton();
        buttonPanel.add( cancelButton );

        getContentPane().add( mainPanel );
        getContentPane().add( buttonPanel, BorderLayout.SOUTH );

        //set this for Absolute Value
        textValue.setEnabled(false);
        radioClip.setSelected(true);
        radioClip.setEnabled(false);
        radioPromote.setEnabled(false);


        pack();
        //setVisible(true);
    }

    /**
     *   Accessor that returns the image
     *   @return          the result image
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

    /*
     *  Accessor that sets the value to be used when performing the algorithm
     *  @param v value
     */
    public void setValue( float v ) {
        value = v;
    }

    /*
     *  Accessor that sets the valueI to be used when performing the algorithm
     *  @param v value
     */
    public void setValueI( float v ) {
        valueI = v;
    }


    /*
     * Accessor that sets the operator type
     * @param n   operator type
     */
    public void setOperator( int n ) {
        opType = n;
    }

    /* Accessor that sets the clip mode
     * @param n  the clip mode to be used when performing the math algorithm
     */
    public void setClipMode( int n ) {
        clipMode = n;
    }

    /**
     *   Return the correct extension for the new image based on the
     *   given opType.
     *   @param  op          - integer defining the math operation. These
     *                         are defined in AlgorithmImageMath.
     *   @return string      - the proper extension to append to the image
     *                         name.  For instance, for operator = AlgorithmImageMath.ADD
     *                         the extension returned would be "_add".
     */
    public String getOpName( int op ) {

        String name = null;

        if ( op == AlgorithmImageMath.ADD ) {
            name = new String( "_add" );
        } else if ( op == AlgorithmImageMath.SUBTRACT ) {
            name = new String( "_subtract" );
        } else if ( op == AlgorithmImageMath.MULTIPLY ) {
            name = new String( "_multiply" );
        } else if ( op == AlgorithmImageMath.DIVIDE ) {
            name = new String( "_divide" );
        } else if ( op == AlgorithmImageMath.SQUARE ) {
            name = new String( "_square" );
        } else if ( op == AlgorithmImageMath.SQRT ) {
            name = new String( "_sqrt" );
        } else if ( op == AlgorithmImageMath.LOG ) {
            name = new String( "_log" );
        } else if ( op == AlgorithmImageMath.CONSTANT ) {
            name = new String( "_constant" );
        } else if ( op == AlgorithmImageMath.ABSOLUTE_VALUE ) {
            name = new String( "_absolute_value" );
        }else if ( op == AlgorithmImageMath.AVERAGE ) {
            name = new String( "_average" );
        } else if ( op == AlgorithmImageMath.SUM ) {
            name = new String( "_sum" );
        }
        else {
            name = new String( "_math" );
        }

        return name;

    } // end getOpName

    /**
     *	Use the GUI results to set up the variables needed to run the algorithm.
     *	@return		<code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        String tmpStr;

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

        tmpStr = textValue.getText();
        if ( testParameter( tmpStr, -Float.MAX_VALUE, Float.MAX_VALUE ) ) {
            value = Float.valueOf( tmpStr ).floatValue();
        } else {
            textValue.requestFocus();
            textValue.selectAll();
            return false;
        }

        if (useComplex) {
            tmpStr = textValueI.getText();
            if ( testParameter( tmpStr, -Float.MAX_VALUE, Float.MAX_VALUE ) ) {
                valueI = Float.valueOf( tmpStr ).floatValue();
            } else {
                textValueI.requestFocus();
                textValueI.selectAll();
                return false;
            }
        } // if (useComplex)

        if ( radioClip.isSelected() ) {
            clipMode = AlgorithmImageMath.CLIP;
        } else if ( radioPromote.isSelected() ) {
            clipMode = AlgorithmImageMath.PROMOTE;
        }

        return true;
    }

    /**
     *	Once all the necessary variables are set, call the Gaussian Blur
     *	algorithm based on what type of image this is and whether or not there
     *	is a separate destination image.
     */
    private void callAlgorithm() {

        if ( image.getNDims() <= 5 ) {
            if ( displayLoc == NEW ) {
                try {
                    // make the new image name
                    String math = getOpName( opType );
                    String name = makeImageName( image.getImageName(), math );

                    // Make result image of source type

                    if ( opType != AlgorithmImageMath.AVERAGE &&
                         opType != AlgorithmImageMath.SUM) {
                        resultImage = new ModelImage(image.getType(), image.getExtents(), name, userInterface);
                    } else {
                        int [] extents = new int[2];
                        extents[0] = image.getExtents()[0];
                        extents[1] = image.getExtents()[1];

                        if (opType == AlgorithmImageMath.AVERAGE) {
                            resultImage = new ModelImage(image.getType(), extents, name, userInterface);
                        } else {
                            resultImage = new ModelImage(ModelImage.FLOAT, extents, name, userInterface);
                        }
                    }
                    // Make algorithm
                    mathAlgo = new AlgorithmImageMath( resultImage, image, opType, value,
                                                       valueI, clipMode, regionFlag );
                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    mathAlgo.addListener( this );
                    // Hide dialog
                    setVisible( false );

                    if ( runInSeparateThread ) {
                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if ( mathAlgo.startMethod( Thread.MIN_PRIORITY ) == false ) {
                            MipavUtil.displayError( "A thread is already running on this object" );
                        }
                    } else {
                        mathAlgo.setActiveImage( isActiveImage );
                        if ( !userInterface.isAppFrameVisible() ) {
                            mathAlgo.setProgressBarVisible( false );
                        }
                        mathAlgo.run();
                    }
                } catch ( OutOfMemoryError x ) {

                    if ( resultImage != null ) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }
                    System.gc();
                    MipavUtil.displayError( "Dialog Image math: unable to allocate enough memory" );
                    return;
                }
            } else {
                try {
                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    mathAlgo = new AlgorithmImageMath( image, opType, value,
                                                       valueI, clipMode, regionFlag );
                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    mathAlgo.addListener( this );
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
                        if ( mathAlgo.startMethod( Thread.MIN_PRIORITY ) == false ) {
                            MipavUtil.displayError( "A thread is already running on this object" );
                        }
                    } else {
                        mathAlgo.setActiveImage( isActiveImage );
                        if ( !userInterface.isAppFrameVisible() ) {
                            mathAlgo.setProgressBarVisible( false );
                        }
                        mathAlgo.run();
                    }
                } catch ( OutOfMemoryError x ) {
                    System.gc();
                    MipavUtil.displayError( "Dialog Image Math: unable to allocate enough memory" );
                    return;
                }
            }
        }
    }

    /**
     *  Closes dialog box when the OK button is pressed and
     *  calls the algorithm
     *  @param event       event that triggers function
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

        if ( Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && this.getOwner() != null && !isScriptRunning() ) {
            saveDefaults();
        }

        ViewJFrameImage imageFrame = null;
        if ( algorithm instanceof AlgorithmImageMath ) {
            image.clearMask();
            if ( mathAlgo.isCompleted() == true && resultImage != null ) {
                //The algorithm has completed and produced a new image to be displayed.

                updateFileInfo( image, resultImage );
                resultImage.clearMask();

                try {
                    imageFrame = new ViewJFrameImage( resultImage, null, new Dimension( 610, 200 ) );
                } catch ( OutOfMemoryError error ) {
                    System.gc();
                    MipavUtil.displayError( "Out of memory: unable to open new frame" );
                }
            } else if ( resultImage == null ) {
                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registered to the userinterface.
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
                System.gc();
            }
        }

        insertScriptLine( algorithm );

        // Update frame
        if ( parentFrame != null ) {
            ( (ViewJFrameBase) parentFrame ).updateImages( true );
        }
        mathAlgo.finalize();
        mathAlgo = null;
        dispose();
    }

    //*******************************************************************
    //************************* Item Events ****************************
    //*******************************************************************

    /**
     *  itemStateChanged - unchanged
     */
    public void itemStateChanged( ItemEvent event ) {
        Object source = event.getSource();

        textValue.setEnabled( true );
        newImage.setEnabled(true);
        replaceImage.setEnabled(true);
        radioClip.setEnabled(true);
        radioPromote.setEnabled(true);
        int index = 0;

        if ( source == comboBoxOperator ) {
            index = comboBoxOperator.getSelectedIndex();

            if ( index == AlgorithmImageMath.ADD ) {
                opType = AlgorithmImageMath.ADD;
            } else if ( index == AlgorithmImageMath.SUBTRACT) {
                opType = AlgorithmImageMath.SUBTRACT;
            } else if ( index == AlgorithmImageMath.MULTIPLY ) {
                opType = AlgorithmImageMath.MULTIPLY;
            } else if ( index == AlgorithmImageMath.DIVIDE ) {
                opType = AlgorithmImageMath.DIVIDE;
            } else if ( index == AlgorithmImageMath.SQUARE ) {
                textValue.setEnabled( false );
                opType = AlgorithmImageMath.SQUARE;
            } else if ( index == AlgorithmImageMath.SQRT ) {
                textValue.setEnabled( false );
                opType = AlgorithmImageMath.SQRT;
            } else if ( index == AlgorithmImageMath.LOG ) {
                textValue.setEnabled( false );
                opType = AlgorithmImageMath.LOG;
            } else if ( index == AlgorithmImageMath.CONSTANT ) {
                opType = AlgorithmImageMath.CONSTANT;
            } else if ( index == AlgorithmImageMath.ABSOLUTE_VALUE ) {
                textValue.setEnabled(false);
                radioClip.setSelected(true);
                radioClip.setEnabled(false);
                radioPromote.setEnabled(false);
                opType = AlgorithmImageMath.ABSOLUTE_VALUE;
            } else if ( index == AlgorithmImageMath.AVERAGE ) {
                textValue.setEnabled(false);
                newImage.setSelected(true);
                newImage.setEnabled(false);
                replaceImage.setEnabled(false);
                radioClip.setSelected(true);
                radioClip.setEnabled(false);
                radioPromote.setEnabled(false);
                opType = AlgorithmImageMath.AVERAGE;
            } else if ( index == AlgorithmImageMath.SUM ) {
                textValue.setEnabled(false);
                newImage.setSelected(true);
                newImage.setEnabled(false);
                replaceImage.setEnabled(false);
                radioClip.setSelected(true);
                radioClip.setEnabled(false);
                radioPromote.setEnabled(false);
                opType = AlgorithmImageMath.SUM;
            }
        }
    }

    /**
     *  focusLost    - when the user clicks the mouse out of a text field,
     *                 resets the neccessary variables.
     *  @param event   event that triggers this function
     */
    public void focusLost( FocusEvent event ) {}

}
