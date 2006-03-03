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
 *  This is the dialog to permit user to perform
 *  Local Normalization.
 *  <p>
 *  Local Normalization equalizes colour levels among pixels
 *  removing variations due to lighting, bringing out contrasts
 *  in detail.
 *  <p>
 *  This dialog presents the X- and Y- gaussian dimensions and the blurring
 *  weight from the JDialogUnsharpMask dialog and the cut-off frequency from
 *  the JDialogFrequencyFilter dialog.  There is also a colour channel
 *  selection panel which permits the selection of colours to be processed when
 *  the image is in aRGB colour.
 *  <p>
 *  Described by Halyo, Rahman and Park:
 *  <blockquote>
 *   Local Normalization seperates the image into a local or
 *  low-frequency signal, and a suface detail or
 *  high-frequency signal.  The locally normalized signal is then
 *  obtained by normalizing (ie., dividing) the detail signal by
 *  the local average.
 *  </blockquote>
 *  References:
 *  <ol>
 *  <li>Local Normalization.  <a href="http://bigwww.epfl.ch/demo/normalize/desc.html">
 *      http://bigwww.epfl.ch/demo/normalize/desc.html</a></li>
 *  <li>Halyo, Nesim; Rahman, Zia-ur; Park, Stephen.  "Information Content
 *      in Nonlinear Local Normalization Processing of Digital Images".
 *      College of William and Mary.  Williamsburg, Virgiana.</li>
 *  </ol>
 *
 *  @see JDialogUnsharpMask
 *  @see JDialogFrequencyFilter
 *  @see AlgorithmLocalNormalization
 */
public class JDialogLocalNormalization extends JDialogBase
    implements AlgorithmInterface, ScriptableInterface, DialogDefaultsInterface {

    /** minimum value for unsharpening variables, at 0.5 */
    public static final float UNSHARP_MIN = (float) 0.5;

    /** maximum value for unsharpening variables, at 5.0 */
    public static final float UNSHARP_MAX = (float) 5.0;

    /** minimum value for unsharpening weighting, at 0.0 */
    public static final float UNSHARP_WEIGHT_MIN = (float) 0.0;

    /** maximum value for unsharpening weighting, at 1.0 */
    public static final float UNSHARP_WEIGHT_MAX = (float) 1.0;

    /** minimum frequency value for blurring frequency, at 0.0 */
    public static final float FREQ_MIN = (float) 0.0;

    /** default frequency value for blurring frequency, at 0.2 */
    public static final float FREQ_DEFAULT = (float) 0.2;

    /** minimum value for blurring, at 1.0 */
    public static final int BLUR_MIN = 1;

    private ViewUserInterface userInterface;
    private AlgorithmLocalNormalization algoLocal;
    private JTextField errorComponent;

    private ModelImage sourceImage;
    private ModelImage resultImage;

    private int displayLoc; // Flag indicating if a new image is to be generated
    // or if the source image is to be replaced

    private boolean isColorImage = false;

    private JTextField unsharpXtext, unsharpYtext, unsharpZtext;
    private JTextField unsharpWeightText;

    // user-selectable variables used in the unsharping operation.
    // used as interim variables in starting the algorithm op.
    private float[] unsharp = { (float) 1.0, (float) 1.0 }; // for smooth filter of mean of f(x,y)
    private float unsharpWeight = (float) 0.75;

    private JTextField blurringFreqText;
    private JTextField blurringDiameterText;

    // user-selectable variables used in the FFT-blurring operation
    // used as interim variables in starting the algorithm op.
    private float blurringFreq = FREQ_DEFAULT;
    private int blurringDiameter = 15; // the default

    private JCheckBox redChannel, greenChannel, blueChannel;

    // used as interim variables in starting the algorithm op.
    private boolean red = true;
    private boolean green = true;
    private boolean blue = true;

    private String[] titles; // used to save image names when replacing an image

    /** Creates an modal extension of JDialogBase, using the title,
     *   "Local Normalization".  Creates an options panel; this
     *   contains: the inputs for the unsharp masking;
     *   the inputs for blurring; and the inputs for choosing
     *   which colour channels to process.  This last set of options
     *   are not selectable on images which are not colour images.
     *   It creates the OKAY and CANCEL buttons on a panel, to b
     *   be placed at the bottom of the dialog.
     *   <p>
     *   The panel is then pack()'d and then setVisible(true).
     */
    public JDialogLocalNormalization( JFrame owner, ModelImage mi ) {
        super( owner, false );
        setTitle( "Local Normalization" );
        userInterface = ( (ViewJFrameBase) ( owner ) ).getUserInterface();
        sourceImage = mi;
        checkColour( mi );
        displayLoc = NEW; // currently replace is not supported

        getContentPane().setLayout( new BorderLayout() );
        getContentPane().add( buildOptionsPanel(), BorderLayout.CENTER );
        getContentPane().add( buildOkayCancelPanel(), BorderLayout.SOUTH );

        pack();
        loadDefaults();
        setVisible( true );
    }

    /** Used primarily for the script to store variables and run the algorithm.
     *  No actual dialog will appear but the set up info and result image will
     *  be stored here.
     *  <p>
     *  Replace is currently not supported.
     *  @param UI   The user interface, needed to create the image frame.
     *  @param im	Source image.
     */
    public JDialogLocalNormalization( ViewUserInterface UI, ModelImage im ) {
        super();
        userInterface = UI;
        sourceImage = im;
        checkColour( im );
        displayLoc = NEW; // currently replace is not supported
        parentFrame = im.getParentFrame();
    }

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogLocalNormalization() {}

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

        sourceImage = im;
        checkColour( im );
        displayLoc = NEW;
        userInterface = sourceImage.getUserInterface();
        parentFrame = sourceImage.getParentFrame();

        // the result image
        try {
            destImageKey = parser.getNextString();
        } catch ( Exception e ) {
            throw new IllegalArgumentException();
        }

        try {
            setUnsharp( parser.getNextFloat(), parser.getNextFloat() );
            setUnsharpWeight( parser.getNextFloat() );
            setBlurringFreq( parser.getNextFloat() );
            setBlurringDiameter( parser.getNextInteger() );
            setRed( parser.getNextBoolean() );
            setGreen( parser.getNextBoolean() );
            setBlue( parser.getNextBoolean() );
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
                //check to see if the  image is already in the ImgTable
                if ( userInterface.getScriptDialog().getImgTableVar( sourceImage.getImageName() ) == null ) {
                    if ( userInterface.getScriptDialog().getActiveImgTableVar( sourceImage.getImageName() ) == null ) {
                        userInterface.getScriptDialog().putActiveVar( sourceImage.getImageName() );
                    }
                }

                userInterface.getScriptDialog().append(
                        "LocalNormalization " + userInterface.getScriptDialog().getVar( sourceImage.getImageName() )
                        + " " );
                if ( displayLoc == NEW ) {
                    userInterface.getScriptDialog().putVar( resultImage.getImageName() );
                    userInterface.getScriptDialog().append(
                            userInterface.getScriptDialog().getVar( resultImage.getImageName() ) + " " + getParameterString(" ") + "\n" );
                } else {
                    userInterface.getScriptDialog().append(
                            userInterface.getScriptDialog().getVar( sourceImage.getImageName() ) + " " + getParameterString(" ") + "\n" );
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
        str += unsharp[0] + delim;
        str += unsharp[1] + delim;
        str += unsharpWeight + delim;
        str += blurringFreq + delim;
        str += blurringDiameter + delim;
        str += red + delim;
        str += green + delim;
        str += blue;

        return str;
    }

    /**
     *  Loads the default settings from Preferences to set up the dialog
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults( getDialogName() );

        if ( defaultsString != null && unsharpXtext != null ) {
            StringTokenizer st = new StringTokenizer( defaultsString, "," );

            try {
                unsharpXtext.setText( "" + MipavUtil.getFloat( st ) );
                unsharpYtext.setText( "" + MipavUtil.getFloat( st ) );
                unsharpWeightText.setText( "" + MipavUtil.getFloat( st ) );
                blurringFreqText.setText( "" + MipavUtil.getFloat( st ) );
                blurringDiameterText.setText( "" + MipavUtil.getInt( st ) );
                redChannel.setSelected( MipavUtil.getBoolean( st ) );
                greenChannel.setSelected( MipavUtil.getBoolean( st ) );
                blueChannel.setSelected( MipavUtil.getBoolean( st ) );
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
        String defaultsString = new String( getParameterString(",") );
        Preferences.saveDialogDefaults( getDialogName(), defaultsString );
    }

    /** part of the algorithm rests on finding the original
     *   image minus an estimation of the local mean.  So
     *   the input panel to set variables related to finding
     *   an unsharp mask image is created here.
     *   <p>
     *   Part of the algorithm rests on blurring the original
     *   image.  So the input panel to set the variables related
     *   to blurring the image is created here.
     *   <p>
     *   A colour image may have any of its three
     *   colour channels filtered, so a colour-selection
     *   panel is created.  A colour panel will be generated even
     *   for a monochrome image, the colour panel will be
     *   disabled.
     */
    private JPanel buildOptionsPanel() {
        JPanel optsp = new JPanel( new BorderLayout() );
        optsp.add( buildUnsharpPanel(), BorderLayout.WEST ); // unsharpmask
        optsp.add( buildBlurringPanel(), BorderLayout.EAST ); // blurring
        optsp.add( buildColourPanel(), BorderLayout.SOUTH ); // colour channel

        return optsp;
    }

    /** part of the algorithm rests on finding the original
     *  image minus an estimation of the local mean.
     *  <p>
     *  This is the same as using as unsharp-mask filter; so, this panel
     *  is a recreation of the inputs made in the JDialogUnsharpMask.
     *  <p>
     *  The panel is returned to the caller.
     *  @see JDialogUnsharpMask
     */
    private JPanel buildUnsharpPanel() {
        JPanel unshrp = new JPanel();
        Insets spacer = new Insets( 0, 10, 0, 0 );
        Insets nospace = new Insets( 0, 0, 0, 0 );

        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();

        unshrp.setLayout( gbl );
        unshrp.setBorder( buildTitledBorder( "Unsharp masking" ) );

        gbc.gridwidth = gbc.RELATIVE;
        gbc.insets = nospace;
        gbc.anchor = gbc.WEST;
        unshrp.add( createLabel( "X-Dimension Gaussian scale:" ), gbc );

        gbc.gridwidth = gbc.REMAINDER;
        gbc.insets = spacer;
        gbc.anchor = gbc.EAST;
        unsharpXtext = createEntryField( "1.0" );
        MipavUtil.makeNumericsOnly( unsharpXtext, true );
        unshrp.add( unsharpXtext, gbc );

        gbc.gridwidth = gbc.RELATIVE;
        gbc.insets = nospace;
        gbc.anchor = gbc.WEST;
        unshrp.add( createLabel( "Y-Dimension Gaussian scale:" ), gbc );

        gbc.gridwidth = gbc.REMAINDER;
        gbc.insets = spacer;
        gbc.anchor = gbc.EAST;
        unsharpYtext = createEntryField( "1.0" );
        MipavUtil.makeNumericsOnly( unsharpYtext, true );
        unshrp.add( unsharpYtext, gbc );

        /* options for Z-dimension are not added to the display because
         * we do not use them.  We may never, but the code is left in
         * just-in-case.
         */
        gbc.gridwidth = gbc.RELATIVE;
        gbc.insets = nospace;
        gbc.anchor = gbc.WEST;
        JLabel zDimlabel = createLabel( "Z-Dimension Gaussian scale:" );
        //unshrp.add(zDimlabel, gbc);

        gbc.gridwidth = gbc.REMAINDER;
        gbc.insets = spacer;
        gbc.anchor = gbc.EAST;
        unsharpZtext = createEntryField( "1.0" );
        MipavUtil.makeNumericsOnly( unsharpZtext, true );
        //unshrp.add(unsharpZtext, gbc);

        zDimlabel.setForeground( Color.gray ); // always set unusable, since always processing
        //unsharpZtext.setEnabled(false); // slices independntly -- ie, 2.5d

        gbc.gridwidth = gbc.RELATIVE;
        gbc.insets = nospace;
        gbc.anchor = gbc.CENTER;
        unshrp.add( createLabel( "Weight of Blur (image - weight*blur):" ), gbc );

        gbc.gridwidth = gbc.REMAINDER;
        gbc.insets = spacer;
        unsharpWeightText = createEntryField( "0.75" );

        MipavUtil.makeNumericsOnly( unsharpWeightText, true );
        unshrp.add( unsharpWeightText, gbc );

        unshrp.setToolTipText( "(original image) - (local mean)" );

        return unshrp;
    }

    /** part of the algorithm rests on blurring the original
     *   image.
     *
     *   This is the same as using as blurring filter; so, this panel
     *   is a modified JDialogFrequencyFilter, permitting only a
     *   Gaussian low-pass, so only a top-end frequency input
     *   is created.
     *   <p>
     *   The panel is returned to the caller.
     *   @see JDialogUnsharpMask
     */
    private JPanel buildBlurringPanel() {
        JPanel blurp = new JPanel();
        Insets spacer = new Insets( 0, 10, 0, 0 );
        Insets nospace = new Insets( 0, 0, 0, 0 );

        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();

        blurp.setLayout( gbl );
        blurp.setBorder( buildTitledBorder( "Blurring Lowpass Filter" ) );

        gbc.anchor = gbc.CENTER;

        gbc.gridwidth = gbc.RELATIVE;
        gbc.insets = nospace;
        blurp.add( createLabel( "Blurring Frequency (f > 0.0):" ), gbc );

        gbc.gridwidth = gbc.REMAINDER;
        gbc.insets = spacer;
        blurringFreqText = createEntryField( "0.2" );
        MipavUtil.makeNumericsOnly( blurringFreqText, true );
        blurp.add( blurringFreqText, gbc );

        /* options for Z-dimension are not added to the display because
         * we do not use them.  We may never, but the code is left in
         * just-in-case.
         */
        gbc.gridwidth = gbc.RELATIVE;
        gbc.insets = nospace;
        //blurp.add(createLabel("Kernel Diameter:"), gbc);

        gbc.gridwidth = gbc.REMAINDER;
        gbc.insets = spacer;
        blurringDiameterText = createEntryField( "15" );
        blurringDiameterText.setEnabled( true );
        //blurp.add(blurringDiameterText, gbc);

        blurp.setToolTipText( "for blurring original" );

        return blurp;
    }

    /** makes the  panel to allow user selection of colour channels to filter.
     *   nothing editable when image not in ARGB or ARGB_USHORT or ARGB_FLOAT
     *   @return the colour panel with adjustable attributes already added
     */
    protected JPanel buildColourPanel() {
        JPanel colourPanel = new JPanel();
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();

        colourPanel.setLayout( gbl );
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        colourPanel.setForeground( Color.black );
        colourPanel.setBorder( buildTitledBorder( "Color channel selection" ) ); //set the border ... "Colour channel Selection"

        redChannel = new JCheckBox(
                "Red Channel", true );
        redChannel.setFont( serif12 );
        gbl.setConstraints( redChannel, gbc );
        colourPanel.add( redChannel );

        greenChannel = new JCheckBox( "Green Channel", true );
        greenChannel.setFont( serif12 );
        gbl.setConstraints( greenChannel, gbc );
        colourPanel.add( greenChannel );

        blueChannel = new JCheckBox( "Blue Channel", true );
        blueChannel.setFont( serif12 );
        gbl.setConstraints( blueChannel, gbc );
        colourPanel.add( blueChannel );

        // if not a colour image, block access to the channel switches
        // since they don't mean anything
        if ( !isColorImage ) {
            redChannel.setEnabled( false );
            greenChannel.setEnabled( false );
            blueChannel.setEnabled( false );
        }

        colourPanel.setToolTipText( "Colour images can be filtered over any combination of colour channels" );
        return colourPanel;
    }

    /** creates the planel which contains the OKAY and Cancel buttons.
     *   sets their sizes, colours and listeners.
     */
    private JPanel buildOkayCancelPanel() {
        return buildButtons();
    }

    //************************************************************************
    //************************** Action Events *******************************
    //************************************************************************

    /** a button has been clicked!  Cancel will dispose of the dialog,
     *  OK sets the variables and calls the algorithm; any errors in setting
     *  the variables upon OK are written to the debug pane.
     *  Help brings up the help text.
     */
    public void actionPerformed( ActionEvent ae ) {
        String command = ae.getActionCommand();
        if ( command.equalsIgnoreCase( "cancel" ) ) {
            dispose();
        } else if ( command.equalsIgnoreCase( "ok" ) ) {
            if ( setVariables() ) {
                setVisible( false );
                callAlgorithm();

            } else {
                Preferences.debug( "JDialogLocalNormalization: " + "error setting variables." );
            }
        } else if ( command.equals( "Help" ) ) {
            MipavUtil.showHelp(
                    "filtersspatialgaussianblur_filters_(spatial)_gaussian_blur_htm_toc_applying_the_gaussian" );
        }
    }

    //************************************************************************
    //************************** Access Methods*******************************
    //************************************************************************

    /**
     *  Accessor that returns the image.
     *  @return          The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     *  Accessor that sets the unsharp array.
     *  @param unsharp		Value to set the unsharp array to
     *                     (should be between 0.5 and 5.0).
     */
    public void setUnsharp( float[] unsharp ) {
        for ( int i = 0; i < unsharp.length || i < this.unsharp.length; i++ ) {
            // copy the array in case the size is wrong!
            this.unsharp[i] = unsharp[i];
        }

        // make sure that values are valid
        if ( unsharp[0] < UNSHARP_MIN ) {
            unsharp[0] = UNSHARP_MIN;
        }
        if ( unsharp[0] > UNSHARP_MAX ) {
            unsharp[0] = UNSHARP_MAX;
        }
        if ( unsharp[1] < UNSHARP_MIN ) {
            unsharp[1] = UNSHARP_MIN;
        }
        if ( unsharp[1] > UNSHARP_MAX ) {
            unsharp[1] = UNSHARP_MAX;
        }

    }

    /**
     *  Accessor that sets the unsharp array values.
     *  @param unsharpX		Values to set the unsharp array values to
     *                          (should be between 0.5 and 5.0).
     *  @param unsharpY		Values to set the unsharp array values to
     *                          (should be between 0.5 and 5.0).
     */
    public void setUnsharp( float unsharpX, float unsharpY ) {

        // make sure that values are valid
        if ( unsharpX < UNSHARP_MIN ) {
            unsharpX = UNSHARP_MIN;
        }
        if ( unsharpX > UNSHARP_MAX ) {
            unsharpX = UNSHARP_MAX;
        }
        if ( unsharpY < UNSHARP_MIN ) {
            unsharpX = UNSHARP_MIN;
        }
        if ( unsharpY > UNSHARP_MAX ) {
            unsharpX = UNSHARP_MAX;
        }

        this.unsharp[0] = unsharpX;
        this.unsharp[1] = unsharpY;
    }

    /**
     *  Accessor that sets the unsharp weight value.
     *  @param weight		Value to set the unsharp weight to
     *                          (should be between 0.0 and 1.0).
     */
    public void setUnsharpWeight( float weight ) {

        // make sure that value is valid
        if ( weight < UNSHARP_WEIGHT_MIN ) {
            weight = UNSHARP_WEIGHT_MIN;
        }
        if ( weight > UNSHARP_WEIGHT_MAX ) {
            weight = UNSHARP_WEIGHT_MAX;
        }

        unsharpWeight = weight;
    }

    /**
     *  Accessor that sets the blurring frequency value.
     *  @param freq 		Value to set the blurring frequency to
     *                          (should be positive).
     */
    public void setBlurringFreq( float freq ) {
        // make sure that value is valid
        if ( freq <= FREQ_MIN ) {
            freq = FREQ_DEFAULT;
        }

        blurringFreq = freq;
    }

    /**
     *  Accessor that sets the blurring diameter value.
     *  @param dia		Value to set the blurring diameter to
     *                          (should be positive and odd).
     */
    public void setBlurringDiameter( int dia ) {
        // make sure that value is valid
        if ( dia < BLUR_MIN ) {
            dia = BLUR_MIN;
        }

        // dia is even, then add 1 to make it odd (kludgy!)
        if ( dia % 2 == 0 ) {
            dia += 1;
        }

        blurringDiameter = dia;
    }

    /**
     *	Accessor that sets the red component flag.
     *	@param red		Value to set the red to
     */
    public void setRed( boolean red ) {

        if ( isColorImage ) {
            this.red = red;
        } else {
            this.red = false;
        }
    }

    /**
     *	Accessor that sets the green component flag.
     *	@param green		Value to set the green to
     */
    public void setGreen( boolean green ) {

        if ( isColorImage ) {
            this.green = green;
        } else {
            this.green = false;
        }
    }

    /**
     *	Accessor that sets the blue component flag.
     *	@param blue		Value to set the blue to
     */
    public void setBlue( boolean blue ) {

        if ( isColorImage ) {
            this.blue = blue;
        } else {
            this.blue = false;
        }
    }

    /**
     *	Accessor that sets the r, g, b components flag.
     *	@param red		Value to set the red to
     *	@param green	Value to set the green to
     *	@param blue		Value to set the blue to
     */
    public void setRGB( boolean red, boolean green, boolean blue ) {

        if ( isColorImage ) {
            this.red = red;
            this.green = green;
            this.blue = blue;
        } else {
            this.red = false;
            this.green = false;
            this.blue = false;
        }
    }

    /**
     * Accessor that sets the display loc variable to replace, so the current
     * image is replaced once the algorithm completes.
     */
    public void setDisplayLocReplace() {
        // eventually this will set this... for now, only NEW is supported
        //displayLoc = REPLACE;
        displayLoc = NEW;
    }

    /**
     *  Accessor that sets the display loc variable to new, so that a new image
     *  is created once the algorithm completes.
     */
    public void setDisplayLocNew() {
        displayLoc = NEW;
    }

    /**
     *  Use the GUI results to set up the variables needed to run the algorithm.
     *  @return	<code>true</code> if parameters set successfully,
     *         <code>false</code> otherwise.
     */
    private boolean setVariables() {
        //String tmpStr;

        //if (replaceImage.isSelected())    displayLoc = REPLACE;
        //else if (newImage.isSelected())   displayLoc = NEW;

        //if (wholeImage.isSelected())      regionFlag = true;
        //else if (VOIRegions.isSelected()) regionFlag = false;

        // check the variables in the unsharp-masking panel as they are
        // translated
        if ( !checkUnsharping() ) {
            return false;
        }

        // check the variables in the blurring panel as they are translated
        if ( !checkBlurring() ) {
            return false;
        }

        red = redChannel.isSelected();
        green = greenChannel.isSelected();
        blue = blueChannel.isSelected();
        //if (bySlice != null) image25D = bySlice.isSelected();
        return true;
    }

    /**
     *  Once all the necessary variables are set, call the local normalization
     *  algorithm based on what type of image this is and whether or not there
     *  is a separate destination image.
     */
    private void callAlgorithm() {
        String name = makeImageName( sourceImage.getImageName(), "_LocalNormalization" );
        // stuff to do when working on 2-D images.
        if ( sourceImage.getNDims() == 2 ) { // source image is 2D
            int[] destExtents = new int[2];
            destExtents[0] = sourceImage.getExtents()[0]; // X dim
            destExtents[1] = sourceImage.getExtents()[1]; // Y dim

            //if (displayLoc == NEW) {        // (2D)
            try {
                // Make result image of float type
                //resultImage     = new ModelImage(image.getType(), destExtents, name, userInterface);
                resultImage = (ModelImage) sourceImage.clone();
                resultImage.setImageName( name );
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
                algoLocal = new AlgorithmLocalNormalization( resultImage, sourceImage, unsharp, unsharpWeight,
                        blurringDiameter, blurringFreq );
                algoLocal.setUserInterface( userInterface );
                // only if the src image is colour will any channel
                // checkboxes be enabled:
                algoLocal.setRGBChannelFilter( red, green, blue );
                // This is very important. Adding this object as a listener
                // allows the algorithm to notify this object when it
                // has completed or failed. See algorithm performed event.
                // This is made possible by implementing
                // AlgorithmedPerformed interface
                algoLocal.addListener( this );
                setVisible( false ); // Hide dialog

                if ( runInSeparateThread ) {
                    // Start the thread as a low priority because we wish
                    // to still have user interface work fast.
                    if ( algoLocal.startMethod( Thread.MIN_PRIORITY ) == false ) {
                        MipavUtil.displayError( "A thread is already running on this object" );
                    }
                } else {
                    algoLocal.setActiveImage( isActiveImage );
                    if ( !userInterface.isAppFrameVisible() ) {
                        algoLocal.setProgressBarVisible( false );
                    }
                    algoLocal.run();
                }
            } catch ( OutOfMemoryError x ) {
                MipavUtil.displayError( "Dialog LocalNormalization: unable to allocate enough memory" );
                if ( resultImage != null ) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }
                return;
            }
            //}
            /*else {  // displayLoc == REPLACE        (2D)
             try{
             // No need to make new image space because the user has choosen to replace the source image
             // Make the algorithm class
             medianAlgo = new AlgorithmMedian(image, iters, kernelSize, kernelShape, stdDev, regionFlag);
             // only if the src image is colour will any channel checkboxes be enabled
             medianAlgo.setRGBChannelFilter(red, green, blue);
             // This is very important. Adding this object as a listener allows the algorithm to
             // notify this object when it has completed or failed. See algorithm performed event.
             // This is made possible by implementing AlgorithmedPerformed interface
             medianAlgo.addListener(this);
             // Hide the dialog since the algorithm is about to run.
             setVisible(false);
             // These next lines set the titles in all frames where the source image is displayed to
             // "locked - " image name so as to indicate that the image is now read/write locked!
             // The image frames are disabled and then unregisted from the userinterface until the
             // algorithm has completed.
             Vector imageFrames = image.getImageFrameVector();
             titles = new String[imageFrames.size()];
             for (int i = 0; i < imageFrames.size(); i++) {
             titles[i] = ((ViewJFrameBase)(imageFrames.elementAt(i))).getTitle();
             ((ViewJFrameBase)(imageFrames.elementAt(i))).setTitle("Locked: " + titles[i] );
             ((ViewJFrameBase)(imageFrames.elementAt(i))).setEnabled(false);
             userInterface.unregisterFrame((Frame)(imageFrames.elementAt(i)));
             }
             if (runInSeparateThread) {
             // Start the thread as a low priority because we wish to still have user interface work fast.
             if (medianAlgo.startMethod(Thread.MIN_PRIORITY) == false){
             MipavUtil.displayError("A thread is already running on this object");
             }
             }
             else {
             medianAlgo.run();
             }
             }
             catch (OutOfMemoryError x){
             MipavUtil.displayError("Dialog median: unable to allocate enough memory");
             return;
             }
             }*/
        } else if ( sourceImage.getNDims() == 3 ) {
            int[] destExtents = new int[3];
            destExtents[0] = sourceImage.getExtents()[0];
            destExtents[1] = sourceImage.getExtents()[1];
            destExtents[2] = sourceImage.getExtents()[2];

            //if (displayLoc == NEW) {        //     (3D)
            try {
                // Make result image of float type
                //resultImage     = new ModelImage(image.getType(), destExtents, name, userInterface);
                resultImage = (ModelImage) sourceImage.clone();
                resultImage.setImageName( name );
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
                algoLocal = new AlgorithmLocalNormalization( resultImage, sourceImage, unsharp, unsharpWeight,
                        blurringDiameter, blurringFreq );
                // only if the src image is colour will any channel checkboxes be enabled
                algoLocal.setRGBChannelFilter( red, green, blue );

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                algoLocal.addListener( this );
                setVisible( false ); // Hide dialog

                if ( runInSeparateThread ) {
                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if ( algoLocal.startMethod( Thread.MIN_PRIORITY ) == false ) {
                        MipavUtil.displayError( "A thread is already running on this object" );
                    }
                } else {
                    algoLocal.setActiveImage( isActiveImage );
                    if ( !userInterface.isAppFrameVisible() ) {
                        algoLocal.setProgressBarVisible( false );
                    }
                    algoLocal.run();
                }
            } catch ( OutOfMemoryError x ) {
                MipavUtil.displayError( "Dialog LocalNormalization: unable to allocate enough memory" );
                if ( resultImage != null ) {
                    resultImage.disposeLocal(); // Clean up image memory
                    resultImage = null;
                }
                return;
            }
            //}
            /*else {  // displayLoc == REPLACE         (3D)
             try{
             // Make algorithm
             medianAlgo = new AlgorithmMedian(image, iters, kernelSize, kernelShape, stdDev, image25D, regionFlag);
             // only if the src image is colour will any channel checkboxes be enabled
             medianAlgo.setRGBChannelFilter(red, green, blue);
             // This is very important. Adding this object as a listener allows the algorithm to
             // notify this object when it has completed or failed. See algorithm performed event.
             // This is made possible by implementing AlgorithmedPerformed interface
             medianAlgo.addListener(this);
             // Hide dialog
             setVisible(false);
             // These next lines set the titles in all frames where the source image is displayed to
             // "locked - " image name so as to indicate that the image is now read/write locked!
             // The image frames are disabled and then unregisted from the userinterface until the
             // algorithm has completed.
             Vector imageFrames = image.getImageFrameVector();
             titles = new String[imageFrames.size()];
             for (int i = 0; i < imageFrames.size(); i++) {
             titles[i] = ((ViewJFrameBase)(imageFrames.elementAt(i))).getTitle();
             ((ViewJFrameBase)(imageFrames.elementAt(i))).setTitle("Locked: " + titles[i] );
             ((ViewJFrameBase)(imageFrames.elementAt(i))).setEnabled(false);
             userInterface.unregisterFrame((Frame)(imageFrames.elementAt(i)));
             }
             if (runInSeparateThread) {
             // Start the thread as a low priority because we wish to still have user interface work fast.
             if (medianAlgo.startMethod(Thread.MIN_PRIORITY) == false){
             MipavUtil.displayError("A thread is already running on this object");
             }
             }
             else {
             medianAlgo.run();
             }
             }
             catch (OutOfMemoryError x){
             MipavUtil.displayError("Dialog median: unable to allocate enough memory");
             return;
             }
             }*/
        }
    } // end callAlgorithm()

    //************************************************************************
    //************************** Algorithm Events ****************************
    //************************************************************************

    /**
     *	This method is required if the AlgorithmPerformed interface
     *   is implemented.  It is called by the algorithms when it
     *   has completed or failed to to complete, so that the
     *   dialog can be display the result image and/or clean up.
     *
     *   @param algorithm   Algorithm that caused the event.
     */
    public void algorithmPerformed( AlgorithmBase algorithm ) {
        ViewJFrameImage imageFrame = null;

        if ( Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && this.getOwner() != null && !isScriptRunning() ) {
            saveDefaults();
        }

        if ( algorithm instanceof AlgorithmLocalNormalization ) {
            sourceImage.clearMask();
            if ( algoLocal.isCompleted() == true && resultImage != null ) {

                //The algorithm has completed and produced a new image to be displayed.
                updateFileInfo( sourceImage, resultImage );
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
                Vector imageFrames = sourceImage.getImageFrameVector();
                titles = new String[imageFrames.size()];
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

                sourceImage.notifyImageDisplayListeners( null, true );
            } else if ( resultImage != null ) {
                //algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();
            }

            insertScriptLine( algorithm );
        } // else not a Local Normalization algorithm
        else {
              Preferences.debug(
                      "JDialogLocalNormalization caught algorithm performed for: " + algorithm.getClass().getName(), Preferences.DEBUG_ALGORITHM );
        }
        algoLocal.finalize();
        algoLocal = null;
        dispose();
    }

    /** sets the value of the variable isColorImage
     *   @param mi   the model image to be checked to determine if it is color.
     */
    private void checkColour( ModelImage mi ) {
        isColorImage = mi.isColorImage();
    }



    /** Builds a new JTextField, with the given String,
     *   sets its font (to MipavUtil.font12),
     *   sets the foreground colour (to Color.black),
     *   sets column width to 7,
     *   then returns the newly made JTextField.
     *   @param labelString the String to have the JTextField display.
     *   @return a black-text, font12'd, JTextField displaying presetText.
     */
    private JTextField createEntryField( String presetText ) {
        JTextField jtf = new JTextField( presetText );
        jtf.setFont( MipavUtil.font12 );
        jtf.setForeground( Color.black );
        jtf.setColumns( 7 );
        jtf.setHorizontalAlignment( JTextField.RIGHT );

        return jtf;
    }

    /** verify that the numeric value of the text of the submitted
     *   JTextField is between a and b.
     *   <p>
     *   @throws a NullPointerException if jtf is empty.
     *   @throws an IllegalArgumentException when the String is either
     *           not translatable to a float (ie., when
     *           <code>Float.parseFloat(jtf.getText())</code> throws a
     *           ClassCastException), or when the number is
     *           out-of-bounds.  Note: a JTextField properly
     *           implementing <code>makeNumericsOnly(JTextField)</code>
     *           should <i>always</i> translate into a float in the
     *           example above.
     *   @see makeNumericsOnly(JTextField)
     *   @see NullPointerException
     *   @see IllegalArgumentException
     */
    private float checkText( JTextField jtf, float a, float b ) {
        String number;
        float val;

        try {
            number = jtf.getText();
        } catch ( NullPointerException npe ) {
            errorComponent = jtf;
            throw new NullPointerException( "No value!" );
        }

        try {
            val = Float.parseFloat( number );
        } catch ( ClassCastException cce ) {
            errorComponent = jtf;
            throw new IllegalArgumentException( "Value is not a number!" );
        }

        if ( ( val < a ) || ( val > b ) ) {
            errorComponent = jtf;
            throw new IllegalArgumentException( "Value is out of bounds!" );
        }
        // else
        return val;
    }

    /** check the variables of the unsharping-mask panel as they are
     *   translated from dialog inputs (ie., <code>JTextField</code>s)
     *   to more usable, native types.
     *   <P>
     *   The panel displays the appropriate error when there is an input
     *   violation.
     *   @returns <code>true</code> when the variables were copied correctly;
     *            <code>false</code> when there is an error in an input;
     */
    private boolean checkUnsharping() {
        try {
            unsharp[0] = checkText( unsharpXtext, UNSHARP_MIN, UNSHARP_MAX );
            unsharp[1] = checkText( unsharpYtext, UNSHARP_MIN, UNSHARP_MAX );
            //unsharp[2] = checkText(unsharpZtext, UNSHARP_MIN, UNSHARP_MAX); // unused. FIXME
        } catch ( NullPointerException npe ) {
            MipavUtil.displayError( npe.getMessage() ); // something more instructive?
            errorComponent.requestFocus();
            errorComponent.selectAll(); // doesn't really do anything since the object is empty
            return false;
        } catch ( IllegalArgumentException iae ) {
            MipavUtil.displayError( iae.getMessage() + "  Use values between " + UNSHARP_MIN + " and " + UNSHARP_MAX );
            errorComponent.requestFocus();
            errorComponent.selectAll();
            return false;
        }

        try {
            unsharpWeight = checkText( unsharpWeightText, UNSHARP_WEIGHT_MIN, UNSHARP_WEIGHT_MAX );
        } catch ( NullPointerException npe ) {
            MipavUtil.displayError( npe.getMessage() ); // something more instructive?
            errorComponent.requestFocus();
            errorComponent.selectAll(); // doesn't really do anything since the object is empty
            return false;
        } catch ( IllegalArgumentException iae ) {
            MipavUtil.displayError(
                    iae.getMessage() + "  Use values between " + UNSHARP_WEIGHT_MIN + " and " + UNSHARP_WEIGHT_MAX );
            errorComponent.requestFocus();
            errorComponent.selectAll();
            return false;
        }

        return true;
    }

    /** check the variables of the unsharping-mask panel as they are
     *  translated from dialog inputs (ie., <code>JTextField</code>s)
     *  to more usable, native types.
     *  <P>
     *  The panel displays the appropriate error when there is an input
     *  violation.
     *  @returns <code>true</code> when the variables were copied correctly;
     *           <code>false</code> when there is an error in an input;
     */
    private boolean checkBlurring() {
        try {
            blurringFreq = Float.parseFloat( blurringFreqText.getText() );
        } catch ( NullPointerException npe ) {
            MipavUtil.displayError( "No value!  Value must be greater than 0.0" ); // something more instructive?
            errorComponent = blurringFreqText;
            errorComponent.requestFocus();
            errorComponent.selectAll(); // doesn't really do anything since the object is empty
            return false;
        }

        if ( blurringFreq <= 0.0 ) {
            MipavUtil.displayError( "Value must be greater than 0.0" ); // something more instructive?
            errorComponent = blurringFreqText;
            errorComponent.requestFocus();
            errorComponent.selectAll(); // doesn't really do anything since the object is empty
            return false;
        }

        try {
            blurringDiameter = Integer.parseInt( blurringDiameterText.getText() );
        } catch ( NullPointerException npe ) {
            MipavUtil.displayError( "No value!  Value must be both positive and odd." ); // something more instructive?
            errorComponent = blurringDiameterText;
            errorComponent.requestFocus();
            errorComponent.selectAll(); // doesn't really do anything since the object is empty
            return false;
        }

        if ( ( blurringDiameter % 2 == 0 ) || ( blurringDiameter <= 0 ) ) {
            MipavUtil.displayError( "Value must be both positive and odd." ); // something more instructive?
            errorComponent = blurringDiameterText;
            errorComponent.requestFocus();
            errorComponent.selectAll(); // doesn't really do anything since the object is empty
            return false;
        }

        // looks okay, so
        return true;
    }

    /** has the submitted text field listen to itself and
     *   selectively filter out unwanted characters.
     *   <p>
     *   Permits only the characters: <ul>
     *   <li>numbers 0-9</li>
     *   <li>delete/bask-space</li>
     *   <li>enter</li>
     *   <li>and ONE period</li>
     *   </ul>
     *   All other characters are ignored.
     */

    /**
     private void makeNumericsOnly(JTextField txt) {
     txt.addKeyListener(new KeyAdapter() { // make the field
     public void keyTyped(KeyEvent evt) { // not accept letters
     JTextField t = (JTextField) evt.getComponent();
     char ch = evt.getKeyChar();
     if (ch == '.') {
     if (t.getSelectedText() != null) {
     if (t.getText().length() == t.getSelectedText().length()) {
     t.setText("0");
     }
     else if ( (t.getText().indexOf('.') != -1) // there is a '.', but no
     && (t.getSelectedText().indexOf('.') == -1)) { // in the selected text
     evt.consume(); // ignore
     }
     }
     else if (t.getText().indexOf('.') != -1) {
     evt.consume();
     }
     else if (t.getText().length() == 0) {
     t.setText("0");
     }
     }

     // negative values are not allowed
     else if ( ( (ch < '0') || (ch > '9'))
     &&
     ( (ch != KeyEvent.VK_DELETE) && (ch != KeyEvent.VK_BACK_SPACE))) {
     // if is the case that ch is outside the bounds of a number
     // AND it is the case that ch is neither a BS or a DE, then
     // key is not a digit or a deletion char
     evt.consume();
     }
     }
     });
     }
     */
}
