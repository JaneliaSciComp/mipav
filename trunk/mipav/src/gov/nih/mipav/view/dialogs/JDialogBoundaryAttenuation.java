package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmScriptParser;
import gov.nih.mipav.model.algorithms.filters.AlgorithmBoundaryAttenuation;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.DialogDefaultsInterface;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ScriptableInterface;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.util.StringTokenizer;
import javax.swing.*;


/**
 * Dialog for an algorithm which reduces the intensity of an image near the boundary
 * of the VOIs within an image volume.
 *
 * @author Evan McCreedy
 */
public class JDialogBoundaryAttenuation extends JDialogBase
    implements AlgorithmInterface, ScriptableInterface, DialogDefaultsInterface {

    /**
     * The attenuation algorithm.
     */
    private AlgorithmBoundaryAttenuation attenuationAlgo;

    /**
     * The MIPAV user interface.
     */
    private ViewUserInterface userInterface;

    /**
     * The image to attenuate.
     */
    private ModelImage srcImage;

    /**
     * The attenuated image.
     */
    private ModelImage destImage;

    /**
     * The number of levels of attenuation to perform (done through morphological erosion).
     */
    private int numErosions = 5;

    /**
     * The maximum amount of attenuation to apply to the image (0 = no attenuation, 1 = full attenuation).
     */
    private float maxAttenuation = 0.5f;

    /**
     * Number of erosions text field.
     */
    private JTextField numErosionsTF;

    /**
     * Maximum attenuation text field.
     */
    private JTextField maxAttenuationTF;

    /**
     * Set up the algorithm dialog.
     * @param theParentFrame  the (image) frame that the dialog is attached to
     * @param im              the image to apply the algorithm to
     */
    public JDialogBoundaryAttenuation( Frame theParentFrame, ModelImage im ) {
        super( theParentFrame, false );

        if ( im.getNDims() != 3 ) {
            MipavUtil.displayError( "The Boundary Attenuation algorithm can only be applied to 3D images." );
            dispose();
            return;
        }
        if ( im.getVOIs().size() == 0 ) {
            MipavUtil.displayError( "The Boundary Attenuation algorithm requires at least one VOI within the image." );
            dispose();
            return;
        }

        srcImage = im;

        userInterface = srcImage.getUserInterface();

        init();

        loadDefaults();
        setVisible( true );
    }

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogBoundaryAttenuation() {};

    /**
     * Run this algorithm from a script.
     * @param parser  the script parser we get the state from
     * @throws java.lang.IllegalArgumentException  if there is something wrong with the arguments in the script
     */
    public void scriptRun (AlgorithmScriptParser parser) throws IllegalArgumentException {
        setScriptRunning( true );

        String srcImageKey = null;
        String destImageKey = null;

        try {
            srcImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }
        ModelImage im = parser.getImage(srcImageKey);

        srcImage = im;
        userInterface = srcImage.getUserInterface();
        parentFrame = srcImage.getParentFrame();

        // the result image
        try {
            destImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        try {
            numErosions = parser.getNextInteger();
            maxAttenuation = parser.getNextFloat();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        setActiveImage(parser.isActiveImage());
        setSeparateThread(false);

        callAlgorithm();

        parser.putVariable(destImageKey, destImage.getImageName());
    }

    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     * @param algo the algorithm to make an entry for
     */
    public void insertScriptLine (AlgorithmBase algo) {
        if (algo.isCompleted()) {
            if (userInterface.isScriptRecording()) {
                //check to see if the match image is already in the ImgTable
                if (userInterface.getScriptDialog().getImgTableVar(srcImage.getImageName()) == null) {
                    if (userInterface.getScriptDialog().getActiveImgTableVar(srcImage.getImageName()) == null) {
                        userInterface.getScriptDialog().putActiveVar(srcImage.getImageName());
                    }
                }

                String line = "BoundaryAttenuation " + userInterface.getScriptDialog().getVar(srcImage.getImageName()) + " ";
                userInterface.getScriptDialog().putVar(destImage.getImageName());
                line += userInterface.getScriptDialog().getVar(destImage.getImageName()) + " " + getParameterString(" ") + "\n";

                userInterface.getScriptDialog().append(line);
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
        str += numErosions + delim;
        str += maxAttenuation;

        return str;
    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if (defaultsString != null) {
            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");

                numErosionsTF.setText( "" + MipavUtil.getInt( st ) );
                maxAttenuationTF.setText( "" + MipavUtil.getFloat( st ) );
            }
            catch (Exception ex) {
                // since there was a problem parsing the defaults string, start over with the original defaults
                Preferences.debug( "Resetting defaults for dialog: " + getDialogName() );
                Preferences.removeProperty( getDialogName() );
            }
        }
    }

    /**
     * Saves the default settings into the Preferences file.
     */
    public void saveDefaults() {
        String defaultsString = new String( getParameterString(",") );
        Preferences.saveDialogDefaults(getDialogName(),defaultsString);
    }

    /**
     * Construct and run the algorithm.
     */
    private void callAlgorithm() {
        setVisible( false );

        attenuationAlgo = new AlgorithmBoundaryAttenuation( srcImage, numErosions, maxAttenuation );
        attenuationAlgo.addListener( this );

        if ( runInSeparateThread ) {
            // Start the thread as a low priority because we wish to still have user interface work fast.
            if ( attenuationAlgo.startMethod( Thread.MIN_PRIORITY ) == false ) {
                MipavUtil.displayError( "A thread is already running on this object" );
            }
        } else {
            attenuationAlgo.setActiveImage( isActiveImage );
            if ( !userInterface.isAppFrameVisible() ) {
                attenuationAlgo.setProgressBarVisible( false );
            }
            attenuationAlgo.run();
        }
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr = numErosionsTF.getText();
        if ( testParameter( tmpStr, 1, 20 ) ) {
            numErosions = Integer.valueOf( tmpStr ).intValue();
        } else {
            numErosionsTF.requestFocus();
            numErosionsTF.selectAll();
            return false;
        }

        tmpStr = maxAttenuationTF.getText();
        if ( testParameter( tmpStr, 0.0, 1.0 ) ) {
            maxAttenuation = Float.valueOf( tmpStr ).floatValue();
        } else {
            maxAttenuationTF.requestFocus();
            maxAttenuationTF.selectAll();
            return false;
        }

        return true;
    }

    /**
     * Set up the algorithm GUI.
     */
    private void init() {
        setForeground( Color.black );
        setTitle( "Boundary Attenuation" );

        JPanel paramPanel = new JPanel( new GridBagLayout() );
        paramPanel.setBorder( MipavUtil.buildTitledBorder( "Parameters" ) );

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets( 3, 3, 3, 3 );

        gbc.gridx = 0;
        gbc.gridy = 0;
        JLabel numErosionsLabel = new JLabel( "Number of border erosion iterations" );
        numErosionsLabel.setFont( MipavUtil.font12 );
        paramPanel.add( numErosionsLabel, gbc );

        numErosionsTF = new JTextField( "" + numErosions );
        numErosionsTF.setFont( MipavUtil.font12 );
        numErosionsTF.setColumns( 4 );
        gbc.gridx++;
        paramPanel.add( numErosionsTF, gbc );

        JLabel maxAttenuationLabel = new JLabel( "Maximum level of attenuation" );
        maxAttenuationLabel.setFont( MipavUtil.font12 );
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add( maxAttenuationLabel, gbc );

        maxAttenuationTF = new JTextField( "" + maxAttenuation );
        maxAttenuationTF.setFont( MipavUtil.font12 );
        maxAttenuationTF.setColumns( 4 );
        gbc.gridx++;
        paramPanel.add( maxAttenuationTF, gbc );

        getContentPane().setLayout( new BorderLayout() );
        getContentPane().add( paramPanel, BorderLayout.CENTER );
        getContentPane().add( buildButtons(), BorderLayout.SOUTH );
        pack();
        setResizable( true );
    }

    /**
     * Handle action events from the GUI.
     * @param event  GUI action event
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
     * Respond to the completion or failure of the algorithm we called.
     * @param algo  the algorithm which has completed execution
     */
    public void algorithmPerformed( AlgorithmBase algo ) {
        if ( algo.isCompleted() ) {
            if (Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && this.getOwner() != null && !isScriptRunning()) {
                saveDefaults();
            }

            // show dest image
            destImage = attenuationAlgo.getResultImage();
            new ViewJFrameImage( destImage, null, userInterface.getNewFrameLocation() );

            insertScriptLine( algo );
        }

        srcImage.clearMask();
        attenuationAlgo.finalize();
    }
}
