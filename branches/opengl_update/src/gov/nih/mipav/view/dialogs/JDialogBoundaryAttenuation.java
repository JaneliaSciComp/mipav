package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog for an algorithm which reduces the intensity of an image near the boundary of the VOIs within an image volume.
 *
 * @author  Evan McCreedy
 */
public class JDialogBoundaryAttenuation extends JDialogScriptableBase
        implements AlgorithmInterface, LegacyDialogDefaultsInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7736447447170989384L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The attenuation algorithm. */
    private AlgorithmBoundaryAttenuation attenuationAlgo;

    /** The attenuated image. */
    private ModelImage destImage;

    /** The maximum amount of attenuation to apply to the image (0 = no attenuation, 1 = full attenuation). */
    private float maxAttenuation = 0.5f;

    /** Maximum attenuation text field. */
    private JTextField maxAttenuationTF;

    /** The number of levels of attenuation to perform (done through morphological erosion). */
    private int numErosions = 5;

    /** Number of erosions text field. */
    private JTextField numErosionsTF;

    /** The image to attenuate. */
    private ModelImage srcImage;

    /** The MIPAV user interface. */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogBoundaryAttenuation() { }

    /**
     * Set up the algorithm dialog.
     *
     * @param  theParentFrame  the (image) frame that the dialog is attached to
     * @param  im              the image to apply the algorithm to
     */
    public JDialogBoundaryAttenuation(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        if (im.getNDims() != 3) {
            MipavUtil.displayError("The Boundary Attenuation algorithm can only be applied to 3D images.");
            dispose();

            return;
        }

        if (im.getVOIs().size() == 0) {
            MipavUtil.displayError("The Boundary Attenuation algorithm requires at least one VOI within the image.");
            dispose();

            return;
        }

        srcImage = im;

        userInterface = ViewUserInterface.getReference();

        init();

        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Handle action events from the GUI.
     *
     * @param  event  GUI action event
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * Respond to the completion or failure of the algorithm we called.
     *
     * @param  algo  the algorithm which has completed execution
     */
    public void algorithmPerformed(AlgorithmBase algo) {

        if (algo.isCompleted()) {

            // show dest image
            destImage = attenuationAlgo.getResultImage();
            new ViewJFrameImage(destImage, null, userInterface.getNewFrameLocation(destImage.getExtents()[0], destImage.getExtents()[1]));

            insertScriptLine();
        }

        srcImage.clearMask();
        attenuationAlgo.finalize();
    }

    /**
     * Construct a delimited string that contains the parameters to this algorithm.
     *
     * @param   delim  the parameter delimiter (defaults to " " if empty)
     *
     * @return  the parameter string
     */
    public String getParameterString(String delim) {

        if (delim.equals("")) {
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
    public void legacyLoadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if (defaultsString != null) {

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");

                numErosionsTF.setText("" + MipavUtil.getInt(st));
                maxAttenuationTF.setText("" + MipavUtil.getFloat(st));
            } catch (Exception ex) {

                // since there was a problem parsing the defaults string, start over with the original defaults
                Preferences.debug("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
            }
        }
    }

    /**
     * Saves the default settings into the Preferences file.
     */
    public void legacySaveDefaults() {
        String defaultsString = new String(getParameterString(","));
        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }

    /**
     * Construct and run the algorithm.
     */
    protected void callAlgorithm() {
        setVisible(false);


        attenuationAlgo = new AlgorithmBoundaryAttenuation(srcImage, numErosions, maxAttenuation);
        attenuationAlgo.addListener(this);
        createProgressBar(srcImage.getImageName(), attenuationAlgo);

        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still have user interface work fast.
            if (attenuationAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            attenuationAlgo.run();
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(destImage);
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        srcImage = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = srcImage.getParentFrame();

        if (srcImage.getNDims() != 3) {
            MipavUtil.displayError("The Boundary Attenuation algorithm can only be applied to 3D images.");
            dispose();

            return;
        }

        if (srcImage.getVOIs().size() == 0) {
            MipavUtil.displayError("The Boundary Attenuation algorithm requires at least one VOI within the image.");
            dispose();

            return;
        }

        maxAttenuation = scriptParameters.getParams().getFloat("max_attenuation");
        numErosions = scriptParameters.getParams().getInt("num_erosions");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(srcImage);
        scriptParameters.storeImageInRecorder(destImage);

        scriptParameters.getParams().put(ParameterFactory.newParameter("max_attenuation", maxAttenuation));
        scriptParameters.getParams().put(ParameterFactory.newParameter("num_erosions", numErosions));
    }

    /**
     * Set up the algorithm GUI.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Boundary Attenuation");

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(MipavUtil.buildTitledBorder("Parameters"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(3, 3, 3, 3);

        gbc.gridx = 0;
        gbc.gridy = 0;

        JLabel numErosionsLabel = new JLabel("Number of border erosion iterations");
        numErosionsLabel.setFont(MipavUtil.font12);
        paramPanel.add(numErosionsLabel, gbc);

        numErosionsTF = new JTextField("" + numErosions);
        numErosionsTF.setFont(MipavUtil.font12);
        numErosionsTF.setColumns(4);
        gbc.gridx++;
        paramPanel.add(numErosionsTF, gbc);

        JLabel maxAttenuationLabel = new JLabel("Maximum level of attenuation");
        maxAttenuationLabel.setFont(MipavUtil.font12);
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(maxAttenuationLabel, gbc);

        maxAttenuationTF = new JTextField("" + maxAttenuation);
        maxAttenuationTF.setFont(MipavUtil.font12);
        maxAttenuationTF.setColumns(4);
        gbc.gridx++;
        paramPanel.add(maxAttenuationTF, gbc);

        getContentPane().setLayout(new BorderLayout());
        getContentPane().add(paramPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr = numErosionsTF.getText();

        if (testParameter(tmpStr, 1, 20)) {
            numErosions = Integer.valueOf(tmpStr).intValue();
        } else {
            numErosionsTF.requestFocus();
            numErosionsTF.selectAll();

            return false;
        }

        tmpStr = maxAttenuationTF.getText();

        if (testParameter(tmpStr, 0.0, 1.0)) {
            maxAttenuation = Float.valueOf(tmpStr).floatValue();
        } else {
            maxAttenuationTF.requestFocus();
            maxAttenuationTF.selectAll();

            return false;
        }

        return true;
    }
}
