package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;


/**
 * Dialog to get user input, then call the midsagittal alignment algorithm.
 *
 * @version  0.1 Dec 30, 2004
 * @author   Evan McCreedy
 */
public class JDialogMidsagittal extends JDialogScriptableBase implements AlgorithmInterface, DialogDefaultsInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2594860465921132823L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmMidsagittal alignAlgo;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogMidsagittal() { }

    /**
     * Set up the dialog (nothing right now), and run the algorithm (since there really is no dialog).
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogMidsagittal(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
        loadDefaults();

        callAlgorithm();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            // MipavUtil.showHelp( "10009" );
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && (this.getOwner() != null) && !isScriptRunning()) {
            saveDefaults();
        }

        if (algorithm instanceof AlgorithmMidsagittal) {
            Preferences.debug("Midsagittal elapsed: " + algorithm.getElapsedTime());

            if (alignAlgo.isCompleted() == true) {
                resultImage = alignAlgo.getResultImage();

                // The algorithm has completed and produced a new image to be displayed.
                if (resultImage.isColorImage()) {
                    updateFileInfo(image, resultImage);
                }

                resultImage.clearMask();

                try {
                    new ViewJFrameImage(resultImage, null, ViewUserInterface.getReference().getNewFrameLocation());
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        }

        if (alignAlgo != null) {
            alignAlgo.finalize();
            alignAlgo = null;
        }

        dispose();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if (defaultsString != null) {

            try {
                // no params
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
    public void saveDefaults() {
        Preferences.saveDialogDefaults(getDialogName(), new String());
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        try {

            // Make algorithm
            alignAlgo = new AlgorithmMidsagittal(image);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            alignAlgo.addListener(this);

            createProgressBar(image.getImageName(), alignAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (alignAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                alignAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog midsagittal: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        AlgorithmParameters.storeImageInRecorder(getResultImage());
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setVisible(false);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        return true;
    }
}
