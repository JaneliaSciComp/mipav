package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * This is the dialog to permit user to perform Local Normalization.
 *
 * <p>Local Normalization equalizes colour levels among pixels removing variations due to lighting, bringing out
 * contrasts in detail.</p>
 *
 * <p>This dialog presents the X- and Y- gaussian dimensions and the blurring weight from the JDialogUnsharpMask dialog
 * and the cut-off frequency from the JDialogFrequencyFilter dialog. There is also a colour channel selection panel
 * which permits the selection of colours to be processed when the image is in aRGB colour.</p>
 *
 * <p>Described by Halyo, Rahman and Park:</p>
 *
 * <blockquote>Local Normalization seperates the image into a local or low-frequency signal, and a suface detail or
 * high-frequency signal. The locally normalized signal is then obtained by normalizing (ie., dividing) the detail
 * signal by the local average.</blockquote>
 *
 * <p>References:</p>
 *
 * <ol>
 *   <li>Local Normalization. <a href="http://bigwww.epfl.ch/demo/normalize/desc.html">
 *     http://bigwww.epfl.ch/demo/normalize/desc.html</a></li>
 *   <li>Halyo, Nesim; Rahman, Zia-ur; Park, Stephen. "Information Content in Nonlinear Local Normalization Processing
 *     of Digital Images". College of William and Mary. Williamsburg, Virgiana.</li>
 * </ol>
 *
 * @see  JDialogUnsharpMask
 * @see  JDialogFrequencyFilter
 * @see  AlgorithmLocalNormalization
 */
public class JDialogLocalNormalization extends JDialogScriptableBase
        implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4460969182116331919L;

    /** minimum value for unsharpening variables, at 0.5. */
    public static final float UNSHARP_MIN = (float) 0.5;

    /** maximum value for unsharpening variables, at 5.0. */
    public static final float UNSHARP_MAX = (float) 5.0;

    /** minimum value for unsharpening weighting, at 0.0. */
    public static final float UNSHARP_WEIGHT_MIN = (float) 0.0;

    /** maximum value for unsharpening weighting, at 1.0. */
    public static final float UNSHARP_WEIGHT_MAX = (float) 1.0;

    /** minimum frequency value for blurring frequency, at 0.0. */
    public static final float FREQ_MIN = (float) 0.0;

    /** default frequency value for blurring frequency, at 0.2. */
    public static final float FREQ_DEFAULT = (float) 0.2;

    /** minimum value for blurring, at 1.0. */
    public static final int BLUR_MIN = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmLocalNormalization algoLocal;

    /** DOCUMENT ME! */
    private int blurringDiameter = 15; // the default

    /** DOCUMENT ME! */
    private JTextField blurringDiameterText;

    /**
     * user-selectable variables used in the FFT-blurring operation used as interim variables in starting the algorithm
     * op.
     */
    private float blurringFreq = FREQ_DEFAULT;

    /** DOCUMENT ME! */
    private JTextField blurringFreqText;

    /** DOCUMENT ME! */
    private JPanelColorChannels colorPanel;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    // or if the source image is to be replaced

    /** DOCUMENT ME! */
    private JTextField errorComponent;

    /** DOCUMENT ME! */
    private ModelImage resultImage;

    /** DOCUMENT ME! */
    private ModelImage sourceImage;

    /** DOCUMENT ME! */
    private String[] titles; // used to save image names when replacing an image

    /**
     * user-selectable variables used in the unsharping operation. used as interim variables in starting the algorithm
     * op.
     */
    private float[] unsharp = { (float) 1.0, (float) 1.0 }; // for smooth filter of mean of f(x,y)

    /** DOCUMENT ME! */
    private float unsharpWeight = (float) 0.75;

    /** DOCUMENT ME! */
    private JTextField unsharpWeightText;

    /** DOCUMENT ME! */
    private JTextField unsharpXtext, unsharpYtext, unsharpZtext;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogLocalNormalization() { }

    /**
     * Creates an modal extension of JDialogBase, using the title, "Local Normalization". Creates an options panel; this
     * contains: the inputs for the unsharp masking; the inputs for blurring; and the inputs for choosing which colour
     * channels to process. This last set of options are not selectable on images which are not colour images. It
     * creates the OKAY and CANCEL buttons on a panel, to b be placed at the bottom of the dialog.
     *
     * <p>The panel is then pack()'d and then setVisible(true).</p>
     *
     * @param  owner  DOCUMENT ME!
     * @param  mi     DOCUMENT ME!
     */
    public JDialogLocalNormalization(JFrame owner, ModelImage mi) {
        super(owner, false);
        setTitle("Local Normalization");
        userInterface = ViewUserInterface.getReference();
        sourceImage = mi;
        displayLoc = NEW; // currently replace is not supported

        getContentPane().setLayout(new BorderLayout());
        getContentPane().add(buildOptionsPanel(), BorderLayout.CENTER);
        getContentPane().add(buildOkayCancelPanel(), BorderLayout.SOUTH);

        pack();
        loadDefaults();
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Action Events *******************************
    // ************************************************************************

    /**
     * a button has been clicked! Cancel will dispose of the dialog, OK sets the variables and calls the algorithm; any
     * errors in setting the variables upon OK are written to the debug pane. Help brings up the help text.
     *
     * @param  ae  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent ae) {
        String command = ae.getActionCommand();

        if (command.equalsIgnoreCase("cancel")) {
            dispose();
        } else if (command.equalsIgnoreCase("ok")) {

            if (setVariables()) {
                setVisible(false);
                callAlgorithm();

            } else {
                Preferences.debug("JDialogLocalNormalization: " + "error setting variables.");
            }
        } else if (command.equals("Help")) {
            // MipavUtil.showHelp("");
        } else {
            super.actionPerformed(ae);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && (this.getOwner() != null) && !isScriptRunning()) {
            saveDefaults();
        }

        if (algorithm instanceof AlgorithmLocalNormalization) {
            sourceImage.clearMask();

            if ((algoLocal.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                updateFileInfo(sourceImage, resultImage);
                resultImage.clearMask();

                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registered to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = sourceImage.getImageFrameVector();
                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                sourceImage.notifyImageDisplayListeners(null, true);
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();
            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        } // else not a Local Normalization algorithm
        else {
            Preferences.debug("JDialogLocalNormalization caught algorithm performed for: " +
                              algorithm.getClass().getName(), Preferences.DEBUG_ALGORITHM);
        }

        algoLocal.finalize();
        algoLocal = null;
        dispose();
    }

    // ************************************************************************
    // ************************** Access Methods*******************************
    // ************************************************************************

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Accessor that sets the blurring diameter value.
     *
     * @param  dia  Value to set the blurring diameter to (should be positive and odd).
     */
    public void setBlurringDiameter(int dia) {

        // make sure that value is valid
        if (dia < BLUR_MIN) {
            dia = BLUR_MIN;
        }

        // dia is even, then add 1 to make it odd (kludgy!)
        if ((dia % 2) == 0) {
            dia += 1;
        }

        blurringDiameter = dia;
    }

    /**
     * Accessor that sets the blurring frequency value.
     *
     * @param  freq  Value to set the blurring frequency to (should be positive).
     */
    public void setBlurringFreq(float freq) {

        // make sure that value is valid
        if (freq <= FREQ_MIN) {
            freq = FREQ_DEFAULT;
        }

        blurringFreq = freq;
    }

    /**
     * Accessor that sets the display loc variable to new, so that a new image is created once the algorithm completes.
     */
    public void setDisplayLocNew() {
        displayLoc = NEW;
    }

    /**
     * Accessor that sets the display loc variable to replace, so the current image is replaced once the algorithm
     * completes.
     */
    public void setDisplayLocReplace() {

        // eventually this will set this... for now, only NEW is supported
        // displayLoc = REPLACE;
        displayLoc = NEW;
    }

    /**
     * Accessor that sets the unsharp array.
     *
     * @param  unsharp  Value to set the unsharp array to (should be between 0.5 and 5.0).
     */
    public void setUnsharp(float[] unsharp) {

        for (int i = 0; (i < unsharp.length) || (i < this.unsharp.length); i++) {

            // copy the array in case the size is wrong!
            this.unsharp[i] = unsharp[i];
        }

        // make sure that values are valid
        if (unsharp[0] < UNSHARP_MIN) {
            unsharp[0] = UNSHARP_MIN;
        }

        if (unsharp[0] > UNSHARP_MAX) {
            unsharp[0] = UNSHARP_MAX;
        }

        if (unsharp[1] < UNSHARP_MIN) {
            unsharp[1] = UNSHARP_MIN;
        }

        if (unsharp[1] > UNSHARP_MAX) {
            unsharp[1] = UNSHARP_MAX;
        }

    }

    /**
     * Accessor that sets the unsharp array values.
     *
     * @param  unsharpX  Values to set the unsharp array values to (should be between 0.5 and 5.0).
     * @param  unsharpY  Values to set the unsharp array values to (should be between 0.5 and 5.0).
     */
    public void setUnsharp(float unsharpX, float unsharpY) {

        // make sure that values are valid
        if (unsharpX < UNSHARP_MIN) {
            unsharpX = UNSHARP_MIN;
        }

        if (unsharpX > UNSHARP_MAX) {
            unsharpX = UNSHARP_MAX;
        }

        if (unsharpY < UNSHARP_MIN) {
            unsharpX = UNSHARP_MIN;
        }

        if (unsharpY > UNSHARP_MAX) {
            unsharpX = UNSHARP_MAX;
        }

        this.unsharp[0] = unsharpX;
        this.unsharp[1] = unsharpY;
    }

    /**
     * Accessor that sets the unsharp weight value.
     *
     * @param  weight  Value to set the unsharp weight to (should be between 0.0 and 1.0).
     */
    public void setUnsharpWeight(float weight) {

        // make sure that value is valid
        if (weight < UNSHARP_WEIGHT_MIN) {
            weight = UNSHARP_WEIGHT_MIN;
        }

        if (weight > UNSHARP_WEIGHT_MAX) {
            weight = UNSHARP_WEIGHT_MAX;
        }

        unsharpWeight = weight;
    }

    /**
     * makes the panel to allow user selection of colour channels to filter. nothing editable when image not in ARGB or
     * ARGB_USHORT or ARGB_FLOAT
     *
     * @return  the colour panel with adjustable attributes already added
     */
    protected JPanel buildColourPanel() {
        colorPanel = new JPanelColorChannels(sourceImage);
        colorPanel.setToolTipText("Colour images can be filtered over any combination of colour channels");

        return colorPanel;
    }

    /**
     * Once all the necessary variables are set, call the local normalization algorithm based on what type of image this
     * is and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(sourceImage.getImageName(), "_LocalNormalization");

        // stuff to do when working on 2-D images.
        if (sourceImage.getNDims() == 2) { // source image is 2D

            int[] destExtents = new int[2];
            destExtents[0] = sourceImage.getExtents()[0]; // X dim
            destExtents[1] = sourceImage.getExtents()[1]; // Y dim

            // if (displayLoc == NEW) {        // (2D)
            try {

                // Make result image of float type
                // resultImage     = new ModelImage(image.getType(), destExtents, name, userInterface);
                resultImage = (ModelImage) sourceImage.clone();
                resultImage.setImageName(name);

                if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                    ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                }

                // Make algorithm
                algoLocal = new AlgorithmLocalNormalization(resultImage, sourceImage, unsharp, unsharpWeight,
                                                            blurringDiameter, blurringFreq);

                // only if the src image is colour will any channel
                // checkboxes be enabled:
                algoLocal.setRGBChannelFilter(colorPanel.isRedProcessingRequested(),
                                              colorPanel.isGreenProcessingRequested(),
                                              colorPanel.isBlueProcessingRequested());

                // This is very important. Adding this object as a listener
                // allows the algorithm to notify this object when it
                // has completed or failed. See algorithm performed event.
                // This is made possible by implementing
                // AlgorithmedPerformed interface
                algoLocal.addListener(this);

                createProgressBar(sourceImage.getImageName(), algoLocal);
                setVisible(false); // Hide dialog

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish
                    // to still have user interface work fast.
                    if (algoLocal.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    algoLocal.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog LocalNormalization: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }

                return;
            }
            // }
            /*else {  // displayLoc == REPLACE        (2D)
             * try{ // No need to make new image space because the user has choosen to replace the source image // Make
             * the algorithm class medianAlgo = new AlgorithmMedian(image, iters, kernelSize, kernelShape, stdDev,
             * regionFlag); // only if the src image is colour will any channel checkboxes be enabled
             * medianAlgo.setRGBChannelFilter(red, green, blue); // This is very important. Adding this object as a
             * listener allows the algorithm to // notify this object when it has completed or failed. See algorithm
             * performed event. // This is made possible by implementing AlgorithmedPerformed interface
             * medianAlgo.addListener(this); // Hide the dialog since the algorithm is about to run. setVisible(false);
             * // These next lines set the titles in all frames where the source image is displayed to // "locked - "
             * image name so as to indicate that the image is now read/write locked! // The image frames are disabled
             * and then unregisted from the userinterface until the // algorithm has completed. Vector imageFrames =
             * image.getImageFrameVector(); titles = new String[imageFrames.size()]; for (int i = 0; i <
             * imageFrames.size(); i++) { titles[i] = ((ViewJFrameBase)(imageFrames.elementAt(i))).getTitle();
             * ((ViewJFrameBase)(imageFrames.elementAt(i))).setTitle("Locked: " + titles[i] );
             * ((ViewJFrameBase)(imageFrames.elementAt(i))).setEnabled(false);
             * userInterface.unregisterFrame((Frame)(imageFrames.elementAt(i))); } if (isRunInSeparateThread()) { //
             * Start the thread as a low priority because we wish to still have user interface work fast. if
             * (medianAlgo.startMethod(Thread.MIN_PRIORITY) == false){ MipavUtil.displayError("A thread is already
             * running on this object"); } } else { medianAlgo.run(); } } catch (OutOfMemoryError x){
             * MipavUtil.displayError("Dialog median: unable to allocate enough memory"); return; } }*/
        } else if (sourceImage.getNDims() == 3) {
            int[] destExtents = new int[3];
            destExtents[0] = sourceImage.getExtents()[0];
            destExtents[1] = sourceImage.getExtents()[1];
            destExtents[2] = sourceImage.getExtents()[2];

            // if (displayLoc == NEW) {        //     (3D)
            try {

                // Make result image of float type
                // resultImage     = new ModelImage(image.getType(), destExtents, name, userInterface);
                resultImage = (ModelImage) sourceImage.clone();
                resultImage.setImageName(name);

                if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {

                    for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                        ((FileInfoDicom) (resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                    }
                }

                // Make algorithm
                algoLocal = new AlgorithmLocalNormalization(resultImage, sourceImage, unsharp, unsharpWeight,
                                                            blurringDiameter, blurringFreq);

                // only if the src image is colour will any channel checkboxes be enabled
                algoLocal.setRGBChannelFilter(colorPanel.isRedProcessingRequested(),
                                              colorPanel.isGreenProcessingRequested(),
                                              colorPanel.isBlueProcessingRequested());

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                algoLocal.addListener(this);
                createProgressBar(sourceImage.getImageName(), algoLocal);
                setVisible(false); // Hide dialog

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (algoLocal.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    algoLocal.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog LocalNormalization: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up image memory
                    resultImage = null;
                }

                return;
            }
            // }
            /*else {  // displayLoc == REPLACE         (3D)
             * try{ // Make algorithm medianAlgo = new AlgorithmMedian(image, iters, kernelSize, kernelShape, stdDev,
             * image25D, regionFlag); // only if the src image is colour will any channel checkboxes be enabled
             * medianAlgo.setRGBChannelFilter(red, green, blue); // This is very important. Adding this object as a
             * listener allows the algorithm to // notify this object when it has completed or failed. See algorithm
             * performed event. // This is made possible by implementing AlgorithmedPerformed interface
             * medianAlgo.addListener(this); // Hide dialog setVisible(false); // These next lines set the titles in all
             * frames where the source image is displayed to // "locked - " image name so as to indicate that the image
             * is now read/write locked! // The image frames are disabled and then unregisted from the userinterface
             * until the // algorithm has completed. Vector imageFrames = image.getImageFrameVector(); titles = new
             * String[imageFrames.size()]; for (int i = 0; i < imageFrames.size(); i++) { titles[i] =
             * ((ViewJFrameBase)(imageFrames.elementAt(i))).getTitle();
             * ((ViewJFrameBase)(imageFrames.elementAt(i))).setTitle("Locked: " + titles[i] );
             * ((ViewJFrameBase)(imageFrames.elementAt(i))).setEnabled(false);
             * userInterface.unregisterFrame((Frame)(imageFrames.elementAt(i))); } if (isRunInSeparateThread()) { //
             * Start the thread as a low priority because we wish to still have user interface work fast. if
             * (medianAlgo.startMethod(Thread.MIN_PRIORITY) == false){ MipavUtil.displayError("A thread is already
             * running on this object"); } } else { medianAlgo.run(); } } catch (OutOfMemoryError x){
             * MipavUtil.displayError("Dialog median: unable to allocate enough memory"); return; } }*/
        }
    } // end callAlgorithm()

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        if (displayLoc == NEW) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        sourceImage = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = sourceImage.getParentFrame();

        if (scriptParameters.getParams().getBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE)) {
            displayLoc = NEW;
        } else {
            displayLoc = REPLACE;
        }

        setUnsharp(scriptParameters.getParams().getList("unsharp_scale").getAsFloatArray());
        setUnsharpWeight(scriptParameters.getParams().getFloat("unsharp_weight"));
        setBlurringFreq(scriptParameters.getParams().getFloat("blurring_freq"));
        setBlurringDiameter(scriptParameters.getParams().getInt("blurring_diameter"));

        colorPanel = new JPanelColorChannels(sourceImage);
        scriptParameters.setColorOptionsGUI(colorPanel);
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(sourceImage);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("unsharp_scale", unsharp));
        scriptParameters.getParams().put(ParameterFactory.newParameter("unsharp_weight", unsharpWeight));
        scriptParameters.getParams().put(ParameterFactory.newParameter("blurring_freq", blurringFreq));
        scriptParameters.getParams().put(ParameterFactory.newParameter("blurring_diameter", blurringDiameter));

        scriptParameters.storeColorOptions(colorPanel);
    }

    /**
     * part of the algorithm rests on blurring the original image.
     *
     * <p>This is the same as using as blurring filter; so, this panel is a modified JDialogFrequencyFilter, permitting
     * only a Gaussian low-pass, so only a top-end frequency input is created.</p>
     *
     * <p>The panel is returned to the caller.</p>
     *
     * @return  The blurring parameter panel.
     *
     * @see     JDialogUnsharpMask
     */
    private JPanel buildBlurringPanel() {
        JPanel blurp = new JPanel();
        Insets spacer = new Insets(0, 10, 0, 0);
        Insets nospace = new Insets(0, 0, 0, 0);

        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();

        blurp.setLayout(gbl);
        blurp.setBorder(buildTitledBorder("Blurring Lowpass Filter"));

        gbc.anchor = GridBagConstraints.CENTER;

        gbc.gridwidth = GridBagConstraints.RELATIVE;
        gbc.insets = nospace;
        blurp.add(createLabel("Blurring Frequency (f > 0.0):"), gbc);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.insets = spacer;
        blurringFreqText = createEntryField("0.2");
        MipavUtil.makeNumericsOnly(blurringFreqText, true);
        blurp.add(blurringFreqText, gbc);

        /* options for Z-dimension are not added to the display because
         * we do not use them.  We may never, but the code is left in just-in-case.
         */
        gbc.gridwidth = GridBagConstraints.RELATIVE;
        gbc.insets = nospace;
        // blurp.add(createLabel("Kernel Diameter:"), gbc);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.insets = spacer;
        blurringDiameterText = createEntryField("15");
        blurringDiameterText.setEnabled(true);
        // blurp.add(blurringDiameterText, gbc);

        blurp.setToolTipText("for blurring original");

        return blurp;
    }

    /**
     * creates the planel which contains the OKAY and Cancel buttons. sets their sizes, colours and listeners.
     *
     * @return  DOCUMENT ME!
     */
    private JPanel buildOkayCancelPanel() {
        return buildButtons();
    }

    /**
     * part of the algorithm rests on finding the original image minus an estimation of the local mean. So the input
     * panel to set variables related to finding an unsharp mask image is created here.
     *
     * <p>Part of the algorithm rests on blurring the original image. So the input panel to set the variables related to
     * blurring the image is created here.</p>
     *
     * <p>A colour image may have any of its three colour channels filtered, so a colour-selection panel is created. A
     * colour panel will be generated even for a monochrome image, the colour panel will be disabled.</p>
     *
     * @return  A panel containing all the other algorithm option panels.
     */
    private JPanel buildOptionsPanel() {
        PanelManager manager = new PanelManager();

        manager.addOnNextLine(buildUnsharpPanel());
        manager.addOnNextLine(buildBlurringPanel());
        manager.addOnNextLine(buildColourPanel());

        return manager.getPanel();
    }

    /**
     * part of the algorithm rests on finding the original image minus an estimation of the local mean.
     *
     * <p>This is the same as using as unsharp-mask filter; so, this panel is a recreation of the inputs made in the
     * JDialogUnsharpMask.</p>
     *
     * <p>The panel is returned to the caller.</p>
     *
     * @return  The unsharp mask parameter panel.
     *
     * @see     JDialogUnsharpMask
     */
    private JPanel buildUnsharpPanel() {
        JPanel unshrp = new JPanel();
        Insets spacer = new Insets(0, 10, 0, 0);
        Insets nospace = new Insets(0, 0, 0, 0);

        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();

        unshrp.setLayout(gbl);
        unshrp.setBorder(buildTitledBorder("Unsharp masking"));

        gbc.gridwidth = GridBagConstraints.RELATIVE;
        gbc.insets = nospace;
        gbc.anchor = GridBagConstraints.WEST;
        unshrp.add(createLabel("X-Dimension Gaussian scale:"), gbc);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.insets = spacer;
        gbc.anchor = GridBagConstraints.EAST;
        unsharpXtext = createEntryField("1.0");
        MipavUtil.makeNumericsOnly(unsharpXtext, true);
        unshrp.add(unsharpXtext, gbc);

        gbc.gridwidth = GridBagConstraints.RELATIVE;
        gbc.insets = nospace;
        gbc.anchor = GridBagConstraints.WEST;
        unshrp.add(createLabel("Y-Dimension Gaussian scale:"), gbc);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.insets = spacer;
        gbc.anchor = GridBagConstraints.EAST;
        unsharpYtext = createEntryField("1.0");
        MipavUtil.makeNumericsOnly(unsharpYtext, true);
        unshrp.add(unsharpYtext, gbc);

        /* options for Z-dimension are not added to the display because
         * we do not use them.  We may never, but the code is left in just-in-case.
         */
        gbc.gridwidth = GridBagConstraints.RELATIVE;
        gbc.insets = nospace;
        gbc.anchor = GridBagConstraints.WEST;

        JLabel zDimlabel = createLabel("Z-Dimension Gaussian scale:");
        // unshrp.add(zDimlabel, gbc);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.insets = spacer;
        gbc.anchor = GridBagConstraints.EAST;
        unsharpZtext = createEntryField("1.0");
        MipavUtil.makeNumericsOnly(unsharpZtext, true);
        // unshrp.add(unsharpZtext, gbc);

        zDimlabel.setForeground(Color.gray); // always set unusable, since always processing

        // unsharpZtext.setEnabled(false); // slices independntly -- ie, 2.5d

        gbc.gridwidth = GridBagConstraints.RELATIVE;
        gbc.insets = nospace;
        gbc.anchor = GridBagConstraints.CENTER;
        unshrp.add(createLabel("Weight of Blur (image - weight*blur):"), gbc);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.insets = spacer;
        unsharpWeightText = createEntryField("0.75");

        MipavUtil.makeNumericsOnly(unsharpWeightText, true);
        unshrp.add(unsharpWeightText, gbc);

        unshrp.setToolTipText("(original image) - (local mean)");

        return unshrp;
    }

    /**
     * check the variables of the unsharping-mask panel as they are translated from dialog inputs (ie., <code>
     * JTextField</code>s) to more usable, native types.
     *
     * <P>The panel displays the appropriate error when there is an input violation.</P>
     *
     * @return  <code>true</code> when the variables were copied correctly; <code>false</code> when there is an error in
     *          an input;
     */
    private boolean checkBlurring() {

        try {
            blurringFreq = Float.parseFloat(blurringFreqText.getText());
        } catch (NullPointerException npe) {
            MipavUtil.displayError("No value!  Value must be greater than 0.0"); // something more instructive?
            errorComponent = blurringFreqText;
            errorComponent.requestFocus();
            errorComponent.selectAll(); // doesn't really do anything since the object is empty

            return false;
        }

        if (blurringFreq <= 0.0) {
            MipavUtil.displayError("Value must be greater than 0.0"); // something more instructive?
            errorComponent = blurringFreqText;
            errorComponent.requestFocus();
            errorComponent.selectAll(); // doesn't really do anything since the object is empty

            return false;
        }

        try {
            blurringDiameter = Integer.parseInt(blurringDiameterText.getText());
        } catch (NullPointerException npe) {
            MipavUtil.displayError("No value!  Value must be both positive and odd."); // something more instructive?
            errorComponent = blurringDiameterText;
            errorComponent.requestFocus();
            errorComponent.selectAll(); // doesn't really do anything since the object is empty

            return false;
        }

        if (((blurringDiameter % 2) == 0) || (blurringDiameter <= 0)) {
            MipavUtil.displayError("Value must be both positive and odd."); // something more instructive?
            errorComponent = blurringDiameterText;
            errorComponent.requestFocus();
            errorComponent.selectAll(); // doesn't really do anything since the object is empty

            return false;
        }

        // looks okay, so
        return true;
    }

    /**
     * verify that the numeric value of the text of the submitted JTextField is between a and b.
     *
     * @param   jtf  The text field to check.
     * @param   a    The minimum allowable value for the text field.
     * @param   b    The maximum allowable value for the text field.
     *
     * @return  The value contained within the text field.
     *
     * @throws  NullPointerException      if jtf is empty.
     * @throws  IllegalArgumentException  when the String is either not translatable to a float (ie., when <code>
     *                                    Float.parseFloat(jtf.getText())</code> throws a ClassCastException), or when
     *                                    the number is out-of-bounds. Note: a JTextField properly implementing <code>
     *                                    makeNumericsOnly(JTextField)</code> should <i>always</i> translate into a
     *                                    float in the example above.
     *
     * @see     MipavUtil#makeNumericsOnly(JTextField, boolean)
     * @see     NullPointerException
     * @see     IllegalArgumentException
     */
    private float checkText(JTextField jtf, float a, float b) {
        String number;
        float val;

        try {
            number = jtf.getText();
        } catch (NullPointerException npe) {
            errorComponent = jtf;
            throw new NullPointerException("No value!");
        }

        try {
            val = Float.parseFloat(number);
        } catch (ClassCastException cce) {
            errorComponent = jtf;
            throw new IllegalArgumentException("Value is not a number!");
        }

        if ((val < a) || (val > b)) {
            errorComponent = jtf;
            throw new IllegalArgumentException("Value is out of bounds!");
        }

        // else
        return val;
    }

    /**
     * check the variables of the unsharping-mask panel as they are translated from dialog inputs (ie., <code>
     * JTextField</code>s) to more usable, native types.
     *
     * <P>The panel displays the appropriate error when there is an input violation.</P>
     *
     * @return  <code>true</code> when the variables were copied correctly; <code>false</code> when there is an error in
     *          an input;
     */
    private boolean checkUnsharping() {

        try {
            unsharp[0] = checkText(unsharpXtext, UNSHARP_MIN, UNSHARP_MAX);
            unsharp[1] = checkText(unsharpYtext, UNSHARP_MIN, UNSHARP_MAX);
            // unsharp[2] = checkText(unsharpZtext, UNSHARP_MIN, UNSHARP_MAX); // unused. FIXME
        } catch (NullPointerException npe) {
            MipavUtil.displayError(npe.getMessage()); // something more instructive?
            errorComponent.requestFocus();
            errorComponent.selectAll(); // doesn't really do anything since the object is empty

            return false;
        } catch (IllegalArgumentException iae) {
            MipavUtil.displayError(iae.getMessage() + "  Use values between " + UNSHARP_MIN + " and " + UNSHARP_MAX);
            errorComponent.requestFocus();
            errorComponent.selectAll();

            return false;
        }

        try {
            unsharpWeight = checkText(unsharpWeightText, UNSHARP_WEIGHT_MIN, UNSHARP_WEIGHT_MAX);
        } catch (NullPointerException npe) {
            MipavUtil.displayError(npe.getMessage()); // something more instructive?
            errorComponent.requestFocus();
            errorComponent.selectAll(); // doesn't really do anything since the object is empty

            return false;
        } catch (IllegalArgumentException iae) {
            MipavUtil.displayError(iae.getMessage() + "  Use values between " + UNSHARP_WEIGHT_MIN + " and " +
                                   UNSHARP_WEIGHT_MAX);
            errorComponent.requestFocus();
            errorComponent.selectAll();

            return false;
        }

        return true;
    }


    /**
     * Builds a new JTextField, with the given String, sets its font (to MipavUtil.font12), sets the foreground colour
     * (to Color.black), sets column width to 7, then returns the newly made JTextField.
     *
     * @param   presetText  the String to have the JTextField display.
     *
     * @return  a black-text, font12'd, JTextField displaying presetText.
     */
    private JTextField createEntryField(String presetText) {
        JTextField jtf = new JTextField(presetText);
        jtf.setFont(MipavUtil.font12);
        jtf.setForeground(Color.black);
        jtf.setColumns(7);
        jtf.setHorizontalAlignment(JTextField.RIGHT);

        return jtf;
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        // String tmpStr;

        // if (replaceImage.isSelected())    displayLoc = REPLACE;
        // else if (newImage.isSelected())   displayLoc = NEW;

        // if (wholeImage.isSelected())      regionFlag = true;
        // else if (VOIRegions.isSelected()) regionFlag = false;

        // check the variables in the unsharp-masking panel as they are
        // translated
        if (!checkUnsharping()) {
            return false;
        }

        // check the variables in the blurring panel as they are translated
        if (!checkBlurring()) {
            return false;
        }

        // if (bySlice != null) image25D = bySlice.isSelected();
        return true;
    }
}
