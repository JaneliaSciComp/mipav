package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

import javax.swing.*;

import WildMagic.LibFoundation.Curves.BSplineBasisf;


/**
 * This class is used to display the options to the user for performing 2D, 3D, and 2.5D B-Spline registration. In the
 * case of 2D and 3D, a separate target image is specified. In the case of 2.5D, the target image (slice) is selected
 * from the input source image. The registration is always performed against the target image so the output registered
 * image has the values from the input source image but the dimensions of the target for 2D/3D and the dimensions of the
 * source image for 2.5D. The same dialog is presented for 2D and 3D. The dialog is nearly the same for 2.5D except for
 * how the target slice is selected.
 */
public class JDialogRegistrationBSpline extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 8193864043052368084L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME.* */
    private String kStringDimension;

    /**
     * Contains names of compatible target images that can be registered to the input source image for 2D/3D
     * registration. Null reference for 2.5D registration.
     */
    private String[] m_akNamesCompatibleTargetImages = null;

    /**
     * Flag set if the deformation computation is to be performed. Reflects the state of the associated check box in the
     * dialog.
     */
    private boolean m_bCreateDeformationImage = false;

    /**
     * Index of the slice from the source image to use for 2.5 registration. The index may be -1 to indicate
     * registration is with adjacent slice. Reflects the state of the target slice radio buttons and the combo box if a
     * particular reference slice is to be used.
     */
    private int m_iTargetSlice = -1;

    /** Flag set if the deformation computation is to be performed. */
    private AlgorithmRegBSpline m_kAlgorithmReg;

    /** DOCUMENT ME! */
    private JCheckBox m_kCheckCreateDeformationImage;

    /** DOCUMENT ME! */
    private JCheckBox m_kCheckMultiPass;

    /** DOCUMENT ME! */
    private JComboBox m_kComboBoxCostFunction;

    /** Controls displayed in the dialog box. All of these controls are always displayed unless otherwise noted. */
    private JComboBox m_kComboBoxTargetImage; // only for 2D/3D

    /** DOCUMENT ME! */
    private JComboBox m_kComboBoxTargetSlice; // only for 2.5D

    /**
     * Container for controls to select the parameters for the first pass of registration. These controls are always
     * displayed.
     */
    private Controls m_kControlsPass1;

    /**
     * Container for controls to select the parameters for the optional second pass of registration. These controls are
     * created but are only displayed when the check box is selected for two-pass registration.
     */
    private Controls m_kControlsPass2;

    /**
     * Reference to new image creatd with computed deformation resulting from registration. Null reference is
     * deformation computation is not selected.
     */
    private ModelImage m_kImageDef = null;

    /** Reference to new image created with result of registration of source image. Cannot be null. */
    private ModelImage m_kImageReg = null;

    /** Reference to the input source image. Cannot be null. */
    private ModelImage m_kImageSrc;

    /** Reference to the selected target image. Null reference for 2.5D registration. */
    private ModelImage m_kImageTrg = null;

    /** DOCUMENT ME! */
    private JLabel m_kLabelIterationsPass1;

    /** DOCUMENT ME! */
    private JLabel m_kLabelIterationsPass2; // only if two-pass registration

    /** DOCUMENT ME! */
    private JLabel m_kLabelOptionsPass1;

    /** DOCUMENT ME! */
    private JLabel m_kLabelOptionsPass2; // only if two-pass registration

    /** Reference to parameters to use for first pass of registration. Cannot be null. */
    private AlgorithmRegBSpline.Options m_kOptionsPass1 = new AlgorithmRegBSpline.Options();

    /**
     * Reference to parameters to use for optional second pass of registration. Null reference if only a single pass of
     * registration is to be performed.
     */
    private AlgorithmRegBSpline.Options m_kOptionsPass2 = null;

    /** DOCUMENT ME! */
    private JRadioButton m_kRadioSliceAdjacent; // only for 2.5D

    /** DOCUMENT ME! */
    private JRadioButton m_kRadioSliceReference; // only for 2.5D

    /**
     * Reference to concrete implementation of the RegistrationMeasure abstract class which defines the particular
     * measure to use during registration. Cannot be null. Reflects the selection in the associated combo box in the
     * dialog.
     */
    private RegistrationMeasure m_kRegMeasure = null;

    /** Used for scripting and to access the currently registered images. */
    private ViewUserInterface m_kUI;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation. Used primarily for the script to store variables and run the
     * algorithm. No actual dialog will appear but the set up info and result image will be stored here.
     */
    public JDialogRegistrationBSpline() {}

    /**
     * Creates new registration dialog.
     * 
     * @param kParentFrame Parent frame
     * @param kImageSrc Source image
     * @param akNamesCompatibleTargetImages String[] Array containing the names of target images which are compatible
     *            for registering to the input source image. This list must contain at least one image name and the list
     *            must not contain the name of the input source image. A target image is used for 2D/3D registration. If
     *            this reference is null, then the registration is for 2.5 meaning the target image will be a selected
     *            slice from within the 3D image.
     */
    public JDialogRegistrationBSpline(final Frame kParentFrame, final ModelImage kImageSrc,
            final String[] akNamesCompatibleTargetImages) {
        super(kParentFrame, false);
        m_kImageSrc = kImageSrc;
        m_kUI = ViewUserInterface.getReference();
        m_akNamesCompatibleTargetImages = akNamesCompatibleTargetImages;
        initControls();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Find the frame images that are compatible as targets with the specified source image for the purpose of B-spline
     * registration.
     * 
     * @param kImageSrc ModelImage Reference to the source image.
     * 
     * @return String[] Array of names of compatible images.
     */
    public static String[] getNamesCompatibleTargetImages(final ModelImage kImageSrc) {

        // Get the names of all the registered images.
        final Enumeration<String> kRegisteredImageNames = ViewUserInterface.getReference().getRegisteredImageNames();
        final Vector<String> kNamesList = new Vector<String>();

        // Find framed target images which can be registered to the specified
        // source image given the type of source image. Must match in
        // number of dimensions and type (i.e., color vs. intensity).
        while (kRegisteredImageNames.hasMoreElements()) {
            final String kName = kRegisteredImageNames.nextElement();

            // Skip the source image when it is found.
            if (kName.equals(kImageSrc.getImageName())) {
                continue;
            }

            // Retrieve image by its name.
            final ModelImage kImage = ViewUserInterface.getReference().getRegisteredImageByName(kName);

            // Skip image if it is not in a frame.
            // That must mean it is a temporary image.
            if (null == ViewUserInterface.getReference().getFrameContainingImage(kImage)) {
                continue;
            }

            // Skip image if it is the source image.
            // Only accept images that have the same dimensions.
            if (kImage.getNDims() != kImageSrc.getNDims()) {
                continue;
            }

            // Only accept images that have the same number of channels;
            // i.e., both are color or both are monochrome.
            if (kImage.isColorImage() != kImageSrc.isColorImage()) {
                continue;
            }

            kNamesList.add(kName);
        }

        final String[] akNamesCompatibleTargetImages = new String[kNamesList.size()];

        for (int i = 0; i < akNamesCompatibleTargetImages.length; i++) {
            akNamesCompatibleTargetImages[i] = kNamesList.get(i);
        }

        return akNamesCompatibleTargetImages;
    }

    /**
     * Closes dialog box when the OK button is pressed, sets up the variables needed for running the algorithm, and
     * calls the algorithm.
     * 
     * @param event Event that triggers function
     */
    public void actionPerformed(final ActionEvent event) {
        final Object source = event.getSource();

        if (source == OKButton) {

            // In order to retrieve the dialog values for the regisration,
            // the size of each slice to be registered needs to be used
            // as a basis for computing the limits of certain input values.
            int[] aiExtentsReg;

            if (isRefImageSourceSlice()) {
                aiExtentsReg = new int[2];
                aiExtentsReg[0] = m_kImageSrc.getExtents()[0];
                aiExtentsReg[1] = m_kImageSrc.getExtents()[1];
            } else {
                aiExtentsReg = m_kImageTrg.getExtents();
            }

            if (m_kControlsPass1.getValues(m_kOptionsPass1, aiExtentsReg)
                    && ( !m_kCheckMultiPass.isSelected() || m_kControlsPass2.getValues(m_kOptionsPass2, aiExtentsReg))) {

                setVisible(false);
                callAlgorithm();
            }

            aiExtentsReg = null;
        } else if (source == cancelButton) {
            dispose();
        } else if (source == helpButton) {

            if (kStringDimension.equals("3")) {

                if (m_kCheckMultiPass.isSelected()) {
                    MipavUtil.showHelp("19018");
                } else {
                    MipavUtil.showHelp("19016");
                }
            } else if (kStringDimension.equals("2.5")) {

                if (m_kCheckMultiPass.isSelected()) {
                    MipavUtil.showHelp("19022");
                } else {
                    MipavUtil.showHelp("19020");
                }
            } else if (kStringDimension.equals("2")) {

                if (m_kCheckMultiPass.isSelected()) {
                    MipavUtil.showHelp("19018");
                } else {
                    MipavUtil.showHelp("19016");
                }
            }
        }
    }

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can display the result image and/or clean up.
     * 
     * @param algorithm Algorithm that caused the event.
     */
    public void algorithmPerformed(final AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmRegBSpline) {
            m_kImageSrc.clearMask();

            if ( (m_kAlgorithmReg.isCompleted() == true) && (m_kImageReg != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                JDialogBase.updateFileInfo(m_kImageSrc, m_kImageReg);
                m_kImageReg.clearMask();

                try {
                    new ViewJFrameImage(m_kImageReg, null, new Dimension(610, 200));

                    if (m_kImageDef != null) {
                        new ViewJFrameImage(m_kImageDef, null, new Dimension(610, 240));
                    }
                } catch (final OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }

                if ( !isRefImageSourceSlice()) {
                    m_kImageReg.getMatrixHolder().replaceMatrices(m_kImageTrg.getMatrixHolder().getMatrices());

                    if (m_kImageReg.getNDims() == 3) {
                        for (int i = 0; i < m_kImageReg.getExtents()[2]; i++) {
                            m_kImageReg.getFileInfo(i).setOrigin(m_kImageTrg.getFileInfo(i).getOrigin());
                        }
                    } else {
                        m_kImageReg.getFileInfo(0).setOrigin(m_kImageTrg.getFileInfo(0).getOrigin());
                    }
                }

            }
            // algorithm failed but result image(s) still has garbage
            else {

                if (m_kImageReg != null) {
                    m_kImageReg.disposeLocal();
                    m_kImageReg = null;
                    System.gc();
                }

                if (m_kImageDef != null) {
                    m_kImageDef.disposeLocal();
                    m_kImageDef = null;
                    System.gc();
                }
            }
        }

        // Update frame
        m_kImageSrc.notifyImageDisplayListeners(null, true);

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        // save the completion status for later
        setComplete(algorithm.isCompleted());

        if (m_kAlgorithmReg != null) {
            m_kAlgorithmReg.finalize();
            m_kAlgorithmReg = null;
        }

        dispose();
    }

    /**
     * Implementation of JDialogBase abstract method. Method to handle item events.
     * 
     * @param event Event that caused the method to fire
     */
    public void itemStateChanged(final ItemEvent event) {
        final Object source = event.getSource();

        // If the target image is changed reset the defaults on certain
        // other fields in the dialog.
        if (source == m_kComboBoxTargetImage) {
            userSetRefImage(m_kUI.getRegisteredImageByName((String) m_kComboBoxTargetImage.getSelectedItem()));
            userSetDefaults();
            pack();
        }

        // If the multipass check box is changed, then show/hide
        // the 2nd pass controls.
        else if (source == m_kCheckMultiPass) {
            userSetMultiPassControls();
            pack();
        }

        // If the cost function is changed, then select the appropriate
        // registration measure instance.
        else if (source == m_kComboBoxCostFunction) {
            userSetCostFunction();
        }

        // If the state of the check box changed for creating
        // the deformation image.
        else if (source == m_kCheckCreateDeformationImage) {
            m_bCreateDeformationImage = m_kCheckCreateDeformationImage.isSelected();
        }

        // If the state of any of the target slice controls for 2.5
        // registration changed.
        else if ( (source == m_kRadioSliceAdjacent) || (source == m_kRadioSliceReference)
                || (source == m_kComboBoxTargetSlice)) {
            userSetTargetSlice();
        }
    }

    /**
     * Runs the algorithm.
     */
    protected void callAlgorithm() {

        try {

            // Note the extents/resolutions of the registered result image.
            final int[] aiImageExtentsReg = m_kImageTrg.getExtents();
            final float[] afImageResolutionsReg = m_kImageTrg.getFileInfo(0).getResolutions();

            // Create deformation image if option is selected.
            // This is a single channel image with the same dimensions
            // as the target image.
            if (m_bCreateDeformationImage) {
                m_kImageDef = new ModelImage(ModelStorageBase.FLOAT, aiImageExtentsReg, JDialogBase.makeImageName(
                        m_kImageSrc.getImageName(), "_deformation"));
                m_kImageDef.getFileInfo(0).setResolutions(afImageResolutionsReg);
            } else {
                m_kImageDef = null;
            }

            // Create registration result image.
            final int iImageTypeReg = m_kImageSrc.isColorImage() ? ModelStorageBase.ARGB_FLOAT : ModelStorageBase.FLOAT;
            m_kImageReg = new ModelImage(iImageTypeReg, aiImageExtentsReg, JDialogBase.makeImageName(m_kImageSrc
                    .getImageName(), "_registered"));
            m_kImageReg.getFileInfo(0).setResolutions(afImageResolutionsReg);

            // 2.5D registration
            if (isRefImageSourceSlice()) {
                m_kAlgorithmReg = new AlgorithmRegBSpline25D(m_kImageReg, m_kImageSrc, m_iTargetSlice, m_kImageDef,
                        m_kRegMeasure, m_kOptionsPass1, m_kOptionsPass2);
            }

            // 2D registration
            else if (2 == m_kImageSrc.getNDims()) {
                m_kAlgorithmReg = new AlgorithmRegBSpline2D(m_kImageReg, m_kImageSrc, m_kImageTrg, m_kImageDef,
                        m_kRegMeasure, m_kOptionsPass1, m_kOptionsPass2);
            }

            // 3D registration
            else if (3 == m_kImageSrc.getNDims()) {
                m_kAlgorithmReg = new AlgorithmRegBSpline3D(m_kImageReg, m_kImageSrc, m_kImageTrg, m_kImageDef,
                        m_kRegMeasure, m_kOptionsPass1, m_kOptionsPass2);
            } else {
                MipavUtil.displayError("JDialogRegistrationBSpline - unexpected.");
                dispose();

                return;
            }
        } catch (final OutOfMemoryError x) {
            MipavUtil.displayError("Dialog RegistrationNonlinear: unable to allocate enough memory");

            if (m_kImageReg != null) {
                m_kImageReg.disposeLocal();
                m_kImageReg = null;
            }

            if (m_kImageDef != null) {
                m_kImageDef.disposeLocal();
                m_kImageDef = null;
            }

            dispose();

            return;
        }

        // This is very important. Adding this object as a listener allows
        // the algorithm to notify this object when it has completed or failed.
        // See algorithm performed event. This is made possible by implementing
        m_kAlgorithmReg.addListener(this);

        createProgressBar(m_kImageSrc.getImageName(), m_kAlgorithmReg);

        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still have
            // user interface work fast
            if (m_kAlgorithmReg.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            m_kAlgorithmReg.run();
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(m_kImageReg);
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings("unchecked")
    protected void setGUIFromParams() {
        m_kImageSrc = scriptParameters.retrieveInputImage();
        userSetRefImage(scriptParameters.retrieveImage("reference_image"));

        m_kUI = ViewUserInterface.getReference();
        parentFrame = m_kImageSrc.getParentFrame();
        m_akNamesCompatibleTargetImages = JDialogRegistrationBSpline.getNamesCompatibleTargetImages(m_kImageSrc);

        if (scriptParameters.getParams().getBoolean("is_ref_img_a_src_img_slice")) {
            m_iTargetSlice = scriptParameters.getParams().getInt("ref_slice_in_src_img");
            userSetRefImage(m_kImageSrc);
        } else {
            m_iTargetSlice = -1;
        }

        // get cost function
        final String kCostMeasureName = scriptParameters.getParams().getString("cost_measure");

        try {
            final Class<RegistrationMeasure> kCostMeasureClass = (Class<RegistrationMeasure>) Class
                    .forName(kCostMeasureName);
            m_kRegMeasure = kCostMeasureClass.newInstance();
        } catch (final ClassNotFoundException cnfe) {
            throw new ParameterException("cost_measure", "Unable to find cost measure class: " + kCostMeasureName);
        } catch (final InstantiationException ie) {
            throw new ParameterException("cost_measure", "Unable to instantiate cost measure class: "
                    + kCostMeasureName);
        } catch (final IllegalAccessException iae) {
            throw new ParameterException("cost_measure",
                    "Access denied while trying to instantiate cost measure class: " + kCostMeasureName);
        }

        // get the options for each pass
        final int iNumPasses = scriptParameters.getParams().getInt("num_passes");
        m_kOptionsPass1 = new AlgorithmRegBSpline.Options();

        // old versions of the scripting kept all the pass options in one string
        if (scriptParameters.getParams().containsParameter("pass_options_1")) {
            try {
                m_kOptionsPass1.setFromString(scriptParameters.getParams().getString("pass_options_1"), " ");
            } catch (final TokenizerException te) {
                throw new ParameterException("pass_options_1",
                        "There is a problem with the options for the first pass.");
            }

            // assume that if the pass_options_1 parameter was used, the pass_options_2 parameter will also be used
            if (2 == iNumPasses) {
                m_kOptionsPass2 = new AlgorithmRegBSpline.Options();

                try {
                    m_kOptionsPass2.setFromString(scriptParameters.getParams().getString("pass_options_2"), " ");
                } catch (final TokenizerException te) {
                    throw new ParameterException("pass_options_2",
                            "There is a problem with the options for the second pass.");
                }
            } else {
                m_kOptionsPass2 = null;
            }
        } else {
            // new version of the script parameters with the pass options broken out into separate params
            String paramPrefix = "pass_1_";
            m_kOptionsPass1 = new AlgorithmRegBSpline.Options();
            m_kOptionsPass1.bSubsample = scriptParameters.getParams().getBoolean(paramPrefix + "do_subsample");
            m_kOptionsPass1.iBSplineDegree = scriptParameters.getParams().getInt(paramPrefix + "bspline_degree");
            m_kOptionsPass1.iBSplineNumControlPoints = scriptParameters.getParams().getInt(
                    paramPrefix + "num_control_points");
            m_kOptionsPass1.fGradientDescentMinimizeStepSize = scriptParameters.getParams().getFloat(
                    paramPrefix + "gradient_descent_minimize_step_size");
            m_kOptionsPass1.iGradientDescentMinimizeMaxSteps = scriptParameters.getParams().getInt(
                    paramPrefix + "gradient_descent_minimize_max_steps");
            m_kOptionsPass1.fConvergenceLimit = scriptParameters.getParams()
                    .getFloat(paramPrefix + "convergence_limit");
            m_kOptionsPass1.iMaxIterations = scriptParameters.getParams().getInt(paramPrefix + "max_iterations");

            if (2 == iNumPasses) {
                paramPrefix = "pass_2_";
                m_kOptionsPass2 = new AlgorithmRegBSpline.Options();
                m_kOptionsPass2.bSubsample = scriptParameters.getParams().getBoolean(paramPrefix + "do_subsample");
                m_kOptionsPass2.iBSplineDegree = scriptParameters.getParams().getInt(paramPrefix + "bspline_degree");
                m_kOptionsPass2.iBSplineNumControlPoints = scriptParameters.getParams().getInt(
                        paramPrefix + "num_control_points");
                m_kOptionsPass2.fGradientDescentMinimizeStepSize = scriptParameters.getParams().getFloat(
                        paramPrefix + "gradient_descent_minimize_step_size");
                m_kOptionsPass2.iGradientDescentMinimizeMaxSteps = scriptParameters.getParams().getInt(
                        paramPrefix + "gradient_descent_minimize_max_steps");
                m_kOptionsPass2.fConvergenceLimit = scriptParameters.getParams().getFloat(
                        paramPrefix + "convergence_limit");
                m_kOptionsPass2.iMaxIterations = scriptParameters.getParams().getInt(paramPrefix + "max_iterations");
            } else {
                m_kOptionsPass2 = null;
            }
        }

        // get whether or not to create the deformation image
        m_bCreateDeformationImage = scriptParameters.getParams().getBoolean("do_create_deformation_image");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(m_kImageSrc);
        scriptParameters.storeImage(m_kImageTrg, "reference_image");

        scriptParameters.getParams().put(
                ParameterFactory.newParameter("is_ref_img_a_src_img_slice", isRefImageSourceSlice()));

        if (isRefImageSourceSlice()) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("ref_slice_in_src_img", m_iTargetSlice));
        } else {
            scriptParameters.storeImageInRecorder(m_kImageReg);
        }

        scriptParameters.getParams().put(
                ParameterFactory.newParameter("cost_measure", m_kRegMeasure.getClass().getName()));
        scriptParameters.getParams()
                .put(ParameterFactory.newParameter("num_passes", (m_kOptionsPass2 == null) ? 1 : 2));

        // old scripting params that are no longer used because they were opaque
        /*
         * scriptParameters.getParams() .put(ParameterFactory.newParameter("pass_options_1", m_kOptionsPass1.toString("
         * ")));
         * 
         * if (m_kOptionsPass2 != null) { scriptParameters.getParams().put(
         * ParameterFactory.newParameter("pass_options_2", m_kOptionsPass2.toString(" "))); }
         */

        // broke out individual pass parameters
        String paramPrefix = "pass_1_";
        scriptParameters.getParams().put(
                ParameterFactory.newParameter(paramPrefix + "do_subsample", m_kOptionsPass1.bSubsample));
        scriptParameters.getParams().put(
                ParameterFactory.newParameter(paramPrefix + "bspline_degree", m_kOptionsPass1.iBSplineDegree));
        scriptParameters.getParams().put(
                ParameterFactory.newParameter(paramPrefix + "num_control_points",
                        m_kOptionsPass1.iBSplineNumControlPoints));
        scriptParameters.getParams().put(
                ParameterFactory.newParameter(paramPrefix + "gradient_descent_minimize_step_size",
                        m_kOptionsPass1.fGradientDescentMinimizeStepSize));
        scriptParameters.getParams().put(
                ParameterFactory.newParameter(paramPrefix + "gradient_descent_minimize_max_steps",
                        m_kOptionsPass1.iGradientDescentMinimizeMaxSteps));
        scriptParameters.getParams().put(
                ParameterFactory.newParameter(paramPrefix + "convergence_limit", m_kOptionsPass1.fConvergenceLimit));
        scriptParameters.getParams().put(
                ParameterFactory.newParameter(paramPrefix + "max_iterations", m_kOptionsPass1.iMaxIterations));

        if (m_kOptionsPass2 != null) {
            paramPrefix = "pass_2_";
            scriptParameters.getParams().put(
                    ParameterFactory.newParameter(paramPrefix + "do_subsample", m_kOptionsPass2.bSubsample));
            scriptParameters.getParams().put(
                    ParameterFactory.newParameter(paramPrefix + "bspline_degree", m_kOptionsPass2.iBSplineDegree));
            scriptParameters.getParams().put(
                    ParameterFactory.newParameter(paramPrefix + "num_control_points",
                            m_kOptionsPass2.iBSplineNumControlPoints));
            scriptParameters.getParams().put(
                    ParameterFactory.newParameter(paramPrefix + "gradient_descent_minimize_step_size",
                            m_kOptionsPass2.fGradientDescentMinimizeStepSize));
            scriptParameters.getParams().put(
                    ParameterFactory.newParameter(paramPrefix + "gradient_descent_minimize_max_steps",
                            m_kOptionsPass2.iGradientDescentMinimizeMaxSteps));
            scriptParameters.getParams()
                    .put(
                            ParameterFactory.newParameter(paramPrefix + "convergence_limit",
                                    m_kOptionsPass2.fConvergenceLimit));
            scriptParameters.getParams().put(
                    ParameterFactory.newParameter(paramPrefix + "max_iterations", m_kOptionsPass2.iMaxIterations));
        }

        scriptParameters.getParams().put(
                ParameterFactory.newParameter("do_create_deformation_image", m_bCreateDeformationImage));
    }

    /**
     * Initializes the GUI components and displays the dialog.
     */
    private void initControls() {

        // What is the image dimension?
        kStringDimension = isRefImageSourceSlice() ? "2.5" : String.valueOf(m_kImageSrc.getNDims());

        setForeground(Color.black);
        setTitle("B-Spline Automatic Registration - " + kStringDimension + "D "
                + (m_kImageSrc.isColorImage() ? "Color" : "Intensity"));

        // Create controls for Pass 1.
        m_kControlsPass1 = new Controls(this);
        m_kLabelOptionsPass1 = new JLabel("Pass 1");
        m_kLabelOptionsPass1.setForeground(Color.black);
        m_kLabelOptionsPass1.setFont(serif12);
        m_kLabelOptionsPass1.setAlignmentX(Component.LEFT_ALIGNMENT);
        m_kLabelIterationsPass1 = new JLabel("Pass 1");
        m_kLabelIterationsPass1.setForeground(Color.black);
        m_kLabelIterationsPass1.setFont(serif12);
        m_kLabelIterationsPass1.setAlignmentX(Component.LEFT_ALIGNMENT);

        // Create controls for Pass 2.
        m_kControlsPass2 = new Controls(this);
        m_kControlsPass2.kCheckSubsample.setVisible(false);
        m_kLabelOptionsPass2 = new JLabel("Pass 2");
        m_kLabelOptionsPass2.setForeground(Color.black);
        m_kLabelOptionsPass2.setFont(serif12);
        m_kLabelOptionsPass2.setAlignmentX(Component.LEFT_ALIGNMENT);
        m_kLabelIterationsPass2 = new JLabel("Pass 2");
        m_kLabelIterationsPass2.setForeground(Color.black);
        m_kLabelIterationsPass2.setFont(serif12);
        m_kLabelIterationsPass2.setAlignmentX(Component.LEFT_ALIGNMENT);

        // label to select target (reference image)
        final JLabel labelImage = new JLabel("Register [" + m_kImageSrc.getImageName() + "] "
                + (isRefImageSourceSlice() ? "slices " : "") + "to:");
        labelImage.setForeground(Color.black);
        labelImage.setBounds(10, 20, 230, 25);
        labelImage.setFont(serif12);
        labelImage.setAlignmentX(Component.LEFT_ALIGNMENT);

        // For 2.5D - select target slice (adjacent or reference)
        if (isRefImageSourceSlice()) {

            // radio button for adjacent slice
            m_kRadioSliceAdjacent = new JRadioButton("Adjacent slice");
            m_kRadioSliceAdjacent.setFont(serif12);
            m_kRadioSliceAdjacent.addItemListener(this);

            // radio button for reference slice
            m_kRadioSliceReference = new JRadioButton("Reference slice");
            m_kRadioSliceReference.setFont(serif12);
            m_kRadioSliceReference.addItemListener(this);

            // Create a button group so that only one radio button
            // can be selected at a time.
            final ButtonGroup kButtonGroup = new ButtonGroup();
            kButtonGroup.add(m_kRadioSliceAdjacent);
            kButtonGroup.add(m_kRadioSliceReference);

            // combo box for reference slice number
            m_kComboBoxTargetSlice = new JComboBox();
            m_kComboBoxTargetSlice.setFont(serif12);
            m_kComboBoxTargetSlice.setBackground(Color.white);

            for (int i = 0; i < m_kImageSrc.getExtents()[2]; i++) {
                m_kComboBoxTargetSlice.addItem(String.valueOf(i));
            }

            m_kComboBoxTargetSlice.addItemListener(this);
        }
        // For 2D/3D - select target image combo box
        else {
            m_kComboBoxTargetImage = new JComboBox();
            m_kComboBoxTargetImage.setFont(serif12);
            m_kComboBoxTargetImage.setBackground(Color.white);

            for (final String element : m_akNamesCompatibleTargetImages) {
                m_kComboBoxTargetImage.addItem(element);
            }

            m_kComboBoxTargetImage.addItemListener(this);
        }

        // Combo box to select cost function.
        final JLabel labelCostFunction = new JLabel("Cost function:");
        labelCostFunction.setForeground(Color.black);
        labelCostFunction.setBounds(10, 20, 230, 25);
        labelCostFunction.setFont(serif12);
        labelCostFunction.setAlignmentX(Component.LEFT_ALIGNMENT);
        m_kComboBoxCostFunction = new JComboBox();
        m_kComboBoxCostFunction.setFont(serif12);
        m_kComboBoxCostFunction.setBackground(Color.white);
        m_kComboBoxCostFunction.setToolTipText("Cost function");
        m_kComboBoxCostFunction.addItem(RegistrationMeasureLeastSquares.getStaticName());
        m_kComboBoxCostFunction.addItem(RegistrationMeasureCorrelationRatio.getStaticName());
        m_kComboBoxCostFunction.addItem(RegistrationMeasureNormalizedMutualInformation.getStaticName());
        m_kComboBoxCostFunction.setSelectedIndex(1);
        m_kComboBoxCostFunction.setEnabled(true);
        m_kComboBoxCostFunction.addItemListener(this);

        // Check box to select single or two-pass registration.
        m_kCheckMultiPass = new JCheckBox("Perform two-pass registration.");
        m_kCheckMultiPass.setFont(serif12);
        m_kCheckMultiPass.setForeground(Color.black);
        m_kCheckMultiPass.setSelected(false);
        m_kCheckMultiPass.setEnabled(true);
        m_kCheckMultiPass.addItemListener(this);
        m_kCheckMultiPass.setSelected(false);

        // Check box to select computation of deformation field image.
        m_kCheckCreateDeformationImage = new JCheckBox("Create deformation field image.");
        m_kCheckCreateDeformationImage.setFont(serif12);
        m_kCheckCreateDeformationImage.setForeground(Color.black);
        m_kCheckCreateDeformationImage.setEnabled(false);
        m_kCheckCreateDeformationImage.setEnabled(true);
        m_kCheckCreateDeformationImage.setSelected(m_bCreateDeformationImage);
        m_kCheckCreateDeformationImage.addItemListener(this);

        // check box to subsample image for speed
        final JLabel kLabelSubsampleImage = new JLabel("Subsample image for speed");
        kLabelSubsampleImage.setForeground(Color.black);
        kLabelSubsampleImage.setFont(serif12);
        kLabelSubsampleImage.setAlignmentX(Component.LEFT_ALIGNMENT);

        // combo box to select degree+1 for B-Spline
        final JLabel kLabelBSplineDegree = new JLabel("B-Spline Degree (same for all axes):");
        kLabelBSplineDegree.setForeground(Color.black);
        kLabelBSplineDegree.setFont(serif12);
        kLabelBSplineDegree.setAlignmentX(Component.LEFT_ALIGNMENT);

        // text box to enter number of B-Spline control points
        final JLabel kLabelBSplineNumControlPoints = new JLabel("B-Spline Control Points (same for all axes):");
        kLabelBSplineNumControlPoints.setForeground(Color.black);
        kLabelBSplineNumControlPoints.setFont(serif12);

        // text box to enter the gradient descent minimize step size
        final JLabel kLabelGradientDescentMinimizeStepSize = new JLabel(
                "Gradient Descent Minimize Step Size (sample units):");
        kLabelGradientDescentMinimizeStepSize.setForeground(Color.black);
        kLabelGradientDescentMinimizeStepSize.setFont(serif12);

        // text box to enter the gradient descent minimize maximum number of steps
        final JLabel kLabelGradientDescentMinimizeMaxSteps = new JLabel("Gradient Descent Minimize Max Search Steps:");
        kLabelGradientDescentMinimizeMaxSteps.setForeground(Color.black);
        kLabelGradientDescentMinimizeMaxSteps.setFont(serif12);

        // text box for convergence limit
        final JLabel kLabelConvergenceLimit = new JLabel("Convergence limit (min change rate for one iteration):");
        kLabelConvergenceLimit.setForeground(Color.black);
        kLabelConvergenceLimit.setFont(serif12);

        // text box for maximum number of iterations
        final JLabel kLabelMaxIterations = new JLabel("Maximum Number of Iterations:");
        kLabelMaxIterations.setForeground(Color.black);
        kLabelMaxIterations.setFont(serif12);

        // Default constraints for a GridBagLayout
        final GridBagConstraints kGBC = new GridBagConstraints();
        kGBC.insets = new Insets(0, 2, 0, 2);
        kGBC.gridwidth = 1;
        kGBC.gridheight = 1;
        kGBC.anchor = GridBagConstraints.WEST;

        // Build the general sub 2-column panel.
        final JPanel kPanelGeneralSub = new JPanel(new GridBagLayout());

        kGBC.gridy = 0;
        kGBC.gridx = 0;
        kGBC.weightx = 1;
        kGBC.fill = GridBagConstraints.NONE;
        kPanelGeneralSub.add(labelImage, kGBC);
        kGBC.gridx = 1;
        kGBC.weightx = 0;
        kGBC.fill = GridBagConstraints.HORIZONTAL;

        if (isRefImageSourceSlice()) {

            // Build the sub panel to select the target slice.
            final JPanel kPanelTargetSlice = new JPanel(new GridBagLayout());
            kPanelGeneralSub.add(kPanelTargetSlice, kGBC);

            kGBC.gridy = 0;
            kGBC.gridx = 0;
            kGBC.weightx = 0.5;
            kGBC.fill = GridBagConstraints.HORIZONTAL;
            kPanelTargetSlice.add(m_kRadioSliceAdjacent, kGBC);
            kGBC.gridx = 1;
            kGBC.weightx = 0.25;
            kGBC.fill = GridBagConstraints.HORIZONTAL;
            kPanelTargetSlice.add(m_kRadioSliceReference, kGBC);
            kGBC.gridx = 2;
            kGBC.weightx = 0.25;
            kGBC.fill = GridBagConstraints.HORIZONTAL;
            kPanelTargetSlice.add(m_kComboBoxTargetSlice, kGBC);
        } else {
            kPanelGeneralSub.add(m_kComboBoxTargetImage, kGBC);
        }

        kGBC.gridy = 1;
        kGBC.gridx = 0;
        kGBC.weightx = 1;
        kGBC.fill = GridBagConstraints.NONE;
        kPanelGeneralSub.add(labelCostFunction, kGBC);
        kGBC.gridx = 1;
        kGBC.weightx = 0;
        kGBC.fill = GridBagConstraints.HORIZONTAL;
        kPanelGeneralSub.add(m_kComboBoxCostFunction, kGBC);

        // Build the general panel
        final JPanel kPanelGeneral = new JPanel(new GridBagLayout());
        kPanelGeneral.setBorder(buildTitledBorder("General"));

        kGBC.gridy = 0;
        kGBC.gridx = 0;
        kGBC.weightx = 1;
        kGBC.fill = GridBagConstraints.BOTH;
        kPanelGeneral.add(kPanelGeneralSub, kGBC);
        kGBC.gridy = 1;
        kGBC.gridx = 0;
        kGBC.weightx = 1;
        kGBC.fill = GridBagConstraints.BOTH;
        kPanelGeneral.add(m_kCheckMultiPass, kGBC);

        // Build the options panel.
        final JPanel kPanelOptions = new JPanel();
        kPanelOptions.setLayout(new GridBagLayout());
        kPanelOptions.setBorder(buildTitledBorder("Transformation Options"));

        kGBC.gridy = 0;
        kGBC.fill = GridBagConstraints.HORIZONTAL;
        kGBC.gridx = 1;
        kGBC.weightx = 0.5;
        kPanelOptions.add(m_kLabelOptionsPass1, kGBC);
        kGBC.gridx = 2;
        kGBC.weightx = 0.5;
        kPanelOptions.add(m_kLabelOptionsPass2, kGBC);

        kGBC.gridy = 1;
        kGBC.gridx = 0;
        kGBC.weightx = 1;
        kGBC.fill = GridBagConstraints.NONE;
        kPanelOptions.add(kLabelSubsampleImage, kGBC);
        kGBC.fill = GridBagConstraints.HORIZONTAL;
        kGBC.gridx = 1;
        kGBC.weightx = 0;
        kPanelOptions.add(m_kControlsPass1.kCheckSubsample, kGBC);
        kGBC.gridx = 2;
        kGBC.weightx = 0;
        kPanelOptions.add(m_kControlsPass2.kCheckSubsample, kGBC);

        kGBC.gridy = 2;
        kGBC.gridx = 0;
        kGBC.weightx = 1;
        kGBC.fill = GridBagConstraints.NONE;
        kPanelOptions.add(kLabelBSplineDegree, kGBC);
        kGBC.fill = GridBagConstraints.HORIZONTAL;
        kGBC.gridx = 1;
        kGBC.weightx = 0;
        kPanelOptions.add(m_kControlsPass1.kComboBoxBSplineDegree, kGBC);
        kGBC.gridx = 2;
        kGBC.weightx = 0;
        kPanelOptions.add(m_kControlsPass2.kComboBoxBSplineDegree, kGBC);

        kGBC.gridy = 3;
        kGBC.gridx = 0;
        kGBC.weightx = 1;
        kGBC.fill = GridBagConstraints.NONE;
        kPanelOptions.add(kLabelBSplineNumControlPoints, kGBC);
        kGBC.fill = GridBagConstraints.HORIZONTAL;
        kGBC.gridx = 1;
        kGBC.weightx = 0;
        kPanelOptions.add(m_kControlsPass1.kTextBSplineNumControlPoints, kGBC);
        kGBC.gridx = 2;
        kGBC.weightx = 0;
        kPanelOptions.add(m_kControlsPass2.kTextBSplineNumControlPoints, kGBC);

        kGBC.gridy = 4;
        kGBC.gridx = 0;
        kGBC.weightx = 1;
        kGBC.fill = GridBagConstraints.NONE;
        kPanelOptions.add(kLabelGradientDescentMinimizeStepSize, kGBC);
        kGBC.fill = GridBagConstraints.HORIZONTAL;
        kGBC.gridx = 1;
        kGBC.weightx = 0;
        kPanelOptions.add(m_kControlsPass1.kTextGradientDescentMinimizeStepSize, kGBC);
        kGBC.gridx = 2;
        kGBC.weightx = 0;
        kPanelOptions.add(m_kControlsPass2.kTextGradientDescentMinimizeStepSize, kGBC);

        kGBC.gridy = 5;
        kGBC.gridx = 0;
        kGBC.weightx = 1;
        kGBC.fill = GridBagConstraints.NONE;
        kPanelOptions.add(kLabelGradientDescentMinimizeMaxSteps, kGBC);
        kGBC.fill = GridBagConstraints.HORIZONTAL;
        kGBC.gridx = 1;
        kGBC.weightx = 0;
        kPanelOptions.add(m_kControlsPass1.kTextGradientDescentMinimizeMaxSteps, kGBC);
        kGBC.gridx = 2;
        kGBC.weightx = 0;
        kPanelOptions.add(m_kControlsPass2.kTextGradientDescentMinimizeMaxSteps, kGBC);

        // Build the iterations panel.
        final JPanel kPanelIterations = new JPanel();
        kPanelIterations.setLayout(new GridBagLayout());
        kPanelIterations
                .setBorder(buildTitledBorder("Iteration Options (one iteration = move each control point once)"));

        kGBC.gridy = 0;
        kGBC.fill = GridBagConstraints.HORIZONTAL;
        kGBC.gridx = 1;
        kGBC.weightx = 0.5;
        kPanelIterations.add(m_kLabelIterationsPass1, kGBC);
        kGBC.gridx = 2;
        kGBC.weightx = 0.5;
        kGBC.fill = GridBagConstraints.HORIZONTAL;
        kPanelIterations.add(m_kLabelIterationsPass2, kGBC);

        kGBC.gridy = 1;
        kGBC.gridx = 0;
        kGBC.weightx = 1;
        kGBC.fill = GridBagConstraints.NONE;
        kPanelIterations.add(kLabelConvergenceLimit, kGBC);
        kGBC.fill = GridBagConstraints.HORIZONTAL;
        kGBC.gridx = 1;
        kGBC.weightx = 0;
        kPanelIterations.add(m_kControlsPass1.kTextConvergenceLimit, kGBC);
        kGBC.gridx = 2;
        kGBC.weightx = 0;
        kPanelIterations.add(m_kControlsPass2.kTextConvergenceLimit, kGBC);

        kGBC.gridy = 2;
        kGBC.gridx = 0;
        kGBC.weightx = 1;
        kGBC.fill = GridBagConstraints.NONE;
        kPanelIterations.add(kLabelMaxIterations, kGBC);
        kGBC.fill = GridBagConstraints.HORIZONTAL;
        kGBC.gridx = 1;
        kGBC.weightx = 0;
        kPanelIterations.add(m_kControlsPass1.kTextMaxIterations, kGBC);
        kGBC.gridx = 2;
        kGBC.weightx = 0;
        kPanelIterations.add(m_kControlsPass2.kTextMaxIterations, kGBC);

        // Build the results panel
        final JPanel kPanelResults = new JPanel(new GridBagLayout());
        kPanelResults.setBorder(buildTitledBorder("Results"));

        kGBC.gridy = 0;
        kGBC.gridx = 0;
        kGBC.weightx = 1;
        kGBC.fill = GridBagConstraints.NONE;
        kPanelResults.add(m_kCheckCreateDeformationImage, kGBC);

        final JPanel kPanelButton = new JPanel();
        buildOKButton();
        buildCancelButton();
        buildHelpButton();
        kPanelButton.add(OKButton);
        kPanelButton.add(cancelButton);
        kPanelButton.add(helpButton);

        getContentPane().setLayout(new GridBagLayout());
        kGBC.gridx = 0;
        kGBC.gridy = 0;
        kGBC.weightx = 1;
        kGBC.fill = GridBagConstraints.BOTH;
        getContentPane().add(kPanelGeneral, kGBC);
        kGBC.gridy = 1;
        getContentPane().add(kPanelOptions, kGBC);
        kGBC.gridy = 2;
        getContentPane().add(kPanelIterations, kGBC);
        kGBC.gridy = 3;
        getContentPane().add(kPanelResults, kGBC);
        kGBC.gridy = 4;
        getContentPane().add(kPanelButton, kGBC);

        // If 2.5D registration, the target image is the same as the
        // source image.
        if (isRefImageSourceSlice()) {
            userSetRefImage(m_kImageSrc);

            // select reference slice to be middle slice
            m_kRadioSliceReference.setSelected(true);
            m_kComboBoxTargetSlice.setSelectedIndex( (m_kComboBoxTargetSlice.getItemCount() / 2) - 1);
            userSetTargetSlice();
        }
        // In 2D/3D registration, the target image is the currently
        // selected one in the combo box.
        else {

            // select first image in the list
            m_kComboBoxTargetImage.setSelectedIndex(0);
            userSetRefImage(m_kUI.getRegisteredImageByName(m_akNamesCompatibleTargetImages[0]));
        }

        userSetDefaults();
        userSetMultiPassControls();
        userSetCostFunction();
        pack();
        setVisible(true);
    }

    /**
     * Convenience method to determine if this is a 2.5D registration where the reference (target) image is once of the
     * slices in the source image.
     * 
     * @return boolean
     */
    private boolean isRefImageSourceSlice() {
        return null == m_akNamesCompatibleTargetImages;
    }

    /**
     * Called to create the RegistrationMeasure-derived class associated with the current selection in the combo box of
     * possible registration measures.
     */
    private void userSetCostFunction() {

        // What is the text that appears in the combo box?
        final String kStrDescription = (String) m_kComboBoxCostFunction.getSelectedItem();

        // Match that text to the known cost functions.
        if (RegistrationMeasureLeastSquares.getStaticName() == kStrDescription) {
            m_kRegMeasure = new RegistrationMeasureLeastSquares();
        } else if (RegistrationMeasureCorrelationRatio.getStaticName() == kStrDescription) {
            m_kRegMeasure = new RegistrationMeasureCorrelationRatio();
        } else if (RegistrationMeasureNormalizedMutualInformation.getStaticName() == kStrDescription) {
            m_kRegMeasure = new RegistrationMeasureNormalizedMutualInformation();
        }
    }

    /**
     * Called to reset all of the controls to their default values.
     */
    private void userSetDefaults() {

        // Set defaults for Pass 1.
        m_kOptionsPass1.iBSplineDegree = 1;
        m_kOptionsPass1.iBSplineNumControlPoints = 8;
        m_kOptionsPass1.fGradientDescentMinimizeStepSize = 1.0f;
        m_kOptionsPass1.iGradientDescentMinimizeMaxSteps = 10;
        m_kOptionsPass1.fConvergenceLimit = 0.1f;
        m_kOptionsPass1.iMaxIterations = 10;
        m_kOptionsPass1.bSubsample = true;
        m_kControlsPass1.setValues(m_kOptionsPass1);

        // Disable multipass.
        m_kCheckMultiPass.setSelected(false);
        userSetMultiPassControls();
    }

    /**
     * Called to setup controls passed on the current state of the check box for selecting single- or two-pass
     * registration.
     */
    private void userSetMultiPassControls() {

        if (m_kCheckMultiPass.isSelected()) {

            if (null == m_kOptionsPass2) {
                m_kOptionsPass2 = new AlgorithmRegBSpline.Options();
                m_kOptionsPass2.iBSplineDegree = 2;
                m_kOptionsPass2.iBSplineNumControlPoints = 16;
                m_kOptionsPass2.fGradientDescentMinimizeStepSize = 0.5f;
                m_kOptionsPass2.iGradientDescentMinimizeMaxSteps = 10;
                m_kOptionsPass2.fConvergenceLimit = 0.01f;
                m_kOptionsPass2.iMaxIterations = 10;
                m_kOptionsPass2.bSubsample = true;
                m_kControlsPass2.setValues(m_kOptionsPass2);
            }

            m_kLabelOptionsPass1.setVisible(true);
            m_kLabelOptionsPass2.setVisible(true);
            m_kLabelIterationsPass1.setVisible(true);
            m_kLabelIterationsPass2.setVisible(true);
            m_kControlsPass2.setVisible(true);
        } else {
            m_kLabelOptionsPass1.setVisible(false);
            m_kLabelOptionsPass2.setVisible(false);
            m_kLabelIterationsPass1.setVisible(false);
            m_kLabelIterationsPass2.setVisible(false);
            m_kControlsPass2.setVisible(false);
            m_kOptionsPass2 = null;
        }
    }

    /**
     * Accessor to set the target image.
     * 
     * @param image The target image.
     */
    private void userSetRefImage(final ModelImage image) {
        m_kImageTrg = image;
    }

    /**
     * Called whenever the user changes the selection of the target slice for 2.5D image registration.
     */
    private void userSetTargetSlice() {

        // Register to adjacent slice?
        if (m_kRadioSliceAdjacent.isSelected()) {
            m_iTargetSlice = -1;
            m_kComboBoxTargetSlice.setEnabled(false);
        }

        // Register to a specific slice?
        else {
            m_iTargetSlice = m_kComboBoxTargetSlice.getSelectedIndex();
            m_kComboBoxTargetSlice.setEnabled(true);
        }
    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms.Registration");
            }

            public String getDescription() {
                return new String(
                        "Performs a non-linear B-Spline Automatic Registration of one image to a reference image.");
            }

            public String getDescriptionLong() {
                return new String(
                        "Performs a non-linear B-Spline Automatic Registration of one image to a reference image.");
            }

            public String getShortLabel() {
                return new String("RegBSpline");
            }

            public String getLabel() {
                return new String("B-Spline registration");
            }

            public String getName() {
                return new String("B-Spline registration");
            }
        };
    }

    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
    public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterExternalImage("reference_image"));

            table.put(new ParameterBoolean("is_ref_img_a_src_img_slice", false));
            Parameter tempParam = new ParameterInt("ref_slice_in_src_img", 0);
            tempParam.setParentCondition(table.getParameter("is_ref_img_a_src_img_slice"), "true");
            table.put(tempParam);

            table.put(new ParameterString("cost_measure",
                    "gov.nih.mipav.model.algorithms.registration.RegistrationMeasureLeastSquares"));
            table.put(new ParameterInt("num_passes", 1));

            String paramPrefix = "pass_1_";
            table.put(new ParameterBoolean(paramPrefix + "do_subsample", true));
            table.put(new ParameterInt(paramPrefix + "bspline_degree", 1));
            table.put(new ParameterInt(paramPrefix + "num_control_points", 8));
            table.put(new ParameterFloat(paramPrefix + "gradient_descent_minimize_step_size", 1.0f));
            table.put(new ParameterInt(paramPrefix + "gradient_descent_minimize_max_steps", 10));
            table.put(new ParameterFloat(paramPrefix + "convergence_limit", 0.1f));
            table.put(new ParameterInt(paramPrefix + "max_iterations", 10));

            final Parameter numPassesParam = table.getParameter("num_passes");
            paramPrefix = "pass_2_";
            tempParam = new ParameterBoolean("do_subsample", true);
            tempParam.setParentCondition(numPassesParam, "2");
            table.put(tempParam);
            tempParam = new ParameterInt(paramPrefix + "bspline_degree", 2);
            tempParam.setParentCondition(numPassesParam, "2");
            table.put(tempParam);
            tempParam = new ParameterInt(paramPrefix + "num_control_points", 16);
            tempParam.setParentCondition(numPassesParam, "2");
            table.put(tempParam);
            tempParam = new ParameterFloat(paramPrefix + "gradient_descent_minimize_step_size", 0.5f);
            tempParam.setParentCondition(numPassesParam, "2");
            table.put(tempParam);
            tempParam = new ParameterInt(paramPrefix + "gradient_descent_minimize_max_steps", 10);
            tempParam.setParentCondition(numPassesParam, "2");
            table.put(tempParam);
            tempParam = new ParameterFloat(paramPrefix + "convergence_limit", 0.01f);
            tempParam.setParentCondition(numPassesParam, "2");
            table.put(tempParam);
            tempParam = new ParameterInt(paramPrefix + "max_iterations", 10);
            tempParam.setParentCondition(numPassesParam, "2");
            table.put(tempParam);

            table.put(new ParameterBoolean("do_create_deformation_image", false));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    /**
     * Returns a table listing the output parameters of this algorithm (usually just labels used to obtain output image
     * names later).
     * 
     * @return A parameter table listing the outputs of this algorithm.
     */
    public ParameterTable createOutputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE));
            table.put(new ParameterImage("deformation_image"));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    /**
     * Returns the name of an image output by this algorithm, the image returned depends on the parameter label given
     * (which can be used to retrieve the image object from the image registry).
     * 
     * @param imageParamName The output image parameter label for which to get the image name.
     * @return The image name of the requested output image parameter label.
     */
    public String getOutputImageName(final String imageParamName) {
        if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE)) {
            if (m_kImageReg != null) {
                return m_kImageReg.getImageName();
            }
        } else if (imageParamName.equals("deformation_image")) {
            if (m_kImageDef != null) {
                return m_kImageDef.getImageName();
            }
        }

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;
    }

    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
    }

    // ~ Inner Classes
    // --------------------------------------------------------------------------------------------------

    /**
     * Private class which holds dialog controls associated with the values in the AlgorithmRegBSpline.Options class for
     * a "pass" of registration.
     */
    private static class Controls {

        /** DOCUMENT ME! */
        private static final int[] ms_aiBSplineDegreeOptions = {1, 2, 3, 4};

        /** DOCUMENT ME! */
        public final JCheckBox kCheckSubsample = new JCheckBox();

        /** DOCUMENT ME! */
        public final JComboBox kComboBoxBSplineDegree = new JComboBox();

        /** DOCUMENT ME! */
        public final JTextField kTextBSplineNumControlPoints = new JTextField();

        /** DOCUMENT ME! */
        public final JTextField kTextConvergenceLimit = new JTextField();

        /** DOCUMENT ME! */
        public final JTextField kTextGradientDescentMinimizeMaxSteps = new JTextField();

        /** DOCUMENT ME! */
        public final JTextField kTextGradientDescentMinimizeStepSize = new JTextField();

        /** DOCUMENT ME! */
        public final JTextField kTextMaxIterations = new JTextField();

        /**
         * Constructor which creates the controls.
         * 
         * @param kDialog JDialogBase Dialog class from which the controls inherit certain properties (e.g., font).
         */
        public Controls(final JDialogBase kDialog) {

            // combo box to select degree+1 for B-Spline
            kComboBoxBSplineDegree.setFont(MipavUtil.font12);
            kComboBoxBSplineDegree.setBackground(Color.white);

            for (final int element : Controls.ms_aiBSplineDegreeOptions) {
                kComboBoxBSplineDegree.addItem(Integer.toString(element));
            }

            // text box to enter number of B-Spline control points
            kTextBSplineNumControlPoints.setColumns(10);
            kTextBSplineNumControlPoints.setFont(kDialog.serif12);

            // text box to enter the gradient descent minimize step size
            kTextGradientDescentMinimizeStepSize.setColumns(10);
            kTextGradientDescentMinimizeStepSize.setFont(kDialog.serif12);

            // text box to enter the gradient descent minimize maximum number of steps
            kTextGradientDescentMinimizeMaxSteps.setColumns(10);
            kTextGradientDescentMinimizeMaxSteps.setFont(kDialog.serif12);

            // text box for convergence limit
            kTextConvergenceLimit.setColumns(10);
            kTextConvergenceLimit.setFont(kDialog.serif12);

            // text box for maximum number of iterations
            kTextMaxIterations.setColumns(10);
            kTextMaxIterations.setFont(kDialog.serif12);

            // check box for subsample image
            kCheckSubsample.setFont(kDialog.serif12);
            kCheckSubsample.setForeground(Color.black);
        }

        /**
         * Extract the values from the specified set of controls and store them into the options structure used by the
         * algorithm.
         * 
         * @param kOptions AlgorithmRegBSpline.Options Structure for storing the values extracted from those controls.
         * @param aiExtentsReg int[] Dimensions of the reference image to be used for registration which is used to
         *            compute certain limits on the input values.
         * 
         * @return boolean True if the input values in the controls are acceptable.
         */
        public boolean getValues(final AlgorithmRegBSpline.Options kOptions, final int[] aiExtentsReg) {

            // Get the dimensions of the target image.
            int iExtentMin, iExtentMax;
            iExtentMin = aiExtentsReg[0];
            iExtentMax = aiExtentsReg[0];

            for (int iDim = 1; iDim < aiExtentsReg.length; iDim++) {

                if (aiExtentsReg[iDim] < iExtentMin) {
                    iExtentMin = aiExtentsReg[iDim];
                } else if (aiExtentsReg[iDim] > iExtentMax) {
                    iExtentMax = aiExtentsReg[iDim];
                }
            }

            // BSpline degree
            kOptions.iBSplineDegree = Controls.ms_aiBSplineDegreeOptions[kComboBoxBSplineDegree.getSelectedIndex()];

            String tmpStr;

            // Number of control points.
            final int iNumControlPointsMin = BSplineBasisf.GetMinNumControlPoints(kOptions.iBSplineDegree);
            final int iNumControlPointsMax = iExtentMin / 2;
            tmpStr = kTextBSplineNumControlPoints.getText();

            if (JDialogBase.testParameter(tmpStr, iNumControlPointsMin, iNumControlPointsMax)) {
                kOptions.iBSplineNumControlPoints = (Integer.valueOf(tmpStr).intValue());
            } else {
                kTextBSplineNumControlPoints.requestFocus();
                kTextBSplineNumControlPoints.selectAll();

                return false;
            }

            // Gradient descent minimize step size.
            final float fStepSizeMin = 0.1f;
            final float fStepSizeMax = 5.0f;
            tmpStr = kTextGradientDescentMinimizeStepSize.getText();

            if (JDialogBase.testParameter(tmpStr, fStepSizeMin, fStepSizeMax)) {
                kOptions.fGradientDescentMinimizeStepSize = (Float.valueOf(tmpStr).floatValue() / iExtentMax);
            } else {
                kTextGradientDescentMinimizeStepSize.requestFocus();
                kTextGradientDescentMinimizeStepSize.selectAll();

                return false;
            }

            // Gradient descent minimize maximum number of steps
            tmpStr = kTextGradientDescentMinimizeMaxSteps.getText();

            if (JDialogBase.testParameter(tmpStr, 1, 100)) {
                kOptions.iGradientDescentMinimizeMaxSteps = (Integer.valueOf(tmpStr).intValue());
            } else {
                kTextGradientDescentMinimizeMaxSteps.requestFocus();
                kTextGradientDescentMinimizeMaxSteps.selectAll();

                return false;
            }

            // Iteration convergence limit.
            tmpStr = kTextConvergenceLimit.getText();

            if (JDialogBase.testParameter(tmpStr, 0.001, 0.5)) {
                kOptions.fConvergenceLimit = (Float.valueOf(tmpStr).floatValue());
            } else {
                kTextConvergenceLimit.requestFocus();
                kTextConvergenceLimit.selectAll();

                return false;
            }

            // Max iterations.
            tmpStr = kTextMaxIterations.getText();

            if (JDialogBase.testParameter(tmpStr, 1, 100)) {
                kOptions.iMaxIterations = (Integer.valueOf(tmpStr).intValue());
            } else {
                kTextMaxIterations.requestFocus();
                kTextMaxIterations.selectAll();

                return false;
            }

            // Subsample image
            kOptions.bSubsample = kCheckSubsample.isSelected();

            return true;
        }

        /**
         * Set the state of the controls based on the specified values.
         * 
         * @param kOptions Options Structure which contains the values to use to set the state of the controls.
         */
        public void setValues(final AlgorithmRegBSpline.Options kOptions) {

            kComboBoxBSplineDegree.setSelectedIndex(0);

            for (int i = 0; i < Controls.ms_aiBSplineDegreeOptions.length; i++) {

                if (kOptions.iBSplineDegree == Controls.ms_aiBSplineDegreeOptions[i]) {
                    kComboBoxBSplineDegree.setSelectedIndex(i);

                    break;
                }
            }

            kTextBSplineNumControlPoints.setText(Integer.toString(kOptions.iBSplineNumControlPoints));
            kTextGradientDescentMinimizeStepSize.setText(Float.toString(kOptions.fGradientDescentMinimizeStepSize));
            kTextGradientDescentMinimizeMaxSteps.setText(Integer.toString(kOptions.iGradientDescentMinimizeMaxSteps));
            kTextConvergenceLimit.setText(Float.toString(kOptions.fConvergenceLimit));
            kTextMaxIterations.setText(Integer.toString(kOptions.iMaxIterations));
            kCheckSubsample.setSelected(kOptions.bSubsample);
        }

        /**
         * Makes the controls visible or invisible.
         * 
         * @param bEnable boolean True to make the controls visible.
         */
        public void setVisible(final boolean bEnable) {

            kComboBoxBSplineDegree.setVisible(bEnable);
            kTextBSplineNumControlPoints.setVisible(bEnable);
            kTextGradientDescentMinimizeStepSize.setVisible(bEnable);
            kTextGradientDescentMinimizeMaxSteps.setVisible(bEnable);
            kTextConvergenceLimit.setVisible(bEnable);
            kTextMaxIterations.setVisible(bEnable);
            kCheckSubsample.setVisible(bEnable);
        }
    }
}
