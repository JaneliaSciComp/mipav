package gov.nih.mipav.view.renderer;


import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.J3D.ViewJFrameVolumeView;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.border.*;


/**
 * Dialog to ask user to resample the images to power of 2 before volume rendering.
 * 
 * @author Ruida Cheng
 */
public class JDialogVolViewResample extends JDialogBase {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1966951185588087479L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Lookup table of the color imageA, B. */
    protected ModelRGB RGBTA = null, RGBTB = null;

    /** Original dimensions extents value array. */
    private int[] extents;

    /** Original X, Y, Z dimension extents values. */
    private JTextField extXInput, extYInput, extZInput;

    /** Power of 2 X, Y, Z dimension extents values. */
    private JTextField extXOutput, extYOutput, extZOutput;

    /** Flag to force the padding of blank images to the end of images. */
    private boolean forcePadding = false;

    /** Boolean flag to do resample images. */
    private boolean forceResample = false;

    /** Model images A and B. */
    private ModelImage imageA, imageB;

    /** The non-cloned image which we want to load into the renderer. */
    private ModelImage imageAOriginal;

    /** The left panel renderer mode. */
    private int leftPanelRenderMode = -1;

    /** Lookup table of the imageA, B. */
    private ModelLUT LUTa = null, LUTb = null;

    /** Resample filter type, default: Trilinear Interpoloation. */
    private int m_iFilter = 0;

    /** Resample filter selection box. */
    private JComboBox m_kFilterType;

    /** Number of available dimension. */
    private final int nDim;

    /** Resample resolutioin corresponding to Power of 2. */
    private float[] newRes = new float[3];

    /**
     * Pad button is used for inserting blank images to the end of the image. This approach creates the power of 2
     * image. Pad Button is only enabled when the power of 2 value is greater than the slices number.
     */
    private JButton padButton;

    /** Radio button of the brainsurface flattener render mode in the right panel. */
    private JRadioButton radioBrainSurfaceFlattenerR;

    /** Radio button of the fly througth mode in the left panel. */
    private JRadioButton radioFlythruL;

    /** Radio button of the surface view mode in the left panel. */
    private JRadioButton radioSurfaceView;

    /** Radio button of the none mode in the right panel. */
    private JRadioButton radioNoneR;

    /** Radio button of the raycast render mode in the right panel. */
    private JRadioButton radioRaycastR;

    /** Radio button of the shearwarp render mode in the right panel. */
    private JRadioButton radioShearwarpR;

    /** Radio button of the surface render mode int the left panel. */
    private JRadioButton radioSurfaceL;

    /** Radio button of the surface render mode in the right panel. */
    private JRadioButton radioSurfaceR;

    /** Original resolutioin arrray. */
    private float[] res;

    /** Temp Model image. */
    private ModelImage resampledImage = null;

    /** The right panel renderer mode. */
    private int rightPanelRenderMode = -1;

    /** An image with areas designated as special (vessels, etc). */
    private ModelImage segmentationImage;

    /** Reference to ViewJFrameSurfaceRenderer. */
    private ViewJFrameVolumeView sr;

    /**
     * The action command to send to the volume renderer after we start it (can be used to open a tab). Used by RFAST to
     * open up the RFA simulation immediately.
     */
    private String startupCommand = null;

    /** Resampled dimension value in Power of 2. */
    private int[] volExtents = new int[3];

    /** Volume size X*Y*Z. */
    private int volSize = 1;

    /** Type of ViewJFrameVolumeView object (using WildMagic or not) */
    private final String m_kVolViewType;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates the dialog, using the input parameters to place it on the screen.
     * 
     * @param _imageA Model image A.
     * @param _imageB Model image B.
     */
    public JDialogVolViewResample(final ModelImage _imageA, final ModelImage _imageB, final String kCommand) {

        super(ViewUserInterface.getReference().getMainFrame(), false);

        m_kVolViewType = kCommand;
        imageAOriginal = _imageA;
        imageA = (ModelImage) (_imageA.clone());

        if (imageA.isColorImage()) {
            RGBTA = (ModelRGB) (_imageA.getParentFrame().getRGBTA().clone());

            if (RGBTA == null) {
                final int[] RGBExtents = new int[2];
                RGBExtents[0] = 4;
                RGBExtents[1] = 256;
                RGBTA = new ModelRGB(RGBExtents);
            }
        } else {
            LUTa = (ModelLUT) (_imageA.getParentFrame().getLUTa().clone());

            if (LUTa == null) {
                LUTa = initLUT(imageA);
            }
        }

        if (_imageB != null) {
            imageB = (ModelImage) (_imageB.clone());

            if (imageB.isColorImage()) {
                ModelRGB modelRGB = _imageB.getParentFrame().getRGBTB();

                if (modelRGB == null) {
                    modelRGB = new ModelRGB(_imageB.getExtents());
                } else {
                    RGBTB = (ModelRGB) modelRGB.clone();
                }

                if (RGBTB == null) {
                    final int[] RGBExtents = new int[2];
                    RGBExtents[0] = 4;
                    RGBExtents[1] = 256;
                    RGBTB = new ModelRGB(RGBExtents);
                }
            } else {
                final ModelLUT modelLUT = _imageB.getParentFrame().getLUTb();

                if (modelLUT == null) {
                    LUTb = new ModelLUT(ModelLUT.GRAY, 256, _imageB.getExtents());
                } else {
                    LUTb = (ModelLUT) modelLUT.clone();

                    if (LUTa == null) {
                        LUTb = initLUT(imageB);
                    }
                }
            }
        }

        extents = imageA.getExtents();
        res = imageA.getFileInfo(0).getResolutions();
        nDim = extents.length;

        if (nDim != 3) {
            MipavUtil.displayError("The volume renderer only supports 3D image volumes.");

            return;
        }

        // Checking to see if the image has all dimensions that are a power of 2.
        for (int i = 0; i < extents.length; i++) {
            volExtents[i] = MipavMath.dimPowerOfTwo(extents[i]);
            volSize *= volExtents[i];
            newRes[i] = (res[i] * extents[i]) / volExtents[i];
        }

        if (nDim >= 3) {

            if ( (extents[0] == volExtents[0]) && (extents[1] == volExtents[1]) && (extents[2] == volExtents[2])) {
                forceResample = false;
            } else {
                forceResample = true;
            }
        }

        try {
            init();

            // if do not need resample, set the resample button text to OK.
            if ( !forceResample) {
                OKButton.setText("OK");
            }
        } catch (final NoClassDefFoundError error) {
            Preferences.debug("Unable to load volume renderer.  Missing Java3D class. " + error.getMessage() + "\n");
            MipavUtil.displayError("Unable to load volume renderer.  Missing Java3D class.");

            return;
        }

        if (forceResample && (volExtents[2] > extents[2])) {
            padButton.setEnabled(true);
        }

    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * On "OK", sets the name variable to the text entered. On "Cancel" disposes of this dialog and sets cancel flag.
     * 
     * @param event Event that triggered this method.
     */
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equals("Resample")) {

            volExtents[0] = Integer.parseInt(extXOutput.getText());

            if ( !MipavMath.isPowerOfTwo(volExtents[0])) {
                MipavUtil.displayInfo("Reample to Power of 2.");
                volExtents[0] = MipavMath.dimPowerOfTwo(volExtents[0]);
            }

            if (radioSurfaceView.isSelected()) {
                volExtents[0] = 16;
            }

            newRes[0] = (extents[0] * res[0]) / volExtents[0];

            volExtents[1] = Integer.parseInt(extYOutput.getText());

            if ( !MipavMath.isPowerOfTwo(volExtents[1])) {
                MipavUtil.displayInfo("Reample to Power of 2.");
                volExtents[1] = MipavMath.dimPowerOfTwo(volExtents[1]);
            }

            if (radioSurfaceView.isSelected()) {
                volExtents[1] = 16;
            }

            newRes[1] = (extents[1] * res[1]) / volExtents[1];

            if (nDim >= 3) {
                volExtents[2] = Integer.parseInt(extZOutput.getText());

                if ( !MipavMath.isPowerOfTwo(volExtents[2])) {
                    MipavUtil.displayInfo("Reample to Power of 2.");
                    volExtents[2] = MipavMath.dimPowerOfTwo(volExtents[2]);
                }
                if (radioSurfaceView.isSelected()) {
                    volExtents[2] = 16;
                }
                newRes[2] = (extents[2] * res[2]) / volExtents[2];
            }

            if (nDim >= 3) {

                if ( (extents[0] == volExtents[0]) && (extents[1] == volExtents[1]) && (extents[2] == volExtents[2])) {
                    forceResample = false;
                } else {
                    forceResample = true;
                }
            } else {

                if ( (extents[0] == volExtents[0]) && (extents[1] == volExtents[1])) {
                    forceResample = false;
                } else {
                    forceResample = true;
                }
            }

            setVisible(false);
            exec();
        } else if (command.equals("Pad")) {
            volExtents[0] = MipavMath.dimPowerOfTwo(Integer.parseInt(extXOutput.getText()));
            newRes[0] = (extents[0] * res[0]) / volExtents[0];
            volExtents[1] = MipavMath.dimPowerOfTwo(Integer.parseInt(extYOutput.getText()));
            newRes[1] = (extents[1] * res[1]) / volExtents[1];

            if (nDim >= 3) {
                volExtents[2] = MipavMath.dimPowerOfTwo(Integer.parseInt(extZOutput.getText()));
                newRes[2] = (extents[2] * res[2]) / volExtents[2];
            }

            if (nDim >= 3) {

                if ( (extents[0] == volExtents[0]) && (extents[1] == volExtents[1]) && (extents[2] == volExtents[2])) {
                    forceResample = false;
                    forcePadding = false;
                } else {

                    if (extents[2] < volExtents[2]) {
                        forceResample = true;
                        forcePadding = true;
                    }
                }
            }

            setVisible(false);
            exec();
        } else if (command.equals("xChanged")) {
            int x = Integer.parseInt(extXOutput.getText());

            if ( !MipavMath.isPowerOfTwo(x)) {
                MipavUtil.displayInfo("Reample to Power of 2.");
                x = MipavMath.dimPowerOfTwo(x);
            }

            extXOutput.setText(Integer.toString(x));
        } else if (command.equals("yChanged")) {
            int y = Integer.parseInt(extYOutput.getText());

            if ( !MipavMath.isPowerOfTwo(y)) {
                MipavUtil.displayInfo("Reample to Power of 2.");
                y = MipavMath.dimPowerOfTwo(y);
            }

            extYOutput.setText(Integer.toString(y));
        } else if (command.equals("zChanged")) {
            int z = Integer.parseInt(extZOutput.getText());

            if ( !MipavMath.isPowerOfTwo(z)) {
                MipavUtil.displayInfo("Reample to Power of 2.");
                z = MipavMath.dimPowerOfTwo(z);
            }

            extZOutput.setText(Integer.toString(z));
        } else if (command.equals("FilterChanged")) {
            m_iFilter = m_kFilterType.getSelectedIndex();

            /*
             * Bilinear is not allowed for volumes, and is not in the list, if a type that is definted after bilinear is
             * selected, increment the m_iFilter value so it matches the AlgotithmTransform type definition for that
             * filter:
             */
            if (m_iFilter > 0) {
                m_iFilter++;
            }
        } else if (command.equals("Cancel")) {
            disposeLocal();
            dispose();
        } else {
            super.actionPerformed(event);
        }
    } 

    /**
     * Builds the Cancel button. Sets it internally as well return the just-built button.
     * 
     * @return Return the noll resample button.
     */
    public JButton buildNotResampleButton() {
        cancelButton = new JButton("Cancel");
        cancelButton.addActionListener(this);
        cancelButton.setMinimumSize(MipavUtil.defaultButtonSize);
        cancelButton.setPreferredSize(MipavUtil.defaultButtonSize);
        cancelButton.setFont(serif12B);

        return cancelButton;
    }

    /**
     * Dispose memory.
     */
    public void disposeLocal() {

        if (resampledImage != null) {
            resampledImage.disposeLocal();
            resampledImage = null;
        }

        if (sr != null) {
            sr.dispose();
            sr = null;
        }

        if (imageA != null) {
            imageA.disposeLocal();
        }

        imageA = null;

        if (imageB != null) {
            imageB.disposeLocal();
        }

        imageB = null;

        if (imageAOriginal != null) { // imageAOriginal.removeImageDisplayListener(userInterface);

            // imageAOriginal.disposeLocal();
        }

        imageAOriginal = null;

        extents = null;
        res = null;
        volExtents = null;
        newRes = null;

        super.dispose();
    }

    /**
     * Do resampling.
     */
    public void exec() {

        try {
            if (m_kVolViewType.equals("VolTriplanar")) {
                if (m_kVolViewType.equals("VolTriplanar")) {
                    sr = new ViewJFrameVolumeView(imageA, LUTa, RGBTA, imageB, LUTb, RGBTB, leftPanelRenderMode,
                            rightPanelRenderMode, this);
                    sr.setImageOriginal(imageAOriginal);

                    if (forcePadding) {
                        sr.doPadding(extents, volExtents);
                    } else if (forceResample) {
                        sr.doResample(volExtents, newRes, forceResample, nDim, m_iFilter);
                    }

                    if (rightPanelRenderMode == ViewJFrameVolumeView.SHEARWARP) {
                        sr.calcShearWarpImage(imageA, imageB);
                    }

                    sr.constructRenderers();

                    // can't do this before sr.initialize() since it uses the plane renderer list, which is setup there
                    sr.addAttachedSurfaces();

                    if (sr.getProbeDialog() != null) {

                        // need to update the rfa target labels in case there were attached surfaces that we should show
                        // info
                        // about
                        sr.getProbeDialog().updateTargetList();
                    }

                    if (forceResample) {

                        if (imageA != null) {
                            imageA.disposeLocal();
                            imageA = null;
                        }

                        if (imageB != null) {
                            imageB.disposeLocal();
                            imageB = null;
                        }
                    }

                    if (startupCommand != null) {
                        sr.actionPerformed(new ActionEvent(this, 0, startupCommand));
                    }

                    if (segmentationImage != null) {
                        sr.setSegmentationImage(segmentationImage);
                    }
                }

            }
        } catch (final NoClassDefFoundError notAvailableError) {
            Preferences.debug("ViewJFrameSurfaceRenderer cannot be called; encountered "
                    + "a NoClassDefFoundError.  \nIt is likely that java3D is "
                    + "not available on this system.  The error is: \n" + notAvailableError.getLocalizedMessage());
            MipavUtil.displayError("The surface renderer requires java 3D and it cannot " + "be found.");
        } catch (final OutOfMemoryError notEnoughError) {
            Preferences.debug("ViewJFrameSurfaceRenderer cannot be called as there was "
                    + "not enough memory allocated.  \n" + "The error is: \n" + notEnoughError.getLocalizedMessage());
            MipavUtil.displayError("The surface renderer requires more memory " + "than is currently available;\n"
                    + "See the Memory Allocation menu");
        }

    }

    /**
     * Build the resample dialog.
     */
    public void init() {
        setTitle("Resample Dialog");

        final Box mainBox = new Box(BoxLayout.Y_AXIS);

        final JPanel endPanel = new JPanel();

        endPanel.setLayout(new BorderLayout());
        endPanel.add(new JLabel(" Selecting _Resample_ will resample the image's extents to a Power of 2."),
                BorderLayout.NORTH);
        endPanel.add(new JLabel(" Selecting _Do not resample_ will disable the volume render button of "),
                BorderLayout.CENTER);
        endPanel.add(new JLabel(" Surface Renderer since the image's extents are not a Power of 2."),
                BorderLayout.SOUTH);
        mainBox.add(endPanel);

        final Box contentBox = new Box(BoxLayout.X_AXIS);
        final JPanel leftPanel = new JPanel();
        final JPanel rightPanel = new JPanel();

        // make border
        leftPanel.setBorder(buildTitledBorder("Original Extents"));
        contentBox.add(leftPanel);

        // set layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();

        leftPanel.setLayout(gbl);

        // extent X
        leftPanel.add(Box.createHorizontalStrut(10));

        final JLabel extXLabel = new JLabel("extent X:");

        extXLabel.setFont(serif12);
        extXLabel.setForeground(Color.black);
        extXLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(extXLabel, gbc);
        leftPanel.add(extXLabel);
        leftPanel.add(Box.createHorizontalStrut(10));

        extXInput = new JTextField(Integer.toString(extents[0]), 3);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(extXInput, gbc);
        extXInput.setEnabled(false);
        leftPanel.add(extXInput);

        // extent Y
        leftPanel.add(Box.createHorizontalStrut(10));

        final JLabel extYLabel = new JLabel("extent Y:");

        extYLabel.setFont(serif12);
        extYLabel.setForeground(Color.black);
        extYLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(extYLabel, gbc);
        leftPanel.add(extYLabel);
        leftPanel.add(Box.createHorizontalStrut(10));

        extYInput = new JTextField(Integer.toString(extents[1]), 3);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(extYInput, gbc);
        extYInput.setEnabled(false);
        leftPanel.add(extYInput);

        if (nDim >= 3) {

            // extent Z
            leftPanel.add(Box.createHorizontalStrut(10));

            final JLabel extZLabel = new JLabel("extent Z:");

            extZLabel.setFont(serif12);
            extZLabel.setForeground(Color.black);
            extZLabel.setRequestFocusEnabled(false);
            gbc.gridwidth = 2;
            gbl.setConstraints(extZLabel, gbc);
            leftPanel.add(extZLabel);
            leftPanel.add(Box.createHorizontalStrut(10));

            extZInput = new JTextField(Integer.toString(extents[2]), 3);
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(extZInput, gbc);
            extZInput.setEnabled(false);
            leftPanel.add(extZInput);
        }

        // make border
        rightPanel.setBorder(buildTitledBorder("Expected Extents"));
        contentBox.add(rightPanel);

        // set layout
        gbl = new GridBagLayout();
        gbc = new GridBagConstraints();
        rightPanel.setLayout(gbl);

        // extent X expected
        rightPanel.add(Box.createHorizontalStrut(10));

        final JLabel extXNewLabel = new JLabel("extent X:");

        extXNewLabel.setFont(serif12);
        extXNewLabel.setForeground(Color.black);
        extXNewLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(extXNewLabel, gbc);
        rightPanel.add(extXNewLabel);
        rightPanel.add(Box.createHorizontalStrut(10));

        extXOutput = new JTextField(Integer.toString(volExtents[0]), 3);
        extXOutput.addActionListener(this);
        extXOutput.setActionCommand("xChanged");
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(extXOutput, gbc);
        MipavUtil.makeNumericsOnly(extXOutput, false);
        rightPanel.add(extXOutput);

        // extent Y expected
        rightPanel.add(Box.createHorizontalStrut(10));

        final JLabel extYNewLabel = new JLabel("extent Y:");

        extYNewLabel.setFont(serif12);
        extYNewLabel.setForeground(Color.black);
        extYNewLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(extYNewLabel, gbc);
        rightPanel.add(extYNewLabel);
        rightPanel.add(Box.createHorizontalStrut(10));

        extYOutput = new JTextField(Integer.toString(volExtents[1]), 3);
        extYOutput.addActionListener(this);
        extYOutput.setActionCommand("yChanged");
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(extYOutput, gbc);
        MipavUtil.makeNumericsOnly(extYOutput, false);
        rightPanel.add(extYOutput);

        if (nDim >= 3) {

            // extent Z expected
            rightPanel.add(Box.createHorizontalStrut(10));

            final JLabel extZNewLabel = new JLabel("extent Z:");

            extZNewLabel.setFont(serif12);
            extZNewLabel.setForeground(Color.black);
            extZNewLabel.setRequestFocusEnabled(false);
            gbc.gridwidth = 2;
            gbl.setConstraints(extZNewLabel, gbc);
            rightPanel.add(extZNewLabel);
            rightPanel.add(Box.createHorizontalStrut(10));

            extZOutput = new JTextField(Integer.toString(volExtents[2]), 3);
            extZOutput.addActionListener(this);
            extZOutput.setActionCommand("zChanged");
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(extZOutput, gbc);
            MipavUtil.makeNumericsOnly(extZOutput, false);
            rightPanel.add(extZOutput);
        }

        mainBox.add(contentBox);

        /* Filter selection: */
        final JPanel filterPanel = new JPanel();
        filterPanel.setBorder(buildTitledBorder("Select Resampling Filter"));
        m_kFilterType = new JComboBox();
        m_kFilterType.addItem(new String("Trilinear Interpolation"));
        m_kFilterType.addItem(new String("Nearest Neighbor"));
        m_kFilterType.addItem(new String("Cubic BSpline"));
        m_kFilterType.addItem(new String("Quadratic BSpline"));
        m_kFilterType.addItem(new String("Cubic lagrangian"));
        m_kFilterType.addItem(new String("Quintic lagrangian"));
        m_kFilterType.addItem(new String("Heptic lagrangian"));
        m_kFilterType.addItem(new String("Windowed sinc"));
        m_kFilterType.setSelectedIndex(0);
        m_kFilterType.addActionListener(this);
        m_kFilterType.setActionCommand("FilterChanged");
        filterPanel.add(m_kFilterType);
        mainBox.add(filterPanel);

        final Box radioBox = new Box(BoxLayout.X_AXIS);
        final JPanel radioButtonPanelLeft = new JPanel();

        gbl = new GridBagLayout();
        gbc = new GridBagConstraints();
        gbc.gridwidth = 2;
        radioButtonPanelLeft.setBorder(MipavUtil.buildTitledBorder("Left Panel"));
        radioButtonPanelLeft.setLayout(gbl);

        final ButtonGroup group1 = new ButtonGroup();

        radioSurfaceL = new JRadioButton("Surface & 3D Texture Volume Renderer", false);
        radioSurfaceL.setFont(serif12);
        radioSurfaceL.addItemListener(this);
        group1.add(radioSurfaceL);

        final JLabel emptyLabelUp = new JLabel(" ");
        final JLabel emptyLabelDown = new JLabel(" ");

        radioFlythruL = new JRadioButton("Flythru Renderer", false);
        radioFlythruL.setFont(serif12);
        radioFlythruL.addItemListener(this);
        group1.add(radioFlythruL);

        radioSurfaceView = new JRadioButton("Surface View", false);
        radioSurfaceView.setFont(serif12);
        radioSurfaceView.addItemListener(this);
        group1.add(radioSurfaceView);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.WEST;
        radioButtonPanelLeft.add(radioSurfaceL, gbc);
        gbc.gridy++;
        radioButtonPanelLeft.add(emptyLabelUp, gbc);
        gbc.gridy++;
        radioButtonPanelLeft.add(radioFlythruL, gbc);
        gbc.gridy++;
        radioButtonPanelLeft.add(emptyLabelDown, gbc);
        gbc.gridy++;
        radioButtonPanelLeft.add(radioSurfaceView, gbc);

        gbl = new GridBagLayout();
        gbc = new GridBagConstraints();
        gbc.gridwidth = 2;

        final JPanel radioButtonPanelRight = new JPanel();

        radioButtonPanelRight.setBorder(MipavUtil.buildTitledBorder("Right Panel"));
        radioButtonPanelRight.setLayout(gbl);

        final ButtonGroup group2 = new ButtonGroup();

        radioSurfaceR = new JRadioButton("Surface & 3D Texture Volume Renderer", false);
        radioSurfaceR.setFont(serif12);
        radioSurfaceR.addItemListener(this);
        group2.add(radioSurfaceR);

        radioRaycastR = new JRadioButton("Raycast Volume Renderer", false);
        radioRaycastR.setFont(serif12);
        radioRaycastR.addItemListener(this);
        group2.add(radioRaycastR);

        radioShearwarpR = new JRadioButton("Shearwarp Volume Renderer", false);
        radioShearwarpR.setFont(serif12);
        group2.add(radioShearwarpR);
        radioShearwarpR.addItemListener(this);
        radioShearwarpR.setEnabled(false);

        radioBrainSurfaceFlattenerR = new JRadioButton("Brain Surface Flattener Renderer", false);
        radioBrainSurfaceFlattenerR.setFont(serif12);
        group2.add(radioBrainSurfaceFlattenerR);
        radioBrainSurfaceFlattenerR.addItemListener(this);
        radioBrainSurfaceFlattenerR.setEnabled(false);

        radioNoneR = new JRadioButton("None", false);
        radioNoneR.setFont(serif12);
        radioNoneR.addItemListener(this);
        group2.add(radioNoneR);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.WEST;
        radioButtonPanelRight.add(radioSurfaceR, gbc);
        gbc.gridy++;
        radioButtonPanelRight.add(radioRaycastR, gbc);
        gbc.gridy++;
        radioButtonPanelRight.add(radioShearwarpR, gbc);
        gbc.gridy++;
        radioButtonPanelRight.add(radioBrainSurfaceFlattenerR, gbc);
        gbc.gridy++;
        radioButtonPanelRight.add(radioNoneR, gbc);

        radioBox.add(radioButtonPanelLeft);
        radioBox.add(radioButtonPanelRight);
        radioBox.setBorder(MipavUtil.buildTitledBorder("Select Renderer"));

        mainBox.add(radioBox);

        final JPanel OKCancelPanel = new JPanel(new FlowLayout());

        OKButton = buildResampleButton();
        OKCancelPanel.add(OKButton);

        buildPadButton();
        OKCancelPanel.add(padButton);

        cancelButton = buildNotResampleButton();
        OKCancelPanel.add(cancelButton);

        mainBox.add(OKCancelPanel);
        radioNoneR.setSelected(true);
        radioSurfaceL.setSelected(true);

        mainDialogPanel.add(mainBox);
        getContentPane().add(mainDialogPanel);

        pack();
        setVisible(true);

        // so that hitting space or enter makes the dialog go
        // (focus used to go to the first text field, which the user doesn't change much)
        OKButton.requestFocus();
    }

    /**
     * Sets the flags for the checkboxes.
     * 
     * @param event event that triggered this function
     */
    public synchronized void itemStateChanged(final ItemEvent event) {

        if (radioSurfaceL.isSelected()) {
            leftPanelRenderMode = ViewJFrameVolumeView.SURFACE;

            if (radioSurfaceR != null) {
                radioSurfaceR.setSelected(false);
                radioSurfaceR.setEnabled(false);
                radioRaycastR.setEnabled(true);
                radioShearwarpR.setEnabled(true);
                radioBrainSurfaceFlattenerR.setEnabled(true);
            }
        } else if (radioFlythruL.isSelected()) {
            leftPanelRenderMode = ViewJFrameVolumeView.ENDOSCOPY;
            radioShearwarpR.setSelected(false);
            radioShearwarpR.setEnabled(false);
            radioBrainSurfaceFlattenerR.setSelected(false);
            radioBrainSurfaceFlattenerR.setEnabled(false);
            radioSurfaceR.setSelected(false);
            radioSurfaceR.setEnabled(false);
            radioRaycastR.setSelected(false);
            radioRaycastR.setEnabled(false);
            radioNoneR.setSelected(true);
            radioNoneR.setEnabled(false);

        } else if (radioSurfaceView.isSelected()) {
            leftPanelRenderMode = ViewJFrameVolumeView.SURFACEVIEW;

            radioShearwarpR.setSelected(false);
            radioShearwarpR.setEnabled(false);
            radioBrainSurfaceFlattenerR.setSelected(false);
            radioBrainSurfaceFlattenerR.setEnabled(false);
            radioSurfaceR.setSelected(false);
            radioSurfaceR.setEnabled(false);
            radioRaycastR.setSelected(false);
            radioRaycastR.setEnabled(false);
            radioNoneR.setSelected(true);
            radioNoneR.setEnabled(false);

        }
        if (radioSurfaceR.isSelected()) {
            rightPanelRenderMode = ViewJFrameVolumeView.SURFACE;
        } else if (radioRaycastR.isSelected()) {
            rightPanelRenderMode = ViewJFrameVolumeView.RAYCAST;
        } else if (radioShearwarpR.isSelected()) {
            rightPanelRenderMode = ViewJFrameVolumeView.SHEARWARP;
        } else if (radioBrainSurfaceFlattenerR.isSelected()) {
            rightPanelRenderMode = ViewJFrameVolumeView.BRAINSURFACE_FLATTENER;
        } else if (radioNoneR.isSelected()) {
            rightPanelRenderMode = ViewJFrameVolumeView.NONE;
        }

    }

    /**
     * Sets a command that should be sent to the renderer after it is started.
     * 
     * @param cmd the action command
     */
    public void sendActionOnStart(final String cmd) {
        startupCommand = cmd;
    }

    /**
     * Set the image which we can check to see if the probe is hitting anything important (such as vessels, etc).
     * 
     * @param img segmentation image
     */
    public void setSegmentationImage(final ModelImage img) {
        segmentationImage = img;
    }

    /**
     * Builds a titled border with the given title, an etched border, and the proper font and color.
     * 
     * @param title Title of the border
     * @param _border Return the border built
     * 
     * @return The titled border.
     */
    protected TitledBorder buildTitledBorder(final String title, final Border _border) {
        return new TitledBorder(_border, title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
    }

    /**
     * Clear up memory from gc.
     * 
     * @throws Throwable Call the dispose local to dispose memory.
     */
    protected void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
    }

    /**
     * Creates and initializes the LUT for an image.
     * 
     * @param img the image to create a LUT for
     * 
     * @return a LUT for the image <code>img</code> (null if a color image)
     * 
     * @throws OutOfMemoryError if enough memory cannot be allocated for this method
     */
    protected ModelLUT initLUT(final ModelImage img) throws OutOfMemoryError {
        ModelLUT newLUT = null;

        // only make a lut for non color images
        if (img.isColorImage() == false) {
            final int[] dimExtentsLUT = new int[2];

            dimExtentsLUT[0] = 4;
            dimExtentsLUT[1] = 256;

            newLUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);

            float min, max;

            if (img.getType() == ModelStorageBase.UBYTE) {
                min = 0;
                max = 255;
            } else if (img.getType() == ModelStorageBase.BYTE) {
                min = -128;
                max = 127;
            } else {
                min = (float) img.getMin();
                max = (float) img.getMax();
            }

            final float imgMin = (float) img.getMin();
            final float imgMax = (float) img.getMax();

            newLUT.resetTransferLine(min, imgMin, max, imgMax);
        }

        return newLUT;
    }

    /**
     * Builds the Pad button for padding the blank images.
     */
    private void buildPadButton() {
        padButton = new JButton("Pad");
        padButton.addActionListener(this);
        padButton.setMinimumSize(MipavUtil.defaultButtonSize);
        padButton.setPreferredSize(MipavUtil.defaultButtonSize);
        padButton.setFont(serif12B);
        padButton.setEnabled(false);
    }

    /**
     * Builds the OK button. Sets it internally as well return the just-built button.
     * 
     * @return Return the resample button built.
     */
    private JButton buildResampleButton() {
        OKButton = new JButton("Resample");
        OKButton.setActionCommand("Resample");
        OKButton.addActionListener(this);
        OKButton.setMinimumSize(MipavUtil.widenButtonSize);
        OKButton.setPreferredSize(MipavUtil.widenButtonSize);
        OKButton.setFont(serif12B);

        return OKButton;
    }
}
