package gov.nih.mipav.view.renderer.WildMagic;


import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;


/**
 * @author Alexandra
 *
 */
public class VolumeTriPlanarDialog extends JInterfaceBase {

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

    /** Lookup table of the imageA, B. */
    private ModelLUT LUTa = null, LUTb = null;

    /** Resample filter type, default: Trilinear Interpoloation. */
    private int m_iFilter = 0;

    /** Resample filter selection box. */
    private JComboBox m_kFilterType;

    /** Number of available dimension. */
    private int nDim;

    /** Resample resolution corresponding to Power of 2. */
    private float[] newRes = new float[3];

    /**
     * Pad button is used for inserting blank images to the end of the image. This approach creates the power of 2
     * image. Pad Button is only enabled when the power of 2 value is greater than the slices number.
     */
    private JButton padButton;

    /** Original resolution array. */
    private float[] res;

    /** Temp Model image. */
    private ModelImage resampledImage = null;

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
    private String m_kVolViewType;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates the dialog, using the input parameters to place it on the screen.
     * 
     * @param _imageA Model image A.
     * @param _imageB Model image B.
     * @param kCommand the re-sample command.
     */
    public VolumeTriPlanarDialog(ModelImage _imageA, ModelImage _imageB, String kCommand) {
        m_kVolViewType = kCommand;
        imageAOriginal = _imageA;
        imageA = (ModelImage) (_imageA.clone());

        if (imageA.isColorImage()) {
            RGBTA = (ModelRGB) (_imageA.getParentFrame().getRGBTA().clone());

            if (RGBTA == null) {
                int[] RGBExtents = new int[2];
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
                    int[] RGBExtents = new int[2];
                    RGBExtents[0] = 4;
                    RGBExtents[1] = 256;
                    RGBTB = new ModelRGB(RGBExtents);
                }
            } else {
                ModelLUT modelLUT = _imageB.getParentFrame().getLUTb();

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
            volExtents[i] = dimPowerOfTwo(extents[i]);
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
        } catch (NoClassDefFoundError error) {
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


    /* (non-Javadoc)
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Resample")) {

            volExtents[0] = Integer.parseInt(extXOutput.getText());

            if ( !isPowerOfTwo(volExtents[0])) {
                MipavUtil.displayInfo("Reample to Power of 2.");
                volExtents[0] = dimPowerOfTwo(volExtents[0]);
            }

            newRes[0] = (extents[0] * res[0]) / volExtents[0];

            volExtents[1] = Integer.parseInt(extYOutput.getText());

            if ( !isPowerOfTwo(volExtents[1])) {
                MipavUtil.displayInfo("Reample to Power of 2.");
                volExtents[1] = dimPowerOfTwo(volExtents[1]);
            }

            newRes[1] = (extents[1] * res[1]) / volExtents[1];

            if (nDim >= 3) {
                volExtents[2] = Integer.parseInt(extZOutput.getText());

                if ( !isPowerOfTwo(volExtents[2])) {
                    MipavUtil.displayInfo("Reample to Power of 2.");
                    volExtents[2] = dimPowerOfTwo(volExtents[2]);
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
            volExtents[0] = dimPowerOfTwo(Integer.parseInt(extXOutput.getText()));
            newRes[0] = (extents[0] * res[0]) / volExtents[0];
            volExtents[1] = dimPowerOfTwo(Integer.parseInt(extYOutput.getText()));
            newRes[1] = (extents[1] * res[1]) / volExtents[1];

            if (nDim >= 3) {
                volExtents[2] = dimPowerOfTwo(Integer.parseInt(extZOutput.getText()));
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

            if ( !isPowerOfTwo(x)) {
                MipavUtil.displayInfo("Reample to Power of 2.");
                x = dimPowerOfTwo(x);
            }

            extXOutput.setText(Integer.toString(x));
        } else if (command.equals("yChanged")) {
            int y = Integer.parseInt(extYOutput.getText());

            if ( !isPowerOfTwo(y)) {
                MipavUtil.displayInfo("Reample to Power of 2.");
                y = dimPowerOfTwo(y);
            }

            extYOutput.setText(Integer.toString(y));
        } else if (command.equals("zChanged")) {
            int z = Integer.parseInt(extZOutput.getText());

            if ( !isPowerOfTwo(z)) {
                MipavUtil.displayInfo("Reample to Power of 2.");
                z = dimPowerOfTwo(z);
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
        cancelButton.setFont(MipavUtil.font12B);

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
            if (m_kVolViewType.equals("WMVolTriplanar")) {
                if ( (imageA.getType() != ModelStorageBase.UBYTE) ||
                        (imageA.isColorImage() && (imageA.getType() != ModelStorageBase.ARGB)) )
                {
                    AlgorithmChangeType changeTypeAlgo = null;
                    if ( imageA.isColorImage() )
                    {
                        changeTypeAlgo = new AlgorithmChangeType(imageA, ModelStorageBase.ARGB, 
                                imageA.getMin(), imageA.getMax(), 
                                0, 255, false);
                    }
                    else
                    {
                        changeTypeAlgo = new AlgorithmChangeType(imageA, ModelStorageBase.UBYTE, 
                            imageA.getMin(), imageA.getMax(), 
                            0, 255, false);
                    }
                    changeTypeAlgo.setRunningInSeparateThread(false);
                    changeTypeAlgo.run();
                    changeTypeAlgo.finalize();
                    changeTypeAlgo = null;
                }
                if ( (imageB != null) && ((imageB.getType() != ModelStorageBase.UBYTE) ||
                        (imageB.isColorImage() && (imageB.getType() != ModelStorageBase.ARGB))) )
                {
                    AlgorithmChangeType changeTypeAlgo = null;
                    if ( imageB.isColorImage() )
                    {
                        changeTypeAlgo = new AlgorithmChangeType(imageB, ModelStorageBase.ARGB, 
                                imageB.getMin(), imageB.getMax(), 
                                0, 255, false);
                    }
                    else
                    {
                        changeTypeAlgo = new AlgorithmChangeType(imageB, ModelStorageBase.UBYTE, 
                                imageB.getMin(), imageB.getMax(), 
                            0, 255, false);
                    }
                    changeTypeAlgo.setRunningInSeparateThread(false);
                    changeTypeAlgo.run();

                    if (changeTypeAlgo.isCompleted() == false) {

                        // What to do
                        changeTypeAlgo.finalize();
                        changeTypeAlgo = null;
                    }
                }
/*
                String kExternalDirs = VolumeTriPlanarInterface.getExternalDirs();        
                ImageCatalog.SetActive( new ImageCatalog("Main", kExternalDirs) );      
                VertexProgramCatalog.SetActive(new VertexProgramCatalog("Main", kExternalDirs));       
                PixelProgramCatalog.SetActive(new PixelProgramCatalog("Main", kExternalDirs));
                CompiledProgramCatalog.SetActive(new CompiledProgramCatalog());
                VolumeImage kVolumeImageA = new VolumeImage(  imageA, LUTa, RGBTA, "A" );
                VolumeImageViewer.main(null, kVolumeImageA, true);
*/
                
                VolumeTriPlanarInterface kWM = new VolumeTriPlanarInterface(imageA, LUTa, RGBTA, imageB, LUTb, RGBTB);
                if (forcePadding) {
                    kWM.doPadding(extents, volExtents);
                } else if (forceResample) {
                    kWM.doResample(volExtents, newRes, forceResample, nDim, m_iFilter);
                }
                kWM.constructRenderers();

                // can't do this before kWM.initialize() since it uses the plane renderer list, which is setup there
                /*
                 * kWM.addAttachedSurfaces(); if (kWM.getProbeDialog() != null) {
                 *  // need to update the rfa target labels in case there were attached surfaces that we should show
                 * info // about kWM.getProbeDialog().updateTargetList(); }
                 */

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
                    kWM.actionPerformed(new ActionEvent(this, 0, startupCommand));
                }
            }

        } catch (NoClassDefFoundError notAvailableError) {
            Preferences.debug("ViewJFrameSurfaceRenderer cannot be called; encountered "
                    + "a NoClassDefFoundError.  \nIt is likely that JOGL is "
                    + "not available on this system.  The error is: \n" + notAvailableError.getLocalizedMessage());
            MipavUtil.displayError("The surface renderer requires JOGL and it cannot be found.");
        } catch (OutOfMemoryError notEnoughError) {
            Preferences.debug("ViewJFrameSurfaceRenderer cannot be called as there was "
                    + "not enough memory allocated.  \n" + "The error is: \n" + notEnoughError.getLocalizedMessage());
            MipavUtil.displayError("The surface renderer requires more memory " + "than is currently available;\n"
                    + "See the Memory Allocation menu");
        }

    }

    /**
     * Initializes the dialog user-interface.
     */
    public void init() {
        setTitle("Resample Dialog");

        Box mainBox = new Box(BoxLayout.Y_AXIS);

        JPanel endPanel = new JPanel();

        endPanel.setLayout(new BorderLayout());
        endPanel.add(new JLabel(" Selecting _Resample_ will resample the image's extents to a Power of 2."),
                BorderLayout.NORTH);
        endPanel.add(new JLabel(" Selecting _Do not resample_ will disable the volume render button of "),
                BorderLayout.CENTER);
        endPanel.add(new JLabel(" Surface Renderer since the image's extents are not a Power of 2."),
                BorderLayout.SOUTH);
        mainBox.add(endPanel);

        Box contentBox = new Box(BoxLayout.X_AXIS);
        JPanel leftPanel = new JPanel();
        JPanel rightPanel = new JPanel();

        // make border
        leftPanel.setBorder(buildTitledBorder("Original Extents"));
        contentBox.add(leftPanel);

        // set layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();

        leftPanel.setLayout(gbl);

        // extent X
        leftPanel.add(Box.createHorizontalStrut(10));

        JLabel extXLabel = new JLabel("extent X:");

        extXLabel.setFont(MipavUtil.font12);
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

        JLabel extYLabel = new JLabel("extent Y:");

        extYLabel.setFont(MipavUtil.font12);
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

            JLabel extZLabel = new JLabel("extent Z:");

            extZLabel.setFont(MipavUtil.font12);
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

        JLabel extXNewLabel = new JLabel("extent X:");

        extXNewLabel.setFont(MipavUtil.font12);
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

        JLabel extYNewLabel = new JLabel("extent Y:");

        extYNewLabel.setFont(MipavUtil.font12);
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

            JLabel extZNewLabel = new JLabel("extent Z:");

            extZNewLabel.setFont(MipavUtil.font12);
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
        JPanel filterPanel = new JPanel();
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

        gbl = new GridBagLayout();
        gbc = new GridBagConstraints();
        gbc.gridwidth = 2;

        JPanel OKCancelPanel = new JPanel(new FlowLayout());

        OKButton = buildResampleButton();
        OKCancelPanel.add(OKButton);

        buildPadButton();
        OKCancelPanel.add(padButton);

        cancelButton = buildNotResampleButton();
        OKCancelPanel.add(cancelButton);

        mainBox.add(OKCancelPanel);

        mainPanel = new JPanel();
        mainPanel.add(mainBox);
        getContentPane().add(mainPanel);

        pack();
        setVisible(true);

        // so that hitting space or enter makes the dialog go
        // (focus used to go to the first text field, which the user doesn't change much)
        OKButton.requestFocus();
    }

    /**
     * @param event
     */
    public synchronized void itemStateChanged(@SuppressWarnings("unused")
    ItemEvent event) {}

    /**
     * Sets a command that should be sent to the renderer after it is started.
     * 
     * @param cmd the action command
     */
    public void sendActionOnStart(String cmd) {
        startupCommand = cmd;
    }

    /**
     * Makes the dialog visible in center of screen.
     * 
     * @param status Flag indicating if the dialog should be visible.
     */
    public void setVisible(boolean status) {

        if ( (status == true) && (isVisible() == false)) {

            // move to center if not yet visible. note that if 'status' param is false
            // the dialog should not be centered, because centering it just before
            // it is made invisible will cause it to flash on screen briefly in the
            // center of the screen. its not noticable if the dialog is already
            // centered, but if the user has moved the dialog, it is annoying to see.
            MipavUtil.centerOnScreen(this);
        }

        super.setVisible(status);
    }

    /**
     * Builds a titled border with the given title, an etched border, and the proper font and color.
     * 
     * @param title Title of the border
     * @param _border Return the border built
     * 
     * @return The titled border.
     */
    protected TitledBorder buildTitledBorder(String title, Border _border) {
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
    protected ModelLUT initLUT(ModelImage img) throws OutOfMemoryError {
        ModelLUT newLUT = null;

        // only make a lut for non color images
        if (img.isColorImage() == false) {
            int[] dimExtentsLUT = new int[2];

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

            float imgMin = (float) img.getMin();
            float imgMax = (float) img.getMax();

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
        padButton.setFont(MipavUtil.font12B);
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
        OKButton.setFont(MipavUtil.font12B);

        return OKButton;
    }

    /**
     * Calculate the dimension value to power of 2.
     * 
     * @param dim dimension value.
     * 
     * @return value dimension value in power of 2
     */
    private int dimPowerOfTwo(int dim) {

        // 128^3 x 4 is 8MB
        // 256^3 x 4 is 64MB
        if (dim <= 16) {
            return 16;
        } else if (dim <= 32) {
            return 32;
        } else if (dim <= 64) {

            if (dim > 40) {
                return 64;
            } 
            return 32;
        } else if (dim <= 128) {

            if (dim > 80) {
                return 128;
            } 
            return 64;
        } else if (dim <= 256) {

            if (dim > 160) {
                return 256;
            } 
            return 128;
        } else if (dim <= 512) {

            if (dim > 448) {
                return 512;
            } 
            return 256;
        } else {
            return 512;
        }
    }

    /**
     * Check if given value is power of 2 within the range 16 to 512.
     * @param value given integer value.
     * @return true is power of 2, false not.
     */
    private boolean isPowerOfTwo(int value) {

        if ( (value == 16) || (value == 16) || (value == 32) || (value == 64) || (value == 128) || (value == 256)
                || (value == 512)) {
            return true;
        }

        return false;
    }
}
