package gov.nih.mipav.view.renderer.WildMagic;


import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase;

import java.awt.*;
import java.awt.event.*;
import java.io.File;

import javax.swing.*;
import javax.swing.border.*;

import WildMagic.LibFoundation.Mathematics.BitHacks;


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

    /** Model images A and B. */
    private ModelImage imageA, imageB;

    /** Lookup table of the imageA, B. */
    private final ModelLUT LUTa = null, LUTb = null;

    /** Resample filter type, default: Trilinear Interpoloation. */
    private int m_iFilter = 0;

    /** Resample filter selection box. */
    private JComboBox m_kFilterType;

    /** Number of available dimension. */
    private final int nDim;

    /** Resample resolution corresponding to Power of 2. */
    private float[] newRes = new float[3];

    /** Original resolution array. */
    private float[] res;

    /** Resampled dimension value in Power of 2. */
    private int[] volExtents = new int[3];

    /** Volume size X*Y*Z. */
    private final int volSize = 1;

    private String m_kParentDir = null;

    private JRadioButton m_kCompute = null;

    /**
     * Creates the dialog, using the input parameters to place it on the screen.
     * 
     * @param _imageA Model image A.
     * @param _imageB Model image B.
     * @param kCommand the re-sample command.
     */
    public VolumeTriPlanarDialog(final ModelImage _imageA, final ModelImage _imageB) {
        extents = _imageA.getExtents();
        res = _imageA.getFileInfo(0).getResolutions();
        nDim = extents.length;

        if (nDim < 3) {
            MipavUtil.displayError("The volume renderer only supports 3D or 4D image volumes.");
            return;
        }
        boolean bDirExists = true;
        final String kImageName = ModelImage.makeImageName(_imageA.getFileInfo(0).getFileName(), "");
        m_kParentDir = _imageA.getFileInfo()[0].getFileDirectory().concat(kImageName + "_RenderFiles" + File.separator);
        // System.err.println( m_kParentDir );
        final File kDir = new File(m_kParentDir);
        if ( !kDir.exists()) {
            bDirExists = false;
            try {
                kDir.mkdir();
            } catch (final SecurityException e) {}
        }
        init(bDirExists);
        imageA = _imageA;
        imageB = _imageB;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equals("OK")) {

            volExtents[0] = Integer.parseInt(extXOutput.getText());

            if ( !BitHacks.IsPowerOfTwo(volExtents[0])) {
                MipavUtil.displayInfo("Reample to Power of 2.");
                volExtents[0] = MipavMath.dimPowerOfTwo(volExtents[0]);
            }

            newRes[0] = (extents[0] * res[0]) / volExtents[0];

            volExtents[1] = Integer.parseInt(extYOutput.getText());

            if ( !BitHacks.IsPowerOfTwo(volExtents[1])) {
                MipavUtil.displayInfo("Reample to Power of 2.");
                volExtents[1] = MipavMath.dimPowerOfTwo(volExtents[1]);
            }

            newRes[1] = (extents[1] * res[1]) / volExtents[1];

            if (nDim >= 3) {
                volExtents[2] = Integer.parseInt(extZOutput.getText());

                if ( !BitHacks.IsPowerOfTwo(volExtents[2])) {
                    MipavUtil.displayInfo("Reample to Power of 2.");
                    volExtents[2] = MipavMath.dimPowerOfTwo(volExtents[2]);
                }
                newRes[2] = (extents[2] * res[2]) / volExtents[2];
            }

            setVisible(false);
            exec();
        } else if (command.equals("xChanged")) {
            int x = Integer.parseInt(extXOutput.getText());

            if ( !BitHacks.IsPowerOfTwo(x)) {
                MipavUtil.displayInfo("Reample to Power of 2.");
                x = MipavMath.dimPowerOfTwo(x);
            }

            extXOutput.setText(Integer.toString(x));
        } else if (command.equals("yChanged")) {
            int y = Integer.parseInt(extYOutput.getText());

            if ( !BitHacks.IsPowerOfTwo(y)) {
                MipavUtil.displayInfo("Reample to Power of 2.");
                y = MipavMath.dimPowerOfTwo(y);
            }

            extYOutput.setText(Integer.toString(y));
        } else if (command.equals("zChanged")) {
            int z = Integer.parseInt(extZOutput.getText());

            if ( !BitHacks.IsPowerOfTwo(z)) {
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
        }
    }

    /**
     * Dispose memory.
     */
    public void disposeLocal() {

        imageA = null;
        imageB = null;

        extents = null;
        res = null;
        volExtents = null;
        newRes = null;

        super.dispose();
    }

    public void exec() {

        try {
            final VolumeTriPlanarInterface kWM = new VolumeTriPlanarInterface(imageA, imageB, m_iFilter, m_kCompute
                    .isSelected(), m_kParentDir, volExtents);
        } catch (final NoClassDefFoundError notAvailableError) {
            Preferences.debug("ViewJFrameSurfaceRenderer cannot be called; encountered "
                    + "a NoClassDefFoundError.  \nIt is likely that JOGL is "
                    + "not available on this system.  The error is: \n" + notAvailableError.getLocalizedMessage());
            MipavUtil.displayError("The surface renderer requires JOGL and it cannot be found.");
        } catch (final OutOfMemoryError notEnoughError) {
            Preferences.debug("ViewJFrameSurfaceRenderer cannot be called as there was "
                    + "not enough memory allocated.  \n" + "The error is: \n" + notEnoughError.getLocalizedMessage());
            MipavUtil.displayError("The surface renderer requires more memory " + "than is currently available;\n"
                    + "See the Memory Allocation menu");
        }

    }

    /**
     * Initializes the dialog user-interface.
     */
    public void init(boolean bUsePreComputedFiles) {

        try {
            setIconImage(MipavUtil.getIconImage("wm.gif"));
        } catch (final Exception e) {
            // setIconImage() is not part of the Java 1.5 API - catch any runtime error on those systems
        }

        setTitle("3D Volume & Surface Viewer");

        final Box mainBox = new Box(BoxLayout.Y_AXIS);
        final Box kReuseBox = new Box(BoxLayout.X_AXIS);
        final JRadioButton kReuse = new JRadioButton("Reuse PreComputed Files");
        kReuse.setEnabled(bUsePreComputedFiles);
        kReuse.setSelected(bUsePreComputedFiles);
        kReuse.addActionListener(this);
        kReuse.setActionCommand("ReuseFiles");
        kReuseBox.add(kReuse);
        m_kCompute = new JRadioButton("Compute Render Files");
        m_kCompute.setEnabled(true);
        m_kCompute.setSelected( !bUsePreComputedFiles);
        m_kCompute.addActionListener(this);
        m_kCompute.setActionCommand("ComputeFiles");
        kReuseBox.add(m_kCompute);
        final ButtonGroup kGroup = new ButtonGroup();
        kGroup.add(kReuse);
        kGroup.add(m_kCompute);
        mainBox.add(kReuseBox);

        final Box contentBox = new Box(BoxLayout.X_AXIS);
        final JPanel leftPanel = new JPanel();
        final JPanel rightPanel = new JPanel();

        // make border
        leftPanel.setBorder(JInterfaceBase.buildTitledBorder("Original Extents"));
        contentBox.add(leftPanel);

        // set layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();

        leftPanel.setLayout(gbl);

        // extent X
        leftPanel.add(Box.createHorizontalStrut(10));

        final JLabel extXLabel = new JLabel("extent X:");

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

        final JLabel extYLabel = new JLabel("extent Y:");

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

        // extent Z
        leftPanel.add(Box.createHorizontalStrut(10));

        final JLabel extZLabel = new JLabel("extent Z:");

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

        // make border
        rightPanel.setBorder(JInterfaceBase.buildTitledBorder("Expected Extents"));
        contentBox.add(rightPanel);

        // set layout
        gbl = new GridBagLayout();
        gbc = new GridBagConstraints();
        rightPanel.setLayout(gbl);

        // extent X expected
        rightPanel.add(Box.createHorizontalStrut(10));

        final JLabel extXNewLabel = new JLabel("extent X:");

        extXNewLabel.setFont(MipavUtil.font12);
        extXNewLabel.setForeground(Color.black);
        extXNewLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(extXNewLabel, gbc);
        rightPanel.add(extXNewLabel);
        rightPanel.add(Box.createHorizontalStrut(10));

        // Checking to see if the image has all dimensions that are a power of 2.
        for (int i = 0; i < 3; i++) {
            volExtents[i] = extents[i];
        }

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

        // extent Z expected
        rightPanel.add(Box.createHorizontalStrut(10));

        final JLabel extZNewLabel = new JLabel("extent Z:");

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

        mainBox.add(contentBox);

        /* Filter selection: */
        final JPanel filterPanel = new JPanel();
        filterPanel.setBorder(JInterfaceBase.buildTitledBorder("Select Resampling Filter"));
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

        final JPanel OKCancelPanel = new JPanel(new FlowLayout());
        OKCancelPanel.add(buildOKButton());
        OKCancelPanel.add(buildCancelButton());

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
    final ItemEvent event) {}

    /**
     * Makes the dialog visible in center of screen.
     * 
     * @param status Flag indicating if the dialog should be visible.
     */
    public void setVisible(final boolean status) {

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
}
