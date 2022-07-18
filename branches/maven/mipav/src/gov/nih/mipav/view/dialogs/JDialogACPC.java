package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.AlgorithmTalairachTransform;
import gov.nih.mipav.model.file.FileInfoAfni;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TalairachTransformInfo;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJComponentTriImage;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJFrameTriImage;
import gov.nih.mipav.view.ViewJProgressBar;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * Dialog to enter points for creating an ACPC image.
 * 
 * @author William Gandler
 * @author Pierre-Louis Bazin
 * @see TalairachTransformInfo
 * @see AlgorithmTalairachTransform
 * @see JDialogTalairachTransform
 * @see JDialogTLRC
 */
public class JDialogACPC extends JDialogBase {
    // private static final float ATLAS_ALIGNBOX_LAT = 95.0f; /* dimensions in mm. used for AC-PC */
    // private static final float ATLAS_ALIGNBOX_ANT = 95.0f; /* aligned view clipping box */
    // private static final float ATLAS_ALIGNBOX_INF = 70.0f;
    // private static final float ATLAS_ALIGNBOX_POS = 140.0f; /* Maximum distances allowed above and */
    // private static final float ATLAS_ALIGNBOX_SUP = 100.0f; /* below Talairach zero point */

    private JComboBox comboBoxOrientX, comboBoxOrientY, comboBoxOrientZ;

    private ButtonGroup ACPCGroup;

    private JButton applyACPCButton;

    private JButton cancelACPCButton;

    private JButton setACPCButton;

    private JButton clearACPCButton;

    private JButton setOrientationButton;

    private JRadioButton superiorEdge;

    private JRadioButton posteriorMargin;

    private JRadioButton inferiorEdge;

    private JRadioButton firstPt;

    private JRadioButton anotherPt;

    private final ViewJFrameTriImage frame;

    private final ModelImage image;

    private final TalairachTransformInfo transform;

    private JTextField textVoxelLength;

    private boolean haveSuperiorEdge = false;

    private boolean havePosteriorMargin = false;

    private boolean haveInferiorEdge = false;

    private boolean haveFirstPt = false;

    private boolean haveAnotherPt = false;

    private int[] orient;

    private Vector3f superiorEdgePt;

    private Vector3f posteriorMarginPt;

    private Vector3f inferiorEdgePt;

    private Vector3f firstMidSagPt;

    @SuppressWarnings("unused")
    private Vector3f anotherMidSagPt;

    private final int interpolation;

    private float voxelLength;

    public ModelImage ACPCImage = null;

    /**
     * Flag used to force acpc x dim to old (incorrect) value of 192 instead of fixed value calculation (which results
     * in x dim of 191).
     */
    private boolean useIncorrectAcpcXDim = false;

    /**
     * This method creates a dialog for selecting markers used for generating an AC-PC aligned view image from an
     * original image
     * 
     * @param theParentFrame Pointer to the frame that created this dialog.
     * @param im Pointer to image represented in frame.
     */
    public JDialogACPC(final ViewJFrameTriImage theParentFrame, final ModelImage im, final ModelImage acpc,
            final TalairachTransformInfo trans, final int interp) {
        super(theParentFrame, false);
        frame = theParentFrame;
        orient = im.getFileInfo(0).getAxisOrientation();
        image = im;
        ACPCImage = acpc;
        transform = trans;
        voxelLength = 1;
        interpolation = interp;
        init();
        boolean haveACPCInfo = false;

        // check for the transform info
        if (image.getTalairachTransformInfo() != null) {
            final TalairachTransformInfo transf = image.getTalairachTransformInfo();
            final Vector3f pt = new Vector3f();
            if (transf.isAcpc()) {
                // check if the image is the original one
                if ( (image.getExtents()[0] == transf.getOrigDim()[0])
                        && (image.getExtents()[1] == transf.getOrigDim()[1])
                        && (image.getExtents()[2] == transf.getOrigDim()[2])) {
                    // set the points for AC, PC, mid sagittal
                    final Vector3f ac = transf.getOrigAC();
                    if (ac == null) {
                        Preferences
                                .debug("Talairach transform info does not have anterior comissure in original space\n");
                    }
                    final Vector3f pc = transf.getOrigPC();
                    if (pc == null) {
                        Preferences
                                .debug("Talairach transform info does not have posterior comissure in original space\n");
                    }
                    final float[][] rot = transf.getOrigOrient();
                    if (rot == null) {
                        Preferences
                                .debug("Talairach transform info does not have ACPC orientation in original image\n");
                    }
                    final float acpcRes = transf.getAcpcRes();
                    final float[] origRes = transf.getOrigRes();
                    if (origRes == null) {
                        Preferences.debug("Talairach transform info does not have original image voxel resolutions\n");
                    }
                    if ( (ac != null) && (pc != null) && (rot != null) && (origRes != null)) {
                        haveACPCInfo = true;

                        pt.X = ac.X - origRes[0] * rot[1][0] / origRes[0] * acpcRes;
                        pt.Y = ac.Y - origRes[1] * rot[1][1] / origRes[1] * acpcRes;
                        pt.Z = ac.Z - origRes[2] * rot[1][2] / origRes[2] * acpcRes;
                        setSuperiorEdge(pt);
                        frame.getTriImage(0).removeReference("ACS");
                        frame.getTriImage(0).setReferenceXY(ViewJComponentTriImage.SUPERIOR_EDGE, pt);

                        pt.X = ac.X - origRes[0] * rot[2][0] / origRes[0] * acpcRes;
                        pt.Y = ac.Y - origRes[1] * rot[2][1] / origRes[1] * acpcRes;
                        pt.Z = ac.Z - origRes[2] * rot[2][2] / origRes[2] * acpcRes;
                        setPosteriorMargin(pt);
                        frame.getTriImage(0).removeReference("ACP");
                        frame.getTriImage(0).setReferenceXY(ViewJComponentTriImage.POSTERIOR_MARGIN, pt);

                        setInferiorEdge(pc);
                        pt.X = pc.X;
                        pt.Y = pc.Y;
                        pt.Z = pc.Z;
                        frame.getTriImage(0).removeReference("PC");
                        frame.getTriImage(0).setReferenceXY(ViewJComponentTriImage.INFERIOR_EDGE, pt);

                        pt.X = ac.X + 50 * rot[2][0] / origRes[0] * acpcRes;
                        pt.Y = ac.Y + 50 * rot[2][1] / origRes[1] * acpcRes;
                        pt.Z = ac.Z + 50 * rot[2][2] / origRes[2] * acpcRes;
                        setFirstPt(pt);
                        frame.getTriImage(0).removeReference("MS1");
                        frame.getTriImage(0).setReferenceXY(ViewJComponentTriImage.FIRST_PT, pt);

                        pt.X = pc.X + 50 * rot[2][0] / origRes[0] * acpcRes;
                        pt.Y = pc.Y + 50 * rot[2][1] / origRes[1] * acpcRes;
                        pt.Z = pc.Z + 50 * rot[2][2] / origRes[2] * acpcRes;
                        setAnotherPt(pt);
                        frame.getTriImage(0).removeReference("MS2");
                        frame.getTriImage(0).setReferenceXY(ViewJComponentTriImage.ANOTHER_PT, pt);
                    }
                } else {
                    Preferences.debug("Talairach transform dimensions do not match image dimensions\n");
                }
            } // if (transf.isAcpc())
            else {
                Preferences.debug("Talairach transform info does not have data to compute orig to acpc\n");
            }
        } // if (image.getTalairachTransformInfo()!=null)
        else {
            Preferences.debug("image.getTalairachTransformInfo() is null\n");
        }

        if ( ( !haveACPCInfo) && (image.getFileInfo()[0] instanceof FileInfoAfni)) {
            final FileInfoAfni fileInfoAfni = (FileInfoAfni) (image.getFileInfo()[0]);
            final Vector3f superiorEdgePt = fileInfoAfni.getSuperiorEdge();
            if (superiorEdgePt != null) {
                setSuperiorEdge(superiorEdgePt);
                frame.getTriImage(0).removeReference("ACS");
                frame.getTriImage(0).setReferenceXY(ViewJComponentTriImage.SUPERIOR_EDGE, superiorEdgePt);
            }
            final Vector3f posteriorMarginPt = fileInfoAfni.getPosteriorMargin();
            if (posteriorMarginPt != null) {
                setPosteriorMargin(posteriorMarginPt);
                frame.getTriImage(0).removeReference("ACP");
                frame.getTriImage(0).setReferenceXY(ViewJComponentTriImage.POSTERIOR_MARGIN, posteriorMarginPt);
            }
            final Vector3f inferiorEdgePt = fileInfoAfni.getInferiorEdge();
            if (inferiorEdgePt != null) {
                setInferiorEdge(inferiorEdgePt);
                frame.getTriImage(0).removeReference("PC");
                frame.getTriImage(0).setReferenceXY(ViewJComponentTriImage.INFERIOR_EDGE, inferiorEdgePt);
            }
            final Vector3f firstPt = fileInfoAfni.getFirstPt();
            if (firstPt != null) {
                setFirstPt(firstPt);
                frame.getTriImage(0).removeReference("MS1");
                frame.getTriImage(0).setReferenceXY(ViewJComponentTriImage.FIRST_PT, firstPt);
            }
            final Vector3f anotherPt = fileInfoAfni.getAnotherPt();
            if (anotherPt != null) {
                setAnotherPt(anotherPt);
                frame.getTriImage(0).removeReference("MS2");
                frame.getTriImage(0).setReferenceXY(ViewJComponentTriImage.ANOTHER_PT, anotherPt);
            }
        }
        pack();
        setVisible(true);
    }

    /**
     * This method creates a dialog for selecting markers used for generating an AC-PC aligned view image from an
     * original image
     * 
     * @param theParentFrame Pointer to the frame that created this dialog.
     * @param im Pointer to image represented in frame.
     * @param isVisible The dialog visible or not.
     */
    /*
     * public JDialogACPC(ViewJFrameTriImage theParentFrame, ModelImage im, ModelImage acpc, boolean isVisible,
     * TalairachTransformInfo trans) { super(theParentFrame, false); frame = theParentFrame; orient =
     * im.getFileInfo(0).getAxisOrientation(); image = im; ACPCImage = acpc; transform = trans; voxelLength = 1; init();
     * 
     * 
     * // check for the transform info if (image.getTalairachTransformInfo()!=null) { TalairachTransformInfo transf =
     * image.getTalairachTransformInfo(); Vector3f pt = new Vector3f(); if (transf.isAcpc()) { // check if the image is
     * the original one if ( (image.getExtents()[0]==transf.getOrigDim()[0]) &&
     * (image.getExtents()[1]==transf.getOrigDim()[1]) && (image.getExtents()[2]==transf.getOrigDim()[2]) ) { // set the
     * points for AC, PC, mid sagittal Vector3f ac = transf.getOrigAC(); Vector3f pc = transf.getOrigPC(); float[][] rot
     * = transf.getOrigOrient(); float acpcRes = transf.getAcpcRes(); float[] origRes = transf.getOrigRes();
     * 
     * pt.X = ac.X - 1*rot[1][0]/origRes[0]*acpcRes; pt.Y = ac.Y - 1*rot[1][1]/origRes[1]*acpcRes; pt.Z = ac.Z -
     * 1*rot[1][2]/origRes[2]*acpcRes; setSuperiorEdge(pt); //pt = frame.toDicom(pt,image);
     * ((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("ACS");
     * ((ViewJComponentTriImage)frame.getTriImage(0)).setReferenceXY(ViewJComponentTriImage.SUPERIOR_EDGE,pt);
     * 
     * pt.X = ac.X - 1*rot[2][0]/origRes[0]*acpcRes; pt.Y = ac.Y - 1*rot[2][1]/origRes[1]*acpcRes; pt.Z = ac.Z -
     * 1*rot[2][2]/origRes[2]*acpcRes; setPosteriorMargin(pt); //pt = frame.toDicom(pt,image);
     * ((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("ACP");
     * ((ViewJComponentTriImage)frame.getTriImage(0)).setReferenceXY(ViewJComponentTriImage.POSTERIOR_MARGIN,pt);
     * 
     * setInferiorEdge(pc); //pt = frame.toDicom(pc,image); pt = pc;
     * ((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("PC");
     * ((ViewJComponentTriImage)frame.getTriImage(0)).setReferenceXY(ViewJComponentTriImage.INFERIOR_EDGE,pt);
     * 
     * pt.X = ac.X + 40*rot[2][0]/origRes[0]*acpcRes; pt.Y = ac.Y + 40*rot[2][1]/origRes[1]*acpcRes; pt.Z = ac.Z +
     * 40*rot[2][2]/origRes[2]*acpcRes; setFirstPt(pt); //pt = frame.toDicom(pt,image);
     * ((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("MS1");
     * ((ViewJComponentTriImage)frame.getTriImage(0)).setReferenceXY(ViewJComponentTriImage.FIRST_PT,pt);
     * 
     * pt.X = pc.X + 40*rot[2][0]/origRes[0]*acpcRes; pt.Y = pc.Y + 40*rot[2][1]/origRes[1]*acpcRes; pt.Z = pc.Z +
     * 40*rot[2][2]/origRes[2]*acpcRes; setAnotherPt(pt); //pt = frame.toDicom(pt,image);
     * ((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("MS2");
     * ((ViewJComponentTriImage)frame.getTriImage(0)).setReferenceXY(ViewJComponentTriImage.ANOTHER_PT,pt); } } } }
     */

    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        JPanel pointPanel;
        JPanel orientPanel = null;
        JPanel voxelPanel;
        JLabel labelVoxelLength;
        setTitle("Create AC-PC image");
        if (orient[0] == FileInfoBase.ORI_UNKNOWN_TYPE) {
            orientPanel = new JPanel(new GridLayout(4, 2));
            orientPanel.setForeground(Color.black);
            orientPanel.setBorder(buildTitledBorder("Describe input orientation"));

            final JLabel labelOrientX = new JLabel("X axis:");
            labelOrientX.setForeground(Color.black);
            labelOrientX.setFont(serif12);
            orientPanel.add(labelOrientX);
            comboBoxOrientX = new JComboBox();
            comboBoxOrientX.setFont(serif12);
            comboBoxOrientX.setBackground(Color.white);

            comboBoxOrientX.addItem("Right to Left");
            comboBoxOrientX.addItem("Left to right");
            comboBoxOrientX.addItem("Anterior to posterior");
            comboBoxOrientX.addItem("Posterior to anterior");
            comboBoxOrientX.addItem("Inferior to superior");
            comboBoxOrientX.addItem("Superior to inferior");
            orientPanel.add(comboBoxOrientX);

            final JLabel labelOrientY = new JLabel("Y axis:");
            labelOrientY.setForeground(Color.black);
            labelOrientY.setFont(serif12);
            orientPanel.add(labelOrientY);
            comboBoxOrientY = new JComboBox();
            comboBoxOrientY.setFont(serif12);
            comboBoxOrientY.setBackground(Color.white);

            comboBoxOrientY.addItem("Right to Left");
            comboBoxOrientY.addItem("Left to right");
            comboBoxOrientY.addItem("Anterior to posterior");
            comboBoxOrientY.addItem("Posterior to anterior");
            comboBoxOrientY.addItem("Inferior to superior");
            comboBoxOrientY.addItem("Superior to inferior");
            orientPanel.add(comboBoxOrientY);

            final JLabel labelOrientZ = new JLabel("Z axis:");
            labelOrientZ.setForeground(Color.black);
            labelOrientZ.setFont(serif12);
            orientPanel.add(labelOrientZ);
            comboBoxOrientZ = new JComboBox();
            comboBoxOrientZ.setFont(serif12);
            comboBoxOrientZ.setBackground(Color.white);

            comboBoxOrientZ.addItem("Right to Left");
            comboBoxOrientZ.addItem("Left to right");
            comboBoxOrientZ.addItem("Anterior to posterior");
            comboBoxOrientZ.addItem("Posterior to anterior");
            comboBoxOrientZ.addItem("Inferior to superior");
            comboBoxOrientZ.addItem("Superior to inferior");
            orientPanel.add(comboBoxOrientZ);

            final JLabel labelFixOrient = new JLabel("Fix orientation:");
            labelFixOrient.setForeground(Color.black);
            labelFixOrient.setFont(serif12);
            orientPanel.add(labelFixOrient);

            setOrientationButton = new JButton("Set new orientation");
            setOrientationButton.setFont(serif12B);
            setOrientationButton.addActionListener(this);
            setOrientationButton.setActionCommand("setOrientation");
            orientPanel.add(setOrientationButton);
        } // if (orient[0] == -1)

        voxelPanel = new JPanel(new GridLayout(1, 2));
        voxelPanel.setForeground(Color.black);
        voxelPanel.setBorder(buildTitledBorder("Specify cubic voxel length (mm.)"));

        labelVoxelLength = new JLabel("Cubic voxel length");
        labelVoxelLength.setForeground(Color.black);
        labelVoxelLength.setFont(serif12);
        labelVoxelLength.setEnabled(true);
        voxelPanel.add(labelVoxelLength);

        textVoxelLength = new JTextField();
        textVoxelLength.setText("1.0");
        textVoxelLength.setFont(serif12);
        textVoxelLength.setEnabled(true);
        textVoxelLength.addFocusListener(this);
        voxelPanel.add(textVoxelLength);

        pointPanel = new JPanel(new GridLayout(5, 1));
        pointPanel.setForeground(Color.black);
        pointPanel.setBorder(buildTitledBorder("Select point type"));

        ACPCGroup = new ButtonGroup();
        superiorEdge = new JRadioButton("AC superior edge", true);
        superiorEdge.setFont(serif12);
        superiorEdge.addActionListener(this);
        superiorEdge.setActionCommand("superiorEdgeCommand");
        ACPCGroup.add(superiorEdge);
        pointPanel.add(superiorEdge);

        posteriorMargin = new JRadioButton("AC posterior margin", false);
        posteriorMargin.setFont(serif12);
        posteriorMargin.addActionListener(this);
        posteriorMargin.setActionCommand("posteriorMarginCommand");
        ACPCGroup.add(posteriorMargin);
        pointPanel.add(posteriorMargin);

        inferiorEdge = new JRadioButton("PC inferior edge", false);
        inferiorEdge.setFont(serif12);
        inferiorEdge.addActionListener(this);
        inferiorEdge.setActionCommand("inferiorEdgeCommand");
        ACPCGroup.add(inferiorEdge);
        pointPanel.add(inferiorEdge);

        firstPt = new JRadioButton("First mid-sag pt", false);
        firstPt.setFont(serif12);
        firstPt.addActionListener(this);
        firstPt.setActionCommand("firstPtCommand");
        ACPCGroup.add(firstPt);
        pointPanel.add(firstPt);

        anotherPt = new JRadioButton("Another mid-sag pt", false);
        anotherPt.setFont(serif12);
        anotherPt.addActionListener(this);
        anotherPt.setActionCommand("anotherPtCommand");
        ACPCGroup.add(anotherPt);
        pointPanel.add(anotherPt);

        final JPanel optionPanel = new JPanel();
        optionPanel.setForeground(Color.black);
        optionPanel.setBorder(buildTitledBorder("Other options"));

        useIncorrectAcpcXDim = Preferences.is(Preferences.PREF_USE_INCORRECT_ACPC_XDIM);

        final JCheckBox useIncorrectAcpcXDimCheckBox = new JCheckBox(
                "Use incorrect, old MIPAV calculation of ACPC X dim", useIncorrectAcpcXDim);
        useIncorrectAcpcXDimCheckBox.setFont(serif12);
        useIncorrectAcpcXDimCheckBox.addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(final ItemEvent e) {
                useIncorrectAcpcXDim = useIncorrectAcpcXDimCheckBox.isSelected();
            }
        });
        optionPanel.add(useIncorrectAcpcXDimCheckBox, BorderLayout.NORTH);

        setACPCButton = new JButton("Set");
        setACPCButton.setFont(serif12B);
        setACPCButton.addActionListener(this);
        setACPCButton.setActionCommand("setACPC");
        // setACPCButton.setPreferredSize(buttonSize);

        clearACPCButton = new JButton("Clear");
        clearACPCButton.setFont(serif12B);
        clearACPCButton.addActionListener(this);
        clearACPCButton.setActionCommand("clearACPC");
        clearACPCButton.setEnabled(false);
        // clearACPCButton.setPreferredSize(buttonSize);

        applyACPCButton = new JButton("Apply");
        applyACPCButton.setFont(serif12B);
        applyACPCButton.addActionListener(this);
        applyACPCButton.setActionCommand("applyACPC");
        applyACPCButton.setEnabled(false);
        // applyACPCButton.setPreferredSize(buttonSize);

        cancelACPCButton = new JButton("Cancel");
        cancelACPCButton.setFont(serif12B);
        cancelACPCButton.addActionListener(this);
        cancelACPCButton.setActionCommand("cancelACPC");
        // cancelACPCButton.setPreferredSize(buttonSize);

        final JPanel buttonPanel = new JPanel(new GridBagLayout());
        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        buttonPanel.add(setACPCButton, gbc);
        gbc.gridx = 1;
        buttonPanel.add(clearACPCButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        buttonPanel.add(applyACPCButton, gbc);
        gbc.gridx = 1;
        buttonPanel.add(cancelACPCButton, gbc);

        final JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        int y = 0;
        if (orient[0] == FileInfoBase.ORI_UNKNOWN_TYPE) {
            mainPanel.add(orientPanel, gbc);
            gbc.gridy = 1;
            y = 1;
        }
        mainPanel.add(voxelPanel, gbc);
        gbc.gridy = y + 1;
        mainPanel.add(pointPanel, gbc);
        gbc.gridy++;
        mainPanel.add(optionPanel, gbc);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
    }

    /**
     * Get the dialog main panel.
     * 
     * @return the main panel of the dialog
     */
    public JPanel getMainPanel() {
        JPanel pointPanel;
        // JPanel orientPanel = null;
        JPanel voxelPanel;
        JLabel labelVoxelLength;
        setTitle("Create AC-PC image");
        /*
         * if (orient[0] == FileInfoBase.ORI_UNKNOWN_TYPE) { orientPanel = new JPanel(new GridLayout(3,2));
         * orientPanel.setForeground(Color.black);
         * orientPanel.setBorder(buildTitledBorder("Describe input orientation"));
         * 
         * JLabel labelOrientX = new JLabel("X axis:"); labelOrientX.setForeground(Color.black);
         * labelOrientX.setFont(serif12); orientPanel.add(labelOrientX); comboBoxOrientX = new JComboBox();
         * comboBoxOrientX.setFont(serif12); comboBoxOrientX.setBackground(Color.white);
         * 
         * comboBoxOrientX.addItem("Right to Left"); comboBoxOrientX.addItem("Left to right");
         * comboBoxOrientX.addItem("Anterior to posterior"); comboBoxOrientX.addItem("Posterior to anterior");
         * comboBoxOrientX.addItem("Inferior to superior"); comboBoxOrientX.addItem("Superior to inferior");
         * orientPanel.add(comboBoxOrientX);
         * 
         * JLabel labelOrientY = new JLabel("Y axis:"); labelOrientY.setForeground(Color.black);
         * labelOrientY.setFont(serif12); orientPanel.add(labelOrientY); comboBoxOrientY = new JComboBox();
         * comboBoxOrientY.setFont(serif12); comboBoxOrientY.setBackground(Color.white);
         * 
         * comboBoxOrientY.addItem("Right to Left"); comboBoxOrientY.addItem("Left to right");
         * comboBoxOrientY.addItem("Anterior to posterior"); comboBoxOrientY.addItem("Posterior to anterior");
         * comboBoxOrientY.addItem("Inferior to superior"); comboBoxOrientY.addItem("Superior to inferior");
         * orientPanel.add(comboBoxOrientY);
         * 
         * JLabel labelOrientZ = new JLabel("Z axis:"); labelOrientZ.setForeground(Color.black);
         * labelOrientZ.setFont(serif12); orientPanel.add(labelOrientZ); comboBoxOrientZ = new JComboBox();
         * comboBoxOrientZ.setFont(serif12); comboBoxOrientZ.setBackground(Color.white);
         * 
         * comboBoxOrientZ.addItem("Right to Left"); comboBoxOrientZ.addItem("Left to right");
         * comboBoxOrientZ.addItem("Anterior to posterior"); comboBoxOrientZ.addItem("Posterior to anterior");
         * comboBoxOrientZ.addItem("Inferior to superior"); comboBoxOrientZ.addItem("Superior to inferior");
         * orientPanel.add(comboBoxOrientZ); } // if (orient[0] == -1)
         */
        voxelPanel = new JPanel(new GridLayout(1, 2));
        voxelPanel.setForeground(Color.black);
        voxelPanel.setBorder(buildTitledBorder("Specify cubic voxel length (mm.)"));

        labelVoxelLength = new JLabel("Cubic voxel length");
        labelVoxelLength.setForeground(Color.black);
        labelVoxelLength.setFont(serif12);
        labelVoxelLength.setEnabled(true);
        voxelPanel.add(labelVoxelLength);

        textVoxelLength = new JTextField();
        textVoxelLength.setText("1.0");
        textVoxelLength.setFont(serif12);
        textVoxelLength.setEnabled(true);
        textVoxelLength.addFocusListener(this);
        voxelPanel.add(textVoxelLength);

        pointPanel = new JPanel(new GridLayout(5, 1));
        pointPanel.setForeground(Color.black);
        pointPanel.setBorder(buildTitledBorder("Select point type"));

        ACPCGroup = new ButtonGroup();
        superiorEdge = new JRadioButton("AC superior edge", true);
        superiorEdge.setFont(serif12);
        superiorEdge.addActionListener(this);
        superiorEdge.setActionCommand("superiorEdgeCommand");
        ACPCGroup.add(superiorEdge);
        pointPanel.add(superiorEdge);

        posteriorMargin = new JRadioButton("AC posterior margin", false);
        posteriorMargin.setFont(serif12);
        posteriorMargin.addActionListener(this);
        posteriorMargin.setActionCommand("posteriorMarginCommand");
        ACPCGroup.add(posteriorMargin);
        pointPanel.add(posteriorMargin);

        inferiorEdge = new JRadioButton("PC inferior edge", false);
        inferiorEdge.setFont(serif12);
        inferiorEdge.addActionListener(this);
        inferiorEdge.setActionCommand("inferiorEdgeCommand");
        ACPCGroup.add(inferiorEdge);
        pointPanel.add(inferiorEdge);

        firstPt = new JRadioButton("First mid-sag pt", false);
        firstPt.setFont(serif12);
        firstPt.addActionListener(this);
        firstPt.setActionCommand("firstPtCommand");
        ACPCGroup.add(firstPt);
        pointPanel.add(firstPt);

        anotherPt = new JRadioButton("Another mid-sag pt", false);
        anotherPt.setFont(serif12);
        anotherPt.addActionListener(this);
        anotherPt.setActionCommand("anotherPtCommand");
        ACPCGroup.add(anotherPt);
        pointPanel.add(anotherPt);

        setACPCButton = new JButton("Set");
        setACPCButton.setFont(serif12B);
        setACPCButton.addActionListener(this);
        setACPCButton.setActionCommand("setACPC");
        // setACPCButton.setPreferredSize(buttonSize);

        clearACPCButton = new JButton("Clear");
        clearACPCButton.setFont(serif12B);
        clearACPCButton.addActionListener(this);
        clearACPCButton.setActionCommand("clearACPC");
        clearACPCButton.setEnabled(false);
        // clearACPCButton.setPreferredSize(buttonSize);

        applyACPCButton = new JButton("Apply");
        applyACPCButton.setFont(serif12B);
        applyACPCButton.addActionListener(this);
        applyACPCButton.setActionCommand("applyACPC");
        applyACPCButton.setEnabled(false);
        // applyACPCButton.setPreferredSize(buttonSize);

        cancelACPCButton = new JButton("Cancel");
        cancelACPCButton.setFont(serif12B);
        cancelACPCButton.addActionListener(this);
        cancelACPCButton.setActionCommand("cancelACPC");
        // cancelACPCButton.setPreferredSize(buttonSize);

        final JPanel buttonPanel = new JPanel(new GridBagLayout());
        final GridBagConstraints gbc = new GridBagConstraints();
        buttonPanel.add(setACPCButton, gbc);
        gbc.gridx = 1;
        buttonPanel.add(clearACPCButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        buttonPanel.add(applyACPCButton, gbc);
        gbc.gridx = 1;
        // buttonPanel.add(cancelACPCButton, gbc);

        final JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        final int y = 0;
        /*
         * if (orient[0] == FileInfoBase.ORI_UNKNOWN_TYPE) { mainPanel.add(orientPanel, gbc); gbc.gridy = 1; y = 1; }
         */
        mainPanel.add(voxelPanel, gbc);
        gbc.gridy = y + 1;
        mainPanel.add(pointPanel, gbc);
        final JPanel panel = new JPanel();
        panel.add(mainPanel);
        panel.add(buttonPanel, BorderLayout.SOUTH);
        return panel;
    }

    /**
     * If user clicks "Set", sets point here and in component image. If user clicks "Clear", clears point here and in
     * component image. If user clicks "Apply", creates new Talairach image based on points. If user clicks "Cancel",
     * disposes this dialog.
     * 
     * @param event Event that triggered this method.
     */
    @Override
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();
        boolean found;
        int pointType;
        Vector3f pt = null;
        boolean dotoOriginal;
        if (command.equals("setOrientation")) {
            setOrient();
            return;
        }
        if ( !getOrient()) {
            return;
        }

        if (command.equals("setACPC")) {
            found = false;
            if (frame.getCurrentOrientation() == FileInfoBase.AXIAL) {
                pt = new Vector3f(frame.getTriImage(ViewJFrameTriImage.AXIAL_A).getCenter());
                dotoOriginal = false;
            } else if (frame.getCurrentOrientation() == FileInfoBase.CORONAL) {
                pt = new Vector3f(frame.getTriImage(ViewJFrameTriImage.CORONAL_A).getCenter());
                dotoOriginal = false;
            } else if (frame.getCurrentOrientation() == FileInfoBase.SAGITTAL) {
                pt = new Vector3f(frame.getTriImage(ViewJFrameTriImage.SAGITTAL_A).getCenter());
                dotoOriginal = false;
            } else {
                pt = new Vector3f(frame.getSagittalComponentSlice(), frame.getCoronalComponentSlice(),
                        frame.getAxialComponentSlice());
                dotoOriginal = true;
            }
            // System.out.println("pt: " + (int)pt.X + "," + (int)pt.Y + "," + (int)pt.Z);
            // System.out.println("corrected pt: " + (int)toOriginal(pt).X + "," + (int)toOriginal(pt).Y + "," +
            // (int)toOriginal(pt).Z);
            if (superiorEdge.isSelected()) {
                pointType = ViewJComponentTriImage.SUPERIOR_EDGE;
                frame.getTriImage(0).removeReference("ACS");
                if (dotoOriginal) {
                    setSuperiorEdge(toOriginal(pt));
                } else {
                    setSuperiorEdge(pt);
                }
                posteriorMargin.setSelected(true);
                if (havePosteriorMargin) {
                    setACPCButton.setEnabled(false);
                    clearACPCButton.setEnabled(true);
                } else {
                    setACPCButton.setEnabled(true);
                    clearACPCButton.setEnabled(false);
                }
            } else if (posteriorMargin.isSelected()) {
                pointType = ViewJComponentTriImage.POSTERIOR_MARGIN;
                frame.getTriImage(0).removeReference("ACP");
                if (dotoOriginal) {
                    setPosteriorMargin(toOriginal(pt));
                } else {
                    setPosteriorMargin(pt);
                }
                inferiorEdge.setSelected(true);
                if (haveInferiorEdge) {
                    setACPCButton.setEnabled(false);
                    clearACPCButton.setEnabled(true);
                } else {
                    setACPCButton.setEnabled(true);
                    clearACPCButton.setEnabled(false);
                }
            } else if (inferiorEdge.isSelected()) {
                pointType = ViewJComponentTriImage.INFERIOR_EDGE;
                frame.getTriImage(0).removeReference("PC");
                if (dotoOriginal) {
                    setInferiorEdge(toOriginal(pt));
                } else {
                    setInferiorEdge(pt);
                }
                firstPt.setSelected(true);
                if (haveFirstPt) {
                    setACPCButton.setEnabled(false);
                    clearACPCButton.setEnabled(true);
                } else {
                    setACPCButton.setEnabled(true);
                    clearACPCButton.setEnabled(false);
                }
            } else if (firstPt.isSelected()) {
                pointType = ViewJComponentTriImage.FIRST_PT;
                frame.getTriImage(0).removeReference("MS1");
                if (dotoOriginal) {
                    setFirstPt(toOriginal(pt));
                } else {
                    setFirstPt(pt);
                }
                anotherPt.setSelected(true);
                if (haveAnotherPt) {
                    setACPCButton.setEnabled(false);
                    clearACPCButton.setEnabled(true);
                } else {
                    setACPCButton.setEnabled(true);
                    clearACPCButton.setEnabled(false);
                }
            } else {
                pointType = ViewJComponentTriImage.ANOTHER_PT;
                frame.getTriImage(0).removeReference("MS2");
                if (dotoOriginal) {
                    setAnotherPt(toOriginal(pt));
                } else {
                    setAnotherPt(pt);
                }
            }
            if (dotoOriginal) {
                frame.getTriImage(0).setReferenceXY(pointType, toOriginal(pt));
            } else {
                frame.getTriImage(0).setReferenceXY(pointType, pt);
            }
            // alternate: add a VOI point for each new point

        } else if (command.equals("clearACPC")) {
            if (superiorEdge.isSelected()) {
                found = frame.getTriImage(0).removeReference("ACS");
                if (found) {
                    haveSuperiorEdge = false;
                    superiorEdge.setText("AC superior edge");
                    clearACPCButton.setEnabled(false);
                    setACPCButton.setEnabled(true);
                    applyACPCButton.setEnabled(false);
                } else {
                    MipavUtil.displayError("Error! Failed to remove AC superior edge point");
                }
            } else if (posteriorMargin.isSelected()) {
                found = frame.getTriImage(0).removeReference("ACP");
                if (found) {
                    havePosteriorMargin = false;
                    posteriorMargin.setText("AC posterior margin");
                    clearACPCButton.setEnabled(false);
                    setACPCButton.setEnabled(true);
                    applyACPCButton.setEnabled(false);
                } else {
                    MipavUtil.displayError("Error! Failed to remove AC posterior margin point");
                }
            } else if (inferiorEdge.isSelected()) {
                found = frame.getTriImage(0).removeReference("PC");
                if (found) {
                    haveInferiorEdge = false;
                    inferiorEdge.setText("PC inferior edge");
                    clearACPCButton.setEnabled(false);
                    setACPCButton.setEnabled(true);
                    applyACPCButton.setEnabled(false);
                } else {
                    MipavUtil.displayError("Error! Failed to remove PC inferior edge point");
                }
            } else if (firstPt.isSelected()) {
                found = frame.getTriImage(0).removeReference("MS1");
                if (found) {
                    haveFirstPt = false;
                    firstPt.setText("First mid-sag pt");
                    clearACPCButton.setEnabled(false);
                    setACPCButton.setEnabled(true);
                    applyACPCButton.setEnabled(false);
                } else {
                    MipavUtil.displayError("Error! Failed to remove first mid-sag point");
                }
            } else {
                found = frame.getTriImage(0).removeReference("MS2");
                if (found) {
                    haveAnotherPt = false;
                    anotherPt.setText("Another mid-sag pt");
                    clearACPCButton.setEnabled(false);
                    setACPCButton.setEnabled(true);
                    applyACPCButton.setEnabled(false);
                } else {
                    MipavUtil.displayError("Error! Failed to remove another mid-sag point");
                }
            }
        } else if (command.equals("applyACPC")) {
            if (orient[0] == FileInfoBase.ORI_UNKNOWN_TYPE) {
                MipavUtil.displayError("Please set the image orientation first");
                return;
            }
            final String tmpStr = textVoxelLength.getText();
            voxelLength = Float.parseFloat(tmpStr);
            if (voxelLength < 0.0) {
                MipavUtil.displayError("kernelDiameter must be greater than 0");
                textVoxelLength.requestFocus();
                textVoxelLength.selectAll();
                return;
            }
            if (convertToACPC()) {
                setVisible(false);
                dispose();
            }
        } else if (command.equals("cancelACPC")) {
            setVisible(false);
            frame.setVisible(false);
            dispose();
        } else if (command.equals("superiorEdgeCommand")) {
            if (haveSuperiorEdge) {
                setACPCButton.setEnabled(false);
                clearACPCButton.setEnabled(true);
            } else {
                setACPCButton.setEnabled(true);
                clearACPCButton.setEnabled(false);
            }
        } else if (command.equals("posteriorMarginCommand")) {
            if (havePosteriorMargin) {
                setACPCButton.setEnabled(false);
                clearACPCButton.setEnabled(true);
            } else {
                setACPCButton.setEnabled(true);
                clearACPCButton.setEnabled(false);
            }
        } else if (command.equals("inferiorEdgeCommand")) {
            if (haveInferiorEdge) {
                setACPCButton.setEnabled(false);
                clearACPCButton.setEnabled(true);
            } else {
                setACPCButton.setEnabled(true);
                clearACPCButton.setEnabled(false);
            }
        } else if (command.equals("firstPtCommand")) {
            if (haveFirstPt) {
                setACPCButton.setEnabled(false);
                clearACPCButton.setEnabled(true);
            } else {
                setACPCButton.setEnabled(true);
                clearACPCButton.setEnabled(false);
            }
        } else if (command.equals("anotherPtCommand")) {
            if (haveAnotherPt) {
                setACPCButton.setEnabled(false);
                clearACPCButton.setEnabled(true);
            } else {
                setACPCButton.setEnabled(true);
                clearACPCButton.setEnabled(false);
            }
        }
    }

    private boolean getOrient() {
        boolean success = true;
        orient = image.getFileInfo()[0].getAxisOrientation();
        // check that all directions are accounted for - one patient x-axis, one patient y-axis, and one patient z-axis
        if ( ! ( (orient[0] == FileInfoBase.ORI_R2L_TYPE) || (orient[0] == FileInfoBase.ORI_L2R_TYPE)
                || (orient[1] == FileInfoBase.ORI_R2L_TYPE) || (orient[1] == FileInfoBase.ORI_L2R_TYPE)
                || (orient[2] == FileInfoBase.ORI_R2L_TYPE) || (orient[2] == FileInfoBase.ORI_L2R_TYPE))) {
            success = false;
        }

        if ( ! ( (orient[0] == FileInfoBase.ORI_P2A_TYPE) || (orient[0] == FileInfoBase.ORI_A2P_TYPE)
                || (orient[1] == FileInfoBase.ORI_P2A_TYPE) || (orient[1] == FileInfoBase.ORI_A2P_TYPE)
                || (orient[2] == FileInfoBase.ORI_P2A_TYPE) || (orient[2] == FileInfoBase.ORI_A2P_TYPE))) {
            success = false;
        }

        if ( ! ( (orient[0] == FileInfoBase.ORI_I2S_TYPE) || (orient[0] == FileInfoBase.ORI_S2I_TYPE)
                || (orient[1] == FileInfoBase.ORI_I2S_TYPE) || (orient[1] == FileInfoBase.ORI_S2I_TYPE)
                || (orient[2] == FileInfoBase.ORI_I2S_TYPE) || (orient[2] == FileInfoBase.ORI_S2I_TYPE))) {
            success = false;
        }

        if ( !success) {
            MipavUtil.displayError("Illegal selections for axes orientations - Must correct axis orientations");
        }

        return success;
    }

    /**
     * Gets the orientation from the combo boxes and checks if it's consistent.
     * 
     * @return <code>true</code> if orientation is consistent.
     */

    private boolean setOrient() {
        boolean success = true;

        // Obtain the input axes orientations
        int i = comboBoxOrientX.getSelectedIndex();
        switch (i) {
            case 0:
                orient[0] = FileInfoBase.ORI_R2L_TYPE;
                break;
            case 1:
                orient[0] = FileInfoBase.ORI_L2R_TYPE;
                break;
            case 2:
                orient[0] = FileInfoBase.ORI_A2P_TYPE;
                break;
            case 3:
                orient[0] = FileInfoBase.ORI_P2A_TYPE;
                break;
            case 4:
                orient[0] = FileInfoBase.ORI_I2S_TYPE;
                break;
            case 5:
                orient[0] = FileInfoBase.ORI_S2I_TYPE;
                break;
        }

        i = comboBoxOrientY.getSelectedIndex();
        switch (i) {
            case 0:
                orient[1] = FileInfoBase.ORI_R2L_TYPE;
                break;
            case 1:
                orient[1] = FileInfoBase.ORI_L2R_TYPE;
                break;
            case 2:
                orient[1] = FileInfoBase.ORI_A2P_TYPE;
                break;
            case 3:
                orient[1] = FileInfoBase.ORI_P2A_TYPE;
                break;
            case 4:
                orient[1] = FileInfoBase.ORI_I2S_TYPE;
                break;
            case 5:
                orient[1] = FileInfoBase.ORI_S2I_TYPE;
                break;
        }

        i = comboBoxOrientZ.getSelectedIndex();
        switch (i) {
            case 0:
                orient[2] = FileInfoBase.ORI_R2L_TYPE;
                break;
            case 1:
                orient[2] = FileInfoBase.ORI_L2R_TYPE;
                break;
            case 2:
                orient[2] = FileInfoBase.ORI_A2P_TYPE;
                break;
            case 3:
                orient[2] = FileInfoBase.ORI_P2A_TYPE;
                break;
            case 4:
                orient[2] = FileInfoBase.ORI_I2S_TYPE;
                break;
            case 5:
                orient[2] = FileInfoBase.ORI_S2I_TYPE;
                break;
        }

        // check that all directions are accounted for - one patient x-axis, one patient y-axis, and one patient z-axis
        if ( ! ( (orient[0] == FileInfoBase.ORI_R2L_TYPE) || (orient[0] == FileInfoBase.ORI_L2R_TYPE)
                || (orient[1] == FileInfoBase.ORI_R2L_TYPE) || (orient[1] == FileInfoBase.ORI_L2R_TYPE)
                || (orient[2] == FileInfoBase.ORI_R2L_TYPE) || (orient[2] == FileInfoBase.ORI_L2R_TYPE))) {
            success = false;
        }

        if ( ! ( (orient[0] == FileInfoBase.ORI_P2A_TYPE) || (orient[0] == FileInfoBase.ORI_A2P_TYPE)
                || (orient[1] == FileInfoBase.ORI_P2A_TYPE) || (orient[1] == FileInfoBase.ORI_A2P_TYPE)
                || (orient[2] == FileInfoBase.ORI_P2A_TYPE) || (orient[2] == FileInfoBase.ORI_A2P_TYPE))) {
            success = false;
        }

        if ( ! ( (orient[0] == FileInfoBase.ORI_I2S_TYPE) || (orient[0] == FileInfoBase.ORI_S2I_TYPE)
                || (orient[1] == FileInfoBase.ORI_I2S_TYPE) || (orient[1] == FileInfoBase.ORI_S2I_TYPE)
                || (orient[2] == FileInfoBase.ORI_I2S_TYPE) || (orient[2] == FileInfoBase.ORI_S2I_TYPE))) {
            success = false;
        }

        if ( !success) {
            MipavUtil.displayError("Illegal selections for axes orientations - Must correct axis orientations");
        }

        for (i = 0; i < image.getExtents()[2]; i++) {
            image.getFileInfo()[i].setAxisOrientation(orient);
        }

        return success;

    }

    /**
     * Sets superior label based on the point. Enables "Apply" if all points have been set.
     * 
     * @param pt Point that was set.
     */
    private void setSuperiorEdge(final Vector3f pt) {
        superiorEdge.setSelected(true);
        posteriorMargin.setSelected(false);
        inferiorEdge.setSelected(false);
        firstPt.setSelected(false);
        anotherPt.setSelected(false);
        superiorEdgePt = new Vector3f(pt.X, pt.Y, pt.Z);
        haveSuperiorEdge = true;
        superiorEdge.setText("AC superior edge " + Math.round(pt.X) + "," + Math.round(pt.Y) + "," + Math.round(pt.Z));
        setACPCButton.setEnabled(false);
        clearACPCButton.setEnabled(true);
        if ( (haveSuperiorEdge) && (havePosteriorMargin) && (haveInferiorEdge) && (haveFirstPt) && (haveAnotherPt)) {
            applyACPCButton.setEnabled(true);
        }
    }

    /**
     * Sets posterior label based on the point. Enables "Apply" if all points have been set.
     * 
     * @param pt Point that was set.
     */
    private void setPosteriorMargin(final Vector3f pt) {
        superiorEdge.setSelected(false);
        posteriorMargin.setSelected(true);
        inferiorEdge.setSelected(false);
        firstPt.setSelected(false);
        anotherPt.setSelected(false);
        posteriorMarginPt = new Vector3f(pt.X, pt.Y, pt.Z);
        havePosteriorMargin = true;
        posteriorMargin.setText("AC posterior margin " + Math.round(pt.X) + "," + Math.round(pt.Y) + ","
                + Math.round(pt.Z));
        setACPCButton.setEnabled(false);
        clearACPCButton.setEnabled(true);
        if ( (haveSuperiorEdge) && (havePosteriorMargin) && (haveInferiorEdge) && (haveFirstPt) && (haveAnotherPt)) {
            applyACPCButton.setEnabled(true);
        }
    }

    /**
     * Sets inferior label based on the point. Enables "Apply" if all points have been set.
     * 
     * @param pt Point that was set.
     */
    private void setInferiorEdge(final Vector3f pt) {
        superiorEdge.setSelected(false);
        posteriorMargin.setSelected(false);
        inferiorEdge.setSelected(true);
        firstPt.setSelected(false);
        anotherPt.setSelected(false);
        inferiorEdgePt = new Vector3f(pt.X, pt.Y, pt.Z);
        haveInferiorEdge = true;
        inferiorEdge.setText("PC inferior edge " + Math.round(pt.X) + "," + Math.round(pt.Y) + "," + Math.round(pt.Z));
        setACPCButton.setEnabled(false);
        clearACPCButton.setEnabled(true);
        if ( (haveSuperiorEdge) && (havePosteriorMargin) && (haveInferiorEdge) && (haveFirstPt) && (haveAnotherPt)) {
            applyACPCButton.setEnabled(true);
        }
    }

    /**
     * Sets first label based on the point. Enables "Apply" if all points have been set.
     * 
     * @param pt Point that was set.
     */
    private void setFirstPt(final Vector3f pt) {
        superiorEdge.setSelected(false);
        posteriorMargin.setSelected(false);
        inferiorEdge.setSelected(false);
        firstPt.setSelected(true);
        anotherPt.setSelected(false);
        firstMidSagPt = new Vector3f(pt.X, pt.Y, pt.Z);
        haveFirstPt = true;
        firstPt.setText("First mid-sag pt " + Math.round(pt.X) + "," + Math.round(pt.Y) + "," + Math.round(pt.Z));
        setACPCButton.setEnabled(false);
        clearACPCButton.setEnabled(true);
        if ( (haveSuperiorEdge) && (havePosteriorMargin) && (haveInferiorEdge) && (haveFirstPt) && (haveAnotherPt)) {
            applyACPCButton.setEnabled(true);
        }
    }

    /**
     * Sets another label based on the point. Enables "Apply" if all points have been set.
     * 
     * @param pt Point that was set.
     */
    private void setAnotherPt(final Vector3f pt) {
        superiorEdge.setSelected(false);
        posteriorMargin.setSelected(false);
        inferiorEdge.setSelected(false);
        firstPt.setSelected(false);
        anotherPt.setSelected(true);
        anotherMidSagPt = new Vector3f(pt.X, pt.Y, pt.Z);
        haveAnotherPt = true;
        anotherPt.setText("Another mid-sag pt " + Math.round(pt.X) + "," + Math.round(pt.Y) + "," + Math.round(pt.Z));
        setACPCButton.setEnabled(false);
        clearACPCButton.setEnabled(true);
        if ( (haveSuperiorEdge) && (havePosteriorMargin) && (haveInferiorEdge) && (haveFirstPt) && (haveAnotherPt)) {
            applyACPCButton.setEnabled(true);
        }
    }

    /**
     * Converts image to AC-PC image. Returns flag indicating success.
     * 
     * @return <code>true</code> if successful conversion.
     */
    protected boolean convertToACPC() {
        @SuppressWarnings("unused")
        ViewJFrameImage imageFrame;

        // compute AC, PC and orientations
        final float[][] origOrient = new float[3][3];
        final Vector3f origAC = new Vector3f(0.0f, 0.0f, 0.0f);
        final Vector3f origPC = new Vector3f(inferiorEdgePt.X, inferiorEdgePt.Y, inferiorEdgePt.Z);
        final float[] origRes = new float[3];
        final int[] origDim = image.getExtents();
        final float acpcRes = voxelLength;

        // Get the original resolutions in millimeters
        for (int i = 0; i <= 2; i++) {
            final int unit = image.getFileInfo(0).getUnitsOfMeasure(i);
            if ( (unit == Unit.INCHES.getLegacyNum()) || (unit == Unit.MILS.getLegacyNum())
                    || (unit == Unit.CENTIMETERS.getLegacyNum()) || (unit == Unit.ANGSTROMS.getLegacyNum())
                    || (unit == Unit.NANOMETERS.getLegacyNum()) || (unit == Unit.MICROMETERS.getLegacyNum())
                    || (unit == Unit.MILLIMETERS.getLegacyNum()) || (unit == Unit.METERS.getLegacyNum())
                    || (unit == Unit.KILOMETERS.getLegacyNum()) || (unit == Unit.MILES.getLegacyNum())) {
                origRes[i] = image.getFileInfo(0).getResolutions()[i];
                if (origRes[i] <= 0.0f) {
                    MipavUtil.displayWarning("resolution[" + i + "] was recorded as " + origRes[i]
                            + " It is being changed to 1.0");
                    origRes[i] = 1.0f;
                }
                // Be ready for conversions between different units.
                if (unit == Unit.MILLIMETERS.getLegacyNum()) {
                    origRes[i] = origRes[i];
                } else if (unit == Unit.INCHES.getLegacyNum()) {
                    origRes[i] = 25.4f * origRes[i];
                } else if (unit == Unit.MILS.getLegacyNum()) {
                    origRes[i] = 2.54e-2f * origRes[i];
                } else if (unit == Unit.CENTIMETERS.getLegacyNum()) {
                    origRes[i] = 10.0f * origRes[i];
                } else if (unit == Unit.ANGSTROMS.getLegacyNum()) {
                    origRes[i] = 1.0e-7f * origRes[i];
                } else if (unit == Unit.NANOMETERS.getLegacyNum()) {
                    origRes[i] = 1.0e-6f * origRes[i];
                } else if (unit == Unit.MICROMETERS.getLegacyNum()) {
                    origRes[i] = 1.0e-3f * origRes[i];
                } else if (unit == Unit.METERS.getLegacyNum()) {
                    origRes[i] = 1.0e3f * origRes[i];
                } else if (unit == Unit.KILOMETERS.getLegacyNum()) {
                    origRes[i] = 1.0e6f * origRes[i];
                } else if (unit == Unit.MILES.getLegacyNum()) {
                    origRes[i] = 1.6093e6f * origRes[i];
                }
            } else {
                MipavUtil.displayError("ResUnit[" + i + "] is not a distance unit");
                return false;
            }
        }

        final float[] alpha = new float[3];
        final float[] alpha1 = new float[3];
        final float[] alpha2 = new float[3];
        final float[] beta = new float[3];
        final float[] gamma = new float[3];
        final float[] tmp1 = new float[3];
        final float[] tmp2 = new float[3];
        float norm;

        beta[0] = (inferiorEdgePt.X - superiorEdgePt.X) * origRes[0];
        beta[1] = (inferiorEdgePt.Y - superiorEdgePt.Y) * origRes[1];
        beta[2] = (inferiorEdgePt.Z - superiorEdgePt.Z) * origRes[2];
        norm = (float) Math.sqrt(beta[0] * beta[0] + beta[1] * beta[1] + beta[2] * beta[2]);
        if (norm == 0) {
            MipavUtil.displayError("Error! The AC and PC are too close.");
            return false;
        }
        beta[0] /= norm;
        beta[1] /= norm;
        beta[2] /= norm;

        tmp1[0] = (firstMidSagPt.X - superiorEdgePt.X) * origRes[0];
        tmp1[1] = (firstMidSagPt.Y - superiorEdgePt.Y) * origRes[1];
        tmp1[2] = (firstMidSagPt.Z - superiorEdgePt.Z) * origRes[2];

        alpha1[0] = beta[1] * tmp1[2] - beta[2] * tmp1[1];
        alpha1[1] = beta[2] * tmp1[0] - beta[0] * tmp1[2];
        alpha1[2] = beta[0] * tmp1[1] - beta[1] * tmp1[0];
        norm = (float) Math.sqrt(alpha1[0] * alpha1[0] + alpha1[1] * alpha1[1] + alpha1[2] * alpha1[2]);
        if (norm == 0) {
            MipavUtil.displayError("Error! The AC and SG1 are too close.");
            return false;
        }
        alpha1[0] /= norm;
        alpha1[1] /= norm;
        alpha1[2] /= norm;

        tmp2[0] = (firstMidSagPt.X - superiorEdgePt.X) * origRes[0];
        tmp2[1] = (firstMidSagPt.Y - superiorEdgePt.Y) * origRes[1];
        tmp2[2] = (firstMidSagPt.Z - superiorEdgePt.Z) * origRes[2];

        alpha2[0] = beta[1] * tmp2[2] - beta[2] * tmp2[1];
        alpha2[1] = beta[2] * tmp2[0] - beta[0] * tmp2[2];
        alpha2[2] = beta[0] * tmp2[1] - beta[1] * tmp2[0];
        norm = (float) Math.sqrt(alpha2[0] * alpha2[0] + alpha2[1] * alpha2[1] + alpha2[2] * alpha2[2]);
        if (norm == 0) {
            MipavUtil.displayError("Error! The AC and SG1 are too close.");
            return false;
        }
        alpha2[0] /= norm;
        alpha2[1] /= norm;
        alpha2[2] /= norm;

        alpha[0] = alpha1[0] + alpha2[0];
        alpha[1] = alpha1[1] + alpha2[1];
        alpha[2] = alpha1[2] + alpha2[2];
        norm = (float) Math.sqrt(alpha[0] * alpha[0] + alpha[1] * alpha[1] + alpha[2] * alpha[2]);
        if (norm == 0) {
            MipavUtil.displayError("Error! The AC and SG2 are too close.");
            return false;
        }
        alpha[0] /= norm;
        alpha[1] /= norm;
        alpha[2] /= norm;

        gamma[0] = alpha[1] * beta[2] - alpha[2] * beta[1];
        gamma[1] = alpha[2] * beta[0] - alpha[0] * beta[2];
        gamma[2] = alpha[0] * beta[1] - alpha[1] * beta[0];
        norm = (float) Math.sqrt(gamma[0] * gamma[0] + gamma[1] * gamma[1] + gamma[2] * gamma[2]);
        if (norm == 0) {
            MipavUtil.displayError("Error! The points are all in a straight line.");
            return false;
        }
        gamma[0] /= norm;
        gamma[1] /= norm;
        gamma[2] /= norm;

        norm = (superiorEdgePt.X - posteriorMarginPt.X) * gamma[0] + (superiorEdgePt.Y - posteriorMarginPt.Y)
                * gamma[1] + (superiorEdgePt.Z - posteriorMarginPt.Z) * gamma[2];

        tmp1[0] = posteriorMarginPt.X + norm * gamma[0];
        tmp1[1] = posteriorMarginPt.Y + norm * gamma[1];
        tmp1[2] = posteriorMarginPt.Z + norm * gamma[2];

        norm = (superiorEdgePt.X - posteriorMarginPt.X) * beta[0] + (superiorEdgePt.Y - posteriorMarginPt.Y) * beta[1]
                + (superiorEdgePt.Z - posteriorMarginPt.Z) * beta[2];

        tmp2[0] = superiorEdgePt.X - norm * beta[0];
        tmp2[1] = superiorEdgePt.Y - norm * beta[1];
        tmp2[2] = superiorEdgePt.Z - norm * beta[2];

        origOrient[0][0] = alpha[0];
        origOrient[0][1] = alpha[1];
        origOrient[0][2] = alpha[2];
        origOrient[1][0] = beta[0];
        origOrient[1][1] = beta[1];
        origOrient[1][2] = beta[2];
        origOrient[2][0] = gamma[0];
        origOrient[2][1] = gamma[1];
        origOrient[2][2] = gamma[2];

        origAC.X = 0.5f * (tmp1[0] + tmp2[0]);
        origAC.Y = 0.5f * (tmp1[1] + tmp2[1]);
        origAC.Z = 0.5f * (tmp1[2] + tmp2[2]);

        transform.setOrigOrient(origOrient);
        transform.setOrigAC(origAC);
        transform.setOrigPC(origPC);
        transform.setOrigRes(origRes);
        transform.setOrigDim(origDim);
        transform.setOrigOrigin(image.getFileInfo()[0].getOrigin());
        transform.setUseIncorrectAcpcXDim(useIncorrectAcpcXDim);
        if (useIncorrectAcpcXDim != Preferences.is(Preferences.PREF_USE_INCORRECT_ACPC_XDIM)) {
            Preferences.setProperty(Preferences.PREF_USE_INCORRECT_ACPC_XDIM, String.valueOf(useIncorrectAcpcXDim));
        }
        transform.setAcpcRes(acpcRes);
        final Vector3f pt = new Vector3f();
        transform.origToAcpc(origPC, pt);
        transform.setAcpcPC(pt);
        transform.isAcpc(true);

        ACPCImage = new ModelImage(image.getType(), transform.getAcpcDim(),
                makeImageName(image.getImageName(), "_acpc"));

        final AlgorithmTalairachTransform algo = new AlgorithmTalairachTransform(ACPCImage, image, transform,
                AlgorithmTalairachTransform.ORIG_TO_ACPC, interpolation, true, true);

        createProgressBar(image.getImageName(), algo);

        algo.run();

        ACPCImage.calcMinMax();
        ACPCImage.setImageOrientation(FileInfoBase.AXIAL);

        final int[] acpcOrient = new int[3];
        acpcOrient[0] = FileInfoBase.ORI_R2L_TYPE;
        acpcOrient[1] = FileInfoBase.ORI_A2P_TYPE;
        acpcOrient[2] = FileInfoBase.ORI_I2S_TYPE;
        final float[] res = new float[3];
        res[0] = transform.getAcpcRes();
        res[1] = transform.getAcpcRes();
        res[2] = transform.getAcpcRes();
        final int[] units = new int[3];
        units[0] = Unit.MILLIMETERS.getLegacyNum();
        units[1] = Unit.MILLIMETERS.getLegacyNum();
        units[2] = Unit.MILLIMETERS.getLegacyNum();

        for (int i = 0; i < transform.getAcpcDim()[2]; i++) {
            ACPCImage.getFileInfo(i).setUnitsOfMeasure(units);
            ACPCImage.getFileInfo(i).setResolutions(res);
            ACPCImage.getFileInfo(i).setExtents(transform.getAcpcDim());
            ACPCImage.getFileInfo(i).setAxisOrientation(acpcOrient);
            ACPCImage.getFileInfo(i).setImageOrientation(FileInfoBase.AXIAL);
        }
        setTalairachHeader(ACPCImage);
        setTalairachHeader(image);

        try {
            imageFrame = new ViewJFrameImage(ACPCImage, null, new Dimension(610, 200));
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: unable to open new frame");
        }

        frame.updateImages(true);
        frame.setVisible(false);
        return true;
    }

    /** add the Talairach Transform to the image header */
    public void setTalairachHeader(final ModelImage img) {
        img.setTalairachTransformInfo(transform);
        /*
         * for (int z=0;z<img.getFileInfo().length;z++) {
         * img.getFileInfo()[z].setTransformID(FileInfoBase.TRANSFORM_TALAIRACH_TOURNOUX); }
         */
    }

    /** to convert frame coordinates into the original image ones */
    private Vector3f toOriginal(final Vector3f in) {
        final int[] orient = image.getFileInfo(0).getAxisOrientation();
        final int xDim = image.getExtents()[0];
        final int yDim = image.getExtents()[1];
        final int zDim = image.getExtents()[2];
        final Vector3f out = new Vector3f(0.0f, 0.0f, 0.0f);

        switch (orient[0]) {
            case FileInfoBase.ORI_R2L_TYPE:
                out.X = in.X;
                break;

            case FileInfoBase.ORI_L2R_TYPE:
                out.X = xDim - 1 - in.X;
                break;

            case FileInfoBase.ORI_A2P_TYPE:
                out.X = in.Y;
                break;

            case FileInfoBase.ORI_P2A_TYPE:
                out.X = xDim - 1 - in.Y;
                break;

            case FileInfoBase.ORI_I2S_TYPE:
                out.X = in.Z;
                break;

            case FileInfoBase.ORI_S2I_TYPE:
                out.X = xDim - 1 - in.Z;
                break;
        }

        switch (orient[1]) {
            case FileInfoBase.ORI_R2L_TYPE:
                out.Y = in.X;
                break;

            case FileInfoBase.ORI_L2R_TYPE:
                out.Y = yDim - 1 - in.X;
                break;

            case FileInfoBase.ORI_A2P_TYPE:
                out.Y = in.Y;
                break;

            case FileInfoBase.ORI_P2A_TYPE:
                out.Y = yDim - 1 - in.Y;
                break;

            case FileInfoBase.ORI_I2S_TYPE:
                out.Y = in.Z;
                break;

            case FileInfoBase.ORI_S2I_TYPE:
                out.Y = yDim - 1 - in.Z;
                break;
        }

        switch (orient[2]) {
            case FileInfoBase.ORI_R2L_TYPE:
                out.Z = in.X;
                break;

            case FileInfoBase.ORI_L2R_TYPE:
                out.Z = zDim - 1 - in.X;
                break;

            case FileInfoBase.ORI_A2P_TYPE:
                out.Z = in.Y;
                break;

            case FileInfoBase.ORI_P2A_TYPE:
                out.Z = zDim - 1 - in.Y;
                break;

            case FileInfoBase.ORI_I2S_TYPE:
                out.Z = in.Z;
                break;

            case FileInfoBase.ORI_S2I_TYPE:
                out.Z = zDim - 1 - in.Z;
                break;
        }

        return out;
    }

    /**
     * Finds the distance between two points based on resolution.
     * 
     * @param pt1 First point.
     * @param pt2 Second point.
     * @param resol Resolutions of each dimension.
     */
    @SuppressWarnings("unused")
    private float dist(final Vector3f pt1, final Vector3f pt2, final float[] resol) {
        float distX, distY, distZ;
        float length;
        distX = (pt1.X - pt2.X) * resol[0];
        distX = distX * distX;
        distY = (pt1.Y - pt2.Y) * resol[1];
        distY = distY * distY;
        distZ = (pt1.Z - pt2.Z) * resol[2];
        distZ = distZ * distZ;
        length = (float) Math.sqrt(distX + distY + distZ);
        return length;
    }

    /**
     * Subtracts one vector from another and returns result. result = pt1 - pt2.
     * 
     * @param pt1 Vector to subtract from.
     * @param pt2 Vector to be subtracted.
     * @return pt1 - pt2
     */
    @SuppressWarnings("unused")
    private Vector3f sub(final Vector3f pt1, final Vector3f pt2) {
        final Vector3f pt = new Vector3f(0.0f, 0.0f, 0.0f);
        pt.X = pt1.X - pt2.X;
        pt.Y = pt1.Y - pt2.Y;
        pt.Z = pt1.Z - pt2.Z;
        return pt;
    }

    /**
     * Finds the normal to the vector.
     * 
     * @param pt Vector to find normal to.
     * @return Normal of pt.
     */
    @SuppressWarnings("unused")
    private Vector3f norm(final Vector3f pt) {
        float scale;
        final Vector3f normPt = new Vector3f(0.0f, 0.0f, 0.0f);
        scale = pt.X * pt.X + pt.Y * pt.Y + pt.Z * pt.Z;
        scale = (float) ( (scale > 0) ? (1.0 / Math.sqrt(scale)) : 0);
        normPt.X = pt.X * scale;
        normPt.Y = pt.Y * scale;
        normPt.Z = pt.Z * scale;
        return normPt;

    }

    /**
     * Finds crossproduct of two vectors.
     * 
     * @param pt1 First vector.
     * @param pt2 Second vector
     * @return Cross product of pt1 and pt2.
     */
    @SuppressWarnings("unused")
    private Vector3f crossProduct(final Vector3f pt1, final Vector3f pt2) {
        final Vector3f crossPt = new Vector3f(0.0f, 0.0f, 0.0f);
        crossPt.X = pt1.Y * pt2.Z - pt1.Z * pt2.Y;
        crossPt.Y = pt1.Z * pt2.X - pt1.X * pt2.Z;
        crossPt.Z = pt1.X * pt2.Y - pt1.Y * pt2.X;
        return crossPt;
    }

    /**
     * Finds dotproduct of two vectors.
     * 
     * @param pt1 First vector.
     * @param pt2 Second vector
     * @return Dot product of pt1 and pt2.
     */
    @SuppressWarnings("unused")
    private float dotProduct(final Vector3f pt1, final Vector3f pt2) {
        float dot;
        dot = pt1.X * pt2.X + pt1.Y * pt2.Y + pt1.Z * pt2.Z;
        return dot;
    }

    /**
     * Scale and add two vectors.
     * 
     * @param fa Scale for vector a.
     * @param a Vector a.
     * @param fb Scale for vector b.
     * @param b Vector b.
     */
    @SuppressWarnings("unused")
    private Vector3f sclAdd(final float fa, final Vector3f a, final float fb, final Vector3f b) {
        final Vector3f pt = new Vector3f(0.0f, 0.0f, 0.0f);
        pt.X = fa * a.X + fb * b.X;
        pt.Y = fa * a.Y + fb * b.Y;
        pt.Z = fa * a.Z + fb * b.Z;
        return pt;
    }

    /**
     * Makes a mm (physical space) point from a pixel space point.
     * 
     * @param pt Point to convert.
     * @param resol Resolutions to use when converting.
     * @return Same point in mm.
     */
    @SuppressWarnings("unused")
    private Vector3f makemmVector3f(final Vector3f pt, final float[] resol) {
        final Vector3f mmPt = new Vector3f(0.0f, 0.0f, 0.0f);
        mmPt.X = resol[0] * pt.X;
        mmPt.Y = resol[1] * pt.Y;
        mmPt.Z = resol[2] * pt.Z;
        return mmPt;
    }

    /**
     * Makes a pixel space point from a physical space point.
     * 
     * @param pt Point to convert.
     * @param resol Resolutions to use when converting.
     * @return Same point in pixel space.
     */
    @SuppressWarnings("unused")
    private Vector3f makeVoxelCoord3Df(final Vector3f pt, final float[] resol) {
        final Vector3f voxelPt = new Vector3f(0.0f, 0.0f, 0.0f);
        voxelPt.X = pt.X / resol[0];
        voxelPt.Y = pt.Y / resol[1];
        voxelPt.Z = pt.Z / resol[2];
        return voxelPt;
    }

    /**
     * Transforms and resamples volume using trilinear interpolation
     * 
     * @param image Image.
     * @param imgBuffer Image array.
     * @param xfrm Transformation matrix to be applied.
     * @param iXres In X resolution.
     * @param iYres In Y resolution.
     * @param iZres In Z resolution.
     * @param iXdim In X dimension.
     * @param iYdim In Y dimension.
     * @param iZdim In Z dimension.
     * @param oXres Out X resolution.
     * @param oYres Out Y resolution.
     * @param oZres Out Z resolution.
     * @param oXdim Out X dimension.
     * @param oYdim Out Y dimension.
     * @param oZdim Out Z dimension.
     * @param oXlow Out X low.
     * @param oYlow Out Y low.
     * @param oZlow Out Z low.
     * @param oXhigh Out X high.
     * @param oYhigh Out Y high.
     * @param oZhigh Out Z high.
     * @param progressBar Progress bar.
     */
    @SuppressWarnings("unused")
    private void transformACPCTrilinear(final ModelImage image, final float imgBuffer[], final double xfrm[][],
            final float iXres, final float iYres, final float iZres, final int iXdim, final int iYdim, final int iZdim,
            final float oXres, final float oYres, final float oZres, final int oXdim, final int oYdim, final int oZdim,
            final ViewJProgressBar progressBar) {
        int i, j, k;
        int X0pos, Y0pos, Z0pos;
        int X1pos, Y1pos, Z1pos;
        float X, Y, Z;
        float x0, y0, z0;
        float x1, y1, z1;
        float value;
        int sliceSize;
        float imm, jmm, kmm;
        float i1, i2, i3, j1, j2, j3;
        float temp1, temp2, temp3, temp4, temp5, temp6, temp7;
        int roundX, roundY, roundZ;
        sliceSize = iXdim * iYdim;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        final int mod = oXdim / 50;

        T00 = (float) xfrm[0][0];
        T01 = (float) xfrm[0][1];
        T02 = (float) xfrm[0][2];
        T03 = (float) xfrm[0][3];
        T10 = (float) xfrm[1][0];
        T11 = (float) xfrm[1][1];
        T12 = (float) xfrm[1][2];
        T13 = (float) xfrm[1][3];
        T20 = (float) xfrm[2][0];
        T21 = (float) xfrm[2][1];
        T22 = (float) xfrm[2][2];
        T23 = (float) xfrm[2][3];

        for (i = 0; i < oXdim; i++) {
            if (i % mod == 0) {
                progressBar.updateValue((int) ( ((float) i / (oXdim - 1)) * 100 + .5), false);
            }
            imm = i * oXres;
            i1 = imm * T00 + T03;
            i2 = imm * T10 + T13;
            i3 = imm * T20 + T23;
            for (j = 0; j < oYdim; j++) {
                jmm = j * oYres;
                j1 = jmm * T01;
                j2 = jmm * T11;
                j3 = jmm * T21;
                temp1 = i3 + j3;
                temp2 = i2 + j2;
                temp3 = i1 + j1;
                for (k = 0; k < oZdim; k++) {
                    // transform i,j,k
                    value = 0; // remains zero if voxel is transformed out of bounds
                    kmm = k * oZres;
                    X = (temp3 + (kmm * T02)) / iXres;
                    roundX = (int) (X + 0.5f);
                    if ( (X >= 0) && (roundX < iXdim)) {
                        Y = (temp2 + (kmm * T12)) / iYres;
                        roundY = (int) (Y + 0.5f);
                        if ( (Y >= 0) && (roundY < iYdim)) {
                            Z = (temp1 + (kmm * T22)) / iZres;
                            roundZ = (int) (Z + 0.5f);
                            if ( (Z >= 0) && (roundZ < iZdim)) {
                                if ( (roundX == iXdim - 1) || (roundY == iYdim - 1) || (roundZ == iZdim - 1)) {
                                    X0pos = roundX;
                                    Y0pos = roundY * iXdim;
                                    Z0pos = roundZ * sliceSize;
                                    value = imgBuffer[Z0pos + Y0pos + X0pos];
                                } else {
                                    // set intensity of i,j,k to new transformed coordinate if
                                    // x,y,z is w/in dimensions of image
                                    x0 = X - (int) X;
                                    y0 = Y - (int) Y;
                                    z0 = Z - (int) Z;
                                    x1 = 1 - x0;
                                    y1 = 1 - y0;
                                    z1 = 1 - z0;
                                    X0pos = (int) X;
                                    Y0pos = (int) Y * iXdim;
                                    Z0pos = (int) Z * sliceSize;
                                    X1pos = X0pos + 1;
                                    Y1pos = Y0pos + iXdim;
                                    Z1pos = Z0pos + sliceSize;
                                    temp4 = y1 * z1;
                                    temp5 = y0 * z1;
                                    temp6 = y1 * z0;
                                    temp7 = y0 * z0;
                                    value = x1 * temp4 * imgBuffer[Z0pos + Y0pos + X0pos] + x0 * temp4
                                            * imgBuffer[Z0pos + Y0pos + X1pos] + x1 * temp5
                                            * imgBuffer[Z0pos + Y1pos + X0pos] + x0 * temp5
                                            * imgBuffer[Z0pos + Y1pos + X1pos] + x1 * temp6
                                            * imgBuffer[Z1pos + Y0pos + X0pos] + x0 * temp6
                                            * imgBuffer[Z1pos + Y0pos + X1pos] + x1 * temp7
                                            * imgBuffer[Z1pos + Y1pos + X0pos] + x0 * temp7
                                            * imgBuffer[Z1pos + Y1pos + X1pos];
                                }
                            }// end if Z in bounds
                        }// end if Y in bounds
                    }// end if X in bounds
                    image.set(i, j, k, value);
                }// end for k
            }// end for j
        }// end for i
    }

    public ModelImage getACPCImage() {
        return ACPCImage;
    }
}
