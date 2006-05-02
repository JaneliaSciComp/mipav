package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.swing.*;


/**
 * Dialog to enter points for creating an ACPC image.
 */
public class JDialogACPC extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -117566377286711649L;

    /** DOCUMENT ME! */
    private static final float ATLAS_ALIGNBOX_LAT = 95.0f; /* dimensions in mm. used for AC-PC */

    /** DOCUMENT ME! */
    private static final float ATLAS_ALIGNBOX_ANT = 95.0f; /* aligned view clipping box */

    /** DOCUMENT ME! */
    private static final float ATLAS_ALIGNBOX_INF = 70.0f;

    /** DOCUMENT ME! */
    private static final float ATLAS_ALIGNBOX_POS = 140.0f; /* Maximum distances allowed above and */

    /** DOCUMENT ME! */
    private static final float ATLAS_ALIGNBOX_SUP = 100.0f; /* below Talairach zero point */

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ButtonGroup ACPCGroup;

    /** DOCUMENT ME! */
    private JRadioButton anotherPt;

    /** DOCUMENT ME! */
    private Point3Df anotherPtDicom;

    /** DOCUMENT ME! */
    private JButton applyACPCButton;

    /** DOCUMENT ME! */
    private JButton cancelACPCButton;

    /** DOCUMENT ME! */
    private JButton clearACPCButton;

    /** DOCUMENT ME! */
    private JComboBox comboBoxOrientX, comboBoxOrientY, comboBoxOrientZ;

    /** DOCUMENT ME! */
    private JRadioButton firstPt;

    /** DOCUMENT ME! */
    private Point3Df firstPtDicom;

    /** DOCUMENT ME! */
    private ViewJFrameTriImage frame;

    /** DOCUMENT ME! */
    private boolean haveAnotherPt = false;

    /** DOCUMENT ME! */
    private boolean haveFirstPt = false;

    /** DOCUMENT ME! */
    private boolean haveInferiorEdge = false;

    /** DOCUMENT ME! */
    private boolean havePosteriorMargin = false;

    /** DOCUMENT ME! */
    private boolean haveSuperiorEdge = false;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private JRadioButton inferiorEdge;

    /** DOCUMENT ME! */
    private Point3Df inferiorEdgeDicom;

    /** DOCUMENT ME! */
    private int[] orient;

    /** DOCUMENT ME! */
    private Point3Df originalPosteriorMargin;

    /** DOCUMENT ME! */
    private Point3Df originalSuperiorEdge;

    /** DOCUMENT ME! */
    private JRadioButton posteriorMargin;

    /** DOCUMENT ME! */
    private Point3Df posteriorMarginDicom;

    /** DOCUMENT ME! */
    private JButton setACPCButton;

    /** DOCUMENT ME! */
    private JRadioButton superiorEdge;

    /** DOCUMENT ME! */
    private Point3Df superiorEdgeDicom;

    /** DOCUMENT ME! */
    private JTextField textVoxelLength;

    /** DOCUMENT ME! */
    private float voxelLength;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * This method creates a dialog for selecting markers used for generating an AC-PC aligned view image from an
     * original image.
     *
     * @param  theParentFrame  Pointer to the frame that created this dialog.
     * @param  im              Pointer to image represented in frame.
     */
    public JDialogACPC(ViewJFrameTriImage theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        frame = theParentFrame;
        orient = im.getFileInfo(0).getAxisOrientation();
        image = im;
        voxelLength = 1;
        init();

        Point3Df pt = new Point3Df(Float.POSITIVE_INFINITY, Float.POSITIVE_INFINITY, Float.POSITIVE_INFINITY);

        if (image.getFileInfo()[0].getFileFormat() == FileBase.AFNI) {
            pt = ((FileInfoAfni) image.getFileInfo()[0]).getSuperiorEdge();
        }

        if (pt.x != Float.POSITIVE_INFINITY) {
            pt = frame.toDicom(pt, im);
            ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).setReferenceXY(ViewJComponentTriImage.SUPERIOR_EDGE,
                                                                                                    pt);
            setSuperiorEdge(pt);
        }

        if (image.getFileInfo()[0].getFileFormat() == FileBase.AFNI) {
            pt = ((FileInfoAfni) image.getFileInfo()[0]).getPosteriorMargin();
        }

        if (pt.x != Float.POSITIVE_INFINITY) {
            pt = frame.toDicom(pt, im);
            ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).setReferenceXY(ViewJComponentTriImage.POSTERIOR_MARGIN,
                                                                                                    pt);
            setPosteriorMargin(pt);
        }

        if (image.getFileInfo()[0].getFileFormat() == FileBase.AFNI) {
            pt = ((FileInfoAfni) image.getFileInfo()[0]).getInferiorEdge();
        }

        if (pt.x != Float.POSITIVE_INFINITY) {
            pt = frame.toDicom(pt, im);
            ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).setReferenceXY(ViewJComponentTriImage.INFERIOR_EDGE,
                                                                                                    pt);
            setInferiorEdge(pt);
        }

        if (image.getFileInfo()[0].getFileFormat() == FileBase.AFNI) {
            pt = ((FileInfoAfni) image.getFileInfo()[0]).getFirstPt();
        }

        if (pt.x != Float.POSITIVE_INFINITY) {
            pt = frame.toDicom(pt, im);
            ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).setReferenceXY(ViewJComponentTriImage.FIRST_PT,
                                                                                                    pt);
            setFirstPt(pt);
        }

        if (image.getFileInfo()[0].getFileFormat() == FileBase.AFNI) {
            pt = ((FileInfoAfni) image.getFileInfo()[0]).getAnotherPt();
        }

        if (pt.x != Float.POSITIVE_INFINITY) {
            pt = frame.toDicom(pt, im);
            ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).setReferenceXY(ViewJComponentTriImage.ANOTHER_PT,
                                                                                                    pt);
            setAnotherPt(pt);
        }

        pack();
        setVisible(true);
    }

    /**
     * This method creates a dialog for selecting markers used for generating an AC-PC aligned view image from an
     * original image.
     *
     * @param  theParentFrame  Pointer to the frame that created this dialog.
     * @param  im              Pointer to image represented in frame.
     * @param  isVisible       The dialog visible or not.
     */
    public JDialogACPC(ViewJFrameTriImage theParentFrame, ModelImage im, boolean isVisible) {
        super(theParentFrame, false);
        frame = theParentFrame;
        orient = im.getFileInfo(0).getAxisOrientation();
        image = im;
        voxelLength = 1;
        init();

        Point3Df pt = new Point3Df(Float.POSITIVE_INFINITY, Float.POSITIVE_INFINITY, Float.POSITIVE_INFINITY);

        if (image.getFileInfo()[0].getFileFormat() == FileBase.AFNI) {
            pt = ((FileInfoAfni) image.getFileInfo()[0]).getSuperiorEdge();
        }

        if (pt.x != Float.POSITIVE_INFINITY) {
            pt = frame.toDicom(pt, im);
            ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).setReferenceXY(ViewJComponentTriImage.SUPERIOR_EDGE,
                                                                                                    pt);
            setSuperiorEdge(pt);
        }

        if (image.getFileInfo()[0].getFileFormat() == FileBase.AFNI) {
            pt = ((FileInfoAfni) image.getFileInfo()[0]).getPosteriorMargin();
        }

        if (pt.x != Float.POSITIVE_INFINITY) {
            pt = frame.toDicom(pt, im);
            ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).setReferenceXY(ViewJComponentTriImage.POSTERIOR_MARGIN,
                                                                                                    pt);
            setPosteriorMargin(pt);
        }

        if (image.getFileInfo()[0].getFileFormat() == FileBase.AFNI) {
            pt = ((FileInfoAfni) image.getFileInfo()[0]).getInferiorEdge();
        }

        if (pt.x != Float.POSITIVE_INFINITY) {
            pt = frame.toDicom(pt, im);
            ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).setReferenceXY(ViewJComponentTriImage.INFERIOR_EDGE,
                                                                                                    pt);
            setInferiorEdge(pt);
        }

        if (image.getFileInfo()[0].getFileFormat() == FileBase.AFNI) {
            pt = ((FileInfoAfni) image.getFileInfo()[0]).getFirstPt();
        }

        if (pt.x != Float.POSITIVE_INFINITY) {
            pt = frame.toDicom(pt, im);
            ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).setReferenceXY(ViewJComponentTriImage.FIRST_PT,
                                                                                                    pt);
            setFirstPt(pt);
        }

        if (image.getFileInfo()[0].getFileFormat() == FileBase.AFNI) {
            pt = ((FileInfoAfni) image.getFileInfo()[0]).getAnotherPt();
        }

        if (pt.x != Float.POSITIVE_INFINITY) {
            pt = frame.toDicom(pt, im);
            ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).setReferenceXY(ViewJComponentTriImage.ANOTHER_PT,
                                                                                                    pt);
            setAnotherPt(pt);
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * If user clicks "Set", sets point here and in component image. If user clicks "Clear", clears point here and in
     * component image. If user clicks "Apply", creates new Talairach image based on points. If user clicks "Cancel",
     * disposes this dialog.
     *
     * @param  event  Event that triggered this method.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        boolean found, success;
        int pointType;
        Point3Df pt;

        if (command.equals("setACPC")) {
            found = false;
            pt = new Point3Df(frame.getSagittalComponentSlice(), frame.getCoronalComponentSlice(),
                              frame.getAxialComponentSlice());

            if (superiorEdge.isSelected()) {
                pointType = ViewJComponentTriImage.SUPERIOR_EDGE;
                ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).removeReference("ACS");
                setSuperiorEdge(pt);
            } else if (posteriorMargin.isSelected()) {
                pointType = ViewJComponentTriImage.POSTERIOR_MARGIN;
                ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).removeReference("ACP");
                setPosteriorMargin(pt);
            } else if (inferiorEdge.isSelected()) {
                pointType = ViewJComponentTriImage.INFERIOR_EDGE;
                ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).removeReference("PC");
                setInferiorEdge(pt);
            } else if (firstPt.isSelected()) {
                pointType = ViewJComponentTriImage.FIRST_PT;
                ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).removeReference("MS1");
                setFirstPt(pt);
            } else {
                pointType = ViewJComponentTriImage.ANOTHER_PT;
                ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).removeReference("MS2");
                setAnotherPt(pt);
            }

            ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).setReferenceXY(pointType, pt);
        } else if (command.equals("clearACPC")) {

            if (superiorEdge.isSelected()) {
                found = ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).removeReference("ACS");

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
                found = ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).removeReference("ACP");

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
                found = ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).removeReference("PC");

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
                found = ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).removeReference("MS1");

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
                found = ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).removeReference("MS2");

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
        } else if (command.equals("applyACPC")) {

            if (orient[0] == FileInfoBase.ORI_UNKNOWN_TYPE) {
                success = getOrient();
            } else {
                success = true;
            }

            if (success) {
                String tmpStr = textVoxelLength.getText();
                voxelLength = Float.parseFloat(tmpStr);

                if (voxelLength < 0.0) {
                    MipavUtil.displayError("kernelDiameter must be greater than 0");
                    textVoxelLength.requestFocus();
                    textVoxelLength.selectAll();

                    return;
                }

                setVisible(false);

                if (!convertToACPC()) {
                    setVisible(true);
                }
            }
        } else if (command.equals("cancelACPC")) {
            dispose();
        }
    }

    /**
     * Get the dialog main panel.
     *
     * @return  DOCUMENT ME!
     */
    public JPanel getMainPanel() {
        JPanel pointPanel;
        JPanel orientPanel = null;
        JPanel voxelPanel;
        JLabel labelVoxelLength;
        setTitle("Create AC-PC image");

        if (orient[0] == FileInfoBase.ORI_UNKNOWN_TYPE) {
            orientPanel = new JPanel(new GridLayout(3, 2));
            orientPanel.setForeground(Color.black);
            orientPanel.setBorder(buildTitledBorder("Describe input orientation"));

            JLabel labelOrientX = new JLabel("X axis:");
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

            JLabel labelOrientY = new JLabel("Y axis:");
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

            JLabel labelOrientZ = new JLabel("Z axis:");
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

        setACPCButton = new JButton("Set");
        setACPCButton.setFont(serif12B);
        setACPCButton.addActionListener(this);
        setACPCButton.setActionCommand("setACPC");
        setACPCButton.setPreferredSize(MipavUtil.defaultButtonSize);

        clearACPCButton = new JButton("Clear");
        clearACPCButton.setFont(serif12B);
        clearACPCButton.addActionListener(this);
        clearACPCButton.setActionCommand("clearACPC");
        clearACPCButton.setEnabled(false);
        clearACPCButton.setPreferredSize(MipavUtil.defaultButtonSize);

        applyACPCButton = new JButton("Apply");
        applyACPCButton.setFont(serif12B);
        applyACPCButton.addActionListener(this);
        applyACPCButton.setActionCommand("applyACPC");
        applyACPCButton.setEnabled(false);
        applyACPCButton.setPreferredSize(MipavUtil.defaultButtonSize);

        cancelACPCButton = new JButton("Cancel");
        cancelACPCButton.setFont(serif12B);
        cancelACPCButton.addActionListener(this);
        cancelACPCButton.setActionCommand("cancelACPC");
        cancelACPCButton.setPreferredSize(MipavUtil.defaultButtonSize);

        JPanel buttonPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        buttonPanel.add(setACPCButton, gbc);
        gbc.gridx = 1;
        buttonPanel.add(clearACPCButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        buttonPanel.add(applyACPCButton, gbc);
        gbc.gridx = 1;
        // buttonPanel.add(cancelACPCButton, gbc);

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;

        int y = 0;

        if (orient[0] == FileInfoBase.ORI_UNKNOWN_TYPE) {
            mainPanel.add(orientPanel, gbc);
            gbc.gridy = 1;
            y = 1;
        }

        mainPanel.add(voxelPanel, gbc);
        gbc.gridy = y + 1;
        mainPanel.add(pointPanel, gbc);

        JPanel panel = new JPanel();
        panel.add(mainPanel);
        panel.add(buttonPanel, BorderLayout.SOUTH);

        return panel;
    }

    /**
     * Converts image to AC-PC image. Returns flag indicating success.
     *
     * @return  <code>true</code> if successful conversion.
     */
    private boolean convertToACPC() {
        int newXDim, newYDim, newZDim, newSliceSize, xyzSize;
        int newX = 0, newY = 0, newZ = 0;
        ViewJProgressBar progressBar;

        Point3Df rr, beta, alpha1, alpha2, alpha, dif, rr1, rr2, acpos, acsup, gamma, AFNIrr;
        Point3Df translation = new Point3Df(0.0f, 0.0f, 0.0f);
        Point3Df AFNIbvec = new Point3Df(0.0f, 0.0f, 0.0f);
        Point3Df AFNIsvec = new Point3Df(0.0f, 0.0f, 0.0f);

        // float magalpha, magbeta, maggamma, magabgx, magabgy, magabgz;
        float costheta, theta;

        // double thetaX, thetaY, thetaZ;
        Point3Df center;
        float oXres, oYres, oZres;
        float[] oRes = new float[3];
        int xDim = image.getExtents()[0];
        int yDim = image.getExtents()[1];
        int zDim = image.getExtents()[2];
        int sliceSize;
        int oXdim, oYdim, oZdim;

        // double tempf;
        // Point3Df bvec = new Point3Df(0.0f,0.0f,0.0f);
        ModelImage ACPCImage = null;
        ViewJFrameImage imageFrame;
        double[][] Mat;
        Point3Df TCenter = new Point3Df(0.0f, 0.0f, 0.0f);
        int[] MeasureUnits = new int[5];
        int[] newOrient = new int[3];

        // The coordinate transformed posterior commissure inferior edge
        // Must be stored in fileInfo for the +acpc to +tlrc transformation
        Point3Df pcie = new Point3Df(0.0f, 0.0f, 0.0f);

        // The millimeter distance of the Talairach center in the +acpc image
        Point3Df Tal = new Point3Df(ATLAS_ALIGNBOX_LAT, ATLAS_ALIGNBOX_ANT, ATLAS_ALIGNBOX_INF);
        double Tr03, Tr13, Tr23;
        Matrix Ac;
        Matrix X;
        Matrix b;
        double Tx, Ty, Tz;
        float lowX = 0, lowY = 0, lowZ = 0, highX = 0, highY = 0, highZ = 0;
        float dicomLowX = 0.0f;
        float dicomLowY = 0.0f;
        float dicomLowZ = 0.0f;
        float dicomHighX = 0.0f;
        float dicomHighY = 0.0f;
        float dicomHighZ = 0.0f;
        FileInfoAfni[] fileInfo;
        float[] orgResol = new float[3];
        float[] mmResol = new float[3];

        newXDim = xDim;
        newYDim = yDim;
        newZDim = zDim;

        /* Determine the new resolutions */
        for (int i = 0; i <= 2; i++) {
            int unit = image.getFileInfo(0).getUnitsOfMeasure(i);

            if ((unit == FileInfoBase.INCHES) || (unit == FileInfoBase.CENTIMETERS) ||
                    (unit == FileInfoBase.ANGSTROMS) || (unit == FileInfoBase.NANOMETERS) ||
                    (unit == FileInfoBase.MICROMETERS) || (unit == FileInfoBase.MILLIMETERS) ||
                    (unit == FileInfoBase.METERS) || (unit == FileInfoBase.KILOMETERS) ||
                    (unit == FileInfoBase.MILES)) {
                orgResol[i] = image.getFileInfo(0).getResolutions()[i];

                if (orgResol[i] <= 0.0f) {
                    MipavUtil.displayWarning("resolution[" + i + "] was recorded as " + orgResol[i] +
                                             " It is being changed to 1.0");
                    orgResol[i] = 1.0f;
                }

                // Be ready for conversions between different units.
                if (unit == FileInfoBase.MILLIMETERS) {
                    mmResol[i] = orgResol[i];
                } else if (unit == FileInfoBase.INCHES) {
                    mmResol[i] = 25.4f * orgResol[i];
                } else if (unit == FileInfoBase.CENTIMETERS) {
                    mmResol[i] = 10.0f * orgResol[i];
                } else if (unit == FileInfoBase.ANGSTROMS) {
                    mmResol[i] = 1.0e-7f * orgResol[i];
                } else if (unit == FileInfoBase.NANOMETERS) {
                    mmResol[i] = 1.0e-6f * orgResol[i];
                } else if (unit == FileInfoBase.MICROMETERS) {
                    mmResol[i] = 1.0e-3f * orgResol[i];
                } else if (unit == FileInfoBase.METERS) {
                    mmResol[i] = 1.0e3f * orgResol[i];
                } else if (unit == FileInfoBase.KILOMETERS) {
                    mmResol[i] = 1.0e6f * orgResol[i];
                } else if (unit == FileInfoBase.MILES) {
                    mmResol[i] = 1.6093e6f * orgResol[i];
                }
            } else {
                MipavUtil.displayError("ResUnit[" + i + "] is not a distance unit");

                return false;
            }
        } // for (i = 0; i <= 2; i++)

        if (image.getFileInfo()[0].getFileFormat() == FileBase.AFNI) {
            lowX = ((FileInfoAfni) image.getFileInfo()[0]).getLowXmm();
            lowY = ((FileInfoAfni) image.getFileInfo()[0]).getLowYmm();
            lowZ = ((FileInfoAfni) image.getFileInfo()[0]).getLowZmm();
            highX = ((FileInfoAfni) image.getFileInfo()[0]).getHighXmm();
            highY = ((FileInfoAfni) image.getFileInfo()[0]).getHighYmm();
            highZ = ((FileInfoAfni) image.getFileInfo()[0]).getHighZmm();
        }

        switch (orient[0]) {

            case FileInfoBase.ORI_R2L_TYPE:
            case FileInfoBase.ORI_L2R_TYPE:
                orgResol[0] = mmResol[0];
                newXDim = xDim;
                dicomLowX = lowX;
                dicomHighX = highX;
                break;

            case FileInfoBase.ORI_P2A_TYPE:
            case FileInfoBase.ORI_A2P_TYPE:
                orgResol[1] = mmResol[0];
                newYDim = xDim;
                dicomLowY = lowX;
                dicomHighY = highX;
                break;

            case FileInfoBase.ORI_I2S_TYPE:
            case FileInfoBase.ORI_S2I_TYPE:
                orgResol[2] = mmResol[0];
                newZDim = xDim;
                dicomLowZ = lowX;
                dicomHighZ = highX;
                break;
        }

        switch (orient[1]) {

            case FileInfoBase.ORI_R2L_TYPE:
            case FileInfoBase.ORI_L2R_TYPE:
                orgResol[0] = mmResol[1];
                newXDim = yDim;
                dicomLowX = lowY;
                dicomHighX = highY;
                break;

            case FileInfoBase.ORI_P2A_TYPE:
            case FileInfoBase.ORI_A2P_TYPE:
                orgResol[1] = mmResol[1];
                newYDim = yDim;
                dicomLowY = lowY;
                dicomHighY = highY;
                break;

            case FileInfoBase.ORI_I2S_TYPE:
            case FileInfoBase.ORI_S2I_TYPE:
                orgResol[2] = mmResol[1];
                newZDim = yDim;
                dicomLowZ = lowY;
                dicomHighZ = highY;
                break;
        }

        switch (orient[2]) {

            case FileInfoBase.ORI_R2L_TYPE:
            case FileInfoBase.ORI_L2R_TYPE:
                orgResol[0] = mmResol[2];
                newXDim = zDim;
                dicomLowX = lowZ;
                dicomHighX = highZ;
                break;

            case FileInfoBase.ORI_P2A_TYPE:
            case FileInfoBase.ORI_A2P_TYPE:
                orgResol[1] = mmResol[2];
                newYDim = zDim;
                dicomLowY = lowZ;
                dicomHighY = highZ;
                break;

            case FileInfoBase.ORI_I2S_TYPE:
            case FileInfoBase.ORI_S2I_TYPE:
                orgResol[2] = mmResol[2];
                newZDim = zDim;
                dicomLowZ = lowZ;
                dicomHighZ = highZ;
                break;
        }

        sliceSize = xDim * yDim;
        newSliceSize = newXDim * newYDim;

        /* compute the new y direction (beta) */
        beta = sub(inferiorEdgeDicom, superiorEdgeDicom);
        beta = makemmPoint3Df(beta, orgResol);
        beta = norm(beta);

        /* Compute the new x direction (alpha) */
        rr = sub(firstPtDicom, superiorEdgeDicom);
        rr = makemmPoint3Df(rr, orgResol);
        alpha1 = crossProduct(beta, rr);
        alpha1 = norm(alpha1);

        rr = sub(anotherPtDicom, superiorEdgeDicom);
        rr = makemmPoint3Df(rr, orgResol);
        alpha2 = crossProduct(beta, rr);
        alpha2 = norm(alpha2);

        float size = dotProduct(alpha1, alpha2); /* angle < 2 degrees */

        if (size < 0.99939) { /* size = cos(angle) */
            MipavUtil.displayError("Error! The AC + PC + mid-sag points do not form a good plane.");

            return false;
        }

        size = (float) (Math.acos((double) size) * 180.0 / Math.PI); /* report angle */
        Preferences.debug("Angular deviation between AC+PC+mid-sag pts: " + size + " degrees\n");

        alpha = sclAdd(0.5f, alpha1, 0.5f, alpha2);
        alpha = norm(alpha);

        /*  compute the new z direction (gamma) */
        gamma = crossProduct(alpha, beta);
        gamma = norm(gamma);

        /* Now consider the ray from the AC posterior margin (posteriorMarginDicom)
         * in the gamma direction, and the ray from the AC superior edge (superiorEdgeDicom) in the beta direction.
         * Nominally, these rays should intersect.  Find their points of closest approach (rr1,rr2).  The average of
         * these is the Talairach center of coordinates (rr). */

        dif = sub(superiorEdgeDicom, posteriorMarginDicom);
        dif = makemmPoint3Df(dif, orgResol);
        size = dotProduct(dif, gamma);
        acpos = makemmPoint3Df(posteriorMarginDicom, orgResol);
        rr1 = sclAdd(1.0f, acpos, size, gamma);

        size = dotProduct(dif, beta);
        acsup = makemmPoint3Df(superiorEdgeDicom, orgResol);
        rr2 = sclAdd(1.0f, acsup, -size, beta);

        size = dist(rr1, rr2, orgResol);

        if (size > 2.0) {
            MipavUtil.displayError("Error! AC Talairach origin mismatch more than 2 mm");

            return false;
        }

        /* Reorder the data from dataset order to dicom order unless the data is already in dicom order */
        int[] newExtents = new int[3];
        newExtents[0] = newXDim;
        newExtents[1] = newYDim;
        newExtents[2] = newZDim;

        if ((orient[0] != FileInfoBase.ORI_R2L_TYPE) || (orient[1] != FileInfoBase.ORI_A2P_TYPE) ||
                (orient[2] != FileInfoBase.ORI_I2S_TYPE)) {
            progressBar = new ViewJProgressBar(image.getFileInfo()[0].getFileName(),
                                               "Reordering image to dicom order ...", 0, 100, false, null, null);
            progressBar.setLocation(Toolkit.getDefaultToolkit().getScreenSize().width / 2, 50);
            progressBar.setVisible(true);
            ACPCImage = new ModelImage(image.getFileInfo()[0].getDataType(), newExtents,
                                       image.getFileInfo()[0].getFileName(), image.getUserInterface());

            for (int i = 0; i < newZDim; i++) {
                ACPCImage.getFileInfo(i).setResolutions(orgResol);
            }

            xyzSize = xDim * yDim * zDim;

            float[] datasetBuffer = new float[xyzSize];

            try {
                image.exportData(0, xyzSize, datasetBuffer);
            } catch (IOException e) {
                MipavUtil.displayError("Error on image.exportData(0,xyzSize,datasetBuffer)");

                return false;
            }

            float[] dicomBuffer = new float[xyzSize];

            for (int x = 0; x < xDim; x++) {
                progressBar.updateValue(Math.round(100 * x / (xDim - 1)), true);

                switch (orient[0]) {

                    case FileInfoBase.ORI_R2L_TYPE:
                        newX = x;
                        break;

                    case FileInfoBase.ORI_L2R_TYPE:
                        newX = xDim - 1 - x;
                        break;

                    case FileInfoBase.ORI_A2P_TYPE:
                        newY = x;
                        break;

                    case FileInfoBase.ORI_P2A_TYPE:
                        newY = xDim - 1 - x;
                        break;

                    case FileInfoBase.ORI_I2S_TYPE:
                        newZ = x;
                        break;

                    case FileInfoBase.ORI_S2I_TYPE:
                        newZ = xDim - 1 - x;
                        break;
                }

                for (int y = 0; y < yDim; y++) {

                    switch (orient[1]) {

                        case FileInfoBase.ORI_R2L_TYPE:
                            newX = y;
                            break;

                        case FileInfoBase.ORI_L2R_TYPE:
                            newX = yDim - 1 - y;
                            break;

                        case FileInfoBase.ORI_A2P_TYPE:
                            newY = y;
                            break;

                        case FileInfoBase.ORI_P2A_TYPE:
                            newY = yDim - 1 - y;
                            break;

                        case FileInfoBase.ORI_I2S_TYPE:
                            newZ = y;
                            break;

                        case FileInfoBase.ORI_S2I_TYPE:
                            newZ = yDim - 1 - y;
                            break;
                    }

                    for (int z = 0; z < zDim; z++) {

                        switch (orient[2]) {

                            case FileInfoBase.ORI_R2L_TYPE:
                                newX = z;
                                break;

                            case FileInfoBase.ORI_L2R_TYPE:
                                newX = zDim - 1 - z;
                                break;

                            case FileInfoBase.ORI_A2P_TYPE:
                                newY = z;
                                break;

                            case FileInfoBase.ORI_P2A_TYPE:
                                newY = zDim - 1 - z;
                                break;

                            case FileInfoBase.ORI_I2S_TYPE:
                                newZ = z;
                                break;

                            case FileInfoBase.ORI_S2I_TYPE:
                                newZ = zDim - 1 - z;
                                break;
                        }

                        dicomBuffer[newX + (newXDim * newY) + (newSliceSize * newZ)] = datasetBuffer[x + (xDim * y) +
                                                                                                     (sliceSize * z)];
                    } // for (z = 0; z < zDim; z++)
                } // for (y = 0; y < yDim; y++)
            } // for (x = 0; x < xDim; x++)

            datasetBuffer = null;

            try {
                ACPCImage.importData(0, dicomBuffer, true);
            } catch (IOException e) {
                MipavUtil.displayError("Error on ACPCImage.importData(0,dicomBuffer,true)");
            }

            dicomBuffer = null;
            progressBar.dispose();
        } // if ((orient[0] != FileInfoBase.ORI_R2L_TYPE) || (orient[1] != FileInfoBase.ORI_A2P_TYPE) || (orient[2] !=
          // FileInfoBase.ORI_I2S_TYPE))

        rr = sclAdd(0.5f, rr1, 0.5f, rr2);
        rr = makeVoxelCoord3Df(rr, orgResol);
        progressBar = new ViewJProgressBar(image.getFileInfo()[0].getFileName(), "Rotation transformation ...", 0, 100,
                                           false, null, null);
        progressBar.setLocation(Toolkit.getDefaultToolkit().getScreenSize().width / 2, 50);
        progressBar.setVisible(true);

        if (image.getFileInfo(0).getFileFormat() == FileBase.AFNI) {
            originalSuperiorEdge = ((FileInfoAfni) image.getFileInfo(0)).getOriginalSuperiorEdge();
            originalPosteriorMargin = ((FileInfoAfni) image.getFileInfo(0)).getOriginalPosteriorMargin();

            if ((originalSuperiorEdge.x != Float.POSITIVE_INFINITY) &&
                    (originalPosteriorMargin.x != Float.POSITIVE_INFINITY)) {

                dif = sub(originalSuperiorEdge, originalPosteriorMargin);
                size = dotProduct(dif, gamma);
                rr1 = sclAdd(1.0f, originalPosteriorMargin, size, gamma);

                size = dotProduct(dif, beta);
                rr2 = sclAdd(1.0f, originalSuperiorEdge, -size, beta);

                size = dist(rr1, rr2, orgResol);

                if (size > 2.0) {
                    MipavUtil.displayError("Error! AC Talairach origin mismatch more than 2 mm");

                    return false;
                }

                AFNIrr = sclAdd(0.5f, rr1, 0.5f, rr2);
                Preferences.debug("AFNIrr = " + AFNIrr.x + "," + AFNIrr.y + "," + AFNIrr.z + "\n");
                AFNIsvec.x = -AFNIrr.x;
                AFNIsvec.y = -AFNIrr.y;
                AFNIsvec.z = -AFNIrr.z;

                // These are the same numbers as the svec numbers in WARP_DATA 21 to 23.
                Preferences.debug("AFNIsvec = " + AFNIsvec.x + "," + AFNIsvec.y + "," + AFNIsvec.z + "\n");
                AFNIbvec.x = (alpha.x * AFNIrr.x) + (alpha.y * AFNIrr.y) + (alpha.z * AFNIrr.z);
                AFNIbvec.y = (beta.x * AFNIrr.x) + (beta.y * AFNIrr.y) + (beta.z * AFNIrr.z);
                AFNIbvec.z = (gamma.x * AFNIrr.x) + (gamma.y * AFNIrr.y) + (gamma.z * AFNIrr.z);
                Preferences.debug("AFNIbvec = " + AFNIbvec.x + "," + AFNIbvec.y + "," + AFNIbvec.z + "\n");
            }
        }

        /* At this point we have the new origin of Talairach coordinates in rr; new axes
         * directions in alpha, beta, gamma.  Now construct the transformation between the Dicom coordinate systems. */

        /* Use the trace of the rotation matrix to find
         * the total rotation matrix */

        costheta = (float) (0.5 * Math.sqrt(1.0 + alpha.x + beta.y + gamma.z));
        theta = (float) (2.0 * Math.acos(costheta) * 180 / Math.PI);
        Preferences.debug("Total rotation to align AC-PC and mid-sag: " + theta + " degrees\n");
        // calculate the offset 3 vector bvec for forward transformation needed to place the origin at the appropriate
        // place in the image.  Subtract bvec to obtain the actual coordinates.  However, our interpolation routines map
        // from output to input so we do a backward transformation and do not use bvec bvec.x = alpha.x * rr.x + alpha.y
        // * rr.y + alpha.z * rr.z; bvec.y = beta.x * rr.x + beta.y * rr.y + beta.z * rr.z; bvec.z = gamma.x * rr.x +
        // gamma.y * rr.y + gamma.z * rr.z; Preferences.debug("bvec = " + bvec.x + "," + bvec.y + "," + bvec.z + "\n");

        // Preferences.debug( "superiorEdgeDicom = " + superiorEdgeDicom.x + "," +
        // superiorEdgeDicom.y + "," + superiorEdgeDicom.z + "\n");
        // Preferences.debug( "acsup = " + acsup.x + "," + acsup.y + "," + acsup.z + "\n");
        // Preferences.debug( "posteriorMarginDicom = " +
        // posteriorMarginDicom.x + "," + posteriorMarginDicom.y + "," +
        // posteriorMarginDicom.z + "\n");
        // Preferences.debug( "acpos = " + acpos.x + "," + acpos.y + "," + acpos.z + "\n");
        // Preferences.debug( "inferiorEdgeDicom = " +
        // inferiorEdgeDicom.x + "," + inferiorEdgeDicom.y + "," +
        // inferiorEdgeDicom.z + "\n");
        // Preferences.debug( "firstPtDicom = " + firstPtDicom.x + "," +
        // firstPtDicom.y + "," + firstPtDicom.z + "\n");
        // Preferences.debug( "anotherPtDicom = " + anotherPtDicom.x + "," +
        // anotherPtDicom.y + "," + anotherPtDicom.z + "\n");
        // magbeta = (float)Math.sqrt(beta.x*beta.x + beta.y*beta.y + beta.z*beta.z);
        // Preferences.debug( "beta = " + beta.x + "," + beta.y + "," + beta.z +
        // " mag beta = " + magbeta + "\n");
        // Preferences.debug( "alpha1 = " + alpha1.x + "," + alpha1.y + "," +
        // alpha1.z + "\n");
        // Preferences.debug( "alpha2 = " + alpha2.x + "," + alpha2.y + "," +
        // alpha2.z + "\n");
        // magalpha = (float)Math.sqrt(alpha.x*alpha.x + alpha.y*alpha.y + alpha.z*alpha.z);
        // Preferences.debug( "alpha = " + alpha.x + "," + alpha.y + "," +
        // alpha.z + " mag alpha = " + magalpha + "\n");
        // maggamma = (float)Math.sqrt(gamma.x*gamma.x + gamma.y*gamma.y + gamma.z*gamma.z);
        // Preferences.debug( "gamma = " + gamma.x + "," + gamma.y + "," +
        // gamma.z + " mag gamma = " + maggamma + "\n");
        // magabgx = (float)Math.sqrt(alpha.x*alpha.x + beta.x*beta.x + gamma.x*gamma.x);
        // magabgy = (float)Math.sqrt(alpha.y*alpha.y + beta.y*beta.y + gamma.y*gamma.y);
        // magabgz = (float)Math.sqrt(alpha.z*alpha.z + beta.z*beta.z + gamma.z*gamma.z);
        // Preferences.debug("mag alpha beta gamma x = " + magabgx + " y = " +
        // magabgy + " z = " + magabgz + "\n");
        // Preferences.debug( "rr1 = " + rr1.x + "," + rr1.y + "," + rr1.z + "\n");
        // Preferences.debug( "rr2 = " + rr2.x + "," + rr2.y + "," + rr2.z + "\n");
        // Preferences.debug( "rr = " + rr.x + "," + rr.y + "," + rr.z + "\n");

        TransMatrix xfrm = new TransMatrix(4);

        if (ACPCImage != null) {
            center = ACPCImage.getImageCentermm();
        } else {
            center = image.getImageCentermm();
        }

        // Preferences.debug("center x = " + center.x + " y = " + center.y +
        // " z = " + center.z + "\n");
        xfrm.setTranslate(center.x, center.y, center.z);

        /* Find the equivalent thetaX, thetaY, thetaZ */
        // mat[0][0] = cy*cz + sx*sy*sz = alpha.x
        // mat[0][1] = -cy*sz + sx*sy*cz = alpha.y
        // mat[0][2] = cx*sy = alpha.z
        // mat[1][0] = cx*sz = beta.x
        // mat[1][1] = cx*cz = beta.y
        // mat[1][2] = -sx = beta.z
        // mat[2][0] = -sy*cz + sx*cy*sz = gamma.x
        // mat[2][1] = sy*sz + sx*cy*cz = gamma.y
        // mat[2][2] = cx*cy = gamma.z
        // thetaX = -Math.asin(beta.z);
        // thetaY = Math.atan(alpha.z/gamma.z);
        // thetaZ = Math.atan(beta.x/beta.y);
        // tempf = Math.cos(thetaY)*Math.cos(thetaZ) + Math.sin(thetaX)*Math.sin(thetaY)*Math.sin(thetaZ);
        // Preferences.debug("mat[0][0] = " + tempf + "\n");
        // tempf = -Math.cos(thetaY)*Math.sin(thetaZ) + Math.sin(thetaX)*Math.sin(thetaY)*Math.cos(thetaZ);
        // Preferences.debug("mat[0][1] = " + tempf + "\n");
        // tempf = -Math.sin(thetaY)*Math.cos(thetaZ) + Math.sin(thetaX)*Math.cos(thetaY)*Math.sin(thetaZ);
        // Preferences.debug("mat[2][0] = " + tempf + "\n");
        // tempf = Math.sin(thetaY)*Math.sin(thetaZ) + Math.sin(thetaX)*Math.cos(thetaY)*Math.cos(thetaZ);
        // Preferences.debug("mat[2][1] = " + tempf + "\n");
        // thetaX = thetaX * 180.0/Math.PI;
        // thetaY = thetaY * 180.0/Math.PI;
        // thetaZ = thetaZ * 180.0/Math.PI;
        // Preferences.debug("thetaX = " + thetaX + " thetaY = " + thetaY +
        // " thetaZ = " + thetaZ + "\n");

        // Since our interpolation routines are doing a output to input mapping, create the mbac
        // transformation matrix which is the transpose(also the inverse) of the mfor matrix.
        float tempVar;
        tempVar = alpha.y;
        alpha.y = beta.x;
        beta.x = tempVar;
        tempVar = alpha.z;
        alpha.z = gamma.x;
        gamma.x = tempVar;
        tempVar = gamma.y;
        gamma.y = beta.z;
        beta.z = tempVar;

        xfrm.setRotate(alpha, beta, gamma);

        /* Code to make cubic voxels required in acpc and Talairach space */

        /* Almost everyone uses the default 1 mm. */
        oXres = voxelLength;
        oYres = voxelLength;
        oZres = voxelLength;
        oXdim = Math.round(2 * (ATLAS_ALIGNBOX_LAT + 1) / oXres);
        oYdim = Math.round((ATLAS_ALIGNBOX_ANT + ATLAS_ALIGNBOX_POS + 1) / oYres);
        oZdim = Math.round((ATLAS_ALIGNBOX_INF + ATLAS_ALIGNBOX_SUP + 1) / oZres);

        int bufferSize = newXDim * newYDim * newZDim;
        float[] imgBuffer = new float[bufferSize];

        try {

            if (ACPCImage != null) {
                ACPCImage.exportData(0, bufferSize, imgBuffer);
            } else {
                image.exportData(0, bufferSize, imgBuffer);
            }

        } catch (IOException error) {
            MipavUtil.displayError("ViewJFrameTriImage: IOException error on exportData");
        }

        int[] extents = new int[3];
        extents[0] = oXdim;
        extents[1] = oYdim;
        extents[2] = oZdim;

        if (ACPCImage != null) {
            ACPCImage = new ModelImage(ACPCImage.getType(), extents, "ACPC image", image.getUserInterface());
        } else {
            ACPCImage = new ModelImage(image.getType(), extents, "ACPC image", image.getUserInterface());
        }

        ACPCImage.setImageOrientation(FileInfoBase.AXIAL);

        Mat = xfrm.getMatrix();
        Tr03 = (orgResol[0] * rr.x) - (Tal.x * Mat[0][0]) - (Tal.y * Mat[0][1]) - (Tal.z * Mat[0][2]);
        Tr13 = (orgResol[1] * rr.y) - (Tal.x * Mat[1][0]) - (Tal.y * Mat[1][1]) - (Tal.z * Mat[1][2]);
        Tr23 = (orgResol[2] * rr.z) - (Tal.x * Mat[2][0]) - (Tal.y * Mat[2][1]) - (Tal.z * Mat[2][2]);

        /*
         * Tr03 = M[0][0] * Tx + M[0][1] * Ty + M[0][2] * Tz + M[0][3] Tr13 = M[1][0] * Tx + M[1][1] * Ty + M[1][2] * Tz
         * + M[1][3] Tr23 = M[2][2] * Tx + M[2][1] * Ty + M[2][2] * Tz + M[2][3]
         */
        Ac = new Matrix(3, 3);
        Ac.set(0, 0, Mat[0][0]);
        Ac.set(0, 1, Mat[0][1]);
        Ac.set(0, 2, Mat[0][2]);
        Ac.set(1, 0, Mat[1][0]);
        Ac.set(1, 1, Mat[1][1]);
        Ac.set(1, 2, Mat[1][2]);
        Ac.set(2, 0, Mat[2][0]);
        Ac.set(2, 1, Mat[2][1]);
        Ac.set(2, 2, Mat[2][2]);
        b = new Matrix(3, 1);
        b.set(0, 0, Tr03 - Mat[0][3]);
        b.set(1, 0, Tr13 - Mat[1][3]);
        b.set(2, 0, Tr23 - Mat[2][3]);
        X = Ac.solve(b);
        Tx = X.get(0, 0);
        Ty = X.get(1, 0);
        Tz = X.get(2, 0);
        xfrm.setTranslate(Tx, Ty, Tz);

        Mat = xfrm.getMatrix();
        Preferences.debug("T03 = " + Mat[0][3] + " T13 = " + Mat[1][3] + " T23 = " + Mat[2][3] + "\n");
        transformACPCTrilinear(ACPCImage, imgBuffer, Mat, orgResol[0], orgResol[1], orgResol[2], newXDim, newYDim,
                               newZDim, oXres, oYres, oZres, oXdim, oYdim, oZdim, progressBar);

        ACPCImage.calcMinMax();

        /* Find the new Talairach center, the new origin, in the newly transformed coordinates */

        /* X = (i*oXres*T00 + j*oYres*T01 + k*oZres*T02 + T03)/iXres
         * Y = (i*oXres*T10 + j*oYres*T11 + k*oZres*T12 + T13)/iYres Z = (i*oXres*T20 + j*oYres*T21 + k*oZres*T22 +
         * T23)/iZres Wish to find i,j,k from X,Y,Z Wish to find TCenter.x,TCenter.y,TCenter.z from rr.x,rr.y,rr.z */
        translation.x = (float) Mat[0][3];
        translation.y = (float) Mat[1][3];
        translation.z = (float) Mat[2][3];

        Ac = new Matrix(3, 3);
        Ac.set(0, 0, oXres * Mat[0][0] / orgResol[0]);
        Ac.set(0, 1, oYres * Mat[0][1] / orgResol[0]);
        Ac.set(0, 2, oZres * Mat[0][2] / orgResol[0]);
        Ac.set(1, 0, oXres * Mat[1][0] / orgResol[1]);
        Ac.set(1, 1, oYres * Mat[1][1] / orgResol[1]);
        Ac.set(1, 2, oZres * Mat[1][2] / orgResol[1]);
        Ac.set(2, 0, oXres * Mat[2][0] / orgResol[2]);
        Ac.set(2, 1, oYres * Mat[2][1] / orgResol[2]);
        Ac.set(2, 2, oZres * Mat[2][2] / orgResol[2]);
        b = new Matrix(3, 1);

        /* b.set(0,0,rr.x - Mat[0][3]/orgResol[0]);
         * b.set(1,0,rr.y - Mat[1][3]/orgResol[1]); b.set(2,0,rr.z - Mat[2][3]/orgResol[2]); X = Ac.solve(b); TCenter.x
         * = (float)(X.get(0,0)); TCenter.y = (float)(X.get(1,0)); TCenter.z = (float)(X.get(2,0)); */
        TCenter.x = Tal.x / oXres;
        TCenter.y = Tal.y / oYres;
        TCenter.z = Tal.z / oZres;
        Preferences.debug("Transformed Talairach origin coordinates = " + TCenter.x + "  " + TCenter.y + "  " +
                          TCenter.z + "\n");

        // Find pcie the coordinate transformed posterior commissure inferior edge
        b.set(0, 0, inferiorEdgeDicom.x - (Mat[0][3] / orgResol[0]));
        b.set(1, 0, inferiorEdgeDicom.y - (Mat[1][3] / orgResol[1]));
        b.set(2, 0, inferiorEdgeDicom.z - (Mat[2][3] / orgResol[2]));
        X = Ac.solve(b);
        pcie.x = (float) (X.get(0, 0));
        pcie.y = (float) (X.get(1, 0));
        pcie.z = (float) (X.get(2, 0));

        MeasureUnits[0] = FileInfoBase.MILLIMETERS;
        MeasureUnits[1] = FileInfoBase.MILLIMETERS;
        MeasureUnits[2] = FileInfoBase.MILLIMETERS;
        oRes[0] = oXres;
        oRes[1] = oYres;
        oRes[2] = oZres;
        newOrient[0] = FileInfoBase.ORI_R2L_TYPE;
        newOrient[1] = FileInfoBase.ORI_A2P_TYPE;
        newOrient[2] = FileInfoBase.ORI_I2S_TYPE;
        extents = new int[3];
        extents[0] = oXdim;
        extents[1] = oYdim;
        extents[2] = oZdim;

        fileInfo = new FileInfoAfni[oZdim];

        for (int i = 0; i < oZdim; i++) {
            fileInfo[i] = new FileInfoAfni(image.getFileInfo()[0].getFileName(),
                                           image.getFileInfo()[0].getFileDirectory(), FileBase.AFNI);
            fileInfo[i].setFileFormat(FileBase.AFNI);
            fileInfo[i].setModality(image.getFileInfo()[0].getModality());
            fileInfo[i].setFileDirectory(image.getFileInfo()[0].getFileDirectory());
            fileInfo[i].setDataType(image.getFileInfo()[0].getDataType());
            fileInfo[i].setEndianess(image.getFileInfo()[0].getEndianess());
            fileInfo[i].setUnitsOfMeasure(MeasureUnits);
            fileInfo[i].setResolutions(oRes);
            fileInfo[i].setExtents(extents);
            fileInfo[i].setAxisOrientation(newOrient);
            fileInfo[i].setImageOrientation(FileInfoBase.AXIAL);
            fileInfo[i].setpcie(pcie);
            fileInfo[i].setTalairachCenter(TCenter);
            fileInfo[i].setMax(ACPCImage.getMax());
            fileInfo[i].setMin(ACPCImage.getMin());
            fileInfo[i].setPixelPadValue(image.getFileInfo()[0].getPixelPadValue());
            fileInfo[i].setPhotometric(image.getFileInfo()[0].getPhotometric());
            fileInfo[i].setAFNIViewType(FileInfoAfni.AFNI_ACPC);
            fileInfo[i].setAlpha(alpha);
            fileInfo[i].setBeta(beta);
            fileInfo[i].setGamma(gamma);
            fileInfo[i].setTranslation(translation);
            fileInfo[i].setrr(rr);
            fileInfo[i].setAFNIOrigExtents(newExtents);
            fileInfo[i].setAFNIOrigResolutions(orgResol);

            if (image.getFileInfo()[0].getFileFormat() == FileBase.AFNI) {

                // otherwise there will be junk in these variables
                fileInfo[i].setLowXmm(dicomLowX);
                fileInfo[i].setLowYmm(dicomLowY);
                fileInfo[i].setLowZmm(dicomLowZ);
                fileInfo[i].setHighXmm(dicomHighX);
                fileInfo[i].setHighYmm(dicomHighY);
                fileInfo[i].setHighZmm(dicomHighZ);
                fileInfo[i].setFuncType(((FileInfoAfni) image.getFileInfo()[0]).getFuncType());
                fileInfo[i].setAFNITypeString(((FileInfoAfni) image.getFileInfo()[0]).getAFNITypeString());
            }
        }

        ACPCImage.setFileInfo(fileInfo);

        try {
            ACPCImage.setImageName("ACPC image");
            imageFrame = new ViewJFrameImage(ACPCImage, null, new Dimension(610, 200));
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: unable to open new frame");
        }

        progressBar.dispose();
        frame.updateImages(true);

        return true;
    }

    /**
     * Finds crossproduct of two vectors.
     *
     * @param   pt1  First vector.
     * @param   pt2  Second vector
     *
     * @return  Cross product of pt1 and pt2.
     */
    private Point3Df crossProduct(Point3Df pt1, Point3Df pt2) {
        Point3Df crossPt = new Point3Df(0.0f, 0.0f, 0.0f);
        crossPt.x = (pt1.y * pt2.z) - (pt1.z * pt2.y);
        crossPt.y = (pt1.z * pt2.x) - (pt1.x * pt2.z);
        crossPt.z = (pt1.x * pt2.y) - (pt1.y * pt2.x);

        return crossPt;
    }

    /**
     * Finds the distance between two points based on resolution.
     *
     * @param   pt1    First point.
     * @param   pt2    Second point.
     * @param   resol  Resolutions of each dimension.
     *
     * @return  DOCUMENT ME!
     */
    private float dist(Point3Df pt1, Point3Df pt2, float[] resol) {
        float distX, distY, distZ;
        float length;
        distX = (pt1.x - pt2.x) * resol[0];
        distX = distX * distX;
        distY = (pt1.y - pt2.y) * resol[1];
        distY = distY * distY;
        distZ = (pt1.z - pt2.z) * resol[2];
        distZ = distZ * distZ;
        length = (float) Math.sqrt(distX + distY + distZ);

        return length;
    }

    /**
     * Finds dotproduct of two vectors.
     *
     * @param   pt1  First vector.
     * @param   pt2  Second vector
     *
     * @return  Dot product of pt1 and pt2.
     */
    private float dotProduct(Point3Df pt1, Point3Df pt2) {
        float dot;
        dot = (pt1.x * pt2.x) + (pt1.y * pt2.y) + (pt1.z * pt2.z);

        return dot;
    }

    /**
     * Gets the orientation from the combo boxes and checks if it's consistent.
     *
     * @return  <code>true</code> if orientation is consistent.
     */
    private boolean getOrient() {
        boolean success = true;

        /* Obtain the input axes orientations */
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
        if (!((orient[0] == FileInfoBase.ORI_R2L_TYPE) || (orient[0] == FileInfoBase.ORI_L2R_TYPE) ||
                  (orient[1] == FileInfoBase.ORI_R2L_TYPE) || (orient[1] == FileInfoBase.ORI_L2R_TYPE) ||
                  (orient[2] == FileInfoBase.ORI_R2L_TYPE) || (orient[2] == FileInfoBase.ORI_L2R_TYPE))) {
            success = false;
        }

        if (!((orient[0] == FileInfoBase.ORI_P2A_TYPE) || (orient[0] == FileInfoBase.ORI_A2P_TYPE) ||
                  (orient[1] == FileInfoBase.ORI_P2A_TYPE) || (orient[1] == FileInfoBase.ORI_A2P_TYPE) ||
                  (orient[2] == FileInfoBase.ORI_P2A_TYPE) || (orient[2] == FileInfoBase.ORI_A2P_TYPE))) {
            success = false;
        }

        if (!((orient[0] == FileInfoBase.ORI_I2S_TYPE) || (orient[0] == FileInfoBase.ORI_S2I_TYPE) ||
                  (orient[1] == FileInfoBase.ORI_I2S_TYPE) || (orient[1] == FileInfoBase.ORI_S2I_TYPE) ||
                  (orient[2] == FileInfoBase.ORI_I2S_TYPE) || (orient[2] == FileInfoBase.ORI_S2I_TYPE))) {
            success = false;
        }

        if (!success) {
            MipavUtil.displayError("Illegal selections for axes orientations");
        }

        return success;

    }

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
            orientPanel = new JPanel(new GridLayout(3, 2));
            orientPanel.setForeground(Color.black);
            orientPanel.setBorder(buildTitledBorder("Describe input orientation"));

            JLabel labelOrientX = new JLabel("X axis:");
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

            JLabel labelOrientY = new JLabel("Y axis:");
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

            JLabel labelOrientZ = new JLabel("Z axis:");
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

        setACPCButton = new JButton("Set");
        setACPCButton.setFont(serif12B);
        setACPCButton.addActionListener(this);
        setACPCButton.setActionCommand("setACPC");
        setACPCButton.setPreferredSize(MipavUtil.defaultButtonSize);

        clearACPCButton = new JButton("Clear");
        clearACPCButton.setFont(serif12B);
        clearACPCButton.addActionListener(this);
        clearACPCButton.setActionCommand("clearACPC");
        clearACPCButton.setEnabled(false);
        clearACPCButton.setPreferredSize(MipavUtil.defaultButtonSize);

        applyACPCButton = new JButton("Apply");
        applyACPCButton.setFont(serif12B);
        applyACPCButton.addActionListener(this);
        applyACPCButton.setActionCommand("applyACPC");
        applyACPCButton.setEnabled(false);
        applyACPCButton.setPreferredSize(MipavUtil.defaultButtonSize);

        cancelACPCButton = new JButton("Cancel");
        cancelACPCButton.setFont(serif12B);
        cancelACPCButton.addActionListener(this);
        cancelACPCButton.setActionCommand("cancelACPC");
        cancelACPCButton.setPreferredSize(MipavUtil.defaultButtonSize);

        JPanel buttonPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        buttonPanel.add(setACPCButton, gbc);
        gbc.gridx = 1;
        buttonPanel.add(clearACPCButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        buttonPanel.add(applyACPCButton, gbc);
        gbc.gridx = 1;
        buttonPanel.add(cancelACPCButton, gbc);

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;

        int y = 0;

        if (orient[0] == FileInfoBase.ORI_UNKNOWN_TYPE) {
            mainPanel.add(orientPanel, gbc);
            gbc.gridy = 1;
            y = 1;
        }

        mainPanel.add(voxelPanel, gbc);
        gbc.gridy = y + 1;
        mainPanel.add(pointPanel, gbc);

        mainDialogPanel.add(mainPanel);
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);
    }

    /**
     * Makes a mm (physical space) point from a pixel space point.
     *
     * @param   pt     Point to convert.
     * @param   resol  Resolutions to use when converting.
     *
     * @return  Same point in mm.
     */
    private Point3Df makemmPoint3Df(Point3Df pt, float[] resol) {
        Point3Df mmPt = new Point3Df(0.0f, 0.0f, 0.0f);
        mmPt.x = resol[0] * pt.x;
        mmPt.y = resol[1] * pt.y;
        mmPt.z = resol[2] * pt.z;

        return mmPt;
    }

    /**
     * Makes a pixel space point from a physical space point.
     *
     * @param   pt     Point to convert.
     * @param   resol  Resolutions to use when converting.
     *
     * @return  Same point in pixel space.
     */
    private Point3Df makeVoxelCoord3Df(Point3Df pt, float[] resol) {
        Point3Df voxelPt = new Point3Df(0.0f, 0.0f, 0.0f);
        voxelPt.x = pt.x / resol[0];
        voxelPt.y = pt.y / resol[1];
        voxelPt.z = pt.z / resol[2];

        return voxelPt;
    }

    /**
     * Finds the normal to the vector.
     *
     * @param   pt  Vector to find normal to.
     *
     * @return  Normal of pt.
     */
    private Point3Df norm(Point3Df pt) {
        float scale;
        Point3Df normPt = new Point3Df(0.0f, 0.0f, 0.0f);
        scale = (pt.x * pt.x) + (pt.y * pt.y) + (pt.z * pt.z);
        scale = (float) ((scale > 0) ? (1.0 / Math.sqrt(scale)) : 0);
        normPt.x = pt.x * scale;
        normPt.y = pt.y * scale;
        normPt.z = pt.z * scale;

        return normPt;

    }

    /**
     * Scale and add two vectors.
     *
     * @param   fa  Scale for vector a.
     * @param   a   Vector a.
     * @param   fb  Scale for vector b.
     * @param   b   Vector b.
     *
     * @return  DOCUMENT ME!
     */
    private Point3Df sclAdd(float fa, Point3Df a, float fb, Point3Df b) {
        Point3Df pt = new Point3Df(0.0f, 0.0f, 0.0f);
        pt.x = (fa * a.x) + (fb * b.x);
        pt.y = (fa * a.y) + (fb * b.y);
        pt.z = (fa * a.z) + (fb * b.z);

        return pt;
    }

    /**
     * Sets another label based on the point. Enables "Apply" if all points have been set.
     *
     * @param  pt  Point that was set.
     */
    private void setAnotherPt(Point3Df pt) {
        superiorEdge.setSelected(false);
        posteriorMargin.setSelected(false);
        inferiorEdge.setSelected(false);
        firstPt.setSelected(false);
        anotherPt.setSelected(true);
        anotherPtDicom = pt;
        haveAnotherPt = true;
        anotherPt.setText("Another mid-sag pt " + (int) pt.x + "," + (int) pt.y + "," + (int) pt.z);
        setACPCButton.setEnabled(false);
        clearACPCButton.setEnabled(true);

        if ((haveSuperiorEdge) && (havePosteriorMargin) && (haveInferiorEdge) && (haveFirstPt) && (haveAnotherPt)) {
            applyACPCButton.setEnabled(true);
        }
    }

    /**
     * Sets first label based on the point. Enables "Apply" if all points have been set.
     *
     * @param  pt  Point that was set.
     */
    private void setFirstPt(Point3Df pt) {
        superiorEdge.setSelected(false);
        posteriorMargin.setSelected(false);
        inferiorEdge.setSelected(false);
        firstPt.setSelected(true);
        anotherPt.setSelected(false);
        firstPtDicom = pt;
        haveFirstPt = true;
        firstPt.setText("First mid-sag pt " + (int) pt.x + "," + (int) pt.y + "," + (int) pt.z);
        setACPCButton.setEnabled(false);
        clearACPCButton.setEnabled(true);

        if ((haveSuperiorEdge) && (havePosteriorMargin) && (haveInferiorEdge) && (haveFirstPt) && (haveAnotherPt)) {
            applyACPCButton.setEnabled(true);
        }
    }

    /**
     * Sets inferior label based on the point. Enables "Apply" if all points have been set.
     *
     * @param  pt  Point that was set.
     */
    private void setInferiorEdge(Point3Df pt) {
        superiorEdge.setSelected(false);
        posteriorMargin.setSelected(false);
        inferiorEdge.setSelected(true);
        firstPt.setSelected(false);
        anotherPt.setSelected(false);
        inferiorEdgeDicom = pt;
        haveInferiorEdge = true;
        inferiorEdge.setText("PC inferior edge " + (int) pt.x + "," + (int) pt.y + "," + (int) pt.z);
        setACPCButton.setEnabled(false);
        clearACPCButton.setEnabled(true);

        if ((haveSuperiorEdge) && (havePosteriorMargin) && (haveInferiorEdge) && (haveFirstPt) && (haveAnotherPt)) {
            applyACPCButton.setEnabled(true);
        }
    }

    /**
     * Sets posterior label based on the point. Enables "Apply" if all points have been set.
     *
     * @param  pt  Point that was set.
     */
    private void setPosteriorMargin(Point3Df pt) {
        superiorEdge.setSelected(false);
        posteriorMargin.setSelected(true);
        inferiorEdge.setSelected(false);
        firstPt.setSelected(false);
        anotherPt.setSelected(false);
        posteriorMarginDicom = pt;
        havePosteriorMargin = true;
        posteriorMargin.setText("AC posterior margin " + (int) pt.x + "," + (int) pt.y + "," + (int) pt.z);
        setACPCButton.setEnabled(false);
        clearACPCButton.setEnabled(true);

        if ((haveSuperiorEdge) && (havePosteriorMargin) && (haveInferiorEdge) && (haveFirstPt) && (haveAnotherPt)) {
            applyACPCButton.setEnabled(true);
        }
    }

    /**
     * Sets superior label based on the point. Enables "Apply" if all points have been set.
     *
     * @param  pt  Point that was set.
     */
    private void setSuperiorEdge(Point3Df pt) {
        superiorEdge.setSelected(true);
        posteriorMargin.setSelected(false);
        inferiorEdge.setSelected(false);
        firstPt.setSelected(false);
        anotherPt.setSelected(false);
        superiorEdgeDicom = pt;
        haveSuperiorEdge = true;
        superiorEdge.setText("AC superior edge " + (int) pt.x + "," + (int) pt.y + "," + (int) pt.z);
        setACPCButton.setEnabled(false);
        clearACPCButton.setEnabled(true);

        if ((haveSuperiorEdge) && (havePosteriorMargin) && (haveInferiorEdge) && (haveFirstPt) && (haveAnotherPt)) {
            applyACPCButton.setEnabled(true);
        }
    }

    /**
     * Subtracts one vector from another and returns result. result = pt1 - pt2.
     *
     * @param   pt1  Vector to subtract from.
     * @param   pt2  Vector to be subtracted.
     *
     * @return  pt1 - pt2
     */
    private Point3Df sub(Point3Df pt1, Point3Df pt2) {
        Point3Df pt = new Point3Df(0.0f, 0.0f, 0.0f);
        pt.x = pt1.x - pt2.x;
        pt.y = pt1.y - pt2.y;
        pt.z = pt1.z - pt2.z;

        return pt;
    }

    /**
     * Transforms and resamples volume using trilinear interpolation.
     *
     * @param  image        Image.
     * @param  imgBuffer    Image array.
     * @param  xfrm         Transformation matrix to be applied.
     * @param  iXres        In X resolution.
     * @param  iYres        In Y resolution.
     * @param  iZres        In Z resolution.
     * @param  iXdim        In X dimension.
     * @param  iYdim        In Y dimension.
     * @param  iZdim        In Z dimension.
     * @param  oXres        Out X resolution.
     * @param  oYres        Out Y resolution.
     * @param  oZres        Out Z resolution.
     * @param  oXdim        Out X dimension.
     * @param  oYdim        Out Y dimension.
     * @param  oZdim        Out Z dimension.
     * @param  progressBar  Progress bar.
     */
    private void transformACPCTrilinear(ModelImage image, float[] imgBuffer, double[][] xfrm, float iXres, float iYres,
                                        float iZres, int iXdim, int iYdim, int iZdim, float oXres, float oYres,
                                        float oZres, int oXdim, int oYdim, int oZdim, ViewJProgressBar progressBar) {
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

        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23, T30, T31, T32, T33;

        int mod = oXdim / 50;

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
        T30 = (float) xfrm[3][0];
        T31 = (float) xfrm[3][1];
        T32 = (float) xfrm[3][2];
        T33 = (float) xfrm[3][3];

        for (i = 0; i < oXdim; i++) {

            if ((i % mod) == 0) {
                progressBar.updateValue((int) ((((float) i / (oXdim - 1)) * 100) + .5), true);
            }

            imm = (float) i * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = 0; j < oYdim; j++) {
                jmm = (float) j * oYres;
                j1 = jmm * T01;
                j2 = jmm * T11;
                j3 = jmm * T21;
                temp1 = i3 + j3;
                temp2 = i2 + j2;
                temp3 = i1 + j1;

                for (k = 0; k < oZdim; k++) {

                    // transform i,j,k
                    value = 0; // remains zero if voxel is transformed out of bounds
                    kmm = (float) k * oZres;
                    X = (temp3 + (kmm * T02)) / iXres;
                    roundX = (int) (X + 0.5f);

                    if ((X >= 0) && (roundX < iXdim)) {
                        Y = (temp2 + (kmm * T12)) / iYres;
                        roundY = (int) (Y + 0.5f);

                        if ((Y >= 0) && (roundY < iYdim)) {
                            Z = (temp1 + (kmm * T22)) / iZres;
                            roundZ = (int) (Z + 0.5f);

                            if ((Z >= 0) && (roundZ < iZdim)) {

                                if ((roundX == (iXdim - 1)) || (roundY == (iYdim - 1)) || (roundZ == (iZdim - 1))) {
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
                                    value = (x1 * temp4 * imgBuffer[Z0pos + Y0pos + X0pos]) +
                                            (x0 * temp4 * imgBuffer[Z0pos + Y0pos + X1pos]) +
                                            (x1 * temp5 * imgBuffer[Z0pos + Y1pos + X0pos]) +
                                            (x0 * temp5 * imgBuffer[Z0pos + Y1pos + X1pos]) +
                                            (x1 * temp6 * imgBuffer[Z1pos + Y0pos + X0pos]) +
                                            (x0 * temp6 * imgBuffer[Z1pos + Y0pos + X1pos]) +
                                            (x1 * temp7 * imgBuffer[Z1pos + Y1pos + X0pos]) +
                                            (x0 * temp7 * imgBuffer[Z1pos + Y1pos + X1pos]);
                                }
                            } // end if Z in bounds
                        } // end if Y in bounds
                    } // end if X in bounds

                    image.set(i, j, k, value);
                } // end for k
            } // end for j
        } // end for i
    }
}
