package gov.nih.mipav.view.dialogs;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.swing.*;


/**
 * Dialog to enter points for creating an Talaraich image.
 */
public class JDialogTalairach extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2923298916025034767L;

    /** DOCUMENT ME! */
    private static final float MAX_ALLOWED_DEVIATION = 2.0f;

    /** DOCUMENT ME! */
    private static final float MIN_ALLOWED_DEVIATION = 0.5f;

    /** DOCUMENT ME! */
    private static final int R = 0;

    /** DOCUMENT ME! */
    private static final int L = 1;

    /** DOCUMENT ME! */
    private static final int A = 0;

    /** DOCUMENT ME! */
    private static final int M = 1;

    /** DOCUMENT ME! */
    private static final int P = 2;

    /** DOCUMENT ME! */
    private static final int I = 0;

    /** DOCUMENT ME! */
    private static final int S = 1;

    /** DOCUMENT ME! */
    private static final float ATLAS_FRONT_TO_AC = 70.0f;

    /** DOCUMENT ME! */
    private static final float ATLAS_PC_TO_BACK = 79.0f;

    /** DOCUMENT ME! */
    private static final float ATLAS_BOT_TO_AC = 42.0f;

    /** DOCUMENT ME! */
    private static final float ATLAS_AC_TO_TOP = 74.0f;

    /** DOCUMENT ME! */
    private static final float ATLAS_AC_TO_LAT = 68.0f;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JRadioButton anteriorPt;

    /** DOCUMENT ME! */
    private Vector3f anteriorPt3Df;

    /** DOCUMENT ME! */
    private JButton applyTalairachButton;

    /** DOCUMENT ME! */
    private JButton cancelTalairachButton;

    /** DOCUMENT ME! */
    private JButton clearTalairachButton;


    /** DOCUMENT ME! */
    private ViewJFrameTriImage frame;

    /** DOCUMENT ME! */
    private boolean haveAnteriorPt = false;

    /** DOCUMENT ME! */
    private boolean haveInferiorPt = false;

    /** DOCUMENT ME! */
    private boolean haveLeftPt = false;

    /** DOCUMENT ME! */
    private boolean havePosteriorPt = false;

    /** DOCUMENT ME! */
    private boolean haveRightPt = false;

    /** DOCUMENT ME! */
    private boolean haveSuperiorPt = false;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private JRadioButton inferiorPt;

    /** DOCUMENT ME! */
    private Vector3f inferiorPt3Df;

    /** DOCUMENT ME! */
    private JRadioButton leftPt;

    /** DOCUMENT ME! */
    private Vector3f leftPt3Df;

    /** DOCUMENT ME! */
    private JRadioButton posteriorPt;

    /** DOCUMENT ME! */
    private Vector3f posteriorPt3Df;

    /** DOCUMENT ME! */
    private JRadioButton rightPt;

    /** DOCUMENT ME! */
    private Vector3f rightPt3Df;

    /** DOCUMENT ME! */
    private JButton setTalairachButton;

    /** DOCUMENT ME! */
    private JRadioButton superiorPt;

    /** DOCUMENT ME! */
    private Vector3f superiorPt3Df;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * This method creates a dialog for selecting markers used for generating a Talairach view image from an AC-PC
     * aligned view image.
     *
     * @param  theParentFrame  The tri planar view frame that called this dialog.
     * @param  im              Image to generate a Talairach view from.
     */
    public JDialogTalairach(ViewJFrameTriImage theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        frame = theParentFrame;
        image = im;
        init();

        Vector3f pt = new Vector3f(Float.POSITIVE_INFINITY, Float.POSITIVE_INFINITY, Float.POSITIVE_INFINITY);

        if (image.getFileInfo()[0].getFileFormat() == FileUtility.AFNI) {
            pt = ((FileInfoAfni) image.getFileInfo(0)).getAnteriorPt();
        }

        if (pt.X != Float.POSITIVE_INFINITY) {
            ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).setReferenceXY(ViewJComponentTriImage.ANTERIOR_PT,
                                                                                                    pt);
            setAnteriorPt(pt);
        }

        if (image.getFileInfo()[0].getFileFormat() == FileUtility.AFNI) {
            pt = ((FileInfoAfni) image.getFileInfo(0)).getPosteriorPt();
        }

        if (pt.X != Float.POSITIVE_INFINITY) {
            ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).setReferenceXY(ViewJComponentTriImage.POSTERIOR_PT,
                                                                                                    pt);
            setPosteriorPt(pt);
        }

        if (image.getFileInfo()[0].getFileFormat() == FileUtility.AFNI) {
            pt = ((FileInfoAfni) image.getFileInfo(0)).getSuperiorPt();
        }

        if (pt.X != Float.POSITIVE_INFINITY) {
            ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).setReferenceXY(ViewJComponentTriImage.SUPERIOR_PT,
                                                                                                    pt);
            setSuperiorPt(pt);
        }

        if (image.getFileInfo()[0].getFileFormat() == FileUtility.AFNI) {
            pt = ((FileInfoAfni) image.getFileInfo(0)).getInferiorPt();
        }

        if (pt.X != Float.POSITIVE_INFINITY) {
            ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).setReferenceXY(ViewJComponentTriImage.INFERIOR_PT,
                                                                                                    pt);
            setInferiorPt(pt);
        }

        if (image.getFileInfo()[0].getFileFormat() == FileUtility.AFNI) {
            pt = ((FileInfoAfni) image.getFileInfo(0)).getLeftPt();
        }

        if (pt.X != Float.POSITIVE_INFINITY) {
            ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).setReferenceXY(ViewJComponentTriImage.LEFT_PT,
                                                                                                    pt);
            setLeftPt(pt);
        }

        if (image.getFileInfo()[0].getFileFormat() == FileUtility.AFNI) {
            pt = ((FileInfoAfni) image.getFileInfo(0)).getRightPt();
        }

        if (pt.X != Float.POSITIVE_INFINITY) {
            ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).setReferenceXY(ViewJComponentTriImage.RIGHT_PT,
                                                                                                    pt);
            setRightPt(pt);
        }

        pack();
        setVisible(true);
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
        int pointType;
        Vector3f pt;
        boolean found;

        if (command.equals("setTalairach")) {
            pt = new Vector3f(frame.getSagittalComponentSlice(), frame.getCoronalComponentSlice(),
                              frame.getAxialComponentSlice());

            if (anteriorPt.isSelected()) {
                pointType = ViewJComponentTriImage.ANTERIOR_PT;
                ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).removeReference("A");
                setAnteriorPt(pt);
            } else if (posteriorPt.isSelected()) {
                pointType = ViewJComponentTriImage.POSTERIOR_PT;
                ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).removeReference("P");
                setPosteriorPt(pt);
            } else if (superiorPt.isSelected()) {
                pointType = ViewJComponentTriImage.SUPERIOR_PT;
                ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).removeReference("S");
                setSuperiorPt(pt);
            } else if (inferiorPt.isSelected()) {
                pointType = ViewJComponentTriImage.INFERIOR_PT;
                ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).removeReference("I");
                setInferiorPt(pt);
            } else if (leftPt.isSelected()) {
                pointType = ViewJComponentTriImage.LEFT_PT;
                ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).removeReference("L");
                setLeftPt(pt);
            } else {
                pointType = ViewJComponentTriImage.RIGHT_PT;
                ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).removeReference("R");
                setRightPt(pt);
            }

            ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).setReferenceXY(pointType, pt);
        } else if (command.equals("clearTalairach")) {

            if (anteriorPt.isSelected()) {
                found = ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).removeReference("A");

                if (found) {
                    haveAnteriorPt = false;
                    anteriorPt.setText("Most anterior point");
                    clearTalairachButton.setEnabled(false);
                    setTalairachButton.setEnabled(true);
                    applyTalairachButton.setEnabled(false);
                } else {
                    MipavUtil.displayError("Error! Failed to remove most anterior point");
                }
            } else if (posteriorPt.isSelected()) {
                found = ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).removeReference("P");

                if (found) {
                    havePosteriorPt = false;
                    posteriorPt.setText("Most posterior point");
                    clearTalairachButton.setEnabled(false);
                    setTalairachButton.setEnabled(true);
                    applyTalairachButton.setEnabled(false);
                } else {
                    MipavUtil.displayError("Error! Failed to remove most posterior point");
                }
            } else if (superiorPt.isSelected()) {
                found = ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).removeReference("S");

                if (found) {
                    haveSuperiorPt = false;
                    superiorPt.setText("Most superior point");
                    clearTalairachButton.setEnabled(false);
                    setTalairachButton.setEnabled(true);
                    applyTalairachButton.setEnabled(false);
                } else {
                    MipavUtil.displayError("Error! Failed to remove most superior point");
                }
            } else if (inferiorPt.isSelected()) {
                found = ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).removeReference("I");

                if (found) {
                    haveInferiorPt = false;
                    inferiorPt.setText("Most inferior point");
                    clearTalairachButton.setEnabled(false);
                    setTalairachButton.setEnabled(true);
                    applyTalairachButton.setEnabled(false);
                } else {
                    MipavUtil.displayError("Error! Failed to remove most inferior point");
                }
            } else if (leftPt.isSelected()) {
                found = ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).removeReference("L");

                if (found) {
                    haveLeftPt = false;
                    leftPt.setText("Most left point");
                    clearTalairachButton.setEnabled(false);
                    setTalairachButton.setEnabled(true);
                    applyTalairachButton.setEnabled(false);
                } else {
                    MipavUtil.displayError("Error! Failed to remove most left point");
                }
            } else {
                found = ((ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).removeReference("R");

                if (found) {
                    haveRightPt = false;
                    rightPt.setText("Most right point");
                    clearTalairachButton.setEnabled(false);
                    setTalairachButton.setEnabled(true);
                    applyTalairachButton.setEnabled(false);
                } else {
                    MipavUtil.displayError("Error! Failed to remove most right point");
                }
            }
        } else if (command.equals("anteriorPtCommand")) {

            if (haveAnteriorPt) {
                setTalairachButton.setEnabled(false);
                clearTalairachButton.setEnabled(true);
            } else {
                setTalairachButton.setEnabled(true);
                clearTalairachButton.setEnabled(false);
            }
        } else if (command.equals("posteriorPtCommand")) {

            if (havePosteriorPt) {
                setTalairachButton.setEnabled(false);
                clearTalairachButton.setEnabled(true);
            } else {
                setTalairachButton.setEnabled(true);
                clearTalairachButton.setEnabled(false);
            }
        } else if (command.equals("superiorPtCommand")) {

            if (haveSuperiorPt) {
                setTalairachButton.setEnabled(false);
                clearTalairachButton.setEnabled(true);
            } else {
                setTalairachButton.setEnabled(true);
                clearTalairachButton.setEnabled(false);
            }
        } else if (command.equals("inferiorPtCommand")) {

            if (haveInferiorPt) {
                setTalairachButton.setEnabled(false);
                clearTalairachButton.setEnabled(true);
            } else {
                setTalairachButton.setEnabled(true);
                clearTalairachButton.setEnabled(false);
            }
        } else if (command.equals("leftPtCommand")) {

            if (haveLeftPt) {
                setTalairachButton.setEnabled(false);
                clearTalairachButton.setEnabled(true);
            } else {
                setTalairachButton.setEnabled(true);
                clearTalairachButton.setEnabled(false);
            }
        } else if (command.equals("rightPtCommand")) {

            if (haveRightPt) {
                setTalairachButton.setEnabled(false);
                clearTalairachButton.setEnabled(true);
            } else {
                setTalairachButton.setEnabled(true);
                clearTalairachButton.setEnabled(false);
            }
        } else if (command.equals("applyTalairach")) {
            dispose();
            convertToTalairach();
        } else if (command.equals("cancelTalairach")) {
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * Creates Talairach image based on points that were set in component images.
     */
    private void convertToTalairach() {
        int[] orient = new int[3];
        float[] orgResol = new float[3];
        Vector3f pcie = new Vector3f(Float.POSITIVE_INFINITY, Float.POSITIVE_INFINITY, Float.POSITIVE_INFINITY);
        Vector3f TCenter = new Vector3f(Float.POSITIVE_INFINITY, Float.POSITIVE_INFINITY, Float.POSITIVE_INFINITY);
        ;

        Vector3f alpha, beta, gamma, translation, rr;
        Vector3f posrr = new Vector3f(0.0f, 0.0f, 0.0f);
        int[] AFNIOrigExtents;
        float[] AFNIOrigResols;
        float vlength;
        float dist_ant, dist_med, dist_pos, dist_sup, dist_inf, dist_lef, dist_rig;
        float scale_A, scale_M, scale_P, scale_S, scale_I, scale_L, scale_R;
        float shift_P;
        int bufferSize;
        float[] imgBuffer;
        int[] extents;
        float[] resolutions;
        String name;
        FileInfoAfni[] fileInfo;
        int oXdim, oYdim, oZdim;
        Vector3f center = new Vector3f(0.0f, 0.0f, 0.0f);
        int x, y, z;
        float Sx, Sy, Sz;
        float Ty;
        float oXres = 1.0f;
        float oYres = 1.0f;
        float oZres = 1.0f;
        Vector3f[] alphaArray = {
            new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
            new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
            new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
            new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f)
        };
        Vector3f[] betaArray = {
            new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
            new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
            new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
            new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f)
        };
        Vector3f[] gammaArray = {
            new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
            new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
            new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
            new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f)
        };

        // Talairach origin in original image in coordinates
        Vector3f[] rrArray = {
            new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
            new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
            new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
            new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f)
        };

        // Talairach origin in +tlrc image in coordinates
        Vector3f Tal = new Vector3f(0.0f, 0.0f, 0.0f);
        int[] botX = new int[12];
        int[] botY = new int[12];
        int[] botZ = new int[12];
        int[] topX = new int[12];
        int[] topY = new int[12];
        int[] topZ = new int[12];
        @SuppressWarnings("unused")
        float bot_A, bot_M, bot_P, bot_R, bot_L, bot_I, bot_S;
        @SuppressWarnings("unused")
        float top_A, top_M, top_P, top_R, top_L, top_I, top_S;

        Vector3f bv_P = new Vector3f(0.0f, 0.0f, 0.0f);
        Vector3f sv_P = new Vector3f(0.0f, 0.0f, 0.0f);
        Vector3f BTCenter = new Vector3f(0.0f, 0.0f, 0.0f);
        int[] axisOrientation = new int[3];
        ViewJProgressBar progressBar;

        /* Determine the new resolutions */
        for (int i = 0; i <= 2; i++) {
            int unit = image.getFileInfo(0).getUnitsOfMeasure(i);

            if (unit != Unit.MILLIMETERS.getLegacyNum()) {
                MipavUtil.displayError("Units of measure[" + i + "] = " + unit +
                                       " instead of the required MILLIMETERS");

                return;
            }

            orgResol[i] = image.getFileInfo(0).getResolutions()[i];

            if (orgResol[i] <= 0.0f) {
                MipavUtil.displayError("resolution[" + i + "] was recorded as " + orgResol[i] +
                                       " It is being changed to positive");
                orgResol[i] = -orgResol[i];
            }
        }

        if ((orgResol[0] != orgResol[1]) || (orgResol[0] != orgResol[2])) {
            MipavUtil.displayError("Voxels are not cubic as required. Have res[0] = " + orgResol[0] + " res[1] = " +
                                   orgResol[1] + " res[2] = " + orgResol[2]);

            return;
        }

        vlength = orgResol[0];

        orient = image.getFileInfo(0).getAxisOrientation();

        if ((orient[0] != FileInfoBase.ORI_R2L_TYPE) || (orient[1] != FileInfoBase.ORI_A2P_TYPE) ||
                (orient[2] != FileInfoBase.ORI_I2S_TYPE)) {
            MipavUtil.displayError("Orientation is dicom as required. Have orient[0] = " + orient[0] + " orient[1] = " +
                                   orient[1] + " orient[2] = " + orient[2]);

            return;
        }

        if (image.getFileInfo(0).getFileFormat() == FileUtility.AFNI) {
            pcie = ((FileInfoAfni) image.getFileInfo(0)).getpcie();
        }

        if (pcie.X == Float.POSITIVE_INFINITY) {
            MipavUtil.displayError("Error! Posterior commissure inferior edge from +orig to +acpc" +
                                   " transformation is not stored in fileInfo");

            return;
        }

        if (image.getFileInfo(0).getFileFormat() == FileUtility.AFNI) {
            TCenter = ((FileInfoAfni) image.getFileInfo(0)).getTalairachCenter();
        }

        if (TCenter.X == Float.POSITIVE_INFINITY) {
            MipavUtil.displayError("Error! The Talairach center from +orig to +acpc" +
                                   " transformation is not stored in fileInfo");

            return;
        }

        Preferences.debug("Talairach center before Talairach conversion = " + TCenter.X + "," + TCenter.Y + "," +
                          TCenter.Z + "\n");

        /* compute distances between points in various directions */

        dist_ant = (TCenter.Y - anteriorPt3Df.Y) * vlength;
        dist_med = (pcie.Y - TCenter.Y) * vlength;
        dist_pos = (posteriorPt3Df.Y - pcie.Y) * vlength;

        dist_sup = (superiorPt3Df.Z - TCenter.Z) * vlength;
        dist_inf = (TCenter.Z - inferiorPt3Df.Z) * vlength;

        dist_lef = (leftPt3Df.X - TCenter.X) * vlength;
        dist_rig = (TCenter.X - rightPt3Df.X) * vlength;
        Preferences.debug("pcie.Y = " + pcie.Y + "\n");

        /* Check anterior distance */
        if (((dist_ant / ATLAS_FRONT_TO_AC) < MIN_ALLOWED_DEVIATION) ||
                ((dist_ant / ATLAS_FRONT_TO_AC) > MAX_ALLOWED_DEVIATION)) {
            MipavUtil.displayError("Front to Anterior commissure distance outside allowed range dist = " + dist_ant +
                                   " Standard ATLAS_FRONT_TO_AC = " + ATLAS_FRONT_TO_AC);

            return;
        }

        /* Check medial distance */
        if (((dist_med / ViewJFrameTriImage.ATLAS_AC_TO_PC) < MIN_ALLOWED_DEVIATION) ||
                ((dist_med / ViewJFrameTriImage.ATLAS_AC_TO_PC) > MAX_ALLOWED_DEVIATION)) {
            MipavUtil.displayError("Intercommissural distance outside allowed range dist = " + dist_med +
                                   " Standard ViewJFrameTriImage.ATLAS_AC_TO_PC = " +
                                   ViewJFrameTriImage.ATLAS_AC_TO_PC);

            return;
        }

        /* Check posterior distance */
        if (((dist_pos / ATLAS_PC_TO_BACK) < MIN_ALLOWED_DEVIATION) ||
                ((dist_pos / ATLAS_PC_TO_BACK) > MAX_ALLOWED_DEVIATION)) {
            MipavUtil.displayError("Posterior commissure to back distance outside allowed range dist = " + dist_pos +
                                   " Standard ATLAS_PC_TO_BACK = " + ATLAS_PC_TO_BACK);

            return;
        }

        /* Check inferior distance */
        if (((dist_inf / ATLAS_BOT_TO_AC) < MIN_ALLOWED_DEVIATION) ||
                ((dist_inf / ATLAS_BOT_TO_AC) > MAX_ALLOWED_DEVIATION)) {
            MipavUtil.displayError("Bottom to Anterior commissure distance outside allowed range dist = " + dist_inf +
                                   " Standard ATLAS_BOT_TO_AC = " + ATLAS_BOT_TO_AC);

            return;
        }

        /* Check superior distance */
        if (((dist_sup / ATLAS_AC_TO_TOP) < MIN_ALLOWED_DEVIATION) ||
                ((dist_sup / ATLAS_AC_TO_TOP) > MAX_ALLOWED_DEVIATION)) {
            MipavUtil.displayError("Anterior commissure to top distance outside allowed range dist = " + dist_sup +
                                   " Standard ATLAS_AC_TO_TOP = " + ATLAS_AC_TO_TOP);

            return;
        }

        /* Check left distance */
        if (((dist_lef / ATLAS_AC_TO_LAT) < MIN_ALLOWED_DEVIATION) ||
                ((dist_lef / ATLAS_AC_TO_LAT) > MAX_ALLOWED_DEVIATION)) {
            MipavUtil.displayError("Anterior commissure to left distance outside allowed range dist = " + dist_lef +
                                   " Standard ATLAS_AC_TO_LAT = " + ATLAS_AC_TO_LAT);

            return;
        }

        /* Check right distance */
        if (((dist_rig / ATLAS_AC_TO_LAT) < MIN_ALLOWED_DEVIATION) ||
                ((dist_rig / ATLAS_AC_TO_LAT) > MAX_ALLOWED_DEVIATION)) {
            MipavUtil.displayError("Anterior commissure to right distance outside allowed range dist = " + dist_rig +
                                   "Standard ATLAS_AC_TO_LAT = " + ATLAS_AC_TO_LAT);

            return;
        }

        /* Compute scalings needed in each direction and the shift needed posterior to the PC */

        scale_A = ATLAS_FRONT_TO_AC / dist_ant;
        scale_M = ViewJFrameTriImage.ATLAS_AC_TO_PC / dist_med;
        scale_P = ATLAS_PC_TO_BACK / dist_pos;
        scale_S = ATLAS_AC_TO_TOP / dist_sup;
        scale_I = ATLAS_BOT_TO_AC / dist_inf;
        scale_L = ATLAS_AC_TO_LAT / dist_lef;
        scale_R = ATLAS_AC_TO_LAT / dist_rig;

        shift_P = (scale_P * dist_med) - ViewJFrameTriImage.ATLAS_AC_TO_PC;

        /* shift vectors in each direction, for each y cell (A,M,P) */

        bv_P.Y = shift_P;
        sv_P.Y = -shift_P / scale_P;

        /* bounds information for each direction, for each cell */

        bot_A = -9999.0f;
        bot_M = 0.0f;
        bot_P = ViewJFrameTriImage.ATLAS_AC_TO_PC;

        bot_R = -9999.0f;
        bot_L = 0.0f;

        bot_I = -9999.0f;
        bot_S = 0.0f;

        top_A = 0.0f;
        top_M = ViewJFrameTriImage.ATLAS_AC_TO_PC;
        top_P = 9999.0f;

        top_R = 0.0f;
        top_L = 9999.9f;

        top_I = 0.0f;
        top_S = 9999.9f;

        /* Compute the 12 linear maps:
         * They are all linear scalings (diagonal matrices); posterior to the PC, they also contain shifts to align
         * stuff to the nominal PC location. These are maps from AC-PC aligned coordinates to the Talairachsystem. */

        bufferSize = image.getSliceSize() * image.getExtents()[2];
        imgBuffer = new float[bufferSize];

        try {
            image.exportData(0, bufferSize, imgBuffer);
        } catch (IOException error) {
            MipavUtil.displayError("ViewJFrameTriImage: IOException error on exportData");
        }

        oXdim = (int) (((2 * ViewJFrameTriImage.ATLAS_BBOX_LAT) + 1) / vlength); // 161/vlength
        oYdim = (int) ((ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_BBOX_POS + 1) / vlength); // 191/vlength
        oZdim = (int) ((ViewJFrameTriImage.ATLAS_BBOX_INF_NEW + ViewJFrameTriImage.ATLAS_BBOX_SUP + 1) / vlength); // 151/vlength
        Tal.X = ViewJFrameTriImage.ATLAS_BBOX_LAT / vlength;
        Tal.Y = ViewJFrameTriImage.ATLAS_BBOX_ANT / vlength;
        Tal.Z = ViewJFrameTriImage.ATLAS_BBOX_INF_NEW / vlength;
        extents = new int[] { oXdim, oYdim, oZdim };

        // The resolutions actually vary over the 12 differently scaled regions - just use
        // vlength from the +acpc image
        resolutions = new float[] { vlength, vlength, vlength };

        name = JDialogBase.makeImageName(image.getImageName(), "_Talairach");

        // dicom axis orientation
        axisOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
        axisOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
        axisOrientation[2] = FileInfoBase.ORI_I2S_TYPE;

        ModelImage talairachImage = new ModelImage(image.getType(), extents, name);
        talairachImage.setImageOrientation(FileInfoBase.AXIAL);
        fileInfo = new FileInfoAfni[oZdim];

        for (int i = 0; i < oZdim; i++) {
            fileInfo[i] = new FileInfoAfni(image.getFileInfo()[0].getFileName(),
                                           image.getFileInfo()[0].getFileDirectory(), FileUtility.AFNI);
            fileInfo[i].setModality(image.getFileInfo()[0].getModality());
            fileInfo[i].setFileDirectory(image.getFileInfo()[0].getFileDirectory());
            fileInfo[i].setDataType(image.getFileInfo()[0].getDataType());
            fileInfo[i].setEndianess(image.getFileInfo()[0].getEndianess());
            fileInfo[i].setUnitsOfMeasure(image.getFileInfo()[0].getUnitsOfMeasure());
            fileInfo[i].setResolutions(resolutions);
            fileInfo[i].setExtents(extents);
            fileInfo[i].setPixelPadValue(image.getFileInfo()[0].getPixelPadValue());
            fileInfo[i].setPhotometric(image.getFileInfo()[0].getPhotometric());
            fileInfo[i].setAFNIViewType(FileInfoAfni.AFNI_TLRC);
            fileInfo[i].setAxisOrientation(axisOrientation);
            fileInfo[i].setImageOrientation(FileInfoBase.AXIAL);

            if (image.getFileInfo()[0].getFileFormat() == FileUtility.AFNI) {
                fileInfo[i].setLowXmm(((FileInfoAfni) image.getFileInfo()[0]).getLowXmm());
                fileInfo[i].setLowYmm(((FileInfoAfni) image.getFileInfo()[0]).getLowYmm());
                fileInfo[i].setLowZmm(((FileInfoAfni) image.getFileInfo()[0]).getLowZmm());
                fileInfo[i].setHighXmm(((FileInfoAfni) image.getFileInfo()[0]).getHighXmm());
                fileInfo[i].setHighYmm(((FileInfoAfni) image.getFileInfo()[0]).getHighYmm());
                fileInfo[i].setHighZmm(((FileInfoAfni) image.getFileInfo()[0]).getHighZmm());
                fileInfo[i].setAFNITypeString(((FileInfoAfni) image.getFileInfo()[0]).getAFNITypeString());
            }
        }

        talairachImage.setFileInfo(fileInfo);

        for (int i = 0; i < oXdim; i++) {

            for (int j = 0; j < oYdim; j++) {

                for (int k = 0; k < oZdim; k++) {
                    talairachImage.set(i, j, k, 0.0f);
                }
            }
        }

        TransMatrix xfrm = new TransMatrix(4);

        /*
         * TCenterOriginal.X = TCenterNew.X * Sx + center.X + Sx*dx dx = (TCenter.X -
         * ViewJFrameTriImage.ATLAS_BBOX_LAT*Sx - center.X)/Sx TCenterOriginal.Y = TCenterNew.Y * Sy + center.Y + Sy*dy
         * dy = (TCenter.Y - ViewJFrameTriImage.ATLAS_BBOX_ANT*Sy - center.Y)/Sy pcieOriginal.Y = pcieNew.Y * Sy +
         * center.Y + Sy*dy2 dy2 = (pcie.Y - (ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC)*Sy
         * - center.Y)/Sy
         */
        center = image.getImageCentermm(false);
        oXres = vlength;
        oYres = vlength;
        oZres = vlength;

        if (image.getFileInfo()[0].getFileFormat() == FileUtility.AFNI) {
            FileInfoAfni afniInfo = (FileInfoAfni) image.getFileInfo()[0];

            // In the Talairach image pcie.Y = TCenter.Y + ViewJFrameTriImage.ATLAS_AC_TO_PC
            // Use the transformation matrix to determine TCenter in the ACPC image from a
            // posterior section in the Talairach section.  Call this TCenter calculated back
            // from the Talairach posterior BTCenter.
            // BTCenter.X = TCenter.X and BTCenter.Z = TCenter.Z
            // However, BTCenter.Y = pcie.Y -(ViewJFrameTriImage.ATLAS_AC_TO_PC)/scale_P
            // This follows from the fact that in the Talairach image:
            // TCenter.Y = pcie.Y - (ViewJFrameTriImage.ATLAS_AC_TO_PC)
            alpha = afniInfo.getAlpha();
            beta = afniInfo.getBeta();
            gamma = afniInfo.getGamma();
            translation = afniInfo.getTranslation();
            rr = afniInfo.getrr();
            AFNIOrigExtents = afniInfo.getAFNIOrigExtents();
            AFNIOrigResols = afniInfo.getAFNIOrigResolutions();

            // Create the transformation that goes from +acpc to +orig
            xfrm.MakeIdentity();
            xfrm.setTranslate(translation.X, translation.Y, translation.Z);
            xfrm.setRotate(alpha, beta, gamma);
            //Mat = xfrm.getMatrix();

            // There are 2 rr needed with the +orig image for the +orig to +tlrc transformation
            // One rr is the back projection in +orig of a Talairach origin in anterior and
            // median Talairach sections.  The other rr, called posrr, is the back projection in +orig
            // of a Talairach origin in posterior Talairach sections.
            BTCenter.X = TCenter.X;
            BTCenter.Y = pcie.Y - ((ViewJFrameTriImage.ATLAS_AC_TO_PC) / scale_P);
            BTCenter.Z = TCenter.Z;
            posrr.X = (float) ((BTCenter.X * vlength * xfrm.Get(0, 0)) + (BTCenter.Y * vlength * xfrm.Get(0, 1)) +
                               (BTCenter.Z * vlength * xfrm.Get(0, 2)) + xfrm.Get(0, 3)) / AFNIOrigResols[0];
            posrr.Y = (float) ((BTCenter.X * vlength * xfrm.Get(1, 0)) + (BTCenter.Y * vlength * xfrm.Get(1, 1)) +
                               (BTCenter.Z * vlength * xfrm.Get(1, 2)) + xfrm.Get(1, 3)) / AFNIOrigResols[1];
            posrr.Z = (float) ((BTCenter.X * vlength * xfrm.Get(2, 0)) + (BTCenter.Y * vlength * xfrm.Get(2, 1)) +
                               (BTCenter.Z * vlength * xfrm.Get(2, 2)) + xfrm.Get(2, 3)) / AFNIOrigResols[2];
            Preferences.debug("posrr = " + posrr.X + " " + posrr.Y + " " + posrr.Z + "\n");

            progressBar = new ViewJProgressBar(afniInfo.getFileName(), "Transformation pass #" + 1, 0, 100, false, null,
                                               null);
            progressBar.setLocation(Toolkit.getDefaultToolkit().getScreenSize().width / 2, 50);
            progressBar.setVisible(true);

            for (int i = 0; i < 12; i++) {
                progressBar.setMessage("Transformation pass #" + (i + 1));
                xfrm.MakeIdentity();
                xfrm.setTranslate(center.X, center.Y, center.Z);

                switch (i) {

                    case 0:
                        x = R;
                        y = A;
                        z = S;
                        break;

                    case 1:
                        x = L;
                        y = A;
                        z = S;
                        break;

                    case 2:
                        x = R;
                        y = M;
                        z = S;
                        break;

                    case 3:
                        x = L;
                        y = M;
                        z = S;
                        break;

                    case 4:
                        x = R;
                        y = P;
                        z = S;
                        break;

                    case 5:
                        x = L;
                        y = P;
                        z = S;
                        break;

                    case 6:
                        x = R;
                        y = A;
                        z = I;
                        break;

                    case 7:
                        x = L;
                        y = A;
                        z = I;
                        break;

                    case 8:
                        x = R;
                        y = M;
                        z = I;
                        break;

                    case 9:
                        x = L;
                        y = M;
                        z = I;
                        break;

                    case 10:
                        x = R;
                        y = P;
                        z = I;
                        break;

                    case 11:
                        x = L;
                        y = P;
                        z = I;
                        break;

                    default:
                        x = R;
                        y = A;
                        z = I;
                        break;
                } // switch(i)

                switch (x) {

                    case R:
                        Sx = 1.0f / scale_R;
                        botX[i] = 0;
                        topX[i] = (int) Math.round(ViewJFrameTriImage.ATLAS_BBOX_LAT / vlength);
                        break;

                    case L:
                        Sx = 1.0f / scale_L;
                        botX[i] = (int) Math.round(ViewJFrameTriImage.ATLAS_BBOX_LAT / vlength);
                        topX[i] = oXdim - 1;
                        break;

                    default:
                        Sx = 1.0f;
                        botX[i] = 0;
                        topX[i] = oXdim - 1;
                        break;
                } // switch(x)

                switch (y) {

                    case A:
                        Sy = 1.0f / scale_A;
                        Ty = (TCenter.Y - (ViewJFrameTriImage.ATLAS_BBOX_ANT * Sy) - center.Y) / Sy;
                        botY[i] = 0;
                        topY[i] = (int) Math.round(ViewJFrameTriImage.ATLAS_BBOX_ANT / vlength);
                        rrArray[i].X = rr.X;
                        rrArray[i].Y = rr.Y;
                        rrArray[i].Z = rr.Z;
                        break;

                    case M:
                        Sy = 1.0f / scale_M;
                        Ty = (TCenter.Y - (ViewJFrameTriImage.ATLAS_BBOX_ANT * Sy) - center.Y) / Sy;
                        botY[i] = (int) Math.round(ViewJFrameTriImage.ATLAS_BBOX_ANT / vlength);
                        topY[i] = (int)
                                      Math.round((ViewJFrameTriImage.ATLAS_BBOX_ANT +
                                                  ViewJFrameTriImage.ATLAS_AC_TO_PC) / vlength);
                        rrArray[i].X = rr.X;
                        rrArray[i].Y = rr.Y;
                        rrArray[i].Z = rr.Z;
                        break;

                    case P:
                        Sy = 1.0f / scale_P;
                        Ty = (pcie.Y - ((ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC) * Sy) -
                              center.Y) / Sy;
                        botY[i] = (int)
                                      Math.round((ViewJFrameTriImage.ATLAS_BBOX_ANT +
                                                  ViewJFrameTriImage.ATLAS_AC_TO_PC) / vlength);
                        topY[i] = oYdim - 1;
                        rrArray[i].X = posrr.X;
                        rrArray[i].Y = posrr.Y;
                        rrArray[i].Z = posrr.Z;
                        break;

                    default:
                        Sy = 1.0f;
                        Ty = 0.0f;
                        botY[i] = 0;
                        topY[i] = oYdim - 1;
                        rrArray[i].X = rr.X;
                        rrArray[i].Y = rr.Y;
                        rrArray[i].Z = rr.Z;
                        break;
                } // switch(y)

                switch (z) {

                    case I:
                        Sz = 1.0f / scale_I;
                        botZ[i] = 0;
                        topZ[i] = (int) Math.round(ViewJFrameTriImage.ATLAS_BBOX_INF_NEW / vlength);
                        break;

                    case S:
                        Sz = 1.0f / scale_S;
                        botZ[i] = (int) Math.round(ViewJFrameTriImage.ATLAS_BBOX_INF_NEW / vlength);
                        topZ[i] = oZdim - 1;
                        break;

                    default:
                        Sz = 1.0f;
                        botZ[i] = 0;
                        topZ[i] = oZdim - 1;
                        break;
                } // switch(z)

                xfrm.setZoom(Sx, Sy, Sz);

                xfrm.setTranslate((TCenter.X - (ViewJFrameTriImage.ATLAS_BBOX_LAT * Sx) - center.X) / Sx, Ty,
                                  (TCenter.Z - (ViewJFrameTriImage.ATLAS_BBOX_INF_NEW * Sz) - center.Z) / Sz);
                

                transformTalairachTrilinear(imgBuffer, xfrm, vlength, image.getExtents()[0], image.getExtents()[1],
                                            image.getExtents()[2], oXres, oYres, oZres, oXdim, oYdim, oZdim, botX[i],
                                            botY[i], botZ[i], topX[i], topY[i], topZ[i], progressBar, talairachImage);

                // Find the combined matrix that allows going directly from +orig to +tlrc
                alphaArray[i].X = Sx * alpha.X;
                alphaArray[i].Y = Sy * alpha.Y;
                alphaArray[i].Z = Sz * alpha.Z;
                betaArray[i].X = Sx * beta.X;
                betaArray[i].Y = Sy * beta.Y;
                betaArray[i].Z = Sz * beta.Z;
                gammaArray[i].X = Sx * gamma.X;
                gammaArray[i].Y = Sy * gamma.Y;
                gammaArray[i].Z = Sz * gamma.Z;
                Preferences.debug("alphaArray[" + i + "]= " + alphaArray[i].X + " " + alphaArray[i].Y + " " +
                                  alphaArray[i].Z + "\n");
                Preferences.debug("betaArray[" + i + "]=" + betaArray[i].X + " " + betaArray[i].Y + " " +
                                  betaArray[i].Z + "\n");
                Preferences.debug("gammaArray[" + i + "]=" + gammaArray[i].X + " " + gammaArray[i].Y + " " +
                                  gammaArray[i].Z + "\n");
            } // for (i = 0; i < 12; i++)

            talairachImage.calcMinMax();

            for (int i = 0; i < oZdim; i++) {
                fileInfo[i].setMin(talairachImage.getMin());
                fileInfo[i].setMax(talairachImage.getMax());
                fileInfo[i].setAFNIOrigExtents(AFNIOrigExtents);
                fileInfo[i].setAFNIOrigResolutions(AFNIOrigResols);
                fileInfo[i].setAlphaArray(alphaArray);
                fileInfo[i].setBetaArray(betaArray);
                fileInfo[i].setGammaArray(gammaArray);
                fileInfo[i].setrrArray(rrArray);
                fileInfo[i].setBotX(botX);
                fileInfo[i].setBotY(botY);
                fileInfo[i].setBotZ(botZ);
                fileInfo[i].setTopX(topX);
                fileInfo[i].setTopY(topY);
                fileInfo[i].setTopZ(topZ);
                fileInfo[i].setTalairachCenter(Tal);
            }

            try {
                talairachImage.setImageName("Talairach image");
                new ViewJFrameImage(talairachImage, null, new Dimension(610, 200));
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }

            /* Find the new Talairach center in the newly transformed coordinates */
            /* X = (i*oXres*T00 + j*oYres*T01 + k*oZres*T02 + T03)/iXres
             * Y = (i*oXres*T10 + j*oYres*T11 + k*oZres*T12 + T13)/iYres Z = (i*oXres*T20 + j*oYres*T21 + k*oZres*T22 +
             * T23)/iZresWish to find i,j,k from X,Y,Z */

            /* xfrm.identity();
             * xfrm.setTranslate(center.X, center.Y, center.Z); Sx = 1.0f/scale_R; Sy = 1.0f/scale_A; Sz = 1.0f/scale_I;
             * xfrm.setZoom(Sx,Sy,Sz); xfrm.setTranslate((TCenter.X - ViewJFrameTriImage.ATLAS_BBOX_LAT*Sx -
             * center.X)/Sx,              (TCenter.Y - ViewJFrameTriImage.ATLAS_BBOX_ANT*Sy - center.Y)/Sy, (TCenter.Z -
             * ViewJFrameTriImage.ATLAS_BBOX_INF_NEW*Sz -center.Z)/Sz); Mat = xfrm.getMatrix(); Matrix Ac = new
             * Matrix(3,3); Ac.set(0,0,oXres*xfrm.Get(0, 0)/vlength); Ac.set(0,1,oYres*xfrm.Get(0, 1)/vlength);
             * Ac.set(0,2,oZres*xfrm.Get(0, 2)/vlength); Ac.set(1,0,oXres*xfrm.Get(1, 0)/vlength);
             * Ac.set(1,1,oYres*xfrm.Get(1, 1)/vlength); Ac.set(1,2,oZres*xfrm.Get(1, 2)/vlength);
             * Ac.set(2,0,oXres*xfrm.Get(2, 0)/vlength); Ac.set(2,1,oYres*xfrm.Get(2, 1)/vlength);
             * Ac.set(2,2,oZres*xfrm.Get(2, 2)/vlength); Matrix b = new Matrix(3,1); b.set(0,0,TCenter.X -
             * xfrm.Get(0, 3)/vlength); b.set(1,0,TCenter.Y - xfrm.Get(1, 3)/vlength); b.set(2,0,TCenter.Z - xfrm.Get(2, 3)/vlength);
             * Matrix X = Ac.solve(b); // Note that the final tranformed Talairach origin in millimeters should be at //
             * (ViewJFrameTriImage.ATLAS_BBOX_LAT, ViewJFrameTriImage.ATLAS_BBOX_ANT,
             * ViewJFrameTriImage.ATLAS_BBOX_INF_NEW) = (80,80,65) Preferences.debug("Non rounded T = " + X.get(0,0) +
             * "," +                              X.get(1,0) + "," + X.get(2,0) + "\n"); NTCenter.X =
             * (int)Math.round(X.get(0,0)); NTCenter.Y = (int)Math.round(X.get(1,0)); NTCenter.Z =
             * (int)Math.round(X.get(2,0)); Preferences.debug("Transformed Talairach origin = " + NTCenter.X + "  " +
             * NTCenter.Y +              "  " + NTCenter.Z + "\n"); // Now check the location of the new transformed
             * pcie y coordinate.  This should be at // (ViewJFrameTriImage.ATLAS_BBOX_ANT +
             * ViewJFrameTriImage.ATLAS_AC_TO_PC) = 103 xfrm.identity(); xfrm.setTranslate(center.X, center.Y,
             * center.Z); Sx = 1.0f/scale_R; Sy = 1.0f/scale_P; Sz = 1.0f/scale_I; xfrm.setZoom(Sx,Sy,Sz);
             * xfrm.setTranslate((TCenter.X - ViewJFrameTriImage.ATLAS_BBOX_LAT*Sx - center.X)/Sx,              (pcie.Y
             * - (ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC)*Sy - center.Y)/Sy, (TCenter.Z -
             * ViewJFrameTriImage.ATLAS_BBOX_INF_NEW*Sz -center.Z)/Sz); Mat = xfrm.getMatrix(); Ac = new Matrix(3,3);
             * Ac.set(0,0,oXres*xfrm.Get(0, 0)/vlength); Ac.set(0,1,oYres*xfrm.Get(0, 1)/vlength);
             * Ac.set(0,2,oZres*xfrm.Get(0, 2)/vlength); Ac.set(1,0,oXres*xfrm.Get(1, 0)/vlength);
             * Ac.set(1,1,oYres*xfrm.Get(1, 1)/vlength); Ac.set(1,2,oZres*xfrm.Get(1, 2)/vlength);
             * Ac.set(2,0,oXres*xfrm.Get(2, 0)/vlength); Ac.set(2,1,oYres*xfrm.Get(2, 1)/vlength);
             * Ac.set(2,2,oZres*xfrm.Get(2, 2)/vlength); b = new Matrix(3,1); b.set(0,0,pcie.X - xfrm.Get(0, 3)/vlength);
             * b.set(1,0,pcie.Y - xfrm.Get(1, 3)/vlength); b.set(2,0,pcie.Z - xfrm.Get(2, 3)/vlength); X = Ac.solve(b);
             *
             * Preferences.debug("Non rounded pcie = " + X.get(0,0) + "," +                              X.get(1,0) + ","
             * + X.get(2,0) + "\n"); tpcie.X = (int)Math.round(X.get(0,0)); tpcie.Y = (int)Math.round(X.get(1,0));
             * tpcie.Z = (int)Math.round(X.get(2,0)); Preferences.debug("Transformed pcie = " + tpcie.X + "  " + tpcie.Y
             * +              "  " + tpcie.Z + "\n"); */

            progressBar.dispose();

            frame.updateImages(true);
        } else {
            MipavUtil.displayError("Error! Posterior commissure inferior edge from +orig to +acpc" +
                                   " transformation is not stored in fileInfo");

            return;
        }


    }

    /**
     * Initializes GUI components of dialog.
     */
    private void init() {

        setTitle("Create Talairach image");

        JPanel pointPanel = new JPanel(new GridLayout(6, 1));
        pointPanel.setForeground(Color.black);
        pointPanel.setBorder(buildTitledBorder("Select point type"));

        ButtonGroup talairachGroup = new ButtonGroup();
        anteriorPt = new JRadioButton("Most anterior point", true);
        anteriorPt.setFont(serif12);
        anteriorPt.addActionListener(this);
        anteriorPt.setActionCommand("anteriorPtCommand");
        talairachGroup.add(anteriorPt);
        pointPanel.add(anteriorPt);

        posteriorPt = new JRadioButton("Most posterior point", false);
        posteriorPt.setFont(serif12);
        posteriorPt.addActionListener(this);
        posteriorPt.setActionCommand("posteriorPtCommand");
        talairachGroup.add(posteriorPt);
        pointPanel.add(posteriorPt);

        superiorPt = new JRadioButton("Most superior point", false);
        superiorPt.setFont(serif12);
        superiorPt.addActionListener(this);
        superiorPt.setActionCommand("superiorPtCommand");
        talairachGroup.add(superiorPt);
        pointPanel.add(superiorPt);

        inferiorPt = new JRadioButton("Most inferior point", false);
        inferiorPt.setFont(serif12);
        inferiorPt.addActionListener(this);
        inferiorPt.setActionCommand("inferiorPtCommand");
        talairachGroup.add(inferiorPt);
        pointPanel.add(inferiorPt);

        leftPt = new JRadioButton("Most left point", false);
        leftPt.setFont(serif12);
        leftPt.addActionListener(this);
        leftPt.setActionCommand("leftPtCommand");
        talairachGroup.add(leftPt);
        pointPanel.add(leftPt);

        rightPt = new JRadioButton("Most right point", false);
        rightPt.setFont(serif12);
        rightPt.addActionListener(this);
        rightPt.setActionCommand("rightPtCommand");
        talairachGroup.add(rightPt);
        pointPanel.add(rightPt);

        setTalairachButton = new JButton("Set");
        setTalairachButton.setFont(serif12B);
        setTalairachButton.addActionListener(this);
        setTalairachButton.setActionCommand("setTalairach");
        setTalairachButton.setPreferredSize(MipavUtil.defaultButtonSize);

        clearTalairachButton = new JButton("Clear");
        clearTalairachButton.setFont(serif12B);
        clearTalairachButton.addActionListener(this);
        clearTalairachButton.setActionCommand("clearTalairach");
        clearTalairachButton.setEnabled(false);
        clearTalairachButton.setPreferredSize(MipavUtil.defaultButtonSize);

        applyTalairachButton = new JButton("Apply");
        applyTalairachButton.setFont(serif12B);
        applyTalairachButton.addActionListener(this);
        applyTalairachButton.setActionCommand("applyTalairach");
        applyTalairachButton.setEnabled(false);
        applyTalairachButton.setPreferredSize(MipavUtil.defaultButtonSize);

        cancelTalairachButton = new JButton("Cancel");
        cancelTalairachButton.setFont(serif12B);
        cancelTalairachButton.addActionListener(this);
        cancelTalairachButton.setActionCommand("cancelTalairach");
        cancelTalairachButton.setPreferredSize(MipavUtil.defaultButtonSize);

        JPanel buttonPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        buttonPanel.add(setTalairachButton, gbc);
        gbc.gridx = 1;
        buttonPanel.add(clearTalairachButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        buttonPanel.add(applyTalairachButton, gbc);
        gbc.gridx = 1;
        buttonPanel.add(cancelTalairachButton, gbc);
        buttonPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        mainPanel.add(pointPanel);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
    }

    /**
     * Sets anterior label based on the point. Enables "Apply" if all points have been set.
     *
     * @param  pt  Point that was set.
     */
    private void setAnteriorPt(Vector3f pt) {
        anteriorPt.setSelected(true);
        posteriorPt.setSelected(false);
        superiorPt.setSelected(false);
        inferiorPt.setSelected(false);
        leftPt.setSelected(false);
        rightPt.setSelected(false);
        anteriorPt3Df = pt;
        haveAnteriorPt = true;
        anteriorPt.setText("Most anterior point " + (int) pt.X + "," + (int) pt.Y + "," + (int) pt.Z);
        setTalairachButton.setEnabled(false);
        clearTalairachButton.setEnabled(true);

        if ((haveAnteriorPt) && (havePosteriorPt) && (haveSuperiorPt) && (haveInferiorPt) && (haveLeftPt) &&
                (haveRightPt)) {
            applyTalairachButton.setEnabled(true);
        }
    }

    /**
     * Sets inferior label based on the point. Enables "Apply" if all points have been set.
     *
     * @param  pt  Point that was set.
     */
    private void setInferiorPt(Vector3f pt) {
        anteriorPt.setSelected(false);
        posteriorPt.setSelected(false);
        superiorPt.setSelected(false);
        inferiorPt.setSelected(true);
        leftPt.setSelected(false);
        rightPt.setSelected(false);
        inferiorPt3Df = pt;
        haveInferiorPt = true;
        inferiorPt.setText("Most inferior point " + (int) pt.X + "," + (int) pt.Y + "," + (int) pt.Z);
        setTalairachButton.setEnabled(false);
        clearTalairachButton.setEnabled(true);

        if ((haveAnteriorPt) && (havePosteriorPt) && (haveSuperiorPt) && (haveInferiorPt) && (haveLeftPt) &&
                (haveRightPt)) {
            applyTalairachButton.setEnabled(true);
        }
    }

    /**
     * Sets left label based on the point. Enables "Apply" if all points have been set.
     *
     * @param  pt  Point that was set.
     */
    private void setLeftPt(Vector3f pt) {
        anteriorPt.setSelected(false);
        posteriorPt.setSelected(false);
        superiorPt.setSelected(false);
        inferiorPt.setSelected(false);
        leftPt.setSelected(true);
        rightPt.setSelected(false);
        leftPt3Df = pt;
        haveLeftPt = true;
        leftPt.setText("Most left point " + (int) pt.X + "," + (int) pt.Y + "," + (int) pt.Z);
        setTalairachButton.setEnabled(false);
        clearTalairachButton.setEnabled(true);

        if ((haveAnteriorPt) && (havePosteriorPt) && (haveSuperiorPt) && (haveInferiorPt) && (haveLeftPt) &&
                (haveRightPt)) {
            applyTalairachButton.setEnabled(true);
        }
    }

    /**
     * Sets posterior label based on the point. Enables "Apply" if all points have been set.
     *
     * @param  pt  Point that was set.
     */
    private void setPosteriorPt(Vector3f pt) {
        anteriorPt.setSelected(false);
        posteriorPt.setSelected(true);
        superiorPt.setSelected(false);
        inferiorPt.setSelected(false);
        leftPt.setSelected(false);
        rightPt.setSelected(false);
        posteriorPt3Df = pt;
        havePosteriorPt = true;
        posteriorPt.setText("Most posterior point " + (int) pt.X + "," + (int) pt.Y + "," + (int) pt.Z);
        setTalairachButton.setEnabled(false);
        clearTalairachButton.setEnabled(true);

        if ((haveAnteriorPt) && (havePosteriorPt) && (haveSuperiorPt) && (haveInferiorPt) && (haveLeftPt) &&
                (haveRightPt)) {
            applyTalairachButton.setEnabled(true);
        }
    }

    /**
     * Sets right label based on the point. Enables "Apply" if all points have been set.
     *
     * @param  pt  Point that was set.
     */
    private void setRightPt(Vector3f pt) {
        anteriorPt.setSelected(false);
        posteriorPt.setSelected(false);
        superiorPt.setSelected(false);
        inferiorPt.setSelected(false);
        leftPt.setSelected(false);
        rightPt.setSelected(true);
        rightPt3Df = pt;
        haveRightPt = true;
        rightPt.setText("Most right point " + (int) pt.X + "," + (int) pt.Y + "," + (int) pt.Z);
        setTalairachButton.setEnabled(false);
        clearTalairachButton.setEnabled(true);

        if ((haveAnteriorPt) && (havePosteriorPt) && (haveSuperiorPt) && (haveInferiorPt) && (haveLeftPt) &&
                (haveRightPt)) {
            applyTalairachButton.setEnabled(true);
        }
    }

    /**
     * Sets superior label based on the point. Enables "Apply" if all points have been set.
     *
     * @param  pt  Point that was set.
     */
    private void setSuperiorPt(Vector3f pt) {
        anteriorPt.setSelected(false);
        posteriorPt.setSelected(false);
        superiorPt.setSelected(true);
        inferiorPt.setSelected(false);
        leftPt.setSelected(false);
        rightPt.setSelected(false);
        superiorPt3Df = pt;
        haveSuperiorPt = true;
        superiorPt.setText("Most superior point " + (int) pt.X + "," + (int) pt.Y + "," + (int) pt.Z);
        setTalairachButton.setEnabled(false);
        clearTalairachButton.setEnabled(true);

        if ((haveAnteriorPt) && (havePosteriorPt) && (haveSuperiorPt) && (haveInferiorPt) && (haveLeftPt) &&
                (haveRightPt)) {
            applyTalairachButton.setEnabled(true);
        }
    }

    /**
     * Transforms and resamples volume using trilinear interpolation.
     *
     * @param  imgBuffer    Image array.
     * @param  xfrm         Transformation matrix to be applied.
     * @param  ires         In resolution (same in all dimensions).
     * @param  iXdim        In X dimension.
     * @param  iYdim        In Y dimension.
     * @param  iZdim        In Z dimension.
     * @param  oXres        Out X resolution.
     * @param  oYres        Out Y resolution.
     * @param  oZres        Out Z resolution.
     * @param  oXdim        Out X dimension.
     * @param  oYdim        Out Y dimension.
     * @param  oZdim        Out Z dimension.
     * @param  oXlow        Out X low.
     * @param  oYlow        Out Y low.
     * @param  oZlow        Out Z low.
     * @param  oXhigh       Out X high.
     * @param  oYhigh       Out Y high.
     * @param  oZhigh       Out Z high.
     * @param  progressBar  Progress bar.
     * @param  image        Image.
     */
    private void transformTalairachTrilinear(float[] imgBuffer, TransMatrix xfrm, float ires, int iXdim, int iYdim,
                                             int iZdim, float oXres, float oYres, float oZres, int oXdim, int oYdim,
                                             int oZdim, int oXlow, int oYlow, int oZlow, int oXhigh, int oYhigh,
                                             int oZhigh, ViewJProgressBar progressBar, ModelImage image) {
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
        boolean doTransform;


        T00 = xfrm.Get(0, 0);
        T01 = xfrm.Get(0, 1);
        T02 = xfrm.Get(0, 2);
        T03 = xfrm.Get(0, 3);
        T10 = xfrm.Get(1, 0);
        T11 = xfrm.Get(1, 1);
        T12 = xfrm.Get(1, 2);
        T13 = xfrm.Get(1, 3);
        T20 = xfrm.Get(2, 0);
        T21 = xfrm.Get(2, 1);
        T22 = xfrm.Get(2, 2);
        T23 = xfrm.Get(2, 3);


        for (i = oXlow; i <= oXhigh; i++) {
            progressBar.updateValue((int) (((float) (i - oXlow) / (oXhigh - oXlow) * 100) + .5), true);
            imm = (float) i * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = oYlow; j <= oYhigh; j++) {
                jmm = (float) j * oYres;
                j1 = jmm * T01;
                j2 = jmm * T11;
                j3 = jmm * T21;
                temp1 = i3 + j3;
                temp2 = i2 + j2;
                temp3 = i1 + j1;

                for (k = oZlow; k <= oZhigh; k++) {

                    // transform i,j,k
                    doTransform = false;
                    value = 0.0f;
                    kmm = (float) k * oZres;
                    X = (temp3 + (kmm * T02)) / ires;
                    roundX = (int) (X + 0.5f);

                    if ((X >= 0) && (roundX < iXdim)) {
                        Y = (temp2 + (kmm * T12)) / ires;
                        roundY = (int) (Y + 0.5f);

                        if ((Y >= 0) && (roundY < iYdim)) {
                            Z = (temp1 + (kmm * T22)) / ires;
                            roundZ = (int) (Z + 0.5f);

                            if ((Z >= 0) && (roundZ < iZdim)) {

                                if ((roundX == (iXdim - 1)) || (roundY == (iYdim - 1)) || (roundZ == (iZdim - 1))) {
                                    X0pos = roundX;
                                    Y0pos = roundY * iXdim;
                                    Z0pos = roundZ * sliceSize;
                                    value = imgBuffer[Z0pos + Y0pos + X0pos];
                                    doTransform = true;
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
                                    doTransform = true;

                                }
                            } // end if Z in bounds
                        } // end if Y in bounds
                    } // end if X in bounds

                    if (doTransform) {
                        image.set(i, j, k, value);
                    }
                } // end for k
            } // end for j
        } // end for i
    }


}
