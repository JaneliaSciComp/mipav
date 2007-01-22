package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * This structures contains the information that describes how an OSM image is stored on disk.
 */

public class FileInfoOSM extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5125356994727655396L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float alpha, beta, gamma;

    /** DOCUMENT ME! */
    private int footerSize;

    /** DOCUMENT ME! */
    private short idType;

    /** DOCUMENT ME! */
    private int ispg, nSymbt;

    /** DOCUMENT ME! */
    private String[] label = null;

    /** DOCUMENT ME! */
    private short lens;

    /** DOCUMENT ME! */
    private int mapC, mapR, mapS;

    /** DOCUMENT ME! */
    private float[] tiltAngles;

    /** DOCUMENT ME! */
    private int wave1, wave2, wave3;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * FileInfoMRC - file info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoOSM(String name, String directory, int format) {
        super(name, directory, format);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Displays the file information.
     *
     * @param  dlog    dialog box that is written to
     * @param  matrix  transformation matrix
     */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix) {
        JDialogText dialog = (JDialogText) dlog;
        displayPrimaryInfo(dialog, matrix);
        dialog.append("\n\n                Other information\n\n");

        dialog.append("Rotation: " + alpha + "," + beta + "," + gamma + "\n");

        switch (mapC) {

            case 1:
                dialog.append("Map column = x\n");
                break;

            case 2:
                dialog.append("Map column = y\n");
                break;

            case 3:
                dialog.append("Map column = z\n");
                break;
        }

        switch (mapR) {

            case 1:
                dialog.append("Map row = x\n");
                break;

            case 2:
                dialog.append("Map row = y\n");
                break;

            case 3:
                dialog.append("Map row = z\n");
                break;
        }

        switch (mapS) {

            case 1:
                dialog.append("Map section = x\n");
                break;

            case 2:
                dialog.append("Map section = y\n");
                break;

            case 3:
                dialog.append("Map section = z\n");
                break;
        }

        dialog.append("Space group number = " + ispg + "\n");
        dialog.append("Number of bytes used for symmetry data = " + nSymbt + "\n");
        dialog.append("Number of bytes in footer = " + footerSize + "\n");

        dialog.append("Type of data: ");

        switch (idType) {

            case 0:
                dialog.append("mono\n");
                break;

            case 1:
                dialog.append("tilt\n");
                break;

            case 2:
                dialog.append("tilts\n");
                break;

            case 3:
                dialog.append("lina\n");
                break;

            case 4:
                dialog.append("lins\n");
                break;
        }

        dialog.append("lens = " + lens + "\n");
        dialog.append("Angles: (" + tiltAngles[0] + "," + tiltAngles[1] + "," + tiltAngles[2] + ") --> (" +
                      tiltAngles[3] + "," + tiltAngles[4] + "," + tiltAngles[5] + ")\n");

        dialog.append("wave1 = " + wave1 + "\n");

        dialog.append("wave2 = " + wave2 + "\n");

        dialog.append("wave3 = " + wave3 + "\n");

        if (label != null) {

            for (int i = 0; i < label.length; i++) {
                dialog.append(label[i].trim() + "\n");
            }
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  alpha  DOCUMENT ME!
     */
    public void setAlpha(float alpha) {
        this.alpha = alpha;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  beta  DOCUMENT ME!
     */
    public void setBeta(float beta) {
        this.beta = beta;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  footerSize  DOCUMENT ME!
     */
    public void setFooterSize(int footerSize) {
        this.footerSize = footerSize;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  gamma  DOCUMENT ME!
     */
    public void setGamma(float gamma) {
        this.gamma = gamma;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  idType  DOCUMENT ME!
     */
    public void setIDType(short idType) {
        this.idType = idType;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  ispg  DOCUMENT ME!
     */
    public void setIspg(int ispg) {
        this.ispg = ispg;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  label  DOCUMENT ME!
     */
    public void setLabel(String[] label) {
        this.label = label;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  lens  DOCUMENT ME!
     */
    public void setLens(short lens) {
        this.lens = lens;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  mapC  DOCUMENT ME!
     */
    public void setMapC(int mapC) {
        this.mapC = mapC;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  mapR  DOCUMENT ME!
     */
    public void setMapR(int mapR) {
        this.mapR = mapR;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  mapS  DOCUMENT ME!
     */
    public void setMapS(int mapS) {
        this.mapS = mapS;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  nSymbt  DOCUMENT ME!
     */
    public void setNSymbt(int nSymbt) {
        this.nSymbt = nSymbt;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  tiltAngles  DOCUMENT ME!
     */
    public void setTiltAngles(float[] tiltAngles) {
        this.tiltAngles = tiltAngles;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  wave1  DOCUMENT ME!
     */
    public void setWave1(int wave1) {
        this.wave1 = wave1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  wave2  DOCUMENT ME!
     */
    public void setWave2(int wave2) {
        this.wave2 = wave2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  wave3  DOCUMENT ME!
     */
    public void setWave3(int wave3) {
        this.wave3 = wave3;
    }
}
