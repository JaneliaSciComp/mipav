package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * This structures contains the information that describes how a MRC image is stored on disk.
 */

public class FileInfoMRC extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4677697781087324517L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float alpha, beta, gamma;

    /** DOCUMENT ME! */
    private short creatorID;

    /** DOCUMENT ME! */
    private short idType;

    /** DOCUMENT ME! */
    private int impliedFlagBytes;

    /** DOCUMENT ME! */
    private short ispg, nSymbt;

    /** DOCUMENT ME! */
    private String[] label = null;

    /** DOCUMENT ME! */
    private short lens;

    /** DOCUMENT ME! */
    private int mapC, mapR, mapS;

    /** DOCUMENT ME! */
    private short nd1, nd2, vd1, vd2;

    /** DOCUMENT ME! */
    private int next;

    /** DOCUMENT ME! */
    private short nint;

    /** DOCUMENT ME! */
    private short nreal;

    /** DOCUMENT ME! */
    private short nwave = -1;

    /** DOCUMENT ME! */
    private float rms = Float.NaN;

    /** DOCUMENT ME! */
    private float[] tiltAngles;

    /** DOCUMENT ME! */
    private short wave1, wave2, wave3, wave4, wave5;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * FileInfoMRC - file info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoMRC(String name, String directory, int format) {
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
        dialog.append("Number of bytes in extended header = " + next + "\n");
        dialog.append("Creator ID = " + creatorID + "\n");
        impliedFlagBytes = 0;

        if ((nreal & 0x01) == 0x01) {
            impliedFlagBytes += 2;
        }

        if ((nreal & 0x02) == 0x02) {
            impliedFlagBytes += 6;
        }

        if ((nreal & 0x04) == 0x04) {
            impliedFlagBytes += 4;
        }

        if ((nreal & 0x08) == 0x08) {
            impliedFlagBytes += 2;
        }

        if ((nreal & 0x10) == 0x10) {
            impliedFlagBytes += 4;
        }

        if (nint == impliedFlagBytes) {
            dialog.append("Number of bytes per section in extended header = " + nint + "\n");
            dialog.append("Flags for which type of data from serial EM = " + nreal + "\n");

            if ((nreal & 0x01) == 0x01) {
                dialog.append("Flag for tilt angle * 100 present\n");
            }

            if ((nreal & 0x02) == 0x02) {
                dialog.append("Flag for piece coordinate present\n");
            }

            if ((nreal & 0x04) == 0x04) {
                dialog.append("Flag for stage position present\n");
            }

            if ((nreal & 0x08) == 0x08) {
                dialog.append("Flag for magnification/100 present\n");
            }

            if ((nreal & 0x10) == 0x10) {
                dialog.append("Flag for Reserved (image size?) present\n");
            }
        } // if (nint == impliedFlagBytes)
        else {
            dialog.append("Integers per section in extended header = " + nint + "\n");
            dialog.append("Floats per section in extended header = " + nreal + "\n");
        }

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
        dialog.append("nd1 = " + nd1 + "\n");
        dialog.append("nd2 = " + nd2 + "\n");
        dialog.append("Tilt increment = " + (vd1 / 100.0f) + "\n");
        dialog.append("Starting angle = " + (vd2 / 100.0f) + "\n");

        dialog.append("Angles: (" + tiltAngles[0] + "," + tiltAngles[1] + "," + tiltAngles[2] + ") --> (" +
                      tiltAngles[3] + "," + tiltAngles[4] + "," + tiltAngles[5] + ")\n");

        if (!Float.isNaN(rms)) {
            dialog.append("rms = " + rms + "\n");
        }

        if (nwave >= 1) {
            dialog.append("wave1 = " + wave1 + "\n");
        }

        if (nwave >= 2) {
            dialog.append("wave2 = " + wave2 + "\n");
        }

        if (nwave >= 3) {
            dialog.append("wave3 = " + wave3 + "\n");
        }

        if (nwave >= 4) {
            dialog.append("wave4 = " + wave4 + "\n");
        }

        if (nwave >= 5) {
            dialog.append("wave5 = " + wave5 + "\n");
        }

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
     * @param  creatorID  DOCUMENT ME!
     */
    public void setCreatorID(short creatorID) {
        this.creatorID = creatorID;
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
    public void setIspg(short ispg) {
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
     * @param  nd1  DOCUMENT ME!
     */
    public void setND1(short nd1) {
        this.nd1 = nd1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  nd2  DOCUMENT ME!
     */
    public void setND2(short nd2) {
        this.nd2 = nd2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  next  DOCUMENT ME!
     */
    public void setNext(int next) {
        this.next = next;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  nint  DOCUMENT ME!
     */
    public void setNint(short nint) {
        this.nint = nint;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  nreal  DOCUMENT ME!
     */
    public void setNreal(short nreal) {
        this.nreal = nreal;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  nSymbt  DOCUMENT ME!
     */
    public void setNSymbt(short nSymbt) {
        this.nSymbt = nSymbt;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  nwave  DOCUMENT ME!
     */
    public void setNWave(short nwave) {
        this.nwave = nwave;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  rms  DOCUMENT ME!
     */
    public void setRMS(float rms) {
        this.rms = rms;
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
     * @param  vd1  DOCUMENT ME!
     */
    public void setVD1(short vd1) {
        this.vd1 = vd1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  vd2  DOCUMENT ME!
     */
    public void setVD2(short vd2) {
        this.vd2 = vd2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  wave1  DOCUMENT ME!
     */
    public void setWave1(short wave1) {
        this.wave1 = wave1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  wave2  DOCUMENT ME!
     */
    public void setWave2(short wave2) {
        this.wave2 = wave2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  wave3  DOCUMENT ME!
     */
    public void setWave3(short wave3) {
        this.wave3 = wave3;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  wave4  DOCUMENT ME!
     */
    public void setWave4(short wave4) {
        this.wave4 = wave4;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  wave5  DOCUMENT ME!
     */
    public void setWave5(short wave5) {
        this.wave5 = wave5;
    }
}
