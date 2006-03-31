package gov.nih.mipav.model.file;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;

/**
*   This structures contains the information that describes how
*   a MRC image is stored on disk.
*
*/

public class FileInfoMRC extends FileInfoBase {
    private float alpha, beta, gamma;
    private int mapC, mapR, mapS;
    private short ispg, nSymbt;
    private int next;
    private short creatorID;
    private short nint;
    private short nreal;
    private short idType;
    private short lens;
    private short nd1, nd2, vd1, vd2;
    private float tiltAngles[];
    private String label[] = null;
    private int impliedFlagBytes;
    private float rms = Float.NaN;
    private short nwave = -1;
    private short wave1, wave2, wave3, wave4, wave5;

    /**
    *  FileInfoMRC      - file info storage constructor
    *  @param name        file name
    *  @param directory   directory
    *  @param format      file format
    */
    public FileInfoMRC(String name, String directory, int format) {
        super(name, directory, format);
    }

    public void setAlpha(float alpha) {
        this.alpha = alpha;
    }

    public void setBeta(float beta) {
        this.beta = beta;
    }

    public void setGamma(float gamma) {
        this.gamma = gamma;
    }

    public void setMapC(int mapC) {
        this.mapC = mapC;
    }

    public void setMapR(int mapR) {
        this.mapR = mapR;
    }

    public void setMapS(int mapS) {
        this.mapS = mapS;
    }

    public void setIspg(short ispg) {
        this.ispg = ispg;
    }

    public void setNSymbt(short nSymbt) {
        this.nSymbt = nSymbt;
    }

    public void setNext(int next) {
        this.next = next;
    }

    public void setCreatorID(short creatorID) {
        this.creatorID = creatorID;
    }

    public void setNint(short nint) {
        this.nint = nint;
    }

    public void setNreal(short nreal) {
        this.nreal = nreal;
    }

    public void setIDType(short idType) {
        this.idType = idType;
    }

    public void setLens(short lens) {
        this.lens = lens;
    }

    public void setND1(short nd1) {
        this.nd1 = nd1;
    }

    public void setND2(short nd2) {
        this.nd2 = nd2;
    }

    public void setVD1(short vd1) {
        this.vd1 = vd1;
    }

    public void setVD2(short vd2) {
        this.vd2 = vd2;
    }

    public void setTiltAngles(float[] tiltAngles) {
        this.tiltAngles = tiltAngles;
    }

    public void setLabel(String label[]) {
        this.label = label;
    }

    public void setRMS(float rms) {
        this.rms = rms;
    }

    public void setNWave(short nwave) {
        this.nwave = nwave;
    }

    public void setWave1(short wave1) {
        this.wave1 = wave1;
    }

    public void setWave2(short wave2) {
        this.wave2 = wave2;
    }

    public void setWave3(short wave3) {
        this.wave3 = wave3;
    }

    public void setWave4(short wave4) {
        this.wave4 = wave4;
    }

    public void setWave5(short wave5) {
        this.wave5 = wave5;
    }

    /**
    *  Displays the file information
    *  @param dlog    dialog box that is written to
    *  @param matrix  transformation matrix
    */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix){
        JDialogText dialog = (JDialogText) dlog;
        displayPrimaryInfo(dialog, matrix);
        dialog.append("\n\n                Other information\n\n");

        dialog.append("Rotation: " + alpha + "," + beta + "," + gamma + "\n");

        switch(mapC) {
            case 1: dialog.append("Map column = x\n");
                    break;
            case 2: dialog.append("Map column = y\n");
                    break;
            case 3: dialog.append("Map column = z\n");
                    break;
        }

        switch(mapR) {
            case 1: dialog.append("Map row = x\n");
                    break;
            case 2: dialog.append("Map row = y\n");
                    break;
            case 3: dialog.append("Map row = z\n");
                    break;
        }

        switch(mapS) {
            case 1: dialog.append("Map section = x\n");
                    break;
            case 2: dialog.append("Map section = y\n");
                    break;
            case 3: dialog.append("Map section = z\n");
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
        switch(idType) {
            case 0: dialog.append("mono\n");
                    break;
            case 1: dialog.append("tilt\n");
                    break;
            case 2: dialog.append("tilts\n");
                    break;
            case 3: dialog.append("lina\n");
                    break;
            case 4: dialog.append("lins\n");
                    break;
        }

        dialog.append("lens = " + lens + "\n");
        dialog.append("nd1 = " + nd1 + "\n");
        dialog.append("nd2 = " + nd2 + "\n");
        dialog.append("Tilt increment = " + vd1/100.0f + "\n");
        dialog.append("Starting angle = " + vd2/100.0f + "\n");

        dialog.append("Angles: (" + tiltAngles[0] + "," + tiltAngles[1] + "," + tiltAngles[2]
        + ") --> (" + tiltAngles[3] + "," + tiltAngles[4] + "," + tiltAngles[5] + ")\n");

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
}
