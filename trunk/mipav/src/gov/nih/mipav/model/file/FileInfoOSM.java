package gov.nih.mipav.model.file;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;

/**
*   This structures contains the information that describes how
*   an OSM image is stored on disk.
*
*/

public class FileInfoOSM extends FileInfoBase {
    private float alpha, beta, gamma;
    private int mapC, mapR, mapS;
    private int ispg, nSymbt;
    private int footerSize;
    private short idType;
    private short lens;
    private float tiltAngles[];
    private String label[] = null;
    private int wave1, wave2, wave3;

    /**
    *  FileInfoMRC      - file info storage constructor
    *  @param name        file name
    *  @param directory   directory
    *  @param format      file format
    */
    public FileInfoOSM(String name, String directory, int format) {
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

    public void setIspg(int ispg) {
        this.ispg = ispg;
    }

    public void setNSymbt(int nSymbt) {
        this.nSymbt = nSymbt;
    }

    public void setFooterSize(int footerSize) {
        this.footerSize = footerSize;
    }

    public void setLens(short lens) {
        this.lens = lens;
    }

    public void setIDType(short idType) {
        this.idType = idType;
    }

    public void setTiltAngles(float[] tiltAngles) {
        this.tiltAngles = tiltAngles;
    }

    public void setLabel(String label[]) {
        this.label = label;
    }

    public void setWave1(int wave1) {
        this.wave1 = wave1;
    }

    public void setWave2(int wave2) {
        this.wave2 = wave2;
    }

    public void setWave3(int wave3) {
        this.wave3 = wave3;
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
        dialog.append("Number of bytes in footer = " + footerSize + "\n");

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
        dialog.append("Angles: (" + tiltAngles[0] + "," + tiltAngles[1] + "," + tiltAngles[2]
        + ") --> (" + tiltAngles[3] + "," + tiltAngles[4] + "," + tiltAngles[5] + ")\n");

        dialog.append("wave1 = " + wave1 + "\n");

        dialog.append("wave2 = " + wave2 + "\n");

        dialog.append("wave3 = " + wave3 + "\n");

        if (label != null) {
            for (int i = 0; i < label.length; i++) {
                dialog.append(label[i].trim() + "\n");
            }
        }
    }
}
