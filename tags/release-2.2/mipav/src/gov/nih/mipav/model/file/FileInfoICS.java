package gov.nih.mipav.model.file;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;

/**
*   This structures contains the information that describes how
*   an ICS image is stored on disk.
*
*       @see        FileICS
*
*/

public class FileInfoICS extends FileInfoBase {
    private String version = null;
    private String dataSetFileName = null;
    private String labels[] = null;
    private String probe[] = null;
    private String sensorType = null;
    private String sensorModel = null;
    private String channels[] = null;
    private String pinholeRadius[] = null;
    private String lambdaEx[] = null;
    private String lambdaEm[] = null;
    private String exPhotonCnt[] = null;
    private String refrInxMedium[] = null;
    private String numAperture[] = null;
    private String refrInxLensMedium[] = null;
    private String history[] = null;
    private String captureVersion = null;
    private String filterExposureTimeX = null;
    private String filterFluorophoreX[] = null;
    private String mapchannel[] = null;
    private String plateChamberID = null;
    private String plateVesselID = null;
    private String specimen[] = null;
    private String specimenSpecies[] = null;
    private String scilType = null;

    /**
    *   File info storage constructor
    *   @param name        file name
    *   @param directory   directory
    *   @param format      file format
    */
    public FileInfoICS(String name, String directory, int format) {
        super(name, directory, format);
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public void setDataSetFileName(String dataSetFileName) {
        this.dataSetFileName = dataSetFileName;
    }

    public void setLabels(String[] labels) {
        this.labels = labels;
    }

    public void setProbe(String[] probe) {
        this.probe = probe;
    }

    public void setSensorType(String sensorType) {
        this.sensorType = sensorType;
    }

    public void setSensorModel(String sensorModel) {
        this.sensorModel = sensorModel;
    }

    public void setChannels(String[] channels) {
        this.channels = channels;
    }

    public void setPinholeRadius(String[] pinholeRadius) {
        this.pinholeRadius = pinholeRadius;
    }

    public void setLambdaEx(String[] lambdaEx) {
        this.lambdaEx = lambdaEx;
    }

    public void setLambdaEm(String[] lambdaEm) {
        this.lambdaEm = lambdaEm;
    }

    public void setExPhotonCnt(String[] exPhotonCnt) {
        this.exPhotonCnt = exPhotonCnt;
    }

    public void setRefrInxMedium(String[] refrInxMedium) {
        this.refrInxMedium = refrInxMedium;
    }

    public void setNumAperture(String[] numAperture) {
        this.numAperture = numAperture;
    }

    public void setRefrInxLensMedium(String[] refrInxLensMedium) {
        this.refrInxLensMedium = refrInxLensMedium;
    }

    public void setHistory(String[] history) {
        this.history = history;
    }

    public void setCaptureVersion(String captureVersion) {
        this.captureVersion = captureVersion;
    }

    public void setFilterExposureTimeX(String filterExposureTimeX) {
        this.filterExposureTimeX = filterExposureTimeX;
    }

    public void setFilterFluorophoreX(String[] filterFluorophoreX) {
        this.filterFluorophoreX = filterFluorophoreX;
    }

    public void setMapchannel(String[] mapchannel) {
        this.mapchannel = mapchannel;
    }

    public void setPlateChamberID(String plateChamberID) {
        this.plateChamberID = plateChamberID;
    }

    public void setPlateVesselID(String plateVesselID) {
        this.plateVesselID = plateVesselID;
    }

    public void setSpecimen(String[] specimen) {
        this.specimen = specimen;
    }

    public void setSpecimenSpecies(String[] specimenSpecies) {
        this.specimenSpecies = specimenSpecies;
    }

    public void setScilType(String scilType) {
        this.scilType = scilType;
    }

    /**
    *   Displays the file information
    *   @param dlog    dialog box that is written to
    *   @param matrix  transformation matrix
    */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix){
        int i;
        JDialogText dialog = (JDialogText) dlog;
        displayPrimaryInfo(dialog, matrix);

        if (version != null) {
            dialog.append("ics_version: " + version + "\n");
        }

        if (dataSetFileName != null) {
            dialog.append("Data set filename: " + dataSetFileName + "\n");
        }

        if (labels != null) {
            dialog.append("labels: ");
            for (i = 0; i < labels.length - 1; i++) {
                dialog.append(labels[i] + "  ");
            }
            dialog.append(labels[labels.length - 1] + "\n");
        }

        if (scilType != null) {
            dialog.append("SCIL_TYPE: " + scilType + "\n");
        }

        if (probe != null) {
            dialog.append("probe: ");
            for (i = 0; i < probe.length - 1; i++) {
                dialog.append(probe[i] + "  ");
            }
            dialog.append(probe[probe.length - 1] + "\n");
        }

        if (sensorType != null) {
            dialog.append("sensor type: " + sensorType + "\n");
        }

        if (sensorModel!= null) {
            dialog.append("sensor model: " + sensorModel + "\n");
        }

        if (channels != null) {
            dialog.append("sensor  s_params  " + channels[0] + ":  ");
            for (i = 1; i < channels.length - 1; i++) {
                dialog.append(channels[i] + "  ");
            }
            dialog.append(channels[channels.length - 1] + "\n");
        }

        if (pinholeRadius != null) {
            dialog.append("sensor  s_params  " + pinholeRadius[0] + ":  ");
            for (i = 1; i < pinholeRadius.length - 1; i++) {
                dialog.append(pinholeRadius[i] + "  ");
            }
            dialog.append(pinholeRadius[pinholeRadius.length - 1] + "\n");
        }

        if (lambdaEx != null) {
            dialog.append("sensor  s_params  " + lambdaEx[0] + ":  ");
            for (i = 1; i < lambdaEx.length - 1; i++) {
                dialog.append(lambdaEx[i] + "  ");
            }
            dialog.append(lambdaEx[lambdaEx.length - 1] + "\n");
        }

        if (lambdaEm != null) {
            dialog.append("sensor  s_params  " + lambdaEm[0] + ":  ");
            for (i = 1; i < lambdaEm.length - 1; i++) {
                dialog.append(lambdaEm[i] + "  ");
            }
            dialog.append(lambdaEm[lambdaEm.length - 1] + "\n");
        }

        if (exPhotonCnt != null) {
            dialog.append("sensor  s_params  " + exPhotonCnt[0] + ":  ");
            for (i = 1; i < exPhotonCnt.length - 1; i++) {
                dialog.append(exPhotonCnt[i] + "  ");
            }
            dialog.append(exPhotonCnt[exPhotonCnt.length - 1] + "\n");
        }

        if (refrInxMedium != null) {
            dialog.append("sensor  s_params  " + refrInxMedium[0] + ":  ");
            for (i = 1; i < refrInxMedium.length - 1; i++) {
                dialog.append(refrInxMedium[i] + "  ");
            }
            dialog.append(refrInxMedium[refrInxMedium.length - 1] + "\n");
        }

        if (numAperture != null) {
            dialog.append("sensor  s_params  " + numAperture[0] + ":  ");
            for (i = 1; i < numAperture.length - 1; i++) {
                dialog.append(numAperture[i] + "  ");
            }
            dialog.append(numAperture[numAperture.length - 1] + "\n");
        }

        if (refrInxLensMedium != null) {
            dialog.append("sensor  s_params  " + refrInxLensMedium[0] + ":  ");
            for (i = 1; i < refrInxLensMedium.length - 1; i++) {
                dialog.append(refrInxLensMedium[i] + "  ");
            }
            dialog.append(refrInxLensMedium[refrInxLensMedium.length - 1] + "\n");
        }

        if (history != null) {
            for (i = 0; i < history.length; i++) {
                dialog.append(history[i] + "\n");
            }
        }

        if (captureVersion != null) {
          dialog.append("capture_version : " + captureVersion + "\n");
        }

        if (filterExposureTimeX != null) {
          dialog.append("filter exposure timeX : " + filterExposureTimeX + "\n");
        }

        if (filterFluorophoreX != null) {
          dialog.append("filter fluorophoreX : ");
          for (i = 0; i < filterFluorophoreX.length - 1; i++) {
                dialog.append(filterFluorophoreX[i] + "  ");
          }
          dialog.append(filterFluorophoreX[filterFluorophoreX.length - 1] + "\n");
        }

        if (mapchannel != null) {
          dialog.append("mapchannel : ");
          for (i = 0; i < mapchannel.length - 1; i++) {
                dialog.append(mapchannel[i] + "  ");
          }
          dialog.append(mapchannel[mapchannel.length - 1] + "\n");
        }

        if (plateChamberID != null) {
          dialog.append("plate chamber ID : " + plateChamberID + "\n");
        }

        if (plateVesselID != null) {
          dialog.append("plate vessel ID : " + plateVesselID + "\n");
        }

        if (specimen != null) {
          dialog.append("specimen : ");
          for (i = 0; i < specimen.length - 1; i++) {
                dialog.append(specimen[i] + "  ");
          }
          dialog.append(specimen[specimen.length - 1] + "\n");
        }

        if (specimenSpecies != null) {
          dialog.append("specimen species : ");
          for (i = 0; i < specimenSpecies.length - 1; i++) {
                dialog.append(specimenSpecies[i] + "  ");
          }
          dialog.append(specimenSpecies[specimenSpecies.length - 1] + "\n");
        }


    }

}
