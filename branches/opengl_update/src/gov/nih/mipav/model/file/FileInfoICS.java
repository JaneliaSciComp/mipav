package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * This structures contains the information that describes how an ICS image is stored on disk.
 *
 * @see  FileICS
 */

public class FileInfoICS extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4859517795116658689L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private String captureVersion = null;

    /** DOCUMENT ME! */
    private String[] channels = null;

    /** DOCUMENT ME! */
    private String dataSetFileName = null;

    /** DOCUMENT ME! */
    private String[] exPhotonCnt = null;

    /** DOCUMENT ME! */
    private String filterExposureTimeX = null;

    /** DOCUMENT ME! */
    private String[] filterFluorophoreX = null;

    /** DOCUMENT ME! */
    private String[] history = null;

    /** DOCUMENT ME! */
    private String[] labels = null;

    /** DOCUMENT ME! */
    private String[] lambdaEm = null;

    /** DOCUMENT ME! */
    private String[] lambdaEx = null;

    /** DOCUMENT ME! */
    private String[] mapchannel = null;

    /** DOCUMENT ME! */
    private String[] numAperture = null;

    /** DOCUMENT ME! */
    private String[] pinholeRadius = null;

    /** DOCUMENT ME! */
    private String plateChamberID = null;

    /** DOCUMENT ME! */
    private String plateVesselID = null;

    /** DOCUMENT ME! */
    private String[] probe = null;

    /** DOCUMENT ME! */
    private String[] refrInxLensMedium = null;

    /** DOCUMENT ME! */
    private String[] refrInxMedium = null;

    /** DOCUMENT ME! */
    private String scilType = null;

    /** DOCUMENT ME! */
    private String sensorModel = null;

    /** DOCUMENT ME! */
    private String sensorType = null;

    /** DOCUMENT ME! */
    private String[] specimen = null;

    /** DOCUMENT ME! */
    private String[] specimenSpecies = null;

    /** DOCUMENT ME! */
    private String version = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * File info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoICS(String name, String directory, int format) {
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

            for (i = 0; i < (labels.length - 1); i++) {
                dialog.append(labels[i] + "  ");
            }

            dialog.append(labels[labels.length - 1] + "\n");
        }

        if (scilType != null) {
            dialog.append("SCIL_TYPE: " + scilType + "\n");
        }

        if (probe != null) {
            dialog.append("probe: ");

            for (i = 0; i < (probe.length - 1); i++) {
                dialog.append(probe[i] + "  ");
            }

            dialog.append(probe[probe.length - 1] + "\n");
        }

        if (sensorType != null) {
            dialog.append("sensor type: " + sensorType + "\n");
        }

        if (sensorModel != null) {
            dialog.append("sensor model: " + sensorModel + "\n");
        }

        if (channels != null) {
            dialog.append("sensor  s_params  " + channels[0] + ":  ");

            for (i = 1; i < (channels.length - 1); i++) {
                dialog.append(channels[i] + "  ");
            }

            dialog.append(channels[channels.length - 1] + "\n");
        }

        if (pinholeRadius != null) {
            dialog.append("sensor  s_params  " + pinholeRadius[0] + ":  ");

            for (i = 1; i < (pinholeRadius.length - 1); i++) {
                dialog.append(pinholeRadius[i] + "  ");
            }

            dialog.append(pinholeRadius[pinholeRadius.length - 1] + "\n");
        }

        if (lambdaEx != null) {
            dialog.append("sensor  s_params  " + lambdaEx[0] + ":  ");

            for (i = 1; i < (lambdaEx.length - 1); i++) {
                dialog.append(lambdaEx[i] + "  ");
            }

            dialog.append(lambdaEx[lambdaEx.length - 1] + "\n");
        }

        if (lambdaEm != null) {
            dialog.append("sensor  s_params  " + lambdaEm[0] + ":  ");

            for (i = 1; i < (lambdaEm.length - 1); i++) {
                dialog.append(lambdaEm[i] + "  ");
            }

            dialog.append(lambdaEm[lambdaEm.length - 1] + "\n");
        }

        if (exPhotonCnt != null) {
            dialog.append("sensor  s_params  " + exPhotonCnt[0] + ":  ");

            for (i = 1; i < (exPhotonCnt.length - 1); i++) {
                dialog.append(exPhotonCnt[i] + "  ");
            }

            dialog.append(exPhotonCnt[exPhotonCnt.length - 1] + "\n");
        }

        if (refrInxMedium != null) {
            dialog.append("sensor  s_params  " + refrInxMedium[0] + ":  ");

            for (i = 1; i < (refrInxMedium.length - 1); i++) {
                dialog.append(refrInxMedium[i] + "  ");
            }

            dialog.append(refrInxMedium[refrInxMedium.length - 1] + "\n");
        }

        if (numAperture != null) {
            dialog.append("sensor  s_params  " + numAperture[0] + ":  ");

            for (i = 1; i < (numAperture.length - 1); i++) {
                dialog.append(numAperture[i] + "  ");
            }

            dialog.append(numAperture[numAperture.length - 1] + "\n");
        }

        if (refrInxLensMedium != null) {
            dialog.append("sensor  s_params  " + refrInxLensMedium[0] + ":  ");

            for (i = 1; i < (refrInxLensMedium.length - 1); i++) {
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

            for (i = 0; i < (filterFluorophoreX.length - 1); i++) {
                dialog.append(filterFluorophoreX[i] + "  ");
            }

            dialog.append(filterFluorophoreX[filterFluorophoreX.length - 1] + "\n");
        }

        if (mapchannel != null) {
            dialog.append("mapchannel : ");

            for (i = 0; i < (mapchannel.length - 1); i++) {
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

            for (i = 0; i < (specimen.length - 1); i++) {
                dialog.append(specimen[i] + "  ");
            }

            dialog.append(specimen[specimen.length - 1] + "\n");
        }

        if (specimenSpecies != null) {
            dialog.append("specimen species : ");

            for (i = 0; i < (specimenSpecies.length - 1); i++) {
                dialog.append(specimenSpecies[i] + "  ");
            }

            dialog.append(specimenSpecies[specimenSpecies.length - 1] + "\n");
        }


    }

    /**
     * DOCUMENT ME!
     *
     * @param  captureVersion  DOCUMENT ME!
     */
    public void setCaptureVersion(String captureVersion) {
        this.captureVersion = captureVersion;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  channels  DOCUMENT ME!
     */
    public void setChannels(String[] channels) {
        this.channels = channels;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  dataSetFileName  DOCUMENT ME!
     */
    public void setDataSetFileName(String dataSetFileName) {
        this.dataSetFileName = dataSetFileName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  exPhotonCnt  DOCUMENT ME!
     */
    public void setExPhotonCnt(String[] exPhotonCnt) {
        this.exPhotonCnt = exPhotonCnt;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  filterExposureTimeX  DOCUMENT ME!
     */
    public void setFilterExposureTimeX(String filterExposureTimeX) {
        this.filterExposureTimeX = filterExposureTimeX;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  filterFluorophoreX  DOCUMENT ME!
     */
    public void setFilterFluorophoreX(String[] filterFluorophoreX) {
        this.filterFluorophoreX = filterFluorophoreX;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  history  DOCUMENT ME!
     */
    public void setHistory(String[] history) {
        this.history = history;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  labels  DOCUMENT ME!
     */
    public void setLabels(String[] labels) {
        this.labels = labels;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  lambdaEm  DOCUMENT ME!
     */
    public void setLambdaEm(String[] lambdaEm) {
        this.lambdaEm = lambdaEm;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  lambdaEx  DOCUMENT ME!
     */
    public void setLambdaEx(String[] lambdaEx) {
        this.lambdaEx = lambdaEx;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  mapchannel  DOCUMENT ME!
     */
    public void setMapchannel(String[] mapchannel) {
        this.mapchannel = mapchannel;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  numAperture  DOCUMENT ME!
     */
    public void setNumAperture(String[] numAperture) {
        this.numAperture = numAperture;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pinholeRadius  DOCUMENT ME!
     */
    public void setPinholeRadius(String[] pinholeRadius) {
        this.pinholeRadius = pinholeRadius;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  plateChamberID  DOCUMENT ME!
     */
    public void setPlateChamberID(String plateChamberID) {
        this.plateChamberID = plateChamberID;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  plateVesselID  DOCUMENT ME!
     */
    public void setPlateVesselID(String plateVesselID) {
        this.plateVesselID = plateVesselID;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  probe  DOCUMENT ME!
     */
    public void setProbe(String[] probe) {
        this.probe = probe;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  refrInxLensMedium  DOCUMENT ME!
     */
    public void setRefrInxLensMedium(String[] refrInxLensMedium) {
        this.refrInxLensMedium = refrInxLensMedium;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  refrInxMedium  DOCUMENT ME!
     */
    public void setRefrInxMedium(String[] refrInxMedium) {
        this.refrInxMedium = refrInxMedium;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  scilType  DOCUMENT ME!
     */
    public void setScilType(String scilType) {
        this.scilType = scilType;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  sensorModel  DOCUMENT ME!
     */
    public void setSensorModel(String sensorModel) {
        this.sensorModel = sensorModel;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  sensorType  DOCUMENT ME!
     */
    public void setSensorType(String sensorType) {
        this.sensorType = sensorType;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  specimen  DOCUMENT ME!
     */
    public void setSpecimen(String[] specimen) {
        this.specimen = specimen;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  specimenSpecies  DOCUMENT ME!
     */
    public void setSpecimenSpecies(String[] specimenSpecies) {
        this.specimenSpecies = specimenSpecies;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  version  DOCUMENT ME!
     */
    public void setVersion(String version) {
        this.version = version;
    }

}
