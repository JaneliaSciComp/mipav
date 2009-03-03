package gov.nih.mipav.model.dicomcomm;


/**
 * DICOM_UID.
 */
public class DICOM_UID {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** String representing the Series Instance UID */
    private String seriesInstanceUID = " ";

    /** String representing the SOP Instance UID */
    private String SOPInstanceUID = " ";

    /** String representing the Study Instance UID */
    private String studyInstanceUID = " ";

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DICOM_UID object.
     *
     * @param  studyUID  the study instance UID
     */
    public DICOM_UID(String studyUID) {
        studyInstanceUID = studyUID;
    }

    /**
     * Creates a new DICOM_UID object.
     *
     * @param  seriesUID  the series instance UID
     * @param  studyUID   the study instance UID
     */
    public DICOM_UID(String seriesUID, String studyUID) {
        seriesInstanceUID = seriesUID;
        studyInstanceUID = studyUID;
    }

    /**
     * Creates a new DICOM_UID object.
     *
     * @param  seriesUID  the series instance UID
     * @param  SOP_UID    the SOP instance UID
     * @param  studyUID   the study instance UID
     */
    public DICOM_UID(String seriesUID, String SOP_UID, String studyUID) {
        seriesInstanceUID = seriesUID;
        SOPInstanceUID = SOP_UID;
        studyInstanceUID = studyUID;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Gets the Series Instance UID.
     *
     * @return  the series instance UID
     */
    public String getSeriesInstanceUID() {
        return seriesInstanceUID;
    }

    /**
     * Gets the SOP Instance UID.
     *
     * @return  the SOP instance UID
     */
    public String getSOPInstanceUID() {
        return SOPInstanceUID;
    }

    /**
     * gets the Study Instance UID.
     *
     * @return  the study instance UID
     */
    public String getStudyInstanceUID() {
        return studyInstanceUID;
    }


    /**
     * Sets Series Instance UID.
     *
     * @param  seriesUID  - the series instance UID
     */
    public void setSeriesInstanceUID(String seriesUID) {
        seriesInstanceUID = seriesUID;
    }

    /**
     * Sets SOP Instance UID.
     *
     * @param  SOP_UID  the SOP instance UID
     */
    public void setSOPInstanceUID(String SOP_UID) {
        SOPInstanceUID = SOP_UID;
    }

    /**
     * Sets Study Instance UID.
     *
     * @param  studyUID  the study instance UID
     */
    public void setStudyInstanceUID(String studyUID) {
        studyInstanceUID = studyUID;
    }


}
