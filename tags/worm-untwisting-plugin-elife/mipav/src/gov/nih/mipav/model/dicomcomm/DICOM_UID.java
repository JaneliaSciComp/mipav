package gov.nih.mipav.model.dicomcomm;


/**
 * DICOM_UID.
 * 
 * <hr>
 * 
 * This DICOM communication package was originally based on the Java Dicom Package, whose license is below:
 * 
 * <pre>
 * Java Dicom Package (com.zmed.dicom)
 * 
 *  Copyright (c) 1996-1997 Z Medical Imaging Systems, Inc.
 * 
 *  This software is provided, as is, for non-commercial educational
 *  purposes only.   Use or incorporation of this software or derivative
 *  works in commercial applications requires written consent from
 *  Z Medical Imaging Systems, Inc.
 * 
 *  Z MEDICAL IMAGING SYSTEMS MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT
 *  THE SUITABILITY OF THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING
 *  BUT NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS
 *  FOR A PARTICULAR PURPOSE, NON-INFRINGEMENT, OR CONFORMANCE TO ANY
 *  SPECIFICATION OR STANDARD.  Z MEDICAL IMAGING SYSTEMS SHALL NOT BE
 *  LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING OR
 *  MODIFYING THIS SOFTWARE OR ITS DERIVATIVES.
 * 
 *  =============================================================================
 * 
 *  This software package is implemented similarly to the UC Davis public
 *  domain C++ DICOM implementation which contains the following copyright
 *  notice:
 * 
 *  Copyright (C) 1995, University of California, Davis
 * 
 *  THIS SOFTWARE IS MADE AVAILABLE, AS IS, AND THE UNIVERSITY
 *  OF CALIFORNIA DOES NOT MAKE ANY WARRANTY ABOUT THE SOFTWARE, ITS
 *  PERFORMANCE, ITS MERCHANTABILITY OR FITNESS FOR ANY PARTICULAR
 *  USE, FREEDOM FROM ANY COMPUTER DISEASES OR ITS CONFORMITY TO ANY
 *  SPECIFICATION. THE ENTIRE RISK AS TO QUALITY AND PERFORMANCE OF
 *  THE SOFTWARE IS WITH THE USER.
 * 
 *  Copyright of the software and supporting documentation is
 *  owned by the University of California, and free access
 *  is hereby granted as a license to use this software, copy this
 *  software and prepare derivative works based upon this software.
 *  However, any distribution of this software source code or
 *  supporting documentation or derivative works (source code and
 *  supporting documentation) must include this copyright notice.
 * 
 *  The UC Davis C++ source code is publicly available from the following
 *  anonymous ftp site:
 * 
 *  ftp://imrad.ucdmc.ucdavis.edu/pub/dicom/UCDMC/
 * </pre>
 */
public class DICOM_UID {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** String representing the Series Instance UID */
    private String seriesInstanceUID = " ";

    /** String representing the SOP Instance UID */
    private String SOPInstanceUID = " ";

    /** String representing the Study Instance UID */
    private String studyInstanceUID = " ";

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DICOM_UID object.
     * 
     * @param studyUID the study instance UID
     */
    public DICOM_UID(final String studyUID) {
        studyInstanceUID = studyUID;
    }

    /**
     * Creates a new DICOM_UID object.
     * 
     * @param seriesUID the series instance UID
     * @param studyUID the study instance UID
     */
    public DICOM_UID(final String seriesUID, final String studyUID) {
        seriesInstanceUID = seriesUID;
        studyInstanceUID = studyUID;
    }

    /**
     * Creates a new DICOM_UID object.
     * 
     * @param seriesUID the series instance UID
     * @param SOP_UID the SOP instance UID
     * @param studyUID the study instance UID
     */
    public DICOM_UID(final String seriesUID, final String SOP_UID, final String studyUID) {
        seriesInstanceUID = seriesUID;
        SOPInstanceUID = SOP_UID;
        studyInstanceUID = studyUID;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Gets the Series Instance UID.
     * 
     * @return the series instance UID
     */
    public String getSeriesInstanceUID() {
        return seriesInstanceUID;
    }

    /**
     * Gets the SOP Instance UID.
     * 
     * @return the SOP instance UID
     */
    public String getSOPInstanceUID() {
        return SOPInstanceUID;
    }

    /**
     * gets the Study Instance UID.
     * 
     * @return the study instance UID
     */
    public String getStudyInstanceUID() {
        return studyInstanceUID;
    }

    /**
     * Sets Series Instance UID.
     * 
     * @param seriesUID - the series instance UID
     */
    public void setSeriesInstanceUID(final String seriesUID) {
        seriesInstanceUID = seriesUID;
    }

    /**
     * Sets SOP Instance UID.
     * 
     * @param SOP_UID the SOP instance UID
     */
    public void setSOPInstanceUID(final String SOP_UID) {
        SOPInstanceUID = SOP_UID;
    }

    /**
     * Sets Study Instance UID.
     * 
     * @param studyUID the study instance UID
     */
    public void setStudyInstanceUID(final String studyUID) {
        studyInstanceUID = studyUID;
    }

}
